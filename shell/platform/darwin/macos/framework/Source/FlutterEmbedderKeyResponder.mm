// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#import <objc/message.h>
#include <functional>
#include <memory>
#include <unordered_map>

#import "FlutterEmbedderKeyResponder.h"
#import "KeyCodeMap_Internal.h"
#import "flutter/shell/platform/darwin/common/framework/Headers/FlutterCodecs.h"
#import "flutter/shell/platform/darwin/macos/framework/Source/FlutterViewController_Internal.h"
#import "flutter/shell/platform/embedder/embedder.h"

namespace {

NSDictionary* ToNSDictionary(std::unordered_map<uint64_t, uint64_t> source) {
  NSMutableDictionary* result = [NSMutableDictionary dictionary];
  for (auto [key, value] : source) {
    result[@(key)] = @(value);
  }
  return result;
}

/**
 * Isolate the least significant 1-bit.
 *
 * For example,
 *
 *  * lowestSetBit(0x1010) returns 0x10.
 *  * lowestSetBit(0) returns 0.
 */
static NSUInteger lowestSetBit(NSUInteger bitmask) {
  // This utilizes property of two's complement (negation), which propagates a
  // carry bit from LSB to the lowest set bit.
  return bitmask & -bitmask;
}

/**
 * Whether a string represents a control character.
 */
static bool IsControlCharacter(uint64_t character) {
  return (character <= 0x1f && character >= 0x00) || (character >= 0x7f && character <= 0x9f);
}

/**
 * Whether a string represents an unprintable key.
 */
static bool IsUnprintableKey(uint64_t character) {
  return character >= 0xF700 && character <= 0xF8FF;
}

/**
 * Returns a key code composed with a base key and a plane.
 *
 * Examples of unprintable keys are "NSUpArrowFunctionKey = 0xF700" or
 * "NSHomeFunctionKey = 0xF729".
 *
 * See
 * https://developer.apple.com/documentation/appkit/1535851-function-key_unicodes?language=objc
 * for more information.
 */
static uint64_t KeyOfPlane(uint64_t baseKey, uint64_t plane) {
  return plane | (baseKey & flutter::kValueMask);
}

/**
 * Returns the physical key for a key code.
 */
static uint64_t GetPhysicalKeyForKeyCode(unsigned short keyCode) {
  NSNumber* physicalKey = [flutter::keyCodeToPhysicalKey objectForKey:@(keyCode)];
  if (physicalKey == nil) {
    return KeyOfPlane(keyCode, flutter::kMacosPlane);
  }
  return physicalKey.unsignedLongLongValue;
}

/**
 * Returns the logical key for a modifier physical key.
 */
static uint64_t GetLogicalKeyForModifier(unsigned short keyCode) {
  NSNumber* fromKeyCode = [flutter::keyCodeToLogicalKey objectForKey:@(keyCode)];
  if (fromKeyCode != nil) {
    return fromKeyCode.unsignedLongLongValue;
  }
  return KeyOfPlane(keyCode, flutter::kMacosPlane);
}

/**
 * Converts upper letters to lower letters in ASCII, and returns as-is
 * otherwise.
 *
 * Independent of locale.
 */
static uint64_t toLower(uint64_t n) {
  constexpr uint64_t lowerA = 0x61;
  constexpr uint64_t upperA = 0x41;
  constexpr uint64_t upperZ = 0x5a;

  constexpr uint64_t lowerAGrave = 0xe0;
  constexpr uint64_t upperAGrave = 0xc0;
  constexpr uint64_t upperThorn = 0xde;
  constexpr uint64_t division = 0xf7;

  // ASCII range.
  if (n >= upperA && n <= upperZ) {
    return n - upperA + lowerA;
  }

  // EASCII range.
  if (n >= upperAGrave && n <= upperThorn && n != division) {
    return n - upperAGrave + lowerAGrave;
  }

  return n;
}

// Decode a UTF-16 sequence to an array of char32 (UTF-32).
//
// See https://en.wikipedia.org/wiki/UTF-16#Description for the algorithm.
//
// The returned character array must be deallocated with delete[]. The length of
// the result is stored in `out_length`.
//
// Although NSString has a dataUsingEncoding method, we implement our own
// because dataUsingEncoding outputs redundant characters for unknown reasons.
static uint32_t* DecodeUtf16(NSString* target, size_t* out_length) {
  // The result always has a length less or equal to target.
  size_t result_pos = 0;
  uint32_t* result = new uint32_t[target.length];
  uint16_t high_surrogate = 0;
  for (NSUInteger target_pos = 0; target_pos < target.length; target_pos += 1) {
    uint16_t codeUnit = [target characterAtIndex:target_pos];
    // BMP
    if (codeUnit <= 0xD7FF || codeUnit >= 0xE000) {
      result[result_pos] = codeUnit;
      result_pos += 1;
      // High surrogates
    } else if (codeUnit <= 0xDBFF) {
      high_surrogate = codeUnit - 0xD800;
      // Low surrogates
    } else {
      uint16_t low_surrogate = codeUnit - 0xDC00;
      result[result_pos] = (high_surrogate << 10) + low_surrogate + 0x10000;
      result_pos += 1;
    }
  }
  *out_length = result_pos;
  return result;
}

/**
 * Converts NSEvent.timestamp to the timestamp for Flutter.
 */
static double GetFlutterTimestampFrom(NSTimeInterval timestamp) {
  // Timestamp in microseconds. The event.timestamp is in seconds with sub-ms precision.
  return timestamp * 1000000.0;
}

/**
 * Compute |modifierFlagOfInterestMask| out of |keyCodeToModifierFlag|.
 *
 * This is equal to the bitwise-or of all values of |keyCodeToModifierFlag|.
 */
static NSUInteger computeModifierFlagOfInterestMask() {
  __block NSUInteger modifierFlagOfInterestMask = 0;
  [flutter::keyCodeToModifierFlag
      enumerateKeysAndObjectsUsingBlock:^(NSNumber* keyCode, NSNumber* flag, BOOL* stop) {
        modifierFlagOfInterestMask = modifierFlagOfInterestMask | [flag unsignedLongValue];
      }];
  return modifierFlagOfInterestMask;
}

/**
 * The C-function sent to the embedder's |SendKeyEvent|, wrapping
 * |FlutterEmbedderKeyResponder.handleResponse|.
 *
 * For the reason of this wrap, see |FlutterKeyPendingResponse|.
 */
void HandleResponse(bool handled, void* user_data);

}  // namespace

/**
 * The invocation context for |HandleResponse|, wrapping
 * |FlutterEmbedderKeyResponder.handleResponse|.
 */
struct FlutterKeyPendingResponse {
  FlutterEmbedderKeyResponder* responder;
  uint64_t responseId;
};

enum class EventType {
  kUp,
  kDown,
  kRepeat,
};

FlutterKeyEventType ToEmbedderApiType(EventType type) {
  switch (type) {
    case EventType::kUp:
      return kFlutterKeyEventTypeUp;
    case EventType::kDown:
      return kFlutterKeyEventTypeDown;
    case EventType::kRepeat:
      return kFlutterKeyEventTypeRepeat;
    default:
      FML_UNREACHABLE();
  }
}

/**
 * Guards a |AsyncKeyCallback| to make sure it's handled exactly once
 * throughout |FlutterEmbedderKeyResponder.handleEvent|.
 *
 * A callback can be resolved either with |ResolveByPending| or
 * |ResolveByHandling|. Either way, the callback cannot be resolved again, or an
 * assertion will be thrown.
 */
class FlutterKeyCallbackGuard {
 public:
  static constexpr uint64_t kDontNeedResponse = 0;

  FlutterKeyCallbackGuard(uint64_t response_id) : _response_id(response_id) {}

  ~FlutterKeyCallbackGuard() {
    if (_response_id != kDontNeedResponse && !resolved()) {
      FML_LOG(ERROR) << "The callback is returned without being resolved.";
    }
    if (_response_id != kDontNeedResponse && !sent_any_events()) {
      FML_LOG(ERROR) << "The callback is returned without sending any events.";
    }
  }

  /**
   * Mark that this guard has been used to send a primary event a return the
   * stored response ID.
   */
  uint64_t ResolveByPending() {
    FML_DCHECK(!_resolved) << "This callback has been resolved.";
    FML_DCHECK(_response_id != kDontNeedResponse) << "Unexpected empty response";
    if (_resolved) {
      return 0;
    }
    _resolved = true;
    _sent_primary_event = true;
    return _response_id;
  }

  /**
   * Mark that this guard has been resolved by directly invoking the handler callback
   * and without a primary event.
   */
  void ResolveByHandling() {
    FML_DCHECK(!_resolved) << "This callback has been resolved.";
    if (_resolved) {
      return;
    }
    _resolved = true;
  }

  void MarkSentSynthesizedEvent() { _sent_synthesized_events = true; }

  bool resolved() const { return _resolved; }

  bool sent_any_events() const { return _sent_primary_event || _sent_synthesized_events; }

 private:
  const uint64_t _response_id;
  bool _resolved = false;
  bool _sent_primary_event = false;
  bool _sent_synthesized_events = false;

  FML_DISALLOW_COPY_AND_ASSIGN(FlutterKeyCallbackGuard);
};

class NativeEvent {
 public:
  NativeEvent() {}
  virtual ~NativeEvent() {}
  virtual uint64_t physical_key() = 0;
  virtual uint64_t logical_key() = 0;
  virtual double timestamp() = 0;
  virtual const char* character() { return nullptr; }

 private:
  FML_DISALLOW_COPY_AND_ASSIGN(NativeEvent);
};

enum class LockState { kReleasedOff, kPressedOn, kReleasedOn, kPressedOff };

class StateTrackerBase {
 protected:
  uint64_t EnsureLogicalKey(NativeEvent& native_event, bool force_update = false) {
    const uint64_t physical_key = native_event.physical_key();
    if (!force_update) {
      auto found_entry = _logical_key_record.find(physical_key);
      if (found_entry != _logical_key_record.end()) {
        return found_entry->second;
      }
    }
    const uint64_t logical_key = native_event.logical_key();
    _logical_key_record[physical_key] = logical_key;
    return logical_key;
  }

  std::unordered_map<uint64_t, uint64_t> _logical_key_record;
};

// PressStateTracker handles keys that have two states, pressed or not, and that
// can be uniquely indexed by their physical keys.
//
// The corresponding logical key is allowed to change at each down event. Some
// methods of also allows "repeat" events.
class PressStateTracker : public StateTrackerBase {
 public:
  std::unordered_map<uint64_t, uint64_t> GetPressedState() {
    std::unordered_map<uint64_t, uint64_t> result;
    for (auto [physical_key, pressed] : _pressed_keys) {
      if (pressed) {
        auto logical_key_entry = _logical_key_record.find(physical_key);
        FML_DCHECK(logical_key_entry != _logical_key_record.end());
        result[physical_key] = logical_key_entry->second;
      }
    }
    return result;
  }

  void RequireTextKeyState(std::vector<FlutterKeyEvent>* output,
                           NativeEvent& source_event,
                           std::optional<bool> require_pressed_before,
                           bool require_pressed_after) {
    FML_DCHECK(output != nullptr);

    const uint64_t physical_key = source_event.physical_key();
    std::vector<EventType> event_types =
        ModelTextKeyEvent(physical_key, require_pressed_before, require_pressed_after);
    for (EventType event_type : event_types) {
      const uint64_t logical_key =
          EnsureLogicalKey(source_event, /*force_update=*/event_type == EventType::kDown);
      ;
      output->push_back(FlutterKeyEvent{
          .struct_size = sizeof(FlutterKeyEvent),
          .timestamp = source_event.timestamp(),
          .type = ToEmbedderApiType(event_type),
          .physical = physical_key,
          .logical = logical_key,
          .character = nullptr,  // Assigning later
          .synthesized = true,   // Assigning later
      });
    }

    // Correctly assign the primary event, which is the last event.
    if (!event_types.empty()) {
      output->back().synthesized = false;
      if (require_pressed_after) {
        output->back().character = source_event.character();
      }
    } else {
      // If the required state is pressed, then there must be at least an event
      // to convey the character.
      FML_DCHECK(!require_pressed_after);
    }
  }

  void RequireModifierKeyState(std::vector<FlutterKeyEvent>* output,
                               NativeEvent& source_event,
                               bool require_pressed_before_primary,
                               std::optional<bool> require_pressed_after_primary) {
    FML_DCHECK(output != nullptr);

    const uint64_t physical_key = source_event.physical_key();
    std::vector<ModifierStateChange> state_changes;
    ModelModifierState(&state_changes, physical_key,
                       /*require_pressed_after=*/require_pressed_before_primary,
                       /*synthesized=*/true);
    if (require_pressed_after_primary.has_value()) {
      ModelModifierState(&state_changes, physical_key,
                         /*require_pressed_after=*/require_pressed_after_primary.value(),
                         /*synthesized=*/false);
    }
    for (ModifierStateChange& state_change : state_changes) {
      const uint64_t logical_key =
          EnsureLogicalKey(source_event, /*force_update=*/state_change.type == EventType::kDown);
      ;
      output->push_back(FlutterKeyEvent{
          .struct_size = sizeof(FlutterKeyEvent),
          .timestamp = source_event.timestamp(),
          .type = ToEmbedderApiType(state_change.type),
          .physical = physical_key,
          .logical = logical_key,
          .character = nullptr,
          .synthesized = state_change.synthesized,
      });
    }
  }

 private:
  typedef std::unordered_map<uint64_t, bool> StateMap;

  struct ModifierStateChange {
    EventType type;
    bool synthesized;
  };

  std::vector<EventType> ModelTextKeyEvent(uint64_t physical_key,
                                           std::optional<bool> require_pressed_before,
                                           bool require_pressed_after) {
    std::vector<EventType> output;
    StateMap::iterator current = _pressed_keys.try_emplace(physical_key, false).first;

    // Align with "require_pressed_before".
    if (require_pressed_before.has_value()) {
      MaybePushBack(&output, EnsurePressed(current, require_pressed_before.value()));
    }

    // Align with "require_pressed_after".
    MaybePushBack(&output, EnsurePressed(current, require_pressed_after));

    // Special case: A repeat event will require "pressed" state on a pressed
    // key, needing this special clause to generate output.
    if (output.size() == 0 && require_pressed_after) {
      FML_DCHECK(current->second)
          << "Bad implementation: This can only happen when the key is pressed.";
      output.push_back(EventType::kRepeat);
    }
    return output;
  }

  void ModelModifierState(std::vector<ModifierStateChange>* output,
                          uint64_t physical_key,
                          bool require_pressed_after,
                          bool synthesized) {
    StateMap::iterator current = _pressed_keys.try_emplace(physical_key, false).first;

    auto maybe_event_type = EnsurePressed(current, require_pressed_after);
    if (maybe_event_type.has_value()) {
      output->push_back(ModifierStateChange{
          .type = maybe_event_type.value(),
          .synthesized = synthesized,
      });
    }
  }

  StateMap _pressed_keys;

  // Ensure key state requirement by optionally pushing an event to `output` and
  // changing the value of `current` accordingly.
  static std::optional<EventType> EnsurePressed(StateMap::iterator current, bool require_pressed) {
    if (current->second == require_pressed) {
      return std::nullopt;
    }
    current->second = !current->second;
    return require_pressed ? EventType::kDown : EventType::kUp;
  }

  static void MaybePushBack(std::vector<EventType>* output, std::optional<EventType> event_type) {
    if (event_type.has_value()) {
      output->push_back(event_type.value());
    }
  }
};

struct StateChange {
  EventType change;
  bool synthesized;
};

// LockStateTracker handles keys that have four states, pressed or not, lock on
// or off. These keys must be uniquely indexed by their logical keys.
class LockStateTracker {
 public:
  std::vector<StateChange> RequireState(uint64_t logical_key,
                                        std::optional<LockState> require_primary_state,
                                        LockState require_after_cleanup) {
    std::vector<StateChange> output;
    StateMap::iterator current = _states.try_emplace(logical_key, LockState::kReleasedOff).first;
    if (require_primary_state.has_value()) {
      EnsureLockState(&output, current, PreviousStateOf(require_primary_state.value()),
                      /*synthesized=*/true);
      EnsureLockState(&output, current, require_primary_state.value(), /*synthesized=*/false);
    }
    EnsureLockState(&output, current, require_after_cleanup, /*synthesized=*/true);
    return output;
  }

  std::unordered_map<uint64_t, uint64_t> GetPressedState() {
    std::unordered_map<uint64_t, uint64_t> result;
    if (IsPressed(_states[flutter::kCapsLockLogicalKey])) {
      result[flutter::kCapsLockPhysicalKey] = flutter::kCapsLockLogicalKey;
    }
    return result;
  }

 private:
  typedef std::unordered_map<uint64_t, LockState> StateMap;

  StateMap _states;

  static bool IsPressed(LockState lock_state) {
    switch (lock_state) {
      case LockState::kReleasedOff:
      case LockState::kReleasedOn:
        return false;
      case LockState::kPressedOn:
      case LockState::kPressedOff:
      default:
        FML_UNREACHABLE();
    }
  }

  // Ensure key state requirement by optionally pushing an event to `output` and
  // changing the value of `current` accordingly.
  static void EnsureLockState(std::vector<StateChange>* output,
                              StateMap::iterator& current,
                              LockState required_state,
                              bool synthesized) {
    while (current->second != required_state) {
      EventType change = ProgressState(current);
      output->push_back({
          .change = change,
          .synthesized = synthesized,
      });
    }
  }

  static LockState PreviousStateOf(LockState target) {
    switch (target) {
      case LockState::kReleasedOff:
        return LockState::kPressedOff;
      case LockState::kPressedOn:
        return LockState::kReleasedOff;
      case LockState::kReleasedOn:
        return LockState::kPressedOn;
      case LockState::kPressedOff:
        return LockState::kReleasedOn;
      default:
        FML_UNREACHABLE();
    }
  }

  static EventType ProgressState(StateMap::iterator& current) {
    switch (current->second) {
      case LockState::kReleasedOff:
        current->second = LockState::kPressedOn;
        return EventType::kDown;
      case LockState::kPressedOn:
        current->second = LockState::kReleasedOn;
        return EventType::kUp;
      case LockState::kReleasedOn:
        current->second = LockState::kPressedOff;
        return EventType::kDown;
      case LockState::kPressedOff:
        current->second = LockState::kReleasedOff;
        return EventType::kUp;
      default:
        FML_UNREACHABLE();
    }
  }
};

class NativeEventMacos : public NativeEvent {
 public:
  NativeEventMacos(NSEvent* event) : _native_event((__bridge_retained void*)event) {
    FML_DCHECK(event.type == NSEventTypeKeyDown || event.type == NSEventTypeKeyUp ||
               event.type == NSEventTypeFlagsChanged);
  }

  uint64_t physical_key() override {
    if (_physical_key == 0) {
      unsigned short key_code = native_event().keyCode;
      NSNumber* found_object = [flutter::keyCodeToPhysicalKey objectForKey:@(key_code)];
      if (found_object == nil) {
        return KeyOfPlane(key_code, flutter::kMacosPlane);
      }
      _physical_key = found_object.unsignedLongLongValue;
    }
    return _physical_key;
  }

  double timestamp() override { return GetFlutterTimestampFrom(native_event().timestamp); }

 protected:
  NSEvent* native_event() { return (__bridge NSEvent*)_native_event; }

 private:
  void* _native_event;
  uint64_t _physical_key;
};

class NativeEventMacosText : public NativeEventMacos {
 public:
  NativeEventMacosText(NSEvent* event, NSMutableDictionary<NSNumber*, NSNumber*>* layout_map)
      : NativeEventMacos(event), _layout_map(layout_map) {}

  uint64_t logical_key() override {
    NSNumber* from_layout_map = _layout_map[@(native_event().keyCode)];
    return from_layout_map != nil ? [from_layout_map unsignedLongLongValue]
                                  : GetLogicalKeyForEvent(native_event(), physical_key());
  }

  const char* character() override {
    if (native_event().type == NSEventTypeKeyUp) {
      return nullptr;
    }
    NSString* characters = native_event().characters;
    if ([characters length] == 0) {
      return nullptr;
    }
    unichar utf16Code = [characters characterAtIndex:0];
    if (utf16Code >= 0xf700 && utf16Code <= 0xf7ff) {
      // Some function keys are assigned characters with codepoints from the
      // private use area. These characters are filtered out since they're
      // unprintable.
      //
      // The official documentation reserves 0xF700-0xF8FF as private use area
      // (https://developer.apple.com/documentation/appkit/1535851-function-key_unicodes?language=objc).
      // But macOS seems to only use a reduced range of it. The official doc
      // defines a few constants, all of which are within 0xF700-0xF747.
      // (https://developer.apple.com/documentation/appkit/1535851-function-key_unicodes?language=objc).
      // This mostly aligns with the experimentation result, except for 0xF8FF,
      // which is used for the "Apple logo" character (Option-Shift-K on a US
      // keyboard.)
      //
      // Assume that non-printable function keys are defined from
      // 0xF700 upwards, and printable private keys are defined from 0xF8FF
      // downwards. This function filters out 0xF700-0xF7FF in order to keep
      // the printable private keys.
      return nullptr;
    }
    return [characters UTF8String];
  }

 private:
  NSMutableDictionary<NSNumber*, NSNumber*>* _layout_map;

  /**
   * Returns the logical key of a KeyUp or KeyDown event.
   *
   * For FlagsChanged event, use GetLogicalKeyForModifier.
   */
  static uint64_t GetLogicalKeyForEvent(NSEvent* event, uint64_t physicalKey) {
    // Look to see if the keyCode can be mapped from keycode.
    NSNumber* fromKeyCode = [flutter::keyCodeToLogicalKey objectForKey:@(event.keyCode)];
    if (fromKeyCode != nil) {
      return fromKeyCode.unsignedLongLongValue;
    }

    // Convert `charactersIgnoringModifiers` to UTF32.
    NSString* keyLabelUtf16 = event.charactersIgnoringModifiers;

    // Check if this key is a single character, which will be used to generate the
    // logical key from its Unicode value.
    //
    // Multi-char keys will be minted onto the macOS plane because there are no
    // meaningful values for them. Control keys and unprintable keys have been
    // converted by `keyCodeToLogicalKey` earlier.
    uint32_t character = 0;
    if (keyLabelUtf16.length != 0) {
      size_t keyLabelLength;
      uint32_t* keyLabel = DecodeUtf16(keyLabelUtf16, &keyLabelLength);
      if (keyLabelLength == 1) {
        uint32_t keyLabelChar = *keyLabel;
        NSCAssert(!IsControlCharacter(keyLabelChar) && !IsUnprintableKey(keyLabelChar),
                  @"Unexpected control or unprintable keylabel 0x%x", keyLabelChar);
        NSCAssert(keyLabelChar <= 0x10FFFF, @"Out of range keylabel 0x%x", keyLabelChar);
        character = keyLabelChar;
      }
      delete[] keyLabel;
    }
    if (character != 0) {
      return KeyOfPlane(toLower(character), flutter::kUnicodePlane);
    }

    // We can't represent this key with a single printable unicode, so a new code
    // is minted to the macOS plane.
    return KeyOfPlane(event.keyCode, flutter::kMacosPlane);
  }
};

class NativeEventMacosModifier : public NativeEventMacos {
 public:
  NativeEventMacosModifier(NSEvent* event) : NativeEventMacos(event) {}

  uint64_t logical_key() override { return GetLogicalKeyForModifier(native_event().keyCode); }
};

class NativeEventMacosModifierFlag : public NativeEvent {
 public:
  NativeEventMacosModifierFlag(NSUInteger modifier_flag, NSTimeInterval timestamp)
      : _modifier_flag(modifier_flag), _timestamp(timestamp) {}

  uint64_t physical_key() override { return GetPhysicalKeyForKeyCode(key_code()); }

  double timestamp() override { return GetFlutterTimestampFrom(_timestamp); }

  uint64_t logical_key() override { return GetLogicalKeyForModifier(key_code()); }

 private:
  unsigned short key_code() {
    NSNumber* key_code = [flutter::modifierFlagToKeyCode objectForKey:@(_modifier_flag)];
    // This is something we can assert because _modifier_flag must be of interest.
    FML_DCHECK(key_code != nil) << "Invalid modifier flag 0x" << std::hex << _modifier_flag;
    if (key_code == nil) {
      return 0;
    }
    return [key_code unsignedShortValue];
  }

  NSUInteger _modifier_flag;
  NSTimeInterval _timestamp;
};

@interface FlutterEmbedderKeyResponder ()
/**
 * Processes the response from the framework.
 */
- (void)handleResponseForId:(uint64_t)responseId withResult:(BOOL)handled;
@end

@implementation FlutterEmbedderKeyResponder {
  PressStateTracker* _press_tracker;
  LockStateTracker* _lock_tracker;

  FlutterSendEmbedderKeyEvent _sendEventToEngine;

  // A self-incrementing ID used to label key events sent to the framework.
  //
  // All IDs are positive. 0 is reserved for empty.
  uint64_t _nextResponseId;

  // A map of unresponded key events sent to the framework.
  //
  // Its values are |responseId|s, and keys are the callback that was received
  // along with the event.
  NSMutableDictionary<NSNumber*, FlutterAsyncKeyCallback>* _pendingResponses;

  // A constant mask for NSEvent.modifierFlags that Flutter synchronizes with.
  //
  // Flutter keeps track of the last |modifierFlags| and compares it with the
  // incoming one. Any bit within |_modifierFlagOfInterestMask| that is different
  // (except for the one that corresponds to the event key) indicates that an
  // event for this modifier was missed, and Flutter synthesizes an event to make
  // up for the state difference.
  //
  // It is computed by computeModifierFlagOfInterestMask.
  NSUInteger _modifierFlagOfInterestMask;

  // The modifier flags of the last received key event, excluding uninterested
  // bits.
  //
  // This should be kept synchronized with the last |NSEvent.modifierFlags|
  // after masking with |_modifierFlagOfInterestMask|. This should also be kept
  // synchronized with the corresponding keys of |_pressing_records|.
  //
  // This is used by |SynchronizeModifiers| to quickly find
  // out modifier keys that are desynchronized.
  NSUInteger _lastModifierFlagsOfInterest;
}

@synthesize layoutMap;

- (nonnull instancetype)initWithSendEvent:(FlutterSendEmbedderKeyEvent)sendEvent {
  self = [super init];
  if (self != nil) {
    _sendEventToEngine = sendEvent;
    _nextResponseId = 1;  // Starts at 1; 0 reserved for empty
    _pendingResponses = [NSMutableDictionary dictionary];
    _press_tracker = new PressStateTracker;
    _lock_tracker = new LockStateTracker;
    _modifierFlagOfInterestMask = computeModifierFlagOfInterestMask();
    _lastModifierFlagsOfInterest = 0;
  }
  return self;
}

- (void)dealloc {
  delete _press_tracker;
  delete _lock_tracker;
}

- (void)sendEvent:(const FlutterKeyEvent&)event guard:(FlutterKeyCallbackGuard&)guard {
  if (event.synthesized) {
    // Send a synthesized key event, never expecting its event result.
    guard.MarkSentSynthesizedEvent();
    _sendEventToEngine(event, nullptr, nullptr);
  } else {
    // Send a primary event to the framework, expecting its response.
    uint64_t response_id = guard.ResolveByPending();
    // The `pending` is released in `HandleResponse`.
    FlutterKeyPendingResponse* pending = new FlutterKeyPendingResponse{self, response_id};
    _sendEventToEngine(event, HandleResponse, pending);
  }
}

- (void)handleEvent:(NSEvent*)event callback:(FlutterAsyncKeyCallback)callback {
  // The conversion algorithm relies on a non-nil callback to properly compute
  // `synthesized`.
  NSAssert(callback != nil, @"The callback must not be nil.");

  uint64_t responseId = _nextResponseId;
  _nextResponseId += 1;
  _pendingResponses[@(responseId)] = callback;
  auto guarded_callback = std::make_unique<FlutterKeyCallbackGuard>(responseId);

  switch (event.type) {
    case NSEventTypeKeyDown:
    case NSEventTypeKeyUp:
      [self DoSynchronizeModifierFlags:event.modifierFlags
                         ignoringFlags:0
                             timestamp:event.timestamp
             basedOnCapslockFlagChange:false
                                 guard:*guarded_callback];
      [self HandlePressEvent:event guard:*guarded_callback];
      break;
    case NSEventTypeFlagsChanged:
      [self HandleFlagEvent:event guard:*guarded_callback];
      break;
    default:
      NSAssert(false, @"Unexpected key event type: |%@|.", @(event.type));
  }
  // Every handleEvent's callback expects a reply. If the native event generates
  // no primary events, reply it now with "handled".
  if (!guarded_callback->resolved()) {
    guarded_callback->ResolveByHandling();
    [self handleResponseForId:responseId withResult:true];
  }
  // Every native event must send at least one event to satisfy the protocol for
  // event modes. If there are no any events sent, synthesize an empty one here.
  // This will not be needed when the channel mode is no more.
  if (!guarded_callback->sent_any_events()) {
    FlutterKeyEvent flutterEvent = {
        .struct_size = sizeof(FlutterKeyEvent),
        .timestamp = 0,
        .type = kFlutterKeyEventTypeDown,
        .physical = 0,
        .logical = 0,
        .character = nil,
        .synthesized = true,
    };
    [self sendEvent:flutterEvent guard:*guarded_callback];
    guarded_callback->MarkSentSynthesizedEvent();
  }
  NSAssert(_lastModifierFlagsOfInterest == (event.modifierFlags & _modifierFlagOfInterestMask),
           @"The modifier flags are not properly updated: recorded 0x%lx, event with mask 0x%lx",
           _lastModifierFlagsOfInterest, event.modifierFlags & _modifierFlagOfInterestMask);
}

#pragma mark - Private

- (void)HandlePressEvent:(NSEvent*)event guard:(FlutterKeyCallbackGuard&)guard {
  NativeEventMacosText native_event(event, self.layoutMap);
  std::vector<FlutterKeyEvent> events;
  switch (event.type) {
    case NSEventTypeKeyDown:
      if (!event.isARepeat) {
        _press_tracker->RequireTextKeyState(&events, native_event, /*require_pressed_before=*/false,
                                            /*require_pressed_after=*/true);
      } else {
        _press_tracker->RequireTextKeyState(&events, native_event,
                                            /*require_pressed_before=*/std::nullopt,
                                            /*require_pressed_after=*/true);
      }
      break;
    case NSEventTypeKeyUp:
      _press_tracker->RequireTextKeyState(&events, native_event,
                                          /*require_pressed_before=*/std::nullopt,
                                          /*require_pressed_after=*/false);
      break;
    default:
      FML_UNREACHABLE();
  }
  for (FlutterKeyEvent& event : events) {
    [self sendEvent:event guard:guard];
  }
}

- (void)HandleFlagEvent:(NSEvent*)event guard:(FlutterKeyCallbackGuard&)guard {
  const uint64_t physical_key = GetPhysicalKeyForKeyCode(event.keyCode);
  if (physical_key == flutter::kCapsLockPhysicalKey) {
    [self DoSynchronizeModifierFlags:event.modifierFlags
                       ignoringFlags:0
                           timestamp:event.timestamp
           basedOnCapslockFlagChange:true
                               guard:guard];
    return;
  }

  NativeEventMacosModifier native_event(event);
  NSNumber* targetModifierFlagObj = flutter::keyCodeToModifierFlag[@(event.keyCode)];
  NSUInteger target_modifier_flag =
      targetModifierFlagObj == nil ? 0 : [targetModifierFlagObj unsignedLongValue];
  [self DoSynchronizeModifierFlags:event.modifierFlags
                     ignoringFlags:target_modifier_flag
                         timestamp:event.timestamp
         basedOnCapslockFlagChange:false
                             guard:guard];

  std::vector<FlutterKeyEvent> events;
  bool require_pressed_after_primary = event.modifierFlags & target_modifier_flag;
  _press_tracker->RequireModifierKeyState(
      &events, native_event,
      /*require_pressed_before_primary=*/!require_pressed_after_primary,
      /*require_pressed_after_primary=*/require_pressed_after_primary);
  for (FlutterKeyEvent& event : events) {
    [self sendEvent:event guard:guard];
  }
  _lastModifierFlagsOfInterest = (_lastModifierFlagsOfInterest & ~target_modifier_flag) |
                                 (require_pressed_after_primary ? 0 : target_modifier_flag);
}

- (void)DoSynchronizeModifierFlags:(NSUInteger)current_flags
                     ignoringFlags:(NSUInteger)ignoring_flags
                         timestamp:(NSTimeInterval)timestamp
         basedOnCapslockFlagChange:(bool)based_on_capslock_flag_change
                             guard:(FlutterKeyCallbackGuard&)guard {
  [self SynchronizeModifierKeys:current_flags
                  ignoringFlags:ignoring_flags
                      timestamp:timestamp
                          guard:guard];
  [self SynchronizeCapsLock:timestamp
                     shouldBeOn:current_flags & NSEventModifierFlagCapsLock
      basedOnCapslockFlagChange:based_on_capslock_flag_change
                          guard:guard];
}

// Compare the last modifier flags and the current, and dispatch synthesized
// key events for each different modifier flag bit.
//
// The flags compared are all flags after masking with
// |modifierFlagOfInterestMask| and excluding |ignoringFlags|.
//
// The |guard| is basically a regular guarded callback, but instead of being
// called, it is only used to record whether an event is sent.
- (void)SynchronizeModifierKeys:(NSUInteger)current_flags
                  ignoringFlags:(NSUInteger)ignoring_flags
                      timestamp:(NSTimeInterval)timestamp
                          guard:(FlutterKeyCallbackGuard&)guard {
  const NSUInteger current_flags_of_interest = current_flags & _modifierFlagOfInterestMask;
  NSUInteger flag_difference =
      (current_flags_of_interest ^ _lastModifierFlagsOfInterest) & ~ignoring_flags;
  std::vector<FlutterKeyEvent> events;
  while (true) {
    const NSUInteger current_flag = lowestSetBit(flag_difference);
    if (current_flag == 0) {
      break;
    }
    flag_difference = flag_difference & ~current_flag;
    BOOL should_be_pressed = (current_flags_of_interest & current_flag) != 0;
    NativeEventMacosModifierFlag source_event(current_flag, timestamp);
    _press_tracker->RequireModifierKeyState(&events, source_event,
                                            /*require_pressed_before_primary=*/should_be_pressed,
                                            /*require_pressed_after_primary=*/std::nullopt);
    _lastModifierFlagsOfInterest =
        (_lastModifierFlagsOfInterest & ~current_flag) | (should_be_pressed ? 0 : current_flag);
  }
  for (FlutterKeyEvent& event : events) {
    [self sendEvent:event guard:guard];
  }
}

- (void)SynchronizeCapsLock:(NSTimeInterval)timestamp
                   shouldBeOn:(bool)should_be_on
    basedOnCapslockFlagChange:(bool)based_on_capslock_flag_change
                        guard:(FlutterKeyCallbackGuard&)guard {
  const double timestamp_us = GetFlutterTimestampFrom(timestamp);
  std::optional<LockState> require_primary_state;
  if (based_on_capslock_flag_change) {
    require_primary_state = should_be_on ? LockState::kPressedOn : LockState::kPressedOff;
  }
  LockState require_after_cleanup = should_be_on ? LockState::kReleasedOn : LockState::kReleasedOff;
  for (StateChange state_change : _lock_tracker->RequireState(
           flutter::kCapsLockLogicalKey, require_primary_state, require_after_cleanup)) {
    FlutterKeyEvent out_event = {
        .struct_size = sizeof(FlutterKeyEvent),
        .timestamp = timestamp_us,
        .type = ToEmbedderApiType(state_change.change),
        .physical = flutter::kCapsLockPhysicalKey,
        .logical = flutter::kCapsLockLogicalKey,
        .character = nullptr,
        .synthesized = state_change.synthesized,
    };

    [self sendEvent:out_event guard:guard];
  }
}

- (void)handleResponseForId:(uint64_t)responseId withResult:(BOOL)handled {
  FlutterAsyncKeyCallback callback = _pendingResponses[@(responseId)];
  NSAssert(callback != nil, @"Invalid response ID %llu", responseId);
  callback(handled);
  [_pendingResponses removeObjectForKey:@(responseId)];
}

- (void)syncModifiersIfNeeded:(NSEventModifierFlags)modifierFlags
                    timestamp:(NSTimeInterval)timestamp {
  auto guard =
      std::make_unique<FlutterKeyCallbackGuard>(FlutterKeyCallbackGuard::kDontNeedResponse);
  [self DoSynchronizeModifierFlags:modifierFlags
                     ignoringFlags:0
                         timestamp:timestamp
         basedOnCapslockFlagChange:false
                             guard:*guard];
  guard->MarkSentSynthesizedEvent();
}

- (nonnull NSDictionary*)getPressedState {
  std::unordered_map<uint64_t, uint64_t> result;
  auto press_state = _press_tracker->GetPressedState();
  result.insert(press_state.begin(), press_state.end());
  auto lock_state = _lock_tracker->GetPressedState();
  result.insert(lock_state.begin(), lock_state.end());
  return ToNSDictionary(result);
}

// - (void)printPressedState {
//   printf("Pressed: {");
//   for (auto [phy, log] : _common_keyboard->GetPressedState()) {
//     printf("0x%llx: 0x%llx, ", phy, log);
//   }
//   printf("}\n");
//   fflush(stdout);
// }

@end

namespace {
void HandleResponse(bool handled, void* user_data) {
  // Use unique_ptr to release on leaving.
  auto pending = std::unique_ptr<FlutterKeyPendingResponse>(
      reinterpret_cast<FlutterKeyPendingResponse*>(user_data));
  [pending->responder handleResponseForId:pending->responseId withResult:handled];
}
}  // namespace
