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
static uint64_t GetLogicalKeyForModifier(unsigned short keyCode, uint64_t hidCode) {
  NSNumber* fromKeyCode = [flutter::keyCodeToLogicalKey objectForKey:@(keyCode)];
  if (fromKeyCode != nil) {
    return fromKeyCode.unsignedLongLongValue;
  }
  return KeyOfPlane(hidCode, flutter::kMacosPlane);
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
 * This is equal to the bitwise-or of all values of |keyCodeToModifierFlag| as
 * well as NSEventModifierFlagCapsLock.
 */
static NSUInteger computeModifierFlagOfInterestMask() {
  __block NSUInteger modifierFlagOfInterestMask = NSEventModifierFlagCapsLock;
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

/**
 * Converts NSEvent.characters to a C-string for FlutterKeyEvent.
 */
const char* getEventString(NSEvent* event) {
  if (event.type == NSEventTypeKeyUp) {
    return nullptr;
  }
  NSString* characters = event.characters;
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
}  // namespace

/**
 * The invocation context for |HandleResponse|, wrapping
 * |FlutterEmbedderKeyResponder.handleResponse|.
 */
struct FlutterKeyPendingResponse {
  FlutterEmbedderKeyResponder* responder;
  uint64_t responseId;
};

/**
 * Guards a |AsyncKeyCallback| to make sure it's handled exactly once
 * throughout |FlutterEmbedderKeyResponder.handleEvent|.
 *
 * A callback can either be handled with |pendTo|, or with |resolveTo:|.
 * Either way, the callback cannot be handled again, or an assertion will be
 * thrown.
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
   * Handle the callback by storing it to pending responses and return the response ID.
   */
  uint64_t ResolveByPending() {
    FML_DCHECK(!_resolved) << "This callback has been resolved.";
    FML_DCHECK(_response_id != 0) << "Unexpected empty response";
    if (_resolved) {
      return 0;
    }
    _resolved = true;
    _sent_primary_event = true;
    return _response_id;
  }

  void ResolveWithoutPrimaryEvent() {
    FML_DCHECK(!_resolved) << "This callback has been resolved.";
    if (_resolved) {
      return;
    }
    _resolved = true;
    _sent_primary_event = false;
  }

  void MarkSentSynthesizedEvent() { _sent_synthesized_events = true; }

  bool resolved() const { return _resolved; }

  bool sent_primary_event() const { return _sent_primary_event; }

  bool sent_any_events() const { return _sent_primary_event || _sent_synthesized_events; }

 private:
  const uint64_t _response_id;
  bool _resolved = false;
  bool _sent_primary_event = false;
  bool _sent_synthesized_events = false;

  FML_DISALLOW_COPY_AND_ASSIGN(FlutterKeyCallbackGuard);
};

struct NativeKeyData {
  // Flutter timestamp in us.
  double timestamp;
  // Flutter physical key.
  uint64_t physical_key;
  // Flutter logical key.
  uint64_t logical_key;
  // The macOS NSEvent.character.
  NSString* characters;
  // Whether the event is down or repeat.
  bool is_down;
  // Whether the event is repeat.
  bool is_repeat;
  // The macOS modifier flag.
  NSUInteger modifier_flag;
};

enum class EventType {
  kUp,
  kDown,
  kRepeat,
};

class PressStateTracker {
 public:
  std::vector<EventType> RequireState(uint64_t physical_key,
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

  std::optional<EventType> RequireModifierState(uint64_t physical_key, bool require_pressed_after) {
    StateMap::iterator current = _pressed_keys.try_emplace(physical_key, false).first;

    // Align with "require_pressed_after".
    return EnsurePressed(current, require_pressed_after);
  }

  std::unordered_map<uint64_t, bool> pressed_keys() const { return _pressed_keys; }

 private:
  typedef std::unordered_map<uint64_t, bool> StateMap;

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

class CommonKeyboard {
 public:
  using SendEvent = std::function<void(const FlutterKeyEvent* event, uint64_t response_id)>;
  using DeriveLogicalKey = std::function<uint64_t(NSEvent* event, uint64_t physical_key)>;

  CommonKeyboard(SendEvent send_event, DeriveLogicalKey derive_logical_key)
      : _send_event(send_event),
        _derive_logical_key(derive_logical_key),
        _modifier_flag_of_interest_mask(computeModifierFlagOfInterestMask()) {}

  std::unordered_map<uint64_t, uint64_t> GetPressedState() {
    std::unordered_map<uint64_t, uint64_t> result = _pressing_records;
    for (auto [physical_key, pressed] : _press_tracker.pressed_keys()) {
      if (pressed) {
        auto logical_key_entry = _logical_key_record.find(physical_key);
        FML_DCHECK(logical_key_entry != _logical_key_record.end());
        result[physical_key] = logical_key_entry->second;
      }
    }
    return result;
  }

  void HandlePressEvent(NSEvent* native_event, FlutterKeyCallbackGuard& guard) {
    SynchronizeModifiers(native_event.modifierFlags,
                         /*ignoringFlags=*/0, GetFlutterTimestampFrom(native_event.timestamp),
                         guard);

    uint64_t physical_key = GetPhysicalKeyForKeyCode(native_event.keyCode);

    std::vector<EventType> event_types;
    const char* pending_character = nullptr;
    switch (native_event.type) {
      case NSEventTypeKeyDown:
        if (!native_event.isARepeat) {
          event_types = _press_tracker.RequireState(physical_key, /*require_pressed_before=*/false,
                                                    /*require_pressed_after=*/true);
        } else {
          event_types =
              _press_tracker.RequireState(physical_key, /*require_pressed_before=*/std::nullopt,
                                          /*require_pressed_after=*/true);
        }
        pending_character = getEventString(native_event);
        break;
      case NSEventTypeKeyUp:
        event_types =
            _press_tracker.RequireState(physical_key, /*require_pressed_before=*/std::nullopt,
                                        /*require_pressed_after=*/false);
        break;
      default:
        FML_UNREACHABLE();
    }
    size_t num_events = event_types.size();

    for (size_t event_idx = 0; event_idx < num_events; event_idx += 1) {
      EventType out_type = event_types[event_idx];
      uint64_t logical_key;
      if (out_type == EventType::kDown) {
        logical_key = EnsureLogicalKey(
            physical_key,
            [native_event, this](uint64_t physical_key) {
              return _derive_logical_key(native_event, physical_key);
            },
            /*force_update=*/true);
      } else {
        // For repeat and up events, use the existing record.
        auto found_logical_key = _logical_key_record.find(physical_key);
        FML_DCHECK(found_logical_key != _logical_key_record.end());
        logical_key = found_logical_key->second;
      }

      const char* event_character;
      if (out_type == EventType::kDown || out_type == EventType::kRepeat) {
        event_character = pending_character;
        pending_character = nullptr;
      } else {
        event_character = nullptr;
      }

      FlutterKeyEvent out_event = {
          .struct_size = sizeof(FlutterKeyEvent),
          .timestamp = GetFlutterTimestampFrom(native_event.timestamp),
          .type = ToEmbedderApiType(out_type),
          .physical = physical_key,
          .logical = logical_key,
          .character = event_character,
          // The last event is the primary event; other events are synthesized.
          .synthesized = event_idx != num_events - 1,
      };

      SendEventToEngine(&out_event, guard);
    }

    FML_DCHECK(pending_character == nullptr) << "The character is left undispatched.";
    if (num_events == 0) {
      guard.ResolveWithoutPrimaryEvent();
    }
  }

  void HandleCapsLockEvent(NSEvent* event, FlutterKeyCallbackGuard& guard) {
    SynchronizeModifiers(event.modifierFlags,
                         /*ignoringFlags=*/NSEventModifierFlagCapsLock,
                         GetFlutterTimestampFrom(event.timestamp), guard);
    if ((_last_modifier_flags_of_interest & NSEventModifierFlagCapsLock) !=
        (event.modifierFlags & NSEventModifierFlagCapsLock)) {
      SendCapsLockTap(GetFlutterTimestampFrom(event.timestamp), /*synthesizeDown=*/false, guard);
      _last_modifier_flags_of_interest =
          _last_modifier_flags_of_interest ^ NSEventModifierFlagCapsLock;
    } else {
      guard.ResolveWithoutPrimaryEvent();
    }
  }

  void HandleFlagEvent(NSEvent* native_event, FlutterKeyCallbackGuard& guard) {
    uint64_t physical_key = GetPhysicalKeyForKeyCode(native_event.keyCode);
    if (physical_key == flutter::kCapsLockPhysicalKey) {
      return HandleCapsLockEvent(native_event, guard);
    }

    NSNumber* targetModifierFlagObj = flutter::keyCodeToModifierFlag[@(native_event.keyCode)];
    NSUInteger target_modifier_flag =
        targetModifierFlagObj == nil ? 0 : [targetModifierFlagObj unsignedLongValue];
    SynchronizeModifiers(native_event.modifierFlags,
                         /*ignoringFlags=*/target_modifier_flag,
                         GetFlutterTimestampFrom(native_event.timestamp), guard);

    bool should_be_pressed = (native_event.modifierFlags & target_modifier_flag) != 0;

    std::optional<EventType> maybe_event_type =
        _press_tracker.RequireModifierState(physical_key,
                                            /*require_pressed_after=*/should_be_pressed);

    if (maybe_event_type.has_value()) {
      uint64_t logical_key = EnsureLogicalKey(physical_key, [native_event](uint64_t physical_key) {
        return GetLogicalKeyForModifier(native_event.keyCode, physical_key);
      });

      FlutterKeyEvent out_event = {
          .struct_size = sizeof(FlutterKeyEvent),
          .timestamp = GetFlutterTimestampFrom(native_event.timestamp),
          .type = ToEmbedderApiType(maybe_event_type.value()),
          .physical = physical_key,
          .logical = logical_key,
          .character = nullptr,
          .synthesized = false,
      };
      SendEventToEngine(&out_event, guard);
    } else {
      guard.ResolveWithoutPrimaryEvent();
    }

    _last_modifier_flags_of_interest = native_event.modifierFlags & _modifier_flag_of_interest_mask;
  }

  // Compare the last modifier flags and the current, and dispatch synthesized
  // key events for each different modifier flag bit.
  //
  // The flags compared are all flags after masking with
  // |modifierFlagOfInterestMask| and excluding |ignoringFlags|.
  //
  // The |guard| is basically a regular guarded callback, but instead of being
  // called, it is only used to record whether an event is sent.
  void SynchronizeModifiers(NSUInteger current_flags,
                            NSUInteger ignoring_flags,
                            double timestamp_us,
                            FlutterKeyCallbackGuard& guard) {
    const NSUInteger current_flags_of_interest = current_flags & _modifier_flag_of_interest_mask;
    NSUInteger flag_difference =
        (current_flags_of_interest ^ _last_modifier_flags_of_interest) & ~ignoring_flags;
    if (flag_difference & NSEventModifierFlagCapsLock) {
      SendCapsLockTap(timestamp_us, /*synthesize_down=*/true, guard);
      flag_difference = flag_difference & ~NSEventModifierFlagCapsLock;
    }
    while (true) {
      const NSUInteger current_flag = lowestSetBit(flag_difference);
      if (current_flag == 0) {
        break;
      }
      flag_difference = flag_difference & ~current_flag;
      NSNumber* key_code = [flutter::modifierFlagToKeyCode objectForKey:@(current_flag)];
      FML_DCHECK(key_code != nil) << "Invalid modifier flag 0x" << std::hex << current_flag;
      if (key_code == nil) {
        continue;
      }
      BOOL should_be_pressed = (current_flags_of_interest & current_flag) != 0;
      uint64_t physical_key = GetPhysicalKeyForKeyCode([key_code unsignedShortValue]);
      std::optional<EventType> maybe_event_type =
          _press_tracker.RequireModifierState(physical_key,
                                              /*require_pressed_after=*/should_be_pressed);
      if (maybe_event_type.has_value()) {
        uint64_t logical_key =
            EnsureLogicalKey(physical_key, [current_flag](uint64_t physical_key) {
              NSNumber* key_code = [flutter::modifierFlagToKeyCode objectForKey:@(current_flag)];
              FML_DCHECK(key_code != nil) << "Invalid modifier flag 0x" << std::hex << current_flag;
              return GetLogicalKeyForModifier([key_code unsignedShortValue], physical_key);
            });

        FlutterKeyEvent out_event = {
            .struct_size = sizeof(FlutterKeyEvent),
            .timestamp = timestamp_us,
            .type = ToEmbedderApiType(maybe_event_type.value()),
            .physical = physical_key,
            .logical = logical_key,
            .character = nullptr,
            .synthesized = true,
        };
        SendEventToEngine(&out_event, guard);
        _last_modifier_flags_of_interest = _last_modifier_flags_of_interest ^ current_flag;
      }
    }
  }

 private:
  using GetLogicalKey = std::function<uint64_t(uint64_t physical_key)>;

  uint64_t EnsureLogicalKey(uint64_t physical_key,
                            GetLogicalKey get_logical_key,
                            bool force_update = false) {
    if (force_update) {
      return _logical_key_record.insert_or_assign(physical_key, get_logical_key(physical_key))
          .first->second;
    }
    auto found_entry = _logical_key_record.find(physical_key);
    if (found_entry != _logical_key_record.end()) {
      return found_entry->second;
    }
    uint64_t logical_key = get_logical_key(physical_key);
    _logical_key_record[physical_key] = logical_key;
    return logical_key;
  }

  // Update the pressing state.
  //
  // If `logicalKey` is not 0, `physical_key` is pressed as `logical_key`.
  // Otherwise, `physical_key` is released.
  void UpdateKeyPressedState(uint64_t physical_key, uint64_t logical_key) {
    if (logical_key == 0) {
      _pressing_records.erase(physical_key);
    } else {
      _pressing_records[physical_key] = logical_key;
    }
  }

  void SendEventToEngine(const FlutterKeyEvent* event, FlutterKeyCallbackGuard& guard) {
    if (!event->synthesized) {
      // Send a primary event to the framework, expecting its response.
      uint64_t response_id = guard.ResolveByPending();
      _send_event(event, response_id);
    } else {
      // Send a synthesized key event, never expecting its event result.
      guard.MarkSentSynthesizedEvent();
      _send_event(event, 0);
    }
  }

  // Send a CapsLock down event, then a CapsLock up event.
  //
  // If synthesize_down is true, then both events will be synthesized. Otherwise,
  // the callback will be used as the callback for the down event, which is not
  // synthesized, while the up event will always be synthesized.
  void SendCapsLockTap(double timestamp_us, bool synthesize_down, FlutterKeyCallbackGuard& guard) {
    // MacOS sends a down *or* an up when CapsLock is tapped, alternatively on
    // even taps and odd taps. A CapsLock down or CapsLock up should always be
    // converted to a down *and* an up, and the up should always be a synthesized
    // event, since the FlutterEmbedderKeyResponder will never know when the
    // button is released.
    FlutterKeyEvent flutter_event = {
        .struct_size = sizeof(FlutterKeyEvent),
        .timestamp = timestamp_us,
        .type = kFlutterKeyEventTypeDown,
        .physical = flutter::kCapsLockPhysicalKey,
        .logical = flutter::kCapsLockLogicalKey,
        .character = nullptr,
        .synthesized = synthesize_down,
    };
    SendEventToEngine(&flutter_event, guard);

    flutter_event.type = kFlutterKeyEventTypeUp;
    flutter_event.synthesized = true;
    SendEventToEngine(&flutter_event, guard);
  }

  // The function to send converted events to.
  //
  // Set by the constructor.
  SendEvent _send_event;

  DeriveLogicalKey _derive_logical_key;

  typedef std::unordered_map<uint64_t, uint64_t> LogicalKeyRecord;
  LogicalKeyRecord _logical_key_record;

  PressStateTracker _press_tracker;

  // A map of presessd keys.
  //
  // The keys of the dictionary are physical keys, while the values are the logical keys
  // of the key down event.
  std::unordered_map<uint64_t, uint64_t> _pressing_records;

  // A constant mask for NSEvent.modifierFlags that Flutter synchronizes with.
  //
  // Flutter keeps track of the last |modifierFlags| and compares it with the
  // incoming one. Any bit within |_modifier_flag_of_interest_mask| that is different
  // (except for the one that corresponds to the event key) indicates that an
  // event for this modifier was missed, and Flutter synthesizes an event to make
  // up for the state difference.
  //
  // It is computed by computeModifierFlagOfInterestMask.
  const NSUInteger _modifier_flag_of_interest_mask;

  // The modifier flags of the last received key event, excluding uninterested
  // bits.
  //
  // This should be kept synchronized with the last |NSEvent.modifierFlags|
  // after masking with |_modifier_flag_of_interest_mask|. This should also be kept
  // synchronized with the corresponding keys of |_pressing_records|.
  //
  // This is used by |SynchronizeModifiers| to quickly find
  // out modifier keys that are desynchronized.
  NSUInteger _last_modifier_flags_of_interest = 0;
};

@interface FlutterEmbedderKeyResponder ()
/**
 * Processes the response from the framework.
 */
- (void)handleResponseForId:(uint64_t)responseId withResult:(BOOL)handled;

- (void)sendEvent:(const FlutterKeyEvent&)event responseId:(uint64_t)responseId;
@end

@implementation FlutterEmbedderKeyResponder {
  CommonKeyboard* _common_keyboard;

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
}

@synthesize layoutMap;

- (nonnull instancetype)initWithSendEvent:(FlutterSendEmbedderKeyEvent)sendEvent {
  self = [super init];
  if (self != nil) {
    _sendEventToEngine = sendEvent;
    _nextResponseId = 1;  // Starts at 1; 0 reserved for empty
    _pendingResponses = [NSMutableDictionary dictionary];
    _common_keyboard = new CommonKeyboard(
        /*send_event=*/
        [responder = self](const FlutterKeyEvent* event, uint64_t response_id) {
          [responder sendEvent:*event responseId:response_id];
        },
        /*derive_logical_key=*/
        [responder = self](NSEvent* event, uint64_t physical_key) {
          NSNumber* logicalKeyFromMap = responder.layoutMap[@(event.keyCode)];
          return logicalKeyFromMap != nil ? [logicalKeyFromMap unsignedLongLongValue]
                                          : GetLogicalKeyForEvent(event, physical_key);
        });
  }
  return self;
}

- (void)dealloc {
  delete _common_keyboard;
}

- (void)sendEvent:(const FlutterKeyEvent&)event responseId:(uint64_t)responseId {
  NSAssert(event.synthesized == (responseId == 0), @"event.synthesize is %d but responseId is %llu",
           event.synthesized, responseId);
  if (event.synthesized) {
    _sendEventToEngine(event, nullptr, nullptr);
    return;
  }

  // The `pending` is released in `HandleResponse`.
  FlutterKeyPendingResponse* pending = new FlutterKeyPendingResponse{self, responseId};
  _sendEventToEngine(event, HandleResponse, pending);
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
      _common_keyboard->HandlePressEvent(event, *guarded_callback);
      break;
    case NSEventTypeFlagsChanged:
      _common_keyboard->HandleFlagEvent(event, *guarded_callback);
      break;
    default:
      NSAssert(false, @"Unexpected key event type: |%@|.", @(event.type));
  }
  NSAssert(guarded_callback->resolved(), @"The guard is not resolved");
  // Every handleEvent's callback expects a reply. If the native event generates
  // no primary events, reply it now with "handled".
  if (!guarded_callback->sent_primary_event()) {
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
    [self sendEvent:flutterEvent responseId:0];
    guarded_callback->MarkSentSynthesizedEvent();
  }
  // TODO(dkwingsmt)
  // NSAssert(_lastModifierFlagsOfInterest == (event.modifierFlags & _modifierFlagOfInterestMask),
  //          @"The modifier flags are not properly updated: recorded 0x%lx, event with mask 0x%lx",
  //          _lastModifierFlagsOfInterest, event.modifierFlags & _modifierFlagOfInterestMask);
}

#pragma mark - Private

- (void)handleResponseForId:(uint64_t)responseId withResult:(BOOL)handled {
  FlutterAsyncKeyCallback callback = _pendingResponses[@(responseId)];
  NSAssert(callback != nil, @"Invalid response ID %llu", responseId);
  callback(handled);
  [_pendingResponses removeObjectForKey:@(responseId)];
}

- (void)syncModifiersIfNeeded:(NSEventModifierFlags)modifierFlags
                    timestamp:(NSTimeInterval)timestamp {
  auto guarded_callback =
      std::make_unique<FlutterKeyCallbackGuard>(FlutterKeyCallbackGuard::kDontNeedResponse);
  _common_keyboard->SynchronizeModifiers(modifierFlags,
                                         /*ignoringFlags=*/0, GetFlutterTimestampFrom(timestamp),
                                         *guarded_callback);
  guarded_callback->ResolveWithoutPrimaryEvent();
  guarded_callback->MarkSentSynthesizedEvent();
}

- (nonnull NSDictionary*)getPressedState {
  return ToNSDictionary(_common_keyboard->GetPressedState());
}

- (void)printPressedState {
  printf("Pressed: {");
  for (auto [phy, log] : _common_keyboard->GetPressedState()) {
    printf("0x%llx: 0x%llx, ", phy, log);
  }
  printf("}\n");
  fflush(stdout);
}

@end

namespace {
void HandleResponse(bool handled, void* user_data) {
  // Use unique_ptr to release on leaving.
  auto pending = std::unique_ptr<FlutterKeyPendingResponse>(
      reinterpret_cast<FlutterKeyPendingResponse*>(user_data));
  [pending->responder handleResponseForId:pending->responseId withResult:handled];
}
}  // namespace
