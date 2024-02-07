// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#import <objc/message.h>
#include <functional>
#include <memory>
#include <unordered_map>

#import "FlutterEmbedderKeyResponder.h"
#import "KeyCodeMap_Internal.h"
#import "flutter/shell/platform/common/keyboard_models.h"
#import "flutter/shell/platform/darwin/common/framework/Headers/FlutterCodecs.h"
#import "flutter/shell/platform/darwin/macos/framework/Source/FlutterViewController_Internal.h"
#import "flutter/shell/platform/embedder/embedder.h"

namespace {

using flutter::FlutterKeyCallbackGuard;
using flutter::LockState;

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

static NSUInteger setBitMask(NSUInteger base, NSUInteger mask, bool value) {
  return (base & ~mask) | (value ? mask : 0);
}

/**
 * The C-function sent to the embedder's |SendKeyEvent|, wrapping
 * |FlutterEmbedderKeyResponder.handleResponse|.
 *
 * For the reason of this wrap, see |FlutterKeyPendingResponse|.
 */
void HandleResponse(bool handled, void* user_data);

class KeyEventCallback {
 public:
  KeyEventCallback(FlutterAsyncKeyCallback callback)
      : callback_((__bridge_retained void*)callback) {}

  ~KeyEventCallback() {
    FML_DCHECK(called) << "The callback has not been called.";
    // Transfer the ownership to release it at the end of the destructor.
    FlutterAsyncKeyCallback transferred_callback =
        (__bridge_transfer FlutterAsyncKeyCallback)callback_;
    (void)transferred_callback;  // Suppress unused variable warning
  }

  void Handle(bool handled) {
    FML_DCHECK(!called) << "The callback has been called.";
    called = true;
    FlutterAsyncKeyCallback callback = (__bridge FlutterAsyncKeyCallback)callback_;
    callback(handled);
  }

 private:
  void* callback_;
  bool called = false;
};

}  // namespace

class NativeEventMacos : public flutter::NativeEvent {
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
  void* _native_event = nullptr;
  uint64_t _physical_key = 0;
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

class NativeEventMacosModifierFlag : public flutter::NativeEvent {
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

class NativeEventMacosCapsLock : public flutter::NativeEvent {
 public:
  NativeEventMacosCapsLock(NSTimeInterval timestamp) : _timestamp(timestamp) {}

  uint64_t physical_key() override { return flutter::kCapsLockPhysicalKey; }

  uint64_t logical_key() override { return flutter::kCapsLockLogicalKey; }

  double timestamp() override { return GetFlutterTimestampFrom(_timestamp); }

 private:
  NSTimeInterval _timestamp;
};

@interface FlutterEmbedderKeyResponder ()
@end

@implementation FlutterEmbedderKeyResponder {
  flutter::PressStateTracker* _press_tracker;
  flutter::ModifierStateTracker* _modifier_tracker;
  flutter::LockStateTracker* _lock_tracker;

  FlutterSendEmbedderKeyEvent _sendEventToEngine;

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
    _press_tracker = new flutter::PressStateTracker;
    _modifier_tracker = new flutter::ModifierStateTracker;
    _lock_tracker = new flutter::LockStateTracker;
    _modifierFlagOfInterestMask = computeModifierFlagOfInterestMask();
    _lastModifierFlagsOfInterest = 0;
  }
  return self;
}

- (void)dealloc {
  delete _press_tracker;
  delete _modifier_tracker;
  delete _lock_tracker;
}

- (void)handleEvent:(NSEvent*)event callback:(FlutterAsyncKeyCallback)callback {
  // The conversion algorithm relies on a non-nil callback to properly compute
  // `synthesized`.
  NSAssert(callback != nil, @"The callback must not be nil.");

  FlutterKeyCallbackGuard guard(new KeyEventCallback(callback));

  switch (event.type) {
    case NSEventTypeKeyDown:
    case NSEventTypeKeyUp:
      [self handlePressEvent:event guard:guard];
      break;
    case NSEventTypeFlagsChanged:
      [self handleFlagEvent:event guard:guard];
      break;
    default:
      NSAssert(false, @"Unexpected key event type: |%@|.", @(event.type));
  }
  // Every handleEvent's callback expects a reply. If the native event generates
  // no primary events, reply it now with "handled".
  std::unique_ptr<KeyEventCallback> remaining_callback(
      reinterpret_cast<KeyEventCallback*>(guard.Release()));
  if (remaining_callback != nullptr) {
    remaining_callback->Handle(true);
  }
  // Every native event must send at least one event to satisfy the protocol for
  // event modes. If there are no any events sent, synthesize an empty one here.
  // This will not be needed when the channel mode is no more.
  if (!guard.sent_any_events()) {
    FlutterKeyEvent flutterEvent = {
        .struct_size = sizeof(FlutterKeyEvent),
        .timestamp = 0,
        .type = kFlutterKeyEventTypeDown,
        .physical = 0,
        .logical = 0,
        .character = nil,
        .synthesized = true,
    };
    [self sendEvent:flutterEvent guard:guard];
    guard.MarkSentSynthesizedEvent();
  }
  NSAssert(_lastModifierFlagsOfInterest == (event.modifierFlags & _modifierFlagOfInterestMask),
           @"The modifier flags are not properly updated: recorded 0x%lx, event with mask 0x%lx",
           _lastModifierFlagsOfInterest, event.modifierFlags & _modifierFlagOfInterestMask);
}

- (void)syncModifiersIfNeeded:(NSEventModifierFlags)modifierFlags
                    timestamp:(NSTimeInterval)timestamp {
  FlutterKeyCallbackGuard guard(nullptr);
  [self synchronizeAllModifierFlags:modifierFlags
                      ignoringFlags:0
                          timestamp:timestamp
              isACapslockFlagChange:false
                              guard:guard];
  guard.MarkSentSynthesizedEvent();
}

#pragma mark - Private

- (void)sendEvent:(const FlutterKeyEvent&)event guard:(FlutterKeyCallbackGuard&)guard {
  if (event.synthesized) {
    // Send a synthesized key event, never expecting its event result.
    guard.MarkSentSynthesizedEvent();
    _sendEventToEngine(event, nullptr, nullptr);
  } else {
    void* callback = guard.MarkSentPrimaryEvent();
    _sendEventToEngine(event, HandleResponse, callback);
  }
}

- (void)handlePressEvent:(NSEvent*)event guard:(FlutterKeyCallbackGuard&)guard {
  [self synchronizeAllModifierFlags:event.modifierFlags
                      ignoringFlags:0
                          timestamp:event.timestamp
              isACapslockFlagChange:false
                              guard:guard];

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

- (void)handleFlagEvent:(NSEvent*)event guard:(FlutterKeyCallbackGuard&)guard {
  const uint64_t physical_key = GetPhysicalKeyForKeyCode(event.keyCode);
  if (physical_key == flutter::kCapsLockPhysicalKey) {
    [self synchronizeAllModifierFlags:event.modifierFlags
                        ignoringFlags:0
                            timestamp:event.timestamp
                isACapslockFlagChange:true
                                guard:guard];
    return;
  }

  NativeEventMacosModifier native_event(event);
  NSNumber* targetModifierFlagObj = flutter::keyCodeToModifierFlag[@(event.keyCode)];
  NSUInteger target_modifier_flag =
      targetModifierFlagObj == nil ? 0 : [targetModifierFlagObj unsignedLongValue];
  [self synchronizeAllModifierFlags:event.modifierFlags
                      ignoringFlags:target_modifier_flag
                          timestamp:event.timestamp
              isACapslockFlagChange:false
                              guard:guard];

  std::vector<FlutterKeyEvent> events;
  bool require_pressed_after_primary = event.modifierFlags & target_modifier_flag;
  _modifier_tracker->RequireModifierKeyState(
      &events, native_event,
      /*require_pressed_before_primary=*/std::nullopt,
      /*require_pressed_after_primary=*/require_pressed_after_primary);
  for (FlutterKeyEvent& event : events) {
    [self sendEvent:event guard:guard];
  }
  _lastModifierFlagsOfInterest =
      setBitMask(_lastModifierFlagsOfInterest, target_modifier_flag, require_pressed_after_primary);
}

- (void)synchronizeAllModifierFlags:(NSUInteger)current_flags
                      ignoringFlags:(NSUInteger)ignoring_flags
                          timestamp:(NSTimeInterval)timestamp
              isACapslockFlagChange:(bool)is_a_capslock_flag_change
                              guard:(FlutterKeyCallbackGuard&)guard {
  [self synchronizePressModifierFlags:current_flags
                        ignoringFlags:ignoring_flags
                            timestamp:timestamp
                                guard:guard];
  [self synchronizeCapsLock:current_flags & NSEventModifierFlagCapsLock
                  timestamp:timestamp
      isACapslockFlagChange:is_a_capslock_flag_change
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
- (void)synchronizePressModifierFlags:(NSUInteger)current_flags
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
    _modifier_tracker->RequireModifierKeyState(&events, source_event,
                                               /*require_pressed_before_primary=*/should_be_pressed,
                                               /*require_pressed_after_primary=*/std::nullopt);
    _lastModifierFlagsOfInterest =
        setBitMask(_lastModifierFlagsOfInterest, current_flag, should_be_pressed);
  }
  for (FlutterKeyEvent& event : events) {
    [self sendEvent:event guard:guard];
  }
}

- (void)synchronizeCapsLock:(bool)should_be_on
                  timestamp:(NSTimeInterval)timestamp
      isACapslockFlagChange:(bool)is_a_capslock_flag_change
                      guard:(FlutterKeyCallbackGuard&)guard {
  NativeEventMacosCapsLock native_event(timestamp);
  std::optional<LockState> require_primary_state;
  if (is_a_capslock_flag_change) {
    require_primary_state = should_be_on ? LockState::kPressedOn : LockState::kPressedOff;
  }
  LockState require_after_cleanup = should_be_on ? LockState::kReleasedOn : LockState::kReleasedOff;

  std::vector<FlutterKeyEvent> events;
  _lock_tracker->RequireState(&events, native_event, require_primary_state, require_after_cleanup);
  for (FlutterKeyEvent& event : events) {
    [self sendEvent:event guard:guard];
  }
}

- (nonnull NSDictionary*)getPressedState {
  std::unordered_map<uint64_t, uint64_t> result;
  auto press_state = _press_tracker->GetPressedState();
  result.insert(press_state.begin(), press_state.end());
  auto modifier_state = _modifier_tracker->GetPressedState();
  result.insert(modifier_state.begin(), modifier_state.end());
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
  std::unique_ptr<KeyEventCallback> callback(reinterpret_cast<KeyEventCallback*>(user_data));
  FML_DCHECK(callback != nullptr);
  callback->Handle(handled);
}
}  // namespace
