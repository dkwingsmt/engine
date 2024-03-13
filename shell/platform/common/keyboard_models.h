// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef FLUTTER_SHELL_PLATFORM_COMMON_KEYBOARD_MODELS_H_
#define FLUTTER_SHELL_PLATFORM_COMMON_KEYBOARD_MODELS_H_

#include <functional>
#include <optional>
#include <unordered_map>
#include <vector>

#include "flutter/fml/macros.h"
#include "flutter/shell/platform/embedder/embedder.h"

namespace flutter {

typedef std::list<const FlutterKeyEvent&> EventList;

enum class EventType {
  kUp,
  kDown,
  kRepeat,
};

struct StateChange {
  EventType change;
  bool synthesized;
};

typedef uint8_t LockStateGroup;
enum class LockState : LockStateGroup {
  kReleasedOff = 0x1,
  kPressedOn = 0x2,
  kReleasedOn = 0x4,
  kPressedOff = 0x8
};

constexpr LockStateGroup Exactly(LockState a) {
  return static_cast<LockStateGroup>(a);
}

constexpr LockStateGroup AnyOf(LockState a, LockState b) {
  return static_cast<LockStateGroup>(a) | static_cast<LockStateGroup>(b);
}

constexpr LockStateGroup AnyOf(LockState a, LockState b, LockState c) {
  return static_cast<LockStateGroup>(a) | static_cast<LockStateGroup>(b) |
         static_cast<LockStateGroup>(c);
}

LockState Previous(LockState target);
LockState Next(LockState target);
LockStateGroup Next(LockStateGroup target);

FlutterKeyEventType ToEmbedderApiType(EventType type);

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
  using SendKeyEvent =
      std::function<void(const FlutterKeyEvent& event, void* response_context)>;

  FlutterKeyCallbackGuard();

  ~FlutterKeyCallbackGuard();

  EventList* GetEventList(bool is_target_event);

  // Returns true if any events have been sent.
  template <typename T>
  bool SendEvent(SendKeyEvent send_event, std::unique_ptr<T>& response_context) {
    GetResponseContext get_response_context = [&response_context]() mutable {
      return reinterpret_cast<void*>(response_context.release());
    };
    return SendEventImpl(std::move(send_event), std::move(get_response_context));
  }

 private:
  using GetResponseContext = std::function<void*()>;

  void SendEventImpl(SendKeyEvent send_event, GetResponseContext get_response_context);

  EventList target_key_events_;
  EventList other_key_events_;

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

class PhysicallyIndexed {
 protected:
  uint64_t EnsureLogicalKey(NativeEvent& native_event,
                            bool force_update = false);

  std::unordered_map<uint64_t, uint64_t> physical_to_logical_;
};

class LogicallyIndexed {
 protected:
  typedef std::unordered_map<uint64_t, uint64_t> Mapping;

  uint64_t EnsurePhysicalKey(NativeEvent& native_event,
                             bool force_update = false);

  Mapping logical_to_physical_;
};

// PressStateTracker handles keys that have two states, pressed or not, and that
// can be uniquely indexed by their physical keys.
//
// The corresponding logical key is allowed to change at each down event. Some
// methods of also allows "repeat" events.
class PressStateTracker : private PhysicallyIndexed {
 public:
  std::unordered_map<uint64_t, uint64_t> GetPressedState();

  void RequireTextKeyState(EventList* output,
                           NativeEvent& source_event,
                           std::optional<bool> require_pressed_before,
                           bool require_pressed_after);

 private:
  typedef std::unordered_map<uint64_t, bool> StateMap;

  std::vector<EventType> ModelTextKeyEvent(
      uint64_t physical_key,
      std::optional<bool> require_pressed_before,
      bool require_pressed_after);

  StateMap pressed_keys_;

  // Ensure key state requirement by optionally pushing an event to `output` and
  // changing the value of `current` accordingly.
  static std::optional<EventType> EnsurePressed(StateMap::iterator current,
                                                bool require_pressed);
};

class ModifierStateTracker : private LogicallyIndexed {
 public:
  std::unordered_map<uint64_t, uint64_t> GetPressedState();

  void RequireModifierKeyState(
      EventList* output,
      NativeEvent& source_event,
      std::optional<bool> require_pressed_before_primary,
      std::optional<bool> require_pressed_after_primary);

 private:
  typedef std::unordered_map<uint64_t, bool> StateMap;

  void ModelModifierState(std::vector<StateChange>* output,
                          uint64_t physical_key,
                          bool require_pressed_after,
                          bool synthesized);

  StateMap pressed_keys_;

  // Ensure key state requirement by optionally pushing an event to `output` and
  // changing the value of `current` accordingly.
  static std::optional<EventType> EnsurePressed(StateMap::iterator current,
                                                bool require_pressed);
};

class ModifierPairStateTracker : private LogicallyIndexed {
 public:
  void Register(uint64_t flag, uint64_t logical_key_1, uint64_t logical_key_2);

  std::unordered_map<uint64_t, uint64_t> GetPressedState();

  void RequireEventState(EventList* output,
                         NativeEvent& source_event,
                         std::optional<bool> require_pressed_before,
                         std::optional<bool> require_pressed_after);

  void RequireEventFlagState(EventList* output,
                         NativeEvent& source_event,
                         uint64_t flag,
                         std::optional<bool> require_pressed_before,
                         std::optional<bool> require_pressed_after);

  void RequireFlagState(EventList* output,
                        uint64_t flag,
                        double timestamp,
                        bool require_pressed);

 private:
  typedef std::pair<uint64_t, uint64_t> LogicalKeyPair;
  typedef std::unordered_map<uint64_t, bool> StateMap;

  uint64_t PhysicalKeyFromRecord(uint64_t logical_key);

  void RequireKeyState(EventList* output,
                       StateMap::iterator current,
                       double timestamp,
                       bool require_pressed);

  std::unordered_map<uint64_t, LogicalKeyPair> flag_to_logical_keys_;
  std::unordered_map<uint64_t, uint64_t> logical_key_to_flag_;
  StateMap logical_key_pressed_;

  // Ensure key state requirement by optionally pushing an event to `output` and
  // changing the value of `current` accordingly.
  static std::optional<StateChange> EnsurePressed(StateMap::iterator current,
                                                  bool require_pressed,
                                                  bool synthesized);
};

// LockStateTracker handles keys that have four states, pressed or not, lock on
// or off. These keys must be uniquely indexed by their logical keys.
class LockStateTracker : private LogicallyIndexed {
 public:
  void RequireState(EventList* output,
                    NativeEvent& source_event,
                    std::optional<LockStateGroup> require_primary_state,
                    LockStateGroup require_after_cleanup);

  void RequireState(EventList* output,
                    uint64_t logical_key,
                    double timestamp,
                    LockStateGroup require_state);

  std::unordered_map<uint64_t, uint64_t> GetPressedState();

  LockState GetState(uint64_t logical_key);

 private:
  typedef std::unordered_map<uint64_t, LockState> StateMap;

  uint64_t PhysicalKeyFromRecord(uint64_t logical_key);

  StateMap states_;

  // Ensure key state requirement by optionally pushing an event to `output` and
  // changing the value of `current` accordingly.
  static void EnsureLockState(std::vector<StateChange>* output,
                              StateMap::iterator& current,
                              LockStateGroup required_state,
                              bool synthesized);

  static EventType ProgressState(StateMap::iterator& current);
};

}  // namespace flutter

#endif  // FLUTTER_SHELL_PLATFORM_COMMON_KEYBOARD_MODELS_H_
