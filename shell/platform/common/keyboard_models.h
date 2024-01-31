// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef FLUTTER_SHELL_PLATFORM_COMMON_KEYBOARD_MODELS_H_
#define FLUTTER_SHELL_PLATFORM_COMMON_KEYBOARD_MODELS_H_

#include <vector>
#include <unordered_map>

#include "flutter/shell/platform/embedder/embedder.h"
#include "flutter/fml/macros.h"

namespace flutter {

enum class EventType {
  kUp,
  kDown,
  kRepeat,
};

enum class LockState { kReleasedOff, kPressedOn, kReleasedOn, kPressedOff };

FlutterKeyEventType ToEmbedderApiType(EventType type);

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
  uint64_t EnsureLogicalKey(NativeEvent& native_event, bool force_update = false);

  std::unordered_map<uint64_t, uint64_t> _physical_to_logical;
};

class LogicallyIndexed {
 protected:
  uint64_t EnsurePhysicalKey(NativeEvent& native_event, bool force_update = false);

  std::unordered_map<uint64_t, uint64_t> _logical_to_physical;
};

// PressStateTracker handles keys that have two states, pressed or not, and that
// can be uniquely indexed by their physical keys.
//
// The corresponding logical key is allowed to change at each down event. Some
// methods of also allows "repeat" events.
class PressStateTracker : private PhysicallyIndexed {
 public:
  std::unordered_map<uint64_t, uint64_t> GetPressedState();

  void RequireTextKeyState(std::vector<FlutterKeyEvent>* output,
                           NativeEvent& source_event,
                           std::optional<bool> require_pressed_before,
                           bool require_pressed_after);

  void RequireModifierKeyState(std::vector<FlutterKeyEvent>* output,
                               NativeEvent& source_event,
                               std::optional<bool> require_pressed_before_primary,
                               std::optional<bool> require_pressed_after_primary);

 private:
  typedef std::unordered_map<uint64_t, bool> StateMap;

  struct ModifierStateChange {
    EventType type;
    bool synthesized;
  };

  std::vector<EventType> ModelTextKeyEvent(uint64_t physical_key,
                                           std::optional<bool> require_pressed_before,
                                           bool require_pressed_after);

  void ModelModifierState(std::vector<ModifierStateChange>* output,
                          uint64_t physical_key,
                          bool require_pressed_after,
                          bool synthesized);

  StateMap _pressed_keys;

  // Ensure key state requirement by optionally pushing an event to `output` and
  // changing the value of `current` accordingly.
  static std::optional<EventType> EnsurePressed(StateMap::iterator current, bool require_pressed);
};

// LockStateTracker handles keys that have four states, pressed or not, lock on
// or off. These keys must be uniquely indexed by their logical keys.
class LockStateTracker : protected LogicallyIndexed {
 public:
  void RequireState(std::vector<FlutterKeyEvent>* output,
                    NativeEvent& source_event,
                    std::optional<LockState> require_primary_state,
                    LockState require_after_cleanup);

  std::unordered_map<uint64_t, uint64_t> GetPressedState();

 private:
  struct StateChange {
    EventType change;
    bool synthesized;
  };

  typedef std::unordered_map<uint64_t, LockState> StateMap;

  std::vector<StateChange> ModelState(uint64_t logical_key,
                                      std::optional<LockState> require_primary_state,
                                      LockState require_after_cleanup);

  StateMap _states;

  // Ensure key state requirement by optionally pushing an event to `output` and
  // changing the value of `current` accordingly.
  static void EnsureLockState(std::vector<StateChange>* output,
                              StateMap::iterator& current,
                              LockState required_state,
                              bool synthesized);

  static EventType ProgressState(StateMap::iterator& current);
};

}  // namespace flutter

#endif // FLUTTER_SHELL_PLATFORM_COMMON_KEYBOARD_MODELS_H_
