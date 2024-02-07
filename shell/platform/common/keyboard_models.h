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

enum class EventType {
  kUp,
  kDown,
  kRepeat,
};

struct StateChange {
  EventType change;
  bool synthesized;
};

enum class LockState { kReleasedOff, kPressedOn, kReleasedOn, kPressedOff };

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
  FlutterKeyCallbackGuard(void* content);

  ~FlutterKeyCallbackGuard();

  /**
   * Mark that this guard has been used to send a primary event a return the
   * stored response ID.
   */
  [[nodiscard]] void* MarkSentPrimaryEvent();

  void MarkSentSynthesizedEvent();

  [[nodiscard]] void* Release();

  bool sent_any_events() const {
    return sent_primary_event_ || sent_synthesized_events_;
  }

 private:
  void* content_;
  bool sent_primary_event_ = false;
  bool sent_synthesized_events_ = false;

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

  void RequireTextKeyState(std::vector<FlutterKeyEvent>* output,
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
      std::vector<FlutterKeyEvent>* output,
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

  void RequireEventState(
      std::vector<FlutterKeyEvent>* output,
      NativeEvent& source_event,
      std::optional<bool> require_pressed_before,
      std::optional<bool> require_pressed_after);

  void RequireFlagState(
      std::vector<FlutterKeyEvent>* output,
      uint64_t flag,
      double timestamp,
      bool require_pressed);

 private:
  typedef std::pair<uint64_t, uint64_t> LogicalKeyPair;
  typedef std::unordered_map<uint64_t, bool> StateMap;

  uint64_t PhysicalKeyFromRecord(uint64_t logical_key);

  void RequireKeyState(std::vector<FlutterKeyEvent>* output,
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

  static uint64_t FabricatePhysicalKey(uint64_t logical_key);
};


// LockStateTracker handles keys that have four states, pressed or not, lock on
// or off. These keys must be uniquely indexed by their logical keys.
class LockStateTracker : private LogicallyIndexed {
 public:
  void RequireState(std::vector<FlutterKeyEvent>* output,
                    NativeEvent& source_event,
                    std::optional<LockState> require_primary_state,
                    LockState require_after_cleanup);

  std::unordered_map<uint64_t, uint64_t> GetPressedState();

 private:
  typedef std::unordered_map<uint64_t, LockState> StateMap;

  std::vector<StateChange> ModelState(
      uint64_t logical_key,
      std::optional<LockState> require_primary_state,
      LockState require_after_cleanup);

  StateMap states_;

  // Ensure key state requirement by optionally pushing an event to `output` and
  // changing the value of `current` accordingly.
  static void EnsureLockState(std::vector<StateChange>* output,
                              StateMap::iterator& current,
                              LockState required_state,
                              bool synthesized);

  static EventType ProgressState(StateMap::iterator& current);
};

}  // namespace flutter

#endif  // FLUTTER_SHELL_PLATFORM_COMMON_KEYBOARD_MODELS_H_
