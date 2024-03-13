// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "flutter/shell/platform/linux/fl_key_embedder_responder.h"

#include <gtk/gtk.h>
#include <cinttypes>

#include "flutter/shell/platform/common/keyboard_models.h"
#include "flutter/shell/platform/embedder/embedder.h"
#include "flutter/shell/platform/linux/fl_key_embedder_responder_private.h"
#include "flutter/shell/platform/linux/key_mapping.h"
#include "fml/logging.h"

constexpr uint64_t kMicrosecondsPerMillisecond = 1000;

static const FlutterKeyEvent kEmptyEvent{
    .struct_size = sizeof(FlutterKeyEvent),
    .timestamp = 0,
    .type = kFlutterKeyEventTypeDown,
    .physical = 0,
    .logical = 0,
    .character = nullptr,
    .synthesized = true,
};

// Look up a hash table that maps a uint64_t to a uint64_t.
//
// Returns std::nullopt if not found.
static std::optional<uint64_t> lookup_hash_table(
    const std::unordered_map<uint64_t, uint64_t>& table,
    uint64_t key) {
  auto found = table.find(key);
  if (found == table.cend()) {
    return std::nullopt;
  }
  return found->second;
}

// Look up a hash table that maps a uint64_t to a uint64_t; given its value,
// find its key.
//
// Returns 0 if not found.
static std::optional<uint64_t> reverse_lookup_hash_table(
    const std::unordered_map<uint64_t, uint64_t>& table,
    uint64_t target_value) {
  for (auto [key, value] : table) {
    if (value == target_value) {
      return key;
    }
  }
  return std::nullopt;
}

static GHashTable* ToGHashTable(
    const std::unordered_map<uint64_t, uint64_t>& source) {
  GHashTable* result = g_hash_table_new(g_direct_hash, g_direct_equal);
  for (auto [key, value] : source) {
    g_hash_table_insert(result, uint64_to_gpointer(key),
                        uint64_to_gpointer(value));
  }
  return result;
}

static uint64_t to_lower(uint64_t n) {
  constexpr uint64_t lower_a = 0x61;
  constexpr uint64_t upper_a = 0x41;
  constexpr uint64_t upper_z = 0x5a;

  constexpr uint64_t lower_a_grave = 0xe0;
  constexpr uint64_t upper_a_grave = 0xc0;
  constexpr uint64_t upper_thorn = 0xde;
  constexpr uint64_t division = 0xf7;

  // ASCII range.
  if (n >= upper_a && n <= upper_z) {
    return n - upper_a + lower_a;
  }

  // EASCII range.
  if (n >= upper_a_grave && n <= upper_thorn && n != division) {
    return n - upper_a_grave + lower_a_grave;
  }

  return n;
}

namespace {

using flutter::FlutterKeyCallbackGuard;
using flutter::LockState;
using flutter::LockStateGroup;

typedef enum {
  kStateLogicUndecided,
  kStateLogicNormal,
  kStateLogicReversed,
} StateLogicInferrence;

// Revert-map a logical_key_to_lock_bit table to a logical_key_to_lock_bit
// table.
static std::map<uint64_t, uint64_t> initialize_logical_key_to_lock_bit(
    const CheckedKeysMap& lock_bit_to_checked_keys) {
  std::map<uint64_t, uint64_t> logical_to_lock_bit;
  for (const auto& [lock_bit, checked_key] : lock_bit_to_checked_keys) {
    logical_to_lock_bit[checked_key.primary_logical_key] = lock_bit;
  }
  return logical_to_lock_bit;
}

static uint64_t apply_id_plane(uint64_t logical_id, uint64_t plane) {
  return (logical_id & kValueMask) | plane;
}

static uint64_t event_to_physical_key(const FlKeyEvent* event) {
  auto found = xkb_to_physical_key_map.find(event->keycode);
  if (found != xkb_to_physical_key_map.end()) {
    return found->second;
  }
  return apply_id_plane(event->keycode, kGtkPlane);
}

static uint64_t event_to_logical_key(const FlKeyEvent* event) {
  guint keyval = event->keyval;
  auto found = gtk_keyval_to_logical_key_map.find(keyval);
  if (found != gtk_keyval_to_logical_key_map.end()) {
    return found->second;
  }
  // EASCII range
  if (keyval < 256) {
    return apply_id_plane(to_lower(keyval), kUnicodePlane);
  }
  // Auto-generate key
  return apply_id_plane(keyval, kGtkPlane);
}

static uint64_t event_to_timestamp(const FlKeyEvent* event) {
  return kMicrosecondsPerMillisecond * static_cast<double>(event->time);
}

// Returns a newly accocated UTF-8 string from event->keyval that must be
// freed later with g_free().
static char* event_to_character(const FlKeyEvent* event) {
  gunichar unicodeChar = gdk_keyval_to_unicode(event->keyval);
  glong items_written;
  gchar* result = g_ucs4_to_utf8(&unicodeChar, 1, NULL, &items_written, NULL);
  if (items_written == 0) {
    if (result != NULL) {
      g_free(result);
    }
    return nullptr;
  }
  return result;
}

// Try to infer the precise lock state right before an event for the target key
// of the event.
//
// If this function returns std::nullopt, then the key's state before the event
// can't be precisely derived. The caller must infer using `event_is_press`
// (`event_state_is_on` is unreliable).
static std::optional<LockState> lock_state_by_self_event(
    bool event_is_press,
    bool event_state_is_on,
    std::optional<bool> reverse_state_logic) {
  if (reverse_state_logic == std::nullopt) {
    return std::nullopt;
  }
  if (!event_state_is_on) {
    // Out of the 4 lock states, only one has 0 for the event state, which can
    // be used to precisely infer.
    return reverse_state_logic.value() ? LockState::kReleasedOn
                                       : LockState::kReleasedOff;
  }
  if (event_is_press) {
    // With the previous case out of the way, we can also precisely infer for
    // this case.
    return reverse_state_logic.value() ? LockState::kReleasedOff
                                       : LockState::kReleasedOn;
  }
  return std::nullopt;
}

// Try to infer the possible lock states right before an event for the target
// key of the event.
//
// Different from `lock_state_by_self_event`, this function always returns one
// or a few lock states. In the case where `lock_state_by_self_event` might
// return std::nullopt, this function use event_is_press to infer something.
static LockStateGroup possible_lock_states_by_self_event(
    bool event_is_press,
    bool event_state_is_on,
    std::optional<bool> reverse_state_logic) {
  std::optional<LockState> precise_state = lock_state_by_self_event(
      event_is_press, event_state_is_on, reverse_state_logic);
  if (precise_state != std::nullopt) {
    return Exactly(precise_state.value());
  }
  if (event_is_press) {
    return AnyOf(LockState::kReleasedOff, LockState::kReleasedOn);
  } else {
    return AnyOf(LockState::kPressedOff, LockState::kPressedOn);
  }
}

// Infer the lock state during an event for a non-target key of the event.
//
// The `reverse_state_logic` is required. If the reverse state logic has not
// been decided yet, then the CapsLock state can not be synchronized.
static LockStateGroup lock_state_by_others_event(bool event_state_is_on,
                                                 bool reverse_state_logic) {
  if (event_state_is_on) {
    return reverse_state_logic
               ? flutter::AnyOf(LockState::kReleasedOff, LockState::kPressedOn,
                                LockState::kPressedOff)
               : flutter::AnyOf(LockState::kReleasedOn, LockState::kPressedOn,
                                LockState::kPressedOff);
  } else {
    return Exactly(reverse_state_logic ? LockState::kReleasedOn
                                       : LockState::kReleasedOff);
  }
}

static uint64_t corrected_modifier_physical_key(
    const CheckedKeysMap& modifier_bit_to_checked_keys,
    uint64_t physical_key_from_event,
    uint64_t logical_key) {
  // Check if the physical key is one of the known modifier physical key.
  bool known_modifier_physical_key = false;
  for (auto& [modifier_bit, checked_key] : modifier_bit_to_checked_keys) {
    if (checked_key.primary_physical_key == physical_key_from_event) {
      known_modifier_physical_key = true;
    }
  }

  uint64_t corrected_physical_key = physical_key_from_event;
  // If the physical key matches a known modifier key, find the modifier
  // physical key from the logical key.
  if (known_modifier_physical_key) {
    for (auto& [modifier_bit, checked_key] : modifier_bit_to_checked_keys) {
      if (checked_key.primary_logical_key == logical_key ||
          checked_key.secondary_logical_key == logical_key) {
        corrected_physical_key = checked_key.primary_physical_key;
      }
    }
  }

  return corrected_physical_key;
}

class KeyEventCallback {
 public:
  KeyEventCallback(FlKeyResponderAsyncCallback callback, gpointer user_data)
      : callback_(callback), user_data_(user_data) {}

  ~KeyEventCallback() {
    FML_DCHECK(called) << "The callback has not been called.";
  }

  void Handle(bool handled) {
    FML_DCHECK(!called) << "The callback has been called.";
    called = true;
    callback_(handled, user_data_);
  }

  // Handles a response from the embedder API to a key event sent to the
  // framework earlier.
  static void HandleResponse(bool handled, void* user_data) {
    std::unique_ptr<KeyEventCallback> self(
        reinterpret_cast<KeyEventCallback*>(user_data));
    FML_DCHECK(self != nullptr);
    self->Handle(handled);
  }

 private:
  FlKeyResponderAsyncCallback callback_;
  gpointer user_data_;
  bool called = false;
};

class NativeEventLinux : public flutter::NativeEvent {
 public:
  NativeEventLinux(FlKeyEvent* event, uint64_t specified_logical_key)
      : event_(event),
        logical_key_(specified_logical_key != 0 ? specified_logical_key
                                                : event_to_logical_key(event)) {
  }

  ~NativeEventLinux() {
    if (character_ != nullptr) {
      g_free(character_);
    }
    fl_key_event_dispose(event_);
  }

  bool is_press() { return event_->is_press; }

  uint64_t physical_key() override { return event_to_physical_key(event_); }

  uint64_t logical_key() override { return logical_key_; }

  double timestamp() override { return event_to_timestamp(event_); }

  const char* character() override {
    if (character_ == nullptr) {
      character_ = event_to_character(event_);
    }
    return character_;
  }

 protected:
  FlKeyEvent* event_;
  const uint64_t logical_key_;
  char* character_;
};

class EmbedderResponder {
 public:
  EmbedderResponder(EmbedderSendKeyEvent send_key_event,
                    void* send_key_event_user_data)
      : _send_key_event(send_key_event),
        _send_key_event_user_data(send_key_event_user_data),
        _modifier_bit_to_checked_keys(
            initialize_modifier_bit_to_checked_keys()),
        _lock_bit_to_checked_keys(initialize_lock_bit_to_checked_keys()),
        _logical_key_to_lock_bit(
            initialize_logical_key_to_lock_bit(_lock_bit_to_checked_keys)) {
    _lock_records = 0;

    for (auto& [modifier_bit, checked_key] : _modifier_bit_to_checked_keys) {
      modifier_state_tracker_.Register(modifier_bit,
                                       checked_key.primary_logical_key,
                                       checked_key.secondary_logical_key);
      logical_key_to_flag_[checked_key.primary_logical_key] = modifier_bit;
      logical_key_to_flag_[checked_key.secondary_logical_key] = modifier_bit;
    }
  }

  ~EmbedderResponder() {}

  void SyncModifiersIfNeeded(guint state,
                             double event_time,
                             FlutterKeyCallbackGuard& guard) {
    const double timestamp = event_time * kMicrosecondsPerMillisecond;

    // Update pressing states.
    for (auto& [modifier_bit, checked_key] : _modifier_bit_to_checked_keys) {
      SynchronizePressedState(modifier_bit, state, timestamp, &checked_key,
                              guard);
    }
  }

  void SyncModifiers(guint state, NativeEventLinux) {
    std::vector<FlutterKeyEvent> output_target_key;
    std::vector<FlutterKeyEvent> output_other_keys;

    NativeEventLinux source_event(event, specified_logical_key);
    const uint64_t logical_key = source_event.logical_key();

    bool target_key_found = false;
    for (auto& [lock_bit, checked_key] : _lock_bit_to_checked_keys) {
      uint64_t target_logical_key = checked_key.primary_logical_key;
      const bool is_for_this_event =
          target_logical_key == source_event.logical_key();
      target_key_found = target_key_found || is_for_this_event;
      SynchronizeLock(
          is_for_this_event ? &output_target_key : &output_other_keys,
          source_event, event->state & lock_bit, target_logical_key,
          checked_key.is_caps_lock, guard);
    }

    const uint64_t physical_key_from_event = event_to_physical_key(event);
    const uint64_t physical_key = corrected_modifier_physical_key(
        _modifier_bit_to_checked_keys, physical_key_from_event, logical_key);
    const bool is_down_event = event->is_press;

    // Update pressing states
    for (auto& [modifier_bit, checked_key] : _modifier_bit_to_checked_keys) {
      uint64_t target_logical_key = checked_key.primary_logical_key;
      const bool is_for_this_event =
          target_logical_key == source_event.logical_key();
      target_key_found = target_key_found || is_for_this_event;
      bool event_state_is_on = event->state & modifier_bit;
      if (is_for_this_event) {
        modifier_state_tracker_.RequireEventState(
            &output_target_key, source_event,
            /*require_pressed_before=*/std::nullopt,
            /*require_pressed_after=*/event_state_is_on);
      }
      modifier_state_tracker_.RequireFlagState(&output_other_keys, modifier_bit,
                                               source_event.timestamp(),
                                               event_state_is_on);
    }
  }


  void HandleEvent(FlKeyEvent* event,
                   uint64_t specified_logical_key,
                   FlKeyResponderAsyncCallback callback,
                   gpointer user_data) {
    FlutterKeyCallbackGuard guard(new KeyEventCallback(callback, user_data));
    HandleEventImpl(event, specified_logical_key, guard);
    // Every handleEvent's callback expects a reply. If the native event
    // generates no primary events, reply it now with "handled".
    std::unique_ptr<KeyEventCallback> remaining_callback(
        reinterpret_cast<KeyEventCallback*>(guard.Release()));
    if (remaining_callback != nullptr) {
      remaining_callback->Handle(true);
    }
    // Every native event must send at least one event to satisfy the protocol
    // for event modes. If there are no any events sent, synthesize an empty one
    // here. This will not be needed when the channel mode is no more.
    if (!guard.sent_any_events()) {
      SendEvent(&kEmptyEvent, guard);
    }
  }

  std::unordered_map<uint64_t, uint64_t> GetPressedState() {
    return _pressing_records;
  }

 private:
  void SendEvent(const FlutterKeyEvent* event, FlutterKeyCallbackGuard& guard) {
    if (event->synthesized) {
      // Send a synthesized key event, never expecting its event result.
      guard.MarkSentSynthesizedEvent();
      _send_key_event(event, nullptr, nullptr, _send_key_event_user_data);
    } else {
      void* callback = guard.MarkSentPrimaryEvent();
      _send_key_event(event, KeyEventCallback::HandleResponse, callback,
                      _send_key_event_user_data);
    }
  }

  void UpdateMappingRecord(uint64_t physical_key, uint64_t logical_key) {
    _mapping_records[logical_key] = physical_key;
  }

  void SynchronizeLock(std::vector<FlutterKeyEvent>* output,
                       NativeEventLinux& source_event,
                       uint64_t target_logical_key,
                       bool target_is_caps_lock,
                       bool event_state_is_on,
                       FlutterKeyCallbackGuard& guard) {
    const uint64_t event_logical_key = source_event.logical_key();
    const bool is_for_this_event = target_logical_key == event_logical_key;
    if (is_for_this_event && target_is_caps_lock) {
      UpdateCapsLockStateLogicInferrence(
          source_event.is_press(), event_state_is_on,
          lock_tracker_.GetState(target_logical_key));
    }
    std::optional<bool> reverse_logic =
        target_is_caps_lock ? _caps_lock_state_logic_reversed : false;
    if (is_for_this_event) {
      LockStateGroup state_before_event = possible_lock_states_by_self_event(
          source_event.is_press(), event_state_is_on, reverse_logic);
      LockStateGroup state_after_event = flutter::Next(state_before_event);
      lock_tracker_.RequireState(output, source_event,
                                 /*require_primary_state=*/state_after_event,
                                 /*require_after_cleanup=*/state_after_event);
    } else {
      if (reverse_logic.has_value()) {
        LockStateGroup possible_states = lock_state_by_others_event(
            event_state_is_on, reverse_logic.value());
        lock_tracker_.RequireState(output, target_logical_key,
                                   source_event.timestamp(), possible_states);
      }
    }
  }

  void HandleEventImpl(FlKeyEvent* event,
                       uint64_t specified_logical_key,
                       FlutterKeyCallbackGuard& guard) {
    FML_DCHECK(event != nullptr);

    std::vector<FlutterKeyEvent> output_target_key;
    std::vector<FlutterKeyEvent> output_other_keys;

    NativeEventLinux source_event(event, specified_logical_key);
    const uint64_t logical_key = source_event.logical_key();

    bool target_key_found = false;
    for (auto& [lock_bit, checked_key] : _lock_bit_to_checked_keys) {
      uint64_t target_logical_key = checked_key.primary_logical_key;
      const bool is_for_this_event =
          target_logical_key == source_event.logical_key();
      target_key_found = target_key_found || is_for_this_event;
      SynchronizeLock(
          is_for_this_event ? &output_target_key : &output_other_keys,
          source_event, event->state & lock_bit, target_logical_key,
          checked_key.is_caps_lock, guard);
    }

    const uint64_t physical_key_from_event = event_to_physical_key(event);
    const uint64_t physical_key = corrected_modifier_physical_key(
        _modifier_bit_to_checked_keys, physical_key_from_event, logical_key);
    const bool is_down_event = event->is_press;

    // Update pressing states
    for (auto& [modifier_bit, checked_key] : _modifier_bit_to_checked_keys) {
      uint64_t target_logical_key = checked_key.primary_logical_key;
      const bool is_for_this_event =
          target_logical_key == source_event.logical_key();
      target_key_found = target_key_found || is_for_this_event;
      bool event_state_is_on = event->state & modifier_bit;
      if (is_for_this_event) {
        modifier_state_tracker_.RequireEventState(
            &output_target_key, source_event,
            /*require_pressed_before=*/std::nullopt,
            /*require_pressed_after=*/event_state_is_on);
      }
      modifier_state_tracker_.RequireFlagState(&output_other_keys, modifier_bit,
                                               source_event.timestamp(),
                                               event_state_is_on);
    }

    if (!target_key_found) {
      press_state_tracker_.RequireTextKeyState(
          &output_target_key, source_event,
          /*require_pressed_before=*/std::nullopt,
          /*require_pressed_after=*/event.is_press());
    }

    for (FlutterKeyEvent& event : out_other_keys) {
      [self sendEvent:event guard:guard];
    }
    for (FlutterKeyEvent& event : out_target_key) {
      [self sendEvent:event guard:guard];
    }
  }

  // Infer the logic type of CapsLock on the current platform if applicable.
  //
  // In most cases, when a lock key is pressed or released, its event has the
  // key's state as 0-1-1-1 for the 4 stages (as documented in
  // #synchronize_lock_states_loop_body) respectively. But in very rare cases it
  // produces 1-1-0-1, which we call "reversed state logic". This is observed
  // when using Chrome Remote Desktop on macOS (likely a bug).
  //
  // To detect whether the current platform behaves normally or reversed, this
  // function is called on the first down event of CapsLock before calculating
  // stages. This function then store the inferred mode as
  // self->_caps_lock_state_logic_reversed.
  //
  // This does not help if the same app session is used alternatively between a
  // reversed platform and a normal platform. But this is the best we can do.
  void UpdateCapsLockStateLogicInferrence(bool event_is_press,
                                          bool event_state_is_on,
                                          LockState state_by_record) {
    if (_caps_lock_state_logic_reversed != std::nullopt) {
      return;
    }
    std::optional<LockState> state_by_event = lock_state_by_self_event(
        event_is_press, event_state_is_on, /*reverse_state_logic=*/false);
    // Not in a state that can be inferred.
    if (state_by_event == std::nullopt) {
      return;
    }
    if (state_by_event == state_by_record) {
      _caps_lock_state_logic_reversed = false;
    } else if ((state_by_event.value() == LockState::kReleasedOff &&
                state_by_record == LockState::kReleasedOn) ||
               (state_by_event.value() == LockState::kReleasedOn &&
                state_by_record == LockState::kReleasedOff)) {
      _caps_lock_state_logic_reversed = true;
    } else {
      // This shouldn't happen according to our theory. Let's just not update
      // the inferrence.
    }
  }

  // Sends a synthesized event to the framework with no demand for callback.
  void SynthesizeSimpleEvent(FlutterKeyEventType type,
                             uint64_t physical,
                             uint64_t logical,
                             double timestamp,
                             FlutterKeyCallbackGuard& guard) {
    FlutterKeyEvent out_event;
    out_event.struct_size = sizeof(out_event);
    out_event.timestamp = timestamp;
    out_event.type = type;
    out_event.physical = physical;
    out_event.logical = logical;
    out_event.character = nullptr;
    out_event.synthesized = true;
    SendEvent(&out_event, guard);
  }

  // void SynchronizeLockState(guint modifier_bit,
  //                           guint state,
  //                           uint64_t event_logical_key,
  //                           bool is_down,
  //                           double timestamp,
  //                           FlKeyEmbedderCheckedKey* checked_key,
  //                           FlutterKeyCallbackGuard& guard) {
  //   const uint64_t logical_key = checked_key->primary_logical_key;
  //   const std::optional<uint64_t> recorded_physical_key =
  //       lookup_hash_table(_mapping_records, logical_key);
  //   // The physical key is derived from past mapping record if possible.
  //   //
  //   // If the event to be synthesized is a key up event, then there must have
  //   // been a key down event before, which has updated the mapping record.
  //   // If the event to be synthesized is a key down event, then there might
  //   // not have been a mapping record, in which case the hard-coded
  //   // #primary_physical_key is used.
  //   const uint64_t physical_key =
  //       recorded_physical_key.value_or(checked_key->primary_physical_key);

  //   // A lock mode key can be at any of a 4-stage cycle, depending on whether
  //   // it's pressed and enabled. The following table lists the definition of
  //   // each stage (TruePressed and TrueEnabled), the event of the lock key
  //   // between every 2 stages (SelfType and SelfState), and the event of
  //   other
  //   // keys at each stage (OthersState). On certain platforms SelfState uses
  //   a
  //   // reversed rule for certain keys (SelfState(rvsd), as documented in
  //   // #UpdateCapsLockStateLogicInferrence).
  //   //
  //   //               #    [0]         [1]          [2]           [3]
  //   //     TruePressed: Released    Pressed      Released      Pressed
  //   //     TrueEnabled: Disabled    Enabled      Enabled       Disabled
  //   //        SelfType:         Down         Up           Down            Up
  //   //       SelfState:          0           1             1              1
  //   // SelfState(rvsd):          1           1             0              1
  //   //     OthersState:    0           1            1              1
  //   //
  //   // When the exact stage can't be derived, choose the stage that requires
  //   the
  //   // minimal synthesization.

  //   const std::optional<uint64_t> pressed_logical_key =
  //       recorded_physical_key.has_value()
  //           ? lookup_hash_table(_pressing_records,
  //                               recorded_physical_key.value())
  //           : std::nullopt;

  //   FML_DCHECK(!pressed_logical_key.has_value() ||
  //              pressed_logical_key.value() == logical_key);
  //   LockState state_by_record = lock_state_by_record(
  //       pressed_logical_key.has_value(), (_lock_records & modifier_bit) !=
  //       0);

  //   const bool enabled_by_state = (state & modifier_bit) != 0;
  //   const bool this_key_is_event_key = logical_key == event_logical_key;
  //   if (this_key_is_event_key && checked_key->is_caps_lock) {
  //     UpdateCapsLockStateLogicInferrence(is_down, enabled_by_state,
  //                                        state_by_record);
  //     FML_DCHECK(_caps_lock_state_logic_reversed != kStateLogicUndecided);
  //   }
  //   const bool reverse_state_logic =
  //       checked_key->is_caps_lock &&
  //       _caps_lock_state_logic_reversed == kStateLogicReversed;
  //   const int stage_by_event =
  //       this_key_is_event_key
  //           ? find_stage_by_self_event(stage_by_record, is_down,
  //                                      enabled_by_state, reverse_state_logic)
  //           : find_stage_by_others_event(stage_by_record, enabled_by_state);

  //   // The destination stage is equal to stage_by_event but shifted
  //   cyclically
  //   // to be no less than stage_by_record.
  //   constexpr int kNumStages = 4;
  //   const int destination_stage = stage_by_event >= stage_by_record
  //                                     ? stage_by_event
  //                                     : stage_by_event + kNumStages;

  //   FML_DCHECK(stage_by_record <= destination_stage);
  //   if (stage_by_record == destination_stage) {
  //     return;
  //   }
  //   for (int current_stage = stage_by_record; current_stage <
  //   destination_stage;
  //        current_stage += 1) {
  //     if (current_stage == 9) {
  //       return;
  //     }

  //     const int standard_current_stage = current_stage % kNumStages;
  //     const bool is_down_event =
  //         standard_current_stage == 0 || standard_current_stage == 2;
  //     if (is_down_event && !recorded_physical_key.has_value()) {
  //       UpdateMappingRecord(physical_key, logical_key);
  //     }
  //     FlutterKeyEventType type =
  //         is_down_event ? kFlutterKeyEventTypeDown : kFlutterKeyEventTypeUp;
  //     UpdatePressingState(physical_key, is_down_event ? logical_key : 0);
  //     PossiblyUpdateLockBit(logical_key, is_down_event);
  //     SynthesizeSimpleEvent(type, physical_key, logical_key, timestamp,
  //     guard);
  //   }
  // }

  // Synchronizes the pressing state of a key to its state from the event by
  // synthesizing events.
  void SynchronizePressedState(guint modifier_bit,
                               guint state,
                               double timestamp,
                               FlKeyEmbedderCheckedKey* checked_key,
                               FlutterKeyCallbackGuard& guard) {
    // Each TestKey contains up to two logical keys, typically the left modifier
    // and the right modifier, that correspond to the same modifier_bit. We'd
    // like to infer whether to synthesize a down or up event for each key.
    //
    // The hard part is that, if we want to synthesize a down event, we don't
    // know which physical key to use. Here we assume the keyboard layout do not
    // change frequently and use the last physical-logical relationship,
    // recorded in #_mapping_records.
    const uint64_t logical_keys[] = {
        checked_key->primary_logical_key,
        checked_key->secondary_logical_key,
    };
    const guint length = checked_key->secondary_logical_key == 0 ? 1 : 2;

    const bool any_pressed_by_state = (state & modifier_bit) != 0;

    bool any_pressed_by_record = false;

    // Traverse each logical key of this modifier bit for 2 purposes:
    //
    //  1. Perform the synthesization of release events: If the modifier bit is
    //     0 and the key is pressed, synthesize a release event.
    //  2. Prepare for the synthesization of press events: If the modifier bit
    //     is 1, and no keys are pressed (discovered here), synthesize a press
    //     event later.
    for (guint logical_key_idx = 0; logical_key_idx < length;
         logical_key_idx++) {
      const uint64_t logical_key = logical_keys[logical_key_idx];
      FML_DCHECK(logical_key != 0);
      const bool this_key_pressed_before_event =
          reverse_lookup_hash_table(_pressing_records, logical_key).has_value();

      any_pressed_by_record =
          any_pressed_by_record || this_key_pressed_before_event;

      if (this_key_pressed_before_event && !any_pressed_by_state) {
        // Since this key has been pressed before, there must have been a
        // recorded physical key.
        const uint64_t recorded_physical_key =
            lookup_hash_table(_mapping_records, logical_key).value();
        // In rare cases #recorded_logical_key is different from #logical_key.
        const uint64_t recorded_logical_key =
            lookup_hash_table(_pressing_records, recorded_physical_key).value();
        SynthesizeSimpleEvent(kFlutterKeyEventTypeUp, recorded_physical_key,
                              recorded_logical_key, timestamp, guard);
        UpdatePressingState(recorded_physical_key, 0);
      }
    }
    // If the modifier should be pressed, synthesize a down event for its
    // primary key.
    if (any_pressed_by_state && !any_pressed_by_record) {
      const uint64_t logical_key = checked_key->primary_logical_key;
      const std::optional<uint64_t> recorded_physical_key =
          lookup_hash_table(_mapping_records, logical_key);
      // The physical key is derived from past mapping record if possible.
      //
      // The event to be synthesized is a key down event. There might not have
      // been a mapping record, in which case the hard-coded
      // #primary_physical_key is used.
      const uint64_t physical_key =
          recorded_physical_key.value_or(checked_key->primary_physical_key);
      if (!recorded_physical_key.has_value()) {
        UpdateMappingRecord(physical_key, logical_key);
      }
      SynthesizeSimpleEvent(kFlutterKeyEventTypeDown, physical_key, logical_key,
                            timestamp, guard);
      UpdatePressingState(physical_key, logical_key);
    }
  }

  EmbedderSendKeyEvent _send_key_event;
  void* _send_key_event_user_data;

  flutter::PressStateTracker press_state_tracker_;
  flutter::ModifierPairStateTracker modifier_state_tracker_;
  flutter::LockStateTracker lock_tracker_;

  std::unordered_map<uint64_t, uint64_t> logical_key_to_flag_;

  // Internal record for states of whether a key is pressed.
  //
  // It is a map from Flutter physical key to Flutter logical key.  Both keys
  // and values are directly stored uint64s.  This table is freed by the
  // responder.
  std::unordered_map<uint64_t, uint64_t> _pressing_records;

  // Internal record for states of whether a lock mode is enabled.
  //
  // It is a bit mask composed of GTK mode bits.
  guint _lock_records;

  // Internal record for the last observed key mapping.
  //
  // It stores the physical key last seen during a key down event for a logical
  // key. It is used to synthesize modifier keys and lock keys.
  //
  // It is a map from Flutter logical key to physical key.  Both keys and
  // values are directly stored uint64s.  This table is freed by the responder.
  std::unordered_map<uint64_t, uint64_t> _mapping_records;

  // The inferred logic type indicating whether the CapsLock state logic is
  // reversed on this platform.
  //
  // For more information, see #UpdateCapsLockStateLogicInferrence.
  std::optional<bool> _caps_lock_state_logic_reversed;

  // A static map from GTK modifier bits to #FlKeyEmbedderCheckedKey to
  // configure the modifier keys that needs to be tracked and kept synchronous
  // on.
  //
  // The keys are directly stored guints.  The values must be freed with g_free.
  // This table is freed by the responder.
  CheckedKeysMap _modifier_bit_to_checked_keys;

  // A static map from GTK modifier bits to #FlKeyEmbedderCheckedKey to
  // configure the lock mode bits that needs to be tracked and kept synchronous
  // on.
  CheckedKeysMap _lock_bit_to_checked_keys;

  // A static map generated by reverse mapping _lock_bit_to_checked_keys.
  std::map<uint64_t, uint64_t> _logical_key_to_lock_bit;
};

}  // namespace

/* Define FlKeyEmbedderResponder */

struct _FlKeyEmbedderResponder {
  GObject parent_instance;

  EmbedderResponder* responder;
};

static void fl_key_embedder_responder_iface_init(
    FlKeyResponderInterface* iface);
static void fl_key_embedder_responder_dispose(GObject* object);

#define FL_TYPE_EMBEDDER_RESPONDER_USER_DATA \
  fl_key_embedder_responder_get_type()
G_DEFINE_TYPE_WITH_CODE(
    FlKeyEmbedderResponder,
    fl_key_embedder_responder,
    G_TYPE_OBJECT,
    G_IMPLEMENT_INTERFACE(FL_TYPE_KEY_RESPONDER,
                          fl_key_embedder_responder_iface_init))

static void fl_key_embedder_responder_handle_event(
    FlKeyResponder* responder,
    FlKeyEvent* event,
    uint64_t specified_logical_key,
    FlKeyResponderAsyncCallback callback,
    gpointer user_data);

static void fl_key_embedder_responder_iface_init(
    FlKeyResponderInterface* iface) {
  iface->handle_event = fl_key_embedder_responder_handle_event;
}

// Initializes the FlKeyEmbedderResponder class methods.
static void fl_key_embedder_responder_class_init(
    FlKeyEmbedderResponderClass* klass) {
  G_OBJECT_CLASS(klass)->dispose = fl_key_embedder_responder_dispose;
}

// Initializes an FlKeyEmbedderResponder instance.
static void fl_key_embedder_responder_init(FlKeyEmbedderResponder* self) {}

// Disposes of an FlKeyEmbedderResponder instance.
static void fl_key_embedder_responder_dispose(GObject* object) {
  FlKeyEmbedderResponder* self = FL_KEY_EMBEDDER_RESPONDER(object);

  delete self->responder;
  self->responder = nullptr;

  G_OBJECT_CLASS(fl_key_embedder_responder_parent_class)->dispose(object);
}

// Creates a new FlKeyEmbedderResponder instance.
FlKeyEmbedderResponder* fl_key_embedder_responder_new(
    EmbedderSendKeyEvent send_key_event,
    void* send_key_event_user_data) {
  FlKeyEmbedderResponder* self = FL_KEY_EMBEDDER_RESPONDER(
      g_object_new(FL_TYPE_EMBEDDER_RESPONDER_USER_DATA, nullptr));

  self->responder =
      new EmbedderResponder(send_key_event, send_key_event_user_data);

  return self;
}

// Sends a key event to the framework.
static void fl_key_embedder_responder_handle_event(
    FlKeyResponder* responder,
    FlKeyEvent* event,
    uint64_t specified_logical_key,
    FlKeyResponderAsyncCallback callback,
    gpointer user_data) {
  FlKeyEmbedderResponder* self = FL_KEY_EMBEDDER_RESPONDER(responder);
  self->responder->HandleEvent(event, specified_logical_key, callback,
                               user_data);
}

void fl_key_embedder_responder_sync_modifiers_if_needed(
    FlKeyEmbedderResponder* responder,
    guint state,
    double event_time) {
  FlutterKeyCallbackGuard guard(nullptr);
  responder->responder->SyncModifiersIfNeeded(state, event_time, guard);
  guard.MarkSentSynthesizedEvent();
}

GHashTable* fl_key_embedder_responder_get_pressed_state(
    FlKeyEmbedderResponder* self) {
  return ToGHashTable(self->responder->GetPressedState());
}
