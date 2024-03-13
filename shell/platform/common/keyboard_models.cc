// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "flutter/shell/platform/common/keyboard_models.h"

#include "flutter/fml/logging.h"

namespace flutter {

namespace {

constexpr uint64_t kPlaneMask = 0xFF00000000;
constexpr uint64_t kValueMask = 0x00FFFFFFFF;
constexpr uint64_t kUnprintablePlane = 0x0100000000;
constexpr uint64_t kUnprintablePhysicalPlane = 0x0200000000;

bool IsUnprintablePlane(uint64_t logical_key) {
  return (logical_key & kPlaneMask) == kUnprintablePlane;
}

uint64_t FabricatePhysicalKey(uint64_t logical_key) {
  FML_DCHECK(IsUnprintablePlane(logical_key));
  return (kValueMask & logical_key) | kUnprintablePhysicalPlane;
}

bool IsPressed(LockState lock_state) {
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

constexpr LockStateGroup kAllValidLockStates =
    static_cast<LockStateGroup>(LockState::kReleasedOff) |
    static_cast<LockStateGroup>(LockState::kPressedOn) |
    static_cast<LockStateGroup>(LockState::kReleasedOn) |
    static_cast<LockStateGroup>(LockState::kPressedOff);

bool IsValid(LockStateGroup target) {
  return (target & ~kAllValidLockStates) == 0 &&  //
         (target != 0) &&                         //
         (target != kAllValidLockStates);
}

template <typename T>
void MaybePushBack(std::vector<T>* output, std::optional<T> event_type) {
  if (event_type.has_value()) {
    output->push_back(event_type.value());
  }
}

}  // namespace

LockState Previous(LockState target) {
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

LockState Next(LockState target) {
  switch (target) {
    case LockState::kReleasedOff:
      return LockState::kPressedOn;
    case LockState::kPressedOn:
      return LockState::kReleasedOn;
    case LockState::kReleasedOn:
      return LockState::kPressedOff;
    case LockState::kPressedOff:
      return LockState::kReleasedOff;
    default:
      FML_UNREACHABLE();
  }
}

LockStateGroup Next(LockStateGroup target) {
  // Algorithm:
  //   For      0000abcd
  //   Return   0000bcda
  return (target & 0x08) >> 3 | (target & 0x07) << 1;
}

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

static const FlutterKeyEvent kEmptyEvent{
    .struct_size = sizeof(FlutterKeyEvent),
    .timestamp = 0,
    .type = kFlutterKeyEventTypeDown,
    .physical = 0,
    .logical = 0,
    .character = nullptr,
    .synthesized = true,
};

FlutterKeyCallbackGuard::FlutterKeyCallbackGuard() {}

FlutterKeyCallbackGuard::~FlutterKeyCallbackGuard() {}

EventList* FlutterKeyCallbackGuard::GetEventList(bool is_target_event) {
  return &(is_target_event ? target_key_events_ : other_key_events_);
}

bool FlutterKeyCallbackGuard::SendEventImpl(SendKeyEvent send_event, GetResponseContext get_response_context) {
  std::list<FlutterKeyEvent> all_events;
  all_events.insert(other_key_events_.begin(), other_key_events_.end());
  other_key_events_.clear();
  all_events.insert(target_key_events_.begin(), target_key_events_.end());
  target_key_events_.clear();

  bool met_primary_event = false;
  for (const FlutterKeyEvent& event : all_events) {
    if (!event.synthesized) {
      FML_DCHECK(!met_primary_event);
      met_primary_event = true;
      send_event(&event, get_response_context());
    } else {
      send_event(&event, nullptr);
    }
  }
  return all_events.size() != 0;
}

uint64_t PhysicallyIndexed::EnsureLogicalKey(NativeEvent& native_event,
                                             bool force_update) {
  const uint64_t physical_key = native_event.physical_key();
  if (!force_update) {
    auto found_entry = physical_to_logical_.find(physical_key);
    if (found_entry != physical_to_logical_.end()) {
      return found_entry->second;
    }
  }
  const uint64_t logical_key = native_event.logical_key();
  physical_to_logical_[physical_key] = logical_key;
  return logical_key;
}

uint64_t LogicallyIndexed::EnsurePhysicalKey(NativeEvent& native_event,
                                             bool force_update) {
  const uint64_t logical_key = native_event.logical_key();
  if (!force_update) {
    auto found_entry = logical_to_physical_.find(logical_key);
    if (found_entry != logical_to_physical_.end()) {
      return found_entry->second;
    }
  }
  const uint64_t physical_key = native_event.physical_key();
  logical_to_physical_[logical_key] = physical_key;
  return physical_key;
}

std::unordered_map<uint64_t, uint64_t> PressStateTracker::GetPressedState() {
  std::unordered_map<uint64_t, uint64_t> result;
  for (auto [physical_key, pressed] : pressed_keys_) {
    if (pressed) {
      auto found_entry = physical_to_logical_.find(physical_key);
      FML_DCHECK(found_entry != physical_to_logical_.end());
      result[physical_key] = found_entry->second;
    }
  }
  return result;
}

void PressStateTracker::RequireTextKeyState(
    EventList* output,
    NativeEvent& source_event,
    std::optional<bool> require_pressed_before,
    bool require_pressed_after) {
  FML_DCHECK(output != nullptr);

  const uint64_t physical_key = source_event.physical_key();
  std::vector<EventType> event_types = ModelTextKeyEvent(
      physical_key, require_pressed_before, require_pressed_after);
  for (EventType event_type : event_types) {
    const uint64_t logical_key = EnsureLogicalKey(
        source_event, /*force_update=*/event_type == EventType::kDown);
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

std::vector<EventType> PressStateTracker::ModelTextKeyEvent(
    uint64_t physical_key,
    std::optional<bool> require_pressed_before,
    bool require_pressed_after) {
  std::vector<EventType> output;
  StateMap::iterator current =
      pressed_keys_.try_emplace(physical_key, false).first;

  // Align with "require_pressed_before".
  if (require_pressed_before.has_value()) {
    MaybePushBack(&output,
                  EnsurePressed(current, require_pressed_before.value()));
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

std::optional<EventType> PressStateTracker::EnsurePressed(
    StateMap::iterator current,
    bool require_pressed) {
  if (current->second == require_pressed) {
    return std::nullopt;
  }
  current->second = !current->second;
  return require_pressed ? EventType::kDown : EventType::kUp;
}

std::unordered_map<uint64_t, uint64_t> ModifierStateTracker::GetPressedState() {
  std::unordered_map<uint64_t, uint64_t> result;
  for (auto [logical_key, pressed] : pressed_keys_) {
    if (pressed) {
      auto found_entry = logical_to_physical_.find(logical_key);
      FML_DCHECK(found_entry != logical_to_physical_.end());
      result[found_entry->second] = logical_key;
    }
  }
  return result;
}

void ModifierStateTracker::RequireModifierKeyState(
    EventList* output,
    NativeEvent& source_event,
    std::optional<bool> require_pressed_before_primary,
    std::optional<bool> require_pressed_after_primary) {
  FML_DCHECK(output != nullptr);

  const uint64_t logical_key = source_event.logical_key();
  std::vector<StateChange> state_changes;
  if (require_pressed_before_primary.has_value()) {
    ModelModifierState(
        &state_changes, logical_key,
        /*require_pressed_after=*/require_pressed_before_primary.value(),
        /*synthesized=*/true);
  }
  if (require_pressed_after_primary.has_value()) {
    ModelModifierState(
        &state_changes, logical_key,
        /*require_pressed_after=*/require_pressed_after_primary.value(),
        /*synthesized=*/false);
  }
  for (StateChange& state_change : state_changes) {
    const uint64_t physical_key = EnsurePhysicalKey(
        source_event, /*force_update=*/state_change.change == EventType::kDown);
    output->push_back(FlutterKeyEvent{
        .struct_size = sizeof(FlutterKeyEvent),
        .timestamp = source_event.timestamp(),
        .type = ToEmbedderApiType(state_change.change),
        .physical = physical_key,
        .logical = logical_key,
        .character = nullptr,
        .synthesized = state_change.synthesized,
    });
  }
}

void ModifierStateTracker::ModelModifierState(std::vector<StateChange>* output,
                                              uint64_t physical_key,
                                              bool require_pressed_after,
                                              bool synthesized) {
  StateMap::iterator current =
      pressed_keys_.try_emplace(physical_key, false).first;

  auto maybe_event_type = EnsurePressed(current, require_pressed_after);
  if (maybe_event_type.has_value()) {
    output->push_back(StateChange{
        .change = maybe_event_type.value(),
        .synthesized = synthesized,
    });
  }
}

std::optional<EventType> ModifierStateTracker::EnsurePressed(
    StateMap::iterator current,
    bool require_pressed) {
  if (current->second == require_pressed) {
    return std::nullopt;
  }
  current->second = !current->second;
  return require_pressed ? EventType::kDown : EventType::kUp;
}

void ModifierPairStateTracker::Register(uint64_t flag,
                                        uint64_t logical_key_1,
                                        uint64_t logical_key_2) {
  bool new_entry =
      flag_to_logical_keys_.try_emplace(flag, logical_key_1, logical_key_2)
          .second;
  // The logical keys are required to be in a specific plane in order to be
  // safely fabricated into a physical key in case a down event must be
  // synthesized before a real event is encountered.
  FML_DCHECK(IsUnprintablePlane(logical_key_1))
      << std::hex << "Logical key is not an unprintable 0x" << logical_key_1;
  FML_DCHECK(IsUnprintablePlane(logical_key_2))
      << std::hex << "Logical key is not an unprintable 0x" << logical_key_2;
  FML_DCHECK(new_entry) << std::hex << "Duplicate entry: flag 0x" << flag  //
                        << " logical key 1 0x" << logical_key_1            //
                        << " logical key 2 0x" << logical_key_2;
  (void)(new_entry);
}

void ModifierPairStateTracker::RequireEventState(
    EventList* output,
    NativeEvent& source_event,
    std::optional<bool> require_pressed_before,
    std::optional<bool> require_pressed_after) {
  const uint64_t logical_key = source_event.logical_key();
  StateMap::iterator current =
      logical_key_pressed_.try_emplace(logical_key, false).first;

  std::vector<StateChange> state_changes;
  if (require_pressed_before.has_value()) {
    MaybePushBack(&state_changes,
                  EnsurePressed(current, require_pressed_before.value(),
                                /*synthesized=*/true));
  }
  if (require_pressed_after.has_value()) {
    MaybePushBack(&state_changes,
                  EnsurePressed(current, require_pressed_after.value(),
                                /*synthesized=*/false));
  }

  for (const StateChange& state_change : state_changes) {
    const uint64_t physical_key = EnsurePhysicalKey(
        source_event, /*force_update=*/state_change.change == EventType::kDown);
    output->push_back(FlutterKeyEvent{
        .struct_size = sizeof(FlutterKeyEvent),
        .timestamp = source_event.timestamp(),
        .type = ToEmbedderApiType(state_change.change),
        .physical = physical_key,
        .logical = logical_key,
        .character = nullptr,
        .synthesized = state_change.synthesized,
    });
  }
}

void ModifierPairStateTracker::RequireFlagState(
    EventList* output,
    uint64_t flag,
    double timestamp,
    bool require_pressed) {
  auto found_key_pair = flag_to_logical_keys_.find(flag);
  FML_DCHECK(found_key_pair != flag_to_logical_keys_.end())
      << std::hex << "Unregistered flag 0x" << flag;
  const uint64_t logical_key_1 = found_key_pair->second.first;
  const uint64_t logical_key_2 = found_key_pair->second.second;
  StateMap::iterator current_1 =
      logical_key_pressed_.try_emplace(logical_key_1, false).first;
  StateMap::iterator current_2 =
      logical_key_pressed_.try_emplace(logical_key_2, false).first;

  if (require_pressed) {
    // Only needs to synthesize if neither is pressed.
    if (!current_1->second && !current_2->second) {
      // Choose one of the keys to synthesize a down event. Prefer the key that
      // we've seen before to minimize the need to fabricate physical keys.
      const bool met_key_1_before = logical_to_physical_.find(logical_key_1) !=
                                    logical_to_physical_.end();
      if (met_key_1_before) {
        RequireKeyState(output, current_1, timestamp, /*require_pressed=*/true);
      } else {
        // No matter if we've met key 2 before, we're choosing key 2.
        RequireKeyState(output, current_2, timestamp, /*require_pressed=*/true);
      }
    }
  } else {
    RequireKeyState(output, current_1, timestamp, /*require_pressed=*/false);
    RequireKeyState(output, current_2, timestamp, /*require_pressed=*/false);
  }
}

void ModifierPairStateTracker::RequireKeyState(
    EventList* output,
    StateMap::iterator current,
    double timestamp,
    bool require_pressed) {
  std::optional<StateChange> maybe_state_change =
      EnsurePressed(current, require_pressed,
                    /*synthesized=*/true);
  if (maybe_state_change.has_value()) {
    const uint64_t logical_key = current->first;
    output->push_back(FlutterKeyEvent{
        .struct_size = sizeof(FlutterKeyEvent),
        .timestamp = timestamp,
        .type = ToEmbedderApiType(maybe_state_change.value().change),
        .physical = PhysicalKeyFromRecord(logical_key),
        .logical = logical_key,
        .character = nullptr,
        .synthesized = true,
    });
  }
}

uint64_t ModifierPairStateTracker::PhysicalKeyFromRecord(uint64_t logical_key) {
  auto found = logical_to_physical_.find(logical_key);
  if (found == logical_to_physical_.end()) {
    found = logical_to_physical_
                .try_emplace(logical_key, FabricatePhysicalKey(logical_key))
                .first;
  }
  return found->second;
}

std::optional<StateChange> ModifierPairStateTracker::EnsurePressed(
    StateMap::iterator current,
    bool require_pressed,
    bool synthesized) {
  if (current->second == require_pressed) {
    return std::nullopt;
  }
  current->second = !current->second;
  return StateChange{
      .change = require_pressed ? EventType::kDown : EventType::kUp,
      .synthesized = synthesized,
  };
}

void LockStateTracker::RequireState(
    EventList* output,
    NativeEvent& source_event,
    std::optional<LockStateGroup> require_primary_state,
    LockStateGroup require_after_cleanup) {
  const uint64_t logical_key = source_event.logical_key();
  const double timestamp = source_event.timestamp();

  std::vector<StateChange> state_changes;
  StateMap::iterator current =
      states_.try_emplace(logical_key, LockState::kReleasedOff).first;
  if (require_primary_state.has_value()) {
    EnsureLockState(&state_changes, current, require_primary_state.value(),
                    /*synthesized=*/true);
    if (!state_changes.empty()) {
      state_changes.back().synthesized = false;
    }
  }
  EnsureLockState(&state_changes, current, require_after_cleanup,
                  /*synthesized=*/true);

  for (StateChange state_change : state_changes) {
    const uint64_t physical_key = EnsurePhysicalKey(
        source_event,
        /*force_update=*/state_change.change == EventType::kDown);
    output->push_back(FlutterKeyEvent{
        .struct_size = sizeof(FlutterKeyEvent),
        .timestamp = timestamp,
        .type = ToEmbedderApiType(state_change.change),
        .physical = physical_key,
        .logical = logical_key,
        .character = nullptr,
        .synthesized = state_change.synthesized,
    });
  }
}

void LockStateTracker::RequireState(EventList* output,
                                    uint64_t logical_key,
                                    double timestamp,
                                    LockStateGroup require_state) {
  std::vector<StateChange> state_changes;
  StateMap::iterator current =
      states_.try_emplace(logical_key, LockState::kReleasedOff).first;
  EnsureLockState(&state_changes, current, require_state,
                  /*synthesized=*/true);

  for (StateChange state_change : state_changes) {
    output->push_back(FlutterKeyEvent{
        .struct_size = sizeof(FlutterKeyEvent),
        .timestamp = timestamp,
        .type = ToEmbedderApiType(state_change.change),
        .physical = PhysicalKeyFromRecord(logical_key),
        .logical = logical_key,
        .character = nullptr,
        .synthesized = state_change.synthesized,
    });
  }
}

std::unordered_map<uint64_t, uint64_t> LockStateTracker::GetPressedState() {
  std::unordered_map<uint64_t, uint64_t> result;
  for (auto [logical_key, lock_state] : states_) {
    if (IsPressed(lock_state)) {
      auto found_entry = logical_to_physical_.find(logical_key);
      FML_DCHECK(found_entry != logical_to_physical_.end());
      result[found_entry->second] = logical_key;
    }
  }
  return result;
}

LockState LockStateTracker::GetState(uint64_t logical_key) {
  StateMap::iterator current =
      states_.try_emplace(logical_key, LockState::kReleasedOff).first;
  return current->second;
}

void LockStateTracker::EnsureLockState(std::vector<StateChange>* output,
                                       StateMap::iterator& current,
                                       LockStateGroup required_state,
                                       bool synthesized) {
  FML_DCHECK(IsValid(required_state));
  while ((Exactly(current->second) & required_state) == 0) {
    EventType change = ProgressState(current);
    output->push_back({
        .change = change,
        .synthesized = synthesized,
    });
  }
}

uint64_t LockStateTracker::PhysicalKeyFromRecord(uint64_t logical_key) {
  auto found = logical_to_physical_.find(logical_key);
  if (found == logical_to_physical_.end()) {
    found = logical_to_physical_
                .try_emplace(logical_key, FabricatePhysicalKey(logical_key))
                .first;
  }
  return found->second;
}

EventType LockStateTracker::ProgressState(StateMap::iterator& current) {
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

}  // namespace flutter
