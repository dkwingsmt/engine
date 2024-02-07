// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "flutter/shell/platform/common/keyboard_models.h"

#include "flutter/fml/logging.h"

namespace flutter {

namespace {

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

LockState PreviousStateOf(LockState target) {
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

template <typename T>
void MaybePushBack(std::vector<T>* output, std::optional<T> event_type) {
  if (event_type.has_value()) {
    output->push_back(event_type.value());
  }
}

}  // namespace

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

FlutterKeyCallbackGuard::FlutterKeyCallbackGuard(void* content)
    : content_(content) {}

FlutterKeyCallbackGuard::~FlutterKeyCallbackGuard() {
  if (content_ != nullptr) {
    FML_LOG(ERROR) << "The guard is destroyed without content being resolved.";
  }
  if (!sent_any_events()) {
    FML_LOG(ERROR) << "The guard is destroyed without sending any events.";
  }
}

void* FlutterKeyCallbackGuard::MarkSentPrimaryEvent() {
  FML_DCHECK(!sent_primary_event_);
  FML_DCHECK(content_ != nullptr);
  sent_primary_event_ = true;
  return Release();
}

void FlutterKeyCallbackGuard::MarkSentSynthesizedEvent() {
  sent_synthesized_events_ = true;
}

void* FlutterKeyCallbackGuard::Release() {
  void* content = content_;
  content_ = nullptr;
  return content;
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
    std::vector<FlutterKeyEvent>* output,
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
    std::vector<FlutterKeyEvent>* output,
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

void ModifierStateTracker::ModelModifierState(
    std::vector<StateChange>* output,
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

static constexpr uint64_t kPlaneMask = 0xFF00000000;
static constexpr uint64_t kValueMask = 0x00FFFFFFFF;
static constexpr uint64_t kUnprintablePlane = 0x0100000000;
static constexpr uint64_t kUnprintablePhysicalPlane = 0x0200000000;

static bool IsUnprintablePlane(uint64_t logical_key) {
  return (logical_key & kPlaneMask) == kUnprintablePlane;
}

void ModifierPairStateTracker::Register(uint64_t flag, uint64_t logical_key_1, uint64_t logical_key_2) {
  bool new_entry = flag_to_logical_keys_.try_emplace(flag, logical_key_1, logical_key_2).second;
  FML_DCHECK(IsUnprintablePlane(logical_key_1))
      << std::hex << "Logical key is not an unprintable 0x" << logical_key_1;
  FML_DCHECK(IsUnprintablePlane(logical_key_2))
      << std::hex << "Logical key is not an unprintable 0x" << logical_key_2;
  FML_DCHECK(new_entry)
      << std::hex << "Duplicate entry: flag 0x" << flag  //
      << " logical key 1 0x" << logical_key_1            //
      << " logical key 2 0x" << logical_key_2;
  (void)(new_entry);
}

void ModifierPairStateTracker::RequireEventState(
    std::vector<FlutterKeyEvent>* output,
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
    std::vector<FlutterKeyEvent>* output,
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
    std::vector<FlutterKeyEvent>* output,
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

uint64_t ModifierPairStateTracker::FabricatePhysicalKey(uint64_t logical_key) {
  FML_DCHECK(IsUnprintablePlane(logical_key));
  return (kValueMask & logical_key) | kUnprintablePhysicalPlane;
}

void LockStateTracker::RequireState(
    std::vector<FlutterKeyEvent>* output,
    NativeEvent& source_event,
    std::optional<LockState> require_primary_state,
    LockState require_after_cleanup) {
  const uint64_t logical_key = source_event.logical_key();
  const double timestamp = source_event.timestamp();
  for (StateChange state_change :
       ModelState(logical_key, require_primary_state, require_after_cleanup)) {
    const uint64_t physical_key = EnsurePhysicalKey(
        source_event, /*force_update=*/state_change.change == EventType::kDown);
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

std::vector<StateChange> LockStateTracker::ModelState(
    uint64_t logical_key,
    std::optional<LockState> require_primary_state,
    LockState require_after_cleanup) {
  std::vector<StateChange> output;
  StateMap::iterator current =
      states_.try_emplace(logical_key, LockState::kReleasedOff).first;
  if (require_primary_state.has_value()) {
    EnsureLockState(&output, current,
                    PreviousStateOf(require_primary_state.value()),
                    /*synthesized=*/true);
    EnsureLockState(&output, current, require_primary_state.value(),
                    /*synthesized=*/false);
  }
  EnsureLockState(&output, current, require_after_cleanup,
                  /*synthesized=*/true);
  return output;
}

void LockStateTracker::EnsureLockState(std::vector<StateChange>* output,
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
