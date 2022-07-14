// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

import 'dart:async';
import 'dart:typed_data';

import 'keyboard_binding.dart';
import 'platform_dispatcher.dart';
import 'services.dart';

/// Parses keyboard events and send raw information through the
/// `flutter/keyevent` channel.
///
/// An instance of this is typically owned by [KeyboardBinding].
class RawKeyboard {
  RawKeyboard({this.onMacOs = false});

  /// A mapping of [KeyboardEvent.code] to [Timer].
  ///
  /// The timer is for when to synthesize a keyup for the [KeyboardEvent.code]
  /// if no repeat events were received.
  final Map<String, Timer> _keydownTimers = <String, Timer>{};

  /// Terminates unfinished tasks in this [RawKeyboard] instance.
  ///
  /// The [dispose] must be called when a [RawKeyboard] instance is no longer
  /// used.
  void dispose() {
    for (final String key in _keydownTimers.keys) {
      _keydownTimers[key]!.cancel();
    }
    _keydownTimers.clear();
  }

  static const JSONMessageCodec _messageCodec = JSONMessageCodec();

  /// Contains meta state from the latest event.
  ///
  /// Initializing with `0x0` which means no meta keys are pressed.
  int _lastMetaState = 0x0;

  final bool onMacOs;

  // When the user enters a browser/system shortcut (e.g. `cmd+alt+i`) on macOS,
  // the browser doesn't send a keyup for it. This puts the framework in a
  // corrupt state because it thinks the key was never released.
  //
  // To avoid this, we rely on the fact that browsers send repeat events
  // while the key is held down by the user. If we don't receive a repeat
  // event within a specific duration ([_kKeydownCancelDurationMac]) we assume
  // the user has released the key and we synthesize a keyup event.
  bool _shouldDoKeyGuard() {
    return onMacOs;
  }

  void handleEvent(FlutterHtmlKeyboardEvent event) {
    final String timerKey = event.code!;

    // Don't handle synthesizing a keyup event for modifier keys
    if (!_isModifierKey(event) && _shouldDoKeyGuard()) {
      _keydownTimers[timerKey]?.cancel();

      // Only keys affected by modifiers, require synthesizing
      // because the browser always sends a keyup event otherwise
      if (event.type == 'keydown' && _isAffectedByModifiers(event)) {
        _keydownTimers[timerKey] = Timer(_kKeydownCancelDurationMac, () {
          _keydownTimers.remove(timerKey);
          _synthesizeKeyup(event);
        });
      } else {
        _keydownTimers.remove(timerKey);
      }
    }

    _lastMetaState = _getMetaState(event);
    if (event.type == 'keydown') {
      // For lock modifiers _getMetaState won't report a metaState at keydown.
      // See https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/getModifierState.
      if (event.key == 'CapsLock') {
        _lastMetaState |= modifierCapsLock;
      } else if (event.code == 'NumLock') {
        _lastMetaState |= modifierNumLock;
      } else if (event.key == 'ScrollLock') {
        _lastMetaState |= modifierScrollLock;
      }
    }
    final Map<String, dynamic> eventData = <String, dynamic>{
      'type': event.type,
      'keymap': 'web',
      'code': event.code,
      'key': event.key,
      'location': event.location,
      'metaState': _lastMetaState,
      'keyCode': event.keyCode,
    };

    EnginePlatformDispatcher.instance.invokeOnPlatformMessage('flutter/keyevent',
      _messageCodec.encodeMessage(eventData), (ByteData? data) {
        if (data == null) {
          return;
        }
        final Map<String, dynamic> jsonResponse = _messageCodec.decodeMessage(data) as Map<String, dynamic>;
        if (jsonResponse['handled'] as bool) {
          // If the framework handled it, then don't propagate it any further.
          event.preventDefault();
        }
      },
    );
  }

  void _synthesizeKeyup(FlutterHtmlKeyboardEvent event) {
    final Map<String, dynamic> eventData = <String, dynamic>{
      'type': 'keyup',
      'keymap': 'web',
      'code': event.code,
      'key': event.key,
      'location': event.location,
      'metaState': _lastMetaState,
      'keyCode': event.keyCode,
    };

    EnginePlatformDispatcher.instance.invokeOnPlatformMessage('flutter/keyevent',
        _messageCodec.encodeMessage(eventData), _noopCallback);
  }

  /// After a keydown is received, this is the duration we wait for a repeat event
  /// before we decide to synthesize a keyup event.
  ///
  /// This value is only for macOS, where the keyboard repeat delay goes up to
  /// 2000ms.
  static const Duration _kKeydownCancelDurationMac = Duration(milliseconds: 2000);
}

const int _modifierNone = 0x00;
const int _modifierShift = 0x01;
const int _modifierAlt = 0x02;
const int _modifierControl = 0x04;
const int _modifierMeta = 0x08;
const int modifierNumLock = 0x10;
const int modifierCapsLock = 0x20;
const int modifierScrollLock = 0x40;

/// Creates a bitmask representing the meta state of the [event].
int _getMetaState(FlutterHtmlKeyboardEvent event) {
  int metaState = _modifierNone;
  if (event.getModifierState('Shift')) {
    metaState |= _modifierShift;
  }
  if (event.getModifierState('Alt') || event.getModifierState('AltGraph')) {
    metaState |= _modifierAlt;
  }
  if (event.getModifierState('Control')) {
    metaState |= _modifierControl;
  }
  if (event.getModifierState('Meta')) {
    metaState |= _modifierMeta;
  }
  // See https://github.com/flutter/flutter/issues/66601 for why we don't
  // set the ones below based on persistent state.
  // if (event.getModifierState("CapsLock")) {
  //   metaState |= modifierCapsLock;
  // }
  // if (event.getModifierState("NumLock")) {
  //   metaState |= modifierNumLock;
  // }
  // if (event.getModifierState("ScrollLock")) {
  //   metaState |= modifierScrollLock;
  // }
  return metaState;
}

/// Returns true if the [event] was caused by a modifier key.
///
/// Modifier keys are shift, alt, ctrl and meta/cmd/win. These are the keys used
/// to perform keyboard shortcuts (e.g. `cmd+c`, `cmd+l`).
bool _isModifierKey(FlutterHtmlKeyboardEvent event) {
  final String key = event.key!;
  return key == 'Meta' || key == 'Shift' || key == 'Alt' || key == 'Control';
}

/// Returns true if the [event] is been affects by any of the modifiers key
///
/// This is a strong indication that this key is been used for a shortcut
bool _isAffectedByModifiers(FlutterHtmlKeyboardEvent event) {
  return event.ctrlKey || event.shiftKey || event.altKey || event.metaKey;
}

void _noopCallback(ByteData? data) {}
