// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

import 'dart:typed_data';

import 'package:quiver/testing/async.dart';
import 'package:test/bootstrap/browser.dart';
import 'package:test/test.dart';
import 'package:ui/src/engine/browser_detection.dart';
import 'package:ui/src/engine/dom.dart';
import 'package:ui/src/engine/raw_keyboard.dart';
import 'package:ui/src/engine/services.dart';
import 'package:ui/ui.dart' as ui;

import 'keyboard_test_common.dart';

void main() {
  internalBootstrapBrowserTest(() => testMain);
}

void testMain() {
  group('RawKeyboard', () {
    /// Used to save and restore [ui.window.onPlatformMessage] after each test.
    ui.PlatformMessageCallback? savedCallback;

    setUp(() {
      savedCallback = ui.window.onPlatformMessage;
    });

    tearDown(() {
      ui.window.onPlatformMessage = savedCallback;
    });

    test('dispatches keyup to flutter/keyevent channel', () {
      final RawKeyboard keyboard = RawKeyboard();

      String? channelReceived;
      Map<String, dynamic>? dataReceived;
      ui.window.onPlatformMessage = (String channel, ByteData? data,
          ui.PlatformMessageResponseCallback? callback) {
        channelReceived = channel;
        dataReceived = const JSONMessageCodec().decodeMessage(data) as Map<String, dynamic>?;
      };

      MockKeyboardEvent event;

      event = dispatchKeyboardEvent(keyboard, 'keyup', key: 'SomeKey', code: 'SomeCode', keyCode: 1);

      expect(event.defaultPrevented, isFalse);
      expect(channelReceived, 'flutter/keyevent');
      expect(dataReceived, <String, dynamic>{
        'type': 'keyup',
        'keymap': 'web',
        'code': 'SomeCode',
        'location': 0,
        'key': 'SomeKey',
        'metaState': 0x0,
        'keyCode': 1,
      });

      keyboard.dispose();
    },
        // TODO(mdebbar): https://github.com/flutter/flutter/issues/50815
        skip: browserEngine == BrowserEngine.edge);

    test('dispatches keydown to flutter/keyevent channel', () {
      final RawKeyboard keyboard = RawKeyboard();

      String? channelReceived;
      Map<String, dynamic>? dataReceived;
      ui.window.onPlatformMessage = (String channel, ByteData? data,
          ui.PlatformMessageResponseCallback? callback) {
        channelReceived = channel;
        dataReceived = const JSONMessageCodec().decodeMessage(data) as Map<String, dynamic>?;
      };

      MockKeyboardEvent event;

      event =
          dispatchKeyboardEvent(keyboard, 'keydown', key: 'SomeKey', code: 'SomeCode', keyCode: 1);

      expect(channelReceived, 'flutter/keyevent');
      expect(dataReceived, <String, dynamic>{
        'type': 'keydown',
        'keymap': 'web',
        'code': 'SomeCode',
        'key': 'SomeKey',
        'location': 0,
        'metaState': 0x0,
        'keyCode': 1,
      });
      expect(event.defaultPrevented, isFalse);

      keyboard.dispose();
    },
        // TODO(mdebbar): https://github.com/flutter/flutter/issues/50815
        skip: browserEngine == BrowserEngine.edge);

    test('dispatches correct meta state', () {
      final RawKeyboard keyboard = RawKeyboard();

      Map<String, dynamic>? dataReceived;
      ui.window.onPlatformMessage = (String channel, ByteData? data,
          ui.PlatformMessageResponseCallback? callback) {
        dataReceived = const JSONMessageCodec().decodeMessage(data) as Map<String, dynamic>?;
      };

      MockKeyboardEvent event;

      event = dispatchKeyboardEvent(
        keyboard,
        'keydown',
        key: 'SomeKey',
        code: 'SomeCode',
        isControlPressed: true,
      );
      expect(event.defaultPrevented, isFalse);
      expect(dataReceived, <String, dynamic>{
        'type': 'keydown',
        'keymap': 'web',
        'code': 'SomeCode',
        'key': 'SomeKey',
        'location': 0,
        //          ctrl
        'metaState': 0x4,
        'keyCode': 0,
      });

      event = dispatchKeyboardEvent(
        keyboard,
        'keydown',
        key: 'SomeKey',
        code: 'SomeCode',
        isShiftPressed: true,
        isAltPressed: true,
        isMetaPressed: true,
      );
      expect(event.defaultPrevented, isFalse);
      expect(dataReceived, <String, dynamic>{
        'type': 'keydown',
        'keymap': 'web',
        'code': 'SomeCode',
        'key': 'SomeKey',
        'location': 0,
        //          shift  alt   meta
        'metaState': 0x1 | 0x2 | 0x8,
        'keyCode': 0,
      });

      keyboard.dispose();
    },
        // TODO(mdebbar): https://github.com/flutter/flutter/issues/50815
        skip: browserEngine == BrowserEngine.edge);

    test('dispatches repeat events', () {
      final RawKeyboard keyboard = RawKeyboard();

      final List<Map<String, dynamic>> messages = <Map<String, dynamic>>[];
      ui.window.onPlatformMessage = (String channel, ByteData? data,
          ui.PlatformMessageResponseCallback? callback) {
        messages.add(const JSONMessageCodec().decodeMessage(data) as Map<String, dynamic>);
      };

      MockKeyboardEvent event;

      event = dispatchKeyboardEvent(
        keyboard,
        'keydown',
        key: 'SomeKey',
        code: 'SomeCode',
        repeat: true,
      );
      expect(event.defaultPrevented, isFalse);

      event = dispatchKeyboardEvent(
        keyboard,
        'keydown',
        key: 'SomeKey',
        code: 'SomeCode',
        repeat: true,
      );
      expect(event.defaultPrevented, isFalse);

      event = dispatchKeyboardEvent(
        keyboard,
        'keydown',
        key: 'SomeKey',
        code: 'SomeCode',
        repeat: true,
      );
      expect(event.defaultPrevented, isFalse);

      final Map<String, dynamic> expectedMessage = <String, dynamic>{
        'type': 'keydown',
        'keymap': 'web',
        'code': 'SomeCode',
        'key': 'SomeKey',
        'location': 0,
        'metaState': 0,
        'keyCode': 0,
      };
      expect(messages, <Map<String, dynamic>>[
        expectedMessage,
        expectedMessage,
        expectedMessage,
      ]);

      keyboard.dispose();
    },
        // TODO(mdebbar): https://github.com/flutter/flutter/issues/50815
        skip: browserEngine == BrowserEngine.edge);

    test('prevents default when key is handled by the framework', () {
      final RawKeyboard keyboard = RawKeyboard();

      int count = 0;
      ui.window.onPlatformMessage = (String channel, ByteData? data,
          ui.PlatformMessageResponseCallback? callback) {
        count += 1;
        final ByteData response = const JSONMessageCodec().encodeMessage(<String, dynamic>{'handled': true})!;
        callback!(response);
      };

      final MockKeyboardEvent event = dispatchKeyboardEvent(
        keyboard,
        'keydown',
        key: 'Tab',
        code: 'Tab',
      );

      expect(event.defaultPrevented, isTrue);
      expect(count, 1);

      keyboard.dispose();
    });

    test("Doesn't prevent default when key is not handled by the framework", () {
      final RawKeyboard keyboard = RawKeyboard();

      int count = 0;
      ui.window.onPlatformMessage = (String channel, ByteData? data,
          ui.PlatformMessageResponseCallback? callback) {
        count += 1;
        final ByteData response = const JSONMessageCodec().encodeMessage(<String, dynamic>{'handled': false})!;
        callback!(response);
      };

      final MockKeyboardEvent event = dispatchKeyboardEvent(
        keyboard,
        'keydown',
        key: 'Tab',
        code: 'Tab',
      );

      expect(event.defaultPrevented, isFalse);
      expect(count, 1);

      keyboard.dispose();
    });

    testFakeAsync(
      'On macOS, synthesize keyup when shortcut is handled by the system',
      (FakeAsync async) {
        // This can happen when the user clicks `cmd+alt+i` to open devtools. Here
        // is the sequence we receive from the browser in such case:
        //
        // keydown(cmd) -> keydown(alt) -> keydown(i) -> keyup(alt) -> keyup(cmd)
        //
        // There's no `keyup(i)`. The web engine is expected to synthesize a
        // `keyup(i)` event.
        final RawKeyboard keyboard = RawKeyboard(onMacOs: true);

        final List<Map<String, dynamic>> messages = <Map<String, dynamic>>[];
        ui.window.onPlatformMessage = (String channel, ByteData? data,
            ui.PlatformMessageResponseCallback? callback) {
          messages.add(const JSONMessageCodec().decodeMessage(data) as Map<String, dynamic>);
        };

        dispatchKeyboardEvent(
          keyboard,
          'keydown',
          key: 'Meta',
          code: 'MetaLeft',
          location: 1,
          isMetaPressed: true,
        );
        dispatchKeyboardEvent(
          keyboard,
          'keydown',
          key: 'Alt',
          code: 'AltLeft',
          location: 1,
          isMetaPressed: true,
          isAltPressed: true,
        );
        dispatchKeyboardEvent(
          keyboard,
          'keydown',
          key: 'i',
          code: 'KeyI',
          isMetaPressed: true,
          isAltPressed: true,
        );
        async.elapse(const Duration(milliseconds: 10));
        dispatchKeyboardEvent(
          keyboard,
          'keyup',
          key: 'Meta',
          code: 'MetaLeft',
          location: 1,
          isAltPressed: true,
        );
        dispatchKeyboardEvent(
          keyboard,
          'keyup',
          key: 'Alt',
          code: 'AltLeft',
          location: 1,
        );
        // Notice no `keyup` for "i".

        expect(messages, <Map<String, dynamic>>[
          <String, dynamic>{
            'type': 'keydown',
            'keymap': 'web',
            'key': 'Meta',
            'code': 'MetaLeft',
            'location': 1,
            //           meta
            'metaState': 0x8,
            'keyCode': 0,
          },
          <String, dynamic>{
            'type': 'keydown',
            'keymap': 'web',
            'key': 'Alt',
            'code': 'AltLeft',
            'location': 1,
            //           alt   meta
            'metaState': 0x2 | 0x8,
            'keyCode': 0,
          },
          <String, dynamic>{
            'type': 'keydown',
            'keymap': 'web',
            'key': 'i',
            'code': 'KeyI',
            'location': 0,
            //           alt   meta
            'metaState': 0x2 | 0x8,
            'keyCode': 0,
          },
          <String, dynamic>{
            'type': 'keyup',
            'keymap': 'web',
            'key': 'Meta',
            'code': 'MetaLeft',
            'location': 1,
            //           alt
            'metaState': 0x2,
            'keyCode': 0,
          },
          <String, dynamic>{
            'type': 'keyup',
            'keymap': 'web',
            'key': 'Alt',
            'code': 'AltLeft',
            'location': 1,
            'metaState': 0x0,
            'keyCode': 0,
          },
        ]);
        messages.clear();

        // Still too eary to synthesize a keyup event.
        async.elapse(const Duration(milliseconds: 50));
        expect(messages, isEmpty);

        async.elapse(const Duration(seconds: 3));
        expect(messages, <Map<String, dynamic>>[
          <String, dynamic>{
            'type': 'keyup',
            'keymap': 'web',
            'key': 'i',
            'code': 'KeyI',
            'location': 0,
            'metaState': 0x0,
            'keyCode': 0,
          }
        ]);

        keyboard.dispose();
      },
    );

    testFakeAsync(
      'On macOS, do not synthesize keyup when we receive repeat events',
      (FakeAsync async) {
        final RawKeyboard keyboard = RawKeyboard(onMacOs: true);

        final List<Map<String, dynamic>> messages = <Map<String, dynamic>>[];
        ui.window.onPlatformMessage = (String channel, ByteData? data,
            ui.PlatformMessageResponseCallback? callback) {
          messages.add(const JSONMessageCodec().decodeMessage(data) as Map<String, dynamic>);
        };

        dispatchKeyboardEvent(
          keyboard,
          'keydown',
          key: 'Meta',
          code: 'MetaLeft',
          location: 1,
          isMetaPressed: true,
        );
        dispatchKeyboardEvent(
          keyboard,
          'keydown',
          key: 'Alt',
          code: 'AltLeft',
          location: 1,
          isMetaPressed: true,
          isAltPressed: true,
        );
        dispatchKeyboardEvent(
          keyboard,
          'keydown',
          key: 'i',
          code: 'KeyI',
          isMetaPressed: true,
          isAltPressed: true,
        );
        async.elapse(const Duration(milliseconds: 10));
        dispatchKeyboardEvent(
          keyboard,
          'keyup',
          key: 'Meta',
          code: 'MetaLeft',
          location: 1,
          isAltPressed: true,
        );
        dispatchKeyboardEvent(
          keyboard,
          'keyup',
          key: 'Alt',
          code: 'AltLeft',
          location: 1,
        );
        // Notice no `keyup` for "i".

        messages.clear();

        // Spend more than 2 seconds sending repeat events and make sure no
        // keyup was synthesized.
        for (int i = 0; i < 20; i++) {
          async.elapse(const Duration(milliseconds: 100));
          dispatchKeyboardEvent(
          keyboard,
            'keydown',
            key: 'i',
            code: 'KeyI',
            repeat: true,
          );
        }

        // There should be no synthesized keyup.
        expect(messages, hasLength(20));
        for (int i = 0; i < 20; i++) {
          expect(messages[i], <String, dynamic>{
            'type': 'keydown',
            'keymap': 'web',
            'key': 'i',
            'code': 'KeyI',
            'location': 0,
            'metaState': 0x0,
            'keyCode': 0,
          });
        }
        messages.clear();

        keyboard.dispose();
      },
    );

    testFakeAsync(
      'On macOS, do not synthesize keyup when keys are not affected by meta modifiers',
      (FakeAsync async) {
        final RawKeyboard keyboard = RawKeyboard();

        final List<Map<String, dynamic>> messages = <Map<String, dynamic>>[];
        ui.window.onPlatformMessage = (String channel, ByteData? data,
            ui.PlatformMessageResponseCallback? callback) {
          messages.add(const JSONMessageCodec().decodeMessage(data) as Map<String, dynamic>);
        };

        dispatchKeyboardEvent(
          keyboard,
          'keydown',
          key: 'i',
          code: 'KeyI',
        );
        dispatchKeyboardEvent(
          keyboard,
          'keydown',
          key: 'o',
          code: 'KeyO',
        );
        messages.clear();

        // Wait for a long-enough period of time and no events
        // should be synthesized
        async.elapse(const Duration(seconds: 3));
        expect(messages, hasLength(0));

        keyboard.dispose();
      },
    );

    testFakeAsync('On macOS, do not synthesize keyup for meta keys', (FakeAsync async) {
      final RawKeyboard keyboard = RawKeyboard(onMacOs: true);

      final List<Map<String, dynamic>> messages = <Map<String, dynamic>>[];
      ui.window.onPlatformMessage = (String channel, ByteData? data,
          ui.PlatformMessageResponseCallback? callback) {
        messages.add(const JSONMessageCodec().decodeMessage(data) as Map<String, dynamic>);
      };

      dispatchKeyboardEvent(
        keyboard,
        'keydown',
        key: 'Meta',
        code: 'MetaLeft',
        location: 1,
        isMetaPressed: true,
      );
      dispatchKeyboardEvent(
        keyboard,
        'keydown',
        key: 'Alt',
        code: 'AltLeft',
        location: 1,
        isMetaPressed: true,
        isAltPressed: true,
      );
      dispatchKeyboardEvent(
        keyboard,
        'keydown',
        key: 'i',
        code: 'KeyI',
        isMetaPressed: true,
        isAltPressed: true,
      );
      async.elapse(const Duration(milliseconds: 10));
      dispatchKeyboardEvent(
        keyboard,
        'keyup',
        key: 'Meta',
        code: 'MetaLeft',
        location: 1,
        isAltPressed: true,
      );
      // Notice no `keyup` for "AltLeft" and "i".

      messages.clear();

      // There has been no repeat events for "AltLeft" nor "i". Only "i" should
      // synthesize a keyup event.
      async.elapse(const Duration(seconds: 3));
      expect(messages, <Map<String, dynamic>>[
        <String, dynamic>{
          'type': 'keyup',
          'keymap': 'web',
          'key': 'i',
          'code': 'KeyI',
          'location': 0,
          //           alt
          'metaState': 0x2,
          'keyCode': 0,
        }
      ]);

      keyboard.dispose();
    });

    testFakeAsync(
      'On non-macOS, do not synthesize keyup for shortcuts',
      (FakeAsync async) {
        final RawKeyboard keyboard = RawKeyboard(onMacOs: false);

        final List<Map<String, dynamic>> messages = <Map<String, dynamic>>[];
        ui.window.onPlatformMessage = (String channel, ByteData? data,
            ui.PlatformMessageResponseCallback? callback) {
          messages.add(const JSONMessageCodec().decodeMessage(data) as Map<String, dynamic>);
        };

        dispatchKeyboardEvent(
          keyboard,
          'keydown',
          key: 'Meta',
          code: 'MetaLeft',
          location: 1,
          isMetaPressed: true,
        );
        dispatchKeyboardEvent(
          keyboard,
          'keydown',
          key: 'Alt',
          code: 'AltLeft',
          location: 1,
          isMetaPressed: true,
          isAltPressed: true,
        );
        dispatchKeyboardEvent(
          keyboard,
          'keydown',
          key: 'i',
          code: 'KeyI',
          isMetaPressed: true,
          isAltPressed: true,
        );
        async.elapse(const Duration(milliseconds: 10));
        dispatchKeyboardEvent(
          keyboard,
          'keyup',
          key: 'Meta',
          code: 'MetaLeft',
          location: 1,
          isAltPressed: true,
        );
        dispatchKeyboardEvent(
          keyboard,
          'keyup',
          key: 'Alt',
          code: 'AltLeft',
          location: 1,
        );
        // Notice no `keyup` for "i".

        expect(messages, hasLength(5));
        messages.clear();

        // Never synthesize keyup events.
        async.elapse(const Duration(seconds: 3));
        expect(messages, isEmpty);

        keyboard.dispose();
      },
    );

  });
}

MockKeyboardEvent dispatchKeyboardEvent(
  RawKeyboard keyboard,
  String type, {
  String? key,
  String? code,
  int location = 0,
  bool repeat = false,
  bool isShiftPressed = false,
  bool isAltPressed = false,
  bool isControlPressed = false,
  bool isMetaPressed = false,
  int keyCode = 0,
}) {
  final MockKeyboardEvent event = MockKeyboardEvent(
    type: type,
    key: key,
    code: code,
    location: location,
    repeat: repeat,
    shiftKey: isShiftPressed,
    altKey: isAltPressed,
    ctrlKey: isControlPressed,
    metaKey: isMetaPressed,
    keyCode: keyCode,
  );
  keyboard.handleEvent(event);

  return event;
}

typedef FakeAsyncTest = void Function(FakeAsync);

void testFakeAsync(String description, FakeAsyncTest fn) {
  test(description, () {
    FakeAsync().run(fn);
  });
}
