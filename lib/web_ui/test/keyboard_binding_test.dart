// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

import 'dart:js_util' as js_util;
import 'dart:typed_data';
import 'package:meta/meta.dart' show isTest;
import 'package:quiver/testing/async.dart';
import 'package:test/bootstrap/browser.dart';
import 'package:test/test.dart';
import 'package:ui/src/engine.dart';
import 'package:ui/ui.dart' as ui;

const int kLocationStandard = 0;
const int kLocationLeft = 1;
const int kLocationRight = 2;
const int kLocationNumpad = 3;

final int kPhysicalKeyA = kWebToPhysicalKey['KeyA']!;
final int kPhysicalKeyE = kWebToPhysicalKey['KeyE']!;
final int kPhysicalKeyU = kWebToPhysicalKey['KeyU']!;
final int kPhysicalDigit1 = kWebToPhysicalKey['Digit1']!;
final int kPhysicalNumpad1 = kWebToPhysicalKey['Numpad1']!;
final int kPhysicalShiftLeft = kWebToPhysicalKey['ShiftLeft']!;
final int kPhysicalShiftRight = kWebToPhysicalKey['ShiftRight']!;
final int kPhysicalMetaLeft = kWebToPhysicalKey['MetaLeft']!;
final int kPhysicalTab = kWebToPhysicalKey['Tab']!;
final int kPhysicalCapsLock = kWebToPhysicalKey['CapsLock']!;
final int kPhysicalScrollLock = kWebToPhysicalKey['ScrollLock']!;
// A web-specific physical key when code is empty.
const int kPhysicalEmptyCode = 0x1700000000;

const int kLogicalKeyA = 0x00000000061;
const int kLogicalKeyU = 0x00000000075;
const int kLogicalDigit1 = 0x00000000031;
final int kLogicalNumpad1 = kWebLogicalLocationMap['1']![kLocationNumpad]!;
final int kLogicalShiftLeft = kWebLogicalLocationMap['Shift']![kLocationLeft]!;
final int kLogicalShiftRight = kWebLogicalLocationMap['Shift']![kLocationRight]!;
final int kLogicalCtrlLeft = kWebLogicalLocationMap['Control']![kLocationLeft]!;
final int kLogicalAltLeft = kWebLogicalLocationMap['Alt']![kLocationLeft]!;
final int kLogicalMetaLeft = kWebLogicalLocationMap['Meta']![kLocationLeft]!;
const int kLogicalTab = 0x0000000009;
final int kLogicalCapsLock = kWebToLogicalKey['CapsLock']!;
final int kLogicalScrollLock = kWebToLogicalKey['ScrollLock']!;

typedef VoidCallback = void Function();

void main() {
  internalBootstrapBrowserTest(() => testMain);
}

ui.PlatformMessageCallback storeChannelDataTo(List<Map<String, dynamic>> storage) {
  return (String channel, ByteData? data, ui.PlatformMessageResponseCallback? callback) {
    expect(channel, 'flutter/keyevent');
    final Map<String, dynamic>? dataReceived =
        const JSONMessageCodec().decodeMessage(data) as Map<String, dynamic>?;
    expect(dataReceived, isNotNull);
    storage.add(dataReceived!);
  };
}

void testMain() {
  /// Used to save and restore [ui.window.onPlatformMessage] after each test.
  ui.PlatformMessageCallback? savedCallback;
  KeyboardBinding.initInstance();

  setUp(() {
    savedCallback = ui.window.onPlatformMessage;
  });

  tearDown(() {
    ui.window.onPlatformMessage = savedCallback;
    ui.PlatformDispatcher.instance.onKeyData = null;
    KeyboardBinding.instance.reset();
  });

  test('Single key press, repeat, and release', () {
    final List<ui.KeyData> keyDataList = <ui.KeyData>[];
    final List<Map<String, dynamic>> channelData = <Map<String, dynamic>>[];
    ui.window.onPlatformMessage = storeChannelDataTo(channelData);
    ui.PlatformDispatcher.instance.onKeyData = (ui.KeyData data) {
      keyDataList.add(data);
      return true;
    };

    dispatchKeyboardEvent(
      'keydown',
      key: 'a',
      code: 'KeyA',
      keyCode: 0x41,
    );
    expect(keyDataList, hasLength(1));
    expectKeyData(keyDataList.last,
      type: ui.KeyEventType.down,
      physical: kPhysicalKeyA,
      logical: kLogicalKeyA,
      character: 'a',
    );
    expect(channelData, hasLength(1));
    expect(channelData.last, <String, dynamic>{
      'type': 'keydown',
      'keymap': 'web',
      'code': 'KeyA',
      'location': 0,
      'key': 'a',
      'metaState': 0x0,
      'keyCode': 0x41,
    });

    // converter.handleEvent(keyRepeatedDownEvent('KeyA', 'a')
    //   ..timeStamp = 1.5
    //   ..onPreventDefault = onPreventDefault
    // );
    // expectKeyData(keyDataList.last,
    //   timeStamp: const Duration(milliseconds: 1, microseconds: 500),
    //   type: ui.KeyEventType.repeat,
    //   physical: kPhysicalKeyA,
    //   logical: kLogicalKeyA,
    //   character: 'a',
    // );
    // expect(preventedDefault, isFalse);

    // converter.handleEvent(keyRepeatedDownEvent('KeyA', 'a')
    //   ..timeStamp = 1500
    //   ..onPreventDefault = onPreventDefault
    // );
    // expectKeyData(keyDataList.last,
    //   timeStamp: const Duration(seconds: 1, milliseconds: 500),
    //   type: ui.KeyEventType.repeat,
    //   physical: kPhysicalKeyA,
    //   logical: kLogicalKeyA,
    //   character: 'a',
    // );
    // expect(preventedDefault, isFalse);

    // converter.handleEvent(keyUpEvent('KeyA', 'a')
    //   ..timeStamp = 2000.5
    //   ..onPreventDefault = onPreventDefault
    // );
    // expectKeyData(keyDataList.last,
    //   timeStamp: const Duration(seconds: 2, microseconds: 500),
    //   type: ui.KeyEventType.up,
    //   physical: kPhysicalKeyA,
    //   logical: kLogicalKeyA,
    //   character: null,
    // );
    // expect(preventedDefault, isFalse);
  });
}

DomKeyboardEvent dispatchKeyboardEvent(
  String type, {
  DomEventTarget? target,
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
  target ??= domWindow;

  final Function jsKeyboardEvent =
      js_util.getProperty<Function>(domWindow, 'KeyboardEvent');
  final List<dynamic> eventArgs = <dynamic>[
    type,
    <String, dynamic>{
      'key': key,
      'code': code,
      'location': location,
      'repeat': repeat,
      'shiftKey': isShiftPressed,
      'altKey': isAltPressed,
      'ctrlKey': isControlPressed,
      'metaKey': isMetaPressed,
      'keyCode': keyCode,
      'bubbles': true,
      'cancelable': true,
    }
  ];
  final DomKeyboardEvent event = js_util.callConstructor<DomKeyboardEvent>(
    jsKeyboardEvent,
    js_util.jsify(eventArgs) as List<Object?>,
  );
  target.dispatchEvent(event);

  return event;
}

// Flags used for the `activeLocks` argument of expectKeyData.
const int kCapsLock = 0x1;
const int kNumlLock = 0x2;
const int kScrollLock = 0x4;

void expectKeyData(
  ui.KeyData target, {
  required ui.KeyEventType type,
  required int physical,
  required int logical,
  required String? character,
  bool synthesized = false,
}) {
  expect(target.type, type);
  expect(target.physical, physical);
  expect(target.logical, logical);
  expect(target.character, character);
  expect(target.synthesized, synthesized);
}

// typedef FakeAsyncTest = void Function(FakeAsync);

// @isTest
// void testFakeAsync(String description, FakeAsyncTest fn) {
//   test(description, () {
//     FakeAsync().run(fn);
//   });
// }
