// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

import 'dart:convert' as convert;
import 'dart:ffi' as ffi show Abi;
import 'dart:io' as io;

import 'package:engine_build_configs/engine_build_configs.dart';
import 'package:engine_repo_tools/engine_repo_tools.dart';
import 'package:engine_tool/src/commands/command_runner.dart';
import 'package:engine_tool/src/environment.dart';
import 'package:litetest/litetest.dart';
import 'package:logging/logging.dart' as log;
import 'package:platform/platform.dart';

import 'fixtures.dart' as fixtures;
import 'utils.dart';

void main() {
  final Engine engine;
  try {
    engine = Engine.findWithin();
  } catch (e) {
    io.stderr.writeln(e);
    io.exitCode = 1;
    return;
  }

  final BuilderConfig linuxTestConfig = BuilderConfig.fromJson(
    path: 'ci/builders/linux_test_config.json',
    map: convert.jsonDecode(fixtures.testConfig('Linux', Platform.linux))
        as Map<String, Object?>,
  );

  final BuilderConfig macTestConfig = BuilderConfig.fromJson(
    path: 'ci/builders/mac_test_config.json',
    map: convert.jsonDecode(fixtures.testConfig('Mac-12', Platform.macOS))
        as Map<String, Object?>,
  );

  final BuilderConfig winTestConfig = BuilderConfig.fromJson(
    path: 'ci/builders/win_test_config.json',
    map: convert.jsonDecode(fixtures.testConfig('Windows-11', Platform.windows))
        as Map<String, Object?>,
  );

  final Map<String, BuilderConfig> configs = <String, BuilderConfig>{
    'linux_test_config': linuxTestConfig,
    'linux_test_config2': linuxTestConfig,
    'mac_test_config': macTestConfig,
    'win_test_config': winTestConfig,
  };

  List<String> stringsFromLogs(List<log.LogRecord> logs) {
    return logs.map((log.LogRecord r) => r.message).toList();
  }

  final List<CannedProcess> cannedProcesses = <CannedProcess>[
    CannedProcess((List<String> command) => command.contains('desc'),
        stdout: fixtures.gnDescOutput()),
  ];

  test('query command returns builds for the host platform.', () async {
    final TestEnvironment testEnvironment = TestEnvironment(engine,
        abi: ffi.Abi.linuxX64, cannedProcesses: cannedProcesses);
    final Environment env = testEnvironment.environment;
    final ToolCommandRunner runner = ToolCommandRunner(
      environment: env,
      configs: configs,
    );
    final int result = await runner.run(<String>[
      'query',
      'builders',
    ]);
    expect(result, equals(0));
    expect(
      stringsFromLogs(env.logger.testLogs),
      equals(<String>[
        'Add --verbose to see detailed information about each builder\n',
        '\n',
        '"linux_test_config" builder:\n',
        '   "ci/build_name" config\n',
        '   "linux/host_debug" config\n',
        '   "linux/android_debug_arm64" config\n',
        '   "ci/android_debug_rbe_arm64" config\n',
        '"linux_test_config2" builder:\n',
        '   "ci/build_name" config\n',
        '   "linux/host_debug" config\n',
        '   "linux/android_debug_arm64" config\n',
        '   "ci/android_debug_rbe_arm64" config\n',
      ]),
    );
  });

  test('query command with --builder returns only from the named builder.',
      () async {
    final TestEnvironment testEnvironment = TestEnvironment(engine,
        abi: ffi.Abi.linuxX64, cannedProcesses: cannedProcesses);
    final Environment env = testEnvironment.environment;
    final ToolCommandRunner runner = ToolCommandRunner(
      environment: env,
      configs: configs,
    );
    final int result = await runner.run(<String>[
      'query',
      'builders',
      '--builder',
      'linux_test_config',
    ]);
    expect(result, equals(0));
    expect(
        stringsFromLogs(env.logger.testLogs),
        equals(<String>[
          'Add --verbose to see detailed information about each builder\n',
          '\n',
          '"linux_test_config" builder:\n',
          '   "ci/build_name" config\n',
          '   "linux/host_debug" config\n',
          '   "linux/android_debug_arm64" config\n',
          '   "ci/android_debug_rbe_arm64" config\n',
        ]));
  });

  test('query command with --all returns all builds.', () async {
    final TestEnvironment testEnvironment = TestEnvironment(engine,
        abi: ffi.Abi.linuxX64, cannedProcesses: cannedProcesses);
    final Environment env = testEnvironment.environment;
    final ToolCommandRunner runner = ToolCommandRunner(
      environment: env,
      configs: configs,
    );
    final int result = await runner.run(<String>[
      'query',
      'builders',
      '--all',
    ]);
    expect(result, equals(0));
    expect(
      env.logger.testLogs.length,
      equals(30),
    );
  });

  test('query targets', () async {
    final TestEnvironment testEnvironment = TestEnvironment(engine,
        abi: ffi.Abi.linuxX64, cannedProcesses: cannedProcesses);
    final Environment env = testEnvironment.environment;
    final ToolCommandRunner runner = ToolCommandRunner(
      environment: env,
      configs: configs,
    );
    final int result = await runner.run(<String>[
      'query',
      'targets',
    ]);
    expect(result, equals(0));
    expect(
      env.logger.testLogs.length,
      equals(3),
    );
    expect(env.logger.testLogs[0].message,
        startsWith('//flutter/display_list:display_list_unittests'));
  });
}
