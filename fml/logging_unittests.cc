// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <signal.h>

#include "flutter/fml/build_config.h"
#include "flutter/fml/log_settings.h"
#include "flutter/fml/logging.h"
#include "gtest/gtest.h"

namespace fml {
namespace testing {

#ifndef OS_FUCHSIA
class MakeSureFmlLogDoesNotSegfaultWhenStaticallyCalled {
 public:
  MakeSureFmlLogDoesNotSegfaultWhenStaticallyCalled() {
    SegfaultCatcher catcher;
    // If this line causes a segfault, FML is using a method of logging that is
    // not safe from static initialization on your platform.
    FML_LOG(INFO)
        << "This log exists to verify that static logging from FML works.";
  }

 private:
  struct SegfaultCatcher {
    typedef void (*sighandler_t)(int);

    SegfaultCatcher() {
      handler = ::signal(SIGSEGV, SegfaultHandler);
      FML_CHECK(handler != SIG_ERR);
    }

    ~SegfaultCatcher() { FML_CHECK(::signal(SIGSEGV, handler) != SIG_ERR); }

    static void SegfaultHandler(int signal) {
      fprintf(stderr,
              "FML failed to handle logging from static initialization.\n");
      exit(signal);
    }

    sighandler_t handler;
  };
};

static MakeSureFmlLogDoesNotSegfaultWhenStaticallyCalled fml_log_static_check_;
#endif  // !defined(OS_FUCHSIA)

int UnreachableScopeWithoutReturnDoesNotMakeCompilerMad() {
  KillProcess();
  // return 0; <--- Missing but compiler is fine.
}

int UnreachableScopeWithMacroWithoutReturnDoesNotMakeCompilerMad() {
  FML_UNREACHABLE();
  // return 0; <--- Missing but compiler is fine.
}

TEST(LoggingTest, UnreachableKillProcess) {
  ::testing::FLAGS_gtest_death_test_style = "threadsafe";
  ASSERT_DEATH(KillProcess(), "");
}

TEST(LoggingTest, UnreachableKillProcessWithMacro) {
  ::testing::FLAGS_gtest_death_test_style = "threadsafe";
  ASSERT_DEATH({ FML_UNREACHABLE(); }, "");
}

}  // namespace testing
}  // namespace fml
