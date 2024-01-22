// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef FLUTTER_SHELL_PLATFORM_WINDOWS_TESTING_MOCK_ANGLE_SURFACE_MANAGER_H_
#define FLUTTER_SHELL_PLATFORM_WINDOWS_TESTING_MOCK_ANGLE_SURFACE_MANAGER_H_

#include "flutter/fml/macros.h"
#include "flutter/shell/platform/windows/angle_surface_manager.h"
#include "gmock/gmock.h"

namespace flutter {
namespace testing {

/// Mock for the |AngleSurfaceManager| base class.
class MockAngleSurfaceManager : public AngleSurfaceManager {
 public:
  MockAngleSurfaceManager() : AngleSurfaceManager(false) {}

  MOCK_METHOD(bool, CreateSurface, (int64_t, HWND, EGLint, EGLint), (override));
  MOCK_METHOD(void,
              ResizeSurface,
              (int64_t, HWND, EGLint, EGLint, bool),
              (override));
  MOCK_METHOD(void, DestroySurface, (int64_t), (override));

  MOCK_METHOD(bool, MakeRenderContextCurrent, (), (override));
  MOCK_METHOD(bool, MakeSurfaceCurrent, (int64_t), (override));
  MOCK_METHOD(bool, ClearCurrent, (), (override));
  MOCK_METHOD(void, SetVSyncEnabled, (int64_t, bool), (override));

 private:
  FML_DISALLOW_COPY_AND_ASSIGN(MockAngleSurfaceManager);
};

}  // namespace testing
}  // namespace flutter

#endif  // FLUTTER_SHELL_PLATFORM_WINDOWS_TESTING_MOCK_ANGLE_SURFACE_MANAGER_H_
