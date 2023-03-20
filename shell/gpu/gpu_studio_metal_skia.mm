// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "flutter/shell/gpu/gpu_studio_metal_skia.h"

#import <Metal/Metal.h>
#import <QuartzCore/QuartzCore.h>

#include <utility>

#include "flutter/common/graphics/persistent_cache.h"
#include "flutter/fml/make_copyable.h"
#include "flutter/fml/platform/darwin/cf_utils.h"
#include "flutter/fml/platform/darwin/scoped_nsobject.h"
#include "flutter/fml/trace_event.h"
#include "flutter/shell/gpu/gpu_surface_metal_delegate.h"
#include "third_party/skia/include/core/SkCanvas.h"
#include "third_party/skia/include/core/SkColorSpace.h"
#include "third_party/skia/include/core/SkColorType.h"
#include "third_party/skia/include/core/SkMatrix.h"
#include "third_party/skia/include/core/SkRect.h"
#include "third_party/skia/include/core/SkRefCnt.h"
#include "third_party/skia/include/core/SkSize.h"
#include "third_party/skia/include/ports/SkCFObject.h"

static_assert(!__has_feature(objc_arc), "ARC must be disabled.");

namespace flutter {

GPUStudioMetalSkia::GPUStudioMetalSkia(GPUSurfaceMetalDelegate* delegate,
                                       sk_sp<GrDirectContext> context)
    : delegate_(delegate), context_(std::move(context)) {}

GPUStudioMetalSkia::~GPUStudioMetalSkia() = default;

// |Studio|
bool GPUStudioMetalSkia::IsValid() {
  return context_ != nullptr;
}

void GPUStudioMetalSkia::PrecompileKnownSkSLsIfNecessary() {
  auto* current_context = GetContext();
  if (current_context == precompiled_sksl_context_) {
    // Known SkSLs have already been prepared in this context.
    return;
  }
  precompiled_sksl_context_ = current_context;
  flutter::PersistentCache::GetCacheForProcess()->PrecompileKnownSkSLs(precompiled_sksl_context_);
}

// |Studio|
GrDirectContext* GPUStudioMetalSkia::GetContext() {
  return context_.get();
}

// |Studio|
std::unique_ptr<GLContextResult> GPUStudioMetalSkia::MakeRenderContextCurrent() {
  // A context may either be necessary to render to the surface or to snapshot an offscreen
  // surface. Either way, SkSL precompilation must be attempted.
  PrecompileKnownSkSLsIfNecessary();

  // This backend has no such concept.
  return std::make_unique<GLContextDefaultResult>(true);
}

bool GPUStudioMetalSkia::AllowsDrawingWhenGpuDisabled() const {
  return delegate_->AllowsDrawingWhenGpuDisabled();
}

}  // namespace flutter
