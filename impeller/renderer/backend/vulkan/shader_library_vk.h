// Copyright 2013 The Flutter Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#pragma once

#include "flutter/fml/macros.h"
#include "impeller/base/comparable.h"
#include "impeller/renderer/backend/vulkan/vk.h"
#include "impeller/renderer/shader_key.h"
#include "impeller/renderer/shader_library.h"

namespace impeller {

class ShaderLibraryVK final : public ShaderLibrary {
 public:
  // |ShaderLibrary|
  ~ShaderLibraryVK() override;

  // |ShaderLibrary|
  bool IsValid() const override;

 private:
  friend class ContextVK;
  const UniqueID library_id_;
  ShaderFunctionMap functions_;
  bool is_valid_ = false;

  ShaderLibraryVK(
      const vk::Device& device,
      const std::vector<std::shared_ptr<fml::Mapping>>& shader_libraries_data);

  // |ShaderLibrary|
  std::shared_ptr<const ShaderFunction> GetFunction(std::string_view name,
                                                    ShaderStage stage) override;

  FML_DISALLOW_COPY_AND_ASSIGN(ShaderLibraryVK);
};

}  // namespace impeller
