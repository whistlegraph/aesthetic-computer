#pragma once

#include <d3dcompiler.h>
#include <wrl/client.h>
#include <algorithm>
#include <chrono>
#include <string>
#include <vector>

namespace ac::xbox {
constexpr std::size_t kLiveShaderLimit = 64 * 1024;
constexpr char kLiveShaderReset[] = "// AC_RESET_POST_SHADER\n";

struct LiveShaderRequest {
  std::string id, source;
  bool reset = false;
};

inline LiveShaderRequest parse_live_shader(const std::string& text) {
  LiveShaderRequest request{"local", text};
  constexpr char prefix[] = "// AC_LIVE_SHADER ";
  if (text.compare(0, sizeof(prefix) - 1, prefix) == 0) {
    const auto newline = text.find('\n');
    if (newline != std::string::npos && newline < 100) {
      const auto id = text.substr(sizeof(prefix) - 1, newline - (sizeof(prefix) - 1));
      if (!id.empty() && id.find_first_not_of("0123456789abcdef-") == std::string::npos) {
        request.id = id;
        request.source = text.substr(newline + 1);
      }
    }
  }
  request.reset = request.source == kLiveShaderReset;
  return request;
}

struct LiveShaderResult {
  std::vector<unsigned char> bytes;
  std::string error;
  double milliseconds = 0;
};

// Shared by the Xbox dev loader and Windows preflight. The compiler sees
// only this source string; #include cannot open files or fetch resources.
inline LiveShaderResult compile_live_post_shader(const std::string& source) {
  LiveShaderResult result;
  if (source.empty() || source.size() > kLiveShaderLimit) {
    result.error = "HLSL must contain 1..65536 bytes";
    return result;
  }
  const auto started = std::chrono::steady_clock::now();
  Microsoft::WRL::ComPtr<ID3DBlob> code, errors;
  const auto hr = D3DCompile(source.data(), source.size(), "live-post.hlsl",
    nullptr, nullptr, "main", "ps_5_0", D3DCOMPILE_ENABLE_STRICTNESS |
    D3DCOMPILE_OPTIMIZATION_LEVEL1, 0, &code, &errors);
  result.milliseconds = std::chrono::duration<double, std::milli>(
    std::chrono::steady_clock::now() - started).count();
  if (FAILED(hr)) {
    result.error = errors ? std::string(static_cast<const char*>(errors->GetBufferPointer()),
      errors->GetBufferSize()) : "D3DCompile failed: " + std::to_string(hr);
    while (!result.error.empty() && result.error.back() == '\0') result.error.pop_back();
    result.error.resize((std::min)(result.error.size(), std::size_t{4096}));
    return result;
  }
  const auto* data = static_cast<const unsigned char*>(code->GetBufferPointer());
  result.bytes.assign(data, data + code->GetBufferSize());
  return result;
}
} // namespace ac::xbox
