#include "../LivePostShader.hpp"
#include <d3d11.h>
#include <cassert>
#include <fstream>
#include <iterator>
#include <iostream>

int main(int argc, char** argv) {
  using namespace ac::xbox;
  const auto request = parse_live_shader("// AC_LIVE_SHADER abc-123\nfloat4 main():SV_TARGET{return 1;}");
  assert(request.id == "abc-123" && !request.reset);
  assert(parse_live_shader("// AC_LIVE_SHADER abc-124\n" + std::string(kLiveShaderReset)).reset);
  assert(!compile_live_post_shader("").error.empty());
  assert(!compile_live_post_shader(std::string(kLiveShaderLimit + 1, ' ')).error.empty());
  assert(!compile_live_post_shader("#include \"private.hlsl\"\n" + request.source).error.empty());
  assert(!compile_live_post_shader("this is invalid HLSL").error.empty());
  auto compiled = compile_live_post_shader(request.source);
  assert(compiled.error.empty() && !compiled.bytes.empty());
  Microsoft::WRL::ComPtr<ID3D11Device> device;
  assert(SUCCEEDED(D3D11CreateDevice(nullptr, D3D_DRIVER_TYPE_WARP, nullptr,
    0, nullptr, 0, D3D11_SDK_VERSION, &device, nullptr, nullptr)));
  Microsoft::WRL::ComPtr<ID3D11PixelShader> shader;
  assert(SUCCEEDED(device->CreatePixelShader(compiled.bytes.data(), compiled.bytes.size(), nullptr, &shader)));
  assert(argc == 2);
  std::ifstream file(argv[1]);
  const std::string source((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
  compiled = compile_live_post_shader(source);
  if (!compiled.error.empty()) std::cerr << compiled.error << '\n';
  assert(compiled.error.empty() && !compiled.bytes.empty());
  shader.Reset();
  assert(SUCCEEDED(device->CreatePixelShader(compiled.bytes.data(), compiled.bytes.size(), nullptr, &shader)));
  std::cout << "live HLSL: compile, errors, limits, reset, packaged post shader and D3D11 creation OK\n";
}
