/**
 * Windows OSR GPU Native Addon
 * 
 * Imports D3D11 textures from Electron's offscreen rendering shared texture mode.
 * 
 * Based on:
 * - https://github.com/electron/electron/blob/main/shell/browser/osr/README.md
 * - https://github.com/electron/electron/blob/main/spec/fixtures/native-addon/osr-gpu/binding_win.cc
 */

#include "napi_utils.h"

#include <d3d11.h>
#include <d3d11_1.h>
#include <dxgi.h>
#include <dxgi1_2.h>
#include <iostream>
#include <wrl/client.h>

using Microsoft::WRL::ComPtr;

// D3D11 device and context
static ComPtr<ID3D11Device> g_device;
static ComPtr<ID3D11Device1> g_device1;
static ComPtr<ID3D11DeviceContext> g_context;
static ComPtr<ID3D11Texture2D> g_staging_texture;
static UINT g_cached_width = 0;
static UINT g_cached_height = 0;

/**
 * Initialize D3D11 device
 */
Napi::Value InitContext(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  
  D3D_FEATURE_LEVEL featureLevel;
  HRESULT hr = D3D11CreateDevice(
    nullptr,
    D3D_DRIVER_TYPE_HARDWARE,
    nullptr,
    D3D11_CREATE_DEVICE_BGRA_SUPPORT,
    nullptr,
    0,
    D3D11_SDK_VERSION,
    &g_device,
    &featureLevel,
    &g_context
  );
  
  if (FAILED(hr)) {
    Napi::Error::New(env, "Failed to create D3D11 device").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  hr = g_device.As(&g_device1);
  if (FAILED(hr)) {
    Napi::Error::New(env, "Failed to get ID3D11Device1 interface").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  std::cout << "[osr-gpu] D3D11 device initialized" << std::endl;
  return Napi::Boolean::New(env, true);
}

/**
 * Copy shared texture data to CPU buffer
 */
Napi::Value CopyTextureToBuffer(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  
  if (info.Length() < 1 || !info[0].IsObject()) {
    Napi::TypeError::New(env, "Expected textureInfo object").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  if (!g_device1) {
    Napi::Error::New(env, "D3D11 device not initialized. Call initContext() first.").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  Napi::Object textureInfo = info[0].As<Napi::Object>();
  
  // Get the shared texture handle buffer
  if (!textureInfo.Has("sharedTextureHandle")) {
    Napi::Error::New(env, "Missing sharedTextureHandle in textureInfo").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  Napi::Value handleValue = textureInfo.Get("sharedTextureHandle");
  if (!handleValue.IsBuffer()) {
    Napi::Error::New(env, "sharedTextureHandle must be a Buffer").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  Napi::Buffer<uint8_t> handleBuffer = handleValue.As<Napi::Buffer<uint8_t>>();
  HANDLE handle = *reinterpret_cast<HANDLE*>(handleBuffer.Data());
  
  // Open the shared texture
  ComPtr<ID3D11Texture2D> shared_texture;
  HRESULT hr = g_device1->OpenSharedResource1(handle, IID_PPV_ARGS(&shared_texture));
  if (FAILED(hr)) {
    Napi::Error::New(env, "Failed to open shared resource").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  // Get texture description
  D3D11_TEXTURE2D_DESC desc;
  shared_texture->GetDesc(&desc);
  
  // Create or recreate staging texture if size changed
  if (!g_staging_texture || g_cached_width != desc.Width || g_cached_height != desc.Height) {
    g_staging_texture.Reset();
    
    desc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
    desc.Usage = D3D11_USAGE_STAGING;
    desc.BindFlags = 0;
    desc.MiscFlags = 0;
    
    hr = g_device->CreateTexture2D(&desc, nullptr, &g_staging_texture);
    if (FAILED(hr)) {
      Napi::Error::New(env, "Failed to create staging texture").ThrowAsJavaScriptException();
      return env.Undefined();
    }
    
    g_cached_width = desc.Width;
    g_cached_height = desc.Height;
  }
  
  // Copy to staging texture
  g_context->CopyResource(g_staging_texture.Get(), shared_texture.Get());
  
  // Map the staging texture for CPU read
  D3D11_MAPPED_SUBRESOURCE mapped;
  hr = g_context->Map(g_staging_texture.Get(), 0, D3D11_MAP_READ, 0, &mapped);
  if (FAILED(hr)) {
    Napi::Error::New(env, "Failed to map staging texture").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  // Create output buffer (RGBA, 4 bytes per pixel)
  size_t bufferSize = desc.Width * desc.Height * 4;
  Napi::Buffer<uint8_t> outputBuffer = Napi::Buffer<uint8_t>::New(env, bufferSize);
  uint8_t* output = outputBuffer.Data();
  uint8_t* source = static_cast<uint8_t*>(mapped.pData);
  
  // Copy and convert BGRA -> RGBA
  for (UINT y = 0; y < desc.Height; y++) {
    uint8_t* srcRow = source + y * mapped.RowPitch;
    uint8_t* dstRow = output + y * desc.Width * 4;
    
    for (UINT x = 0; x < desc.Width; x++) {
      // BGRA -> RGBA
      dstRow[x * 4 + 0] = srcRow[x * 4 + 2]; // R
      dstRow[x * 4 + 1] = srcRow[x * 4 + 1]; // G
      dstRow[x * 4 + 2] = srcRow[x * 4 + 0]; // B
      dstRow[x * 4 + 3] = srcRow[x * 4 + 3]; // A
    }
  }
  
  g_context->Unmap(g_staging_texture.Get(), 0);
  
  // Return result object with buffer and dimensions
  Napi::Object result = Napi::Object::New(env);
  result.Set("data", outputBuffer);
  result.Set("width", Napi::Number::New(env, desc.Width));
  result.Set("height", Napi::Number::New(env, desc.Height));
  
  return result;
}

/**
 * Cleanup
 */
Napi::Value DestroyContext(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  
  g_staging_texture.Reset();
  g_context.Reset();
  g_device1.Reset();
  g_device.Reset();
  g_cached_width = 0;
  g_cached_height = 0;
  
  std::cout << "[osr-gpu] D3D11 device destroyed" << std::endl;
  return Napi::Boolean::New(env, true);
}

/**
 * Not implemented for Windows (no GL textures)
 */
Napi::Value ImportTexture(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  Napi::Error::New(env, "importTexture not supported on Windows - use copyTextureToBuffer instead").ThrowAsJavaScriptException();
  return env.Undefined();
}

Napi::Value ReleaseTexture(const Napi::CallbackInfo& info) {
  return info.Env().Undefined();
}

/**
 * Module initialization
 */
Napi::Object Init(Napi::Env env, Napi::Object exports) {
  exports.Set("initContext", Napi::Function::New(env, InitContext));
  exports.Set("importTexture", Napi::Function::New(env, ImportTexture));
  exports.Set("copyTextureToBuffer", Napi::Function::New(env, CopyTextureToBuffer));
  exports.Set("releaseTexture", Napi::Function::New(env, ReleaseTexture));
  exports.Set("destroyContext", Napi::Function::New(env, DestroyContext));
  return exports;
}

NODE_API_MODULE(osr_gpu, Init)
