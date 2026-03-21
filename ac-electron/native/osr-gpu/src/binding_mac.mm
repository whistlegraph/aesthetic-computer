/**
 * macOS OSR GPU Native Addon
 * 
 * Imports IOSurface textures from Electron's offscreen rendering shared texture mode
 * and provides them as WebGL-compatible textures.
 * 
 * Based on:
 * - https://github.com/electron/electron/blob/main/shell/browser/osr/README.md
 * - CEF OSR implementation
 */

#import <Cocoa/Cocoa.h>
#import <OpenGL/OpenGL.h>
#import <OpenGL/gl.h>
#import <IOSurface/IOSurface.h>

#include "napi_utils.h"
#include <iostream>
#include <unordered_map>

// Cache for opened textures
static CGLContextObj g_cgl_context = nullptr;
static std::unordered_map<IOSurfaceRef, GLuint> g_texture_cache;

/**
 * Initialize OpenGL context for texture operations
 */
Napi::Value InitContext(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  
  // Create a pixel format
  CGLPixelFormatAttribute attrs[] = {
    kCGLPFAAccelerated,
    kCGLPFAOpenGLProfile, (CGLPixelFormatAttribute) kCGLOGLPVersion_3_2_Core,
    (CGLPixelFormatAttribute) 0
  };
  
  CGLPixelFormatObj pix;
  GLint npix;
  CGLError err = CGLChoosePixelFormat(attrs, &pix, &npix);
  if (err != kCGLNoError) {
    Napi::Error::New(env, "Failed to choose pixel format").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  err = CGLCreateContext(pix, nullptr, &g_cgl_context);
  CGLReleasePixelFormat(pix);
  
  if (err != kCGLNoError) {
    Napi::Error::New(env, "Failed to create CGL context").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  CGLSetCurrentContext(g_cgl_context);
  
  std::cout << "[osr-gpu] OpenGL context initialized" << std::endl;
  return Napi::Boolean::New(env, true);
}

/**
 * Import IOSurface texture from Electron's shared texture handle
 * 
 * @param textureInfo - Object containing sharedTextureHandle buffer and dimensions
 * @returns Object with textureId (GLuint) for use in WebGL
 */
Napi::Value ImportTexture(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  
  if (info.Length() < 1 || !info[0].IsObject()) {
    Napi::TypeError::New(env, "Expected textureInfo object").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  if (!g_cgl_context) {
    Napi::Error::New(env, "OpenGL context not initialized. Call initContext() first.").ThrowAsJavaScriptException();
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
  
  // Extract IOSurfaceRef from buffer
  IOSurfaceRef io_surface = *reinterpret_cast<IOSurfaceRef*>(handleBuffer.Data());
  
  if (!io_surface) {
    Napi::Error::New(env, "Invalid IOSurface handle").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  CGLSetCurrentContext(g_cgl_context);
  
  // Get dimensions from IOSurface
  GLsizei width = (GLsizei)IOSurfaceGetWidth(io_surface);
  GLsizei height = (GLsizei)IOSurfaceGetHeight(io_surface);
  
  // Create a new texture bound to the IOSurface
  GLuint io_surface_tex;
  glGenTextures(1, &io_surface_tex);
  glEnable(GL_TEXTURE_RECTANGLE_ARB);
  glBindTexture(GL_TEXTURE_RECTANGLE_ARB, io_surface_tex);
  
  // Bind IOSurface to texture
  CGLError err = CGLTexImageIOSurface2D(
    g_cgl_context,
    GL_TEXTURE_RECTANGLE_ARB,
    GL_RGBA8,
    width,
    height,
    GL_BGRA,
    GL_UNSIGNED_INT_8_8_8_8_REV,
    io_surface,
    0
  );
  
  if (err != kCGLNoError) {
    glDeleteTextures(1, &io_surface_tex);
    Napi::Error::New(env, "Failed to bind IOSurface to texture").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_RECTANGLE_ARB, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glBindTexture(GL_TEXTURE_RECTANGLE_ARB, 0);
  
  // Return texture info
  Napi::Object result = Napi::Object::New(env);
  result.Set("textureId", Napi::Number::New(env, io_surface_tex));
  result.Set("width", Napi::Number::New(env, width));
  result.Set("height", Napi::Number::New(env, height));
  result.Set("target", Napi::String::New(env, "TEXTURE_RECTANGLE_ARB"));
  
  return result;
}

/**
 * Copy IOSurface texture data to a CPU buffer (for WebGL compatibility)
 * 
 * This is needed because WebGL can't directly use GL_TEXTURE_RECTANGLE_ARB,
 * so we copy the data to a regular 2D texture format.
 * 
 * @param textureInfo - Object containing sharedTextureHandle buffer
 * @returns Uint8Array with RGBA pixel data
 */
Napi::Value CopyTextureToBuffer(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  
  if (info.Length() < 1 || !info[0].IsObject()) {
    Napi::TypeError::New(env, "Expected textureInfo object").ThrowAsJavaScriptException();
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
  
  // Extract IOSurfaceRef from buffer
  IOSurfaceRef io_surface = *reinterpret_cast<IOSurfaceRef*>(handleBuffer.Data());
  
  if (!io_surface) {
    Napi::Error::New(env, "Invalid IOSurface handle").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  // Lock the IOSurface for CPU access
  IOSurfaceLock(io_surface, kIOSurfaceLockReadOnly, nullptr);
  
  size_t width = IOSurfaceGetWidth(io_surface);
  size_t height = IOSurfaceGetHeight(io_surface);
  size_t bytesPerRow = IOSurfaceGetBytesPerRow(io_surface);
  void* baseAddress = IOSurfaceGetBaseAddress(io_surface);
  
  // Create output buffer (RGBA, 4 bytes per pixel)
  size_t bufferSize = width * height * 4;
  Napi::Buffer<uint8_t> outputBuffer = Napi::Buffer<uint8_t>::New(env, bufferSize);
  uint8_t* output = outputBuffer.Data();
  uint8_t* source = static_cast<uint8_t*>(baseAddress);
  
  // Copy and convert BGRA -> RGBA
  for (size_t y = 0; y < height; y++) {
    uint8_t* srcRow = source + y * bytesPerRow;
    uint8_t* dstRow = output + y * width * 4;
    
    for (size_t x = 0; x < width; x++) {
      // BGRA -> RGBA
      dstRow[x * 4 + 0] = srcRow[x * 4 + 2]; // R
      dstRow[x * 4 + 1] = srcRow[x * 4 + 1]; // G
      dstRow[x * 4 + 2] = srcRow[x * 4 + 0]; // B
      dstRow[x * 4 + 3] = srcRow[x * 4 + 3]; // A
    }
  }
  
  IOSurfaceUnlock(io_surface, kIOSurfaceLockReadOnly, nullptr);
  
  // Return result object with buffer and dimensions
  Napi::Object result = Napi::Object::New(env);
  result.Set("data", outputBuffer);
  result.Set("width", Napi::Number::New(env, width));
  result.Set("height", Napi::Number::New(env, height));
  
  return result;
}

/**
 * Release/delete a texture
 */
Napi::Value ReleaseTexture(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  
  if (info.Length() < 1 || !info[0].IsNumber()) {
    Napi::TypeError::New(env, "Expected textureId number").ThrowAsJavaScriptException();
    return env.Undefined();
  }
  
  GLuint textureId = info[0].As<Napi::Number>().Uint32Value();
  
  if (g_cgl_context) {
    CGLSetCurrentContext(g_cgl_context);
    glDeleteTextures(1, &textureId);
  }
  
  return Napi::Boolean::New(env, true);
}

/**
 * Cleanup and destroy context
 */
Napi::Value DestroyContext(const Napi::CallbackInfo& info) {
  Napi::Env env = info.Env();
  
  if (g_cgl_context) {
    CGLSetCurrentContext(nullptr);
    CGLDestroyContext(g_cgl_context);
    g_cgl_context = nullptr;
    std::cout << "[osr-gpu] OpenGL context destroyed" << std::endl;
  }
  
  return Napi::Boolean::New(env, true);
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
