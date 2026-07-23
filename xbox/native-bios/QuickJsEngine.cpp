#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <memory>
#include <string>
#include "QuickJsEngine.hpp"
extern "C" {
#include "third_party/quickjs-ng/quickjs.h"
}

#include <stdexcept>

namespace ac::xbox {
namespace {
struct CallScope { Api* api; };

JSValue Wipe(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  int32_t r = 0, g = 0, b = 0;
  if (!scope || !scope->api || argc < 3 || JS_ToInt32(context, &r, argv[0]) ||
      JS_ToInt32(context, &g, argv[1]) || JS_ToInt32(context, &b, argv[2]))
    return JS_EXCEPTION;
  scope->api->graphics.wipe({static_cast<uint8_t>(r), static_cast<uint8_t>(g),
                             static_cast<uint8_t>(b), 255});
  return JS_UNDEFINED;
}

JSValue Synth(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  double frequency = 440.0, duration = .05;
  if (!scope || !scope->api || argc < 1 || JS_ToFloat64(context, &frequency, argv[0]))
    return JS_EXCEPTION;
  if (argc > 1) JS_ToFloat64(context, &duration, argv[1]);
  SynthVoice voice; voice.frequency_hz = static_cast<float>(frequency);
  voice.duration_s = static_cast<float>(duration);
  scope->api->sound.synth(voice);
  return JS_UNDEFINED;
}

JSValue Oscillator(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  double frequency = 220.0, volume = .12;
  if (!scope || !scope->api || argc < 1 || JS_ToFloat64(context, &frequency, argv[0]))
    return JS_EXCEPTION;
  if (argc > 1 && JS_ToFloat64(context, &volume, argv[1])) return JS_EXCEPTION;
  scope->api->sound.oscillator(static_cast<float>(frequency), static_cast<float>(volume));
  return JS_UNDEFINED;
}

JSValue OscillatorStop(JSContext* context, JSValueConst, int, JSValueConst*) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api) return JS_EXCEPTION;
  scope->api->sound.oscillator_stop();
  return JS_UNDEFINED;
}

JSValue Write(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api || argc < 1) return JS_EXCEPTION;
  const char* value = JS_ToCString(context, argv[0]);
  if (!value) return JS_EXCEPTION;
  double x = 80, y = 80, size = 64;
  if (argc > 1) JS_ToFloat64(context, &x, argv[1]);
  if (argc > 2) JS_ToFloat64(context, &y, argv[2]);
  if (argc > 3) JS_ToFloat64(context, &size, argv[3]);
  int32_t r = 255, g = 255, b = 255;
  if (argc > 4) JS_ToInt32(context, &r, argv[4]);
  if (argc > 5) JS_ToInt32(context, &g, argv[5]);
  if (argc > 6) JS_ToInt32(context, &b, argv[6]);
  scope->api->graphics.write({value, static_cast<float>(x), static_cast<float>(y),
                              static_cast<float>(size), {static_cast<uint8_t>(r),
                              static_cast<uint8_t>(g), static_cast<uint8_t>(b), 255}});
  JS_FreeCString(context, value);
  return JS_UNDEFINED;
}

JSValue Box(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  double x = 0, y = 0, width = 0, height = 0;
  int32_t r = 255, g = 255, b = 255;
  if (!scope || !scope->api || argc < 4 || JS_ToFloat64(context, &x, argv[0]) ||
      JS_ToFloat64(context, &y, argv[1]) || JS_ToFloat64(context, &width, argv[2]) ||
      JS_ToFloat64(context, &height, argv[3])) return JS_EXCEPTION;
  if (argc > 4) JS_ToInt32(context, &r, argv[4]);
  if (argc > 5) JS_ToInt32(context, &g, argv[5]);
  if (argc > 6) JS_ToInt32(context, &b, argv[6]);
  scope->api->graphics.box({static_cast<float>(x), static_cast<float>(y),
    static_cast<float>(width), static_cast<float>(height),
    {static_cast<uint8_t>(r), static_cast<uint8_t>(g), static_cast<uint8_t>(b), 255}});
  return JS_UNDEFINED;
}

JSValue Line(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  double x1 = 0, y1 = 0, x2 = 0, y2 = 0, width = 1;
  int32_t r = 255, g = 255, b = 255;
  if (!scope || !scope->api || argc < 4 || JS_ToFloat64(context, &x1, argv[0]) ||
      JS_ToFloat64(context, &y1, argv[1]) || JS_ToFloat64(context, &x2, argv[2]) ||
      JS_ToFloat64(context, &y2, argv[3])) return JS_EXCEPTION;
  if (argc > 4) JS_ToFloat64(context, &width, argv[4]);
  if (argc > 5) JS_ToInt32(context, &r, argv[5]);
  if (argc > 6) JS_ToInt32(context, &g, argv[6]);
  if (argc > 7) JS_ToInt32(context, &b, argv[7]);
  scope->api->graphics.line({static_cast<float>(x1), static_cast<float>(y1),
    static_cast<float>(x2), static_cast<float>(y2), static_cast<float>(width),
    {static_cast<uint8_t>(r), static_cast<uint8_t>(g), static_cast<uint8_t>(b), 255}});
  return JS_UNDEFINED;
}

JSValue TriangleFill(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  double x1 = 0, y1 = 0, x2 = 0, y2 = 0, x3 = 0, y3 = 0;
  int32_t r = 255, g = 255, b = 255;
  if (!scope || !scope->api || argc < 6 ||
      JS_ToFloat64(context, &x1, argv[0]) || JS_ToFloat64(context, &y1, argv[1]) ||
      JS_ToFloat64(context, &x2, argv[2]) || JS_ToFloat64(context, &y2, argv[3]) ||
      JS_ToFloat64(context, &x3, argv[4]) || JS_ToFloat64(context, &y3, argv[5]))
    return JS_EXCEPTION;
  if (argc > 6) JS_ToInt32(context, &r, argv[6]);
  if (argc > 7) JS_ToInt32(context, &g, argv[7]);
  if (argc > 8) JS_ToInt32(context, &b, argv[8]);
  const std::array<double, 6> coordinates{x1, y1, x2, y2, x3, y3};
  if (!std::all_of(coordinates.begin(), coordinates.end(), [](double value) {
        return std::isfinite(value) && std::abs(value) <= 32768.0;
      })) return JS_ThrowRangeError(context, "invalid triangle coordinates");
  scope->api->graphics.triangle({static_cast<float>(x1), static_cast<float>(y1),
    0, static_cast<float>(x2), static_cast<float>(y2), 0, static_cast<float>(x3),
    static_cast<float>(y3), 0, {static_cast<uint8_t>(r), static_cast<uint8_t>(g),
    static_cast<uint8_t>(b), 255}});
  return JS_UNDEFINED;
}

JSValue Triangle3d(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  std::array<double, 9> value{};
  if (!scope || !scope->api || argc < 9) return JS_EXCEPTION;
  for (int index = 0; index < 9; ++index)
    if (JS_ToFloat64(context, &value[index], argv[index])) return JS_EXCEPTION;
  if (!std::all_of(value.begin(), value.end(), [](double item) {
        return std::isfinite(item) && std::abs(item) <= 32768.0;
      })) return JS_ThrowRangeError(context, "invalid 3D triangle coordinates");
  int32_t r = 255, g = 255, b = 255;
  if (argc > 9) JS_ToInt32(context, &r, argv[9]);
  if (argc > 10) JS_ToInt32(context, &g, argv[10]);
  if (argc > 11) JS_ToInt32(context, &b, argv[11]);
  scope->api->graphics.triangle({static_cast<float>(value[0]), static_cast<float>(value[1]),
    static_cast<float>(value[2]), static_cast<float>(value[3]), static_cast<float>(value[4]),
    static_cast<float>(value[5]), static_cast<float>(value[6]), static_cast<float>(value[7]),
    static_cast<float>(value[8]), {static_cast<uint8_t>(r), static_cast<uint8_t>(g),
    static_cast<uint8_t>(b), 255}});
  return JS_UNDEFINED;
}

// One native boundary crossing for an entire frame's triangle stream. The
// Float32Array layout repeats x1,y1,z1,x2,y2,z2,x3,y3,z3,r,g,b. Keeping this
// API deliberately flat lets QuickJS fill a reusable buffer without allocating
// thousands of argument arrays or calling into C++ once per face.
JSValue Triangles3d(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api || argc < 1)
    return JS_ThrowTypeError(context, "triangles3d requires a Float32Array");
  if (JS_GetTypedArrayType(argv[0]) != JS_TYPED_ARRAY_FLOAT32)
    return JS_ThrowTypeError(context, "triangles3d requires a Float32Array");

  size_t byteOffset = 0, byteLength = 0, bytesPerElement = 0;
  JSValue arrayBuffer = JS_GetTypedArrayBuffer(
    context, argv[0], &byteOffset, &byteLength, &bytesPerElement);
  if (JS_IsException(arrayBuffer)) return arrayBuffer;
  size_t bufferLength = 0;
  uint8_t* bytes = JS_GetArrayBuffer(context, &bufferLength, arrayBuffer);
  if (!bytes || bytesPerElement != sizeof(float) ||
      byteOffset > bufferLength || byteLength > bufferLength - byteOffset ||
      byteLength % (12 * sizeof(float)) != 0) {
    JS_FreeValue(context, arrayBuffer);
    return JS_ThrowRangeError(context,
      "triangles3d length must be a multiple of 12 floats");
  }
  const size_t capacity = byteLength / (12 * sizeof(float));
  uint32_t requestedCount = static_cast<uint32_t>(capacity);
  if (argc > 1 && JS_ToUint32(context, &requestedCount, argv[1])) {
    JS_FreeValue(context, arrayBuffer);
    return JS_EXCEPTION;
  }
  const size_t count = requestedCount;
  if (count > capacity) {
    JS_FreeValue(context, arrayBuffer);
    return JS_ThrowRangeError(context, "triangles3d count exceeds the buffer");
  }
  if (count > 4096) {
    JS_FreeValue(context, arrayBuffer);
    return JS_ThrowRangeError(context, "triangles3d accepts at most 4096 triangles");
  }

  const float* values = reinterpret_cast<const float*>(bytes + byteOffset);
  for (size_t triangle = 0; triangle < count; ++triangle) {
    const float* value = values + triangle * 12;
    bool valid = true;
    for (int index = 0; index < 9; ++index)
      valid = valid && std::isfinite(value[index]) && std::abs(value[index]) <= 32768.f;
    if (!valid) {
      JS_FreeValue(context, arrayBuffer);
      return JS_ThrowRangeError(context, "invalid 3D triangle coordinates");
    }
    const auto color = [](float channel) {
      return static_cast<uint8_t>((std::max)(0.f, (std::min)(255.f, channel)));
    };
    scope->api->graphics.triangle({value[0], value[1], value[2], value[3], value[4],
      value[5], value[6], value[7], value[8],
      {color(value[9]), color(value[10]), color(value[11]), 255}});
  }
  JS_FreeValue(context, arrayBuffer);
  return JS_NewInt32(context, static_cast<int32_t>(count));
}

// Screen-facing point-sampled sprites. Layout repeats x,y,z,size,r,g,b,frame.
JSValue Sprites3d(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api || argc < 1 ||
      JS_GetTypedArrayType(argv[0]) != JS_TYPED_ARRAY_FLOAT32)
    return JS_ThrowTypeError(context, "sprites3d requires a Float32Array");
  size_t byteOffset = 0, byteLength = 0, bytesPerElement = 0;
  JSValue arrayBuffer = JS_GetTypedArrayBuffer(
    context, argv[0], &byteOffset, &byteLength, &bytesPerElement);
  if (JS_IsException(arrayBuffer)) return arrayBuffer;
  size_t bufferLength = 0;
  uint8_t* bytes = JS_GetArrayBuffer(context, &bufferLength, arrayBuffer);
  if (!bytes || bytesPerElement != sizeof(float) || byteOffset > bufferLength ||
      byteLength > bufferLength - byteOffset || byteLength % (8 * sizeof(float)) != 0) {
    JS_FreeValue(context, arrayBuffer);
    return JS_ThrowRangeError(context, "sprites3d length must be a multiple of 8 floats");
  }
  const size_t capacity = byteLength / (8 * sizeof(float));
  uint32_t requestedCount = static_cast<uint32_t>(capacity);
  if (argc > 1 && JS_ToUint32(context, &requestedCount, argv[1])) {
    JS_FreeValue(context, arrayBuffer);
    return JS_EXCEPTION;
  }
  if (requestedCount > capacity || requestedCount > 512) {
    JS_FreeValue(context, arrayBuffer);
    return JS_ThrowRangeError(context, "sprites3d count exceeds the buffer or 512 sprites");
  }
  const float* values = reinterpret_cast<const float*>(bytes + byteOffset);
  const auto color = [](float channel) {
    return static_cast<uint8_t>((std::max)(0.f, (std::min)(255.f, channel)));
  };
  for (size_t sprite = 0; sprite < requestedCount; ++sprite) {
    const float* value = values + sprite * 8;
    bool valid = true;
    for (int index = 0; index < 4; ++index)
      valid = valid && std::isfinite(value[index]) && std::abs(value[index]) <= 32768.f;
    if (!valid || value[3] <= 0) {
      JS_FreeValue(context, arrayBuffer);
      return JS_ThrowRangeError(context, "invalid sprite coordinates or size");
    }
    scope->api->graphics.sprite({value[0], value[1], value[2], value[3],
      {color(value[4]), color(value[5]), color(value[6]), 255},
      static_cast<uint8_t>((std::max)(0.f, (std::min)(1.f, value[7])))});
  }
  JS_FreeValue(context, arrayBuffer);
  return JS_NewInt32(context, static_cast<int32_t>(requestedCount));
}

// UV-mapped triangle stream. Layout repeats three x,y,z,u,v vertices followed
// by an RGB light tint: 18 floats per triangle.
JSValue TexturedTriangles3d(JSContext* context, JSValueConst, int argc,
    JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api || argc < 1 ||
      JS_GetTypedArrayType(argv[0]) != JS_TYPED_ARRAY_FLOAT32)
    return JS_ThrowTypeError(context, "texturedTriangles3d requires a Float32Array");
  size_t byteOffset = 0, byteLength = 0, bytesPerElement = 0;
  JSValue arrayBuffer = JS_GetTypedArrayBuffer(
    context, argv[0], &byteOffset, &byteLength, &bytesPerElement);
  if (JS_IsException(arrayBuffer)) return arrayBuffer;
  size_t bufferLength = 0;
  uint8_t* bytes = JS_GetArrayBuffer(context, &bufferLength, arrayBuffer);
  if (!bytes || bytesPerElement != sizeof(float) || byteOffset > bufferLength ||
      byteLength > bufferLength - byteOffset || byteLength % (18 * sizeof(float)) != 0) {
    JS_FreeValue(context, arrayBuffer);
    return JS_ThrowRangeError(context,
      "texturedTriangles3d length must be a multiple of 18 floats");
  }
  const size_t capacity = byteLength / (18 * sizeof(float));
  uint32_t requestedCount = static_cast<uint32_t>(capacity);
  if (argc > 1 && JS_ToUint32(context, &requestedCount, argv[1])) {
    JS_FreeValue(context, arrayBuffer);
    return JS_EXCEPTION;
  }
  if (requestedCount > capacity || requestedCount > 2048) {
    JS_FreeValue(context, arrayBuffer);
    return JS_ThrowRangeError(context,
      "texturedTriangles3d count exceeds the buffer or 2048 triangles");
  }
  const float* values = reinterpret_cast<const float*>(bytes + byteOffset);
  const auto color = [](float channel) {
    return static_cast<uint8_t>((std::max)(0.f, (std::min)(255.f, channel)));
  };
  for (size_t triangle = 0; triangle < requestedCount; ++triangle) {
    const float* value = values + triangle * 18;
    bool valid = true;
    for (int vertex = 0; vertex < 3; ++vertex) {
      const int base = vertex * 5;
      for (int index = 0; index < 5; ++index)
        valid = valid && std::isfinite(value[base + index]) &&
          std::abs(value[base + index]) <= 32768.f;
    }
    if (!valid) {
      JS_FreeValue(context, arrayBuffer);
      return JS_ThrowRangeError(context, "invalid textured triangle coordinates");
    }
    scope->api->graphics.textured_triangle({
      value[0], value[1], value[2], value[3], value[4],
      value[5], value[6], value[7], value[8], value[9],
      value[10], value[11], value[12], value[13], value[14],
      {color(value[15]), color(value[16]), color(value[17]), 255}});
  }
  JS_FreeValue(context, arrayBuffer);
  return JS_NewInt32(context, static_cast<int32_t>(requestedCount));
}

JSValue SystemWrite(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api || argc < 1) return JS_EXCEPTION;
  const char* value = JS_ToCString(context, argv[0]);
  if (!value) return JS_EXCEPTION;
  double x = 80, y = 80, size = 48;
  int32_t r = 255, g = 255, b = 255;
  if (argc > 1) JS_ToFloat64(context, &x, argv[1]);
  if (argc > 2) JS_ToFloat64(context, &y, argv[2]);
  if (argc > 3) JS_ToFloat64(context, &size, argv[3]);
  if (argc > 4) JS_ToInt32(context, &r, argv[4]);
  if (argc > 5) JS_ToInt32(context, &g, argv[5]);
  if (argc > 6) JS_ToInt32(context, &b, argv[6]);
  scope->api->graphics.system_write({value, "Segoe UI", static_cast<float>(x),
    static_cast<float>(y), static_cast<float>(size),
    {static_cast<uint8_t>(r), static_cast<uint8_t>(g), static_cast<uint8_t>(b), 255}});
  JS_FreeCString(context, value);
  return JS_UNDEFINED;
}

JSValue SystemGlyph(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api || argc < 1) return JS_EXCEPTION;
  const char* name = JS_ToCString(context, argv[0]);
  if (!name) return JS_EXCEPTION;
  double x = 80, y = 80, size = 64;
  int32_t r = 255, g = 255, b = 255;
  if (argc > 1) JS_ToFloat64(context, &x, argv[1]);
  if (argc > 2) JS_ToFloat64(context, &y, argv[2]);
  if (argc > 3) JS_ToFloat64(context, &size, argv[3]);
  if (argc > 4) JS_ToInt32(context, &r, argv[4]);
  if (argc > 5) JS_ToInt32(context, &g, argv[5]);
  if (argc > 6) JS_ToInt32(context, &b, argv[6]);
  scope->api->graphics.system_glyph({name, static_cast<float>(x),
    static_cast<float>(y), static_cast<float>(size),
    {static_cast<uint8_t>(r), static_cast<uint8_t>(g), static_cast<uint8_t>(b), 255}});
  JS_FreeCString(context, name);
  return JS_UNDEFINED;
}

JSValue Painting(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  double x = 0, y = 0, width = 640, height = 480;
  if (!scope || !scope->api) return JS_EXCEPTION;
  if (argc > 0) JS_ToFloat64(context, &x, argv[0]);
  if (argc > 1) JS_ToFloat64(context, &y, argv[1]);
  if (argc > 2) JS_ToFloat64(context, &width, argv[2]);
  if (argc > 3) JS_ToFloat64(context, &height, argv[3]);
  scope->api->graphics.image({"latest-painting", static_cast<float>(x),
    static_cast<float>(y), static_cast<float>(width), static_cast<float>(height)});
  return JS_UNDEFINED;
}

JSValue StampPainting(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api || argc < 3) return JS_EXCEPTION;
  const char* source = JS_ToCString(context, argv[0]);
  if (!source) return JS_EXCEPTION;
  std::string code(source);
  JS_FreeCString(context, source);
  const bool valid = code.size() >= 2 && code.size() <= 9 && code.front() == '#' &&
    std::all_of(code.begin() + 1, code.end(), [](unsigned char character) {
      return (character >= '0' && character <= '9') ||
        (character >= 'A' && character <= 'Z') ||
        (character >= 'a' && character <= 'z');
    });
  if (!valid) return JS_ThrowTypeError(context, "painting code must match #[0-9A-Za-z]{1,8}");
  double x = 0, y = 0, scale = 1;
  if (JS_ToFloat64(context, &x, argv[1]) || JS_ToFloat64(context, &y, argv[2]) ||
      (argc > 3 && JS_ToFloat64(context, &scale, argv[3]))) return JS_EXCEPTION;
  if (!std::isfinite(x) || !std::isfinite(y) || !std::isfinite(scale) ||
      std::abs(scale) > 8.0) return JS_ThrowRangeError(context, "invalid painting transform");
  scope->api->graphics.image({code, static_cast<float>(x), static_cast<float>(y),
    0, 0, static_cast<float>(scale), true});
  return JS_UNDEFINED;
}

JSValue Blur(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  int32_t radius = 0;
  if (!scope || !scope->api || argc < 1 || JS_ToInt32(context, &radius, argv[0]))
    return JS_EXCEPTION;
  if (radius < 0 || radius > 16)
    return JS_ThrowRangeError(context, "blur radius must be between 0 and 16");
  scope->api->graphics.blur(static_cast<unsigned>(radius));
  return JS_UNDEFINED;
}

JSValue Telemetry(JSContext* context, JSValueConst, int argc, JSValueConst* argv) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api || !scope->api->telemetry || argc < 1) return JS_UNDEFINED;
  const char* event = JS_ToCString(context, argv[0]);
  const char* detail = argc > 1 ? JS_ToCString(context, argv[1]) : nullptr;
  if (!event) return JS_EXCEPTION;
  std::string line = "JS ";
  line += event;
  if (detail) { line += " "; line += detail; }
  scope->api->telemetry(line);
  if (detail) JS_FreeCString(context, detail);
  JS_FreeCString(context, event);
  return JS_UNDEFINED;
}

JSValue RuntimeInfo(JSContext* context, JSValueConst, int, JSValueConst*) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api) return JS_EXCEPTION;
  JSValue result = JS_NewObject(context);
  JS_SetPropertyStr(context, result, "width", JS_NewInt32(context, scope->api->screen.width));
  JS_SetPropertyStr(context, result, "height", JS_NewInt32(context, scope->api->screen.height));
  JS_SetPropertyStr(context, result, "sampleRate", JS_NewInt32(context, scope->api->sound.sample_rate()));
  JS_SetPropertyStr(context, result, "simCount", JS_NewInt64(context, scope->api->sim_count));
  JS_SetPropertyStr(context, result, "paintCount", JS_NewInt64(context, scope->api->paint_count));
  JS_SetPropertyStr(context, result, "monotonicUs", JS_NewInt64(context, scope->api->clock.monotonic_us));
  JS_SetPropertyStr(context, result, "unixMs", JS_NewInt64(context, scope->api->clock.unix_ms));
  JS_SetPropertyStr(context, result, "clockSynced", JS_NewBool(context, scope->api->clock.network_synced));
  JS_SetPropertyStr(context, result, "clockOffsetMs", JS_NewInt64(context, scope->api->clock.network_offset_ms));
  JS_SetPropertyStr(context, result, "clockRttMs", JS_NewInt32(context, scope->api->clock.network_rtt_ms));
  JS_SetPropertyStr(context, result, "clockSyncAgeMs", JS_NewInt64(context, scope->api->clock.network_sync_age_ms));
  JS_SetPropertyStr(context, result, "audioLatencyMs", JS_NewFloat64(context, scope->api->audio.output_latency_ms));
  JS_SetPropertyStr(context, result, "audioSubmitUs", JS_NewFloat64(context, scope->api->audio.submit_us));
  JS_SetPropertyStr(context, result, "inputToAudioUs", JS_NewFloat64(context, scope->api->audio.input_to_submit_us));
  JS_SetPropertyStr(context, result, "audioGlitches", JS_NewInt32(context, scope->api->audio.glitches));
  JS_SetPropertyStr(context, result, "midiInputs", JS_NewInt32(context, scope->api->audio.midi_inputs));
  JS_SetPropertyStr(context, result, "midiEvents", JS_NewInt64(context, scope->api->audio.midi_events));
  JS_SetPropertyStr(context, result, "midiNote", JS_NewInt32(context, scope->api->audio.midi_note));
  JS_SetPropertyStr(context, result, "midiVelocity", JS_NewInt32(context, scope->api->audio.midi_velocity));
  JS_SetPropertyStr(context, result, "midiStatus", JS_NewString(context, scope->api->audio.midi_status.c_str()));
  return result;
}

JSValue GamepadState(JSContext* context, JSValueConst, int, JSValueConst*) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api) return JS_EXCEPTION;
  const auto& pad = scope->api->gamepad;
  JSValue result = JS_NewObject(context);
  JS_SetPropertyStr(context, result, "leftX", JS_NewFloat64(context, pad.left_x));
  JS_SetPropertyStr(context, result, "leftY", JS_NewFloat64(context, pad.left_y));
  JS_SetPropertyStr(context, result, "rightX", JS_NewFloat64(context, pad.right_x));
  JS_SetPropertyStr(context, result, "rightY", JS_NewFloat64(context, pad.right_y));
  JS_SetPropertyStr(context, result, "leftTrigger", JS_NewFloat64(context, pad.left_trigger));
  JS_SetPropertyStr(context, result, "rightTrigger", JS_NewFloat64(context, pad.right_trigger));
  JSValue down = JS_NewArray(context); uint32_t index = 0;
  for (const auto& button : pad.down)
    JS_SetPropertyUint32(context, down, index++, JS_NewString(context, button.c_str()));
  JS_SetPropertyStr(context, result, "down", down);
  return result;
}

JSValue Controllers(JSContext* context, JSValueConst, int, JSValueConst*) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api) return JS_EXCEPTION;
  JSValue result = JS_NewArray(context);
  uint32_t index = 0;
  for (const auto& controller : scope->api->gamepad.controllers) {
    JSValue item = JS_NewObject(context);
    JS_SetPropertyStr(context, item, "id", JS_NewString(context, controller.id.c_str()));
    JS_SetPropertyStr(context, item, "name", JS_NewString(context, controller.name.c_str()));
    JS_SetPropertyStr(context, item, "vendorId", JS_NewInt32(context, controller.vendor_id));
    JS_SetPropertyStr(context, item, "productId", JS_NewInt32(context, controller.product_id));
    JS_SetPropertyStr(context, item, "axes", JS_NewInt32(context, controller.axes));
    JS_SetPropertyStr(context, item, "buttons", JS_NewInt32(context, controller.buttons));
    JS_SetPropertyStr(context, item, "switches", JS_NewInt32(context, controller.switches));
    JS_SetPropertyStr(context, item, "gamepad", JS_NewBool(context, controller.gamepad));
    JS_SetPropertyUint32(context, result, index++, item);
  }
  return result;
}

JSValue Capabilities(JSContext* context, JSValueConst, int, JSValueConst*) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api) return JS_EXCEPTION;
  JSValue result = JS_NewObject(context);
  JS_SetPropertyStr(context, result, "platform", JS_NewString(context, "xbox-uwp"));
  JS_SetPropertyStr(context, result, "quickjs", JS_NewBool(context, true));
  JS_SetPropertyStr(context, result, "direct3d11", JS_NewBool(context, true));
  JS_SetPropertyStr(context, result, "xaudio2", JS_NewBool(context, true));
  JS_SetPropertyStr(context, result, "internetClient", JS_NewBool(context, true));
  JS_SetPropertyStr(context, result, "privateNetworkClientServer", JS_NewBool(context, true));
  JS_SetPropertyStr(context, result, "online", JS_NewBool(context, scope->api->system.online));
  JS_SetPropertyStr(context, result, "networkLevel",
    JS_NewString(context, scope->api->system.network_level.c_str()));
  JS_SetPropertyStr(context, result, "networkName",
    JS_NewString(context, scope->api->system.network_name.c_str()));
  JS_SetPropertyStr(context, result, "version",
    JS_NewString(context, scope->api->system.version.c_str()));
  JS_SetPropertyStr(context, result, "deviceFamily",
    JS_NewString(context, scope->api->system.device_family.c_str()));
  JS_SetPropertyStr(context, result, "deviceFamilyVersion",
    JS_NewString(context, scope->api->system.device_family_version.c_str()));
  JS_SetPropertyStr(context, result, "productName",
    JS_NewString(context, scope->api->system.product_name.c_str()));
  JS_SetPropertyStr(context, result, "memoryUsageBytes",
    JS_NewInt64(context, scope->api->system.memory_usage_bytes));
  JS_SetPropertyStr(context, result, "memoryLimitBytes",
    JS_NewInt64(context, scope->api->system.memory_limit_bytes));
  JS_SetPropertyStr(context, result, "expectedMemoryLimitBytes",
    JS_NewInt64(context, scope->api->system.expected_memory_limit_bytes));
  JS_SetPropertyStr(context, result, "width", JS_NewInt32(context, scope->api->screen.width));
  JS_SetPropertyStr(context, result, "height", JS_NewInt32(context, scope->api->screen.height));
  JS_SetPropertyStr(context, result, "liveLocalState", JS_NewBool(context, true));
  return result;
}

JSValue AcData(JSContext* context, JSValueConst, int, JSValueConst*) {
  auto* scope = static_cast<CallScope*>(JS_GetContextOpaque(context));
  if (!scope || !scope->api) return JS_EXCEPTION;
  const auto snapshot = std::atomic_load(&scope->api->ac);
  JSValue result = JS_NewObject(context);
  if (!snapshot) return result;
  JS_SetPropertyStr(context, result, "mood", JS_NewString(context, snapshot->mood.c_str()));
  JS_SetPropertyStr(context, result, "moodHandle", JS_NewString(context, snapshot->mood_handle.c_str()));
  JS_SetPropertyStr(context, result, "clockFrom", JS_NewString(context, snapshot->clock_from.c_str()));
  JS_SetPropertyStr(context, result, "clockText", JS_NewString(context, snapshot->clock_text.c_str()));
  JS_SetPropertyStr(context, result, "paintingUrl", JS_NewString(context, snapshot->painting_url.c_str()));
  JS_SetPropertyStr(context, result, "paintingHandle", JS_NewString(context, snapshot->painting_handle.c_str()));
  JS_SetPropertyStr(context, result, "status", JS_NewString(context, snapshot->status.c_str()));
  JS_SetPropertyStr(context, result, "refreshedUnixMs", JS_NewInt64(context, snapshot->refreshed_unix_ms));
  return result;
}

class QuickJsPiece final : public JsPiece {
 public:
  QuickJsPiece(const PieceBundle& bundle, const JsLimits& limits, std::string& error)
      : slug_(bundle.slug), version_(bundle.version) {
    runtime_ = JS_NewRuntime();
    if (!runtime_) { error = "JS_NewRuntime failed"; return; }
    JS_SetMemoryLimit(runtime_, limits.max_heap_bytes);
    JS_SetMaxStackSize(runtime_, 1024 * 1024);
    context_ = JS_NewContext(runtime_);
    if (!context_) { error = "JS_NewContext failed"; return; }
    JS_SetContextOpaque(context_, &scope_);
    JSValue global = JS_GetGlobalObject(context_);
    JS_SetPropertyStr(context_, global, "wipe", JS_NewCFunction(context_, Wipe, "wipe", 3));
    JS_SetPropertyStr(context_, global, "synth", JS_NewCFunction(context_, Synth, "synth", 2));
    JS_SetPropertyStr(context_, global, "oscillator", JS_NewCFunction(context_, Oscillator, "oscillator", 2));
    JS_SetPropertyStr(context_, global, "oscillatorStop", JS_NewCFunction(context_, OscillatorStop, "oscillatorStop", 0));
    JS_SetPropertyStr(context_, global, "write", JS_NewCFunction(context_, Write, "write", 7));
    JS_SetPropertyStr(context_, global, "box", JS_NewCFunction(context_, Box, "box", 7));
    JS_SetPropertyStr(context_, global, "line", JS_NewCFunction(context_, Line, "line", 8));
    JS_SetPropertyStr(context_, global, "triangle", JS_NewCFunction(context_, TriangleFill, "triangle", 9));
    JS_SetPropertyStr(context_, global, "triangle3d", JS_NewCFunction(context_, Triangle3d, "triangle3d", 12));
    JS_SetPropertyStr(context_, global, "triangles3d", JS_NewCFunction(context_, Triangles3d, "triangles3d", 2));
    JS_SetPropertyStr(context_, global, "sprites3d", JS_NewCFunction(context_, Sprites3d, "sprites3d", 2));
    JS_SetPropertyStr(context_, global, "texturedTriangles3d", JS_NewCFunction(context_, TexturedTriangles3d, "texturedTriangles3d", 2));
    JS_SetPropertyStr(context_, global, "systemWrite", JS_NewCFunction(context_, SystemWrite, "systemWrite", 7));
    JS_SetPropertyStr(context_, global, "systemGlyph", JS_NewCFunction(context_, SystemGlyph, "systemGlyph", 7));
    JS_SetPropertyStr(context_, global, "painting", JS_NewCFunction(context_, Painting, "painting", 4));
    JS_SetPropertyStr(context_, global, "stampPainting", JS_NewCFunction(context_, StampPainting, "stampPainting", 4));
    JS_SetPropertyStr(context_, global, "blur", JS_NewCFunction(context_, Blur, "blur", 1));
    JS_SetPropertyStr(context_, global, "telemetry", JS_NewCFunction(context_, Telemetry, "telemetry", 2));
    JS_SetPropertyStr(context_, global, "runtime", JS_NewCFunction(context_, RuntimeInfo, "runtime", 0));
    JS_SetPropertyStr(context_, global, "gamepad", JS_NewCFunction(context_, GamepadState, "gamepad", 0));
    JS_SetPropertyStr(context_, global, "controllers", JS_NewCFunction(context_, Controllers, "controllers", 0));
    JS_SetPropertyStr(context_, global, "capabilities", JS_NewCFunction(context_, Capabilities, "capabilities", 0));
    JS_SetPropertyStr(context_, global, "ac", JS_NewCFunction(context_, AcData, "ac", 0));
    JS_FreeValue(context_, global);
    JSValue result = JS_Eval(context_, bundle.source.data(), bundle.source.size(),
                             bundle.slug.c_str(), JS_EVAL_TYPE_GLOBAL);
    if (JS_IsException(result)) error = ExceptionText();
    else valid_ = true;
    JS_FreeValue(context_, result);
  }
  ~QuickJsPiece() override { if (context_) JS_FreeContext(context_); if (runtime_) JS_FreeRuntime(runtime_); }
  bool valid() const { return valid_; }
  std::string_view slug() const noexcept override { return slug_; }
  std::string_view version() const noexcept override { return version_; }
  void boot(Api& api) override { Call("boot", api); }
  void sim(Api& api) override { Call("sim", api); }
  void paint(Api& api) override { Call("paint", api); }
  void leave(Api& api) override { Call("leave", api); }
  void act(Api& api, const Event& event) override {
    scope_.api = &api;
    JSValue global = JS_GetGlobalObject(context_);
    JSValue function = JS_GetPropertyStr(context_, global, "act");
    if (JS_IsFunction(context_, function)) {
      JSValue argument = JS_NewString(context_, event.name.c_str());
      JSValue result = JS_Call(context_, function, global, 1, &argument);
      JS_FreeValue(context_, argument);
      if (JS_IsException(result)) { JS_FreeValue(context_, result); JS_FreeValue(context_, function); JS_FreeValue(context_, global); throw std::runtime_error(ExceptionText()); }
      JS_FreeValue(context_, result);
    }
    JS_FreeValue(context_, function); JS_FreeValue(context_, global); scope_.api = nullptr;
  }
 private:
  void Call(const char* name, Api& api) {
    scope_.api = &api;
    JSValue global = JS_GetGlobalObject(context_);
    JSValue function = JS_GetPropertyStr(context_, global, name);
    if (JS_IsFunction(context_, function)) {
      JSValue result = JS_Call(context_, function, global, 0, nullptr);
      if (JS_IsException(result)) { JS_FreeValue(context_, result); JS_FreeValue(context_, function); JS_FreeValue(context_, global); throw std::runtime_error(ExceptionText()); }
      JS_FreeValue(context_, result);
    }
    JS_FreeValue(context_, function); JS_FreeValue(context_, global); scope_.api = nullptr;
  }
  std::string ExceptionText() {
    JSValue exception = JS_GetException(context_); const char* text = JS_ToCString(context_, exception);
    std::string result = text ? text : "JavaScript exception";
    if (text) JS_FreeCString(context_, text); JS_FreeValue(context_, exception); return result;
  }
  std::string slug_, version_; JSRuntime* runtime_ = nullptr; JSContext* context_ = nullptr;
  CallScope scope_{nullptr}; bool valid_ = false;
};
}

std::unique_ptr<JsPiece> QuickJsEngine::compile(const PieceBundle& bundle,
                                                const JsLimits& limits,
                                                std::string& error) {
  auto piece = std::make_unique<QuickJsPiece>(bundle, limits, error);
  return piece->valid() ? std::move(piece) : nullptr;
}
}  // namespace ac::xbox
