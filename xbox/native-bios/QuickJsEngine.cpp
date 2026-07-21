#include <cstdint>
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
  JS_SetPropertyStr(context, result, "width", JS_NewInt32(context, scope->api->screen.width));
  JS_SetPropertyStr(context, result, "height", JS_NewInt32(context, scope->api->screen.height));
  JS_SetPropertyStr(context, result, "liveLocalState", JS_NewBool(context, true));
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
    JS_SetPropertyStr(context_, global, "write", JS_NewCFunction(context_, Write, "write", 7));
    JS_SetPropertyStr(context_, global, "box", JS_NewCFunction(context_, Box, "box", 7));
    JS_SetPropertyStr(context_, global, "telemetry", JS_NewCFunction(context_, Telemetry, "telemetry", 2));
    JS_SetPropertyStr(context_, global, "runtime", JS_NewCFunction(context_, RuntimeInfo, "runtime", 0));
    JS_SetPropertyStr(context_, global, "gamepad", JS_NewCFunction(context_, GamepadState, "gamepad", 0));
    JS_SetPropertyStr(context_, global, "controllers", JS_NewCFunction(context_, Controllers, "controllers", 0));
    JS_SetPropertyStr(context_, global, "capabilities", JS_NewCFunction(context_, Capabilities, "capabilities", 0));
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
