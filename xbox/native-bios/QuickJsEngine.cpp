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
