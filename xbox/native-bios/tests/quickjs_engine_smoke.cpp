#include "QuickJsEngine.hpp"
#include <cassert>
using namespace ac::xbox;
namespace {
class GraphicsProbe final : public Graphics { public: Color color{}; void wipe(Color value) override { color = value; } void box(const Rect&) override {} void line(const Line&) override {} void write(const Text&) override {} };
class SoundProbe final : public Sound { public: int calls = 0; void synth(const SynthVoice&) override { ++calls; } void stop_all() override {} int sample_rate() const override { return 48000; } };
}
int main() {
  GraphicsProbe graphics; SoundProbe sound; Api api{{}, {}, {}, {}, graphics, sound};
  QuickJsEngine engine; std::string error;
  auto piece = engine.compile({"smoke", "test", "function boot(){} function sim(){} function paint(){wipe(1,2,3)} function act(b){if(b==='A')synth(440,.01)}", "test"}, {}, error);
  assert(piece && error.empty()); piece->boot(api); piece->paint(api);
  assert(graphics.color.r == 1 && graphics.color.g == 2 && graphics.color.b == 3);
  piece->act(api, {"A"}); assert(sound.calls == 1);
}
