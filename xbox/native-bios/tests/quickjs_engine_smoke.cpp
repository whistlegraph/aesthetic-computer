#include "QuickJsEngine.hpp"
#include <cassert>
using namespace ac::xbox;
namespace {
class GraphicsProbe final : public Graphics { public: Color color{}; int boxes = 0; int writes = 0; void wipe(Color value) override { color = value; } void box(const Rect&) override { ++boxes; } void line(const Line&) override {} void write(const Text&) override { ++writes; } };
class SoundProbe final : public Sound { public: int calls = 0; void synth(const SynthVoice&) override { ++calls; } void stop_all() override {} int sample_rate() const override { return 48000; } };
}
int main() {
  GraphicsProbe graphics; SoundProbe sound; Api api{{}, {}, {}, {}, graphics, sound};
  int telemetryCalls = 0;
  api.telemetry = [&](std::string_view) { ++telemetryCalls; };
  QuickJsEngine engine; std::string error;
  auto piece = engine.compile({"smoke", "test", "function boot(){telemetry('BOOT','OK')} function sim(){gamepad();runtime();capabilities();controllers()} function paint(){wipe(1,2,3);box(1,2,3,4,5,6,7);write('OK',8,9,10,11,12,13)} function act(b){if(b==='A')synth(440,.01)}", "test"}, {}, error);
  assert(piece && error.empty()); piece->boot(api); piece->paint(api);
  assert(graphics.color.r == 1 && graphics.color.g == 2 && graphics.color.b == 3);
  assert(graphics.boxes == 1 && graphics.writes == 1 && telemetryCalls == 1);
  piece->act(api, {"A"}); assert(sound.calls == 1);
}
