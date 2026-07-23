#include "QuickJsEngine.hpp"
#include <cassert>
using namespace ac::xbox;
namespace {
class GraphicsProbe final : public Graphics { public: Color color{}; int boxes = 0; int lines = 0; int triangles = 0; int textured = 0; int sprites = 0; int writes = 0; int systemWrites = 0; int glyphs = 0; int images = 0; int blurs = 0; ImageDraw lastImage{}; void wipe(Color value) override { color = value; } void box(const Rect&) override { ++boxes; } void line(const Line&) override { ++lines; } void triangle(const Triangle&) override { ++triangles; } void textured_triangle(const TexturedTriangle&) override { ++textured; } void sprite(const Sprite&) override { ++sprites; } void write(const Text&) override { ++writes; } void system_write(const SystemText&) override { ++systemWrites; } void system_glyph(const SystemGlyph&) override { ++glyphs; } void image(const ImageDraw& draw) override { ++images; lastImage = draw; } void blur(unsigned) override { ++blurs; } };
class SoundProbe final : public Sound { public: int calls = 0; int oscillators = 0; int stops = 0; void synth(const SynthVoice&) override { ++calls; } void stop_all() override {} int sample_rate() const override { return 48000; } void oscillator(float, float) override { ++oscillators; } void oscillator_stop() override { ++stops; } };
}
int main() {
  GraphicsProbe graphics; SoundProbe sound; Api api{{}, {}, {}, {}, graphics, sound, {}};
  int telemetryCalls = 0;
  api.telemetry = [&](std::string_view) { ++telemetryCalls; };
  QuickJsEngine engine; std::string error;
  api.clock.network_synced = true; api.clock.network_offset_ms = 3; api.clock.network_rtt_ms = 21;
  api.audio.output_latency_ms = 11.5; api.audio.midi_status = "no-input";
  api.audio.midi_gate = true; api.audio.midi_pitch_bend = 9000;
  auto piece = engine.compile({"smoke", "test", "function boot(){telemetry('BOOT','OK');ac()} function sim(){gamepad();const r=runtime();if(!r.clockSynced||r.clockOffsetMs!==3||r.audioLatencyMs!==11.5||r.midiStatus!=='no-input'||!r.midiGate||r.midiPitchBend!==9000)throw Error('runtime telemetry');capabilities();controllers();oscillator(220,.1)} function paint(){wipe(1,2,3);box(1,2,3,4,5,6,7);line(1,2,3,4,2,5,6,7);triangle(1,2,3,4,5,6,7,8,9);const batch=new Float32Array([1,2,.1,3,4,.1,5,6,.1,7,8,9,10,20,.2,30,40,.2,50,60,.2,70,80,90]);if(triangles3d(batch)!==2)throw Error('triangle batch');const textured=new Float32Array([1,2,.1,0,0,3,4,.1,1,0,5,6,.1,0,1,255,255,255]);if(texturedTriangles3d(textured,1)!==1)throw Error('texture batch');const sprites=new Float32Array([100,200,.3,16,255,80,90,1]);if(sprites3d(sprites,1)!==1)throw Error('sprite batch');write('OK',8,9,10,11,12,13);systemWrite('HI',20,30,40);systemGlyph('ButtonA',50,60,70);painting(80,90,100,110);stampPainting('#j8t',200,300,1);blur(4)} function act(b){if(b==='A')synth(440,.01);if(b==='B')oscillatorStop()}", "test"}, {}, error);
  assert(piece && error.empty()); piece->boot(api); piece->paint(api);
  assert(graphics.color.r == 1 && graphics.color.g == 2 && graphics.color.b == 3);
  assert(graphics.boxes == 1 && graphics.lines == 1 && graphics.triangles == 3 && graphics.textured == 1 && graphics.sprites == 1 && graphics.writes == 1 &&
    graphics.systemWrites == 1 && graphics.glyphs == 1 && graphics.images == 2 &&
    graphics.blurs == 1 && graphics.lastImage.source == "#j8t" &&
    graphics.lastImage.centered &&
    telemetryCalls == 1);
  piece->sim(api); assert(sound.oscillators == 1);
  piece->act(api, {"A"}); assert(sound.calls == 1);
  piece->act(api, {"B"}); assert(sound.stops == 1);
}
