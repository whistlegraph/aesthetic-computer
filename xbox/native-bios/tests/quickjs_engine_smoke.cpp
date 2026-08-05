#include "QuickJsEngine.hpp"
#include <cassert>
using namespace ac::xbox;
namespace {
class GraphicsProbe final : public Graphics { public: Color color{}; int boxes = 0; int lines = 0; int triangles = 0; int textured = 0; int sprites = 0; int writes = 0; int systemWrites = 0; int glyphs = 0; int images = 0; int blurs = 0; ImageDraw lastImage{}; void wipe(Color value) override { color = value; } void box(const Rect&) override { ++boxes; } void line(const Line&) override { ++lines; } void triangle(const Triangle&) override { ++triangles; } void textured_triangle(const TexturedTriangle&) override { ++textured; } void sprite(const Sprite&) override { ++sprites; } void write(const Text&) override { ++writes; } void system_write(const SystemText&) override { ++systemWrites; } void system_glyph(const SystemGlyph&) override { ++glyphs; } void image(const ImageDraw& draw) override { ++images; lastImage = draw; } void blur(unsigned) override { ++blurs; } };
class SoundProbe final : public Sound { public: int calls = 0; int oscillators = 0; int stops = 0; int drums = 0; void synth(const SynthVoice&) override { ++calls; } void stop_all() override {} int sample_rate() const override { return 48000; } void oscillator(float, float) override { ++oscillators; } void oscillator_stop() override { ++stops; } void drum(std::string_view, float, float) override { ++drums; } };
}
int main() {
  GraphicsProbe graphics; SoundProbe sound; Api api{{}, {}, {}, {}, graphics, sound, {}};
  int telemetryCalls = 0;
  int gameSignalCalls = 0;
  int replayCalls = 0;
  int liveCalls = 0;
  int discScans = 0, discShows = 0, discCopies = 0;
  api.telemetry = [&](std::string_view) { ++telemetryCalls; };
  api.game_signal = [&](std::string_view event, int player, float value, float value2) {
    if (event == "bullet" && player == 1 && value == 0.5f && value2 == 0.25f)
      ++gameSignalCalls;
  };
  api.replay_save = [&](std::string_view replay) {
    if (replay == "{\"format\":\"ac.oskiedemo\"}") ++replayCalls;
  };
  api.live_publish = [&](std::string_view match, std::string_view state) {
    if (match == "ow-bafegu-dorimi-kunapo" && state == "{\"seq\":1}") ++liveCalls;
  };
  auto disc = std::make_shared<PhotoDiscSnapshot>();
  disc->status = "ready"; disc->volume = "D:"; disc->name = "PHOTO.JPG";
  disc->count = 3; disc->index = 1; disc->width = 1600; disc->height = 1200;
  disc->current_ready = true;
  api.disc.snapshot = std::static_pointer_cast<const PhotoDiscSnapshot>(disc);
  api.disc.scan = [&]() { ++discScans; };
  api.disc.show = [&](std::int64_t index) { assert(index == -1); ++discShows; };
  api.disc.copy = [&]() { ++discCopies; };
  QuickJsEngine engine; std::string error;
  PadState firstPad; firstPad.connected = true; firstPad.down.insert("A");
  PadState secondPad; secondPad.connected = true; secondPad.left_x = -1;
  api.gamepad.connected = true; api.gamepad.pads = {firstPad, secondPad};
  api.clock.network_synced = true; api.clock.network_offset_ms = 3; api.clock.network_rtt_ms = 21;
  api.audio.output_latency_ms = 11.5; api.audio.midi_status = "no-input";
  api.audio.midi_gate = true; api.audio.midi_pitch_bend = 9000;
  auto piece = engine.compile({"smoke", "test", "function boot(){telemetry('BOOT','OK');gameSignal('bullet',1,.5,.25);saveReplay('{\"format\":\"ac.oskiedemo\"}');publishLive('ow-bafegu-dorimi-kunapo','{\"seq\":1}');ac();if(!discScan())throw Error('disc scan')} function sim(){if(!gamepad().connected||!gamepad(0).connected||!gamepad(1).connected||gamepad(2).connected||gamepad(1).leftX!==-1)throw Error('indexed gamepads');drum('kick',1,0);const r=runtime();if(!r.clockSynced||r.clockOffsetMs!==3||r.audioLatencyMs!==11.5||r.midiStatus!=='no-input'||!r.midiGate||r.midiPitchBend!==9000)throw Error('runtime telemetry');const d=disc();if(d.status!=='ready'||d.volume!=='D:'||d.name!=='PHOTO.JPG'||d.count!==3||d.index!==1||d.width!==1600||d.height!==1200||!d.currentReady)throw Error('disc state');if(!discShow(-1))throw Error('disc show');capabilities();controllers();oscillator(220,.1)} function paint(){wipe(1,2,3);box(1,2,3,4,5,6,7);line(1,2,3,4,2,5,6,7);triangle(1,2,3,4,5,6,7,8,9);const batch=new Float32Array([1,2,.1,3,4,.1,5,6,.1,7,8,9,10,20,.2,30,40,.2,50,60,.2,70,80,90]);if(triangles3d(batch)!==2)throw Error('triangle batch');const textured=new Float32Array([1,2,.1,0,0,3,4,.1,1,0,5,6,.1,0,1,255,255,255]);if(texturedTriangles3d(textured,1)!==1)throw Error('texture batch');const sprites=new Float32Array([100,200,.3,16,255,80,90,1]);if(sprites3d(sprites,1)!==1)throw Error('sprite batch');write('OK',8,9,10,11,12,13);systemWrite('HI',20,30,40);ywftWrite('YWFT',20,70,40);comicWrite('COMIC',20,110,40);systemGlyph('ButtonA',50,60,70);painting(80,90,100,110);stampPainting('#j8t',200,300,1);discPhoto(0,0,1920,1080);blur(4)} function act(b){if(b==='A')synth(440,.01);if(b==='B'){oscillatorStop();if(!discCopy())throw Error('disc copy')}}", "test"}, {}, error);
  assert(piece && error.empty()); piece->boot(api); piece->paint(api);
  assert(graphics.color.r == 1 && graphics.color.g == 2 && graphics.color.b == 3);
  assert(graphics.boxes == 1 && graphics.lines == 1 && graphics.triangles == 3 && graphics.textured == 1 && graphics.sprites == 1 && graphics.writes == 1 &&
    graphics.systemWrites == 3 && graphics.glyphs == 1 && graphics.images == 3 &&
    graphics.blurs == 1 && graphics.lastImage.source == "disc-photo" &&
    !graphics.lastImage.centered && telemetryCalls == 1 && gameSignalCalls == 1 &&
    replayCalls == 1 && liveCalls == 1 && discScans == 1);
  piece->sim(api); assert(sound.oscillators == 1 && sound.drums == 1 && discShows == 1);
  piece->act(api, {"A"}); assert(sound.calls == 1);
  piece->act(api, {"B"}); assert(sound.stops == 1 && discCopies == 1);
}
