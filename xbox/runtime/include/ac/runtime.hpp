#pragma once

#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

namespace ac::xbox {

// Keep these spellings aligned with the browser/ac-native piece API.  The
// Xbox host translates Windows.Gaming.Input readings into these event names.
struct Event {
  std::string name;
  float value = 1.0f;
  std::uint64_t timestamp_us = 0;
  [[nodiscard]] bool is(std::string_view candidate) const noexcept {
    return name == candidate;
  }
};

struct Screen { int width = 1920; int height = 1080; float scale = 1.0f; };
struct GamepadState {
  float left_x = 0, left_y = 0, right_x = 0, right_y = 0;
  float left_trigger = 0, right_trigger = 0;
  std::unordered_set<std::string> down;
  [[nodiscard]] bool pressed(std::string_view button) const {
    return down.contains(std::string(button));
  }
};

struct Color { std::uint8_t r, g, b, a = 255; };
struct Rect { float x, y, width, height; Color color; };
struct Line { float x1, y1, x2, y2, width; Color color; };
struct Text { std::string value; float x, y, size; Color color; };

class Graphics {
 public:
  virtual ~Graphics() = default;
  virtual void wipe(Color color) = 0;
  virtual void box(const Rect&) = 0;
  virtual void line(const Line&) = 0;
  virtual void write(const Text&) = 0;
};

struct SynthVoice {
  float frequency_hz = 440;
  float duration_s = .1f;
  float volume = .25f;
  float attack_s = .001f;
  std::string wave = "sine";
};

class Sound {
 public:
  virtual ~Sound() = default;
  // Implementations enqueue directly to the preallocated XAudio2 voice.  This
  // mirrors api.sound.synth({...}) without waiting for the render frame.
  virtual void synth(const SynthVoice&) = 0;
  virtual void stop_all() = 0;
  virtual int sample_rate() const = 0;
};

struct Clock {
  std::uint64_t monotonic_us = 0;
  std::int64_t unix_ms = 0;
  double seconds = 0;
};

struct System {
  std::string platform = "xbox";
  std::string version;
  std::string handle;
  bool dark = true;
  bool online = false;
};

// The stable native lifecycle context. Names intentionally follow piece API
// fields so portable engines need a thin adapter rather than a rewrite.
struct Api {
  Screen screen;
  Clock clock;
  System system;
  GamepadState gamepad;
  Graphics& graphics;
  Sound& sound;
  std::uint64_t sim_count = 0;
  std::uint64_t paint_count = 0;
  double seconds = 0;
};

class Piece {
 public:
  virtual ~Piece() = default;
  virtual void boot(Api&) {}
  virtual void sim(Api&) = 0;
  virtual void paint(Api&) = 0;
  virtual void act(Api&, const Event&) = 0;
  virtual void leave(Api&) {}
};

// Control-plane commands are deliberately data/configuration, never downloaded
// executable code. A shim can change pieces, settings, and probes remotely.
enum class CommandKind { load_piece, configure, run_probe, reload, ping };
struct Command {
  CommandKind kind;
  std::string request_id;
  std::string target;
  std::string json;
};
struct Telemetry {
  std::string type;
  std::string request_id;
  std::uint64_t timestamp_us = 0;
  std::string json;
};

class ControlChannel {
 public:
  virtual ~ControlChannel() = default;
  virtual std::optional<Command> poll() = 0;
  virtual void send(const Telemetry&) = 0;
};

}  // namespace ac::xbox
