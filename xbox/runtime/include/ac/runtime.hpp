#pragma once

#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
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
struct ControllerInfo {
  std::string id;
  std::string name;
  std::uint16_t vendor_id = 0;
  std::uint16_t product_id = 0;
  std::uint16_t axes = 0;
  std::uint16_t buttons = 0;
  std::uint16_t switches = 0;
  bool gamepad = false;
};
struct GamepadState {
  float left_x = 0, left_y = 0, right_x = 0, right_y = 0;
  float left_trigger = 0, right_trigger = 0;
  std::unordered_set<std::string> down;
  std::vector<ControllerInfo> controllers;
  [[nodiscard]] bool pressed(std::string_view button) const {
    return down.find(std::string(button)) != down.end();
  }
};

struct Color { std::uint8_t r, g, b, a = 255; };
struct Rect { float x, y, width, height; Color color; };
struct Line { float x1, y1, x2, y2, width; Color color; };
struct Text { std::string value; float x, y, size; Color color; };
struct SystemText {
  std::string value;
  std::string family = "Segoe UI";
  float x = 0, y = 0, size = 32;
  Color color{255, 255, 255, 255};
};
struct SystemGlyph {
  std::string name;
  float x = 0, y = 0, size = 64;
  Color color{255, 255, 255, 255};
};
struct ImageDraw {
  std::string source = "latest-painting";
  float x = 0, y = 0, width = 0, height = 0;
  float scale = 1;
  bool centered = false;
};

class Graphics {
 public:
  virtual ~Graphics() = default;
  virtual void wipe(Color color) = 0;
  virtual void box(const Rect&) = 0;
  virtual void line(const Line&) = 0;
  virtual void write(const Text&) = 0;
  virtual void system_write(const SystemText&) {}
  virtual void system_glyph(const SystemGlyph&) {}
  virtual void image(const ImageDraw&) {}
  virtual void blur(unsigned) {}
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
  virtual void oscillator(float, float) {}
  virtual void oscillator_stop() {}
};

struct Clock {
  std::uint64_t monotonic_us = 0;
  std::int64_t unix_ms = 0;
  double seconds = 0;
};

struct System {
  std::string platform = "xbox";
  std::string version;
  std::string device_family;
  std::string device_family_version;
  std::string product_name;
  std::string handle;
  std::string network_level = "none";
  std::string network_name;
  std::uint64_t memory_usage_bytes = 0;
  std::uint64_t memory_limit_bytes = 0;
  std::uint64_t expected_memory_limit_bytes = 0;
  bool dark = true;
  bool online = false;
};

struct AcSnapshot {
  std::string mood;
  std::string mood_handle;
  std::string clock_from;
  std::string clock_text;
  std::string painting_url;
  std::string painting_handle;
  std::string status = "loading";
  std::int64_t refreshed_unix_ms = 0;
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
  // Host-polled, read-only snapshots from allowlisted aesthetic.computer
  // endpoints. Pieces never receive a general network primitive.
  std::shared_ptr<const AcSnapshot> ac = std::make_shared<const AcSnapshot>();
  // Sandboxed pieces can emit structured diagnostic lines without receiving
  // filesystem, process, Device Portal, or arbitrary WinRT access.
  std::function<void(std::string_view)> telemetry = {};
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

// A downloaded piece is JavaScript source evaluated by an embedded engine. It
// is data to the UWP package: no PE/DLL/native code is accepted by this API.
// sha256 is supplied by the AC endpoint and verified by the platform host
// before stage() is called.
struct PieceBundle {
  std::string slug;
  std::string version;
  std::string source;
  std::string sha256;
};

struct JsLimits {
  std::size_t max_source_bytes = 2 * 1024 * 1024;
  std::size_t max_heap_bytes = 32 * 1024 * 1024;
  std::uint64_t max_callback_us = 8'000;
};

// Adapter seam for QuickJS (or another interpreter compiled into the app).
// The engine exposes only the AC Api bindings; browser, filesystem, process,
// WinRT and arbitrary network globals are intentionally absent.
class JsPiece : public Piece {
 public:
  ~JsPiece() override = default;
  [[nodiscard]] virtual std::string_view slug() const noexcept = 0;
  [[nodiscard]] virtual std::string_view version() const noexcept = 0;
};

class JsEngine {
 public:
  virtual ~JsEngine() = default;
  virtual std::unique_ptr<JsPiece> compile(const PieceBundle&, const JsLimits&,
                                           std::string& error) = 0;
};

// Owns the active and last-known-good JS contexts. Reload is transactional:
// compile and boot the candidate first, then swap at a frame boundary. If a
// callback throws or exceeds its budget the previous generation is restored.
class PieceSupervisor {
 public:
  explicit PieceSupervisor(JsEngine& engine, JsLimits limits = {})
      : engine_(engine), limits_(limits) {}

  bool stage(const PieceBundle& bundle, Api& api, std::string& error) {
    if (bundle.source.size() > limits_.max_source_bytes) {
      error = "piece source exceeds configured byte limit";
      return false;
    }
    auto candidate = engine_.compile(bundle, limits_, error);
    if (!candidate) return false;
    try {
      candidate->boot(api);
    } catch (...) {
      error = "piece boot failed";
      return false;
    }
    staged_ = std::move(candidate);
    return true;
  }

  bool activate(Api& api) {
    if (!staged_) return false;
    if (active_) {
      active_->leave(api);
      fallback_ = std::move(active_);
    }
    active_ = std::move(staged_);
    ++generation_;
    return true;
  }

  bool rollback(Api& api) {
    if (!fallback_) return false;
    if (active_) active_->leave(api);
    active_ = std::move(fallback_);
    ++generation_;
    return true;
  }

  [[nodiscard]] Piece* active() const noexcept { return active_.get(); }
  [[nodiscard]] std::uint64_t generation() const noexcept { return generation_; }

 private:
  JsEngine& engine_;
  JsLimits limits_;
  std::unique_ptr<JsPiece> active_;
  std::unique_ptr<JsPiece> staged_;
  std::unique_ptr<JsPiece> fallback_;
  std::uint64_t generation_ = 0;
};

// Control-plane commands are data/configuration or sandboxed JS piece source,
// never downloaded native executable code.
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
