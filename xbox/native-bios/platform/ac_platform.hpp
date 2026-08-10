#pragma once

// The OS seam. `ac/runtime.hpp` faces the piece; this faces the machine.
// Everything App.cpp asks of Windows lives behind one of these interfaces and
// nothing else does, so porting is writing a second backend rather than
// rewriting the app.
//
// Two backends are planned:
//   winrt/  — C++/CX against CoreWindow, the shipping UWP build.
//   gdk/    — plain x64 Win32 + GameRuntime, the retail Xbox target.
// Each defines exactly one `make_platform`. Nothing above this header knows
// which one it linked.
//
// This header is deliberately free of Windows headers so it can be read, and
// eventually compiled, on a machine that has none.

#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "ac/runtime.hpp"

namespace ac::xbox::platform {

// What the OS asks of the app. Today App.cpp answers only `closed` and `back`;
// the GDK backend must deliver the rest, because XR-001 requires a console
// title to handle suspend, resume and constrained mode.
enum class Lifecycle {
  activated, suspending, resuming, constrained, unconstrained, closed
};

class HostEvents {
 public:
  virtual ~HostEvents() = default;
  virtual void on_lifecycle(Lifecycle) {}
  virtual void on_back() {}
  virtual void on_resize(int width, int height) {}
};

// The window and its pump. `open` is fullscreen on console; width and height
// are a request, and the values that come back are the ones to render at.
class Window {
 public:
  virtual ~Window() = default;
  virtual bool open(int width, int height) = 0;
  virtual void pump() = 0;  // drain pending OS messages, never blocks
  [[nodiscard]] virtual bool closed() const = 0;
  [[nodiscard]] virtual int width() const = 0;
  [[nodiscard]] virtual int height() const = 0;
  // The thing the swap chain is created against: an IUnknown* CoreWindow under
  // WinRT, an HWND under the GDK on desktop. Which of the two the GDK wants on
  // console is the one D3D question this header cannot answer from macOS.
  [[nodiscard]] virtual void* native_handle() const = 0;
};

// Three roots, all wide, because every reader in App.cpp is already _wfopen_s
// on a wide path. That is the happy accident that makes storage nearly free to
// port: only path acquisition is WinRT, not the file I/O.
class Paths {
 public:
  virtual ~Paths() = default;
  virtual std::wstring package() = 0;  // read-only, signed, Store-delivered
  virtual std::wstring local() = 0;    // writable, app-private
  // Mounted removable volumes, for the photo disc. A retail console backend
  // returns nothing: Store Policy 10.13.4 bars browsing attached media.
  virtual std::vector<std::wstring> removable() { return {}; }
};

class Clock {
 public:
  virtual ~Clock() = default;
  // Since boot. Split the divide so the multiply cannot overflow — the naive
  // counter * 1000000 / frequency wraps negative at about ten days of uptime on
  // a 10 MHz QPC, and has done so three times in the field.
  [[nodiscard]] virtual std::uint64_t monotonic_us() = 0;
  [[nodiscard]] virtual double seconds() = 0;
  [[nodiscard]] virtual std::int64_t unix_ms() = 0;
  // Coarse and cheap. Every 500 ms / 3 s / 10 s poll gate in App.cpp uses this.
  [[nodiscard]] virtual std::uint64_t tick_ms() = 0;
};

class Input {
 public:
  virtual ~Input() = default;
  // Fills `state` with AC button spellings from `ac/runtime.hpp`, not the
  // platform's. The translation table is the backend's problem.
  virtual void read(GamepadState& state) = 0;
  virtual void enumerate(std::vector<ControllerInfo>& out) = 0;
};

struct HttpResult {
  bool ok = false;
  int status = 0;
  std::string text;
  std::vector<std::uint8_t> bytes;
};
using HttpDone = std::function<void(HttpResult)>;

// Three verbs, which is all App.cpp uses: clock and AC snapshots (text),
// paintings (bytes), telemetry and replays (post). Async with a completion
// callback, matching the create_task shape already in place.
class Http {
 public:
  virtual ~Http() = default;
  virtual void get_text(std::string_view url, HttpDone) = 0;
  virtual void get_bytes(std::string_view url, HttpDone) = 0;
  virtual void post_json(std::string_view url, std::string body, HttpDone) = 0;
};

// One bound port, one callback per datagram. The MIDI inlet on 51337.
class UdpInlet {
 public:
  virtual ~UdpInlet() = default;
  virtual bool listen(std::uint16_t port,
                      std::function<void(const std::uint8_t*, std::size_t)>) = 0;
  virtual void close() = 0;
};

// One fixed destination, fire and forget. OSC to 255.255.255.255:51338.
class UdpOutlet {
 public:
  virtual ~UdpOutlet() = default;
  virtual bool open(std::string_view host, std::uint16_t port) = 0;
  virtual void send(const std::uint8_t* data, std::size_t size) = 0;
  virtual void close() = 0;
};

// Text frames only — the oskiewar-live match publisher sends JSON and reads
// nothing back.
class WebSocket {
 public:
  virtual ~WebSocket() = default;
  virtual bool connect(std::string_view url, std::function<void()> on_closed) = 0;
  virtual bool send(std::string_view text) = 0;
  virtual void close() = 0;
  [[nodiscard]] virtual bool connected() const = 0;
};

// Raw bytes, not a decoded message type. App.cpp switches on NoteOn, NoteOff,
// PitchBend and ControlChange itself, so handing back the status byte keeps the
// backend small and the decode shared.
struct MidiEvent {
  std::uint8_t status = 0, data1 = 0, data2 = 0;
  std::uint64_t timestamp_us = 0;
};

class Midi {
 public:
  virtual ~Midi() = default;
  // Rescan and open the first input found. Called on a 3 s gate while no port
  // is held. `opened` fires with the device name on success.
  virtual void scan(std::function<void(std::string_view name)> opened) = 0;
  virtual void on_message(std::function<void(const MidiEvent&)>) = 0;
  virtual void close() = 0;
  [[nodiscard]] virtual unsigned input_count() const = 0;
};

struct AudioFormat { int sample_rate = 48000; int channels = 2; };

// XAudio2 mixes on both backends and the voice graph and DSP are shared code.
// Only engine creation differs — UWP links xaudio2.lib, the GDK links the
// GameRuntime redist — so that is all this hides.
class AudioDevice {
 public:
  virtual ~AudioDevice() = default;
  virtual bool open(AudioFormat) = 0;
  virtual void close() = 0;
  [[nodiscard]] virtual double output_latency_ms() const = 0;
  // The IXAudio2*, as void* so this header stays free of Windows headers.
  // Both backends hand back the same COM interface; the caller casts it once.
  [[nodiscard]] virtual void* engine() const = 0;
};

struct Bitmap {
  unsigned width = 0, height = 0;
  std::vector<std::uint8_t> bgra;
};

// PNG and JPEG in, BGRA out. This is the capability with no drop-in GDK
// replacement: Windows.Graphics.Imaging is WinRT and WIC is not guaranteed on
// console, so the GDK backend owns a decoder or a middleware dependency.
class ImageDecoder {
 public:
  virtual ~ImageDecoder() = default;
  virtual bool decode(const std::uint8_t* data, std::size_t size, Bitmap& out) = 0;
};

// System text, drawn from the packaged TTFs. Under WinRT this is DirectWrite
// and Direct2D sharing the D3D11 device. Whether the GDK ships D2D and DWrite
// on console is unverified and is the largest open risk in the port — if it
// does not, this backend rasterizes glyphs from the font file itself.
class TextRenderer {
 public:
  virtual ~TextRenderer() = default;
  [[nodiscard]] virtual bool ready() const = 0;
  virtual void draw(const SystemText&) = 0;
  virtual void glyph(const SystemGlyph&) = 0;
};

struct DeviceInfo {
  std::string family;          // "Windows.Xbox"
  std::string family_version;
  std::string product_name;
};
struct MemoryInfo {
  std::uint64_t used = 0, limit = 0, expected_limit = 0;
};
struct NetworkInfo {
  std::string level = "none";
  std::string name;
  bool online = false;
};

class SystemInfo {
 public:
  virtual ~SystemInfo() = default;
  [[nodiscard]] virtual DeviceInfo device() = 0;
  [[nodiscard]] virtual MemoryInfo memory() = 0;
  [[nodiscard]] virtual NetworkInfo network() = 0;
};

// One object the app holds. A backend supplies all of it or none of it.
class Platform {
 public:
  virtual ~Platform() = default;
  virtual Window& window() = 0;
  virtual Paths& paths() = 0;
  virtual Clock& clock() = 0;
  virtual Input& input() = 0;
  virtual Http& http() = 0;
  virtual Midi& midi() = 0;
  virtual AudioDevice& audio() = 0;
  virtual ImageDecoder& images() = 0;
  virtual TextRenderer& text() = 0;
  virtual SystemInfo& info() = 0;
  virtual std::unique_ptr<UdpInlet> udp_inlet() = 0;
  virtual std::unique_ptr<UdpOutlet> udp_outlet() = 0;
  virtual std::unique_ptr<WebSocket> websocket() = 0;
  virtual void debug_out(std::string_view) = 0;  // OutputDebugStringA
};

// Exactly one backend defines this. Which one is a link-time fact, chosen by
// the build configuration, not a runtime branch.
std::unique_ptr<Platform> make_platform(HostEvents&);

// The piece the package ships. Retail reads `oskiewar.js` out of the signed
// package; there is no other source of script in a retail build.
[[nodiscard]] std::string packaged_piece(Paths&);

#if AC_DEV_LIVE_PIECE
// Dev only, and absent by construction from retail. The retail configuration
// does not compile a definition, so a stray call fails at link rather than
// quietly shipping a path that can reach out-of-band script.
// See xbox/GDK-PORT.md for why this is source exclusion and not a runtime flag.
class LiveSource {
 public:
  virtual ~LiveSource() = default;
  // True, with `source` and `stamp` filled, when the watched file changed since
  // the last call. False every other time, including on error.
  virtual bool poll(std::string& source, std::string& stamp) = 0;
};
std::unique_ptr<LiveSource> make_live_source(Paths&);
#endif

}  // namespace ac::xbox::platform
