// GDK backend — the second implementation of ac_platform.hpp.
//
// Built by xbox/native-bios/gdk/NativeBiosGdk.vcxproj against the *public*
// Microsoft GDK (the PC/desktop target), which is the only GDK a hosted CI
// runner may install: the Xbox Extensions (GXDK) that produce
// Gaming.Xbox.Scarlett.x64 are gated behind an NDA developer program. See
// xbox/GDK-PORT.md §1 for the sourcing.
//
// So: what compiles here is *the PC half of the port*. Where a call would have
// to change on console it is marked `#if defined(_GAMING_XBOX)` with a
// TODO(gxdk), rather than pretended. `_GAMING_XBOX` is defined only by the
// Gaming.Xbox.* MSBuild platforms, so none of those branches has been compiled.
//
// Everything still stubbed carries TODO(gdk) naming the replacement API and the
// App.cpp line it stands in for.

#include "../ac_platform.hpp"

#include <windows.h>

#include <xaudio2.h>
#if !defined(_GAMING_XBOX)
#include <psapi.h>
#endif

#include <GameInput.h>
#include <XGameRuntime.h>

#include <cstdio>

namespace ac::xbox::platform {
namespace {

std::wstring Widen(const char* utf8) {
  if (!utf8 || !*utf8) return {};
  const int need = MultiByteToWideChar(CP_UTF8, 0, utf8, -1, nullptr, 0);
  if (need <= 1) return {};
  std::wstring wide(static_cast<std::size_t>(need) - 1, L'\0');
  MultiByteToWideChar(CP_UTF8, 0, utf8, -1, wide.data(), need);
  return wide;
}

constexpr wchar_t kWindowClass[] = L"AcNativeBiosGdk";

class GdkWindow final : public Window {
 public:
  explicit GdkWindow(HostEvents& host) : host_(host) {}

  ~GdkWindow() override {
    if (hwnd_) DestroyWindow(hwnd_);
    if (registered_) UnregisterClassW(kWindowClass, GetModuleHandleW(nullptr));
  }

  // Replaces CoreApplication::Run + CoreWindow::Activate (App.cpp:288-298, 507).
  bool open(int width, int height) override {
    const HINSTANCE instance = GetModuleHandleW(nullptr);
    WNDCLASSEXW cls{};
    cls.cbSize = sizeof(cls);
    cls.style = CS_HREDRAW | CS_VREDRAW;
    cls.lpfnWndProc = &GdkWindow::Proc;
    cls.hInstance = instance;
    cls.hCursor = LoadCursorW(nullptr, IDC_ARROW);
    cls.lpszClassName = kWindowClass;
    if (!RegisterClassExW(&cls) && GetLastError() != ERROR_CLASS_ALREADY_EXISTS)
      return false;
    registered_ = true;

    width_ = width > 0 ? width : width_;
    height_ = height > 0 ? height : height_;
    RECT rect{0, 0, width_, height_};
#if !defined(_GAMING_XBOX)
    // Console has no window chrome, so no adjust and no title bar style.
    AdjustWindowRectEx(&rect, WS_OVERLAPPEDWINDOW, FALSE, 0);
#endif
    hwnd_ = CreateWindowExW(0, kWindowClass, L"Aesthetic Computer",
                            WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
                            rect.right - rect.left, rect.bottom - rect.top,
                            nullptr, nullptr, instance, this);
    if (!hwnd_) return false;
    SetWindowLongPtrW(hwnd_, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(this));
    ShowWindow(hwnd_, SW_SHOW);
    host_.on_lifecycle(Lifecycle::activated);
    return true;
  }

  // Replaces CoreDispatcher::ProcessEvents(ProcessAllIfPresent) (App.cpp:447).
  void pump() override {
    MSG message{};
    while (PeekMessageW(&message, nullptr, 0, 0, PM_REMOVE)) {
      if (message.message == WM_QUIT) closed_ = true;
      TranslateMessage(&message);
      DispatchMessageW(&message);
    }
  }

  [[nodiscard]] bool closed() const override { return closed_; }
  [[nodiscard]] int width() const override { return width_; }
  [[nodiscard]] int height() const override { return height_; }
  // An HWND. IDXGIFactory2::CreateSwapChainForHwnd takes it directly, which is
  // the whole substitution for CreateSwapChainForCoreWindow (App.cpp:559).
  [[nodiscard]] void* native_handle() const override { return hwnd_; }

 private:
  // TODO(gxdk): the console PLM callbacks — RegisterAppStateChangeNotification
  // for suspend/constrain/resume — are not WM_* messages and have to be wired
  // separately to reach host_.on_lifecycle. XR-001 requires all three.
  static LRESULT CALLBACK Proc(HWND hwnd, UINT message, WPARAM w, LPARAM l) {
    auto* self = reinterpret_cast<GdkWindow*>(GetWindowLongPtrW(hwnd, GWLP_USERDATA));
    if (!self) return DefWindowProcW(hwnd, message, w, l);
    switch (message) {
      case WM_SIZE:
        self->width_ = LOWORD(l);
        self->height_ = HIWORD(l);
        self->host_.on_resize(self->width_, self->height_);
        return 0;
      case WM_CLOSE:
      case WM_DESTROY:
        self->closed_ = true;
        self->host_.on_lifecycle(Lifecycle::closed);
        PostQuitMessage(0);
        return 0;
      default:
        break;
    }
    return DefWindowProcW(hwnd, message, w, l);
  }

  HostEvents& host_;
  HWND hwnd_ = nullptr;
  bool registered_ = false, closed_ = false;
  int width_ = 1920, height_ = 1080;
};

class GdkPaths final : public Paths {
 public:
  // Replaces Package::Current->InstalledLocation->Path (App.cpp:156, 593).
  // No trailing separator, matching what WinRT hands back.
  std::wstring package() override {
    if (!package_.empty()) return package_;
    wchar_t buffer[MAX_PATH]{};
    const DWORD used = GetModuleFileNameW(nullptr, buffer, MAX_PATH);
    if (used == 0 || used >= MAX_PATH) return {};
    std::wstring path(buffer, used);
    const auto slash = path.find_last_of(L"\\/");
    if (slash != std::wstring::npos) path.resize(slash);
    package_ = std::move(path);
    return package_;
  }

  // Replaces ApplicationData::Current->LocalFolder->Path (App.cpp:65, 2129).
  // The GDK spells it XPersistentLocalStorageGetPath, not GetRootPath as the
  // first draft of GDK-PORT.md guessed, and it hands back UTF-8 rather than
  // UTF-16 — the one place the storage port is not a straight swap.
  std::wstring local() override {
    if (!local_.empty()) return local_;
    std::size_t size = 0;
    if (SUCCEEDED(XPersistentLocalStorageGetPathSize(&size)) && size > 0) {
      std::string utf8(size, '\0');
      std::size_t used = 0;
      if (SUCCEEDED(XPersistentLocalStorageGetPath(size, utf8.data(), &used)) && used > 0) {
        local_ = Widen(utf8.c_str());
        while (!local_.empty() && (local_.back() == L'\\' || local_.back() == L'/'))
          local_.pop_back();
        if (!local_.empty()) return local_;
      }
    }
#if defined(_GAMING_XBOX)
    // TODO(gxdk): on console PLS is the only writable root and the call above
    // cannot fail for a correctly packaged title. No fallback exists, and
    // silently writing somewhere else would be worse than not writing.
    return {};
#else
    // Unpackaged desktop — no package identity, so PLS is unavailable. This is
    // the CI and loose-build case, not a shipping one.
    wchar_t buffer[MAX_PATH]{};
    if (GetEnvironmentVariableW(L"LOCALAPPDATA", buffer, MAX_PATH))
      local_ = buffer;
    return local_;
#endif
  }

  // Store Policy 10.13.4 bars browsing attached media on console; the photo
  // disc is a desktop-only capability from here on.
  std::vector<std::wstring> removable() override {
#if defined(_GAMING_XBOX)
    return {};
#else
    std::vector<std::wstring> mounted;
    const DWORD mask = GetLogicalDrives();
    for (int letter = 0; letter < 26; ++letter) {
      if (!(mask & (1u << letter))) continue;
      wchar_t root[4] = {static_cast<wchar_t>(L'A' + letter), L':', L'\\', L'\0'};
      const UINT kind = GetDriveTypeW(root);
      if (kind == DRIVE_REMOVABLE || kind == DRIVE_CDROM) mounted.emplace_back(root);
    }
    return mounted;
#endif
  }

 private:
  std::wstring package_, local_;
};

class GdkClock final : public Clock {
 public:
  GdkClock() { QueryPerformanceFrequency(&frequency_); }

  // Split the divide. counter * 1000000 overflows int64 at about ten days of
  // uptime on a 10 MHz QPC and starts reporting negative time; the remainder
  // term is bounded by frequency so it cannot overflow either. Same value,
  // no wrap until roughly 29,000 years. Mirrors App.cpp RefreshClock.
  [[nodiscard]] std::uint64_t monotonic_us() override {
    LARGE_INTEGER counter{};
    QueryPerformanceCounter(&counter);
    if (frequency_.QuadPart <= 0) return 0;
    return static_cast<std::uint64_t>(
      counter.QuadPart / frequency_.QuadPart * 1000000 +
      counter.QuadPart % frequency_.QuadPart * 1000000 / frequency_.QuadPart);
  }

  [[nodiscard]] double seconds() override {
    LARGE_INTEGER counter{};
    QueryPerformanceCounter(&counter);
    if (frequency_.QuadPart <= 0) return 0;
    return static_cast<double>(counter.QuadPart) / frequency_.QuadPart;
  }

  [[nodiscard]] std::int64_t unix_ms() override {
    FILETIME now{};
    GetSystemTimeAsFileTime(&now);
    ULARGE_INTEGER ticks{};
    ticks.LowPart = now.dwLowDateTime;
    ticks.HighPart = now.dwHighDateTime;
    // 100 ns ticks since 1601-01-01; 11644473600 s to the Unix epoch.
    return static_cast<std::int64_t>(ticks.QuadPart / 10000) - 11644473600000LL;
  }

  [[nodiscard]] std::uint64_t tick_ms() override { return GetTickCount64(); }

 private:
  LARGE_INTEGER frequency_{};
};

// Replaces Gamepad::Gamepads + GetCurrentReading (App.cpp:1679-1740). The bit
// values in GameInputGamepadButtons are identical to the WinRT GamepadButtons
// ones the existing table indexes by, so the name table carries across.
class GdkInput final : public Input {
 public:
  ~GdkInput() override {
    if (input_) input_->Release();
    if (module_) FreeLibrary(module_);
  }

  void read(GamepadState& state) override {
    state.down.clear();
    state.pads.clear();
    state.connected = false;
    state.left_x = state.left_y = state.right_x = state.right_y = 0;
    state.left_trigger = state.right_trigger = 0;
    if (!ensure()) return;

    IGameInputReading* reading = nullptr;
    if (FAILED(input_->GetCurrentReading(GameInputKindGamepad, nullptr, &reading)) ||
        !reading)
      return;

    GameInputGamepadState pad{};
    if (reading->GetGamepadState(&pad)) {
      PadState first;
      first.connected = true;
      first.left_x = pad.leftThumbstickX;
      first.left_y = pad.leftThumbstickY;
      first.right_x = pad.rightThumbstickX;
      first.right_y = pad.rightThumbstickY;
      first.left_trigger = pad.leftTrigger;
      first.right_trigger = pad.rightTrigger;
      for (const auto& named : kNames)
        if (static_cast<unsigned>(pad.buttons) & named.bit) first.down.insert(named.name);
      state.connected = true;
      state.left_x = first.left_x;
      state.left_y = first.left_y;
      state.right_x = first.right_x;
      state.right_y = first.right_y;
      state.left_trigger = first.left_trigger;
      state.right_trigger = first.right_trigger;
      state.down = first.down;
      state.pads.push_back(std::move(first));
    }
    reading->Release();
    // TODO(gdk): App.cpp reads every connected pad. GetCurrentReading with a
    // null device gives only the most recent one; the rest wants
    // RegisterDeviceCallback to hold IGameInputDevice* and one reading each.
  }

  void enumerate(std::vector<ControllerInfo>& out) override {
    out.clear();
    // TODO(gdk): IGameInput::RegisterDeviceCallback + IGameInputDevice::GetDeviceInfo
    // for vendor/product/axis/button counts. Replaces RawGameController::
    // RawGameControllers (App.cpp:1876-1885).
  }

 private:
  // GameInput.dll is not in the box on every Windows edition — Server, which is
  // what CI runs on, is one of the ones without it. Resolving it at runtime
  // means a machine with no GameInput reports "no pad" instead of failing to
  // start the process, which is the difference between a useful CI signal and
  // none. On console it is always present and this costs one LoadLibrary.
  bool ensure() {
    if (input_) return true;
    if (tried_) return false;
    tried_ = true;
    module_ = LoadLibraryExW(L"GameInput.dll", nullptr, LOAD_LIBRARY_SEARCH_SYSTEM32);
    if (!module_) return false;
    using Create = HRESULT(__stdcall*)(IGameInput**);
    const auto create = reinterpret_cast<Create>(
      reinterpret_cast<void*>(GetProcAddress(module_, "GameInputCreate")));
    return create && SUCCEEDED(create(&input_)) && input_ != nullptr;
  }

  struct Named { unsigned bit; const char* name; };
  static constexpr Named kNames[] = {
    {GameInputGamepadA, "A"}, {GameInputGamepadB, "B"},
    {GameInputGamepadX, "X"}, {GameInputGamepadY, "Y"},
    {GameInputGamepadDPadUp, "ArrowUp"}, {GameInputGamepadDPadDown, "ArrowDown"},
    {GameInputGamepadDPadLeft, "ArrowLeft"}, {GameInputGamepadDPadRight, "ArrowRight"},
    {GameInputGamepadLeftShoulder, "LeftShoulder"},
    {GameInputGamepadRightShoulder, "RightShoulder"},
    {GameInputGamepadMenu, "Menu"}, {GameInputGamepadView, "View"},
    {GameInputGamepadLeftThumbstick, "LeftStick"},
    {GameInputGamepadRightThumbstick, "RightStick"}
  };

  IGameInput* input_ = nullptr;
  HMODULE module_ = nullptr;
  bool tried_ = false;
};

class GdkSystemInfo final : public SystemInfo {
 public:
  // Replaces AnalyticsInfo::VersionInfo and EasClientDeviceInformation
  // (App.cpp:1841-1850). XSystemGetAnalyticsInfo is the whole substitution.
  [[nodiscard]] DeviceInfo device() override {
    const XSystemAnalyticsInfo info = XSystemGetAnalyticsInfo();
    DeviceInfo out;
    out.family = info.family;
    char version[48]{};
    std::snprintf(version, sizeof(version), "%u.%u.%u.%u",
                  info.osVersion.major, info.osVersion.minor,
                  info.osVersion.build, info.osVersion.revision);
    out.family_version = version;
    switch (XSystemGetDeviceType()) {
      case XSystemDeviceType::Pc: out.product_name = "Windows PC"; break;
      case XSystemDeviceType::XboxOne: out.product_name = "Xbox One"; break;
      case XSystemDeviceType::XboxOneS: out.product_name = "Xbox One S"; break;
      case XSystemDeviceType::XboxOneX: out.product_name = "Xbox One X"; break;
      case XSystemDeviceType::XboxOneXDevkit: out.product_name = "Xbox One X Devkit"; break;
      case XSystemDeviceType::XboxScarlettLockhart: out.product_name = "Xbox Series S"; break;
      case XSystemDeviceType::XboxScarlettAnaconda: out.product_name = "Xbox Series X"; break;
      case XSystemDeviceType::XboxScarlettDevkit: out.product_name = "Xbox Series Devkit"; break;
      default: out.product_name = "Unknown"; break;
    }
    return out;
  }

  [[nodiscard]] MemoryInfo memory() override {
    MemoryInfo out;
#if defined(_GAMING_XBOX)
    // TODO(gxdk): XMemGetAllocationStatistics. Console has a fixed title budget
    // rather than MemoryManager's advisory limits, so `limit` and
    // `expected_limit` collapse onto the same number.
#else
    PROCESS_MEMORY_COUNTERS counters{};
    counters.cb = sizeof(counters);
    if (GetProcessMemoryInfo(GetCurrentProcess(), &counters, sizeof(counters)))
      out.used = counters.WorkingSetSize;
    MEMORYSTATUSEX status{};
    status.dwLength = sizeof(status);
    if (GlobalMemoryStatusEx(&status)) {
      out.limit = status.ullTotalPhys;
      out.expected_limit = status.ullTotalPhys;
    }
#endif
    return out;
  }

  // Replaces NetworkInformation::GetInternetConnectionProfile (App.cpp:1859).
  // The SSID has no GDK equivalent, so `name` stays empty rather than lying.
  [[nodiscard]] NetworkInfo network() override {
    NetworkInfo out;
    XNetworkingConnectivityHint hint{};
    if (FAILED(XNetworkingGetConnectivityHint(&hint))) return out;
    switch (hint.connectivityLevel) {
      case XNetworkingConnectivityLevelHint::InternetAccess:
        out.level = "internet"; out.online = true; break;
      case XNetworkingConnectivityLevelHint::ConstrainedInternetAccess:
        out.level = "constrained"; out.online = true; break;
      case XNetworkingConnectivityLevelHint::LocalAccess:
        out.level = "local"; break;
      case XNetworkingConnectivityLevelHint::None:
        out.level = "none"; break;
      default:
        out.level = "unknown"; break;
    }
    return out;
  }
};

// XAudio2 mixes on both sides and the voice graph and DSP at App.cpp:972-1008
// are shared code, so only engine creation lives here.
class GdkAudio final : public AudioDevice {
 public:
  ~GdkAudio() override { close(); }

  bool open(AudioFormat format) override {
    if (engine_) return true;
    if (FAILED(XAudio2Create(&engine_, 0, XAUDIO2_DEFAULT_PROCESSOR))) {
      engine_ = nullptr;
      return false;
    }
    if (FAILED(engine_->CreateMasteringVoice(&master_,
                                             static_cast<UINT32>(format.channels),
                                             static_cast<UINT32>(format.sample_rate)))) {
      close();
      return false;
    }
    return true;
  }

  void close() override {
    if (master_) { master_->DestroyVoice(); master_ = nullptr; }
    if (engine_) { engine_->Release(); engine_ = nullptr; }
  }

  [[nodiscard]] double output_latency_ms() const override {
    // TODO(gdk): App.cpp derives this from XAUDIO2_PERFORMANCE_DATA. Same call
    // on both backends once the voice graph moves across.
    return 0;
  }

  [[nodiscard]] void* engine() const override { return engine_; }

 private:
  IXAudio2* engine_ = nullptr;
  IXAudio2MasteringVoice* master_ = nullptr;
};

// ---- Still stubs. Each is compiled, so the signatures are checked, but none
// does anything yet. Ordered by what a headless smoke test needs least.

class GdkHttp final : public Http {
 public:
  void get_text(std::string_view, HttpDone done) override {
    // TODO(gdk): libHttpClient — HCHttpCallCreate, HCHttpCallRequestSetUrl,
    // HCHttpCallPerformAsync with an XAsyncBlock, HCHttpCallResponseGetResponseString.
    // libHttpClient.lib ships in the public GDK NuGet package (Xbox.LibHttpClient),
    // so this is available on the PC target too. Replaces HttpClient::
    // GetStringAsync (App.cpp:1812, 1921).
    done({});
  }
  void get_bytes(std::string_view, HttpDone done) override {
    // TODO(gdk): same call, HCHttpCallResponseGetResponseBodyBytes.
    // Replaces HttpClient::GetBufferAsync (App.cpp:2062).
    done({});
  }
  void post_json(std::string_view, std::string, HttpDone done) override {
    // TODO(gdk): HCHttpCallRequestSetRequestBodyString + a Content-Type header.
    // Replaces HttpClient::PostAsync with HttpStringContent (App.cpp:1357, 1430).
    done({});
  }
};

class GdkUdpInlet final : public UdpInlet {
 public:
  bool listen(std::uint16_t,
              std::function<void(const std::uint8_t*, std::size_t)>) override {
    // TODO(gdk): WSAStartup, socket(AF_INET, SOCK_DGRAM), bind, recvfrom on a
    // worker. Replaces DatagramSocket + BindServiceNameAsync (App.cpp:1227-1236).
    // Dev configuration only: a retail console title binding a LAN port is an
    // unusual shape and nothing in the game depends on it.
    return false;
  }
  void close() override {}
};

class GdkUdpOutlet final : public UdpOutlet {
 public:
  bool open(std::string_view, std::uint16_t) override {
    // TODO(gdk): socket + setsockopt(SO_BROADCAST) + sendto. Replaces
    // DatagramSocket::GetOutputStreamAsync to 255.255.255.255 (App.cpp:1253-1259).
    return false;
  }
  void send(const std::uint8_t*, std::size_t) override {}
  void close() override {}
};

class GdkWebSocket final : public WebSocket {
 public:
  bool connect(std::string_view, std::function<void()>) override {
    // TODO(gdk): libHttpClient's WebSocket half — HCWebSocketCreate,
    // HCWebSocketConnectAsync, HCWebSocketSendMessageAsync. Replaces
    // MessageWebSocket in OskiewarLivePublisher.cpp.
    return false;
  }
  bool send(std::string_view) override { return false; }
  void close() override {}
  [[nodiscard]] bool connected() const override { return false; }
};

class GdkMidi final : public Midi {
 public:
  void scan(std::function<void(std::string_view)>) override {
    // TODO(gdk): no GDK surface at all. Windows.Devices.Midi is WinRT; the
    // desktop target can fall back to winmm's midiInOpen/midiInStart, and
    // console has nothing. Replaces DeviceInformation::FindAllAsync
    // (App.cpp:1513-1542).
  }
  void on_message(std::function<void(const MidiEvent&)>) override {}
  void close() override {}
  [[nodiscard]] unsigned input_count() const override { return 0; }
};

class GdkImages final : public ImageDecoder {
 public:
  bool decode(const std::uint8_t*, std::size_t, Bitmap&) override {
    // TODO(gdk): no drop-in. Windows.Graphics.Imaging is WinRT and WIC is not
    // available on the Xbox Game OS, so this wants vendored stb_image for both
    // backends. Replaces BitmapDecoder::CreateAsync (App.cpp:2076-2087).
    return false;
  }
};

class GdkText final : public TextRenderer {
 public:
  [[nodiscard]] bool ready() const override { return false; }
  void draw(const SystemText&) override {
    // SETTLED, and it splits: Direct2D and DirectWrite ARE present on the GDK
    // PC target (a GDK PC title keeps the full desktop API surface) and are NOT
    // present on the Xbox Game OS, which also has no D3D11 at all — only
    // D3D12.x. So on desktop this is App.cpp:2412 verbatim, and on console it
    // is a rewrite: rasterize the packaged ywft-processing-regular.ttf into an
    // atlas and draw it through the sprite path. See xbox/GDK-PORT.md §1.
    // TODO(gdk): wire the desktop path once the renderer leaves App.cpp — this
    // interface needs the ID2D1DeviceContext, which the renderer still owns.
  }
  void glyph(const SystemGlyph&) override {}
};

class GdkPlatform final : public Platform {
 public:
  explicit GdkPlatform(HostEvents& host) : window_(host) {}

  Window& window() override { return window_; }
  Paths& paths() override { return paths_; }
  Clock& clock() override { return clock_; }
  Input& input() override { return input_; }
  Http& http() override { return http_; }
  Midi& midi() override { return midi_; }
  AudioDevice& audio() override { return audio_; }
  ImageDecoder& images() override { return images_; }
  TextRenderer& text() override { return text_; }
  SystemInfo& info() override { return info_; }
  std::unique_ptr<UdpInlet> udp_inlet() override { return std::make_unique<GdkUdpInlet>(); }
  std::unique_ptr<UdpOutlet> udp_outlet() override { return std::make_unique<GdkUdpOutlet>(); }
  std::unique_ptr<WebSocket> websocket() override { return std::make_unique<GdkWebSocket>(); }
  void debug_out(std::string_view line) override {
    OutputDebugStringA(std::string(line).c_str());
  }

 private:
  GdkWindow window_;
  GdkPaths paths_;
  GdkClock clock_;
  GdkInput input_;
  GdkHttp http_;
  GdkMidi midi_;
  GdkAudio audio_;
  GdkImages images_;
  GdkText text_;
  GdkSystemInfo info_;
};

}  // namespace

std::unique_ptr<Platform> make_platform(HostEvents& host) {
  // XGameRuntimeInitialize must come first and, on console, must succeed.
  // On PC it needs the Store-delivered Gaming Runtime plus package identity,
  // so a loose build (and CI) will fail here — the platform is still usable
  // for everything that is plain Win32, which is why this is not fatal.
  const HRESULT started = XGameRuntimeInitialize();
#if defined(_GAMING_XBOX)
  if (FAILED(started)) return nullptr;
#else
  (void)started;
#endif
  return std::make_unique<GdkPlatform>(host);
}

// Identical to ReadPackageBytes (App.cpp:155) once Paths::package works — the
// happy accident of the port is that every read was already _wfopen_s on a wide
// path, so only path acquisition was ever WinRT.
std::string packaged_piece(Paths& paths) {
  const auto root = paths.package();
  if (root.empty()) return {};
  const std::wstring path = root + L"\\oskiewar.js";
  FILE* file = nullptr;
  if (_wfopen_s(&file, path.c_str(), L"rb") != 0 || !file) return {};
  std::fseek(file, 0, SEEK_END);
  const long length = std::ftell(file);
  std::rewind(file);
  if (length <= 0) { std::fclose(file); return {}; }
  std::string source(static_cast<std::size_t>(length), '\0');
  const auto read = std::fread(source.data(), 1, source.size(), file);
  std::fclose(file);
  source.resize(read);
  return source;
}

}  // namespace ac::xbox::platform

// No make_live_source here, and none in any retail configuration. That absence
// is the mechanism, not an omission: a stray call fails at link.
