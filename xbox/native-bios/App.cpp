#include "pch.h"
#include "QuickJsEngine.hpp"

using Microsoft::WRL::ComPtr;
using namespace Platform;
using namespace Windows::ApplicationModel::Core;
using namespace Windows::ApplicationModel::Activation;
using namespace Windows::Gaming::Input;
using namespace Windows::Networking::Connectivity;
using namespace Windows::Storage;
using namespace Windows::UI::Core;
using namespace Windows::Foundation;

namespace NativeBios {

using namespace ac::xbox;

static constexpr char kSmokePiece[] = R"JS(
let color=[12,8,24];
function boot(){color=[12,8,24]}
function sim(){}
function paint(){wipe(color[0],color[1],color[2])}
function act(button){
 if(button==='A')color=[20,180,70];
 else if(button==='B')color=[220,45,35];
 else if(button==='X')color=[35,100,230];
 else if(button==='Y')color=[235,190,20];
 synth(button==='Y'?990:660,.04)
}
function leave(){}
)JS";

static void LogTelemetry(const std::string& line) {
  const auto folder = ApplicationData::Current->LocalFolder->Path;
  std::wstring path(folder->Data());
  path += L"\\ac-native-bios.log";
  FILE* file = nullptr;
  if (_wfopen_s(&file, path.c_str(), L"a") != 0 || !file) return;
  std::fwrite(line.data(), 1, line.size(), file);
  std::fwrite("\n", 1, 1, file);
  std::fclose(file);
}

static std::string Utf8(String^ value) {
  if (!value || value->IsEmpty()) return {};
  const int size = WideCharToMultiByte(CP_UTF8, 0, value->Data(), value->Length(),
    nullptr, 0, nullptr, nullptr);
  std::string result(static_cast<std::size_t>(size), '\0');
  WideCharToMultiByte(CP_UTF8, 0, value->Data(), value->Length(), result.data(),
    size, nullptr, nullptr);
  return result;
}

static std::wstring Wide(const std::string& value) {
  if (value.empty()) return {};
  const int size = MultiByteToWideChar(CP_UTF8, 0, value.data(),
    static_cast<int>(value.size()), nullptr, 0);
  std::wstring result(static_cast<std::size_t>(size), L'\0');
  MultiByteToWideChar(CP_UTF8, 0, value.data(), static_cast<int>(value.size()),
    result.data(), size);
  return result;
}

static std::array<unsigned char, 7> BlockGlyph(char value) {
  if (value >= 'a' && value <= 'z') value -= 'a' - 'A';
  switch (value) {
    case 'A': return {14,17,17,31,17,17,17}; case 'B': return {30,17,17,30,17,17,30};
    case 'C': return {14,17,16,16,16,17,14}; case 'D': return {30,17,17,17,17,17,30};
    case 'E': return {31,16,16,30,16,16,31}; case 'F': return {31,16,16,30,16,16,16};
    case 'G': return {14,17,16,23,17,17,15}; case 'H': return {17,17,17,31,17,17,17};
    case 'I': return {31,4,4,4,4,4,31}; case 'J': return {7,2,2,2,18,18,12};
    case 'K': return {17,18,20,24,20,18,17}; case 'L': return {16,16,16,16,16,16,31};
    case 'M': return {17,27,21,21,17,17,17}; case 'N': return {17,25,21,19,17,17,17};
    case 'O': return {14,17,17,17,17,17,14}; case 'P': return {30,17,17,30,16,16,16};
    case 'Q': return {14,17,17,17,21,18,13}; case 'R': return {30,17,17,30,20,18,17};
    case 'S': return {15,16,16,14,1,1,30}; case 'T': return {31,4,4,4,4,4,4};
    case 'U': return {17,17,17,17,17,17,14}; case 'V': return {17,17,17,17,17,10,4};
    case 'W': return {17,17,17,21,21,21,10}; case 'X': return {17,17,10,4,10,17,17};
    case 'Y': return {17,17,10,4,4,4,4}; case 'Z': return {31,1,2,4,8,16,31};
    case '0': return {14,17,19,21,25,17,14}; case '1': return {4,12,4,4,4,4,14};
    case '2': return {14,17,1,2,4,8,31}; case '3': return {30,1,1,14,1,1,30};
    case '4': return {2,6,10,18,31,2,2}; case '5': return {31,16,16,30,1,1,30};
    case '6': return {14,16,16,30,17,17,14}; case '7': return {31,1,2,4,8,8,8};
    case '8': return {14,17,17,14,17,17,14}; case '9': return {14,17,17,15,1,1,14};
    case '.': return {0,0,0,0,0,12,12}; case ':': return {0,4,4,0,4,4,0};
    case '-': return {0,0,0,31,0,0,0}; case '/': return {1,2,2,4,8,8,16};
    case '!': return {4,4,4,4,4,0,4}; case ' ': return {0,0,0,0,0,0,0};
    default: return {14,17,1,2,4,0,4};
  }
}

class HostGraphics final : public Graphics {
 public:
  std::function<void(Color)> on_wipe;
  std::function<void(const ac::xbox::Rect&)> on_box;
  std::function<void(const ac::xbox::Text&)> on_write;
  void wipe(Color color) override { if (on_wipe) on_wipe(color); }
  void box(const ac::xbox::Rect& rect) override { if (on_box) on_box(rect); }
  void line(const ac::xbox::Line&) override {}
  void write(const ac::xbox::Text& text) override { if (on_write) on_write(text); }
};
class HostSound final : public Sound {
 public:
  std::function<void(const SynthVoice&)> on_synth;
  std::function<void()> on_stop;
  std::function<int()> get_rate;
  void synth(const SynthVoice& voice) override { if (on_synth) on_synth(voice); }
  void stop_all() override { if (on_stop) on_stop(); }
  int sample_rate() const override { return get_rate ? get_rate() : 0; }
};

static void Check(HRESULT hr) {
  if (FAILED(hr)) throw Exception::CreateException(hr);
}

ref class App sealed : public IFrameworkView {
public:
  virtual void Initialize(CoreApplicationView^ view) {
    view->Activated += ref new TypedEventHandler<CoreApplicationView^, IActivatedEventArgs^>(
      this, &App::OnActivated);
  }

  virtual void SetWindow(CoreWindow^ window) {
    m_window = window;
    window->Closed += ref new TypedEventHandler<CoreWindow^, CoreWindowEventArgs^>(
      this, &App::OnClosed);
    SystemNavigationManager::GetForCurrentView()->BackRequested +=
      ref new EventHandler<BackRequestedEventArgs^>(this, &App::OnBackRequested);
    CreateGraphics();
    CreateAudio(48000);
    m_graphics = std::make_unique<HostGraphics>();
    m_sound = std::make_unique<HostSound>();
    m_graphics->on_wipe = [this](Color color) {
      m_frameColor = {color.r / 255.f, color.g / 255.f, color.b / 255.f, color.a / 255.f};
    };
    m_graphics->on_box = [this](const ac::xbox::Rect& rect) { m_frameRects.push_back(rect); };
    m_graphics->on_write = [this](const ac::xbox::Text& text) { m_frameTexts.push_back(text); };
    m_sound->on_synth = [this](const SynthVoice&) { if (m_voice) TriggerAudio(1); };
    m_sound->on_stop = [this]() { if (m_voice) { m_voice->Stop(0); m_voice->FlushSourceBuffers(); } };
    m_sound->get_rate = [this]() { return static_cast<int>(m_sampleRate); };
    m_api = std::make_unique<Api>(Api{{1920, 1080, 1}, {}, {}, {}, *m_graphics, *m_sound});
    m_api->system.version = "1.0.0.10";
    m_api->telemetry = [](std::string_view line) {
      std::string safe(line);
      for (auto& character : safe) if (character == '\n' || character == '\r') character = ' ';
      if (safe.size() > 1024) safe.resize(1024);
      LogTelemetry("AC_NATIVE_" + safe);
    };
    RefreshCapabilities(true);
    m_engine = std::make_unique<QuickJsEngine>();
    m_supervisor = std::make_unique<PieceSupervisor>(*m_engine);
    std::string error;
    if (!m_supervisor->stage({"smoke", "bundled-v1", kSmokePiece, "bundled"}, *m_api, error) ||
        !m_supervisor->activate(*m_api)) {
      OutputDebugStringA(("AC_NATIVE_BIOS_BOOT_ERROR " + error + "\n").c_str());
    } else {
      OutputDebugStringA("AC_NATIVE_BIOS_READY engine=quickjs-ng piece=smoke\n");
      LogTelemetry("AC_NATIVE_BIOS_READY engine=quickjs-ng piece=smoke");
    }
  }

  virtual void Load(String^) {}
  virtual void Uninitialize() { DestroyAudio(); }

  virtual void Run() {
    Render({0.025f, 0.02f, 0.04f, 1.0f});
    while (!m_closed) {
      m_window->Dispatcher->ProcessEvents(CoreProcessEventsOption::ProcessAllIfPresent);
      RefreshCapabilities(false);
      PollController();
      PollLivePiece();
      RefreshClock();
      m_frameRects.clear();
      m_frameTexts.clear();
      if (m_supervisor && m_supervisor->active()) {
        try {
          m_supervisor->active()->sim(*m_api);
          ++m_api->sim_count;
          m_supervisor->active()->paint(*m_api);
          ++m_api->paint_count;
        } catch (const std::exception& error) {
          OutputDebugStringA((std::string("AC_NATIVE_BIOS_JS_ERROR ") + error.what() + "\n").c_str());
          LogTelemetry(std::string("AC_NATIVE_BIOS_JS_ERROR ") + error.what());
          m_supervisor->rollback(*m_api);
        }
      }
      if (m_flashFrames > 0) {
        Render(m_flashColor);
        --m_flashFrames;
      } else Render(m_frameColor);
      SwitchToThread();
    }
  }

private:
  void OnActivated(CoreApplicationView^, IActivatedEventArgs^) {
    m_window->Activate();
  }

  void OnClosed(CoreWindow^, CoreWindowEventArgs^) { m_closed = true; }

  void OnBackRequested(Object^, BackRequestedEventArgs^ args) {
    // Xbox promotes the B button to platform Back. Keep the app alive so the
    // same physical press remains available to the piece through Gamepad.
    args->Handled = true;
  }

  void CreateGraphics() {
    UINT flags = D3D11_CREATE_DEVICE_BGRA_SUPPORT;
#if defined(_DEBUG)
    flags |= D3D11_CREATE_DEVICE_DEBUG;
#endif
    const D3D_FEATURE_LEVEL levels[] = {
      D3D_FEATURE_LEVEL_11_1, D3D_FEATURE_LEVEL_11_0,
      D3D_FEATURE_LEVEL_10_1, D3D_FEATURE_LEVEL_10_0,
    };
    D3D_FEATURE_LEVEL actual{};
    ComPtr<ID3D11Device> baseDevice;
    ComPtr<ID3D11DeviceContext> baseContext;
    Check(D3D11CreateDevice(nullptr, D3D_DRIVER_TYPE_HARDWARE, nullptr, flags,
      levels, ARRAYSIZE(levels), D3D11_SDK_VERSION, &baseDevice, &actual, &baseContext));
    Check(baseDevice.As(&m_device));
    Check(baseContext.As(&m_context));

    ComPtr<IDXGIDevice1> dxgiDevice;
    ComPtr<IDXGIAdapter> adapter;
    ComPtr<IDXGIFactory2> factory;
    Check(m_device.As(&dxgiDevice));
    Check(dxgiDevice->GetAdapter(&adapter));
    Check(adapter->GetParent(IID_PPV_ARGS(&factory)));

    DXGI_SWAP_CHAIN_DESC1 desc{};
    // Xbox may choose its 8x8 placeholder surface when width/height are left at
    // zero for a CoreWindow swap chain. Solid clears still stretch fullscreen,
    // hiding the mistake while every useful drawing coordinate gets clipped.
    desc.Width = 1920;
    desc.Height = 1080;
    desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
    desc.SampleDesc.Count = 1;
    desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    desc.BufferCount = 2;
    desc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL;
    desc.Scaling = DXGI_SCALING_STRETCH;
    desc.AlphaMode = DXGI_ALPHA_MODE_IGNORE;
    Check(factory->CreateSwapChainForCoreWindow(
      m_device.Get(), reinterpret_cast<IUnknown*>(m_window), &desc, nullptr, &m_swapChain));

    Check(m_swapChain->GetBuffer(0, IID_PPV_ARGS(&m_backBuffer)));
    D3D11_TEXTURE2D_DESC backBufferDesc{};
    m_backBuffer->GetDesc(&backBufferDesc);
    m_frameWidth = backBufferDesc.Width;
    m_frameHeight = backBufferDesc.Height;
    Check(m_device->CreateRenderTargetView(m_backBuffer.Get(), nullptr, &m_target));

    Check(D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED,
      IID_PPV_ARGS(&m_d2dFactory)));
    ComPtr<IDXGIDevice> d2dDxgiDevice;
    Check(m_device.As(&d2dDxgiDevice));
    Check(m_d2dFactory->CreateDevice(d2dDxgiDevice.Get(), &m_d2dDevice));
    Check(m_d2dDevice->CreateDeviceContext(D2D1_DEVICE_CONTEXT_OPTIONS_NONE, &m_d2dContext));
    ComPtr<IDXGISurface> surface;
    Check(m_backBuffer.As(&surface));
    const auto bitmapProperties = D2D1::BitmapProperties1(
      D2D1_BITMAP_OPTIONS_TARGET | D2D1_BITMAP_OPTIONS_CANNOT_DRAW,
      D2D1::PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_IGNORE));
    Check(m_d2dContext->CreateBitmapFromDxgiSurface(surface.Get(), &bitmapProperties,
      &m_d2dTarget));
    m_d2dContext->SetTarget(m_d2dTarget.Get());
    Check(DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, __uuidof(IDWriteFactory),
      reinterpret_cast<IUnknown**>(m_dwriteFactory.GetAddressOf())));
    Check(m_d2dContext->CreateSolidColorBrush(D2D1::ColorF(D2D1::ColorF::White),
      &m_textBrush));
  }

  void CreateAudio(uint32_t sampleRate) {
    DestroyAudio();
    m_sampleRate = sampleRate;
    Check(XAudio2Create(&m_audio));
    Check(m_audio->CreateMasteringVoice(&m_master, XAUDIO2_DEFAULT_CHANNELS,
      sampleRate, 0, nullptr, nullptr, AudioCategory_GameEffects));

    WAVEFORMATEX format{};
    format.wFormatTag = WAVE_FORMAT_PCM;
    format.nChannels = 1;
    format.nSamplesPerSec = sampleRate;
    format.wBitsPerSample = 16;
    format.nBlockAlign = format.nChannels * format.wBitsPerSample / 8;
    format.nAvgBytesPerSec = format.nSamplesPerSec * format.nBlockAlign;
    Check(m_audio->CreateSourceVoice(&m_voice, &format, 0, XAUDIO2_DEFAULT_FREQ_RATIO));

    const uint32_t frames = sampleRate / 20; // 50 ms; data is allocated once per rate.
    m_samples.resize(frames);
    constexpr double tau = 6.2831853071795864769;
    for (uint32_t i = 0; i < frames; ++i) {
      const double envelope = 1.0 - static_cast<double>(i) / frames;
      m_samples[i] = static_cast<int16_t>(std::sin(tau * 880.0 * i / sampleRate) * envelope * 12000.0);
    }
    m_buffer = {};
    m_buffer.AudioBytes = static_cast<UINT32>(m_samples.size() * sizeof(int16_t));
    m_buffer.pAudioData = reinterpret_cast<const BYTE*>(m_samples.data());
    m_buffer.Flags = XAUDIO2_END_OF_STREAM;

    wchar_t line[160];
    swprintf_s(line, L"AC_NATIVE_AUDIO_READY rate=%uHz frames=%u\n", sampleRate, frames);
    OutputDebugStringW(line);
  }

  void DestroyAudio() {
    if (m_voice) { m_voice->DestroyVoice(); m_voice = nullptr; }
    if (m_master) { m_master->DestroyVoice(); m_master = nullptr; }
    m_audio.Reset();
  }

  void TriggerAudio(unsigned buttonMask) {
    LARGE_INTEGER before{}, after{}, frequency{};
    QueryPerformanceFrequency(&frequency);
    QueryPerformanceCounter(&before);
    m_voice->Stop(0);
    m_voice->FlushSourceBuffers();
    Check(m_voice->SubmitSourceBuffer(&m_buffer));
    Check(m_voice->Start(0));
    QueryPerformanceCounter(&after);
    const double micros = (after.QuadPart - before.QuadPart) * 1000000.0 / frequency.QuadPart;
    wchar_t line[200];
    swprintf_s(line,
      L"AC_NATIVE_INPUT button=0x%X rate=%uHz submit=%.2fus qpc=%lld\n",
      buttonMask, m_sampleRate, micros, before.QuadPart);
    OutputDebugStringW(line);
  }

  void PollController() {
    const auto pads = Gamepad::Gamepads;
    m_api->gamepad.down.clear();
    if (pads->Size == 0) {
      m_previousButtons = 0;
      m_api->gamepad.left_x = m_api->gamepad.left_y = 0;
      m_api->gamepad.right_x = m_api->gamepad.right_y = 0;
      m_api->gamepad.left_trigger = m_api->gamepad.right_trigger = 0;
      return;
    }
    const auto reading = pads->GetAt(0)->GetCurrentReading();
    const unsigned buttons = static_cast<unsigned>(reading.Buttons);
    m_api->gamepad.left_x = static_cast<float>(reading.LeftThumbstickX);
    m_api->gamepad.left_y = static_cast<float>(reading.LeftThumbstickY);
    m_api->gamepad.right_x = static_cast<float>(reading.RightThumbstickX);
    m_api->gamepad.right_y = static_cast<float>(reading.RightThumbstickY);
    m_api->gamepad.left_trigger = static_cast<float>(reading.LeftTrigger);
    m_api->gamepad.right_trigger = static_cast<float>(reading.RightTrigger);
    struct ButtonName { GamepadButtons bit; const char* name; };
    static constexpr ButtonName names[] = {
      {GamepadButtons::A, "A"}, {GamepadButtons::B, "B"},
      {GamepadButtons::X, "X"}, {GamepadButtons::Y, "Y"},
      {GamepadButtons::DPadUp, "ArrowUp"}, {GamepadButtons::DPadDown, "ArrowDown"},
      {GamepadButtons::DPadLeft, "ArrowLeft"}, {GamepadButtons::DPadRight, "ArrowRight"},
      {GamepadButtons::LeftShoulder, "LeftShoulder"},
      {GamepadButtons::RightShoulder, "RightShoulder"},
      {GamepadButtons::Menu, "Menu"}, {GamepadButtons::View, "View"},
      {GamepadButtons::LeftThumbstick, "LeftStick"},
      {GamepadButtons::RightThumbstick, "RightStick"}
    };
    for (const auto& named : names)
      if (buttons & static_cast<unsigned>(named.bit)) m_api->gamepad.down.insert(named.name);
    const unsigned pressed = buttons & ~m_previousButtons;
    m_previousButtons = buttons;
    if (!pressed) return;

    if (m_supervisor && m_supervisor->active()) {
      for (const auto& named : names) {
        if (pressed & static_cast<unsigned>(named.bit)) {
          LARGE_INTEGER now{}, frequency{};
          QueryPerformanceCounter(&now);
          QueryPerformanceFrequency(&frequency);
          LogTelemetry("AC_NATIVE_INPUT button=" + std::string(named.name) +
            " qpc_us=" + std::to_string(
              static_cast<unsigned long long>(now.QuadPart * 1000000 / frequency.QuadPart)));
          try { m_supervisor->active()->act(*m_api, {named.name, 1, 0}); }
          catch (const std::exception& error) {
            OutputDebugStringA((std::string("AC_NATIVE_BIOS_ACT_ERROR ") + error.what() + "\n").c_str());
          }
        }
      }
    }

    const unsigned menu = static_cast<unsigned>(GamepadButtons::Menu);
    if (pressed & menu) {
      static const uint32_t rates[] = {44100, 48000, 96000};
      m_rateIndex = (m_rateIndex + 1) % ARRAYSIZE(rates);
      CreateAudio(rates[m_rateIndex]);
      m_flashColor = m_sampleRate == 44100
        ? std::array<float, 4>{0.15f, 0.35f, 0.95f, 1.0f}
        : m_sampleRate == 48000
          ? std::array<float, 4>{0.15f, 0.9f, 0.35f, 1.0f}
          : std::array<float, 4>{0.9f, 0.2f, 0.8f, 1.0f};
    } else {
      TriggerAudio(pressed);
      m_flashColor = {1.0f, 0.72f, 0.08f, 1.0f};
    }
    m_flashFrames = 2;
    m_needsIdleFrame = true;
  }

  void RefreshClock() {
    LARGE_INTEGER counter{}, frequency{};
    QueryPerformanceCounter(&counter);
    QueryPerformanceFrequency(&frequency);
    m_api->clock.monotonic_us = static_cast<std::uint64_t>(
      counter.QuadPart * 1000000 / frequency.QuadPart);
    m_api->clock.seconds = static_cast<double>(counter.QuadPart) / frequency.QuadPart;
    m_api->seconds = m_api->clock.seconds;
    FILETIME fileTime{};
    GetSystemTimeAsFileTime(&fileTime);
    ULARGE_INTEGER ticks{};
    ticks.LowPart = fileTime.dwLowDateTime;
    ticks.HighPart = fileTime.dwHighDateTime;
    m_api->clock.unix_ms = static_cast<std::int64_t>(
      (ticks.QuadPart - 116444736000000000ULL) / 10000ULL);
  }

  void RefreshCapabilities(bool force) {
    const auto now = GetTickCount64();
    if (!force && now < m_nextCapabilityPollMs) return;
    m_nextCapabilityPollMs = now + 1000;
    const auto profile = NetworkInformation::GetInternetConnectionProfile();
    m_api->system.online = false;
    m_api->system.network_level = "none";
    m_api->system.network_name.clear();
    if (profile) {
      m_api->system.network_name = Utf8(profile->ProfileName);
      switch (profile->GetNetworkConnectivityLevel()) {
        case NetworkConnectivityLevel::InternetAccess:
          m_api->system.network_level = "internet"; m_api->system.online = true; break;
        case NetworkConnectivityLevel::ConstrainedInternetAccess:
          m_api->system.network_level = "constrained"; break;
        case NetworkConnectivityLevel::LocalAccess:
          m_api->system.network_level = "local"; break;
        default: break;
      }
    }
    m_api->gamepad.controllers.clear();
    for (auto raw : RawGameController::RawGameControllers) {
      ControllerInfo info;
      info.id = Utf8(raw->NonRoamableId);
      info.name = Utf8(raw->DisplayName);
      info.vendor_id = raw->HardwareVendorId;
      info.product_id = raw->HardwareProductId;
      info.axes = raw->AxisCount;
      info.buttons = raw->ButtonCount;
      info.switches = raw->SwitchCount;
      info.gamepad = Gamepad::FromGameController(raw) != nullptr;
      m_api->gamepad.controllers.push_back(std::move(info));
    }
    std::string inventory = "online=" + std::to_string(m_api->system.online) +
      " network=" + m_api->system.network_level + " profile=" + m_api->system.network_name +
      " controllers=" + std::to_string(m_api->gamepad.controllers.size());
    for (const auto& controller : m_api->gamepad.controllers) {
      inventory += " | " + controller.name + " vendor=" +
        std::to_string(controller.vendor_id) + " product=" +
        std::to_string(controller.product_id) + " axes=" +
        std::to_string(controller.axes) + " buttons=" +
        std::to_string(controller.buttons) + " switches=" +
        std::to_string(controller.switches) + " gamepad=" +
        std::to_string(controller.gamepad);
    }
    if (inventory != m_lastCapabilityInventory) {
      m_lastCapabilityInventory = inventory;
      LogTelemetry("AC_NATIVE_CAPABILITIES " + inventory);
    }
  }

  void PollLivePiece() {
    const auto now = GetTickCount64();
    if (now < m_nextLivePollMs) return;
    m_nextLivePollMs = now + 500;

    const auto folder = ApplicationData::Current->LocalFolder->Path;
    std::wstring path(folder->Data());
    path += L"\\live-piece.js";
    struct _stat64 info{};
    if (_wstat64(path.c_str(), &info) != 0) return;
    const auto signature = static_cast<unsigned long long>(info.st_mtime) ^
      (static_cast<unsigned long long>(info.st_size) << 32);
    if (signature == m_livePieceSignature) return;
    m_livePieceSignature = signature;

    if (info.st_size <= 0 || info.st_size > 2 * 1024 * 1024) {
      LogTelemetry("AC_NATIVE_LIVE_REJECT reason=source-size");
      return;
    }
    FILE* file = nullptr;
    if (_wfopen_s(&file, path.c_str(), L"rb") != 0 || !file) {
      LogTelemetry("AC_NATIVE_LIVE_REJECT reason=open-failed");
      return;
    }
    std::string source(static_cast<std::size_t>(info.st_size), '\0');
    const auto bytes = std::fread(source.data(), 1, source.size(), file);
    std::fclose(file);
    source.resize(bytes);

    std::string error;
    if (!m_supervisor->stage({"live", std::to_string(signature), source, "local-dev"},
                             *m_api, error) || !m_supervisor->activate(*m_api)) {
      for (auto& character : error) if (character == '\n' || character == '\r') character = ' ';
      LogTelemetry("AC_NATIVE_LIVE_REJECT reason=" + error);
      return;
    }
    LogTelemetry("AC_NATIVE_LIVE_READY bytes=" + std::to_string(source.size()) +
      " generation=" + std::to_string(m_supervisor->generation()));
  }

  void Render(const std::array<float, 4>& color) {
    if (!m_target) return;
    if (!m_frameTexts.empty() || !m_frameRects.empty()) {
      const auto byte = [](float value) {
        return static_cast<unsigned>(255.0f * (std::max)(0.0f, (std::min)(1.0f, value)));
      };
      const uint32_t background = 0xff000000u | (byte(color[0]) << 16) |
        (byte(color[1]) << 8) | byte(color[2]);
      m_cpuFrame.assign(static_cast<std::size_t>(m_frameWidth) * m_frameHeight, background);
      const auto fill = [this](int left, int top, int right, int bottom, uint32_t value) {
        left = (std::max)(0, left); top = (std::max)(0, top);
        right = (std::min)(static_cast<int>(m_frameWidth), right);
        bottom = (std::min)(static_cast<int>(m_frameHeight), bottom);
        for (int y = top; y < bottom; ++y)
          std::fill(m_cpuFrame.begin() + static_cast<std::size_t>(y) * m_frameWidth + left,
            m_cpuFrame.begin() + static_cast<std::size_t>(y) * m_frameWidth + right, value);
      };
      const float scaleX = m_frameWidth / 1920.0f;
      const float scaleY = m_frameHeight / 1080.0f;
      const auto packed = [](Color value) {
        return 0xff000000u | (static_cast<uint32_t>(value.r) << 16) |
          (static_cast<uint32_t>(value.g) << 8) | value.b;
      };
      for (const auto& rect : m_frameRects) {
        fill(static_cast<int>(rect.x * scaleX), static_cast<int>(rect.y * scaleY),
          static_cast<int>((rect.x + rect.width) * scaleX),
          static_cast<int>((rect.y + rect.height) * scaleY), packed(rect.color));
      }
      for (const auto& text : m_frameTexts) {
        const int cell = (std::max)(2, static_cast<int>(text.size / 7.0f * scaleY));
        const int originX = static_cast<int>(text.x * scaleX);
        int penX = originX;
        int penY = static_cast<int>(text.y * scaleY);
        const uint32_t ink = packed(text.color);
        for (const char character : text.value) {
          if (character == '\n') { penX = originX; penY += cell * 9; continue; }
          const auto glyph = BlockGlyph(character);
          for (int row = 0; row < 7; ++row) for (int column = 0; column < 5; ++column) {
            if (!(glyph[row] & (1 << (4 - column)))) continue;
            fill(penX + column * cell, penY + row * cell,
              penX + (column + 1) * cell - 1, penY + (row + 1) * cell - 1, ink);
          }
          penX += cell * 6;
        }
      }
      m_context->OMSetRenderTargets(0, nullptr, nullptr);
      m_context->UpdateSubresource(m_backBuffer.Get(), 0, nullptr, m_cpuFrame.data(),
        m_frameWidth * sizeof(uint32_t), 0);
      if (!m_loggedTextFrame) {
        LogTelemetry("AC_NATIVE_CPU_FRAME texts=" + std::to_string(m_frameTexts.size()) +
          " boxes=" + std::to_string(m_frameRects.size()) + " surface=" +
          std::to_string(m_frameWidth) + "x" + std::to_string(m_frameHeight));
        m_loggedTextFrame = true;
      }
    } else {
      m_context->OMSetRenderTargets(1, m_target.GetAddressOf(), nullptr);
      m_context->ClearRenderTargetView(m_target.Get(), color.data());
    }
    const HRESULT hr = m_swapChain->Present(1, 0);
    if (FAILED(hr) && hr != DXGI_STATUS_OCCLUDED) Check(hr);
  }

  CoreWindow^ m_window = nullptr;
  bool m_closed = false;
  bool m_needsIdleFrame = false;
  unsigned m_previousButtons = 0;
  unsigned m_flashFrames = 0;
  unsigned m_rateIndex = 1;
  unsigned long long m_livePieceSignature = 0;
  unsigned long long m_nextLivePollMs = 0;
  unsigned long long m_nextCapabilityPollMs = 0;
  unsigned m_frameWidth = 0;
  unsigned m_frameHeight = 0;
  std::string m_lastCapabilityInventory;
  uint32_t m_sampleRate = 48000;
  std::array<float, 4> m_flashColor{1, 1, 1, 1};
  std::array<float, 4> m_frameColor{0.025f, 0.02f, 0.04f, 1.0f};
  std::vector<ac::xbox::Rect> m_frameRects;
  std::vector<ac::xbox::Text> m_frameTexts;
  bool m_loggedTextFrame = false;

  ComPtr<ID3D11Device1> m_device;
  ComPtr<ID3D11DeviceContext1> m_context;
  ComPtr<IDXGISwapChain1> m_swapChain;
  ComPtr<ID3D11Texture2D> m_backBuffer;
  ComPtr<ID3D11RenderTargetView> m_target;
  ComPtr<ID2D1Factory1> m_d2dFactory;
  ComPtr<ID2D1Device> m_d2dDevice;
  ComPtr<ID2D1DeviceContext> m_d2dContext;
  ComPtr<ID2D1Bitmap1> m_d2dTarget;
  ComPtr<ID2D1SolidColorBrush> m_textBrush;
  ComPtr<IDWriteFactory> m_dwriteFactory;
  ComPtr<IXAudio2> m_audio;
  IXAudio2MasteringVoice* m_master = nullptr;
  IXAudio2SourceVoice* m_voice = nullptr;
  std::vector<int16_t> m_samples;
  std::vector<uint32_t> m_cpuFrame;
  XAUDIO2_BUFFER m_buffer{};
  std::unique_ptr<HostGraphics> m_graphics;
  std::unique_ptr<HostSound> m_sound;
  std::unique_ptr<Api> m_api;
  std::unique_ptr<QuickJsEngine> m_engine;
  std::unique_ptr<PieceSupervisor> m_supervisor;
};

ref class AppSource sealed : public IFrameworkViewSource {
public:
  virtual IFrameworkView^ CreateView() { return ref new App(); }
};

} // namespace NativeBios

[MTAThread]
int main(Array<String^>^) {
  CoreApplication::Run(ref new NativeBios::AppSource());
  return 0;
}
