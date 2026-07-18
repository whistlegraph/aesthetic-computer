#include "pch.h"

using Microsoft::WRL::ComPtr;
using namespace Platform;
using namespace Windows::ApplicationModel::Core;
using namespace Windows::Gaming::Input;
using namespace Windows::UI::Core;
using namespace Windows::Foundation;

namespace XboxLatency {

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
    CreateGraphics();
    CreateAudio(48000);
  }

  virtual void Load(String^) {}
  virtual void Uninitialize() { DestroyAudio(); }

  virtual void Run() {
    Render({0.025f, 0.02f, 0.04f, 1.0f});
    while (!m_closed) {
      m_window->Dispatcher->ProcessEvents(CoreProcessEventsOption::ProcessAllIfPresent);
      PollController();
      if (m_flashFrames > 0) {
        Render(m_flashColor);
        --m_flashFrames;
      } else if (m_needsIdleFrame) {
        Render({0.025f, 0.02f, 0.04f, 1.0f});
        m_needsIdleFrame = false;
      }
      SwitchToThread();
    }
  }

private:
  void OnActivated(CoreApplicationView^, IActivatedEventArgs^) {
    m_window->Activate();
  }

  void OnClosed(CoreWindow^, CoreWindowEventArgs^) { m_closed = true; }

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
    desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
    desc.SampleDesc.Count = 1;
    desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    desc.BufferCount = 2;
    desc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL;
    desc.Scaling = DXGI_SCALING_STRETCH;
    desc.AlphaMode = DXGI_ALPHA_MODE_IGNORE;
    Check(factory->CreateSwapChainForCoreWindow(
      m_device.Get(), reinterpret_cast<IUnknown*>(m_window), &desc, nullptr, &m_swapChain));

    ComPtr<ID3D11Texture2D> backBuffer;
    Check(m_swapChain->GetBuffer(0, IID_PPV_ARGS(&backBuffer)));
    Check(m_device->CreateRenderTargetView(backBuffer.Get(), nullptr, &m_target));
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
    if (pads->Size == 0) { m_previousButtons = 0; return; }
    const auto reading = pads->GetAt(0)->GetCurrentReading();
    const unsigned buttons = static_cast<unsigned>(reading.Buttons);
    const unsigned pressed = buttons & ~m_previousButtons;
    m_previousButtons = buttons;
    if (!pressed) return;

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

  void Render(const std::array<float, 4>& color) {
    if (!m_target) return;
    m_context->OMSetRenderTargets(1, m_target.GetAddressOf(), nullptr);
    m_context->ClearRenderTargetView(m_target.Get(), color.data());
    const HRESULT hr = m_swapChain->Present(1, 0);
    if (FAILED(hr) && hr != DXGI_STATUS_OCCLUDED) Check(hr);
  }

  CoreWindow^ m_window = nullptr;
  bool m_closed = false;
  bool m_needsIdleFrame = false;
  unsigned m_previousButtons = 0;
  unsigned m_flashFrames = 0;
  unsigned m_rateIndex = 1;
  uint32_t m_sampleRate = 48000;
  std::array<float, 4> m_flashColor{1, 1, 1, 1};

  ComPtr<ID3D11Device1> m_device;
  ComPtr<ID3D11DeviceContext1> m_context;
  ComPtr<IDXGISwapChain1> m_swapChain;
  ComPtr<ID3D11RenderTargetView> m_target;
  ComPtr<IXAudio2> m_audio;
  IXAudio2MasteringVoice* m_master = nullptr;
  IXAudio2SourceVoice* m_voice = nullptr;
  std::vector<int16_t> m_samples;
  XAUDIO2_BUFFER m_buffer{};
};

ref class AppSource sealed : public IFrameworkViewSource {
public:
  virtual IFrameworkView^ CreateView() { return ref new App(); }
};

} // namespace XboxLatency

[MTAThread]
int main(Array<String^>^) {
  CoreApplication::Run(ref new XboxLatency::AppSource());
  return 0;
}

