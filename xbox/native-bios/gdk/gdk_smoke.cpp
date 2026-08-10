// The GDK spike, as an executable.
//
// The paper's question was: do QuickJS, D3D11 and oskiewar.js come up under the
// GDK? This answers it for the PC target — the only one a hosted runner can
// build, since the Xbox Extensions are NDA-gated. Every check prints its own
// verdict; required checks that fail set the exit code, so CI fails loudly and
// names the thing that broke rather than dumping a build log.
//
// Console subsystem on purpose. A GDK title is wWinMain, but a CI harness that
// cannot write to stdout is useless.

#include "../platform/ac_platform.hpp"
#include "../QuickJsEngine.hpp"

#include <windows.h>

#include <d2d1_1.h>
#include <d3d11_1.h>
#include <dwrite.h>
#include <dxgi1_2.h>
#include <wrl/client.h>

#include <GRDK.h>

#include <sys/stat.h>

#include <cstdio>
#include <cstring>
#include <exception>
#include <string>
#include <vector>

using Microsoft::WRL::ComPtr;
using namespace ac::xbox;
using namespace ac::xbox::platform;

namespace {

int g_required_failures = 0;

// `required` false means the check reports but cannot fail the build — used for
// anything that needs hardware or a registered package, neither of which a
// hosted runner has.
void Report(const char* name, bool ok, bool required, const std::string& detail) {
  const char* tag = ok ? "ok  " : (required ? "FAIL" : "skip");
  std::printf("[%s] %-28s %s\n", tag, name, detail.c_str());
  if (!ok && required) ++g_required_failures;
  std::fflush(stdout);
}

std::string Hr(HRESULT hr) {
  char buffer[24];
  std::snprintf(buffer, sizeof(buffer), "0x%08lX", static_cast<unsigned long>(hr));
  return buffer;
}

std::vector<std::uint8_t> ReadPackageBytes(const std::wstring& root, const wchar_t* name) {
  const std::wstring path = root + L"\\" + name;
  FILE* file = nullptr;
  if (_wfopen_s(&file, path.c_str(), L"rb") != 0 || !file) return {};
  std::fseek(file, 0, SEEK_END);
  const long length = std::ftell(file);
  std::rewind(file);
  if (length <= 0) { std::fclose(file); return {}; }
  std::vector<std::uint8_t> bytes(static_cast<std::size_t>(length));
  const auto read = std::fread(bytes.data(), 1, bytes.size(), file);
  std::fclose(file);
  bytes.resize(read);
  return bytes;
}

// The overflow the fix exists for. Naive is counter * 1000000 / frequency;
// split is the form now in App.cpp RefreshClock and in GdkClock. They must
// agree wherever naive does not overflow, and split must stay sane where it
// does.
bool ClockMathHolds(std::string& detail) {
  struct Case { std::int64_t counter, frequency; };
  static constexpr Case cases[] = {
    {0, 10'000'000},
    {1, 10'000'000},
    {9'999'999, 10'000'000},
    {10'000'000, 10'000'000},
    {86'400LL * 10'000'000, 10'000'000},          // one day
    {9LL * 86'400 * 10'000'000, 10'000'000},      // nine days, still safe
    {30LL * 86'400 * 10'000'000, 10'000'000},     // thirty days, naive is negative
    {365LL * 86'400 * 10'000'000, 10'000'000},    // a year of uptime
    {123'456'789, 3'579'545},                     // an odd real frequency
  };
  for (const auto& c : cases) {
    const std::int64_t split =
      c.counter / c.frequency * 1000000 + c.counter % c.frequency * 1000000 / c.frequency;
    if (split < 0) {
      detail = "split form went negative";
      return false;
    }
    const std::int64_t expected_us = static_cast<std::int64_t>(
      static_cast<long double>(c.counter) * 1000000.0L / c.frequency);
    // A microsecond of slack: the split form truncates twice.
    if (split < expected_us - 1 || split > expected_us + 1) {
      detail = "split form disagrees with the exact value at counter=" +
               std::to_string(c.counter);
      return false;
    }
    const bool naive_overflows =
      c.counter > (9'223'372'036'854'775'807LL / 1000000);
    if (!naive_overflows && split != c.counter * 1000000 / c.frequency) {
      detail = "split form disagrees with naive where naive is safe";
      return false;
    }
  }
  detail = "9 cases, incl. 365 days of uptime where the naive form wraps";
  return true;
}

class ProbeGraphics final : public Graphics {
 public:
  Color color{};
  int boxes = 0, lines = 0, triangles = 0, writes = 0, systemWrites = 0;
  void wipe(Color value) override { color = value; }
  void box(const Rect&) override { ++boxes; }
  void line(const Line&) override { ++lines; }
  void triangle(const Triangle&) override { ++triangles; }
  void write(const Text&) override { ++writes; }
  void system_write(const SystemText&) override { ++systemWrites; }
};

class ProbeSound final : public Sound {
 public:
  int synths = 0;
  void synth(const SynthVoice&) override { ++synths; }
  void stop_all() override {}
  int sample_rate() const override { return 48000; }
};

class SilentHost final : public HostEvents {};

}  // namespace

int main(int argc, char** argv) {
  bool window_allowed = true;
  for (int i = 1; i < argc; ++i)
    if (std::strcmp(argv[i], "--no-window") == 0) window_allowed = false;

  std::printf("AC native BIOS — Microsoft GDK (PC target) smoke\n");
  std::printf("GDK edition %d, %s %d %s\n\n", _GRDK_EDITION, _GRDK_MM_NAME,
              _GRDK_FULLYY, _GRDK_QFE_NAME);

  SilentHost host;
  auto plat = make_platform(host);
  Report("platform", plat != nullptr, true,
         plat ? "make_platform linked the GDK backend" : "make_platform returned null");
  if (!plat) return 1;

  // ---- clock
  {
    std::string detail;
    Report("clock split-divide", ClockMathHolds(detail), true, detail);
    const auto first = plat->clock().monotonic_us();
    Sleep(20);
    const auto second = plat->clock().monotonic_us();
    const auto unix_ms = plat->clock().unix_ms();
    // 1.7e12 ms is 2023; anything below it means the epoch conversion is wrong.
    Report("clock monotonic + unix", second > first && unix_ms > 1'700'000'000'000LL, true,
           "advanced " + std::to_string(second - first) + " us, unix_ms " +
           std::to_string(unix_ms));
  }

  // ---- paths and file I/O. The claim under test is that only path acquisition
  // was ever WinRT and every read is already _wfopen_s on a wide path.
  const auto package = plat->paths().package();
  const auto local = plat->paths().local();
  Report("paths.package", !package.empty(), true,
         package.empty() ? "GetModuleFileNameW gave nothing" : "ok");
  Report("paths.local", !local.empty(), true,
         local.empty() ? "no PLS and no LOCALAPPDATA" : "ok");
  if (!package.empty()) std::printf("       package: %ls\n", package.c_str());
  if (!local.empty()) std::printf("       local:   %ls\n", local.c_str());

  if (!local.empty()) {
    const std::wstring scratch = local + L"\\ac-gdk-smoke.txt";
    bool ok = false;
    std::string detail = "write failed";
    FILE* file = nullptr;
    if (_wfopen_s(&file, scratch.c_str(), L"wb") == 0 && file) {
      std::fputs("aesthetic", file);
      std::fclose(file);
      struct _stat64 info{};
      if (_wstat64(scratch.c_str(), &info) == 0 && info.st_size == 9) {
        char back[16]{};
        if (_wfopen_s(&file, scratch.c_str(), L"rb") == 0 && file) {
          const auto read = std::fread(back, 1, sizeof(back) - 1, file);
          std::fclose(file);
          ok = read == 9 && std::strcmp(back, "aesthetic") == 0;
          detail = ok ? "_wfopen_s + _wstat64 round-trip on a wide path" : "read back wrong";
        }
      } else {
        detail = "_wstat64 disagreed";
      }
      _wremove(scratch.c_str());
    }
    Report("file i/o", ok, true, detail);
  }

  // ---- the game, out of the package
  const auto hello = ReadPackageBytes(package, L"oskiewar.js");
  Report("packaged oskiewar.js", hello.size() > 200 * 1024, true,
         std::to_string(hello.size()) + " bytes");

  // ---- QuickJS. The core, and the fastest real signal: the whole interpreter
  // binding has no WinRT in it, so it should cross untouched.
  {
    ProbeGraphics graphics;
    ProbeSound sound;
    Api api{{}, {}, {}, {}, graphics, sound, {}};
    QuickJsEngine engine;
    std::string error;
    auto piece = engine.compile(
      {"gdk-smoke", "1",
       "function boot(){}"
       "function sim(){}"
       "function paint(){wipe(9,8,7);box(1,2,3,4,5,6,7);line(1,2,3,4,2,5,6,7);"
       "triangle(1,2,3,4,5,6,7,8,9);write('OK',8,9,10,11,12,13);systemWrite('HI',20,30,40)}"
       "function act(b){if(b==='A')synth(440,.01)}",
       ""},
      {}, error);
    bool ok = piece != nullptr;
    if (ok) {
      piece->boot(api);
      piece->sim(api);
      piece->paint(api);
      piece->act(api, {"A"});
      ok = graphics.color.r == 9 && graphics.boxes == 1 && graphics.lines == 1 &&
           graphics.triangles == 1 && graphics.writes == 1 &&
           graphics.systemWrites == 1 && sound.synths == 1;
      error = ok ? "compile + boot/sim/paint/act, bindings verified" : "bindings misfired";
    }
    Report("quickjs engine", ok, true, error);
  }

  if (!hello.empty()) {
    QuickJsEngine engine;
    std::string error;
    auto piece = engine.compile(
      {"hello", "packaged", std::string(hello.begin(), hello.end()), ""}, {}, error);
    Report("oskiewar.js compiles", piece != nullptr, true,
           piece ? "top level evaluated under QuickJS-ng" : error);
    if (piece) {
      // Boot is reported, not required: the shipped game expects a live host
      // (clock sync, AC snapshots, a real pad) that this harness does not fake.
      ProbeGraphics graphics;
      ProbeSound sound;
      Api api{{}, {}, {}, {}, graphics, sound, {}};
      std::string detail = "boot + sim + paint ran headless";
      bool ok = true;
      try {
        piece->boot(api);
        piece->sim(api);
        piece->paint(api);
      } catch (const std::exception& thrown) {
        ok = false;
        detail = std::string("threw: ") + thrown.what();
      }
      Report("oskiewar.js runs headless", ok, false, detail);
    }
  }

  // ---- D3D11. Present on the GDK PC target; absent on the Xbox Game OS, which
  // is D3D12.x only. WARP first-class here so the check works on a runner with
  // no GPU.
  ComPtr<ID3D11Device> device;
  ComPtr<ID3D11DeviceContext> context;
  {
    static constexpr D3D_FEATURE_LEVEL levels[] = {
      D3D_FEATURE_LEVEL_11_1, D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_10_1};
    D3D_FEATURE_LEVEL got{};
    HRESULT hr = D3D11CreateDevice(nullptr, D3D_DRIVER_TYPE_HARDWARE, nullptr,
                                   D3D11_CREATE_DEVICE_BGRA_SUPPORT, levels,
                                   ARRAYSIZE(levels), D3D11_SDK_VERSION,
                                   &device, &got, &context);
    const char* driver = "hardware";
    if (FAILED(hr)) {
      driver = "WARP";
      hr = D3D11CreateDevice(nullptr, D3D_DRIVER_TYPE_WARP, nullptr,
                             D3D11_CREATE_DEVICE_BGRA_SUPPORT, levels,
                             ARRAYSIZE(levels), D3D11_SDK_VERSION,
                             &device, &got, &context);
    }
    Report("d3d11 device", SUCCEEDED(hr), true,
           SUCCEEDED(hr) ? std::string(driver) + ", feature level 0x" +
                             std::to_string(static_cast<unsigned>(got))
                         : Hr(hr));
  }

  // ---- Direct2D and DirectWrite. This is the check that settles the port's
  // largest unpriced item. It creates the same objects App.cpp does, including
  // a font face over the packaged TTF, so a pass means the text path is a
  // verbatim carry-over on PC.
  if (device) {
    ComPtr<ID2D1Factory1> d2dFactory;
    HRESULT hr = D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED,
                                   __uuidof(ID2D1Factory1),
                                   reinterpret_cast<void**>(d2dFactory.GetAddressOf()));
    ComPtr<ID2D1Device> d2dDevice;
    ComPtr<ID2D1DeviceContext> d2dContext;
    if (SUCCEEDED(hr)) {
      ComPtr<IDXGIDevice> dxgiDevice;
      hr = device.As(&dxgiDevice);
      if (SUCCEEDED(hr)) hr = d2dFactory->CreateDevice(dxgiDevice.Get(), &d2dDevice);
      if (SUCCEEDED(hr))
        hr = d2dDevice->CreateDeviceContext(D2D1_DEVICE_CONTEXT_OPTIONS_NONE, &d2dContext);
    }
    Report("direct2d", SUCCEEDED(hr), true,
           SUCCEEDED(hr) ? "factory + device + context, sharing the D3D11 device" : Hr(hr));

    ComPtr<IDWriteFactory> dwrite;
    HRESULT wr = DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, __uuidof(IDWriteFactory),
                                     reinterpret_cast<IUnknown**>(dwrite.GetAddressOf()));
    std::string detail = SUCCEEDED(wr) ? "factory" : Hr(wr);
    if (SUCCEEDED(wr)) {
      const std::wstring ttf = package + L"\\Assets\\ywft-processing-regular.ttf";
      ComPtr<IDWriteFontFile> fontFile;
      wr = dwrite->CreateFontFileReference(ttf.c_str(), nullptr, &fontFile);
      BOOL supported = FALSE;
      DWRITE_FONT_FILE_TYPE fileType = DWRITE_FONT_FILE_TYPE_UNKNOWN;
      DWRITE_FONT_FACE_TYPE faceType = DWRITE_FONT_FACE_TYPE_UNKNOWN;
      UINT32 faces = 0;
      if (SUCCEEDED(wr))
        wr = fontFile->Analyze(&supported, &fileType, &faceType, &faces);
      ComPtr<IDWriteFontFace> face;
      if (SUCCEEDED(wr) && supported) {
        IDWriteFontFile* files[] = {fontFile.Get()};
        wr = dwrite->CreateFontFace(faceType, 1, files, 0,
                                    DWRITE_FONT_SIMULATIONS_NONE, &face);
      } else if (SUCCEEDED(wr)) {
        wr = E_FAIL;
      }
      UINT16 glyph = 0;
      if (SUCCEEDED(wr)) {
        const UINT32 codepoint = L'A';
        wr = face->GetGlyphIndices(&codepoint, 1, &glyph);
      }
      detail = SUCCEEDED(wr)
        ? "font face over the packaged YWFT TTF, 'A' is glyph " + std::to_string(glyph)
        : "font face failed " + Hr(wr);
    }
    Report("directwrite", SUCCEEDED(wr), true, detail);
  }

  // ---- shaders. Proves the HLSL compiled for this target and that the runtime
  // accepts the bytecode, which is the whole render graph's entry condition.
  if (device) {
    static constexpr const wchar_t* vertex[] = {
      L"TriangleVertexShader.cso", L"PostVertexShader.cso", L"SpriteVertexShader.cso"};
    static constexpr const wchar_t* pixel[] = {
      L"TrianglePixelShader.cso", L"PostPixelShader.cso", L"SpritePixelShader.cso"};
    bool ok = true;
    std::string missing;
    for (const auto* name : vertex) {
      const auto bytes = ReadPackageBytes(package, name);
      ComPtr<ID3D11VertexShader> shader;
      if (bytes.empty() ||
          FAILED(device->CreateVertexShader(bytes.data(), bytes.size(), nullptr, &shader))) {
        ok = false;
        missing += (missing.empty() ? "" : ", ");
        missing += bytes.empty() ? "missing " : "rejected ";
        missing += std::to_string(bytes.size());
      }
    }
    for (const auto* name : pixel) {
      const auto bytes = ReadPackageBytes(package, name);
      ComPtr<ID3D11PixelShader> shader;
      if (bytes.empty() ||
          FAILED(device->CreatePixelShader(bytes.data(), bytes.size(), nullptr, &shader))) {
        ok = false;
        missing += (missing.empty() ? "" : ", ");
        missing += bytes.empty() ? "missing " : "rejected ";
        missing += std::to_string(bytes.size());
      }
    }
    Report("hlsl shaders", ok, true, ok ? "6 of 6 loaded and created" : missing);
  }

  // ---- window and swap chain. CreateSwapChainForHwnd is the one-line
  // substitution for CreateSwapChainForCoreWindow (App.cpp:559) on this target.
  if (device && window_allowed) {
    const bool opened = plat->window().open(640, 360);
    std::string detail = opened ? "" : "CreateWindowExW failed";
    bool ok = opened;
    if (opened) {
      ComPtr<IDXGIDevice> dxgiDevice;
      ComPtr<IDXGIAdapter> adapter;
      ComPtr<IDXGIFactory2> factory;
      HRESULT hr = device.As(&dxgiDevice);
      if (SUCCEEDED(hr)) hr = dxgiDevice->GetAdapter(&adapter);
      if (SUCCEEDED(hr)) hr = adapter->GetParent(IID_PPV_ARGS(&factory));
      ComPtr<IDXGISwapChain1> swapChain;
      if (SUCCEEDED(hr)) {
        DXGI_SWAP_CHAIN_DESC1 desc{};
        desc.Width = 640;
        desc.Height = 360;
        desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
        desc.SampleDesc.Count = 1;
        desc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
        desc.BufferCount = 2;
        desc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL;
        hr = factory->CreateSwapChainForHwnd(
          device.Get(), static_cast<HWND>(plat->window().native_handle()),
          &desc, nullptr, nullptr, &swapChain);
      }
      ComPtr<ID3D11Texture2D> back;
      ComPtr<ID3D11RenderTargetView> target;
      if (SUCCEEDED(hr)) hr = swapChain->GetBuffer(0, IID_PPV_ARGS(&back));
      if (SUCCEEDED(hr)) hr = device->CreateRenderTargetView(back.Get(), nullptr, &target);
      if (SUCCEEDED(hr)) {
        const float clear[4] = {0.07f, 0.06f, 0.10f, 1.0f};
        context->ClearRenderTargetView(target.Get(), clear);
        hr = swapChain->Present(0, 0);
      }
      plat->window().pump();
      ok = SUCCEEDED(hr);
      detail = ok ? "CreateSwapChainForHwnd + clear + present" : Hr(hr);
    }
    Report("window + swap chain", ok, true, detail);
  } else if (!window_allowed) {
    Report("window + swap chain", false, false, "--no-window");
  }

  // ---- advisory. All three need something a hosted runner does not have: a
  // registered package, a pad, an audio endpoint.
  {
    const auto device_info = plat->info().device();
    Report("gaming runtime", !device_info.family.empty(), false,
           device_info.family.empty()
             ? "XGameRuntimeInitialize did not take (no package identity)"
             : device_info.family + " " + device_info.family_version + " / " +
               device_info.product_name);
    const auto network = plat->info().network();
    Report("network hint", network.level != "none", false, network.level);
    GamepadState pad;
    plat->input().read(pad);
    Report("gameinput", pad.connected, false,
           pad.connected ? "one pad" : "no pad attached");
    Report("xaudio2", plat->audio().open({}), false, "mastering voice");
  }

  std::printf("\n%d required check(s) failed\n", g_required_failures);
  return g_required_failures;
}
