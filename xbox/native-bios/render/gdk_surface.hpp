#pragma once

// The desktop half of the render seam, against an HWND.
//
// Its opposite number is `CoreWindowSurface` in App.cpp. Between them they are
// the entire platform surface of the render path: everything else that creates
// the device, builds the pipelines and draws a frame is written once.
//
// Header-only on purpose. It is two calls, it links into both the smoke and
// any desktop host, and giving it a translation unit would mean a build-file
// edit in two projects to carry thirty lines.

#include "ac_surface.hpp"

namespace ac::xbox::render {

class HwndSurface final : public SurfaceHost {
 public:
  explicit HwndSurface(HWND hwnd) : hwnd_(hwnd) {}

  // The client rect is the truth on desktop — there is no HDMI display mode to
  // ask, and a window is whatever size the user left it. A zero-sized client
  // area (minimised) leaves the caller's seed alone rather than asking DXGI
  // for a zero swap chain.
  void preferred_size(unsigned& width, unsigned& height) override {
    RECT client{};
    if (!GetClientRect(hwnd_, &client)) return;
    const auto w = static_cast<unsigned>(client.right - client.left);
    const auto h = static_cast<unsigned>(client.bottom - client.top);
    if (w > 0 && h > 0) { width = w; height = h; }
  }

  HRESULT create_swap_chain(IDXGIFactory2* factory, ID3D11Device* device,
                            const DXGI_SWAP_CHAIN_DESC1& desc,
                            IDXGISwapChain1** out) override {
    return factory->CreateSwapChainForHwnd(device, hwnd_, &desc, nullptr, nullptr, out);
  }

 private:
  HWND hwnd_;
};

}  // namespace ac::xbox::render
