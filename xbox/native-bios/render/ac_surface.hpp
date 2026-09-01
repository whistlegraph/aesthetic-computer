#pragma once

// The render path's seam with the OS.
//
// `platform/ac_platform.hpp` faces the machine for the app as a whole. This
// faces it for the renderer alone, and it is deliberately much smaller: of the
// ~800 lines that create the D3D11 device, build the three pipelines and draw a
// frame, exactly two touch anything platform-specific. Both are here.
//
// Everything else in that path — the device, the swap-chain description, the
// scene texture, Direct2D, DirectWrite, the shaders and every draw — is plain
// desktop-and-console C++ that compiles unchanged against either backend. That
// is the whole reason a Steam build is not a renderer rewrite: on PC the
// answers are `CreateSwapChainForHwnd` and DirectWrite verbatim, and only the
// console loses D3D11 (see `xbox/GDK-PORT.md` §1a).
//
// Still outside this seam, and named rather than hidden: `ReadPackageBytes`
// resolves the packaged `.cso` shaders and TTFs through
// `Windows.ApplicationModel.Package`. That belongs on `platform::Paths`, whose
// `package()` already answers it, and moves when App.cpp adopts the platform
// layer.

#include <d3d11.h>
#include <dxgi1_2.h>

namespace ac::xbox::render {

class SurfaceHost {
 public:
  virtual ~SurfaceHost() = default;

  // The size to ask the swap chain for, in raw pixels. A backend that cannot
  // answer must leave both untouched: the caller seeds them with an explicit
  // 1080p, because DXGI hands back an 8x8 placeholder when a CoreWindow swap
  // chain is created at zero, and solid clears still stretch fullscreen over
  // that mistake while every useful drawing coordinate gets clipped.
  virtual void preferred_size(unsigned& width, unsigned& height) = 0;

  // `CreateSwapChainForCoreWindow` under WinRT, `CreateSwapChainForHwnd` on
  // desktop. The one call in the render path that cannot be written once.
  virtual HRESULT create_swap_chain(IDXGIFactory2* factory, ID3D11Device* device,
                                    const DXGI_SWAP_CHAIN_DESC1& desc,
                                    IDXGISwapChain1** out) = 0;
};

}  // namespace ac::xbox::render
