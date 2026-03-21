# Android GPU Rendering Investigation

**Date:** 2026-03-19
**Device:** Unihertz Jelly Star (Android 13, Mali-G57 MC2, 350x622 @ 1.375dpr)
**Piece:** `$fsf` (KidLisp — blur, sharpen, zoom, scroll, contrast, flood)

## Problem

`$fsf` renders only blinking solid colors on Android — no patterns, no accumulated
effects. Desktop rendering works correctly. Three prior fix commits failed to resolve
the issue.

## Timeline

| Commit | Approach | Result |
|--------|----------|--------|
| `7c26d6590` | Replace `exp()` with precomputed weight array, `texture()` → `texelFetch`, `gl.finish()` between blur passes | Still failing |
| `e27ac5b93` | Fully unrolled blur shaders (no arrays, no loops, no `break`), framebuffer completeness checks, GPU frame logging every 8 frames, cache key v3 | Intermittent — effects both succeed AND fail in same 8-frame window |
| `92ed40364` | Layer0 diagnostic logging (pixel density, post-composite commands) | Confirmed effects execute but GPU output is unreliable |

## Root Cause

GPU telemetry at frame 480 on Mali-G57 MC2:
```
ok=[zoom, blur, sharpen, contrast]  failed=[blur, sharpen, contrast]
```

Blur, sharpen, and contrast **intermittently produce blank output** on Mali-G57.
The sanity check catches some failures and falls back to CPU, but the cycling between
GPU success and GPU failure creates visible glitching (frames alternate between
rendered and blank).

`zoom` is stable (single-pass, no multi-texture reads). The failing effects all involve
either multi-pass rendering (blur: H pass → V pass) or multiple texture samples in one
fragment shader invocation.

## Resolution

**Immediate fix:** `mobileSafeMode` flag in `gpu-effects.mjs` — detected via
`WEBGL_debug_renderer_info` renderer string matching `/mali|adreno/i`. When active:
- `gpuBlur` → returns `false` (CPU fallback)
- `gpuSharpen` → returns `false` (CPU fallback)
- `gpuComposite` → returns `false` (CPU fallback)
- Single-pass effects (spin, zoom, scroll, flood, shear, suck) → still GPU-accelerated

## Research: Cross-Platform WebGL2 GPU Effects

### Existing Libraries

| Library | WebGL | Blur Strategy | Mobile Tested | Notes |
|---------|-------|---------------|---------------|-------|
| [glfx.js](https://github.com/evanw/glfx.js/) | 1 | Separable pyramid | Desktop-focused | By Evan Wallace (esbuild/Figma). Most comprehensive. [ES6 fork](https://github.com/daviestar/glfx-es6) available |
| [WebGLImageFilter](https://github.com/phoboslab/WebGLImageFilter) | 1 | Convolution 3x3 | Basic | By phoboslab. Lightweight, chainable. [@longlost/webgl-filter](https://www.npmjs.com/package/@longlost/webgl-filter) ES module fork |
| [luma.gl @luma.gl/effects](https://luma.gl/) | 2 + WebGPU | Adapted from glfx.js | Yes (Uber/deck.gl) | Most mature WebGL2-native option. Active maintenance. Includes blur, sharpen, contrast |
| [glsl-fast-gaussian-blur](https://github.com/Experience-Monks/glsl-fast-gaussian-blur) | 1/2 | Linear sampling (5/9/13 tap) | Yes (PowerVR optimized) | Halves texture reads by sampling between texels. Vertex shader tex coords avoid dependent reads |
| [FivekoGFX](https://github.com/fiveko/fivekogfx) | 1 | Gaussian + separable | Unknown | Computer-vision oriented |

### Known Mali/Adreno Issues (Documented by Other Projects)

- **PixiJS** disabled WebGL2 on non-Apple mobile by default due to Chromium bug
- **TensorFlow.js** found `texelFetch` has *less* robust support than `texture2D` on Mali
- **Google Filament** documented Mali fragment shader mis-compilation
- **Unity** documented Mali-G72/G76/T830/T720 rendering black with linear color space
- **Three.js** documented Adreno 300 series black texture flickering
- **Chrome** notes `gl.finish()` is implemented as just `gl.flush()` — does NOT actually sync

### Recommended Approaches (Future Work)

1. **Dual Kawase blur** — downscale/upscale passes instead of separable Gaussian. 1.5-3x
   faster, scales logarithmically. Fewer texture reads per fragment. Best mobile perf.
   [Comprehensive writeup](https://blog.frost.kiwi/dual-kawase/)

2. **`fenceSync` + `clientWaitSync`** instead of `gl.finish()` for inter-pass sync —
   Chrome treats `gl.finish()` as just a flush, so it may not actually prevent pipeline
   hazards.

3. **Vertex shader tex coords** — compute texture coordinates in vertex shader and pass
   as varyings. Avoids dependent texture reads which stress mobile GPUs.
   ([glsl-fast-gaussian-blur](https://github.com/Experience-Monks/glsl-fast-gaussian-blur) does this)

4. **luma.gl shader source** as reference — battle-tested GLSL ES 3.0 implementations
   of blur, sharpen, contrast with WebGL2+WebGPU backends.

### References

- [WebGL2 Cross Platform Issues](https://webgl2fundamentals.org/webgl/lessons/webgl-cross-platform-issues.html)
- [WebGL2 Precision Issues](https://webgl2fundamentals.org/webgl/lessons/webgl-precision-issues.html)
- [MDN WebGL Best Practices](https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/WebGL_best_practices)
- [Intel — Fast Real-Time GPU Blur Algorithms](https://www.intel.com/content/www/us/en/developer/articles/technical/an-investigation-of-fast-real-time-gpu-based-image-blur-algorithms.html)
- [OpenGL ES Shading Language Potholes](https://bitiotic.com/blog/2013/09/24/opengl-es-shading-language-potholes-and-problems/)
- [Arm Mali GPU Best Practices (PDF)](https://armkeil.blob.core.windows.net/developer/Arm%20Developer%20Community/PDF/Arm%20Mali%20GPU%20Best%20Practices.pdf)
