# WebGL Migration (Aesthetic Computer)

## Goal
Move the rendering pipeline from Canvas2D compositing to a WebGL-based composite path.

## Status
- [x] Add WebGL blitter module (full-screen quad + texture upload)
- [x] Wire WebGL composite canvas into BIOS render path
- [x] Move overlay compositing off the main canvas
- [x] Preserve recording outputs with WebGL composite
- [x] Preserve screenshot outputs with WebGL composite
- [ ] Verify underlay/tape playback behavior
- [ ] Validate WebGPU path still works (no conflicts)
- [x] Add runtime flag/toggle and telemetry
- [ ] Performance profiling in sandboxed iframes

## Subtasks

### 1) WebGL Blitter (Base Frame)
- [x] Create module to upload `ImageData` as a texture and draw full-screen quad
- [x] Add canvas sizing + visibility toggles in `bios.mjs`
- [x] Replace `ctx.putImageData` with WebGL blit when enabled

### 2) Overlay Compositing
- [x] Add dedicated overlay canvas layer
- [x] Route overlay painters to overlay canvas when WebGL composite enabled
- [x] Ensure overlay canvas clears each frame to avoid accumulation

### 3) Recording / Screenshot Capture
- [x] Composite WebGL canvas + overlay canvas into offscreen capture canvas
- [x] Preserve existing ordering (HUD before tape progress)
- [x] Ensure clean screenshots exclude overlays

### 4) Runtime Flags + Fallbacks
- [x] Add `content.webglCompositeEnabled` flag path
- [x] Add `globalThis.acUseWebGLComposite` override
- [x] Fallback to Canvas2D if WebGL init fails

### 5) Validation
- [ ] objkt sandbox: compare FPS vs Canvas2D path
- [ ] Regular browser: confirm no regressions
- [ ] Recording output: verify overlays + tape progress timing
- [ ] Glaze path: ensure compatibility with composite pipeline

## Notes
- WebGL composite should coexist with existing WebGPU path; if `content.webgpuEnabled` is true, WebGL composite should stay off.
- Keep UI canvas (cursor/spinner) above overlays to avoid GPU text rendering complexity.
