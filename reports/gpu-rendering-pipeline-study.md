# GPU Rendering Pipeline Study

**Date:** 2026-03-07

## Five Separate Rendering Paths Today

AC currently has **five disconnected rendering systems**. Understanding their
relationships is the first step toward unifying them.

```
                    PIECE CODE (paint/sim/act)
                           |
                    disk.mjs (API layer)
                           |
               +-----------+-----------+
               |                       |
          graph.mjs                twoDCommands[]
       (CPU software)              (command queue)
               |                       |
     +---------+---------+            2d.mjs
     |                   |         (DORMANT - commented out)
  pixels[]          gpu-effects.mjs    |
  (Uint8Clamped)    (OffscreenCanvas   pass-vert/frag.glsl
     |               WebGL2)           (simple line/point
     |                   |              GPU renderer)
     +--------+----------+
              |
        bios.mjs draw()
              |
     +--------+--------+--------+
     |        |        |        |
   canvas   webGL    webGPU   overlay
   (2D ctx  blitter  canvas   canvas
    putImage)
```

---

## Path 1: graph.mjs (CPU Software Renderer) -- ACTIVE, PRIMARY

**Role:** All rendering. Every pixel of every piece goes through here.

**Flow:**
```
piece.paint() -> ink(r,g,b) -> graph.color(r,g,b,a)  [sets c = [r,g,b,a]]
              -> write(text) -> printLine() -> draw() -> plot(x,y)
              -> line(x1,y1,x2,y2) -> plot(x,y) per pixel
              -> box(x,y,w,h) -> plot(x,y) per pixel
```

**Core data:** `pixels: Uint8ClampedArray` (RGBA, 4 bytes/pixel)
- All operations write directly into this buffer
- `plot(x,y)` at graph.mjs:1618 is the atomic pixel writer
- `clear()` uses binary-doubling `copyWithin` for O(log n) wipe

**Text rendering path (the bottleneck for chat):**
```
write(text, pos)                    disk.mjs:5232
  -> textContainsColorCodes(text)   check for \r,g,b\ syntax
  -> if has codes: regex split + per-segment color parsing
  -> tf.print(cleanText, x, y, charColors)   type.mjs
    -> printLine(text, x, y, charColors)      graph.mjs:4915
      -> for each char:
           getGlyphForChar(char)              glyph lookup
           draw(glyph, x, y)                  graph.mjs:4652
             -> for each pixel in glyph bitmap (8x10 for BDF):
                  rotation math (2 mul + 2 sub per pixel)
                  scale math
                  plot(finalX, finalY)
                    -> bounds check (4 comparisons)
                    -> mask check (4 comparisons if active)
                    -> pixels.set(color, index)  or  blend()
```

**Per-glyph cost (BDF fonts like MatrixChunky8 / 6x10):**
- 80 pixels per glyph (8x10 bitmap)
- Each pixel: bounds check + mask check + array write = ~15 ops
- Total: ~1,200 ops per character

**Per write() call with 50 chars:**
- Color code parsing: ~100 ops
- Glyph iteration: 50 x 1,200 = ~60,000 ops
- Overhead: ~500 ops
- **Total: ~60,600 ops per write() call**

**Chat's 120 write() calls/frame: ~7.3M ops** (mostly in glyph pixel iteration)

---

## Path 2: gpu-effects.mjs (WebGL2 Post-Processing) -- ACTIVE

**Role:** Pixel buffer filters. Takes `pixels[]` in, processes on GPU, writes
back to same `pixels[]`. Never sees individual draw calls.

**Architecture:**
- Own `OffscreenCanvas` + WebGL2 context (worker-safe)
- 12 shader programs (inline GLSL), all sharing one vertex shader
- Full-screen quad rendering (2 triangles)
- Upload via `texSubImage2D`, readback via `readPixels` (synchronous)

**Programs:**
| Program | Purpose | Passes |
|---------|---------|--------|
| spin | Rotation around anchor | 1 |
| composite | Zoom + scroll + contrast + brightness | 1 |
| invert | RGB inversion | 1 (BROKEN - missing helpers) |
| blurH + blurV | Separable Gaussian blur | 2 (ping-pong) |
| sharpen | Unsharp mask | 1 |
| shear | KidPix row/col shifting | 1 |
| suck | Radial displacement | 1 |
| floodSeed + floodJFA + floodFill | Jump Flooding Algorithm flood fill | 3+ |
| layerComposite | 8-layer alpha blend | 1 |

**Integration with graph.mjs:**
```js
// Pattern used everywhere (e.g. graph.mjs:5670)
if (gpuSpinEnabled && gpuSpinAvailable && gpuSpinModule?.gpuSpin) {
  const success = gpuSpinModule.gpuSpin(pixels, width, height, ...);
  if (!success) { /* CPU fallback */ }
}
```

**Key limitation:** Round-trip. Every GPU effect does:
1. Upload entire `pixels[]` to texture (`texSubImage2D`)
2. Render full-screen quad with shader
3. Read back entire result (`readPixels` -- CPU-GPU sync stall)
4. No PBO (Pixel Buffer Object) for async readback

This is fine for per-frame effects (one stall per frame), but would be
expensive if called per-glyph or per-line.

---

## Path 3: 2d.mjs (WebGL2 Geometry Renderer) -- DORMANT

**Role:** Direct GPU rendering of 2D primitives (lines, points, text).
Intended to replace the CPU per-pixel path for basic geometry.

**Status:** Import commented out in bios.mjs:47-48:
```js
// import * as TwoD from "./lib/2d.mjs";
const TwoD = undefined;
```

All call sites use optional chaining (`TwoD?.render()`) -- currently no-ops.

**What it has:**
- Own WebGL2 canvas (overlaid on main canvas)
- Interleaved vertex buffer: 6 floats per vertex (xy + rgba)
- Simple pass-through shaders (screen coords -> clip space)
- Lines and points only
- Text support: **stub, incomplete** (line 126-138, just splits chars)

**Command queue bridge:**
```
graph.mjs line() -> twoDCommands.push(["line", ...])
                  -> disk.mjs sends to bios.mjs
                  -> TwoD?.pack({code: twoDCommands})
                  -> TwoD?.render()
```

The `twoDCommands` queue still gets populated by `graph.line()` (line 2999)
even though 2d.mjs is disabled.

---

## Display Compositing (bios.mjs)

**Multiple canvas outputs** exist but only one is active at a time:

| Canvas | Type | Usage |
|--------|------|-------|
| `canvas` (main) | 2D context | `ctx.putImageData(imageData, 0, offset)` |
| `webglCompositeCanvas` | WebGL | `webglBlitter.render(imageData)` |
| `webgpuCanvas` | WebGPU | WebGPU command buffer |
| `overlayCan` | 2D context | UI elements painted on top |

**Zero-copy optimization:**
```js
imageData = window.pixelOptimizer.createImageDataZeroCopy(
  content.pixels, content.width, content.height
);
```
Wraps the worker's `Uint8ClampedArray` directly as `ImageData` without copying.

**DirtyBox optimization:** If piece only changed a region, only that rect
gets redrawn via `ctx.drawImage(dirtyBoxBitmapCan, db.x, db.y)`.

---

## Where GPU Text Fits: Unification Strategy

### The Core Problem

All three paths operate independently:
1. **graph.mjs** renders text pixel-by-pixel into `pixels[]`
2. **gpu-effects.mjs** processes `pixels[]` as a flat texture
3. **2d.mjs** would render geometry to its own canvas

For GPU text, we need to **render glyphs on the GPU** and get the result
into `pixels[]` so the rest of the pipeline (effects, display) works unchanged.

### Option A: Glyph Atlas in gpu-effects.mjs (Recommended)

Add a `gpuText()` function to gpu-effects.mjs that:

1. **Init:** Build glyph atlas texture from BDF font data (one-time)
   - MatrixChunky8: 256 chars x 8x8 = 16KB atlas (fits in 128x128 texture)
   - 6x10 font: 256 chars x 6x10 = 15KB atlas (fits in 128x128 texture)
   - System font (8x8): same deal

2. **Per-frame:** Accept a batch of glyph instances
   ```js
   gpuText(pixels, width, height, glyphs, atlasId)
   // glyphs = [{char, x, y, r, g, b, a}, ...]
   ```

3. **Render:** Instanced textured quads in one draw call
   - Vertex shader: position + UV offset into atlas
   - Fragment shader: sample atlas, multiply by vertex color, alpha blend
   - Output to framebuffer

4. **Readback:** Same `readPixels` pattern as existing effects

**Why this works:**
- Same WebGL2 context, same OffscreenCanvas
- Same upload/readback pattern (already proven)
- One GPU draw call replaces ~6,000 `plot()` calls for chat
- Shadows = second draw call with darkened colors (nearly free)
- Falls back to CPU `write()` if GPU unavailable (existing pattern)

**Integration point in graph.mjs:**
```js
// In printLine() or a new gpuPrintLine():
if (gpuTextAvailable && batchedGlyphs.length > 0) {
  gpuSpinModule.gpuText(pixels, width, height, batchedGlyphs, fontAtlasId);
} else {
  // existing per-pixel draw() path
}
```

**Estimated speedup for chat:**
- Current: 120 write() x ~60,600 ops = ~7.3M CPU ops/frame
- GPU text: 1 texture upload + 1 instanced draw + 1 readback
- Net: ~95% reduction in CPU text cost
- Readback stall: ~0.5ms for 320x200 buffer (acceptable)

### Option B: Revive 2d.mjs as Overlay

Render text to 2d.mjs's own WebGL canvas (overlaid on main canvas).
No readback needed -- the browser compositor blends the canvases.

**Pros:**
- No readback stall (huge win)
- True GPU rendering end-to-end
- Could use SDF fonts for smooth scaling

**Cons:**
- GPU-rendered text wouldn't be in `pixels[]` -- effects like blur/contrast
  wouldn't apply to text
- Pieces that read back pixels (screenshots, flood fill) wouldn't see text
- Two-canvas compositing adds complexity
- Breaks the immediate-mode mental model

### Option C: Unified GPU Pipeline (Future)

Move everything to GPU. `pixels[]` becomes a GPU texture.
Primitives (box, line, circle) render as GPU geometry.
Text renders as instanced quads. Effects are shader passes.
Only readback for screenshots/flood fill.

This is the WebGPU path that's partially stubbed (`webgpuCanvas`).
Too large for now, but Option A is a stepping stone toward it.

---

## Recommendation

**Start with Option A** -- `gpuText()` in gpu-effects.mjs:

1. Build glyph atlas from BDF font bitmaps at init
2. Batch text rendering into single instanced draw call
3. Same readback pattern as existing effects
4. Fallback to CPU for unsupported environments
5. Chat opts in via a flag; other pieces unaffected

The readback cost (~0.5ms) is acceptable because chat already takes
~7ms+ in CPU text rendering. Trading 7ms of CPU work for 0.5ms of
GPU stall + ~0.1ms of GPU render = huge net win.

**Future path:** Once gpuText works, it naturally extends to all pieces.
Then we can explore eliminating readback by rendering text to a separate
canvas layer (Option B hybrid), and eventually the full GPU pipeline
(Option C / WebGPU).

---

---

## Path 4: lib/gpu/ Backend System -- ACTIVE (for test pieces only)

**Role:** Pluggable GPU renderer abstraction. Pieces opt in via
`api.webgpu.enabled = true`, commands route through disk worker to bios
main thread, which dispatches to the active backend.

**Architecture:**
```
piece.paint()
  -> api.webgpu.clear/line/box/circle()
  -> send({type: "webgpu-command", content: {...}})
  -> bios.mjs receives message
  -> activeGPUBackend.handleCommand(content)
  -> backend renders to its own canvas (overlaid on main)
```

**Registry system** (`lib/gpu/backends.mjs`):
- `registerBackend(name, factory)` — register a backend
- `createRenderer(name, canvas)` — instantiate by name
- `switchBackend(name)` — hot-swap at runtime
- `initGPU(wrapper, preferred)` — init with fallback chain

**5 backends registered in `lib/gpu/index.mjs`:**

| Backend | File | Status | Browser Support |
|---------|------|--------|-----------------|
| Canvas2D | `canvas2d-backend.mjs` | Complete | 100% |
| WebGL2 | `webgl2-backend.mjs` | Complete | 98%+ |
| WebGPU | `webgpu.mjs` (legacy standalone) | Complete | Chrome stable |
| Vello | `vello-backend.mjs` | Canvas2D fallback working, WASM present | Chrome (WebGPU) |
| ThorVG | `thorvg-backend.mjs` | Code complete, WASM not compiled | Depends on WASM |
| Blend2D | `blend2d-backend.mjs` | Code complete, WASM not compiled | Depends on WASM |

**What they render:** Lines only (+ clear/wipe). No box, circle, text yet.

**WASM assets (`lib/gpu/wasm/`):**
- `vello_wasm.js` + `vello_wasm_bg.wasm` — present, compiled from
  Rust source at `gpu/vello-wasm/` (uses vello 0.3, wgpu 22)
- ThorVG and Blend2D WASM: backend code ready but binaries not built

**Test pieces** (all in `disks/`):

| Piece | Backend | Notes |
|-------|---------|-------|
| `blank.mjs` | WebGPU (legacy) | Original test, rainbow lines |
| `blank-webgl2.mjs` | WebGL2 | Interactive line count, perf stats |
| `blank-canvas2d.mjs` | Canvas2D | Baseline comparison |
| `blank-vello.mjs` | Vello | Purple theme, uses Canvas2D fallback |
| `blank-thorvg.mjs` | ThorVG | Needs WASM binary |
| `blank-blend2d.mjs` | Blend2D | Needs WASM binary |

All test pieces share: adjustable line count (arrows), animation toggle
(space), perf overlay (P), 4 animation patterns.

**Key gap:** This system renders to its **own canvas overlaid on main**.
It does NOT write to `pixels[]`. So:
- GPU-rendered content is invisible to `pixels[]`-based operations
  (flood fill, screenshots, gpu-effects filters)
- The piece must choose: CPU path (full API) or GPU path (lines only)
- No mixing of CPU and GPU primitives in the same frame

---

## Path 5: Vello Compute Pipeline -- COMPILED, NOT YET WIRED

**Role:** GPU compute-based 2D vector renderer. Vello uses WebGPU compute
shaders to rasterize 2D paths on the GPU — no triangulation, no
CPU rasterization.

**Rust source:** `gpu/vello-wasm/src/lib.rs` (188 lines)
- Exports `VelloScene` struct via wasm-bindgen
- Methods: `new`, `clear`, `line`, `rect`, `rect_stroke`, `circle`,
  `circle_stroke`, `rounded_rect`
- Dependencies: vello 0.3, wgpu 22, wasm-bindgen, web-sys

**Current state in JS:** `vello-backend.mjs` creates a `VelloScene` and
mirrors all draw commands to it, but actually renders via Canvas2D.
The compute shader path is not connected yet — would need:
1. WebGPU device/adapter initialization in JS
2. Vello renderer instantiation with the device
3. Scene submission to Vello's compute pipeline
4. Render target output to canvas or readback to pixels

**Performance promise:** Native Vello benchmarks show 177fps for 30k paths
on M1 Max. In-browser via WebGPU compute would be slower but still
dramatically faster than CPU rasterization.

---

## How All Five Paths Relate

```
                         PIECE CODE
                            |
                     disk.mjs API layer
                            |
            +-------+-------+-------+
            |       |               |
       graph.mjs  twoDCommands  webgpu-command
       (CPU pixel  (dormant)    (message to bios)
        buffer)                      |
            |                   bios.mjs
            |                        |
     +------+------+          +------+------+
     |             |          |             |
  pixels[]   gpu-effects    lib/gpu/     webgpuCanvas
  (Uint8     (WebGL2        backend       (overlaid)
   Clamped)  OffscreenCanvas system)
     |       filters)            |
     |             |        Canvas2D / WebGL2 /
     +------+------+       WebGPU / Vello / etc.
            |
      bios.mjs draw()
            |
    putImageData / webglBlitter
```

**The fundamental split:**
- **Left side** (graph.mjs + gpu-effects): Everything goes through `pixels[]`.
  Full API (text, box, circle, line, flood, etc.). GPU only for post-processing.
- **Right side** (lib/gpu/ backends): Direct GPU rendering to overlay canvas.
  Limited API (lines only). No `pixels[]` integration.

**They don't share:**
- WebGL contexts (gpu-effects has its own OffscreenCanvas)
- Textures or framebuffers
- Color state or transform state
- The pixel buffer

---

## Unification Opportunities

### Near-term: GPU text via gpu-effects.mjs (for chat)

Add `gpuText()` to gpu-effects.mjs with a glyph atlas. This stays in the
`pixels[]` world — compatible with everything. See earlier section.

### Medium-term: Merge gpu-effects into lib/gpu/ backend system

The backend system (`lib/gpu/`) already has a clean abstraction. gpu-effects.mjs
predates it and duplicates WebGL2 setup. Could be refactored as effects that
any backend can apply:

```
backend.applyEffect("blur", {strength: 5})
backend.applyEffect("contrast", {level: 1.5})
```

### Medium-term: Add text primitive to lib/gpu/ backends

Extend the backend interface with `text(str, x, y, font, color)`. Each
backend implements it:
- Canvas2D: `ctx.fillText()` (fast, native)
- WebGL2: glyph atlas + instanced quads
- Vello: `VelloScene` text paths (when compute pipeline connected)

### Long-term: Eliminate pixels[] round-trip

The holy grail: render everything on GPU, only readback when needed
(screenshot, flood fill). The lib/gpu/ backend system is the right
foundation for this — it just needs more primitives (text, box, circle,
sprites) and a way to apply effects without readback.

### Vello as the endgame

Vello's compute pipeline is the most promising path to full GPU rendering:
- Handles arbitrary 2D paths (not just lines)
- Anti-aliased by default
- Text via font outlines (no bitmap atlas needed)
- WebGPU compute = massively parallel
- Already compiled to WASM and present in the repo

The gap: connecting VelloScene's compute output to a visible surface and
integrating with AC's coordinate system, color model, and mask system.

---

## Key File Locations

| File | Role |
|------|------|
| `lib/graph.mjs` | CPU software renderer, plot/draw/clear |
| `lib/gpu-effects.mjs` | WebGL2 pixel buffer filters (OffscreenCanvas) |
| `lib/2d.mjs` | Dormant WebGL2 geometry renderer |
| `lib/gpu/index.mjs` | Backend registry, auto-registers all backends |
| `lib/gpu/backends.mjs` | Backend factory, `createRenderer`/`switchBackend` |
| `lib/gpu/renderer-interface.mjs` | Base class, command dispatch, perf stats |
| `lib/gpu/canvas2d-backend.mjs` | Canvas2D backend (fallback) |
| `lib/gpu/webgl2-backend.mjs` | WebGL2 backend (lines) |
| `lib/gpu/vello-backend.mjs` | Vello backend (Canvas2D fallback + WASM) |
| `lib/gpu/thorvg-backend.mjs` | ThorVG backend (awaits WASM) |
| `lib/gpu/blend2d-backend.mjs` | Blend2D backend (awaits WASM) |
| `lib/gpu/wasm/` | WASM binaries (vello present) |
| `lib/gpu/webgpu.mjs` | Legacy standalone WebGPU renderer |
| `gpu/vello-wasm/` | Rust source for Vello WASM build |
| `lib/type.mjs` | Font loading, glyph access, advance cache |
| `lib/disk.mjs` | API surface, write(), ink(), text.box() |
| `bios.mjs` | Main thread, canvas display, draw loop |
| `plans/RENDERER-BACKENDS.md` | Master backend plan (559 lines) |
| `plans/webgpu-renderer-plan.md` | WebGPU roadmap |
| `disks/blank*.mjs` | Test pieces for each backend |
