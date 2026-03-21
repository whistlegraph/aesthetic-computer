# GPU Renderer Backends Research & Plan

## 🎯 Goal

Create a swappable GPU renderer backend system that allows iterating on different 2D rendering technologies (WebGPU, WebGL2, ThorVG, Blend2D, etc.) with **minimal changes** to:
- `disks/blank.mjs` (piece code)
- `lib/disk.mjs` (worker/API layer)

The renderer runs on the **main thread** (bios), receiving commands from the disk worker.

---

## 📐 Current Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│ Piece (blank.mjs)                                               │
│ ┌─────────────────────────────────────────────────────────────┐ │
│ │ boot({ api }) {                                             │ │
│ │   api.webgpu.enabled = true;  // ← Only piece-level change  │ │
│ │ }                                                           │ │
│ │ paint({ wipe, ink, line }) {                               │ │
│ │   wipe(r,g,b);  // → sends webgpu-command if enabled       │ │
│ │   ink(r,g,b);   // → sets current color                    │ │
│ │   line(x1,y1,x2,y2); // → sends webgpu-command if enabled  │ │
│ │ }                                                           │ │
│ └─────────────────────────────────────────────────────────────┘ │
└───────────────────────────────┬─────────────────────────────────┘
                                │ postMessage({ type: "webgpu-command", ... })
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│ Disk Worker (disk.mjs)                                          │
│ ┌─────────────────────────────────────────────────────────────┐ │
│ │ $commonApi.webgpu = {                                       │ │
│ │   enabled: false,                                           │ │
│ │   clear, line, render, perf                                 │ │
│ │ }                                                           │ │
│ │                                                             │ │
│ │ // Intercepts wipe() / line() when webgpu.enabled          │ │
│ │ wipe → send({ type: "webgpu-command", content: {...} })    │ │
│ │ line → send({ type: "webgpu-command", content: {...} })    │ │
│ └─────────────────────────────────────────────────────────────┘ │
└───────────────────────────────┬─────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│ BIOS Main Thread (bios.mjs)                                     │
│ ┌─────────────────────────────────────────────────────────────┐ │
│ │ import * as WebGPU from "./lib/webgpu.mjs";                 │ │
│ │                                                             │ │
│ │ if (type === "webgpu-command") {                            │ │
│ │   WebGPU.handleCommand(content);  // ← Direct dispatch      │ │
│ │ }                                                           │ │
│ └─────────────────────────────────────────────────────────────┘ │
│                                                                 │
│ ┌─────────────────────────────────────────────────────────────┐ │
│ │ WebGPU Canvas (hidden by default, shown when enabled)       │ │
│ │ webgpuCanvas.style.zIndex = "1" (above main canvas)         │ │
│ └─────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

### Current Files
- **[lib/webgpu.mjs](../system/public/aesthetic.computer/lib/webgpu.mjs)** - WebGPU renderer (589 lines)
- **[lib/disk.mjs#L3317-3365](../system/public/aesthetic.computer/lib/disk.mjs#L3317-L3365)** - `webgpu` API in `$commonApi`
- **[bios.mjs#L11928](../system/public/aesthetic.computer/bios.mjs#L11928)** - `webgpu-command` handler
- **[disks/blank.mjs](../system/public/aesthetic.computer/disks/blank.mjs)** - Test piece using WebGPU

### Current Command Types
| Command | Payload | Notes |
|---------|---------|-------|
| `clear` | `{ color: [r,g,b,a] }` | Clears canvas with color |
| `line` | `{ x1,y1,x2,y2, color }` | Draws line segment |
| `render` | `{}` | (unused, frame-end does this) |
| `frame-end` | `{}` | Flushes command queue, presents |
| `perf-overlay` | `{ enabled }` | Toggle FPS overlay |

---

## 🔧 Proposed Backend Abstraction

### 1. Renderer Interface

Create a standard interface that all backends must implement:

```javascript
// lib/gpu/renderer-interface.mjs
export const RendererInterface = {
  // Lifecycle
  async init(canvas) { /* return true/false */ },
  destroy() {},
  
  // Commands
  handleCommand(command) {},  // Dispatch based on command.type
  
  // Individual operations
  clear(r, g, b, a) {},
  line(x1, y1, x2, y2, color) {},
  box(x, y, w, h, color) {},       // Future
  circle(cx, cy, r, color) {},     // Future
  path(points, color, closed) {},  // Future
  image(imgData, x, y, w, h) {},   // Future
  
  // Frame control
  beginFrame() {},   // Optional setup
  endFrame() {},     // Flush/present
  
  // State
  resize(width, height) {},
  
  // Info
  getName() {},      // "WebGPU", "WebGL2", etc.
  getStats() {},     // Performance data
};
```

### 2. Backend Registry

```javascript
// lib/gpu/backends.mjs
const backends = new Map();

export function registerBackend(name, factory) {
  backends.set(name, factory);
}

export async function createRenderer(name, canvas) {
  const factory = backends.get(name);
  if (!factory) throw new Error(`Unknown backend: ${name}`);
  const renderer = factory();
  const success = await renderer.init(canvas);
  return success ? renderer : null;
}

export function listBackends() {
  return [...backends.keys()];
}
```

### 3. Unified Dispatcher (bios.mjs changes)

```javascript
// bios.mjs - MINIMAL CHANGE
import { createRenderer, listBackends } from "./lib/gpu/backends.mjs";

let activeRenderer = null;

// In boot sequence:
const preferredBackend = localStorage.getItem("ac-gpu-backend") || "webgpu";
activeRenderer = await createRenderer(preferredBackend, gpuCanvas);
if (!activeRenderer) {
  // Fallback chain
  for (const backend of ["webgl2", "canvas2d"]) {
    activeRenderer = await createRenderer(backend, gpuCanvas);
    if (activeRenderer) break;
  }
}

// In message handler (UNCHANGED from current):
if (type === "webgpu-command") {  // Keep name for compatibility
  activeRenderer?.handleCommand(content);
  return;
}
```

---

## 🎨 Backend Candidates

### 1. WebGPU (Current)
**Status:** ✅ Implemented  
**File:** `lib/webgpu.mjs`

**Pros:**
- Modern, high performance
- Good for batching many draw calls
- Native shader language (WGSL)

**Cons:**
- Limited browser support (Chrome, Edge)
- Complex initialization
- Overkill for simple 2D

### 2. WebGL2

**Status:** 🔄 Research needed  
**Effort:** ~2-3 days

**Pros:**
- Wide browser support (98%+)
- Still GPU accelerated
- Mature ecosystem
- Can use GLSL shaders

**Cons:**
- More boilerplate than Canvas2D
- State machine model

**Implementation approach:**
```javascript
// lib/gpu/webgl2-backend.mjs
export function createWebGL2Backend() {
  let gl, lineProgram, quadProgram;
  
  return {
    async init(canvas) {
      gl = canvas.getContext("webgl2");
      if (!gl) return false;
      // Create shaders, programs, buffers...
      return true;
    },
    
    clear(r, g, b, a) {
      gl.clearColor(r/255, g/255, b/255, a/255);
      gl.clear(gl.COLOR_BUFFER_BIT);
    },
    
    line(x1, y1, x2, y2, color) {
      // Use line program, upload vertices, draw
    },
    
    endFrame() {
      gl.flush();
    }
  };
}
```

### 3. ThorVG (via WebAssembly)

**Status:** 🔬 Experimental  
**Effort:** ~1 week

**What is it:** High-performance vector graphics library (used by Lottie, Samsung Tizen)  
**Website:** https://thorvg.org

**Pros:**
- Very fast SVG/vector rendering
- Small WASM binary (~150KB)
- Anti-aliased by default
- Path operations, gradients, effects

**Cons:**
- Need to compile to WASM
- Additional complexity
- May need canvas2d as output target

**Implementation approach:**
```javascript
// lib/gpu/thorvg-backend.mjs
import ThorVG from "./thorvg.wasm.js";

export function createThorVGBackend() {
  let tvg, canvas, ctx;
  
  return {
    async init(targetCanvas) {
      canvas = targetCanvas;
      ctx = canvas.getContext("2d");
      tvg = await ThorVG.init();
      return true;
    },
    
    clear(r, g, b, a) {
      tvg.clear([r, g, b, a]);
    },
    
    line(x1, y1, x2, y2, color) {
      tvg.line(x1, y1, x2, y2, color);
    },
    
    endFrame() {
      // Render TVG canvas to our canvas
      tvg.render();
      ctx.drawImage(tvg.getCanvas(), 0, 0);
    }
  };
}
```

### 4. Blend2D (via WebAssembly)

**Status:** 🔬 Experimental  
**Effort:** ~1 week

**What is it:** High-performance 2D vector graphics engine (C++)  
**Website:** https://blend2d.com

**Pros:**
- JIT-compiled rendering pipelines
- Very fast anti-aliased primitives
- Rich feature set (gradients, patterns, text)
- Good for complex vector art

**Cons:**
- Larger WASM (~500KB-1MB)
- Need to compile to WASM
- C++ codebase

### 5. CanvasKit (Skia via WASM)

**Status:** 🔬 Experimental  
**Effort:** ~3 days (pre-built)

**What is it:** Google Skia graphics library compiled to WASM  
**Website:** https://skia.org/docs/user/modules/canvaskit/

**Pros:**
- Same renderer as Chrome, Flutter
- Very feature complete
- Pre-built WASM available
- Excellent text rendering

**Cons:**
- Large (~3-6MB WASM)
- Complex API
- Heavy for simple use cases

### 5. Vello (via Rust/WASM + WebGPU)

**Status:** 🔬 Experimental (backend created)  
**Effort:** ~1 week (need to build Rust WASM bindings)

**What is it:** GPU compute-centric 2D renderer by Linebender (Xilem team)  
**Website:** https://vello.dev

**Pros:**
- **Very fast** - 177fps for 30k paths on M1 Max
- Uses WebGPU compute shaders for parallel rendering
- Modern architecture, production-quality
- Same team as Xilem, Druid
- Anti-aliased, PostScript-style API

**Cons:**
- Requires WebGPU (no WebGL fallback)
- Must build your own WASM bindings from Rust
- Non-Chrome browsers have limited WebGPU support
- More complex integration than other options

**Implementation:**
```javascript
// lib/gpu/vello-backend.mjs (created)
// Requires: lib/gpu/wasm/vello_bindings.js + vello_bindings_bg.wasm
```

### 6. Canvas2D (Fallback)

**Status:** 🎯 Easy fallback  
**Effort:** ~1 day

**Pros:**
- Universal support
- Simple API
- No initialization needed

**Cons:**
- CPU rendered (mostly)
- Slower for many primitives
- No shader support

```javascript
// lib/gpu/canvas2d-backend.mjs
export function createCanvas2DBackend() {
  let ctx;
  
  return {
    async init(canvas) {
      ctx = canvas.getContext("2d");
      return !!ctx;
    },
    
    clear(r, g, b, a) {
      ctx.fillStyle = `rgba(${r},${g},${b},${a/255})`;
      ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
    },
    
    line(x1, y1, x2, y2, [r, g, b, a]) {
      ctx.strokeStyle = `rgba(${r},${g},${b},${a/255})`;
      ctx.beginPath();
      ctx.moveTo(x1, y1);
      ctx.lineTo(x2, y2);
      ctx.stroke();
    },
    
    endFrame() {
      // Canvas2D auto-presents
    }
  };
}
```

### 7. Pixi.js / Two.js (Libraries)

**Status:** 🤔 Consider  
**Effort:** ~2 days

Could wrap existing 2D libraries that already handle WebGL/Canvas fallback.

---

## 📋 Implementation Status

### ✅ Phase 1: Abstraction Layer (COMPLETE)
- [x] `lib/gpu/renderer-interface.mjs` - Base renderer with shared logic
- [x] `lib/gpu/backends.mjs` - Registry with `registerBackend`, `createRenderer`, `initGPU`
- [x] `lib/gpu/index.mjs` - Auto-registers all backends
- [x] Existing `webgpu.mjs` works via message passing (unchanged)

### ✅ Phase 2: WebGL2 Backend (COMPLETE)
- [x] `lib/gpu/webgl2-backend.mjs` - Full implementation
- [x] GLSL shaders for lines
- [x] Vertex buffer management
- [x] Performance tracking

### ✅ Phase 3: Canvas2D Fallback (COMPLETE)
- [x] `lib/gpu/canvas2d-backend.mjs` - Universal fallback
- [x] Performance overlay with FPS/MS/DC
- [x] Full command queue batching

### ✅ Phase 4: WASM Backends (STUBS CREATED)
- [x] `lib/gpu/thorvg-backend.mjs` - Ready for WASM
- [x] `lib/gpu/blend2d-backend.mjs` - Ready for WASM
- [x] `lib/gpu/vello-backend.mjs` - Ready for WASM (WebGPU compute)
- [x] `lib/gpu/wasm/README.md` - Build instructions

### Test Pieces Created
| Piece | Backend | Color Theme |
|-------|---------|-------------|
| `blank.mjs` | WebGPU | Rainbow |
| `blank-webgl2.mjs` | WebGL2 | Cyan-Blue |
| `blank-canvas2d.mjs` | Canvas2D | Green-Yellow |
| `blank-thorvg.mjs` | ThorVG | Orange-Red |
| `blank-blend2d.mjs` | Blend2D | Magenta-Pink |
| `blank-vello.mjs` | Vello | Purple-Violet |

### 🚧 Remaining Work
1. [ ] Wire up `api.gpu.backend` in `disk.mjs` to send backend preference
2. [ ] Update `bios.mjs` to use backend registry for initialization
3. [ ] Download/build ThorVG WASM
4. [ ] Download/build Blend2D WASM
5. [ ] Benchmark all backends

---

## 🔬 Benchmarking Plan

Create a test piece that measures:

```javascript
// disks/gpu-benchmark.mjs
function paint({ line, screen }) {
  const start = performance.now();
  
  for (let i = 0; i < 10000; i++) {
    line(
      Math.random() * screen.width,
      Math.random() * screen.height,
      Math.random() * screen.width,
      Math.random() * screen.height
    );
  }
  
  const elapsed = performance.now() - start;
  console.log(`10k lines: ${elapsed.toFixed(2)}ms`);
}
```

Metrics to track:
- Frame time (ms)
- Memory usage
- GPU utilization (if measurable)
- Initialization time

---

## 🎯 Disk/Piece Changes Required

### Current (blank.mjs)
```javascript
function boot({ api }) {
  api.webgpu.enabled = true;  // This is the ONLY piece-level change
}
```

### Proposed (UNCHANGED or better)
```javascript
function boot({ api }) {
  // Option A: Keep current API (no change)
  api.webgpu.enabled = true;
  
  // Option B: More explicit backend selection (optional)
  api.gpu.backend = "webgl2";  // or "webgpu", "canvas2d", "auto"
  api.gpu.enabled = true;
}
```

### Disk.mjs Changes

The `webgpu` object in `$commonApi` would become:
```javascript
// Option A: Keep as-is, "webgpu" name is legacy
webgpu: {
  enabled: false,
  clear: (r, g, b, a) => send({ type: "webgpu-command", ... }),
  line: (x1, y1, ...) => send({ type: "webgpu-command", ... }),
}

// Option B: Rename to "gpu" (breaking change)
gpu: {
  enabled: false,
  backend: "auto",  // "webgpu" | "webgl2" | "canvas2d" | "auto"
  clear: ...,
  line: ...,
}
```

**Recommendation:** Option A (keep `webgpu` name internally, swap backend on main thread)

---

## 📝 Questions to Resolve

1. **Should pieces be able to request specific backends?**
   - Pros: Some pieces may need WebGPU features
   - Cons: Complicates API, may not work on all browsers

2. **How to handle backend-specific features?**
   - WebGPU: Custom shaders
   - ThorVG: SVG loading
   - CanvasKit: Advanced text

3. **Should we support hybrid rendering?**
   - E.g., CPU canvas for painting, GPU for effects

4. **WASM loading strategy?**
   - Lazy load only when needed
   - Bundle with main app
   - Separate optional modules

---

## 📚 Resources

- [WebGPU Spec](https://www.w3.org/TR/webgpu/)
- [WebGL2 Reference](https://webgl2fundamentals.org/)
- [ThorVG GitHub](https://github.com/thorvg/thorvg)
- [Blend2D Docs](https://blend2d.com/doc/index.html)
- [CanvasKit](https://skia.org/docs/user/modules/canvaskit/)
- [gpu.rocks Benchmarks](https://gpu.rocks/) - Compare GPU rendering

---

## ✅ Next Steps

1. **Immediate:** Create backend interface and registry
2. **This week:** Implement WebGL2 backend
3. **Testing:** Benchmark WebGPU vs WebGL2 vs Canvas2D
4. **Research:** Evaluate ThorVG/Blend2D WASM builds
