# WebGPU Renderer Plan for aesthetic.computer

## Current State Analysis

### What's Already Built ✅

**1. `/system/public/aesthetic.computer/lib/webgpu.mjs`** (381 lines)
A foundational WebGPU renderer with:
- WebGPU initialization with adapter/device setup
- Canvas context configuration with alpha blending
- **Clear pipeline** - full-screen clear with configurable color
- **Line pipeline** - basic 1px line rendering in pixel coordinates
- Proper uniform buffers (color, resolution) with WGSL shaders
- Graceful fallback when WebGPU unavailable

**2. BIOS Integration (`bios.mjs`)**
- WebGPU module imported at line 41
- `initWebGPU(canvas)` async initializer (line 1611-1617)
- Message handler for `webgpu-command` (lines 10353-10355)

**3. Disk API (`disk.mjs`)**
- `$commonApi.webgpu` object with:
  - `enabled` flag to disable CPU renderer
  - `clear(r, g, b, a)` function
  - `line(x1, y1, x2, y2, r, g, b, a)` function
  - `render()` function
- Auto-routing in `ink()` and `wipe()` when `webgpu.enabled = true`

### Existing CPU Renderer Capabilities (graph.mjs)

The current `graph.mjs` has sophisticated features that WebGPU needs to match/accelerate:

**Pixel Manipulation:**
- `plot(x, y)` - single pixel
- `point(x, y)` - with pan translation
- `pixel(x, y)` - read pixel color
- `copy()` / `paste()` - buffer blitting with alpha, scaling, rotation
- `copyRow()` / `copyRegion()` - optimized sub-region operations

**Primitives:**
- `line()` - Bresenham with thickness support
- `lineh()` - horizontal line (fast path)
- `lineAngle()` - angle-based line
- `circle()` - filled/outline with thickness
- `rect()` / `box()` - rectangles
- `poly()` / `pline()` - polylines with thickness

**Sprite/Texture:**
- `paste(from, destX, destY, scale, blit)` - full sprite system
  - Supports ImageBitmap, canvas buffers, pixel arrays
  - Integer scaling (1-8x) with fast nearest-neighbor path
  - Rotation via transform object `{ scale, angle, anchor, crop }`
  - Alpha blending
- `bitmapPixelCache` - WeakMap cache for ImageBitmap pixel extraction

**Text System (type.mjs + graph.mjs):**
- `printLine()` - renders text using `draw()` function
- `draw()` - interprets glyph command lists:
  - BDF fonts (unifont, MatrixChunky8) → pixel arrays
  - Vector fonts → `line` and `point` commands
  - Supports rotation, scaling, thickness
- Typeface glyphs are just drawing commands (`{name: "line", args: [x1,y1,x2,y2]}`)
- **Key insight:** Text can be accelerated by accelerating the underlying primitives!

**Effects:**
- `spin()` - radial blur/rotation with SIMD-like batching
- `noise16()` variants - procedural noise
- Mask system (`activeMask`) for clipping

### What's Missing / Needs Work 🚧

**1. Core Primitives for WebGPU**
- [ ] Rectangle fill (`rect`) - enables text backgrounds
- [ ] Circle/ellipse (`circle`, `oval`)
- [ ] Filled polygon
- [ ] Thick lines (line width > 1px)
- [ ] Point rendering (single pixel)

**2. Sprite/Texture Support** (HIGH PRIORITY)
Since `paste()` is heavily used:
- [ ] Texture upload from ImageBitmap/Uint8ClampedArray
- [ ] Textured quad rendering
- [ ] Nearest-neighbor sampling for pixel art aesthetic
- [ ] Alpha blending modes

**3. Text Rendering Strategy**
Two approaches:
1. **Primitive-based:** Accelerate `point()` and `line()`, text rendering follows automatically
2. **Glyph atlas:** Pre-render glyphs to texture atlas, sample as textured quads

**4. Compute Shader Rasterization** (from tutorial)
The current implementation uses traditional render pipelines. For maximum flexibility and performance, a compute shader approach would enable:
- Atomic depth testing
- Custom blending modes (averaging, XOR, etc.)
- Order-independent transparency
- Performance wins for many small triangles

**5. Command Batching**
Current implementation executes commands immediately. Should batch for performance:
- Accumulate commands during frame
- Submit single command buffer at end of frame
- Reuse buffers between frames

---

## Tutorial Reference Summary

The [WebGPU Compute Rasterizer tutorial](https://github.com/OmarShehata/webgpu-compute-rasterizer) demonstrates:

### Architecture
```
[Compute Pass] → outputColorBuffer (storage buffer)
      ↓
[Fullscreen Pass] → reads buffer → draws to screen
```

### Key Techniques
1. **Storage Buffers** for pixel data (`WIDTH * HEIGHT * 3` for RGB)
2. **Barycentric Coordinates** for triangle fill
3. **Atomic Operations** (`atomicMin`) for depth testing without race conditions
4. **Workgroup Dispatch** - one compute thread per triangle

### Code Patterns Worth Adopting

**Triangle Fill (WGSL):**
```wgsl
fn barycentric(v1: vec3<f32>, v2: vec3<f32>, v3: vec3<f32>, p: vec2<f32>) -> vec3<f32> {
  let u = cross(
    vec3<f32>(v3.x - v1.x, v2.x - v1.x, v1.x - p.x), 
    vec3<f32>(v3.y - v1.y, v2.y - v1.y, v1.y - p.y)
  );
  if (abs(u.z) < 1.0) { return vec3<f32>(-1.0, 1.0, 1.0); }
  return vec3<f32>(1.0 - (u.x+u.y)/u.z, u.y/u.z, u.x/u.z); 
}

fn draw_triangle(v1: vec3<f32>, v2: vec3<f32>, v3: vec3<f32>) {
  let min_max = get_min_max(v1, v2, v3);
  for (var x = startX; x <= endX; x++) {
    for (var y = startY; y <= endY; y++) {
      let bc = barycentric(v1, v2, v3, vec2<f32>(f32(x), f32(y))); 
      if (bc.x >= 0.0 && bc.y >= 0.0 && bc.z >= 0.0) {
        color_pixel(x, y, color);
      }
    }
  }
}
```

**Atomic Depth Testing:**
```wgsl
struct ColorBuffer {
  values: array<atomic<u32>>,
};

fn color_pixel(x: u32, y: u32, r: u32, g: u32, b: u32) {
  let pixelID = u32(x + y * u32(uniforms.screenWidth)) * 3u;
  atomicMin(&outputColorBuffer.values[pixelID + 0u], r);
  // ... etc
}
```

---

## Implementation Phases

### Phase 1: Solidify Current Renderer ✨
**Goal:** Make `webgpu.mjs` useful for basic pieces

- [ ] Add `rect(x, y, w, h, color)` with fill
- [ ] Add `circle(x, y, radius, color)` with fill
- [ ] Add thick line support (line width uniform)
- [ ] Test with a demo piece that uses `webgpu.enabled = true`

### Phase 2: Compute Rasterizer Foundation 🔧
**Goal:** Set up compute-based rendering pipeline

- [ ] Create dual-buffer system (color storage + screen texture)
- [ ] Implement fullscreen quad pass to display storage buffer
- [ ] Port clear pass to compute shader
- [ ] Add basic triangle fill using barycentric coordinates

### Phase 3: Full 2D Primitive Set 📐
**Goal:** Replace all CPU drawing with GPU

- [ ] Filled rectangles via compute
- [ ] Filled circles via distance function
- [ ] Lines with thickness (quad-based)
- [ ] Polygons via triangle fan
- [ ] Depth buffer for proper layering

### Phase 4: Advanced Features 🚀
**Goal:** Enable effects not possible with CPU renderer

- [ ] Custom blend modes (multiply, screen, overlay)
- [ ] Per-pixel effects (blur, glow, distortion)
- [ ] Text rendering with SDF fonts
- [ ] Instanced rendering for particles
- [ ] Real-time filters on pixel buffer

---

## Questions to Answer

1. **Canvas Sizing:** How does `webgpu.mjs` handle resolution changes? Should it match the low-res aesthetic or render at native resolution?

2. **CPU/GPU Hybrid:** When `webgpu.enabled = true`, should ALL rendering go through GPU, or should we support hybrid mode?

3. **Compatibility:** WebGPU is still experimental. What's the fallback strategy?
   - WebGL2 via existing `2d.mjs`?
   - CPU rendering via canvas?

4. **Memory Budget:** How large can the storage buffer be for high-res displays?

---

## Test Piece Idea

Create `system/public/aesthetic.computer/disks/webgpu-test.mjs`:

```javascript
// webgpu-test.mjs - WebGPU Renderer Demo
export const desc = "WebGPU renderer test";

export function boot({ webgpu }) {
  webgpu.enabled = true;
}

export function paint({ webgpu, screen }) {
  webgpu.clear(20, 20, 40, 255);
  
  // Draw some lines
  for (let i = 0; i < 10; i++) {
    webgpu.line(
      10 + i * 10, 10,
      screen.width - 10, 10 + i * 20,
      255, 100 + i * 15, 50, 255
    );
  }
}

export function act({ event: e }) {
  // Handle input
}
```

---

## Related Files

- `/workspaces/aesthetic-computer/gpu/RESEARCH.md` - Original research doc
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/2d.mjs` - Disabled WebGL2 renderer
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/3d.mjs` - ThreeJS renderer (for reference)
- `/workspaces/aesthetic-computer/TODO.txt` line 627 - WebGPU rasterizer TODO
- `/workspaces/aesthetic-computer/TODO.txt` line 1491 - WebGL2/WebGPU Backend TODO

---

## Next Steps

1. **Create test piece** to validate current `clear` and `line` commands work
2. **Add `rect` primitive** as first compute-based fill
3. **Set up command batching** architecture
4. **Profile performance** vs CPU renderer

---

*Last updated: November 26, 2025*
