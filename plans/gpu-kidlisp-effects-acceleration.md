# GPU Acceleration Plan for KidLisp Effects

## Overview

Analysis of `$cow` and the current CPU/GPU hybrid architecture to accelerate `flood`, `contrast`, and embedded layer compositing for better performance on complex KidLisp pieces.

## Current `$cow` Source

```lisp
($39i 0 0 w h 128)
($r2f 0 0 w h 128)
(contrast 1.5)
```

This piece embeds two other KidLisp pieces (`$39i` and `$r2f`) as fullscreen layers with 50% alpha (128), then applies contrast adjustment. The performance bottlenecks are:

1. **Embedded layer rendering** - Each frame renders 2 full child interpreters
2. **Layer compositing** - Alpha blending 2 fullscreen layers onto the main buffer
3. **Contrast adjustment** - Per-pixel LUT-based processing on CPU

## Current Architecture

### CPU Effects (`graph.mjs`)

| Effect | Implementation | Performance |
|--------|---------------|-------------|
| `flood` | Stack-based flood fill with visited array | O(n) pixels, high memory churn |
| `contrast` | Pre-computed LUT (256 entries), per-pixel loop | Fast but sequential |
| `brightness` | Pre-computed LUT, per-pixel loop | Fast but sequential |
| `blur` | Separable Gaussian, 2-pass convolution | GPU fallback available |
| `spin` | Polar coordinate transform | GPU fallback available |
| `zoom` | Inverse transform sampling | GPU fallback available |
| `scroll` | Wrapped coordinate offset | GPU fallback available |

### GPU Effects (`gpu-effects.mjs`)

Already implemented with WebGL2:
- ✅ `spin` - Polar rotation shader (pixel-perfect match to CPU)
- ✅ `zoom` - Inverse transform with wrapping
- ✅ `scroll` - Coordinate offset with wrapping
- ✅ `contrast` - Fragment shader adjustment (in composite shader)
- ✅ `brightness` - Fragment shader adjustment
- ✅ `blur` - Separable Gaussian (horizontal + vertical passes)
- ✅ `sharpen` - Unsharp mask filter

### Embedded Layers (`kidlisp.mjs`)

Current flow:
1. `embed` creates a persistent `EmbeddedLayer` object
2. Each frame, child KidLisp interpreter runs in isolated buffer
3. Buffer is `paste`d to main screen with alpha blending
4. `bake` creates persistent background layers

## Proposed GPU Acceleration

### Phase 1: GPU Flood Fill (High Impact)

The current CPU flood fill is a major bottleneck for pieces that use `flood` heavily.

**Approach**: Jump Flooding Algorithm (JFA) on GPU

```glsl
// Jump Flooding Algorithm - O(log n) passes for flood fill
// Pass 1: Initialize seed pixels
// Pass 2-N: Propagate nearest seed with halving step sizes

#version 300 es
precision highp float;

uniform sampler2D u_seeds;      // Current seed map (RGB = position, A = distance)
uniform sampler2D u_source;     // Original image for color matching
uniform vec2 u_resolution;
uniform int u_stepSize;         // Jump distance (starts at max, halves each pass)
uniform vec4 u_targetColor;     // Color to match for boundary

out vec4 fragColor;

void main() {
  ivec2 coord = ivec2(gl_FragCoord.xy);
  vec4 best = texelFetch(u_seeds, coord, 0);
  
  // Check 8 neighbors at current step size
  for (int dy = -1; dy <= 1; dy++) {
    for (int dx = -1; dx <= 1; dx++) {
      if (dx == 0 && dy == 0) continue;
      
      ivec2 neighbor = coord + ivec2(dx, dy) * u_stepSize;
      if (neighbor.x < 0 || neighbor.y < 0 || 
          neighbor.x >= int(u_resolution.x) || neighbor.y >= int(u_resolution.y)) continue;
      
      vec4 neighborSeed = texelFetch(u_seeds, neighbor, 0);
      if (neighborSeed.a < best.a) {
        // Check if path crosses boundary (color mismatch)
        vec4 sourceColor = texelFetch(u_source, coord, 0);
        if (sourceColor == u_targetColor) {
          best = neighborSeed;
        }
      }
    }
  }
  
  fragColor = best;
}
```

**Performance**: O(log₂(max(width, height))) passes vs O(n) pixels

### Phase 2: GPU Layer Compositing (High Impact for $cow)

Current: CPU `paste` with alpha blending per pixel
Proposed: Batch all embedded layers into single GPU composite pass

```glsl
#version 300 es
precision highp float;

uniform sampler2D u_background;
uniform sampler2D u_layer0;
uniform sampler2D u_layer1;
// ... up to 8 layers

uniform vec4 u_layerBounds[8];  // x, y, w, h for each layer
uniform float u_layerAlpha[8];
uniform int u_layerCount;

out vec4 fragColor;

void main() {
  ivec2 coord = ivec2(gl_FragCoord.xy);
  vec4 color = texelFetch(u_background, coord, 0);
  
  // Composite each layer in order
  for (int i = 0; i < 8; i++) {
    if (i >= u_layerCount) break;
    
    vec4 bounds = u_layerBounds[i];
    if (float(coord.x) >= bounds.x && float(coord.x) < bounds.x + bounds.z &&
        float(coord.y) >= bounds.y && float(coord.y) < bounds.y + bounds.w) {
      
      ivec2 layerCoord = coord - ivec2(bounds.xy);
      vec4 layerColor;
      
      // Sample from appropriate layer texture
      if (i == 0) layerColor = texelFetch(u_layer0, layerCoord, 0);
      else if (i == 1) layerColor = texelFetch(u_layer1, layerCoord, 0);
      // ... etc
      
      // Alpha blend
      float alpha = layerColor.a * u_layerAlpha[i] / 255.0;
      color = mix(color, layerColor, alpha);
    }
  }
  
  fragColor = color;
}
```

**Benefits**:
- Single GPU draw call for all layers
- No CPU-GPU round trips per layer
- Parallel alpha blending

### Phase 3: GPU Contrast/Brightness Pipeline

Already partially implemented in `COMPOSITE_FRAGMENT_SHADER`. Extend to be usable standalone:

```javascript
// In gpu-effects.mjs
export function gpuContrast(pixels, width, height, level, mask = null) {
  if (!initialized || !gl) return false;
  
  ensureResources(width, height);
  uploadPixels(pixels, width, height);
  
  gl.useProgram(compositeProgram);
  setUniform('u_zoomScale', 1.0);
  setUniform('u_scrollOffset', [0, 0]);
  setUniform('u_contrast', level);
  setUniform('u_brightness', 0);
  setBounds(mask || { x: 0, y: 0, width, height });
  
  renderAndReadback(pixels, width, height);
  return true;
}
```

### Phase 4: Batched Effect Pipeline

For pieces like `$cow` that chain multiple effects, batch them into a single GPU pipeline:

```javascript
// New API: Batched effect execution
export function gpuEffectBatch(pixels, width, height, effects) {
  // effects = [
  //   { type: 'layer', texture: layer0, bounds: {...}, alpha: 128 },
  //   { type: 'layer', texture: layer1, bounds: {...}, alpha: 128 },
  //   { type: 'contrast', level: 1.5 },
  // ]
  
  // Single upload, multiple shader passes, single readback
  ensureResources(width, height);
  uploadPixels(pixels, width, height);
  
  for (const effect of effects) {
    switch (effect.type) {
      case 'layer':
        applyLayerComposite(effect);
        break;
      case 'contrast':
        applyContrast(effect.level);
        break;
      // ... etc
    }
    // Ping-pong between framebuffers
    swapBuffers();
  }
  
  readbackPixels(pixels, width, height);
  return true;
}
```

## Implementation Priority

| Phase | Effect | Impact | Complexity | Est. Time |
|-------|--------|--------|------------|-----------|
| 1 | GPU Flood Fill (JFA) | High | Medium | 2-3 days |
| 2 | GPU Layer Compositing | High | Medium | 2 days |
| 3 | Standalone GPU Contrast | Medium | Low | 0.5 day |
| 4 | Batched Effect Pipeline | High | High | 3-4 days |

## Current GPU Hooks in graph.mjs

```javascript
// Existing GPU fallback pattern (blur example)
function blur(strength = 1, quality = "medium") {
  // 🚀 TRY GPU BLUR FIRST
  if (gpuSpinEnabled && gpuSpinAvailable && gpuSpinModule?.gpuBlur) {
    const success = gpuSpinModule.gpuBlur(pixels, width, height, strength, mask);
    if (success) {
      blurAccumulator = 0.0;
      return;
    }
  }
  
  // CPU FALLBACK
  // ... existing CPU implementation
}
```

This pattern should be extended for:
- `flood()` → `gpuSpinModule.gpuFlood()`
- `contrast()` → `gpuSpinModule.gpuContrast()`

## Memory Considerations

- Flood fill JFA requires 2 textures for ping-pong
- Layer compositing needs texture per layer (up to 8)
- All use existing `gl` context from `gpu-effects.mjs`
- Readback buffer already allocated (`readbackBuffer`)

## Testing Strategy

1. **Visual parity**: Compare GPU vs CPU output pixel-by-pixel
2. **Performance benchmarks**: 
   - `$cow` FPS before/after
   - Isolated `flood` on 1920x1080 canvas
   - 4-layer composite vs 4 sequential `paste` calls
3. **Edge cases**:
   - Flood fill at boundaries
   - Layers with partial transparency
   - Chained effects order

## Next Steps

1. Profile `$cow` to identify actual bottleneck percentages
2. Implement `gpuFlood` with JFA algorithm
3. Add GPU layer compositing to `embed` system
4. Create batched effect API for complex pieces
5. Add performance metrics to compare CPU vs GPU paths
