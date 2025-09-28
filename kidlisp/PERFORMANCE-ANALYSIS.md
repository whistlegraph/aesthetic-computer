# KidLisp Performance Analysis & Optimization Report

## Executive Summary

Analysis of the `$cow` piece rendering pipeline reveals several performance bottlenecks in the alpha compositing and embedded layer system. The piece composites two animated layers (`$39i` and `$r2f`) at 120fps with complex timing expressions, creating significant computational overhead.

## Piece Analysis: $cow

### Structure
```
📁 $cow (composite layer)
 ├─ 📄 $39i (background effects layer)
 └─ 📄 $r2f (foreground zoom layer)
```

### Source Code Breakdown

**Main Compositor ($cow)**:
```kidlisp
($39i 0 0 w h 128)  ; Background layer at 50% opacity
($r2f 0 0 w h 128)  ; Foreground layer at 50% opacity  
(contrast 1.5)      ; Expensive post-processing effect
```

**Background Layer ($39i)**: 11 operations/frame
- Complex timing: 5 different timer expressions (0.1s, 1.5s, 1s, 2s, 0.3s)
- Heavy operations: `flood`, `scroll`, `zoom`, `blur`, `contrast`, `spin`
- Random point generation: `(repeat 30 point)`

**Foreground Layer ($r2f)**: 11 operations/frame  
- High-frequency zoom: `(0.1s (zoom (? 1.89 1 1.1 1.2)))` every 100ms
- Continuous effects: `scroll`, `spin`, `blur`
- Multiple flood fills: `(repeat 2 (flood ? ?))`

## Performance Bottlenecks

### 1. Alpha Compositing Pipeline

**Current Implementation** (`graph.mjs` lines 1558-1610):
```javascript
function blend(dst, src, si, di, alphaIn = 1) {
  // Branch A: Transparent pixel compositing (expensive)
  if (dst[di + 3] < 255 && src[si + 3] > 0) {
    const alphaSrc = (src[si + 3] * alphaIn) / 255;
    const alphaDst = dst[di + 3] / 255;
    const combinedAlpha = alphaSrc + (1.0 - alphaSrc) * alphaDst;
    // Per-channel floating-point math (3 divisions, 6 multiplications)
    for (let offset = 0; offset < 3; offset++) {
      dst[di + offset] = (src[si + offset] * alphaSrc + 
                         dst[di + offset] * (1.0 - alphaSrc) * alphaDst) / 
                         (combinedAlpha + epsilon);
    }
  } else {
    // Branch B: Opaque pixel compositing (faster integer math)
    const alpha = src[si + 3] * alphaIn + 1;
    const invAlpha = 256 - alpha;
    dst[di] = (alpha * src[si + 0] + invAlpha * dst[di + 0]) >> 8;
    // ... similar for G, B channels
  }
}
```

**Performance Issues**:
- **Floating-point overhead**: Branch A uses expensive division and floating-point arithmetic
- **Branch prediction**: Transparent vs opaque pixel branching creates CPU pipeline stalls
- **Memory access pattern**: Non-sequential pixel access in compositing loops

### 2. Layer Rendering Frequency

**Current Execution Pattern**:
- `$cow` renders at 120fps (8.33ms budget per frame)
- Each embedded layer renders independently 
- `$39i`: 5 timer expressions firing at different intervals
- `$r2f`: High-frequency zoom (10 times per second)
- Double alpha compositing: Each layer → main buffer → final output

**Frame Budget Breakdown** (estimated):
```
Per-frame operations (120fps = 8.33ms budget):
├─ $39i rendering:     ~2.5ms (timer evaluation + effects)
├─ $r2f rendering:     ~2.0ms (zoom calculations + blending)  
├─ Alpha compositing:  ~2.5ms (pixel-by-pixel blending)
├─ Contrast effect:    ~1.0ms (post-processing)
└─ Overhead:           ~0.33ms (timing, evaluation)
Total:                 ~8.33ms (100% budget utilization)
```

### 3. Timing System Overhead

**Recent Fix Applied**: Removed `setTimeout`-based timing in favor of frame-based counting:
```javascript
// OLD (expensive): 
setTimeout(() => { /* execute timing expression */ }, delay);

// NEW (efficient):
if (this.frameCount - lastExecution >= targetFrames) {
  // execute immediately in frame context
}
```

**Remaining Issues**:
- Timer expressions are evaluated every frame even when not firing
- Context switching between embedded layers
- Redundant timing key generation: `${head}-${cacheId}-${JSON.stringify(args)}`

## Optimization Opportunities

### 1. Alpha Compositing Optimizations

**A. SIMD-style Bulk Operations**
```javascript
// Instead of pixel-by-pixel blending, process in chunks
function fastBlendChunk(dst, src, dstIdx, srcIdx, count, alpha) {
  // Process 4 pixels at once using typed array operations
  const alpha256 = (alpha * 256) | 0;
  const invAlpha = 256 - alpha256;
  
  for (let i = 0; i < count * 4; i += 4) {
    dst[dstIdx + i] = ((alpha256 * src[srcIdx + i] + invAlpha * dst[dstIdx + i]) >> 8);
    dst[dstIdx + i + 1] = ((alpha256 * src[srcIdx + i + 1] + invAlpha * dst[dstIdx + i + 1]) >> 8);
    dst[dstIdx + i + 2] = ((alpha256 * src[srcIdx + i + 2] + invAlpha * dst[dstIdx + i + 2]) >> 8);
    // Skip alpha channel for opaque blending
  }
}
```

**B. Pre-multiplied Alpha**
```javascript
// Store layers in pre-multiplied format to avoid runtime multiplication
function convertToPremultiplied(pixels) {
  for (let i = 0; i < pixels.length; i += 4) {
    const alpha = pixels[i + 3] / 255;
    pixels[i] *= alpha;     // R
    pixels[i + 1] *= alpha; // G  
    pixels[i + 2] *= alpha; // B
  }
}
```

**Estimated Gain**: 40-60% faster compositing

### 2. Layer Rendering Optimizations

**A. Dirty Rectangle Tracking**
```javascript
class LayerBuffer {
  constructor(width, height) {
    this.pixels = new Uint8Array(width * height * 4);
    this.dirtyBox = null; // Track changed regions
  }
  
  markDirty(x, y, w, h) {
    if (!this.dirtyBox) {
      this.dirtyBox = { x, y, w, h };
    } else {
      // Expand dirty box to include new region
      this.dirtyBox = expandBox(this.dirtyBox, { x, y, w, h });
    }
  }
}
```

**B. Layer Caching Strategy**
```javascript
// Cache layer results when no animations are active
const layerCache = new Map();

function renderLayerWithCaching(layerId, hasActiveTimers) {
  if (!hasActiveTimers && layerCache.has(layerId)) {
    return layerCache.get(layerId);
  }
  
  const result = renderLayer(layerId);
  if (!hasActiveTimers) {
    layerCache.set(layerId, result);
  }
  return result;
}
```

**Estimated Gain**: 30-50% reduction in redundant layer rendering

### 3. Timing System Optimizations

**A. Timer Batching**
```javascript
// Group timers by interval for batch processing
const timerBatches = {
  '0.1s': [], // 12-frame intervals
  '1s': [],   // 120-frame intervals  
  '1.5s': [], // 180-frame intervals
};

function processTimerBatch(interval, frameCount) {
  if (frameCount % getFramesForInterval(interval) === 0) {
    timerBatches[interval].forEach(timer => timer.execute());
  }
}
```

**B. Lazy Timer Key Generation**
```javascript
// Cache timer keys to avoid JSON.stringify overhead
const timerKeyCache = new WeakMap();

function getTimerKey(head, cacheId, args) {
  if (!timerKeyCache.has(args)) {
    timerKeyCache.set(args, `${head}-${cacheId}-${JSON.stringify(args)}`);
  }
  return timerKeyCache.get(args);
}
```

**Estimated Gain**: 15-25% reduction in timing overhead

## Recommended Implementation Plan

### Phase 1: Critical Path Optimization (High Impact)
1. **Implement integer-only alpha compositing** for opaque blending cases
2. **Add SIMD-style bulk pixel operations** for large layer composites  
3. **Implement dirty rectangle tracking** for incremental updates

### Phase 2: Memory & Caching (Medium Impact)  
4. **Add layer result caching** for static content
5. **Implement pre-multiplied alpha storage** format
6. **Optimize timer key generation** with caching

### Phase 3: Advanced Optimizations (Lower Impact)
7. **WebGL compositing pipeline** for complex effects
8. **Worker thread layer rendering** for parallel processing
9. **Adaptive quality scaling** based on performance metrics

## Measurement & Validation

### Performance Metrics to Track
```javascript
// Add to graph.mjs for performance monitoring
const perfMetrics = {
  blendTime: 0,
  layerRenderTime: 0,
  timingOverhead: 0,
  frameDrops: 0
};

function measureBlendPerformance(fn) {
  const start = performance.now();
  fn();
  perfMetrics.blendTime += performance.now() - start;
}
```

### Expected Performance Gains
- **Alpha Compositing**: 40-60% faster → ~1.5ms savings per frame
- **Layer Rendering**: 30-50% reduction → ~1.0ms savings per frame  
- **Timing Overhead**: 15-25% reduction → ~0.2ms savings per frame
- **Total Improvement**: ~2.7ms per frame (32% performance gain)

This would provide significant headroom for more complex effects and better frame stability at 120fps.

## Current Status

✅ **Completed**: Removed setTimeout-based timing (major architectural fix)  
🔄 **In Progress**: Performance measurement infrastructure  
📋 **Next**: Implement integer-only alpha compositing optimization

---
*Last Updated: September 6, 2025*  
*Analysis Target: $cow embedded layer composition*
