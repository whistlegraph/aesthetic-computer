# Parallel KidLisp Workers Feasibility Report

**Date:** January 29, 2026  
**Status:** Feasibility Analysis  
**Context:** Can embedded layers like `$cow` render `$39i` and `$r2f` in parallel workers?

---

## Executive Summary

**Verdict: Not Recommended** - The complexity significantly outweighs the benefits.

The current architecture already runs all KidLisp execution in a dedicated worker (`disk.mjs`). Spawning child workers for embedded layers introduces substantial complexity (state synchronization, buffer management, worker lifecycle) for marginal performance gains. The recently implemented GPU compositing provides better ROI.

---

## 1. Current Architecture

### Execution Context

```
┌─────────────────────────────────────────────────────────────────┐
│  Main Thread (bios.mjs)                                         │
│  - Input handling, resize events                                │
│  - Window management, DOM                                       │
└────────────────────────┬────────────────────────────────────────┘
                         │ postMessage
                         ▼
┌─────────────────────────────────────────────────────────────────┐
│  Disk Worker (disk.mjs)                                         │
│  - Runs piece code (paint(), beat(), act())                     │
│  - Manages embedded layers                                      │
│  - Calls graph.mjs for rendering                                │
│                                                                 │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │ Embedded Layer: $39i                                        ││
│  │ - Evaluates KidLisp code each frame                         ││
│  │ - Renders to its own buffer                                 ││
│  └─────────────────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────────────────┐│
│  │ Embedded Layer: $r2f                                        ││
│  │ - Evaluates KidLisp code each frame                         ││
│  │ - Renders to its own buffer                                 ││
│  └─────────────────────────────────────────────────────────────┘│
│                                                                 │
│  SEQUENTIAL: embeddedLayers.forEach(layer => render(layer))    │
└─────────────────────────────────────────────────────────────────┘
```

### Key Code Paths

**From kidlisp.mjs:5130-5175** - Sequential embedded layer rendering:
```javascript
// Paint all embedded layers first
this.embeddedLayers.forEach((embeddedLayer) => {
  this.renderSingleLayer(embeddedLayer, api, paintCount);
});

// Then composite them onto parent buffer
const layerBuffers = this.embeddedLayers.map((l) => ({
  pixels: l.buffer.pixels,
  width: l.buffer.width,
  height: l.buffer.height,
  alpha: l.alpha,
}));
api.compositeLayers(layerBuffers);
```

**From kidlisp.mjs:14625-14750** - `renderSingleLayer()` implementation:
- Switches to embedded buffer
- Updates frame counters
- Evaluates KidLisp source code
- Handles beat scheduling
- Restores parent buffer

### Why It's Sequential Now

1. **Shared State** - Embedded layers inherit from parent context (`parentEnv`)
2. **Buffer Management** - Each layer has its own buffer, but compositing happens on shared array
3. **Determinism** - Sequential execution ensures consistent frame results
4. **Simplicity** - No synchronization overhead

---

## 2. Proposed Parallel Architecture

### Design Option A: Child Workers

```
┌─────────────────────────────────────────────────────────────────┐
│  Disk Worker (Parent)                                           │
│  - Spawns child workers for each embedded layer                 │
│  - Coordinates frame timing                                     │
│  - Receives pixel buffers back                                  │
│  - Composites final result                                      │
└────────────────────────┬────────────────────────────────────────┘
                         │ spawn workers
         ┌───────────────┼───────────────┐
         ▼               ▼               ▼
┌────────────────┐┌────────────────┐┌────────────────┐
│ Worker: $39i   ││ Worker: $r2f   ││ Worker: $other │
│ - KidLisp eval ││ - KidLisp eval ││ - KidLisp eval │
│ - Own buffer   ││ - Own buffer   ││ - Own buffer   │
└────────────────┘└────────────────┘└────────────────┘
         │               │               │
         └───────────────┼───────────────┘
                         │ transferPixels
                         ▼
                 GPU Composite Layer
```

### Design Option B: SharedArrayBuffer Pool

```
┌─────────────────────────────────────────────────────────────────┐
│  Shared Memory Pool (SharedArrayBuffer)                         │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐               │
│  │Buffer 0 │ │Buffer 1 │ │Buffer 2 │ │ ...     │               │
│  │$39i     │ │$r2f     │ │         │ │         │               │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘               │
└─────────────────────────────────────────────────────────────────┘
         ▲               ▲               ▲
         │               │               │
    Worker $39i     Worker $r2f    (parallel write)
```

---

## 3. Technical Barriers

### 3.1 SharedArrayBuffer Not Available

From [shared-array-buffer-and-embedding.md](shared-array-buffer-and-embedding.md):

| Header | Required | Current |
|--------|----------|---------|
| `Cross-Origin-Embedder-Policy` | `require-corp` | **COMMENTED OUT** |
| `Cross-Origin-Opener-Policy` | `same-origin` | `same-origin-allow-popups` |

**Status:** `window.crossOriginIsolated = false`

**Why it's disabled:** COEP `require-corp` would break:
- NFT platform embedding (objkt, teia, OpenSea)
- External images without CORP headers
- YouTube embeds in chat
- External CDN assets

### 3.2 Without SharedArrayBuffer

Must use **Transferable ArrayBuffers** instead:

```javascript
// Send buffer TO worker (transfers ownership, original becomes detached)
childWorker.postMessage({ 
  pixels: buffer.data.buffer 
}, [buffer.data.buffer]);

// Worker sends back (also transfers)
self.postMessage({
  pixels: resultBuffer
}, [resultBuffer]);
```

**Problems:**
1. **Double copy per frame** - Send to worker, receive back
2. **No concurrent access** - Only one owner at a time
3. **Race conditions** - Must serialize access carefully
4. **Memory pressure** - Multiple full-frame buffers in flight

### 3.3 State Synchronization Challenges

Each KidLisp instance maintains state:

```javascript
// Per-layer state that must be synchronized
{
  frameCount: number,
  paintCount: number,
  beatCount: number,
  lastBeatTime: number,
  environment: Map,     // Variables defined in KidLisp
  canvas: { width, height },
  audio: { bpm, triggered sounds },
  random: { seed state }
}
```

**Synchronization needed each frame:**
- `frameCount`, `paintCount` from parent
- BPM and timing from global audio
- Random seed continuity
- Parent-defined variables via `parentEnv`

### 3.4 Worker Lifecycle Complexity

Current embedded layers are cheap to create/destroy:
```javascript
// Current: Just add to array
this.embeddedLayers.push(new EmbeddedLayer(...));

// Proposed: Must spawn worker, wait for init, handle errors
const worker = new Worker(embeddedLayerWorkerURL);
await new Promise(resolve => {
  worker.onmessage = (e) => {
    if (e.data.type === 'ready') resolve();
  };
});
```

**New failure modes:**
- Worker creation failure
- Worker crash mid-frame
- Message queue backup
- Memory leaks from abandoned workers

---

## 4. Performance Analysis

### Current Frame Budget

At 60 FPS: **16.67ms per frame**

**Typical $cow frame breakdown:**
| Phase | Time | Notes |
|-------|------|-------|
| Parent eval | ~2-4ms | $cow's own code |
| $39i eval | ~3-6ms | Simple piece |
| $r2f eval | ~3-6ms | Simple piece |
| GPU composite | ~0.5-1ms | ✅ Already optimized |
| **Total** | ~9-17ms | Usually fits budget |

### Parallel Execution Ceiling

**Best case** (perfect parallelism):
- Parent + max(child1, child2) instead of parent + child1 + child2
- Savings: ~3-6ms per frame

**Realistic case** (with overhead):
| Overhead | Time |
|----------|------|
| Worker spawn | ~5-50ms (one-time) |
| postMessage per frame | ~0.5-2ms |
| Buffer transfer | ~0.5-1ms |
| Synchronization wait | ~1-3ms |
| **Per-frame overhead** | ~2-6ms |

**Net gain: Potentially negative!**

The overhead of worker communication can exceed the time saved by parallelization for simple embedded pieces.

### When Parallelization Would Help

| Scenario | Benefit |
|----------|---------|
| 4+ embedded layers | Moderate |
| Complex layers (>5ms each) | High |
| Long-running simulations | High |
| Simple layers (<3ms each) | **Negative** |
| `$cow` (2 simple layers) | **Minimal** |

---

## 5. Alternative Approaches (Already Implemented)

### ✅ GPU Layer Compositing (Phase 2)

From [gpu-acceleration-plan.md](gpu-acceleration-plan.md):

```javascript
// 8 layers composited in single GPU call
gpuCompositeLayers(layerBuffers); // ~0.5ms for any count
```

**Status:** Implemented in `gpu-effects.mjs`

### ✅ Batched Effect Pipeline (Phase 4)

```javascript
// zoom + scroll + contrast in one pass
gpuComposite({ zoom, scroll, contrast }); // ~1ms total
```

**Status:** Implemented in `kidlisp.mjs`

### 💡 Proposed: Async Layer Evaluation

Instead of full worker parallelism, evaluate layers asynchronously within the same worker:

```javascript
// Current (blocking)
embeddedLayers.forEach(layer => this.renderSingleLayer(layer));

// Proposed (cooperative scheduling)
for (const layer of embeddedLayers) {
  this.renderSingleLayer(layer);
  await scheduler.yield(); // Let other tasks run
}
```

**Benefits:**
- No new workers
- No buffer transfer
- Better responsiveness
- Simpler error handling

**Drawback:** Still sequential, just non-blocking

---

## 6. Recommendation

### For `$cow` Specifically: **Don't Parallelize**

1. **Overhead > Savings** - Worker communication costs exceed the ~6ms saved
2. **GPU already helps** - Layer compositing is now GPU-accelerated
3. **Complexity budget** - Better spent on other features

### For Future High-Performance Pieces

If a piece absolutely needs parallel embedded layers:

1. **Opt-in flag** - `(embed $piece {:parallel true})`
2. **SharedArrayBuffer subdomain** - `isolated.kidlisp.com` with strict COOP/COEP
3. **Worker pool** - Pre-spawned workers, reused across frames
4. **Threshold** - Only parallelize layers that take >5ms individually

### Better Investment of Effort

| Priority | Feature | Impact |
|----------|---------|--------|
| 1 | ✅ GPU compositing | Done - big win |
| 2 | ✅ Batched effects | Done - reduces passes |
| 3 | WASM KidLisp interpreter | 10-50x eval speedup |
| 4 | Smarter layer caching | Skip unchanged layers |
| 5 | Parallel workers | Last resort |

---

## 7. Conclusion

Parallel Web Workers for embedded KidLisp layers is **technically possible but not recommended** for the following reasons:

1. **SharedArrayBuffer unavailable** due to embedding requirements
2. **Transferable overhead** negates parallelism gains for simple layers
3. **State sync complexity** adds maintenance burden
4. **GPU acceleration already delivered** the easy performance wins
5. **WASM interpreter** would provide better speedup for less complexity

**The recently implemented GPU compositing (Phases 1-4) provides a better cost/benefit ratio than parallel workers.**

---

## Appendix: Quick Reference

### Check If Parallelization Would Help

```javascript
// In piece code, measure layer eval time
const t0 = performance.now();
embeddedLayers.forEach(l => renderSingleLayer(l));
const evalTime = performance.now() - t0;

// Worth parallelizing if:
// - embeddedLayers.length >= 3
// - evalTime > 8ms
// - individual layers take >3ms each
```

### Enable SharedArrayBuffer (If Needed Later)

In `netlify.toml`:
```toml
# Uncomment and accept breaking changes:
Cross-Origin-Embedder-Policy = "require-corp"
Cross-Origin-Opener-Policy = "same-origin"
```

Then verify:
```javascript
console.log(window.crossOriginIsolated); // Should be true
console.log(typeof SharedArrayBuffer); // Should be 'function'
```
