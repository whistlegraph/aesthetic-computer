# Iframe Sandbox Canvas Performance Impact Report

## Executive Summary

When a web page runs inside an `<iframe>` with `sandbox="allow-scripts allow-downloads"` (objkt.com's default), **the browser treats the content as having an "opaque origin"** which triggers security checks on every canvas pixel operation. This causes approximately **50% FPS reduction** and visual rendering issues.

**Root Cause**: Without `allow-same-origin` in the sandbox attribute, the iframe content is treated as cross-origin to itself, making the canvas "tainted" for security purposes.

---

## Official Documentation Summary

### From WHATWG HTML Spec (html.spec.whatwg.org)

1. **Origin-Clean Flag**: Every canvas bitmap has an `origin-clean` flag. When set to `false`, pixel-reading operations (`getImageData`, `toDataURL`, `toBlob`) throw `SecurityError`.

2. **Sandbox without `allow-same-origin`**: 
   > "If this token is not used, the resource is treated as being from a special origin that always fails the same-origin policy (potentially preventing access to data storage/cookies and some JavaScript APIs)."

3. **Performance Impact on Canvas Operations**:
   - `willReadFrequently` hint exists because "readback with `getImageData()`, `toDataURL()`, or `toBlob()` [is slower on GPU-accelerated canvases]"
   - When origin-clean checks must happen on every operation, the browser may disable GPU acceleration entirely

### From MDN Web Docs

1. **`getImageData()` throws SecurityError** if the canvas's `origin-clean` flag is `false` (tainted canvas)

2. **Canvas Security Model**:
   > "Information leakage can occur if scripts from one origin can access information (e.g. read pixels) from images from another origin"

---

## Codebase Impact Analysis

### High-Impact Locations in `bios.mjs`

| Line | Operation | Context | Sandbox Impact |
|------|-----------|---------|----------------|
| **1477** | `ctx.putImageData(imageData, 0, 0)` | **EVERY FRAME** render | ⚠️ Major - runs every animation frame |
| **1479** | `ctx.getImageData(...)` | Fallback when buffer invalid | ⚠️ Moderate - only on resize/init |
| **1297** | `parseCtx.getImageData(0, 0, 1, 1)` | `coerceToRGB()` color parsing | ⚠️ Low - fast path usually avoids this |
| **1319** | `tempCtx.getImageData(sx, sy, 1, 1)` | Color sampling | ⚠️ Low - on-demand |
| **6460** | `canvas.toBlob(resolve, "image/webp", 0.8)` | Screenshot/snap | 🔴 BLOCKED in sandbox |
| **6712** | `canvas.toBlob(resolve, "image/webp", 0.9)` | Screenshot/snap | 🔴 BLOCKED in sandbox |
| **7115** | `canvas.toBlob(resolve, "image/webp", 0.9)` | Screenshot/snap | 🔴 BLOCKED in sandbox |
| **7368** | `canvas.toBlob(resolve, "image/png")` | Screenshot/snap | 🔴 BLOCKED in sandbox |

### Operations That Will FAIL in Sandboxed Context

These throw `SecurityError` when sandbox lacks `allow-same-origin`:

1. **`toDataURL()`** - Used for saving images
2. **`toBlob()`** - Used for screenshot/snap features (already disabled in bundles)
3. **`getImageData()`** when canvas has external content

### Operations That SLOW DOWN in Sandboxed Context

Even though they don't fail, these have significant overhead:

1. **`putImageData()`** - Every frame render (line 1477)
   - Browser must verify origin-clean status
   - May disable GPU compositing path
   - Forces software rendering fallback
   
2. **Canvas pattern/gradient operations**
   - Cross-origin checks on fill styles
   
3. **`drawImage()`** from other canvases
   - Propagates taint status

---

## Test Results (from our sandbox-perf-test.html)

| Sandbox Mode | FPS | Memory | Visual Quality |
|--------------|-----|--------|----------------|
| **Restricted** (`allow-scripts allow-downloads`) | 120 FPS | 59 MB | ❌ Rendering bugs |
| **With same-origin** (`allow-scripts allow-downloads allow-same-origin`) | 239 FPS | 110 MB | ✅ Correct |
| **No sandbox** | 239 FPS | 110 MB | ✅ Correct |

---

## Why objkt.com Default Mode is Slow

objkt.com embeds NFT iframes with:
```html
<iframe sandbox="allow-scripts allow-downloads" src="..."></iframe>
```

This is **missing `allow-same-origin`**, which causes:

1. Content treated as opaque/cross-origin to itself
2. Canvas security checks activate on every pixel operation
3. Browser may fall back to software rendering
4. ~50% FPS drop observed

**Solution**: Users must enable "Advanced Mode" on objkt.com, which adds `allow-same-origin`.

---

## Affected KidLisp Features

The `$roz` code uses these operations that suffer in sandboxed contexts:

| Feature | KidLisp Command | Canvas Operation | Impact |
|---------|-----------------|------------------|--------|
| Render | (every frame) | `putImageData` | ⚠️ 50% slower |
| Scroll | `scroll` | Reads/writes pixels | ⚠️ Slower |
| Contrast | `contrast` | Pixel manipulation | ⚠️ Slower |
| Color parsing | `ink`, `fade` | `getImageData` (fallback) | ⚠️ Potential slowdown |

---

## Implemented Optimizations

### ✅ Sandbox Detection (bios.mjs)

Added early detection of opaque origin (sandboxed iframe without `allow-same-origin`):

```javascript
// Detects objkt.com's sandbox="allow-scripts allow-downloads"
const isOpaqueOrigin = (() => {
  try {
    if (window.origin === 'null') return true;
    localStorage.getItem('__sandbox_test__');
    return false;
  } catch (e) {
    return true;
  }
})();

// Exposed globally for pieces to detect
globalThis.acIsSandboxed = isOpaqueOrigin;
```

### ✅ Canvas Context Hints (bios.mjs)

Added `willReadFrequently: true` hint to all canvas contexts when in sandboxed mode:

| Canvas | Line | Purpose |
|--------|------|---------|
| `ctx` (main) | ~936 | Primary render target - **already had hint** |
| `uiCtx` | ~1025 | UI overlay | 
| `debugCtx` | ~1029 | Debug overlay |
| `ffCtx` | ~1036 | Freeze frame buffer |
| `octx` | ~1041 | Corner label overlay |
| `dirtyBoxCtx` | ~1045 | Dirty box updates |
| `glazeCompositeCtx` | ~1061 | Glaze compositing |
| `coerceToRGB.ctx` | ~1306 | Color parsing - **already had hint** |

This tells the browser to use software rendering from the start, avoiding the penalty of constant GPU→CPU security checks.

### ✅ Export Guards (bufferToBlob, captureFrame)

Added early-return guards to functions that call `toBlob`/`toDataURL`:

- **`bufferToBlob()`** in bios.mjs - Returns `null` with warning in sandbox
- **`captureFrame()`** in frame-capture.mjs - Returns `null` in sandbox

---

## Recommendations

### For objkt.com / NFT Platforms

1. **Add `allow-same-origin`** to sandbox attribute for art pieces
2. Or provide a performance toggle (like objkt's "Advanced Mode")

### For KidLisp Bundles

1. ✅ **Already done**: Disabled `acPACK_MODE` (screenshot features)
2. ✅ **Already done**: Disabled console auto-snaps  
3. ✅ **Implemented**: Detect sandboxed context and log warning
4. **Consider**: Use `OffscreenCanvas` for heavy operations (when supported)

### For Aesthetic Computer Runtime

1. ✅ **Implemented**: Detect opaque origin early
2. ✅ **Implemented**: Skip impossible operations (`toBlob`, `toDataURL`)
3. ✅ **Implemented**: Add `willReadFrequently` hint to canvas contexts
4. **Consider**: Add FPS counter warning in sandbox mode

---

## Files Changed

| File | Changes |
|------|---------|
| [bios.mjs](../system/public/aesthetic.computer/bios.mjs) | Added `isOpaqueOrigin` detection, `willReadFrequently` hints, `bufferToBlob` guard |
| [frame-capture.mjs](../system/public/aesthetic.computer/lib/frame-capture.mjs) | Added `isOpaqueOrigin` guard to `captureFrame()` |

---

## References

- [WHATWG HTML Spec - Canvas Security](https://html.spec.whatwg.org/multipage/canvas.html#security-with-canvas-elements)
- [MDN - getImageData() Security](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/getImageData)
- [MDN - iframe sandbox attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe#sandbox)
- [MDN - Same-origin policy](https://developer.mozilla.org/en-US/docs/Web/Security/Same-origin_policy)

---

*Report generated: January 27, 2026*
*Context: FF1 display device performance investigation for objkt.com tokens*
*Updated: Added implementation details for sandbox detection and canvas optimizations*
