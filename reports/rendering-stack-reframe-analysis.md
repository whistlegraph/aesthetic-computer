# Rendering Stack & Reframe Analysis

## Problem Statement
Black flicker occurs during page resize (reframe). The flicker happens for a single frame when transitioning from the disk.mjs worker-side screen buffer to the WebGL composite rendering.

## Canvas Stack (Z-Index Order)

| Canvas | Z-Index | Purpose |
|--------|---------|---------|
| `underlayFrame` | -1 | Tape video playback (behind everything) |
| `canvas` | (default) | Main 2D software rendering |
| `webglCompositeCanvas` | 0 | WebGL2 blitter for pixel buffer rendering |
| `webgpuCanvas` | 1 | WebGPU 2D renderer surface |
| `overlayCan` | 2 | HUD labels, QR codes, progress bars |
| `recordingUICan` | 3 | Recording UI (not captured in tapes) |
| `freezeFrameCan` | 10 | Resolution transition buffer |

## Pixel Data Flow

```
DISK WORKER (disk.mjs)                    BIOS MAIN THREAD (bios.mjs)
─────────────────────────                 ────────────────────────────
piece.paint()                             
    ↓                                     
screen.pixels (Uint8ClampedArray)         
    ↓                                     
postMessage(buffer, [transferables])  →   onMessage
                                              ↓
                                          new Uint8ClampedArray(content.pixels)
                                              ↓
                                          new ImageData(pixels, w, h)
                                              ↓
                                          webglBlitter.render(imageData)
                                          OR ctx.putImageData(imageData)
```

## Reframe Sequence

### 1. Resize Event (bios.mjs ~L1750)
```javascript
window.addEventListener("resize", () => {
  setTimeout(() => {
    freezeFrame = true;
    freezeFrameGlaze = glaze.on;
    freezeFrameCan.width = imageData.width;
    freezeFrameCan.height = imageData.height;
    needsReframe = true;
  }, curReframeDelay);
});
```

### 2. frame() Function Called (bios.mjs ~L1163)

**Step A: Capture freeze frame (if conditions met)**
```javascript
if (freezeFrame && imageData && imageData.data.buffer.byteLength > 0 && !underlayFrame) {
  // Position freeze frame canvas
  freezeFrameCan.style.width = canvas.getBoundingClientRect().width + "px";
  freezeFrameCan.style.height = canvas.getBoundingClientRect().height + "px";
  freezeFrameCan.style.left = canvasRect.x + "px";
  freezeFrameCan.style.top = canvasRect.y + "px";
  
  // Capture from correct source
  const freezeSource = webglCompositeIsActive ? webglCompositeCanvas : canvas;
  ffCtx.drawImage(freezeSource, 0, 0);
  
  wrapper.append(freezeFrameCan);
  freezeFrameFrozen = true;
}
```

**Step B: Preserve pixels in temp canvas**
```javascript
tempCanvas.width = canvas.width;
tempCanvas.height = canvas.height;
tempCtx.putImageData(imageData, 0, 0);  // or tempCtx.drawImage(canvas, 0, 0)
```

**Step C: Resize canvases (CLEARS THEM)**
```javascript
canvas.width = width;
canvas.height = height;
webgpuCanvas.width = width;
webgpuCanvas.height = height;
webglCompositeCanvas.width = width;  // ← CLEARED TO BLACK
webglCompositeCanvas.height = height;
overlayCan.width = width;
overlayCan.height = height;
```

**Step D: Restore main canvas**
```javascript
ctx.drawImage(tempCanvas, 0, 0);
```

**Step E: Send reframe to worker**
```javascript
send({ type: "reframed", content: { width, height, ... } });
send({ type: "needs-paint" });
```

### 3. Worker Handles Reframe (disk.mjs ~L10455)
```javascript
screen.width = content.width;
screen.height = content.height;
screen.pixels = new Uint8ClampedArray(content.width * content.height * 4);
// (Currently attempts to copy old pixels)
```

### 4. Worker Sends Pixels Back
```javascript
sendData.pixels = screen.pixels;
send({ type: "render", content: sendData }, [sendData.pixels.buffer]);
```

### 5. Bios Renders (bios.mjs ~L16230)
```javascript
if (content.pixels?.byteLength > 0 && content.width === screen.width) {
  screen.pixels = new Uint8ClampedArray(content.pixels);
  imageData = new ImageData(screen.pixels, width, height);
}
// Later in draw():
webglBlitter.render(imageData);
```

### 6. Freeze Frame Removal (bios.mjs ~L17479)
```javascript
if (freezeFrame && freezeFrameFrozen && (!glaze.on || glazeReady)) {
  freezeFrameCan.style.opacity = "0";
  setTimeout(() => freezeFrameCan.remove(), 60);
  freezeFrame = false;
}
```

## Identified Issues

### Issue 1: Freeze Frame Canvas Positioning
**Location:** bios.mjs frame() function

The freeze frame uses `canvas.getBoundingClientRect()` but this returns the **CSS layout** dimensions, not the canvas bitmap dimensions. If the canvas is scaled via CSS (which it often is), the freeze frame won't align correctly.

**Current code:**
```javascript
freezeFrameCan.style.width = canvas.getBoundingClientRect().width + "px";
freezeFrameCan.style.height = canvas.getBoundingClientRect().height + "px";
freezeFrameCan.style.left = canvasRect.x + "px";
freezeFrameCan.style.top = canvasRect.y + "px";
```

**Problem:** `canvasRect` may be stale (captured earlier at `canvasRect = canvas.getBoundingClientRect()`)

### Issue 2: WebGL Composite Not Preserved During Resize
**Location:** bios.mjs frame() function

When `webglCompositeCanvas.width = width` is set, the WebGL context is cleared. Even with `preserveDrawingBuffer: true`, resizing the canvas clears it.

**The freeze frame MUST be visible before this happens.**

### Issue 3: Worker Pixel Copy Logic Bug
**Location:** disk.mjs reframed handler

The current pixel copy logic may create a buffer that doesn't match expected dimensions if oldPixels is from a detached buffer or wrong size:

```javascript
// Error: "The input data length is not equal to (4 * width * height)"
// This happens when screen.pixels.length !== width * height * 4
```

### Issue 4: Freeze Frame Capture Source
**Location:** bios.mjs frame() function

The freeze frame is captured from either `canvas` or `webglCompositeCanvas`, but:
1. If WebGL composite is active, `canvas` is hidden (`visibility: hidden`)
2. The WebGL composite needs `preserveDrawingBuffer: true` to be readable

**Current check:**
```javascript
const webglCompositeIsActive = webglBlitter?.isReady() && 
                               webglCompositeCanvas.style.display !== "none";
```

### Issue 5: Timing of Freeze Frame Display
**Location:** bios.mjs frame() function

The freeze frame is appended to the wrapper, but it may not be painted by the browser before the canvas resize happens (both in same JS execution context).

**Potential fix:** Force a browser repaint before resizing canvases.

## Recommended Fixes

### ✅ Fix 1: Force Browser Paint Before Resize (IMPLEMENTED)
```javascript
// After appending freeze frame, force a paint
wrapper.append(freezeFrameCan);
freezeFrameCan.offsetHeight; // Force reflow/paint
freezeFrameFrozen = true;
```

### ✅ Fix 2: Match Freeze Frame to Wrapper, Not Canvas (IMPLEMENTED)
```javascript
// Use 100% dimensions instead of canvas getBoundingClientRect
freezeFrameCan.style.width = "100%";
freezeFrameCan.style.height = "100%";
freezeFrameCan.style.left = "0";
freezeFrameCan.style.top = "0";
freezeFrameCan.style.imageRendering = "pixelated";
```

### ✅ Fix 3: Use imageData for Freeze Frame Capture (IMPLEMENTED)
```javascript
// Use imageData directly instead of canvas drawImage to avoid WebGL readback issues
if (imageData && imageData.data && imageData.data.buffer && imageData.data.buffer.byteLength > 0) {
  try {
    ffCtx.putImageData(imageData, 0, 0);
  } catch (e) {
    // Fallback to canvas copy
    ffCtx.drawImage(freezeSource, 0, 0);
  }
}
```

### ✅ Fix 4: Remove Pixel Copy in Worker (IMPLEMENTED)
```javascript
// In disk.mjs reframed handler - just create new buffer, don't copy
screen.width = content.width;
screen.height = content.height;
screen.pixels = new Uint8ClampedArray(content.width * content.height * 4);
// The freeze frame in bios.mjs provides visual continuity
// The piece's paint() will fill correct content on next frame
```

### ✅ Fix 5: High Z-Index for Freeze Frame (IMPLEMENTED)
```javascript
// Freeze frame canvas initialized with high z-index
freezeFrameCan.style.position = "absolute";
freezeFrameCan.style.zIndex = "10"; // Above all other canvases
freezeFrameCan.style.pointerEvents = "none";
```

## Files Involved

- `system/public/aesthetic.computer/bios.mjs` - Main thread rendering, reframe handling
- `system/public/aesthetic.computer/lib/disk.mjs` - Worker rendering, pixel buffer management  
- `system/public/aesthetic.computer/lib/webgl-blit.mjs` - WebGL2 pixel blitter
- `system/public/aesthetic.computer/lib/glaze.mjs` - WebGL shader effects layer

## Test Cases

1. Resize window slowly - should see freeze frame hold steady
2. Resize window rapidly - should not see black flashes
3. Resize while glaze is enabled - should transition smoothly
4. Resize during tape playback - freeze frame should NOT appear (underlayFrame active)
