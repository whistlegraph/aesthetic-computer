# Glaze Resize/Reframe Synchronization Plan

## Problem Statement
During window resize on `prompt.mjs` (dark mode), users occasionally see **both** the glaze canvas and the underlying main canvas, causing a visual "double vision" or layering artifact.

## Root Cause Analysis

### Canvas Stack Architecture (z-index order)
1. **3D canvas** (z-index: 0) - Three.js
2. **WebGPU canvas** (z-index: 1)
3. **Glaze canvas** (z-index: 2) - WebGL2 post-processing
4. **Freeze frame canvas** (z-index: 3, position: fixed)
5. **UI canvas** (z-index: 6)
6. **Main 2D canvas** (z-index: 7)
7. **Debug canvas** (z-index: 8)

### Current Resize Flow (bios.mjs)

1. **Window resize event** → debounced (80ms `REFRAME_DELAY`)
2. **`needsReframe = true`** triggers `frame()` call
3. **Freeze frame creation** (lines 1016-1071):
   - Copy current canvas to `freezeFrameCan`
   - If glaze is on: `Glaze.freeze(ffCtx)` - draws glaze canvas to freeze frame
   - Set `canvas.style.opacity = 0` (hide main canvas)
   - Append freeze frame to wrapper
4. **Canvas resize** (lines 1156-1158):
   - `canvas.width = width; canvas.height = height`
   - This clears the canvas!
5. **Glaze reload** (lines 1538-1557):
   - `Glaze.on()` checks if dimensions changed
   - If changed: creates new Glaze instance, reloads shaders
   - Glaze canvas starts with `opacity: 0` (glaze.mjs line 40)
6. **Frame completion** (lines 15938-15951):
   - Remove freeze frame: `freezeFrameCan.remove()`
   - Call `Glaze.unfreeze()` - removes opacity property
   - If glaze off: `canvas.style.removeProperty("opacity")`

### Identified Race Conditions

#### Issue 1: Glaze shader reload timing
In `glaze.mjs` `on()` function (line 340):
```javascript
await glaze.load(() => {
  offed = false;
  frame(w, h, rect, nativeWidth, nativeHeight, wrapper);
  loaded();
});
```
The glaze shaders load **asynchronously**, but `frame()` is called inside the callback. If a resize happens during shader compilation, the canvas dimensions and glaze dimensions can get out of sync.

#### Issue 2: Opacity transition timing (CSS)
```css
canvas[data-type="glaze"].first-glaze {
  transition: 0.5s opacity;
}
```
The `first-glaze` class adds a 500ms opacity transition. During rapid resizes, this transition may not complete before another resize triggers, leaving both canvases partially visible.

#### Issue 3: Freeze frame removal timing
In `bios.mjs` lines 15938-15951:
```javascript
if (freezeFrame && freezeFrameFrozen) {
  if (glaze.on === false) {
    canvas.style.removeProperty("opacity");
  }
  freezeFrameCan.remove();
  freezeFrame = false;
  // ...
}

if (glaze.on) {
  Glaze.unfreeze();  // This just removes opacity property
} else {
  canvas.style.removeProperty("opacity");
}
```
The freeze frame is removed **before** we can guarantee that:
1. The glaze canvas has finished its shader reload
2. The glaze canvas has been properly positioned/sized
3. The main canvas has been hidden (opacity: 0)

#### Issue 4: Missing synchronization between canvas opacity states
When glaze is on:
- Main canvas should have `opacity: 0`
- Glaze canvas should have `opacity: 1` (no opacity set)

But there's no atomic transition between these states.

## Proposed Solutions

### Solution A: Deferred Freeze Frame Removal (Low Risk)
Wait for glaze to be fully ready before removing freeze frame.

**Changes in bios.mjs:**
```javascript
// Add a flag for glaze readiness
let glazeReady = false;

// In Glaze.on() callback:
currentGlaze = Glaze.on(
  canvas.width,
  canvas.height,
  canvasRect,
  projectedWidth,
  projectedHeight,
  wrapper,
  glaze.type,
  () => {
    glazeReady = true;  // Mark glaze as ready
    send({ type: "needs-paint" });
  },
);

// In paint loop, before removing freeze frame:
if (freezeFrame && freezeFrameFrozen && (!glaze.on || glazeReady)) {
  // ... remove freeze frame
  glazeReady = false;  // Reset for next resize
}
```

### Solution B: Eliminate CSS Transition During Resize (Medium Risk)
Remove the `first-glaze` class during resize operations.

**Changes:**
1. In glaze.mjs `frame()`: Don't add `first-glaze` class if dimensions are changing
2. Or: Add `transition: none` inline style during resize, restore after

### Solution C: Atomic Opacity Swap (Higher Risk, Most Robust)
Ensure main canvas opacity and glaze canvas opacity change atomically.

**Changes in bios.mjs:**
```javascript
// Before showing glaze canvas
requestAnimationFrame(() => {
  canvas.style.opacity = 0;  // Hide main canvas
  Glaze.unfreeze();           // Show glaze canvas
  freezeFrameCan.remove();    // Remove freeze frame
});
```

### Solution D: Use Visibility Instead of Opacity
Replace opacity manipulation with visibility for instant switching.

**Changes:**
- Replace `canvas.style.opacity = 0` with `canvas.style.visibility = "hidden"`
- Replace `removeProperty("opacity")` with `style.visibility = "visible"`

## Recommended Implementation Order

1. **Phase 1** (Quick Win): Implement Solution A - deferred freeze frame removal
2. **Phase 2** (If still issues): Add Solution B - disable transition during resize  
3. **Phase 3** (If needed): Implement Solution C - atomic opacity swap

## Testing Checklist

- [ ] Resize window slowly in dark mode (prompt.mjs)
- [ ] Resize window rapidly in dark mode
- [ ] Resize while content is actively animating
- [ ] Test with different screen densities (devicePixelRatio)
- [ ] Test switching between light/dark mode during resize
- [ ] Test on macOS, Windows, Linux
- [ ] Test in VS Code webview (ac-electron)

## Files to Modify

1. [bios.mjs](../system/public/aesthetic.computer/bios.mjs) - Main resize/glaze coordination
2. [glaze.mjs](../system/public/aesthetic.computer/lib/glaze.mjs) - Shader loading and canvas management
3. [style.css](../system/public/aesthetic.computer/style.css) - CSS transitions (if Solution B)

## Performance Considerations

- Avoid adding more async operations in the paint loop
- The current 80ms `REFRAME_DELAY` is already a debounce - don't reduce it
- Consider using `will-change: opacity` CSS hint on glaze canvas
- Monitor for memory leaks from repeated shader program creation

## References

- bios.mjs freeze frame logic: lines 1016-1071
- bios.mjs glaze.on() call: lines 1538-1557  
- bios.mjs freeze frame removal: lines 15938-15951
- glaze.mjs on() function: lines 326-361
- glaze.mjs frame() function: lines 120-200
- glaze.mjs freeze/unfreeze: lines 392-405
