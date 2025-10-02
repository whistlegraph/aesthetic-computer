# Bake Layer Persistence Fix

**Date**: October 1, 2025  
**Issue**: The `bake` command doesn't persist across frames like embedded layers do  
**File**: `system/public/aesthetic.computer/lib/kidlisp.mjs`

## Problem Description

When using the `bake` command in KidLisp (e.g., in `$fezw`), the baked layers don't work the same way as kidlisp embedded pasting. After `bake` runs, subsequent drawing commands should appear on a transparent layer over the baked content, and that state should persist across frames.

### Example Code (from $fezw)
```lisp
1 fade:red-green-blue-green-red
2 ink zebra 32, bunch 8 point
3 scroll -1
4 (1s (zoom 0.5))
5 bake
6 ink white
7 line 0 0 w h
```

**Expected Behavior**:
- Lines 1-4 create animated content
- Line 5 (`bake`) captures this content into a persistent background layer
- Lines 6-7 draw on a transparent layer OVER the baked content
- The baked layer persists across frames
- New drawing commands continue to appear on top of the baked layer

**Current Behavior**:
- The baked layer is cleared every frame
- Drawing after `bake` doesn't appear layered over the baked content
- The effect doesn't persist like embedded layers do

## Root Cause Analysis

### How `bake` Currently Works

1. **Bake Function** (lines 5440-5499):
   - Captures current screen buffer into `this.bakedLayers[]`
   - Uses `this.onceExecuted.add("bake_call")` to prevent repeated calls
   - Optionally clears the screen buffer after baking
   - Stores pixel data: `new Uint8ClampedArray(api.screen.pixels)`

2. **Rendering Baked Layers** (lines 8612-8664):
   - `renderBakedLayers()` composites baked layers underneath current content
   - Uses alpha blending: only shows baked pixels where current pixels are transparent
   - Called during frame execution (line 2645)

3. **THE PROBLEM** (lines 1419-1421 in `reset()` method):
   ```javascript
   // Reset baked layers state
   this.bakedLayers = [];
   this.bakeCallCount = 0;
   ```
   **The baked layers are cleared every frame in `reset()`!**

### How `embed` Layers Work (for comparison)

1. **Embedded Layer Storage**:
   - Stored in `this.embeddedLayers[]` and `this.embeddedLayerCache` (lines 737-738)
   - **NOT cleared in `reset()` method** - they persist across frames
   - Only cleared when explicitly requested (line 1376 comment)

2. **Rendering Embedded Layers** (lines 9415+):
   - `renderEmbeddedLayers()` re-evaluates and re-renders each frame
   - Layers persist and are re-rendered with updated frame state
   - Complex optimization logic for when to re-evaluate vs just re-paste

3. **Lifecycle Management**:
   - Cleared in `clearEmbeddedLayerCache()` method
   - Explicitly controlled, not automatically reset

## Solution Design

### Primary Fix: Don't Clear Baked Layers in `reset()`

The `reset()` method should NOT clear `this.bakedLayers`. This array should persist across frames like embedded layers do.

**Rationale**:
- The `onceExecuted` set already prevents re-baking on each frame
- The baking operation is meant to be a one-time capture that persists
- Similar to how embedded layers work - they persist until explicitly cleared
- The rendering pipeline already handles compositing baked layers correctly

### Changes Required

#### 1. Remove Baked Layer Clearing from `reset()` (lines 1419-1421)

```javascript
// BEFORE:
// Reset baked layers state
this.bakedLayers = [];
this.bakeCallCount = 0;

// AFTER:
// Don't reset baked layers - they should persist across frames like embedded layers
// Baked layers are only cleared when source code changes or explicitly requested
// this.bakedLayers = []; // REMOVED
// this.bakeCallCount = 0; // REMOVED (or keep for tracking but don't clear layers)
```

#### 2. Clear Baked Layers When Source Changes

Baked layers should be cleared when:
- Source code changes (when `clearOnceExecuted` is true in `reset()`)
- Module changes (similar to embedded layers)
- Explicitly requested via a clear/reset command

Add to `reset()` method:
```javascript
// Clear onceExecuted only when explicitly requested (when source changes)
if (clearOnceExecuted) {
  this.onceExecuted.clear();
  // Also clear baked layers when source changes
  this.bakedLayers = [];
  this.bakeCallCount = 0;
}
```

#### 3. Optional: Add Method to Explicitly Clear Baked Layers

Add a convenience method for clearing baked layers:
```javascript
clearBakedLayers() {
  this.bakedLayers = [];
  this.bakeCallCount = 0;
  this.onceExecuted.delete("bake_call");
  console.log("🍞 Cleared all baked layers");
}
```

This could be called from `module()` or other lifecycle methods as needed.

## Implementation Steps

1. **Modify `reset()` method** (around line 1419):
   - Remove unconditional clearing of `this.bakedLayers`
   - Move clearing into the `if (clearOnceExecuted)` block

2. **Test the fix**:
   - Run the example code from `$fezw`
   - Verify baked content persists across frames
   - Verify new drawing appears on top of baked content
   - Verify baking only happens once

3. **Edge case handling**:
   - Ensure baked layers are cleared when source changes
   - Verify screen resize doesn't break baked layer compositing
   - Check memory usage with multiple bake calls

## Expected Behavior After Fix

Using the example code:
```lisp
fade:red-green-blue-green-red
ink zebra 32, bunch 8 point
scroll -1
(1s (zoom 0.5))
bake
ink white
line 0 0 w h
```

1. **First frame execution**:
   - Lines 1-4 create animated zebra pattern with fade and scroll
   - Line 5 captures this into `bakedLayers[0]`
   - Line 5 clears the screen buffer (optional, based on args)
   - Lines 6-7 draw white line on fresh transparent buffer

2. **Subsequent frames**:
   - `onceExecuted` prevents re-execution of `bake`
   - `renderBakedLayers()` composites the baked zebra pattern
   - Lines 1-4 continue to animate (scroll, fade, zoom)
   - Lines 6-7 draw white line on top
   - Result: animated background with static white line overlay

**Key difference**: The baked layer persists and is re-composited each frame, rather than being cleared and lost.

## Alternative Approaches Considered

### 1. Make `bake` work like `embed` with re-evaluation
- More complex, would require creating a KidLisp instance per baked layer
- Overkill for the use case - `bake` is meant to capture static state
- Would change the semantics of `bake` significantly

### 2. Add a `persist` flag to `bake`
- `(bake)` - clear layers each frame (current behavior)
- `(bake persist)` - keep layers across frames (new behavior)
- More flexible but adds API complexity
- Not needed - persistence should be the default behavior

### 3. Use a separate command like `freeze` or `snapshot`
- Keep `bake` as-is, add new command for persistent layers
- Redundant - `bake` should work correctly rather than adding another command
- Increases API surface without clear benefit

## References

- **Bake function definition**: lines 5438-5499
- **Bake rendering**: lines 8612-8664 (`renderBakedLayers()`, `compositeBakedLayer()`)
- **Bake call in frame pipeline**: line 2645
- **Reset method**: lines 1400-1430
- **Embedded layers for comparison**: lines 737-738, 9415+ (`renderEmbeddedLayers()`)

## Success Criteria

✅ Baked layers persist across frames  
✅ Subsequent drawing commands appear on transparent layer over baked content  
✅ Baking only happens once per program execution (via `onceExecuted`)  
✅ Baked layers are cleared when source code changes  
✅ No memory leaks or performance regressions  
✅ Example code from `$fezw` works as expected  

## Implementation Status

**COMPLETED** - October 1, 2025

### Phase 1: Layer Persistence (Completed)

1. **Modified `reset()` method** (lines ~1406-1425):
   - Moved `this.bakedLayers = []` and `this.bakeCallCount = 0` inside the `if (clearOnceExecuted)` block
   - Added comment explaining that baked layers persist across frames like embedded layers
   - Baked layers now only clear when source code changes

2. **Added `clearBakedLayers()` method** (after line 8968):
   ```javascript
   clearBakedLayers() {
     this.bakedLayers = [];
     this.bakeCallCount = 0;
     this.onceExecuted.delete("bake_call");
     console.log("🍞 Cleared all baked layers");
   }
   ```

3. **Called `clearBakedLayers()` in constructor** (line ~772):
   - Added call after `clearEmbeddedLayerCache()` during initialization

4. **Called `clearBakedLayers()` in `module()` method** (line ~2382):
   - Added call after `clearEmbeddedLayerCache()` when loading new modules
   - Ensures fresh state when entering/re-entering a piece

### Phase 2: Drawing Suppression (Completed)

The persistence fix alone wasn't enough - code before `bake` was still executing and drawing every frame. Added a suppression mechanism to prevent drawing operations before the bake point on subsequent frames:

1. **Added flags to constructor** (lines ~897-900):
   ```javascript
   this.hasBakedContent = false; // Track if bake has been called
   this.suppressDrawingBeforeBake = false; // Suppress drawing before bake point
   ```

2. **Modified `bake()` function** (lines ~5448-5510):
   - Sets `this.hasBakedContent = true` when baking
   - On subsequent frames, sets `this.suppressDrawingBeforeBake = false` when reached
   - **CRITICAL FIX**: On subsequent frames, clears the screen buffer (`pixels.fill(0)`) before returning
   - This ensures post-bake drawing starts with a clean transparent buffer
   - Without this, post-bake content would composite with previous frame's content
   - This creates a "bake point" that divides the code

3. **Updated `reset()` method** (lines ~1414-1424):
   - If `hasBakedContent` is true, sets `suppressDrawingBeforeBake = true` at frame start
   - When evaluation reaches `bake`, flag is reset to `false`
   - This suppresses drawing before bake, allows drawing after bake

4. **Added suppression checks to drawing AND effect functions**:
   - **Drawing functions**: `line`, `box`, `circle`, `wipe`, `paste`, `stamp`, `write`, `backdrop`, `flood`
   - **Effect functions**: `scroll`, `zoom`, `suck`, `blur`, `contrast`
   - Each function now returns early if `this.suppressDrawingBeforeBake` is true
   - **Critical**: Effects like `scroll` and `zoom` transform the entire screen buffer, so they must also be suppressed to prevent affecting content drawn after `bake`

5. **Updated `clearBakedLayers()`** (lines ~8988-8998):
   - Resets both `hasBakedContent` and `suppressDrawingBeforeBake` flags

6. **Fixed alpha compositing for baked layers** (lines ~8674-8720):
   - Changed from simple transparent pixel replacement to proper alpha blending
   - Uses formula: `result = current * alpha_current + baked * alpha_baked * (1 - alpha_current)`
   - Ensures baked layer appears as background with current content properly composited on top

### How It Works

**First Frame (when bake is called)**:
1. Code before `bake` executes normally and draws
2. `bake` captures the screen buffer
3. `hasBakedContent` is set to `true`
4. Screen is optionally cleared
5. Code after `bake` executes and draws

**Subsequent Frames**:
1. `reset()` sees `hasBakedContent = true`, sets `suppressDrawingBeforeBake = true`
2. Code before `bake` executes but drawing functions return early (suppressed)
3. When `bake` is reached, sets `suppressDrawingBeforeBake = false`
4. Code after `bake` executes and draws normally
5. `renderBakedLayers()` composites the baked layer underneath current content

**Result**: Content before `bake` is frozen in the baked layer, content after `bake` animates on top.

### Testing Notes

Test with the example code from `$fezw`:
```lisp
fade:red-green-blue-green-red
ink zebra 32, bunch 8 point
scroll -1
(1s (zoom 0.5))
bake
ink white
line 0 0 w h
```

Expected behavior:
- Animated zebra pattern with fade and scroll runs once, gets baked (lines 1-4)
- White line draws on top of baked pattern (lines 6-7)
- Baked pattern persists across all frames (static background)
- White line continues to draw on transparent layer on each frame
- Effect is similar to two embedded KidLisp pieces layered on top of each other

