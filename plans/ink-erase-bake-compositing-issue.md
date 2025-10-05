# Ink Erase + Bake Compositing Issue

## Problem Summary
Command `red, ink erase, line, bake` should create accumulating erase lines (at random coordinates) on a red background that persist across frames. Currently:
- ✅ Commands execute in correct order (logs confirm)
- ✅ Erase color `[-1, -1, -1, -1]` is set properly
- ✅ Lines are drawn to baked buffer with erase ink active
- ❌ **Visual output does NOT show erased pixels** - red background persists unchanged
- ❌ Erase lines should keep drawing/accumulating over the red surface each frame

NOTE!!!!
- The issue is itht ehe use of both 'erase' and 'bake' with no 'bake' it works perfectly!

## Root Cause Analysis

### Issue 1: Compositing Functions Don't Handle Erase Marker ✅ FIXED
**File**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/kidlisp.mjs`

**Lines**: 9338-9390 (`compositeBakedLayer`) and 9406-9460 (`compositePostBakeLayer`)

**Problem**: `graph.erase()` zeroes RGBA directly, so erased pixels land in the baked buffer as `[0, 0, 0, 0]`. Our earlier “255 sentinel” guess meant compositing treated those pixels as ordinary transparent foreground instead of actively clearing the destination.

**Fix Applied**: Detect true zero-alpha pixels (while still supporting legacy sentinel frames) and clear the destination immediately:
```javascript
const isAlphaClear = fgAValue === 0 && fgR === 0 && fgG === 0 && fgB === 0;
const isLegacyEraseMarker = fgR === 255 && fgG === 255 && fgB === 255 && fgAValue === 255;

if (isAlphaClear || isLegacyEraseMarker) {
  currentPixels[currentIndex] = 0;
  currentPixels[currentIndex + 1] = 0;
  currentPixels[currentIndex + 2] = 0;
  currentPixels[currentIndex + 3] = 0;
  continue;
}
```

### Issue 3: First-Line Color Red Background vs Baked Content
**File**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/kidlisp.mjs`

**Lines**: ~6927 and ~7000 (first-line color wipe logic)

**Problem**: The first-line color shorthand `red` creates a wipe that might be:
1. Re-executing every frame and overwriting the baked buffer with solid red
2. Or the baked buffer's red background is never being "erased into" because it's a fresh wipe each frame

**Logs show**: `🎨 FIRST-LINE COLOR: Applying wipe with red` happens before body evaluation

**Theory**: If the red wipe is re-applied to the baked buffer each frame BEFORE the erase lines are drawn, then the erase lines are drawing into a fresh red surface, but then the compositing back to main screen isn't showing the erased pixels.

## Debugging Data from Logs

```
📏 LINE | args=[] | inkState=["erase"] | hasBakedContent=true | suppressDrawingBeforeBake=true
```

Key observations:
1. `args=[]` - **LINE HAS NO COORDINATES** ⚠️⚠️⚠️
2. `inkState=["erase"]` - Ink state is correct ✅
3. `hasBakedContent=true` - Bake has executed ✅
4. `suppressDrawingBeforeBake=true` - Drawing suppression still active (might be wrong?) ⚠️

## Potential Fixes (Priority Order)

### Priority 1: Fix Empty Line Args 🔴 CRITICAL
**Location**: Lines 4013-4053 in kidlisp.mjs

**Investigation needed**:
1. Check how `line` command receives args in the `line: (api, args = []) => { ... }` function
2. Trace back to where commands are called from `KidLisp.evaluate` at line 7703
3. Verify the AST/command storage is preserving arguments between frames
4. Check if `runWithBakedBuffer` (line ~9195) is stripping arguments when routing

**Potential fix**: Ensure the command invocation preserves the arguments array when routing to baked buffer context.

### Priority 2: Verify Baked Buffer Persistence
**Location**: Lines 5825-5900 (bake command) and 9308-9337 (renderBakedLayers)

**Issue**: The baked buffer should:
1. Capture the red background on first frame ✅
2. Receive erase lines each subsequent frame ✅
3. Preserve previous erase lines (accumulation) ❓

**Check**: Is the baked buffer being cleared/reset each frame, or does it truly persist?

**Line 5883**: `pixels: new Uint8ClampedArray(api.screen.pixels)` - Initial bake clones screen

**Line 9332-9333**: Compositing updated baked layer back to main screen

**Verify**: The baked buffer should NOT be re-initialized on subsequent frames, only drawn to.

### Priority 3: First-Line Color Interaction
**Location**: Lines 6927, 7000, 5893-5899

**Issue**: First-line color `red` might be:
1. Wiping the baked buffer clean with red each frame (BAD)
2. Only wiping main screen before bake captures (GOOD)

**Line 6927**: 
```javascript
if (!suppressDrawingBeforeBake || !hasBakedContent) {
  // Apply wipe with first-line color
}
```

**Current state**: After bake, `suppressDrawingBeforeBake=true` and `hasBakedContent=true`, so wipe should NOT execute. ✅

**But check**: Is the first-line color being applied to the baked buffer somehow?

### Priority 4: Compositing Layer Order
**Location**: Lines 2825-2870 (makeFrame rendering pipeline)

**Issue**: Verify the rendering order:
1. Clear main screen
2. Composite baked layers to main screen (line 2853: `this.renderBakedLayers($)`)
3. Execute new drawing commands
4. Composite post-bake overlay (line 2869)

**Check**: Is the red wipe happening AFTER the baked layers are composited? If so, it would overwrite the erased pixels.

## Test Commands

### Test 1: Verify without first-line color
```
ink red, wipe, bake, ink erase, line 0 0 100 100
```
This separates the red wipe from first-line color shorthand.

### Test 2: Verify with explicit coordinates
```
red, bake, ink erase, line 10 10 100 100
```
Use different coordinates to see if it's a parsing issue.

### Test 3: Verify line args are preserved
Add debug logging right before line draws to baked buffer to inspect args.

## Expected Behavior
1. Frame 1: Red wipe → Bake captures red screen
2. Add logging to see what args are passed to `line` function vs what it receives
3. Verify baked buffer pixel values contain `[255, 255, 255, 255]` where erase lines should be

## Graphics Pipeline Research (2025-10-03)

- **Worker orchestration (`system/public/aesthetic.computer/lib/disk.mjs`)**
  - `makeFrame()` runs inside the dedicated rendering worker. It dispatches incoming messages from the main thread (boot, sim, act, paint) and hands rendering over to the active KidLisp piece via `painting.paint()`.
  - The `Painting` wrapper queues `$paintApi` calls into ordered layers and flushes them per frame, while exposing helpers like `page()`, `paste()`, and buffer factory utilities shared with KidLisp.
- **KidLisp frame loop (`system/public/aesthetic.computer/lib/kidlisp.mjs`)**
  - `KidLisp.paint($)` is the heartbeat of a piece. It restores ink state, clears frame caches, and calls `beginBakedFrameRouting($)` so initial drawing happens inside the baked buffer when one exists.
  - `evaluate(this.ast, $)` executes the program, honoring routing flags:
    - `bake` copies the current screen into `bakedLayers[0]`, initializes `postBakeLayer`, and flips `suppressDrawingBeforeBake` to ensure pre-bake commands only run inside the baked buffer on later frames.
    - `embed` spawns nested KidLisp instances with their own buffers; they reuse cached timing/random state and later paste with optional alpha.
    - Drawing helpers call `executePreBakeDraw()` so first-line wipes/backdrops hit the baked buffer when appropriate.
  - After evaluation the pipeline composites in layers: `renderBakedLayers()` (background), `compositePostBakeLayer()` (persistent overlay), `renderEmbeddedLayers()` (nested pieces), `postEmbedCommands`, then HUD/perf overlays.
- **Buffer hand-off back to main thread**
  - `painting.paint()` flushes deferred `$paintApi` invocations, leaving final pixels in `screen.pixels`. `makeFrame()` posts the frame back through the worker boundary to the display.

### ASCII pipeline overview

```
Main Thread (bios)                         Worker (disk.mjs)
┌──────────────────────┐   postMessage    ┌──────────────────────────────────┐
│ UI + display loop    ├────────────────►│ makeFrame() routes events         │
└─────────┬────────────┘                 │  ├─ boot / sim / act handlers     │
          │                              │  └─ 'paint' → painting.paint()    │
          │                              └─────────┬────────────────────────┘
          │                                        │ KidLisp instance
          ▼                                        ▼
                                      ┌──────────────────────────────────────┐
                                      │ KidLisp.paint($) per frame           │
                                      │ 1. Restore state & caches            │
                                      │ 2. beginBakedFrameRouting()          │
                                      │ 3. evaluate(AST):                    │
                                      │    ├─ pre-bake draws                 │
                                      │    ├─ bake → bakedLayers/postBake    │
                                      │    └─ embed → nested buffers         │
                                      │ 4. renderBakedLayers()               │
                                      │ 5. compositePostBakeLayer()          │
                                      └─────────┬────────────────────────────┘
                                                │ final screen.pixels
                                                ▼
                                    ┌───────────────────────────────┐
                                    │ Painting.flush → graph engine │
                                    └─────────┬─────────────────────┘
                                              │ frame back to main thread
                                              ▼
                                          Display
```

### Erase trajectory inside the bake stack (KidLisp focus)

1. **Frame setup**
  - `KidLisp.reset()` sees `hasBakedContent` and flips `suppressDrawingBeforeBake = true` so pre-bake draw calls won’t stamp over the baked buffer on subsequent frames.
  - `KidLisp.paint($)` calls `beginBakedFrameRouting($)`, swapping `$.page()` to the baked buffer for the duration of evaluation while remembering the original screen.

2. **`(ink erase)` command**
  - `KidLisp.ink()` normalises the arguments, stores `this.inkState = ["erase"]`, and issues `api.ink("erase")`.
  - When routed through the immediate path (e.g., during `runWithBakedBuffer`), `$paintApiUnwrapped.ink("erase")` funnels into `graph.ink() → findColor()`, which maps the erase token to the sentinel colour `[-1, -1, -1, α]` and sets the blend mode to erase.

3. **`(line …)` before/after bake**
  - **First frame (before bake):** `hasBakedContent` is still `false`, so the queued `api.line` runs against the live screen; when `bake` executes later in the same frame it snapshots those pixels into `bakedLayers[0]`.
  - **Subsequent frames:** `KidLisp.line()` sees `hasBakedContent && suppressDrawingBeforeBake` and calls `runWithBakedBuffer()`.
    - That helper temporarily `page()`s into the baked buffer, replays the stored ink via `$paintApiUnwrapped.ink(...this.inkState)` to restore the erase blend, then executes `$paintApiUnwrapped.line(...args)` so the stroke lands directly in `bakedLayer.buffer`.

4. **Pixel storage semantics**
  - Inside `graph.line()` the erase fast-path zeroes all four channels in-place, so the baked buffer ends up with `[0, 0, 0, 0]` wherever erase touched. There’s no sentinel stored in the pixels themselves—only the in-flight ink state carries `[-1, -1, -1, α]`.

5. **Compositing back to the frame**
  - After evaluation, `renderBakedLayers()` iterates baked buffers and `compositeBakedLayer()` now clears the destination whenever it sees a zero-alpha pixel (while still tolerating the legacy `[255,255,255,255]` sentinel).
  - The screen then receives post-bake overlays, embedded layers, and HUD elements before `painting.paint(true)` flushes everything through the `graph` engine and the worker returns the finished frame.

```
(ink "erase")
  │ KidLisp.ink() → this.inkState = ["erase"]
  │    └─ painting.api.ink("erase") → $paintApiUnwrapped.ink("erase")
  │         └─ graph.findColor() sets c = [-1,-1,-1,α], blendMode = erase
  │
(line x0 y0 x1 y1)
  │ KidLisp.line() (hasBakedContent && suppressDrawingBeforeBake)
  │    └─ runWithBakedBuffer()
  │         ├─ $paintApiUnwrapped.ink(...this.inkState)  // reapply erase
  │         └─ $paintApiUnwrapped.line(...args)
  │              └─ graph.line() → erase() zeros bakedLayer.buffer pixels
  │
(render pass)
  │ KidLisp.renderBakedLayers()
  │    └─ compositeBakedLayer(): if fg == [255,255,255,255] → clear target
  │
Final Frame → screen pixels now contain the erased stroke before overlays
```
