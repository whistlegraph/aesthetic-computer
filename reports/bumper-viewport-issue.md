# Bumper Viewport Issue Report

**Date:** 2026-02-10
**Status:** ⚠️ UNRESOLVED - Needs architectural decision

## The Problem

The bumper overlay system needs to create a proper viewport abstraction where:
- The bumper renders at the very top (y=0-18)
- Piece content renders BELOW the bumper (starting at y=18)
- Piece coordinate system should map: piece y=0 → physical y=18

## What We've Tried

### Attempt 1: Reduce height + Pan offset
```javascript
screen.height = physical - bumperHeight  // 394px
pan(0, bumperHeight)                      // shift down 18px
```
**Result:** ❌ Content cut off at bottom
- Piece renders y=0-394 in piece coords
- Pan shifts to y=18-412 in physical coords
- y=412 is out of bounds (screen is 0-411)

### Attempt 2: Reduce height only (current)
```javascript
screen.height = physical - bumperHeight  // 394px
// No pan offset
```
**Result:** ❌ Bumper overlays on top of content
- Piece renders y=0-394 in physical coords
- Bumper overlays at y=0-18
- Top 18px of piece content is covered

## The Core Issue

We need the piece to:
1. Think it has 394px of height (reduced)
2. Render starting at physical y=18 (not y=0)
3. Fill the viewport from y=18 to y=412

**Current approaches don't achieve both 1 and 2 simultaneously.**

## Possible Solutions

### Option A: Pixel Buffer Offset
- Give piece a pixel buffer that starts at physical y=18
- More complex, requires buffer management

### Option B: Global Translation Without Pan
- Use a different mechanism than pan() that doesn't add to piece coordinates
- Modify the rendering pipeline to offset the framebuffer

### Option C: Piece Responsibility
- Don't reduce screen.height
- Give piece full 412px height
- Piece checks `screen.bumperOffset` and manually adds 18px to all Y coordinates
- Problem: Requires every piece to handle this

### Option D: CSS/Canvas Transform
- Use CSS transform or canvas context transform to shift rendered output
- Piece renders to full screen, output gets shifted visually

## Debug Data

From console logs:
```
Physical screen.height: 412
Bumper offset: 18
Piece screen.height: 394
```

With pan removed:
- Piece renders: y=0 to y=394 (physical coords)
- Bumper overlays: y=0 to y=18
- Problem: Overlap at y=0-18

## Current Code State

**Files Modified:**
- `system/public/aesthetic.computer/lib/disk.mjs`
  - Lines 11668-11682: Screen height reduction
  - Lines 11818-11824: Event coordinate adjustment
  - Lines 12425-12437: Pan removed from boot
  - Lines 12618-12625: Pan removed from paint

**Event Coordinates:** ✅ Fixed (adjusted by bumperOffset)
**Screen Height:** ⚠️ Reduced but causes overlap
**Pan Offset:** ❌ Removed (caused bottom cutoff)

## Next Steps

1. Review architectural options A-D above
2. Decide on proper abstraction for bumper viewport
3. Implement chosen solution
4. Test with notepat piece
5. Ensure works for all pieces that enable bumper

## Related Code

- Bumper enable: `notepat.mjs:1176` - `api.bumper.enable(BUMPER_HEIGHT, renderBumper)`
- Bumper config: `disk.mjs:2071-2089` - `bumperConfig` object
- Bumper overlay: `disk.mjs:14849-14881` - Bumper overlay rendering
- Piano boot animation: `system/netlify/functions/index.mjs:1186-1242`

## Test Case

Navigate to `/notepat` and check:
- [ ] Bumper ticker visible at top
- [ ] Piano keys/top bar NOT overlapped by bumper
- [ ] Pads visible and NOT cut off at bottom
- [ ] Touch/mouse events work correctly
- [ ] Screen dimensions logged correctly

---

**Commits Related to This Issue:**
- `df357560` - Initial bumper viewport with pan
- `2d988910` - Enable notepat boot animation
- `62a7e821` - Add debug logging
- `28c4cb82` - Remove pan, adjust event coords (current state)
