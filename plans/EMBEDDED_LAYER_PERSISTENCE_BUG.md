# Embedded Layer Persistence Bug Analysis

## The Problem

Embedded KidLisp layers (`$39i`) appear as a static color instead of showing their animated grayscale artwork, even though alpha blending is working correctly.

## What the Logs Reveal

Looking at console output with `wipe "brown"` as background:

### Frame 2:
```
SRC FIRST PIXEL: [32,32,32,255]          (dark gray from embedded layer)
DST BEFORE BLEND: [165,42,42,255]        (brown background)
DST AFTER BLEND: [97,36,36,255]          (correctly blended: darker brownish)
SCREEN AFTER COMPOSITE: [97,36,36,255]   ✅ Blend succeeded!
```

### Frame 5:
```
SRC FIRST PIXEL: [116,116,116,255]       (light gray from embedded layer)
DST BEFORE BLEND: [165,42,42,255]        🚨 BROWN AGAIN! Should be [97,36,36,255]
DST AFTER BLEND: [140,79,79,255]         (correctly blended: lighter brownish)
SCREEN AFTER COMPOSITE: [140,79,79,255]  ✅ Blend succeeded!
```

### Frame 8:
```
SRC FIRST PIXEL: [98,98,98,255]
DST BEFORE BLEND: [165,42,42,255]        🚨 BROWN AGAIN! Should be [140,79,79,255]
DST AFTER BLEND: [49,49,49,255]
SCREEN AFTER COMPOSITE: [49,49,49,255]
```

## The Root Cause

**The screen buffer is being reset to the background color BETWEEN FRAMES**, erasing the previous frame's blended pixels.

### Expected Behavior:
1. Frame N: Blend gray onto brown → get brownish-gray
2. Frame N+1: Blend new gray onto **brownish-gray from frame N**
3. Result: Accumulated blending creates visible animation

### Actual Behavior:
1. Frame N: Blend gray onto brown → get brownish-gray ✅
2. **🔥 Screen wiped back to brown**
3. Frame N+1: Blend new gray onto **fresh brown** (not previous result)
4. Result: Each frame blends onto the same brown, no accumulation visible

## Why This Happens

The screen is wiped at the **start of `paint()`** every frame (line ~3228 in `kidlisp.mjs`):

```javascript
if (this.firstLineColor) {
  // Fill with first-line color as background
  $.wipe(this.firstLineColor);  // ← Wipes screen to brown EVERY FRAME
} else {
  // Clear to transparent or black
  screen.pixels.fill(0);
}
```

This is correct for **regular KidLisp pieces** where you redraw everything each frame. But for **embedded layers that accumulate**, this wipes out the previous frame's work.

## Why the Blending Math is Correct

The alpha blending formula works perfectly:
- Source: `[116,116,116,255]` at 50% alpha
- Dest: `[165,42,42,255]` (brown)
- Result: `[140,79,79,255]` = `(116*.5 + 165*.5, 116*.5 + 42*.5, 116*.5 + 42*.5)`

**The problem is NOT the blending** - it's that we're always blending onto a fresh brown background instead of the accumulated result.

## The Solution

Embedded layers need to render into a **persistent buffer** that is NOT wiped each frame. The composite should:

1. **Once at start:** Wipe screen to background color
2. **Each frame:** 
   - Evaluate embedded layer (updates its own buffer)
   - Blend embedded layer buffer onto screen (accumulates)
   - **DON'T wipe screen** - let pixels accumulate

OR

The embedded layer buffers should accumulate their own drawing (like `layer0` does), and each frame should paste the **accumulated embedded buffer** onto a fresh background.

## Current Architecture Issue

The current flow:
```
paint() {
  1. Wipe screen to brown          ← Destroys previous frame
  2. Evaluate main code
  3. Evaluate embedded layers       ← Renders to embedded buffer
  4. Composite: paste embedded layer → screen
}
```

The embedded layer's **buffer** persists (it's like `layer0`), but the **screen** is wiped fresh each frame. Since we're pasting the embedded buffer (which has new content) onto a fresh brown background each time, we lose the accumulation effect.

## Key Insight

Looking at frame 2 vs frame 5:
- Frame 2: Embedd layer first pixel = `[32,32,32,255]`
- Frame 5: Embedded layer first pixel = `[116,116,116,255]`

**The embedded layer buffer IS changing** (animating). But because we paste it onto a fresh brown background each frame, the user only sees a brown square with a slight tint that changes - not the animated gray artwork.

The embedded layer is rendering correctly, blending correctly, but the accumulation is lost because the destination is reset every frame.

## Fix Strategy

Need to ensure that when embedded layers are present:
1. Screen is only wiped **once** (not every frame), OR
2. Embedded layers accumulate in their own buffers, and we paste the accumulated buffer onto fresh background each frame (current behavior), which means the embedded layer itself needs to NOT wipe its buffer between frames.

The real issue: The embedded layer's buffer at line ~11965 might be getting wiped when it shouldn't be.
