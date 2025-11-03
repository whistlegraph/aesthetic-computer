# BDF Font Rotation Issue

## Problem Statement
MatrixChunky8 (BDF pixel-based font) does not rotate the same way as vector fonts. When rotating text, individual pixels within glyphs appear to be in the wrong positions, particularly noticeable in characters like "A" where the center dot gets displaced.

## Current Implementation

### Vector Fonts (Working)
Vector fonts use `commands` array with line/point instructions. Rotation is applied in `graph.mjs` `draw()` function:
- Converts angle to radians
- Applies sin/cos rotation matrix to each command's coordinates
- Works cleanly at all angles

### BDF Fonts (Broken)
BDF fonts use `pixels` 2D array. Current implementation in `graph.mjs` lines ~4030-4120:
```javascript
// Start with pixel center position
let pixelX = col + 0.5;
let pixelY = row + 0.5;

// Apply rotation around character origin
if (hasRotation) {
  const rotatedX = pixelX * cosA - pixelY * sinA;
  const rotatedY = pixelX * sinA + pixelY * cosA;
  pixelX = rotatedX;
  pixelY = rotatedY;
}

// Apply scale
pixelX *= scale;
pixelY *= scale;

// Apply position offset and floor for final pixel position
const finalX = floor(pixelX + x);
const finalY = floor(pixelY + y);

// Render pixel
point(finalX, finalY);
```

## Observed Issues

1. **Pixel Displacement**: Individual pixels within rotated glyphs appear in wrong positions
   - Example: "A" character's center hole dot moves incorrectly
   - More noticeable at 45°, 90°, 270° angles

2. **Inconsistent with Vector Fonts**: Same rotation angle produces different visual results between font types

3. **Possible Root Causes**:
   - Rounding/flooring at wrong stage?
   - Rotation origin incorrect (should be glyph center, not top-left)?
   - Scale being applied before/after rotation incorrectly?
   - Missing transformation that vector fonts get automatically?

## Test Case
View `aesthetic.computer/rotate-text-demo` to see:
- Top row: Vector font "PASTE" at 0°, 90°, 180°, 270°
- Bottom row: MatrixChunky8 "PASTE" at same angles
- Center: Animated "A" rotating in both fonts side-by-side

## Investigation Steps

1. **Compare Rotation Matrices**
   - Check if vector font rotation uses different math
   - Verify sin/cos calculations match

2. **Check Rotation Origin**
   - Vector fonts might rotate around glyph center
   - BDF fonts currently rotate around (0, 0) - is this wrong?

3. **Pixel Coordinate System**
   - Should we use pixel corners instead of centers?
   - Does +0.5 offset cause cumulative errors?

4. **Look at printLine()**
   - In `graph.mjs` line ~4198, how does it position each character?
   - Does it apply additional transforms we're missing?

5. **Check Draw Function for Vector Fonts**
   - Lines ~4140+ handle vector commands
   - What's different in their rotation application?

## Potential Solutions

### Option 1: Match Vector Font Logic
Study how vector fonts handle rotation in the `draw()` function after line 4140 and apply the same approach to BDF pixels.

### Option 2: Rotate Around Glyph Center
Instead of rotating around (0,0), calculate glyph center and rotate around that:
```javascript
const centerX = charWidth / 2;
const centerY = charHeight / 2;
let pixelX = col - centerX;
let pixelY = row - centerY;
// ... apply rotation ...
pixelX += centerX;
pixelY += centerY;
```

### Option 3: Pre-rotate Pixel Array
Instead of rotating each pixel during render, rotate the entire pixels array once before rendering (but this doesn't help with multi-character text).

### Option 4: Use Canvas Transform
Apply canvas transformation matrix before rendering BDF pixels (but this might conflict with pan system).

## Current Status
- Text rotation API added to `write()` function in `disk.mjs`
- Rotation parameter propagates through `type.mjs` `print()` to `graph.mjs` `draw()`
- Vector fonts rotate correctly
- BDF fonts (MatrixChunky8) have pixel displacement issues
- Test piece updated to compare both font types

## Next Steps
1. Run test piece and document exact pixel positions at 0°, 90°, 180°, 270°
2. Compare with expected positions from vector font
3. Add debug logging to see rotation calculations
4. Try Option 2 (center-based rotation) as first fix attempt
