# 3D Text Rendering Plan (`sign`)

## Goal
Add a `sign` API that creates line-based Forms from text, allowing text labels in 3D space (like player names above camera frustums).

**Naming Convention:**
- 2D text: `write()` - writing on a flat surface
- 3D text: `sign()` - placing a sign in physical space

---

## Current 2D Text System Overview

### Font Data Structure

**BDF Pixel Fonts (MatrixChunky8):**
```javascript
{
  resolution: [width, height],  // e.g., [4, 8] for a 4x8 character
  offset: [x, y],               // positioning offset
  baselineOffset: [x, y],       // baseline correction
  advance: number,              // spacing to next character
  pixels: [                     // 2D array of 1s and 0s
    [0, 1, 1, 0],
    [1, 0, 0, 1],
    // ... 8 rows for 8px tall font
  ]
}
```

**Vector Fonts (font_1):**
```javascript
{
  commands: [
    { name: "line", args: [x1, y1, x2, y2] },
    { name: "line", args: [x1, y1, x2, y2] },
    // ... line segments that draw the glyph
  ],
  resolution: [width, height],
  offset: [x, y]
}
```

### 2D Rendering Flow

1. **`ink(r,g,b).write(text, pos, ...)`** - Entry point in disk.mjs
2. **Typeface class** (`type.mjs`) - Manages font loading and glyph lookup
3. **Graph rendering** (`graph.mjs`) - Draws glyphs:
   - BDF fonts: Loop through `pixels` array, call `point()` for each `1`
   - Vector fonts: Execute `commands` array, draw lines

### Key Files
- `/system/public/aesthetic.computer/lib/type.mjs` - Typeface class, glyph loading
- `/system/public/aesthetic.computer/lib/graph.mjs` - `draw()` function renders glyphs
- `/system/public/aesthetic.computer/disks/common/fonts.mjs` - Font metadata

---

## 3D Text Implementation Plan

### Option A: Convert Pixel Glyphs to Line-Based Forms (Recommended)

For MatrixChunky8, convert each "on" pixel into a small line segment or cross pattern.

**Approach:**
```javascript
// For each pixel at (col, row), create a small cross or dot representation
// This creates a "dotted" text effect in 3D

function text3D(text, options = {}) {
  const { 
    font = "MatrixChunky8",
    scale = 0.1,        // World units per pixel
    spacing = 0.05,     // Extra space between chars
    color = [1,1,1,1],
  } = options;
  
  const positions = [];
  const colors = [];
  
  let cursorX = 0;
  
  for (const char of text) {
    const glyph = getGlyph(char, font);
    
    if (glyph.pixels) {
      // BDF font - convert pixels to 3D points/crosses
      for (let row = 0; row < glyph.pixels.length; row++) {
        for (let col = 0; col < glyph.pixels[row].length; col++) {
          if (glyph.pixels[row][col] === 1) {
            const x = cursorX + col * scale;
            const y = -row * scale;  // Y flipped (text reads top-down)
            const z = 0;
            
            // Create small cross at this point
            const s = scale * 0.4;  // Cross size
            // Horizontal line segment
            positions.push([x - s, y, z, 1], [x + s, y, z, 1]);
            colors.push(color, color);
            // Vertical line segment  
            positions.push([x, y - s, z, 1], [x, y + s, z, 1]);
            colors.push(color, color);
          }
        }
      }
    }
    
    cursorX += (glyph.advance || glyph.resolution?.[0] || 4) * scale + spacing;
  }
  
  return new Form(
    { type: "line", positions, colors },
    { pos: [0, 0, 0], scale: 1 }
  );
}
```

### Option B: Outline/Stroke Glyphs (More Complex)

Convert pixel boundaries to connected line segments forming character outlines.
- More visually appealing but significantly more complex
- Would need edge detection algorithm on pixel grid
- Better for large text

### Option C: Use Vector Font Data

If a vector font (like font_1) is available, extract the line commands directly:

```javascript
if (glyph.commands) {
  // Vector font - extract line segments directly
  for (const cmd of glyph.commands) {
    if (cmd.name === "line") {
      const [x1, y1, x2, y2] = cmd.args;
      positions.push(
        [cursorX + x1 * scale, -y1 * scale, 0, 1],
        [cursorX + x2 * scale, -y2 * scale, 0, 1]
      );
      colors.push(color, color);
    }
  }
}
```

---

## Proposed API

### In `graph.mjs` - Add `sign()` Form Generator

```javascript
// Create a 3D text Form that can be positioned/rotated in world space
const nameLabel = sign("Player1", {
  font: "MatrixChunky8",
  scale: 0.05,          // Size in world units
  color: [0, 1, 0, 1],  // RGBA
  align: "center",      // left, center, right
  style: "dots",        // dots, crosses, outline (future)
});

// Position it in 3D
nameLabel.position = [px, py + 0.5, pz];  // Above player
nameLabel.rotation = [0, yaw, 0];         // Face camera direction (billboard?)

// Render
ink(255, 255, 255).form(nameLabel);
```

### Alternative: Chained API via `ink()`

```javascript
// Direct rendering with ink chain
ink(0, 255, 0).sign("Player1", [px, py + 0.5, pz], { scale: 0.05 });
```

### Billboard Option (Always Face Camera)

For labels that should always face the viewer:

```javascript
const label = sign("Name", { billboard: true });
// In paint(), before rendering:
label.rotation = [0, -cameraYaw, 0];  // Counter-rotate to face camera
```

---

## Implementation Steps

### Phase 1: Basic `sign()` Function
1. Add `sign()` function to graph.mjs (exported for piece use)
2. Accept font name, get glyph data from Typeface
3. Convert pixel data to line positions
4. Return Form with line geometry

### Phase 2: Integration with `ink().sign()`
1. Add `sign()` to the ink chain in disk.mjs
2. Handle Form creation and rendering in one call
3. Support same parameters as 2D write

### Phase 3: Enhancements
1. Add billboard mode (auto-rotate to face camera)
2. Add outline rendering style
3. Add text measuring (get width before rendering)
4. Support multi-line text

---

## Usage Example in 1v1.mjs

```javascript
// In boot():
function boot({ Form, sign, glyphs, ... }) {
  globalSign = sign;
  globalGlyphs = glyphs;
}

// When player joins, create name sign:
playerBoxes[id] = {
  // ... other Forms ...
  nameSign: globalSign(content.handle || id.slice(0, 6), {
    scale: 0.03,
    color: playerColor,
    align: "center",
    glyphs: globalGlyphs?.("MatrixChunky8") || {},
  }),
};

// In paint():
const label = playerModel.nameSign;
if (label) {
  label.position = [px, py + 0.35, pz];  // Above camera box
  // Billboard: face toward viewer
  label.rotation = [0, (self.rot?.y || 0) + 180, 0];
  ink(255, 255, 255).form(label);
}
```

---

## Technical Notes

### Glyph Access
- Glyphs are loaded async via Typeface class
- For MatrixChunky8, glyphs load from BDF endpoint: `/api/bdf-glyph?char=X`
- Need to handle missing glyphs gracefully (use `?` fallback)

### Performance Considerations
- Cache generated text Forms (don't recreate every frame)
- Only recreate when text changes
- Consider LOD (level of detail) - simpler geometry at distance

### Coordinate System
- Text is generated in local space (centered or left-aligned at origin)
- Y is flipped (negative Y goes down in screen space)
- Z=0 is the text plane
