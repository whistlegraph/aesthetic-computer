# Zebra Color Sequence Implementation Plan

## Current Rainbow Implementation Analysis

### Overview
The `rainbow` color theme in Aesthetic Computer is a special dynamic color sequence that cycles through rainbow colors. It's implemented across multiple layers of the system with special handling in both the color system and KidLisp interpreter.

### Current Rainbow Implementation

#### 1. Core Rainbow Function (`num.mjs`)
- **Location**: `/system/public/aesthetic.computer/lib/num.mjs` lines 735-751
- **Implementation**: 
  ```javascript
  let currentRainbowIndex = 0;
  const rainbowColors = [
    cssColors.red,
    cssColors.orange, 
    cssColors.yellow,
    cssColors.green,
    cssColors.blue,
    cssColors.indigo,
    cssColors.violet,
  ];

  export function rainbow() {
    const color = rainbowColors[currentRainbowIndex];
    currentRainbowIndex = (currentRainbowIndex + 1) % rainbowColors.length;
    return color.slice();
  }
  ```

#### 2. Color Parsing Support (`num.mjs`)
- **Location**: Lines 522-523
- **Special case handling**: When parsing color names, `"rainbow"` returns `["rainbow", alpha]` instead of RGB values
- **Integration**: Exported to disk.mjs as `rainbow: num.rainbow` (line 1896)

#### 3. Graphics System Integration (`graph.mjs`)
- **Rainbow caching**: Uses `currentRainbowColor` to cache rainbow value per drawing operation
- **Fade support**: Rainbow colors work within fade sequences (lines 278-299)
- **Special markers**: Uses `["rainbow", alpha]` as special markers that get resolved at render time

#### 4. KidLisp Integration (`kidlisp.mjs`)
- **Function binding**: `rainbow: (api) => api.num?.rainbow() || [255, 0, 0]` (lines 3525-3526)
- **Color validation**: Special validation for rainbow colors (line 918)
- **Syntax highlighting**: Rainbow text gets colored character-by-character with rainbow colors (lines 977-987)
- **Optimization handling**: Special cases for rainbow in optimization routines (lines 1104-1131)

#### 5. Palette System
- **P0 mapping**: `p0` palette maps to `"rainbow"` (num.mjs line 837)
- **Palette resolution**: Special handling in `intToPalette()` function (lines 1015-1016)

### Syntax Highlighting Implementation
The syntax highlighting for `rainbow` in KidLisp uses this pattern:
```javascript
if (colorValue === "RAINBOW") {
  const rainbowColors = ["red", "orange", "yellow", "lime", "blue", "purple", "magenta"];
  for (let charIndex = 0; charIndex < colorName.length; charIndex++) {
    const charColor = rainbowColors[charIndex % rainbowColors.length];
    result += `\\${charColor}\\${colorName[charIndex]}`;
  }
}
```

## Proposed Zebra Implementation

### 1. Core Zebra Function (`num.mjs`)
Add alongside the rainbow function:
```javascript
// 🦓 Zebra color cycling (black/white alternating)
let currentZebraIndex = 0;

const zebraColors = [
  cssColors.black,   // [0, 0, 0]
  cssColors.white,   // [255, 255, 255]
];

export function zebra() {
  const color = zebraColors[currentZebraIndex];
  currentZebraIndex = (currentZebraIndex + 1) % zebraColors.length;
  return color.slice();
}
```

### 2. Color Parsing Support (`num.mjs`)
Add to the `parseColor` function around line 523:
```javascript
} else if (name === "rainbow") {
  return ["rainbow", alpha];
} else if (name === "zebra") {
  return ["zebra", alpha];
} else {
```

### 3. Export to Disk System (`disk.mjs`)
Add to the num exports around line 1896:
```javascript
rainbow: num.rainbow,
zebra: num.zebra,
```

### 4. Graphics System Support (`graph.mjs`)
Update the rainbow handling to support zebra:
```javascript
// Update rainbow color once per drawing operation if needed
if (currentRainbowColor === null && fadeColors.some(color => 
  color[0] === "rainbow" || color[0] === "zebra")) {
  currentRainbowColor = rainbow();
}

// Add zebra caching
let currentZebraColor = null;
if (currentZebraColor === null && fadeColors.some(color => color[0] === "zebra")) {
  currentZebraColor = zebra();
}

// Handle dynamic colors
if (startColor[0] === "rainbow") {
  startColor = [...currentRainbowColor, startColor[1] || 255];
} else if (startColor[0] === "zebra") {
  startColor = [...currentZebraColor, startColor[1] || 255];
}
```

### 5. KidLisp Integration (`kidlisp.mjs`)

#### Function Binding
Add around line 3526:
```javascript
rainbow: (api) => {
  return api.num?.rainbow() || [255, 0, 0];
},
zebra: (api) => {
  return api.num?.zebra() || [0, 0, 0];
},
```

#### Syntax Highlighting
Update the color highlighting code around lines 977-987:
```javascript
} else if (colorName === "rainbow") {
  colorValue = "RAINBOW";
} else if (colorName === "zebra") {
  colorValue = "ZEBRA";

// Later in the highlighting section:
if (colorValue === "RAINBOW") {
  const rainbowColors = ["red", "orange", "yellow", "lime", "blue", "purple", "magenta"];
  for (let charIndex = 0; charIndex < colorName.length; charIndex++) {
    const charColor = rainbowColors[charIndex % rainbowColors.length];
    result += `\\${charColor}\\${colorName[charIndex]}`;
  }
} else if (colorValue === "ZEBRA") {
  const zebraColors = ["black", "white"];
  for (let charIndex = 0; charIndex < colorName.length; charIndex++) {
    const charColor = zebraColors[charIndex % zebraColors.length];
    result += `\\${charColor}\\${colorName[charIndex]}`;
  }
}
```

#### Color Validation
Update validation around line 918:
```javascript
typeof result === "string" && (result.startsWith("fade:") || 
  result === "rainbow" || result === "zebra");
```

### 6. Palette System Extension
Could optionally add zebra as a palette:
```javascript
// In paletteColorMap
1: "zebra", // p1 = zebra
```

## Implementation Steps

✅ **1. Add zebra function to `num.mjs`** - Core cycling function
✅ **2. Update color parsing in `num.mjs`** - Handle "zebra" string  
✅ **3. Export zebra to `disk.mjs`** - Make available to pieces
✅ **4. Add KidLisp function binding** - `(zebra)` function
✅ **5. Add syntax highlighting support** - Alternating black/white characters *(Fixed: Added missing token recognition in 2 locations)*
✅ **6. Update graphics system** - Handle zebra in fades and caching
✅ **7. Add validation support** - Include zebra in color validation
✅ **8. Add palette mapping** - p1 = zebra
✅ **9. Add fade support** - zebra works in fade sequences
✅ **10. Add disk validation** - bare "zebra" recognized as KidLisp
✅ **11. Update colors.mjs index** - Add zebra to color listing piece *(Fixed: Added directional fade preview support)*

## IMPLEMENTATION COMPLETED! 🎉

The zebra color sequence has been successfully implemented across all layers of the Aesthetic Computer system. Here's what's now available:

- **`(ink zebra)`** - Sets ink to alternating black/white 
- **`(zebra)`** - Function that returns current zebra color
- **`p1`** - Palette shorthand for zebra (p1 is zebra)
- **`zebra`** - Bare color name works as piece content
- **Syntax highlighting** - "zebra" displays with alternating black/white letters
- **Fade support** - Works in sequences like `fade:black-zebra-white`
- **Graphics integration** - Works with all drawing primitives (line, rect, circle, etc.)

### 🔧 Additional Fixes Made:

- **Fixed fade direction previews** in colors.mjs - Now correctly shows vertical, diagonal, and angled fades in thumbnails
- **Added animated dynamic colors in fades** - `fade:zebra-rainbow` now shows animated zebra and rainbow colors
- **Added comprehensive fade examples** including `fade:red-blue:vertical` and directional fade support
- **Enhanced colors.mjs** with proper p0/p1 palette support and zebra animations
- **Fixed missing color names** - Added `darkbrown`, `darkerbrown`, `darksienna` to CSS colors for c124, c125, c126
- **Updated section labels** - "PALETTE COLORS" → "SEQUENCES", "SPECIAL COLORS" → "FADES"
- **Removed unwanted built-in fades** - Removed 'ocean', 'sunset', and 'fire' from both implementation and documentation
- **Fixed fade:zebra-rainbow thumbnail** - Now shows proper animated gradient between current zebra and rainbow colors
- **Added continuous repainting logic** - Thumbnails with dynamic colors now animate properly by returning appropriate paint state

## Future Extensibility

This pattern can be extended to support additional color sequences:
- `gradient` - smooth color transitions
- `neon` - bright electric colors
- `pastel` - soft muted colors

Each would follow the same pattern:
1. Core cycling function in `num.mjs`
2. Color parsing support
3. Export to disk system
4. KidLisp integration
5. Graphics system support
6. Syntax highlighting (optional)

## Files to Modify

1. `/system/public/aesthetic.computer/lib/num.mjs` - Core implementation
2. `/system/public/aesthetic.computer/lib/disk.mjs` - Export zebra function
3. `/system/public/aesthetic.computer/lib/kidlisp.mjs` - KidLisp integration
4. `/system/public/aesthetic.computer/lib/graph.mjs` - Graphics support
5. `/system/public/aesthetic.computer/disks/colors.mjs` - Color index listing piece

## Testing Considerations

- Test `(ink zebra)` in KidLisp pieces
- Test zebra in fade sequences: `fade:black-zebra-white`
- Test zebra syntax highlighting in KidLisp editor
- Test zebra with various graphics primitives (line, rect, circle)
- Test zebra persistence across frame boundaries
