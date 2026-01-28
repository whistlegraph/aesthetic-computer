# List.mjs Design Improvements Proposal

## Study: AC Design Language (from commits.mjs, chat.mjs, wallet.mjs, colors.mjs)

### Font System Observations
1. **MatrixChunky8** - Compact 4px char width, 8px height + 1px spacing = 9px row. Great for dense data.
2. **Unifont** - 8px char width, 16px height. Supports Unicode/emoji. Good for readable content.
3. **Default typeface** - 6px char width, ~10px row height. Balanced readability.

### Design Patterns
- **commits.mjs**: Uses `MatrixChunky8` for compact timeline display, timeline markers with color hierarchy (year=gold, month=purple, week/day=muted)
- **chat.mjs**: Multi-font system allowing user preference (`CHAT_FONTS`), syntax highlighting for KidLisp
- **wallet.mjs**: Rich color syntax highlighting, data streams as background decoration, financial ticker
- **colors.mjs**: Per-component RGB coloring with `MatrixChunky8` for dense numeric display

---

## Current State Analysis

### PIECE_CATEGORIES (Current)
```js
"🎨 Creative": ["box", "line", "circle", "colors", "fill", "brush", "crop", "download", "pixel", "shape", "poly", "oval", "blur", "nopaint", "painting", "plot", "drawings", "doodle", "handprint"],
```

### Discovered Nopaint Brushes (system = "nopaint")
From grep search, these are actual painting tools:
- `line.mjs` - Line brush
- `rect.mjs` - Rectangle brush (fill/outline modes)
- `oval.mjs` - Oval brush (fill/outline modes)  
- `smear.mjs` - Smear brush by @rapter
- `shape.mjs` - Shape brush
- `oldline.mjs` - Legacy line brush
- `wipe.mjs` - Wipe tool
- `bits.mjs` - Bit manipulation
- `icon.mjs` - Icon drawing
- `paint.mjs` - Paint tool
- `vary.mjs` - Variation tool
- `signature.mjs` - Signature overlay
- `desktop.mjs` - Desktop environment
- `colplay.mjs` - Color play
- `robo.mjs` - Robot drawing

---

## Proposed Improvements

### 1. 🖌️ New "Brushes" Category for Nopaint Tools

```js
const PIECE_CATEGORIES = {
  "🖌️ Brushes": [
    "line", "rect", "oval", "box", "circle", "shape", "poly",
    "smear", "blur", "spray", "fill", "wipe", "marker", "sparkle",
    "pull", "bits", "vary"
  ],
  "🎨 Creative": [
    "colors", "nopaint", "painting", "plot", "drawings", "doodle", 
    "crop", "download", "pixel", "handprint", "icon", "signature"
  ],
  // ... rest unchanged
};
```

### 2. 🔤 Multi-Font System for Better Density

Inspired by `chat.mjs` CHAT_FONTS pattern:

```js
// Font configurations for list display
const LIST_FONTS = {
  "compact": {
    name: "Compact",
    typeface: "MatrixChunky8",
    rowHeight: 9,      // 8px font + 1px
    charWidth: 4,
    maxDescChars: 40,  // More desc fits in compact mode
  },
  "default": {
    name: "Default",
    typeface: null,    // Uses system default
    rowHeight: 14,
    charWidth: 6,
    maxDescChars: 30,
  },
  "large": {
    name: "Large", 
    typeface: "unifont",
    rowHeight: 17,     // 16px font + 1px
    charWidth: 8,
    maxDescChars: 20,
  },
};
```

**Implementation:**
- Add font toggle in header (tap "🔤" cycles fonts)
- Use smaller `MatrixChunky8` for hit counts, dates, metadata
- Use `unifont` for piece names when accessibility needed
- Save font preference to store

### 3. 📊 Improved Information Density

**Current single-line format:**
```
pieceName  description...
```

**Proposed dual-density format (in compact mode):**
```
📦 pieceName               12k hits  3d ago
   Short description here...
```

Using `MatrixChunky8` for the metadata line, default font for name/desc.

### 4. 🎨 Enhanced Category Headers

Inspired by `commits.mjs` timeline markers:

```js
// Category header styling by importance
const CATEGORY_STYLES = {
  "🖌️ Brushes":  { bg: [40, 35, 60], text: [255, 200, 100], priority: 1 },
  "🎨 Creative": { bg: [35, 45, 35], text: [100, 220, 150], priority: 2 },
  "🎵 Audio":    { bg: [45, 35, 50], text: [200, 140, 255], priority: 3 },
  // etc.
};
```

### 5. 🔍 Search/Filter with Syntax Highlighting

Add a quick filter input at top that uses syntax highlighting similar to `wallet.mjs`:

```js
// Filter syntax colors
const filterColors = {
  "@": [200, 140, 255],     // @handle filter
  "brush:": [255, 200, 100], // category filter
  "#": [100, 220, 150],     // tag filter
  "default": [180, 190, 200],
};
```

### 6. 📱 Responsive Font Switching

```js
function updateLayoutMode(screen) {
  const w = screen.width;
  if (w < 128) {
    layoutMode = "tiny";
    currentFont = "compact";     // Force MatrixChunky8 on tiny screens
  } else if (w < 192) {
    layoutMode = "small";
    currentFont = "compact";
  } else if (w < 320) {
    layoutMode = "medium";
    currentFont = "default";
  } else {
    layoutMode = "large";
    currentFont = userFontPref || "default"; // Honor user preference on large
  }
}
```

### 7. ✨ Visual Polish (from wallet.mjs patterns)

- **Animated background streams** (subtle, like wallet.mjs data streams)
- **Hover glow** on items
- **Smooth scroll** with momentum
- **Selection pulse** animation

### 8. 📑 Sub-categorization for Brushes

```js
const BRUSH_SUBCATEGORIES = {
  "Shape Tools": ["line", "rect", "oval", "box", "circle", "poly", "shape"],
  "Effect Brushes": ["smear", "blur", "spray", "sparkle"],
  "Utility": ["fill", "wipe", "crop", "bits"],
};
```

---

## Implementation Priority

1. **High**: Add "🖌️ Brushes" category (simple category change)
2. **High**: Multi-font system with `MatrixChunky8` for metadata
3. **Medium**: Category styling hierarchy
4. **Medium**: Responsive font switching  
5. **Low**: Search/filter with syntax highlighting
6. **Low**: Visual polish animations

---

## Code Changes Summary

### Files to Modify
- [list.mjs](../system/public/aesthetic.computer/disks/list.mjs) - Main changes

### New Constants to Add
```js
// At top of file
const LIST_FONTS = { ... };
const BRUSH_PIECES = ["line", "rect", "oval", ...];
const CATEGORY_STYLES = { ... };

// In PIECE_CATEGORIES - reorder and add Brushes
const PIECE_CATEGORIES = {
  "🖌️ Brushes": BRUSH_PIECES,
  "🎨 Creative": [...],
  // ...
};
```

### Paint Function Changes
- Use `text.width(str, "MatrixChunky8")` for compact metadata
- Add font parameter to `write()` calls: `write(text, pos, bg, bounds, wrap, "MatrixChunky8")`
- Layer multiple font sizes in same row

---

## Example Visual Mockup

```
┌─────────────────────────────────────────┐
│ list                             [🔤] 42│  <- Header with font toggle, count
├─────────────────────────────────────────┤
│ 🔥Hot │ All │ Pieces │ Cmds │ @User    │  <- Tabs (unchanged)
├─────────────────────────────────────────┤
│ ▼ 🖌️ Brushes (15)                       │  <- New category!
│   line          Simple line brush   3.2k│  <- Name (default) + desc + hits (matrix)
│   rect          Draw rectangles    2.1k │
│   smear         Smear brush by...  1.8k │
│   blur          Blur effect        1.2k │
├─────────────────────────────────────────┤
│ ▸ 🎨 Creative (12)                      │  <- Collapsed
│ ▸ 🎵 Audio (8)                          │
│ ▼ 🎮 Games (5)                          │
│   pong          Classic pong        890 │
│   snake         Snake game          780 │
└─────────────────────────────────────────┘
```

---

## Questions to Consider

1. Should brush pieces show a small preview thumbnail?
2. Should there be a "recently used" section?
3. Should nopaint brushes have a special indicator icon?
4. Should the font toggle persist or reset per session?
