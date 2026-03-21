# Button Width Sizing Issue - Technical Plan

## Problem Summary
The HUD button width calculation for KidLisp syntax highlighting is not working optimally. Buttons are sometimes too wide, too narrow, or inconsistent when resizing windows.

## Current State (September 6, 2025)

### Key Files & Code Locations
- **Primary File**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/disk.mjs`
- **Button Width Logic**: Lines ~8000-8070 (makeFrame function)
- **Text Rendering Pipeline**: `write() → text.box() → typeface.print() → $.printLine()`
- **Supporting Files**: 
  - `type.mjs`: Typeface class with blockWidth/blockHeight properties
  - `kidlisp.mjs`: updateHUDWithSyntaxHighlighting() function

### Current Implementation Details

#### Text Cleaning (Lines ~8000-8015)
```javascript
// Use plain text for width calculation to avoid counting color codes
const textForWidthCalculation = currentHUDPlainTxt || currentHUDTxt;

// Double-check: strip color codes directly if they're still present
const colorCodeRegex = /\\[^\\]*\\/g;
const cleanText = textForWidthCalculation.replace(colorCodeRegex, '');
```

#### Button Width Calculation (Lines ~8015-8035)
```javascript
// Use consistent scaling regardless of screen size
const screenScale = 1.0;
const scaledBlockWidth = tf.blockWidth * screenScale;

// Calculate button width based on text content
let maxButtonWidth;

if (cleanText.includes('\n')) {
  // Multiline: calculate bounds based on longest line with generous buffer to prevent wrapping
  const lines = cleanText.split('\n');
  const longestLineLength = Math.max(...lines.map(line => line.length));
  maxButtonWidth = (longestLineLength + 10) * scaledBlockWidth; // Add larger buffer to prevent wrapping
} else {
  // Single line: use generous bounds to ensure text is never cut off
  maxButtonWidth = Math.max(cleanText.length * scaledBlockWidth + scaledBlockWidth * 4, $api.screen.width);
}

// Use text.box to get proper layout
const textBounds = $api.text.box(
  cleanText,
  undefined,
  maxButtonWidth,
  screenScale
);

// Use the width from text.box
const textWidth = textBounds.box.width;
const padding = scaledBlockWidth * 1.5; // 1.5 character buffer
let w = textWidth + currentHUDScrub + padding;
```

#### Height Calculation (Lines ~8065-8070)
```javascript
// Use the text box height with padding - add full line height for multiline text
const heightPadding = cleanText.includes('\n') ? 
  tf.blockHeight * screenScale + 4 : // Full line height + 4px for multiline
  tf.blockHeight * screenScale * 0.4; // 40% for single line
const h = textBounds.box.height + heightPadding;
```

### Font System Properties
- **Default Font**: font_1 (monospace, defined in `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/common/fonts.mjs`)
- **Glyph Width**: 6px (`tf.blockWidth` from `font_1.glyphWidth`)
- **Glyph Height**: 10px (`tf.blockHeight` from `font_1.glyphHeight`) 
- **Screen Scale**: 1.0 (fixed, no longer responsive)
- **Global Typeface**: `tf` initialized at line 4729 in disk.mjs as `new Typeface().load()`

### Font System Architecture

#### Font Definition Structure (fonts.mjs)
```javascript
export const font_1 = {
  glyphHeight: 10,           // Natural block height (tf.blockHeight)
  glyphWidth: 6,             // Natural block width (tf.blockWidth) 
  proportional: false,       // Monospace font - fixed character width
  // Character mappings...
  a: "lowercase/a - 2022.1.11.16.12.07",
  // etc...
};
```

#### Typeface Class (type.mjs)
**Location**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/type.mjs`

**Key Properties**:
- `get blockWidth()` (line 35): Returns `this.data.glyphWidth` (6px for font_1)
- `get blockHeight()` (line 46): Returns `this.data.glyphHeight` (10px for font_1)
- `constructor(name = "font_1")` (line 23): Defaults to font_1, loads from fonts.mjs

**Important Methods**:
- `print()` (line 319): Main text rendering method, handles proportional vs monospace
- `load()` (line 50): Preloads glyphs, handles different font types (BDF, microtype, etc.)

#### Global Font Initialization (disk.mjs)
**Location**: Line 4729 in `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/disk.mjs`
```javascript
if (!tf) tf = await new Typeface(/*"unifont"*/).load($commonApi.net.preload);
```
- `tf` is the global typeface instance used throughout the system
- Defaults to font_1 (6px × 10px monospace bitmap font)
- Loaded once when first piece loads
- `tf.blockWidth` and `tf.blockHeight` are the source values for all width/height calculations

#### Text Width Calculation Methods

**1. Monospace (font_1 - current default)**:
```javascript
const textWidth = text.length * tf.blockWidth; // Simple: chars × 6px
```

**2. Proportional (MatrixChunky8, unifont)**:
```javascript
// Uses character-specific advance widths from font definition
const advances = this.data?.advances || {};
let totalWidth = 0;
[...text].forEach(char => {
  totalWidth += (advances[char] || defaultWidth) * scale;
});
```

#### Font Types Available
1. **font_1** (default): 6×10px monospace bitmap font
2. **MatrixChunky8**: Proportional BDF font with character-specific widths
3. **unifont**: 8×16px monospace Unicode font via BDF
4. **microtype**: 4×5px ultra-compact font for special cases

#### Text Rendering Pipeline
```
write() → text.box() → typeface.print() → $.printLine()
```
1. `write()`: Entry point for text rendering
2. `text.box()`: Layout calculation using `tf.blockWidth * scale`
3. `typeface.print()`: Character-by-character rendering with proper spacing
4. `$.printLine()`: Low-level pixel rendering

#### Critical Width Calculation Insight
**The Issue**: `text.box()` defaults to total character count when bounds is undefined:
```javascript
// In text.box() at line 1745:
if (bounds === undefined) bounds = (text.length + 2) * blockWidth;
```
**For 239-char multiline text**: `(239 + 2) * 6 = 1446px` (way too wide!)
**Should be**: Based on longest line, not total character count

#### Natural Block Width Values
- **tf.blockWidth**: 6px (from font_1.glyphWidth)
- **tf.blockHeight**: 10px (from font_1.glyphHeight)
- **With scaling**: `scaledBlockWidth = tf.blockWidth * screenScale`
- **In button calc**: Used as base unit for all width/height calculations

### Known Issues

#### 1. Debug Output Analysis
Recent debug showed:
```
cleanText: "fade:red-blue-black-blue-red\nink (? rainbow white 0)..."
cleanTextLength: 239
textWidth: 1084.5 (too wide!)
finalWidth: 1091.25
```

**Root Cause**: When `maxButtonWidth` is undefined, text.box() defaults to `(text.length + 2) * blockWidth = (239 + 2) * 4.5 = 1084.5px`

#### 2. Window Resize Behavior
- User wants consistent button size regardless of window dimensions
- Text should never be cut off or wrapped unexpectedly
- Current implementation removes responsive scaling but still has sizing inconsistencies

#### 3. Text.box() Function Behavior (Lines 1740-1750)
```javascript
if (bounds === undefined) bounds = (text.length + 2) * blockWidth;
```
This is the source of the 1084px width - it uses total character count instead of longest line.

### Architecture Decisions Made

#### ✅ Preserved Features
- Multiline text boundary support maintained
- Original text rendering pipeline intact
- KidLisp syntax highlighting integration working
- Single-line vs multiline handling separation

#### ✅ Fixed Issues
- Removed responsive scaling (screenScale always 1.0)
- Fixed duplicate variable declarations
- Added proper height padding for multiline (full line height + 4px)
- Improved color code stripping

#### ❌ Remaining Issues
- Multiline button width still inconsistent
- Debug logging not comprehensive enough
- Text wrapping edge cases not fully handled

### User Requirements
1. **Always show full text**: Never cut off or wrap unexpectedly
2. **Consistent sizing**: Button dimensions shouldn't change on window resize
3. **Optimal width**: Width should match longest line, not total character count
4. **Multiline support**: Preserve existing multiline text layout capabilities

### Suggested Next Steps for New Agent

#### Immediate Priority
1. **Fix multiline width calculation**: Ensure maxButtonWidth for multiline uses longest line length, not total text length
2. **Implement better debug logging**: Log dimensions only when they change per frame
3. **Test edge cases**: Very long lines, mixed content, empty lines

#### Implementation Strategy
```javascript
// Proposed improved approach for multiline:
if (cleanText.includes('\n')) {
  const lines = cleanText.split('\n');
  const longestLineLength = Math.max(...lines.map(line => line.length));
  // Use actual longest line + minimal buffer, not total character count
  maxButtonWidth = longestLineLength * scaledBlockWidth + (scaledBlockWidth * 2);
}
```

#### Debug Logging Strategy
```javascript
// Track dimensions per frame and only log changes:
const currentDimensions = { width: textBounds.box.width, height: textBounds.box.height };
if (!window.lastButtonDimensions || 
    window.lastButtonDimensions.width !== currentDimensions.width ||
    window.lastButtonDimensions.height !== currentDimensions.height) {
  // Log comprehensive button state
  window.lastButtonDimensions = currentDimensions;
}
```

### Test Cases to Validate
1. **Single-line commands**: "rect 255 255 255 128" should be ~120px wide
2. **Multiline KidLisp**: 8-line code block should size to longest line width
3. **Window resize**: Button dimensions should remain constant
4. **Empty lines**: Multiline with blank lines should handle correctly
5. **Very long lines**: Lines exceeding screen width should not wrap

### Context for New Agent
- This is aesthetic-computer, a creative coding platform
- KidLisp is the Lisp interpreter with syntax highlighting
- HUD shows command history with colored syntax
- Text rendering uses monospace font with precise pixel calculations
- User prioritizes functionality over responsive design

### Font System Research Locations
**Primary Files**:
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/common/fonts.mjs` - Font definitions and glyph mappings
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/type.mjs` - Typeface class and text rendering logic
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/disk.mjs` - Global font initialization and text.box() function

**Key Research Points**:
1. **Font Metrics**: Search for `glyphWidth`, `glyphHeight`, `blockWidth`, `blockHeight` in type.mjs and fonts.mjs
2. **Text Layout**: Study `text.box()` function at disk.mjs:1733-1850 for width calculation logic
3. **Proportional Fonts**: Check `advances` property in fonts.mjs for character-specific widths
4. **Font Loading**: Examine `tf` initialization at disk.mjs:4729 and Typeface constructor
5. **Width Calculations**: Look for `tf.blockWidth * scale` patterns throughout disk.mjs

**Font System Debug Commands**:
```javascript
// In browser console:
console.log("Font info:", tf.blockWidth, tf.blockHeight, tf.data);
console.log("Font_1 definition:", fonts.font_1);
console.log("Text box test:", $api.text.box("test", undefined, undefined, 1));
```

## Session History Summary
- Started with buttons too wide (200px+ for short text)
- Evolved through multiple approaches: character-based calculation, screen percentage, longest-line calculation
- Discovered text.box() default behavior causing width inflation
- Fixed height padding and screen scaling consistency
- Still need optimal multiline width solution

---
*Created: September 6, 2025*
*Status: Ready for new agent continuation*
