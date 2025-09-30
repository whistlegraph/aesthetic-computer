# RGB First-Line Color Support for KidLisp

## Goal
Support RGB color values (like `255 0 0`) as valid first-line KidLisp programs that serve as aliases for `(once (wipe 255 0 0))`, similar to how named colors like `red`, `blue`, etc. already work.

## Current System Overview

### 1. KidLisp Detection (`kidlisp.mjs`)
- **Function**: `isKidlispSource(text)` - Line ~9985
- **Current Detection Logic**:
  - Checks for `$-prefixed` cached codes
  - Checks if starts with `(`
  - Checks for newlines
  - Checks for **first-line color indicators**: `fade:`, `c\d+`, named CSS colors, `rainbow`
  - Checks for comma-separated KidLisp expressions
  - Does NOT currently check for bare RGB values like `255 0 0`

### 2. Prompt Cursor Color (`type.mjs`)
- **Location**: Line ~1206
- **Logic**: 
  ```javascript
  const cursorColor = $.system?.prompt?.kidlispMode
    ? $.dark
      ? [100, 255, 100]  // Light green in dark mode
      : [0, 150, 0]      // Dark green in light mode
    : this.pal.block;    // Normal cursor color
  ```
- **Trigger**: Set by `prompt.mjs` via `$.system.prompt.kidlispMode = inKidlispMode`

### 3. Prompt Mode Detection (`prompt.mjs`)
- **Location**: Line ~1583
- **Logic**:
  ```javascript
  const inKidlispMode = isPromptInKidlispMode(currentInputText);
  $.system.prompt.kidlispMode = inKidlispMode;
  ```
- **Function Used**: `isPromptInKidlispMode()` from `kidlisp.mjs`

### 4. First-Line Color Detection (`kidlisp.mjs`)
- **Function**: `detectFirstLineColor()` - Line ~1684
- **Current Support**:
  - Named colors (e.g., `red`, `blue`, `cyan`)
  - Color codes (e.g., `c0`, `c1`, `c2`)
  - Fade strings (e.g., `fade:red-blue`)
  - `rainbow` and `zebra`
- **Does NOT Support**: Raw RGB values like `255 0 0` or `255, 0, 0`

### 5. Color Parsing (`kidlisp.mjs` & `num.mjs`)
- **Named Colors**: Stored in `cssColors` map from `num.mjs`
- **Static Color Codes**: `staticColorMap` - indices 0-15
- **RGB Format**: Can be comma-separated (`255,0,0`) or space-separated (`255 0 0`)

### 6. Box Brush RGB Syntax (`box.mjs`)
- The `box` brush in nopaint system has syntax highlighting for RGB
- `robo` also has custom syntax highlighting for RGB parameters
- These show that RGB values are already understood in other contexts

## Connection Points to Modify

### A. `isKidlispSource()` in `kidlisp.mjs` (~line 10040)
**Current Check**:
```javascript
// Check for first-line color indicators (fade strings and color codes)
const trimmedText = text.trim();
if (trimmedText.startsWith("fade:") ||
    trimmedText.match(/^c\d+$/) ||
    cssColors[trimmedText] ||
    trimmedText === "rainbow") {
  return true;
}
```

**Needs Addition**:
```javascript
// Check for RGB values (space or comma separated)
// Format: "255 0 0" or "255, 0, 0" or "255,0,0"
if (isValidRGBString(trimmedText)) {
  return true;
}
```

### B. `detectFirstLineColor()` in `kidlisp.mjs` (~line 1684)
**Current Logic**: Only checks for named colors, color codes, and fade strings

**Needs Addition**: Check if first item is an RGB array or valid RGB string

### C. Color String Validation Helper Function (NEW)
**Location**: `kidlisp.mjs` (near other color utilities)

**Function Needed**:
```javascript
// Check if a string represents valid RGB values
function isValidRGBString(str) {
  if (!str) return false;
  
  // Try space-separated: "255 0 0"
  const spaceParts = str.trim().split(/\s+/);
  if (spaceParts.length === 3) {
    const [r, g, b] = spaceParts.map(p => parseInt(p, 10));
    if (isValidRGBComponent(r) && isValidRGBComponent(g) && isValidRGBComponent(b)) {
      return true;
    }
  }
  
  // Try comma-separated: "255, 0, 0" or "255,0,0"
  const commaParts = str.trim().split(/,\s*/);
  if (commaParts.length === 3) {
    const [r, g, b] = commaParts.map(p => parseInt(p, 10));
    if (isValidRGBComponent(r) && isValidRGBComponent(g) && isValidRGBComponent(b)) {
      return true;
    }
  }
  
  return false;
}

function isValidRGBComponent(value) {
  return Number.isInteger(value) && value >= 0 && value <= 255;
}
```

### D. RGB to Color Array Conversion (NEW)
**Location**: `kidlisp.mjs`

**Function Needed**:
```javascript
// Parse RGB string into [r, g, b] array
function parseRGBString(str) {
  if (!str) return null;
  
  // Try space-separated
  let parts = str.trim().split(/\s+/);
  if (parts.length === 3) {
    const rgb = parts.map(p => parseInt(p, 10));
    if (rgb.every(v => isValidRGBComponent(v))) {
      return rgb;
    }
  }
  
  // Try comma-separated
  parts = str.trim().split(/,\s*/);
  if (parts.length === 3) {
    const rgb = parts.map(p => parseInt(p, 10));
    if (rgb.every(v => isValidRGBComponent(v))) {
      return rgb;
    }
  }
  
  return null;
}
```

### E. AST Handling for RGB Arrays (`kidlisp.mjs`)
**Location**: In evaluation logic and `detectFirstLineColor()`

**Needs**: Recognition that a bare array of 3 numbers `[255, 0, 0]` or bare space-separated numbers should be treated as RGB color for wipe

### F. Syntax Highlighting for RGB (`kidlisp.mjs`)
**Location**: `getTokenColor()` method (~line 7873)

**Enhancement**: When RGB values are detected in first position, color them appropriately
- Could use pink for numbers (already done)
- Could add special RGB highlighting similar to how we handle color names

## Implementation Plan

### Phase 1: Detection & Validation
1. ✅ Add `isValidRGBString()` helper function
2. ✅ Add `parseRGBString()` helper function
3. ✅ Add `isValidRGBComponent()` helper function
4. ✅ Update `isKidlispSource()` to detect RGB strings
5. ✅ Test prompt cursor turns green when typing `255 0 0`

### Phase 2: First-Line Color Support
6. ✅ Update `detectFirstLineColor()` to handle RGB strings
7. ✅ Store RGB values as first-line color
8. ✅ Test that `255 0 0` sets background color correctly

### Phase 3: Execution & Aliasing
9. ✅ Ensure RGB strings execute as `(once (wipe r g b))`
10. ✅ Handle both space-separated and comma-separated formats
11. ✅ Test various RGB combinations

### Phase 4: Syntax Highlighting (Optional Enhancement)
12. 🔲 Add special RGB syntax highlighting for first-line RGB values
13. 🔲 Ensure RGB values in HUD show with proper shadows

## Test Cases

### Should Turn Cursor Green (Valid KidLisp):
- ✅ `255 0 0` (space-separated)
- ✅ `255, 0, 0` (comma-separated with spaces)
- ✅ `255,0,0` (comma-separated without spaces)
- ✅ `0 0 0` (black)
- ✅ `128 128 128` (gray)

### Should NOT Turn Cursor Green (Invalid):
- ❌ `256 0 0` (out of range)
- ❌ `255 0` (only 2 values)
- ❌ `255 0 0 255` (4 values - RGBA not supported in this context)
- ❌ `red 0 0` (mixed named and numeric)
- ❌ `box 255 0` (command with numbers)

### Should Execute Correctly:
- `255 0 0` → Sets background to bright red
- `0 255 0` → Sets background to bright green
- `0 0 255` → Sets background to bright blue
- `0 0 0` → Sets background to black

## Related Files
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/kidlisp.mjs` - Main logic
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/type.mjs` - Cursor color
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/prompt.mjs` - Mode detection
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/num.mjs` - Color utilities
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/box.mjs` - RGB syntax reference
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/robo.mjs` - RGB syntax reference

## Notes
- The system already has infrastructure for first-line colors
- RGB values are already parsed in other contexts (box brush, robo)
- This feature maintains consistency: if you can type `red` for `(once (wipe red))`, you should be able to type `255 0 0` for `(once (wipe 255 0 0))`
- Both space-separated and comma-separated formats should be supported for user flexibility
