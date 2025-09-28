# KidLisp HUD Text Layout Issue

## Problem Summary
KidLisp syntax-highlighted text in corner HUD labels is being forced into a single column on the left side of the screen, instead of maintaining the original multi-column code layout with proper character positioning.

## Current Status
- ✅ **QR corner label visible** - Fixed by reverting to default font (font_1) 
- ✅ **QR size optimized** - Set to 1:1 pixel size
- ✅ **Parentheses flashing fixed** - Excluded from timing-based color changes
- ✅ **Shadow conflicts resolved** - Simplified to uniform black shadows
- ❌ **Text layout broken** - All characters forced to left side in single column

## Expected Behavior
Text should render in natural multi-column layout matching original KidLisp code structure:
```
fade:red-blue-black-blue-red
ink (? rainbow white 0) (1s... 24 64)
line w/2 0 w/2 h
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
ink (? cyan yellow magenta) 8
circle w/2 h/2 (? 2 4 8)
```

Characters should be positioned at 6px intervals across the natural width (~228px), with syntax highlighting intact.

## Actual Behavior
All characters appear in a narrow single column on the left side of the screen, regardless of:
- Buffer width (set to natural 228px)
- Bounds constraint (`{ w: bufferW, h: bufferH }`)
- Word wrap disabled (`false`)
- Clean text vs color-coded text

## Technical Investigation Done

### Root Cause Analysis
1. **Text Content**: Confirmed not the issue - tested with both:
   - Clean KidLisp source (28-38 chars per line, fits in 228px buffer)
   - Original color-coded text (157-186 chars per line with embedded color codes)

2. **Buffer Sizing**: Confirmed working correctly:
   - Natural width: 228px (calculated from text.box())
   - Screen width: 253px (no clipping needed)
   - Buffer created at full natural width

3. **Text Rendering Parameters**: All tested:
   - Bounds constraint: `{ w: 228, h: 88 }`
   - Word wrap: `false`
   - Position: `{ x: 0, y: 0 }`
   - Font: Default (font_1, 6px×10px)

### Test Results
Created isolated test (`test-text-wrapping.mjs`) showing:
- Line 0: 28 chars = 168px (✅ fits in 228px buffer, should spread 0-162px)
- Line 1: 39 chars = 234px (❌ slightly too long, would wrap to 2 lines)
- Lines 2-7: 14-22 chars = 84-132px (✅ all fit, should spread naturally)

### Code Locations
- **Main rendering**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/disk.mjs` lines ~9570-9580
- **Buffer creation**: Lines ~9370-9380
- **Text processing**: Lines ~9450-9460

## Current Implementation
```javascript
// Use full buffer bounds to preserve character positioning
const boundsConstraint = { w: bufferW, h: bufferH };

// Go back to the original approach - render the color-coded text as intended
// The issue is not the text content but how we handle the color system

// Render text with color codes processed by the system
$.write(
  text,
  { x: 0 + currentHUDScrub, y: 0 },
  boundsConstraint, // Use full buffer bounds to allow natural flow
  1, // scale
  false, // No word wrap - preserve original layout
  useTinyHudLabel ? "MatrixChunky8" : undefined
);
```

## Suspected Issue
The `$.write()` function appears to have internal text wrapping/positioning logic that overrides our bounds and word wrap settings when processing color-coded text. Even with:
- `bounds: { w: 228, h: 88 }`
- `wordWrap: false` 
- Natural buffer width of 228px

Characters are still forced into a narrow column on the left.

## Next Steps for Investigation

### 1. Deep Dive into $.write() Function
- Locate implementation of `$.write()` in the codebase
- Understand how it processes color codes (e.g., `\\cyan\\`, `\\red\\`)
- Check if there's internal width limiting or column forcing logic
- Test if color code processing is causing the positioning issue

### 2. Alternative Rendering Approaches
- **Character-by-character rendering**: Manually position each character
- **Line-by-line with color processing**: Process color codes then render each line
- **Direct pixel manipulation**: Bypass text system entirely for positioning

### 3. Font System Investigation
- Check if default font (font_1) has positioning limitations
- Test with different fonts to isolate font-specific issues
- Verify font metrics are being calculated correctly (6px per character)

### 4. Color Code Processing
- Investigate how color codes like `\\mediumseagreen\\fade\\lime\\` are parsed
- Check if the parsing process is affecting character positioning
- Test rendering plain text (without color codes) vs color-coded text

### 5. Buffer vs Screen Rendering
- Verify buffer is being created at correct dimensions (228×88px)
- Check how buffer is composited to screen in main thread
- Ensure buffer-to-screen positioning preserves character layout

## Environment Details
- Screen width: 253px (varies, was 195px earlier)
- Font: Default (font_1) - 6px×10px glyphs, 144×66 buffer
- Buffer: 228×88px (natural text dimensions)
- Characters should position at: 0px, 6px, 12px, 18px, 24px, etc.

## Test Commands
```bash
# Get actual KidLisp source
st roz

# Run text wrapping analysis
node test-text-wrapping.mjs
```

## Files Modified
- `/workspaces/aesthetic-computer/system/public/aesthetic.computer/lib/disk.mjs`
- `/workspaces/aesthetic-computer/test-text-wrapping.mjs` (test file)

## Priority
**HIGH** - This affects the core functionality of KidLisp piece display and user experience. The QR functionality works, but without proper text layout, the syntax highlighting and code readability are severely compromised.