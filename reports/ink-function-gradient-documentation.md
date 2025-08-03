# Ink Function and Gradient Feature Documentation

**Date:** August 3, 2025  
**Version:** Enhanced with gradient support  
**Location:** `system/public/aesthetic.computer/lib/disk.mjs` (ink function) and `system/public/aesthetic.computer/lib/graph.mjs` (gradient implementation)

## Overview

The `ink()` function is the primary color-setting function in the aesthetic computer system. It has been enhanced with gradient support that allows for smooth color transitions across drawing primitives.

## Basic Usage

### Solid Colors

```javascript
// Named CSS colors
ink("red");
ink("blue");
ink("purple");

// RGB values
ink(255, 0, 0);        // Red
ink(0, 255, 0);        // Green
ink(0, 0, 255);        // Blue

// RGBA with alpha transparency
ink(255, 0, 0, 128);   // Semi-transparent red

// Hex colors
ink("#FF0000");        // Red
ink("0xFF0000");       // Red

// Arrays
ink([255, 0, 0]);      // Red
ink([255, 0, 0, 128]); // Semi-transparent red

// Random color
ink();                 // Generates random color
```

## Gradient Feature

### Gradient Syntax

Gradients use the syntax: `"gradient:color1-color2-color3-..."`

```javascript
// Two-color gradients
ink("gradient:blue-white");
ink("gradient:red-yellow");
ink("gradient:green-purple");

// Multi-color gradients
ink("gradient:red-yellow-blue");
ink("gradient:red-orange-yellow-green-blue-purple");
ink("gradient:cyan-magenta-yellow");
```

### Supported Drawing Primitives

Gradients work with:
- **Lines**: Gradient flows from start to end point
- **Filled boxes**: Gradient flows horizontally or vertically based on dimensions
- **Other primitives that use line/box internally**

```javascript
// Gradient line
ink("gradient:blue-white");
line(0, 0, 100, 100);

// Gradient filled box
ink("gradient:red-yellow");
box(50, 50, 100, 100);
```

## Mask Integration

When a mask is active, gradients are calculated relative to the mask bounds rather than the individual shape bounds. This creates consistent gradient behavior across multiple shapes within the same masked area.

```javascript
// Set mask bounds
mask({ x: 100, y: 100, width: 200, height: 200 });

// All shapes within this mask will have gradients 
// calculated relative to the mask bounds
ink("gradient:red-blue");
box(110, 110, 50, 50);  // Gradient starts from mask left edge
box(150, 150, 30, 30);  // Continues same gradient mapping

unmask();
```

### Mask Examples

```javascript
// Example: Three boxes with consistent gradient mapping
const maskBounds = { x: 50, y: 50, width: 300, height: 100 };
mask(maskBounds);

ink("gradient:red-yellow-blue");
box(60, 60, 80, 80);   // Shows red to yellow portion
box(160, 60, 80, 80);  // Shows yellow to blue portion  
box(260, 60, 80, 80);  // Shows blue portion

unmask();
```

## Technical Implementation

### Architecture

The gradient system consists of several key components:

1. **Color Parsing** (`findColor` in `graph.mjs`):
   - Detects `"gradient:"` prefix
   - Parses color names using CSS color table
   - Returns special object with `isGradient: true` flag

2. **Gradient State Management** (`graph.mjs`):
   - `gradientMode`: Boolean flag indicating active gradient
   - `gradientColors`: Array of parsed RGBA color arrays
   - `gradientDirection`: Orientation hint (future enhancement)

3. **Color Interpolation** (`getGradientColor` in `graph.mjs`):
   - Takes position parameter `t` (0.0 to 1.0)
   - Interpolates between gradient colors
   - Supports multi-color gradients with even spacing

4. **Drawing Integration**:
   - **Lines**: Calculate `t` based on distance along line
   - **Boxes**: Calculate `t` based on pixel position within box
   - **Mask-aware**: Uses mask bounds when active

### Key Functions

#### `parseGradient(gradientString)` in `graph.mjs`
```javascript
// Parses "gradient:blue-white" into array of RGBA colors
// Returns: [[0,0,255,255], [255,255,255,255]] or null if invalid
```

#### `getGradientColor(t)` in `graph.mjs`
```javascript
// Takes t (0.0-1.0) and returns interpolated RGBA color
// Example: t=0.5 on "gradient:red-blue" returns purple
```

#### `ink()` in `disk.mjs`
```javascript
// Enhanced to detect gradient objects from findColor
// Calls graph.color() with preventGradientReset flag for gradients
```

### Color State Management

The system carefully manages when gradient mode is active vs. solid color mode:

- **Gradient mode enabled**: When `ink("gradient:...")` is called
- **Gradient mode disabled**: When solid color is set via `ink("red")` or `color()` directly
- **State preservation**: During gradient drawing, state is preserved with `preventGradientReset` flag

## Usage Patterns

### Basic Gradient Lines
```javascript
function paint({ ink, line, screen }) {
  // Simple horizontal gradient
  ink("gradient:red-blue");
  line(0, 100, screen.width, 100);
  
  // Diagonal gradient
  ink("gradient:yellow-purple");
  line(0, 0, screen.width, screen.height);
}
```

### Gradient Boxes
```javascript
function paint({ ink, box }) {
  // Horizontal gradient (wider than tall)
  ink("gradient:green-yellow");
  box(50, 50, 200, 100);
  
  // Vertical gradient (taller than wide)  
  ink("gradient:blue-white");
  box(300, 50, 100, 200);
}
```

### Masked Gradient Regions
```javascript
function paint({ ink, box, mask, unmask }) {
  // Create consistent gradient across multiple shapes
  mask({ x: 0, y: 0, width: 400, height: 200 });
  
  ink("gradient:red-orange-yellow-green-blue");
  
  // All boxes share same gradient mapping
  for (let i = 0; i < 5; i++) {
    box(i * 80, 50, 60, 100);
  }
  
  unmask();
}
```

### Complex Multi-Color Gradients
```javascript
function paint({ ink, line, screen }) {
  // Rainbow gradient
  ink("gradient:red-orange-yellow-green-blue-indigo-violet");
  line(0, screen.height / 2, screen.width, screen.height / 2);
  
  // Sunset gradient
  ink("gradient:purple-red-orange-yellow-white");
  line(0, 100, screen.width, 100);
}
```

## Future Enhancements

Potential areas for expansion:

1. **Gradient Direction Control**: `"gradient:red-blue:vertical"` or `"gradient:red-blue:diagonal"`
2. **Radial Gradients**: `"radial-gradient:center-red-blue"`
3. **Gradient Stops**: `"gradient:red@0.2-blue@0.8"` for non-uniform distribution
4. **Easing Functions**: `"gradient:red-blue:ease-in-out"`

## Relationship Documentation

### File Relationships

- **`disk.mjs`**: Contains `ink()` function that serves as the public API
- **`graph.mjs`**: Contains gradient implementation (`parseGradient`, `getGradientColor`, `findColor`)
- **Connection**: `ink()` calls `graph.findColor()` and `graph.color()` with gradient awareness

### Maintenance Notes

When modifying gradient functionality:

1. **Always update both files**: Changes to gradient parsing in `graph.mjs` may require updates to `ink()` in `disk.mjs`
2. **Preserve API compatibility**: The `ink("gradient:...")` syntax should remain stable
3. **Test with masks**: Ensure gradient behavior works correctly with and without active masks
4. **Verify state management**: Gradient mode should be properly enabled/disabled
5. **Update this documentation**: Keep this file synchronized with implementation changes

### Testing

The gradient functionality can be tested using `gradient-test.mjs`:

```javascript
// Load the test piece
// Visit: /gradient-test in the aesthetic computer

// Expected behavior:
// - Lines show smooth color transitions
// - Boxes show gradients based on their dimensions
// - Masked regions show consistent gradient mapping
// - Solid colors work normally after gradients
```

## Error Handling

The gradient system gracefully handles errors:

- **Invalid gradient strings**: Fall back to random color
- **Unknown color names**: Use CSS color table, fallback to random
- **Empty gradient**: Treats as solid color
- **Single color gradient**: Treats as solid color

## Performance Considerations

- **Line gradients**: Efficient, calculated per-pixel during line drawing
- **Box gradients**: More intensive, calculated per-pixel for filled boxes
- **Mask calculations**: Additional overhead when mask is active
- **Color interpolation**: Optimized linear interpolation between colors

---

**Note**: This documentation should be updated whenever the gradient implementation changes. The relationship between `ink()` in `disk.mjs` and the gradient functions in `graph.mjs` is critical for proper functionality.
