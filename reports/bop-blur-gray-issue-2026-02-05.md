# $bop Gray Screen / Blur Issue Report

**Date:** February 5, 2026  
**Piece:** `$bop`  
**Source:** `1 purple, ink, line, blur 5`  
**Author:** anonymous  
**Hits:** 5732

## Issue Description

When loading `$bop` on FF1 (or refreshing), the display shows all gray and the console is flooded with errors:

```
🎨 Paint failure... TypeError: Array.prototype.filter called on null or undefined
    at filter (<anonymous>)
    at makeFrame (disk.mjs:12432:19)
    at onmessage (disk.mjs:9165:5)
```

## Root Cause Analysis

### Primary Bug: Null Check in `form()` Function

The error originates in [disk.mjs#L5471](system/public/aesthetic.computer/lib/disk.mjs#L5471) in the `form()` function:

```javascript
// BEFORE (buggy):
if (forms === undefined || forms?.length === 0) return;

// This check fails when forms is `null` because:
// - `null === undefined` is false
// - `null?.length` is undefined, and `undefined === 0` is false
// So when forms is null, the function continues and crashes on:
forms.filter(Boolean).forEach((form) => form.graph(cam));
```

### Fix Applied

Changed to use loose equality (`==`) which catches both `null` and `undefined`:

```javascript
// AFTER (fixed):
if (forms == null || forms?.length === 0) return;
```

## KidLisp Blur Implementation Path

For `$bop`, the blur command flows through:

1. **KidLisp Parser** ([kidlisp.mjs#L16006](system/public/aesthetic.computer/lib/kidlisp.mjs#L16006)) - Parses `blur 5` command
2. **KidLisp Executor** ([kidlisp.mjs#L7563](system/public/aesthetic.computer/lib/kidlisp.mjs#L7563)) - Handles blur execution with embedded layer logic
3. **Graph API** ([graph.mjs#L6221](system/public/aesthetic.computer/lib/graph.mjs#L6221)) - CPU blur implementation
4. **GPU Effects** ([gpu-effects.mjs#L879](system/public/aesthetic.computer/lib/gpu-effects.mjs#L879)) - GPU blur implementation (preferred path)

### Blur Implementation Details

The blur uses a **2-pass separable Gaussian filter**:
- **Pass 1:** Horizontal blur (texture → pingPong buffer)
- **Pass 2:** Vertical blur (pingPong → output)

GPU blur has Y-flip handling for correct orientation:
```javascript
// Upload with Y-flip
gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
gl.texSubImage2D(...);

// Readback with Y-flip correction
for (let y = 0; y < height; y++) {
  const srcRow = (height - 1 - y) * rowSize;
  pixels.set(readbackBuffer.subarray(srcRow, srcRow + rowSize), dstRow);
}
```

## Related Warnings

The NOPAINT interference warning is also appearing:
```
🚫 NOPAINT INTERFERENCE: Attempting to broadcast "resized" during nopaint operation
```

This may be a timing issue where resize events fire during KidLisp paint operations.

## Files Changed

- [disk.mjs](system/public/aesthetic.computer/lib/disk.mjs#L5471) - Fixed null check in `form()` function

## Testing

To test the fix:
```bash
ac-ff1 cast '$bop'
```

Or visit: `https://aesthetic.computer/$bop`

## Additional Notes

- The blur shaders are in [gpu-effects.mjs](system/public/aesthetic.computer/lib/gpu-effects.mjs#L256-L320)
- CPU fallback blur exists in [graph.mjs](system/public/aesthetic.computer/lib/graph.mjs#L6221)
- The gray screen was caused by repeated paint failures preventing any valid pixels from rendering
