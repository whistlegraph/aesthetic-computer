# Invert Function Implementation

This document describes the implementation of the `invert` function for KidLisp.

## Summary

Added the missing `invert` function to the graphics system, with both CPU and GPU implementations. The function inverts RGB color values (255 - value) while preserving the alpha channel, following the same pattern as other image effects like `contrast` and `brightness`.

## Implementation Details

### CPU Implementation (graph.mjs)

- Added `invert()` function that processes pixel buffer
- Performs RGB inversion: `RGB' = 255 - RGB`
- Preserves alpha channel unchanged
- Supports masking for partial screen effects
- Skips fully transparent pixels (optimization)
- Uses performance tracking for monitoring

### GPU Implementation (gpu-effects.mjs)

- Added `INVERT_FRAGMENT_SHADER` with simple inversion logic
- Created `gpuInvert()` function with same signature as other GPU effects
- Compiled and cached shader program and uniform locations
- Follows GPU-first, CPU-fallback pattern used by other effects
- Properly handles cleanup in `cleanupGpuEffects()`

### Integration

- Exported `invert` from graph.mjs
- Added to disk.mjs API (already present, was just calling non-existent function)
- KidLisp can now call `(invert)` in code
- Works with embedded layers and baking system (uses existing post-composite infrastructure)

## Usage Examples

### KidLisp Code

```lisp
; Simple invert
(wipe "red")
(invert)
; Screen is now cyan

; Partial invert with masking
(wipe "white")
(ink "blue")
(box 50 50 100 100)
(mask 60 60 80 80)
(invert)
(unmask)
; Only the masked region is inverted

; Animation with invert
(wipe "black")
(ink "yellow")
(circle width/2 height/2 50)
(if (even frame) (invert))
; Flashing circle effect
```

## Testing

- Created `tests/invert.test.mjs` with comprehensive logic tests
- Verified RGB inversion formula for various colors
- Tested alpha channel preservation
- Confirmed double invert restores original values
- Validated boundary cases

## Performance

- GPU implementation provides hardware acceleration when available
- CPU fallback ensures compatibility on all platforms
- Transparent pixel skipping reduces unnecessary computation
- Performance tracking integrated for monitoring

## Related Issues

This implementation enables KidLisp codes like `$beli` (similar to `$4bb` but with invert) to work correctly.
