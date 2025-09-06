# zoom - Scale Transformation Function

*Scale transformation from center point*

## Syntax
```lisp
(zoom factor)           ; Scale from screen center
(zoom factor x y)       ; Scale from custom center point
```

## Parameters
- **factor**: Scale multiplier (>1 = zoom in, <1 = zoom out)
- **x, y**: Center point for scaling (optional, defaults to screen center)

## Behavior
- Scales all pixels toward or away from the center point
- Uses accumulation system for smooth continuous zooming
- Maintains aspect ratio and pixel relationships
- Values above 1.0 zoom in, below 1.0 zoom out

## Examples

### Basic Scaling
```lisp
(zoom 1.1)         ; Gradual zoom in
(zoom 0.9)         ; Gradual zoom out
(zoom 2.0)         ; 2x magnification
```

### Custom Center Point
```lisp
(zoom 1.5 100 100)    ; Zoom into point (100,100)
(zoom 0.8 width height) ; Zoom out from bottom-right
```

### Animation Sequences
```lisp
(s0.1 (zoom 1.01))    ; Slow continuous zoom
["3s..." 1.1 0.9]     ; Alternating zoom in/out
```

### Combined Transformations
```lisp
(zoom 1.05)        ; Scale slightly
(spin 0.5)         ; Add rotation
(scroll 1 0)       ; Add movement
```

## Implementation Notes
- Accumulation threshold prevents excessive computation
- Bicubic interpolation for smooth results
- Compatible with embedded layer system
- Efficient real-time performance

## Related Functions
- `scroll` - Translation transformation
- `spin` - Rotation transformation
- `suck` - Radial displacement transformation
