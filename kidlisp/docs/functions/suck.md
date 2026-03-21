# suck - Radial Displacement Function

*Radial pixel displacement with fluid dynamics*

## Syntax
```lisp
(suck strength)                ; Radial effect from screen center
(suck strength centerX centerY) ; Radial effect from custom point
```

## Parameters
- **strength**: Displacement intensity (positive = inward, negative = outward)
- **centerX, centerY**: Optional center point (defaults to screen center)

## Behavior
- Moves pixels radially toward or away from center point
- Uses polar coordinate transformation for smooth circular patterns
- Positive strength creates inward vortex effect
- Negative strength creates outward expansion effect

## Examples

### Basic Effects
```lisp
(suck 1)           ; Strong inward vortex
(suck -0.5)        ; Gentle outward flow
(suck 0.1)         ; Subtle breathing effect
```

### Animation Patterns
```lisp
(s1 (suck 0.5))    ; Gentle pulse every second
(s0.1 (suck 0.1))  ; Rapid micro-movements
["2s..." 1 0 -1]   ; Multi-phase timing with varying strength
```

### Combined Transformations
```lisp
(blur 1)           ; Soften edges first
(suck 0.2)         ; Add subtle vortex
(contrast 1.2)     ; Enhance final result
```

### Custom Center Points
```lisp
(suck 0.5 100 100)    ; Vortex at specific coordinate
(suck -0.3 width height) ; Outward from bottom-right
```

## Implementation Notes
- Uses polar coordinate mathematics for smooth radial displacement
- Bilinear interpolation provides anti-aliasing
- Accumulation system prevents excessive computation
- Compatible with embedded layer system and mask regions
- Optimized for real-time performance

## Related Functions
- `scroll` - Translation transformation
- `zoom` - Scale transformation  
- `spin` - Rotation transformation
- `blur` - Blur effect (often combined with suck)
