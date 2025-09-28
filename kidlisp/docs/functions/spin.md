# spin - Canvas Rotation Function

*Rotate canvas around center point*

## Syntax
```lisp
(spin angle)           ; Rotate by angle in degrees
(resetSpin)            ; Reset rotation to zero
(smoothspin angle)     ; Smooth rotation animation
```

## Parameters
- **angle**: Rotation amount in degrees (positive = clockwise)

## Behavior
- Rotates entire canvas around the center point
- Accumulates rotation over multiple calls
- Smooth interpolation maintains visual quality
- Can be reset to zero with `resetSpin`

## Examples

### Basic Rotation
```lisp
(spin 90)          ; Quarter turn clockwise
(spin -45)         ; 45 degrees counter-clockwise
(spin 0.5)         ; Very slow rotation
```

### Animation Patterns
```lisp
(s0.1 (spin 1))       ; Continuous slow spin
(s1 (spin 15))        ; Discrete jumps every second
(repeat 360 i (spin 1)) ; Full rotation in steps
```

### Combined Effects
```lisp
(zoom 1.02)        ; Slight zoom
(spin 2)           ; Plus rotation
(blur 0.5)         ; Soft edges
```

### Reset and Control
```lisp
(spin 45)          ; Rotate 45 degrees
(resetSpin)        ; Back to zero
(smoothspin 30)    ; Smooth animated rotation
```

## Implementation Notes
- Uses optimized rotation matrices for performance
- Anti-aliasing prevents jagged edges during rotation
- Compatible with other transformations
- Deferred execution for embedded layers

## Related Functions
- `zoom` - Scale transformation
- `scroll` - Translation transformation
- `resetSpin` - Reset rotation state
- `smoothspin` - Animated rotation variant
