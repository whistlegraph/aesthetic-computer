# scroll - Pixel Translation Function

*Basic pixel movement with edge wrapping*

## Syntax
```lisp
(scroll dx dy)     ; Move pixels by dx, dy
(scroll dx)        ; Move horizontally by dx only
(scroll)           ; Random direction (sticky per session)
```

## Parameters
- **dx**: Horizontal displacement in pixels
- **dy**: Vertical displacement in pixels (optional, defaults to 0)

## Behavior
- Moves all screen pixels by the specified offset
- Pixels that move off one edge wrap to the opposite edge
- Creates seamless scrolling effect with no data loss
- Random direction mode chooses once and sticks to it per session

## Examples

### Basic Movement
```lisp
(scroll 5 0)       ; Move right by 5 pixels
(scroll 0 -3)      ; Move up by 3 pixels
(scroll -2 1)      ; Move left 2, down 1
```

### Animation Patterns
```lisp
(s0.1 (scroll 1 0))  ; Continuous rightward scroll
["2s..." 1 0 -1]     ; Timing-based directional changes
```

### Combined Effects
```lisp
(blur 1)           ; Soften edges
(scroll 2 0)       ; Then scroll
(contrast 1.1)     ; Enhance result
```

## Implementation Notes
- Uses pixel-perfect translation with no interpolation
- Deferred execution compatible with embedded layers
- Works with mask regions for selective scrolling
- Optimized for real-time performance

## Related Functions
- `pan` - Camera movement (different from pixel scrolling)
- `zoom` - Scale transformation
- `spin` - Rotation transformation
