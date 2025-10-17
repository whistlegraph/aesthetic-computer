# `fill`

## Overview
Sets the global drawing mode to **fill** for all subsequent shape drawing operations.

## Syntax
```lisp
(fill)
```

## Parameters
None

## Returns
`undefined`

## Description
The `fill` command switches the global drawing mode to filled shapes. After calling `fill`, all shapes drawn with `circle`, `box`, `tri`, and other primitives will be rendered as solid filled shapes until the mode is changed with `outline` or explicitly overridden with a mode parameter.

This command is inspired by Processing's stateful drawing API, making it familiar to creative coders.

## Default Behavior
- KidLisp starts in **fill** mode by default (matches box's default behavior)
- Calling `fill` explicitly ensures fill mode is active
- The mode persists across frames until explicitly changed
- Individual shapes can override the global mode with explicit parameters

## Aliases
- `nostroke` - Processing-style alias for `fill`

## Examples

### Basic Fill Mode
```lisp
(fill)
(circle 100 100 50)  ; Filled circle
(box 150 150 60 60)  ; Filled rectangle
```

### Switching Between Modes
```lisp
(fill)
(circle 100 100 50)  ; Filled

(outline)
(circle 200 100 50)  ; Outlined

(fill)
(circle 300 100 50)  ; Filled again
```

### Explicit Override
```lisp
(fill)  ; Set global to fill
(circle 100 100 50)           ; Filled (uses global)
(circle 200 100 50 "outline") ; Outlined (explicit override)
(circle 300 100 50)           ; Filled (back to global)
```

### Concentric Circles
```lisp
(ink "blue")
(fill)
(circle 200 200 100)

(ink "white")
(outline)
(circle 200 200 80)

(ink "red")
(fill)
(circle 200 200 60)
```

### Animation Pattern
```lisp
; Expanding filled circle
(let ((size (* 50 (+ 1 (sin (/ ticks 30))))))
  (fill)
  (ink "purple")
  (circle 200 200 size))
```

## Related Commands
- `outline` - Switch to outline mode
- `stroke` - Alias for `outline`
- `nofill` - Alias for `outline`
- `circle`, `box`, `tri` - Shape commands that respect fill/outline mode

## Notes
- The fill mode is the **default state** in KidLisp (matches box's default)
- Individual shapes can always override with explicit `"fill"` or `"outline"` parameters
- The mode is reset to `fill` when the KidLisp interpreter resets (e.g., when switching pieces)
- Thickness can still be controlled for outlined shapes using `"outline:N"` notation

## See Also
- [outline.md](./outline.md) - Switch to outline mode
- [circle.md](./circle.md) - Circle drawing with mode support
- [box.md](./box.md) - Rectangle drawing with mode support
- [tri.md](./tri.md) - Triangle drawing with mode support
