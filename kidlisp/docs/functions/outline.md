# `outline`

## Overview
Sets the global drawing mode to **outline** for all subsequent shape drawing operations.

## Syntax
```lisp
(outline)
```

## Parameters
None

## Returns
`undefined`

## Description
The `outline` command switches the global drawing mode to outlined shapes. After calling `outline`, all shapes drawn with `circle`, `box`, `tri`, and other primitives will be rendered as hollow outlines until the mode is changed with `fill` or explicitly overridden with a mode parameter.

This is the **default mode** in KidLisp, ensuring backward compatibility with existing code.

This command is inspired by Processing's stateful drawing API, making it familiar to creative coders.

## Default Behavior
- KidLisp starts in **fill** mode by default (matches box's default behavior)
- Calling `outline` explicitly switches to outline mode
- The mode persists across frames until explicitly changed
- Individual shapes can override the global mode with explicit parameters

## Aliases
- `stroke` - Processing-style alias for `outline`
- `nofill` - Processing-style alias for `outline`

## Examples

### Basic Outline Mode
```lisp
(outline)  ; Explicit, though this is the default
(circle 100 100 50)  ; Outlined circle
(box 150 150 60 60)  ; Outlined rectangle
```

### Switching Between Modes
```lisp
(fill)
(circle 100 100 50)  ; Filled

(outline)
(circle 200 100 50)  ; Outlined

(fill)
(circle 300 100 50)  ; Filled

(outline)
(circle 400 100 50)  ; Outlined again
```

### Explicit Override
```lisp
(outline)  ; Set global to outline
(circle 100 100 50)        ; Outlined (uses global)
(circle 200 100 50 "fill") ; Filled (explicit override)
(circle 300 100 50)        ; Outlined (back to global)
```

### Variable Thickness Outlines
```lisp
(outline)
(circle 100 100 50 "outline:1")
(circle 200 100 50 "outline:3")
(circle 300 100 50 "outline:5")
(circle 400 100 50 "outline:10")
```

### Neon Effect
```lisp
(ink "black")
(fill)
(box 0 0 width height)  ; Black background

(ink "cyan")
(outline)
(circle 200 200 100 "outline:8")
(circle 200 200 80 "outline:5")
(circle 200 200 60 "outline:3")
(circle 200 200 40 "outline:1")
```

### Animation Pattern
```lisp
; Pulsing outline thickness
(let ((thickness (* 5 (+ 1 (sin (/ ticks 20))))))
  (outline)
  (ink "green")
  (circle 200 200 80 (str "outline:" thickness)))
```

## Related Commands
- `fill` - Switch to fill mode
- `nostroke` - Alias for `fill`
- `circle`, `box`, `tri` - Shape commands that respect fill/outline mode

## Notes
- Fill mode is the **default** in KidLisp (matches box's default behavior)
- Call `outline` to switch from fill to outline mode
- Individual shapes can always override with explicit `"fill"` or `"outline"` parameters
- Thickness can be controlled using `"outline:N"` notation (e.g., `"outline:5"` for 5px thick)
- The mode is reset to `fill` when the KidLisp interpreter resets

## See Also
- [fill.md](./fill.md) - Switch to fill mode
- [circle.md](./circle.md) - Circle drawing with mode support
- [box.md](./box.md) - Rectangle drawing with mode support
- [tri.md](./tri.md) - Triangle drawing with mode support
