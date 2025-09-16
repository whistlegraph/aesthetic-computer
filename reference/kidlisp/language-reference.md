# KidLisp Language Reference

*Documentation for the KidLisp programming language in Aesthetic Computer*

## Overview

KidLisp is a Lisp-based visual programming language designed for creating interactive art and animations in Aesthetic Computer. It provides a simple, expressive syntax for graphics, animation, and user interaction.

## Basic Syntax

### S-Expressions
KidLisp uses S-expressions (parenthesized lists) where the first element is a function name followed by arguments:

```lisp
(function-name arg1 arg2 arg3)
```

### Comments
```lisp
; This is a comment
; Comments start with semicolon and go to end of line
```

### Multi-line Expressions
Expressions can span multiple lines for readability:
```lisp
(box 10 20 
     100 150 
     "outline")
```

## Core Language Features

### Variables
Define variables with `def`:
```lisp
(def x 10)
(def color "red")
(def size 50)
```

**Important**: Variable names cannot contain dashes/hyphens (-) as they are parsed as subtraction:
- ✅ Valid: `myVar`, `line_width`, `color2`
- ❌ Invalid: `my-var`, `line-width` (parsed as subtraction)

### Functions
Define reusable functions with `later`:
```lisp
(later square x y size
  (box x y size size))

; Call the function
(square 50 50 100)
```

### Math Operations
```lisp
(+ 5 3 2)     ; Addition: 10
(- 10 3)      ; Subtraction: 7  
(* 4 5 2)     ; Multiplication: 40
(/ 20 4)      ; Division: 5
```

## Graphics Functions

### Screen Management
```lisp
(wipe "black")              ; Clear screen with color
(wipe 255 0 0)             ; Clear with RGB red
(resolution 800 600)        ; Set canvas size
```

### Drawing Colors
```lisp
(ink "red")                ; Set color by name
(ink 255 0 0)             ; Set RGB color
(ink "blue" 128)          ; Set color with transparency
(ink 100 200 255 180)     ; RGBA color
```

### Drawing Primitives
```lisp
(line 10 10 90 90)                    ; Draw line
(box 50 50 100 75)                    ; Draw rectangle
(circle 100 100 50)                   ; Draw circle
(tri 50 10 10 90 90 90)              ; Draw triangle
(tri 50 10 10 90 90 90 "outline")    ; Triangle outline
(plot 100 200)                       ; Set single pixel
```

### Images
```lisp
(paste "https://example.com/image.png" 0 0)     ; Paste image at position
(paste @user/123456 50 50)                      ; Paste from user timestamp
(stamp "image.png" 100 100)                     ; Paste centered at position
(paste "image.png" 0 0 0.5)                    ; Paste with 50% scale
```

## Animation & Timing

### Timing Expressions
```lisp
1s      ; Execute after 1 second
2s...   ; Execute every 2 seconds (repeating)
0.5s!   ; Execute once after 0.5 seconds
```

### Dynamic Values
```lisp
(wiggle 10)     ; Random variation (±5)
width           ; Canvas width
height          ; Canvas height
```

### Animation Example
```lisp
(def x 0)

; Move box across screen
(def x (+ x 1))
(box x 100 50 50)

; Reset when reaching edge
(def x (% x width))
```

## Color System

### Named Colors
```lisp
"red" "blue" "green" "yellow" "orange" "purple" 
"pink" "cyan" "magenta" "lime" "brown" "gray"
"black" "white"
```

### RGB Values
```lisp
(ink 255 0 0)      ; Pure red
(ink 0 255 0)      ; Pure green  
(ink 0 0 255)      ; Pure blue
(ink 128 128 128)  ; Gray
```

### Transparency
```lisp
(ink "red" 128)           ; 50% transparent red
(ink 255 0 0 128)        ; 50% transparent red (RGBA)
```

## Advanced Features

### Control Flow
```lisp
; Conditional logic (basic comparison)
(def size (+ 50 (wiggle 20)))
```

### Loops and Repetition
```lisp
; Repeating patterns using timing
2s... (circle (wiggle width) (wiggle height) 20)
```

### Complex Compositions
```lisp
(later flower x y
  (circle x y 20)
  (circle (- x 15) (- y 10) 8)
  (circle (+ x 15) (- y 10) 8)
  (circle x (+ y 15) 12))

; Draw multiple flowers
(flower 100 100)
(flower 200 150)
(flower 300 200)
```

## Built-in Constants

### Screen Dimensions
- `width` - Canvas width in pixels
- `height` - Canvas height in pixels

### Math Constants
Access to JavaScript Math constants and functions.

## Examples

### Simple Animation
```lisp
(wipe "black")
(ink "cyan")
(def time (* frame 0.1))
(circle (+ 100 (* 50 (sin time))) 
        (+ 100 (* 30 (cos time))) 
        20)
```

### Interactive Drawing
```lisp
(wipe "navy")
(ink "yellow")
; Draw circles that follow mouse/touch
(circle mouseX mouseY 25)
```

### Geometric Pattern
```lisp
(wipe "black")
(ink "lime")

(later spiral angle radius
  (def x (+ (/ width 2) (* radius (cos angle))))
  (def y (+ (/ height 2) (* radius (sin angle))))
  (circle x y 5))

; Draw spiral pattern
(def angle 0)
(def angle (+ angle 0.2))
(spiral angle (* angle 2))
```

## Integration with JavaScript API

KidLisp programs run on top of the Aesthetic Computer JavaScript API. Functions like `wipe`, `ink`, `box`, etc. are provided by the underlying `disk.mjs` system.

## Language Status

**✅ Core Features Implemented**
- S-expression parsing and evaluation
- Variable definition and scoping
- Function definition with `later`
- Graphics primitives
- Math operations
- Timing expressions
- Image loading and display

**🚧 In Development**
- Advanced control flow
- List manipulation
- More built-in functions
- Error handling improvements
- Debugging tools

---

*This reference is extracted from the comprehensive LLM API specification embedded in kidlisp.mjs and represents the current state of the language.*