// KidLisp Reference Documentation
// Extracted from kidlisp.mjs — this file is NOT bundled into client builds.
// It serves as a reference for LLMs, editors, and developer documentation.

/* #region 🤖 LLM API SPECIFICATION
   This section provides a structured specification for Large Language Models
   to understand and generate valid KidLisp code for Aesthetic Computer pieces.

   ## KidLisp Language Overview

   KidLisp is a Lisp-based language for creating interactive visual art and animations.
   It uses S-expressions (parenthesized lists) where the first element is typically
   a function name followed by arguments.

   Basic syntax: (function-name arg1 arg2 arg3)
   Comments: ; This is a comment
   Multi-line: Expressions can span multiple lines

   ## Core Graphics Functions

   ### Screen Management
   - `(wipe color)` - Clear entire screen with specified color
   - `(resolution width height)` - Set canvas resolution
   - `(scroll)` - Randomly choose and stick to one direction (up/down/left/right) per session
   - `(scroll dx dy)` - Scroll by dx horizontally and dy vertically
   - `(scroll dx)` - Scroll by dx horizontally only

   ### Drawing Primitives
   - `(ink color)` - Set drawing color for subsequent operations
   - `(ink r g b)` - Set RGB color (0-255 each)
   - `(ink color alpha)` - Set color with transparency (0-255)
   - `(line x1 y1 x2 y2)` - Draw line from point 1 to point 2
   - `(box x y width height)` - Draw rectangle (respects fill/outline mode)
   - `(circle x y radius)` - Draw circle (respects fill/outline mode)
   - `(tri x1 y1 x2 y2 x3 y3)` - Draw triangle (respects fill/outline mode)
   - `(plot x y)` - Set single pixel at coordinates

   ### Fill/Outline Mode (Processing-style)
   - `(fill)` - Set global fill mode (shapes will be filled) - DEFAULT
   - `(outline)` - Set global outline mode (shapes will be outlined)
   - `(stroke)` - Alias for outline (Processing compatibility)
   - `(nofill)` - Alias for outline (Processing compatibility)
   - `(nostroke)` - Alias for fill (Processing compatibility)
   - Mode affects circle, box, tri, and shape commands
   - Explicit mode parameters override global state:
     - `(circle x y r "fill")` - Always filled, regardless of global mode
     - `(circle x y r "outline")` - Always outline, regardless of global mode
     - `(circle x y r "outline:5")` - Outline with 5px thickness
     - Same pattern works for box and tri
   - Example workflow:
     ```lisp
     ; Default is fill mode (matches box's default)
     (circle 100 100 50)     ; Filled circle
     (box 50 50 40 40)       ; Filled box
     (outline)               ; Switch to outline mode
     (circle 200 100 50)     ; Outline circle
     (tri 0 0 50 0 25 50)   ; Outline triangle
     ```

   ### Image Functions
   - `(paste url x y)` - Paste image from URL at coordinates
   - `(paste url x y scale)` - Paste image with scaling factor
   - `(stamp url x y)` - Paste image centered at coordinates
   - URLs can be unquoted: `(paste https://example.com/image.png x y)`
   - Quoted URLs also work: `(paste "https://example.com/image.png" x y)`
   - Supports @handle/timestamp format: `(paste @user/123456 x y)`

   ### String Literals
   - Both double quotes `"text"` and single quotes `'text'` work for strings
   - Example: `(write "hello" x y)` or `(write 'hello' x y)`
   - Useful for avoiding escaping when mixing quote types

   ### Color System
   Colors can be specified as:
   - Named colors: "red", "blue", "lime", "orange", "purple", etc.
   - RGB values: (ink 255 0 0) for red
   - With transparency: (ink "red" 128) for 50% transparent red

   ## Animation & Timing

   ### Timing Expressions
   - `1s` - Execute after 1 second
   - `2s...` - Execute every 2 seconds (repeating)
   - `0.5s!` - Execute once after 0.5 seconds

   ### Dynamic Values
   - `(wiggle amount)` - Random variation (±amount/2)
   - `width` and `height` - Canvas dimensions
   - Frame-based animation through re-evaluation

   ## Variables & Logic

   ### Variable Definition
   - `(def name value)` - Define a variable
   - `(def x 10)` - Set x to 10
   - `(def color "red")` - Set color variable

   **Identifier Naming Rules:**
   - Must start with letter (a-z, A-Z) or underscore (_)
   - Can contain letters, digits (0-9), and underscores
   - **Cannot contain dashes/hyphens (-)** - these are parsed as subtraction
   - Examples: `myVar`, `line_width`, `color2` ✅
   - Invalid: `my-var`, `line-width` ❌ (parsed as subtraction)

   ### Math Operations
   - `(+ a b c)` - Addition (can take multiple arguments)
   - `(- a b)` - Subtraction
   - `(* a b c)` - Multiplication
   - `(/ a b)` - Division

   ### Function Definition
   - `(later name param1 param2 body...)` - Define reusable function
   - `(later cross x y (line (- x 10) (- y 10) (+ x 10) (+ y 10)))`
   - Call with: `(cross 50 50)`

   ### Repetition
   - `(repeat count expression...)` - Execute expressions multiple times
   - `(repeat count iterator expression...)` - Repeat with iterator variable
   - `(bunch count expression...)` - Alias for repeat (shorter, playful)
   - Examples:
     - `(repeat 10 (circle (wiggle width) (wiggle height) 5))`
     - `(repeat height i (ink rainbow) (line 0 i width i))`
     - `(bunch 100 (plot (wiggle width) (wiggle height)))`

   ## Advanced Features

   ### Navigation
   - `(jump "piece-name")` - Jump to another Aesthetic Computer piece
   - `(jump "https://example.com")` - Jump to external URL
   - `(jump $abc123)` - Jump to cached KidLisp piece by ID
   - Automatically resets KidLisp state and loads the new piece

   ### One-time Execution
   - `(once expression)` - Execute only once, not every frame
   - Useful for setup code: `(once (wipe "black"))`

   ### Background Layers
   - `(bake)` - Render current drawing to background layer
   - `(once (bake))` - Bake background once for layered effects

   ### Embedded Code
   - `($codeId)` - Execute cached code by ID
   - `($codeId width height)` - Execute in custom buffer size
   - `(embed $codeId x y width height alpha)` - Advanced embedding

   ### Tape Video Embeds
   - `(tape !CODE)` - Play tape fullscreen
   - `(tape !CODE x y)` - Play at position with original dimensions
   - `(tape !CODE x y w h)` - Play at position with specified dimensions
   - `(tape !CODE x y w h speed)` - Play with speed multiplier (1.0 = normal)

   ## Example Patterns for LLMs

   ### Simple Drawing
   ```kidlisp
   (wipe "black")
   (ink "red")
   (circle 50 50 30)
   (ink "blue")
   (box 100 100 50 50)
   (ink "yellow")
   (tri 150 50 180 100 120 100)
   ```

   ### Animated Scene
   ```kidlisp
   (wipe "navy")
   (ink "yellow")
   (circle (+ 100 (wiggle 20)) (+ 100 (wiggle 20)) 10)
   (ink "white" 100)
   (box 0 (+ 150 (wiggle 5)) width 2)
   ```

   ### Interactive Functions
   ```kidlisp
   (later star x y size
     (ink "yellow")
     (circle x y size)
     (ink "white")
     (circle x y (/ size 2)))

   (star 100 100 20)
   (star 200 150 15)
   ```

   ### Image Pasting with URLs
   ```kidlisp
   (wipe "black")
   ; Unquoted URLs are now supported!
   (paste https://example.com/image.png 50 50 0.5)
   (stamp https://example.com/logo.png (/ width 2) 100)
   ; Quoted URLs still work
   (paste "https://example.com/background.jpg" 0 0 1.0)
   ```

   ### Timed Animations
   ```kidlisp
   (once (wipe "black"))
   1s (ink "red") (circle 100 100 50)
   2s (ink "blue") (box 150 150 40 40)
   3s... (ink (wiggle 255) (wiggle 255) (wiggle 255)) (plot (wiggle width) (wiggle height))
   ```

   ### Repetition Patterns
   ```kidlisp
   ; Draw many random stars
   (wipe "black")
   (ink "white")
   (bunch 50 (plot (wiggle width) (wiggle height)))

   ; Rainbow lines with iterator
   (bunch height i
     (ink rainbow)
     (line 0 i width i))

   ; Repeat also works (bunch is shorter alias)
   (repeat 100
     (ink (choose "red" "blue" "yellow"))
     (circle (wiggle width) (wiggle height) 5))
   ```

   ### Navigation & Jumps
   ```kidlisp
   (wipe "black")
   (ink "white")
   (write "Click to jump!" { center: "xy" })

   ; Jump to another piece after 3 seconds
   3s (jump "prompt")

   ; Or jump to a cached KidLisp piece
   ; 5s (jump $abc123)
   ```

   ## Best Practices for LLM Generation

   1. **Start with background**: Always begin with `(wipe color)` to set background
   2. **Set colors before drawing**: Use `(ink color)` before drawing primitives
   3. **Use meaningful coordinates**: Consider canvas size (typically 256x256 default)
   4. **Leverage animation**: Use `wiggle` for organic movement
   5. **Layer effects**: Use `(once (bake))` for background elements
   6. **Readable spacing**: Format code with proper indentation for readability
   7. **Combine timing**: Mix immediate drawing with timed animations
   8. **Reuse patterns**: Define functions with `later` for repeated elements

   ## Error Patterns to Avoid

   - Don't forget parentheses around expressions
   - Don't use undefined color names (stick to CSS colors or RGB values)
   - Don't reference undefined variables before `def`
   - Don't mix coordinate systems (stay consistent with canvas bounds)
   - Don't create infinite loops without timing delays
   - **Don't use dashes in identifiers** - use underscores instead (mouth_y not mouth-y)

#endregion */

/* #region 📚 Examples / Notebook */
// See kidlisp.mjs git history for working program examples and conceptual programs.
/* #endregion */
