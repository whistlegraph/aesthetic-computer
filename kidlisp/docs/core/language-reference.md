# KidLisp

KidLisp is a minimal Lisp dialect designed for creating generative art and interactive experiences within the Aesthetic Computer system. It provides a simple yet powerful language for expressing visual and temporal patterns.

## 🗂️ Directory Structure

### Core Implementation
- **`system/public/aesthetic.computer/lib/kidlisp.mjs`** - The main KidLisp evaluator and runtime
- **`system/netlify/functions/store-kidlisp.mjs`** - Backend storage and retrieval API for KidLisp pieces

### Tools
- **`kidlisp/tools/`** - Development and analysis tools
  - `source-tree.mjs` - Analyzes embedded layer structure and performance characteristics
  - `get-source.mjs` - Simple source code fetcher
  - `README.md` - Tool documentation

### Documentation
- **`docs/kidlisp-embed-feature.md`** - Embedding system documentation
- **`docs/kidlisp-feed-feature.md`** - Feed system documentation
- **`reports/tape-command-research.md`** - Analysis of tape recording system and framerate issues

### Examples & Tests
- **`kidlisp/examples/`** - Example KidLisp pieces and patterns
- **`kidlisp/tests/`** - Test suite for the evaluator

## 🚀 Quick Start

### Running KidLisp
KidLisp pieces are executed within the Aesthetic Computer environment. The main entry point is through the `kidlisp.mjs` evaluator.

### Analyzing Pieces
Use the tools in `kidlisp/tools/` to analyze existing pieces:

```bash
# Analyze structure and performance
./kidlisp/tools/source-tree.mjs $cow

# Get raw source code
./kidlisp/tools/get-source.mjs $ican
```

Both tools require the dev server to be running (`npm run dev`) as they connect to the local KidLisp storage API.

## 📖 Language Reference

### Basic Syntax
KidLisp uses S-expressions (parentheses-based syntax):
```lisp
(function arg1 arg2 ...)
```

### Core Functions
The evaluator in `kidlisp.mjs` implements a comprehensive set of functions for:
- **Graphics**: `rect`, `circle`, `line`, `pixel`, `blur`, etc.
- **Transformations**: `zoom`, `suck`, `scroll`, `spin`, `shear` - pixel-level transformations
- **Animation**: `s()` timing expressions, `frame` counter
- **Interaction**: Mouse, keyboard, and touch input
- **Math**: Arithmetic, trigonometry, random numbers
- **Control**: Conditionals, loops, variables

#### Transformation Functions
KidLisp provides several pixel-level transformation functions that manipulate the screen buffer:

**`(suck strength [centerX] [centerY])`** - NEW! Radial pixel displacement
- `strength`: Positive values suck pixels toward center, negative pushes outward
- `centerX`, `centerY`: Optional center point (defaults to screen center)
- Uses fluid dynamics algorithms for natural, organic motion
- Maintains data through wrapping - pixels loop seamlessly at edges
- Examples:
  ```lisp
  (suck 1)      ; Gentle inward vortex effect
  (suck -2)     ; Strong outward expansion  
  (suck 0.5 100 50)  ; Subtle suck centered at (100,50)
  ```

**`(zoom factor)`** - Scale transformation
- Traditional geometric scaling with accumulation
- Factors > 1 zoom in, < 1 zoom out
- Example: `(zoom 1.1)` for gradual zoom in

**`(scroll x y)`** - Translation transformation  
- Shifts pixels by given offset with wrapping
- Example: `(scroll 5 0)` moves right by 5 pixels

**`(spin angle)`** - Rotation transformation
- Rotates pixels around screen center
- Angle in degrees, positive = clockwise
- Example: `(spin 1)` for slow rotation

*For a complete API reference, see the evaluator source code which serves as the canonical documentation.*

## 🔧 Architecture

### Evaluator (`kidlisp.mjs`)
The main evaluator is a tree-walking interpreter that:
- Parses S-expressions into an AST
- Maintains execution context and variable bindings
- Provides built-in functions for graphics and interaction
- Handles embedded piece loading and execution
- Manages performance optimization for real-time rendering

### Storage Backend (`store-kidlisp.mjs`)
The storage system provides:
- **POST** - Store new KidLisp pieces
- **GET `?code=nanoid`** - Retrieve single piece by ID
- **GET `?codes=id1,id2,id3`** - Batch retrieve multiple pieces
- Persistent storage with nanoid-based addressing

### Embedding System
KidLisp supports embedding other pieces as layers:
```lisp
($other-piece-id arg1 arg2)
```
This enables composition and reusability while maintaining performance through caching.

## 🌪️ Suck Function - Technical Implementation

The `suck` function represents a major advancement in KidLisp's transformation capabilities, implementing fluid dynamics principles for organic pixel displacement.

### Mathematical Foundation
- **Polar Coordinates**: Converts (x,y) to (r,θ) for radial operations
- **Irrotational Vortex**: Uses logarithmic falloff: `displacement = strength * log(r) / r`
- **Wrapping Behavior**: Pixels loop seamlessly at edges, preserving data
- **Mask Support**: Respects active mask boundaries for localized effects

### Integration Details
- **Deferred Execution**: Like `zoom` and `scroll`, respects embedded layer timing
- **Accumulation Pattern**: Follows graph.mjs patterns for consistent behavior  
- **Performance**: Optimized nearest-neighbor sampling with unrolled RGBA copying
- **Safety**: Buffer detachment protection with automatic recreation

### Usage Patterns
```lisp
; Basic vortex effects
(suck 1)           ; Inward spiral
(suck -1)          ; Outward explosion

; Animated sequences  
(s1 (suck 0.5))    ; Gentle pulse every second
(s0.1 (suck 0.1))  ; Rapid micro-movements

; Combined with other transforms
(scroll 1 0)       ; Move right
(suck 0.2)         ; Add subtle vortex

; Localized effects with masks
(mask 50 50 100 100)  ; Define region
(suck 2)              ; Strong effect in masked area
(unmask)              ; Return to full screen
```

### Technical Notes
- Currently uses nearest-neighbor sampling (bilinear interpolation planned)
- Center point defaults to screen/mask center
- Strength parameter affects displacement intensity exponentially
- Compatible with all existing graph.mjs functions and timing systems

## 🛠️ Development

### Tools
The `kidlisp/tools/` directory contains utilities for:
- **Source Analysis** - Understanding piece structure and dependencies
- **Performance Profiling** - Identifying expensive operations
- **Debugging** - Inspecting piece execution

### Testing
Run the test suite to validate evaluator behavior:
```bash
# TODO: Add test runner instructions
```

### Contributing
When modifying the evaluator:
1. Test with existing pieces to ensure compatibility
2. Update this documentation for new features
3. Add examples for new language constructs
4. For new transformation functions, follow the established patterns:
   - Add to `graph.mjs` with mask support and wrapping behavior
   - Add to `kidlisp.mjs` with deferred execution support
   - Include in embedded layer special function lists
   - Test with both immediate and deferred execution contexts

**Recent Additions (2025-09-06):**
- `suck` function: Radial pixel displacement with fluid dynamics algorithms

## 📚 Related Documentation

- [Embedding System](docs/kidlisp-embed-feature.md)
- [Feed System](docs/kidlisp-feed-feature.md)
- [Tools Documentation](kidlisp/tools/README.md)
- [Architecture Analysis Reports](reports/) - Various architectural deep-dives

## 🎯 Philosophy

KidLisp embodies the aesthetic computer philosophy of simplicity and expressiveness. It provides just enough power to create compelling visual experiences while remaining approachable for beginners and powerful for experts.

The language is designed to feel immediate and responsive, with changes reflected in real-time. This makes it ideal for live coding, artistic exploration, and interactive installations.
