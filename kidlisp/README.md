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
The evaluator in `kidlisp.mjs` implements a comprehensive set of **118 functions** across 12 categories:

- **Graphics**: `wipe`, `ink`, `line`, `box`, `circle`, `tri`, `plot`, `flood`, `shape`
- **Transformations**: `zoom`, `scroll`, `spin`, `blur`, `contrast` - 11 pixel-level effects
- **Math**: `+`, `-`, `*`, `/`, `sin`, `cos`, `random`, `wiggle` - 14 mathematical operations  
- **3D Graphics**: `cube`, `form`, `trans` - 8 three-dimensional functions
- **Audio**: `mic`, `melody`, `overtone` - 6 sound processing functions
- **System**: `width`, `height`, `frame`, `clock`, `fps` - 9 system properties
- **Colors**: All CSS colors plus `rainbow`, `zebra` - 19 color functions
- **Control**: `def`, `later`, `if`, `once`, `repeat` - 7 language constructs

#### Graphics & Transformations
KidLisp provides 11 pixel-level transformation functions:

- **`(scroll x y)`** - Translation with wrapping  
- **`(zoom factor)`** - Scale transformation with accumulation
- **`(spin angle)`** - Rotation around screen center
- **`(blur amount)`** - Gaussian blur effect
- **`(contrast level)`** - Contrast adjustment
- **`(blur amount)`** - Gaussian blur effect

Examples:
```lisp
(scroll 5 0)        ; Move right by 5 pixels
(zoom 1.1)          ; Gradual zoom in
(spin 1)            ; Slow rotation
(blur 2)            ; Soft blur effect
```

*For a complete API reference with all 118 functions, see [`COMPLETE_API_MAP.md`](COMPLETE_API_MAP.md).*

*For detailed transformation documentation, see [`docs/suck-complete-technical-guide.md`](docs/suck-complete-technical-guide.md).*

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

## 🔄 Transformation Functions

KidLisp includes 11 transformation functions for pixel manipulation:

| Function | Purpose | Example |
|----------|---------|---------|
| `scroll` | Move pixels with wrapping | `(scroll 5 0)` |
| `zoom` | Scale from center | `(zoom 1.1)` |
| `spin` | Rotate canvas | `(spin 1)` |
| `blur` | Gaussian blur | `(blur 2)` |
| `contrast` | Adjust contrast | `(contrast 1.5)` |
| `suck` | Radial displacement | `(suck 0.5)` |
| `pan` | Camera movement | `(pan 10 5)` |
| `sort` | Sort pixels by brightness | `(sort)` |
| `resetSpin` | Reset rotation | `(resetSpin)` |
| `smoothspin` | Smooth rotation | `(smoothspin 0.1)` |
| `unpan` | Reset camera | `(unpan)` |

```lisp
; Basic transformation examples
(scroll 5 0)        ; Move right by 5 pixels
(zoom 1.1)          ; Gradual zoom in
(spin 1)            ; Slow rotation
(blur 2)            ; Soft blur effect
(suck 0.5)          ; Radial displacement
```

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
- Complete documentation organization with 118-function API map
- Development tools for API analysis and function discovery

## 🚀 Summary

KidLisp is a comprehensive creative coding language with **118 functions** designed for real-time visual art. Key strengths include:

- **Rich API**: 12 function categories covering graphics, math, 3D, audio, and system control
- **Live coding**: Interactive development with immediate visual feedback  
- **Advanced transformations**: 11 pixel-level effects for visual manipulation
- **Embedded systems**: Code composition and layer management
- **Performance**: Optimized evaluation with caching and deferred execution

### Quick Start
```lisp
(wipe "navy")                    ; Set background
(ink "yellow")                   ; Set drawing color  
(repeat 50 i                     ; Draw 50 circles
  (circle (wiggle width) (wiggle height) 10))
```

### Resources
- **[Complete API Map](COMPLETE_API_MAP.md)** - All 118 functions categorized
- **[Function Drilldowns](docs/functions/)** - Detailed guides for specific functions
- **[Development Tools](tools/)** - Analysis and debugging utilities
- **[Technical Reports](reports/)** - Implementation research and comparisons

## 📚 Related Documentation

- [Embedding System](docs/features/embedding-system.md)
- [Feed System](docs/features/feed-system.md)  
- [Architecture Analysis Reports](reports/) - Technical deep-dives and comparisons

## 🎯 Philosophy

KidLisp embodies the aesthetic computer philosophy of simplicity and expressiveness. It provides just enough power to create compelling visual experiences while remaining approachable for beginners and powerful for experts.

The language is designed to feel immediate and responsive, with changes reflected in real-time. This makes it ideal for live coding, artistic exploration, and interactive installations.

*KidLisp: Expressive creative coding for the Aesthetic Computer platform.*
