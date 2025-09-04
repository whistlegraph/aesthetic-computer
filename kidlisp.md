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
- **Graphics**: `rect`, `circle`, `line`, `pixel`, `blur`, `zoom`, etc.
- **Animation**: `s()` timing expressions, `frame` counter
- **Interaction**: Mouse, keyboard, and touch input
- **Math**: Arithmetic, trigonometry, random numbers
- **Control**: Conditionals, loops, variables

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

## 📚 Related Documentation

- [Embedding System](docs/kidlisp-embed-feature.md)
- [Feed System](docs/kidlisp-feed-feature.md)
- [Tools Documentation](kidlisp/tools/README.md)
- [Architecture Analysis Reports](reports/) - Various architectural deep-dives

## 🎯 Philosophy

KidLisp embodies the aesthetic computer philosophy of simplicity and expressiveness. It provides just enough power to create compelling visual experiences while remaining approachable for beginners and powerful for experts.

The language is designed to feel immediate and responsive, with changes reflected in real-time. This makes it ideal for live coding, artistic exploration, and interactive installations.
