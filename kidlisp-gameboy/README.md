# KidLisp GameBoy Integration

This directory contains the complete KidLisp to GameBoy ROM compilation system.

## 🎯 Goal

Enable writing in a minimal version of KidLisp and compiling it to actual GameBoy ROMs with basic graphics primitives like:
- `(point x y)` - Plot a point at coordinates
- `(wipe)` - Clear screen (planned)
- `(line x1 y1 x2 y2)` - Draw lines (planned)

## 📁 Directory Structure

```
kidlisp-gameboy/
├── README.md                      # This file
├── kidlisp-gameboy-integration.md # Complete implementation plan
├── compiler/                      # Compilation tools
│   ├── kidlisp-gb-compiler-cli.mjs       # Node.js CLI compiler
│   └── kidlisp-gb-compiler-browser.mjs   # Browser-compatible parser
├── templates/                     # GameBoy assembly templates
│   └── boot.asm                   # Base ROM template with headers
└── test/                         # Test files and examples
    ├── kidlisp-gb-test.mjs       # Browser test piece
    ├── test-point.asm            # Generated assembly example
    └── minimal-test.asm          # Minimal test case
```

## 🚀 Current Status: Phase 1

✅ **Completed:**
- KidLisp parser for `(point x y)` expressions
- GameBoy assembly code generation
- CLI compiler using gbasm
- Browser test interface
- File organization

🔄 **In Progress:**
- Fixing gbasm assembly syntax
- ROM generation and testing

## 🛠️ Usage

### CLI Compiler
```bash
cd /workspaces/aesthetic-computer/kidlisp-gameboy
node compiler/kidlisp-gb-compiler-cli.mjs "(point 80 72)" output.gb
```

### Browser Test
Navigate to: `http://localhost:8888/kidlisp-gb-test`
(Note: Test file moved to this directory structure)

## 📋 Next Steps

1. Fix gbasm assembly syntax issues
2. Generate working GameBoy ROM
3. Test ROM in existing GameBoy emulator
4. Expand to support more KidLisp primitives
5. Integrate with main aesthetic.computer interface

## 🎮 Integration with Aesthetic Computer

The compiled ROMs will work with the existing GameBoy emulator piece at:
`system/public/aesthetic.computer/disks/gameboy.mjs`

This enables a complete workflow: KidLisp → Assembly → ROM → GameBoy Emulator.
