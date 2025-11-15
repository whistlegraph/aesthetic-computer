# GameBoy ROM Development

This directory contains tools for creating GameBoy ROMs using C (GBDK) and assembly.

## 🚀 Quick Start

**→ For C development, see [C-QUICKSTART.md](./C-QUICKSTART.md)** ← **START HERE**

**→ For Assembly development, see [QUICKSTART.md](./QUICKSTART.md)**

## 🎯 Current Projects

### C ROMs (GBDK)
- **wave-editor** - Interactive Channel 3 waveform editor (working ✅)
- **demo-graphics** - 4-channel audio visualization (working ✅)
- Various test ROMs in `src/`

### Assembly ROMs  
- Line drawing demos in `test/`
- Text scrolling examples

### Future: KidLisp Integration
Enable writing in a minimal version of KidLisp and compiling it to actual GameBoy ROMs with basic graphics primitives like:
- `(point x y)` - Plot a point at coordinates
- `(wipe)` - Clear screen (planned)
- `(line x1 y1 x2 y2)` - Draw lines (planned)


## 📁 Directory Structure

```
kidlisp-gameboy/
├── C-QUICKSTART.md               # ← START HERE for C development
├── QUICKSTART.md                 # Assembly development guide
├── README.md                     # This file
├── src/                          # C source files (wave-editor.c, demo-graphics.c, etc.)
├── gbdk/                         # GBDK toolchain (gitignored)
├── test/                         # Assembly test ROMs
├── compiler/                     # KidLisp compiler (future)
└── templates/                    # Assembly templates
```

## � Documentation

- **[C-QUICKSTART.md](./C-QUICKSTART.md)** - Complete C/GBDK workflow (RECOMMENDED)
- **[QUICKSTART.md](./QUICKSTART.md)** - Assembly workflow
- **[PROGRESS.md](./PROGRESS.md)** - Development history
- **[RESEARCH.md](./RESEARCH.md)** - Technical references
- **[WORKFLOW.md](./WORKFLOW.md)** - Detailed assembly workflow

## 🎮 Running ROMs

All ROMs can be run with:
```bash
cd /workspaces/aesthetic-computer
ac gameboy~<rom-name>
```

Examples: `ac gameboy~wave-editor`, `ac gameboy~demo-graphics`
