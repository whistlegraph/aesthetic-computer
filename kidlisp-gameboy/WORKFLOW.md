# KidLisp GameBoy - Current Status & Workflow

**Date**: November 12, 2025

## 🎯 Where We Are

You have **working Bresenham line drawing** in pure GameBoy Z80 assembly! The `line-demo.asm` draws an X pattern using the Bresenham algorithm.

### ✅ What's Working
- ✅ Bresenham line drawing algorithm (see BRESENHAM-SUCCESS.md)
- ✅ Pixel plotting using tile-based rendering
- ✅ Multiple demo ROMs (line-demo, ascii-scroll, bounce-ball, etc.)
- ✅ Build pipeline with gbasm
- ✅ GameBoy emulator integration in browser

### 🔨 Development Workflow

#### Build a ROM
```bash
cd /workspaces/aesthetic-computer/kidlisp-gameboy
./build.fish line-demo
```

#### Load/Run in Browser
```bash
ac-run gameboy~line-demo
```

#### Watch Mode (Auto-rebuild on save)
For iterative development:
```bash
cd /workspaces/aesthetic-computer/kidlisp-gameboy
./watch-build.fish line-demo
# Edit test/line-demo.asm, save, and it auto-rebuilds
# Then reload with: ac-run gameboy~line-demo
```

#### Manual Build
```bash
cd /workspaces/aesthetic-computer/kidlisp-gameboy
npx gbasm test/line-demo.asm -o test/line-demo.gb
cp test/line-demo.gb ../system/public/aesthetic.computer/gb-emulator/line-demo.gb
# Load: ac-run gameboy~line-demo
```

## 📁 Key Files

### Working ROMs in `test/`
- **line-demo.asm** - Bresenham X pattern (your latest success!)
- **ascii-scroll.asm** - Text scrolling demo
- **bounce-ball.asm** - Bouncing ball physics
- **typewriter.asm** - Text input effect
- **starfield.asm** - Animated starfield
- Many more...

### Compiler (Not Yet Integrated)
- `compiler/kidlisp-gb-compiler-cli.mjs` - KidLisp → Assembly compiler
- `compiler/kidlisp-gb-compiler-browser.mjs` - Browser version

**Status**: Parser works, but needs updating to generate working assembly for current primitives.

## 🎮 Next Steps

### Option 1: Integrate KidLisp Compiler
Update the compiler to generate assembly like `line-demo.asm`:
1. Update `kidlisp-gb-compiler-cli.mjs` to output working assembly
2. Test `(line x0 y0 x1 y1)` compilation
3. Add `(point x y)`, `(wipe)` primitives
4. Create browser interface for live compilation

### Option 2: Expand Assembly Primitives
Add more drawing primitives in pure assembly:
1. Box/rectangle drawing
2. Circle drawing (Bresenham circle algorithm)
3. Text rendering (you have font examples)
4. Sprite support
5. Color/palette control

### Option 3: Create Interactive Demos
Build example programs showing off capabilities:
1. Drawing program (user controlled cursor)
2. Simple game (Pong, Snake, etc.)
3. Generative art
4. Music visualizer

## 🧪 Testing

### Browser Testing
1. Make sure aesthetic.computer is running
2. Navigate to `gameboy~<rom-name>`
3. Hot-reload works with file watching

### Terminal Testing (if needed)
```bash
sdlgnuboy test/line-demo.gb
```

## 💡 Technical Notes

### Bresenham Algorithm
- Implemented in WRAM ($C000-$C009) to avoid stack complexity
- Handles all line slopes (horizontal, vertical, diagonal, shallow, steep)
- Pure integer math, no floating point
- See `test/line-demo.asm` for reference implementation

### Pixel Plotting
- Uses tile-based rendering (GameBoy has no direct pixel access)
- Creates 1-pixel tile pattern in VRAM
- Tile map ($9800-$9BFF) controls which tiles appear where
- Screen is 160×144 pixels = 20×18 tiles

### gbasm Limitations
- No complex expressions in operands
- Limited macro support
- Auto-patches Nintendo logo (convenient!)
- Works in browser (useful for web integration)

## 📚 Documentation
- **BRESENHAM-SUCCESS.md** - How line drawing works
- **PROGRESS.md** - Full development history
- **RESEARCH.md** - Technical GameBoy details
- **README.md** - Project overview

## 🚀 Quick Start

To jump back in and iterate:

```bash
cd /workspaces/aesthetic-computer/kidlisp-gameboy

# Build a ROM
./build.fish line-demo

# Load it in browser
ac-run gameboy~line-demo

# OR use watch mode for auto-rebuild
./watch-build.fish line-demo
# Edit test/line-demo.asm, save to auto-rebuild
# Then: ac-run gameboy~line-demo to reload
```

Happy coding! 🎮✨
