# KidLisp GameBoy ROM Development - Progress Report

**Date**: November 11, 2025  
**Status**: ✅ Successfully assembling GameBoy ROMs!

## 🎉 Achievements

### 1. Working ROM Assembly
- ✅ **gbasm** (v0.0.21) installed and working
- ✅ Successfully assembled 2 working GameBoy ROMs:
  - `hello-world.gb` - Displays a solid color screen
  - `test-minimal.gb` - Minimal bootable ROM
- ✅ ROMs verified as valid Game Boy ROM images (256Kbit)

### 2. Development Environment
- ✅ CLI compiler exists: `compiler/kidlisp-gb-compiler-cli.mjs`
- ✅ Browser compiler exists: `compiler/kidlisp-gb-compiler-browser.mjs`
- ✅ Test infrastructure in place
- ✅ GameBoy emulator installed: **gnuboy-sdl**

### 3. Testing Tools
- ✅ **gnuboy-sdl** installed from Fedora repos
- ✅ `test-rom.sh` script created for quick ROM testing
- ✅ Can now test ROMs directly in terminal

## 📂 Current File Structure

```
kidlisp-gameboy/
├── README.md                           # Project overview
├── RESEARCH.md                         # Detailed research & implementation guide
├── PROGRESS.md                         # This file
├── kidlisp-gameboy-integration.md      # Original integration plan
├── test-rom.sh                         # ROM testing script
├── compiler/
│   ├── kidlisp-gb-compiler-cli.mjs     # Node.js CLI compiler
│   └── kidlisp-gb-compiler-browser.mjs # Browser-compatible parser
├── templates/
│   └── boot.asm                        # Base ROM template
├── test/
│   ├── hello-world.asm                 # ✅ Working solid color ROM
│   ├── minimal-test.asm                # ✅ Working minimal ROM
│   ├── simple-pixel.asm                # 🚧 In progress pixel plotting
│   ├── pixel-plot-v1.asm              # 🚧 Complex pixel plotting attempt
│   ├── test-point.asm                  # Has gbasm syntax issues
│   └── kidlisp-gb-test.mjs            # Browser test piece
├── gameboy-emulator/                   # raphamorim/gameboy (needs newer Rust)
├── hello-world.gb                      # ✅ Working ROM (32KB)
└── test-minimal.gb                     # ✅ Working ROM (32KB)
```

## 🎮 Testing Your ROMs

### Using gnuboy-sdl
```bash
cd /workspaces/aesthetic-computer/kidlisp-gameboy
./test-rom.sh hello-world.gb
```

Or directly:
```bash
sdlgnuboy hello-world.gb
```

**Controls:**
- ESC: Quit emulator
- Arrow keys: D-pad
- Z: A button
- X: B button
- Enter: Start
- Backspace: Select

### Using aesthetic computer gameboy.mjs
1. Navigate to `http://localhost:8888/gameboy`
2. Load the .gb ROM file
3. Play in the browser with full WasmBoy emulator

## 🔧 How to Build ROMs

### Method 1: Using gbasm directly
```bash
cd /workspaces/aesthetic-computer/kidlisp-gameboy
npx gbasm -o output.gb test/hello-world.asm
```

### Method 2: Using the KidLisp compiler (CLI)
```bash
node compiler/kidlisp-gb-compiler-cli.mjs "(point 80 72)" output.gb
```

**Note:** The compiler currently has assembly syntax issues to fix.

### Method 3: Browser-based (via test piece)
Navigate to: `http://localhost:8888/kidlisp-gb-test`

## 🐛 Known Issues

### 1. gbasm Syntax Limitations
**Problem:** gbasm doesn't support all RGBDS assembly features

**Issues Found:**
- ❌ `db` directive with multiple values on one line crashes parser
- ❌ Expression evaluation in operands (e.g., `9 * 32`)  
- ❌ `ld [hl+], value` addressing mode may not work
- ✅ Basic `ld`, `jp`, `jr`, `call`, `ret` work fine

**Workaround:**
- Use simpler syntax
- Calculate expressions manually
- Use `ld [hl], value` + `inc hl` instead of `ld [hl+], value`

### 2. Nintendo Logo Data
**Problem:** gbasm can't parse the Nintendo logo byte array

**Error:**
```
ParseError: Unexpected NAME
    at [test-point.asm] (line 6, col 3)
```

**Solution:** gbasm automatically patches the logo, so we can omit it!

### 3. Pixel Plotting Not Yet Working
**Status:** 🚧 In Progress

**Challenge:** GameBoy uses tile-based graphics (8x8 pixel tiles)

**Current Approach:**
- Pre-create tiles with different pixel patterns
- Use tile map to position them
- Simpler than direct VRAM pixel manipulation

**Next Steps:**
- Finish `simple-pixel.asm` implementation
- Test pixel display in emulator
- Integrate with KidLisp compiler

## 📊 gbasm vs RGBDS Comparison

| Feature | gbasm | RGBDS |
|---------|-------|-------|
| Installation | ✅ npm install | ❌ Not in Fedora repos |
| JavaScript | ✅ Yes | ❌ C-based |
| Browser Support | ✅ Yes | ❌ CLI only |
| Syntax | 🟡 Limited | ✅ Full featured |
| Nintendo Logo | ✅ Auto-patches | ⚠️ Must include |
| Expressions | ❌ Limited | ✅ Full math |
| Multi-byte db | ❌ Crashes | ✅ Works |

**Recommendation:** Stick with gbasm for now since it's already working. If we hit more limitations, consider RGBDS.

## 🎯 Next Steps (Priority Order)

### Phase 1: Get Pixel Plotting Working (This Week)
1. ✅ Install ROM testing emulator (gnuboy-sdl)
2. ✅ Create working minimal ROMs
3. 🚧 Fix `simple-pixel.asm` syntax
4. 🚧 Test pixel display in gnuboy
5. 🚧 Verify in aesthetic computer gameboy.mjs

### Phase 2: KidLisp Integration (Next Week)
1. Update compiler to generate gbasm-compatible assembly
2. Fix syntax issues in `compiler/kidlisp-gb-compiler-cli.mjs`
3. Test `(point x y)` compilation
4. Generate working ROM from KidLisp

### Phase 3: More Primitives (Week 3)
1. Implement `(wipe)` - clear screen
2. Implement `(wipe color)` - clear to color
3. Implement `(line x1 y1 x2 y2)` - draw lines
4. Implement `(box x y w h)` - draw rectangles

### Phase 4: UI Integration (Week 4)
1. Add ROM download feature
2. Create aesthetic computer piece for compilation
3. Live preview in browser
4. Example gallery

## 💡 Technical Insights Learned

### GameBoy Graphics Architecture
- **Tile-based rendering**: 8x8 pixel tiles, not direct pixel access
- **Screen**: 160×144 pixels = 20×18 tiles
- **VRAM**: $8000-$97FF (tile data), $9800-$9BFF (tile map)
- **Colors**: 4 shades (white, light gray, dark gray, black)
- **Palette**: Stored in $FF47 register as %11100100

### Assembly Best Practices
1. **Turn off LCD** before VRAM access: `ld a, 0 / ld [$FF40], a`
2. **Set palette** before turning on LCD: `ld a, %11100100 / ld [$FF47], a`
3. **Turn on LCD** with BG enabled: `ld a, $91 / ld [$FF40], a`
4. **Use VBlank** for safe VRAM updates in running programs

### ROM Structure
```
$100-$103   Entry point (nop + jp Start)
$104-$133   Nintendo logo (gbasm auto-patches)
$134-$14F   ROM header (title, checksums, etc.)
$150+       Your code starts here
```

## 🔗 Useful Resources

### Documentation
- **Pan Docs**: https://gbdev.io/pandocs/ - Complete GB hardware reference
- **Awesome GB Dev**: https://github.com/gbdev/awesome-gbdev - Tools & tutorials
- **gbasm GitHub**: https://github.com/BonsaiDen/gbasm - Our assembler

### Emulators
- **gnuboy-sdl**: Installed via `dnf install gnuboy-sdl`
- **WasmBoy**: Already integrated in aesthetic computer gameboy.mjs
- **BGB**: Windows-only but excellent debugging (Wine?)
- **SameBoy**: Great accuracy, needs newer build

### Example Code
- **Hello World Tutorial**: https://github.com/gbdev/gb-asm-tutorial
- **GB ASM Examples**: https://github.com/gbdev/gb-asm-examples

## 🎨 Example KidLisp Programs (Target)

### Single Pixel
```lisp
(point 80 72)
```
**Expected:** Black pixel at screen center (80, 72)

### Cross Pattern
```lisp
(point 80 72)
(point 79 72)
(point 81 72)
(point 80 71)
(point 80 73)
```
**Expected:** Plus sign (+) at center

### Line
```lisp
(line 0 0 159 143)
```
**Expected:** Diagonal line from top-left to bottom-right

### Box
```lisp
(box 60 52 40 40)
```
**Expected:** 40×40 pixel square, centered

## 📝 Notes & Observations

### gbasm Quirks
1. **No multi-value db**: Can't do `db $00,$01,$02` on one line
2. **Limited expressions**: Must pre-calculate math like `9 * 32 = $120`
3. **Auto-patching**: Nintendo logo is automatically added (helpful!)
4. **Section order**: Sections can be in any order, addresses matter

### Testing Workflow
1. Write assembly in `test/*.asm`
2. Assemble with `npx gbasm -o test.gb test/myfile.asm`
3. Test in gnuboy: `./test-rom.sh test.gb`
4. Verify in browser: Load in gameboy.mjs piece
5. Iterate and improve

### Performance Tips
- Each VRAM write is slow, batch updates
- Use tile map changes instead of tile data when possible
- Pre-calculate as much as possible
- VBlank happens 60 times per second = 16.7ms window

---

**Last Updated:** November 11, 2025  
**Next Review:** After pixel plotting works in emulator
