# KidLisp GameBoy ROM Research & Development Guide

**Date**: November 11, 2025  
**Goal**: Create working GameBoy ROMs from KidLisp code that run in the aesthetic computer gameboy.mjs piece

## 📊 Current State Assessment

### What We Have ✅
1. **GameBoy Emulator**: WasmBoy-based emulator in `gameboy.mjs` (340 lines)
   - Supports .gb and .gbc ROM files
   - Full screen display with controller inputs
   - Working integration with aesthetic computer

2. **KidLisp Compiler Infrastructure**: 
   - CLI compiler: `compiler/kidlisp-gb-compiler-cli.mjs` (222 lines)
   - Browser compiler: `compiler/kidlisp-gb-compiler-browser.mjs`
   - Test piece: `test/kidlisp-gb-test.mjs` (175 lines)

3. **Assembly Templates**:
   - Boot ROM template with Nintendo logo header
   - Basic LCD initialization
   - Pixel plotting function stub (not fully implemented)

4. **Dependencies**:
   - `gbasm@0.0.21` already installed in main project
   - Node.js tooling ready

### What's Missing ❌
1. **Actual VRAM Graphics Implementation**
   - Current `set_pixel` function is a stub
   - No actual GameBoy tile-based graphics
   - No proper VRAM access during VBlank

2. **Working ROM Generation**
   - Assembly may have syntax issues
   - No confirmed working .gb file yet
   - Haven't tested in the gameboy.mjs emulator

3. **GameBoy Graphics Knowledge Gap**
   - Need to understand tile-based rendering
   - Background vs sprite layers
   - VRAM structure (tiles, tile maps, palettes)

## 🎮 GameBoy Graphics Architecture

### Memory Map
```
0x8000-0x97FF  Tile Data (Character RAM)
0x9800-0x9BFF  Background Tile Map 1
0x9C00-0x9FFF  Background Tile Map 2
0xFE00-0xFE9F  Sprite Attribute Table (OAM)
0xFF40         LCD Control Register (LCDC)
0xFF47         Background Palette (BGP)
0xFF48-0xFF49  Sprite Palettes
```

### Display System
- **Resolution**: 160x144 pixels
- **Tile System**: 8x8 pixel tiles
- **Tile Grid**: 20x18 tiles = 160x144 pixels
- **Colors**: 4-shade grayscale (0=white, 1=light gray, 2=dark gray, 3=black)
- **Tile Data**: Each tile = 16 bytes (2 bits per pixel × 8 rows)

### Two Rendering Approaches

#### Option 1: Tile-Based (Traditional GameBoy)
**Pros:**
- Memory efficient
- Fast rendering
- How real GameBoy games work

**Cons:**
- Complex for pixel-level drawing
- Need to manage tile data
- 8x8 pixel granularity

**Best for:** Traditional games with sprite-based graphics

#### Option 2: Pixel-Perfect (Direct VRAM)
**Pros:**
- Perfect for KidLisp `(point x y)` primitive
- Direct pixel control
- Simpler conceptual model

**Cons:**
- More VRAM writes needed
- Slower (need VBlank timing)
- Uses more tiles (one per 8x8 region)

**Best for:** Drawing applications, generative art, KidLisp graphics

## 🔧 Technical Approaches

### Approach A: Pure Background Layer (Recommended)
Use background layer with custom tiles for each pixel pattern.

**Strategy:**
1. Pre-calculate all possible 8x8 tile patterns (256 tiles max)
2. Build tile map that references these patterns
3. Update tile map during VBlank to change display
4. Each pixel change updates one tile

**Code Pattern:**
```asm
; Set pixel at (X, Y) to color C
; 1. Calculate which tile contains the pixel
; 2. Calculate pixel position within tile
; 3. Load tile data from VRAM
; 4. Modify the 2-bit pixel value
; 5. Write tile data back to VRAM
```

### Approach B: Sprite Layer Overlay
Use sprites (OAM) for drawing individual pixels.

**Limitations:**
- Only 40 sprites available
- Max 10 sprites per scanline
- Not suitable for many pixels

**Best for:** Cursor, selection indicators, not general drawing

### Approach C: Hybrid System
Background for static content, sprites for dynamic elements.

## 🚀 Recommended Implementation Plan

### Phase 1: Minimal Working ROM (Week 1)
**Goal**: Create a ROM that displays ANYTHING on screen

1. **Fix Assembly Syntax**
   - Test with `gbasm` directly
   - Verify ROM header is correct
   - Get a blank screen showing

2. **Test ROM in Emulator**
   - Load in gameboy.mjs piece
   - Verify it runs without crashing
   - Confirm LCD turns on

**Success Criteria**: A ROM that boots and shows a solid color screen

### Phase 2: Single Pixel Display (Week 1-2)
**Goal**: `(point 80 72)` displays a black pixel at screen center

1. **Implement Basic VRAM Write**
   ```asm
   ; Simple single-pixel display
   ; Draw one tile with one pixel set
   ```

2. **Wait for VBlank**
   ```asm
   wait_vblank:
       ld a, [$FF44]  ; Read LY register
       cp 144         ; Wait for line 144 (VBlank)
       jr nz, wait_vblank
       ret
   ```

3. **Set Background Palette**
   ```asm
   ld a, %11100100  ; 3=black, 2=dark, 1=light, 0=white
   ld [$FF47], a    ; Background palette register
   ```

**Success Criteria**: A visible pixel on screen in emulator

### Phase 3: Multiple Pixels (Week 2)
**Goal**: `(point x1 y1) (point x2 y2)` displays multiple pixels

1. **Implement Tile Data Generation**
   - Calculate tile index from X/Y coords
   - Modify tile data for specific pixel
   - Handle multiple pixels in same tile

2. **Optimize VRAM Access**
   - Batch writes during VBlank
   - Track which tiles need updates

**Success Criteria**: Draw multiple pixels from KidLisp code

### Phase 4: More Primitives (Week 3-4)
1. **`(wipe)`** - Clear screen to white
2. **`(wipe color)`** - Clear to specific shade
3. **`(line x1 y1 x2 y2)`** - Bresenham's line algorithm
4. **`(box x y w h)`** - Filled rectangle

### Phase 5: Polish (Week 4+)
1. Integration with aesthetic computer UI
2. ROM download feature
3. Live preview in editor
4. Example gallery

## 📚 Essential Resources

### GameBoy Development
- **Pan Docs**: https://gbdev.io/pandocs/
  - Complete hardware reference
  - Memory map documentation
  - LCD control registers

- **Awesome GameBoy Development**: https://github.com/gbdev/awesome-gbdev
  - Tools, libraries, tutorials
  - Example code repositories

- **GameBoy CPU Manual**: Z80 instruction set
  - Assembly language reference
  - Register usage patterns

### Assembly Tools
- **gbasm**: https://github.com/BonsaiDen/gbasm
  - JavaScript-based assembler (already installed)
  - Syntax: RGBDS-like but may have differences

- **RGBDS**: https://rgbds.gbdev.io/
  - Standard GameBoy assembler (not installed)
  - More mature, better documented
  - Consider switching if gbasm is problematic

### Code Examples
- **Hello World**: https://github.com/gbdev/gb-asm-tutorial
  - Minimal working ROM
  - Basic graphics setup

- **Pixel Drawing**: Search for "gameboy pixel plot" examples
  - VRAM manipulation techniques

## 🔍 Next Steps (Immediate Actions)

### 1. Test gbasm with Minimal ROM
```bash
cd /workspaces/aesthetic-computer/kidlisp-gameboy
npx gbasm -o test-minimal.gb test/minimal-test.asm
```

**Expected Issues:**
- Syntax errors (ldh vs ld)
- Section directive differences
- Label formatting

### 2. Research gbasm Syntax
- Check gbasm documentation
- Compare to RGBDS syntax
- Look for working examples

### 3. Consider RGBDS Installation
```bash
# Install RGBDS if gbasm proves problematic
sudo dnf install rgbds
# OR build from source
```

### 4. Create Simplest Possible ROM
**Goal**: Just turn on LCD with solid color

```asm
SECTION "Header", ROM0[$100]
    nop
    jp Start

; ... Nintendo logo and header ...

SECTION "Code", ROM0[$150]
Start:
    ; Turn off LCD
    ld a, 0
    ld [$FF40], a
    
    ; Set BGP to all black
    ld a, $FF
    ld [$FF47], a
    
    ; Fill one tile with data
    ld hl, $8000
    ld a, $FF
    ld [hl], a
    
    ; Turn on LCD
    ld a, $91
    ld [$FF40], a
    
Loop:
    halt
    jr Loop
```

### 5. Test in gameboy.mjs
- Load ROM file through aesthetic computer
- Verify emulator accepts it
- Check for any error messages

## 🎯 Success Metrics

### Milestone 1: ROM Boots
- [ ] ROM file generates without errors
- [ ] Emulator loads ROM
- [ ] Screen turns on (not blank/crash)

### Milestone 2: Visible Output
- [ ] Can see something on screen
- [ ] Screen shows expected color/pattern
- [ ] Consistent rendering

### Milestone 3: KidLisp Integration
- [ ] `(point x y)` compiles to ROM
- [ ] ROM displays pixel at correct location
- [ ] Can compile multiple points

### Milestone 4: Working Piece
- [ ] gameboy.mjs loads KidLisp-generated ROMs
- [ ] Can create and test ROMs in aesthetic computer
- [ ] Share/download generated ROMs

## 🐛 Known Issues to Investigate

1. **gbasm Syntax Compatibility**
   - Current assembly may not match gbasm's expected syntax
   - May need to adjust directives (SECTION, db, dw, etc.)
   - Label syntax might differ

2. **ROM Header Checksum**
   - GameBoy BIOS validates header checksum
   - gbasm should calculate this automatically
   - Verify with hex editor if needed

3. **LCD Timing**
   - VRAM can only be safely accessed during VBlank or with LCD off
   - Current code may violate this
   - Need proper synchronization

4. **Tile Data Format**
   - 2 bits per pixel, but encoded in specific byte pattern
   - Each row of 8 pixels = 2 bytes
   - Low bit in first byte, high bit in second byte

## 💡 Tips & Best Practices

### Assembly Development
1. **Start simple**: Get minimal ROM working first
2. **Test incrementally**: Add one feature at a time
3. **Use comments**: Assembly is hard to read
4. **Check examples**: Don't reinvent the wheel

### Debugging
1. **Hex editor**: Inspect .gb file structure
2. **Emulator debugging**: WasmBoy may have debug output
3. **Console logs**: Add logging to compiler
4. **Test in multiple emulators**: BGB, Gambatte, etc.

### KidLisp Integration
1. **Keep subset small**: Start with just `(point x y)`
2. **Clear error messages**: Help users understand limitations
3. **Show assembly output**: Aid debugging
4. **Provide examples**: Working sample code

## 🎨 Example KidLisp Programs

### Hello Pixel
```lisp
(point 80 72)
```
**Expected**: Single black pixel at screen center

### X Pattern
```lisp
(line 0 0 159 143)
(line 159 0 0 143)
```
**Expected**: X across entire screen

### Grid
```lisp
(repeat 10 i
  (line (* i 16) 0 (* i 16) 143))
(repeat 9 i
  (line 0 (* i 16) 159 (* i 16)))
```
**Expected**: Grid of 16-pixel squares

### Pixel Art Sprite
```lisp
; Simple smiley face
(point 60 60) (point 100 60)   ; Eyes
(line 50 100 110 100)           ; Smile
```

---

**Next Action**: Test the minimal-test.asm with gbasm and see what errors we get!
