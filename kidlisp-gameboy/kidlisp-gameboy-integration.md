# KidLisp → GameBoy ROM Integration Plan

**Date**: September 8, 2025  
**Objective**: Enable KidLisp code to compile into actual GameBoy ROMs that can run on real hardware or any GameBoy emulator.

## 🎯 Project Overview

Transform the existing KidLisp environment to support compilation of minimal graphics-focused KidLisp programs into actual GameBoy ROMs (.gb files). This bridges the gap between creative coding and retro hardware development.

## 🔍 Current State Analysis

### Existing Assets
- **GameBoy Emulation**: Full WasmBoy integration in `system/public/aesthetic.computer/disks/gameboy.mjs`
- **KidLisp Runtime**: Sophisticated 8553+ line interpreter in `system/public/aesthetic.computer/lib/kidlisp.mjs` 
- **Graphics API**: Complete graphics primitives (`wipe`, `line`, `point`, `circle`, `box`, `ink`)
- **Aesthetic Computer Integration**: Unified API system with real-time evaluation

### Target KidLisp Subset for GameBoy
```lisp
;; Basic graphics primitives
(wipe)                    ; Clear screen / set background
(wipe "green")           ; Clear screen with color
(ink "red")              ; Set drawing color  
(line 0 0 100 100)       ; Draw line from (x1,y1) to (x2,y2)
(point x y)              ; Plot single pixel
(box x y w h)            ; Draw rectangle (filled)

;; Simple control flow & variables
(def x 50)               ; Variable assignment
(repeat 100 i (...))     ; Basic loops
(if condition (...))     ; Conditionals
```

## 🛠️ Technical Architecture

### Compilation Pipeline

```
KidLisp Source Code
       ↓
[ KidLisp Parser & AST ]
       ↓ 
[ GameBoy Code Generator ]
       ↓
[ GameBoy Assembly (.asm) ]
       ↓
[ gbasm JavaScript Assembler ]
       ↓
[ GameBoy ROM (.gb file) ]
       ↓
[ Test in WasmBoy Emulator ]
```

### Core Components

#### 1. **KidLisp-GB Compiler** (`kidlisp-gb-compiler.mjs`)
- Extends existing KidLisp parser 
- Generates GameBoy-specific assembly code
- Handles color palette mapping (GameBoy's 4 shades of green)
- Implements graphics primitives in GB assembly
- Memory management for 8KB video RAM

#### 2. **GameBoy Assembly Templates** (`gb-templates/`)
- **Boot sequence** - ROM header and initialization
- **Graphics routines** - Optimized pixel/line/box drawing
- **Memory management** - VRAM and tile data handling
- **Input handling** - D-pad and button support (future)

#### 3. **Assembly Code Generator** (`gb-codegen.mjs`)
- Transforms KidLisp AST to GameBoy Z80 assembly
- Optimizes repeated operations (e.g., loops)
- Manages register allocation
- Handles screen coordinate mapping (160x144)

#### 4. **ROM Builder Integration** (`rom-builder.mjs`)
- Integrates **gbasm** JavaScript assembler
- Packages final ROM with proper GameBoy headers
- Handles ROM banking for larger programs
- Generates downloadable .gb files

## 📚 JavaScript GameBoy Development Tools

### Primary Tool: **gbasm** 
- **Repository**: https://github.com/BonsaiDen/gbasm
- **Type**: JavaScript-based GameBoy assembler
- **Installation**: `npm install gbasm`
- **Output**: Direct .gb ROM files
- **Integration**: Can be embedded directly in Aesthetic Computer

### Alternative Options
1. **RGBDS-Live** (https://gbdev.io/rgbds-live/) - Web-based RGBDS assembler
2. **gbasm-rs** - Rust compiler with JavaScript bindings  
3. **GBDK-2020 + WebAssembly** - C toolchain compiled to WASM

### GameBoy Development Resources
- **Awesome Game Boy Development**: https://github.com/gbdev/awesome-gbdev
- **Pan Docs**: Comprehensive GameBoy hardware documentation
- **Game Boy CPU Manual**: Z80-based processor specifications

## 🎨 Implementation Phases

### Phase 1: Foundation (Week 1-2)
1. **Install and integrate gbasm**
   ```bash
   cd system/public/aesthetic.computer/lib/
   npm install gbasm
   ```

2. **Create minimal GameBoy assembly template**
   - Basic ROM header with Nintendo logo
   - Screen initialization routines
   - Simple pixel-plotting function

3. **Build proof-of-concept pipeline**
   - Single KidLisp statement: `(point 80 72)` 
   - Generate assembly that plots pixel at screen center
   - Compile with gbasm to create working ROM
   - Test in existing WasmBoy emulator

### Phase 2: Core Graphics (Week 3-4)
1. **Implement core KidLisp graphics primitives**
   - `(wipe)` - Clear screen / set background  
   - `(ink color)` - Set drawing color (map to GB 4-color palette)
   - `(point x y)` - Plot pixel
   - `(line x1 y1 x2 y2)` - Bresenham line algorithm
   - `(box x y w h)` - Filled rectangle

2. **GameBoy-specific optimizations**
   - Tile-based graphics system (8x8 pixel tiles)
   - Color palette management (4 shades: white, light gray, dark gray, black)
   - Screen coordinate mapping (160x144 resolution)

3. **Memory management**
   - VRAM organization (0x8000-0x9FFF)
   - Tile data vs tile maps
   - Background vs sprite layers

### Phase 3: Control Flow (Week 5-6)
1. **Variables and arithmetic**
   - `(def name value)` - Variable definitions
   - Basic math operations (+, -, *, /)
   - Register allocation strategies

2. **Control structures**
   - `(repeat count i (...))` - For loops
   - `(if condition (...))` - Conditionals
   - Optimized assembly generation for loops

3. **Compilation optimizations**
   - Constant folding
   - Loop unrolling for small counts
   - Dead code elimination

### Phase 4: Integration & Polish (Week 7-8)
1. **Aesthetic Computer integration**
   - New `compile-gameboy` command
   - ROM download functionality  
   - Integration with existing KidLisp editor
   - Real-time compilation preview

2. **Error handling and debugging**
   - KidLisp syntax validation for GameBoy subset
   - Assembly generation error reporting
   - ROM size validation (32KB limit for simple ROMs)

3. **Documentation and examples**
   - Tutorial pieces demonstrating each primitive
   - Gallery of example ROMs
   - Performance guidelines

## 💾 GameBoy Technical Constraints

### Hardware Limitations
- **Screen**: 160x144 pixels, 4 colors (grayscale)
- **Memory**: 8KB video RAM, 8KB work RAM
- **ROM**: 32KB base (expandable with banking)
- **CPU**: Custom Z80-like processor at 4.19MHz
- **No floating point**: Integer-only arithmetic

### Programming Constraints
- **Pixel-based graphics**: No vector graphics
- **Limited colors**: 4-shade grayscale palette only
- **Tile-based rendering**: Most efficient graphics are 8x8 tiles
- **Frame rate**: 60 FPS maximum
- **Memory management**: Manual VRAM and RAM allocation

## 🚀 Example Usage

### Input KidLisp Code
```lisp
; Simple GameBoy graphics demo
(wipe)                    ; Clear screen
(ink "black")            ; Set color to darkest shade  

; Draw a simple house
(box 60 80 40 40)        ; House base
(line 60 80 80 60)       ; Roof left
(line 80 60 100 80)      ; Roof right
(line 100 80 60 80)      ; Roof bottom

; Draw door
(ink "gray")
(box 75 100 10 20)       ; Door rectangle

; Draw window  
(ink "black")
(box 85 90 8 8)          ; Window
```

### Generated GameBoy Assembly (Simplified)
```asm
; ROM Header
SECTION "ROM Header", ROM0[$100]
    nop
    jp main

; Main program
main:
    ; Initialize LCD
    call lcd_init
    
    ; Clear screen (wipe)
    call clear_screen
    
    ; Set color to black (palette index 3)
    ld a, 3
    ld [current_color], a
    
    ; Draw house base - box(60, 80, 40, 40)  
    ld bc, 60*256 + 80  ; x,y in bc
    ld de, 40*256 + 40  ; w,h in de
    call draw_box
    
    ; ... additional drawing commands
    
    ; Main loop
loop:
    jp loop
```

### Output
- **Compiled ROM**: `house-demo.gb` (32KB file)
- **Compatible with**: Any GameBoy emulator or flash cart
- **Playable on**: Original GameBoy, GameBoy Color, GameBoy Advance, or emulators

## 🎮 Integration with Existing Codebase

### File Structure
```
system/public/aesthetic.computer/
├── lib/
│   ├── kidlisp.mjs              # Existing KidLisp interpreter
│   ├── kidlisp-gb-compiler.mjs  # NEW: GameBoy compiler
│   ├── gb-codegen.mjs           # NEW: Assembly generator
│   └── rom-builder.mjs          # NEW: ROM building utilities
├── disks/
│   ├── gameboy.mjs              # Existing GameBoy emulator interface
│   └── kidlisp-gb.mjs           # NEW: KidLisp→GameBoy compiler interface
└── gb-templates/
    ├── boot.asm                 # NEW: ROM initialization template
    ├── graphics.asm             # NEW: Graphics primitive routines
    └── header.asm               # NEW: GameBoy ROM header template
```

### API Extensions
```javascript
// New functions added to existing KidLisp API
kidlisp.compileToGameBoy(source)     // Returns ROM binary data
kidlisp.downloadROM(source, filename) // Triggers ROM download
kidlisp.validateForGameBoy(source)   // Check GameBoy compatibility

// Example usage in a piece
export function paint({ kidlisp, download, screen, ink, write }) {
  // Regular KidLisp execution
  kidlisp(0, 0, screen.width/2, screen.height, "(wipe) (ink red) (box 20 20 60 60)");
  
  // GameBoy compilation interface
  ink("white");
  write("Click to compile to GameBoy ROM", 10, screen.height - 30);
}

export function act({ event, kidlisp }) {
  if (event.is("click")) {
    const source = "(wipe) (ink black) (box 60 80 40 40)";
    kidlisp.downloadROM(source, "my-game.gb");
  }
}
```

## 🔬 Testing Strategy

### Unit Tests
- **KidLisp→Assembly translation** for each primitive
- **Assembly generation** validation  
- **ROM header** correctness
- **Memory layout** verification

### Integration Tests  
- **End-to-end compilation** from KidLisp to working ROM
- **Emulator compatibility** testing in WasmBoy
- **Performance benchmarks** for compilation speed
- **ROM size optimization** validation

### Manual Testing
- **Visual verification** of generated ROMs
- **Hardware testing** on actual GameBoy (if available)
- **Cross-emulator compatibility** (BGB, SameBoy, etc.)

## 📈 Success Metrics

### Technical Goals
- **Compilation time**: Under 1 second for simple programs
- **ROM size**: Under 32KB for basic graphics programs  
- **Compatibility**: Works in at least 3 different GameBoy emulators
- **Performance**: Generated ROMs run at 60 FPS

### User Experience Goals
- **Simplicity**: Single-click compilation from KidLisp to ROM
- **Immediate feedback**: Real-time preview in existing GameBoy emulator
- **Educational value**: Clear examples and documentation
- **Creative potential**: Enable unique retro art creation

## 🚧 Potential Challenges & Solutions

### Challenge 1: **GameBoy Color Palette Mapping**
- **Problem**: KidLisp supports full color, GameBoy only has 4 shades
- **Solution**: Implement intelligent color quantization algorithm
- **Fallback**: Simple mapping (red→dark, green→medium, blue→light, white→light)

### Challenge 2: **Memory Constraints**  
- **Problem**: GameBoy has only 8KB video RAM
- **Solution**: Implement tile-based graphics system for efficiency
- **Optimization**: Re-use tiles for repeated patterns

### Challenge 3: **Assembly Complexity**
- **Problem**: GameBoy Z80 assembly is complex for graphics operations
- **Solution**: Pre-built optimized graphics routines library
- **Tools**: Use gbasm's macro system for cleaner code generation

### Challenge 4: **Real-time Compilation Performance**
- **Problem**: Assembly generation might be slow for interactive use
- **Solution**: Incremental compilation and caching strategies
- **Optimization**: Compile to intermediate representation first

## 🔮 Future Enhancements

### Phase 5: Advanced Features (Future)
1. **Sound support** - Simple tone generation using GameBoy sound channels
2. **Input handling** - D-pad and button support for interactive programs  
3. **Animation** - Frame-based animation systems
4. **Sprite graphics** - Moving objects beyond background tiles
5. **ROM banking** - Support for larger programs (64KB, 128KB ROMs)

### Phase 6: Community Features (Future)
1. **ROM gallery** - Share and download community-created ROMs
2. **Collaborative editing** - Multiple users working on same ROM
3. **Version control** - Track changes to KidLisp→GameBoy programs
4. **Performance profiler** - Analyze ROM performance characteristics

## 📋 Implementation Checklist

### Setup Phase
- [ ] Install and test gbasm in project
- [ ] Create basic GameBoy assembly templates
- [ ] Set up development environment for ROM testing

### Core Development
- [ ] Implement KidLisp→Assembly parser extension
- [ ] Build code generator for basic primitives
- [ ] Create ROM builder with gbasm integration
- [ ] Test first compiled ROM in WasmBoy emulator

### Graphics Primitives  
- [ ] `(wipe)` - Screen clearing
- [ ] `(ink color)` - Color selection
- [ ] `(point x y)` - Pixel plotting
- [ ] `(line x1 y1 x2 y2)` - Line drawing
- [ ] `(box x y w h)` - Rectangle drawing

### Integration
- [ ] Add GameBoy compilation UI to Aesthetic Computer
- [ ] Implement ROM download functionality
- [ ] Create example programs and documentation
- [ ] Set up automated testing pipeline

### Polish  
- [ ] Error handling and user feedback
- [ ] Performance optimization
- [ ] Cross-platform compatibility testing
- [ ] Documentation and tutorial creation

---

## 🎯 Expected Timeline

**Total Estimated Time**: 6-8 weeks for full implementation

This plan provides a clear roadmap for integrating KidLisp with GameBoy ROM compilation, leveraging existing tools and infrastructure while creating a unique creative coding experience that bridges modern web development with retro gaming hardware.
