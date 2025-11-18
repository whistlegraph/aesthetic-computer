# KidLisp Compiler - Game Boy ROM Generator

## Overview
KidLisp is a simple command-based language that compiles to Game Boy ROMs. It uses a straightforward syntax where each line is a command with arguments.

## Syntax

### Graphics Commands
- `wipe <color>` - Clear screen to color (black/white)
- `ink <color>` - Set drawing color (black/white)
- `line x1 y1 x2 y2` - Draw a line from (x1,y1) to (x2,y2)

### Audio Commands
- `melody "notes"` - Play a sequence of notes (c, d, e, f, g, a, b)

### Comments
- Lines starting with `;` are comments

## Example
```kidlisp
; Grid pattern with melody
wipe black
ink white
line 10 10 150 134
line 150 10 10 134
line 80 10 80 134
line 10 72 150 72
melody "ceg"
```

## Compilation Pipeline

1. **Parse** (`kidlisp-to-gb.mjs`): Read `.lisp` source file
2. **Generate C**: Convert commands to GBDK C code
   - Graphics → Bresenham line drawing
   - Melody → hUGEDriver pattern arrays
3. **Compile**: GBDK builds C code to `.gb` ROM
4. **Deploy**: Copy ROM to web assets for testing

## Usage

```bash
# Direct compilation
./compiler/kidlisp-to-gb.mjs examples/lines.lisp src/lines.c

# Build to ROM
./build.sh examples/lines.lisp

# Test in browser
ac gameboy~lines
```

## Architecture

### Generated C Structure
```c
#include <gb/gb.h>
#include <gb/drawing.h>
#include "hUGEDriver.h"

// hUGEDriver pattern data (if melody present)
static const unsigned char melody_pattern[] = {...};
const hUGESong_t kidlisp_song = {...};

// Bresenham line drawing
void draw_line(uint8_t x0, uint8_t y0, uint8_t x1, uint8_t y1) {...}

void main(void) {
    // Sound initialization
    NR52_REG = 0x80;
    hUGE_init(&kidlisp_song);
    add_VBL(hUGE_dosound);
    set_interrupts(VBL_IFLAG);
    
    // Graphics commands
    fill_rect(...);
    draw_line(...);
    
    // Main loop
    while(1) { vsync(); }
}
```

## Dependencies

- **GBDK**: Game Boy Development Kit
- **hUGEDriver**: Music driver for Game Boy
  - Cloned from: https://github.com/SuperDisk/hUGEDriver
  - Prebuilt library from: https://github.com/LaroldsJubilantJunkyard/gbdk-hugedriver-example
- **RGBDS**: Rednex Game Boy Development System (for building hUGEDriver)

## Current Capabilities

✅ Line drawing with Bresenham algorithm
✅ Screen clearing (wipe)
✅ Color selection (ink)
✅ Music playback via hUGEDriver
✅ Simple note sequences (0.5s per note)
✅ Comments
✅ Integrated build pipeline

## Future Enhancements

- Point drawing
- Rectangle/circle primitives
- More colors (Game Boy has 4 shades)
- Note durations (quarter, half, whole notes)
- Octave control
- Multiple melody channels
- Input handling (act commands)
- Sprites
- Scrolling backgrounds
- More complex music patterns

## Example ROMs

### lines.lisp
Simple grid pattern with C-E-G melody. Demonstrates:
- Multiple line drawing commands
- Basic melody playback
- Screen clearing and color selection

Output: `lines.gb` (32KB ROM)
