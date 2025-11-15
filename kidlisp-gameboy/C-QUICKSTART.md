# GameBoy C Development - Quick Start

## 🎮 Overview
Compile C programs to GameBoy ROMs using GBDK (GameBoy Development Kit).

## 📁 Location
```bash
cd /workspaces/aesthetic-computer/kidlisp-gameboy/src
```

## 🔨 Build & Run

### Quick Build & Test
```bash
# Build wave-editor ROM
cd /workspaces/aesthetic-computer/kidlisp-gameboy/src
/workspaces/aesthetic-computer/kidlisp-gameboy/gbdk/bin/lcc -Wm-yc -o wave-editor.gbc wave-editor.c

# Copy to assets directory
cp wave-editor.gbc /workspaces/aesthetic-computer/system/public/assets/gameboy/

# Run in aesthetic.computer
cd /workspaces/aesthetic-computer
ac gameboy~wave-editor
```

### Available ROMs
- **wave-editor.c** - Interactive Channel 3 waveform editor (32 bars, D-pad controls)
- **wave-test.c** - Minimal input test ROM
- **demo-graphics.c** - 4-channel audio visualization with manual controls
- **hello.c** - Hello world example
- **turtle-*.c** - Turtle graphics experiments
- **circle-anim.c** - Circle animation test

## 🎯 Development Workflow

1. **Edit** a `.c` file in `src/`
2. **Compile** with GBDK's `lcc`:
   ```bash
   /workspaces/aesthetic-computer/kidlisp-gameboy/gbdk/bin/lcc -Wm-yc -o myrom.gbc myrom.c
   ```
3. **Copy** to assets:
   ```bash
   cp myrom.gbc /workspaces/aesthetic-computer/system/public/assets/gameboy/
   ```
4. **Run** with `ac`:
   ```bash
   cd /workspaces/aesthetic-computer
   ac gameboy~myrom
   ```

## 📝 Key Patterns (from GBDK examples)

### Includes
```c
#include <gb/gb.h>
#include <stdint.h>
```

### Main Loop with Input
```c
void main(void) {
    uint8_t i;  // For joypad input
    
    // Setup code here
    
    while(1) {
        vsync();  // Wait for vertical blank
        i = joypad();  // Read input
        
        // Continuous input checking (NOT edge detection)
        if(i & J_LEFT) {
            // Handle left
        }
        if(i & J_RIGHT) {
            // Handle right
        }
        if(i & J_A) {
            // Handle A button
        }
    }
}
```

## ⚠️ Important Notes

- Use `vsync()` for timing, not `wait_vbl_done()` in most cases
- Input checking should be **continuous** (`if(i & J_LEFT)`), not edge detection
- Minimal includes prevent linking issues
- Compile with `-Wm-yc` flag for GameBoy Color support
- ROMs in `src/` are gitignored - production ROMs go in `system/public/assets/gameboy/`
- Use `npm run assets:sync:up` to publish ROMs to CDN

## 📚 Resources

- GBDK examples: `/workspaces/aesthetic-computer/kidlisp-gameboy/gbdk/examples/`
- Study `galaxy.c` for proper input handling patterns
- GameBoy hardware: `hardware.inc` for register addresses
