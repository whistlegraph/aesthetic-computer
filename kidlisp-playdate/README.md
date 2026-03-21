# KidLisp Playdate

A KidLisp runtime for the [Panic Playdate](https://play.date/) handheld console.

KidLisp is a minimal Lisp dialect designed for creative coding, originally created for [aesthetic.computer](https://aesthetic.computer). This port brings KidLisp to the Playdate's 1-bit 400×240 display.

## Features

- **Compiled to C**: KidLisp source is transpiled to C for native Playdate performance
- **1-bit Graphics**: Full drawing API adapted for monochrome display
- **Scroll & Blur Effects**: Framebuffer manipulation at the pixel level
- **Source HUD**: Code displayed on-screen in comma syntax
- **QR Code**: Links to [kidlisp.com](https://kidlisp.com) for discoverability

## Quick Start

### 1. Install SDK (first time only)

```fish
./install-sdk.fish
```

### 2. Add helper functions to your shell

Add to your `~/.config/fish/config.fish`:

```fish
# KidLisp Playdate helpers
function ac-playdate-build
    set -l script $argv[1]
    cd /workspaces/aesthetic-computer/kidlisp-playdate
    fish build.fish $script
end

function ac-playdate-simulate
    set -l pdx $argv[1]
    # Run on host (requires SSH to host machine with Playdate Simulator)
    ssh jas@172.17.0.1 "cd ~/PlaydateSDK && ./bin/PlaydateSimulator $pdx"
end

function ac-playdate
    set -l script $argv[1]
    set -l name (basename $script .lisp)
    ac-playdate-build $script
    # Deploy to host simulator
    scp -r build/$name.pdx jas@172.17.0.1:~/PlaydateSDK/
    ssh jas@172.17.0.1 "DISPLAY=:0 ~/PlaydateSDK/bin/PlaydateSimulator ~/PlaydateSDK/$name.pdx &" 2>/dev/null
    echo "✅ $name running on host simulator"
end
```

### 3. Build and run

```fish
ac-playdate examples/bop.lisp
```

## Example: bop.lisp

```lisp
(ink black)
(line)
(scroll 1)
(blur 5)
```

This creates the classic `$bop` effect from aesthetic.computer:
- Random lines drawn each frame
- Scrolling right at 1px/frame  
- Blur effect erodes black pixels over time

Displayed on screen as: `ink black, line, scroll 1, blur 5`

## KidLisp API

### Drawing

| Function | Description |
|----------|-------------|
| `(ink black)` / `(ink white)` | Set drawing color |
| `(line)` | Draw random line |
| `(line x1 y1 x2 y2)` | Draw line with coords |
| `(box x y w h)` | Draw rectangle outline |
| `(box x y w h fill)` | Draw filled rectangle |
| `(circle x y r)` | Draw circle outline |
| `(circle x y r fill)` | Draw filled circle |
| `(plot x y)` | Draw single pixel |
| `(wipe)` / `(wipe white)` | Clear screen |

### Effects

| Function | Description |
|----------|-------------|
| `(scroll dx dy)` | Scroll framebuffer (pixels) |
| `(scroll dx)` | Scroll horizontally only |
| `(blur amount)` | Erode black pixels randomly |

### Input

| Function | Description |
|----------|-------------|
| `(crank)` | Crank angle (0-360) |
| `(crank-delta)` | Crank change since last frame |
| `(button-a)` | A button held |
| `(button-b)` | B button held |
| `(button-up/down/left/right)` | D-pad held |
| `(button-pressed-a)` | A just pressed this frame |
| `(button-pressed-b)` | B just pressed this frame |

### Math

| Function | Description |
|----------|-------------|
| `(random max)` | Random int 0 to max-1 |
| `(wiggle amount)` | Random int ±amount/2 |
| `(sin deg)` / `(cos deg)` | Trig (degrees) |
| `(+ - * / %)` | Arithmetic |

### System

| Function | Description |
|----------|-------------|
| `(width)` | Screen width (400) |
| `(height)` | Screen height (240) |
| `(frame)` | Current frame number |

## Project Structure

```
kidlisp-playdate/
├── compiler/
│   └── kidlisp-to-pd.mjs    # KidLisp → C transpiler
├── src/
│   ├── kidlisp.c            # Runtime implementation
│   └── kidlisp.h            # Runtime API
├── examples/
│   └── bop.lisp             # Example program
├── build.fish               # Build script
├── sideload.fish            # Upload to hardware
├── install-sdk.fish         # Download Playdate SDK
└── README.md
```

## Development Notes

### Host Machine Setup

The Playdate Simulator runs on the host machine (not in devcontainer). SSH access required:
- Host: `172.17.0.1` (Docker host gateway)
- User: `jas`
- SDK path: `~/PlaydateSDK`

### Hardware Sideload

To run on actual Playdate hardware:

```fish
./sideload.fish build/bop.pdx
```

**Note**: Hardware currently shows "API handler function wasn't loaded" error - investigation pending.

### SDK

The Playdate SDK is not committed to git (gitignored). Run `./install-sdk.fish` to download.

## Status

- ✅ Core drawing API (line, box, circle, plot, ink, wipe)
- ✅ Scroll effect (1px precision, x and y)
- ✅ Blur effect (random pixel erosion)
- ✅ Source code HUD (comma syntax display)
- ✅ QR code (links to kidlisp.com)
- ✅ Simulator deployment
- ⚠️ Hardware sideload (pdex.bin format issue)
- 🔜 Crank input integration
- 🔜 Button input
- 🔜 Variables and loops
- 🔜 Comma syntax parsing (currently paren-only)

## License

Part of [aesthetic.computer](https://github.com/whistlegraph/aesthetic-computer)
