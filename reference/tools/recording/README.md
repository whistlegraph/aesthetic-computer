# Recording Tools

Professional animation recording system for Aesthetic Computer pieces.

## Directory Structure

```
recording/
├── tape.mjs          # Main animation recorder with GIF export
├── headless.mjs      # Shared AC system initialization
└── README.md         # This file

../output/            # Generated animation files
├── *.gif             # Exported GIF animations
└── *.png             # Final frame captures

../test-pieces/       # Sample pieces for testing
└── *.mjs             # Test animation pieces
```

## Usage

### Basic Recording

```bash
# From the recording directory
cd /workspaces/aesthetic-computer/reference/tools/recording

# Record a piece for 3 seconds at 60fps and export as GIF
node tape.mjs ../../../system/public/aesthetic.computer/disks/starfield.mjs 3000 60 --gif

# Record for 2 seconds at 60fps (no GIF export)
node tape.mjs ../../../system/public/aesthetic.computer/disks/starfield.mjs 2000 60
```

### Parameters

- **piece**: Path to the AC piece (relative to recording directory)
- **duration**: Recording duration in milliseconds
- **fps**: Target frames per second for recording
- **--gif**: Optional flag to export as GIF

### Technical Details

- **Simulation Rate**: 120fps for smooth animation
- **Recording Rate**: User-specified (typically 60fps)
- **GIF Export**: 125fps (8ms frame delay) to match simulation rate
- **Pixel Accuracy**: Manual color mapping for perfect pixel fidelity
- **Canvas Size**: 256×256 pixels

## Features

✅ Dual-rate animation system (120fps sim, 60fps recording)  
✅ Professional GIF export with optimized timing  
✅ Manual color mapping for pixel-perfect output  
✅ Terminal progress monitoring  
✅ Comprehensive frame statistics  
✅ PNG final frame capture  

## Output Files

All generated files are saved to `../output/`:
- `tape-YYYY-MM-DDTHH-MM-SS.gif` - Exported GIF animation
- `tape-YYYY.MM.DD.HH.MM.SS.SSS.png` - Final frame capture

## Performance

- Memory-efficient frame buffering
- Optimized color palette generation
- Fast manual pixel mapping
- Professional animation timing