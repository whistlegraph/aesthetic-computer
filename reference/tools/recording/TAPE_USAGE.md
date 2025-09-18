# Tape.mjs Usage Guide

## Quick Reference

The tape tool records KidLisp pieces ($code) into animated GIFs or PNG sequences.

### Basic Usage
```bash
node tape.mjs <piece-path> [options]
```

### Examples
```bash
# Record $roz for 120 frames at 256x256 resolution
node tape.mjs '$roz' --frames 120 --fps 60 --resolution 256

# Record $cow for 10 frames at 1024x1024 with timing info
node tape.mjs '$cow' --frames 10 --resolution 1024 --timing

# Export PNG instead of GIF
node tape.mjs '$rose' --png

# Override default 60fps
node tape.mjs '$piece' --fps 30

# Experimental features
node tape.mjs '$roz' --blocks  # Block-based processing
node tape.mjs '$roz' --simd    # SIMD processing
```

### Options
- `--frames N` - Number of frames to record
- `--fps N` - Frames per second (default: 60)
- `--resolution N` - Canvas size NxN (default: 768)
- `--png` - Export PNG instead of default GIF
- `--timing` - Show detailed timing information
- `--blocks` - 🧪 Experimental block-based processing
- `--simd` - 🚀 Experimental SIMD processing

### Output
- GIFs saved to: `/workspaces/aesthetic-computer/reference/tools/output/`
- Filename format: `tape-YYYY-MM-DDTHH-MM-SS.gif`
- Frame cache: `.tape-frames-cache/` (auto-cleaned)

### Legacy Format
```bash
node tape.mjs test-complex.mjs 2000 30
# piece-file duration(ms) fps
```

### Notes
- Requires dev server running on localhost:8888 for KidLisp pieces
- Uses disk-based frame caching for scalability
- Displays 128x128 sixel preview in terminal during recording
- GIF is default export format (PNG optional with --png)
- 60fps default for smooth animations