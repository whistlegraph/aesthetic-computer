# AC-Pack Tape Recording Guide

## Overview
`ac-pack` can package OBJKTs using pre-recorded tape archives from the aesthetic.computer client. This method gives you full control over the animation recording and generates multiple optimized GIF covers automatically.

## Quick Start

### Option 1: Interactive Mode (Recommended)
```bash
node objkt/ac-pack.mjs piece-name
```
The tool will ask if you want to record a tape first and guide you through the process.

### Option 2: Direct Tape Usage
If you already have a tape file:
```bash
node objkt/ac-pack.mjs piece-name --tape ./path/to/tape.zip --tape-start 50
```

## Recording a Tape

### In the aesthetic.computer client:
1. Navigate to your piece (e.g., `aesthetic.computer/piece-name`)
2. Press **`r`** to start recording
3. Let your piece run and animate
4. Press **`r`** again to stop recording
5. The tape will be saved as a ZIP file with timestamp (e.g., `piece-name-2025.10.08.14.30.00.123.zip`)
6. Download the tape ZIP from your browser's download folder

### Tape Contents
The ZIP contains:
- **PNG frames**: Sequential frames of your animation (e.g., `frame000000.png`, `frame000001.png`, ...)
- **timing.json**: Frame timing data (filename, duration, timestamp for each frame)
- **metadata.json**: Recording metadata (dimensions, scale, duration)
- **soundtrack.wav**: Audio recording (if any)

## AC-Pack Options

### `--tape <path>`
Path to your tape ZIP file. When provided, ac-pack will:
- Skip the orchestrator/renderer completely
- Extract frames from the tape
- Generate all covers from the recorded animation

Example:
```bash
node objkt/ac-pack.mjs $4bb --tape ./\$4bb-2025.10.08.14.30.00.123.zip
```

### `--tape-start <percentage>`
Start the cover animation from a specific percentage of the tape (0-100).
Useful for skipping black/loading frames at the beginning.

Example:
```bash
node objkt/ac-pack.mjs $4bb --tape ./tape.zip --tape-start 50
```
This starts the animation from the middle frame (50% through the tape).

## Generated Outputs

When using `--tape`, ac-pack automatically generates **four optimized GIF versions**:

### 1. Main Cover (`-cover.gif`)
- **Size**: 512x512 pixels
- **Frame Rate**: 50fps (constant, matches tape)
- **Colors**: 128 colors with Bayer dithering
- **Use**: Primary OBJKT cover
- **Typical Size**: ~46MB (full quality, all frames from tape-start to end)

### 2. Twitter/X Cover (`-x.gif`)
- **Size**: 800x800 pixels
- **Frame Rate**: 24fps
- **Duration**: 11 seconds (264 frames)
- **Colors**: 16 colors with Bayer dithering (scale 5)
- **Use**: Social media sharing (Twitter/X has 15MB limit)
- **Typical Size**: ~14.5MB (under 15MB limit)

### 3. objkt.com Cover (`-objkt.gif`)
- **Size**: 600x600 pixels
- **Frame Rate**: 4fps (0.25 seconds per frame - slow slideshow)
- **Frames**: 6 frames (evenly sampled from tape)
- **Colors**: 256 colors (full palette, no reduction!)
- **Use**: Marketplace listing preview
- **Typical Size**: ~1.6MB (well under 2MB objkt.com limit)
- **Style**: Contemplative slideshow with rich, full-color quality

### 4. Icon Set (`icon.gif` + PNGs)
- **Animated GIF**: 128x128 pixels, 64 colors, all frames
- **Static PNGs**: 128x128, 256x256, 512x512 (from final frame)
- **Use**: Favicon, app icons, thumbnails
- **Typical Size**: ~4.2MB (animated)

## Example Workflow

### Complete tape-based packaging:
```bash
# 1. Record your piece in the browser (press 'r' twice)
# 2. Download the tape ZIP
# 3. Package with tape, starting from 50%
node objkt/ac-pack.mjs mypiece --tape ./mypiece-2025.10.08.14.30.00.123.zip --tape-start 50

# Output files created:
# - @author-mypiece-timestamp-cover.gif     (46MB - main)
# - @author-mypiece-timestamp-x.gif         (14.5MB - Twitter)
# - @author-mypiece-timestamp-objkt.gif     (1.6MB - objkt.com)
# - icon/128x128/mypiece.gif                (4.2MB - animated icon)
# - icon/128x128/mypiece.png                (static icon)
# - icon/256x256/mypiece.png                (static icon)
# - icon/512x512/mypiece.png                (static icon)
# - @author-mypiece-timestamp.zip           (50.9MB - complete package)
```

## Tips

### Finding the Right Start Percentage
- Use `--tape-start 0` for the full recording
- Use `--tape-start 25` to skip the first quarter (if loading/black frames)
- Use `--tape-start 50` to start from the middle
- Experiment to find the best starting point for your piece

### File Size Optimization
All three GIF covers use different optimization strategies:
- **Main cover**: Balanced quality (128 colors) for Teia
- **Twitter cover**: Optimized for 15MB social media limit (16 colors, shorter duration)
- **objkt.com cover**: Slideshow preview (6 frames, full colors, slow pace)

### When to Use Tape vs. Orchestrator
**Use tape when**:
- You want precise control over the animation
- Your piece has complex timing or interactions
- You want to capture a specific run/performance
- You need consistent output across all covers

**Use orchestrator (no --tape) when**:
- Your piece is simple and deterministic
- You want quick automated rendering
- You don't need precise frame control

## Troubleshooting

### "Tape file not found"
- Check the file path is correct
- Use relative or absolute paths
- Escape special characters in filenames (e.g., `\$` for `$` in bash)

### Cover is too large
- Increase `--tape-start` to use fewer frames
- The tool will warn if covers exceed their target limits
- Twitter cover must be under 15MB
- objkt.com cover should be under 2MB

### Colors look wrong
- Main cover uses 128 colors (high quality)
- Twitter cover uses 16 colors (optimized for size)
- objkt.com cover uses 256 colors (full palette slideshow)
- This is intentional for different use cases

## File Size Targets

| Cover Type | Resolution | Target Size | Purpose |
|------------|-----------|-------------|---------|
| Main | 512x512 | ~46MB | Teia primary cover (full quality) |
| Twitter/X | 800x800 | <15MB | Social sharing (X platform limit) |
| objkt.com | 600x600 | <2MB | Marketplace listing preview |
| Icon | 128x128 | ~4MB | Favicon/thumbnails |

## Notes

- All GIF outputs are created automatically when using `--tape`
- The objkt.com cover is a slow slideshow (6 frames, 0.25s each) with full color palette
- The Twitter cover is optimized for social media (24fps, 11 seconds, under 15MB)
- The main cover includes all frames from tape-start to end
- Icons use the final frame to show the most progressed state
- Soundtrack is noted but not bundled (keeps package size manageable)
