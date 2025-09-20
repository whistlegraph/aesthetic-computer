# Deprecated Recording Tools

This folder contains deprecated recording tools that have been superseded by the new **orchestrator.mjs** pipeline.

## Migration Summary

**Date**: September 20, 2025  
**Reason**: Simplified rendering pipeline using orchestrator.mjs + frame-renderer.mjs

## Current Active Pipeline

The recording system now uses:

```
orchestrator.mjs        # Main orchestrator
├── frame-renderer.mjs  # Renders individual frames  
├── headless.mjs        # Base AC system
├── logger.mjs          # Logging utility
└── elcid-flyer.mjs     # Example piece
```

**Usage**: `node orchestrator.mjs elcid-flyer 30000`

## Deprecated Files

### 🗂️ `tape.mjs` (1849 lines)
- **Purpose**: Legacy animation recorder with GIF/PNG export
- **Why deprecated**: Replaced by simpler orchestrator + frame-renderer approach
- **Features**: 
  - Complex GIF encoding with gifenc
  - Sixel terminal preview
  - Multiple export formats (GIF, PNG, MP4)
  - KidLisp piece support via $code
- **Migration**: Use `orchestrator.mjs` for MP4 exports instead

### 🗂️ `dashboard-main.mjs` (633 lines)  
- **Purpose**: TUI dashboard controller for recording process
- **Why deprecated**: 
  - Imports missing `dashboard.mjs` file (broken dependency)
  - Overly complex for current workflow
- **Features**:
  - Process orchestration with TUI
  - Session tracking and summaries
  - Frame counting and progress monitoring
- **Migration**: Use `orchestrator.mjs` directly (has built-in progress)

### 🗂️ `television.mjs` (247 lines)
- **Purpose**: Sixel preview generator for terminal display  
- **Why deprecated**: Terminal preview not needed in current workflow
- **Features**:
  - Raw RGBA to sixel conversion
  - File watching for live updates
- **Migration**: No replacement needed (orchestrator shows progress without preview)

### 🗂️ `burn-flyer.mjs` (44 lines)
- **Purpose**: High-resolution elcid-flyer renderer using dashboard
- **Why deprecated**: Depends on broken `dashboard-main.mjs`
- **Features**: Wrapper script for dashboard rendering
- **Migration**: Use `node orchestrator.mjs elcid-flyer 30000` directly

### 🗂️ `noise-test.mjs` (92 lines)
- **Purpose**: Stress test piece for blur patterns
- **Why deprecated**: Test piece, not core rendering infrastructure
- **Features**: Generates noise patterns for testing blur performance
- **Migration**: Can be kept if needed for testing, but not part of core workflow

### 🗂️ `TAPE_USAGE.md`
- **Purpose**: Documentation for legacy tape.mjs system
- **Why deprecated**: Documents the deprecated tape.mjs workflow
- **Migration**: See main README.md for orchestrator usage

## Key Differences: Old vs New

| Feature | Old (tape.mjs) | New (orchestrator.mjs) |
|---------|----------------|------------------------|
| **Memory** | Kept frames in memory | Stateless, frame-by-frame |
| **Output** | GIF, PNG, MP4 | RGB frames → MP4 |
| **Preview** | Sixel terminal display | Progress bar |
| **Resume** | No state persistence | Resumable via state.json |
| **Complexity** | 1849 lines | ~400 lines total |
| **Dependencies** | Many (chalk, boxen, figlet, gifenc) | Minimal (Node.js built-ins) |

## If You Need These Tools

If you need any of these deprecated tools for specific use cases:

1. **GIF Export**: The new pipeline only exports MP4. If you need GIF, you can:
   - Use `ffmpeg` to convert MP4 → GIF: `ffmpeg -i video.mp4 output.gif`
   - Or temporarily restore `tape.mjs` for specific projects

2. **Terminal Preview**: If you need live preview:
   - The new pipeline shows progress but no visual preview
   - Could be added back if needed

3. **Dashboard UI**: The TUI dashboard could be rebuilt if needed:
   - Would need to create the missing `dashboard.mjs` file
   - Or simplify `dashboard-main.mjs` to work without it

## Safety Notes

- These files have been moved, not deleted, so they can be restored if needed
- The package.json dependencies for these tools are still installed
- The core orchestrator pipeline is much simpler and more reliable
- All functionality has been replaced or is unnecessary

## Contact

If you need to restore any of these tools or have questions about the migration, ask Jeffrey.