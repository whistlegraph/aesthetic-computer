# Recording Tools

Professional offline rendering system for Aesthetic Computer pieces.

## Directory Structure

```
recording/
├── orchestrator.mjs     # Main render coordinator
├── frame-renderer.mjs   # Individual frame renderer
├── headless.mjs         # AC system initialization
├── logger.mjs           # Logging utility
├── cleanup-*.sh         # Cleanup utilities
├── pieces/              # Pieces to be recorded
│   └── elcid-flyer.mjs  # Example event flyer piece
└── deprecated/          # Legacy tools (tape.mjs, etc.)

../output/               # Generated video files  
└── *.mp4               # Rendered MP4 videos
```

## Usage

### Basic Rendering

```bash
# From the recording directory
cd /workspaces/aesthetic-computer/reference/tools/recording

# Render elcid-flyer for 30 seconds (1800 frames at 60fps)
node orchestrator.mjs elcid-flyer 30000

# Render any piece for custom duration
node orchestrator.mjs piece-name 10000  # 10 seconds

# You can also use explicit paths if needed
node orchestrator.mjs pieces/elcid-flyer.mjs 30000
```

### Parameters

- **piece**: Name of the piece (e.g., "elcid-flyer" looks for pieces/elcid-flyer.mjs)
- **duration**: Recording duration in milliseconds
- **output-dir**: Optional custom output directory

The orchestrator automatically looks for pieces in the `pieces/` subfolder when you provide just a name.

### Technical Details

- **Frame Rate**: 60fps output
- **Resolution**: 2048×2048 pixels (configurable in piece)
- **Format**: Raw RGB frames → MP4 conversion
- **Memory**: Stateless rendering (zero memory leaks)
- **Resume**: Automatically resumes from state.json if interrupted

## Features

✅ **Stateless Rendering**: Each frame in fresh process (no memory leaks)  
✅ **Resumable**: Crashes/interruptions resume automatically  
✅ **High Resolution**: 2048×2048 default (piece-configurable)  
✅ **Progress Tracking**: Real-time progress bar with ETA  
✅ **MP4 Export**: Professional video output with H.264  
✅ **Background Continuity**: Preserves inter-frame state  

## Output Files

Generated in `../output/`:
- `piece-timestamp-duration.mp4` - Final rendered video
- Artifacts directory with RGB frames and state (auto-cleaned)

## Performance

- Zero memory leaks (stateless process-per-frame)
- Automatic resume from any point
- Efficient background buffer preservation
- High-quality H.264 encoding