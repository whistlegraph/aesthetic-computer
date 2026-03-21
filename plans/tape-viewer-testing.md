# Testing the Tape Viewer

## Quick Start

### 1. Create a Test Tape
Open Aesthetic Computer and run:
```
tape 3 notepat
```

This will:
- Record 3 seconds of the `notepat` piece
- Upload frames + audio to S3 as a ZIP
- Generate a short code (e.g., `#abc`)
- Display the code in the console

Look for the console message:
```
📼 Tape code: #abc
```

### 2. View the Tape
Load the tape viewer with your code:
```
tape %abc
```

Replace `abc` with your actual tape code.

### 3. Test Playback
- Click **Play** button to start playback
- Press **Space** to pause/resume
- Use **Arrow keys** to navigate frames (when paused)
- Press **Home** to jump to start
- Press **End** to jump to end

### 4. Test Download
- Click **Download** button to download the original ZIP file
- Should download as `tape-abc.zip`

### 5. Test Show Mode
Try lightbox mode:
```
tape %abc:show
```

Should display without UI buttons (just the video playback).

## Expected Behavior

### Loading Phase
1. Display "Loading..." with animated ellipsis
2. Fetch tape metadata from MongoDB
3. Download ZIP from S3 (user or guest bucket)
4. Extract frames, timing.json, metadata.json, soundtrack.wav
5. Display first frame with Play button

### Playback Phase
1. Click Play → frames advance based on timing.json
2. Audio starts synchronized with frames
3. Frame counter updates: "Frame 1/90"
4. Time display shows: "0.0s / 3.0s"
5. Playback indicator shows: ▶ or ⏸

### End of Tape
1. Playback stops automatically at last frame
2. Audio stops cleanly
3. Can restart by clicking Play again

## Testing Different Scenarios

### Guest Tape (No Login)
```
tape 3 wand
```
- Should create anonymous tape
- Code generated (e.g., `#xyz`)
- Stored in `art-aesthetic-computer` bucket
- Viewable with `tape %xyz`

### User Tape (Logged In)
```
tape 5 notepat
```
- Should create user-owned tape
- Code generated and associated with user
- Stored in `user-aesthetic-computer` bucket
- Viewable with `tape %code`

### Different Pieces
Try recording different pieces:
```
tape 3 wand
tape 5 line
tape 2 prompt
```

Each should generate unique codes and be viewable.

### Error Cases

**Non-existent Code:**
```
tape %zzz
```
Expected: "Tape %zzz not found (404)"

**Invalid Code:**
```
tape %
```
Expected: "Enter a tape code like: tape %abc"

**Nuked Tape:**
If a tape has been deleted:
Expected: "Tape %abc has been deleted"

## Console Logs to Check

### During Load:
```
📼 tape.mjs boot - params: [...] hash: abc
📼 Loading tape by code from hash: %abc
📡 Fetching tape metadata for code: abc
📊 Tape metadata: { slug: "...", bucket: "...", ... }
📦 Downloading ZIP from: https://...
📦 ZIP downloaded: 3.34 MB
📦 ZIP loaded, files: ["frame-00000.png", ...]
📋 Metadata: { piece: "notepat", totalDuration: 3.0, ... }
⏱️  Timing data: 90 frames
🖼️  Loading 90 frames...
  Loaded 10/90 frames
  Loaded 20/90 frames
  ...
  Loaded 90/90 frames
🎵 Loading soundtrack.wav...
🎵 Audio loaded: 3.00s
✅ Tape loaded successfully!
```

### During Playback:
```
🎵 Audio playback started at offset 0.00s
```

### On Download:
```
(Browser download dialog should appear)
```

## Known Limitations

### Current Version
- No scrubbing/seek bar (coming soon)
- No speed controls (1x only)
- No loop option
- No volume control
- All frames loaded into memory (may use significant RAM for long tapes)

### Browser Requirements
- Modern browser with:
  - `createImageBitmap` API
  - Web Audio API
  - Fetch API
- Audio may require user interaction to start (autoplay policy)

## Troubleshooting

### "Tape %abc not found"
- Check if the tape code is correct
- Verify tape exists in MongoDB
- Check console for API response status

### No audio playback
- Check console for audio loading errors
- Try clicking Play button (may need user interaction)
- Verify `soundtrack.wav` exists in ZIP
- Check browser audio permissions

### Frames not advancing
- Check console for timing data
- Verify `timing.json` exists in ZIP
- Check if browser is throttling animations

### ZIP download fails
- Check S3 URL in console
- Verify bucket permissions
- Check network connectivity

## Next Steps

After successful testing:
1. Create tapes with different pieces
2. Test with different durations (1s, 10s, 30s)
3. Share tape codes with others
4. Test on mobile devices
5. Test in different browsers

## Feedback

Report any issues or suggestions:
- Frame timing accuracy
- Audio sync quality
- UI responsiveness
- Loading performance
- Error handling
