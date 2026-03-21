# Tape Viewer Implementation

## Overview
Created a new `tape.mjs` piece that loads and plays back tape recordings from their ZIP files, similar to how `painting.mjs` works for paintings.

## Usage

### Loading a Tape
```
tape %code
```

Where `code` is the short tape code (e.g., `abc`, `xyz`, etc.)

Examples:
- `tape %abc` - Load and view tape with code "abc"
- `tape %abc:show` - Lightbox mode (no UI)
- Visit `aesthetic.computer/#%abc` - Direct URL access

### Visual Signifier
The `%` symbol was chosen as a visual signifier for tapes because:
- Resembles a film reel / movie symbol
- Distinct from `#` (used for paintings) 
- Indicates video/motion content

## Features Implemented

### Core Functionality
- ✅ Load tape metadata from MongoDB via `/api/get-tape?code=XXX`
- ✅ Download and parse ZIP files from S3 (user or guest buckets)
- ✅ Extract all frames, timing data, and metadata
- ✅ Display frames with proper scaling and centering
- ✅ Audio playback from `soundtrack.wav` (Web Audio API)
- ✅ Frame-accurate timing using timing.json

### Playback Controls
- ✅ Play/Pause button
- ✅ Keyboard controls:
  - `Space` - Toggle play/pause
  - `Arrow Left` - Previous frame (when paused)
  - `Arrow Right` - Next frame (when paused)
  - `Home` - Jump to first frame
  - `End` - Jump to last frame
- ✅ Frame counter display
- ✅ Time display (current/total duration)
- ✅ Playback indicator (▶/⏸)

### UI Features
- ✅ Download button - downloads original ZIP file
- ✅ Loading state with animated ellipsis
- ✅ Error handling with user-friendly messages
- ✅ Show mode (`:show` suffix) for lightbox/embed view
- ✅ Responsive frame scaling to fit screen

### Audio Integration
- ✅ Automatic detection of `soundtrack.wav` in ZIP
- ✅ Web Audio API integration
- ✅ Frame-synced audio playback
- ✅ Audio offset calculation for mid-tape playback
- ✅ Audio stops on pause/end

## Technical Details

### File Structure
The tape viewer expects ZIPs with this structure:
```
tape-code.zip
├── frame-00000.png
├── frame-00001.png
├── ...
├── timing.json        (frame durations)
├── metadata.json      (piece name, duration, etc.)
└── soundtrack.wav     (optional audio)
```

### API Endpoints Used
- `GET /api/get-tape?code=XXX` - Fetch tape metadata from MongoDB
  - Returns: `{ slug, code, when, bucket, user, mp4Status, nuked }`

### ZIP Loading
- Uses JSZip library (loaded dynamically from CDN)
- Downloads from appropriate S3 bucket (user vs guest)
- Extracts frames as ImageBitmaps for efficient rendering
- Parses JSON files for timing and metadata

### Timing System
- Uses `timing.json` for frame-accurate playback
- Each entry: `{ frame: number, duration: number (ms) }`
- Advances frames based on actual duration
- Fallback to 30fps if timing data missing

### Audio Synchronization
- Decodes WAV file using Web Audio API
- Calculates playback offset based on current frame
- Syncs audio start time with frame progression
- Handles audio ending gracefully

## Testing

To test the tape viewer:

1. **Record a tape** (if you haven't already):
   ```
   tape 3 notepat
   ```
   This will generate a tape code (e.g., `#abc`)

2. **View the tape**:
   ```
   tape %abc
   ```

3. **Try different modes**:
   ```
   tape %abc:show    (lightbox mode)
   ```

4. **Direct URL**:
   Navigate to `https://aesthetic.computer/#%abc`

## Code Location
- **Piece file**: `/workspaces/aesthetic-computer/system/public/aesthetic.computer/disks/tape.mjs`
- **Backend API**: `/workspaces/aesthetic-computer/system/netlify/functions/get-tape.mjs` (already exists)

## Comparison to painting.mjs

### Similarities
- Short code system (`#code` for paintings, `%code` for tapes)
- MongoDB lookup via API
- S3 file download
- Loading states and error handling
- Show mode support
- Download button

### Differences
- **File type**: ZIP (tapes) vs PNG (paintings)
- **Playback**: Animated sequence vs static image
- **Audio**: Includes soundtrack vs silent
- **Controls**: Play/pause/scrub vs static view
- **Timing**: Frame-based animation vs instant load

## Future Enhancements

### Planned Features
- [ ] Scrubbing timeline/progress bar
- [ ] Speed controls (0.5x, 1x, 2x)
- [ ] Loop toggle
- [ ] Frame-by-frame download (export individual frames)
- [ ] Share button (copy link)
- [ ] Fullscreen mode
- [ ] Volume control
- [ ] Keyboard shortcuts help overlay

### Integration Ideas
- [ ] Gallery view of user's tapes
- [ ] Related tapes suggestions
- [ ] Tape annotations/comments
- [ ] MP4 fallback (when conversion service ready)
- [ ] Social sharing previews

## Notes

### Browser Compatibility
- Requires browsers with:
  - `createImageBitmap` API (modern browsers)
  - Web Audio API (for audio playback)
  - Fetch API (for downloading)

### Performance
- All frames loaded into memory for smooth playback
- May use significant RAM for long tapes (100+ frames)
- Consider lazy loading for very long recordings

### Audio Autoplay
- Web Audio context may be suspended by browser autoplay policies
- First interaction (play button click) resumes audio context
- Audio starts synced with current frame position

## Integration with Existing System

The tape viewer integrates seamlessly with the existing ecosystem:

1. **Recording**: `tape X piece` creates tape → generates short code
2. **Upload**: ZIP uploaded to S3, record created in MongoDB
3. **Viewing**: `tape %code` loads and plays back recording
4. **Sharing**: Short codes make sharing easy (`%abc` vs full S3 URL)

This completes the tape workflow from creation to playback! 🎬📼
