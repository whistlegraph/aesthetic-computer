# Tape MP4 Conversion for ATProto Sync

## Current State
- Tapes are uploaded as ZIP files containing:
  - `frame-{n}.png` - Scaled frames (6x original size)
  - `timing.json` - Frame durations and timestamps
  - `metadata.json` - Original dimensions, scale, frame count, duration
- MongoDB `tapes` collection stores: `{ code, slug, when, bucket, user, nuked }`
- NO automatic MP4 conversion or ATProto sync yet

## Desired Architecture

### Regular Tape Creation (track-tape.mjs)
```javascript
// Client uploads ZIP to storage
// Server creates MongoDB record
// NO MP4 conversion
// NO ATProto sync
{
  code: "abc",
  slug: "wand-2025.10.23.12.30.00.123",
  when: Date,
  bucket: "user-aesthetic-computer",
  user: "auth0|123",
  nuked: false
}
```

### Sync Script (sync-atproto.mjs)
When syncing tapes to ATProto:
1. Download ZIP from storage
2. Extract frames from ZIP
3. Convert frames to MP4 using ffmpeg
4. Upload MP4 blob to ATProto
5. Create ATProto record with:
   - `zipUrl`: Direct link to download the ZIP
   - `video`: MP4 blob (for viewing in ATProto apps)
   - `acUrl`: Permalink to view on aesthetic.computer

### ATProto Record Structure
```javascript
{
  $type: "computer.aesthetic.tape",
  slug: "wand-2025.10.23.12.30.00.123",
  code: "abc",
  zipUrl: "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/auth0|123/slug.zip",
  acUrl: "https://aesthetic.computer/!abc",
  when: "2025-10-23T12:30:00.123Z",
  video: {  // MP4 blob
    $type: "blob",
    ref: { $link: "bafkreiabcdef..." },
    mimeType: "video/mp4",
    size: 5242880
  },
  ref: "mongoId"
}
```

## Implementation Tasks

### Phase 1: Basic Sync (Without MP4)
- [x] Add TAPE to MediaTypes enum
- [x] Add tape lexicon to media-atproto.mjs
- [x] Include zipUrl and acUrl in record
- [x] Log warning that MP4 conversion not implemented
- [ ] Test sync with jeffrey's tapes (if any)

### Phase 2: MP4 Conversion (Local)
- [ ] Create `tape-to-mp4.mjs` utility module:
  - [ ] Download ZIP from URL
  - [ ] Extract to temp directory
  - [ ] Read timing.json for frame durations
  - [ ] Use ffmpeg to create MP4 from frames
  - [ ] Return MP4 buffer
- [ ] Update TAPE buildRecord to call tape-to-mp4
- [ ] Add ffmpeg dependency check
- [ ] Test locally with sample tape

### Phase 3: Production Deployment
- [ ] Ensure ffmpeg available in production environment
- [ ] Add error handling for failed conversions
- [ ] Add retry logic for transient failures
- [ ] Monitor MP4 file sizes and conversion times
- [ ] Consider caching converted MP4s to avoid re-conversion

## Technical Notes

### ffmpeg Command
```bash
# Create MP4 from PNG sequence with variable frame durations
ffmpeg -r 60 \
  -i frame-%d.png \
  -c:v libx264 \
  -pix_fmt yuv420p \
  -movflags +faststart \
  output.mp4
```

### ZIP Structure
```
wand-2025.10.23.12.30.00.123.zip
├── frame-0.png
├── frame-1.png
├── frame-2.png
├── ...
├── timing.json
└── metadata.json
```

### timing.json Format
```json
{
  "frames": [
    { "index": 0, "duration": 16.67, "timestamp": 0 },
    { "index": 1, "duration": 16.67, "timestamp": 16.67 },
    ...
  ],
  "totalDuration": 4000
}
```

## Future Considerations
- **Caching**: Store converted MP4s to avoid re-conversion
- **Background Jobs**: Queue MP4 conversions for async processing
- **Quality Options**: Allow different MP4 quality levels
- **Alternative Formats**: Support WebM or other video formats
- **Frame Rate**: Consider variable frame rates from timing.json
