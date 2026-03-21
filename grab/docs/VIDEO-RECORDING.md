# Video Recording Feature - Technical Documentation

## Overview

The grab worker now supports **video recording** using Puppeteer's Chrome DevTools Protocol (CDP) screencast API. This captures frames of animated aesthetic.computer pieces over time.

**Status:** ✅ Working (POC stage - returns frame sequence, not encoded video)

---

## API Endpoint

### `/video/{width}x{height}/{piece}.mp4`

Records a video of an aesthetic.computer piece.

**Method:** GET

**URL Format:**
```
https://grab.aesthetic.computer/video/{width}x{height}/{piece}.mp4
```

**Query Parameters:**
- `duration` (optional) - Recording duration in seconds (default: 5, max: 30)

**Examples:**
```bash
# Record 5 seconds (default) at 800x600
curl "https://grab.aesthetic.computer/video/800x600/prompt.mp4" -o output.png

# Record 3 seconds at 1280x720
curl "https://grab.aesthetic.computer/video/1280x720/line.mp4?duration=3" -o output.png

# Record 10 seconds at 1920x1080
curl "https://grab.aesthetic.computer/video/1920x1080/wipe.mp4?duration=10" -o output.png
```

---

## Current Implementation

### What It Does

1. ✅ Starts browser and navigates to piece
2. ✅ Captures frames using CDP `Page.startScreencast`
3. ✅ Records for specified duration (1-30 seconds)
4. ✅ Returns metadata in headers
5. ⚠️ Currently returns **last frame as PNG** (not full video)

### Response Headers

```http
HTTP/2 200
Content-Type: image/png
X-Frame-Count: 162           # Number of frames captured
X-Duration: 5s               # Recording duration
X-Resolution: 800x600        # Video resolution
Cache-Control: public, max-age=3600
```

### Frame Rate

Actual frame rate varies based on browser rendering speed:
- **5 seconds**: ~162 frames (~32 fps)
- **3 seconds**: ~33 frames (~11 fps)  
- **10 seconds**: ~300+ frames (~30 fps)

---

## Technical Details

### Screencast API

Uses Puppeteer's CDP session:

```typescript
const client = await page.createCDPSession();

await client.send('Page.startScreencast', {
  format: 'png',
  quality: 80,
  everyNthFrame: 1, // Capture every frame
});

client.on('Page.screencastFrame', async (frame: any) => {
  await client.send('Page.screencastFrameAck', { 
    sessionId: frame.sessionId 
  });
  
  const buffer = Buffer.from(frame.data, 'base64');
  frames.push(buffer);
});
```

### Workflow

```
1. Parse /video/WxH/piece.mp4?duration=N
   ↓
2. Launch browser (or reuse existing)
   ↓
3. Create CDP session
   ↓
4. Start screencast capture
   ↓
5. Navigate to piece URL
   ↓
6. Wait for canvas + render time
   ↓
7. Record frames for N seconds
   ↓
8. Stop screencast
   ↓
9. Return frames (currently: last frame as PNG)
```

---

## Limitations & Future Work

### Current Limitations

1. ⚠️ **No video encoding** - Returns PNG frames, not MP4/WebM
2. ⚠️ **Single frame returned** - Only last frame sent to client
3. ⚠️ **No audio** - Visual only
4. ⚠️ **Memory usage** - Stores all frames in memory
5. ⚠️ **Max 30 seconds** - To prevent abuse and memory issues

### Why No Video Encoding?

Cloudflare Workers **cannot run ffmpeg** or similar video encoders:
- No native video encoding APIs
- No file system for temp files
- No heavy binary executables
- Limited CPU/memory per request

### Possible Solutions

**Option 1: Return Frame Sequence**
- Return all frames as multipart response
- Client assembles into video/GIF
- Requires client-side processing

**Option 2: Use R2 Storage + External Encoder**
- Store frames in R2
- Trigger external service (Durable Object or separate worker) to encode
- Return video URL after encoding completes
- Async workflow

**Option 3: Animated WebP/APNG**
- Encode frames into animated image format
- Supported natively in browsers
- Smaller than sending individual frames
- Still no audio

**Option 4: Frame Sampling**
- Return every Nth frame to reduce size
- Client interpolates missing frames
- Trade quality for transfer size

---

## Testing

### Test Different Durations

```bash
# 1 second (quick test)
curl -I "https://grab.aesthetic.computer/video/640x480/prompt.mp4?duration=1"

# 5 seconds (default)
curl -I "https://grab.aesthetic.computer/video/800x600/prompt.mp4"

# 10 seconds (longer animation)
curl -I "https://grab.aesthetic.computer/video/1280x720/wipe.mp4?duration=10"

# Max 30 seconds
curl -I "https://grab.aesthetic.computer/video/1920x1080/line.mp4?duration=30"
```

### Check Frame Count

```bash
curl -I "https://grab.aesthetic.computer/video/1280x720/prompt.mp4?duration=5" | grep X-Frame-Count
# Expected: X-Frame-Count: 150-180 (varies by rendering speed)
```

### Download Last Frame

```bash
curl "https://grab.aesthetic.computer/video/1920x1080/prompt.mp4?duration=3" -o last-frame.png
file last-frame.png
# Should show: PNG image data, 1920 x 1080
```

---

## Performance

### Timing Breakdown

| Phase | Time |
|-------|------|
| Browser launch | 2-3s |
| Page navigation | 2-4s |
| Canvas detection | 0.5-1s |
| Recording | {duration}s |
| Frame processing | 0.5-1s |
| **Total** | **~8s + duration** |

### Resource Usage

- **Memory**: ~2-5 MB per second of video (depends on resolution)
- **Frame size**: 10-20 KB per frame (PNG, 800x600)
- **5 second video**: ~150-200 frames = 2-3 MB in memory

---

## Use Cases

### Current (Frame Preview)
- ✅ Preview last frame of animated piece
- ✅ Verify piece renders correctly over time
- ✅ Test animation timing
- ✅ Capture specific moment in animation

### Future (Full Video)
- 🔄 Generate preview videos for social media
- 🔄 Create GIFs of animated pieces
- 🔄 Record piece demos for documentation
- 🔄 Time-lapse of generative pieces
- 🔄 Animation loops for marketing

---

## API Documentation

### Request

```http
GET /video/{width}x{height}/{piece}.mp4?duration={seconds} HTTP/2
Host: grab.aesthetic.computer
```

### Response (Current)

```http
HTTP/2 200 OK
Content-Type: image/png
Content-Length: 14010
X-Frame-Count: 162
X-Duration: 5s
X-Resolution: 800x600
Cache-Control: public, max-age=3600

[PNG image data of last frame]
```

### Error Responses

**Invalid dimensions:**
```http
HTTP/2 400 Bad Request
Content-Type: application/json

{
  "error": "Dimensions too large. Max: 1920x1080"
}
```

**Invalid duration:**
```http
HTTP/2 400 Bad Request
Content-Type: application/json

{
  "error": "Duration must be between 1 and 30 seconds"
}
```

**Recording failed:**
```http
HTTP/2 500 Internal Server Error
Content-Type: application/json

{
  "error": "No frames captured"
}
```

---

## Configuration

### Environment Variables

```toml
# wrangler.toml
MAX_VIEWPORT_WIDTH = "1920"      # Max video width
MAX_VIEWPORT_HEIGHT = "1080"     # Max video height
BROWSER_TIMEOUT_MS = "30000"     # Page load timeout
CACHE_TTL_SECONDS = "3600"       # Cache duration
```

### Recommended Settings

For video recording, you may want to:
- ✅ Increase viewport limits for HD video
- ✅ Increase browser timeout for complex pieces
- ✅ Adjust cache TTL based on content

---

## Integration Examples

### Generate Video Preview

```typescript
// Get last frame of 5-second recording
const response = await fetch(
  'https://grab.aesthetic.computer/video/1280x720/prompt.mp4?duration=5'
);

const frameCount = response.headers.get('X-Frame-Count');
const duration = response.headers.get('X-Duration');

console.log(`Recorded ${frameCount} frames over ${duration}`);

const imageBlob = await response.blob();
// Use as preview image
```

### Check Animation Completeness

```typescript
// Record piece for 10 seconds
const response = await fetch(
  'https://grab.aesthetic.computer/video/1920x1080/wipe.mp4?duration=10'
);

const frameCount = parseInt(response.headers.get('X-Frame-Count'));

// Expect ~30 fps
const expectedFrames = 10 * 30; // 300 frames
const actualFPS = frameCount / 10;

console.log(`Actual FPS: ${actualFPS}`); // ~25-35 fps
```

---

## Comparison: Screenshot vs Video

| Feature | Screenshot | Video |
|---------|-----------|-------|
| **Endpoint** | `/icon/` or `/preview/` | `/video/` |
| **Duration** | Instant (3s render) | 1-30 seconds |
| **Output** | Single PNG frame | Frame sequence (PNG) |
| **File size** | 5-20 KB | 2-5 MB |
| **Use case** | Static preview | Animation capture |
| **Query params** | `?icon=WxH` | `?duration=N` |
| **Response time** | ~8-10s | ~10-40s |

---

## Next Steps

To make this production-ready with actual video output:

1. **Implement frame encoding** (WebP/APNG)
2. **Add R2 storage** for frame persistence
3. **Create encoding worker** (separate DO)
4. **Async video generation** with webhook/callback
5. **Add audio support** (requires piece audio API)
6. **Optimize frame capture** (skip duplicate frames)
7. **Add progress API** (check encoding status)

---

## Summary

**Current Status:**
- ✅ Video recording API implemented
- ✅ Frame capture working (CDP screencast)
- ✅ Configurable duration (1-30s)
- ✅ Returns last frame as preview
- ⚠️ No video encoding yet (limitation of Workers platform)

**Frame Capture Performance:**
- 800x600: ~162 frames in 5s (~32 fps)
- 1280x720: ~33 frames in 3s (~11 fps)
- Varies by piece complexity and rendering speed

**Access:**
```bash
curl "https://grab.aesthetic.computer/video/1280x720/prompt.mp4?duration=5" -o frame.png
```

The foundation is built - full video encoding would require additional infrastructure (R2 + encoding service).
