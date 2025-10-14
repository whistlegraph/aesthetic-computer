# Video Capture - Future Work

## Context

Animated GIF/video capture was explored using Cloudflare Workers' Browser Rendering API but proved unsuitable for this infrastructure.

## What Was Tried

### Approach: CDP Screencast API
- Used Chrome DevTools Protocol `Page.startScreencast`
- Captured frames as PNG from browser rendering
- Decoded PNG to RGBA using `fast-png`
- Encoded to animated GIF using `gifenc`

### Results
- ✅ **Technical Success**: Full pipeline worked correctly
- ❌ **Frame Rate**: Only 0.4 FPS (2 frames per 5 seconds)
- ❌ **Performance**: Too slow for practical video capture

### Investigation
Tested all available CDP parameters:
- `format`: PNG, JPEG
- `quality`: 60-80
- `maxWidth`/`maxHeight`: Various resolutions
- `everyNthFrame`: 1 (capture every frame)
- Navigation timing: Before/after page load
- Event listener ordering: Confirmed correct

**Conclusion**: CDP screencast in Cloudflare Workers Browser Rendering is heavily throttled, making it unsuitable for video capture.

## Recommended Approach

### Infrastructure: Dedicated Video Server

Similar to the `/at` ATProto PDS deployment, video capture should use a **dedicated Digital Ocean VPS**:

#### Why Digital Ocean VPS?
1. **GPU Support**: For hardware-accelerated encoding (H.264, VP9, etc.)
2. **Persistent Resources**: Can maintain browser instances efficiently
3. **Full Control**: No serverless limitations
4. **Cost Effective**: Pay for what you use, not per request

#### Suggested Stack
```
┌─────────────────────────────────────────┐
│  Cloudflare Worker (grab.aesthetic.com)│
│  - Screenshot generation (current)     │
│  - Request routing                     │
└─────────────────┬───────────────────────┘
                  │
                  ├─> Icon/Preview → Browser Rendering API
                  │
                  └─> Video → Forward to VPS
                                    ↓
┌─────────────────────────────────────────┐
│  Digital Ocean VPS                      │
│  - Headless Chrome/Puppeteer           │
│  - FFmpeg with GPU encoding            │
│  - Frame capture at 30-60 FPS          │
│  - R2/S3 storage for rendered videos   │
└─────────────────────────────────────────┘
```

#### Implementation Options

**Option A: Real-time Capture**
```javascript
// VPS server with Puppeteer
const browser = await puppeteer.launch({ 
  args: ['--enable-gpu-rasterization'] 
});
const page = await browser.newPage();

// Capture frames at 30 FPS
const frames = [];
const frameInterval = 1000 / 30;
for (let i = 0; i < targetFrames; i++) {
  const screenshot = await page.screenshot({ encoding: 'binary' });
  frames.push(screenshot);
  await page.waitForTimeout(frameInterval);
}

// Encode with FFmpeg
execSync(`ffmpeg -framerate 30 -i frame_%d.png output.mp4`);
```

**Option B: Canvas Recording**
```javascript
// Client-side in aesthetic.computer pieces
const canvas = document.querySelector('canvas');
const stream = canvas.captureStream(30); // 30 FPS
const recorder = new MediaRecorder(stream);

// Upload chunks to VPS for processing
recorder.ondataavailable = async (e) => {
  await fetch('https://video.aesthetic.computer/upload', {
    method: 'POST',
    body: e.data
  });
};
```

**Option C: Hybrid Approach**
1. Cloudflare Worker initiates request
2. VPS spawns browser and captures frames
3. Store frames in R2/S3
4. Return signed URL to client
5. Async encode video in background

### Cost Estimates

#### Digital Ocean Droplet
- **Basic**: $12/month (2GB RAM, 1 vCPU) - Light usage
- **Standard**: $24/month (4GB RAM, 2 vCPUs) - Medium usage  
- **GPU**: ~$200/month (GPU-enabled droplet) - High performance

#### Cloudflare R2 Storage
- First 10GB free
- $0.015/GB/month thereafter
- No egress fees

### Similar Infrastructure

Reference the `/at` PDS deployment:
- **Location**: `/workspaces/aesthetic-computer/at/`
- **Setup**: Digital Ocean VPS running ATProto PDS
- **Deployment**: Automated with `at/deploy.fish`
- **Model**: Long-running service on dedicated hardware

## Archive

Experimental video code saved in:
- `ANIMATED-GIF-STATUS.md` - Full investigation results
- `VIDEO-RECORDING.md` - Original POC documentation (if exists)
- Git history: Commits leading up to `ea5c6ace-b0da-4891-88ce-d1d9c158e0d5`

## Next Steps

When video capture is needed:

1. **Provision VPS**
   - Digital Ocean account setup
   - Choose droplet size based on usage
   - Consider GPU if budget allows

2. **Install Software**
   - Node.js + Puppeteer
   - FFmpeg with GPU support
   - Nginx for request handling

3. **Implement Capture Service**
   - API endpoint for capture requests
   - Frame capture at target FPS
   - Video encoding pipeline
   - Upload to R2/CDN

4. **Update grab Worker**
   - Add `/video/` endpoint routing
   - Forward to VPS
   - Handle async responses
   - Return CDN URLs

5. **Monitor & Scale**
   - Track encoding times
   - Monitor CPU/GPU usage
   - Scale vertically or horizontally as needed

## References

- ATProto PDS: `/workspaces/aesthetic-computer/at/`
- Cloudflare Browser Rendering: https://developers.cloudflare.com/browser-rendering/
- Digital Ocean VPS: https://www.digitalocean.com/products/droplets
- FFmpeg GPU: https://trac.ffmpeg.org/wiki/HWAccelIntro
- Puppeteer: https://pptr.dev/
