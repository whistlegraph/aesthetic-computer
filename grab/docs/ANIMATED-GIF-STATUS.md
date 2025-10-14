# Animated GIF Feature Status

## ✅ Working Features

- **GIF Encoding**: Successfully encoding frames to animated GIF using `gifenc` library
- **PNG Decoding**: Decoding PNG frames to RGBA using `fast-png` library
- **Video Endpoint**: `/video/{width}x{height}/{piece}.gif?duration={seconds}`
- **Deployment**: Working in Cloudflare Workers production environment
- **Output**: Valid animated GIF files with proper headers

## ⚠️ Current Limitations

### Low Frame Rate (~0.4 FPS, 2 frames per 5 seconds)

**Issue**: CDP `Page.startScreencast` only captures 1-2 frames during a 5-second recording period, regardless of parameters.

**Investigation Results**:
- ✅ Event listener order: Confirmed correct (listener before `startScreencast`)
- ✅ Frame acknowledgment: Working properly
- ✅ Navigation timing: Tested before and after navigation
- ✅ Content rendering: Confirmed 2-second wait for render
- ✅ Parameters tested:
  - `format`: 'png' and 'jpeg' (both same result)
  - `quality`: 60-80 (no effect)
  - `maxWidth`/`maxHeight`: Tested various sizes
  - `everyNthFrame`: 1 (capture every frame)

**Comparison**:
- Previous POC (returning PNG frames): **162 frames in 5s** (~32 FPS)
- Current (with GIF encoding): **2 frames in 5s** (~0.4 FPS)

**Hypothesis**: The CDP screencast API in Cloudflare Workers' Browser Rendering environment appears to be heavily throttled or behaves differently than regular Chrome. The frame encoding process happens AFTER capture, so it shouldn't affect capture rate.

## 🔧 Tested Parameters

```typescript
await client.send('Page.startScreencast', {
  format: 'png',          // Tried: 'png', 'jpeg'
  quality: 80,            // Tried: 60, 80
  maxWidth: params.width, // Tried: various resolutions
  maxHeight: params.height,
  everyNthFrame: 1,       // Tried: with and without
});
```

## 📊 Performance Metrics

- **Request Duration**: ~20-24 seconds for 5-second video
  - ~2s: Navigation + content load
  - ~5s: Recording duration
  - ~13-17s: Frame processing + GIF encoding
- **Output Size**: ~3-4KB for 2 frames at 600x400
- **Memory**: Frames stored in memory as Buffer array

## 🎯 Potential Solutions

### 1. Alternative Capture Method
Instead of CDP screencast, use a loop with `page.screenshot()`:
```typescript
for (let i = 0; i < targetFrames; i++) {
  const screenshot = await page.screenshot();
  frames.push(screenshot);
  await new Promise(resolve => setTimeout(resolve, frameDelay));
}
```
**Trade-off**: More control over frame rate, but higher CPU usage and potentially slower.

### 2. Client-Side Capture
Have the browser capture frames client-side and send them back:
```typescript
await page.evaluate(() => {
  const canvas = document.querySelector('canvas');
  // Capture frames using canvas.toDataURL() or similar
});
```
**Trade-off**: Requires injecting code, but might bypass CDP throttling.

### 3. Accept Lower Frame Rate
The current 0.4 FPS captures "before" and "after" snapshots, which could be useful for certain use cases (e.g., showing initial vs. final state).

### 4. Increase Duration
With longer durations, we might get more frames:
- 10s: Maybe 3-4 frames (~0.3-0.4 FPS)
- 30s: Maybe 10-12 frames (~0.3-0.4 FPS)

## 📝 Files Modified

- `/workspaces/aesthetic-computer/grab/src/index.ts`:
  - Added `recordVideo()` method
  - Integrated `gifenc` for GIF encoding
  - Integrated `fast-png` for PNG decoding
  - Video endpoint routing

- `/workspaces/aesthetic-computer/grab/package.json`:
  - Added `gifenc@1.0.3` (MIT, 0 dependencies)
  - Added `fast-png@7.0.1` (MIT, 3 dependencies)

## 🚀 Usage

```bash
# Generate 5-second animated GIF
curl "https://aesthetic-grab.aesthetic-computer.workers.dev/video/800x600/starfield.gif?duration=5" -o starfield.gif

# Generate 3-second animated GIF
curl "https://aesthetic-grab.aesthetic-computer.workers.dev/video/400x300/prompt.gif?duration=3" -o prompt.gif

# Check metadata
curl -I "https://aesthetic-grab.aesthetic-computer.workers.dev/video/600x400/starfield.gif?duration=5"
# Returns:
# X-Frame-Count: 2
# X-FPS: 0.40
# X-Duration: 5s
# X-Resolution: 600x400
# Content-Type: image/gif
```

## 🎨 Libraries Used

### gifenc
- **Version**: 1.0.3
- **License**: MIT
- **Dependencies**: 0
- **Size**: 172.5 KB unpacked
- **Purpose**: Fast JavaScript GIF encoder
- **Features**: Quantization, palette mapping, streaming encoder

### fast-png
- **Version**: 7.0.1
- **License**: MIT
- **Dependencies**: 3 (`@types/pako`, `iobuffer`, `pako`)
- **Size**: 160.1 KB unpacked
- **Purpose**: PNG decoder/encoder in pure JavaScript
- **Performance**: Fast decoding of PNG to RGBA data

## 📚 References

- CDP Protocol: https://chromedevtools.github.io/devtools-protocol/tot/Page/#method-startScreencast
- gifenc: https://github.com/mattdesl/gifenc
- fast-png: https://github.com/image-js/fast-png
- Cloudflare Browser Rendering: https://developers.cloudflare.com/browser-rendering/

## 🔮 Next Steps

1. **Test Alternative Capture**: Implement loop-based screenshot capture
2. **Investigate CDP Throttling**: Research Cloudflare Workers Browser Rendering limitations
3. **Optimize for 0.4 FPS**: Adjust UI/expectations for current frame rate
4. **Add JPEG Support**: Consider JPEG format for smaller file sizes
5. **R2 Storage**: For longer videos, store intermediate frames in R2
