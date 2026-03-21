# Plan: Integrate Oskie Tokyo Audio into 1v1 Piece

## Overview
Add background music ("oskie - tokyo 90 bpm") to the 1v1.mjs multiplayer FPS piece, following the audio patterns used throughout Aesthetic Computer.

---

## Step 1: Audio File Conversion

### Current State
- Source file: `system/public/assets/oskie/oskie - tokyo 90 bpm.m4a` (670KB)

### Required Conversions
Following the pattern in `system/public/assets/sounds/` and `system/public/assets/bgm/`:

```bash
# Generate OGG version for non-Safari browsers
ffmpeg -i "system/public/assets/oskie/oskie - tokyo 90 bpm.m4a" \
  -c:a libvorbis -q:a 5 \
  "system/public/assets/oskie/oskie - tokyo 90 bpm.ogg"
```

### Final Asset Structure
```
system/public/assets/oskie/
├── oskie - tokyo 90 bpm.m4a  # Safari/iOS (already exists)
└── oskie - tokyo 90 bpm.ogg  # Chrome/Firefox/Edge
```

---

## Step 2: Modify 1v1.mjs

### 2.1 Add Audio State Variables (top of file, after imports)
```javascript
// 🎵 Background Music
let bgmSfx = null;        // The preloaded audio handle
let bgmPlaying = null;    // The playing audio instance
let bgmLoaded = false;    // Track if audio is loaded
```

### 2.2 Add Audio Loading to `boot()` function
Following the pattern from `booted-by.mjs`:
```javascript
function boot({ Form, CUBEL, QUAD, penLock, system, get, 
                net: { socket, udp, host, lan, devIdentity, preload }, 
                handle, help, sign, glyphs, platform }) {
  // ... existing boot code ...
  
  // 🎵 Load background music
  const audioExt = platform?.Safari ? "m4a" : "ogg";
  const bgmUrl = `/assets/oskie/oskie - tokyo 90 bpm.${audioExt}`;
  
  preload(bgmUrl).then((sfx) => {
    bgmSfx = sfx;
    bgmLoaded = true;
    console.log("🎵 1v1: Background music loaded");
  }).catch((err) => {
    console.warn("🎵 1v1: Failed to load background music:", err);
  });
}
```

### 2.3 Start Music on Game Start
Add to the "connected" WebSocket handler or when match begins:
```javascript
// Play BGM when game starts (in socket connected handler)
if (bgmSfx && !bgmPlaying) {
  bgmPlaying = sound.play(bgmSfx, {
    loop: true,
    volume: 0.5  // 50% volume for background music
  });
  console.log("🎵 1v1: Background music started");
}
```

### 2.4 Add Sound API to Function Signatures
Update the function signatures to include `sound`:
```javascript
// In boot:
function boot({ ..., sound, ... }) {

// Or in sim/act if starting music on interaction:
function sim({ sound, ... }) {
function act({ event: e, sound, ... }) {
```

### 2.5 Add Music Controls (Optional Enhancement)
```javascript
// Add to act() for toggling music with a key (e.g., 'M')
if (e.is("keyboard:down:m") || e.is("keyboard:down:M")) {
  if (bgmPlaying && !bgmPlaying.killed) {
    bgmPlaying.kill(0.5);  // Fade out
    bgmPlaying = null;
    console.log("🎵 Music muted");
  } else if (bgmSfx) {
    bgmPlaying = sound.play(bgmSfx, { loop: true, volume: 0.5 });
    console.log("🎵 Music unmuted");
  }
}
```

### 2.6 Stop Music on Piece Exit (Optional)
Add cleanup in a `leave()` function if needed:
```javascript
function leave() {
  if (bgmPlaying && !bgmPlaying.killed) {
    bgmPlaying.kill(0.3);
    bgmPlaying = null;
  }
}
export { boot, sim, paint, act, leave };
```

---

## Step 3: Execution Order

1. **Convert audio** (terminal command)
2. **Add state variables** to top of 1v1.mjs
3. **Update boot function** to include `preload` and `platform` in destructured params
4. **Add preload call** for bgm in boot()
5. **Add music start** when game connects
6. **Optional: Add M key toggle** for mute/unmute
7. **Test** at `https://localhost:8888/1v1`

---

## Reference Examples

### booted-by.mjs (simple preload)
```javascript
net.preload("startup").then((sfx) => (startupSfx = sfx));
// ...
sound.play(startupSfx);
```

### stample.mjs (looped playback)
```javascript
sounds[index] = sound.play(sampleId, { from, to, loop: true });
```

### prutti.mjs (browser-specific format)
```javascript
const ext = api.platform.Safari ? "m4a" : "ogg";
const soundUrl = `${path}/${name}.${ext}`;
api.net.preload(soundUrl).then((sfx) => { ... });
```

---

## Notes

- **Safari requires .m4a** (AAC audio) - .ogg is not supported
- **Chrome/Firefox/Edge prefer .ogg** (Vorbis) - more efficient
- **Looped playback** uses `{ loop: true }` option in `sound.play()`
- **Volume control** is 0-1 range (0.5 = 50%)
- The audio will autoplay when the piece loads and socket connects

---

## File Size Considerations

| Format | Typical Size | Use Case |
|--------|--------------|----------|
| .m4a (AAC) | ~650KB | Safari/iOS |
| .ogg (Vorbis) | ~500-600KB | Chrome/Firefox |

Both formats should be committed to assets for cross-browser support.
