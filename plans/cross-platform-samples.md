# Cross-Platform Sample Storage Plan

## Key Insight: Samples ARE Paintings

AC already has `pixel-sample.mjs` which encodes audio samples as RGB pixel data
in painting bitmaps. `stample.mjs` uses this today. Paintings have full
infrastructure: upload, CDN, short codes (`#k3d`), @handle ownership, track-media
API, and cross-platform rendering.

**We don't need a new storage system. Samples should be stored as paintings.**

Record audio → encode as bitmap → save as painting → get a short code → share.
Anyone loads the painting back as audio on web or native.

---

## Current State

### Web (stample.mjs + pixel-sample.mjs)
- `encodeSampleToBitmap(data, width)` — float32[] → RGB pixels (3 samples/pixel)
- `decodeBitmapToSample(bitmap, meta)` — RGB pixels → float32[]
- `loadPaintingAsAudio(source, opts)` — load a painting code/object as playable audio
- Paintings upload via `track-media.mjs` → DO Spaces + MongoDB + short code
- Download via `/media/@handle/painting/slug.png` or `/media/paintings/CODE.png`

### Native (ac-native)
- Samples stored as raw float32 PCM (rate + length + data) at `/mnt/ac-sample.raw`
- `audio_sample_save()` / `audio_sample_load()` in C
- `sound.sample.saveTo(path)` / `sound.sample.loadFrom(path)` JS bindings
- `sound.sample.getData()` → Float32Array, `sound.sample.loadData(f32, rate)` → load
- `/mnt/samples/` directory with `manifest.json` for local sample library
- No pixel-sample encoding yet, no painting upload capability

---

## Plan

### Phase 1: Native Pixel-Sample Bridge

**Add pixel-sample encoding/decoding to native JS pieces.**

Since native pieces run in QuickJS (no DOM, no Canvas), implement a pure-JS
version of the encode/decode that works without browser APIs:

**File: `fedac/native/pieces/lib/pixel-sample-native.mjs`**
- `encodeSampleToBitmap(float32Array, width)` — same algorithm as web version
  (float32 → 8-bit RGB, 3 samples per pixel)
- `decodeBitmapToSample(rgbaArray, width, height, sampleLength)` — reverse
- These are pure math — no DOM needed

**Modify: `fedac/native/pieces/samples.mjs`**
- When saving, also encode sample as bitmap PNG
- Store both `.raw` (for instant native reload) and `.png` (for upload/sharing)

**Requires:** A way to write PNG from native. Options:
1. Use the existing `graph.c` framebuffer snapshot capability
2. Add a minimal PNG writer in C (stb_image_write.h is ~1KB)
3. Encode as BMP (simpler header, no compression) — paintings can be any image format

### Phase 2: Native Painting Upload

**Let native devices upload paintings to the AC cloud.**

**Add C binding: `system.uploadPainting(localPath, handle, token)`**
- POST to `/api/track-media` with painting metadata
- GET presigned upload URL
- PUT the image file to DO Spaces
- Returns short code on success

**Or simpler: POST the bitmap directly.**
- Encode sample as pixel data in JS
- Use `system.fetchPost()` to send base64-encoded bitmap to a new
  `/api/sample-painting` endpoint that:
  1. Decodes the base64 bitmap
  2. Renders it as PNG via sharp/canvas
  3. Uploads to DO Spaces via existing painting pipeline
  4. Returns a short code

**Update samples.mjs:** Add `u` (upload) key that encodes the current sample
as a painting bitmap and uploads it. Shows the short code on success.

### Phase 3: Native Painting Download (Load Remote Samples)

**Let native devices load samples from painting codes.**

**Flow:**
1. User types a painting code (e.g., `#k3d`) in samples.mjs
2. `system.fetch("/api/painting-code?code=k3d")` → get slug + handle
3. `system.fetchBinary("https://aesthetic.computer/media/paintings/k3d.png", "/tmp/sample.png")`
4. Decode PNG → extract RGB pixels → `decodeBitmapToSample()` → load into audio

**Requires:** PNG decoding in native. Options:
1. Add `stb_image.h` to the C build (single-header PNG decoder, tiny)
2. Decode in JS using a pure-JS PNG decoder
3. Use the existing `graph.c` image loading if it supports PNG

### Phase 4: Unified samples.mjs (Web + Native)

**Create a web `samples.mjs` piece that mirrors the native one.**

**File: `system/public/aesthetic.computer/disks/samples.mjs`**

Uses the existing painting infrastructure:
- List user's paintings that are tagged as samples (metadata flag)
- Record new sample via `Microphone` API
- Encode → upload as painting via `track-media`
- Browse + play back via `loadPaintingAsAudio()`
- Share via short code
- Compatible with native — same painting, same code, playable on both

### Phase 5: notepat Cross-Platform Samples

**Both web and native notepat can load sample paintings.**

**Native notepat.mjs:**
- Add `stample CODE` command or sample bank that loads from painting codes
- After recording, offer to save as painting (upload to cloud)
- Saved samples show their short code in the status bar

**Web notepat.mjs:**
- Already has `stample.mjs` as a sibling piece
- Add sample mode that uses `loadPaintingAsAudio()` to load from codes
- Shared sample format means a sample recorded on bare metal can be
  played in the browser and vice versa

---

## Data Flow

```
Native Record             Web Record
     |                         |
  float32[]               float32[]
     |                         |
  encodeSampleToBitmap    encodeSampleToBitmap
     |                         |
  RGB pixels              RGB pixels
     |                         |
  upload as painting      upload as painting
     |                         |
     +----→  DO Spaces  ←------+
             + MongoDB
             + short code (#k3d)
                  |
     +------------+------------+
     |                         |
  download PNG            download PNG
     |                         |
  decodeBitmapToSample    decodeBitmapToSample
     |                         |
  float32[]               float32[]
     |                         |
  audio playback          audio playback
```

## Why Paintings, Not WAV

1. **Infrastructure exists** — upload, CDN, short codes, @handle, MongoDB, all done
2. **Visual** — you can SEE the sample as an image, share it as art
3. **Compact** — 8-bit RGB is ~4x smaller than float32 WAV for the same data
4. **Cross-platform** — PNG/image works everywhere, WAV needs special handling
5. **Social** — paintings are already the shareable unit in AC, samples become paintings
6. **stample.mjs already does this** — proven format, just extend it

## Storage Format

```
Painting PNG (256px wide, height varies):
  R channel: sample[i*3+0] → 8-bit (mapped from -1..1 to 0..255)
  G channel: sample[i*3+1]
  B channel: sample[i*3+2]
  A channel: 255 (opaque)

Metadata (in MongoDB painting record):
  type: "sample"
  sampleLength: number (exact sample count)
  sampleRate: 48000
  durationSecs: number
  source: "native" | "web"
```

A 5-second 48kHz sample = 240,000 samples = 80,000 pixels = 256×313 PNG ≈ 100KB.

## Key Files

| Existing | Purpose |
|----------|---------|
| `system/public/aesthetic.computer/lib/pixel-sample.mjs` | Encode/decode samples↔paintings |
| `system/public/aesthetic.computer/disks/stample.mjs` | Web sample-painting player |
| `system/netlify/functions/track-media.mjs` | Painting upload API |
| `system/netlify/functions/painting-code.mjs` | Short code → slug resolver |
| `system/netlify/functions/presigned-url.js` | CDN upload/download URLs |

| New/Modified | Purpose |
|-------------|---------|
| `fedac/native/pieces/lib/pixel-sample-native.mjs` | Pure-JS encode/decode for native |
| `fedac/native/pieces/samples.mjs` | Add upload/download/code support |
| `system/public/aesthetic.computer/disks/samples.mjs` | Web sample browser piece |
| `fedac/native/src/js-bindings.c` | PNG read/write bindings if needed |
