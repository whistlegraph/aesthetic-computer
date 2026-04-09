# Cross-Platform Sample Storage Plan

## Key Insight: Samples ARE Paintings

AC already has `pixel-sample.mjs` which encodes audio samples as RGB pixel data
in painting bitmaps. `stample.mjs` uses this today. Paintings have full
infrastructure: upload, CDN, short codes (`#k3d`), @handle ownership, track-media
API, and cross-platform rendering.

**Samples use the same `#` sigil and PNG storage format as paintings, but live in
a separate `samples` MongoDB collection with audio-specific metadata.**

---

## Architecture Decision: Shared Sigil, Separate Collection

### Why `#` (not a new sigil like `^`)
- Samples and paintings share the same PNG storage format
- A painting can BE a sample (load any image as audio via stample)
- A sample IS a painting (the pixel-encoded waveform is visible art)
- `%` and `&` are URL-unfriendly (`%` is URL escape, `&` is query separator)
- Keeps the sigil set small: `@` people, `$` code, `#` media, `*` time

### Why separate collection (not a flag on paintings)
- Clean querying: "list my samples" vs "list my paintings" without filters
- Separate indexes optimized for audio metadata (duration, sampleRate, etc.)
- Separate counts/quotas per media type
- Future-proof: audio-specific features (waveform preview, BPM detection)

### Shared code namespace
- `#` codes must be unique across BOTH `paintings` AND `samples` collections
- `generateUniqueCode()` checks both collections before assigning
- Other sigils (`$` KidLisp, `*` clock, tapes) have independent namespaces
- The code resolver checks both collections to find which type a `#code` refers to

---

## MongoDB Schema

### Collection: `samples`
```js
{
  _id: ObjectId,
  user: "auth0|63effeeb...",      // owner (null for guest)
  slug: "kick-drum",              // user-friendly name
  code: "k3d",                    // unique short code (shared with paintings)
  when: ISODate,                  // upload timestamp
  
  // Sample-specific metadata
  v: 1,                           // pixel-sample encoding version
  sampleRate: 48000,
  sampleLength: 240000,           // exact sample count
  duration: 5.0,                  // seconds
  channels: 1,
  source: "native",               // or "web"
  
  // Standard media fields
  ext: "png",                     // storage format
  width: 256,                     // image dimensions
  height: 313,
}
```

### Indexes
```js
// Unique code (shared namespace with paintings — enforced at generation time)
await samples.createIndex({ code: 1 }, { unique: true, sparse: true });

// User queries: "list my samples"
await samples.createIndex({ user: 1 });

// Audio-specific queries
await samples.createIndex({ duration: 1 });          // sort by length
await samples.createIndex({ "source": 1 });           // native vs web
await samples.createIndex({ v: 1 });                  // encoding version (for migration)
await samples.createIndex({ when: -1 });              // recent first
await samples.createIndex({ user: 1, slug: 1 }, { unique: true }); // per-user slugs
```

### Encoding Version Contract
```
v1 (current — pixel-sample.mjs):
  - 3 audio samples per pixel (R, G, B channels)
  - Float range -1.0..+1.0 mapped to 0..255
  - A channel: 255 (opaque)
  - Width: 256px (configurable)
  - Height: ceil(sampleLength / 3 / width)
  - Mono only
  - The `v` field MUST be stored on every record so decoders know which algorithm to use
```

---

## Implementation Plan

### Phase 1: Backend — track-media + code generation
**Files to modify:**

1. **`system/netlify/functions/track-media.mjs`**
   - Add `mediaType: "sample"` branch for PNG uploads with sample metadata
   - Route to `samples` collection instead of `paintings`
   - Store `v`, `sampleRate`, `sampleLength`, `duration`, `channels`, `source`

2. **`system/backend/generate-short-code.mjs`**
   - `generateUniqueCode()` accepts optional `siblingCollections` array
   - For `#` codes: checks both `paintings` AND `samples` before assigning
   - Other types unchanged (single collection check)

3. **`system/netlify/functions/painting-code.mjs`** (or equivalent resolver)
   - When resolving `#code`, check `paintings` first, then `samples`
   - Return `{ type: "painting" | "sample", ...record }` so the client knows

4. **New: `system/netlify/functions/list-samples.mjs`** (or extend existing)
   - `GET /api/samples/@handle` → list user's samples
   - `GET /api/samples/@handle/:slug` → get specific sample
   - Returns CDN URLs + metadata

### Phase 2: Native pixel-sample bridge
**Files to create/modify:**

1. **`fedac/native/pieces/lib/pixel-sample-native.mjs`**
   - Pure-JS encode/decode (no DOM, no Canvas — works in QuickJS)
   - Same algorithm as web `pixel-sample.mjs`
   - `encodeSampleToBitmap(float32Array, width)` → RGBA pixel array
   - `decodeBitmapToSample(rgbaArray, width, height, sampleLength)` → float32[]

2. **PNG write from native**
   - Option A: Add `stb_image_write.h` (single-header, ~1KB) for PNG encoding in C
   - Option B: Minimal PNG writer in JS (deflate + PNG header — ~100 lines)
   - Option C: BMP format (simpler, no compression, server converts to PNG on upload)

### Phase 3: Upload from native
**Flow:**
```
Record audio → float32[] → encodeSampleToBitmap → PNG bytes
  → POST /api/track-media { ext:"png", mediaType:"sample", sampleMeta:{v:1,...} }
  → GET presigned URL → PUT PNG to Spaces → #code returned
```

**Files:**
- `fedac/native/pieces/samples.mjs` — add upload key (`u`)
- `fedac/native/src/js-bindings.c` — `system.uploadMedia()` binding if needed
- Or use existing `system.fetch()` + `system.fetchPost()` for the API calls

### Phase 4: Download to native
**Flow:**
```
Type #code → resolve via /api/painting-code → get CDN URL
  → fetchBinary(url, /tmp/sample.png) → decode PNG → decodeBitmapToSample
  → sound.sample.loadData(float32, rate) → play
```

**Requires:** PNG decoding in native
- Option A: `stb_image.h` (single-header PNG decoder)
- Option B: Decode in JS (pure-JS PNG inflate)
- Option C: Server endpoint that returns raw PCM (avoid client-side PNG decode)

### Phase 5: Unified experience
- **notepat.mjs**: type `#code` to load a sample from cloud into sample bank
- **samples.mjs**: browse/record/upload/download on web and native
- **stample.mjs**: already works, becomes the web sample player
- **Gallery**: shows speaker icon on `#` codes that are samples
- **Profile**: separate "samples" tab alongside "paintings"

---

## Data Flow

```
Native Record             Web Record
     |                         |
  float32[]               float32[]
     |                         |
  encodeSampleToBitmap    encodeSampleToBitmap
     |                         |
  PNG bytes               PNG bytes (via Canvas)
     |                         |
  POST /api/track-media   POST /api/track-media
  { mediaType:"sample" }  { mediaType:"sample" }
     |                         |
     +----→  DO Spaces  ←------+
             + MongoDB "samples"
             + short code (#abc)
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

## Storage Math

| Duration | Samples @48kHz | Pixels (3/px) | Image (256w) | PNG size |
|----------|---------------|---------------|--------------|----------|
| 1 sec    | 48,000        | 16,000        | 256×63       | ~20 KB   |
| 5 sec    | 240,000       | 80,000        | 256×313      | ~100 KB  |
| 10 sec   | 480,000       | 160,000       | 256×625      | ~200 KB  |

## Key Files

| Existing | Purpose |
|----------|---------|
| `system/public/aesthetic.computer/lib/pixel-sample.mjs` | Encode/decode samples↔bitmaps |
| `system/public/aesthetic.computer/disks/stample.mjs` | Web sample-painting player |
| `system/netlify/functions/track-media.mjs` | Media upload API |
| `system/netlify/functions/painting-code.mjs` | Short code → slug resolver |
| `system/backend/generate-short-code.mjs` | Unique code generation |
| `system/netlify/functions/presigned-url.js` | CDN upload/download URLs |

| New/Modified | Purpose |
|-------------|---------|
| `fedac/native/pieces/lib/pixel-sample-native.mjs` | Pure-JS encode/decode for QuickJS |
| `fedac/native/pieces/samples.mjs` | Native sample browser + upload/download |
| `system/public/aesthetic.computer/disks/samples.mjs` | Web sample browser piece |
| `system/netlify/functions/track-media.mjs` | Add sample branch |
| `system/backend/generate-short-code.mjs` | Cross-collection check for # codes |
