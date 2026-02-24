# Stample ↔ Pixel Store ↔ Share Pipeline Unification Plan

## Problem

The pixel↔audio conversion pipeline is fragmented across three pieces with duplicated code:

1. **`stample.mjs`** — has the canonical `encodeSampleToBitmap()`, `decodeBitmapToSample()`, `imageToBuffer()`, and `loadPaintingCode()` / `loadSystemPainting()` functions, but they're all **local/unexported**
2. **`clock.mjs`** — copy-pasted `decodeBitmapToSample()` and `imageToBuffer()` (comment: "adapted from stample.mjs")
3. **`notepat.mjs`** — can play stample samples but has **no way to load paintings as samples** (no `stample p` param handler, no painting→sample pipeline, no decode functions)

### What Should Work But Doesn't

- `notepat:stample` sets wave type to stample, but only plays whatever is in `store["stample:sample"]`
- `notepat:stample p` or `notepat:stample $roz` — no param handler exists to load a painting or KidLisp as a sample
- The `picture` buffer in notepat accumulates note colors but is never connected to audio
- KidLisp visualization in notepat renders to the screen but never feeds into the sample buffer

---

## Architecture

### Current Flow (Fragmented)

```
stample.mjs                          notepat.mjs                    clock.mjs
┌─────────────────────┐              ┌──────────────────┐           ┌──────────────────┐
│ encodeSampleToBitmap│              │ store.retrieve   │           │ decodeBitmapTo   │
│ decodeBitmapToSample│              │ ("stample:sample")│          │ Sample (COPY)    │
│ imageToBuffer       │              │ → registerSample │           │ imageToBuffer    │
│ loadPaintingCode    │              │                  │           │ (COPY)           │
│ loadSystemPainting  │              │ No painting→audio│           └──────────────────┘
│                     │              │ No KidLisp→audio │
│ $code → pixels →    │              │ No #code loading │
│   decode → register │              └──────────────────┘
└─────────────────────┘
```

### Target Flow (Unified)

```
lib/pixel-sample.mjs (NEW shared module)
┌─────────────────────────────────────────────────────┐
│ encodeSampleToBitmap(data, width)                    │
│ decodeBitmapToSample(bitmap, meta)                   │
│ imageToBuffer(image)                                 │
│ loadPaintingAsAudio(source, {sound, preload, store}) │
│   → handles: #code, $kidlisp, "p"/painting, URL     │
│   → returns {sampleData, sampleId, bitmap, meta}     │
└─────────────────────────────────────────────────────┘
         ↓ imported by
   stample.mjs    notepat.mjs    clock.mjs
```

---

## Step-by-Step Plan

### Step 1: Extract shared module `lib/pixel-sample.mjs`

Create `/system/public/aesthetic.computer/lib/pixel-sample.mjs` with:

```js
// 3 audio samples per pixel (R, G, B channels)
export function encodeSampleToBitmap(data, width = 256) { ... }
export function decodeBitmapToSample(bitmap, meta) { ... }
export async function imageToBuffer(image) { ... }
```

These are verbatim copies from `stample.mjs` lines 1300-1537, now exported.

### Step 2: Extract `loadPaintingAsAudio()` into the shared module

Generalize `loadPaintingCode()` and `loadSystemPainting()` into a single function:

```js
/**
 * Load any pixel source as a playable audio sample.
 * @param {string|object} source - "#code", "$kidlisp", "p"/"painting", or a bitmap object
 * @param {object} opts - { sound, preload, store, system, get, painting, kidlisp }
 * @returns {{ sampleData, sampleId, bitmap, meta } | null}
 */
export async function loadPaintingAsAudio(source, opts) {
  const sampleId = "stample:bitmap";

  if (typeof source === "string") {
    if (source.startsWith("$")) {
      // KidLisp: render to pixel buffer, decode to audio
      // Use opts.painting() + opts.kidlisp() to render
    } else if (source.startsWith("#") || source.startsWith("%23")) {
      // Painting code: fetch from /media/paintings/ or /api/painting-code
      // Reuse loadPaintingCode logic
    } else if (source === "p" || source === "painting") {
      // System painting: from nopaint buffer or store
      // Reuse loadSystemPainting logic
    }
  } else if (source?.pixels) {
    // Direct bitmap object
  }

  if (!bitmap?.pixels?.length) return null;

  const totalPixels = bitmap.width * bitmap.height;
  const meta = { sampleLength: totalPixels * 3, sampleRate: opts.sound?.sampleRate || 48000 };
  const sampleData = decodeBitmapToSample(bitmap, meta);

  if (sampleData?.length) {
    opts.sound?.registerSample?.(sampleId, sampleData, meta.sampleRate);
  }

  return { sampleData, sampleId, bitmap, meta };
}
```

### Step 3: Update `stample.mjs` to import from shared module

Replace local functions with imports:

```js
import {
  encodeSampleToBitmap,
  decodeBitmapToSample,
  imageToBuffer,
  loadPaintingAsAudio,
} from "../lib/pixel-sample.mjs";
```

Remove the local copies (~200 lines). Update `loadPaintingCode` and `loadSystemPainting` callers to use `loadPaintingAsAudio()`. The KidLisp sim loop still calls `decodeBitmapToSample` directly for live frame-by-frame updates.

### Step 4: Update `clock.mjs` to import from shared module

Replace the copy-pasted functions:

```js
import { decodeBitmapToSample, imageToBuffer } from "../lib/pixel-sample.mjs";
```

Remove local copies (~70 lines).

### Step 5: Add painting→sample pipeline to `notepat.mjs`

**5a. Import the shared module:**

```js
import {
  decodeBitmapToSample,
  imageToBuffer,
  loadPaintingAsAudio,
} from "../lib/pixel-sample.mjs";
```

**5b. Handle `stample` params in boot:**

After the existing `params[0] === "piano"` / `params[0] === "twinkle"` checks (~line 1252), add:

```js
// Handle stample with painting source: notepat:stample:p, notepat:stample:#code, notepat:stample:$kidlisp
if (wave === "stample" || requestedWave === "stample" || requestedWave === "sample") {
  const stampleSource = colon[1]; // e.g. "p", "#abc123", "$roz"
  if (stampleSource) {
    const result = await loadPaintingAsAudio(stampleSource, {
      sound, preload, store, system, get, painting, kidlisp,
    });
    if (result) {
      stampleSampleId = result.sampleId;
      stampleSampleData = result.sampleData;
      stampleSampleRate = result.meta.sampleRate;
    }
  }
}
```

This makes these paths work:
- `notepat:stample:p` — load system painting as sample
- `notepat:stample:#abc123` — load painting by code as sample
- `notepat:stample:$roz` — load KidLisp as sample

**5c. Connect KidLisp visualization to sample buffer (live):**

When `kidlispBgEnabled` is true and wave is `stample`, feed the kidlisp pixel buffer into the sample system each frame (in `sim`), similar to stample.mjs lines 514-597:

```js
// In sim(), after the kidlisp background update:
if (kidlispBgEnabled && (wave === "stample" || wave === "sample") && kidlispBackground) {
  // Render kidlisp to a small buffer and decode as audio
  const bufferSize = 128;
  const lispPainting = painting(bufferSize, bufferSize, (paintApi) => {
    paintApi.kidlisp(0, 0, bufferSize, bufferSize, kidlispBackground);
  });
  if (lispPainting?.pixels?.length) {
    const totalPixels = bufferSize * bufferSize;
    const meta = { sampleLength: totalPixels * 3, sampleRate: sound?.sampleRate || 48000 };
    const decoded = decodeBitmapToSample(lispPainting, meta);
    if (decoded?.length) {
      sound.updateSample?.("stample:bitmap", decoded, meta.sampleRate);
      stampleSampleId = "stample:bitmap";
      stampleSampleData = decoded;
    }
  }
}
```

**5d. Connect `picture` buffer to sample (optional/future):**

The `picture` buffer accumulates note-color stamps. It could optionally feed the stample system, making the visual history playable:

```js
// When picture buffer is updated and wave is stample, re-encode to audio
if (wave === "stample" && picture?.pixels?.length) {
  const meta = { sampleLength: picture.width * picture.height * 3, sampleRate: 48000 };
  const decoded = decodeBitmapToSample(picture, meta);
  if (decoded?.length) {
    sound.updateSample?.("stample:bitmap", decoded, meta.sampleRate);
  }
}
```

This is a stretch goal — the picture buffer is low-res and would produce very short samples. Worth experimenting with but not essential for initial unification.

### Step 6: Store pipeline consistency

Ensure all three pieces use the same store keys:
- `"stample:sample"` — raw audio sample data (Float32Array + sampleRate)
- `"stample:bitmap"` — pixel buffer + meta (width, height, pixels, sampleLength, sampleRate)

Notepat currently only reads `"stample:sample"`. After unification, it should also read/write `"stample:bitmap"` when loading from painting sources.

---

## Files Modified

| File | Change |
|------|--------|
| `lib/pixel-sample.mjs` | **NEW** — shared encode/decode/load functions |
| `disks/stample.mjs` | Replace local functions with imports (~200 lines removed) |
| `disks/clock.mjs` | Replace copy-pasted functions with imports (~70 lines removed) |
| `disks/notepat.mjs` | Add import, painting param handling, optional live kidlisp→sample |

## Verification

1. **`stample p`** — loads system painting as sample, plays with pitch control
2. **`stample #abc123`** — loads painting by code, plays as before
3. **`stample $roz`** — KidLisp renders live to audio, plays as before
4. **`notepat:stample:p`** — loads painting into notepat's stample mode
5. **`notepat:stample:$roz`** — KidLisp feeds sample buffer in notepat
6. **`notepat:stample`** — fallback to stored sample (existing behavior preserved)
7. **`clock.mjs`** stample features — unchanged behavior, now using shared module
8. **Tab through wavetypes** — stample mode still cycles correctly
9. **Recording in stample** — mic → encode → store still works

## Risk Assessment

- **Low risk**: Extracting functions to shared module is mechanical (identical code)
- **Medium risk**: `loadPaintingAsAudio` generalization — need to ensure all edge cases from both `loadPaintingCode` and `loadSystemPainting` are covered
- **Low risk**: notepat param handling — additive change, doesn't modify existing paths
- **Higher risk**: Live KidLisp→sample in notepat sim loop — performance-sensitive, may need throttling (stample does it every frame at 128x128)
