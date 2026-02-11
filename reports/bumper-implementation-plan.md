# Bumper System Implementation Plan

**Date:** 2026-02-11
**Status:** Fixes Applied — Testing Required
**Issue:** Bumper overlay overlapping piece content, incorrect screen dimensions

---

## Executive Summary

The bumper system allows pieces to render a horizontal bar overlay at the top of the screen (e.g., for tickers, notifications, banners). Several architectural issues were causing:

1. Piece content overlapping with bumper (bumper painted over piece pixels)
2. bios-side `screen.height` stuck at full canvas height (never matching worker)
3. bumperOffset inferred from overlay image (fragile — fails if overlay absent)
4. bumperOverlay pixels not included in worker transfer list
5. Recovery path incorrectly triggering on every bumper frame

This report documents the root causes and the fixes applied.

---

## Architecture: Corrected Data Flow

```
Physical Canvas (bios.mjs)
    450px × 438px
         ↓
  Frame Message (height: 438 — full canvas)
         ↓
Worker (disk.mjs)
    bumperOffset = bumperConfig.height (18)
    effectiveHeight = 438 - 18 = 420
    screen = { width: 450, height: 420, pixels: Uint8ClampedArray(756000) }
         ↓
  Piece API ($api.screen)
    width: 450, height: 420, bumperOffset: 18
         ↓
  Piece Renders (y=0 is top of piece viewport, below bumper)
    Draws to 450×420 buffer
         ↓
  sendData = { width: 450, height: 420, bumperOffset: 18,
               pixels: [420-row buffer], bumperOverlay: {...} }
         ↓
Main Thread (bios.mjs)
    bumperOffset = content.bumperOffset (18) ← explicit, not inferred
    imageData = new ImageData(pixels, 450, 420)
    ctx.putImageData(imageData, 0, 18)       ← piece below bumper
    paintOverlays["bumperOverlay"]()          ← bumper at y=0
    screen = { width: 450, height: 420 }     ← matches worker
         ↓
    Display: bumper y=0..18, piece y=18..438
```

---

## Root Causes & Fixes Applied

### Fix 1: Explicit `bumperOffset` in sendData (disk.mjs)

**Problem:** bios inferred bumperOffset from `content.bumperOverlay?.img?.height || 0`.
If the overlay was missing for any reason, offset was 0 → piece placed at y=0 → overlap.

**Fix:** Worker now sends `bumperOffset` as an explicit field in sendData:
```javascript
// disk.mjs line ~13487
const bumperOffset = bumperConfig.enabled ? bumperConfig.height : 0;
let sendData = { width: screen.width, height: screen.height, bumperOffset };
```

**Impact:** bios always knows the correct offset, even if the overlay fails to render.

### Fix 2: bumperOverlay pixel transfer (disk.mjs)

**Problem:** Every other overlay (label, qrOverlay, authorOverlay, etc.) had its pixels
copied and added to `transferredObjects`, but bumperOverlay was missing.

**Fix:** Added bumperOverlay to the transfer list:
```javascript
if (sendData.bumperOverlay) {
  const bumperPixelsCopy = new Uint8ClampedArray(sendData.bumperOverlay.img.pixels);
  transferredObjects.push(bumperPixelsCopy.buffer);
}
```

### Fix 3: bios `screen.height` set to imageData height (bios.mjs)

**Problem:** At line ~17553, bios stored screen dimensions as:
```javascript
assign(screen, { ..., width: ctx.canvas.width, height: ctx.canvas.height });
```
This meant `screen.height = 438` (full canvas), not 420 (reduced). Consequence:
- The zero-copy pixel path at line 16621 NEVER matched (content.height 420 ≠ screen.height 438)
- Every frame fell through to the slower paintChanged path

**Fix:**
```javascript
assign(screen, { ..., width: imageData.width, height: imageData.height });
```
Now `screen.height = 420` matches `content.height = 420` → zero-copy path works.

### Fix 4: bios uses explicit bumperOffset (bios.mjs)

**Problem:** All `bumperOffset` calculations used `content.bumperOverlay?.img?.height || 0`.

**Fix:** Changed to `content.bumperOffset || 0` — uses the explicit value from the worker.

### Fix 5: Recovery path dimension check (bios.mjs)

**Problem:** The recovery path at line ~17502 checked:
```javascript
imageData.height !== ctx.canvas.height  // 420 !== 438 → always true with bumper!
```
This incorrectly triggered recovery on EVERY frame when bumper was active.

**Fix:**
```javascript
(imageData.height !== ctx.canvas.height && imageData.height !== ctx.canvas.height - bumperOffset)
```

---

## What Was Already Correct (from previous work)

These were already implemented and verified:

- **Worker screen buffer reduction** (disk.mjs reframe handler + frame handler):
  `effectiveHeight = content.height - bumperOffset` ✓

- **Resolution function** (disk.mjs): Height reduced by bumperOffset ✓

- **$api.screen** (disk.mjs): Uses `screen.height` which is already reduced ✓

- **Pointer Y adjustment** (disk.mjs): `p.y -= bumperOffset` for all pointers ✓

- **Pen event Y adjustment** (disk.mjs): `data.y -= bumperOffset` ✓

- **HUD label positioning** (disk.mjs): `finalY += bumperOffset` ✓

- **paintChanged always true with bumper renderer** (disk.mjs): Ensures overlay repainted ✓

- **All putImageData calls use bumperOffset** (bios.mjs): `ctx.putImageData(imageData, 0, bumperOffset)` ✓

- **Module cache invalidation** (module-loader.mjs): DB_VERSION bumped + `?nocache` param ✓

---

## Input Coordinate Flow

Physical mouse/touch → bios Pen → worker pen events → piece act()

1. **bios Pen mapping** (line ~11955): Maps physical coords to screen coords
   - Uses `screen.height` (now correctly 420) for Y scaling
   - Physical y=0 → screen y=0, Physical y=438 → screen y=420

2. **Worker pen event adjustment** (line ~11822): `data.y -= bumperOffset`
   - screen y=0 → piece y=-18 (in bumper area)
   - screen y=18 → piece y=0 (top of piece viewport)
   - screen y=420 → piece y=402

3. **Worker pointer adjustment** (line ~11066): Same Y subtraction for pointers

Note: With Fix 3, `screen.height` on bios side is now 420. The Pen callback maps
physical coordinates to [0, 420] range. The worker then subtracts 18, giving the
piece a range of [-18, 402]. Clicks in the bumper area (physical y < 18) produce
negative piece Y coordinates.

---

## Testing Checklist

### Visual
- [ ] Bumper visible at top (y=0 to y=18)
- [ ] Piece content starts below bumper (y=18+)
- [ ] No overlap between bumper and piece
- [ ] No black bars at bottom
- [ ] HUD label visible and positioned correctly
- [ ] Notepat pads all visible (including bottom row)

### Dimensions
- [ ] Worker `screen.height` = 420
- [ ] Worker `sendData.height` = 420
- [ ] Worker `sendData.bumperOffset` = 18
- [ ] Bios `imageData.height` = 420
- [ ] Bios `screen.height` = 420 (not 438)
- [ ] Bios `bumperOffset` = 18

### Input
- [ ] Click at physical y=100 → piece receives y ≈ 82
- [ ] Click in bumper area → piece receives negative y
- [ ] Pointer dragging works correctly across bumper boundary

### Performance
- [ ] Zero-copy path at line 16621 is taken (not falling through every frame)
- [ ] No unnecessary recovery path triggering

---

## Files Changed

- `disk.mjs` — Worker: bumperOffset in sendData, buffer transfer, screen reduction
- `bios.mjs` — Main thread: explicit bumperOffset, screen.height fix, recovery path fix
- `module-loader.mjs` — Cache busting: DB_VERSION bump, `?nocache` param
- `notepat.mjs` — Piece: bumper enable with ticker renderer
