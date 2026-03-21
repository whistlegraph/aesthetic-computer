# Brush Artery Tests — Report

**Date:** 2026-02-21
**Status:** Implementation starting with `line` brush

## Goal

Create artery tests for nopaint "brushes" (line, box, rect, shape, fill, oval, etc.) that exercise the full brush lifecycle through the harnessed CDP testing system (`artery-tui`).

## Current State

- **No brush artery tests exist yet.** The `artery/` directory has 24 test files, all music/notepat-focused or prompt-interaction tests.
- `artery-ink.mjs` already has a placeholder entry for `test-line.mjs` in `PATTERN_CONFIGS` (line 146) but the file doesn't exist.
- The `robo.mjs` disk (52KB) exists for client-side robot brush testing but is separate from the artery harness approach.

## Architecture: How Brush Tests Work

### The Flow (for `line` brush test)

```
1. Connect via CDP (artery-auto.mjs)
2. Jump to `prompt` (starting point)
3. Jump to `new~128` → creates fresh 128×128 painting
4. Wait for nopaint system to initialize
5. Jump to `line` → enters line brush mode
6. Wait for piece to load
7. Simulate pointer events: pointerdown → pointermove → pointerup
   (This draws a line from point A to point B)
8. Jump back to `prompt` → completes the stroke (lift/bake)
9. Verify painting state has changed
10. Cleanup & exit
```

### Key Technical Details

**Navigation:** `client.jump('piece')` sets `window.location.href` to `https://localhost:8888/piece`

**Pointer simulation:** Must dispatch `PointerEvent` on the `<canvas>` element (not CDP `Input.dispatchMouseEvent`) because AC's `Pen` class listens for DOM `pointerdown/pointermove/pointerup` events:

```javascript
await client.eval(`(() => {
  const canvas = document.querySelector('canvas');
  const rect = canvas.getBoundingClientRect();
  canvas.dispatchEvent(new PointerEvent('pointerdown', {
    bubbles: true,
    clientX: rect.left + x,
    clientY: rect.top + y,
    pointerId: 1,
    isPrimary: true,
    pressure: 1.0
  }));
})()`);
```

**Nopaint brush lifecycle:**
- `overlay()` — called during draw (preview)
- `lift()` — called on pointerup (commits stroke to painting)
- Returning to `prompt` from a brush is how a stroke "session" ends

**The `new` command:**
- `new~128` → 128×128 square painting
- `new~256~128` → 256×128 rectangular painting
- Clears storage, resets undo stack, resets transform, creates fresh canvas

## Test File: `artery/test-line.mjs`

### What It Tests

1. **Painting creation** — `new~128` produces a valid painting
2. **Line brush loading** — navigating to `line` activates nopaint with the line brush
3. **Stroke simulation** — pointerdown + pointermove + pointerup draws a line
4. **Stroke completion** — returning to prompt bakes the stroke
5. **State verification** — painting pixel data changes after a stroke

### Template Pattern

Following `test-notepat-quick.mjs` as the simplest template:
- Uses `getArtery()` from `artery-auto.mjs` for CDP/Electron compatibility
- Uses `client.eval()` for DOM PointerEvent dispatch (not CDP Input events)
- Uses `client.jump()` for navigation between pieces

### Future Expansion

The same pattern extends to all brushes:
- `test-brush-box.mjs` — rectangle drawing
- `test-brush-shape.mjs` — freeform shapes
- `test-brush-fill.mjs` — flood fill
- `test-brush-oval.mjs` — ellipses
- `test-brush-marker.mjs` — freehand painting

Each brush just needs different stroke simulation and verification logic.

## Files Involved

| File | Role |
|------|------|
| `artery/test-line.mjs` | **NEW** — Line brush artery test |
| `artery/artery-auto.mjs` | Adapter selector (CDP/Electron) |
| `artery/artery.mjs` | Core CDP client with eval/jump/click |
| `artery/artery-ink.mjs` | Pattern config (already has `test-line.mjs` entry) |
| `system/public/aesthetic.computer/disks/line.mjs` | Line brush piece |
| `system/public/aesthetic.computer/systems/nopaint.mjs` | Nopaint system |
| `system/public/aesthetic.computer/disks/prompt.mjs` | Prompt command handler |
