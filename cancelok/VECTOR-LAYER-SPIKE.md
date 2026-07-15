# Vector-layer spike — can the mask + buttons be a crisp Canvas 2D layer over the pixel pads?

Investigated 2026-07-14, at @jeffrey's ask ("make the mask and buttons a Canvas 2D
layer alongside the pixel buffers, for a more approachable interface").

## Finding: possible, but it's infrastructure, not a bolt-on.

AC has the building blocks but not the arrangement:

- **`lib/gpu/`** is a GPU-backend abstraction with backends: `canvas2d`, `webgl2`,
  `vello` (WASM present), `thorvg`, `blend2d` (WASM NOT built). A piece selects ONE
  via `api.gpu.backend = "…"` + `api.webgpu.enabled = true`.
- **It's either/or, per-piece.** All of a piece's draw commands route to the single
  chosen backend's canvas. There is no mode where the pixel-buffer pads render
  underneath AND a Canvas 2D layer draws the UI on top at the same time. "Write to
  both" is not something the pipeline does.
- **`canvas2d-backend.mjs` is minimal** — it implements `clear`, `line`, `box`,
  `circle` and nothing else. No `roundRect`, no `fillText`, no paths. So switching
  to it wouldn't even yield crisp vector text or real rounded rects without
  extending the backend first.
- **nom is NOT a precedent.** `lib/nom.mjs`'s "board" is a game-logic 5×5 grid
  rendered into the pixel buffer with the MatrixChunky8 bitmap font — no Canvas 2D
  layer, no dual-write.

## What a real vector UI layer would take

1. Extend `canvas2d-backend.mjs` with `roundRect`, `fillText` (real fonts), paths,
   gradients, shadows.
2. Build a genuine dual-layer compositor: keep the pixel pipeline for the pads,
   add a second overlay canvas for the UI, composite them.
3. Route input across both layers (hit-test the overlay first, fall through to the
   pad).

A multi-session effort. Worth it only if the vector polish becomes the priority.

## The pragmatic alternative (shipped)

The "approachable" look we associate with the muncher games is mostly the
**MatrixChunky8** bitmap font, not any canvas. cancelok's direction labels now
render in MatrixChunky8, light on their coloured tiles — a real crispness win with
zero new infrastructure. Further pixel-native polish (bigger targets, tile
shadows, smoother hover) buys more approachability per hour than a canvas rewrite.
