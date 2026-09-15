# Oskiewar raster benchmark

Replay the same recorded triangle stream through Canvas2D and WebGL 2.
This is an isolated rasterization experiment, not a replacement game shell.

```sh
node xbox/live/bench/render.mjs --headed --out /tmp/oskiewar-render-bench
node xbox/live/bench/render.mjs --headed --debug --out /tmp/oskiewar-render-bench-debug
```

Use `--frames 30..600` to set measured callbacks per case (default 120).
`--chrome /path/to/browser` selects a browser; otherwise the runner searches
common Chrome locations and then uses Playwright's installed Chromium.
Without `--headed`, it launches headless Chrome. The runner closes its browser
and loopback server when finished. It neither contacts production nor deploys.

`--capture-only` writes the trace and its metadata without opening a browser.
Two captures with the same source and debug flag should produce the same hash.

## Comparison

The fixture warms up the current game, then captures 60 frames from three
seconds of fixed-step skateboard play. Both rasterizers consume those exact
frames in the same order, at 1920×1080 and 1280×720 backing resolution.
Each resolution runs the cases forward and backward to expose order effects.

- **canvas:** same-color triangle paths with the browser shell's winding rule.
- **webgl-stream:** pack the recorded projected vertices into one reusable
  interleaved buffer each callback; upload and draw through the existing
  `WebGLOskiewarScene3D` renderer. This bypasses the allocation-heavy per-triangle
  scene adapter, so its overhead is not mistaken for a GPU limitation.
- **webgl-prepacked:** copy an already packed trace frame, upload and draw.
  This is a lower bound on CPU packing cost, not retained world meshes: vertices
  still upload each frame, and a live game cannot precompute arbitrary frames.

Depth testing is disabled to preserve Canvas painter order. Enabling depth,
retaining world-space terrain, or moving projection onto the GPU are separate
experiments. The present experiment changes neither camera nor clipping.

## Evidence and limits

`report.json` includes source/trace hashes, triangle counts, omitted command
counts, CPU submission p50/p95/p99, callback intervals, empty callback baselines,
WebGL GPU elapsed queries when available, renderer identity, and image differences.
The output also contains `trace.json`, `trace-meta.json`, and matching-frame PNGs.

Boxes, host lines and text are counted but omitted. Triangle-based world geometry,
fighters, shadows, and debug capsules remain. Debug results therefore do not
measure the complete debug meter or HUD. Simulation and command-generation
CPU timings are reported separately from browser replay and come from Node.

CPU submission excludes later GPU/compositor work. GPU elapsed queries are
invalidated when the driver reports a disjoint timer. Canvas has no equivalent
GPU timer here; null means unavailable, not zero. The separate completion probes
use `getImageData` or `gl.finish`: they stall and have different overheads, so
they are diagnostic values, not comparable end-to-end presentation measurements.

Callback gaps over 25 ms are reported as counts, not labeled dropped display
frames: a 30 Hz scheduling policy produces those gaps even without drawing.
Screenshots are inspected at the common CSS display size; antialiasing may differ.
Image differences are evidence of similarity, not a proof of full visual parity.

A production decision still needs a complete shell trial preserving text,
translucency and UI/world ordering, followed by played-session measurements on
browser, Xbox and Apple targets. See [RESULTS.md](RESULTS.md) for the first run.
