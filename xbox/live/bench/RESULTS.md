# Raster experiment — 2026-09-14

The GPU path substantially reduced CPU submission for the captured triangle
workload. It did not raise callback frequency: the empty baseline, Canvas2D,
and WebGL all ran at about 30 Hz in visible Chrome on this host.

| Backing resolution | Canvas CPU p50, two orders | Streamed WebGL CPU p50 | WebGL GPU p50 |
|---|---:|---:|---:|
| 1920×1080 | 2.0–2.6 ms | 0.2 ms | 1.85–1.86 ms |
| 1280×720 | 2.4–2.6 ms | 0.2 ms | 0.91–0.92 ms |

Prepacking reduced CPU p50 to about 0.1 ms, with similar GPU durations.
Canvas CPU time did not improve reliably at lower resolution; WebGL GPU elapsed
time roughly halved. These are short runs, with order variation visible in the
Canvas results. CPU and GPU durations must not be added as if they were serial.

Visible Chrome 153 used ANGLE's Metal backend on Apple A18 Pro, with antialiasing
and GPU timer queries available. Each case measured 120 callbacks after 30 warmup
callbacks, in both orders. The 60-frame trace contained 1,450–1,649 triangles per
frame. It omitted 2,840 box calls, 468 host line calls and 1,320 text calls in total.
No page errors were observed. The empty baseline had a median callback interval
of 33.3 ms before and after the runs; the cause of that scheduling limit is not
established by this experiment.

At the common 1280×720 screenshot size, 0.0208% of pixels differed by more than
16 levels in any RGB channel. Mean absolute channel difference was 0.0235/255.
Visual inspection found consistent geometry and composition with edge-smoothing
differences. This does not establish parity for the omitted commands.

Source SHA-256: `cae7d3aae45d3bf9864dbd596ec36c970ddaffde04acae065496d458bb77e129`

Trace SHA-256: `bd3c09de9399b6a37cf7faa8c4ea7b5bb75545732e45c117c56b2c1263176e2c`

Two later independent captures reproduced the same trace hash.
Archived [visible-run results](results/2026-09-14-visible.json) and
[debug-run results](results/2026-09-14-debug.json). Screenshots and full traces
remain in `/tmp/oskiewar-render-bench-visible/` and `/tmp/oskiewar-render-bench-debug/`.
Run the command in [README.md](README.md) to regenerate evidence from current source.

## Debug triangle run

A separate 60-callback-per-case run captured 2,000–2,199 triangles per frame.
At 1080p, Canvas CPU p50 was 0.6–1.0 ms across the two orders and streamed
WebGL was about 0.2 ms. WebGL GPU p50 was 1.78–1.81 ms. Pixel differences above
16 levels covered 0.0360% of the comparison screenshot.

These figures must be compared within this run: its lower Canvas CPU time than
the earlier non-debug run demonstrates host/order variability, not a claim that
debug makes the game faster. Text, meter boxes and host lines remain excluded.

## Validation

The packing tests compare the fast buffer path directly with OskiewarScene3D,
including clipping-depth extremes and successive frames. Both pass. Browser
runs completed without page errors; the debug run also checked WebGL errors and
context loss. Scene adapter tests pass. Three renderer camera/floor tests fail
identically against repository HEAD; the benchmark does not change those paths.

## Next decision

Prototype the GPU path behind an explicit renderer option, preserving command
ordering and the existing Canvas fallback. Include host text, boxes, lines and
translucency before comparing full-game frame time. The streamed path already
captures most of the measured CPU benefit; retained world meshes are a later
experiment, not a prerequisite for this trial.

Investigate the independent 30 Hz callback ceiling separately. A cheaper renderer
provides more work budget, but this run gives no evidence that it lifts that limit.
