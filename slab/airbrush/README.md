# Airbrush

Native macOS painting prototype: AppKit trackpad contacts, keyboard navigation,
and Metal-rendered analytic brush deposits. Each stroke retains its world-space
path, brush settings, and seed. Zooming never resamples the saved artwork.

```sh
cd slab/airbrush
bash build.sh
open build/Airbrush.app
```

The `computer.aesthetic.airbrush` bundle participates in Slab's normal pane
grid, population observer, and Command–Option–arrow navigation. Rebuild/install
Slab after changing its roster. No global keyboard capture or Accessibility
permission is needed by Airbrush itself.
Starts as a fine dark marker: 2-pixel radius, hard edge, full flow, no scatter.
The single bottom row has radius, edge, and smoothing; flow/scatter live in ⋯; the window title is intentionally blank.

| Control | Action |
| --- | --- |
| Tab | Switch Mouse / Trackpad; the toolbar shows the current mode |
| Mouse mode: drag or Space | Paint at the system pointer; raw pad contacts are ignored |
| Trackpad mode: touch | Every active finger paints its own path; lift to stop. The OS pointer is parked and hidden |
| WASD | Move through the painting, including during a stroke |
| Hold Q / E or ⌘− / ⌘+ (also ⌘=) | Smooth continuous zoom out / in around the brush |
| Shift | Slow pan and zoom |
| Mouse: hold R / H / F / T + horizontal motion | Step radius / edge / flow / scatter |
| [ / ] | Smaller / larger radius |
| N or ⋯ menu | Change the scatter seed |
| Z / Y; ⌘Z / ⇧⌘Z | Undo / redo |
| 0 | Reset the view |
| Escape | Release held input and return to Mouse mode |
| ⌘S / ⌘O | Save / open `.airbrush` strokes |
| ⇧⌘E | Export the visible view at display resolution |

The edge has three levels: soft, medium, and hard. Radius doubles from 1 to
512 screen pixels. Flow steps through 10/25/50/100%; scatter through 0/25/50/100%.
Stored strokes retain their original continuous parameter values.
Scatter mixes the continuous mark with discrete droplets. Marker flow controls
ink opacity; a stationary marker does not keep depositing. Legacy airbrush
strokes retain their time-based exposure. Radius remains
constant on screen, so zooming while spraying records changing world radii.
Each trackpad contact has its own sampler. Simultaneous contacts emit into one
chronological gesture so replay preserves overlapping paint order; undo removes
the whole gesture. Lifting all fingers or losing focus ends it. Focus loss releases
pointer capture. Tab returns the system pointer to the visible brush location.

Smoothing defaults to **String 12**: a pulled-string stabilizer with a 12-screen-point
leash and visible tether. String 6/12/24 hold the brush until the leash is taut,
then stop at the brush tip on lift. **Smooth 1/2/3** use a speed-adaptive 1€ filter:
more filtering at slow speeds, less lag at high speeds, and bounded endpoint
catch-up on lift. **Off** follows raw input. The selection persists across launches.
Every finger owns an independent filter. Filtering happens in screen coordinates
before camera mapping and dense stroke sampling, so zoom does not change its feel.
Only the resulting geometry is saved; replay never smooths twice. Work is constant
per input update; adaptive endpoint completion takes at most 49 filter samples.
These are measured implementations of established methods, not a claim of parity
with every feature of a commercial stabilizer. References:
[Lazy Nezumi's pulled-string interaction](https://lazynezumi.com/smoothing) and
[Casiez, Roussel & Vogel's 1€ filter](https://gery.casiez.net/1euro/).

Version 1 records airbrush deposition knots at 120 Hz. Version 2 adds a marker
flag per sample: full flow lays opaque ink, motion fills samples at half-radius
spacing, and a stationary contact does not keep darkening. Each input update is
capped at 4,096 marker samples. Legacy strokes retain their original exposure. SplitMix64 addresses random
values by stroke seed and knot index; each knot has its own droplet stream.
The same recorded path and seed regenerate the same droplet positions,
irrespective of redraw count, camera, culling, or replay order. Changing hardness
does not reroll the positions. Seed alone does not reproduce a different hand
gesture. GPU pixel identity is tested on the same device; cross-GPU bitwise
identity is not promised. Changes to sampling, PRNG, or brush semantics require
a new document version and a retained version-1 replay path.

Autosave lives in `~/Library/Application Support/Aesthetic Airbrush/`.
`--demo` opens a disposable demonstration without writing the autosave.

```sh
swift test --jobs 2
build/Airbrush.app/Contents/MacOS/Airbrush --verify-input
build/Airbrush.app/Contents/MacOS/Airbrush --verify-smoothing build/smoothing.png
build/Airbrush.app/Contents/MacOS/Airbrush --verify-render /tmp/airbrush-render.png
build/Airbrush.app/Contents/MacOS/Airbrush --verify-cache build/cache-check
```

Tests cover PRNG known answers, random-access scattering, seed changes,
save/open replay, event batching, stationary deposition, hardness, scatter
bounds, zoom anchoring, and document validation. Render verification compares
actual Metal pixels before and after document serialization and rejects blank
output. Input verification exercises contact lifecycles and chronological
multitouch save/replay/undo; physical touch delivery still needs a hardware check.
Inspect its PNG and the live pane before judging brush feel.

The painting is the stroke history; the viewport is a disposable raster cache.
256-pixel world tiles form a sparse pyramid of zoom levels, with two-pixel gutters
for seam-free filtering. A chronological bounding-volume tree locates deposits
without changing their compositing order. New paint invalidates only intersecting
resident tiles and appends their missing history. Undo drops the affected history
suffix and invalidates cached views. Cursor movement updates its own overlay.

Warm frames draw viewport tiles and do no history traversal or deposit rendering.
Cold frames are capped at 8,192 tree/deposit tests, 2,048 deposits, approximately
4 million deposit bounding-box pixels, and 12 tile passes. At most two frames are
in flight. A last-complete-view reprojection and cached coarser tiles cover zoom
transitions while missing detail refines. Partial tiles appear as they render.
Paint added during a cold replay gets a transparent suffix with priority over
old detail; the prefix is frozen, then the suffix is replayed onto an exact copy
before swapping it in. This preserves final BGRA8 compositing order. The live
preview and historical merge share the same frame budgets.
The LRU retains at most 192 tiles (about 50 MiB normally, up to 150 MiB while
all tiles have live suffixes and merge textures), plus one viewport image. Huge viewports select a coarser level
to keep the number of visible tiles bounded.

These bounds control frame work, not exact wall-clock time: dense uncached regions
take more refinement frames, and loading/saving the source document still scales
with its size. Source history grows with the painting. The cache verification
checks unfinished tile visibility and fresh ink during cold replay, then
compares direct and tiled pixels, exact live merges, incremental compositing, undo, zoom levels,
eviction/reconstruction, every cold-frame budget, and warm-frame work at two
densities. Its JSON includes measured CPU/GPU timings and the tested device.

This is an analytic dab prototype, not Mischief's ADF implementation. It has
one paint layer, finite double-precision coordinates, and zoom from 10⁻¹² to 10¹².
Curve compression and tiled 32K export are still absent. The saved strokes remain
independent of display cache resolution and export resolution.
