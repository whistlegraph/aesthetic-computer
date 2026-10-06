# oskiewar performance ledger

Measured on the machine that matters — @jeffrey's Xbox Series X running
oskiewar.com in Edge — through the remote experiment lane: an attached agent
flips render flags over the live relay (`npm run oskiewar:ablate <session>`)
and prices each layer in the fps the session itself reports. Workstation
profiles guide, the console decides.

## 2026-10-06 — native Xbox BIOS 62, v264 → v265

A matched desert close-up at 1920×1080, fixed appearance seed 7341 and neutral
input. Each debug setting warms for 14 seconds, then samples 14 seconds of
native 120-frame averages. [Raw samples and protocol](bench/native-2026-10-06.json).

| HUD | v264 median fps | v265 median fps | gain |
|---|---:|---:|---:|
| debug on | 29.08 | 41.83 | 43.9% |
| debug off | 41.59 | 55.55 | 33.5% |

The flat character and debug capsules now use the host's existing capsule and
disc primitives. Other flat ellipses reuse bounded unit meshes; their projection
runs in the native retained-mesh path. Flat figures no longer simulate invisible
cloth and ponytail springs. The body, collisions, camera and render resolution
are unchanged. Inset clipping and hosts without these primitives keep the JS path.

A separate played session reached 60.02–60.20 fps while riding. The original
reported slowdown measured 23.55–23.70 fps with about 40.3 ms in host/JS work;
those two observations have different views and debug state and are **not** the
before/after comparison. The close-up and full debug cases still fall below 60.
Native host/JS timing includes native primitive execution and host polling;
there is no GPU timestamp measurement here.

Reproduce each source revision separately:

```sh
node xbox/tools/benchmark-native.mjs --source /path/to/oskiewar.js --label baseline --out /tmp/oskiewar-benchmark.js
node xbox/tools/live.mjs deploy /tmp/oskiewar-benchmark.js
# Wait for BENCH_END (56 seconds), then retain the bounded diagnostic rows.
node xbox/tools/live.mjs logs 400
```

The prepared file holds controller input only during the two cases, then restores
live input. Restore the ordinary game after collecting the run. The benchmark
is temporary instrumentation and must not be published as a release.

## 2026-08-18 — rounds one and two (v79–v81, title screen, debug lit)

Round one, session `lippa60` (steady state, ~22 fps):

| experiment | median fps | Δ |
|---|---|---|
| baseline | 22 | — |
| sky off | 22 | +0 |
| grass off | 23 | +1 |
| shadows off | 22 | +0 |
| dust off | 23 | +1 |
| keys off | 22 | +0 |
| bands 1 | 22 | +0 |
| everything off | 25 | +3 |

Round two, session `lebbo875` (fresh page, noisy — the operator was playing):

| experiment | median fps | Δ |
|---|---|---|
| baseline | 49 | — |
| hud off | 60 | +11 |
| res .66 | 60 | +11 |
| res .5 | 45 | −4 |
| grass off | 26 | −23 |
| dust off | 25 | −24 |
| everything off | 60 | +11 |

The negative grass/dust rows are not grass and dust — they are the session
degrading under the walk (see below). `everything off` holding 59–62 at the
end of the same walk is the honest reading.

## Verdicts

1. **Decorative geometry is innocent.** Every flagged layer off together
   bought three frames (~600 primitives ≈ 9µs each). The frame's cost does
   not live in the small stuff.
2. **The frame is raster-area-bound.** `res .66` — shrinking the canvas
   backing store under an unchanged CSS size — held 60 by itself. Edge on
   the Xbox is paying for pixels, not shapes.
3. **The debug scaffolding costs eleven frames.** Hitbox skeletons, corner
   crops and glyph cells (`hud off` keeps the read-out and drops the rest).
4. **Sessions degrade with playtime, not wall time.** Fresh pages run
   49–60; minutes later the same page runs 22–25. A four-minute headless V8
   soak (tmp/oskiewar-soak-probe.mjs) stayed at 60 with a flat heap, so it
   is not a leak in the JS — consistent with fixed-step simulation catch-up
   debt on struggling frames, which a reload forgives.
5. Earlier, workstation-side (v77): a quarter of paint time was clipping
   arrays and their garbage — fast paths for the fully-visible common case
   took the console from ~23 to ~36 median on their own.

## What shipped because of this

- **v77**: clip/allocation fast paths (same pixels, ~20% off paint on V8,
  +13 median fps on the console).
- **v79**: the checkered wall panels became plain sheets (~150 faces).
- **v80–81**: the experiment lane itself — flags `sky grass shadows dust
  keys bands hud res`, relay-forwarded, priced by `oskiewar:ablate`.
- **v82**: the sliding default — the shell governs backing-store resolution
  by the game's own measured rate (down a step under 52 fps, floor .62, up
  a step at 58), which buys the frames and pays the sim debt off. The
  experiment `res` flag pins the dial; `res 0` hands it back.

## Open

- Verify the governor holds a played session at 60 (needs a live session
  after v82; watch the fps row recover on its own).
- Confirm the catch-up-debt account of the dip with sim/paint split
  telemetry on the wire — the Xbox host reports no frameMs today.
- The debug scaffolding's eleven frames: worth a cheaper skeleton (boxes
  instead of capsule overlays) so debug mode stops taxing what it measures.
