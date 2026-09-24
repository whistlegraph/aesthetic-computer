# Notespatial rendering, note labels, echo and flange

Deployed silently to all six laptops after the successful Frisbee singer test. No native rebuild, reboot, USB persistence or measured speedup is implied. The parent wrapper controls battery display, brightness, DMX and master volume.

Upload `notespatial-performance-optimized.mjs` under `/pieces/` and upload `notespatial-echo-flange.nsscore` as `/pieces/notespatial-fx.nsscore`. Keep `/pieces/spatial-rehearsal.nsscore` unchanged. The module uses the same complete binary score-file reader as the verified Neo baseline.

The wrapper can import the candidate and call:

```js
rehearsal.setVisualMode(controls.noteLabels ? 'notes' : 'frames');
rehearsal.paint({ ...api, write: concert ? () => {} : api.write,
  overlayWrite: api.write });
const view = rehearsal.getPerformanceVisualState();
// {mode,note,localGain,frames,frameLimit,hatchLimit,
//  phase,scoreTime,scoreDuration,runId,seat}
```

`scoreTime` uses the most recent simulation audio clock; ready state returns null. Readback requires no filesystem reads. Note mode chooses the strongest locally routed active note, prints its actual name in large type, and releases over180ms. `overlayWrite` preserves the large label when concert mode suppresses ordinary labels. Parent battery/charging-bolt rendering should follow this paint call. The module never changes brightness or volume.

The renderer builds one per-seat event/color index at boot, rather than recalculating event-time spatial routing and sorting frame objects on every paint. Each paint visits a bounded time window, reuses its selected-frame array, draws at most24 note frames,48 hatch lines and one full fill. Note mode skips those frame graphics entirely. Current lane routing is cached per audio time, and surviving voices compact in place. This reduces identifiable work; actual frame-rate gains require a repeat on the machines. Saturated passages deliberately omit some distant visual frames.

The separate score adds two quiet synthesized echo taps on selected held, answer and echo-lane melodies in Lullaby and Return:240/480ms,18%/7% of the original event gain, at most400ms duration. This is synthesized repetition, not a sampled delay effect; the current native Notepat API does not expose a delay bus. The generator rejects taps that would exceed32 overlapping voices. The generated score adds790 taps and peaks at18 global voices. Original events, score duration and SUB bass/kick events remain unchanged.

Flange uses Notepat's existing `sound.wobble.setMix` path: ring seats during Chase, held center during Lullaby, with three-second ramps and a35% wet ceiling. It applies to the selected laptop's entire mix during that section, not an individual voice. Existing room/drive/glitch automation remains. The chain is reset dry on stop and leave. Masters stay under the wrapper's25% control; Windows SUB receives no echo or flange changes.

Reproduce from the repository root:

```sh
python3 fedac/native/candidates/notespatial-2026-09-24/make-effects.py
node --test fedac/native/candidates/notespatial-2026-09-24/candidate.test.mjs
```

Three focused tests pass: original-score synthesis parameters match the baseline across all six seats and selected movement times; graphics respect the work limits; large-note mode bypasses concert text suppression and releases correctly. Score checks also verify all original events, unchanged SUB events, flange bounds and the global voice ceiling. Tests use the bundled verified baseline fixtures; they do not contact devices or invoke a native build.

For a clean performance comparison, use the same score and same brightness/DMX/polling settings on both renderers. Adding echo, note mode and brightness changes simultaneously makes a repeat a new configuration measurement, not an isolated renderer speedup.

Blocking native DMX writes remain a separate profiling candidate. The parent wrapper should cache its immutable seat, consume this module's live readback, and coalesce identical DMX states while retaining a bounded keepalive and final blackout. This module contains no DMX writes and cannot remove native serial blocking.

Baseline provenance: Neo performance snapshot `70ce60d39f3dc1b67e62038ec3cf70300cb2e2593936ec287f1747ee337b8919`; original score `d3ae52f7f427aa753734977c7079ec81c66167100107d88e2960e35cf5066d18`. Baseline fixtures include the original source, compressed score, and SUB-event conversion used for equivalence checks.

## Integrated wrapper

`notespatial-controls.mjs` combines the candidate with the battery lightning
bolt, audio-hit hardware brightness, optional large-note display and warm
held-center DMX. It caches control reads and uses the performance state export
instead of rereading status/config every frame. Duplicate center RGB writes
are skipped, with a one-second retry delay after a failed send.

Its deployed sibling imports map to canonical sources as follows:

- `battery-watch-power-v2.mjs` ← `fedac/native/lib/battery-watch.mjs`
- `score-brightness-audio-v2.mjs` ← `fedac/native/lib/score-brightness.mjs`
- `candlelight.mjs` ← `fedac/native/lib/candlelight.mjs`
- `notespatial-performance-optimized.mjs` ← this candidate's performance module
- `notespatial-fx.nsscore` ← this candidate's `notespatial-echo-flange.nsscore`

After Frisbee completed and released its singer test, the wrapper and
dependencies were uploaded with byte-for-byte readback and loaded silently on
all six. All reported ready, no error, note mode selected and brightness0.
The matching score is loaded in the calibrated SUB receiver. No new full-run
FPS improvement or acoustic acceptance is claimed from this silent validation.
These changes are live in RAM; they have not been added to USB boot images.
