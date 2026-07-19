# Video Scrub Rubric — synthtapes + the scrub instrument

The tape player (`disks/video.mjs` + tape machinery in `bios.mjs`) is an
instrument: rate is the controlled quantity, position follows. This rubric
defines what "working" means and how it is checked.

## Test surfaces

- **`video scrub auto`** — the in-piece autopilot: scripted gesture segments
  driven through the *same state the finger drives*, graded against
  wall-clock ground truth. All output is console-greppable by `🧪`.
  Variants compose: `video scrub break auto`, `video scrub 2 auto` (short
  tape → seam-crossing stress), etc.
- **Synthtapes** (`tape:play-synth` in bios) — deterministic tapes whose
  frames render the soundtrack's own waveform on a scrolling ruler, so
  scrub velocity, direction, and AV alignment are visible by eye.
  Styles: `bed` (melodic), `break` (16th breakbeat), `house`, `dub`,
  `sine` (legato sineline). All pitched voices are quantized to whole
  cycles per loop → the seam is phase-continuous by construction.
- **Headless driving** — wheel gestures via `dispatchEvent(new WheelEvent(...))`,
  taps via CDP clicks; `window.__speaker_telemetry.runningCount` asserts the
  audio source stayed alive.

## The rubric (autopilot segments × criteria)

| Segment | Drives | Pass criteria |
|---|---|---|
| roll | nothing (normal play) | position advances |
| fast forward | rate 3× held 90 ticks | effective rate within ±25% of 3× (motion-integral, wrap-proof) |
| release into inertia | release at speed | inertia decays and converges |
| reverse | rate −2× held | effective −2× ±25%, seam wrap keeps direction |
| slow crawl | rate 0.5× | effective 0.5× ±25% (slow-rate accuracy) |
| tap dip | single-tap dip | rate sags below 0.3×, returns ≤1.05×, negligible net motion |
| scratch | ±3.2× @ 2Hz rocking | both directions exceeded, net drift < 0.7 tape-seconds |
| fast scratch | ±2.2× @ 4Hz | same, amplitude-scaled |
| touch brake | hold from 1× | rate sags to < 0.05× |
| brake release spin-up | lift after brake | ramps back to pre-brake rate |

Every scrub-driven segment also records: physics tick count, integrated
wall time (must ≈ segment wall time), speed min/max, and the tape duration
used — so a failure names its layer.

## Invariants (the things that regress)

1. **Wall-clock honesty** — commanded rate means tape-seconds per
   wall-second at any sim tick rate (measured `simDt`, burst-tolerant).
2. **One audio source** — tape audio is a single `loop: true` sample; the
   worklet wraps the read in both directions (stample ring). Nothing
   kills/restarts it during interaction; `runningCount` stays 1 through
   seam crossings both ways.
3. **Absolute rate** — `tape:audio-rate` sets the worklet speed absolutely;
   no relative-shift accumulation, no estimate drift.
4. **No handoffs** — every landing (park, wheel, dip, brake release,
   friction) keeps the scrub drive running; driving at exactly 1.0× *is*
   playback, so there is never a restart jump near 1×.
5. **Loop parity** — video duration = audio duration exactly
   (`mediaRecorderDuration`, includes the last frame's display time), so
   video and audio wrap at the same point at any rate.
6. **Scrub owns frames** — while scrubbing, the bios RAF clock must not
   advance frames at 1× between seeks.
7. **Musical loop grid** — at rest, loop periods converge to ≈±2ms of the
   tape length (grid-anchored, logged as `🕰️`).

## Gesture map under test

tap = dip · hold = brake · grab+drag = displacement-is-rate scratch
(with an on-screen anchor→finger vector showing direction and energy) ·
flick = prize-wheel spin-down to pre-flick rate · two-finger scroll =
shuttle (±24×) · top-right readout drag = steady-rate dial (friction-free
hold) · ←/→ = one-beat jump (audio relocated via `tape:audio-pos`) ·
hold ↑ = quarter-beat chop repeat, hold ↓ = eighth-beat glitch chop ·
release = seamless park + bearing friction home to 1× ·
spacebar = full reset/pause. A keys legend renders bottom-right.

## Net-time unison

Loop boundaries anchor to UTC multiples of the tape length (bios), and the
at-rest drive continuously lerps phase toward the AC network clock
(`clock.time()`, synced via `/api/clock`) with a ±5% tempo lean — so every
player of the same tape converges into global unison without jumps. The
top-right `sync ±Nms` readout is green when locked (<60ms).

## Open items

- Independent pitch-shift while playing (granular in the speaker worklet —
  `targetDuration`/grain machinery exists in `lib/sound/synth.mjs`, needs a
  live pitch param + a gesture axis).
- Seam-scratch segment (scratching across the loop point) in the autopilot.
- UTC/`net.time` phase alignment of the loop grid across devices.
