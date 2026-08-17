# 10a · Timbre space in AC — audit and build

**Shelf:** timbre · **Sources:** `wessel-1979-timbre-space` · **Follows:** [10](10-timbre-space.md)

[10](10-timbre-space.md) states the shelf. This is the shelf checked against
the code that already exists — AC's synth, notepat, Menu Band, TrackDrum —
and then the shelf's `tools` actually built.

## The verdict

The fleet used Wessel's **synthesis half** heavily and his **geometry half**
not at all. Every one of these systems does low-dimensional control over
high-dimensional spectral data, which is the thing he built the space in order
to enable. None of them had the space. Timbre was addressed by catalogue
number everywhere: a flat list, a 0–127 digit buffer, a random seed.

`dissimilarity`, `multidimensional scaling`, `MDS`, `stream segregation` and
`p-center` appeared nowhere in `system/public/aesthetic.computer`,
`slab/menuband` or `slab/tracktramp`. "Timbre" appeared only in prose.

## Where the paper already lived, unknowingly

**`partial_tilt` is Wessel's shaping function.** `gm_synth.c:2279` —
*"partial_tilt picks the contact brightness; brighter strike = lower coeff."*
Against p. 51: *"One dimension of this space was used to manipulate the shape
of the spectral energy distribution … by appropriately scaling the line
segment amplitude envelopes according to a shaping function."* Same operation,
same axis, arrived at independently. A GMPIANO program row
(`gm_synth.c:283-303`) — `partials`, `B`, `partial_tilt`, `tilt_from`, `tau0`
— is a data-reduced tone object driving per-partial amplitude and decay
arrays. `gm_set_organic(double)` is one global scalar over all 128 programs.

**notepat's `composite` hard-codes onset asynchrony.** `notepat.mjs:6278-6337`:
five oscillators, per-oscillator attacks `0.0025 / 0.0025 / attack / 0.999 /
0.05`. The 0.999 triangle against the 0.0025 sines *is* Wessel's second
control axis — *"the extent of synchronicity among the various components"* —
frozen as a constant instead of exposed as a knob.

**TrackDrum's drum skin was the nearest thing to a real space.**
`MenuBandPercussion.swift:291-380`: strike point → `(radius, θ)` → ten Bessel
eigenmodes excited at `|J_m(root·r)·cos(mθ)|`, resting fingers damping each
mode at their own mode-shape value, five instruments crossfaded along the
radius. And it performs Wessel's §B equalization deliberately
(`:314-317`) — *"perimeter materials are progressively quieter so bright
rim/edge spectra do not win the mix merely through perceived loudness."* The
gap: its axes are **physical** (a membrane), not measured.

**Fluoddity fuses both axes into one self-driven scalar.**
`fluoddity_voice.c:35-37`: a raw layer hot at note birth, decaying, then
sustained proportional to swarm agitation — brightness tied to onset and to
ongoing motion, in one number.

## Where it was absent

| System | Timbre address | Geometry |
|---|---|---|
| notepat | flat `wavetypes[]`, Tab advances by 1 | none |
| notepat / Menu Band GM | 3-digit decimal, 0–127 | none |
| Menu Band picker | 8×16 grid, family-coloured | GM spec numbering |
| Fluoddity | `setSeed` / `mutate` / reseed | random walk in 80-D |

Two absences were structural rather than incidental. `createMutatedNote`
carried `waveType: inferredWaveType` on all three return paths — a mutation
operator for pitch and none for timbre. And Menu Band's `morph`
(`MenuBandController.swift:88-123`) is chord quality; `ToneTrials`' twelve
rungs are twelve scales. Both interactive-transformation surfaces went to
harmony.

## The asymmetry

This is the 1979 imbalance reproduced. Wessel opens by blaming instruments:
*"most acoustical instruments provide for very accurate control over pitch but
provide little in the way of compositionally specifiable manipulation of
timbre."* AC had geometrized pitch thoroughly — `note-colors.mjs` maps pitch
class to hue with octave as brightness, plus chord detection, pitch-bend,
polyrhythm circles. And this platter's **rhythm** shelves shipped:
`pop/lib/necklace.mjs` implements chronotonic distance, swap distance and
`morphPath` ([05](05-distance-similarity.md)) with tests. Shelf 10 got
nothing. The move — subjective structure → geometry → compositional lever —
had been adopted for *when* and not for *what*, in a fleet where the
constraint Wessel blamed no longer applies.

## What got built

The measurement is `bin/gm-timbre-probe.c` + `bin/timbre-analysis.c`, with a
numerically identical JS port at `toolchain/timbre/brightness.mjs`. Both run
Wessel's steps 1 and 4 and **skip 2 and 3**: no listeners, no MDS. They
compute the acoustic correlates he used to *interpret* his measured axes — a
Bark centroid over a Schroeder-spread excitation pattern for brightness, rise
time and cross-band onset spread for bite — and use those as coordinates. That
is a proxy for a timbre space, not one, and every generated file says so.

Equalization is real: one pitch, one amplitude contour, RMS-matched before
analysis, `gm_set_organic(0)` for reproducibility.

- **Axis validated before use.** `--selftest` orders sine 2.72 < square 4.29 <
  sawtooth 5.30 < noise 18.86 Bark. The JS port agrees to three decimals.
- **128 GM programs measured** → `bin/gm-timbre-space.json`, and the Menu Band
  picker re-laid on them (`GMTimbreLayout.swift`, generated): 16 brightness
  bands, brightest on top, bite increasing rightward, per Figure 1. Family
  colours stay keyed to program number, so the palette now scatters — that
  scatter is the finding. `defaults write … instrumentGridLayout catalogue`
  restores the old map.
- **notepat's Tab cycle** is now a measured brightness ramp
  (`wave-timbre.mjs`, generated): sine · composite · triangle · square ·
  whistle · sawtooth · harp. The hand-written order had square and sawtooth
  the wrong way round.
- **`streamSplitRisk`** in `melody-parser.mjs` predicts melodic fission from
  brightness gap × rate, with van Noorden's coherence boundary as the rate
  term. It reports, it does not forbid — the split is also a device.
- **`fluod_rule_lerp` / `_analogy` / `_distance`** make the 80-float genome
  addressable. `bin/fluoddity-timbre-path.c` renders and measures the path
  rather than assuming it: blends showed no cliff (worst step 2.2–3.7× mean),
  and the parallelogram is arithmetically exact (`|B−A| = |D−C|`, residual 0).
- **P-centers.** `pCenterShiftsMs` pulls each note earlier by its own
  perceptual attack lag; kidlisp's `_timeline` applies it. Mean-zero, so tempo
  never drifts, and a single-timbre line is left bit-identical.

## What the measurement caught

Wessel's step 5 is verification in musical situations. Running it found three
things that were not the point of the exercise:

1. **AC's web `square` was an octave flat.** `#up` toggled every full
   wavelength instead of every half: a square asked for 261.63 Hz measured
   130 Hz, while sine/triangle/sawtooth landed on pitch. Both sibling engines
   (`fedac/native/src/audio.c` WAVE_SQUARE, `MenuBandPercussion.swift`) were
   already correct, so the web synth was the lone outlier in a drum kit meant
   to sound identical across three engines. Fixed.
2. **gm_synth's brass and flute are miscalibrated.** Trumpet at C4 renders 90%
   of its power in the fundamental band — harmonics essentially absent. Flute's
   fundamental carries 1%, with ~44% above 5 kHz. The timbre layout maps them
   honestly, which is why they sit in the dark rows next to the woodblocks.
   Open.
3. **Melody strings could not reach `harp` or `whistle`.** Two duplicated
   hard-coded allowlists in `melody-parser.mjs` had drifted behind the synth;
   a `{harp}` was silently ignored and left the previous voice sounding. One
   list now, with the aliases the synth already accepts.

## Limits to carry

The space is **stimulus-set-relative** and it warps with pitch, measurably:
brightness rank correlation against the C4 measurement is ρ = 0.92 at C3 and
ρ = 0.82 at C5. These coordinates describe this synth at this pitch.

The axes are **interpretations, not measurements** — the correlates, without
the judgments that earned them. Wessel is explicitly pessimistic about
subjective units and this inherits that pessimism twice over.

The Fluoddity parallelogram is the weakest claim here. Wessel's version
predicted listener rankings because his coordinates came from dissimilarity
judgments; a genome is not a perceptual space, so its parallelogram is a lever
with a defensible shape and no evidence behind it. One data point exists: an
A→B move shifted brightness −4.53 Bark and the transposed C→D move shifted
−5.35 Bark. Suggestive, not a result.

## tools

- `timbre_brightness(x, sr)` / `timbre_rise_ms` / `timbre_async_ms` — C, in
  `bin/timbre-analysis.c`; JS in `toolchain/timbre/brightness.mjs`.
- `GMTimbreLayout.distance(a, b)` — the "what else sounds like this" the
  picker never had.
- `waveDistance(a, b)` — the same for AC's waves; `null` for modes with no
  single timbre, rather than a guess.
- `streamSplitRisk(notes, {baseTempo, threshold})` — the acceptance test.
- `pCenterShiftsMs(notes)` / `applyPCenterShifts(starts, notes)`.
- `fluod_rule_lerp / _analogy / _distance` — movement through a genome.
- Still missing, and the honest next step: **actual dissimilarity judgments**.
  Everything above is the acoustic half. A listening harness — even @jeffrey
  alone, as Wessel himself served as the sole judge for his 24 tones — would
  turn the proxy into a space.
