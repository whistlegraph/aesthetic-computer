# pop menu — wishlist

Requested capabilities for the `/pop` toolkit. As each item lands it gets
wired into a real `lib/menu.mjs` category and moves to **Landed** below.

**Source:** Abe Edelman, iMessage thread 2026-05-21 — sent after being
asked "any requests?" for the pop pipeline. As of 2026-05-21 his whole
list has shipped.

Status: 🟢 landed · 🟡 in progress · ⚪ proposed

---

## Landed

All from Abe's 2026-05-21 list.

### fx
- 🟢 **ring modulation** (`fx.ringmod`) — multiply the signal by a carrier:
  a synth sine/tri/square at any freq, or *any in-program audio buffer*
  as the carrier/modulator. CLI: `play.mjs --fx ringmod`.
  → `dance/synths/fx.mjs:applyRingMod`
- 🟢 **vocoder** (`fx.vocoder`) — channel vocoder: the buffer is the
  carrier, `modulator` is any other audio; the modulator's per-band
  amplitude spectrum is imprinted on the carrier so it "speaks". TPT
  state-variable bandpass per band.
  → `dance/synths/fx.mjs:applyVocoder`

### analysis — audio → control / triggers
- 🟢 **envelope follower** (`analysis.envelope-follower`) — amplitude
  contour → 0..1 control curve, with `invertControl()` for the
  "opposite amplitude" signal.
  → `dance/synths/fx.mjs:envelopeFollower` + `invertControl`
- 🟢 **pitch tracking** (`analysis.pitch-track`) — audio → per-frame f0 /
  fractional-MIDI curve + clarity, by normalized autocorrelation.
  → `lib/analysis.mjs:pitchTrack`
- 🟢 **audio gate / trigger** (`analysis.audio-gate`) — amplitude onset
  detector with hysteresis + retrigger lockout; beatbox a mic to fire
  drum samples.
  → `lib/analysis.mjs:audioGate`

### score — notation & sequencing
- 🟢 **audio → rhythm sequence** (`score.audio-to-rhythm`) — onset-detect
  a WAV loop, classify hits low/mid/high by zero-crossing rate, quantize
  to a beat grid, emit a drum `.np`. CLI: `bin/audio-to-rhythm.mjs`.
- 🟢 **note subdivision** (`score.note-subdiv`) — split `.np` notes into
  1/2 · 1/4 · 1/8 · 1/16 subdivisions (Ableton-style).
  → `lib/meter.mjs:subdivideNp` / `subdivideCell`

### forms — structure & theory
- 🟢 **Fibonacci meter** (`forms.fib-meter`) — Fibonacci division &
  addition for measures/bars: `fibPartition` golden-splits a bar,
  `fibMeter` builds a form, `fibSequence` is the raw series.
  → `lib/meter.mjs:fibMeter`
- 🟢 **species counterpoint** (`forms.species-counterpoint`) — Fux
  first-species: `checkFirstSpecies` flags rule violations,
  `generateFirstSpecies` backtracks a valid line above a cantus firmus.
  → `lib/counterpoint.mjs`

## Proposed

_(empty — new requests land here)_

---

## Reference links Abe sent

- <https://en.wikipedia.org/wiki/Fibonacci_sequence>
- <https://archive.org/details/imslp-ad-parnassum-fux-johann-joseph>
  (Fux *Gradus ad Parnassum* — that scan is Latin; an English one is around)
- <https://www.youtube.com/watch?v=nENw4q-zRZU>
- <https://www.youtube.com/watch?v=NFryeLwhER0>
- <https://www.youtube.com/watch?v=8y10psbF8gA>
- <https://www.youtube.com/watch?v=WsCYjmkh2dM> — **at 1:17:00** he says
  this gives an algorithm for drum programming worth lifting.
