# pop menu — wishlist

Requested capabilities for the `/pop` toolkit that aren't built yet.
Mirrors the `proposed` block at the bottom of `lib/menu.mjs` — that block
is the machine-readable copy; this file is the human one, with reference
links and context.

As each item lands it gets wired into a real `MENU` category and moves to
the **Landed** section here.

**Source:** Abe Edelman, iMessage thread 2026-05-21. He sent these after
being asked "any requests?" for the pop pipeline.

Status: 🟢 landed · 🟡 in progress · ⚪ proposed

---

## Landed

- 🟢 **ring modulation** (`fx.ringmod`) — multiply the signal by a carrier:
  a synth sine/tri/square at any freq, or *any in-program audio buffer*
  passed as the carrier/modulator. Sub-audio = tremolo, audio-rate =
  clangorous sidebands. CLI-pickable: `play.mjs --fx ringmod`.
  → `dance/synths/fx.mjs:applyRingMod`
- 🟢 **envelope follower** (`analysis.envelope-follower`) — track a signal's
  amplitude contour into a 0..1 control curve (attack/release smoothing).
  Ships with `invertControl()` — the "opposite amplitude" signal Abe
  asked for (loud in → low control, silence → full control).
  → `dance/synths/fx.mjs:envelopeFollower` + `invertControl`

## Proposed

### analysis — audio → control / triggers
- ⚪ **pitch tracking** — audio → f0 → MIDI / control curve. Pairs with the
  envelope follower as the other half of audio-reactive input.
- ⚪ **audio gate / trigger** — amplitude gate that fires events; e.g.
  beatbox into a mic to trigger drum samples.

### score — notation & sequencing
- ⚪ **audio → rhythm sequence** — onset-detect an audio loop (e.g. a loop
  of someone talking) and emit a drum sequence as a `.np` score.
- ⚪ **note subdivision** — Ableton-style: split a long note into
  1/2 · 1/4 · 1/8 · 1/16 subdivisions.

### fx — synthesis
- ⚪ **vocoder** — carrier/modulator vocoder (filter-bank or FFT).

### forms — structure & theory
- ⚪ **Fibonacci meter** — Fibonacci-sequence division & addition for
  measures/bars. ref: <https://en.wikipedia.org/wiki/Fibonacci_sequence>
- ⚪ **species counterpoint** — a rule checker / generator after Fux's
  *Gradus ad Parnassum*.
  ref: <https://archive.org/details/imslp-ad-parnassum-fux-johann-joseph>
  (that scan is the Latin edition — an English one is around)

---

## Reference links Abe sent

- <https://www.youtube.com/watch?v=nENw4q-zRZU>
- <https://www.youtube.com/watch?v=NFryeLwhER0>
- <https://www.youtube.com/watch?v=8y10psbF8gA>
- <https://www.youtube.com/watch?v=WsCYjmkh2dM> — **at 1:17:00** he says
  this gives an algorithm for drum programming worth lifting.
