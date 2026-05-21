# gradus

A classical two-voice /pop track — note-against-note counterpoint in
**D dorian**, composed entirely from the pop compositional tools.

> *gradus* — after Fux's *Gradus ad Parnassum*, the counterpoint treatise.

## How it's built

`bin/compose.mjs` generates and renders the whole piece (deterministic —
fixed RNG seed, identical every run):

- **Form — Fibonacci.** Phrase lengths are consecutive Fibonacci numbers
  (`fibSequence` → 5, 8, 13) laid out as an arch: `5 · 8 · 13 · 13 · 8 · 5`
  notes. `fibPartition` places one internal rhythmic stress per phrase.
- **Pitches — species counterpoint.** Each phrase grows a stepwise
  cantus firmus, then `generateFirstSpecies` writes the upper voice in
  strict Fux first species; `checkFirstSpecies` verifies every phrase.
- **Voice — sinepower.** Both lines render through the stacked-sine
  `pad` voice, gently spread in stereo (cantus left, counterpoint right).
- **Tempo** is solved so the piece lands at ~1:24.

## Files

- `bin/compose.mjs` — composer + renderer
- `gradus.mp3` — the track (~1:24)
- `gradus.cantus.np` / `gradus.counter.np` — the two voices as scores

## Run

```bash
node pop/gradus/bin/compose.mjs
```
