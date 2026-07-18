# maytrax

a prodigy-shaped techno track with juno-reactor / matrix-soundtrack
energy. the rhythmic spine is **breakbeat-driven big-beat techno** —
not 4-on-the-floor — with a layered distorted 909 kick underneath.
over the top: hoover stabs (already in `pop/hippyhayzard/synths/
hoover.mjs`), acid 303 squelch, orchestral hit accents (brass /
flugelhorn / church bell, already sampled in `pop/hellsine/samples/`),
and jeffrey vocal shouts on the drops.

> **the lane question:** can the early-90s rave/big-beat/film-action
> sound (matrix reloaded "burly brawl", prodigy "firestarter", juno
> reactor "god is god") be reconstructed bottom-up from AC primitives
> + a small palette of already-licensed sample sources, without
> sampling the films themselves?

## why "maytrax"

phonetic riff on *matrix*. the film score (don davis + juno reactor)
isn't sampleable for release — every track ships to distrokid, so any
borrowed audio would have to clear with warner. instead this lane
rebuilds the *textures*: detuned-saw hoover stabs, sanskrit-vowel
formant chops, taiko-shaped distorted 909, brass-hit punctuation,
breakbeat under it all.

## ingredients

| element            | source                                                  |
|--------------------|---------------------------------------------------------|
| breakbeat          | inline (programmed amen / funky-drummer chops)          |
| 909 kick (drive)   | inline (pitch-enveloped sine through tanh)              |
| hoover lead/stab   | `../hippyhayzard/synths/hoover.mjs`                     |
| acid 303 squelch   | inline (saw + SVF lowpass with envelope on cutoff)      |
| orchestral hit     | `../hellsine/samples/brass-strong-e2.wav`               |
| flugelhorn accent  | `../hellsine/samples/flugelhorn-asharp.wav`             |
| bell punctuation   | `../hellsine/samples/church-bell.wav`                   |
| vocal shouts       | `../hellsine/samples/jeffrey-vocal-*.wav` (sliced)      |

cc0 reference samples (taiko, sanskrit chants, big orchestral hits)
land in `samples/` as they're auditioned.

## usage

```bash
# render the bed to out/maytrax.mp3
node pop/maytrax/bin/maytrax.mjs

# with overrides
node pop/maytrax/bin/maytrax.mjs --bpm 140 --out ~/m.mp3
```

### microtrax

`microtrax` keeps `pianotrax`'s A-major ragtime motion but removes the piano
entirely. Short additive-sine bells play the syncopated figures over continuous
four-on-the-floor sine booms and tiny 16th-note clockwork. Its 72 bars at 144
BPM land exactly at 2:00.

```bash
node pop/maytrax/bin/render-microtrax.mjs
# → out/microtrax.mp3
```

`femrag` plays the same two-minute micro-rag through a marimba-like hybrid of
bowl body modes and the FEM-derived shell modes from `pop/bell`:

```bash
node pop/maytrax/bin/render-femrag.mjs
# → out/femrag.mp3
```

## conventions

- bpm: 138–142 (prodigy classic; `firestarter`=140, `breathe`=137)
- form: intro (4 bars) → drop (8 bars) → break (4 bars) → drop (8 bars) → outro (4 bars)
- key: F minor (matrix-noir flavor; works with hoover detune)
- mp3 out lives in `out/maytrax.mp3`, scratch wavs in `out/.maytrax-*.wav`

## status

scaffold + minimal bed. vocal score lives in `maytrax.np` (placeholder).
not yet released; not yet in `pop/RELEASES.md`.
