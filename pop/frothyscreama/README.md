# frothyscreama

a screamo track. quiet twinkle, then everything at once, then quiet again,
then the long scream out. ~2:41, 180 bpm, b minor with the ninth and the
major seventh left ringing. part of the *pixsies* body: it is still water,
only now it is boiling.

## the voice

jeffrey-pvc (eleven_v4) screaming whole lines. one lyric line is one
elevenlabs render — a full scream in one breath, never syllables glued
together. the lines live in `lyrics/frothyscreama.txt`, written the way
they are screamed: capitals, held vowels.

- screams: `[screaming] {text}!` at stability 0.5, similarity 0.9, style 0.6
- whispers: `[whispers] {text}` at stability 0.65, similarity 0.9, style 0.15

raw returns are kept in `out/takes/`, named by a hash of text + settings, so
a re-render costs nothing. in the mix each scream is the take itself in the
middle, the same take twice more a few cents apart in each ear, and a quiet
octave underneath. the long holds are the same take stretched, not a new
take.

## the band

- **guitars** — `pop/guitar/c/strum`, the physical six-string. clean and
  picked in the quiet parts; in the loud parts two different performances,
  one per ear, each through two more tanh stages and a cabinet.
- **bass** — synthesized, on the roots, gritty when the guitars are.
- **drums** — synthesized: ride and rim when it is quiet, d-beat, blast
  beats, and a half-time collapse in triplets.

everything is printed to plastic tape (`pop/lib/material.mjs`) before the
limiter.

## run

```bash
node pop/frothyscreama/bin/scream.mjs                  # the takes (billable, cached)
node pop/frothyscreama/bin/render-frothyscreama.mjs    # → out/frothyscreama.mp3
node pop/frothyscreama/bin/render-frothyscreama.mjs --form
node pop/frothyscreama/bin/render-frothyscreama.mjs --material none
pop/.venv/bin/python pop/frothyscreama/bin/probe.py out/takes/*.mp3   # is it screamed?
```

## the c engine

`c/frothyscreama.c` is the track's renderer. `render-frothyscreama.mjs` is
the composition: it writes the arrangement out as a score, every random
choice already made, and the engine makes the samples and `c/run-c.mjs`
masters them. The same voices are still in the JS file as the engine's
reference, and `compare.mjs` holds the two to each other sample for sample.

```bash
node pop/frothyscreama/c/run-c.mjs <score.txt>                    # render a score already written
node pop/frothyscreama/bin/render-frothyscreama.mjs --js --stems  # the JS reference, with per-bus wavs
node pop/frothyscreama/c/compare.mjs                              # engine vs reference
```
