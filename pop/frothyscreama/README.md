# frothyscreama

a screamo track. quiet twinkle, then everything at once, then quiet again,
then the long scream out. ~2:19, 180 bpm, b minor with the ninth and the
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
node pop/frothyscreama/bin/render-frothyscreama.mjs --stems --material none
pop/.venv/bin/python pop/frothyscreama/bin/probe.py out/takes/*.mp3   # is it screamed?
```
