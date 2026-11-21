# frothyscreama

a screamo track. quiet twinkle, then everything at once, then quiet again,
then the long scream out. the opening and the last bars are instrumental: the
whispers that stood there were taken out on 2026-11-20 (they read as too loud);
the two in the middle room ("the house") remain. ~2:41, 180 bpm, b minor with the ninth and the
major seventh left ringing. part of the *pixsies* body: it is still water,
only now it is boiling.

## the voice

jeffrey-pvc (eleven_v4) screaming whole lines. one lyric line is one
elevenlabs render — a full scream in one breath, never syllables glued
together. the lines live in `lyrics/frothyscreama.txt`, written the way
they are screamed: capitals, held vowels.

- screams: `[screaming] {text}!` at stability 0.5, similarity 0.9, style 0.6
- whispers: `[whispers] {text}` at stability 0.65, similarity 0.9, style 0.15

a line that could not be understood once pitched and in the mix has its
delivery picked by audition (`PICKS` in `bin/scream.mjs`: the words respelt
for the take, or another tag such as `[screaming in pain]`); whisper.cpp read
every candidate back.

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
- **drums** — sampled (Virtuosity Drums): ride and rim when it is quiet, d-beat, blast
  beats, and a half-time collapse in triplets.

everything is printed to plastic tape (`pop/lib/material.mjs`) before the
master.

## the master

`bin/frothy-master.mjs`, one chain for both engines: lows mono under 120 Hz,
a fast 4:1 glue, one measured static gain into a 4× oversampled limiter at
−2.3 dBFS. 2026-11-20: −10.4 LUFS, −2.3 dBTP, PLR 8.1, LRA 6.8, phone loss
−1.2 dB, mono loss −0.5 dB (`node pop/bin/master-audit.mjs`); the limiter is
idle 72 % of the record and takes 2.6 dB at worst.

the kit is a rock kit in a big room: room mics up with the close ones, kick,
snare and toms tuned down and hit harder, and the drum bus has its own hall
and a parallel crush (`--drum-hall`, `--drum-crush`; both engines). the voice
is turned up over it rather than the band turned down (`--vox-gain`,
`--lean-gtr`, `--lean-drums`, `--bass-gain`).

## chaos

`bin/chaos.mjs` asks the same voice for wordless roars, shrieks, growls and
stabs (40 takes, cached in `out/takes/x*.mp3`). `bin/chaos-probe.py` measures
each one — length, level, roughness, how unsteady the pitch is, and what
whisper.cpp hears in it — throws out the tame ones, and with `--bank` writes
`out/chaos/`: the keepers by kind, `manifest.json`, and an audition reel.
The renderer lays a selection over the record (`CHAOS_PLAN`, gains 0.52–0.64);
`--no-chaos` leaves it off.

Since 2026-11-20 the lead lines are screamed the same way (`WILD` and `PICKS`
in `bin/scream.mjs`: stability 0, style 1, a descriptive tag, the words in
bursts; `bin/wild-probe.py` measures the candidates). s14, s15 and s16 keep
the plain take. Both the wild leads and the placed chaos takes are
aesthetivoxed in the wild mode of `bin/aesthetivox-scream.py` — breath and
note mixed — which measures each result and falls back to a lighter mix, or
to the raw take, where the note does not hold or the roughness drops.
`render-frothyscreama.mjs --bake <score> --pairs <json>` lists every treated
take beside its raw one, and `aesthetivox-scream.py --reel <json> <mp3>` plays
them raw then treated (`out/chaos/frothyscreama-avox-pairs.mp3`).

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
node pop/frothyscreama/c/compare.mjs --no-chaos                   # the same, without the chaos bank
```
