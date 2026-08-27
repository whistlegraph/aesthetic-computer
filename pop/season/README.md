# pop / season — "whistlegraph season --- remix"

A remix of the **`h0t`** whistlegraph work (spring 2022) — the one whose
chant is already a dialogue:

> it's too hot / no it's not / now I'm back in season

`posts.json` tags **six** posts with the work, and whisper confirms every
take says the same three lines, with a sung "doo doo doo" walk between the
argument and the tagline:

| take | date | views | caption |
| ---- | ---- | ----- | ------- |
| 7080453509149134126 | 2022-03-29 | 28.3M | spring flower |
| 7079639110025088298 | 2022-03-27 | 12.4M | all four seasons in 9 seconds |
| 7087134943930846506 | 2022-04-16 | 7.8M | springy vibesies |
| 7093848478245358894 | 2022-05-04 | 6.9M | do do doo do dooooo 🌼 🫥 nawwww waaa baaaak in seeeeeeeezn |
| 7078095590348836139 | 2022-03-23 | 6.3M | summer, fall, winter, 🌼 spring |
| 7078347899049905450 | 2022-03-23 | 3.5M | |

TikTok blocks this IP, so the mp4s come from the AC asset mirror that
whistlegraph.org's own `posts.json` points at. Same files, no substitution.

**v1 is `out/season-remix-v1.mp3`** — 2:56, 122 BPM, key of A with the
third left open. A dance record: the argument staged across the stereo
field, the tagline as the making-up.

## What the source gave us

Receipts: `analysis/harvest.json` (all six takes — duration, beat_track
tempo, pyin f0 over every whisper word span, whistle scan) and
`analysis/melody.json` (the primary take's note segmentation and onset
grid). `bin/analyze.py` and `bin/melody.py` rebuild both; whisper was
`whisper-cli` with `ggml-small` at word level (`-ml 1`), same recipe as
`pop/cult/alt/` but the multilingual small model — the `small.en` the cult
harvest used lived in a scratchpad that no longer exists.

Three takes feed the bank:

- **P — 7079639110025088298** ("all four seasons in 9 seconds"), the
  primary. The cleanest full statement:
  - the chant is spoken, not pitched — "hot" grazes F3, the "no it's not"
    answer sits near **C#3**;
  - the doo walk is sung: **F4 F4 G4 G4 E4 D4 C4**;
  - the tagline is sung: **E4 D4 C#4 B3 A3** — a descending pentachord
    that **lands on A**, which is where the remix gets its key;
  - the take walks at **~103 BPM** (beat_track 102.3, and the doo notes
    step every ~0.58 s), and the full "it's too hot" phrase spans
    **1.96 s — one bar at 122.4 BPM**. So v1 runs **122** and no vocal is
    ever time-stretched: the call fills its bar exactly.
- **H — 7080453509149134126** ("spring flower"), the most-viewed take, and
  the one where "no it's not" comes back **high — ~A4**. The argument
  crosses registers there, not just people.
- **M — 7087134943930846506** ("springy vibesies"): a mid answer (E3
  falling to C3) and a second doo walk (C#4 D#4 A#3).

Checked honestly, like cult's harvest was: the pyin whistle scan
(500–3000 Hz) finds no sustained tonal run that isn't sitting exactly at
the scan floor — i.e. octave shadows of the sung line, **no actual
whistling** in any of the six takes. The melody here is doo'd, not
whistled.

## The bank

`bin/slice.mjs` cuts **24 slices** — the phrases (`its-too-hot`,
`no-its-not`, `no-its-not-high`, `no-its-not-mid`, `season-line`,
`doo-run`…), the single words (`hot`, `not`, `no`, `too`, `season`), the
doo notes by pitch (`doo-f`, `doo-g`, `doo-ed`, `doo-c`), and the whole
argument (`chant-full`) for the outro. Same dressing as cult's bank:
trim the dead air, normalize, raised-cosine top and tail so no slice can
ever click. `samples/.manifest.json` (tracked) records every cut's take,
span and measured f0.

Top-end percussion is `pop/demos/samples/` (hats, clap, ride, snap, the
noise sweep); the kick and all bass are synthesized so the low end stays
under control.

## v1 — the argument

The material is literally an argument, so the record stages one:
**"it's too hot" states its case from the left; "no it's not" answers
from the right** — and in the second half the answer comes back high.
The sung tagline gets the breakdown, and from bar 72 it sails *over* the
argument: both things true at once. 88 bars:

| bars | time | section | what happens |
| ---- | ---- | ------- | ------------ |
| 0–8 | 0:00 | **intro** | kick swells in under a pad; "hot"/"not" flicker at the edges |
| 8–24 | 0:16 | **hookA** | the argument, low vs low — two bars of call, two of answer |
| 24–32 | 0:47 | **doo** | the doo walk leads; drums thin to kick and ride |
| 32–48 | 1:03 | **hookB** | the answer moves up to A4; claps on 2 and 4, open hats |
| 48–56 | 1:34 | **break** | kick out; "now I'm back in season", twice, over the pad |
| 56–80 | 1:50 | **hookC** | fullest: both answers trade bars, 16th hats, tagline over the top from bar 72 |
| 80–88 | 2:37 | **outro** | drums peel; the original take speaks once, unprocessed; the last word is "season", ringing in the delay |

The harmony is an **A pedal with F and G colour (A·A·F·G, one bar each),
root+fifth everywhere and the third left open** — the doo walk carries
C-natural and the tagline carries C#, and an open fifth is the only floor
both can stand on without anybody flinching. The record refuses to
referee that argument too.

The DSP is `pop/cult/bin/render10.mjs`'s scaffolding stripped to what a
dance record needs — the thick electro kick, sine-bump bass, harmonized
sine pads, the dotted-eighth dub delay (returned at 0.7 width: a hard
ping-pong folds to mono at −3 dB, and the outro is mostly tail), the
mono-safe Special Sign side return, a kick-keyed duck (depth 0.55) with
the vox bus riding +3 dB proud. No DTMF, no tube, no skids. Every rule
survives: 10 ms raised-cosine tails on every voice, ducks that ramp
rather than step, no master tanh, ONE linear trim.
`out/season-remix-v1.events.json` is the score receipt — 1424 events.

## Run it

```bash
node pop/season/bin/slice.mjs        # rebuild the sample bank
node pop/season/bin/render.mjs       # score → out/season-remix-v1.wav
bash pop/season/bin/cut-v1.sh        # master + mp3 (MEASURE → one static dB → limiter)
node pop/cult/bin/qc.mjs pop/season/out/season-remix-v1.mp3
node pop/season/bin/render.mjs --stems   # bus stems, for balance checks
```

The analysis needs `pop/.venv` (see pop/cult/README.md for the recipe):

```bash
cd pop/season
../.venv/bin/python bin/analyze.py   # all six takes → analysis/harvest.json
../.venv/bin/python bin/melody.py    # the primary take → analysis/melody.json
```

`cut-v1.sh` measures once, applies one static dB, and limits. Never a
second loudnorm — at loud targets it silently abandons `linear=true` and
starts riding gain, which manufactures the sample-step artifacts the
renderer was careful never to make.

## Measured — v1

| check | value |
| ----- | ----- |
| duration | 176.1 s (2:56) |
| integrated loudness | **−14.4 LUFS** |
| true peak | **−1.7 dBFS** |
| loudness range | 6.0 LU |
| max raw sample step | 0.339 (0 steps > 0.35) |
| max 2 kHz-band step | 0.159 |
| full-scale runs ≥ 3 samples | 0 |
| mono fold-down | −0.20 dB overall, −1.73 dB worst window |
| bus balance (stems) | vox −14.4 · drums −14.1 · music −17.2 LUFS |

The lead sits ~3 dB proud of the bed, which is the number cult v2
measured its way to and every version since has kept.
