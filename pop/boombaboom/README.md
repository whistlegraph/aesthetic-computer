# boombaboom

@jeffrey's "boom-ba-boom" vocal chant fitted to a peak-time techno bed —
bottom-up from AC's own C synth engine, vocals WORLD-autotuned + stretched.

- **Lane:** `pop/boombaboom/` · peak-time techno · 140 BPM · D minor · 2:27
- **Source:** `~/Downloads/boombaboom.MP4` → `sources/boombaboom-raw.wav`
  (a ~32.5s continuous melodic chant, 96% voiced, sitting around C#4 in a
  D-minor space, with a recurring hook motif **E-E-F** = "boom-ba-boom").
- **Treatment (hybrid, @jeffrey's call):** the hook is kept *as-sung* (gently
  WORLD-locked to the E4/E4/F4 he already sang), the verses are *re-mapped* —
  his phonemes WORLD-locked onto a designed rolling D-minor topline.
- **Bed:** `c/boombaboom.c`, forked from `pop/minitek/c/deeptek.c`. Keeps
  deeptek's **deep, round, chill kick** (96→44 Hz pitch-swept roll, soft
  click) — no clicky warehouse thud — but adds peak-time drivers: a
  relentless offbeat open-hat "tss", a warm backbeat clap, a harder
  sidechain pump (0.80), bigger drop impacts, real breakdowns, and the FM
  counter-lead pulled back so the vocal hook leads.

## Form

`intro(8) build(8) grvA(16) brkdn(8) grvB(16) bigbrk(8) grvC(16) outro(4)`
= 84 bars / 147s. Drops at **27.4s** (grvA), **68.6s** (grvB), **109.7s**
(grvC, lands hardest). The hook repeats every 2 bars through each drop; the
re-mapped verse floats over the drum-less `brkdn`; a held, drenched D4 pad
builds tension through `bigbrk` into the final drop.

## Pipeline

```
bin/slice-cells.py    # onset-slice source → 29 cells + cells.json (pitch-tagged)
boombaboom.np         # the melodic intent (hook + re-mapped verse + pad)
bin/bake.mjs          # WORLD-lock + rubberband-stretch each cell → place → mix → master
  ├─ bin/place-vocal.py   # sample-accurate numpy stem placement
  └─ c/boombaboom.c       # the techno bed
```

Rebuild everything:

```bash
pop/.venv/bin/python boombaboom/bin/slice-cells.py   # only if re-slicing the source
node boombaboom/bin/bake.mjs                          # → ~/Documents/Shelf/boombaboom/
```

Outputs land in `~/Documents/Shelf/boombaboom/` (`boombaboom.mp3` 320k +
`boombaboom-MASTER.wav`). Master chain: minitek club master — sub weight under
the kick, de-mud, presence, glue comp + soft-clip, loudnorm I=-9 / TP=-1.0.

## Conventions used

- WORLD f0-replacement for autotune (`bin/pitchsnap_world.py`), rubberband
  only for time-stretch — see `[[pop_world_autotune]]`.
- Bottom-up: the music is AC-native C synthesis; only the vocal is a
  performance layer — `[[feedback_pop_bottom_up]]`.
