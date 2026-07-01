# fluttabap360 — C renderer

Port of the fluttabap360 DSP to C, mirroring the hellsine / nullabye split:
**JS composes + bakes a score; C does all the DSP.** The arrangement brain
(BLOCKS, the four passes, the key journey, water-stream gating) stays in
`../bin/render-fluttabap360.mjs`; `--bake` emits a flat score of events, and
`fluttabap360.c` synthesizes them sample-for-sample to match the JS reference.

## Pipeline

```
node ../bin/render-fluttabap360.mjs --bake out/fluttabap360.score.txt   # JS → score
node c/run-c.mjs out/fluttabap360.score.txt --out out/fluttabap360-c.mp3 # C → audio
node c/compare.mjs                                                       # parity A/B vs JS
```

`run-c.mjs` builds the engine if stale, renders pre-master f32, then runs the
SAME ffmpeg master chain as the JS engine. `compare.mjs` diffs the C pre-master
f32 against the JS `--keep-raw` f32 (target: RMS diff > 60 dB below the
reference — libm-ulp drift only).

## Port milestones (verify parity at each step)

1. **scaffold** — build.sh, run-c.mjs, compare.mjs, `--bake` / `--keep-raw` /
   `--solo` flags on the JS engine, score header. ← in progress
2. **sub + kick** — SUBSINE (butter sine sub) + kick. First parity check.
3. **modal marimba** — port `../synths/marimba.mjs` (xylophone/bass/kalimba/
   glock/vibraphone presets, modal partials, mallet impulse, tube resonator,
   tremolo, stick-click). Benefits the whole marimba lane.
4. **perc + scratch + scream** — port `../synths/perc.mjs`.
5. **ambience + mix** — PAD, drop, waterStream, reverbInPlace, delayInPlace,
   tape-fuzz + hiss, mixdown panning / normalize / final-lift / clamp.

The ffmpeg master chain (EQ / comp / limiter + DistroKid loudnorm) is NOT
ported — it stays in `run-c.mjs`, fed the C engine's pre-master f32.

## Score format

Text, one event per line; `#` comments. Header sets `sr`, `ns` (sample count),
and global mix params. Per-voice lines carry the synth name + its params +
absolute start time in seconds (post-swing/humanize — the JS bakes the already
jittered times so the C engine stays a pure synthesizer with no RNG of its own,
except the deterministic per-voice noise seeds which are derived from the event
exactly as the JS does).
