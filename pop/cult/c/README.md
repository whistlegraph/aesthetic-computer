# cult remix — C engine

`cultremix.c` is the whistlegraph cult remix renderer (`pop/cult/bin/render10.mjs`,
v10.1) ported to the fleet-standard single-file C engine, in the shape of
`pop/boombaboom/c`, `pop/hellsine/c` and `pop/hopehop/c`.

```
bash pop/cult/c/build.sh        # cc -O2 -o cultremix cultremix.c -lm
pop/cult/c/cultremix            # → pop/cult/c/out/cult-remix-c.wav
```

## Parity vs `node pop/cult/bin/render10.mjs`

Re-ported 2026-09-01 against the wannadash release score (the elastic
explosions, the release-edit geometry, and the two rounds of mix notes:
`bright`, `air`, `staircasePan`, `guitarStrum`, `cultCluster`, `keepTime`,
the dinner bell, the drive under the warble). Measured against a fresh Node
render of the same source, same machine:

| metric | Node | C |
| --- | --- | --- |
| duration | 10,905,600 frames · 227.200 s | identical, frame-exact |
| pre-master peak / linear trim | 1.984729 / 0.464 | 1.984729 / 0.464 |
| sample peak after trim | 0.920000 | 0.920000 |
| max per-sample difference | — | 1.79 × 10⁻⁷ (−135 dBFS) |
| worst 250 ms window, residual vs signal | — | −139.9 dB |
| render wall time | ~17 s | ~3.5 s (~5×) |

Every voice is exact, including the ones added this pass: the Karplus-Strong
guitar (its per-note xorshift seed hashed from `round(t*1000)` and the midi
exactly as the Node file does it, so a chord never shifts the friction
noise), the air bed on its own LCG, the Shepard-pan staircase, the elastic
field over the five buses, and the vox macro arc — which the previous port
had never carried, and which was the entire residual before this pass
(every sung take between bars 8 and 76 was 0.72–1.0× and a shade dark in
Node and not in C).

The fast lane is `bin/bake-c.sh`: build, render, cut the DistroKid master
from the C output. The Node renderer remains the receipt generator for the
review video.

## What differs from the Node renderer

- **No events JSON receipt.** `render10.mjs` remains the receipt generator
  for the video/score pipeline; this engine renders audio only.
- **No `--stems`, `MUTE=`, `ONLY=` debug lanes.**
- **No mp3 decode.** The perc/sweep demos load from the ffmpeg caches the
  Node renderer leaves in `pop/cult/out/.cache-*.wav`; a missing cache is
  built by shelling out to ffmpeg with the same command.
- Output is `c/out/cult-remix-c.wav` (float32 stereo 48 k, same format as
  the Node writer), leaving `out/cult-remix-v10-full.wav` to the Node lane.
- Memory: ~850 MB peak (fifteen N-length float busses plus envelopes, same
  layout as the Node render).

The port tracks the working-tree wannadash source. If `render10.mjs` changes,
re-port before trusting parity — and check with the residual, not by ear: the
last drift (the vox arc) was audible only as "the voices sit differently".
