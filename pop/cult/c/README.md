# cult remix — C engine

`cultremix.c` is the whistlegraph cult remix renderer (`pop/cult/bin/render10.mjs`,
v10.1) ported to the fleet-standard single-file C engine, in the shape of
`pop/boombaboom/c`, `pop/hellsine/c` and `pop/hopehop/c`.

```
bash pop/cult/c/build.sh        # cc -O2 -o cultremix cultremix.c -lm
pop/cult/c/cultremix            # → pop/cult/c/out/cult-remix-c.wav
```

## Parity vs `node pop/cult/bin/render10.mjs`

Measured against a fresh Node render of the same source
(md5 `2e2ac6bab8a5f8cc3c17a2136a9dcf9b`), same machine, 2026-08-16:

| metric | Node | C |
| --- | --- | --- |
| duration | 10,905,600 frames · 227.200 s | identical, frame-exact |
| integrated loudness (ffmpeg loudnorm) | −17.40 LUFS | −17.40 LUFS |
| true peak | −0.72 dBTP | −0.72 dBTP |
| sample peak | −0.724 dBFS | −0.724 dBFS |
| pre-master peak / linear trim | 1.895557 / 0.485 | 1.895557 / 0.485 |
| 1 s window RMS Δ @ 20/60/95/155/210 s | — | 0.000 dB at all five |
| max per-sample difference | — | 1.34 × 10⁻⁷ (residual −165 dBFS) |
| render wall time | ~17.3 s | ~3.9 s (~4.5×) |

Every voice is exact — kick/revKick/wub, bass, sines, DTMF/bops/clicks/taps,
woodTap, friction and frictionPath, shot with pitch wiggle, granular stretch,
subharmonic doubling, choir/secretChoir/raga/phoneTune, tube DC block, dub
delay, both ducks, the side return and the single linear trim. No voice is
approximate: both generators are reproduced bit-for-bit (score LCG seed
20220120 stepping `seed*1664525+1013904223` mod 2³² then /2³²; noise
xorshift32 seed 987654321 divided by 2³²−1), and every `jit()/vel()/nrnd()`
draw happens in the Node score's evaluation order — jit-before-vel pairs are
explicitly sequenced because C leaves argument order unspecified. The
remaining 10⁻⁷ residual is float32 accumulation-order noise.

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

The port tracks the working-tree v10.1 source (iwannaslow takes, withheld
words, choir sub floors, blip counterline, phone tune, raga ornament, phone
gestures, 112 bars / 9 acts). If `render10.mjs` changes, re-port before
trusting parity.
