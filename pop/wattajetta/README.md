# wattajetta

a fighter jet made entirely of water. part of the *pixsies* body —
companion to the water-jet image gens (jeffrey in a cockpit where the
fuselage, canopy and control stick are all clear liquid). ~2:48.

## the law of this lane

everything is water, so everything is one of a few honest materials:

- **sine** — water holding a shape. phase-increment oscillators only.
  the kick (pitch-swept, 12ms hole attack), the sub fuselage, the
  doppler flybys, the bloop drips, the formant-shaped water choir.
- **spray** — water losing its shape. white noise carved by a gliding
  bandpass, kept at whisper level.
- **bells** — water frozen mid-ring. the FEM engine (`pop/bell/c`),
  and over the flight the water hardens: glass → bronze → steel →
  **stone** (a granite lithophone material added to the engine for
  this track), decays tightening as it goes. tubular chimes, bowl
  anchors, one church toll at the coda. runs stay at E4–E5 — higher
  reads tangy on laptop speakers, and bells never pass through the
  crunch.
- **vocal** — jeffrey-pvc via `pop/bin/say.mjs` (assets cached):
  "wattajetta!" and "WAAAYERRR!" drops at the section seams.

a turntablist rides the whole thing: scratch gestures scrub slices of
the track itself (the kick lives on its own engine bus — `--kickraw` —
so scratching layers over the rhythm, never destroys it), a tanh
crunch ramps in as the water hardens, the platter drags mid-breath,
the tempo accelerates ~6% through the back half, and the record
wobbles out underwater.

## arc (bars, 138 BPM)

drop A glass 0–11 · breath 12–15 · drop B glass+bloops 16–27 ·
breath (choir enters) 28–31 · drop C bronze 32–43 (4/4 kick from 38) ·
breath 44–47 · drop D steel 4/4 + trance stabs 48–59 · breath
(underwater bells) 60–63 · drop E STONE 64–75 · church coda 76–79 ·
mist 80–95.

## run

```bash
node pop/wattajetta/bin/render-wattajetta.mjs           # → out/wattajetta.mp3 (cover embedded)
node pop/wattajetta/bin/render-wattajetta.mjs --score   # print the baked engine score
node pop/wattajetta/bin/gen-cover.mjs                   # cover (cached; --force to reroll)
```

the composition bakes a text score; `c/wattajetta.c` renders it
(f32le stereo 48k, kick split via `--kickraw`), the composer mixes the
FEM bells + vocals + scratches + warp on top, and ffmpeg masters.
cover prompt lives in `wattajetta.illy.txt` — refs (the water-jet
still + the pals logo) go to the gpt-image-2 edits endpoint and the
pals mark is DRAWN into the illustration, never composited.
