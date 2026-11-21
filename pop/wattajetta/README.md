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

## rapvox

`pop/bin/rapvox.py` is aesthetivox for rap: a spoken or rapped take, sung-tuned without being slowed.
The sung route stretches vowels and replaces the pitch; rapvox keeps the speed and the place of every
word and uses the pitch side only.

- **speed**: no global time-compression. The take stays at the speed it was performed (`--fit` can
  lengthen a take to a phrase and refuses to shorten one by more than 3 %); a renderer cuts the result
  on its word timings and moves whole words to the nearest sixteenth. If the rap is not fast enough,
  it is recorded again faster, or given more bars.
- **the chant**: every voiced frame takes the note in force. Give a contour (`--contour`, a list of
  `[seconds, midi]`), or word timings (`--words`) and three notes (`--notes chant,lift,cadence`): the
  chant note, the lift on every fourth word and before a full stop, the cadence on a sentence's last
  word. `--nat` (0.25) of the speaker's own pitch movement stays on the note.
- **consonants**: the take's own band above 4 kHz and its unvoiced frames are laid back in untreated.
- **the hold**: `--hold` seconds on the last word's vowel only.
- **strength** (`--strength`, 0.8): how much of the voice is the tuned one.
- **the guard**: whisper.cpp reads the take and the result back against the words; if recall falls
  more than `--drop` (0.1) the strength is lowered (×0.7, ×0.5) and tried again, and past that the
  take goes through untreated. The numbers are written beside the output as `<out>.json`.

```
pop/.venv/bin/python pop/bin/rapvox.py --take take.mp3 --words take.json --notes 47,50,52 --fit 7.2 --out rap.f32
```

Its first job is this track's commit rap (`bin/cutie-rap.mjs` makes the takes and their word timings;
the renderer calls rapvox per phrase and places the words on the machine row's sixteenths).

## Weirdest lip liner

“Put the weirdest lip liner on” returns on the original lead melody. The choir holds “weird.”
The opening comes from the liner cut, with a one-bar handbell pickup before the drums.

The 2:55 listening master is `out/pocket-single/wada-jetter-weirdest-liner.wav`. Restore the
session with `npm run pop:workspace:restore -- wattajetta` from the repository root. The cover
files and raw Cutie phrase takes are in `out/recovery-inputs/`. Extract the cover archive in
the track directory and the phrase archive in the private vault.

The session uses the previous weird choir score and WAV from the cut below. From the track directory:

```sh
src=out/weirdest-liner/source
take=out/weirdest-liner/rebuild
for cadence in cheeky warm whisper; do node bin/cutie-takes.mjs --line lip-weirdest --cadence "$cadence"; done
../.venv/bin/python bin/weirdest-liner.py "$src/local.score.txt" out/weirdest/source/jobs.json out/weirdest/takes/weirdest.score.txt "$take"
node c/run-cutie.mjs "$take/liner.score.txt" --accel 124 --out "$take/render.mp3"
../.venv/bin/python bin/punch-weirdest.py "$take/liner.score.txt" out/pocket-single/wada-jetter-weirdest.wav "$take/render-master.wav" "$take/liner.wav" --liner-opening "$src/wada-jetter-liner-master.wav"
```

## Earlier weirdest bell-break cut

The first lip-liner cue sings “weirdest.” The later cues and the eight choir entrances hold “weird.”
From the track directory, pick a fresh directory for `new_cut`.

```sh
bell_break=out/weirdest/source/bell-break
line_jobs=out/weirdest/source/jobs.json
new_cut=out/weirdest/rebuild
node bin/cutie-takes.mjs --line weirdest --cadence cheeky
node bin/cutie-takes.mjs --line weird --cadence warm
../.venv/bin/python bin/weirdest-cut.py "$bell_break/liner.score.txt" "$line_jobs" "$new_cut"
node c/run-cutie.mjs "$new_cut/weirdest.score.txt" --accel 124 --out "$new_cut/render.mp3"
../.venv/bin/python bin/punch-weirdest.py "$new_cut/weirdest.score.txt" "$bell_break/wada-jetter-bell-break-master.wav" "$new_cut/render-master.wav" "$new_cut/weirdest.wav"
```
