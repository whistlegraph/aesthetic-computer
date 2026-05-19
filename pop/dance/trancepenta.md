# trancepenta

A sibling of `trancenwaltzi` (same `recap/bin/trance.mjs` engine, same
single-mix `scratch-mix.mjs` post-FX) but a **totally different track**:
**5/4 · 126 BPM steady · dorian**, a ported hellsine gabber kick, a
real CC0 horse-gallop / neigh / ocean scene, hats off, beat held until
~0:30. `trancenwaltzi` is untouched — every difference is opt-in flags,
so the non-chill (`trancewaltz`) path stays byte-identical.

## Bake (3 deterministic stages — manual; `bake.mjs` hardcodes the
trancenwaltzi names so trancepenta runs the steps explicitly)

```bash
O=~/Documents/Working\ Desktop/twi-out
# 1 — engine (single all-sine multi-bus source), opt-in flags:
node recap/bin/trance.mjs --mode chill --meter 5 --master \
  --bpm 126 --scale dorian --hell 11 --chill-hats off --gallop \
  --beat-in 30 --out "$O/trancepenta-MASTER-preBright.wav"
# 2 — in-mix post-FX (scratch + restamped ID + breakbeat + slides),
#     beat-locked to the engine's own struct.json:
node pop/dance/bin/scratch-mix.mjs \
  "$O/trancepenta-MASTER-preBright.wav" "$O/.tp-scr.wav" \
  pop/dance/out/.ac-dot-stamp-vocal.mp3 \
  "$O/trancepenta-MASTER-preBright.wav.assets/struct.json"
# 3 — poppier finalize (duration-aware 18 s fade), → MASTER.wav + mp3:
D=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$O/.tp-scr.wav")
ffmpeg -y -i "$O/.tp-scr.wav" -af \
"acompressor=threshold=-19dB:ratio=2.4:attack=15:release=240:makeup=1:knee=6,\
equalizer=f=3000:t=q:w=1.2:g=2,highshelf=f=10000:g=1.5,\
loudnorm=I=-14:TP=-1.5:LRA=11,\
alimiter=limit=0.94:attack=8:release=120:level=disabled,\
afade=t=out:st=$(echo "$D - 18" | bc):d=18" \
-ar 44100 -sample_fmt s16 "$O/trancepenta-MASTER.wav"
```

**Final: ~3:11 · −14.4 LUFS · −1.5 dBTP · 44.1 kHz/16-bit stereo.**

## Flags (engine, all chill-only & opt-in)

| flag | trancepenta | effect |
|------|-------------|--------|
| `--meter 5` `--bpm 126` `--scale dorian` | yes | 5/4, steady 126, dorian (steady tempo because `--bpm` is explicit; default = the 129→155 accelerando) |
| `--hell 11` | yes | ported hellsine gabber/Rotterdam kick (tight "lil kick" first 24 s, then full) |
| `--chill-hats off` | yes | no hi-hats |
| `--gallop` | yes | real-sample horse + ocean scene |
| `--beat-in 30` | yes | kick/snare/tick/floor suppressed until 30 s, ease in ~3 s |

Plus chill-general engine changes (also benefit a future trancenwaltzi
re-bake): enriched piano (sustained holds + a melodic top, some
**reversed / pitch-bent / long-sustain**), guaranteed downbeat
**floor** (beat never fully drops), variant per-hit bass, lead
harmonies, lowered "organ" pad, melodic highs tamed, cat choir −2 oct.

## Sourced SFX — all CC0 (commercial-safe; provenance also in the
gitignored `pop/dance/out/.sfx-credits.txt`)

| local file | source | license |
|-----------|--------|---------|
| `.gallop.wav` | archive.org `Red_Library_Animals_Horses_1` / R13-10 Horse Gallop | CC0 |
| `.neigh.wav` | archive.org `horse-sound-effects` / mixkit-stallion-horse-neigh-1762 | CC0 |
| `.waves.wav` | Freesound #376799 "Crashing Waves into Rocks 1" | CC0 |
| `.foghorn.wav` | Freesound #37915 "fog horn sample(wet)" | CC0 |
| `.boathorn.wav` | Freesound #437687 "Loud boat horn" | CC0 |
| thunder, steam whistle | synthesized in-engine | n/a |

The `.gallop/.neigh/.waves/.foghorn/.boathorn.wav` live in the
gitignored `pop/dance/out/` (not committed — re-fetch from the sources
above; Freesound key is vault-only, see `pop/SCORE.md`). The gallop /
neigh / ocean samples are placed via the engine's deep + echo +
**variable-playback-rate** helper (they bend with the beat & morph
under), beat-spread (random window per hit — not loop-loop).

## Open / next

- **jeffrey ElevenLabs vocal-harmony layer** — humms/toots/whistle via
  `/api/say` jeffrey-pvc, autotuned to the dorian scale + tempo grid,
  harmonized as a counterpoint bouncing between key instrument tones.
  Its own dedicated pass (network + ElevenLabs cost).
