# sailor-song

A cover of Gigi Perez's "Sailor Song", played by @jeffrey's niece: her voice
and a capo'd nylon-string guitar, recorded on one phone mic
(`~/Desktop/IMG_8699.mov`, 2:59). This lane is an **outside remix**. It adds
drums and a bed *under her performance*. Her voice and guitar are the
compositional substrate, the same way fía's live MC is in `jungle/`. That
keeps the lane inside the bottom-up posture, with nothing generated top-down.

## Pipeline

```sh
# 1. audio out of the video (48k stereo) + mono for analysis
ffmpeg -i ~/Desktop/IMG_8699.mov -vn -ac 2 -ar 48000 pop/sailor-song/src/take.wav

# 2. stems: Demucs through the shared sample path
mkdir -p pop/samples/sailor-song-take
cp pop/sailor-song/src/take.wav pop/samples/sailor-song-take/source.wav
node pop/bin/separate-stems.mjs --slug sailor-song-take
#    guitar = other + bass + drums (strum transients land in "drums")

# 3. read the take
pop/.venv/bin/python pop/sailor-song/bin/analyze-take.py pop/sailor-song/src/take.wav \
  --vocals pop/samples/sailor-song-take/stems/htdemucs/vocals.wav \
  --guitar pop/samples/sailor-song-take/stems/htdemucs/guitar.wav \
  --json pop/sailor-song/take.analysis.json
```

`toolchain/whistlegraph/analyze.py` is the wrong reader for this take. Its
pyin floor is 200 Hz, tuned for whistles, so her voice reads as a wall of
"G3 +35¢". `bin/analyze-take.py` tracks the voice from 80 to 1000 Hz and reads
chords per beat from the guitar stem.

## Word bounds by ear (SyllaWizard track pass)

The aligners get the words near; the ear places them. `bin/sylla-prep.py` cuts
one clip per lyric line and writes `src/sylla/spec.json` with a `track` field, so
`SyllaWizard --spec …` opens the WHOLE vocal as one scrolling spectrogram with
every word's box in place (drag edges; "from here ▶" shifts a word and every
later one; Save boundaries). `bin/sylla-collect.py` folds the boxes into
`src/word-bounds.json`, which `bin/word-times.py --fa` applies last.

```sh
pop/.venv/bin/python pop/sailor-song/bin/sylla-prep.py
sylla-wizard/.build/debug/SyllaWizard --spec pop/sailor-song/src/sylla/spec.json   # --lines = one line per page
pop/.venv/bin/python pop/sailor-song/bin/sylla-collect.py
pop/.venv/bin/python pop/sailor-song/bin/bounds-study.py [--apply]      # the ear vs the machines, nudges
node pop/sailor-song/bin/splice.mjs && bash pop/sailor-song/bin/bake.sh   # VERSION is #define'd in c/sailorremix.c
pop/.venv/bin/python pop/sailor-song/bin/word-times.py --fa    # AFTER the bake: it reads the newest events receipt
node pop/sailor-song/bin/score-video.mjs --lyrics --audio pop/sailor-song/out/sailor-song-v21.mp3
node pop/sailor-song/bin/score-video.mjs --lyrics --click --audio pop/sailor-song/out/sailor-song-v21.mp3   # same lanes, her stem on a click + kick
node pop/bin/lyricline.mjs --vocal pop/sailor-song/src/vox/cut/vocals-natural.wav \
  --words pop/sailor-song/src/words-record.json --bars pop/sailor-song/measures.cut.json \
  --receipt pop/sailor-song/out/sailor-song-v19.events.json --from 28 --to 40 \
  --title "sailor song" --out pop/sailor-song/out/sailor-song-v19-lyricline-crossing.mp4   # the seam, on a click
```

What the 2026-09-29 hand pass (273 words) taught the tooling — `bin/bounds-study.py`
measures it, and these are now `word-times.py`'s defaults:

- **Words tile edge to edge.** 267 of 268 neighbouring pairs butt together (gap ≤ 15 ms);
  the only real gap is a breath > 350 ms. So a word runs to the next word's onset unless
  the stem is quiet for a breath (`--breath-ms 250`); the old 60 ms rule cut vowels short.
- **An onset is the consonant, not the vowel.** Hand onsets sit a median 6 dB (p75 13 dB)
  under the word's peak, at the start of the rise. Detected attacks (librosa, backtracked)
  are within 40 ms of the ear for 54 % of words and within 100 ms for 90 %.
- **Raw forced alignment beats the syllable-bump re-cut.** Within 40 ms of the ear: FA 61/273,
  FA snapped to an attack ≤ 40 ms 72/273, the shape pass 30/273. The shape pass is now
  opt-in (`--shape`) and the snap window is 40 ms (`--snap-ms`; 150 pulled onsets onto the
  wrong attack).
- **No machine is close enough on its own** (median error ~90–100 ms against the ear), so the
  pipeline's truth is the SyllaWizard pass; the aligner only seeds it.


## Readout (v2, 2026-09-29)

| | |
|---|---|
| Song | Capo 4. Tab shapes Cmaj7 · Em · G · G sound as **Emaj7 · G#m · B · B** ([UG](https://tabs.ultimate-guitar.com/tab/gigi-perez/sailor-song-chords-5363001), [guide](https://www.guitarsmartsupporter.com/2026/07/sailor-song-chords-by-gigi-perez-easy.html)). Original ~94 BPM, 3:31 |
| Her harmony | **G#m · G#m · B · B**, with B sometimes held 3–6 bars. The bass is G# in both "G#m" bars: the Cmaj7 shape strummed through the open low string sounds G#, so her Emaj7 bar is Emaj7/G# ⊃ G#m |
| Key / tuning | G# minor (B major). Guitar sits +13¢ sharp of A440 |
| Pulse | ~120 (felt 60). The slow stretch is verse 1, 110–114; the rest runs 117–125. 84 bars of 4/4 |
| Strum | Her hand plays a 2-beat motif `X..X` (8ths), so a bar reads `X..XX..X`: 1, &2, 3, &4 |
| Beat tracker | It sits ~35 ms early and slips a beat at 27.85 s and 61.17 s. `measures.py` snaps beats to strums and anchors bars on the motif, so 75% of chord changes land on downbeats |
| Form | intro 1–10 · verse1 11–27 · chorus1 28–43 · verse2 44–51 · chorus2 52–67 · break 68–71 · bridge 72–80 · outro 81–84 |
| Voice | G#3–G#4, median D#4. Words per bar are in `src/bars-words.json` (whisper, local only: the lyric is Gigi Perez's) |
| Aesthetivox | median distance to the grid 16¢ → **4¢** (27–87 s). Snap 1.0, 15 ms retune, hysteresis 0.3 st, 20% vibrato kept |

## v2 (follow)

```sh
pop/.venv/bin/python pop/sailor-song/bin/aesthetivox.py   # → src/vox/vocals-aesthetivox.wav + vox-notes.json
pop/.venv/bin/python pop/sailor-song/bin/measures.py      # → measures.json (bars, strums, chords)
node pop/bin/whisper-words.mjs pop/sailor-song/src/vox/vocals-dry-48k.wav --out pop/sailor-song/src/words.json
node pop/sailor-song/bin/render.mjs                       # → out/sailor-song-v2.mp3
```

Kit hits sit on her strum points: kick on 1 and &2, snare on 3, rim on &4.
Measured: 91% of kick/snare hits land within 30 ms of a strum (median 0 ms).
The sine beds are voice-led: sub, a tenor trio under her, and a high pair
over her, with her own register left clear. In the choruses and bridge a sine
shadows her tuned melody a diatonic 3rd above, and chorus 2 and the bridge add
its octave. The layers per section are the `ORCH` table in `bin/render.mjs`.

### A/B drafts

```sh
node pop/sailor-song/bin/render-draft.mjs --mode follow           # kit on her beats
node pop/sailor-song/bin/render-draft.mjs --mode warp --bpm 120   # take time-mapped to a grid
#   --from/--to (source seconds, default 20–80)  --bed 0.5  --rewarp
```

Warp runs rubberband R3 once over the whole take (`-M` timemap: each detected
beat → k·60/BPM, formant-preserving). There are no per-segment seams.
Re-tracked, the warped take centres on 120.2. The original swings 110–122 over
the same minute.

## v6 (C engine · bedroom → dance)

```sh
node pop/sailor-song/bin/regularize.mjs      # her clock damped from bar 24 → src/vox/reg/, measures.reg.json
bash pop/sailor-song/bin/bake.sh             # bells → chart → build → render (C) → cut (master + mp3 w/ cover)
#   SAILOR_STEMS=locked pop/sailor-song/c/sailorremix   # follow her raw clock instead
```

`c/sailorremix.c` is the score (v5 kept beside it as `c/sailorremix-v5.c`).
The record opens at bar 10, two bars of her guitar (a bedroom leveler on
the soft opening) with a soft shaker already keeping the tempo, then her
first word; from bar 14 one lane enters per phrase (congas 14, sub 16, pads
20, a kick on her 1 and 3 at 24, pickup bass 24, hats 25, riser 26) and the
floor lands whole with an explosion on chorus 1 (bar 28). v6.2 pumps it
K-pop style (an 85% kick duck on sines, guitars and harmonies, slower
release), drives the sines harder and wider, brings the electric replay up
in every chorus, and adds a sixth harmony stem, `harm-up8`: her voice an
octave up, leading at the end of chorus 1, the second half of chorus 2, the
bridge's climb and the outro. `lock-vox.mjs` now clamps every note's local
time ratio to 0.85–1.15 (before that a held note at 79.5 s was squeezed to a
fifth of its length). Cover: `bin/cover.py` at 107.15 s, eyes open at the
lens, mouth open on a held note, no title text. The kit plays her necklace's complement (open hats &1/&3,
claps 2/4) so every 8th is struck once between her hand and the kit; off-beat
hits sit at her measured +5 ms bin. Her harmony stems are gated into
arpeggios per section (8ths in chorus 1, 16ths in chorus 2, triplets in the
bridge), each step at its own seat; every drop displaces the whole ring and
springs it back (chamber-04 rule 10). `bin/regularize.mjs` keeps her rubato
through the bedroom bars and 30% of it after: verse 2 119, choruses 120–121,
outro 122 instead of 130. Master: −10.9 LUFS, −2.1 dBTP, LRA 6.6.

## v25 (natural layout · orchestral pop · the performance video)

2026-10-03, on neo. The v20 rearrangement (verse 1 → verse 2 → chorus 1 …) and its
held-vowel seams are gone: `splice.mjs` plays the take in its own order, one segment,
to the end of the regularized stems (her chord rings down and she reaches for the
phone — that is the ending). The record opens two bars before her first word under a
2 s rise. What the engine adds on top of v6:

- **Orchestra** (`bin/orchestra.mjs` → `src/orch/`, FluidSynth + GeneralUser GS, from
  her chart): strings, cello, pizz on her strum motif, horns, timpani, harp, glock on
  her melody, choir aahs, a string **quartet** with lines of its own, a **taiko**, and a
  **button** chord on the last downbeat. Where a part plays is decided there; how loud,
  in `ORCH[NSEC]`; where it sits, in the `SEAT[]` table (every part on an HRTF path).
- **The build:** kick on her 1 & 3 from bar 14 at 12 % rising on a square; chorus 1 big
  but not the floor; verse 2 pulls back; the floor + implosion on chorus 2; the break
  keeps the beat; bridge → outro is a continuous ramp and the outro is the finale.
- **Her:** 2.5:1 leveler, half the air, a de-esser, thin doubles, close-miked all the
  way; the cathedral tail halved. A **scream** double (clipped, growled, wide) from the
  back half of chorus 1. Her guitar at full from the first sample, the replays under it.
- **Effects:** the "k-kiss" **stutter** (her own "kiss" onset on 16ths into each chorus
  and the bridge), dotted-8th **throws** of "sailor?" spinning overhead, two-bar risers
  and implosions at every lift, whole-ring **turns** across the bar before each lift
  and a continuous lap through the finale, a **wub** bass (saw + swept SVF) from chorus 2.

```sh
node pop/sailor-song/bin/orchestra.mjs                       # the parts (re-run after a chart change)
node pop/sailor-song/bin/splice.mjs && VERSION=v25d bash pop/sailor-song/bin/bake.sh   # VERSION also #define'd in c/sailorremix.c
pop/.venv/bin/python pop/sailor-song/bin/word-times.py --fa   # after the bake (reads the newest receipt)
node pop/sailor-song/bin/perf-strip.mjs --audio pop/sailor-song/out/sailor-song-v25d.mp3 --height 130
node pop/sailor-song/bin/perf-video.mjs --audio pop/sailor-song/out/sailor-song-v25d.mp3 --height 540 \
  --strip pop/sailor-song/out/sailor-song-v25d-strip.mp4      # her video retimed onto the record, strip + section grade
```

`perf-video.mjs` retimes `src/take.mov` (the iMessage copy, 960×540, 30 fps) frame by frame
through `src/vox/reg/timemap.txt` — record clock → her take's clock — and lays the mix under
it, the way `marketing/talking-head/bin/warp-to-sing.mjs` rode the YC talking head onto a
sung take. `--no-grade` skips the colour shift; this ffmpeg has no `drawtext`, so no title.

## v73 (2026-10-04 · the critics' pass and the day after)

Four subagent critics (mix · arrangement · timing · pop) reviewed v26; v27 fixed what they
measured (phantom 5th beats, dead gates, the orchestra voiced on top of her, the drop that was
only louder, the demo ending) and the rest of the day was jeffrey's ear. Where it landed:

- **Form.** Natural order, record opens on her first word. Steady four-on-the-floor from the
  chorus-1 pickup (`pickup_t`, two beats before "kiss"); no double time. **The glitch is the
  drop:** her "k-" false start before chorus 1 stays (lifted), the band stops dead on it, the
  chorus lands on "kiss" with a FEM **gong** (no implosion whoosh over "Oh, won't you"). Her
  bar 27 and her pickup line are untouched (`regularize.mjs --fit-bars` exists but is off).
- **Her.** Voice loudest (lead 3.3, 2.5:1 leveler, body/presence/de-tin bands, consonant
  enhancer); guitar seated WITH her (centre, near, her room send), not hollowed; chorus beds
  swell in over four bars; her own **vowel choir** ("ooo"/"aaa" from her held vowels, WORLD) in
  chorus 1's back half, verse 2, the break, chorus 2, the climb, the finale; quantized octave
  **rises** on both held "long"s with a gong at the next part; the last "out" held and
  chopped-and-screwed into a dissolve (band out from 82, lowpass closing, room gone, her guitar
  bright and dry after the button on her last strum); file ends 0.5 s after the last sound.
- **Kit.** Kick tuned (G#1 body, D#3 box), per-hit velocity/attack/decay, humanized and
  **eager** (−9 ms, +9 %/phrase); synthesized snare + real **toms** (`src/kit/tom-1..4.wav`,
  CC0 Freesound 634272 · 634273 · 808545 · 685559); reverse kicks/snares/toms, pitched toms,
  wub kicks; percussion and hats on HRTF seats; the **gallop** (`src/sfx/gallop.wav`, CC0
  archive.org Red_Library_Animals_Horses_1 / R13-10) one stride per kick through the break
  and bridge, passing right → left. No neigh.
- **Bed.** Sine pads are the main pad (strings at a third), struck on her strum motif in the
  big sections; sister sine + mirror + chorale on an un-ducked melody bus; 12-string octave
  double of her guitar, a quantized guitar climb out of the break and a per-beat pitchy guitar
  through 73–76; pitched **air** (resonators on the bar's chord, an octave up in choruses,
  flexing with her strums) at a trace; turns, shifts, a slow lap across chorus 2, a wobble on
  the climb. The Salamander grand sampler is in the engine behind `PIANO_ON 0` (tried, cut).
- **Master.** `cut.sh` prints through the `vinyl` substrate (`pop/lib/substrate.mjs`): wax
  drive in-engine (sum normalized to 0.5 first), the vinyl EQ/wow/glue chain, then ONLY the
  loudness gain + ceiling — a clipper after the vinyl limiter was eating the master. −12 LUFS.

Gotchas: every hard-wired stem time shifts when the regularizer changes (`SHIFT27`); an edit
to her lead must also hit the derived stems (halo, harm-*, choir-*, sister-*); a `// comment`
appended to a multi-declaration line swallows the rest of it; `out/` grows ~180 MB a version —
purge intermediates, keep mp3s; a NaN anywhere poisons the wax normalization (the engine now
reports the first NaN per bus before the mix).

```sh
node pop/sailor-song/bin/splice.mjs && pop/.venv/bin/python pop/sailor-song/bin/pitch-fx.py && node pop/sailor-song/bin/orchestra.mjs
VERSION=v73 bash pop/sailor-song/bin/bake.sh && pop/.venv/bin/python pop/sailor-song/bin/word-times.py --fa
```

## v104 (2026-10-04 · the second critics' pass, the opening, the drop)

Three more critics (mix · arrangement · pop) read v100 for "a master / pop release"; v101 took what
they measured and jeffrey's ear took the rest through v104. Where it landed:

- **The opening is one guitar path.** The record starts on her strums at bar 10 (`startSecOut =
  bar_n(10)->beats[0] − 0.12`, eight strums before her word). The raw-guitar blend that used to
  brighten the intro is gone before the button (`kRaw` only after bar 84) and the ×7 intro gain is
  ×1 — the twangs and the guitar under her voice are the same sound. No radio, no previews, no
  tape-start. Her verse-1 level eases up over bars 14–20 (`v_her`).
- **The air is born with her word, slowly.** Both the pitched air bed and the wax hiss rise as
  `((t − firstWordT)/12)²` — nothing before she sings, barely there in her first line, under her
  by the second. (v101's linear 8 s read as "it just comes in right away".)
- **The pickup is lazy.** Two beats before each "kiss" (bars 27 and 51) the regular kit, the 16th
  trap hats and the 16th-grid kicks all sit out; what plays is a clap on 3 and a kick on 4,
  ~28 ms *behind* the beat (`hum_lazy`), then her hesitation, then the drop. "A little fast …
  spaced out more … more air there, catch that drop better."
- **No "wrong" exception.** v86's reverse kick + bell frill + her third on "wrong" is deleted —
  "those exceptions feel like bugs now." Her vowel choir, the deep gong on bar 44 and the rise-2
  gong at 68 stay.
- **Critics' pass (v101).** Her register is hers while she sings (`sheSings`: tenor lead 69–79,
  vowel choir from 66, ostinato at ROOT+36, struck-pad octave only when she is out); the quartet
  and aahs enter in chorus 2 / bridge and swell in (`v_orch`), the viola sits 49–55; chorus 2's
  strings swell over 52–56, verse 2's climb over 47–52; verse 2 is stripped so chorus 2 arrives
  (`DANCE[CHORUS2].hbass .9`, `gDrop` floor 0.6→1 over 52–54); her level steps 1.4 → 1.5 → 1.6
  chorus 1 → chorus 2 → bridge; the glitch stop gates every bus and ends 3 ms before the downbeat
  (it was chopping the gong); the bass folds to mono below 500 Hz from bar 81; third/octave sines
  and `H_DOWN8` are out of the chorus harm sets; the fallback kit (`kitOff`) can no longer leak
  after bar 82. `orchestra.mjs`: `sus()` suspends the D♯ voice, pickup strings ÷1.6, no horn
  stab, glock only chorus 2 ≥ 60, timpani/taiko/pizz eager by `EAGER`.
- **Vinyl chain.** The second compressor is 1.5:1 / 30 ms / unity makeup (ffmpeg's `makeup`
  floor is 1, not 0 — v101's first cut died on that). −12.2 LUFS, −2.8 dBFS peak.
- **Video (in flight).** `perf-relight.mjs` relights her room from the record (lamp, string
  lights, window; chord tint; kicks bloom) over a subject matte; round 2 = Apple Vision subject
  lifting for the matte, bouncing-ball singalong lyrics in black-and-white comic lettering with
  mix-coloured shadows, and a VHS pass. `guitar-track.py`, `person-matte.py`, `room-lights.py`,
  `glyph-atlas.py` feed it.

Gotchas this round: a `// comment` appended mid-line swallowed an inline block's closing brace
(again); `sheSings` was used before it was declared; never run two bakes at once.

## Platter reading

[`ANALYSIS.md`](ANALYSIS.md) reads the take against the rhythm, chamber and
bass platters (`bin/platter-analysis.mjs` → `platter-analysis.json`): her
strum is a periodic, unsyncopated necklace the kit currently doubles; her
voice sits 38 ms behind her hand in verse 1 and 5 ms in chorus 2; the take's
loudness peak is the guitar break at 0.79, not a chorus; the melody lives on
5 and b3 and avoids the 2nd; a 1:50 cut runs bars 7–43 + 68–84.

## Next

1. Ear-check the chord cycle and the downbeat (bar phase 1 fits chord changes best).
2. Pick follow or warp.
3. Beat-map drum draft (AC percussion kit) + sub/pad bed on the 2-bar chord cycle.
4. Mix and master (`pop/MASTERING.md`).

Release note: this is a cover. Distribution needs a mechanical licence (for
example DistroKid's cover licensing), and her say on the release.
