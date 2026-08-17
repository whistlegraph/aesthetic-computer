# pop / loner — "whistlegraph loner --- remix"

A remix of **`lonr` — "Loner"**, composed by **Camille Klein
(@cksuperstore)**, the emo whistlegraph. `posts.json` tags **nineteen**
posts with the work — 13.8M views on the biggest — and the whole song is
one sung sentence:

> sitting curled up in myself, i think of a stone,
> just waiting very patiently for time to pass

Where cult got a club record, loner gets a **bedroom ballad**: the voice
close and forward, tape-warm pads, soft-attack bass, brushed-adjacent
percussion that mostly isn't there, and a dotted-eighth delay doing the
work a reverb would. **v2 is the current cut: `out/loner-remix-v2.mp3`.**

| cut | file | what it is |
| --- | ---- | ---------- |
| v1 | `out/loner-remix-v1.mp3` | 2:51. Bedroom ballad, raw takes placed. Kept for reference. |
| v2 | `out/loner-remix-v2.mp3` | v1 through the aesthetivox — every vocal a WORLD render. |
| **v3** | **`out/loner-remix-v3.mp3`** | **2:39. Angelic drum & bass: solo Camille haloed, harped and chopped over a synthesized two-step.** `out/loner-remix.mp3` is this cut. |

## The takes

Four of the nineteen feed v1 (fetched from the assets mirror by
`bin/slice.mjs`; whisper.cpp + per-word `librosa.pyin` receipts in
`harvest.json`):

| prefix | post | what it is |
| ------ | ---- | ---------- |
| `f-` | 7108062006980201771 | **Ten Whistlegraphs / Feral File — the spine.** The whole lyric, one clean unaccompanied voice, ~80 BPM |
| `n-` | 7021262898479549702 | the 13.8M "not again!" take — spoken *"Camille, are you doing emo whistlegraphs again?"*, the sung answer, and *"i knew it"* at the end |
| `o-` | 6988619239657622790 | the origin take — spoken *"Here's a whistlegraph by Camille called loner. Ready?"* |
| `s-` | 6988954628167585030 | a lower-register solo take — sliced, unused in v1 (it sits ~a third below the others and would need repitching) |

No whistling anywhere: a pyin scan of the 500–3000 Hz band over all
seven downloaded takes found nothing. Like cult, loner is sung.

## What the source gave us

The Feral File take sits in **A# minor about +30 cents sharp of A440** —
tonic hits at ~237 Hz against A#3's 233 — so the whole accompaniment
tunes to **TONIC = 237 Hz** and never consults concert pitch. The
melody, per-word (receipt in `harvest.json`):

| words | note | degree |
| ----- | ---- | ------ |
| sitting | F4 | 5 |
| curled up in | C#4 → A#3 | ♭3 → 1 |
| myself | D#4 | 4 |
| i think | G#3 | ♭7, below |
| of a | **A#4** | **the octave — the one loud feeling in the lyric** |
| stone | ~E4 | a blue note drifting between 4 and #4 |
| just waiting | C4 → C#4 | 2 → ♭3 |
| very patiently | A#3 → D#4 | 1 → 4 |
| for time | D#4 → F4 | 4 → 5 |
| to pass | C#4 | ♭3 |

The chords (i / VI / III / VII / iv — Bbm, Gb, Db, Ab, Ebm) were chosen
so every measured word pitch lands as chord colour: "just waiting"'s C4
is the maj7 of Db, "i think"'s G#3 is the 9th of Gb, and "stone"'s drift
always happens over VI, where it reads as 6th/♭7 and never fights the
tonic.

## v1 — the form

56 bars at **80 BPM** (the take's own tempo), 2:48 + tail:

| act | at | bars | what happens |
| --- | -- | ---- | ------------ |
| I · TAPE | 0:00 | 0–4 | hiss, a low drone, and the spoken introduction |
| II · VERSE | 0:12 | 4–14 | the Feral File take at its own rubato — bass and pad only; a hat tick doesn't dare enter until bar 9 |
| III · DRIFT | 0:42 | 14–18 | instrumental breath; "stone" echoes dark from across the room; a music box answers |
| IV · VERSE AGAIN | 0:54 | 18–28 | the "not again!" take sings the same sentence — its spoken question opens the act, tucked low — over a soft thump |
| V · OF A STONE | 1:24 | 28–36 | the lifted middle: both takes' octave leap answer each other over the warmest pads, a shimmer an octave up |
| VI · THINNING | 1:48 | 36–44 | the back half of the sentence returns while the room empties, one element per bar |
| VII · BREATHE | 2:12 | 44–53 | **unaccompanied.** The whole original take, alone with the hiss. Then, spoken: *"i knew it."* |
| VIII · OUT | 2:39 | 53–56 | the delay finishes the sentence |

Sound design, in one paragraph: the pads are detuned sine pairs under a
one-pole lid with a shared **wow** LFO (±4 cents at 0.38 Hz — cassette
transport, and it's one LFO so the whole bed leans together); the bass
is cult's sine-bump with the attack opened to 45 ms; the **thump** is
cult's kick with everything punchy removed (no transient blips, drive
1.3, a felt-mallet 190 Hz touch); the brushes are band-passed noise that
swells *into* downbeats without a snare anywhere; the hiss is a mix
element that breathes with the sections, loudest when the music is gone.
Sidechain depth is **0.18** — a breath, nothing pumps. Every cult mixing
rule that still applies survives: raised-cosine tails everywhere, no
master tanh, one linear trim, mono-safe pans with a band-limited
antisymmetric side return.

## Run it

```bash
node pop/loner/bin/slice.mjs            # fetch takes + cut the bank (20 slices)
node pop/loner/bin/render.mjs           # score → out/loner-remix-v1-full.wav
bash pop/loner/bin/cut-v1.sh            # measure → one static dB → limit → mp3
node pop/loner/bin/render.mjs --stems   # per-bus stems, for balance checks
```

Analysis (needs `pop/.venv` — see pop/cult/README.md for the recipe):

```bash
pop/.venv/bin/python pop/loner/bin/survey.py    # first look at all takes
pop/.venv/bin/python pop/loner/bin/analyze.py   # per-word pyin → harvest.json
```

`cut-v1.sh` follows the cult mastering law — MEASURE → one static dB →
true-peak limiter, never a second loudnorm — with one deliberate
difference: the target is **−16 LUFS, not −14**. A ballad keeps its
dynamics; the render measured −15.1 raw, so mastering is a −0.77 dB trim
and a limiter that has almost nothing to do.

## v2 — the aesthetivox

The rule, established on the cult lane the same day (memory
`feedback_vocals_always_aesthetivox`): **vocals never skip the
aesthetivox** — an exposed unprocessed take is too raw to ship. v1
deliberately featured raw takes; v2 keeps v1's entire arrangement and
fixes exactly that. `bin/aesthetivox.py` puts all sixteen slices the
score plays through the WORLD chain (harvest → stonemask → cheaptrick →
d4c → resynthesize), and `bin/render2.mjs` is v1's score reading `vox/`
instead of `samples/`.

Because loner's charm is the rubato, the sung takes are **not** re-scored
to a melody the way `cult/bin/sing.py` does it — they get the
`pop_world_autotune` move instead: the f0 contour is pulled toward the
nearest scale tone at **strength 0.7**, and the *correction* (never the
pitch) is smoothed over 45 ms, so slides stay slides and vibrato keeps
its shape while the centre of every held tone comes home. Plus
`longdots.sh`'s ballad-gentle **+1.6 dB** singer's formant (not sing.py's
3.2 default). Consonants stay real: f0 runs continuous through unvoiced
gaps into the synth, and the original take's unvoiced audio is
composited back over 5 ms seams — pitchsnap_world's two survival tricks.

**The tuning decision** (the brief's option b): the band keeps
**TONIC = 237 Hz** and the +30¢ offset lives in the aesthetivox targets —
the correction grid is A# natural minor *in the take's own frame*, never
A440's. Rationale: the band already tunes to Camille; correcting her
toward her own centre changes her the least.

The spoken asides survive only as treated material: range compressed
toward the median (^0.55), dropped a semitone, envelope darkened above
~3.5 kHz — still them, no longer raw speech. BREATHE keeps its
nakedness: the naked thing is now the WORLD render of the whole take.

Receipt: `vox/.manifest.json` — per slice, mode and pyin median
|cents-to-grid| before → after. The Feral phrases read ~10¢ off-grid
raw → on-grid after; the "not again!" phrases ~20–30¢ → ~10¢ (strength
0.7 leaves a residual on purpose; pyin's 10¢ resolution quantizes the
readout).

```bash
pop/.venv/bin/python pop/loner/bin/aesthetivox.py   # samples/ → vox/ (16 WORLD renders)
node pop/loner/bin/render2.mjs                      # → out/loner-remix-v2-full.wav
bash pop/loner/bin/cut-v2.sh                        # → out/loner-remix-v2.mp3
```

## v3 — angelic drum & bass

@jeffrey, in order: *"for camilles we need to like make her vocals more
angelic"* · *"more arpeggiating within them"* · *"no alex voice — this
track is all solo camille"* · *"i want a kick in it — sitting kick ick
waiting kick cik"* · *"lets start splitting it up — making it more
almost like a drum and bass"*. All five, in one cut, with v2's rules
intact: every vocal object is a WORLD render (`bin/halo.py`, receipt
`vox3/.manifest.json`), tuning stays TONIC = 237 Hz.

- **The halo.** Behind every lead, the same slice doubled an octave up —
  f0 × 2 with the cheaptrick envelope untouched, so it is her head
  voice, not a chipmunk — two copies at +6/−7 ¢, 28/41 ms late, darker
  (5.5 kHz tilt), 1.5× breathier, **vowels-only** (consonants muted so
  the stack can't smear), panned ±0.55, reverb send 0.55–0.75. Diatonic
  3rd/5th renders swell in on the held phrase-ends ("myself…",
  "stone…", "pass…") with 0.5–0.8 s attacks — the interval computed
  per frame from her corrected contour, so the harmony carries her
  rubato. Leads get AIR (+2.5 dB above 8 kHz in the envelope) and
  BREATH (aperiodicity +0.14 in the same band, ramped in 150 ms into
  each voiced run — held vowels only, never consonants).
- **The harp.** Vowel excerpts ("oh" from *stone*, "ah" from *pass*)
  re-sung flat at grid tones st +12…+27 (12 ¢ / 5.2 Hz vibrato), run
  4–7 notes at 0.12–0.30 s in chord-tone shapes — `dotArp()`'s move
  from cult render10, built from WORLD notes of Camille.
- **Solo Camille.** The origin/ensemble takes and all three spoken
  asides are gone.
- **The kick between her syllables.** In the verse the kicks sit at the
  harvest's word boundaries — sitting·KICK·ick, waiting·KICK·cik — and
  the "ick"s are real: 8 tiny chops of her WORLD leads (forward and
  reversed) bounce off the kicks as percussion.
- **The two-step.** Drums think 160 over the 80 BPM floor: kick steps
  0·10 (+6 every 4th bar), snare 4·12 with ghosts and a roll into every
  4th bar of drop 2, velocity-shaped 16th ticks, a 12 ms-attack sub on
  steps 0/6/10 with a fifth pickup. All synthesized — no sample packs.
- **The space.** The vox bus gets a decorrelated Schroeder pair: 4
  combs per side (44.6–54.2 ms, right offsets +23…+37 samples), RT60
  ≈ 3.2 s with a 3.4 kHz one-pole damp inside the loop, two series
  allpasses per side (g 0.7), 40 ms pre-delay, 180 Hz high-passed
  return at 0.34 — under the lead, hotter under the halos. Worst-window
  mono fold-down is −1.05 dB, and it lands on the intro, where the halo
  is deliberately all side.

Form: INTRO (halo with no lead inside it) → VERSE (syllable kicks) →
BUILD → DROP 1 → BREATHE (the whole take, **naked but haloed**) →
REBUILD → DROP 2 → OUT. 52 bars, 2:39.

```bash
pop/.venv/bin/python pop/loner/bin/halo.py   # samples/ → vox3/ (66 WORLD renders)
node pop/loner/bin/render3.mjs               # → out/loner-remix-v3-full.wav
bash pop/loner/bin/cut-v3.sh                 # → out/loner-remix-v3.mp3
```

## v4 — regulated, immediate (WIP)

@jeffrey, in order (2026-08-17): *"i want the lyrics starting right
away"* · *"and more musical notes"* · *"some words directly seem to
just cut off"* · *"we need a smoother time stretching so we dont cut
off her words"* · *"i wish our musical notes were the same as her
voice"* · *"we could give her a backup vocal to match the words"* ·
*"i guess i want that world snapping for camille / that regulation"*.
Backup voice decision: **Camille herself** — the solo rule holds.

Six moves, one cut:

1. **NOW.** No 12-second overture. Her first word lands inside bar 0 —
   at most a half-bar of hiss/drone as a pickup breath. Everything v3
   put in INTRO (the empty halo, the forming arp) happens *under* the
   first phrase instead of before it.
2. **THE REGULATION.** The aesthetivox snap comes up from 0.7 to ~0.92
   on the sung leads: her notes become NOTES — dead on the A#-minor
   grid in her own 237 Hz frame — while the 45 ms correction smoothing
   still lets slides be slides. Her rubato survives in *time*; the
   drift goes away in *pitch*.
3. **HER MELODY IS THE MELODY.** A plucked lead (music-box register)
   doubles the harvest's per-word pitches — F4 *sitting*, C#4→A#3
   *curled up in*, D#4 *myself*, G#3 *i think*, A#4 *of a*, the E4
   stone-drift, C4→C#4 *just waiting*, A#3→D#4 *very patiently*,
   D#4→F4 *for time*, C#4 *to pass* — in sync under her words, and
   answers with the same line in the instrumental gaps. The arps
   re-voice toward melody tones. More notes everywhere, and every one
   of them is hers.
4. **NO WORD LEFT CUT.** The halo2 bank finally gets played: the held
   words that v3's slices clipped resolve into the `*-long` family
   (sing.py's frame-axis warp — 1:1 through the onset, the vowel
   absorbing the hold for seconds, shimmer + late vibrato, 1:1 release)
   instead of a 10 ms fade mid-phoneme. Any slice whose last word still
   rings past its boundary gets re-cut with a real tail.
5. **THE BACKUP SINGER IS ALSO CAMILLE.** Full-word WORLD renders (not
   vowels-only) at the diatonic 3rd/5th below, singing the whole lyric
   with her, tucked −10 dB under the lead, darker, wetter — a backing
   line that *matches the words*, where v3's swells only matched the
   vowels. The ens-* crowd swells rise behind the drops.
6. **SMOOTHER STRETCH.** Wherever a word must fit a slot, the stretch
   is WORLD frame-warp (phonemes lengthen from inside), never a
   truncation, never a granular smear.

Then the frame changed mid-carve — *"i wanna go from bedroom ballad to
nice dance track"* · *"keep a pretty strict beat that the lyrics can
now regulate around, with a kick and stuff, sidechained into the
lyrics"* · *"i want this all to be in c code"* — so v4 became the
lane's first C-native score AND its dance cut, in one move:

- **The chart.** 122 BPM, four on the floor. `bin/halo3.py` warps every
  charted phrase per-word onto the beat grid (voiced frames absorb the
  stretch at weight 1.0, consonants ride near 1:1 at 0.18 — the smooth
  stretching; the slice tail always plays 1:1 after the last word, so
  nothing ends at a fade). Stretch past 1.8× flattens to the unit's
  grid tone with vibrato fading in — "stone" and "pass" hold whole
  bars. Snap is 0.92: the regulation.
- **The bank.** 9 phrases × 5 renders each in `vox4/`: lead, the
  octave halo pair, and the full-word low-3rd/low-5th backup — Camille
  singing backup for Camille, words and all, from the same warp so
  every layer locks sample-for-sample.
- **The C engine.** `c/lonerremix.c` (single file, fleet-standard) is
  the score. `halo3.py` emits `c/loner-chart.h` — per word, its beat
  slot and measured semitone — and the engine's pluck plays ONLY chart
  notes: the band's melody is her melody, doubling under her and
  answering in the gaps. Kick every beat sidechained into the lyrics
  (vox ducks 0.34, bed 0.52, drums never); clap 2 & 4; offbeat air-hat;
  16th ticks; offbeat house sub. v3's dub delay and Schroeder pair
  ported intact.

Form, 76 bars at 122 (2:35 incl. tail):

| act | at | bars | what happens |
| --- | -- | ---- | ------------ |
| V1 | 0:00 | 0–16 | her first word ON beat 0 of bar 0; pluck doubles, then answers up an octave |
| HOOK | 0:31 | 16–24 | of-a-stone + hk unison + stone-long canon; ens crowd |
| V2 | 0:47 | 24–32 | the "not again!" phrases; backup 3rds enter |
| BREAK | 1:03 | 32–40 | kick out — the naked regulated line, stone-long-echo, riser |
| DROP | 1:18 | 40–60 | everything: both backups, harps every 2 bars, stone-long-17 crown |
| OUT | 1:58 | 60–76 | the pluck alone finishes the sentence; pass-long ghost |

```bash
pop/.venv/bin/python pop/loner/bin/halo3.py   # vox4/ bank + c/loner-chart.h
bash pop/loner/c/build.sh                     # cc → c/lonerremix
pop/loner/c/lonerremix                        # → out/loner-remix-v4-full.wav (~1.3 s)
bash pop/loner/c/cut-v4.sh                    # → out/loner-remix-v4.mp3 (−14 LUFS)
```

## Measured

| check | v1 | v2 | v3 |
| ----- | -- | -- | -- |
| duration | 171.2 s (2:51 incl. tail) | 171.2 s | 159.2 s (2:39) |
| integrated loudness | **−15.9 LUFS** | **−15.9 LUFS** | **−15.9 LUFS** |
| true peak | **−1.5 dBFS** | −1.5 dBFS | −1.5 dBFS |
| loudness range | 10.9 LU | 12.2 LU | 14.7 LU |
| max sample | 0.840 | 0.840 | 0.840 |
| max sample-to-sample step | 0.221 (threshold 0.35) | 0.226 | 0.285 |
| mono fold-down | −0.02 dB | −0.02 dB | −0.10 dB (worst 2 s window −1.05, the all-side intro) |
| bus balance | vox −11.1 · music −17.6 · drums −23.4 LUFS (verse 2 + lift) — the voice ~6.5 dB proud | unchanged score | vox −12.8 · music −19.3 · drums −19.2 (drop 1) |

## Not in v1

- **`s-whole-line`** and **`o-whole-line`** (the ensemble performance
  with Jeffrey and Alex) are sliced and measured but unplayed — both sit
  in a different register and want the speech-to-singing pipeline
  (`pop/cult/bin/sing.py`) or careful repitching before they can join.
  A later cut could end on the three of them.
- No sung-note synthesis at all: v1 is the real takes, placed. The
  WORLD/Saitou machinery stays in the drawer until the arrangement
  earns it.
