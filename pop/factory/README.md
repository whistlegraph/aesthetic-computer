# pop / factory — "whistlegraph factory --- remix"

A remix of **`fact` — "factory 🏭 cookie-cutter🎄personalities"**
(Whistlegraph, 2021-02-04, 50.8M views). The whistlegraph index tags
**twenty-one** posts with the `fact` work; three feed this lane:

| take | post | what it is |
| ---- | ---- | ---------- |
| a | `6925546179275099397` | the original, 50.8M views — and the only take with the count-in ("a one, two, ready, and…") |
| b | `6928682624529485062` | 2021-02-13 — darker and slower; "cutter" lands on B2, the bird runs long and low on G#2 |
| c | `7030651123325308165` | 2021-11-15 — brighter and quicker; "factory" on C#4 |

The poem, whole:

> factory / cookie cutter / personalities
> we must break free from the states that we're in
> spinning away, I hear a bird

## The direction, and why

**The title is an instruction.** A cookie cutter's whole job is to make
identical copies, so the record's central move is a sampler doing exactly
that: the same take of the same line stamped onto the grid again and again
with zero humanization — no velocity spread, no timing jitter, nothing.
Industrial machine techno where the repetition is not a style choice but
the subject. And the poem already carries the whole arc, so the
arrangement only had to obey it: the copies are stamped in spec (line 1),
then the middle line comes true and they drift *out* of spec, and the last
line is the escape. Nothing was invented, only obeyed.

That call was made after measuring, not before. The analysis said the
material wanted it:

- **100 BPM is the chant's own tempo.** Median syllable IOI 0.299 s =
  eighth notes at 100.3; `librosa.beat_track` reads 101.4 / 100.4 on the
  two 2021-02 takes. Each poem line spans ~2 bars at 100 — line 2 is
  4.80 s against a 4.80 s two-bar cell. The take is already ON this grid,
  so every stamp is a raw slice at natural speed and nothing is dragged
  to fit.
- **D minor is the chant's own key.** The pitch set is D·F·G·A·Bb·C and
  it sits hard on D3 (~147 Hz): *factory*, *cookie*, *spinning*, *I* all
  land there. "bird" lands on Bb2 — and take b's long bird on **G#2, a
  note the factory's key never contained**, which is why it gets the last
  word.
- The word grid inside line 1 (word starts at beats 0 · 1.5 · 2.5 · 3.5)
  is the take's own syllable spacing, measured, not designed.

`harvest.json` is the receipt: word timestamps for all three takes
(whisper.cpp ggml-small, word-level), per-slice pyin f0, both tempo
probes. **Those word timestamps are not word timestamps** — see v3's
alignment audit below, which is the first thing in this lane to check
them against the audio.

There are three versions in `out/`. **v3 is the current one.**

| cut | file | what it is |
| --- | ---- | ---------- |
| v1 | `out/factory-remix-v1.mp3` | 3:34 at 100 BPM. Raw stamps on principle. Kept for reference. |
| v2 | `out/factory-remix-v2.mp3` | 3:58 at 90. Every vocal machine-pressed through the aesthetivox; six dies; the stamps breathe. |
| **v3** | **`out/factory-remix-v3.mp3`** | **2:40 at 100, D minor, scored in C. The words are ON THE BEAT — every boundary pinned to a measured sung event, and the machine plays only the melody she sang.** |

---

# v3 — "the regulation"

v1 stamped raw slices and v2 pressed them through the aesthetivox, but
neither one ever put a word on a **beat**: each line went down as one
die-block at the take's own internal word offsets, in seconds. That is an
approximation, and the title is an instruction — a cookie cutter does not
approximate. v3 warps every word of the unbroken chant onto a 100 BPM
chart derived from the take's own rhythm, snaps its pitch to D natural
minor in her own frame, and hands the C engine a generated header so the
press, the belt and the pluck all read the same melody she does.

The lane's first C-native score (`c/factoryremix.c`, fleet single-file
convention, renders 2:40 in 2.4 s). `MINIMAL=1` renders the press+vocals
study the alignment was tuned against.

## The alignment audit — every transcriber was wrong somewhere

This is the part that took the day, and it is the reason `bin/audit.py`
exists as a script the lane keeps rather than a probe someone ran once.
**Word boundaries must be checked against actual amplitude and pitch
events in the audio, not taken on trust from any transcriber.**

**whisper.cpp ggml-small at `-ml 1` does not return words.** It returns
sub-word tokens, and the proof was sitting in this lane's own
`harvest.json` the whole time:

```
' Spin' @13.85   'ning' @14.45   ' away' @15.28
```

"Spinning", split at its /n/. Worse, `CUTS` in `harvest.py` was built on
those numbers, so `samples/line3.wav` and `samples/spinning.wav` are cut
at **13.85 — 0.66 s after she starts singing the word.** The take is
silent from 12.37 to 13.19 and "spinning" begins at 13.19; both slices
are missing their first syllable, which is why line3 measures the word on
G3 while the whole take measures it on E3.

**OpenAI whisper-1 hears real words and still gets the edges wrong.**
`bin/align.py` re-transcribed ten slices with word timestamps. On the
unbroken take it returned 21 units for 20 words: it could not segment the
35 ms /f/ of **"factory" and gave it back as two words, "to read."**
Running its raw spans through the audit produced **19 contamination
flags**, of which the two that matter are:

| flag | what it is |
| ---- | ---------- |
| `we're: SLICES event 23 (0.42 s in, 0.17 s left outside)` · `in: no event` | **"we're" swallows "in" whole.** She sings *that · we're · in* as a descending A3–G3–F3; whisper-1 gave "we're" 0.68 s covering both notes and started "in" 0.43 s late, on silence. This is the same failure as the loner lane's "for" swallowing "time". |
| `bird: SLICES event 33 (0.39 s in, 0.55 s left outside)` | whisper-1 ends "bird" at 15.26 while she sings it until 15.805 — **half a second cut off the poem's last word**, the one the whole record walks toward. Only starts come from a transcript, because every other word's end is its neighbour's start; the final word has no neighbour, so it keeps whatever the transcriber said. It needed its own pin. |

**Counting is what catches it.** The audit segments the take into sung
events — loud AND voiced runs, cut again wherever the de-spiked pitch
steps a whole tone or more and the new plateau HOLDS — and compares:

| | count |
| --- | --- |
| poem words | 20 |
| poem syllables | 30 |
| sung events detected | **34** |
| octave-class tracking errors folded before counting | 10 |

The four events above the syllable count are not detector noise; they are
**the whistlegraph**. She flips an octave inside a single syllable
constantly: *factory* ends on D4, *cookie* on C4, *from* drops D4→Bb2,
*a* and *bird* both flip Bb3→Bb2. A first pass without the plateau-hold
test and the merge-back returned 39–40 events, because vibrato reads as a
note change; requiring the new pitch to settle, then merging any two
neighbours within a semitone, brought it to 34, and every one of those 34
maps onto a syllable or a flip.

So all 20 starts and the last end are **pinned from the events**, and the
manifest records where each number came from — `whisper.cpp`,
`whisper-1`, `whisper-1+merged`, or `pinned (event; whisper-1 was
+425 ms)`. After pinning, the audit on the final source spans reports
**✓ no contamination: every unit holds whole events only**, and it runs as
part of every build so a bad boundary announces itself instead of waiting
to be heard.

The largest drifts it corrected:

| word | whisper-1 was | why it mattered |
| ---- | ------------- | --------------- |
| `in` | 425 ms late | "we're" was singing it |
| `bird` (end) | 545 ms early | the last word, truncated |
| `we` | 300 ms early | started inside "personalitie**s**" |
| `a` | 300 ms early | started in the gap before it |
| `cook` | 110 ms late | opened after her vowel |

### The two ways loner shipped a bad boundary, checked here

The loner lane's contamination had two causes, and both are things a build
can do to a boundary *after* it has been measured correctly. Both were
checked on this lane by measurement, not by reading the code:

| mode | factory | why |
| ---- | ------- | --- |
| **the snapper drags a hand-measured pin**, turning a measurement back into a guess | **absent** — 25/25 units pinned, **0 snaps applied** | `snap_boundaries` takes the pinned set and returns early when every boundary is pinned. It exists for phrases that are still trusting a transcriber; here it never runs. |
| **the attack pre-roll borrows a neighbour's voice** instead of only the silence in front of the word | **absent** — 0 units borrow, and every runway (0–150 ms) is silence or the word's own consonant | The 30 ms walk-back is clamped at the previous unit's end *after* the energy trim, and the trim only ever moves an end back to where that word's audio stopped. Verified structurally too: widening the pre-roll to 220 ms still audits clean, because the clamp — not the constant — bounds the walk. |

The second one exposed a **hole in the audit itself**, which is the more
useful finding. `PARTIAL_S`, the tolerance that stops a boundary landing a
frame or two off from crying wolf, was **40 ms** — wider than the 30 ms
pre-roll it was supposed to police. A tolerance wider than the thing it
polices is not a tolerance, it is a blind spot: a pre-roll that *had*
reached into the previous word's voice would have been waved through by
the very check meant to catch it. Forgiven slivers are now still counted
and reported as a **BORROW**, and the check is tested against three
hand-built spans for the word `in`:

```
correct  (starts at its own event)  →  ✓ clean
borrow   (25 ms back into "we're")  →  in: BORROWS 25 ms of event 22
slice    (whisper-1's real 0.43 s)  →  in: SLICES event 23 (0.17s in, 0.42s left outside)
```

### One detector, not two

`align.py` grew its own copy of the event finder with looser thresholds,
and the two drifted: its receipt claimed **40** events for the unbroken
take while the audit the build actually runs found **34**. A receipt that
disagrees with the build is worse than no receipt. The detector now lives
only in `audit.py`; `align.py` and `chart.py` both import it, and
`align.py --audit-only` re-counts against the saved transcription without
spending an API call — which matters, because re-transcribing would hand
back slightly different times and quietly invalidate every boundary the
chart has pinned against these ones.

## Her D is 148.73 Hz, not 147

`harvest.json` records `chant_root_hz` 147.0 and the notes above round it
to equal-tempered D3. Both are medians over **every** voiced frame of the
take — glides, octave-tracking errors and the octave-up words included.
Converging a ±60 ¢ window onto the **sustained** frames only (pitch flat
within 0.45 st across 90 ms, 202 frames) puts her D at **148.73 Hz, 22
cents above ET D3**, and with that as the frame her stable pitch classes
come out **D 38% · Bb 22% · C 17% · F 9%** — exactly the D-minor spine
described above — at 25 ¢ median deviation from the grid. The tonic is
hers, not the piano's, the way the loner lane used Camille's 237 Hz
rather than A#3.

## The chart

Her syllables are eighths at 100 (median IOI 0.299 s), so the poem gets a
machine grid and each line gets its bars:

| bar | what happens |
| --- | ------------ |
| **0** | `fac`(0) `to`(0.5) `ry`(1) · `cook`(1.5) `ie`(2) · `cut`(2.5) `ter`(3) · `personalities`(3.5) — seven syllables, seven eighths, exactly the measured 0 · 1.5 · 2.5 · 3.5 word grid |
| **1** | the `personalities` drone holds to 6.5, then her breath |
| **2** | `we`(8) `must`(9) `break`(10.5) `free`(11.5) |
| **3** | `from`(12.5) `the`(13.5) `states`(14) `that`(15) `we're`(15.5) |
| **4** | **`in`** — the line lands held on the downbeat (16–18), then the gap |
| **5** | `spin`(20) `ning`(21) `away`(22) |
| **6** | `i`(24) `hear`(26) |
| **7** | `a`(27.5) `bird`(29 → 31.5) |

Four words are split at their event boundary so each syllable gets its own
note, slot and beat — `factory`, `cookie`, `cutter`, `spinning`, the ones
where each syllable owns a flip. **Not** `from`, `a` or `bird`, which flip
inside ONE syllable and would be broken in half by a split.

The melody the chart hands the C engine, and therefore the pluck:

```
bar 0   C3  D3  D4  ·  D3  C4  ·  A#3 C4  ·  D3————
bar 2   C3  A3  D3  D3
bar 3   A#2 C4  A3  A3  G3
bar 4   F3————
bar 5   E3  G3  D3————
bar 6   A3————  D4——
bar 7   A#3——   A#2————
```

No accidentals: D · E · F · G · A · Bb · C. She touches C# three times and
F# once, and the chart snaps those **in cents, not by rounding
semitones** — "fac" measures 138.6 Hz, which rounds to C#3 but is 79 ¢
from C3 and 114 ¢ from D3, so it goes to C. And the note of a unit is the
note of the event it RESTS on, its longest — which puts the lane's own
documented fact back on the page: **`bird` measures Bb2.**

## Stretch, per unit

Nothing is squeezed and nothing is synthesised. The stretch below is the
**vowel's** stretch — output frames landing on voiced source over voiced
source frames — because raw block ratios lie about any unit with silence
in it (`in` reads 0.99× on frames while its 0.59 s of singing is spread
across 1.2 s).

| | | | | |
| --- | --- | --- | --- | --- |
| fac 1.02 | to 1.04 | ry 1.20 | cook 0.82 | ie 0.86 |
| cut 0.98 | ter 0.86 | personalities 1.01 | we 1.11 | must 1.21 |
| break 1.05 | free 1.32 | from 0.99 | the 0.92 | states 1.12 |
| that 0.88 | we're 0.89 | in 1.10 | spin 1.19 | ning 1.54 |
| away 1.10 | i 1.22 | hear 0.98 | a 1.55 | **bird 1.34** |

Range 0.82–1.55×, so THE HOLD (flatten past 1.8× to a grid tone with
vibrato) never fires — every word in the study is her actual voice.

## What sounded wrong, and the measured cause

- **"ie" was pitched to D#3, nine semitones off.** A median across the
  whole unit span, which is 150 ms of C4 sitting behind a 145 ms /k/
  closure. Fixed by measuring the note over the unit's longest EVENT.
- **"cook" was squashed to 0.67×.** The syllable split was at the middle
  of the /k/ closure, which gave "cook" 75 ms of silence to stretch. The
  /k/ of *cookie* belongs to the second syllable (koo-kie); moving the cut
  to the start of the closure at 1.195 s put it in front of "ie" as its
  consonant runway, where a singer puts it, and both syllables came back
  to 0.82–0.86×.
- **The bank loader ate the lead-in.** The fleet's `bank_load` hunts for
  the first sample above 0.008 and starts there — right for a one-shot,
  catastrophic here, because `leadIn` is measured in frames from sample
  zero and shaving the quiet head of the /f/ slides the whole poem off the
  grid. This engine's loader does not trim.
- **The study video ran seven seconds past the end.** `minimal_bars` was
  the loner value (`ceil(beats/4)+2`); the poem is 31.5 beats, so rounding
  up to the bar line and stopping is enough.
- **Two emoji rendered as tofu.** The work's title carries 🏭 and 🎄 and
  Helvetica has neither, so the credit line showed two empty boxes in the
  one place that names the source. The video credit spells it plainly.

## Measured — v3

| check | value |
| ----- | ----- |
| audit on the final source spans | **✓ 0 flags** (19 on whisper-1's raw spans) |
| events · syllables · words | 34 · 30 · 20 |
| units holding one whole event · several whole · a fraction | 18 · 7 · **0** |
| pins dragged by the boundary snapper | **0 of 25** |
| pre-rolls borrowing a neighbour's voice | **0 of 25** |
| press vs the beat grid, in the study mix | **0.0 ms median · 0.0 ms max** |
| sung event vs its beat | −55 ms median (the attack leans in) |
| loudest moment vs its beat | +90 ms median — **the beat falls inside the attack**, which is where a singer puts it. The anticipation cap is 90 ms, so a word peaking later than that lands late by design rather than dragging into its neighbour. |
| pitch vs the D-minor grid | raw take **32 ¢** → regulated render **16 ¢** |
| render f0 vs its own chart note | 10 ¢ median |
| warble (frames jumping >1 st) | raw take **0.132** → render **0.121** · halo 0.093 · low-3rd 0.105 |
| trims (trailing silence dropped) | `in` −200 ms · `hear` −110 ms |
| study | 8 bars · 21.2 s · −0.6 dBFS true peak |
| v3 cut | 159.6 s scored in 2.4 s · measured −17.2 LUFS → +3.28 dB static |

On warble: v2's post-mortem set a 0.12 retry threshold, but that number
was calibrated on per-word renders of a different kind of material. Here
the **source** measures 0.132 by the same metric, because a whistle flip
IS a >1 semitone frame jump. The press did not add warble; it took a
little out.

## Run it — v3

```bash
cd pop/factory
OPENAI_API_KEY=... ../.venv/bin/python bin/align.py   # whisper-1 words → samples/.align.json
../.venv/bin/python bin/audit.py chant-full           # the events, listed
../.venv/bin/python bin/chart.py                      # the bank + chart + c/factory-chart.h
bash c/build.sh && c/factoryremix                     # → out/factory-remix-v3-full.wav
bash c/cut-v3.sh                                      # master once → out/factory-remix-v3.mp3
```

…or, for the tuning loop — change a beat in `chart.py`'s `CHART`, run
this, watch it:

```bash
bash bin/study.sh          # FULL=1 rebuilds the whole bank
                           # → ~/Desktop/factory-kickvox-timeline.mp4 + .mp3
```

`bin/study.sh` re-renders only the lead take, reads the WORLD analysis off
disk, runs the audit, builds the engine, renders the study, draws the
scrolling piano-roll video (`bin/timeline.py` — 60 fps, sub-pixel scroll,
the real waveform inside every block so a bad boundary is visible without
listening) and copies both to the Desktop. About two minutes end to end.

---

# v2 — "the pressed voice"

The rule (established on the cult lane): **vocals never skip the
aesthetivox** — an exposed unprocessed take is too raw to ship. v1
stamped raw slices on principle; v2 keeps the principle and applies it
to the voice itself: every vocal in the score is a WORLD resynthesis
from `bin/aesthetivox.py` (loner's chain, pressed harder).

## The glitch post-mortem

The first pressing of v2 shipped with "bad glitchy aesthetivox", and the
cause was measured, not guessed: **these chant hits glide** — p95 pitch
velocity 1–3 semitones per 5 ms frame — so a per-frame nearest-tone
target flapped between adjacent scale tones **24–71 times per word**
(swings >60 c/frame, max ~187 c), and at strength 0.95 through 18 ms of
smoothing that wrote square-wave FM straight into WORLD's f0. On top,
`harvest` at f0_floor 100 dropped 1–3 octave-class tracking errors per
word into the track. The repair presses just as hard, but presses a
**stable reading** of the pitch:

1. the f0 track is made continuous and **de-spiked** (frames >6 st off
   their 45 ms median are tracking errors, not performance);
2. the target tone comes from a 35 ms median-smoothed contour **with
   hysteresis** — it may only change once the pitch has arrived within
   45 cents of the new tone. No flapping;
3. correction (clamped ±250 c) applies to the de-spiked contour and is
   smoothed 30 ms; micro-detail rides on top untouched;
4. consonant composite seams widened 5 → 15 ms — and slice tails now
   follow the decay to its natural end (`follow_decay` in harvest.py:
   "the vocals keep cutting off" was the old 10 % RMS end-trim
   amputating vowels at −20 dB);
5. every corrected render is **warble-checked** (fraction of pyin frames
   jumping >1 st); anything over 0.12 retries at strength 0.72, then
   0.55, then ships as plain de-spiked resynthesis — treated, never
   corrected into artifacts. Fallbacks land in `vox/.manifest.json`:
   currently `we`, `factory-b`, `personalities-e`, `line3-e`,
   `factory-b.d5` (plain) and `cutter`, `cutter.d4`, `factory-e`
   (reduced strength).

Result: corrected-render warble fell from 0.13–0.50 to ≤0.11, and the
stamp act in the finished mix measures 0.075.

## What v2 is now

- **90 BPM** — "should be slower". The chant measured ~100; at 90 each
  2-bar cell is 5.33 s against line spans of 4.2–5.0 s, so every
  pressing gets real air. Nothing is stretched: each line is stamped as
  one die-block at the take's own internal word offsets, in seconds.
- **Six dies** — "lets also navigate more takes". `bin/survey.py`
  transcribed and probed the other eighteen `fact` posts
  (`survey.json`); the score now rotates **a** (the original), **d**
  (the 6.7M bright pressing) and **e** (the low 2024 one) through THE
  STAMP; FULL SPEC's second cell swaps in d's line 2 and e's line 3;
  OUT OF SPEC's wrong dies are **b**, **f** (2026 "cookay cubber", B4
  register) and **c**; the follow-along take contributes a spoken
  "here's the factory" (POWER-ON), a 3.5 s stretched "spinning" and a
  chain of birds (SPINNING AWAY); and the 58 s talk supplies the
  intercom line that lands at the turn: *"it has a score and it can be
  performed the same way every time."*
- **Pressed voice** — stamps and lines snap to D natural minor on the
  measured 146.83 Hz tonic at strength 0.93 (0.75 when harvest voicing
  is thin); the other dies press against the **chromatic** grid at
  their own pitches; OUT OF SPEC copy *k* plays renders whose
  correction grid is detuned ±(k·9) cents (`vox/<word>.d<k>.wav`) —
  the targets out of calibration, not the takes back to raw. Count-in
  and intercoms get loner's treated-speech move; the final bird is
  range-compressed and darkened with its centre **locked to G#2**,
  untransposed — treated, still outside the factory's key.
- **Thinner backbeat** — "the snares / claps are too much": one clap
  per bar, backbeat only, at ~0.6× the old gain; the conveyor ticks
  are the timekeeper.

## Measured — v2

| check | value |
| ----- | ----- |
| duration | 237.9 s (3:58 incl. tail) |
| integrated loudness | **−14.2 LUFS** |
| true peak | **−1.8 dBFS** |
| loudness range | 7.0 LU |
| max sample | 0.780 (limiter ceiling; 0 full-scale runs) |
| max 2 kHz-band step | **0.125** (fault line ~0.26; raw 0.52 is the ticks) |
| mono fold-down | −0.09 dB |
| stamp-act vox warble | **0.075** (the glitchy renders were 0.13–0.50) |
| a-die cycles vs D-minor grid | **8.7 ¢** median (residue = cutter's reduced press) |
| other-die cycles vs chromatic grid | **1.3 ¢** median |
| bus balance (stems, LUFS) | vox −13.4 · drums −15.9 · music −18.6 · mach −32.4 |

## Run it — v2

```bash
cd pop/factory
../.venv/bin/python bin/survey.py        # the other 18 takes → survey.json
../.venv/bin/python bin/harvest.py       # fetch + measure + slice → samples/, harvest.json
../.venv/bin/python bin/aesthetivox.py   # the press → vox/, .manifest.json
node bin/render2.mjs                     # score → out/factory-remix-v2-full.wav (+ --stems)
bash bin/cut-v2.sh                       # master once → out/factory-remix-v2.mp3
```

`bin/render.mjs` (v1) still builds its own outputs and is untouched.

---

## v1 — "the stamp"

`out/factory-remix-v1.mp3` · 3:34 · 88 bars at 100 BPM · D minor

| act | at | what happens |
| --- | -- | ------------ |
| **I · POWER-ON** | 0:00 | Motor hum (D1, fluttering), relay clicks finding the eighth grid, and the take's own count-in — the operator switching the machine on. The die seats itself. |
| **II · THE STAMP** | 0:19 | Kick = press. Line 1 stamped every two bars, IDENTICAL every time. Conveyor ticks cross the stereo field left to right, one belt-length per bar. |
| **III · FULL SPEC** | 0:58 | The whole poem cycles; the harmony walks Dm·Dm·Bb·C; the factory at capacity. |
| **IV · OUT OF SPEC** | 1:36 | Stamp *k* is *k* steps out of tolerance: 9 cents and 9 ms per copy, die bounces (double-strikes), belt steps dropped, wrong dies in the press (takes b and c). "break free" punches through between copies. |
| **V · BREAK FREE** | 2:14 | **The turn.** Press stops, kick out, the belt slips its clock. The middle line alone on the factory floor — then the same words from the *other pressing*, which is what breaking out of a cookie-cutter personality sounds like. |
| **VI · SPINNING AWAY** | 2:34 | The groove returns rotating instead of stamping: "spinning away" circles the field (a full circle every 8 bars), the harmony finally travels (Dm·Bb·F·C), and the bird arrives — sine chirps over the top, then the word itself an octave up. |
| **VII · SHUTDOWN** | 3:12 | The press decelerates: copies come out slower and lower (real varispeed drift on the read head — the last one never finishes), the hum winds down a minor third, the belt stretches its ticks apart. Then, after the machine: take b's bird. G#2. It was outside the whole time. |

## Sound design

- **The press** — a die coming down: 120→55 Hz thud + three inharmonic
  partials in free-bar ratios (1 : 2.76 : 5.40 — struck metal, not a
  chord) + a 2 ms noise chiff. Fast decay; a press strikes, it does not
  ring.
- **The conveyor** — sixteen ticks a bar that pan across the field left
  to right, one belt-length per bar. In OUT OF SPEC it drops steps; in
  BREAK FREE it slips its timing; in SHUTDOWN the gaps stretch 13% per
  tick.
- **The hum** — D1 + partials with a 0.4 Hz flutter and an 8.3 Hz
  rotation roughness. SHUTDOWN bends the whole stack down by a minor
  third.
- **The bird** — the one voice with no grid: sine chirps gliding up a
  fourth with late vibrato, echoed through the dub delay.
- Kick, sine-bump bass, dub delay, sidechain, Special Sign side return,
  the one-shot player and its wiggle/varispeed are **pop/cult's, carried
  over** (`bin/render10.mjs` lineage). Every rule survives: 10 ms
  raised-cosine tails, ramped ducks, no master tanh, one linear trim.

## Measured — v1

| check | value |
| ----- | ----- |
| duration | 214.4 s (3:34 incl. tail) |
| integrated loudness | **−14.2 LUFS** |
| true peak | **−1.8 dBFS** |
| loudness range | 6.7 LU |
| max sample | 0.780 (limiter ceiling; 0 full-scale runs) |
| max 2 kHz-band step | **0.110** (fault line ~0.26; raw step 0.43 is the ticks, on purpose) |
| mono fold-down | −0.13 dB |
| bus balance (stems, LUFS) | vox −11.8 · drums −15.5 · music −18.6 · mach −32.0 (transient belt; peaks −10 dBFS short-term in the groove) |

## Run it

```bash
cd pop/factory
../.venv/bin/python bin/harvest.py    # fetch + transcribe + measure + slice → samples/, harvest.json
node bin/render.mjs                   # score → out/factory-remix-v1-full.wav (+ --stems)
bash bin/cut-v1.sh                    # master once → out/factory-remix-v1.mp3
```

`harvest.py` needs `pop/.venv` (numpy, soundfile, pyworld, librosa,
`setuptools<81`) and `whisper-cli` on PATH with `~/.whisper-models/
ggml-small.bin`. Everything in `source/` and `samples/` is derived and
gitignored; the scripts and `harvest.json` are the source of truth.

Mastering is **measure → one static dB → true-peak limiter** (cult's
law). Never a second loudnorm: at loud targets it silently abandons
`linear=true` and rides gain, which manufactures the sample-step
artifacts the renderer was careful not to make.

## Where v4 could go

- v3 charts the ONE unbroken take. The other five dies (b, c, d, e, f)
  still stamp raw in v2's arrangement; each could get its own chart and
  its own drift, so OUT OF SPEC is wrong dies on the SAME grid rather
  than wrong dies at their own speeds.
- `bin/audit.py` currently reports; it could refuse. A build that raises
  a SLICES flag is a build that will be heard as a smeared consonant, and
  there is no reason to let it write a WAV.
- The eighteen surveyed posts include a 58 s talk about whistlegraphs as
  generative art and a 2026 take ("mhm yaaa factoreyy :) cookay cubber")
  that is practically a remix already.
- The press could pump the bed harder (cult's tube-bus trick, keyed off
  press hits rather than kicks).
