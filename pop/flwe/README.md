# pop / flwe — "Flower Eater"

The start of an emo-rock lane for **`flwe` — "Flower Eater"**, written and
performed by **Jeffrey Alan Scudder** (Whistlegraph, 2021, 74K views across
eight tagged posts). A solo sung whistlegraph — one voice, one pen, no
accompaniment — about a girl who eats flowers and gets crazy hallucinations.

What's here: the **measured foundation** (tempo, key, the melody per word,
a word-level transcript, a vocalized click track) and the **aesthetivox** —
every vocal through the WORLD speech-to-singing chain onto a derived note
chart, per the house rule (cult → loner → here): no lead vocal ships raw;
every line is a sung NOTE, not a chopped speech hit.

## The takes

All eight tagged posts are downloaded (`bin/fetch.sh` re-pulls them from
the assets mirror; `source/` stays out of git):

| post | date | length | what it is |
| ---- | ---- | ------ | ---------- |
| **6992837952212569350** | 2021-08-05 | 85 s | **the spine** — "behind the scenes" take: spoken intro (0–18 s), the whole song clean (18.6–79.7 s), spoken outro ("it's about a girl who eats flowers and gets crazy hallucinations") |
| 6948629412728360198 | 2021-04-08 | 59 s | "the epic Flower Eater by Jeffrey, who is currently sick in bed" — the quietest room of all eight (noise floor 0.0032) |
| 6949031877718117638 | 2021-04-09 | 54 s | "my greatest (& longest) piece so far" — the premiere |
| 6949568150962703621 | 2021-04-10 | 56 s | 🌻🌛🥀 take |
| 6949737524520602885 | 2021-04-11 | 24 s | excerpt — but it carries the fullest **"Bury them in the ground"** |
| 6950816151547022598 | 2021-04-14 | 55 s | "can u make it all the way?" — cleanest **"Mystery flower"** |
| 6975681078543551749 | 2021-06-20 | 177 s | the Providence tiny-recital — Flower Eater runs **~67–127 s** inside it; noisiest room by 5× (crowd) |
| 6977277752525344005 | 2021-06-24 | 60 s | live take, song only — lyric cross-check |

`bin/takes.py` → `analysis/takes.json` is the per-phrase comparison
(loner's take-rotation move): every canonical phrase matched into every
other take by fuzzy transcript, measured for voiced coverage, cents to the
derived grid (in each take's own fitted tonic — they cluster 130–137 Hz,
all near C +20–60¢), and HF noise floor. **Verdict: the render stays on
the primary for every phrase** (the unbroken-take philosophy — one warp,
no seams); `better_elsewhere` marks where a future dub pass should look
first: *Mystery flower* (6950816…, more voiced, cleaner, closer to grid)
and *Bury them in the ground* (6949737…, the primary's is nearly all fry).

## What the source gave us

**Tempo — rubato, median ≈ 110 BPM.** Onset autocorrelation over the sung
region gives 117.2 global, but 10-second windows (octave-folded) drift
**97 → 137 BPM around a median of 110.3**. This is a hand-and-voice
performance: it breathes phrase by phrase. Verdict in
`analysis/tempo.json`: `"rubato"`.

**Key — C, about +40 cents sharp, minor-leaning and hand-drawn.** The
closing "looping every day" chant circles **~134 Hz = C3 +40¢**, and the
final cadence is "flower" (G3) resolving up a fourth to "eater" (C4) —
5 → 1 in C. The opening strain circles **D♯/E♭ minor** ("Looked" D♯3,
cadence "love" D♯3) before the piece settles onto the C center; E♭ (♭3)
outweighs E in the pitch-class weights, with heavy C♯/D neighbor motion.
The voice sits ~+20–40 cents sharp of equal temperament throughout —
**tune any accompaniment to TONIC ≈ 134 Hz, not concert C.** Receipt:
`analysis/key.json`.

**The melody, per phrase** (per-word pyin receipts in
`analysis/melody.json`; times are the primary take's):

| t | phrase | contour |
| - | ------ | ------- |
| 18.6 | Looked for so long for the one that I love | D♯3 C♯3 D♯3 A♯2 · C3 D3 → **D♯3** |
| 25.2 | Left with my arms up in the air | F♯3 D3 A♯2 G2 F♯3 → low D |
| 30.2 | My lollipop was the stem I grabbed | B3 A3 · E3 G3 → C♯3 |
| 34.2 | Hold up with both of my hands | E3 D♯3 C♯3 D3 E3 F3 → A2 |
| 38.2 | Mystery flower please don't cower | D3 D3 A♯3 A3 → G3 |
| 42.9 | I am hatin' green now | D3 C3 → G2 |
| 46.6 | I'm gonna take you so I can break you up into pieces | D♯3 C♯3 F♯3 A♯3 G♯3 B3 → C4 B3 (the climb) |
| 54.1 | Yum yum yum | G♯4 F4 (the peak of the take) |
| 55.8 | Now I think I wanna have you every day | E4 D4 C♯4 D♯4 C♯4 → A♯3 |
| 60.9 | I'll spit three of your seeds | G♯3 A3 → E4 F4 |
| 63.2 | Bury them in the ground | B2 A2 A♯2 → C3 |
| 65.7 | Harvest moon I'll come collect your spawn | A♯3 C4 C♯4 → C4 |
| 69.8 | They're gonna be all mine cuz I'm **looping every day** ×3 | G♯3 D4 G3 A3, then the chant on C3/C♯3 (~134 Hz) |
| 77.0 | flower eater | **G3 → C4** (the 5→1 cadence) |

**The words** (whisper.cpp `ggml-small.en`, `-ml 1` word-level; full
timestamps in `analysis/transcript.json`, raw JSON in
`analysis/whisper-raw/`):

> Looked for so long for the one that I love. Left with my arms up in the
> air. My lollipop was the stem I grabbed. Hold up with both of my hands.
> Mystery flower please don't cower. I am hatin' green now. I'm gonna take
> you so I can break you up into pieces. Yum yum yum. Now I think I wanna
> have you every day. I'll spit three of your seeds. Bury them in the
> ground. Harvest moon I'll come collect your spawn. They're gonna be all
> mine cuz I'm looping every day, looping every day, looping every day —
> flower eater.

Cross-take notes: the recital and live takes both hear **"three of your
seeds"** (the primary's "twee" is delivery, not lyric); the live take hears
"I am **angry** now" where the primary gives "hatin' green" — a line to
confirm with @jeffrey before words get re-sung.

## The click track

`out/flwe-clickvox.wav` / `.mp3` — the **vocalized click study**
(the kickvox move from cult/loner): a steady click marks the measured grid
— soft kick on downbeats, 1.8 kHz tick on beats, **110.3 BPM, 4/4, two-bar
count-in** — and the primary take's own phrases sit on it dry, each phrase
start snapped to the nearest beat (all shifts ≤ ±0.25 s; receipt with
measured-vs-snapped beats in `analysis/clickvox.json`) with the internal
rubato left intact. The point: hear where Jeffrey's phrasing agrees with a
grid and where the arrangement will have to bend instead.

## The aesthetivox

The full process, in order (study source: `pop/loner/bin/halo3.py` — the
v4pid regulation engine — plus `pop/loner/bin/aesthetivox.py` and
`pop/cult/bin/sing.py`'s Saitou recipe):

**1. The notes** (`bin/notes.py` → `analysis/notes.json` +
`analysis/melody-chart.json`). The scale is derived from the data, not
assumed: a frame-level pyin histogram over the sung region, folded into
pitch classes in the take's own frame (**tonic 133.9 Hz = C3 +40¢**),
degrees kept above the histogram's own largest gap (≥ 7% of voiced mass).
What falls out is **not natural minor** — it is a hand-drawn 8-degree set:

> **1 ♭2 2 ♭3 · 5 · 6 ♭7 7** — a chromatic cluster around the tonic, an
> empty middle (3, 4, ♭5, ♭6 all under 6%), the dominant, and a chromatic
> cluster under the octave.

Per-word targets are re-measured (median of the last 60% of each word's
confidently-voiced frames — the sustain, past the scoop; melody.json's
whole-word medians called "that" D♯2 when the note is A2). One pin: the
closing "eater" → C4, the 5→1 cadence. The chart quantizes each phrase
onto the 110.3 BPM clickvox grid, word durations to 8ths — the chart is a
score, not a transcription, and it is what any future arrangement reads.

**2. Plosives vs vowels + the render** (`bin/aesthetivox.py`). Per charted
phrase of the primary take: WORLD analysis with a **fitted floor** (probe
first — the phrases live anywhere from 67 to 140 Hz); word boundaries
pulled to real acoustic events (whisper times the transcript handover,
not the note change); trailing decay **trimmed, not stretched**; 30 ms of
attack runway kept, borrowed only from silence. Then the regulation:
**consonants ride 1:1 and are never stretched; the voiced nucleus carries
the note** — stretched to fill its beat slot and pulled to the chart
target at **snap 0.92** (45 ms smoothing; the snap fades out where the
pitch genuinely slides). Each vowel onset lands **on its beat slot** with
the consonant leaning in ahead, the way a singer places a word. A nucleus
stretched past 1.8× flattens to its target with vibrato fading in over
0.4 s; a phrase that hard-stops gets a **synthesized WORLD release**
(ping-pong of the last 120 ms, faded). Voiced regions are WORLD audio;
the warped original is composited back through unvoiced regions (5 ms
seams) so /s/ /t/ /k/ stay real, then unvoiced-bright frames get +8 dB
back (halo3's sibilant restore).

Receipts in `analysis/aesthetivox.json` (per word: runway ms, stretch
ratio, holds, boundary moves, trims). QC: the rendered vocal sits at a
**median 9.7 ¢ from its chart targets** (75 measurable words).

Out:

- `out/flwe-aesthetivox.wav/.mp3` — the sung-note vocal on the click grid
  (the clickvox study, but every phrase is now NOTES)
- `out/flwe-aesthetivox-halo.wav/.mp3` — + octave halo (vowels-only,
  darkened, ±6¢ pair) and low self-backup at −2/−4 scale degrees, low gain
- `vox/NN-slug.wav` + `vox/.manifest.json` — the phrase bank: each
  phrase's dry render with its `lead_in_s` noted, so beat 0 is placeable
  in any arrangement

## Re-running

```sh
bin/fetch.sh                                  # pull + decode all 8 takes
../../.venv/bin/python3 bin/analyze.py        # tempo.json, melody.json, transcript.json
../../.venv/bin/python3 bin/clickvox.py       # out/flwe-clickvox.wav+.mp3, clickvox.json
bin/transcribe.sh                             # whisper-raw/ for every take (needs whisper-cli
                                              #   + ~/Models/ggml-small.en.bin, or WHISPER_MODEL=…)
../../.venv/bin/python3 bin/notes.py          # notes.json + melody-chart.json (the score)
../../.venv/bin/python3 bin/takes.py          # takes.json (per-phrase take comparison)
../../.venv/bin/python3 bin/aesthetivox.py    # the render + vox/ bank + aesthetivox.json
```

Whisper raw JSONs come from
`whisper-cli -m ggml-small.en.bin -f <16k wav> -ml 1 -oj` (whisper.cpp).
Everything python runs on `pop/.venv` (librosa 1.0 + soundfile + pyworld);
the key receipt (`analysis/key.json`) is derived from `melody.json`.
