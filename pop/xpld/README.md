# pop / xpld — "Explody Head"

The start of a pop lane for **`xpld` — "Explody Head"**, written and
performed by **Jeffrey Alan Scudder** (Whistlegraph, 2023, 191K views
across five tagged posts). A solo sung whistlegraph — one voice, one pen,
no accompaniment — an emo self-portrait: hit a hump, gave up, got a new
name, and a face full of internet, safe in a bubble with the emo-gee
that signifies an identity.

What's here: the **measured foundation** (tempo, key, the melody per
word, a word-level transcript, the per-phrase take comparison, a
vocalized click track) and the **aesthetivox pass** — the note chart
(`bin/notes.py`), the sample bank (`vox/`), and the WORLD sung-note
render (`bin/aesthetivox.py` → `out/xpld-aesthetivox.wav/.mp3` and the
`-halo` variant).

## The takes

All five tagged posts are downloaded (`bin/fetch.sh` re-pulls them from
the assets mirror; `source/` stays out of git):

| post | date | length | what it is |
| ---- | ---- | ------ | ---------- |
| **7275499036398865706** | 2023-09-06 | 100 s | **the spine** — 95.4K views: spoken intro (0–4.5 s, "Hey come here, I want to show you a whistlegraph called Explodey Head"), then the whole song clean (6.0–99.2 s) |
| 7257275807209458986 | 2023-07-18 | 82 s | "a real adventure" — the premiere; sung straight through, quiet room |
| 7257695616694881582 | 2023-07-20 | 93 s | "thing like a doggy #mood" take — the **quietest room of the five** (noise floor 0.0028, 20× below the primary) |
| 7258670360357276970 | 2023-07-22 | 85 s | "no match for my graphic tee ✒️💀" — real dog barks and coughs mid-take; carries the **cleanest "do you see"** |
| 7278943795482283307 | 2023-09-15 | 81 s | "whistlegraphing is not lame" — the last take, nine days after the spine; the most-voiced delivery of several phrases |

`bin/takes.py` → `analysis/takes.json` is the per-phrase comparison
(loner's take-rotation move): every canonical phrase of the primary
matched into every other take by fuzzy transcript, measured for voiced
coverage, cents to the derived grid (in each take's own fitted tonic —
**all five cluster 187.3–190.6 Hz, F♯3 +22 to +52¢** — two months apart
and the pitch memory holds), and HF noise floor. **Verdict: the render
stays on the primary for every phrase** (the unbroken-take philosophy —
one warp, no seams); `better_elsewhere` marks where a future dub pass
should look first: *Blocking the doorway little doggy*, *Safely in a
bubble that surrounds me* and *Gee defends me in my bubble* (7278943…,
more voiced, no further off-grid — but mind its high HF floor) and
*do you see* (7258670…, ten times quieter than the primary's room).
Caveat: three of the four alternates were transcribed by whisper as long
run-on segments, so several canonical phrases fuzzy-match in only one or
two other takes — the comparison is honest about that in `matches`.

## What the source gave us

**Tempo — rubato, median ≈ 131 BPM.** Onset autocorrelation over the
sung region gives 133.9 global, but 10-second windows (octave-folded)
drift **99 → 148 BPM around a median of 130.8**. A hand-and-voice
performance that breathes phrase by phrase. Verdict in
`analysis/tempo.json`: `"rubato"`.

**Key — F♯, about +31 cents sharp, major/mixolydian-leaning and
hand-drawn.** F♯ is by far the most-sung pitch class (466 voiced frames
vs 371 for the runner-up), and "this emo-**Gee** defends me in my
bubble" cadences square onto F♯3. The scale the histogram keeps is
**1 2 3 5 6 ♭7 — F♯ G♯ A♯ C♯ D♯ E** — mixolydian with the 4th (B)
nearly absent; A♯ (major third) clearly outweighs A. The ♭7 carries the
closing question: "star oh yeah" and the final **"do you see"** both
land on E3, unresolved; "signifying my identity" holds B3, the missing
4th making its one appearance as the high note. The voice sits **~+31
cents sharp** of equal temperament — **tune any accompaniment to
TONIC ≈ 188 Hz (94.2 Hz an octave down), not concert F♯.** Receipt:
`analysis/key.json`.

**The melody, per phrase** (per-word pyin receipts in
`analysis/melody.json`; times are the primary take's):

| t | phrase | contour |
| - | ------ | ------- |
| 6.2 | Hit a hump and plateaued | A2 B2 → D♯3 (the climb out of the hump) |
| 10.4 | then I gave up | F3 C3 → C♯3 |
| 13.4 | now they call me Explodey Head | F♯2 C3 A♯2 → **F♯2** (the low tonic octave — the name lands on 1) |
| 19.0 | Got my arm out waving anticipating | F♯2 A2 B2 D♯3 → G♯3 (rising the whole way) |
| 25.5 | nothing special | D♯3 → B2 |
| 29.1 | Blocking the doorway little doggy | A♯2 G2 F2 E2 → C♯3 |
| 34.0 | thing like a doggy | D♯3 A♯3 A♯3 → F♯3 |
| 37.3 | will you shut the fuck up | C3 E3 **B3 A♯3 G♯3** → E3 (the outburst, stepping down) |
| 43.0 | This house that I built's got cracks at the sides | D2 fry → A2 F3 · C♯3 E3 C3 D3 |
| 48.4 | brittle like life and smoothing out | A♯2 G2 G♯2 B2 → D♯3 |
| 54.2 | Air pockets — you're no match for my graphic tee | **G4** F4 F4 (the peak of the take) then the drop: C3 A♯2 G♯2 A2 A♯2 |
| 59.0 | my face is full of internet | D♯3 → D♯4 E4 F4 D4 D4 (the second summit) |
| 62.0 | Safely in a bubble that surrounds me | F♯3 G3 E3 F♯3 E3 B3 → G3 |
| 68.0 | I don't think I want you back in here | F♯3 D♯3 G3 F♯3 D♯3 G3 → E3 |
| 72.0 | oh no · this emo | G3 F3 · A♯3 G3 |
| 76.2 | Gee defends me in my bubble | E3 A♯3 F♯3 D3 C♯3 → **F♯3** (the tonic cadence) |
| 81.5 | signifying my identity my identity | F♯3 F♯3 → A♯3 A♯3 A♯3 |
| 90.0 | It flickers like a shooting star | C2 fry → C3 D3 G3 E3 |
| 95.6 | oh yeah · do you see | E3 · D3 E3 → **E3** (♭7 — the question left open) |

**The words** (whisper.cpp `ggml-small.en`, `-ml 1` word-level; full
timestamps in `analysis/transcript.json`, raw JSON in
`analysis/whisper-raw/`):

> Hit a hump and plateaued, then I gave up — now they call me Explodey
> Head. Got my arm out, waving, anticipating... nothing special.
> Blocking the doorway, little doggy, thing like a doggy — will you shut
> the fuck up. This house that I built's got cracks at the sides,
> brittle like life, and smoothing out air pockets. You're no match for
> my graphic tee — my face is full of internet. Safely in a bubble that
> surrounds me, I don't think I want you back in here, oh no. This
> emo-gee defends me in my bubble, signifying my identity, my identity.
> It flickers like a shooting star, oh yeah — do you see?

Cross-take notes: four of five takes hear **"blocking** the doorway"
(the primary's "Locking" is delivery); 7278943… hears "built**'s got**
cracks", confirming the primary's "built Scott" as `'s got`; the post
caption on 7258670… confirms **"graphic tee"** (whisper variously hears
teeth/tea/team); three takes hear "this **emoji** defends me" — the
emo-gee/emoji pun is the lyric doing its job. One line to confirm with
@jeffrey before words get re-sung: the syllable after "air pockets"
(primary hears "dream", others "true"/"ew" — possibly just the vowel of
the melisma).

## The click track

`out/xpld-clickvox.wav` / `.mp3` — the **vocalized click study**
(the kickvox move from cult/loner): a steady click marks the measured
grid — soft kick on downbeats, 1.8 kHz tick on beats, **130.8 BPM, 4/4,
two-bar count-in** — and the primary take's own phrases sit on it dry,
each phrase start snapped to the nearest beat (all shifts ≤ ±0.22 s;
receipt with measured-vs-snapped beats in `analysis/clickvox.json`) with
the internal rubato left intact. The point: hear where Jeffrey's
phrasing agrees with a grid and where the arrangement will have to bend
instead.

## The notes and the chart

`bin/notes.py` → `analysis/notes.json` + `analysis/melody-chart.json` —
every sung word's target note, quantized to the derived **1 2 3 5 6 ♭7**
set in the take's own **188.3 Hz (F♯3 +31¢)** frame. The scale of record
is `key.json`'s frame-level derivation; notes.py re-probes it as a
cross-check (the top four ranked degrees must be the 1 6 3 ♭7 spine —
they are). Per word the pitch is **re-measured as the sustain median**
(the last 60% of confidently-voiced pyin frames — the flwe fix: a
whole-word median blurs a scooped onset into the note). The landmarks
hold: *"now they call me Explodey Head"* lands in the low tonic octave
(F♯2 → E2), *"Airpockets"* is the summit (sustain 408 Hz — G4+68¢
concert, which the +31¢ grid keeps as degree 2, G♯4), and the final
**"do you see" ends on the ♭7 (E3), unresolved** — one pin holds it
there against a 30¢ coin-flip toward D♯3; nothing is allowed to resolve
it to the tonic. The chart puts each phrase on its clickvox beat with
word onsets on-grid by construction (8th-note quantize, min half a
beat) — the score any future arrangement reads.

**The dubs, measured honestly:** the four `better_elsewhere` phrases
were checked for renderability and all four alternates sing them in a
**different register/contour** (7278943…'s "Safely in a bubble" orbits
270–380 Hz where the spine sings ~165–250) — regulating that onto the
spine's chart would mean 500–1600-cent pulls, no longer intonation
repair. So the **render never dubs** (unbroken-take philosophy) and each
chart phrase instead carries a `dub` block — the alternate's span,
DP-aligned words, and `hz_dub` receipts — which the **sample bank**
carves: *Locking the doorway* from **7258670…** (the quiet July room
beats the flagged 7278943… on every column), *Safely in a bubble* and
*Gee defends me* from **7278943…**, *do you see* from **7258670…**.

## The sample bank

`vox/` — carves with natural onsets, everything indexed in
`vox/.manifest.json` (source take, time range, f0/note, word):

- `NN-slug.wav` — the 22 rendered sung phrases (dry, lead-in noted so
  beat 0 is placeable)
- `words/NN-MM-word.wav` — 111 raw per-word carves from the primary
  (post boundary-repair/trim/attack-runway, nothing synthesized)
- `dubs/NN-slug--TAKE.wav` — the four flagged phrases carved raw from
  their better takes, in their own register

## The aesthetivox

`bin/aesthetivox.py` → `out/xpld-aesthetivox.wav/.mp3` (+ `-halo`) —
the flwe/halo3 WORLD chain on the 130.8 grid: fitted-floor analysis,
boundary repair, energy trim, 30 ms attack runway, **consonants 1:1
(stretch weight 0.18) / voiced nuclei on the note at snap 0.92 with
45 ms smoothing**, vowel-onset-on-the-beat with the consonant runway
leaning in ahead, holds >1.8× flattening to target with 0.4 s vibrato
fade-in, sibilant restore (+8 dB), tail capped at the next phrase's
opening, synthetic WORLD release where the source hard-stops. The halo
variant adds the octave halo (vowels-only, dark, ±6¢ pair) and low
self-backup at −2/−4 scale degrees.

Two xpld-specific lessons: the primary's noisy room still gates (~19%
of 5 ms frames under −36 dB, so trim/runway hold), and **whisper crams
fry clusters** ("This house that I…", "Hit a…", "It flickers…") into
10 ms slivers — a redistribution pass (`spread_crammed`) spreads any
run of sub-60 ms words over the room up to the next repaired boundary
before warping, or a half-beat slot becomes a 23× frozen loop.

QC (in `analysis/aesthetivox.json`): the render re-measured against the
chart with a target-bracketed harvest — **median 6¢, p90 45¢** over 106
words. The residual outliers are the genuine fights: the "my"→"face"
octave-leap pickup (+516¢ mid-glide — the glide release correctly lets
the slide live), the C♯2 fry words (pitch ill-defined), and
"smoothing"'s scooped onset.

## Re-running

```sh
bin/fetch.sh                                  # pull + decode all 5 takes
../../.venv/bin/python3 bin/analyze.py        # tempo.json, melody.json, transcript.json
../../.venv/bin/python3 bin/clickvox.py       # out/xpld-clickvox.wav+.mp3, clickvox.json
bin/transcribe.sh                             # whisper-raw/ for every take (needs whisper-cli
                                              #   + ~/Models/ggml-small.en.bin, or WHISPER_MODEL=…)
../../.venv/bin/python3 bin/takes.py          # takes.json (per-phrase take comparison)
../../.venv/bin/python3 bin/notes.py          # notes.json + melody-chart.json (the score)
../../.venv/bin/python3 bin/aesthetivox.py    # out/xpld-aesthetivox(+-halo).wav+.mp3,
                                              #   vox/ bank, analysis/aesthetivox.json
```

Whisper raw JSONs come from
`whisper-cli -m ggml-small.en.bin -f <16k wav> -ml 1 -oj` (whisper.cpp).
Everything python runs on `pop/.venv` (librosa + soundfile + pyworld);
the key receipt (`analysis/key.json`) is derived from `melody.json` plus
a frame-level voiced-pyin probe, and carries `tonic_hz` +
`scale_semitones` for `takes.py` and the note chart.
