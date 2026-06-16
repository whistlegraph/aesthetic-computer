# americomputadora

sample-based **bubblegum pop**. the song is built around a single three-syllable hook —
**america · computer · dora** — strung from clipped voices and varied into a library
deep enough to audition for the *ultimate* version of the line.

three sample sources, one per syllable:

| syllable   | source                                                              | tone we're after                          |
| ---------- | ------------------------------------------------------------------- | ----------------------------------------- |
| `america`  | "America the Beautiful" performances (Ray Charles, Whitney, et al.) | sung-out vowel, "Aaaa-MEEE-ri-CAAAA!"     |
| `computer` | macOS `say` voices (Fred, Zarvox, Cellos, Bubbles, Whisper, …)      | flat synth-voice canon — 1990s mac kitsch |
| `dora`     | *Dora the Explorer* theme + episode intros                          | bright kid voice, cartoon downbeat        |

## bubblegum posture (decided 2026-05-26, per Fía's sub-researcher brief)

bubblegum is **earnest production around a goofy hook**, not irony layered on irony.
the three samples already carry the camp — our job is to play it straight around them.

- **tempo** ≈ 108–116 BPM. fast enough to be kinetic, slow enough that the three
  syllables resolve as a line, not a tongue-twister.
- **key**: major. D or E major are kind to Mac-voice formants and to most
  *America the Beautiful* performances (originally Bb/C). pitch-snap each clip to
  scale tones; let **dora** land on tonic or the major third — that's the resolution.
- **drums**: dry kit, kick on 1 & 3, clap on 2 & 4, 16th tambourine. no half-time,
  no trap.
- **synths**: DX-bell stabs on the hook downbeats, square lead doubling the
  syllables an octave up, toy-piano on the verse, sub-sine bass root-fifth.
- **stop-time**: drop everything but vocal + claps for one bar before the final
  chorus. the oldest reliable move in the form.
- **structure (target)**: intro (4) → hook ×4 (4) → verse 1 (8) → hook ×4 (4) →
  verse 2 (8) → bridge (4) → stop-time (1) → hook ×4 (4) → outro (4) ≈ 40 bars,
  ~1:30 at 112 BPM.

what we are **not** doing: pitch-shifting everything into doll-voice (kills the
human anchor); making the syllable string be the whole song (it's the chorus
hook, not the verse); ironic-on-ironic stacking.

## pipeline

```
sources/<group>/*.mp3        # bin/fetch.mjs       — yt-dlp downloads (gitignored)
clips.json                   # extract.mjs seed    — timestamp annotations live here
utterances/<group>/*.wav     # bin/extract.mjs     — clipped syllables (or say-computer.mjs)
variations/<group>/<src>/*.wav  # bin/variations.mjs  — pitch / stretch / chipmunk / reverse
catalog.json + audition.html # bin/catalog.mjs     — browse + arrange in browser
```

### one-time setup

```bash
# 1. download sources (yt-dlp; copyright-bound, you fetch yourself)
node bin/fetch.mjs --list      # preview curated list
node bin/fetch.mjs              # download all
node bin/fetch.mjs america      # one group

# 2. fill in clip timestamps (start/end in clips.json). either by ear, or:
node bin/extract.mjs --scan sources/america/ray-charles-1972.mp3   # silencedetect candidate segments

# 3. cut utterances
node bin/extract.mjs

# 4. (auto-runs daily) generate TTS computer voices — no source needed
node bin/say-computer.mjs

# 5. build variation library (pitch / stretch / chipmunk / reverse)
node bin/variations.mjs

# 6. catalog + audition page
node bin/catalog.mjs
open audition.html
```

### the audition page

`audition.html` lists every clip grouped by syllable. shift-click or
double-click to drop into the right-hand "arrangement" pane, then **▶ play in
sequence** to hear the string. **export json** dumps the arrangement so a
follow-up render script can turn it into a fixed mix.

## word isolation (bin/clean.mjs)

the america/dora cuts came straight off full mixes — crowd, band and
theme-music rode under every word, and the whisper transform smears that bed
into the breath. `clean.mjs` fixes it at the source:

1. **demucs** (two-stems) separates each source recording into vocals /
   accompaniment. full recordings give the model context that sub-second
   clips can't; marathon sources (the 3-hour soy-dora compilation) get a
   ±15 s proxy window per clip so an 8 GB machine survives. stems cache in
   `.cache/demucs/`.
2. every clip in clips.json is **re-cut from the vocal stem**, denoised
   (`afftdn`), silence-trimmed on both ends — the music bleed is silence
   now, so the word boundary tightens itself — micro-faded, loudnormed.
3. results overwrite `utterances/<group>/` under the same names.

```bash
node bin/clean.mjs --resnap   # separate + re-cut + rebuild snapped/ + hooks.html
```

falls back to a denoised mix-cut (and says so) if a stem comes up empty for
some span. demucs installed via `pipx install demucs` + `pipx inject demucs
torchcodec`.

## the hook machine (bin/hooks.mjs)

every utterance gets **autotuned** to every target note its slot needs across
the hook variants, plus a **whispered twin** (FFT phase randomization — the
word's formants survive, the pitch dissolves into breath). that's the
`snapped/` library, indexed by `hooks.json`. from there every combination of
the full phrase — currently **22,218** (14 america × 69 computer × 23 dora) —
is playable instantly, no per-combo render.

```bash
node bin/hooks.mjs                  # build snapped library + hooks.html
node bin/hooks.mjs flight --count 24 --seed 7   # random stitched mp3s → out/hooks/
node bin/serve.mjs                  # then open http://localhost:7777/hooks.html
node bin/hooks.mjs render --fav hooks-favorites.json  # stitch kept combos
```

`hooks.html`: three columns (america / computer / dora), per-slot whisper
toggles, melody-variant selector, beat-grid playback. **space** plays,
**s** surfs a random combo, **f** keeps it, arrows/`,`/`.` cycle each slot.
export favorites json and feed it straight to `render.mjs --fav`.

`out/hooks/_flight-all.mp3` is the contact sheet — every flight combo in one
listen with a breath between.

## bachiamatrixian mode (render.mjs)

the hook sections now ride a **serious four-on-the-floor kick** (42 Hz body,
tanh drive) with a deep sidechain pump (duck to 0.22, cosine recovery across
the beat). a **bach arp** — 16th-note baroque figuration, chord tones + the
diatonic ninth, plucked like a harpsichord pixel — rains above the vocal and
takes the full pump. three `bach-*` hook variants in melody.json give the
vocals baroque contours to follow; the variant rotation reaches them on the
second chorus. the bridge is the matrix breakdown: kick + sub + arp alone in
8ths an octave down.

**the canonical mix** (the old bubblegum-only cut is deprecated):

```bash
node bin/render.mjs \
  --pick america=whitney-houston-w1 \
  --pick computer=samantha-slow,fred-slow,karen-slow,daniel-slow,moira-slow,ralph-slow,kathy-slow,albert-slow \
  --stretch computer=1.4 --decap computer --whisper computer \
  --pick dora=theme-song-w1 \
  --out out/americomputadora-bach-whisperz.mp3
```

- comma lists in `--pick` are a **roster that rotates per hook phrase** —
  a different computer voice every utterance.
- `--stretch group=1.4` time-stretches (pitch kept): "cawwwmputerrrr".
- `--decap group` chops the leading consonant — vowel-onset detection
  (energy + zero-crossing) finds where /k/ ends, so "computer" becomes
  "ahmputer" and pours straight out of america's open "aaaah". detection
  runs on the voiced twin even when the whispered file is used.
- every vocal clip is RMS-matched after processing so the three words sit
  at the same loudness in the same space.
- `--fav hooks-favorites.json --fav-index 2` renders a kept combo.

render pulls clips from `snapped/` directly (already tuned + loudnormed,
whisper twins included) and only live-shifts when a snap is missing.

## current state (2026-06-09)

- ✅ all three sample groups cut: america **14**, computer **69**, dora **23**
  utterances (+1,802 variations)
- ✅ america/dora words demucs-isolated from their mixes, denoised,
  boundary-tightened (`bin/clean.mjs`, 37/37 clean — no fallbacks)
- ✅ `bin/hooks.mjs` — snapped/whispered library (940 wavs), `hooks.html`
  combinatorial audition page, `flight` + `render --fav` stitchers
- ✅ `bin/render.mjs` — full mix with bubblegum kit, bach arps, serious
  sidechained kicks, `--pick/--whisper/--fav` hook selection
- ✅ melody.json — 4 classic + 3 bachiamatrixian hook variants
- ⏳ pick THE hook: surf hooks.html, keep favorites, render each with
  `--fav`, A/B until one sticks

## album

ID3 album: `pixsies` (lowercase). same shelf as the other pop/ singles.

## acknowledgements

bubblegum-pop genre brief by sub-researcher 2026-05-26 — captured the
Kasenetz-Katz lineage through Max Martin to K-pop. see also
[../booch/README.md](../booch/README.md) for the boom-bap counterpart.
