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

## current state (2026-05-26)

- ✅ pipeline scripts (`fetch`, `extract`, `say-computer`, `variations`, `catalog`)
- ✅ `computer/` TTS — **69 utterances**, **1,173 variations**, all in
  `utterances/computer/` and `variations/computer/`
- ⏳ `america/` + `dora/` — sources blocked by harness auto-mode classifier;
  run `node bin/fetch.mjs` yourself, then fill timestamps in `clips.json` and
  re-run `bin/extract.mjs` and `bin/variations.mjs`
- ⏳ render script (`bin/render.mjs`) — not yet written; will consume an
  `arrangement.json` + a bed score (notepat `.np` + bubblegum synths) and mix
  the final track
- ⏳ bed: needs DX-bell stab synth, toy-piano preset (might reuse
  `pop/booch/synths/rhodes.mjs` with stretched partials), square lead

## album

ID3 album: `pixsies` (lowercase). same shelf as the other pop/ singles.

## acknowledgements

bubblegum-pop genre brief by sub-researcher 2026-05-26 — captured the
Kasenetz-Katz lineage through Max Martin to K-pop. see also
[../booch/README.md](../booch/README.md) for the boom-bap counterpart.
