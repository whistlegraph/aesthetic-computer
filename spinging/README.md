# spinging

> "moving spoken audio — metadata'd up and generated — into singing, out of
> text." — @jeffrey

Text → generated spoken TTS audio → enriched with metadata (word/phoneme
boundaries, f0, timing) → lifted into singing. This directory is the one
canonical stack for every vocal tool in the repo. The tools themselves mostly
stay where they grew up (their lanes depend on those paths); spinging is the
map, the door, and — where a module is clean enough — the home.

    node spinging/bin/spinging.mjs help
    node spinging/bin/spinging.mjs doctor

## The canonical chain

1. **say** — text → spoken stem via `/api/say` (provider `jeffrey`; voice ids
   live server-side and in the vault, never in this repo). Stability ≥ 0.5
   keeps identity; `say-dry` validates dryness and retries.
2. **metadata** — whisper-cli word boundaries (`align`), librosa onset snap
   (`refine`), audio-side phonetic events (`phonemes`), speaker f0 floor
   (`floor` — run per source, the floor must track the speaker).
3. **sing** — phoneme-aware WORLD synthesis. Plosives/consonants keep natural
   length and act as onsets ahead of the beat (the p-center); the note lives on
   the lengthened vowel nucleus; pitch is applied **relatively** — detect the
   take's own fundamental and shift by the interval into jeffrey's baritone,
   never absolute-snap — with continuous line-level WORLD f0 replacement
   (envelope untouched, so it stays his voice).
4. **master** — finish to **-14 LUFS / -1.0 dBTP** for sung/music output
   (`spinging/lib/master.mjs` `SING_TARGET`; the podcast spoken-word target is
   -16 / -1.5).

## Inventory (tool → old home → spinging surface)

### Text → spoken audio

| Tool | Home | Role | Spinging surface |
|---|---|---|---|
| say.mjs | pop/bin | lyric file → /api/say spoken stem, content-hash cache | `spinging say` |
| say-dry.mjs | pop/bin | say + dryness validation, auto-retry until ratio < 0.04 | `spinging say-dry` |
| profile-vocal.mjs | pop/bin | measure render dryness (floor/active ratio) | via say-dry |
| perword.mjs | pop/bin | per-word ElevenLabs takes with gaps | in place (say lane) |
| best-of-takes.mjs | pop/bin | multi-take generation, pick best word occurrence per slot | in place (say lane) |
| tts.mjs | recap/bin | recap audience narration → recap.mp3 | `spinging tts` |
| fetch-pvc-samples.mjs | pop/bin | audit the PVC training corpus for dryness | in place |
| say.js | system/netlify/functions | the /api/say endpoint itself (providers: jeffrey/openai/google; DO Spaces cache) | referenced only — server stays put |

### Metadata (boundaries, onsets, phonemes, f0)

| Tool | Home | Role | Spinging surface |
|---|---|---|---|
| align.mjs | pop/bin | whisper-cli per-word timestamps for any stem | `spinging align` |
| align-words.mjs | **spinging/lib** (moved from pop/bin) | reconcile whisper words vs score words (fuzzy match/merge/skip) | canonical lib module; pop/bin path is a shim |
| refine_words.py | pop/bin | snap whisper boundaries to librosa onsets | `spinging refine` |
| refine_onsets.py | pop/bin | QA: score starts vs detected onsets | in place |
| detect_onsets.py | pop/bin | librosa onset list for slicing | in place |
| mfa-align.mjs | pop/bin | forced alignment via Needleman-Wunsch on lyric words | in place |
| phonetics.mjs | recap/bin | audio-side silence/voice/plosive event map | `spinging phonemes` |
| floor.py | live/bin | measure a speaker's f0 distribution → pick f0_floor | `spinging floor` |
| pitchcheck.mjs | pop/bin | audit rendered f0 vs intent, drift in cents | `spinging check` |
| realign-from-whisper.mjs | pop/bin | retime visuals from whisper on the final mix | in place (big-pictures) |
| identify-speaker.py | pop/bin | resemblyzer speaker-similarity windows | in place |

### Spoken → sung (WORLD)

| Tool | Home | Role | Spinging surface |
|---|---|---|---|
| pitchsnap.mjs | pop/bin | per-word grid snap + pitch to .np score | `spinging snap` |
| pitchsnap_world.py | pop/bin | WORLD f0-replacement on a slice (absolute notes; `--f0-floor` flag) | `spinging worldsnap` |
| score-pitch.mjs | pop/bin | ONE line-level WORLD pass — whole vocal sings the score, timing intact | `spinging sing` |
| autotune.py | pop/bin | WORLD scale-snap (note mode = Melodyne-ish, frame mode = hard) | `spinging autotune` |
| pitchwords.mjs | pop/bin | per-word rubberband pitch to .np note | in place |
| timefit.mjs | pop/bin | stretch/compress stem to bar count | in place |
| melody-bells.mjs | pop/bin | .np score → sinebell melody double | in place |
| vocal.mjs / autotune.mjs / sing.mjs | recap/bin | recap-side sung-narration variants | in place (recap lane) |

### Sources (other voices in)

| Tool | Home | Role | Spinging surface |
|---|---|---|---|
| **live/** (whole lane) | repo root | realtime speech-to-singing for DJing — talking heads sing (WORLD f0 replacement, chunked analysis, 564ms to sound; singer.c is the embeddable core) | corralled: `spinging ingest` + `spinging floor` |
| ingest.mjs | live/bin | youtube url → yt-dlp → whisper words → onset-refined words.json | `spinging ingest` |
| sample-from-youtube.mjs | pop/bin | youtube → onset-aligned chops + global sample index | `spinging sample` |
| separate-stems.mjs | pop/bin | demucs stem split (vocals out of a mix) | `spinging stems` |

### Finishing

| Tool | Home | Role | Spinging surface |
|---|---|---|---|
| master.mjs | marketing/podcast/bin | EQ + glue + 2-pass loudnorm + TP limit | `spinging master` + re-exported by `spinging/lib/master.mjs` (with `SING_TARGET` -14/-1.0) |

### Environment

- **Python**: `pop/.venv` — pyworld 0.3.5, librosa 0.11.0, soundfile,
  resemblyzer. Every python route in the CLI runs under this interpreter.
- **Whisper model**: `recap/models/ggml-base.en.bin` (whisper.cpp).
- Voice identity: provider + vault only. No voice ids in this repo.

## Move-by-wrapper ledger

Moved into spinging/lib (old path = shim):

- `pop/bin/align-words.mjs` → `spinging/lib/align-words.mjs` (self-contained,
  one export; the shim re-exports `alignWords`).

Re-exported FROM their homes (too entangled to move):

- `marketing/podcast/bin/master.mjs` → `spinging/lib/master.mjs`
  (produce.mjs + kits.mjs import it in place).

Everything else is a pure CLI with lane-local spawners (say.mjs has 40+,
pitchsnap_world.py 30+) — those stay put and are reached through
`spinging/bin/spinging.mjs` routes.

## TODO — pending adoption

- **Adopt the menuband sing engine as the spinging/lib sing core once the
  smoothing pass lands.** `pop/menuband/bin/sing-jingle.mjs` +
  `sing_word_world.py` are today's phoneme-aware sung-TTS engine (relative
  pitch mapping, consonant preroll, β-contour retention) and are the closest
  expression of the canonical chain — but they are being actively revised
  right now (along with a new line-level `sing_line_world.py` sibling), so
  spinging references them (`paths.mjs` → `singJingle` / `singWordWorld`)
  without routing or importing. When they settle: lift the
  word-level engine into `spinging/lib/sing.mjs` + `spinging/lib/sing_word_world.py`,
  shim the menuband paths, and repoint `spinging sing`.
- Unify autotune forks (pop/jungle/bin/autotune.py is a local variant of
  pop/bin/autotune.py).
- Give `master` a target flag so sung output hits -14 LUFS / -1.0 dBTP without
  borrowing the podcast default.
- `live/` beat tracking (BPM from `ac-m4l/` daw:tempo) and a true phoneme
  aligner for its vowel heuristic — noted in live/README.md.
