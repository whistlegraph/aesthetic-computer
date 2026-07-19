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

### The sing core (round 5 — lives IN spinging/lib)

| Module | Role | Spinging surface |
|---|---|---|
| pronounce.mjs | word → definitive GenAm IPA + syllable {onset, nucleus, coda} (Wiktionary API with PER-LINE accent tags — US/GA required, usSafe() rhotic screen; espeak en-us fallback; builtin seeds; cache versioned in spinging/cache/pronounce.json, `--audit` re-resolves it) | `spinging pronounce` + imported |
| notation.mjs | choral score: jingle notes married to syllable underlay + phoneme classes + PHRASE GROUPING (lyric punctuation + melody rests ≥ 0.4 s) — the ground truth the aligner, synthesizer AND legato bridging consume | imported (sidecar per line) |
| sing_line_world.py | the line-continuous WORLD engine: guided alignment (expected consonants anchor bursts; sibilants vouch by high band; nasal murmurs walk the RMS dip), legato bridging inside phrases (sustained vowel through melody rests, gliding f0, shallow sp dip, coda lands pre-onset), voiced sonorant/fricative onsets THROUGH the pitch path (note begins on the consonant, forced voicing, beta-taper 0), per-line octave fit weighted by voiced evidence, 60–300 Hz harvest clamp (de-kermit), nucleus widening to real vowel extent with edge-bleed penalties, unstretched vowel onsets + natural-pitch glides, note-edge contour tapers, monotone source maps, goalpost-conformed drift/vibrato/energy arcs, +1.4× consonant composites, airy sustains, quiet self-choir; stats carry conformance, click scan, per-phrase voicing continuity, voiced-onset f0-jump and per-nucleus voiced fractions | `spinging singline plan.json` |
| vocal_shapes.py | shared feature extraction: note segmentation, shape features, percentile bands, conformance, click scan | imported by goalposts + engine |
| goalposts.py | reference SUNG wavs → percentile shape bands (spinging/cache/goalposts.json; built from the WIYH acapellas — same singer) | `spinging goalposts` |
| vocal_bus.py | Schroeder reverb halo on the vocal bus; standalone click scan | `spinging bus reverb\|scan` |

First caller: `pop/menuband/bin/sing-jingle.mjs` (`--harmony` 0…1 = contour
lock; rounds 3–5 ship at 0.875). The engine re-renders each line with
adjusted tweaks until its measured features sit inside the reference p10–p90
bands and the click scan is clean, then a WHISPER ROUND-TRIP gate transcribes
the rendered line back (per-line WER ≤ 0.25 + all content words, clarity
re-renders on failure, best take kept); the assembled vocal stem is
re-transcribed per line and the final mix once more — transcripts land
verbatim in `out/<slug>-sung-qa.json`. QA is statistical AND machine-read,
not ear-only.

Changelog — round 6.5 (2026-07, the finisher): `sing_line_world.py` R6·5 — fricative codas actually fricate (run-out past the trimmed window + seek across closure gaps: "diminished"'s /ʃt/, "keys"' /z/), phrase-initial unvoiced fricatives stop reaching back across the phrase gap ("keys. Sus" → "kisses"), expected-unvoiced clusters forced onto the noise path, choir tacets on quick/unstressed syllables, mid-phrase word-initial plosives get a 32 ms set-up + prouder bursts ("control"'s /k/) with matching wider bed ducks in sing-jingle.mjs; the scales reel drops its spoken letter run and TTSes the ladder as spelled letter names ("See. Dee. Ee." — bare letters tripped ElevenLabs spelling mode).

Changelog — round 6 (2026-07, the REGISTER round + the SCALES singalong —
"I could be higher octave?"):
- **Register lift** — `plan.register` (semitones) applies AFTER the per-line
  minimal-|shift| octave fit; sing-jingle's `--register` (default +12 this
  round) drives it with a per-line fallback ladder (+12 → +7 → 0): a line
  that misses the whisper gate re-renders lower and the most intelligible
  take wins (ties prefer the higher register); fallbacks land in the QA
  sidecar (`register.fallbacks`). Conformance is register-aware
  (`vocal_shapes.conformance(register=…)`): the f0-linked bands (glide/
  drift/release/vib cents + hf_ratio) widen 35 %/octave and hf_ratio shifts
  with the harmonic comb (×2^(R/24)); duration/energy/click gates unchanged.
- **R6·2 final unstressed syllables stop starving** ("diminished" → "deman"):
  word-final unstressed vowels get a minimum-duration floor (0.14 s, borrowed
  by anticipating the note into the preceding stressed vowel) and word-final
  coda CLUSTERS (≥2 phones) may articulate into the phrase gap (extension cap
  0.05 → 0.18 s). Chords L4 now closes the word ("demand" — the coda exists;
  the full /ʃt/ remains whisper-hostile).
- **R6·3 phrase-boundary silence** ("keys. Sus" → "kisses"): phrase-initial
  FRICATIVE onsets get a real ~100 ms near-zero gap (verified in stats:
  `gap_ms 100`, onset 115 ms) instead of the 22 ms glottal dip.
- **R6·4 phrase-medial onset prominence** ("control" → "Troll"): raw plosive
  composites in a phrase-medial word's onset ride an extra 1.25× on top of
  RAW_BOOST.
- **The scales reel** (pop/menuband/bin/sing-jingle.mjs `menuband-scales`) —
  spoken lines placed verbatim at absolute times (level-matched to the sung
  material, whisper word timings → captions; all three spoken lines
  transcribe at WER 0) around the sung notepat letter ladder: explicit
  pitches C3..B4, `octave_opt` off, per-letter phrases via a notation-only
  text (letter names + commas) so the ladder articulates instead of
  smearing into one glissando; letter-name lyrics ("see dee ee …", curated
  /eɪ/ "ay" + /aɪ/ "eye" in pronounce.mjs — CURATED wins over cache) and
  letter-folded WER scoring ("C-D-E" == "see dee ee").
- Result: every line CAN now ride an octave above round 5 (13/19 sung
  campaign lines hold +12, 3 land +7, 3 fall to +0 — all reported); scales
  descending ladder hits WER 0.154 (11/13 letters) with the ascending run
  flagged honestly (whisper hears sustained sung letter-vowels as melody);
  clicks 0 and conformance green on every shipped line.

Changelog — round 5 (2026-07, the DICTION round — consonant time-stretching
the way trained choirs handle it; round 4's whisper gate was failing on
swallowed consonants, not on the singing):
- **Consonant time-stretching** — every onset/coda window partitions into
  homogeneous runs: 'pitch' (voiced → WORLD pitch path, ~1.7×), 'noise'
  (unvoiced fricative-ish → WORLD noise synthesis, ~2.0×), 'raw' (plosive
  closure/burst → 1:1 composite, now 1.5×, never stretched). Affricates
  stretch only their fricative frames for free via the runs partition. New
  tweak knob `cons_stretch_scale` (clarity re-renders lean it to 1.3, engine
  hard-caps 2.5×).
- **Glottal set-up gaps** — ~22 ms pre-plosive dips before every plosive/
  affricate onset AND at phrase-initial consonants (a stretched phrase-
  opening /s/ otherwise welds onto the previous coda — "keys. Sus" →
  "kisses"); carved into lead + choir, excluded from the continuity gate.
- **Vowels own the beat** — ALL onsets (voiced included, revising R4·2) end
  AT the note's beat; stretch is stolen from preceding vowel tails and
  bridge sustains (reserve = stretched onset + gap). Word-final codas take
  full value, borrowing up to 50 ms past hardEnd at phrase ends. Line-
  opening words with no runway push into the note (vowel late, R4-style).
- **Guided alignment, symmetric seeks** — the found nucleus often misses the
  real vowel edges: guide_onset now SEEKS back across the word's own under-
  found vowel frames ("store"'s /st/ sat 110 ms before its nucleus),
  guide_coda walks BACK out of over-grown nuclei ("band"'s æ ate its /nd/ →
  "Ben") and seeks FORWARD past leftover vowel frames; nuclei re-trim/extend
  to match, gated on real vowel ENERGY (harvest voicing alone also marks
  near-silent breathy tails — mapping those across a held note went dead
  mid-phrase). Voiced plosives anchor on their burst (the murmur walk used
  to swallow the previous vowel's tail). Nuclei shrink to their longest
  energetic run (merged regions can hide internal silence).
- **Choir gated to vowels** — the self-choir tacets on consonant frames and
  glottal gaps (choirs unify on vowels; the LEAD carries diction) and each
  layer sits 1.5 dB lower; consonant frames get +3 dB spectral prominence;
  stats expose `consonant_spans` and sing-jingle drives an extra ~5 dB bed
  duck under them (the level sidechain ducks least exactly when the lead is
  quietest — its consonants). De-esser eased 0.4 → 0.15 so stretched
  sibilants survive the vocal bus.
- **Deterministic best-take** — every render (QA passes and clarity passes)
  is whisper-transcribed; the best take wins, conformance FIRST (the
  goalposts arbitrate — musicality before WER), then WER. Conformance QA
  measures VOWEL notes only (leading stretched-murmur frames trimmed,
  consonant-dominated notes dropped); vibrato hold threshold 0.6 → 0.45 s
  (stretched codas shortened vowels). WER scorer gains `rewedge` (whisper's
  "full-screen" vs ref "fullscreen") and a per-line LEAD-ONLY diagnostic
  transcript separates diction gains from choir masking.
- Result: 6/19 lines pass the whisper render gate (round 4: 4/19; chords
  "Command option. Sus two." and features "Goes fullscreen." land at WER 0),
  no round-4 pass regressed, all hard gates green (conformance, continuity
  ≥ 0.95 vowel-stream, clicks 0, onset jumps ≤ 27¢, −14 LUFS / ≤ −1.4 dBTP).
  Missing-content-word counts drop broadly (announce L5 recovers "menu
  band", L4 recovers "now"+"mac"); melisma lines remain whisper-hostile and
  ship with FAIL marks recorded honestly.

Changelog — round 4 (2026-07, per jeffrey's round-3 review):
- **Legato bridging** ("some words are too disconnected") — notation.mjs owns
  phrase grouping (punctuation + melody rests ≥ 0.4 s); the engine sustains
  vowels through intra-phrase gaps with gliding f0 and a shallow energy dip,
  breathes only at phrase edges; karaoke word windows ride the bridges.
  Gated: per-phrase voicing continuity ≥ 95 % (ships 0.966–1.0).
- **Voiced onsets through the pitch path** ("mac — m and ac don't connect") —
  nasal/liquid/glide/voiced-fricative onsets begin ON the note, forced
  through WORLD (never raw-composited at spoken pitch), f0 continuous into
  the vowel; voiced plosives keep their raw bursts. Nasal murmur capture
  fixed (was zero-length onsets). Gated: boundary f0 jump (ships ≤ 27¢).
- **Alignment repairs** ("synthesizer is all messed up") — whisper
  punctuation tokens no longer smear timestamps across trailing silence;
  trailing zero-width words reclaim the post-host audio instead of
  re-splitting the host at an in-word energy dip; adjacent word windows and
  nucleus searches can't overlap (the "mac app" double-sing); nucleus finding
  drops quiet-schwa-killing thresholds and penalizes right-edge bleed-ins.
  Reported: per-nucleus voiced fractions in the stats.
- **US IPA** — the Wiktionary accent tag lives in the ADJACENT template
  ({{enPR|stôr|a=GA}}), not inside {{IPA}}; round 3 therefore cached RP
  ("store" /stɔː/, "chord" /kɔːd/). Round 4 reads accents per line, requires
  US/GA (usSafe screen otherwise), falls back espeak en-us; all 55 cached
  words audited, 12 corrected.
- **Whisper round-trip gate** — see above; whisper-cli (ggml-base.en) on
  every rendered line + the stem + the mix, transcripts verbatim in the QA
  sidecars. Sung-choir audio remains genuinely hard ASR; lines that still
  miss the 0.25 WER bar ship with their transcripts and FAIL marks recorded
  honestly (round 4 strictly improves on round 3 line-for-line).

### Spoken → sung (WORLD, older lanes)

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

- ~~Adopt the menuband sing engine as the spinging/lib sing core.~~ **DONE
  (round 3, 2026-07):** the line-level engine now lives at
  `spinging/lib/sing_line_world.py` with `pronounce.mjs` / `notation.mjs` /
  `vocal_shapes.py` / `goalposts.py` / `vocal_bus.py` beside it;
  `pop/menuband/bin/sing-jingle.mjs` imports from spinging and is the first
  caller. The old round-2 `pop/menuband/bin/sing_line_world.py` and the
  word-level `sing_word_world.py` stay in place as history. Next: repoint
  `spinging sing` (score-pitch.mjs) at the new core for non-jingle lanes.
- Rebuild `spinging/cache/goalposts.json` when better/drier reference
  acapellas land — the bands are only as good as the references.
- Unify autotune forks (pop/jungle/bin/autotune.py is a local variant of
  pop/bin/autotune.py).
- Give `master` a target flag so sung output hits -14 LUFS / -1.0 dBTP without
  borrowing the podcast default.
- `live/` beat tracking (BPM from `ac-m4l/` daw:tempo) and a true phoneme
  aligner for its vowel heuristic — noted in live/README.md.
