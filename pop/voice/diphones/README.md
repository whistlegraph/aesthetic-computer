# diphones — the shippable AC-native jeffrey synth

The pivot, 2026-05-04: **Pink Trombone is a vocal-tract instrument, not a speech synthesizer.** It can't reach phoneme-level intelligibility from photogrammetric priors + speech samples alone. Confirmed audibly on Desktop's `pt-word-eye-fitted.wav`.

This lane reframes around a different question: *what would it take to ship a tiny jeffrey-voiced TTS that runs on AC-native hardware?*

## Approach: jeffrey-pvc-distilled diphone bank

Use **ElevenLabs jeffrey-pvc as the teacher.** Generate a corpus of carrier sentences chosen to elicit every English diphone (≈1500 transitions). Forced-align with WhisperX (already in the recap pipeline per memory) to extract each ~50 ms slice. Store as a flat lookup: `diphone[a, b] → 50 ms WAV`. Concat with crossfade at synth time.

Why this and not Pink Trombone:
- **It IS jeffrey.** Diphones are jeffrey's actual voice in fragments, not a model's approximation
- **Tiny shippable size** — 1500 diphones × 50 ms × 22050 Hz × 2 bytes ≈ 3.3 MB raw, less under Opus
- **No NN at runtime** — pure DSP concat, runs trivially in C/WASM
- **AC-native by construction** — every diphone is URL-addressable: `assets.aesthetic.computer/jeffrey-diphones/<a>_<b>.opus`
- **The recap pipeline already has WhisperX** — we reuse the forced-alignment lane

Why not (TBD: revisit if diphone bank fails):
- A tiny RNN/conv vocoder distilled from jeffrey-pvc would be more *natural-sounding* across novel words but adds inference cost + training complexity
- HTS-style HMM parametric synth is proven shippable (Festvox lineage) but the training pipeline is involved

## Cost estimate

ARPABet has 39 phonemes + start/end markers ≈ 41². That's 1681 ordered pairs but most are illegal (English doesn't have /ŋ_t/ word-initial, etc.). Realistic English diphone count: **~1500**.

Each carrier sentence carries ~10–15 diphones. 1500 / 12 ≈ **~125 carrier sentences**, each ~5 words long. ElevenLabs charges ~30k chars/$5 of credits at jeffrey-pvc rates, so:

- 125 carriers × ~25 chars = ~3125 chars
- That's ~$0.50 of credits
- Plus regenerations, alignment passes, retries → call it ~**$2-5 in ElevenLabs**

This is the cost gate. Negligible compared to a real corpus.

## Pipeline

```
diphone-targets.json     (the master list of 1500 ARPABet pairs)
        ↓
build-carriers.py        (assigns pairs to 125 carrier sentences)
        ↓
carriers.json            (sentence text + which diphones it carries)
        ↓
record-carriers.mjs      (jeffrey-pvc /api/say with timestamps)
        ↓
carriers/raw/*.mp3       (~125 mp3s)
        ↓
align-carriers.py        (WhisperX forced alignment per phoneme)
        ↓
diphone-extraction.py    (slice carriers into diphones, ~50 ms each, crossfade-ready)
        ↓
diphones/<a>_<b>.opus    (~1500 tiny files)
        ↓
synth.{c,mjs,wasm}       (concat + crossfade at synth time)
```

Every step caches by content hash. Reruns cost $0 unless inputs change.

## File map (planned)

```
diphones/
  README.md                     this file
  diphone-targets.json          1500 ARPABet pairs + frequency weight
  carriers.json                 carrier sentences + diphone mapping
  bin/
    build-carriers.py           pair → sentence packing
    record-carriers.mjs         /api/say recorder for carriers
    align-carriers.py           WhisperX forced alignment
    extract-diphones.py         slice carriers into diphone WAVs
    synth.mjs                   reference TTS implementation
  raw/                          carrier mp3s (gitignored)
  aligned/                      WhisperX outputs (gitignored)
  bank/                         <a>_<b>.opus diphone files (gitignored; published to CDN)
  manifest.json                 phoneme → URL/path map (committed)
```

## Status

- 2026-05-04 — lane scaffolded.
- 2026-05-04 — `diphone-targets.json` committed: 1527 ARPABet pairs, 532 in tier-1 (vowel-vowel + CV/VC, the load-bearing perceptual ones).
- 2026-05-04 — `carriers.json` committed (v2 paragraph-form, 10 paragraphs, 2326 chars, ~$0.39 ElevenLabs). `raw/p000..p009.mp3` recorded.
- 2026-05-04 — `align-paragraphs.py` working. torchaudio.pipelines.MMS_FA + CTC forced_align + g2p_en for ARPABet. 1510 phonemes aligned across 168.7s of audio.
- 2026-05-04 — `extract-diphones.py` working. 485 unique diphones in `bank/<a>_<b>.wav`, 3.2 MB total. `manifest.json` committed.
- 2026-05-04 — `synth-word.py` working. Concat-synth proof on 10 words → `~/Desktop/voice-lab/jeff-synth/`.
- known gap: word-boundary diphones (`#B_X`, `X_#E`) systematically missing. Paragraphs join words without silence in alignment, so no boundary phonemes get tagged. Fix in next pass: insert silence padding at word boundaries during extraction, or fallback to half-slices.
- coverage: 175/532 tier-1 (33%), 480/1527 all-tier (31%). To hit ~90% tier-1, add ~30 more paragraph carriers (~$1.20 spend).
- next: listen-test the synth; if quality is there, expand carriers + close boundary gap; if not, swap to a true phoneme-output wav2vec2 model and re-align.

## Predecessors

- **Festvox / Festival** — diphone-based concatenative synthesis, the OG approach (Black, Taylor, Caley 1998+). Most "voice cloning" tools today are NN-based; diphone is a deliberate choice for shippability.
- **MBROLA** — diphone synthesis runtime, lots of language-specific voices, GPL.
- **eSpeak NG** — formant-based, lower quality but tiny (~1MB binary). Inspirational for size, not for jeffrey-fidelity.
- **AC-native formant synth** (dropped 2026-05-03 per memory) — the "sounded like tones" attempt. Diphone bank is the structural fix because jeffrey's actual voice replaces the tone generator.

## Why not the PT lane (predecessor in this same `pop/voice/` dir)

`pop/voice/PHYSIOLOGY.md` and `MODEL-EXTENSIONS.md` document a complementary research path: anatomically-grounded Pink Trombone fits. Those measurements are still valuable — they bound jeffrey's vocal-tract length, lip aperture, etc., which inform what diphone *durations* and *pitch ranges* are realistic for him. But PT itself is reframed: **timbral instrument for big-pictures musical layers, not the speech engine.**
