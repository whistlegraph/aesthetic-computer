# whistlepops corpus

Training set for `meetings/detect-whistlepops.mjs` — the detector that finds
whistle events in meeting recordings and labels them so the LaTeX builder can
turn them into section breaks, decision callouts, highlight blocks, etc.

## Grammar that this corpus is meant to train

- **lone whistle** (`single`) → punctuation: section break / beat
- **whistle pair** (`pair-open` … speech … `pair-close`) → whistlepop:
  the speech between two whistles is interpreted as a directive to the
  pipeline, not part of the conversation transcript. See `meetings/dsl.md`.

The acoustic classes the detector learns are the actual sounds, not the
grammatical roles — pairing is a post-pass over event timestamps. Classes:

- `short` — quick tweet
- `medium` — ~1s single pitch
- `long` — 2–3s sustained
- `rising` — two-tone, pitch up
- `falling` — two-tone, pitch down
- `quiet` — soft, close-mic
- `far` — distant, room-mic

Negatives guard against false positives the detector is likely to confuse
with whistles (vowels in the same band, hums, laughs, kettles, fan whine,
keyboards).

## How to record

```bash
cd wave-wizard
swift run WaveWizard samples/whistlepops/spec.json
```

The wizard walks the sample list, speaks the prompt, waits for an onset,
records, auto-trims, then offers Keep / Retry / Skip. Output WAVs land in
`takes/` named after the sample (`pos-short-01.wav`, …).

Goal: 3 takes of every positive variant + 3 takes of every negative class.
~40 prompts × ~15s of effort each ≈ 15–20 minutes of focused recording.

## How the detector uses this

`meetings/detect-whistlepops.mjs` loads every WAV under `takes/`, infers the
class label from the filename prefix (`pos-*` vs `neg-*`, then the variant),
extracts a feature vector (peak freq, bandwidth, harmonic ratio, sustain
duration, attack time, peak band concentration), and fits a small classifier
(k-NN or logistic regression — TBD once we see how separable the features
are). At inference time it slides over a meeting WAV, scores each window,
emits `[{ kind, t_start, t_end, confidence }]` events.

Acoustic variants (`short` / `medium` / `long` / `rising` / `falling`) are
preserved on output so a future DSL revision can use them as distinct
verbs without recapture (e.g. rising = "open", falling = "close").
