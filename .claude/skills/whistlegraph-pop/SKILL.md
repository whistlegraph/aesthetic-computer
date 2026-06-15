---
name: whistlegraph-pop
description: Download a whistlegraph TikTok and read its musicality (tempo, key, whistled-melody note sequence) as the front of the /pop track pipeline. Use when @jeffrey says "grab this whistlegraph", "analyze this tiktok's audio", "turn this whistlegraph into a pop track", or pastes a tiktok.com/@whistlegraph link.
---

# whistlegraph → /pop

Turns a whistled TikTok into a musical readout you can compose a /pop
track from. Tooling lives in `toolchain/whistlegraph/` (see its README).

## Steps

1. **Grab + analyze.** From a tiktok URL (or `--latest N` from the
   @whistlegraph account):
   ```bash
   node toolchain/whistlegraph/grab.mjs <tiktok-url>
   ```
   This downloads the mp4, extracts a mono WAV, and runs `analyze.py`
   (pop `.venv` librosa) → tempo, key/scale, whistled-melody note
   sequence, onsets → `downloads/<id>.analysis.json` + `INDEX.json`.

2. **Read the melody musically.** The pyin track quantizes a portamento
   whistle, so expect neighbor-note jitter (the `centsOff` column shows
   the glides) — read the *skeleton*, not every blip. Identify the hook,
   its repeats (titles like "… x2" mean the phrase repeats), the longest
   held notes (structural anchors), and the cadence.

3. **Hand off to /pop.** Use `key`, `tempoBPM`, and `melody[]` to compose
   **bottom-up from AC instruments — never Suno end-to-end** (see
   `pop/SCORE.md`, `pop/lib/`). The whistled line is the lead/topline.

## Notes
- `*.mp4`/`*.wav` are gitignored (reproducible from URL); `*.analysis.json`
  and `INDEX.json` are tracked.
- `node toolchain/whistlegraph/grab.mjs --list` summarizes the corpus.
