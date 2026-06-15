# whistlegraph toolchain

Pull a whistlegraph TikTok and read its musicality — the front of the
pipeline that turns a whistled clip into a [/pop](../../pop/) track.

## Usage

```bash
cd toolchain/whistlegraph

node grab.mjs <tiktok-url>          # download + analyze one clip
node grab.mjs --latest 3           # newest 3 from @whistlegraph
node grab.mjs --account @someone --latest
node grab.mjs <url> --redo         # refetch + re-analyze
node grab.mjs --list               # summarize downloads/INDEX.json
```

Each run:
1. `yt-dlp` → `downloads/<account>-<id>.mp4`
2. `ffmpeg` → mono 44.1k `.wav`
3. `analyze.py` (pop `.venv` librosa) → tempo, key, whistled-melody note
   sequence + onsets → `<id>.analysis.json`
4. updates `downloads/INDEX.json`

## What's tracked

`*.mp4` / `*.wav` are gitignored (reproducible from the URL). The small
`*.analysis.json` and `INDEX.json` are committed so the melody read
survives without re-pulling from TikTok.

## → /pop

Take the `key`, `tempoBPM`, and `melody[]` from the analysis JSON and
compose bottom-up from AC instruments (never Suno end-to-end). See
`pop/SCORE.md` and `pop/lib/`.
