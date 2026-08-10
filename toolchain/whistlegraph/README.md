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

## ⚠️ Re-posting these clips to Instagram

`grab.mjs` pulls via `yt-dlp`, so the `.mp4` usually carries the **TikTok
watermark**. As of Instagram's April 2026 originality expansion, visible
cross-platform watermarks make a Reel **ineligible for recommendation to
non-followers** — so a watermarked TikTok export posted to IG gets no
cold-start reach. If funnelling @whistlegraph clips to IG, re-render from a
clean (watermark-free) source, not the yt-dlp download. Background + sources:
`social/research/instagram-official-guidance.md`.

## The machine-readable index

`gen-llms.mjs` publishes the archive for LLM and agent readers, generated from
the same `graphs.json` / `posts.json` the site renders — so the machine view can
never drift from the human one.

```bash
node gen-llms.mjs --dry     # report sizes + counts, write nothing
node gen-llms.mjs           # write into system/public/whistlegraph.org/
```

It writes three files:

| File | What it is |
|---|---|
| `llms.txt` | The [llms.txt](https://llmstxt.org) convention — a short linked map. The first thing an agent reads. |
| `index.md` | The entire index as Markdown: every confirmed work, candidate, legacy code, and alias, with resolved score/video URLs. Also served at `/llms-full.txt`. |
| `robots.txt` | The licensing assertion, plus pointers to the above and to the paid endpoints. |

Prose lives in `llms-prose.md` and is spliced in at its `SLOT` markers — **edit
that file, never the generated `index.md`.** Rerun after every `gen-model.mjs`.

The free tier is deliberately everything: `graphs.json` and `posts.json` are
already served unauthenticated, so gating the same facts behind a paywall would
be theater. What is sold instead lives in
`system/netlify/functions/whistlegraph-llm.mjs`, metered with
[x402](https://x402.org) — bulk export, the per-work source-video audit trail,
and signed redistribution licenses. That function fails closed: with no wallet
configured it answers 503 rather than serving paid data for free.
