# Essay reels — podcast readings → Reel / Story

This lane turns a produced episode in `marketing/podcast/out/` into a vertical
social excerpt. It uses the actual mastered Jeffrey reading, transcribes the
chosen excerpt for word timing, and renders a furniture-free 1080×1920 video.

The visual form is deliberately native to the reading series: the square
episode cover becomes a slowly breathing paper field; the current phrase sits
large and legible in the safe center; each spoken word fills green as it lands;
and a small audio-reactive ring makes the recording feel alive without turning
the essay into a slideshow.

```fish
node marketing/essay-reels/bin/render.mjs granularity \
  --start 7 --duration 45 --open
```

Inputs (by slug):

- `marketing/podcast/out/<slug>.mp3`
- `marketing/podcast/out/<slug>.json`
- `marketing/podcast/out/<slug>-cover.png`

Outputs:

- `marketing/essay-reels/out/<slug>-reel.mp4`
- `marketing/essay-reels/out/<slug>-story.mp4` (same master; named for upload)
- `marketing/essay-reels/out/<slug>-words.json`

Flags: `--start N`, `--duration N` (default 45), `--out path`, `--open`.
Keep essential type inside the central safe area: Instagram overlays controls at
the top and bottom. There is no baked progress bar or timecode; the host app
supplies its own furniture.
