# kidlisp-reels/

Turn a live KidLisp piece (e.g. `$tezz`) into a 9:16 Instagram-Reel / TikTok
with AC's **side-stamp chrome** — the same pals-watermark + climbing-title
treatment as the /pop and Menu Band reels. Built from two pieces of recent
work that snap together:

- **capture half** ← `marketing/bin/capture-ac-platter.mjs` (puppeteer drives
  live `aesthetic.computer/$code`, boots it, captures the running piece).
- **chrome half** ← `pop/menuband/bin/chrome-reel.mjs` + `pop/lib/preview-shared.mjs`
  (two pals stamps hugging the edges, 4-pass seep, LED pulse, title columns).

## Pipeline

```
reel.mjs '$tezz'
  └─ bin/capture-kidlisp.mjs '$tezz'   # → out/tezz/frames/*.png + capture.json
  └─ bin/render-reel.mjs   '$tezz'     # → out/tezz/tezz-reel.mp4
```

```bash
node marketing/kidlisp-reels/reel.mjs '$tezz'                 # default 12s @ 30fps
node marketing/kidlisp-reels/reel.mjs '$tezz' --duration 15 --density 3
node marketing/kidlisp-reels/reel.mjs '$tezz' --render-only   # re-chrome cached frames
```

## How it stays clean

The piece is captured with query params that strip the in-piece HUD — **the
reel's side-stamps carry the `$code` to type in, so the source label in the
top-left is hidden** (`system/public/aesthetic.computer/boot.mjs`):

- `nolabel` — hide the top-left source/HUD label
- `nogap` — piece fills the frame
- `tv` — disable input (steady display capture)
- `density=N` — chunky pixels (2–4 reads well as a reel)
- `spoofaudio` — synthetic amplitude so audio-reactive pieces still animate headlessly

## Silent by design

Reels render **silent** — audio is added at post time (in the IG/TikTok
composer). Because there's no audio envelope, the pals' LED pulse is driven by
the piece's own **visual motion** (frame-to-frame luma delta), so the stamps
still breathe with the animation. The stamp tint is sampled from the piece's
average color.

## Layout

1080×1920. The square piece sits full-width in the center band over a blurred,
darkened cover of itself; pals stamps + `$code` columns climb the left/right
edges. No progress bar / timecode — Reels bring their own furniture.

## Outputs

`out/<slug>/` is the working dir per piece (frames + assets + mp4). Frames are
regen-able from the live piece, so `out/` is gitignored per the marketing
README convention.

## TODO / v0.2

- Frame-exact determinism via CDP virtual-time (today: wall-clock screenshot loop).
- Seamless loop option (match first/last frame for a clean Reel loop).
- Optional caption band / hashtag furniture toggle.
