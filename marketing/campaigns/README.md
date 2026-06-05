# Widescreen narrated application videos

A reusable pipeline for 1920×1080 narrated proposal/application videos:
illustrated per-segment slides with **Ken Burns** zoom, top-left titles with
**slide-number superscripts**, a top-right bouncing **wordmark**, a karaoke
**word-train** caption layer, and a narration + ambient-bed audio mix. A
matching single-page **broadside storyboard** PDF falls out of the same config.

Serpentine (`serpentine-video/`) is the template. The engines are generic; each
video is just a config + illustrations.

## Engines (generic — don't edit per-campaign)

- `marketing/bin/compose-widescreen.mjs <campaign-dir>` — builds the .mp4
- `marketing/bin/captions-train.mjs <campaign-dir>` — the word-train caption .mov (called by compose)
- `marketing/bin/storyboard-widescreen.mjs <campaign-dir> [--open]` — the broadside PDF

## A campaign = a folder under `marketing/campaigns/`

```
my-video/
  campaign.mjs            # config (see serpentine-video/campaign.mjs)
  01_intro/
    gens/native.png       # the illustration (or pencil.png fallback)
    cover-prompt-native.txt# the gen prompt (shown in the storyboard)
  02_…/ …                 # one folder per segment, named to match the recap audience
```

Segment **names + timings** come from the recap audience, not the campaign —
the folder names must match the segment `name`s in `recap/out/segments.json`.

## How to make a new one

1. **Author the narration** as a recap audience: `recap/audience/<name>.mjs`
   (narration text + segment markers + on-screen slide text). Set
   `voice.speed` (~1.08 reads well) and `sing: false`.
2. **Build the transcript/audio:** `node recap/cli.mjs build <name>` →
   writes `recap/out/{recap.mp3, words.json, segments.json}`.
3. **Make the illustrations:** one `gens/native.png` per segment folder
   (1024×1536 portrait works; it's cover-cropped to 1920×1080). The pop-style
   prompt convention lives in `serpentine-video/STYLE.md`.
4. **Write `campaign.mjs`** (copy serpentine's): `name`, `audience`, `out`,
   `bed`, `palette`, `wordmark`, `titles{}`, `textFixes[]`, and `storyboard{}`
   metadata (titles/onscreen/markers/beats/spine).
5. **Compose:** `node marketing/bin/compose-widescreen.mjs marketing/campaigns/<name>-video`
6. **Storyboard:** `node marketing/bin/storyboard-widescreen.mjs marketing/campaigns/<name>-video --open`

## Tunables (optional, in `campaign.mjs`)

- `caption: { smooth, activePx, nearPx, ghostPx, pop, spread, gap, bandY }` —
  word-train scaling + scroll feel. `smooth` lower = smoother/laggier scroll
  (default 0.18). `activePx`/`nearPx`/`ghostPx` set the size gradient.
- `palette: { cream, green, shadow }` — text + glow + shadow colors.
- `wordmark: [left, right]` — top-right wordmark (omit to skip it).

## Gotchas

- **node-canvas cannot render the YWFT font** (renders `.notdef` tofu) — that's
  why every glyph is pre-rendered via ImageMagick. Don't "simplify" it back to
  `fillText`.
- Recap's `recap/out/` is shared scratch — build one audience, then compose,
  before building another.
- Intermediates live in `/tmp/wsvideo-<name>/` (word PNGs, captions.mov, the
  ffmpeg filter graph + `cmd.sh` for debugging).
