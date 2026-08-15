# cult remix — cover (real footage, no gen)

Per @jeffrey (2026-08-15): the cover uses **actual whistlegraph footage**, not
AI-generated art. All frames come from the cult performance archive on the
assets CDN (`assets.aesthetic.computer/whistlegraph/index/posts/<id>.mp4`),
indexed by `system/public/whistlegraph.org/posts.json`.

Three 3000×3000 drafts, straight crops + light grade (contrast/saturation via
ffmpeg `eq`), no compositing:

| file | source post | what it is |
| --- | --- | --- |
| `cover-a-glyph.jpg` | `7055106286232325423` (the 39.6M original, 2022-01-20) | the finished cult glyph in white chalk on green, dust smear above, fingertips entering the bottom edge |
| `cover-b-signal.jpg` | `7071087615948148010` ("he he he ha ha ha?", 11.4M) | UV-night version — pink/green/orange glyphs around the black void, blacklit concrete |
| `cover-c-morse.jpg` | `7055106286232325423` (t≈4.3s) | just the "dot dot dot" — iii in chalk, one hand entering with the stick |

Working frames + per-video contact sheets in `source/` (six cult videos
downloaded 2026-08-15). Source is 720×1280, so a square crop is 720² upscaled
4.2× — fine for drafts; for the shipping master, pull the original capture
from the whistlegraph platter Dropbox archive (`papers/whistlegraph-platter/`)
for real resolution.

Rules that apply (`feedback_pop_cover_rules`): no readable brand wordmarks
(none present — chalk only), and covers are **drafts-only** — never auto-push
to the CDN; @jeffrey picks before anything ships.
