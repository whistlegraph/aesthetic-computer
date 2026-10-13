# Fía's drawings — 2026-10-12

Hand-drawn in ink on grid and dot paper, photographed (8 photos in `photos/`, reduced), and mastered into
69 pieces. Every piece has a `svg/` master (traced, filled with choir's ink `#1c1816`) and a
`png/` master (black ink, soft alpha, ≤1024 px). `review-sheet.png` shows them all; `logo-tryouts.png` puts
the daisies in place as the icon, a header lockup and at small sizes.

## Families

- **daisy-loop** (24): daisy-loop-01, daisy-loop-02, daisy-loop-03, daisy-loop-04, daisy-loop-05, daisy-loop-06, daisy-loop-07, daisy-loop-08, daisy-loop-09, daisy-loop-10, daisy-loop-11, daisy-loop-12, daisy-loop-13, daisy-loop-14, daisy-loop-15, daisy-loop-16, daisy-loop-17, daisy-loop-18, daisy-loop-19, daisy-loop-20, daisy-loop-21, daisy-loop-22, daisy-loop-23, daisy-loop-24
- **daisy-petal-dot** (3): daisy-petal-dot-01, daisy-petal-dot-02, daisy-petal-dot-03
- **daisy-petal-o** (4): daisy-petal-o-01, daisy-petal-o-02, daisy-petal-o-03, daisy-petal-o-04
- **daisy-petal-star** (2): daisy-petal-star-01, daisy-petal-star-02
- **daisy-petal-x** (7): daisy-petal-x-01, daisy-petal-x-02, daisy-petal-x-03, daisy-petal-x-04, daisy-petal-x-05, daisy-petal-x-06, daisy-petal-x-07
- **glyph-asterisk** (2): glyph-asterisk-01, glyph-asterisk-02
- **glyph-bang** (1): glyph-bang-01
- **glyph-c-arrow** (3): glyph-c-arrow-01, glyph-c-arrow-02, glyph-c-arrow-03
- **glyph-c-cross** (1): glyph-c-cross-01
- **glyph-plus** (1): glyph-plus-01
- **glyph-question** (3): glyph-question-01, glyph-question-02, glyph-question-03
- **glyph-x** (6): glyph-x-01, glyph-x-02, glyph-x-03, glyph-x-04, glyph-x-05, glyph-x-06
- **word-chat** (4): word-chat-01, word-chat-02, word-chat-03, word-chat-04
- **word-discover** (3): word-discover-01, word-discover-02, word-discover-03
- **word-reply** (2): word-reply-01, word-reply-02
- **word-send** (3): word-send-01, word-send-02, word-send-03

Families: `word` — Fía's handwriting of the app's own words. `daisy-loop` — Continuous-line daisies, one looping stroke (dot paper). Logo alternatives. `daisy-petal` — Five-petal outline daisies (grid paper); the suffix names the centre: dot, o, x, star. Logo alternatives. `glyph` — Doodads: marks for actions and decoration.

## In the app

| Asset | Master | Where |
|---|---|---|
| `WordSend` | word-send-02 | the chat and comment send buttons |
| `WordReply` | word-reply-01 | reply counts on chat messages |
| `WordDiscover` | word-discover-03 | top-right button; title of the discover sheet |
| `WordChat` | word-chat-03 | discover sheet's way back to the chat |
| `GlyphPlus` | glyph-plus-01 | add-a-tag button |
| `GlyphAsterisk` | glyph-asterisk-01 | desk doodads |
| `DaisyLoop` | daisy-loop-17 | logo try-out, desk doodads |
| `DaisyPetal` | daisy-petal-o-01 | logo try-out, desk doodads |
| `GlyphClose`, `GlyphQuestion`, `GlyphBang`, `GlyphSendC` | x-03, question-03, bang-01, c-arrow-02 | in the catalog, not placed yet |

The logo at the top of the room is chosen in You → Logo (four Cs, loop daisy, petal daisy).

## Versions

`versions/` holds each app pick and logo candidate in choir's three inks — `-ink` `#1c1816`, `-red` `#cf4040`,
`-white` — as SVG and 1024 px PNG.

## Regenerate

From the full-size originals (the Messages attachments, converted to PNG):

```sh
py=pop/.venv/bin/python  # numpy, scipy, PIL; potrace + ImageMagick on PATH
$py apple/choir/Art/tools/extract.py <run> IMG_8395.png … IMG_8402.png
$py apple/choir/Art/tools/master.py <run> apple/choir/Art/fia-2026-10-12 <run-the-kind-map-was-read-from>
$py apple/choir/Art/tools/versions.py apple/choir/Art/fia-2026-10-12 apple/choir/Resources/Assets.xcassets
```

`master.py` holds the kind map (which extracted piece is what) and the drop list; duplicates across the
overlapping photos collapse to the highest-resolution take (`index.json` lists each master's other takes).
