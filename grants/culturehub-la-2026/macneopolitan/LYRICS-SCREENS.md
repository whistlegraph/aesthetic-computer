# Lyrics on every screen — the room's one language

For the MacNeoPolitan Trio's pieces, every screen in the room shows the words
the same way. Drafted 2026-09-24 for the 7:00 show.

## The rules

1. **Headline = the lead line being sung.** Each lyric line carries the
   score's own role, `lead` or `hum` (`lineRoles`, carried into `plan.lyrics`
   by `bin/trio-fleet-plan.mjs`). The current lead line is the headline: as
   large as fits in three rows, the **singer's name small above it in their
   colour**, the **next lead line faint beneath**. Syllables light as they are
   sung (`syllables[].t`); unsung syllables are dim.
2. **The answer is shown more.** A line is an `answer` when the piece names
   it ("the answer is four"), or — in a piece that names none — when it is
   the middle of a call and response: a lead line by one member following
   another member's lead line within a bar and followed within a bar by a
   third member's line. An answer is drawn one size bigger, framed in a tint
   of the room colour with a bar beneath, **flashes to white for its first
   quarter second**, and **lingers 2.5 s** after it ends (other lines 0.8 s).
3. **Hums are texture, not words.** `ooh ooh`, `dm dm dm dm`, `hmm hmm` are
   never the headline: small, along the bottom, breathing with the beat, at
   most three at once.
4. **Colour.** neo citrus `[143,209,63]`, blueberry indigo `[90,87,211]`,
   frisbee blush `[242,167,185]`. On the six laptops the whole screen is the
   seat's colour and the text is black or near-white by luminance; on the
   Xbox, ac7 and the Windows page the ground is dark and the text is the
   member's colour, with the six seats standing as dim breathing figures.
5. **Readable from the back of the room.** Laptops: headline size ≥ 3
   (30 px glyphs), name size 2, hums size 2, ≤ 3 rows. Canvas screens:
   headline ≥ 28 px, answers up to 20 % of the height.

## Where it lives

- Laptops: `fleet/native-trio.mjs` `paint()` (immediate mode, cursors, no
  per-frame allocation; bench with `fedac/native/tests/paint-density-bench.mjs`).
- Dark canvases: `fleet/lyric-graphics.js` exporting
  `drawTrioRoom(ctx, w, h, music, elapsed, now)`; preview at
  `fleet/lyric-graphics-preview.html` (open in a browser from the lane).
  The Xbox/ac7 renderer (`oskiewar-dance.js` on blueberry's lane, deployed
  by neo's stage) and the Windows sub page adopt it by calling
  `drawTrioRoom` where they now call `drawTrioLyric(music, elapsed)`, with
  `elapsed` extrapolated locally from the last packet.
- Feed: the runner (`bin/run-full-trio.py`) posts `lyric` (with `role`,
  `answer`, `syllables`, `syllable`), `next` and `faces` at 5 Hz.
