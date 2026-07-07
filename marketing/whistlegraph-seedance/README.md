# whistlegraph-seedance

Seedance 2.0 (fal.ai) variations of personally recorded whistlegraph videos —
new drawings in the exact visual + sonic aesthetic of the source recordings.
Shares `pop/lib/fal-seedance.mjs` with the /pop motion pipeline.

**THE ONE RULE:** the graphics align with the sounds. The voice (whistled or
sung) traces each stroke as it is drawn — line rises, pitch rises; fast
strokes get fast notes — and the chalk scratch is heard exactly and only
while the chalk touches the surface.

## The pipeline (three checkpoints)

1. **Anchor** — extract a real frame from a source recording (ffmpeg). Nothing
   is more on-aesthetic than an actual frame.
2. **Storyboard stills** — gpt-image-2 `/v1/images/edits` of that frame
   (`storyboard/gen-still.mjs`, `gen-tree-stills.mjs`). ~$0.25 each and
   inspected by eye BEFORE any video spend; a chain of edits can re-stage the
   whole scene (blacktop, colored sidewalk chalk) while keeping the real hand.
3. **Animate** — `generateShot(image → endImage)` pins the render between
   validated stills (`storyboard/animate*.mjs`); reference-to-video
   (`gen-*.mjs`) rides the raw recordings as @Video refs instead.
4. **Post-render QC** — `storyboard/validate-clip.mjs <clip>` samples 24
   frames into 3 review sheets, checked against the shot's choreography:
   stroke count / contact / persistence / end state. (Audio needs ears.)

## Findings (2026-07-07 session)

- Video-ref-only + `generate_audio: true` beats passing extracted phone audio
  as @Audio1 — Seedance imitates a ref's noise floor, not just its melody.
- Words work: chalk-handwrote "hello" correctly 2/2 when the prompt
  choreographs letter by letter.
- Exact counts need beat phrasing: "first gill, second gill, third gill —
  three strokes, three chirps" (a bare "three gill lines" drew two in one
  stroke).
- Stray-mark artifact (chalk appearing away from the tip): fix with a
  physical-realism prompt clause + choreography that fits the duration.
  Standard tier is NOT the fix — in an A/B it materialized marks worse than
  fast.
- Singing works: "a warm casual human voice SINGS along, whistlegraph-style"
  with sung words scripted per stroke ("a tru-unk", "one... two... three!").
- Chalk-color swaps render correctly when choreographed as set-down/pick-up.

Scripts reference the working dir `~/Desktop/seedance-variations/` and source
videos in `~/Downloads/` — point SRC/OUT constants at your own material.
Costs: fast tier ≈ $0.24/s, standard ≈ $0.30/s @720p; a typical 8s clip ≈ $2.
