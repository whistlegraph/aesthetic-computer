// Frames, 26.08.10
// The eleven original No Paint borders as a standalone AC brush.
//
// The piece is `frames`, not `frame`: `frame` is too useful a word to spend on
// one brush, and Frames is what Construct called the object anyway. The No
// Paint operation keeps its recovered name `frame`, the way Box keeps `rect` —
// see nopaintProposal.slug versus brush.slug.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { frameProposal as nopaintProposal } from "../lib/nopaint-construct-sprites.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Frames",
  desc: "Paint with the eleven recovered No Paint borders, knocking every second.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
