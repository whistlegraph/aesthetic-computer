// Triangle, 26.08.10
// The original No Paint triangle as a standalone AC piece.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { triangleProposal as nopaintProposal } from "../lib/nopaint-construct-shapes.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Triangle",
  desc: "Paint with the recovered three-point shape as it shakes once a second.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
