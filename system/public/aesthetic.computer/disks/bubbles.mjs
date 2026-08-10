// Bubbles, 26.08.10
// Original No Paint bubble sprites as a standalone AC brush.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { bubblesProposal as nopaintProposal } from "../lib/nopaint-construct-sprites.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Bubbles",
  desc: "Paint with the five original No Paint bubble animations.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
