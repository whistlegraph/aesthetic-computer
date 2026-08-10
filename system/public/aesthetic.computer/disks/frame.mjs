// Frame, 26.08.10
// The eleven original No Paint borders as a standalone AC brush.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { frameProposal as nopaintProposal } from "../lib/nopaint-construct-sprites.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Frame",
  desc: "Paint with the eleven recovered No Paint borders, knocking every five seconds.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
