// Wafer, 26.08.10
// The original No Paint biscuit as a standalone AC piece: it appears, gets
// nibbled around its rim in a shuffled order, then grows and starts again.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { waferProposal as nopaintProposal } from "../lib/nopaint-construct-wafer.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Wafer",
  desc: "Paint with the recovered biscuit as it is nibbled and enlarged.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
