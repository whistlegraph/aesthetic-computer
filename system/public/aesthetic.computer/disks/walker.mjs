// Walker, 26.08.10
// WalkerElla's nine original sprite animations as a standalone AC brush.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { walkerProposal as nopaintProposal } from "../lib/nopaint-construct-sprites.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Walker",
  desc: "Paint with WalkerElla's nine recovered Construct animations.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
