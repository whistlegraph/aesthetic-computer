// Softy, 26.08.10
// The original No Paint soft brush as a standalone AC piece: a soft circle
// that walks, turns, and drifts its colour as it goes.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { softyProposal as nopaintProposal } from "../lib/nopaint-construct-softy.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Softy",
  desc: "Paint with the recovered soft circle as it wanders and shifts hue.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
