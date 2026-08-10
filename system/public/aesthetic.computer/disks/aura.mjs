// Aura, 26.08.10
// The original No Paint aura as a standalone AC piece.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { auraProposal as nopaintProposal } from "../lib/nopaint-construct-vignette.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Aura",
  desc: "Paint with the recovered emitter's coloured bloom.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
