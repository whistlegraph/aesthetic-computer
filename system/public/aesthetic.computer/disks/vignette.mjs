// Vignette, 26.08.10
// The original No Paint vignette as a standalone AC piece.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { vignetteProposal as nopaintProposal } from "../lib/nopaint-construct-vignette.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Vignette",
  desc: "Paint with the recovered soft field, dark or light.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
