// Dark Window, 26.08.10
// The recovered two-window No Paint action as a standalone AC brush.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { darkWindowProposal as nopaintProposal } from "../lib/nopaint-construct-brushes.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Dark Window",
  desc: "Paint with the original two-window, four-note No Paint action.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
