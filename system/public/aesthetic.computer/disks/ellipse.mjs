// Ellipse, 26.08.10
// The original No Paint ellipse as a standalone AC piece.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { ellipseProposal as nopaintProposal } from "../lib/nopaint-construct-shapes.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Ellipse",
  desc: "Paint with the recovered ellipse as it shakes once a second.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
