// Caterpillar, 26.08.10
// The original No Paint caterpillar as a standalone AC brush. Ask for seven
// segments and you get the rainbow road.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { caterpillarProposal as nopaintProposal } from "../lib/nopaint-construct-caterpillar.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Caterpillar",
  desc: "Paint with the recovered caterpillar as it squirms toward wandering targets.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
