// Build, 26.08.10
// The original No Paint build as a standalone AC piece.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { buildProposal as nopaintProposal } from "../lib/nopaint-construct-build.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Build",
  desc: "Paint with the recovered builder as it walks its grid laying bricks.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
