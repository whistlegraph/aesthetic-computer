// Grid Worm, 26.08.10
// The original No Paint grid worm as a standalone AC piece: three exclusion
// channels crawling a quantized grid.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { gridWormProposal as nopaintProposal } from "../lib/nopaint-construct-brushes.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Grid Worm",
  desc: "Paint with the recovered worm as it crawls a 32, 64, 128, or 256 grid.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
