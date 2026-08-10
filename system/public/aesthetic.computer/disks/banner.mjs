// Banner, 26.08.10
// The original No Paint banner as a standalone AC piece.

import { createNoPaintBrushPiece } from "../lib/nopaint-brush-piece.mjs";
import { bannerProposal as nopaintProposal } from "../lib/nopaint-construct-build.mjs";

const system = "nopaint";
const piece = createNoPaintBrushPiece(nopaintProposal, {
  title: "Banner",
  desc: "Paint with the recovered ribbon as it zips and turns.",
});

export const { boot, sim, paint, bake, meta } = piece;
export { nopaintProposal, system };
