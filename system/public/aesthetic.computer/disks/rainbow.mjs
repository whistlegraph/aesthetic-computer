// Rainbow, 26.08.10
// The original No Paint hue rotation as a standalone AC piece. It is a pixel
// transform, not a brush: it rotates the hue of everything already painted.

import { rainbowProposal as nopaintProposal } from "../lib/nopaint-construct-rainbow.mjs";
import { seededRandom } from "../lib/nopaint-proposals.mjs";

const system = "nopaint";
let applied = false;

function boot({ hud }) {
  applied = false;
  hud?.label?.("Rainbow");
}

// A transform has nothing to propose into the buffer; it rewrites the accepted
// painting the moment the gesture lands.
function paint({ system: sys, page, screen, needsPaint }) {
  if (applied || !sys?.nopaint?.is?.("painting")) return false;
  const painting = sys.painting;
  if (!painting?.pixels) return false;
  const score = nopaintProposal.generate({
    random: seededRandom(`rainbow:${painting.width}:${painting.height}`),
    base: { color: [0, 0, 0, 0] },
  });
  painting.pixels.set(nopaintProposal.applyPixels(
    painting.pixels, painting.width, painting.height, score.brush.parameters));
  applied = true;
  page(screen);
  needsPaint();
  return true;
}

function bake() {
  applied = false;
}

function meta() {
  return {
    title: "Rainbow",
    desc: "Rotate the hue of the whole painting, the way the original AdjustHSL did.",
    controls: "tap to rotate the painting's hue",
  };
}

export { boot, paint, bake, meta, nopaintProposal, system };
