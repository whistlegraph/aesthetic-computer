// Breathe, 26.08.10
// The original No Paint bulge as a standalone AC piece. It is a pixel
// transform, not a brush: it swells what is already painted.

import { breatheProposal as nopaintProposal } from "../lib/nopaint-construct-breathe.mjs";
import { seededRandom } from "../lib/nopaint-proposals.mjs";

const system = "nopaint";
let applied = false;

function boot({ hud }) {
  applied = false;
  hud?.label?.("Breathe");
}

function paint({ system: sys, page, screen, needsPaint }) {
  if (applied || !sys?.nopaint?.is?.("painting")) return false;
  const painting = sys.painting;
  if (!painting?.pixels) return false;
  const score = nopaintProposal.generate({
    random: seededRandom(`breathe:${painting.width}:${painting.height}`),
    width: painting.width,
    height: painting.height,
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
    title: "Breathe",
    desc: "Swell the painting the way the original Bulge did.",
    controls: "tap to take one breath",
  };
}

export { boot, paint, bake, meta, nopaintProposal, system };
