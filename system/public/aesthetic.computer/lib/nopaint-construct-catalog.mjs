// Deterministic adapters for non-conflicting Construct-era No Paint brush names.
// The source constants below are transcribed from the exported C3 expression
// table; rendering uses AC primitives and therefore does not claim pixel parity
// for Construct effects (Bulge, Vignette, AdjustHSL, sprite animation).

const frozen = (value) => Object.freeze(value);
const points = (random, width, height, count) => frozen(Array.from({ length: count }, () => frozen({
  x: Math.floor(random() * width), y: Math.floor(random() * height),
  size: 4 + Math.floor(random() * Math.max(5, Math.min(width, height) / 10)),
})));

function contract(slug, label, source, generateScore, render) {
  return frozen({ version: 1, slug, label, compatible: true, source: frozen(source),
    generate(context) {
      const score = generateScore(context);
      return frozen({ ...context.base, ...score, kind: slug,
        brush: frozen({ slug, params: frozen([]), colon: frozen([]),
          parameters: frozen({ source, ...(score.parameters || {}) }) }) });
    }, render });
}

export const buildProposal = contract("build", "Build",
  { brickSizes: [4, 8, 16, 32, 64, 128], colorAlpha: 1 },
  ({ random, width, height, base }) => ({ brickSize: [4, 8, 16, 32, 64, 128][Math.floor(random() * 6)], points: points(random, width, height, 24), color: base.color }),
  ({ ink }, s, frame) => s.points.slice(0, Math.max(1, Math.min(s.points.length, Math.ceil(frame / 4)))).forEach(p => ink(s.color).box(Math.floor(p.x / s.brickSize) * s.brickSize, Math.floor(p.y / s.brickSize) * s.brickSize, s.brickSize, s.brickSize)));

export const bubblesProposal = contract("bubbles", "Bubbles",
  { spawnRates: [.025, .05, .1], renderStep: 1 / 60, scale: [.8, 1.2], rise: [-5, -4, -3] },
  ({ random, width, height, base }) => ({ rate: [.025, .05, .1][Math.floor(random() * 3)], points: points(random, width, height, 18), color: base.color, height }),
  ({ ink }, s, frame) => s.points.forEach((p, i) => ink(s.color).oval(p.x, (p.y - frame * (3 + i % 3) + s.height) % s.height, p.size, p.size, false, 1)));

export const bannerProposal = contract("banner", "Banner",
  { hueChannels: 3, segmentCounts: [4, 8, 16], zipperChoices: [1, 2, 3, 4], depthChoices: [1, 2, 5] },
  ({ random, base }) => ({ segments: [4, 8, 16][Math.floor(random() * 3)], color: base.color }),
  ({ ink }, s, frame) => { for (let i = 0; i < s.segments; i++) ink(s.color[0], s.color[1], s.color[2], 60 + i * 8).box(s.x + Math.sin(frame / 24 + i * .5) * s.drift, s.y + i * s.h / s.segments, s.w, Math.max(1, s.h / s.segments - 1)); });

export const walkerProposal = contract("walker", "Walker",
  { blip: true, noiseShift: 1 / 30, alphaLevels: [60, 80] },
  ({ random, width, height, base }) => ({ points: points(random, width, height, 24), color: base.color }),
  ({ ink }, s, frame) => { const visible = Math.max(2, Math.min(s.points.length, Math.ceil(frame / 3))); for (let i = 1; i < visible; i++) ink(s.color).line(s.points[i - 1].x, s.points[i - 1].y, s.points[i].x, s.points[i].y, s.thickness); });

// A name only stays in this fallback catalog until a real piece owns it; what is
// left here is the last of the procedural brushes.
export const nonConflictingConstructProposals = frozen([
  buildProposal, bubblesProposal, bannerProposal, walkerProposal,
]);
