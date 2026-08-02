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

export const softyProposal = contract("softy", "Softy",
  { modes: ["S", "M", "L"], radii: [[6, 16], [16, 48], [48, 64]], landingFrames: 4 },
  ({ random, width, height, base }) => ({ points: points(random, width, height, 12), mode: ["S", "M", "L"][Math.floor(random() * 3)], color: base.color }),
  ({ ink }, s, frame) => s.points.forEach((p, i) => ink(s.color[0], s.color[1], s.color[2], 22).oval(p.x, p.y, p.size * (1 + .18 * Math.sin(frame / 24 + i * .7)), p.size * (1 + .18 * Math.sin(frame / 24 + i * .7)), true)));

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

export const waferProposal = contract("wafer", "Wafer",
  { cellSizes: [16, 16, 32, 32, 32, 32, 48, 48, 48, 64, 64, 96], bites: 3 },
  ({ random, base }) => ({ cellSize: [16, 16, 32, 32, 32, 32, 48, 48, 48, 64, 64, 96][Math.floor(random() * 12)], color: base.color }),
  ({ ink }, s, frame) => { for (let y = 0; y < s.h; y += s.cellSize) for (let x = 0; x < s.w; x += s.cellSize) if ((x / s.cellSize + y / s.cellSize + Math.floor(frame / 30)) % 5) ink(s.color).box(s.x + x, s.y + y, s.cellSize - 1, s.cellSize - 1); });

export const walkerProposal = contract("walker", "Walker",
  { blip: true, noiseShift: 1 / 30, alphaLevels: [60, 80] },
  ({ random, width, height, base }) => ({ points: points(random, width, height, 24), color: base.color }),
  ({ ink }, s, frame) => { const visible = Math.max(2, Math.min(s.points.length, Math.ceil(frame / 3))); for (let i = 1; i < visible; i++) ink(s.color).line(s.points[i - 1].x, s.points[i - 1].y, s.points[i].x, s.points[i].y, s.thickness); });

export const auraProposal = contract("aura", "Aura",
  { angle: [90, 110], repetitions: [2, 4], amount: [0, 100], radius: [25, 120], spread: [60, 180] },
  ({ random, width, height, base }) => ({ center: frozen({ x: random() * width, y: random() * height }), radius: 25 + random() * 95, repetitions: random() < .5 ? 2 : 4, color: base.color }),
  ({ ink }, s, frame) => { for (let i = s.repetitions; i > 0; i--) { const r = s.radius * i / s.repetitions * (1 + .08 * Math.sin(frame / 30)); ink(s.color[0], s.color[1], s.color[2], 35).oval(s.center.x, s.center.y, r * 2, r * 2, false, Math.max(1, r / 10)); } });

export const triangleProposal = contract("triangle", "Triangle",
  { colorChannels: [5, 6], frameCycle: 3 },
  ({ random, width, height, base }) => ({ points: points(random, width, height, 3), color: base.color }),
  ({ ink }, s) => ink(s.color).poly(s.points.map(p => [p.x, p.y])));

export const ellipseProposal = contract("ellipse", "Ellipse", { speed: .35 },
  ({ base }) => ({ color: base.color }), ({ ink }, s, frame) => ink(s.color).oval(s.x + s.w / 2, s.y + s.h / 2, s.w * (1 + .1 * Math.sin(frame * .35)), s.h * (1 + .1 * Math.sin(frame * .35)), true));

export const breatheProposal = contract("breathe", "Breathe",
  { sizes: [64, 96, 128, 196, 256], bulge: "BulgeCycle" },
  ({ random, width, height, base }) => ({ size: [64, 96, 128, 196, 256][Math.floor(random() * 5)], center: frozen({ x: random() * width, y: random() * height }), color: base.color }),
  ({ ink }, s, frame) => { const scale = .6 + .4 * (1 + Math.sin(frame / 30)); ink(s.color).oval(s.center.x, s.center.y, s.size * scale, s.size * scale, true); });

export const vignetteProposal = contract("vignette", "Vignette", { parameters: ["Radius", "Hardness"] },
  ({ random, width, height, base }) => ({ center: frozen({ x: width / 2, y: height / 2 }), radius: Math.min(width, height) * (.25 + random() * .25), color: base.color }),
  ({ ink }, s) => { for (let i = 5; i > 0; i--) ink(s.color[0], s.color[1], s.color[2], 16).oval(s.center.x, s.center.y, s.radius * (1 + i * .18) * 2, s.radius * (1 + i * .18) * 2, false, Math.max(2, s.radius * .12)); });

export const caterpillarProposal = contract("caterpillar", "Caterpillar",
  { segmentChoices: [1, 3, 32], scales: [.2, .7], frames: [0, 1, 2, 3] },
  ({ random, width, height, base }) => ({ count: [1, 3, 32][Math.floor(random() * 3)], points: points(random, width, height, 32), color: base.color }),
  ({ ink }, s, frame) => s.points.slice(0, s.count).forEach((p, i) => ink(s.color).oval(p.x + Math.sin(frame / 12 + i) * p.size, p.y, p.size, p.size, true)));

export const frameProposal = contract("frame", "Frame", { cycle: "CycleFrame", sound: "frame - knock" },
  ({ base }) => ({ color: base.color }), ({ ink }, s, frame) => { const inset = Math.floor(frame / 15) % Math.max(1, Math.floor(Math.min(s.w, s.h) / 4)); ink(s.color).box(s.x + inset, s.y + inset, s.w - inset * 2, 2); ink(s.color).box(s.x + inset, s.y + s.h - inset - 2, s.w - inset * 2, 2); ink(s.color).box(s.x + inset, s.y + inset, 2, s.h - inset * 2); ink(s.color).box(s.x + s.w - inset - 2, s.y + inset, 2, s.h - inset * 2); });

export const rainbowProposal = contract("rainbow", "Rainbow",
  { effect: "AdjustHSL", hueRange: [-100, 100] },
  ({ base }) => ({ color: base.color }), ({ ink }, s, frame) => { for (let i = 0; i < 7; i++) { const hue = (frame / 120 + i / 7) * Math.PI * 2; ink(Math.round(128 + Math.sin(hue) * 127), Math.round(128 + Math.sin(hue + 2.094) * 127), Math.round(128 + Math.sin(hue + 4.188) * 127), 96).box(s.x, s.y + i * s.h / 7, s.w, Math.ceil(s.h / 7)); } });

export const nonConflictingConstructProposals = frozen([
  softyProposal, buildProposal, bubblesProposal, bannerProposal, waferProposal,
  walkerProposal, auraProposal, triangleProposal, ellipseProposal, breatheProposal,
  vignetteProposal, caterpillarProposal, frameProposal, rainbowProposal,
]);
