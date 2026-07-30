// Deterministic proposal selection for the native No Paint conductor.

export const NOPAINT_LOOP_STATES = Object.freeze([
  "choosing",
  "proposing",
  "committing",
  "discarding",
  "paused",
]);

export const NOPAINT_VERSION = "3.0";
export const NOPAINT_MAX_RANDOM_ALPHA = 192;

// Names recovered from the Construct picker. This is the migration ledger;
// entries stay here even before their AC renderer is ready.
export const NOPAINT_RECOVERED_CATALOG = Object.freeze({
  brushes: Object.freeze([
    "softy", "build", "bubbles", "banner", "dark-window", "wafer",
    "walker", "aura", "box", "triangle", "line", "ellipse", "breathe",
    "vignette", "caterpillar", "frame", "grid-worm", "stamp", "rainbow",
  ]),
  transforms: Object.freeze([
    "wipe", "blur", "sharpen", "mirror", "noise", "flip", "saturate",
    "invert", "scroll", "contrast", "zoom", "recurse", "quicksand",
    "spin", "light-bump", "turn",
  ]),
});

export const NOPAINT_IMPLEMENTED_OPERATIONS = Object.freeze([
  "box", "ellipse", "line", "softy", "bubbles", "grid-worm", "walker",
  "banner", "wafer", "dark-window", "build", "aura", "triangle", "breathe",
  "vignette", "caterpillar", "frame", "rainbow", "wipe", "camera",
  "contrast", "flip", "invert", "light-bump", "mirror", "quicksand",
  "recurse", "saturate", "scroll", "sharpen", "spin", "turn", "zoom",
]);

// The recovered Construct picker contains Box twice. The other first-pass
// operations occur once, so rect keeps that aggregate 2x weight here.
export const NOPAINT_PROPOSAL_CATALOG = Object.freeze([
  Object.freeze({ name: "rect", label: "Box", family: "brush", weight: 2 }),
  Object.freeze({ name: "oval", label: "Ellipse", family: "brush", weight: 1 }),
  Object.freeze({ name: "line", label: "Line", family: "brush", weight: 1 }),
  Object.freeze({ name: "softy", label: "Softy", family: "authored", weight: 1.5 }),
  Object.freeze({ name: "bubbles", label: "Bubbles", family: "authored", weight: 1 }),
  Object.freeze({ name: "grid-worm", label: "Grid Worm", family: "authored", weight: 1 }),
  Object.freeze({ name: "dark-window", label: "Dark Window", family: "authored", weight: 1 }),
  Object.freeze({ name: "build", label: "Build", family: "authored", weight: 1 }),
  Object.freeze({ name: "aura", label: "Aura", family: "authored", weight: 1 }),
  Object.freeze({ name: "triangle", label: "Triangle", family: "brush", weight: 1 }),
  Object.freeze({ name: "ellipse", label: "Ellipse", family: "brush", weight: 1 }),
  Object.freeze({ name: "breathe", label: "Breathe", family: "authored", weight: 1 }),
  Object.freeze({ name: "vignette", label: "Vignette", family: "authored", weight: 1 }),
  Object.freeze({ name: "caterpillar", label: "Caterpillar", family: "authored", weight: 1 }),
  Object.freeze({ name: "frame", label: "Frame", family: "authored", weight: 1 }),
  Object.freeze({ name: "rainbow", label: "Rainbow", family: "authored", weight: 1 }),
  Object.freeze({ name: "walker", label: "Walker", family: "authored", weight: 0.2 }),
  Object.freeze({ name: "banner", label: "Banner", family: "authored", weight: 1 }),
  Object.freeze({ name: "wafer", label: "Wafer", family: "authored", weight: 1 }),
  Object.freeze({ name: "wipe", label: "Wash", family: "transform", weight: 1 }),
  Object.freeze({ name: "contrast", label: "Contrast", family: "transform", weight: 1 }),
  Object.freeze({ name: "flip", label: "Flip", family: "transform", weight: 1 }),
  Object.freeze({ name: "invert", label: "Invert", family: "transform", weight: 1 }),
  Object.freeze({ name: "light-bump", label: "Light Bump", family: "transform", weight: 1 }),
  Object.freeze({ name: "mirror", label: "Mirror", family: "transform", weight: 1 }),
  Object.freeze({ name: "quicksand", label: "Quicksand", family: "transform", weight: 1 }),
  Object.freeze({ name: "recurse", label: "Recurse", family: "transform", weight: 1 }),
  Object.freeze({ name: "saturate", label: "Saturate", family: "transform", weight: 1 }),
  Object.freeze({ name: "scroll", label: "Scroll", family: "transform", weight: 1 }),
  Object.freeze({ name: "sharpen", label: "Sharpen", family: "transform", weight: 1 }),
  Object.freeze({ name: "spin", label: "Spin", family: "transform", weight: 1 }),
  Object.freeze({ name: "turn", label: "Turn", family: "transform", weight: 1 }),
  Object.freeze({ name: "zoom", label: "Zoom", family: "transform", weight: 1 }),
  // Camera is implemented but deliberately absent from random proposals.
  // Device access belongs to an explicit camera-tool action, never page load.
]);

export function proposalDefinition(name, catalog = NOPAINT_PROPOSAL_CATALOG) {
  return catalog.find((entry) => entry.name === name) || null;
}

export function seedFrom(value) {
  const text = String(value ?? "nopaint");
  let hash = 2166136261;
  for (let i = 0; i < text.length; i += 1) {
    hash ^= text.charCodeAt(i);
    hash = Math.imul(hash, 16777619);
  }
  return hash >>> 0 || 1;
}

export function seededRandom(seed) {
  let state = seedFrom(seed);
  return () => {
    state += 0x6d2b79f5;
    let value = state;
    value = Math.imul(value ^ (value >>> 15), value | 1);
    value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
    return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
  };
}

export function pickWeightedProposal(random, catalog = NOPAINT_PROPOSAL_CATALOG) {
  const total = catalog.reduce((sum, proposal) => sum + proposal.weight, 0);
  if (total <= 0) throw new Error("No Paint proposal catalog has no weight");

  let cursor = random() * total;
  for (const proposal of catalog) {
    cursor -= proposal.weight;
    if (cursor < 0) return proposal.name;
  }
  return catalog[catalog.length - 1].name;
}

export function makeProposal(random, width, height) {
  const kind = pickWeightedProposal(random);
  const color = [
    Math.floor(random() * 256),
    Math.floor(random() * 256),
    Math.floor(random() * 256),
    128,
  ];
  const w = Math.max(12, Math.floor(width * (0.12 + random() * 0.42)));
  const h = Math.max(12, Math.floor(height * (0.12 + random() * 0.42)));
  const x = Math.floor(random() * Math.max(1, width - w));
  const y = Math.floor(random() * Math.max(1, height - h));
  const points = Array.from({ length: 18 }, () => Object.freeze({
    x: Math.floor(random() * width),
    y: Math.floor(random() * height),
    size: Math.floor(3 + random() * Math.max(4, Math.min(width, height) * 0.12)),
  }));

  return Object.freeze({
    kind,
    color: Object.freeze(color),
    x,
    y,
    w,
    h,
    drift: Math.floor(4 + random() * Math.max(5, Math.min(width, height) * 0.08)),
    thickness: Math.floor(1 + random() * 8),
    points: Object.freeze(points),
    phase: random() * Math.PI * 2,
  });
}
