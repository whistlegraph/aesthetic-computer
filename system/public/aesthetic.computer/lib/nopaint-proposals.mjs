// Deterministic proposal selection for the native No Paint conductor.

export const NOPAINT_LOOP_STATES = Object.freeze([
  "choosing",
  "proposing",
  "committing",
  "discarding",
  "paused",
]);

// The recovered Construct picker contains Box twice. The other first-pass
// operations occur once, so rect keeps that aggregate 2x weight here.
export const NOPAINT_PROPOSAL_CATALOG = Object.freeze([
  Object.freeze({ name: "rect", weight: 2 }),
  Object.freeze({ name: "oval", weight: 1 }),
  Object.freeze({ name: "line", weight: 1 }),
  Object.freeze({ name: "wipe", weight: 1 }),
  Object.freeze({ name: "camera", weight: 1 }),
]);

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
    224,
  ];
  const w = Math.max(12, Math.floor(width * (0.12 + random() * 0.42)));
  const h = Math.max(12, Math.floor(height * (0.12 + random() * 0.42)));
  const x = Math.floor(random() * Math.max(1, width - w));
  const y = Math.floor(random() * Math.max(1, height - h));

  return Object.freeze({
    kind,
    color: Object.freeze(color),
    x,
    y,
    w,
    h,
    drift: Math.floor(4 + random() * Math.max(5, Math.min(width, height) * 0.08)),
    thickness: Math.floor(1 + random() * 8),
  });
}
