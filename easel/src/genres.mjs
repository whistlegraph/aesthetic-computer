// genres.mjs — the two surfaces Easel can stand up.

import { stamp } from "./runtimes.mjs";

// A No Paint brush is deliberately small. Exporting `brush` is the existing
// runtime contract: disk.mjs recognises it and supplies the persistent canvas,
// gestures, undo, pan, zoom, and bake-on-lift behavior. The starter should not
// reproduce any of that machinery.
function nopaintBrush(name) {
  return `// ${name}, ${stamp()}
// A nopaint.art brush using the shared No Paint canvas and gesture runtime.

const system = "nopaint";
const size = 12;

function brush({ ink, pen }) {
  if (!pen) return;
  ink(255, 100, 255, 160).circle(pen.x, pen.y, size, true);
}

function meta() {
  return {
    title: "${name}",
    desc: "A nopaint.art brush.",
    controls: "drag to paint",
  };
}

export { brush, meta, system };
`;
}

export const GENRES = Object.freeze([
  Object.freeze({
    id: "piece",
    label: "AC piece (blank)",
    runtime: null,
    blank: (name, runtime) => runtime.blank(name),
  }),
  Object.freeze({
    id: "nopaint",
    label: "nopaint.art brush",
    runtime: "mjs",
    blank: (name) => nopaintBrush(name),
  }),
]);

export const DEFAULT_GENRE = "piece";

const ALIASES = Object.freeze({ ac: "piece", blank: "piece", brush: "nopaint" });

export function genreFor(id = DEFAULT_GENRE) {
  const wanted = String(id || DEFAULT_GENRE).trim().toLowerCase();
  const resolved = ALIASES[wanted] || wanted;
  const genre = GENRES.find((candidate) => candidate.id === resolved);
  if (!genre) {
    throw new Error(`unknown genre "${id}" — try piece or nopaint`);
  }
  return genre;
}

export function genreLabels() {
  return GENRES.map(({ label }) => label);
}
