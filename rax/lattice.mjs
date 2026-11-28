// 🧊 The lattice: rax's one geometry (GEOMETRY.md). Space is cubes on three
// axes, and a place is four whole numbers, `level : x , y , z`.
//   x counts cubes east, y up, z north (as geography.mjs: east +x, north +z);
//   all three run without end both ways, and 0,0,0 is the origin.
//   A cube is EDGE metres on a side at level 0 (one room: geography.mjs BLOCK).
//   A cube of level n + 1 is PER cubes of level n on each edge. Going down a
//   level is going into a cube, which is then a lattice of PER³ cubes that
//   are each EDGE metres to the one who went in: the body is made smaller,
//   the rule is the same. No level is the top and none the bottom.
// This file is the arithmetic only: names, neighbours, levels, metres. It
// knows nothing of rooms on a server or of what stands in a cube.
export const EDGE = 16; // metres along a cube, to someone standing in its level
export const PER = 4; // cubes of one level along the edge of a cube of the next
export const ORIGIN = Object.freeze({ level: 0, x: 0, y: 0, z: 0 });
// How far a name may reach: a room's key is 32 letters at most (server.mjs).
const MOST = 999999, DEEP = 99;

const whole = (n) => Number.isInteger(n);
export const isCube = (c) => !!c && whole(c.level) && whole(c.x) && whole(c.y) && whole(c.z) && Math.abs(c.level) <= DEEP && [c.x, c.y, c.z].every((n) => Math.abs(n) <= MOST);
export const cube = (x = 0, y = 0, z = 0, level = 0) => ({ level, x, y, z });
export const same = (a, b) => !!a && !!b && a.level === b.level && a.x === b.x && a.y === b.y && a.z === b.z;

// 🏷️ A cube as people write it: `1,0,-2` at level 0, `2:1,0,-2` at another
// (rax.ac/~1,0,-2). Spaces are let by.
export function word(c) {
  return `${c.level ? `${c.level}:` : ''}${c.x},${c.y},${c.z}`;
}
// (With the level always said: the HUD's line.)
export const full = (c) => `${c.level}:${c.x},${c.y},${c.z}`;
export function fromWord(text) {
  const m = /^\s*(?:(-?\d{1,2})\s*:)?\s*(-?\d{1,6})\s*,\s*(-?\d{1,6})\s*,\s*(-?\d{1,6})\s*$/.exec(String(text ?? ''));
  if (!m) return null;
  const c = cube(Number(m[2]), Number(m[3]), Number(m[4]), Number(m[1] || 0));
  return isCube(c) ? c : null;
}

// 🔑 A cube as a room's key (letters, digits and nothing else a key may not
// hold): `c0x1y0zm2`, an `m` for minus. The level may be below zero too.
const signed = (n) => (n < 0 ? `m${-n}` : `${n}`);
const unsigned = (s) => (s[0] === 'm' ? -Number(s.slice(1)) : Number(s));
export function key(c) {
  return `c${signed(c.level)}x${signed(c.x)}y${signed(c.y)}z${signed(c.z)}`;
}
export function fromKey(text) {
  const m = /^c(m?\d{1,2})x(m?\d{1,6})y(m?\d{1,6})z(m?\d{1,6})$/.exec(String(text ?? ''));
  if (!m) return null;
  const c = cube(unsigned(m[2]), unsigned(m[3]), unsigned(m[4]), unsigned(m[1]));
  // (one key for one cube: `xm0` and `x00` are not names)
  return isCube(c) && key(c) === text ? c : null;
}

// 🧭 The six faces, and the cube through each.
export const FACES = Object.freeze({
  east: [1, 0, 0], west: [-1, 0, 0],
  up: [0, 1, 0], down: [0, -1, 0],
  north: [0, 0, 1], south: [0, 0, -1],
});
export const OPPOSITE = Object.freeze({ east: 'west', west: 'east', up: 'down', down: 'up', north: 'south', south: 'north' });
export function through(c, face) {
  const d = FACES[face];
  if (!d) return null;
  const next = cube(c.x + d[0], c.y + d[1], c.z + d[2], c.level);
  return isCube(next) ? next : null;
}
export const neighbours = (c) => Object.fromEntries(Object.keys(FACES).map((face) => [face, through(c, face)]));

// 🪆 Levels. The cube a cube is in (one level up), where it sits in that
// cube (0 … PER - 1 on each axis), and the cubes in it (one level down).
const floorDiv = (n) => Math.floor(n / PER), within = (n) => ((n % PER) + PER) % PER;
export function outside(c) {
  const up = cube(floorDiv(c.x), floorDiv(c.y), floorDiv(c.z), c.level + 1);
  return isCube(up) ? up : null;
}
export const seat = (c) => ({ i: within(c.x), j: within(c.y), k: within(c.z) });
export function inside(c, i = 0, j = 0, k = 0) {
  if (![i, j, k].every((n) => whole(n) && n >= 0 && n < PER)) return null;
  const down = cube(c.x * PER + i, c.y * PER + j, c.z * PER + k, c.level - 1);
  return isCube(down) ? down : null;
}
export function* insides(c) {
  for (let j = 0; j < PER; j++) for (let k = 0; k < PER; k++) for (let i = 0; i < PER; i++) { const d = inside(c, i, j, k); if (d) yield d; }
}

// 📏 Metres. A cube's edge as seen from level 0 (a level down is PER times
// smaller), where its middle is in level-0 metres from the middle of the
// origin, and the cube of a level that a point in those metres is in.
export const edge = (level = 0) => EDGE * PER ** level;
export function middle(c) {
  const e = edge(c.level);
  // (A cube of level n > 0 holds the cubes 0 … PER - 1 of the level under it,
  // so its own middle is off the origin's by half of what it holds.)
  const off = (e - EDGE) / 2;
  return { x: c.x * e + off, y: c.y * e + off, z: c.z * e + off };
}
export function at(point, level = 0) {
  const e = edge(level), lo = -EDGE / 2;
  const c = cube(Math.floor((point.x - lo) / e), Math.floor((point.y - lo) / e), Math.floor((point.z - lo) / e), level);
  return isCube(c) ? c : null;
}
// A point in a cube's own metres (its middle is 0,0,0; a body's feet stand at
// y = -EDGE / 2 on its floor), and which face it has gone out through, if any.
export function gone(local, half = EDGE / 2) {
  const out = [['east', local.x - half], ['west', -half - local.x], ['up', local.y - half], ['down', -half - local.y], ['north', local.z - half], ['south', -half - local.z]]
    .filter(([, by]) => by > 0)
    .sort((a, b) => b[1] - a[1]);
  return out.length ? out[0][0] : null;
}
// The same point, in the metres of the cube through that face.
export function carried(local, face) {
  const d = FACES[face];
  return d ? { x: local.x - d[0] * EDGE, y: local.y - d[1] * EDGE, z: local.z - d[2] * EDGE } : null;
}
