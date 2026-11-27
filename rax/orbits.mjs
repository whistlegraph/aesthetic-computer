// 🪐 The universe's clockwork: a sun and the planets going round it, each on
// a fixed circle, so where everything is follows from the time alone. Like
// the sky's day (sky.mjs) there is nothing to store or send: any two clients
// agree. Pure, so the sky, the map and (later) travel read the same sums.
// (2026-11-27: the planets are no longer drawn from this clockwork. They
// hang in home's own air as places a bike flies to: HUNG, at the foot.)
//
// Orbits are on rails, not gravity: nothing drifts, nothing collides, and a
// time always gives the same sky. Years are squashed to minutes and the
// distances to a ride's length. Kepler's third law still holds: a planet
// twice as far out takes 2^1.5 times as long round.
//
// The universe is measured in metres from the sun, on its ecliptic plane
// (x, y) with z off the plane. Home is the planet with the tower (planet.mjs's
// PLANET is its radius); its day stays the Los Angeles day of sky.mjs, so
// what is seen from home is placed relative to the sun the day puts up.
import { PLANET } from './planet.mjs';
import { SPAN, LAP } from './park.mjs';

export const HOME = 'home';
// Neighbourhoods, not a solar system (2026-11-24): little worlds hung a
// stone's throw apart, so a bike (14 m/s) is at the next one in a few
// seconds at their nearest, and they visibly swing round while you watch.
export const AU = 180; // home's distance from the sun, metres
export const YEAR_MS = 12 * 60 * 1000; // home's year: once round the sun

export const SUN = { id: 'sun', name: 'sun', radius: 60, ink: 0xfff4c8 };

// Each body: its radius, the circle it keeps (distance from the sun, a
// starting angle, a tilt off the plane and where that tilt crosses it) and
// its colours (`ink` lit, `dark` its night side).
export const BODIES = [
  { id: HOME, name: 'aesthetic', radius: PLANET, distance: AU, start: 0, tilt: 0, node: 0, ink: 0x8fcf7a, dark: 0x1c2a3a },
  { id: 'pinch', name: 'pinch', radius: 30, distance: 110, start: 2.1, tilt: 0.05, node: 0.7, ink: 0xffa07a, dark: 0x3a1c2a },
  { id: 'brume', name: 'brume', radius: 44, distance: 260, start: 4.4, tilt: -0.04, node: 2.3, ink: 0x7ac8e6, dark: 0x16233a },
];
const byId = new Map(BODIES.map((b) => [b.id, b]));
export const body = (id) => byId.get(id) || null;

// Time round the sun, from the distance (Kepler: T ∝ a^1.5).
export const periodOf = (b) => YEAR_MS * Math.pow(b.distance / AU, 1.5);

// Where a body is at `now` (ms since the epoch): [x, y, z] from the sun.
export function positionOf(b, now = Date.now()) {
  if (typeof b === 'string') b = body(b);
  if (!b) return null;
  const turns = now / periodOf(b);
  const a = b.start + 2 * Math.PI * (turns - Math.floor(turns));
  const x = Math.cos(a) * b.distance, y = Math.sin(a) * b.distance;
  const lift = Math.sin(b.tilt) * Math.sin(a - b.node) * b.distance; // off the plane
  return [x, y, lift];
}

export const distanceBetween = (a, b, now = Date.now()) => {
  const p = positionOf(a, now), q = positionOf(b, now);
  return Math.hypot(p[0] - q[0], p[1] - q[1], p[2] - q[2]);
};

// 🌌 The sky's frame. sky.mjs runs the sun round the tilted circle
// (cos s, 0.85 sin s, -0.5 sin s) at s = 2π(phase - 1/4). A direction in the
// universe is put on the same circle by its angle from the sun, and lifted
// off it along the circle's pole by its angle off the plane, so the sun it
// gives is exactly the day's sun, and the planets keep to its road.
const POLE = (() => { const l = Math.hypot(0.5, 0.85); return [0, 0.5 / l, 0.85 / l]; })();
const road = (s) => {
  const v = [Math.cos(s), 0.85 * Math.sin(s), -0.5 * Math.sin(s)], l = Math.hypot(...v);
  return [v[0] / l, v[1] / l, v[2] / l];
};
function toSky(w, sunAngle, sunTurn) {
  const flat = Math.hypot(w[0], w[1]);
  const along = road(Math.atan2(w[1], w[0]) - sunAngle + sunTurn);
  const up = Math.atan2(w[2], flat), c = Math.cos(up), s = Math.sin(up);
  return { x: along[0] * c + POLE[0] * s, y: along[1] * c + POLE[1] * s, z: along[2] * c + POLE[2] * s };
}

// What the sky over `from` holds at `now`, with the day at `phase` (0 … 1,
// sky.mjs): every other body, as a unit direction in the sky (x east, y up,
// z north), its distance, its angular radius, and the direction its sun
// light comes from in the same frame (for its phase), nearest first.
export function skyOf(from = HOME, now = Date.now(), phase = 0.5) {
  const here = positionOf(from, now);
  if (!here) return [];
  const sunAngle = Math.atan2(-here[1], -here[0]);
  const sunTurn = 2 * Math.PI * (phase - 0.25);
  const seen = [];
  for (const b of BODIES) {
    if (b.id === from) continue;
    const p = positionOf(b, now), w = [p[0] - here[0], p[1] - here[1], p[2] - here[2]];
    const distance = Math.hypot(...w);
    seen.push({
      id: b.id, name: b.name, ink: b.ink, dark: b.dark, radius: b.radius, distance,
      dir: toSky(w, sunAngle, sunTurn),
      size: Math.asin(Math.min(1, b.radius / distance)),
      light: toSky([-p[0], -p[1], -p[2]], sunAngle, sunTurn), // body → sun
    });
  }
  return seen.sort((a, b) => a.distance - b.distance);
}

// The sun as seen from `from`, in the same frame (agrees with sky.mjs).
export function sunFrom(from = HOME, now = Date.now(), phase = 0.5) {
  const here = positionOf(from, now);
  return toSky([-here[0], -here[1], -here[2]], Math.atan2(-here[1], -here[0]), 2 * Math.PI * (phase - 0.25));
}

// 🪐 Hung (2026-11-27). The other planets are places now, not lights: small
// balls hung in home's own air, a short flight from the tower, in the same
// metres as everything else (x east, z north from the plaza's middle, y up
// to the ball's middle). A cabin bike (bike.mjs) is pointed at one and flown
// there; nothing is crossed on the way. Each is solid, and the top of it is
// ground: a body stands and walks there as far as CAP of the way to its
// edge, past which it is too steep and the body falls, back to home.
// (They do not go round. The clockwork above is kept, and is not drawn:
// a planet stood on has to be where the server and every page say it is,
// and each room keeps its own count of time.)
export const HUNG = [
  { id: 'pinch', name: 'pinch', r: 6, x: -32, y: 22, z: 16, ink: 0xffa07a, dark: 0x3a1c2a }, // north-west of the tower, two blocks out
  { id: 'brume', name: 'brume', r: 7.5, x: 32, y: 30, z: -32, ink: 0x7ac8e6, dark: 0x16233a }, // south-east, higher
];
export const CAP = 0.92;
// The planets as a room has them: in its own metres (`cell` is its place on
// the lattice, `h` how high its floor is), each with the box round it, for
// whatever asks a room's solids only for boxes. Only those within `reach`
// of the room's middle along the ground (a bike may be a block past an edge
// before the next room has it).
const LAND = LAP * SPAN;
const lap = (n) => n - Math.round(n / LAND) * LAND;
export function hungFor(cell, h = 0, reach = 48) {
  if (!cell) return [];
  const out = [];
  for (const b of HUNG) {
    const x = lap(b.x - cell.i * SPAN), z = lap(b.z - cell.j * SPAN), y = b.y - h;
    if (Math.max(Math.abs(x), Math.abs(z)) > reach) continue;
    out.push({ ball: true, id: b.id, name: b.name, r: b.r, x, y, z, ink: b.ink, dark: b.dark,
      xMin: x - b.r, xMax: x + b.r, yMin: y - b.r, yMax: y + b.r, zMin: z - b.r, zMax: z + b.r });
  }
  return out;
}
// How high a planet's ground is at (x, z), or null where it is no ground
// (off it, or past CAP: too steep to stand).
export const topOf = (b, x, z) => {
  const d = (x - b.x) ** 2 + (z - b.z) ** 2;
  return d < (b.r * CAP) ** 2 ? b.y + Math.sqrt(b.r * b.r - d) : null;
};
