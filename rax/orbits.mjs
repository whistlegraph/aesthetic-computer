// 🪐 The universe's clockwork: a sun and the planets going round it, each on
// a fixed circle, so where everything is follows from the time alone. Like
// the sky's day (sky.mjs) there is nothing to store or send: any two clients
// agree. Pure, so the sky, the map and (later) travel read the same sums.
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
// `altitude` (metres over the ground) is how high the eye is: these worlds
// are near enough that a climb moves them in the sky, and a bike at the top
// of its climb sees each where space will have it (launch, below). Without
// it they are seen from the planet's middle.
export function skyOf(from = HOME, now = Date.now(), phase = 0.5, altitude = null) {
  const own = typeof from === 'string' ? body(from) : from, middle = positionOf(own, now);
  if (!middle) return [];
  const sunAngle = Math.atan2(-middle[1], -middle[0]);
  const sunTurn = 2 * Math.PI * (phase - 0.25);
  const up = Number.isFinite(altitude) ? fromSky([0, 1, 0], sunAngle, sunTurn).map((n) => n * (own.radius + altitude)) : [0, 0, 0];
  const here = middle.map((n, i) => n + up[i]);
  const seen = [];
  for (const b of BODIES) {
    if (b.id === own.id) continue;
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

// 🚀 Travel. A cabin bike (bike.mjs) that climbs past LEAVE metres over a
// planet is in space: one room, SPACE, where the sun stands in the middle
// and the planets go round as real balls. A bike that comes within ARRIVE
// metres of a planet's ground there is set down over it. (LEAVE is over
// ARRIVE, so a bike just out is not taken straight back in. At their nearest
// two planets are closer than that: then leaving one is arriving at the
// other, which is what a stone's throw means.)
//
// Space is measured in metres from the sun with y off the plane, so the
// planets lie about its level and a bike flies between them looking ahead:
// space [x, y, z] is the universe's [x, z, -y] (a turn, not a mirror: the
// sky seen from a planet and the sky seen from space are the same way round).
export const SPACE = 'space';
export const SPACE_HALF = 512; // past the farthest planet's far side, with room to turn
export const LEAVE = 90, ARRIVE = 70;
// A planet other than home is one room for now, named for it, this wide.
export const PLANET_HALF = 40;
// The planet a room stands on (its body), or null for space and for rooms
// that are nowhere (a game's arena, a studio). `inWorld`: the room is a
// place on home (the plaza, a block, the roof).
export const planetOf = (room, inWorld = false) => (room === SPACE ? null : room !== HOME && byId.has(room) ? byId.get(room) : inWorld ? byId.get(HOME) : null);

const toSpace = (u) => [u[0], u[2], -u[1]], fromSpace = (s) => [s[0], -s[2], s[1]];
export const spaceOf = (b, now = Date.now()) => { const p = positionOf(b, now); return p && toSpace(p); };

// The way a yaw and pitch look (world.mjs `direction`), and back.
const RAD = Math.PI / 180;
export const aim = (yaw, pitch = 0) => [Math.sin(yaw * RAD) * Math.cos(pitch * RAD), Math.sin(pitch * RAD), Math.cos(yaw * RAD) * Math.cos(pitch * RAD)];
export const aimOf = (v) => {
  const l = Math.hypot(v[0], v[1], v[2]) || 1;
  return { yaw: Math.atan2(v[0], v[2]) / RAD, pitch: Math.asin(Math.max(-1, Math.min(1, v[1] / l))) / RAD };
};

// toSky undone: a direction in a planet's sky, as one in the universe.
const E2 = (() => { const l = Math.hypot(0.85, 0.5); return [0, 0.85 / l, -0.5 / l, l]; })();
function fromSky(v, sunAngle, sunTurn) {
  const lift = Math.max(-1, Math.min(1, v[0] * POLE[0] + v[1] * POLE[1] + v[2] * POLE[2]));
  const s = Math.atan2((v[1] * E2[1] + v[2] * E2[2]) / E2[3], v[0]);
  const a = s + sunAngle - sunTurn, c = Math.sqrt(1 - lift * lift);
  return [Math.cos(a) * c, Math.sin(a) * c, lift];
}
const frameOf = (b, now, phase) => { const p = positionOf(b, now); return [Math.atan2(-p[1], -p[0]), 2 * Math.PI * (phase - 0.25)]; };
// A vector (its length kept) from a planet's sky into space, and back.
const out = (v, f) => { const l = Math.hypot(...v); return l < 1e-9 ? [0, 0, 0] : toSpace(fromSky(v.map((n) => n / l), ...f)).map((n) => n * l); };
const back = (v, f) => { const l = Math.hypot(...v); if (l < 1e-9) return [0, 0, 0]; const d = toSky(fromSpace(v), ...f); return [d.x * l, d.y * l, d.z * l]; };
// v turned by the least turn that takes unit a to unit b.
function turned(v, a, b) {
  const k = [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]], c = a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
  if (c < -0.9999) return [v[0], -v[1], -v[2]]; // straight opposite: half a turn about x
  const x = (p, q) => [p[1] * q[2] - p[2] * q[1], p[2] * q[0] - p[0] * q[2], p[0] * q[1] - p[1] * q[0]];
  const kv = x(k, v), kkv = x(k, kv);
  return v.map((n, i) => n + kv[i] + kkv[i] / (1 + c));
}

// Out of a planet's air: a bike `altitude` metres over `from`, looking along
// (yaw, pitch) and going at `velocity` ([x, y, z], the room's metres a
// second), as it is in space. Seen from anywhere on a planet the sky is the
// same (it is not wrapped round the ball), so the bike comes out over the
// planet's top, under the middle of that sky: what was overhead is ahead of
// a bike that climbed, and whatever it was looking at it still is.
export function launch(from, now, phase, altitude, yaw, pitch = 0, velocity = [0, 0, 0]) {
  const b = typeof from === 'string' ? body(from) : from, f = frameOf(b, now, phase);
  const centre = spaceOf(b, now), up = out([0, 1, 0], f), r = b.radius + altitude;
  const v = out(velocity, f);
  return { x: centre[0] + up[0] * r, y: centre[1] + up[1] * r, z: centre[2] + up[2] * r, ...aimOf(out(aim(yaw, pitch), f)), vx: v[0], vy: v[1], vz: v[2] };
}

// The planet a place in space has come to (within ARRIVE of its ground), or null.
export function reached(at, now = Date.now()) {
  let best = null;
  for (const b of BODIES) {
    const c = spaceOf(b, now), d = Math.hypot(at.x - c[0], at.y - c[1], at.z - c[2]) - b.radius;
    if (d < ARRIVE && (!best || d < best.over)) best = { body: b, over: d };
  }
  return best;
}

// Into a planet's air from space: the bike at `at` ({ x, y, z, yaw, pitch,
// vx, vy, vz } in space) as it is over planet `to`: how high, looking and
// going which way. Wherever it came in from, the planet is under it (the
// same top it left from), so the way it looked and went is turned with it:
// a bike flown straight at a planet arrives looking straight down at it.
export function setDown(to, at, now, phase, floor = 8) {
  const b = typeof to === 'string' ? body(to) : to, f = frameOf(b, now, phase), c = spaceOf(b, now);
  const off = [at.x - c[0], at.y - c[1], at.z - c[2]], far = Math.hypot(...off);
  const from = far > 1e-6 ? back(off, f).map((n) => n / far) : [0, 1, 0], top = [0, 1, 0];
  const v = turned(back([at.vx || 0, at.vy || 0, at.vz || 0], f), from, top);
  return { y: Math.max(floor, Math.min(ARRIVE, far - b.radius)), ...aimOf(turned(back(aim(at.yaw, at.pitch || 0), f), from, top)), vx: v[0], vy: v[1], vz: v[2] };
}
