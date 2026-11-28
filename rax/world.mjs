import { BASEMENT, SEA_HALF, POOL, floatInPool } from './basement.mjs';
import { objectBoxes } from './objects.mjs';
import { towerSides, carDoor } from './elevator.mjs';
import { fly, rideable } from './bike.mjs';
import { topOf } from './orbits.mjs';
import { fromKey as cubeOf } from './lattice.mjs';
import { DEFAULT_CFG, newState, pmove } from '../system/public/aesthetic.computer/lib/pmove.mjs';

export const HZ = 60;
export const DT = 1 / HZ;
export const INTERP_TICKS = 6;
// 🏃 How far a body may run ahead of the server's word, in commands: four
// seconds. The client predicts this many unanswered, and the server runs
// this many that arrive late, so the two give out at the same moment.
export const HORIZON = 240;
export const PROTOCOL = 'rax-6';
// The same wire under its old name: a page loaded before the rename still plays.
export const PROTOCOLS = [PROTOCOL, 'duel-6'];
// 🛰️ Rooms kept on another box, nearer some players (server.mjs satellite):
// a page opens its socket for one of these there instead of at home. `east`
// is the first, on the New Jersey box (2026-11-25: 78 ms from Puerto Rico
// against lith's 114). Local pages always play at home.
// `e1n0`, the block east of the plaza, is the first of the world's own blocks
// kept there: walked into from the lobby and out again (server.mjs passages).
export const ROOM_HOSTS = { east: 'nj.rax.ac', e1n0: 'nj.rax.ac' };
// 🏠 One room (2026-11-27). Three to six people are on the whole network at a
// time; spread over a lattice of blocks, a tower of floors, a basement and
// two planets they never met. So for now rax is one studio: everyone lands in
// HOME_ROOM (the plaza, walled on all four sides) and no way leads out of it.
// The blocks, the tower, the basement, the planets and the other host are all
// still here behind this one word: set it false and the world opens again.
// A room that is not a place in the world (a game's arena, a room made by
// name) is still its own room.
export const ONE_ROOM = true;
// 🧭 2026-11-27: home is no longer the plaza. It is `origin`, the first cube
// of the geometry to come (GEOMETRY.md): for now one open platform. The old
// world's plaza is still there as it stood, keyed `lobby`, and is reached
// only by asking for it: rax.ac/~plaza.
export const HOME_ROOM = 'origin';
export const PLAZA = 'lobby', PLAZA_WORD = 'plaza';
// 👹 The stage (2026-11-27): home is bare but for one gun in its middle, and
// heads fly in from outside in waves (match.mjs stepHeads). What stood
// there was taken out once (server.mjs, kept by the store) and its AC windows stand in the studio.
export const STAGE = HOME_ROOM;
// ✨ In the one room everyone builds, and what is built is a conjuring: it
// stands this long and is gone (the keeper's stays: the room is theirs to furnish).
export const CONJURE_MS = 60_000;
export const worldPlace = (id) => id === BASEMENT || id === 'lobby' || id === 'roof' || id === 'east' || /^(floor-|pod-)/.test(id || '') || /^[ew]\d{1,4}[ns]\d{1,4}$/.test(id || '') || !!cubeOf(id); // (🧊 a cube of the lattice, lattice.mjs: until cubes are rooms of their own, every one is home)
export const placeFor = (room, one = ONE_ROOM) => (room === PLAZA_WORD ? PLAZA : one && worldPlace(room) && room !== PLAZA ? HOME_ROOM : room);
// 🏙️ Where each host stands, for the line beside the ping: home (lith) is in
// Santa Clara, the satellite in New Jersey. A host not listed shows its name.
export const HOME_CITY = 'santa clara';
export const HOST_CITIES = { 'nj.rax.ac': 'new jersey', 'rax-nj.aesthetic.computer': 'new jersey' };
export const servedFrom = (url, where = globalThis.location) =>
  /^(localhost|127\.|\[::1\])/.test(url.hostname) ? 'local'
  : HOST_CITIES[url.host] || (url.host === where?.host || /(^|\.)(aesthetic\.computer|rax\.ac|prompt\.ac)$/.test(url.host) && !/^(rax-|nj\.)/.test(url.host) ? HOME_CITY : url.host);
export function playURL(room, home, where = globalThis.location, one = ONE_ROOM) {
  const away = one ? null : ROOM_HOSTS[room], local = /^(localhost|127\.|\[::1\])/.test(where?.hostname || '');
  const url = away && !local ? new URL(`wss://${away}/rax/play`) : new URL('./play', home);
  if (!(away && !local)) url.protocol = where?.protocol === 'https:' ? 'wss:' : 'ws:';
  return url;
}
export const FRAG_LIMIT = 10;
export const COLORS = [0xff593b, 0x69bfff, 0xffd23f, 0x5ad17a, 0xc77dff, 0xff8fc7, 0x3fe0d0, 0xff9f40];
// Players one room holds at once. Seats 0 and 1 keep the original duel's
// spawns; the rest stand on a ring facing the middle (2026-11-08).
export const MAX_PLAYERS = 16;
const SPAWN_YAWS = [0, 180, 90, 270, 45, 225, 135, 315];
export const seatColor = (seat) => COLORS[((seat % COLORS.length) + COLORS.length) % COLORS.length];
// Half the width of a room in metres, unless the room says otherwise: a
// small studio. A room's own half-width comes from halfOf (2026-11-18);
// everything that bounds a body, a ray or a build takes it as `half`.
export const WORLD = 8;
// ✏️ Chalk (drawing in the air, and building from the drawing) is retired (2026-11-20): nothing offers it, nobody's strokes are taken or shown. The code stands behind this one word until it is cut out.
export const CHALK = false;
// The tower's rooms (the plaza and every floor) share one size. It is the
// studio's for now; the rest of rax no longer assumes so.
export const TOWER_HALF = WORLD;
export const halfOf = (room) => (room === BASEMENT ? SEA_HALF : room === 'lobby' || /^(floor-|roof$)/.test(room || '') ? TOWER_HALF : WORLD);
// The farthest a point in any room can be from its middle.
export const MAX_HALF = Math.max(WORLD, TOWER_HALF, SEA_HALF);
export const SCREEN = 40;
// The floor's paints, by the number a painted square stores (1 to 6).
export const TILE_COLORS = [0xd9534f, 0xe0b23a, 0x3fa55b, 0x3f86c9, 0x8e5bd6, 0xd66fae];

const box = (xMin, xMax, yMin, yMax, zMin, zMax) => ({
  type: 'box',
  xMin,
  xMax,
  yMin,
  yMax,
  zMin,
  zMax,
});

// 🚶 A side with a neighbour has no wall: a body walks on past it, this far
// at most, while the next block takes it over (the ground is drawn across
// the join, so nothing stops at the line).
export const OVERRUN = 16;
const SIDES = ['west', 'east', 'south', 'north']; // in WALLS' order
// A room's four walls, drawn by the view (buildArena) at the same edges:
// one list for each size, the same list every time it is asked for.
const walled = new Map();
export function wallsOf(half = WORLD) {
  if (!walled.has(half))
    walled.set(half, [
      box(-half - 1, -half, 0, 60, -half - 1, half + 1),
      box(half, half + 1, 0, 60, -half - 1, half + 1),
      box(-half, half, 0, 60, -half - 1, -half),
      box(-half, half, 0, 60, half, half + 1),
    ]);
  return walled.get(half);
}
export const WALLS = wallsOf(WORLD);
// 🧱 A platform is a volume (2026-11-27): a block this deep whose top is the
// floor. The room made of one has no walls and no ground but the block: a
// body walks off its edge and falls, its sides are solid from below, and
// under FALL_Y the body is stood back where it arrives (match.mjs).
// It rides among a room's fixed boxes, marked `slab`; move() takes it from there.
// 🏙️ (2026-11-27: the platform is the roof of a skyscraper. TOWER_DROP metres down is the street, and a body that falls that far has hit it: FALL_Y, the eye's height over the street.)
export const TOWER_DROP = 120;
export const PLATFORM_DEPTH = 2, FALL_Y = -TOWER_DROP + 1.6;
const ABYSS = -1e4;
const slabs = new Map();
export function slabOf(half = WORLD) {
  if (!slabs.has(half)) slabs.set(half, { ...box(-half, half, -PLATFORM_DEPTH, 0, -half, half), slab: true });
  return slabs.get(half);
}
// 🪜 The fire escape (2026-11-27): a landing all round the roof, ESCAPE_DROP
// under its edge and ESCAPE_WIDE out from it, with a rail along its outer
// side; and on the east and west sides a flight of steps back up to the roof.
// Walk off the edge and you land on it and can climb back; run or jump off
// and you clear the rail and go down to the street. roofBoxes is everything a
// roof room has: its block (slabOf), then these, as move() reads them (a
// `platform` is stood on from above, a `stair` is climbed, the rest are solid).
// (Ten steps of 26 cm: a body steps up 32 cm at the most, and eight of 32.5 were a wall.)
export const ESCAPE_DROP = 2.6, ESCAPE_WIDE = 2.4, ESCAPE_STEPS = 10, ESCAPE_RAIL = 1, ESCAPE_RUN = 0.64;
const roofs = new Map();
export function roofBoxes(half = WORLD) {
  if (!roofs.has(half)) {
    const out = half + ESCAPE_WIDE, top = -ESCAPE_DROP, list = [slabOf(half)];
    const landing = (xMin, xMax, zMin, zMax) => list.push({ ...box(xMin, xMax, top - 0.2, top, zMin, zMax), platform: true, escape: 'landing' });
    landing(half, out, -out, out); landing(-out, -half, -out, out); landing(-half, half, half, out); landing(-half, half, -out, -half);
    const rail = (xMin, xMax, zMin, zMax) => list.push({ ...box(xMin, xMax, top, top + ESCAPE_RAIL, zMin, zMax), escape: 'rail' });
    rail(out - 0.1, out, -out, out); rail(-out, -out + 0.1, -out, out); rail(-out, out, out - 0.1, out); rail(-out, out, -out, -out + 0.1);
    // The steps: each a block from the landing up to its own tread, the last tread level with the roof beside it.
    const rise = ESCAPE_DROP / ESCAPE_STEPS, run = ESCAPE_RUN;
    for (const side of [1, -1]) for (let k = 0; k < ESCAPE_STEPS; k++) {
      const z0 = side * (-2 + k * run), z1 = side * (-2 + (k + 1) * run);
      list.push({ ...box(side > 0 ? half : -half - 1.3, side > 0 ? half + 1.3 : -half, top, top + (k + 1) * rise, Math.min(z0, z1), Math.max(z0, z1)), stair: true, escape: 'step' });
    }
    roofs.set(half, list);
  }
  return roofs.get(half);
}
export const CFG = Object.freeze({
  ...DEFAULT_CFG,
  simHz: HZ,
  groundY: 0,
  eyeHeight: 1.7,
  runSpeed: 10,
  jumpVelocity: 10,
  gravity: 28,
  airAccel: 35,
  groundAccel: 14,
  airCapSpeed: 1.5,
  groundFriction: 8,
  playerRadius: 0.4,
  obstacles: WALLS,
});
export const FIRE_COOLDOWN = 42;
const DIVE = 30; // extra fall, m/s², while ducking in the air
export const clamp = (n, lo, hi) => Math.min(hi, Math.max(lo, n));

// Stand back from the middle along the seat's yaw, so every spawn faces in.
// Seats past the first eight take an inner ring, turned half a step.
export function spawnPoint(seat, half = WORLD) {
  const outer = seat < SPAWN_YAWS.length;
  const yaw = SPAWN_YAWS[seat % SPAWN_YAWS.length] + (outer ? 0 : 22.5);
  const r = outer ? half - 3 : half - 5.5;
  const f = direction(yaw);
  return { x: Math.round(-f.x * r * 1000) / 1000 || 0, z: Math.round(-f.z * r * 1000) / 1000 || 0, yaw };
}

export function spawn(seat, life = 1, half = WORLD) {
  return {
    ...newState({ ...spawnPoint(seat, half), cfg: CFG }),
    seat,
    life,
    health: 100,
    score: 0,
    cooldown: 0,
    respawnAt: 0,
  };
}

// `fixed` is the place's own solid boxes (a block's house walls).
// A `platform` among them is a ledge: stood on from above, never a wall.
// `flat` keeps the body on the z = 0 plane (a side-on arena).
// `open` names the sides that lead to a neighbour (['east', 'south']).
// `half` is the room's half-width (halfOf).
const CAR_DOOR = carDoor();
const NO_HUNG = [];
// A bike against a planet is a ball this big, its middle this far under the eyes: on top that leaves it hovering as it does over the land (bike.mjs hover + seat).
const BIKE_MIDDLE = 1.3, BIKE_ROUND = 1.4;
export function move(player, cmd, objects = [], time = 0, fixed = [], flat = false, open = null, half = WORLD) {
  const WALLS = wallsOf(half);
  if (!player.alive) return { ...player };
  // 🪐 The planets hung over this room (orbits.mjs hungFor) are balls among
  // its solids, not boxes: a bike is kept out of them, their tops are ground.
  const hung = fixed.some((b) => b.ball) ? fixed.filter((b) => b.ball) : NO_HUNG;
  if (hung.length) fixed = fixed.filter((b) => !b.ball);
  const poolRoom = fixed.includes(POOL);
  // 🧱 A platform's block: the only ground there is, with nothing round it.
  const slab = fixed.find((b) => b.slab) || null;
  if (slab) fixed = fixed.filter((b) => !b.slab);
  const groundAt = (p) => (poolRoom ? POOL.bottom : !slab || (p.x >= slab.xMin && p.x <= slab.xMax && p.z >= slab.zMin && p.z <= slab.zMax) ? 0 : ABYSS);
  // pmove's +X strafe faces the other way from a +Z-looking Three camera.
  const boxes = objectBoxes(objects, time),
    feet = player.y - CFG.eyeHeight;
  const under = (b, p) => p.x >= b.xMin && p.x <= b.xMax && p.z >= b.zMin && p.z <= b.zMax;
  // 🦆 Ducking (flat rooms): ledges let the body through, a fall is faster,
  // and on the ground the head is lower (bop.mjs).
  const duck = flat && !!cmd.duck;
  const afloat = poolRoom && (player.swimming || player.boat);
  const solid = fixed.some((b) => b.platform || b.stair || b.ceiling) ? fixed.filter((b) => !b.platform && !b.stair && !b.ceiling && !b.water) : fixed,
    ledges = duck ? [] : fixed.filter((b) => b.platform || b.stair);
  // 🛵 The cabin bike (bike.mjs): B gets on and off; on it the body flies,
  // hovering over the highest thing under it, and a build stops it.
  // (Getting on is judged where the body stands; once on, only water or a side-on arena takes the bike away: a roof just stops it rising.)
  const riding = cmd.bike ? !player.bike && rideable(fixed, flat, player) : !!player.bike && !flat && !poolRoom;
  if (riding) {
    let below = poolRoom ? POOL.bottom : 0;
    for (const b of [...boxes, ...fixed]) if (!b.ceiling && under(b, player) && b.yMax <= feet + 0.3) below = Math.max(below, b.yMax);
    // 🪐 Over a planet's top, that is what it hovers on (so it comes down to rest there, and does not slide off the round of it).
    for (const b of hung) { const top = topOf(b, player.x, player.z); if (top !== null && player.y - BIKE_MIDDLE > b.y) below = Math.max(below, top); }
    const next = fly(player, cmd, below, DT, CFG.eyeHeight);
    // What stops it: builds, and the room's own walls (glass, the car, a dispenser), its keel to the top of its cabin.
    // A move is refused only if it comes nearer than the bike's half-width to
    // one, or goes further in; along a wall it slides, and away it is free
    // (so a bike that drifted in close is never stuck there).
    const walls = [...boxes, ...solid.filter((b) => !b.water)];
    const depth = (b, q) => Math.max(b.xMin - 0.6 - q.x, q.x - b.xMax - 0.6, b.zMin - 0.6 - q.z, q.z - b.zMax - 0.6);
    const level = (b, q) => q.y - CFG.eyeHeight - 0.9 < b.yMax - 0.3 && q.y + 0.3 > b.yMin;
    const blocked = (q) => walls.some((b) => level(b, q) && depth(b, q) < 0 && depth(b, q) < depth(b, player) - 1e-9);
    if (blocked(next)) {
      if (!blocked({ ...next, z: player.z })) { next.z = player.z; next.vz = 0; }
      else if (!blocked({ ...next, x: player.x })) { next.x = player.x; next.vx = 0; }
      else { next.x = player.x; next.z = player.z; next.vx = next.vz = 0; }
    }
    // A roof stops it rising (the cabin's top is just over the eyes).
    for (const b of fixed) if (b.ceiling && under(b, next) && player.y + 0.3 <= b.yMin && next.y + 0.3 > b.yMin) { next.y = b.yMin - 0.3; next.vy = Math.min(0, next.vy); }
    // 🪐 A planet stops it: the bike (a ball about its seat) is kept clear of the planet's, and slides round it. On top that is its hover.
    for (const b of hung) {
      const dx = next.x - b.x, dy = next.y - BIKE_MIDDLE - b.y, dz = next.z - b.z, d = Math.hypot(dx, dy, dz), need = b.r + BIKE_ROUND;
      if (d >= need || d < 1e-6 || (dy > 0 && topOf(b, next.x, next.z) !== null)) continue; // (over its top the hover holds it: above)
      const nx = dx / d, ny = dy / d, nz = dz / d, into = next.vx * nx + next.vy * ny + next.vz * nz;
      next.x = b.x + nx * need; next.y = b.y + ny * need + BIKE_MIDDLE; next.z = b.z + nz * need;
      if (into < 0) { next.vx -= into * nx; next.vy -= into * ny; next.vz -= into * nz; }
    }
    const past = (side) => (open?.includes(side) ? OVERRUN : 0);
    next.x = clamp(next.x, -half + 0.4 - past('west'), half - 0.4 + past('east'));
    next.z = clamp(next.z, -half + 0.4 - past('south'), half - 0.4 + past('north'));
    return next;
  }
  if (player.bike) player = { ...player, bike: false, onGround: false }; // got off (or rode somewhere a bike is not): from here the body falls
  const support = [...boxes, ...ledges].find((b) => under(b, player) && Math.abs(feet - b.yMax) < 0.02);
  // 🪐 Standing on a planet: its top under the feet is the ground.
  let dome = null;
  if (player.onGround) for (const b of hung) { const top = topOf(b, player.x, player.z); if (top !== null && Math.abs(feet - top) < 0.06) dome = top; }
  const grounded = player.onGround && (feet < groundAt(player) + 0.02 || !!support || dome !== null);
  const step = (grounded || afloat) && !(cmd.buttons & 1) ? 0.32 : 0.001;
  const cfg = {
    ...CFG,
    airCapSpeed: poolRoom || fixed.some(b => b.window) || open === towerSides ? 4.3 : CFG.airCapSpeed,
    groundY: support?.yMax ?? dome ?? groundAt(player),
    ...(afloat ? {runSpeed: player.boat ? 4.3 : 2.4, walkSpeed: 1.7, hVelDecay: player.boat ? 0.955 : 0.88, jumpVelocity: 0, airAccel: undefined} : {}),
    // (A platform has no walls; its block is solid from the side to a body that is down past its top.)
    obstacles: [...(slab ? (feet < slab.yMax - 0.32 ? [slab] : []) : open?.length ? WALLS.filter((_, i) => !open.includes(SIDES[i])) : WALLS), ...solid, ...boxes.filter((b) => b.yMax > feet + step),
      ...(player.shut > 0 ? [CAR_DOOR] : [])], // 🛗 riding: the car's door is shut for this body (`shut` ticks to go)
  };
  const next = pmove(
    { ...player, onGround: grounded },
    { ...cmd, right: -(cmd.right || 0), dt: DT },
    cfg,
  );
  if (player.shut > 0) next.shut = player.shut - 1;
  // Shared step-up, landing and head collision; generated boxes are solid on all sides.
  const nextFeet = next.y - CFG.eyeHeight;
  let floor = groundAt(next);
  // (Off a platform's edge the top is ground only to a body that was above it: one fallen past is not lifted back.)
  if (slab && floor === 0 && feet < -0.32) floor = ABYSS;
  for (const b of boxes) {
    if (!under(b, next)) continue;
    if (b.yMax <= feet + step && (nextFeet <= b.yMax || (grounded && nextFeet <= b.yMax + step)))
      floor = Math.max(floor, b.yMax);
    if (next.vy > 0 && player.y + 0.2 <= b.yMin && next.y + 0.2 >= b.yMin) {
      next.y = b.yMin - 0.2;
      next.vy = 0;
    }
  }
  // A ledge catches only what was above it.
  for (const b of ledges)
    if (under(b, next) && feet + (b.stair && (grounded || afloat) ? step : 0.02) >= b.yMax && nextFeet <= b.yMax + (b.stair && (grounded || afloat) ? step : 0.001)) floor = Math.max(floor, b.yMax);
  if (next.vy <= 0 && next.y - CFG.eyeHeight <= floor + (grounded ? step : 0.001)) {
    next.y = floor + CFG.eyeHeight;
    next.vy = 0;
    next.onGround = true;
  } else if (next.y - CFG.eyeHeight > floor + 0.001) next.onGround = false;
  // 🪐 A planet's top is ground that rises and falls underfoot: a body on it
  // keeps to it, a body come down on it lands. Past where it can be stood on
  // its side is solid: the body is put out of it, and falls.
  for (const b of hung) {
    const top = topOf(b, next.x, next.z), f = next.y - CFG.eyeHeight;
    if (top !== null && next.vy <= 0 && (dome !== null ? f <= top + 0.6 : f <= top && feet >= top - 0.6)) {
      next.y = top + CFG.eyeHeight; next.vy = 0; next.onGround = true;
      continue;
    }
    const dx = next.x - b.x, dz = next.z - b.z, dy = next.y - 0.8 - b.y, need = b.r + 0.4;
    if (Math.abs(dy) >= need) continue;
    const want = Math.sqrt(need * need - dy * dy), flatly = Math.hypot(dx, dz);
    if (flatly >= want) continue;
    if (flatly < 1e-6) next.x = b.x + want; else { next.x = b.x + (dx * want) / flatly; next.z = b.z + (dz * want) / flatly; }
  }
  if (next.vy > 0) for (const b of fixed) {
    if (b.ceiling && under(b, next) && player.y + 0.15 <= b.yMin && next.y + 0.15 >= b.yMin) {
      next.y = b.yMin - 0.15;
      next.vy = 0;
    }
  }
  // Bound speed also bounds discrete collision steps.
  const speed = Math.hypot(next.vx, next.vz);
  if (speed > 22) {
    next.vx *= 22 / speed;
    next.vz *= 22 / speed;
  }
  const past = (side) => (open?.includes(side) ? OVERRUN : 0);
  if (!slab) { // (a platform's edge holds nobody)
    next.x = clamp(next.x, -half + 0.4 - past('west'), half - 0.4 + past('east'));
    next.z = clamp(next.z, -half + 0.4 - past('south'), half - 0.4 + past('north'));
  }
  if (poolRoom) floatInPool(player, next, cmd, DT);
  else if (next.boat || next.swimming) { delete next.boat; delete next.swimming; delete next.poolJump; }
  if (flat) {
    next.z = next.vz = 0;
    next.duck = duck;
    if (duck && !next.onGround) next.vy -= DIVE * DT;
  }
  return next;
}

export function direction(yaw, pitch = 0) {
  const y = (yaw * Math.PI) / 180,
    p = (pitch * Math.PI) / 180;
  return { x: Math.sin(y) * Math.cos(p), y: Math.sin(p), z: Math.cos(y) * Math.cos(p) };
}

// Slab intersection: distance along a normalized ray, including an origin inside a box.
export function rayBox(origin, dir, box, max = 1000) {
  let near = 0,
    far = max;
  for (const axis of ['x', 'y', 'z']) {
    const lo = box[`${axis}Min`],
      hi = box[`${axis}Max`];
    if (Math.abs(dir[axis]) < 1e-8) {
      if (origin[axis] < lo || origin[axis] > hi) return Infinity;
    } else {
      const a = (lo - origin[axis]) / dir[axis],
        b = (hi - origin[axis]) / dir[axis];
      near = Math.max(near, Math.min(a, b));
      far = Math.min(far, Math.max(a, b));
      if (near > far) return Infinity;
    }
  }
  return near;
}

export function wallDistance(origin, dir, max = 1000, objects = [], time = 0, half = WORLD) {
  let hit = max;
  for (const wall of [...wallsOf(half), ...objectBoxes(objects, time)])
    hit = Math.min(hit, rayBox(origin, dir, wall, max));
  if (dir.y < 0 && origin.y >= 0) hit = Math.min(hit, -origin.y / dir.y);
  return hit;
}

// The first surface along a ray, with the face normal there: walls, solid
// objects and the floor. { t, normal } or null within max.
// `open`: the sides that lead on to a neighbour (east, west, north, south):
// no wall stands there, so a round flies on through (match.mjs hands it over).
// `slab`: the room is a platform (slabOf): no walls stand round it and the
// only floor is its block, which a round meets on any face.
export function surfaceHit(origin, dir, max = 1000, objects = [], time = 0, half = WORLD, open = null, slab = null) {
  let best = null;
  const walls = slab ? [slab] : open?.length ? wallsOf(half).filter((_, k) => !open.includes(SIDES[k])) : wallsOf(half);
  for (const b of [...walls, ...objectBoxes(objects, time)]) {
    let near = -Infinity, far = max, axis = null, sign = 0;
    let miss = false;
    for (const k of ['x', 'y', 'z']) {
      const lo = b[`${k}Min`], hi = b[`${k}Max`];
      if (Math.abs(dir[k]) < 1e-8) {
        if (origin[k] < lo || origin[k] > hi) { miss = true; break; }
        continue;
      }
      const a = (lo - origin[k]) / dir[k], c = (hi - origin[k]) / dir[k];
      const enter = Math.min(a, c);
      if (enter > near) {
        near = enter;
        axis = k;
        sign = dir[k] > 0 ? -1 : 1;
      }
      far = Math.min(far, Math.max(a, c));
      if (near > far) { miss = true; break; }
    }
    if (miss || near < 0 || near > max || !axis) continue;
    if (!best || near < best.t)
      best = { t: near, normal: { x: axis === 'x' ? sign : 0, y: axis === 'y' ? sign : 0, z: axis === 'z' ? sign : 0 } };
  }
  if (!slab && dir.y < 0 && origin.y >= 0) {
    const t = -origin.y / dir.y;
    if (t <= max && (!best || t < best.t)) best = { t, normal: { x: 0, y: 1, z: 0 } };
  }
  return best;
}

// 🔫 Where a held thing's barrel ends, in the thing's own metres: the face of
// its part that reaches furthest forward (+z, the way it points in the hand),
// as { y, z } from its origin. A round leaves from there (muzzleAt), not from
// the middle of the hand, so it comes out of the hole the model shows.
export function muzzleOf(geometry) {
  let best = null;
  for (const b of geometry?.boxes || []) {
    const z = b.z + b.d / 2;
    if (Number.isFinite(z) && Number.isFinite(b.y) && (!best || z > best.z)) best = { y: b.y, z };
  }
  return best;
}
// The hand (match.mjs hand(), view.mjs draws the thing there), and from it the muzzle: up the thing's own up, out along the aim.
export function muzzleAt(p, muzzle = null) {
  const f = direction(p.yaw, p.pitch), r = direction(p.yaw - 90);
  const h = { x: p.x + r.x * 0.3 + f.x * 0.45, y: p.y - 0.55 + f.y * 0.45, z: p.z + r.z * 0.3 + f.z * 0.45 };
  if (!muzzle) return h;
  const u = { x: -f.y * r.z, y: f.x * r.z - f.z * r.x, z: f.y * r.x };
  return { x: h.x + u.x * muzzle.y + f.x * muzzle.z, y: h.y + u.y * muzzle.y + f.y * muzzle.z, z: h.z + u.z * muzzle.y + f.z * muzzle.z };
}

// Reflect velocity v about normal n, keeping `bounce` of the normal part
// and `slide` of the tangential part.
export function reflect(v, n, bounce = 1, slide = 1) {
  const d = v.x * n.x + v.y * n.y + v.z * n.z;
  return {
    x: (v.x - d * n.x) * slide - d * n.x * bounce,
    y: (v.y - d * n.y) * slide - d * n.y * bounce,
    z: (v.z - d * n.z) * slide - d * n.z * bounce,
  };
}

export function bodyBox(p) {
  return {
    xMin: p.x - 0.4,
    xMax: p.x + 0.4,
    yMin: p.y - CFG.eyeHeight,
    yMax: p.y + 0.2,
    zMin: p.z - 0.4,
    zMax: p.z + 0.4,
  };
}

export const along = (p, d, distance) => ({
  x: p.x + d.x * distance,
  y: p.y + d.y * distance,
  z: p.z + d.z * distance,
});
