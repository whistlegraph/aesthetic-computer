import { BASEMENT, SEA_HALF, POOL, floatInPool } from './basement.mjs';
import { objectBoxes } from './objects.mjs';
import { towerSides, carDoor } from './elevator.mjs';
import { fly, rideable } from './bike.mjs';
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
export function move(player, cmd, objects = [], time = 0, fixed = [], flat = false, open = null, half = WORLD) {
  const WALLS = wallsOf(half);
  if (!player.alive) return { ...player };
  // pmove's +X strafe faces the other way from a +Z-looking Three camera.
  const boxes = objectBoxes(objects, time),
    feet = player.y - CFG.eyeHeight;
  const under = (b, p) => p.x >= b.xMin && p.x <= b.xMax && p.z >= b.zMin && p.z <= b.zMax;
  // 🦆 Ducking (flat rooms): ledges let the body through, a fall is faster,
  // and on the ground the head is lower (bop.mjs).
  const duck = flat && !!cmd.duck;
  const poolRoom = fixed.includes(POOL);
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
    const past = (side) => (open?.includes(side) ? OVERRUN : 0);
    next.x = clamp(next.x, -half + 0.4 - past('west'), half - 0.4 + past('east'));
    next.z = clamp(next.z, -half + 0.4 - past('south'), half - 0.4 + past('north'));
    return next;
  }
  if (player.bike) player = { ...player, bike: false, onGround: false }; // got off (or rode somewhere a bike is not): from here the body falls
  const support = [...boxes, ...ledges].find((b) => under(b, player) && Math.abs(feet - b.yMax) < 0.02);
  const grounded = player.onGround && (feet < (poolRoom ? POOL.bottom : 0) + 0.02 || !!support);
  const step = (grounded || afloat) && !(cmd.buttons & 1) ? 0.32 : 0.001;
  const cfg = {
    ...CFG,
    airCapSpeed: poolRoom || fixed.some(b => b.window) || open === towerSides ? 4.3 : CFG.airCapSpeed,
    groundY: support?.yMax ?? (poolRoom ? POOL.bottom : 0),
    ...(afloat ? {runSpeed: player.boat ? 4.3 : 2.4, walkSpeed: 1.7, hVelDecay: player.boat ? 0.955 : 0.88, jumpVelocity: 0, airAccel: undefined} : {}),
    obstacles: [...(open?.length ? WALLS.filter((_, i) => !open.includes(SIDES[i])) : WALLS), ...solid, ...boxes.filter((b) => b.yMax > feet + step),
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
  let floor = poolRoom ? POOL.bottom : 0;
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
  next.x = clamp(next.x, -half + 0.4 - past('west'), half - 0.4 + past('east'));
  next.z = clamp(next.z, -half + 0.4 - past('south'), half - 0.4 + past('north'));
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
export function surfaceHit(origin, dir, max = 1000, objects = [], time = 0, half = WORLD) {
  let best = null;
  for (const b of [...wallsOf(half), ...objectBoxes(objects, time)]) {
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
  if (dir.y < 0 && origin.y >= 0) {
    const t = -origin.y / dir.y;
    if (t <= max && (!best || t < best.t)) best = { t, normal: { x: 0, y: 1, z: 0 } };
  }
  return best;
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
