import { objectBoxes } from './objects.mjs';
import { DEFAULT_CFG, newState, pmove } from '../system/public/aesthetic.computer/lib/pmove.mjs';

export const HZ = 60;
export const DT = 1 / HZ;
export const INTERP_TICKS = 6;
export const PROTOCOL = 'duel-6';
export const FRAG_LIMIT = 10;
export const COLORS = [0xff593b, 0x69bfff];

const box = (xMin, xMax, yMin, yMax, zMin, zMax) => ({
  type: 'box',
  xMin,
  xMax,
  yMin,
  yMax,
  zMin,
  zMax,
});

export const WALLS = [
  box(-19, -18, 0, 12, -19, 19),
  box(18, 19, 0, 12, -19, 19),
  box(-18, 18, 0, 12, -19, -18),
  box(-18, 18, 0, 12, 18, 19),
];
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
export const clamp = (n, lo, hi) => Math.min(hi, Math.max(lo, n));

export function spawn(seat, life = 1) {
  return {
    ...newState({ z: seat ? 12 : -12, yaw: seat ? 180 : 0, cfg: CFG }),
    seat,
    life,
    health: 100,
    score: 0,
    cooldown: 0,
    respawnAt: 0,
  };
}

export function move(player, cmd, objects = [], time = 0) {
  if (!player.alive) return { ...player };
  // pmove's +X strafe faces the other way from a +Z-looking Three camera.
  const boxes = objectBoxes(objects, time),
    feet = player.y - CFG.eyeHeight;
  const under = (b, p) => p.x >= b.xMin && p.x <= b.xMax && p.z >= b.zMin && p.z <= b.zMax;
  const support = boxes.find((b) => under(b, player) && Math.abs(feet - b.yMax) < 0.02);
  const grounded = player.onGround && (feet < 0.02 || !!support);
  const step = grounded && !(cmd.buttons & 1) ? 0.32 : 0.001;
  const cfg = {
    ...CFG,
    groundY: support?.yMax || 0,
    obstacles: [...WALLS, ...boxes.filter((b) => b.yMax > feet + step)],
  };
  const next = pmove(
    { ...player, onGround: grounded },
    { ...cmd, right: -(cmd.right || 0), dt: DT },
    cfg,
  );
  // Shared step-up, landing and head collision; generated boxes are solid on all sides.
  const nextFeet = next.y - CFG.eyeHeight;
  let floor = 0;
  for (const b of boxes) {
    if (!under(b, next)) continue;
    if (b.yMax <= feet + step && (nextFeet <= b.yMax || (grounded && nextFeet <= b.yMax + step)))
      floor = Math.max(floor, b.yMax);
    if (next.vy > 0 && player.y + 0.2 <= b.yMin && next.y + 0.2 >= b.yMin) {
      next.y = b.yMin - 0.2;
      next.vy = 0;
    }
  }
  if (next.vy <= 0 && next.y - CFG.eyeHeight <= floor + (grounded ? step : 0.001)) {
    next.y = floor + CFG.eyeHeight;
    next.vy = 0;
    next.onGround = true;
  } else if (next.y - CFG.eyeHeight > floor + 0.001) next.onGround = false;
  // Bound speed also bounds discrete collision steps.
  const speed = Math.hypot(next.vx, next.vz);
  if (speed > 22) {
    next.vx *= 22 / speed;
    next.vz *= 22 / speed;
  }
  next.x = clamp(next.x, -17.6, 17.6);
  next.z = clamp(next.z, -17.6, 17.6);
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

export function wallDistance(origin, dir, max = 1000, objects = [], time = 0) {
  let hit = max;
  for (const wall of [...WALLS, ...objectBoxes(objects, time)])
    hit = Math.min(hit, rayBox(origin, dir, wall, max));
  if (dir.y < 0 && origin.y >= 0) hit = Math.min(hit, -origin.y / dir.y);
  return hit;
}

// The first surface along a ray, with the face normal there: walls, solid
// objects and the floor. { t, normal } or null within max.
export function surfaceHit(origin, dir, max = 1000, objects = [], time = 0) {
  let best = null;
  for (const b of [...WALLS, ...objectBoxes(objects, time)]) {
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
