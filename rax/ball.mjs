// ⚽ The ball
// A dispensed football belongs to no room: the world's clock
// rolls it, and a room only lends it what stands there (walls, builds,
// bodies). At a block's line it goes on into the next block at the same
// speed, the same way a walker does, and where no block is open it comes
// back off the line. Everyone within a block of it is told where it is, in
// their own room's metres, so it is seen rolling over a seam from both sides.
import { DT, CFG, reflect } from './world.mjs';
import { SPAN } from './park.mjs';

export const BALL = { r: 0.34, room: 'lobby', x: 3, z: -5 };
export const DISPENSER = { x: BALL.x, z: BALL.z, width: 1, depth: 0.8, height: 1.3 };
export const DISPENSER_WALL = {
  type: 'box', xMin: DISPENSER.x - DISPENSER.width / 2, xMax: DISPENSER.x + DISPENSER.width / 2,
  zMin: DISPENSER.z - DISPENSER.depth / 2, zMax: DISPENSER.z + DISPENSER.depth / 2,
  yMin: 0, yMax: DISPENSER.height,
};
// The west-facing chute opens onto the walk, clear of the benches to the south.
const chute = { room: BALL.room, x: DISPENSER_WALL.xMin - BALL.r - 0.04, z: BALL.z };
export const MAX_BALLS = 12;

// Each approach trips the chute once. At capacity, reuse a resting ball.
export function createDispenser(world) {
  const make = () => createBall({ ...world, start: chute });
  const balls = [make()];
  let nearby = new Set(), next = 0;
  return {
    balls, make,
    step() {
      const visitors = new Set();
      for (const p of world.bodies(BALL.room)) {
        if (Math.abs(p.y - CFG.eyeHeight) < 1 && Math.hypot(p.x - BALL.x, p.z - BALL.z) <= 1.55)
          visitors.add(p.seat);
      }
      for (const seat of visitors) {
        if (nearby.has(seat)) continue;
        let slot = balls.find(b => b.still() && b.ball.room === chute.room &&
          Math.hypot(b.ball.x - chute.x, b.ball.z - chute.z) < 0.1);
        if (!slot && balls.length < MAX_BALLS) { slot = make(); balls.push(slot); }
        if (!slot) {
          for (let i = 0; i < balls.length; i++) {
            const index = (next + i) % balls.length;
            if (balls[index].still()) { slot = balls[index]; next = (index + 1) % balls.length; break; }
          }
        }
        if (slot) Object.assign(slot.ball, chute, { y: 0.65, vx: -3, vy: 1.4, vz: 0 });
      }
      nearby = visitors;
      let moved = false;
      for (const sim of balls) if (sim.step()) moved = true;
      return moved;
    },
  };
}

const G = 22, BOUNCE = 0.55, ROLL = 0.6, REACH = 0.4 + BALL.r, KICK = 1.5, LOFT = 0.35;
const SIDES = [['east', 'x', 1], ['west', 'x', -1], ['north', 'z', 1], ['south', 'z', -1]];

// Where a ray first meets a box grown by the ball's radius: { t, normal }.
function meet(origin, dir, max, boxes) {
  let best = null;
  for (const b of boxes) {
    let near = -Infinity, far = max, axis = null, sign = 0, miss = false;
    for (const k of ['x', 'y', 'z']) {
      const lo = b[`${k}Min`] - BALL.r, hi = b[`${k}Max`] + BALL.r;
      if (Math.abs(dir[k]) < 1e-8) {
        if (origin[k] < lo || origin[k] > hi) { miss = true; break; }
        continue;
      }
      const a = (lo - origin[k]) / dir[k], c = (hi - origin[k]) / dir[k];
      const enter = Math.min(a, c);
      if (enter > near) { near = enter; axis = k; sign = dir[k] > 0 ? -1 : 1; }
      far = Math.min(far, Math.max(a, c));
      if (near > far) { miss = true; break; }
    }
    if (miss || near < 0 || near > max || !axis) continue;
    if (!best || near < best.t) best = { t: near, normal: { x: 0, y: 0, z: 0, [axis]: sign } };
  }
  return best;
}

// neighbours(room) → { east, west, north, south } of open blocks (or null);
// boxes(room) → what is solid there; bodies(room) → the players standing there;
// below(room) → { room, dy } for a floor of the tower: what lies under its
// edge and how far down (the ball rolls off the slab and falls there).
export function createBall({ neighbours, boxes = () => [], bodies = () => [], below = () => null, start = BALL } = {}) {
  const ball = { room: start.room, x: start.x, y: BALL.r, z: start.z, vx: 0, vy: 0, vz: 0, crossings: 0 };
  const still = () => ball.y <= BALL.r + 0.001 && !ball.vx && !ball.vy && !ball.vz;
  // A body meets the ball as a wall that moves: the ball leaves with the
  // body's speed and half as much again, and a little of it goes up.
  const kick = (p, dx, dz) => {
    const bx = p.x + dx, bz = p.z + dz, feet = p.y - CFG.eyeHeight;
    if (ball.y - BALL.r > feet + 1.2 || ball.y + BALL.r < feet) return;
    let nx = ball.x - bx, nz = ball.z - bz;
    const d = Math.hypot(nx, nz);
    if (d >= REACH) return;
    const pace = Math.hypot(p.vx || 0, p.vz || 0);
    if (d < 1e-4) { nx = pace ? p.vx / pace : 1; nz = pace ? p.vz / pace : 0; } else { nx /= d; nz /= d; }
    const closing = ((p.vx || 0) - ball.vx) * nx + ((p.vz || 0) - ball.vz) * nz;
    ball.x = bx + nx * REACH;
    ball.z = bz + nz * REACH;
    if (closing <= 0) return;
    ball.vx += nx * closing * KICK;
    ball.vz += nz * closing * KICK;
    ball.vy = Math.max(ball.vy, Math.min(6, closing * LOFT));
  };
  return {
    ball,
    still,
    // One tick of the world's clock. True when the ball is somewhere new.
    step() {
      const near = neighbours(ball.room) || {};
      for (const p of bodies(ball.room)) kick(p, 0, 0);
      for (const [side, axis, sign] of SIDES)
        if (near[side]) for (const p of bodies(near[side])) kick(p, axis === 'x' ? sign * SPAN : 0, axis === 'z' ? sign * SPAN : 0);
      if (still()) return false;
      ball.vy -= G * DT;
      const speed = Math.hypot(ball.vx, ball.vy, ball.vz);
      const solid = boxes(ball.room);
      const steps = Math.max(1, Math.ceil((speed * DT) / 0.15));
      for (let k = 0; k < steps; k++) {
        const sp = Math.hypot(ball.vx, ball.vy, ball.vz);
        if (sp < 1e-4) break;
        const dir = { x: ball.vx / sp, y: ball.vy / sp, z: ball.vz / sp }, travel = (sp * DT) / steps;
        const hit = solid.length ? meet(ball, dir, travel, solid) : null;
        const go = hit ? Math.max(0, hit.t - 0.01) : travel;
        ball.x += dir.x * go; ball.y += dir.y * go; ball.z += dir.z * go;
        if (hit) {
          const v = reflect({ x: ball.vx, y: ball.vy, z: ball.vz }, hit.normal, BOUNCE, 0.9);
          ball.vx = v.x; ball.vy = v.y; ball.vz = v.z;
        }
      }
      if (ball.y <= BALL.r) {
        ball.y = BALL.r;
        ball.vy = ball.vy < -1.5 ? -ball.vy * BOUNCE : 0;
        const slow = Math.max(0, 1 - ROLL * DT);
        ball.vx *= slow; ball.vz *= slow;
        if (Math.hypot(ball.vx, ball.vz) < 0.08) ball.vx = ball.vz = 0;
      }
      // Over a block's line: on into the next block, or back off the line.
      for (const [side, axis, sign] of SIDES) {
        if (ball[axis] * sign <= SPAN / 2) continue;
        if (near[side]) {
          ball.room = near[side];
          ball[axis] -= sign * SPAN;
          ball.crossings++;
        } else {
          const under = below(ball.room);
          if (under) {
            // Off the slab: the same place over the plaza, that much higher.
            ball.room = under.room;
            ball.y += under.dy;
            ball.crossings++;
          } else {
            ball[axis] = sign * SPAN / 2;
            ball[`v${axis}`] = -ball[`v${axis}`] * BOUNCE;
          }
        }
        break; // one line a tick: a corner is two ticks
      }
      return true;
    },
  };
}

// The ball a moment after it was last heard of (the page, between words
// from the server): on its way, falling, never under the ground.
export function ballAt(b, seconds) {
  const t = Math.max(0, Math.min(1, seconds));
  const y = b.y + b.vy * t - (b.y > BALL.r || b.vy ? (G * t * t) / 2 : 0);
  return { x: b.x + b.vx * t, y: Math.max(BALL.r, y), z: b.z + b.vz * t };
}
