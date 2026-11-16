// 🍄 hedbop: land on a head. The one rule runs in every room of dool; a
// room named `hedbop` is its arena, a side-on stage where bodies keep to
// one plane, ledges are stood on from above and jumped through from below,
// and a bopped player is set down again somewhere else.
import { CFG, HZ, WORLD } from './world.mjs';

export const NAME = 'hedbop';
export const isArena = (room) => room === NAME || String(room || '').startsWith(`${NAME}-`);

// A body is 1.9 m from feet to crown and a standing jump lifts the feet
// 1.79 m, so the band reaches a little below the crown: timed at the top of
// a jump, a bop lands on flat ground; from a ledge it is easy.
export const BOP = Object.freeze({
  reach: 0.7, // metres between the two bodies' middles
  low: 1.2, // the stomper's feet above the other's feet, at least
  high: 2.0, // and at most
  bounce: 9, // the stomper leaves at this speed, upward
  rest: Math.round(HZ * 0.75), // ticks before the same head can be bopped again
});

// Is `a` coming down onto `q`'s head?
export function lands(a, q) {
  if (!a?.alive || !q?.alive || a.onGround || a.vy > -0.5 || a.vy - (q.vy || 0) > -0.5) return false;
  const up = a.y - q.y;
  return up >= BOP.low && up <= BOP.high && Math.hypot(a.x - q.x, a.z - q.z) < BOP.reach;
}

// The arena's ledges: thin, 2 m deep, mirrored about the middle. Each is
// within one jump of the one below.
const ledge = (x, top, w) => ({
  type: 'box', platform: true,
  xMin: x - w / 2, xMax: x + w / 2, yMin: top - 0.2, yMax: top, zMin: -1, zMax: 1,
});
export const LEDGES = Object.freeze([
  ledge(-5, 1.4, 3), ledge(5, 1.4, 3),
  ledge(0, 2.7, 4),
  ledge(-5.4, 4, 2.4), ledge(5.4, 4, 2.4),
].map(Object.freeze));

// Where a body is set down in the arena: spread along the floor, moving on
// with each life so nobody comes back under the same feet.
export function arenaSpawn(seat, life = 1) {
  const span = 2 * (WORLD - 1.5), step = (seat * 5 + life * 3) % 13;
  const x = Math.round((-span / 2 + (step / 12) * span) * 100) / 100;
  return { x, z: 0, y: CFG.eyeHeight, yaw: x > 0 ? 270 : 90, pitch: 0 };
}

// The arena's camera: far outside the plane and zoomed in, so the stage
// reads nearly flat (+x is screen right from here); it follows the body a
// little. The stage has no wall on this side.
export function arenaCamera(p) {
  return { x: (p?.x || 0) * 0.3, y: 3.4 + ((p?.y || CFG.eyeHeight) - CFG.eyeHeight) * 0.4, z: 30, yaw: 180, pitch: -1.5, zoom: 4.6 };
}
