// 🍄 botbop: land on a head. The one rule runs in every room of rax; a
// room named `botbop` is its arena, a side-on stage where bodies keep to
// one plane, ledges are stood on from above and jumped through from below,
// and a bopped player is set down again somewhere else.
import { CFG, HZ, WORLD } from './world.mjs';

export const NAME = 'botbop';
// (It opened as `hedbop` on 2026-11-16; that address still leads here.)
const NAMES = [NAME, 'hedbop'];
export const isArena = (room) => NAMES.some((n) => room === n || String(room || '').startsWith(`${n}-`));

// A body is 1.9 m from feet to crown and a standing jump lifts the feet
// 1.79 m, so the band reaches a little below the crown: timed at the top of
// a jump, a bop lands on flat ground; from a ledge it is easy.
export const BOP = Object.freeze({
  reach: 0.7, // metres between the two bodies' middles
  low: 1.2, // the stomper's feet above the other's feet, at least
  high: 2.0, // and at most
  bounce: 9, // the stomper leaves at this speed, upward
  rest: Math.round(HZ * 0.75), // ticks before the same head can be bopped again
  duck: 0.45, // how far a ducking head drops
});
// How far below standing height `q`'s crown is right now.
export const ducked = (q) => (q?.duck && q.onGround ? BOP.duck : 0);

// Is `a` coming down onto `q`'s head?
export function lands(a, q) {
  if (!a?.alive || !q?.alive || a.onGround || a.vy > -0.5 || a.vy - (q.vy || 0) > -0.5) return false;
  const up = a.y - q.y + ducked(q);
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

// 🤖 Drones: dumb robots, not bots. Each walks its own stretch back and
// forth and knows nothing else; landing on one counts as a bop, it is gone,
// and another rolls out a few seconds later.
export const DRONE = Object.freeze({ tall: 1.1, speed: 1.6, away: 3 * HZ });
export function drones(ledges = LEDGES) {
  const walk = (lo, hi, y, k) => ({ id: k, x: lo + ((hi - lo) * ((k * 37) % 100)) / 100, y, lo, hi, dir: k % 2 ? 1 : -1, alive: true, backAt: 0 });
  return [
    walk(-WORLD + 1, -1, 0, 0),
    walk(1, WORLD - 1, 0, 1),
    ...ledges.slice(0, 3).map((l, k) => walk(l.xMin + 0.4, l.xMax - 0.4, l.yMax, k + 2)),
  ];
}
export function stepDrone(d, tick) {
  if (!d.alive) {
    if (tick < d.backAt) return;
    d.alive = true;
    d.x = d.dir > 0 ? d.lo : d.hi; // rolls out from the end it was heading away from
  }
  d.x += (d.dir * DRONE.speed) / HZ;
  if (d.x >= d.hi) { d.x = d.hi; d.dir = -1; }
  if (d.x <= d.lo) { d.x = d.lo; d.dir = 1; }
}
// Is `a` coming down onto drone `d`?
export function landsOnDrone(a, d) {
  if (!a?.alive || !d.alive || a.onGround || a.vy > -0.5) return false;
  const up = a.y - CFG.eyeHeight - d.y; // the stomper's feet above the drone's
  return up >= DRONE.tall - 0.5 && up <= DRONE.tall + 0.25 && Math.hypot(a.x - d.x, a.z) < BOP.reach;
}

// Where a body is set down in the arena: spread along the floor, moving on
// with each life so nobody comes back under the same feet.
export function arenaSpawn(seat, life = 1) {
  const span = 2 * (WORLD - 1.5), step = (seat * 5 + life * 3) % 13;
  const x = Math.round((-span / 2 + (step / 12) * span) * 100) / 100;
  return { x, z: 0, y: CFG.eyeHeight, yaw: x > 0 ? 270 : 90, pitch: 0 };
}

// The arena's camera: far outside the plane and zoomed in, so the stage
// reads nearly flat (+x is screen right from here; the stage has no wall on
// this side). It frames the player and whoever is near enough to bop: wide
// when they are apart, close when they meet, the whole stage when the window
// has room for it. `aspect` is the canvas's width over its height.
const FAR = 30;
export function arenaCamera(me, players = [], aspect = 16 / 9) {
  const x = me?.x || 0;
  let lo = x, hi = x;
  for (const p of players) {
    if (!p?.alive || Math.abs(p.x - x) > 9) continue;
    lo = Math.min(lo, p.x);
    hi = Math.max(hi, p.x);
  }
  const half = Math.max(4.5, (hi - lo) / 2 + 3); // metres to each side of the middle
  const t = aspect < 1 ? Math.min(Math.sqrt(3), 1 / aspect) : 1; // Projector.camera's lens
  const zoom = Math.min((FAR * t * aspect) / half, (FAR * t) / 3.6);
  const seen = (FAR * t * aspect) / zoom; // what that zoom really shows to each side
  const edge = Math.max(0, WORLD + 0.6 - seen);
  const feet = (me?.y || CFG.eyeHeight) - CFG.eyeHeight;
  return {
    x: Math.max(-edge, Math.min(edge, (lo + hi) / 2)),
    y: 3.3 + Math.max(0, feet - 2.5) * 0.5,
    z: FAR, yaw: 180, pitch: -1.5, zoom,
  };
}

// 🎮 The gametype, as the client and the view read it (games.mjs): what the
// room is made of, where the camera stands, which controls the legend names
// and which parts of rax's own interface stay out of the way.
export const GAME = Object.freeze({
  name: NAME,
  at: isArena,
  flat: true,
  ledges: LEDGES,
  camera: arenaCamera,
  hud: Object.freeze({ reticle: false, face: false, aim: false }),
  legend: Object.freeze({
    keys: [
      [['←', ['ArrowLeft', 'KeyA']], ['→', ['ArrowRight', 'KeyD']], ' move'],
      [['↑', ['ArrowUp', 'KeyW', 'Space']], ' jump / ', ['↓', ['ArrowDown', 'KeyS']], ' duck'],
      ['land on a head'],
      [['t', 'KeyT'], ' chat / ', ['$', 'Digit4'], ' menu / ', ['`', 'Backquote'], ' boxes'],
    ],
    pad: ['[LS] move / [LT] jump', '[LS] down to duck', 'land on a head', '[START] $ menu'],
    touch: [],
  }),
});
