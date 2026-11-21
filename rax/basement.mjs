import { PLANET } from './planet.mjs';

// The basement uses metres above its own floor. Its upper landing meets the plaza.
export const BASEMENT = 'basement';
export const DEPTH = 4;
export const STAIR = { left: 3.2, right: 5.6, lip: 2.2, end: 6.4, landing: 1.4, steps: 16 };
const { left, right, lip, end, landing, steps } = STAIR;
const tread = (end - lip) / steps;
export const stairHeight = z => DEPTH * Math.max(0, Math.min(steps, Math.ceil((end - z) / tread))) / steps;

const block = (xMin, xMax, zMin, zMax, yMin, yMax, ink) =>
  ({ type: 'box', xMin, xMax, zMin, zMax, yMin, yMax, ink });
const stone = 0x415266, edge = 0x748a98;
export const treads = Array.from({ length: steps }, (_, i) =>
  ({ ...block(left, right, lip + i * tread, lip + (i + 1) * tread, 0, DEPTH * (steps - i) / steps, i % 2 ? stone : edge), stair: true }));
treads.push({ ...block(left, right, landing, lip, 0, DEPTH, edge), stair: true });
export const cellarWalls = [
  block(7.8,8,-8,8,0,DEPTH,stone),
  block(-1.7,7.8,-8,-7.8,0,DEPTH,stone), block(-1.7,7.8,7.8,8,0,DEPTH,stone),
  block(left-0.12,left,lip,end,0,DEPTH+0.35,edge), block(right,right+0.12,lip,end,0,DEPTH+0.35,edge),
  ...[0].map(x => block(x-0.24,x+0.24,-2.25,-1.75,0,DEPTH,edge)),
];
export const ceiling = [
  [-1.7,left,-8,8], [right,8,-8,8], [left,right,-8,landing], [left,right,end,8],
].map(([a,b,c,d]) => ({ ...block(a,b,c,d,DEPTH-0.18,DEPTH,0x354352), ceiling: true }));

export function stairPassage(room, p) {
  if (!p?.alive || p.x < left || p.x > right) return null;
  if (room === 'lobby' && p.z >= lip + 0.06 && p.z < end && p.y <= 2.15)
    return { room: BASEMENT, y: p.y + DEPTH };
  if (room === BASEMENT && p.z >= landing - 0.25 && p.z < lip - 0.06 && p.y >= DEPTH + 1.5)
    return { room: 'lobby', y: Math.max(1.7, p.y - DEPTH) };
  return null;
}

// The inner sea follows the same sphere as the terrain, eight metres below B1.
export const POOL = { water: true, level: -8, bottom: -11 };
export const SEA_HALF = Math.PI * PLANET;
export const deck = { ...block(-1.7,8,-8,8,-0.2,0,stone), stair: true };
export const seaStairs = Array.from({ length: 40 }, (_, i) => {
  const x = deck.xMin - i * 0.38, top = -(i + 1) * 0.24;
  return { ...block(x-0.38,x,3.4,5.4,top-0.14,top,i % 2 ? stone : edge), stair: true };
});
export const berths = [{ x: -14.4, z: 2.65 }, { x: -14.4, z: 6.15 }];
const plank = (x0, x1, z0, z1, top) => ({ ...block(x0,x1,z0,z1,0,top,0xba956b), stair: true });
export const scaffold = [
  plank(-1.7,-0.35,-3.4,0.8,0.88),
  ...Array.from({length: 4}, (_, i) => plank(-1.7,-0.35,0.8+i*0.4,1.2+i*0.4,0.88-i*0.22)),
];
// Open rail on the scaffold, with a sightline down to the sea.
export const windowFrame = [
  block(-1.78,-1.66,-3.25,-0.38,1.32,1.38,0x438a91),
  ...[-3.25,-0.5].map(z => block(-1.78,-1.66,z,z+0.12,0.88,3.72,0x789398)),
  { ...block(-1.78,-1.66,-3.25,-0.38,3.58,3.72,0x789398), ceiling: true },
];
export const cellarFixtures = [deck, ...seaStairs, ...treads, ...cellarWalls, ...ceiling, ...scaffold, ...windowFrame, POOL];

export function floatInPool(previous, next, cmd, dt) {
  const pressed = !!(cmd.buttons & 1);
  if (next.y > POOL.level + 1.75 || next.onGround && next.y > POOL.level + 0.5) {
    next.boat = false; next.swimming = false; next.poolJump = pressed;
    return;
  }
  const launch = berths.some(b => Math.hypot(next.x-b.x,next.z-b.z) < 0.86);
  let boat = !!previous.boat;
  if (pressed && !previous.poolJump && (boat || launch)) boat = !boat;
  next.boat = boat;
  next.swimming = !boat;
  next.poolJump = pressed;
  const diving = (cmd.buttons & 2) || (cmd.fwd > 0.1 && cmd.pitch < -25);
  const afloat = boat ? 1.42 : diving ? -0.42 : 0.48;
  const target = POOL.level + afloat;
  next.y = previous.y + (target - previous.y) * (1 - Math.exp(-dt * 10));
  next.vy = (next.y - previous.y) / dt;
  next.onGround = false;
}
