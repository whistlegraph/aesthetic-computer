// 🛗 The tower's elevator and the tower's own measurements, as the server and
// the page both know them (tower.mjs holds who lives on which floor).
//
// The tower stands over the plaza on four legs. Its first floor is ATRIUM
// metres up, so the plaza keeps its sky, and every floor after that is one
// STOREY higher. A floor is a real place at a real height: in the full tower
// floor 89 is 628 m above the plaza.
import { DEPTH } from './basement.mjs';
export const STOREY = 7, ATRIUM = 12;
// 🏬 A short tower, for now (2026-11-19). Every handle keeps its floor and its
// number, but only the listed floors (tower.mjs `listed`: @jeffrey's and the
// highest) are given a storey of their own. The numbers between two listed
// floors share one storey, so the building is a few storeys tall whatever the
// numbers are: with floors 1 and 4000 listed, 1 is the first storey, 2 to
// 3999 are all the second, 4000 is the third and the roof lies on that.
// SHORT = false is the full tower again: floor n on storey n.
export const SHORT = true;
let listed = []; // the floors with a storey of their own, lowest first
// The server lists them from its ledger (TowerStore) and tells every page
// (the `tower` message), so both count storeys the same way.
export const listFloors = (numbers = []) =>
  void (listed = [...new Set((numbers || []).filter((n) => Number.isInteger(n) && n > 0))].sort((a, b) => a - b));
export const listedFloors = () => listed;
// The storey a floor stands on (1 is the first; 0 the ground).
export function storeyOf(number) {
  if (!(number > 0)) return 0;
  if (!SHORT) return number;
  let s = 0, prev = 0;
  for (const n of listed) {
    if (number < n) return s + 1; // in the shared storey under n
    s += n - prev > 1 ? 2 : 1; // that shared storey, if there are numbers for it, and n's own
    if (number === n) return s;
    prev = n;
  }
  return s + 1; // over every listed floor
}
// Every height in the tower comes from these: a storey's, a floor's, the roof's.
export const heightOf = (storey) => (storey > 0 ? ATRIUM + (storey - 1) * STOREY : 0);
export const altitude = (number) => number === -1 ? -DEPTH : heightOf(storeyOf(number));
export const roofOf = (top) => (top > 0 ? heightOf(storeyOf(top) + 1) : 0);
// The storey a height falls in (0 below the first).
export const storeyAt = (y) => (y < ATRIUM ? 0 : 1 + Math.floor((y - ATRIUM) / STOREY));
// The floor a height falls in (0 below the first). On a shared storey that is
// one of `among` (the floors a ride runs between) if one stands there, or the
// lowest number of that storey.
export function floorAt(y, among = []) {
  if (y < -DEPTH / 2) return -1;
  const s = storeyAt(y);
  if (!SHORT || !s) return s;
  const known = [...among, ...listed].find((n) => storeyOf(n) === s);
  if (known) return known;
  let prev = 0;
  for (const n of listed) { if (storeyOf(n) > s) break; prev = n; }
  return prev + 1;
}
// The floors with a storey of their own within `reach` storeys of a height:
// what can be drawn from outside, each where it stands.
export function floorsNear(y, reach = 3) {
  const s = storeyAt(Math.max(0, y));
  if (SHORT) return listed.filter((n) => Math.abs(storeyOf(n) - s) <= reach);
  const out = [];
  for (let n = Math.max(1, s - reach); n <= s + reach; n++) out.push(n);
  return out;
}

// The tower's frame, around the 16 m room every floor is: SLAB is a floor
// slab's half-width, and LEG is where the four legs (and every storey's
// corner columns) stand from the middle and how thick a leg is.
export const SLAB = 8.35, LEG = { at: 8.15, wide: 0.7 };

// The car: a glass box against the north edge of the plaza and of every
// floor, its door to the south and the world behind its back wall. CAR is
// its half-width inside the walls; CAR_Z is where its middle stands.
export const CAR = 1.5, CAR_HEIGHT = 3, CAR_WALL = 0.2, CAR_Z = 6.1;

// The car as solids, for the view to draw and the tests to check: nothing
// here shares volume with anything else (see solids.mjs). The sign and the
// walls stop at the corner posts, the floor lies inside the walls, and the
// back pane's mullion stands between its sill and its rail. `ink` names the
// material; the view chooses its colours.
export function carSolids() {
  const o = CAR + CAR_WALL, z0 = CAR_Z, t = CAR_WALL, H = CAR_HEIGHT;
  const back = z0 + CAR + t / 2, door = z0 - CAR - t / 2;
  const solids = [];
  const add = (name, ink, x, y, z, w, h, d) => solids.push({ name, ink, color: ink, x, y, z, w, h, d });
  add('floor', 'pad', 0, 0.03, z0 - t / 2, 2 * CAR, 0.06, 2 * CAR + t);
  add('roof', 'roof', 0, H + 0.1, z0, 2 * o, 0.2, 2 * o);
  // Glass on three sides: a sill to the knee and a rail at the top, between the posts.
  for (const [side, x, z, w, d] of [['back', 0, back, 2 * CAR, t], ['west', -CAR - t / 2, z0, t, 2 * CAR], ['east', CAR + t / 2, z0, t, 2 * CAR]]) {
    add(`${side} sill`, 'sill', x, 0.3, z, w, 0.6, d);
    add(`${side} rail`, 'frame', x, H - 0.1, z, w, 0.2, d);
  }
  for (const x of [-o + t / 2, o - t / 2])
    for (const z of [door, back]) add('post', 'frame', x, H / 2, z, t, H, t);
  add('mullion', 'frame', 0, (0.6 + H - 0.2) / 2, back, 0.08, H - 0.8, 0.08);
  // Over the door a rail like the other sides': no sign, outside or in (the pad inside tells the floor).
  add('door rail', 'frame', 0, H - 0.1, door, 2 * CAR, 0.2, t);
  return solids;
}
// 📟 The screen inside the car, over the door: a bezel plate with the dark
// screen proud of it. Built facing -z; the view hangs it turned round at
// CAR_SCREEN_Z, clear of the sign and under the roof.
export const CAR_SCREEN_Z = CAR_Z - CAR + 0.12;
export function carScreenSolids() {
  return [
    { name: 'bezel', ink: 'bezel', color: 'bezel', x: 0, y: 2.59, z: 0.05, w: 2.78, h: 0.8, d: 0.06 },
    { name: 'screen', ink: 'screen', color: 'screen', x: 0, y: 2.59, z: 0.01, w: 2.7, h: 0.72, d: 0.02 },
  ];
}
// 🔢 The pad at the back of the car: a world panel (panel.mjs) the size of a
// tablet, on the back pane's mullion at the height of a hand, its face to the
// door. Its slab reaches back to the mullion and stops a millimetre short.
// (y is its middle, above the car's floor: it rides with the car.)
export const CAR_PAD = { x: 0, y: 1.6, // (its middle a hand under the eye, which is at 1.7)
  z: CAR_Z + CAR - 0.045, yaw: 0, w: 0.6, h: 0.9, cols: 64, rows: 96, deep: 0.1 };

// 🖥️ A standing screen keeps clear of the car, its shaft and the way to its
// door: a screen asked for there stands at the nearest edge of that ground
// instead. SCREEN_CLEAR is half a screen's width and a step.
const SCREEN_CLEAR = 2.3;
export function clearOfCar(x, z) {
  const side = CAR + CAR_WALL + SCREEN_CLEAR, front = CAR_Z - CAR - CAR_WALL - SCREEN_CLEAR;
  if (Math.abs(x) >= side || z <= front) return { x, z };
  // Out by the shorter way: to a side of the car, or back from its door.
  return side - Math.abs(x) < z - front ? { x: (x < 0 ? -1 : 1) * side, z } : { x, z: front };
}

// The car's three solid walls (the fourth side is the door).
export function carWalls() {
  const t = CAR_WALL, o = CAR + t, z = CAR_Z;
  const slab = (xMin, xMax, zMin, zMax) => ({ type: 'box', xMin, xMax, yMin: 0, yMax: CAR_HEIGHT, zMin, zMax });
  return [slab(-o, o, z + CAR, z + o), slab(-o, -CAR, z - CAR, z + CAR), slab(CAR, o, z - CAR, z + CAR)];
}
// 🚪 The door, shut while the car runs: a rider walks about inside and cannot step out (world.mjs `shut`).
export function carDoor() {
  const o = CAR + CAR_WALL, z = CAR_Z - CAR;
  return { type: 'box', xMin: -o, xMax: o, yMin: 0, yMax: CAR_HEIGHT, zMin: z - CAR_WALL, zMax: z };
}
// How many ticks the door stays shut for a ride of so many seconds: the ride and a moment after the bell.
export const shutTicks = (seconds, hz = 60) => Math.round(seconds * hz) + 12;
// Standing in the car, feet on its floor.
export const inCar = (p) => !!p && Math.abs(p.x) < CAR && Math.abs(p.z - CAR_Z) < CAR && p.y >= 0 && p.y < 2.3;
// Where a rider steps out: the car's middle, facing the door.
export const CAR_ARRIVAL = { x: 0, z: CAR_Z, yaw: 180, pitch: 0 };
// How long a ride between two heights takes: a floor or two is a few seconds,
// the whole tower well under ten. The car eases off its floor and into the next.
export const rideSeconds = (metres) => Math.min(9, 2.6 + 1.1 * Math.log2(1 + Math.abs(metres) / STOREY));
export const rideEase = (t) => (t <= 0 ? 0 : t >= 1 ? 1 : t < 0.5 ? 4 * t * t * t : 1 - (-2 * t + 2) ** 3 / 2);

// 🚪 The lobby: the tower's ground floor, a glass hall under the first floor
// across the north half of the plaza. Its front stands LOBBY_FRONT metres
// north of the plaza's middle with the entrance in its centre; the elevator
// is at the back. DOOR is the entrance's half-width.
export const LOBBY_FRONT = 1.2, DOOR = 1.6, DOOR_HEIGHT = 2.7;
// 🌳 The courtyard: the plaza's south half, before the entrance. A paved walk
// runs up the middle to the door between two planters, each with a tree.
export const PLANTERS = [{ x: -5, z: -3.4 }, { x: 5, z: -3.4 }], PLANTER = 2.2;
export const towerSides = ['west', 'east', 'south', 'north'];
function windowBay(alongX, at, from, to, sill, posts, height) {
  const box = (a, b, low, high, thick) => ({
    type: 'box', window: true, yMin: low, yMax: high,
    xMin: alongX ? a : at - thick, xMax: alongX ? b : at + thick,
    zMin: alongX ? at - thick : a, zMax: alongX ? at + thick : b,
  });
  return [box(from, to, 0, sill, 0.1), ...posts.map(p => box(p-0.06, p+0.06, sill, height, 0.06))];
}
export function storeyWindows() {
  const edge = LEG.at;
  const bay = (alongX, at, from, to) => {
    const posts = [from, to];
    for (let p = from + 4; p < to - 0.5; p += 4) posts.push(p);
    return windowBay(alongX, at, from, to, 1.08, posts, STOREY - 0.4);
  };
  return [...bay(true,-edge,-8,8), ...bay(false,-edge,-8,8), ...bay(false,edge,-8,8),
    ...bay(true,edge,-8,-CAR-0.3), ...bay(true,edge,CAR+0.3,8)];
}
// The ground floor's solid parts: the front either side of the entrance, the
// two ends, and the courtyard's planters.
export function lobbyWalls() {
  const f = LOBBY_FRONT;
  const slab = (xMin, xMax, zMin, zMax, yMax = ATRIUM) => ({ type: 'box', xMin, xMax, yMin: 0, yMax, zMin, zMax });
  const bay = (alongX, at, a, b) => {
    const posts = Array.from({length: 4}, (_, i) => Math.round((a + (b-a)*i/3) * 100) / 100);
    const frame = windowBay(alongX, at, a, b, 0.5, posts, ATRIUM - 0.4);
    for (const y of [3.2, 6.2, 9.2]) frame.push({ ...frame[0], yMin: y-0.06, yMax: y+0.06, ceiling: true });
    return frame;
  };
  return [
    ...bay(true, f, -8, -DOOR), ...bay(true, f, DOOR, 8),
    ...bay(false, -7.95, f, 8), ...bay(false, 7.95, f, 8),
    ...PLANTERS.map((p) => slab(p.x - PLANTER / 2, p.x + PLANTER / 2, p.z - PLANTER / 2, p.z + PLANTER / 2, 0.55)),
  ];
}

// 🛵 The underside of the first floor, over the plaza: a flying cabin bike
// (bike.mjs) is held under it, not let up through the tower. Walkers never
// reach it (it is ATRIUM metres up).
export const atriumRoof = () => ({ type: 'box', xMin: -SLAB, xMax: SLAB, zMin: -SLAB, zMax: SLAB, yMin: ATRIUM, yMax: ATRIUM + 0.3, ceiling: true });

// 🏙️ The roof: the open deck on top of the tower, a room of its own (ROOF)
// and nobody's floor. The car comes up through it as it does through every
// floor, and `top` in the car is here. It is a place only while the tower
// has a floor to lie on.
export const ROOF = 'roof';
// A room of the tower that stands over the plaza: a floor, or the roof.
export const aloft = (room) => room === ROOF || /^floor-/.test(room || '');
// In a ride's sums and in every `number` the roof is one over the highest
// floor: altitude() already puts that number on the roof's slab (the highest
// floor is always listed), so a ride to the roof is counted like any other.
export const roofNumber = (top) => (top > 0 ? top + 1 : 0);
// 🧱 What stands on the roof besides the car: a parapet to the hip round the
// slab's edge, where the storeys below have their glass, and a curb down each
// side of the car, over the gap the slab leaves for the shaft. As solids
// (see carSolids): none shares volume with another or with the car.
export const PARAPET = { at: LEG.at, wide: 0.2, height: 1.1 };
export function roofSolids() {
  const { at, wide: t, height: H } = PARAPET, o = CAR + CAR_WALL, out = at + t / 2, bay = o + 0.35;
  const solids = [];
  const run = (name, x0, x1, z0, z1) => {
    const x = (x0 + x1) / 2, z = (z0 + z1) / 2, w = x1 - x0, d = z1 - z0;
    solids.push({ name, ink: 'sill', color: 'sill', x, y: (H - 0.1) / 2, z, w, h: H - 0.1, d });
    solids.push({ name: `${name} cap`, ink: 'frame', color: 'frame', x, y: H - 0.05, z, w, h: 0.1, d });
  };
  run('south parapet', -out, out, -out, -out + t);
  run('west parapet', -out, -out + t, -out + t, out - t);
  run('east parapet', out - t, out, -out + t, out - t);
  // North, either side of the shaft: the car's glass back is the edge there.
  run('north-west parapet', -out, -bay, out - t, out);
  run('north-east parapet', bay, out, out - t, out);
  run('west curb', -bay, -o, CAR_Z - o, out);
  run('east curb', o, bay, CAR_Z - o, out);
  return solids;
}
// The same, as the boxes a body is walked against (a sill and its cap are one).
export function roofWalls() {
  return roofSolids().filter((s) => s.ink === 'sill').map((s) =>
    ({ type: 'box', xMin: s.x - s.w / 2, xMax: s.x + s.w / 2, yMin: 0, yMax: PARAPET.height, zMin: s.z - s.d / 2, zMax: s.z + s.d / 2 }));
}

// 🧍 Nobody stands on air. Whoever is drawn on the tower from outside (the
// people of its other rooms, what is built on its floors) is given as feet,
// { x, y, z } in the tower's own metres (the plaza's middle, the ground at
// 0), and this says what must be drawn under them: `slabs`, the storeys
// someone stands on, and `cars`, the storeys where someone stands in the
// shaft, where a slab is open and the car is the floor (0 is the car on the
// ground). A storey is a storey: one that many numbers share (SHORT) counts
// as one with a number of its own, and the roof is the storey over the last.
export function standsOn(feet = []) {
  const slabs = new Set(), cars = new Set(), o = CAR + CAR_WALL;
  for (const p of feet) {
    if (!p || !(Math.abs(p.x) <= SLAB && Math.abs(p.z) <= SLAB)) continue; // (off the tower: on the land, or in the air beside it)
    const s = storeyAt((p.y || 0) + 0.5); // (half a metre of grace: a height is told rounded)
    if (s > 0) slabs.add(s);
    if (Math.abs(p.x) < o && Math.abs(p.z - CAR_Z) < o) cars.add(s);
  }
  const up = (a, b) => a - b;
  return { slabs: [...slabs].sort(up), cars: [...cars].sort(up) };
}
