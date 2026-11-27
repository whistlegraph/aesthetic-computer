// 🗺️ rax as a drawing: everything that stands in the world, as a short list
// of plain shapes in world metres, for anything that shows rax from outside
// (the desktop wallpaper's model, a map, a film's establishing shot).
//
// Every shape here is worked out from the measurements the server walks
// people against and the page draws from (geography.mjs, park.mjs,
// elevator.mjs), so a reader of GET /rax/map never keeps a copy of rax's
// geometry and is never out of step with it. Something new that stands in
// the world gets its shapes here, under a role; a reader draws a role it
// does not know in its plain ink.
//
// World metres: x east, z north, y up, the plaza's middle at the origin.
//   flat   an outline on the ground (or at `y`):    x0 z0 x1 z1 [y]
//   box    a standing box:                          x0 z0 x1 z1 y0 y1
//   stack  the same flat every `step` metres up:    x0 z0 x1 z1 y step count
//   panel  an upright rectangle, turned by `yaw`:   x z yaw w h y
// A shape may carry a `color` (0xrrggbb) and a `label`.
import { BLOCK, LOBBY, blockAt, parseBlock, toWorld } from './geography.mjs';
import { STOREY, ATRIUM, SLAB, LEG, CAR, CAR_HEIGHT, CAR_WALL, CAR_Z, altitude, storeyOf, roofOf, carWalls, lobbyWalls, ROOF, roofNumber, roofWalls } from './elevator.mjs';
import { TILE_COLORS } from './world.mjs';
import { BASEMENT, DEPTH } from './basement.mjs';
import { PLANET } from './planet.mjs';
import { HUNG } from './orbits.mjs';
import { DISPENSER_WALL } from './ball.mjs';
import { REACH, LAP, GROUNDS } from './park.mjs';

export const MAP_VERSION = 1;
const round = (n) => Math.round(n * 100) / 100;
const shape = (k, role, at, more = {}) => {
  const out = { k, role };
  for (const [name, v] of Object.entries(at)) out[name] = round(v);
  return Object.assign(out, more);
};
// A solid the server walks people against, as a box on the map.
const solid = (role, w) =>
  shape('box', role, { x0: w.xMin, z0: w.zMin, x1: w.xMax, z1: w.zMax, y0: w.yMin, y1: w.yMax });

// The tower over the plaza, `top` floors tall: its legs, the lobby's glass
// and what stands in it, the courtyard's planters, the car and its shaft,
// and a slab for every storey plus the roof (a storey for every floor in the
// full tower; a few while it is short: elevator.mjs SHORT).
export function towerShapes(top) {
  if (!top) return [];
  const out = [], half = LEG.wide / 2, o = CAR + CAR_WALL;
  for (const x of [-LEG.at, LEG.at])
    for (const z of [-LEG.at, LEG.at])
      out.push(shape('box', 'leg', { x0: x - half, z0: z - half, x1: x + half, z1: z + half, y0: 0, y1: ATRIUM }));
  for (const w of lobbyWalls()) out.push(solid(w.yMax >= ATRIUM ? 'glass' : 'fixture', w));
  for (const w of carWalls()) out.push(solid('car', w));
  out.push(shape('box', 'shaft', { x0: -o, z0: CAR_Z - CAR, x1: o, z1: CAR_Z + o, y0: CAR_HEIGHT, y1: roofOf(top) }));
  out.push(shape('stack', 'floor', { x0: -SLAB, z0: -SLAB, x1: SLAB, z1: SLAB, y: ATRIUM, step: STOREY }, { count: storeyOf(top) + 1 }));
  // 🏙️ The roof's parapet, and the car's place up there, standing on the last slab.
  const roof = roofOf(top), lift = (w) => ({ ...w, yMin: w.yMin + roof, yMax: w.yMax + roof });
  for (const w of roofWalls()) out.push(solid('parapet', lift(w)));
  return out;
}

// The lattice: the plaza, every open block, and the blocks that open next.
export function landShapes(geography, ahead = 6) {
  const out = [], half = BLOCK / 2, census = geography.census();
  const square = (role, b) =>
    shape('flat', role, { x0: b.i * BLOCK - half, z0: b.j * BLOCK - half, x1: b.i * BLOCK + half, z1: b.j * BLOCK + half });
  // 🌳 The tower's yard (park.mjs): open field REACH blocks out on every side.
  const yard = (REACH + 0.5) * BLOCK;
  out.push(shape('flat', 'yard', { x0: -yard, z0: -yard, x1: yard, z1: yard }));
  const settled = new Set(census.blocks.map(b => `${b.i},${b.j}`));
  for (const [i, j] of GROUNDS) if (!settled.has(`${i},${j}`)) out.push(square('grounds', { i, j }));
  for (let k = 1; k <= ahead; k++) out.push(square('next', blockAt(census.opened + k)));
  for (const b of census.blocks) {
    out.push(Object.assign(square(b.n === 0 ? 'plaza' : 'block', b), { id: b.id }));
  }
  return out;
}

// What people have put in a block: painted squares, builds, $code screens.
// `room` is what the store holds for it: { tiles, objects, screens }.
export const BUILD = 1.6, SCREEN_WIDE = 4, SCREEN_TALL = 2.25, SCREEN_LIFT = 0.4;
export function contentShapes(block, room = {}) {
  const out = [], at = toWorld(block, { x: 0, z: 0 });
  for (const [key, c] of Object.entries(room.tiles || {})) {
    const [i, j] = key.split(',').map(Number);
    const x = at.x - 18 + i * 2, z = at.z - 18 + j * 2; // view.mjs: a 2 m square from this corner
    out.push(shape('flat', 'paint', { x0: x, z0: z, x1: x + 2, z1: z + 2 }, { color: TILE_COLORS[(c - 1) % TILE_COLORS.length] }));
  }
  for (const o of room.objects || []) {
    if (o.item) continue; // carried, not standing
    const s = BUILD / 2;
    out.push(shape('box', 'build', { x0: at.x + o.x - s, z0: at.z + o.z - s, x1: at.x + o.x + s, z1: at.z + o.z + s, y0: 0, y1: BUILD },
      o.code ? { label: `$${o.code}` } : {}));
  }
  for (const s of room.screens || [])
    out.push(shape('panel', 'screen', { x: at.x + s.x, z: at.z + s.z, yaw: s.yaw || 0, w: SCREEN_WIDE, h: SCREEN_TALL, y: SCREEN_LIFT },
      s.code ? { label: `$${s.code}` } : {}));
  return out;
}

// Where a body in `room` at its own (x, y, z) stands in the world, or null
// for a room that is not a place in it (a game, a studio). `floor` is the
// floor's number, 0 on the ground; on the roof it is one over the highest
// floor (roofNumber) and `roof` is true.
export function place(room, p, { geography, tower }) {
  if (room === BASEMENT) return { x: round(p.x), y: round((p.y || 0) - DEPTH), z: round(p.z), floor: -1 };
  if (room === ROOF) return tower?.top ? { x: round(p.x), y: round(roofOf(tower.top) + (p.y || 0)), z: round(p.z), floor: roofNumber(tower.top), roof: true } : null;
  const f = tower?.forRoom(room);
  if (f) return { x: round(p.x), y: round(altitude(f.number) + (p.y || 0)), z: round(p.z), floor: f.number };
  const block = room === LOBBY ? { i: 0, j: 0 } : parseBlock(room);
  if (!block) return null;
  const at = toWorld(block, { x: p.x, z: p.z });
  return { x: round(at.x), y: round(p.y || 0), z: round(at.z), floor: 0 };
}

// The whole map. `contents(id)` gives a block's { tiles, objects, screens };
// `seated` is everyone in rax now: [{ handle, room, x, y, z }]; `watching`
// is everyone looking on from the air: [{ name, room, x, y, z, yaw }], where
// x, y, z is their eye.
export function worldMap({ geography, tower = null, contents = () => ({}), seated = [], watching = [], ball = null }) {
  const census = geography.census(), half = BLOCK / 2, top = tower?.top || 0;
  // The yard is part of what is open: people walk it, so the map holds it.
  const is = [...census.blocks.map((b) => b.i), -REACH, REACH], js = [...census.blocks.map((b) => b.j), -REACH, REACH];
  return {
    version: MAP_VERSION,
    revision: `${census.revision}.${tower?.revision || 0}`,
    block: BLOCK,
    planet: { radius: PLANET, lap: LAP * BLOCK },
    // 🪐 The planets hung over the land (orbits.mjs): balls, each its middle and radius.
    hung: HUNG.map((b) => ({ id: b.id, x: b.x, y: b.y, z: b.z, r: b.r, color: b.ink })),
    extent: {
      x0: Math.min(...is) * BLOCK - half, z0: Math.min(...js) * BLOCK - half,
      x1: Math.max(...is) * BLOCK + half, z1: Math.max(...js) * BLOCK + half,
    },
    census: { settled: census.settled, opened: census.opened, across: census.span.metres },
    tower: { floors: tower?.count || 0, top, atrium: ATRIUM, storey: STOREY, height: roofOf(top) },
    structure: [...landShapes(geography), solid('dispenser', DISPENSER_WALL), ...towerShapes(top)],
    content: census.blocks.flatMap((b) => contentShapes(b, contents(b.id))),
    people: seated.flatMap((s) => {
      const at = s.handle ? place(s.room, s, { geography, tower }) : null;
      return at ? [{ handle: String(s.handle).toLowerCase(), ...at, ...(Number.isFinite(s.yaw) ? { yaw: Math.round(s.yaw) } : {}) }] : [];
    }),
    // ⚽ The park's ball, where it is in the world.
    ball: ball ? place(ball.room, ball, { geography, tower }) : null,
    // 👀 Eyes in the air: no body, so a reader draws them apart from people.
    watchers: watching.flatMap((s) => {
      const at = s.name ? place(s.room, s, { geography, tower }) : null;
      return at ? [{ name: String(s.name).toLowerCase(), ...at, ...(Number.isFinite(s.yaw) ? { yaw: Math.round(s.yaw) } : {}) }] : [];
    }),
  };
}

// ⚡ Only who is where, for a reader that already holds the map and wants
// them moving (GET /rax/people): small enough to ask for several times a
// second. `at` is the server's clock.
export function peopleNow({ geography, tower = null, seated = [], watching = [], ball = null, now = Date.now() }) {
  const map = worldMap({ geography, tower, seated, watching, ball });
  return { at: now, people: map.people, watchers: map.watchers, ball: map.ball };
}
