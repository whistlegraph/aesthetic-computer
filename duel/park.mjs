// Shared park geometry and server-authorized edge transitions.
export const PARK_EDGES = ['east', 'west', 'north', 'south'];
// Blocks meet along their whole edge, not only where the lanes cross: walk
// off any side with a neighbour and you are in the neighbour. EDGE sits just
// inside the walking bound (world.mjs keeps bodies within 7.6); an arrival is
// set down at REACH, clear of the edge it came through, or it would be handed
// straight back.
export const EDGE = 7.5, REACH = 7.3, SPAN = 16;

// 🌳 The tower's grounds: lattice cells [i, j] around the plaza that are open
// field and never housing, so the tower stands in a yard and can be seen
// whole from a distance. The two blocks east and north-east were settled
// before the grounds were laid out and stay as they are. The cells south of
// the door are the promenade: a reflecting pool on the tower's axis between
// two rose beds, two blocks long, ending in a terrace.
export const GROUNDS = [[0, 1], [-1, 1], [-1, 0], [-1, -1], [0, -1], [1, -1], [-1, -2], [0, -2], [1, -2]];
// A block room's cell on the lattice ("e0s1" is one south of the plaza).
export function cellOf(id) {
  if (id === 'lobby') return { i: 0, j: 0 };
  const m = /^([ew])(\d{1,4})([ns])(\d{1,4})$/.exec(id || '');
  return m ? { i: (m[1] === 'w' ? -1 : 1) * Number(m[2]), j: (m[3] === 's' ? -1 : 1) * Number(m[4]) } : null;
}
export const isGrounds = (cell) => !!cell && GROUNDS.some(([i, j]) => i === cell.i && j === cell.j);
// The grounds cell a room is, or null for the plaza, housing and anything else.
export const groundsOf = (id) => { const cell = cellOf(id); return isGrounds(cell) ? cell : null; };

// The promenade, measured from the plaza's middle (south is -z): where the
// walk leaves the plaza, the pool, the walks and beds beside it, the terrace.
export const PROMENADE = {
  walk: 1.6, // half-width of the walk from the tower's door
  pool: { half: 2.4, north: -13.5, south: -35, rim: 0.3 },
  path: { inner: 2.9, outer: 4.3 }, // the walks along the pool, each side
  bed: { inner: 4.7, outer: 6.1, height: 0.2, every: 1.6 },
  terrace: { half: 6.1, north: -35.3, south: -39.4 },
};
// One promenade cell as flat pavings and solids, in the cell's own
// coordinates (-8 … 8). Flats are { x0, x1, z0, z1, y, ink }; solids are
// boxes about their middles with an `ink`, none sharing volume (solids.mjs).
export function promenade(cell) {
  if (!cell || cell.i !== 0 || (cell.j !== -1 && cell.j !== -2)) return null;
  const P = PROMENADE, off = cell.j * SPAN, lo = off - SPAN / 2, hi = off + SPAN / 2;
  const flats = [], solids = [];
  // A strip between two world z's, kept to this cell and set in its coordinates.
  const flat = (x0, x1, zSouth, zNorth, y, ink) => {
    const a = Math.max(lo, zSouth), b = Math.min(hi, zNorth);
    if (b > a) flats.push({ x0, x1, z0: a - off, z1: b - off, y, ink });
  };
  const solid = (name, ink, x, y, z, w, h, d) => {
    if (z - d / 2 >= lo && z + d / 2 <= hi) solids.push({ name, ink, color: ink, x, y, z: z - off, w, h, d });
  };
  // No two pavings overlap: each strip ends where the next begins.
  const head = P.pool.north + P.pool.rim, foot = P.pool.south - P.pool.rim;
  flat(-P.walk, P.walk, head + 1, -8, 0.008, 'paving'); // from the door
  flat(-P.bed.outer, P.bed.outer, head, head + 1, 0.008, 'paving'); // the cross walk at the pool's head
  flat(-P.pool.half - P.pool.rim, P.pool.half + P.pool.rim, foot, head, 0.008, 'stone');
  flat(-P.pool.half, P.pool.half, P.pool.south, P.pool.north, 0.014, 'water');
  for (const side of [-1, 1]) {
    const [a, b] = side < 0 ? [-P.path.outer, -P.path.inner] : [P.path.inner, P.path.outer];
    flat(a, b, foot, head, 0.008, 'paving');
  }
  flat(-P.terrace.half, P.terrace.half, P.terrace.south, P.terrace.north, 0.008, 'paving');
  // Rose beds: soil the length of the pool, a bush every so often, blooms on each.
  const bedX = (P.bed.inner + P.bed.outer) / 2, bedW = P.bed.outer - P.bed.inner;
  for (const side of [-1, 1]) {
    const a = Math.max(lo, P.pool.south), b = Math.min(hi, P.pool.north);
    if (b > a) solid('bed', 'soil', side * bedX, P.bed.height / 2, (a + b) / 2, bedW, P.bed.height, b - a);
    for (let z = P.pool.north - 0.8, n = 0; z > P.pool.south + 0.5; z -= P.bed.every, n++) {
      const top = P.bed.height + 0.6;
      solid('bush', 'leaf', side * bedX, P.bed.height + 0.3, z, 0.8, 0.6, 0.8);
      solid('rose', `rose${n % 4}`, side * bedX - 0.2, top + 0.09, z + 0.18, 0.2, 0.18, 0.2);
      solid('rose', `rose${(n + 2) % 4}`, side * bedX + 0.18, top + 0.09, z - 0.2, 0.2, 0.18, 0.2);
    }
    // A lamp at each end of the pool and one midway, beside the walk.
    for (const z of [P.pool.north, (P.pool.north + P.pool.south) / 2, P.pool.south]) {
      const x = side * (P.path.outer + 0.2);
      solid('lamp post', 'post', x, 1.4, z, 0.1, 2.8, 0.1);
      solid('lamp', 'lamp', x, 2.97, z, 0.34, 0.34, 0.34);
    }
    // Two benches on the terrace, facing the tower.
    const z = P.terrace.south + 0.7;
    solid('bench', 'post', side * 3, 0.22, z, 1.9, 0.44, 0.7);
    solid('cushion', 'cushion', side * 3, 0.49, z, 1.94, 0.1, 0.74);
  }
  return { flats, solids };
}
export function edgeAt(p) {
  if (!p || p.y > 2.3) return null;
  if (p.x > EDGE) return 'east';
  if (p.x < -EDGE) return 'west';
  if (p.z > EDGE) return 'north';
  if (p.z < -EDGE) return 'south';
  return null;
}
// The same spot seen from the next block: one block over, held inside the
// arrival line. The ground is drawn across the join, so this reads as a
// short step forward.
export function landing(p, edge) {
  const out = { x: p.x, z: p.z, yaw: p.yaw, pitch: p.pitch };
  const hold = (n) => Math.max(-REACH, Math.min(REACH, n));
  if (edge === 'east') out.x = hold(p.x - SPAN);
  if (edge === 'west') out.x = hold(p.x + SPAN);
  if (edge === 'north') out.z = hold(p.z - SPAN);
  if (edge === 'south') out.z = hold(p.z + SPAN);
  return out;
}
// 🏠 The house on a lot. It stands back from the lanes by a front yard and
// from the block's edge by a deeper one, so a body arriving from the next
// block (REACH, with its 0.4 m radius) lands in the yard, never in a wall.
// The door faces the east-west lane.
export const HOUSE = { front: 0.6, back: 1.2, height: 3, wall: 0.2, door: 1.6, doorHeight: 2.2 };
export function house(rect) {
  const east = rect.xMin > 0, north = rect.zMin > 0;
  const xMin = east ? rect.xMin + HOUSE.front : rect.xMin + HOUSE.back;
  const xMax = east ? rect.xMax - HOUSE.back : rect.xMax - HOUSE.front;
  const zMin = north ? rect.zMin + HOUSE.front : rect.zMin + HOUSE.back;
  const zMax = north ? rect.zMax - HOUSE.back : rect.zMax - HOUSE.front;
  return { xMin, xMax, zMin, zMax, north, frontZ: north ? zMin : zMax, doorX: (xMin + xMax) / 2 };
}
// Its walls as solid boxes: the back, the two sides, and the front either
// side of the doorway. (The lintel over the door is drawn, not solid.)
export function houseWalls(rect) {
  const h = house(rect), t = HOUSE.wall / 2, d = HOUSE.door / 2;
  const slab = (xMin, xMax, zMin, zMax) => ({ type: 'box', xMin, xMax, yMin: 0, yMax: HOUSE.height, zMin, zMax });
  const backZ = h.north ? h.zMax : h.zMin;
  return [
    slab(h.xMin - t, h.xMax + t, backZ - t, backZ + t),
    slab(h.xMin - t, h.xMin + t, h.zMin, h.zMax),
    slab(h.xMax - t, h.xMax + t, h.zMin, h.zMax),
    slab(h.xMin - t, h.doorX - d, h.frontZ - t, h.frontZ + t),
    slab(h.doorX + d, h.xMax + t, h.frontZ - t, h.frontZ + t),
  ];
}
// Every settled home's walls in a block ({ rects, homes } as the server sends).
export const blockWalls = (lots) =>
  (lots?.homes || []).flatMap((h) => (lots.rects[h.lot] ? houseWalls(lots.rects[h.lot]) : []));

// The front door, facing the shared east-west path, with the handle over it.
export function podFront(rect) {
  const h = house(rect);
  return { x: h.doorX, z: h.frontZ, yaw: h.north ? 0 : Math.PI, north: h.north };
}

// Enter only by walking across the front from the public path. A layout
// refresh under a stationary visitor must not teleport them into a pod.
export function crossedPod(from, to, lots) {
  if (!from || !to || to.y > 2.3) return null;
  for (const h of lots?.homes || []) {
    if (!h.pod || !lots.rects[h.lot]) continue;
    const f = podFront(lots.rects[h.lot]);
    if (Math.abs(to.x - f.x) > 0.65) continue;
    if (f.north ? from.z < f.z && to.z >= f.z : from.z > f.z && to.z <= f.z) return h.pod;
  }
  return null;
}
