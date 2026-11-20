// Shared park geometry and server-authorized edge transitions.
export const PARK_EDGES = ['east', 'west', 'north', 'south'];
// Blocks meet along their whole edge, not only where the lanes cross: walk
// off any side with a neighbour and you are in the neighbour. A block is
// 16 m (8 either way) and EDGE sits a little past its line: an arrival is set
// down at the very same spot seen from the next block (0.4 m short of that
// block's own EDGE), so nothing jumps and it is not handed straight back.
// (A side with a neighbour has no wall: world.mjs lets a body walk past it.)
export const EDGE = 8.2, SPAN = 16;
// 🌍 The land goes round: fifty-five blocks one way bring you back where you
// began, east to west and north to south alike (the planet's girth, 880 m,
// in blocks: planet.mjs). So the lattice is cells -27 to 27 each way and the
// one past the last is the first. (A grid of squares cannot lie on a ball:
// it is a ring in each direction, drawn round one. Two walkers who set out
// north and east never meet at a pole.)
export const LAP = 55, HALF_LAP = 27;
export const wrap = (n) => ((((n + HALF_LAP) % LAP) + LAP) % LAP) - HALF_LAP;
export const onLap = (cell) => !!cell && Math.abs(cell.i) <= HALF_LAP && Math.abs(cell.j) <= HALF_LAP;

// 🌳 The tower's grounds: lattice cells [i, j] around the plaza that are open
// field and never housing, so the tower stands in a yard and can be seen
// whole from a distance. The two blocks east and north-east were settled
// before the grounds were laid out and stay as they are. The cells south of
// the door are the promenade: a reflecting pool on the tower's axis between
// two rose beds, two blocks long, ending in a terrace.
// (2026-11-18: the yard is REACH blocks deep on every side, where it was one
// or two, so there is room to walk well away from the tower and look back.)
export const REACH = 3;
const SETTLED = [[1, 0], [1, 1]]; // blocks 1 and 2, east and north-east
export const GROUNDS = [];
for (let j = REACH; j >= -REACH; j--)
  for (let i = -REACH; i <= REACH; i++)
    if ((i || j) && !SETTLED.some(([a, b]) => a === i && b === j)) GROUNDS.push([i, j]);
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
// The same spot seen from the next block: exactly one block over. The ground
// is drawn across the join, so the body does not move at all.
export function landing(p, edge) {
  const out = { x: p.x, z: p.z, yaw: p.yaw, pitch: p.pitch };
  if (edge === 'east') out.x = p.x - SPAN;
  if (edge === 'west') out.x = p.x + SPAN;
  if (edge === 'north') out.z = p.z - SPAN;
  if (edge === 'south') out.z = p.z + SPAN;
  return out;
}
