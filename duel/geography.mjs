// 🗺️ dool's geography: one world for every handle, on one fixed lattice.
//
// A block is a 16 m square with one authority — the room the server already
// runs. Blocks sit on an integer lattice and open one at a time along a square
// spiral out of the lobby, so a block's place never changes and the world only
// grows. A handle settles one lot and its avatar rests wherever it last stood.
// The method and its numbers are in GEOGRAPHY.md.
import { mkdir, readFile, writeFile, rename } from 'node:fs/promises';
import { join } from 'node:path';

export const BLOCK = 16; // metres along a block's edge (2 × world.mjs WORLD)
export const LANE = 2; // width of the two lanes that cross every block
export const LOTS = 8; // homes in a residential block
export const FILL = 0.75; // settled share of open lots that opens the next block
export const RUN = 10; // metres a second (world.mjs CFG.runSpeed), for crossing times

// The lobby is block 0 and has no lots: it is the plaza.
export const LOBBY = 'lobby';

// Square spiral from the middle: 0 is (0,0), ring r holds 8r blocks, each ring
// starting just north of its east corner and running counter-clockwise.
// East is +x, north is +z.
export function blockAt(n) {
  if (!Number.isInteger(n) || n < 0) throw new RangeError('block index');
  if (n === 0) return { i: 0, j: 0 };
  const r = Math.ceil((Math.sqrt(n + 1) - 1) / 2); // ring
  const side = 2 * r;
  let k = n - (2 * r - 1) ** 2; // steps into the ring, 0 … 8r-1
  if (k < side) return { i: r, j: -r + 1 + k };
  k -= side;
  if (k < side) return { i: r - 1 - k, j: r };
  k -= side;
  if (k < side) return { i: -r, j: r - 1 - k };
  k -= side;
  return { i: -r + 1 + k, j: -r };
}

export function blockIndex({ i, j }) {
  const r = Math.max(Math.abs(i), Math.abs(j));
  if (r === 0) return 0;
  const base = (2 * r - 1) ** 2,
    side = 2 * r;
  if (i === r && j > -r) return base + (j + r - 1);
  if (j === r) return base + side + (r - 1 - i);
  if (i === -r) return base + 2 * side + (r - 1 - j);
  return base + 3 * side + (i + r - 1);
}

// A block's room id, in the room grammar the server accepts ([a-z0-9-]):
// "e3n2" is three blocks east and two north of the lobby.
export function blockId({ i, j }) {
  if (i === 0 && j === 0) return LOBBY;
  return `${i < 0 ? 'w' : 'e'}${Math.abs(i)}${j < 0 ? 's' : 'n'}${Math.abs(j)}`;
}

export function parseBlock(id) {
  if (id === LOBBY) return { i: 0, j: 0 };
  const m = /^([ew])(\d{1,4})([ns])(\d{1,4})$/.exec(id || '');
  if (!m) return null;
  const block = { i: (m[1] === 'w' ? -1 : 1) * Number(m[2]), j: (m[3] === 's' ? -1 : 1) * Number(m[4]) };
  return blockId(block) === id ? block : null; // one spelling per block
}

// A point in the world in metres from the lobby's middle, and back.
export const toWorld = (block, local) => ({ x: block.i * BLOCK + local.x, z: block.j * BLOCK + local.z });
export function fromWorld({ x, z }) {
  const block = { i: Math.floor(x / BLOCK + 0.5), j: Math.floor(z / BLOCK + 0.5) };
  return { block, local: { x: x - block.i * BLOCK, z: z - block.j * BLOCK } };
}

// Two lanes cross each block on its axes and meet the neighbours' lanes at the
// edges, so the lattice is also a street grid. Each quadrant is split into two
// lots, long side to the north-south lane. Lots count clockwise from the
// north-east quadrant; local coordinates run -8 … 8.
export function lot(k) {
  if (!Number.isInteger(k) || k < 0 || k >= LOTS) throw new RangeError('lot');
  const half = BLOCK / 2,
    inner = LANE / 2,
    depth = (half - inner) / 2;
  const east = k < 4,
    north = k < 2 || k > 5;
  const outer = k % 2 === (north === east ? 1 : 0); // the lot farther from the lane
  const x0 = inner + (outer ? depth : 0);
  const [xMin, xMax] = east ? [x0, x0 + depth] : [-x0 - depth, -x0];
  const [zMin, zMax] = north ? [inner, half] : [-half, -inner];
  return { xMin, xMax, zMin, zMax };
}

export function lotAt(local) {
  for (let k = 0; k < LOTS; k++) {
    const r = lot(k);
    if (local.x >= r.xMin && local.x <= r.xMax && local.z >= r.zMin && local.z <= r.zMax) return k;
  }
  return -1; // a lane
}

// Residential blocks open for `settled` homes: the next block opens once the
// open lots are FILL full, so the settled world is never mostly empty.
export const blocksFor = (settled) => Math.floor(settled / (LOTS * FILL)) + 1;

// How far across the settled world is, and how long that takes at a run.
export function span(blocks) {
  let reach = 0;
  for (let n = 0; n <= blocks; n++) {
    const b = blockAt(n);
    reach = Math.max(reach, Math.abs(b.i), Math.abs(b.j));
  }
  const metres = (2 * reach + 1) * BLOCK;
  return { metres, seconds: Math.round(metres / RUN) };
}

const key = (handle) => String(handle || '').replace(/^@/, '').toLowerCase();

// The ledger: who settled where, how many blocks are open, and where each
// avatar rests. Blocks never close and a home is never taken back.
export class Geography {
  constructor(data = {}) {
    this.homes = data.homes || {}; // handle → { block: n, lot: k, at }
    this.rests = data.rests || {}; // handle → { room, x, z, yaw, at }
    this.opened = Math.max(1, data.opened || 1); // residential blocks open (1 … opened)
    this.used = new Set(Object.values(this.homes).map((h) => h.block * LOTS + h.lot));
    this.free = LOTS; // the lowest lot number that may be free (block 1's first)
  }
  toJSON() {
    return { homes: this.homes, rests: this.rests, opened: this.opened };
  }
  home(handle) {
    return this.homes[key(handle)] || null;
  }
  taken(block, lotIndex) {
    return this.used.has(block * LOTS + lotIndex);
  }
  // One home per handle. `want` names a free lot in an open block; without it
  // the first free lot in spiral order is given. Settling again with `want`
  // moves the home and frees the old lot.
  settle(handle, want = null, now = Date.now()) {
    const who = key(handle);
    if (!who) throw new Error('A home needs a handle');
    const had = this.homes[who];
    let spot = null;
    if (want) {
      const ok = Number.isInteger(want.block) && want.block >= 1 && want.block <= this.opened &&
        Number.isInteger(want.lot) && want.lot >= 0 && want.lot < LOTS;
      if (!ok) throw new Error('That lot is not open');
      if (had && had.block === want.block && had.lot === want.lot) return { ...had, moved: false, opened: this.opened };
      if (this.taken(want.block, want.lot)) throw new Error('That lot is taken');
      spot = { block: want.block, lot: want.lot };
    } else if (had) {
      return { ...had, moved: false, opened: this.opened };
    } else {
      // First free lot in spiral order; `free` skips the run already settled.
      const end = (this.opened + 1) * LOTS;
      while (this.free < end && this.used.has(this.free)) this.free++;
      if (this.free < end) spot = { block: Math.floor(this.free / LOTS), lot: this.free % LOTS };
    }
    if (!spot) {
      // Every open lot is settled (a run of moves can do this): open the next block.
      this.opened += 1;
      spot = { block: this.opened, lot: 0 };
    }
    if (had) {
      this.used.delete(had.block * LOTS + had.lot);
      this.free = Math.min(this.free, had.block * LOTS + had.lot);
    }
    this.used.add(spot.block * LOTS + spot.lot);
    this.homes[who] = { ...spot, at: had?.at || now };
    this.opened = Math.max(this.opened, blocksFor(Object.keys(this.homes).length));
    return { ...this.homes[who], moved: !!had, opened: this.opened };
  }
  // Where an avatar last stood. It wakes there next time.
  rest(handle, { room, x, z, yaw = 0 }, now = Date.now()) {
    const who = key(handle);
    if (!who || !(room === LOBBY || parseBlock(room))) return null;
    const half = BLOCK / 2;
    const clamp = (n) => Math.round(Math.min(half, Math.max(-half, Number(n) || 0)) * 100) / 100;
    this.rests[who] = { room, x: clamp(x), z: clamp(z), yaw: Math.round(Number(yaw) || 0), at: now };
    return this.rests[who];
  }
  whereabouts(handle) {
    return this.rests[key(handle)] || null;
  }
  // Everyone resting in one block: the bodies a visitor sees there.
  sleepers(room) {
    return Object.entries(this.rests)
      .filter(([, r]) => r.room === room)
      .map(([handle, r]) => ({ handle, ...r }));
  }
  // The map: every open block with its settlers, and the world's size.
  census() {
    const blocks = [{ n: 0, id: LOBBY, ...blockAt(0), homes: [] }];
    for (let n = 1; n <= this.opened; n++) blocks.push({ n, id: blockId(blockAt(n)), ...blockAt(n), homes: [] });
    for (const [handle, h] of Object.entries(this.homes)) blocks[h.block]?.homes.push({ handle, lot: h.lot, at: h.at });
    const settled = Object.keys(this.homes).length;
    return { settled, opened: this.opened, lots: this.opened * LOTS, span: span(this.opened), blocks };
  }
}

// The ledger on disk, written whole and swapped in, one change at a time.
export class GeographyStore {
  constructor(directory) {
    this.directory = directory;
    this.geography = new Geography();
    this.queue = Promise.resolve();
  }
  async load() {
    if (!this.directory) return;
    await mkdir(this.directory, { recursive: true, mode: 0o700 });
    try {
      this.geography = new Geography(JSON.parse(await readFile(join(this.directory, 'geography.json'), 'utf8')));
    } catch (e) {
      if (e.code !== 'ENOENT') throw e;
    }
  }
  change(fn) {
    const task = this.queue.then(async () => {
      const next = new Geography(structuredClone(this.geography.toJSON())),
        result = fn(next);
      if (this.directory) {
        const path = join(this.directory, 'geography.json');
        await writeFile(path + '.tmp', JSON.stringify(next), { mode: 0o600 });
        await rename(path + '.tmp', path);
      }
      this.geography = next;
      return result;
    });
    this.queue = task.catch(() => {});
    return task;
  }
}
