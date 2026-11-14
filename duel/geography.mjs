// 🗺️ dool's geography: one world for every handle, on one fixed lattice.
//
// A block is a 16 m square with one authority — the room the server already
// runs. Blocks sit on an integer lattice and open one at a time along a square
// spiral out of the lobby, so a block's place never changes and the world only
// grows. A pod keeps its identity while its placement can change.
// The method and its numbers are in GEOGRAPHY.md.
import { mkdir, readFile, writeFile, rename, copyFile } from 'node:fs/promises';
import { constants } from 'node:fs';
import { join } from 'node:path';
import { createHash, randomBytes } from 'node:crypto';

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
    this.pods = structuredClone(data.pods || {});
    this.placements = structuredClone(data.placements || {});
    this.rests = structuredClone(data.rests || {});
    this.opened = Math.max(1, data.opened || 1);
    this.revision = data.revision || 0;
    if (!data.pods) for (const [handle, h] of Object.entries(data.homes || {})) {
      // Deterministic until the first write: read-only restarts cannot change links.
      const id = 'pod-' + createHash('sha256').update(`${handle}:${h.at}`).digest('hex').slice(0, 20);
      this.pods[id] = { id, handle, at: h.at };
      this.placements[id] = { block: h.block, lot: h.lot };
    }
  }
  toJSON() {
    return { version: 2, pods: this.pods, placements: this.placements,
      rests: this.rests, opened: this.opened, revision: this.revision };
  }
  get homes() {
    return Object.fromEntries(Object.values(this.pods).map(p => [p.handle,
      { ...this.placements[p.id], at: p.at, pod: p.id }]));
  }
  pod(id) { return Object.hasOwn(this.pods, id || '') ? this.pods[id] : null; }
  forHandle(handle) { return Object.values(this.pods).find(p => p.handle === key(handle)) || null; }
  home(handle) {
    const p = this.forHandle(handle);
    return p ? { ...this.placements[p.id], at: p.at, pod: p.id } : null;
  }
  publicPod(id) {
    const p = this.pod(id), at = this.placements[id];
    return p && at ? { id, handle: p.handle, at: p.at, placement: { ...at },
      room: blockId(blockAt(at.block)), interior: id,
      profile: { handle: p.handle, url: `https://aesthetic.computer/@${encodeURIComponent(p.handle)}` } } : null;
  }
  taken(block, lotIndex) {
    return Object.values(this.placements).some(p => p.block === block && p.lot === lotIndex);
  }
  validSpot(p) {
    return p && Number.isInteger(p.block) && p.block >= 1 && p.block <= this.opened &&
      Number.isInteger(p.lot) && p.lot >= 0 && p.lot < LOTS;
  }
  settle(handle, want = null, now = Date.now()) {
    const who = key(handle);
    if (!/^[a-z0-9]+([._][a-z0-9]+)*$/.test(who) || who.length > 16) throw new Error('A home needs a handle');
    let pod = this.forHandle(who);
    if (pod) {
      const at = this.placements[pod.id];
      const moved = !!want && (at.block !== want.block || at.lot !== want.lot);
      if (moved) this.curate([{ pod: pod.id, ...want }]);
      return { ...this.home(who), moved, opened: this.opened };
    }
    if (want && !this.validSpot(want)) throw new Error('That lot is not open');
    if (want && this.taken(want.block, want.lot)) throw new Error('That lot is taken');
    let spot = want;
    if (!spot) {
      const used = new Set(Object.values(this.placements).map(p => p.block * LOTS + p.lot));
      for (let n = LOTS; n < (this.opened + 1) * LOTS; n++) {
        if (!used.has(n)) { spot = { block: Math.floor(n / LOTS), lot: n % LOTS }; break; }
      }
    }
    if (!spot) spot = { block: ++this.opened, lot: 0 };
    const id = 'pod-' + randomBytes(10).toString('hex');
    this.pods[id] = pod = { id, handle: who, at: now };
    this.placements[id] = { block: spot.block, lot: spot.lot };
    this.opened = Math.max(this.opened, blocksFor(Object.keys(this.pods).length));
    this.revision++;
    return { ...this.home(who), moved: false, opened: this.opened };
  }
  // Verified account IDs stay private; a renamed handle keeps its pod and contents.
  identify(handle, accountId) {
    const who = key(handle);
    if (typeof accountId !== 'string' || !accountId) return this.settle(who);
    const known = Object.values(this.pods).find(p => p.accountId === accountId);
    const named = this.forHandle(who);
    if (known && named && known.id !== named.id) throw new Error('Handle already has a pod');
    if (named?.accountId && named.accountId !== accountId) throw new Error('Pod belongs to another account');
    if (known) {
      if (!/^[a-z0-9]+([._][a-z0-9]+)*$/.test(who) || who.length > 16) throw new Error('A home needs a handle');
      if (known.handle !== who) {
        if (this.rests[known.handle]) { this.rests[who] = this.rests[known.handle]; delete this.rests[known.handle]; }
        known.handle = who; this.revision++;
      }
    } else {
      this.settle(who);
      this.forHandle(who).accountId = accountId;
    }
    return { ...this.home(who), moved: false, opened: this.opened };
  }
  // Validate the entire final arrangement first. Swaps and grouped moves are atomic.
  curate(moves, expectedRevision = this.revision) {
    if (expectedRevision !== this.revision) throw new Error('Park changed; refresh the map');
    if (!Array.isArray(moves) || !moves.length || moves.length > 256) throw new Error('Invalid pod moves');
    const next = structuredClone(this.placements), seen = new Set();
    for (const m of moves) {
      if (!this.pod(m?.pod) || seen.has(m.pod)) throw new Error('Unknown or repeated pod');
      if (!this.validSpot(m)) throw new Error('That lot is not open');
      seen.add(m.pod); next[m.pod] = { block: m.block, lot: m.lot };
    }
    const occupied = new Set();
    for (const p of Object.values(next)) {
      const k = p.block * LOTS + p.lot;
      if (occupied.has(k)) throw new Error('That lot is taken');
      occupied.add(k);
    }
    this.placements = next;
    this.revision++;
    return moves.map(m => this.publicPod(m.pod));
  }
  rest(handle, { room, x, z, yaw = 0 }, now = Date.now()) {
    const who = key(handle);
    if (!who || !(room === LOBBY || parseBlock(room) || this.pod(room))) return null;
    const half = BLOCK / 2;
    const clamp = n => Math.round(Math.min(half, Math.max(-half, Number(n) || 0)) * 100) / 100;
    this.rests[who] = { room, x: clamp(x), z: clamp(z), yaw: Math.round(Number(yaw) || 0), at: now };
    return this.rests[who];
  }
  whereabouts(handle) { return this.rests[key(handle)] || null; }
  sleepers(room) {
    return Object.entries(this.rests).filter(([, r]) => r.room === room).map(([handle, r]) => ({ handle, ...r }));
  }
  neighbours(room) {
    const b = parseBlock(room);
    if (!b || blockIndex(b) > this.opened) return null;
    const out = {};
    for (const [edge, di, dj] of [['east',1,0],['west',-1,0],['north',0,1],['south',0,-1]]) {
      const n = { i: b.i + di, j: b.j + dj };
      if (blockIndex(n) <= this.opened) out[edge] = blockId(n);
    }
    return out;
  }
  census() {
    const blocks = [{ n: 0, id: LOBBY, ...blockAt(0), homes: [] }];
    for (let n = 1; n <= this.opened; n++) blocks.push({ n, id: blockId(blockAt(n)), ...blockAt(n), homes: [] });
    for (const [handle, h] of Object.entries(this.homes)) blocks[h.block]?.homes.push({ handle, lot: h.lot, at: h.at, pod: h.pod });
    const settled = Object.keys(this.pods).length;
    return { revision: this.revision, settled, opened: this.opened, lots: this.opened * LOTS,
      span: span(this.opened), blocks, pods: Object.keys(this.pods).map(id => this.publicPod(id)) };
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
      const path = join(this.directory, 'geography.json');
      const data = JSON.parse(await readFile(path, 'utf8'));
      this.geography = new Geography(data);
      if (!data.pods) {
        // Keep the original ledger before the first v2 write. Never replace a backup.
        await copyFile(path, join(this.directory, 'geography.v1.json'), constants.COPYFILE_EXCL)
          .catch(e => { if (e.code !== 'EEXIST') throw e; });
        await this.change(() => true);
      }
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
