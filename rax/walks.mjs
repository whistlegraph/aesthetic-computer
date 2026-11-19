// 👣 Walks
// Who came into rax, and how they moved. Nothing else in rax keeps a visit:
// the match holds a pose for one tick and the stores only know what was built.
// Two files a UTC day under <state>/walks/, both append-only:
//
// <day>.jsonl — the index, one JSON line per event:
//   {t, e:"enter", v, room}                    a connection arrived
//   {t, e:"who",   v, handle|name, g, seat}    it became someone, or changed seat
//   {t, e:"leave", v}                          it closed
//   {t, e:"seam",  v, from, ms}                it walked in over a seam: the room it left, and how long
//                                             the page had no place in either (its own count)
//
// <day>.motion — the gesture, every simulated tick of every seated body (HZ a
// second, the rate input is applied at). Frames, each:
//   u32 length · 8 bytes visit · f64 start ms · u16 ticks · deflate(samples)
// A sample is x y z (1/1000 unit) yaw pitch (1/100 degree) as zigzag varint
// deltas from the tick before; a frame's first is from zero, so any frame
// reads alone. A frame in which nothing moved is not written: a gap means the
// body held its last pose.
//
// `v` names one visit (a connection). `g` is the start of a guest's owner id:
// two guests can share one of the 256 two-word names. Days older than
// `keepDays` are deleted. World state is not kept here (see
// plans/rax-world-in-datomic.md): this is motion, that is fact.
import { randomUUID } from 'node:crypto';
import { appendFile, mkdir, readdir, unlink } from 'node:fs/promises';
import { join } from 'node:path';
import { deflateRawSync, inflateRawSync } from 'node:zlib';
import { guestName } from './guest-name.mjs';
import { HZ } from './world.mjs';

const FRAME = 2 * HZ; // ticks in a frame
const SCALE = [1000, 1000, 1000, 100, 100]; // x y z yaw pitch
const day = (t) => new Date(t).toISOString().slice(0, 10);

function varint(out, n) {
  let u = ((n << 1) ^ (n >> 31)) >>> 0; // zigzag: small either way stays small
  while (u > 127) { out.push((u & 127) | 128); u >>>= 7; }
  out.push(u);
}

export function createWalks({ directory, now = Date.now, keepDays = 35, flushMs = 2000 } = {}) {
  const visits = new Map(); // room member record → its visit
  let pending = []; // [file name, string or Buffer]
  let queue = Promise.resolve();
  let pruned = '';
  const write = (row) => {
    const t = now();
    pending.push([day(t) + '.jsonl', JSON.stringify({ t, ...row }) + '\n']);
  };
  // Close a visit's open frame and queue it, unless the body stood still.
  const seal = (v) => {
    if (!v.count) return;
    if (v.moved) {
      const body = deflateRawSync(Buffer.from(v.bytes));
      const head = Buffer.alloc(22);
      head.writeUInt32LE(18 + body.length, 0);
      head.write(v.id, 4, 8, 'ascii');
      head.writeDoubleLE(v.start, 12);
      head.writeUInt16LE(v.count, 20);
      pending.push([day(v.start) + '.motion', Buffer.concat([head, body])]);
    }
    v.count = 0;
    v.moved = false;
    v.bytes = [];
  };
  const prune = async (today) => {
    if (pruned === today) return;
    pruned = today;
    const oldest = day(now() - keepDays * 86400000);
    for (const name of await readdir(directory))
      if (/^\d{4}-\d\d-\d\d\.(jsonl|motion)$/.test(name) && name.slice(0, 10) < oldest)
        await unlink(join(directory, name)).catch(() => {});
  };
  const flush = () => {
    if (!directory || !pending.length) return queue;
    const rows = pending;
    pending = [];
    queue = queue.then(async () => {
      await mkdir(directory, { recursive: true, mode: 0o700 });
      const files = new Map();
      for (const [name, data] of rows) (files.get(name) || files.set(name, []).get(name)).push(Buffer.from(data));
      for (const [name, parts] of files) await appendFile(join(directory, name), Buffer.concat(parts), { mode: 0o600 });
      await prune(day(now()));
    }).catch(() => {}); // A full disk loses walks, never the room.
    return queue;
  };
  const timer = directory ? setInterval(flush, flushMs) : null;
  timer?.unref();
  return {
    enabled: !!directory,
    // Once per simulated tick, for each room with anyone in it.
    tick(key, room) {
      if (!directory) return;
      for (const r of room.sessions.values()) {
        if (!r.ready) continue;
        let v = visits.get(r);
        if (!v) {
          visits.set(r, (v = { id: randomUUID().slice(0, 8), seat: null, count: 0, moved: false, bytes: [], last: null, held: null }));
          write({ e: 'enter', v: v.id, room: key });
        }
        if ((r.handle || r.owner) && (v.handle !== r.handle || v.owner !== r.owner || v.seat !== r.seat)) {
          Object.assign(v, { handle: r.handle, owner: r.owner, seat: r.seat });
          write(r.handle ? { e: 'who', v: v.id, handle: r.handle, seat: r.seat }
            : { e: 'who', v: v.id, name: guestName(r.owner), g: r.owner.slice(0, 8), seat: r.seat });
        }
        const p = r.seat >= 0 && room.match.seats[r.seat]?.player;
        if (!p) { seal(v); continue; }
        const s = [p.x, p.y, p.z, p.yaw, p.pitch || 0].map((n, i) => Math.round(n * SCALE[i]));
        if (!v.count) {
          v.start = now();
          v.last = [0, 0, 0, 0, 0];
          v.moved = !v.held || s.some((n, i) => n !== v.held[i]); // against where it last stood
        }
        for (let i = 0; i < 5; i++) {
          if (v.count && s[i] !== v.last[i]) v.moved = true;
          varint(v.bytes, s[i] - v.last[i]);
        }
        v.last = v.held = s;
        if (++v.count >= FRAME) seal(v);
      }
    },
    // Once a visit: the first count is the walk in.
    seam(r, { from, ms }) {
      const v = visits.get(r);
      if (!directory || !v || v.seam) return;
      v.seam = true;
      write({ e: 'seam', v: v.id, from, ms });
    },
    leave(r) {
      const v = visits.get(r);
      if (!v) return;
      visits.delete(r);
      seal(v);
      write({ e: 'leave', v: v.id });
    },
    flush,
    async close() {
      clearInterval(timer);
      for (const r of [...visits.keys()]) this.leave(r);
      await flush();
    },
  };
}

// A day's .motion file → Map(visit → [{t, x, y, z, yaw, pitch}]), every tick.
export function readMotion(buffer, into = new Map()) {
  for (let at = 0; at + 22 <= buffer.length;) {
    const length = buffer.readUInt32LE(at);
    if (length < 18 || at + 4 + length > buffer.length) break; // a torn tail
    const id = buffer.toString('ascii', at + 4, at + 12);
    const start = buffer.readDoubleLE(at + 12);
    const count = buffer.readUInt16LE(at + 20);
    const raw = inflateRawSync(buffer.subarray(at + 22, at + 4 + length));
    at += 4 + length;
    const samples = into.get(id) || into.set(id, []).get(id);
    const last = [0, 0, 0, 0, 0];
    for (let i = 0, k = 0; i < count; i++) {
      for (let f = 0; f < 5; f++) {
        let u = 0, shift = 0, b;
        do { b = raw[k++]; u += (b & 127) * 2 ** shift; shift += 7; } while (b & 128);
        last[f] += (u >>> 1) ^ -(u & 1);
      }
      const [x, y, z, yaw, pitch] = last.map((n, f) => n / SCALE[f]);
      samples.push({ t: Math.round(start + (i * 1000) / HZ), x, y, z, yaw, pitch });
    }
  }
  return into;
}

// Fold the index (and the motion, when given) into visits: who, where, how
// long, how far.
export function readWalks(lines, motion = new Map()) {
  const visits = new Map();
  for (const line of lines) {
    let row;
    try { row = JSON.parse(line); } catch { continue; }
    if (!row?.v) continue;
    let v = visits.get(row.v);
    if (!v) visits.set(row.v, (v = { id: row.v, room: null, who: null, guest: false, seated: false,
      start: row.t, end: row.t, left: false, distance: 0, ticks: 0 }));
    v.end = Math.max(v.end, row.t);
    if (row.e === 'enter') v.room = row.room;
    if (row.e === 'who') {
      v.who = row.handle ? '@' + row.handle : `${row.name} (${row.g})`;
      v.guest = !row.handle;
      if (row.seat >= 0) v.seated = true;
    }
    if (row.e === 'seam') v.seam = { from: row.from, ms: row.ms };
    if (row.e === 'leave') v.left = true;
  }
  for (const v of visits.values()) {
    const path = motion.get(v.id) || [];
    v.ticks = path.length;
    for (let i = 1; i < path.length; i++) v.distance += Math.hypot(path[i].x - path[i - 1].x, path[i].z - path[i - 1].z);
    if (path.length) v.end = Math.max(v.end, path.at(-1).t);
  }
  return [...visits.values()].sort((a, b) => a.start - b.start);
}
