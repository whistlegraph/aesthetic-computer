import { itemOfObject, targetOfObject } from './item-words.mjs';
import { rackFor } from './armory.mjs';
import { randomUUID, randomBytes, createHash } from 'node:crypto';
import { mkdir, readFile, writeFile, rename, copyFile } from 'node:fs/promises';
import { constants } from 'node:fs';
import { join } from 'node:path';
import { geometry, MAX_OBJECTS, MAX_VERSIONS } from './objects.mjs';
import { WORLD, halfOf } from './world.mjs';
const digest = (token) => createHash('sha256').update(token).digest('hex');
// Builds placed when the room was larger come inside the walls: the whole
// room scales about its centre until the farthest build stands within
// `limit`, so the layout keeps its shape. Returns true when anything moved.
export function fitRoom(list, limit = WORLD - 1.5) {
  let far = 0;
  for (const o of list) far = Math.max(far, Math.abs(o.x || 0), Math.abs(o.z || 0));
  if (far <= limit) return false;
  const k = limit / far;
  for (const o of list) {
    o.x = Math.round((o.x || 0) * k * 100) / 100;
    o.z = Math.round((o.z || 0) * k * 100) / 100;
  }
  return true;
}
// Nothing taken out of a room is thrown away at once: the last 64 of each
// room wait in data.removed, with who took them out and when.
function keep(data, room, gone, by) {
  if (!gone.length) return;
  const bin = ((data.removed ||= {})[room] ||= []);
  bin.unshift(...gone.map((o) => ({ ...o, removedAt: Date.now(), removedBy: by })));
  bin.length = Math.min(bin.length, 64);
}

export class ObjectStore {
  constructor(directory) {
    this.directory = directory;
    this.data = { owners: {}, rooms: {} };
    this.queue = Promise.resolve();
    // 🎒 Things carried over a seam, in memory only: id → { room, origin, object }.
    // While carried, a thing is listed in the room it is in and not at home;
    // what is written down never moves (carry, uncarry).
    this.carried = new Map();
    // 🌐 A satellite's rooms as home keeps them (server.mjs, over the link): what is built there is home's.
    this.mirror = new Map();
  }
  setMirror(room, list) { if (Array.isArray(list)) this.mirror.set(room, list.slice(0, 256)); }
  async load() {
    if (!this.directory) return;
    await mkdir(this.directory, { recursive: true, mode: 0o700 });
    try {
      this.data = JSON.parse(await readFile(join(this.directory, 'objects.json'), 'utf8'));
    } catch (e) {
      if (e.code !== 'ENOENT') throw e;
    }
    // Rooms built before the walls closed in: bring their builds inside.
    let moved = false;
    for (const [room, list] of Object.entries(this.data.rooms || {})) moved = fitRoom(list, halfOf(room) - 1.5) || moved;
    if (moved) await this.transact(() => true);
  }
  // 🏢 Everything a room holds goes to another: builds, screens, painted
  // squares (where the other has none), history and what was put away.
  // A copy of the file as it stood is kept first, once.
  async moveRooms(pairs) {
    if (!pairs.length) return 0;
    if (this.directory)
      await copyFile(join(this.directory, 'objects.json'), join(this.directory, 'objects.before-houses.json'), constants.COPYFILE_EXCL)
        .catch((e) => { if (e.code !== 'EEXIST' && e.code !== 'ENOENT') throw e; });
    return this.transact((d) => {
      let moved = 0;
      for (const [from, to] of pairs) {
        if (d.rooms?.[from]?.length) { d.rooms[to] = [...(d.rooms[to] || []), ...d.rooms[from]]; moved += d.rooms[from].length; }
        delete d.rooms?.[from];
        for (const part of ['screens', 'history', 'removed']) {
          if (d[part]?.[from]?.length) d[part][to] = [...(d[part][to] || []), ...d[part][from]];
          delete d[part]?.[from];
        }
        if (d.tiles?.[from] && !Object.keys(d.tiles[to] || {}).length) d.tiles[to] = d.tiles[from];
        for (const part of ['tiles', 'walls', 'wallRes']) delete d[part]?.[from];
      }
      for (const [, to] of pairs) if (d.rooms?.[to]) fitRoom(d.rooms[to], halfOf(to) - 1.5);
      return moved;
    });
  }
  // 🧹 Once: every build standing on the ground (the rooms `ground` says
  // yes to) is put away, as a removal is (it can be put back); the floor's
  // paint stays. A mark is left so it never runs again.
  async clearGround(ground, by = null) {
    if (this.data.marks?.groundCleared) return 0;
    return this.transact((d) => {
      let gone = 0;
      for (const room of Object.keys(d.rooms || {})) {
        if (!ground(room) || !d.rooms[room].length) continue;
        gone += d.rooms[room].length;
        keep(d, room, d.rooms[room], by);
        d.rooms[room] = [];
      }
      (d.marks ||= {}).groundCleared = Date.now();
      return gone;
    });
  }
  transact(fn) {
    const task = this.queue.then(async () => {
      const next = structuredClone(this.data),
        result = fn(next);
      if (this.directory) {
        const path = join(this.directory, 'objects.json');
        await writeFile(path + '.tmp', JSON.stringify(next), { mode: 0o600 });
        await rename(path + '.tmp', path);
      }
      this.data = next;
      return result;
    });
    this.queue = task.catch(() => {});
    return task;
  }
  identity(token) {
    return this.transact((data) => {
      if (typeof token === 'string' && /^[a-f0-9]{64}$/.test(token) && data.owners[digest(token)])
        return { owner: data.owners[digest(token)], token };
      if (Object.keys(data.owners).length >= 10000) throw Error('Identity capacity reached');
      const fresh = randomBytes(32).toString('hex'),
        owner = randomUUID();
      data.owners[digest(fresh)] = owner;
      return { owner, token: fresh };
    });
  }
  objects(room) {
    const here = (o) => { const c = this.carried.get(o.id); return !c || c.room === room; };
    return this.stored(room).concat(rackFor(room)).filter(here) // (the lobby's house guns, armory.mjs)
      .concat([...this.carried.values()].filter((c) => c.room === room && c.room !== c.origin).map((c) => c.object));
  }
  // 🎒 A held thing goes over a seam with whoever holds it: listed in `room`
  // from now on, and not in the room it came from, until uncarried.
  carry(id, room, at) {
    const was = this.carried.get(id);
    const origin = was?.origin || this.whereIs(id);
    if (!origin) return null;
    const object = was?.object || this.objects(origin).find((o) => o.id === id);
    if (!object) return null;
    if (room === origin) { this.carried.delete(id); return { origin, room }; }
    this.carried.set(id, { room, origin, object: { ...object, x: at.x, z: at.z, house: false, carried: true } });
    return { origin, room, from: was?.room || origin };
  }
  // 🌐 A thing carried in from another host (`from`: '~' and that host). If it
  // was ours to begin with (carried away and now back), it is simply carried here.
  carryIn(object, room, at, from) {
    if (!object?.id) return null;
    // (Ours only if it went away from here: every host lists the house racks, so a rack gun is not proof.)
    if (this.carried.has(object.id)) return this.carry(object.id, room, at);
    this.carried.set(object.id, { room, origin: from, object: { ...object, x: at.x, z: at.z, house: false, carried: true } });
    return { origin: from, room };
  }
  // Home again: listed where it was built (or racked) once more. → where it had been.
  uncarry(id) {
    const c = this.carried.get(id);
    this.carried.delete(id);
    return c || null;
  }
  whereIs(id) {
    for (const [room, list] of Object.entries(this.data.rooms)) if (list.some((o) => o.id === id)) return room;
    for (const room of ['lobby', 'east']) if (rackFor(room).some((o) => o.id === id)) return room;
    return null;
  }
  stored(room) {
    if (this.mirror.has(room)) return this.mirror.get(room);
    return (this.data.rooms[room] || []).map((o) => {
      const v = o.versions.at(-1);
      return { id: o.id, owner: o.owner, x: o.x, z: o.z, item: o.item || itemOfObject(o.versions[0]?.prompt, v.geometry?.name), target: !o.item && targetOfObject(o.versions[0]?.prompt, v.geometry?.name), effect: o.effect || null, version: v.version, code: v.code || null, geometry: v.geometry, first: o.versions[0]?.prompt || '' };
    });
  }
  // Record a version's KidLisp $code once the store has issued it, and
  // remember it in the room's history (which outlives clearing the room).
  setCode(room, id, version, code, who = {}) {
    return this.transact((data) => {
      const o = (data.rooms[room] || []).find((x) => x.id === id);
      const v = o?.versions.find((x) => x.version === version);
      if (!v) return false;
      v.code = code;
      const history = ((data.history ||= {})[room] ||= []);
      if (!history.some((h) => h.code === code))
        history.unshift({
          code,
          name: v.geometry?.name || 'object',
          version,
          item: o.item || null,
          effect: o.effect || null,
          geometry: v.geometry,
          at: Date.now(),
          // Signed for the builder's own history across rooms and sessions.
          owner: o.owner || null,
          handle: who.handle || null,
          room,
        });
      history.length = Math.min(history.length, 64);
      return true;
    });
  }
  // One history per user across every room and session (2026-11-03): the
  // entries they signed, by handle when logged in, else by owner identity.
  // Without a user, the room's own list as before.
  history(room, who = null) {
    const all = Object.values(this.data.history || {}).flat();
    const mine = who
      ? all.filter((h) => (who.handle && h.handle === who.handle) || (who.owner && h.owner === who.owner))
      : this.data.history?.[room] || [];
    const seen = new Set();
    return [...mine]
      .sort((a, b) => (b.at || 0) - (a.at || 0))
      .filter((h) => !seen.has(h.code) && seen.add(h.code))
      .slice(0, 64)
      .map(({ code, name, version, item, effect, at }) => ({ code, name, version, item, at, kind: item ? 'item' : effect ? 'effect' : 'object' }));
  }
  historyEntry(room, code) {
    for (const list of [this.data.history?.[room] || [], ...Object.values(this.data.history || {})]) {
      const e = list.find((h) => h.code === code);
      if (e) return e;
    }
    return null;
  }
  removeObject(room, id, by = null) {
    return this.transact((data) => {
      const list = data.rooms[room] || [];
      const i = list.findIndex((o) => o.id === id);
      if (i < 0) return null;
      const [gone] = list.splice(i, 1);
      keep(data, room, [gone], by);
      return { id: gone.id, x: gone.x, z: gone.z };
    });
  }
  // What was taken out of a room, newest first, to put back (putBack).
  removed(room) {
    return (this.data.removed?.[room] || []).map((o) => ({ ...o }));
  }
  putBack(room, id) {
    return this.transact((data) => {
      const bin = data.removed?.[room] || [], i = bin.findIndex((o) => o.id === id);
      if (i < 0) return null;
      const [{ removedAt, removedBy, ...back }] = bin.splice(i, 1);
      (data.rooms[room] ||= []).push(back);
      return back;
    });
  }
  // ✨ Take out of a room every conjuring whose time is up (builds that stand
  // there, screens, painted squares). Back comes where the builds stood, for
  // a puff of dust each, and whether anything changed. A build that is in
  // someone's hands is not in the room's list and is left to them.
  fade(room, now = Date.now()) {
    const due = (list) => (list || []).some((o) => o.fades && o.fades <= now);
    const d = this.data;
    if (!d) return Promise.resolve({ gone: [], changed: false });
    if (!due(d.rooms?.[room]) && !due(d.screens?.[room]) && !Object.values(d.tileFades?.[room] || {}).some((f) => f.until <= now)) return Promise.resolve({ gone: [], changed: false });
    return this.transact((data) => {
      const gone = [];
      const list = data.rooms[room] || [];
      for (let i = list.length - 1; i >= 0; i--) if (list[i].fades && list[i].fades <= now) { const [o] = list.splice(i, 1); gone.push({ id: o.id, x: o.x, z: o.z }); }
      let changed = gone.length > 0;
      if (data.screens?.[room]) {
        const kept = data.screens[room].filter((s) => !(s.fades && s.fades <= now));
        if (kept.length !== data.screens[room].length) { for (const s of data.screens[room]) if (s.fades && s.fades <= now) gone.push({ id: s.id, x: s.x, z: s.z }); data.screens[room] = kept; changed = true; }
      }
      const faded = data.tileFades?.[room];
      if (faded) for (const [key, f] of Object.entries(faded)) if (f.until <= now) {
        const tiles = ((data.tiles ||= {})[room] ||= {});
        if (f.was) tiles[key] = f.was; else delete tiles[key];
        delete faded[key];
        changed = true;
      }
      return { gone, changed };
    });
  }
  // Stand a build somewhere else in its room.
  moveObject(room, id, x, z) {
    return this.transact((data) => {
      const o = (data.rooms[room] || []).find((o) => o.id === id);
      if (o) Object.assign(o, { x, z });
      return !!o;
    });
  }
  clearRoom(room, by = null) {
    return this.transact((data) => {
      keep(data, room, data.rooms[room] || [], by);
      // The floor's paint and screens as they were, once, beside the builds.
      if (data.tiles?.[room] || data.screens?.[room])
        ((data.cleared ||= {})[room] = { at: Date.now(), by, tiles: data.tiles?.[room] || {}, screens: data.screens?.[room] || [] });
      data.rooms[room] = [];
      if (data.tiles) delete data.tiles[room];
      if (data.screens) delete data.screens[room];
      return true;
    });
  }
  // Standing KidLisp screens, one per $code prompt: { id, owner, code, x, z, yaw, at }.
  // (standing: screens a place has of its own, { room: [screen] }, set by the
  // server and never kept here; one of the same id written down earlier gives way.)
  screens(room) {
    const own = this.standing?.[room] || [], ids = new Set(own.map((s) => s.id));
    return [...own, ...(this.data.screens?.[room] || []).filter((s) => !ids.has(s.id))].map((s) => ({ ...s }));
  }
  addScreen(room, screen) {
    return this.transact((data) => {
      const list = ((data.screens ||= {})[room] ||= []);
      if (list.length >= 24) list.shift();
      list.push(screen);
      return screen;
    });
  }
  // KidLisp on the walls: { north: 'roz', west: 'bop' } until changed.
  walls(room) {
    return { north: 'roz', west: 'bop', ...(this.data.walls?.[room] || {}) };
  }
  // Pixel grid of each screen: { north: 48, west: 64 } columns (16:9).
  wallRes(room) {
    return { north: 64, west: 64, ...(this.data.wallRes?.[room] || {}) };
  }
  stepWallRes(room, face, dir) {
    return this.transact((data) => {
      const steps = [16, 24, 32, 48, 64, 96, 128];
      const res = ((data.wallRes ||= {})[room] ||= {});
      const i = steps.indexOf(res[face] || 64);
      res[face] = steps[Math.max(0, Math.min(steps.length - 1, (i < 0 ? 4 : i) + dir))];
      return res[face];
    });
  }
  setWall(room, face, code) {
    return this.transact((data) => {
      ((data.walls ||= {})[room] ||= {})[face] = code;
      return true;
    });
  }
  // The floor's painted squares: { "i,j": colour index 1..6 }.
  tiles(room) {
    return this.data.tiles?.[room] || {};
  }
  // Cycle a square through the palette and back to bare, or set it outright
  // (`colour` 0..6, 0 bare): a held brush dragged across the floor.
  paintTile(room, i, j, colour = null, fades = 0) {
    return this.transact((data) => {
      const tiles = ((data.tiles ||= {})[room] ||= {});
      const key = `${i},${j}`;
      const next = Number.isInteger(colour) && colour >= 0 && colour <= 6 ? colour : ((tiles[key] || 0) + 1) % 7;
      // ✨ A conjured square: what lay there before is kept, and comes back when it fades (the first of a run of them is the one kept).
      const faded = ((data.tileFades ||= {})[room] ||= {});
      if (fades > 0) faded[key] = { until: Date.now() + fades, was: faded[key] ? faded[key].was : tiles[key] || 0 };
      else delete faded[key];
      if (next) tiles[key] = next;
      else delete tiles[key];
      return tiles;
    });
  }
  get(room, id) {
    return this.objects(room).find((o) => o.id === id);
  }
  commit(job, raw, validate = () => {}) {
    const asked = geometry(raw);
    return this.transact((data) => {
      // Placement may move or shrink the shape so it fits; commit what it chose.
      const shape = validate() || asked;
      const objects = (data.rooms[job.room] ||= []);
      let object = objects.find((o) => o.id === job.objectId);
      if (object) {
        if (object.owner !== job.owner || object.versions.at(-1).version !== job.baseVersion)
          throw Error('Object changed; prompt again');
        if (object.versions.length >= MAX_VERSIONS) throw Error('Version limit reached');
        object.x = job.x;
        object.z = job.z;
        // (A plain object that its first prompt names as an item becomes one now.)
        if (job.item && !object.item) Object.assign(object, { item: job.item, effect: job.effect || null });
      } else {
        if (job.baseVersion !== -1 || objects.length >= MAX_OBJECTS)
          throw Error('Object capacity reached');
        object = { id: job.objectId, owner: job.owner, x: job.x, z: job.z, item: job.item || null, effect: job.effect || null, versions: [] };
        objects.push(object);
      }
      // ✨ A conjuring (server.mjs, the one room): it stands until `fades` and is gone (fade below). Said again, it stands that long again.
      if (job.fades > 0) object.fades = Date.now() + job.fades;
      else delete object.fades;
      object.versions.push({
        version: job.baseVersion + 1,
        prompt: job.prompt,
        at: Date.now(),
        geometry: shape,
      });
      return {
        id: object.id,
        owner: object.owner,
        x: object.x,
        z: object.z,
        version: object.versions.at(-1).version,
        geometry: shape,
        ...(object.fades ? { fades: object.fades } : {}),
      };
    });
  }
}
