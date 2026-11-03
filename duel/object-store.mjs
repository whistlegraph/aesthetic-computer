import { randomUUID, randomBytes, createHash } from 'node:crypto';
import { mkdir, readFile, writeFile, rename } from 'node:fs/promises';
import { join } from 'node:path';
import { geometry, MAX_OBJECTS, MAX_VERSIONS } from './objects.mjs';
const digest = (token) => createHash('sha256').update(token).digest('hex');
export class ObjectStore {
  constructor(directory) {
    this.directory = directory;
    this.data = { owners: {}, rooms: {} };
    this.queue = Promise.resolve();
  }
  async load() {
    if (!this.directory) return;
    await mkdir(this.directory, { recursive: true, mode: 0o700 });
    try {
      this.data = JSON.parse(await readFile(join(this.directory, 'objects.json'), 'utf8'));
    } catch (e) {
      if (e.code !== 'ENOENT') throw e;
    }
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
    return (this.data.rooms[room] || []).map((o) => {
      const v = o.versions.at(-1);
      return { id: o.id, owner: o.owner, x: o.x, z: o.z, item: o.item || null, effect: o.effect || null, version: v.version, code: v.code || null, geometry: v.geometry };
    });
  }
  // Record a version's KidLisp $code once the store has issued it, and
  // remember it in the room's history (which outlives clearing the room).
  setCode(room, id, version, code) {
    return this.transact((data) => {
      const o = (data.rooms[room] || []).find((x) => x.id === id);
      const v = o?.versions.find((x) => x.version === version);
      if (!v) return false;
      v.code = code;
      const history = ((data.history ||= {})[room] ||= []);
      if (!history.some((h) => h.code === code))
        history.unshift({ code, name: v.geometry?.name || 'object', version, item: o.item || null, effect: o.effect || null, geometry: v.geometry, at: Date.now() });
      history.length = Math.min(history.length, 64);
      return true;
    });
  }
  history(room) {
    return (this.data.history?.[room] || []).map(({ code, name, version, item, at }) => ({ code, name, version, item, at }));
  }
  historyEntry(room, code) {
    return (this.data.history?.[room] || []).find((h) => h.code === code) || null;
  }
  removeObject(room, id) {
    return this.transact((data) => {
      const list = data.rooms[room] || [];
      const i = list.findIndex((o) => o.id === id);
      if (i < 0) return null;
      const [gone] = list.splice(i, 1);
      return { id: gone.id, x: gone.x, z: gone.z };
    });
  }
  clearRoom(room) {
    return this.transact((data) => {
      data.rooms[room] = [];
      if (data.tiles) delete data.tiles[room];
      return true;
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
  // Cycle a square through the palette and back to bare.
  paintTile(room, i, j) {
    return this.transact((data) => {
      const tiles = ((data.tiles ||= {})[room] ||= {});
      const key = `${i},${j}`;
      const next = ((tiles[key] || 0) + 1) % 7;
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
      } else {
        if (job.baseVersion !== -1 || objects.length >= MAX_OBJECTS)
          throw Error('Object capacity reached');
        object = { id: job.objectId, owner: job.owner, x: job.x, z: job.z, item: job.item || null, effect: job.effect || null, versions: [] };
        objects.push(object);
      }
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
      };
    });
  }
}
