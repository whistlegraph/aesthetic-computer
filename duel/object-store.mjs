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
  // Record a version's KidLisp $code once the store has issued it.
  setCode(room, id, version, code) {
    return this.transact((data) => {
      const v = (data.rooms[room] || [])
        .find((o) => o.id === id)
        ?.versions.find((x) => x.version === version);
      if (v) v.code = code;
      return !!v;
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
