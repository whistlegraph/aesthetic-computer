import { randomUUID, timingSafeEqual } from 'node:crypto';
import {
  geometry,
  objectBoxes,
  MAX_OBJECTS,
  MAX_VERSIONS,
} from './objects.mjs';
import { WALLS, bodyBox } from './world.mjs';
import { OBJECT_FORMAT } from './object-language.mjs';
function envelope(objects) {
  return objects.flatMap((o) =>
    objectBoxes([
      {
        ...o,
        geometry: { ...o.geometry, format: undefined, motion: undefined },
      },
    ]).map((b, i) => {
      const m = o.geometry.motion;
      if (!m) return b;
      const out = {
        ...b,
        [m.axis + 'Min']: b[m.axis + 'Min'] - m.distance - 0.2,
        [m.axis + 'Max']: b[m.axis + 'Max'] + m.distance + 0.2,
      };
      if (m.type === 'fly') {
        const other = m.axis === 'x' ? 'z' : 'x';
        out[other + 'Min'] -= m.distance / 2 + 0.2;
        out[other + 'Max'] += m.distance / 2 + 0.2;
        const part = o.geometry.boxes[i],
          reach = part.part?.endsWith('wing') ? part.w : 0;
        out.yMin += m.height - 0.25 - reach;
        out.yMax += m.height + 0.25 + reach;
      }
      return out;
    }),
  );
}
// Overlap volume, zero when the boxes only touch.
const intersection = (a, b) =>
  ['x', 'y', 'z'].reduce(
    (v, k) =>
      v *
      Math.max(
        0,
        Math.min(a[k + 'Max'], b[k + 'Max']) -
          Math.max(a[k + 'Min'], b[k + 'Min']) -
          0.001,
      ),
    1,
  );
function scaleShape(shape, k) {
  return geometry({
    ...shape,
    boxes: shape.boxes.map((b) => ({
      ...b,
      x: b.x * k,
      y: b.y * k,
      z: b.z * k,
      w: b.w * k,
      h: b.h * k,
      d: b.d * k,
    })),
    motion: shape.motion && { ...shape.motion, distance: shape.motion.distance * k },
  });
}
export class Builders {
  constructor({ token, store, rooms, send, publish = null }) {
    Object.assign(this, { token, store, rooms, send, publish });

    this.workers = new Map();
    this.jobs = new Map();
  }
  authorized(header) {
    const expected = Buffer.from(`Bearer ${this.token || ''}`),
      actual = Buffer.from(header || '');
    return (
      !!this.token &&
      this.token.length >= 32 &&
      expected.length === actual.length &&
      timingSafeEqual(expected, actual)
    );
  }
  state(room) {
    return {
      type: 'objects',
      objects: this.store.objects(room),
      builder: this.workers.size > 0,
      jobs: [...this.jobs.values()]
        .filter((j) => j.room === room)
        .map((j) => ({
          id: j.id,
          owner: j.owner,
          objectId: j.objectId,
          version: j.baseVersion + 1,
          x: j.x,
          z: j.z,
          startedAt: j.startedAt,
          phase: j.phase || 'connecting',
          code: j.code || '',
          preview: j.preview || null,
        })),
    };
  }
  // Give versions that predate $codes theirs.
  async backfill() {
    if (!this.publish) return;
    for (const [room, list] of Object.entries(this.store.data.rooms || {})) {
      for (const o of list) {
        const v = o.versions.at(-1);
        if (v.code || !v.geometry?.source) continue;
        try {
          await this.store.setCode(room, o.id, v.version, await this.publish(v.geometry.source));
        } catch (e) {
          console.log(`dool $code backfill failed: ${e.message}`);
        }
      }
    }
  }
  broadcast(room) {
    const current = this.rooms.get(room);
    if (!current) return;
    current.match.objects = this.store.objects(room);
    for (const client of current.clients.keys())
      this.send(client, this.state(room));
  }
  all() {
    for (const room of this.rooms.keys()) this.broadcast(room);
  }
  attach(ws) {
    const work = new Set();
    this.workers.set(ws, work);
    this.all();
    let alive = true;
    ws.on('pong', () => {
      alive = true;
    });
    const heartbeat = setInterval(() => {
      if (!alive) return ws.terminate();
      alive = false;
      ws.ping();
    }, 10000);
    ws.on('message', async (raw) => {
      let data;
      try {
        data = JSON.parse(raw);
      } catch {
        ws.close(1008, 'Invalid builder response');
        return;
      }
      const job = this.jobs.get(data.id);
      if (!job || job.worker !== ws || !work.has(job.id) || job.finishing)
        return;
      if (data.type === 'progress') {
        if (Date.now() - (job.progressAt || 0) < 100) return;
        job.progressAt = Date.now();
        job.phase = ['generating', 'correcting', 'validating'].includes(
          data.phase,
        )
          ? data.phase
          : 'generating';
        job.code =
          typeof data.code === 'string'
            ? data.code.replace(/[^\x20-\x7e\n]/g, '').slice(-900)
            : '';
        job.preview = null;
        if (data.geometry) {
          try {
            const shape = geometry(data.geometry);
            job.preview = this.place(job, shape);
          } catch {}
        }
        this.broadcast(job.room);
        return;
      }
      job.finishing = true;
      try {
        if (data.type !== 'built')
          throw Error(
            typeof data.error === 'string'
              ? data.error.replace(/[^\x20-\x7e]/g, '').slice(0, 220)
              : 'Aesel could not build that object',
          );
        const shape = geometry(data.geometry);
        const committed = await this.store.commit(job, shape, () => this.place(job, shape));
        // Each finished version is a KidLisp program: give it a $code.
        if (this.publish && committed.geometry?.source)
          this.publish(committed.geometry.source)
            .then((code) => this.store.setCode(job.room, committed.id, committed.version, code))
            .then(() => this.broadcast(job.room))
            .catch((e) => console.log(`dool $code failed: ${e.message}`));
        this.send(job.client, {
          type: 'build-result',
          id: job.id,
          objectId: job.objectId,
          version: job.baseVersion + 1,
        });
      } catch (e) {
        console.log(`dool build failed (${job.room}): ${e.message} | prompt: ${job.prompt}`);
        this.send(job.client, {
          type: 'build-result',
          id: job.id,
          error: e.message,
        });
      } finally {
        this.finish(job);
      }
    });
    ws.on('error', () => ws.terminate());
    ws.on('close', () => {
      clearInterval(heartbeat);
      this.workers.delete(ws);
      for (const id of work) {
        const job = this.jobs.get(id);
        if (job && !job.finishing) {
          this.send(job.client, {
            type: 'build-result',
            id,
            error: 'Aesel disconnected. Your previous version is unchanged.',
          });
          this.finish(job);
        }
      }
      this.all();
    });
  }
  finish(job) {
    clearTimeout(job.timer);
    this.jobs.delete(job.id);
    this.workers.get(job.worker)?.delete(job.id);
    this.broadcast(job.room);
  }
  request(client, room, owner, seat, input) {
    try {
      if (!owner) throw Error('Identity is not ready');
      const prompt =
        typeof input.prompt === 'string' ? input.prompt.trim() : '';
      if (!prompt || prompt.length > 280 || /[\x00-\x1f]/.test(prompt))
        throw Error('Use 1–280 characters');
      if ([...this.jobs.values()].some((j) => j.owner === owner))
        throw Error('Aesel is still working on your prompt');
      const existing = input.objectId
        ? this.store.get(room, input.objectId)
        : null;
      if (input.objectId && (!existing || existing.owner !== owner))
        throw Error('Only the owner can revise this object');
      if (existing && input.baseVersion !== existing.version)
        throw Error('Object changed; reopen the prompt');
      if (existing?.version >= MAX_VERSIONS - 1)
        throw Error('Version limit reached');
      if (!existing && this.store.objects(room).length >= MAX_OBJECTS)
        throw Error('Object capacity reached');
      const worker = [...this.workers].find(([, work]) => work.size < 2)?.[0];
      if (!worker)
        throw Error(
          this.workers.size
            ? 'Aesel is busy. Try again shortly.'
            : 'Aesel is offline',
        );
      const player = this.rooms.get(room)?.match.seats[seat]?.player;
      if (!player) throw Error('Rejoin the room');
      const yaw = (player.yaw * Math.PI) / 180;
      const job = {
        id: randomUUID(),
        objectId: existing?.id || randomUUID(),
        owner,
        room,
        prompt,
        baseVersion: existing?.version ?? -1,
        x: existing?.x ?? Math.round(player.x + Math.sin(yaw) * 6),
        z: existing?.z ?? Math.round(player.z + Math.cos(yaw) * 6),
        client,
        worker,
        startedAt: Date.now(),
      };
      // Reserve a visible initial footprint even before the first code arrives.
      try {
        this.place(
          job,
          existing?.geometry || {
            name: 'Footprint',
            boxes: [{ x: 0, y: 0.1, z: 0, w: 2, h: 0.2, d: 3 }],
          },
        );
      } catch {}
      job.timer = setTimeout(() => {
        if (job.finishing) return;
        this.send(worker, { type: 'cancel', id: job.id });
        this.send(client, {
          type: 'build-result',
          id: job.id,
          error: 'Aesel timed out. Try again.',
        });
        this.finish(job);
      }, 180000);
      this.jobs.set(job.id, job);
      this.workers.get(worker).add(job.id);
      this.send(worker, {
        type: 'build',
        id: job.id,
        prompt,
        previous: existing?.geometry || null,
      });
      this.send(client, {
        type: 'build-accepted',
        id: job.id,
        objectId: job.objectId,
      });
      this.broadcast(room);
    } catch (e) {
      this.send(client, { type: 'build-result', error: e.message });
    }
  }
  cancel(owner, room) {
    const job = [...this.jobs.values()].find(
      (j) => j.owner === owner && j.room === room,
    );
    if (!job || job.finishing) return;
    this.send(job.worker, { type: 'cancel', id: job.id });
    this.send(job.client, {
      type: 'build-result',
      id: job.id,
      cancelled: true,
    });
    this.finish(job);
  }
  place(job, shape) {
    const players =
      this.rooms
        .get(job.room)
        ?.match.seats.filter(Boolean)
        .map((s) => bodyBox(s.player)) || [];
    const occupied = [
      ...WALLS,
      ...envelope(
        this.store.objects(job.room).filter((o) => o.id !== job.objectId),
      ),
      ...players,
      ...[-12, 12].map((z) => ({
        xMin: -1.5,
        xMax: 1.5,
        yMin: 0,
        yMax: 3,
        zMin: z - 1.5,
        zMax: z + 1.5,
      })),
    ];
    // Keep looking until it fits: the asked-for spot, then every metre of the
    // court nearest first, then the same at smaller scales. If the room is
    // truly full, take the spot with the least overlap. Never refuse.
    const positions = [[job.x, job.z]];
    for (let x = -16; x <= 16; x++)
      for (let z = -16; z <= 16; z++) positions.push([x, z]);
    positions.sort(
      (a, b) =>
        Math.hypot(a[0] - job.x, a[1] - job.z) -
        Math.hypot(b[0] - job.x, b[1] - job.z),
    );
    const scalable = shape.format !== OBJECT_FORMAT;
    let best = null;
    for (const k of scalable ? [1, 0.85, 0.7, 0.55, 0.4] : [1]) {
      const sized = k === 1 ? shape : scaleShape(shape, k);
      for (const [x, z] of positions) {
        const boxes = envelope([{ x, z, geometry: sized }]);
        if (
          boxes.some(
            (b) => b.xMin < -17 || b.xMax > 17 || b.zMin < -17 || b.zMax > 17,
          )
        )
          continue;
        const overlap = boxes.reduce(
          (sum, b) => sum + occupied.reduce((t, o) => t + intersection(b, o), 0),
          0,
        );
        if (overlap < 1e-6) {
          job.x = x;
          job.z = z;
          return sized;
        }
        if (!best || overlap < best.overlap) best = { overlap, x, z, sized };
      }
    }
    if (best) {
      job.x = best.x;
      job.z = best.z;
      return best.sized;
    }
    job.x = 0;
    job.z = 0;
    return shape;
  }
  close() {
    for (const job of this.jobs.values()) clearTimeout(job.timer);
    for (const worker of this.workers.keys()) worker.terminate();
    this.jobs.clear();
  }
}
