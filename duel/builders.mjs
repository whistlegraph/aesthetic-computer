import { randomUUID, timingSafeEqual } from 'node:crypto';
import { buildReason } from './diagnostics.mjs';
import {
  geometry,
  objectBoxes,
  MAX_OBJECTS,
  MAX_VERSIONS,
} from './objects.mjs';
import { WALLS, bodyBox, WORLD } from './world.mjs';
import { OBJECT_FORMAT, fitItemSource, sourceGeometry } from './object-language.mjs';
// Builds a guest identity may start per day.
export const GUEST_BUILDS = 5;

export const ITEM_KINDS = ['shoot', 'poke', 'throw'];
// An item is hand-sized with its grip at the origin.
function asItem(shape, kind) {
  if (!kind || shape.format !== OBJECT_FORMAT) return shape;
  return sourceGeometry({ name: shape.name, source: fitItemSource(shape.source) });
}
const ITEM_HINT = {
  shoot: 'a handheld thing that shoots: grip at the origin, barrel pointing +z',
  poke: 'a handheld thing that pokes or swings: handle at the origin, reaching +z',
  throw: 'a small throwable thing, about a hand across, resting on y=0',
};
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
// Not a singleton: each player may have several builds going, and any
// number of builder processes may connect and share the work.
export const PER_PLAYER = 3;
export const PER_WORKER = 3;

export class Builders {
  constructor({ token, store, rooms, send, publish = null, diagnostics }) {
    Object.assign(this, { token, store, rooms, send, publish, diagnostics });

    this.workers = new Map();
    this.names = new Map(); // Each builder's name, from its hello.
    this.jobs = new Map();
  }
  record(event, job, fields = {}) {
    this.diagnostics?.emit(event, { id: job.id, session: job.client?.diagnosticSession,
      kind: job.kind || 'object', elapsedMs: Date.now() - job.startedAt, ...fields });
  }
  phase(job, phase) {
    if (job.phase === phase) return;
    if (job.phase) this.record('build.phase', job, { phase: job.phase, phaseMs: Date.now() - (job.phaseAt || job.startedAt) });
    job.phase = phase;
    job.phaseAt = Date.now();
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
  state(room, member = null) {
    return {
      type: 'objects',
      objects: this.store.objects(room),
      builder: this.workers.size > 0,
      builders: [...this.workers.keys()].map((ws) => this.names.get(ws) || 'aesel'),
      history: this.store.history(room, member ? { owner: member.owner, handle: member.handle } : null).slice(0, 24),
      tiles: this.store.tiles(room),
      walls: this.store.walls(room),
      wallRes: this.store.wallRes(room),
      screens: this.store.screens(room),
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
          kind: j.kind || 'object',
          prompt: String(j.prompt || '').slice(0, 60),
          face: j.face || null,
        })),
    };
  }
  async clearRoom(room) {
    await this.store.clearRoom(room);
    // A clean room: chalk goes too.
    const match = this.rooms.get(room)?.match;
    if (match) for (let seat = 0; seat < match.chalk.length; seat++) match.clearChalk(seat);
    this.broadcast(room);
  }
  // Put a copy of a remembered $code in front of the player, no Aesel needed.
  async restore(client, room, owner, seat, code) {
    try {
      let entry = this.store.historyEntry(room, code);
      if (!entry) {
        const r = await fetch(`https://aesthetic.computer/api/store-kidlisp?code=${code}`, { signal: AbortSignal.timeout(6000) });
        const { source } = await r.json();
        if (typeof source !== 'string') throw Error('Unknown $' + code);
        const name = /^\s*;\s*([^\n]+)/.exec(source)?.[1] || code;
        entry = { code, name, item: null, effect: null, geometry: sourceGeometry({ name, source }) };
      }
      if (this.store.objects(room).length >= MAX_OBJECTS) throw Error('Object capacity reached');
      const player = this.rooms.get(room)?.match.seats[seat]?.player;
      if (!player || !owner) throw Error('Rejoin the room');
      const yaw = (player.yaw * Math.PI) / 180;
      const job = {
        id: randomUUID(), objectId: randomUUID(), owner, room, prompt: `$${code}`, baseVersion: -1,
        item: entry.item, effect: entry.effect,
        x: Math.round(player.x + Math.sin(yaw) * 4), z: Math.round(player.z + Math.cos(yaw) * 4),
      };
      const shape = geometry(entry.geometry);
      const committed = await this.store.commit(job, shape, () => this.place(job, shape));
      await this.store.setCode(room, committed.id, committed.version, code);
      this.send(client, { type: 'build-result', id: job.id, objectId: committed.id, version: committed.version });
      this.broadcast(room);
    } catch (e) {
      this.send(client, { type: 'build-result', error: e.message });
    }
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
    current.match.setObjects(this.store.objects(room));
    for (const client of current.clients.keys())
      this.send(client, this.state(room, current.sessions?.get(client) || null));
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
      if (data.type === 'hello') {
        // The builder introduces itself; the world's readout names it.
        this.names.set(ws, String(data.name || 'aesel').slice(0, 24).toLowerCase());
        this.all();
        return;
      }
      if (data.type === 'resume') return this.resume(ws, work, data.jobs);
      const job = this.jobs.get(data.id);
      // A job handed back after a reconnect is this worker's again.
      if (job && !job.worker && job.adopt === data.id) {
        job.worker = ws;
        work.add(job.id);
      }
      if (!job || job.worker !== ws || !work.has(job.id) || job.finishing)
        return;
      if (data.type === 'progress') {
        if (Date.now() - (job.progressAt || 0) < 100) return;
        job.progressAt = Date.now();
        this.phase(job, ['generating', 'correcting', 'validating', 'waiting'].includes(
          data.phase,
        )
          ? data.phase
          : 'generating');
        job.code =
          typeof data.code === 'string'
            ? data.code.replace(/[^\x20-\x7e\n]/g, '').slice(-900)
            : '';
        job.preview = null;
        if (data.geometry) {
          const began = performance.now();
          job.previewCount = (job.previewCount || 0) + 1;
          try {
            const shape = asItem(geometry(data.geometry), job.item);
            job.preview = this.place(job, shape);
          } catch { job.previewRejected = (job.previewRejected || 0) + 1; }
          job.previewMaxMs = Math.max(job.previewMaxMs || 0, performance.now() - began);
        }
        this.broadcast(job.room);
        return;
      }
      job.finishing = true;
      if (job.kind === 'kidlisp') {
        try {
          if (data.type !== 'built' || typeof data.source !== 'string') throw Error(typeof data.error === 'string' ? data.error.slice(0, 220) : 'Aesel could not change that KidLisp');
          if (!this.publish) throw Error('No KidLisp store');
          const code = await this.publish(data.source, this.tokenOf(job));
          await this.store.setWall(job.room, job.face, code);
          job.outcome = 'committed';
          this.record('build.publication', job, { outcome: 'published', code });
          this.deliver(job, { type: 'build-result', id: job.id, kidlisp: code, face: job.face });
        } catch (e) {
          job.reason = buildReason(e);
          this.deliver(job, { type: 'build-result', id: job.id, error: e.message });
        } finally {
          this.finish(job);
        }
        return;
      }
      try {
        if (data.type !== 'built')
          throw Error(
            typeof data.error === 'string'
              ? data.error.replace(/[^\x20-\x7e]/g, '').slice(0, 220)
              : 'Aesel could not build that object',
          );
        const shape = asItem(geometry(data.geometry), job.item);
        const commitAt = performance.now();
        const committed = await this.store.commit(job, shape, () => this.place(job, shape));
        job.commitMs = performance.now() - commitAt;
        job.outcome = 'committed';
        // Each finished version is a KidLisp program: give it a $code.
        if (this.publish && committed.geometry?.source)
          this.publish(committed.geometry.source, this.tokenOf(job))
            .then(async (code) => {
              const saved = await this.store.setCode(job.room, committed.id, committed.version, code, { handle: this.handleOf(job) });
              this.record('build.publication', job, { outcome: saved ? 'published' : 'failed', code });
            })
            .then(() => this.broadcast(job.room))
            .catch((e) => this.record('build.publication', job, { outcome: 'failed', reason: buildReason(e) }));
        this.deliver(job, {
          type: 'build-result',
          id: job.id,
          objectId: job.objectId,
          version: job.baseVersion + 1,
        });
      } catch (e) {
        job.reason = buildReason(e);
        console.warn(`dool build failed: ${job.reason}`);
        this.deliver(job, {
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
      this.names.delete(ws);
      // The builder keeps working while it reconnects: hold its jobs for a
      // minute before giving up on them.
      for (const id of work) {
        const job = this.jobs.get(id);
        if (!job || job.finishing) continue;
        job.worker = null;
        job.adopt = id;
        job.orphanTimer = setTimeout(() => {
          if (job.finishing || job.worker) return;
          this.deliver(job, { type: 'build-result', id, error: 'Aesel disconnected. Your previous version is unchanged.' });
          job.outcome = 'disconnected';
          this.finish(job);
        }, 60000);
      }
      this.all();
    });
  }
  // The asking player's AC sign-in, if they have one (kept in memory only).
  handleOf(job) {
    for (const [, member] of this.rooms.get(job.room)?.sessions || []) if (member?.owner === job.owner && member.handle) return member.handle;
    return null;
  }
  tokenOf(job) {
    for (const [, member] of this.rooms.get(job.room)?.sessions || []) if (member?.owner === job.owner && member.token) return member.token;
    return null;
  }
  // Results go to the player who asked, found by identity: their socket may
  // have changed since (a reload or a server restart).
  deliver(job, message) {
    const room = this.rooms.get(job.room);
    const open = (ws) => ws && ws.readyState === 1;
    let to = open(job.client) ? job.client : null;
    if (!to && room)
      for (const [ws, member] of room.sessions || [])
        if (member?.owner === job.owner && open(ws)) to = ws;
    if (to) this.send(to, message);
  }
  finish(job) {
    this.record('build.finished', job, { outcome: job.outcome || (job.reason === 'timeout' ? 'timeout' : 'failed'),
      reason: job.reason, phase: job.phase || 'connecting', phaseMs: Date.now() - (job.phaseAt || job.startedAt),
      previewCount: job.previewCount || 0, previewRejected: job.previewRejected || 0,
      previewMaxMs: job.previewMaxMs || 0, commitMs: job.commitMs });
    clearTimeout(job.timer);
    clearTimeout(job.orphanTimer);
    this.jobs.delete(job.id);
    this.workers.get(job.worker)?.delete(job.id);
    this.broadcast(job.room);
  }
  // Guests build on a small daily allowance per identity, counted here in
  // memory; signed-in players read their own braincells instead.
  guestRemaining(owner) {
    const day = new Date().toISOString().slice(0, 10);
    const used = this.guestBuilds?.get(owner);
    return GUEST_BUILDS - (used?.day === day ? used.count : 0);
  }
  spendGuest(owner) {
    const day = new Date().toISOString().slice(0, 10);
    this.guestBuilds ??= new Map();
    const used = this.guestBuilds.get(owner);
    this.guestBuilds.set(owner, { day, count: (used?.day === day ? used.count : 0) + 1 });
  }
  request(client, room, owner, seat, input, { guest = false } = {}) {
    const requestId = randomUUID(), startedAt = Date.now();
    try {
      if (!owner) throw Error('Identity is not ready');
      if (guest && this.guestRemaining(owner) <= 0)
        throw Error(`Guest braincells spent (${GUEST_BUILDS} a day). Sign in to keep building.`);
      const prompt =
        typeof input.prompt === 'string' ? input.prompt.trim() : '';
      if (!prompt || prompt.length > 280 || /[\x00-\x1f]/.test(prompt))
        throw Error('Use 1–280 characters');
      if ([...this.jobs.values()].filter((j) => j.owner === owner).length >= PER_PLAYER)
        throw Error(`Aesel is already making ${PER_PLAYER} things for you`);
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
      // The least busy builder with room takes it.
      const worker = [...this.workers]
        .filter(([, work]) => work.size < PER_WORKER)
        .sort((a, b) => a[1].size - b[1].size)[0]?.[0];
      if (!worker)
        throw Error(
          this.workers.size
            ? 'Aesel is busy. Try again shortly.'
            : 'Aesel is offline',
        );
      const player = this.rooms.get(room)?.match.seats[seat]?.player;
      if (!player) throw Error('Rejoin the room');
      const item = existing ? existing.item : ITEM_KINDS.includes(input.item) ? input.item : null;
      // What an item does beyond its kind: explosives go off.
      const effect = existing
        ? existing.effect
        : item && /\b(bomb|grenade|dynamite|explosive|firework|tnt)s?\b/i.test(prompt)
          ? 'explode'
          : null;
      const yaw = (player.yaw * Math.PI) / 180;
      // Build from chalk: the player's strokes become a sketch to follow,
      // and the object goes where they were drawn.
      const strokes = input.chalk ? this.rooms.get(room)?.match.chalk[seat] || [] : [];
      const points = strokes.flat();
      let sketch = null;
      if (points.length >= 2) {
        const cx = Math.round(points.reduce((n, p) => n + p.x, 0) / points.length);
        const cz = Math.round(points.reduce((n, p) => n + p.z, 0) / points.length);
        const step = Math.max(1, Math.ceil(points.length / 80));
        const lines = strokes
          .map((s) => s.filter((_, k) => k % step === 0 || k === s.length - 1))
          .filter((s) => s.length >= 2)
          .map((s) => s.map((p) => `${(p.x - cx).toFixed(1)} ${p.y.toFixed(1)} ${(p.z - cz).toFixed(1)}`).join(', '));
        sketch = { x: cx, z: cz, text: lines.map((l, k) => `stroke ${k + 1}: ${l}`).join('; ') };
      }
      const job = {
        id: requestId,
        objectId: existing?.id || randomUUID(),
        owner,
        room,
        prompt,
        item,
        effect,
        baseVersion: existing?.version ?? -1,
        x: existing?.x ?? sketch?.x ?? Math.round(player.x + Math.sin(yaw) * 6),
        z: existing?.z ?? sketch?.z ?? Math.round(player.z + Math.cos(yaw) * 6),
        sketch: sketch?.text || null,
        client,
        worker,
        startedAt,
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
        job.outcome = 'timeout';
        this.send(worker, { type: 'cancel', id: job.id });
        this.send(client, {
          type: 'build-result',
          id: job.id,
          error: 'Aesel timed out. Try again.',
        });
        this.finish(job);
      }, 180000);
      if (guest) this.spendGuest(owner);
      this.jobs.set(job.id, job);
      this.record('build.accepted', job);
      this.workers.get(worker).add(job.id);
      this.send(worker, {
        type: 'build',
        id: job.id,
        prompt: [item ? `${prompt} (${ITEM_HINT[item]})` : prompt, job.sketch && `Follow this chalk sketch the player drew (x y z in metres, object origin at 0 0 0, y up); make its shape out of these lines: ${job.sketch}`].filter(Boolean).join('\n').slice(0, 6000),
        previous: existing?.geometry || null,
        meta: Builders.meta(job),
      });
      this.send(client, {
        type: 'build-accepted',
        id: job.id,
        objectId: job.objectId,
      });
      this.broadcast(room);
      return job.id;
    } catch (e) {
      this.record('build.rejected', { id: requestId, startedAt, client }, { reason: buildReason(e) });
      this.send(client, { type: 'build-result', id: requestId, error: e.message });
      return null;
    }
  }
  static meta(job) {
    const { id, objectId, owner, room, prompt, item, effect, baseVersion, x, z, sketch, startedAt, kind, face, code } = job;
    return { id, objectId, owner, room, prompt, item, effect, baseVersion, x, z, sketch, startedAt, kind, face, code };
  }
  // Change a wall's KidLisp: a job of its own kind, handed to a builder.
  async requestKidlisp(client, room, owner, input) {
    const requestId = randomUUID(), startedAt = Date.now();
    try {
      const face = ['north', 'west'].includes(input.face) ? input.face : null;
      const prompt = typeof input.prompt === 'string' ? input.prompt.trim().slice(0, 280) : '';
      if (!owner || !face || !prompt) throw Error('Choose a change for the wall');
      if ([...this.jobs.values()].filter((j) => j.owner === owner).length >= PER_PLAYER)
        throw Error(`Aesel is already making ${PER_PLAYER} things for you`);
      const worker = [...this.workers].filter(([, w]) => w.size < PER_WORKER).sort((a, b) => a[1].size - b[1].size)[0]?.[0];
      if (!worker) throw Error(this.workers.size ? 'Aesel is busy. Try again shortly.' : 'Aesel is offline');
      const code = this.store.walls(room)[face];
      const r = await fetch(`https://aesthetic.computer/api/store-kidlisp?code=${code}`, { signal: AbortSignal.timeout(6000) });
      const { source } = await r.json();
      const job = { id: requestId, kind: 'kidlisp', face, code, owner, room, prompt, objectId: null, baseVersion: 0, x: 0, z: 0, client, worker, startedAt };
      job.timer = setTimeout(() => {
        if (job.finishing) return;
        job.outcome = 'timeout';
        this.send(worker, { type: 'cancel', id: job.id });
        this.deliver(job, { type: 'build-result', id: job.id, error: 'Aesel timed out. Try again.' });
        this.finish(job);
      }, 180000);
      this.jobs.set(job.id, job);
      this.record('build.accepted', job);
      this.workers.get(worker).add(job.id);
      this.send(worker, { type: 'build', kind: 'kidlisp', id: job.id, prompt, source, meta: { ...Builders.meta(job), kind: 'kidlisp', face, code } });
      this.send(client, { type: 'build-accepted', id: job.id, kidlisp: true, face });
      this.broadcast(room);
    } catch (e) {
      this.record('build.rejected', { id: requestId, startedAt, client, kind: 'kidlisp' }, { reason: buildReason(e) });
      this.send(client, { type: 'build-result', id: requestId, error: e.message });
    }
  }
  // A builder that reconnects hands back the jobs it was still working on.
  resume(ws, work, list = []) {
    for (const entry of list.slice(0, PER_WORKER * 2)) {
      const m = entry?.meta;
      // The room may not be open yet (nobody back since the restart); the
      // job waits for it and shows when someone joins.
      if (!m || typeof m.id !== 'string' || this.jobs.has(m.id) || typeof m.room !== 'string') continue;
      const job = { ...m, worker: ws, client: null, phase: entry.phase || 'generating', code: entry.code || '' };
      job.timer = setTimeout(() => {
        if (job.finishing) return;
        job.outcome = 'timeout';
        this.deliver(job, { type: 'build-result', id: job.id, error: 'Aesel timed out. Try again.' });
        this.finish(job);
      }, 180000);
      this.jobs.set(job.id, job);
      work.add(job.id);
      this.broadcast(job.room);
    }
  }
  cancel(owner, room) {
    // The newest of this player's builds.
    const job = [...this.jobs.values()].reverse().find(
      (j) => j.owner === owner && j.room === room,
    );
    if (!job || job.finishing) return;
    this.send(job.worker, { type: 'cancel', id: job.id });
    this.deliver(job, {
      type: 'build-result',
      id: job.id,
      cancelled: true,
    });
    job.outcome = 'cancelled';
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
    for (let x = Math.round(job.x) - 16; x <= Math.round(job.x) + 16; x++)
      for (let z = Math.round(job.z) - 16; z <= Math.round(job.z) + 16; z++)
        if (Math.abs(x) < WORLD - 2 && Math.abs(z) < WORLD - 2) positions.push([x, z]);
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
            (b) => b.xMin < -WORLD + 1 || b.xMax > WORLD - 1 || b.zMin < -WORLD + 1 || b.zMax > WORLD - 1,
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
    for (const job of this.jobs.values()) {
      this.record('build.finished', job, { outcome: 'shutdown', phase: job.phase || 'connecting' });
      clearTimeout(job.timer);
      clearTimeout(job.orphanTimer);
    }
    for (const worker of this.workers.keys()) worker.terminate();
    this.jobs.clear();
  }
}
