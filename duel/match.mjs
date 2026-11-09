import {
  HZ,
  DT,
  FRAG_LIMIT,
  MAX_PLAYERS,
  FIRE_COOLDOWN,
  spawn,
  move,
  direction,
  rayBox,
  wallDistance,
  bodyBox,
  along,
  surfaceHit,
  reflect,
  WORLD,
} from './world.mjs';
import { command } from './net.mjs';

export class Match {
  constructor() {
    this.tick = 0;
    this.objects = [];
    this.phase = 'waiting';
    this.until = 0;
    this.winner = null;
    this.seats = Array(MAX_PLAYERS).fill(null);
    this.rockets = [];
    this.events = [];
    this.eventId = 0;
    // Items: transient pose and physics for objects that can be picked up.
    this.items = new Map();
    this.bullets = [];
    // Chalk: strokes of points in the air, per seat; sent when they change.
    this.chalk = Array.from({ length: MAX_PLAYERS }, () => []);
    this.chalkDirty = false;
    this.chalking = Array(MAX_PLAYERS).fill(false);
    // New points since the last broadcast, [seat, stroke, x, y, z]; a seat
    // whose old points were trimmed or cleared is re-sent whole instead.
    this.chalkOps = [];
    this.chalkWhole = new Set();
  }

  addChalk(seat, at) {
    const p = this.seats[seat]?.player;
    if (!p || Math.hypot(at.x - p.x, at.y - p.y, at.z - p.z) > 9) return;
    const strokes = this.chalk[seat];
    if (!this.chalking[seat] || !strokes.length) strokes.push([]);
    const stroke = strokes[strokes.length - 1];
    const last = stroke[stroke.length - 1];
    if (last && Math.hypot(last.x - at.x, last.y - at.y, last.z - at.z) < 0.08) return;
    const point = { x: Math.round(at.x * 100) / 100, y: Math.round(at.y * 100) / 100, z: Math.round(at.z * 100) / 100 };
    stroke.push(point);
    this.chalkOps.push([seat, strokes.length - 1, point.x, point.y, point.z]);
    // Keep the newest 800 points per player.
    let total = strokes.reduce((n, s) => n + s.length, 0);
    while (total > 800 && strokes.length) {
      strokes[0].shift();
      if (!strokes[0].length) strokes.shift();
      total--;
      this.chalkWhole.add(seat);
    }
    this.chalkDirty = true;
  }

  clearChalk(seat) {
    this.chalk[seat] = [];
    this.chalkWhole.add(seat);
    this.chalkDirty = true;
  }
  // Walking through chalk dusts it (2026-11-03): every point within reach of
  // the body column goes, strokes split where they were cut. Returns the
  // points that fell.
  dustChalk(x, z, r = 0.5) {
    const fallen = [];
    this.chalk = this.chalk.map((strokes, seat) => {
      const out = [];
      let cut = false;
      for (const stroke of strokes) {
        let run = [];
        for (const p of stroke) {
          const near = Math.hypot(p.x - x, p.z - z) < r && p.y >= 0 && p.y < 2.2;
          if (near) {
            fallen.push(p);
            cut = true;
            if (run.length > 1) out.push(run);
            run = [];
          } else run.push(p);
        }
        if (run.length > 1) out.push(run);
      }
      if (cut) {
        this.chalkWhole.add(seat);
        this.chalkDirty = true;
      }
      return cut ? out : strokes;
    });
    return fallen;
  }

  // What to send since last time: whole seats first, then point ops.
  takeChalk() {
    const whole = [...this.chalkWhole].map((seat) => ({ type: 'chalk', seat, strokes: this.chalk[seat] }));
    const ops = this.chalkOps.filter((op) => !this.chalkWhole.has(op[0]));
    this.chalkWhole.clear();
    this.chalkOps = [];
    this.chalkDirty = false;
    return ops.length ? [...whole, { type: 'chalk-ops', ops }] : whole;
  }

  // Solid objects collide; items are carried, thrown and run over instead.
  setObjects(list = []) {
    this.objects = list.filter((o) => !o.item);
    const ids = new Set();
    for (const o of list.filter((o) => o.item)) {
      ids.add(o.id);
      const it = this.items.get(o.id);
      if (it) Object.assign(it, { kind: o.item, effect: o.effect || null, home: { x: o.x, z: o.z } });
      else
        this.items.set(o.id, {
          id: o.id, kind: o.item, effect: o.effect || null, home: { x: o.x, z: o.z }, fuseAt: 0, awayUntil: 0,
          x: o.x, y: 0, z: o.z,
          vx: 0, vy: 0, vz: 0, spin: 0, holder: -1, thrownBy: -1, cool: {}, ready: 0,
        });
    }
    for (const id of this.items.keys()) if (!ids.has(id)) this.items.delete(id);
  }

  held(seat) {
    for (const it of this.items.values()) if (it.holder === seat) return it;
    return null;
  }

  // The hand: a little right of and ahead of the eye, below it.
  hand(p) {
    const f = direction(p.yaw, p.pitch),
      r = direction(p.yaw - 90);
    return {
      x: p.x + r.x * 0.3 + f.x * 0.45,
      y: p.y - 0.55 + f.y * 0.45,
      z: p.z + r.z * 0.3 + f.z * 0.45,
    };
  }

  release(it, p, speed, lift) {
    const f = direction(p.yaw, p.pitch),
      h = this.hand(p);
    Object.assign(it, {
      holder: -1,
      x: h.x, y: Math.max(0.05, h.y), z: h.z,
      vx: f.x * speed + (p.vx || 0) * 0.5,
      vy: f.y * speed + lift,
      vz: f.z * speed + (p.vz || 0) * 0.5,
      spin: speed ? 9 : 0,
    });
    it.cool[p.seat] = this.tick + HZ; // no instant re-pickup
  }

  useItem(s, c) {
    const p = s.player,
      it = this.held(p.seat);
    if (!it || !p.alive) return;
    if (c.drop) {
      this.release(it, p, 1.5, 1);
      this.emit('drop', { seat: p.seat, item: it.id });
      return;
    }
    if (!c.use || this.tick < it.ready) return;
    const f = direction(p.yaw, p.pitch);
    if (it.kind === 'shoot') {
      it.ready = this.tick + Math.round(HZ * 0.3);
      const h = this.hand(p);
      // Visible, dodgeable rounds; at most 24 in the air.
      this.bullets.push({ ...h, vx: f.x * 32, vy: f.y * 32, vz: f.z * 32, owner: p.seat, born: this.tick,
        safeUntil: this.tick + Math.round(HZ * 0.25), bounces: 0, id: ++this.eventId });
      while (this.bullets.length > 24) this.bullets.shift();
      this.emit('shot', { seat: p.seat, at: h });
    } else if (it.kind === 'throw') {
      this.release(it, p, 17, 3.5);
      it.thrownBy = p.seat;
      it.owner = p.seat;
      if (it.effect === 'explode') it.fuseAt = this.tick + Math.round(HZ * 1.6);
      this.emit('throw', { seat: p.seat, item: it.id });
    } else {
      // Poke: whoever is in front, within reach, takes it.
      it.ready = this.tick + Math.round(HZ * 0.45);
      this.emit('poke', { seat: p.seat });
      for (const o of this.seats.filter(Boolean)) {
        const q = o.player;
        if (q.seat === p.seat || !q.alive) continue;
        const dx = q.x - p.x, dz = q.z - p.z, d = Math.hypot(dx, dz);
        if (d > 2.2 || Math.abs(q.y - p.y) > 1.6) continue;
        if ((dx * f.x + dz * f.z) / Math.max(0.01, d) < 0.55) continue;
        this.hit(q, p.seat, 34, { x: dx / d, y: 0.4, z: dz / d }, 9, it.id);
      }
    }
  }

  hit(q, attacker, amount, dir, force, item) {
    q.vx += dir.x * force;
    q.vz += dir.z * force;
    q.vy += Math.max(0.3, dir.y) * force * 0.6;
    q.onGround = false;
    this.emit('hit', { seat: q.seat, attacker, item, at: { x: q.x, y: q.y - 0.5, z: q.z } });
    this.damage(q.seat, amount, attacker);
  }

  // An explosive goes off where it is, then reappears at home a little later.
  detonate(it) {
    this.explode({ x: it.x, y: it.y + 0.2, z: it.z, owner: it.owner ?? -1 }, -1);
    Object.assign(it, { fuseAt: 0, thrownBy: -1, holder: -1, vx: 0, vy: 0, vz: 0, spin: 0 });
    it.awayUntil = this.tick + 3 * HZ;
    Object.assign(it, { x: it.home.x, y: -50, z: it.home.z });
  }

  stepItems() {
    const G = 22, time = this.tick / HZ;
    for (const it of this.items.values()) {
      if (it.awayUntil) {
        if (this.tick < it.awayUntil) continue;
        Object.assign(it, { awayUntil: 0, y: 0 });
      }
      if (it.fuseAt && this.tick >= it.fuseAt) {
        this.detonate(it);
        continue;
      }
      if (it.holder >= 0) {
        const p = this.seats[it.holder]?.player;
        if (!p || !p.alive) {
          // A holder who leaves or falls drops what they held.
          if (p) this.release(it, p, 0, 0);
          else it.holder = -1;
          continue;
        }
        Object.assign(it, this.hand(p), { vx: 0, vy: 0, vz: 0 });
        continue;
      }
      // Ballistics: gravity, walls and solid objects, a bouncy floor.
      it.vy -= G * DT;
      const speed = Math.hypot(it.vx, it.vy, it.vz);
      if (speed > 0.01) {
        // Sub-stepped swept motion; each contact reflects about the face it
        // met, keeping part of the bounce and most of the slide.
        const steps = Math.max(1, Math.ceil((speed * DT) / 0.2));
        for (let k = 0; k < steps; k++) {
          const sp = Math.hypot(it.vx, it.vy, it.vz);
          if (sp < 0.01) break;
          const dir = { x: it.vx / sp, y: it.vy / sp, z: it.vz / sp },
            travel = (sp * DT) / steps;
          const hit = surfaceHit(it, dir, travel, this.objects, time);
          if (hit) {
            Object.assign(it, along(it, dir, Math.max(0, hit.t - 0.02)));
            const v = reflect({ x: it.vx, y: it.vy, z: it.vz }, hit.normal, 0.42, 0.85);
            Object.assign(it, { vx: v.x, vy: v.y, vz: v.z });
            it.spin = (it.spin || 0) * 0.7 + sp * 0.4;
            if (sp > 3) this.emit('bounce', { item: it.id, at: { x: it.x, y: it.y, z: it.z } });
          } else Object.assign(it, along(it, dir, travel));
        }
      }
      if (it.y <= 0) {
        it.y = 0;
        if (it.vy < -2.5) {
          it.vy *= -0.38;
          this.emit('bounce', { item: it.id, at: { x: it.x, y: 0, z: it.z } });
        } else it.vy = 0;
        it.vx *= 0.82;
        it.vz *= 0.82;
        it.spin *= 0.8;
        if (Math.hypot(it.vx, it.vz) < 0.2) it.vx = it.vz = 0;
      }
      it.x = Math.max(-WORLD + 0.5, Math.min(WORLD - 0.5, it.x));
      it.z = Math.max(-WORLD + 0.5, Math.min(WORLD - 0.5, it.z));
      // A thrown item hits the first other player it reaches while fast.
      if (it.thrownBy >= 0) {
        if (Math.hypot(it.vx, it.vy, it.vz) < 5) it.thrownBy = -1;
        else
          for (const o of this.seats.filter(Boolean)) {
            const q = o.player;
            if (q.seat === it.thrownBy || !q.alive) continue;
            const b = bodyBox(q);
            if (it.x > b.xMin - 0.2 && it.x < b.xMax + 0.2 && it.z > b.zMin - 0.2 && it.z < b.zMax + 0.2 && it.y > b.yMin && it.y < b.yMax + 0.2) {
              const d = Math.hypot(it.vx, it.vz) || 1;
              if (it.effect === 'explode') {
                this.detonate(it);
                break;
              }
              this.hit(q, it.thrownBy, 30, { x: it.vx / d, y: 0.3, z: it.vz / d }, 7, it.id);
              it.vx *= -0.3;
              it.vz *= -0.3;
              it.thrownBy = -1;
              break;
            }
          }
      }
      // Run over it to pick it up, one item per hand (not while its fuse burns).
      if (!it.fuseAt && it.y < 1.2 && Math.hypot(it.vx, it.vy, it.vz) < 6)
        for (const o of this.seats.filter(Boolean)) {
          const q = o.player;
          if (!q.alive || this.held(q.seat) || this.tick < (it.cool[q.seat] || 0)) continue;
          if (Math.hypot(q.x - it.x, q.z - it.z) < 1.1) {
            it.holder = q.seat;
            it.thrownBy = -1;
            this.emit('pickup', { seat: q.seat, item: it.id });
            break;
          }
        }
    }
  }

  // Bullets after oskiewar: no clock on a round, it ricochets off every
  // surface until it meets a body (or another round, which cancels both),
  // sub-stepped so it never tunnels; the shooter is safe from it only for a
  // moment. Headshots kill.
  stepBullets() {
    const time = this.tick / HZ;
    for (const b of this.bullets) {
      b.px = b.x;
      b.py = b.y;
      b.pz = b.z;
      const speed = Math.hypot(b.vx, b.vy, b.vz);
      const steps = Math.max(1, Math.ceil((speed * DT) / 0.25));
      for (let k = 0; k < steps && !b.dead; k++) {
        const dir = { x: b.vx / speed, y: b.vy / speed, z: b.vz / speed },
          travel = (speed * DT) / steps;
        let body = null, at = travel;
        for (const s of this.seats.filter(Boolean)) {
          const q = s.player;
          if (!q.alive || (q.seat === b.owner && this.tick < b.safeUntil)) continue;
          const d = rayBox(b, dir, bodyBox(q), travel);
          if (d < at) {
            at = d;
            body = q;
          }
        }
        const surface = surfaceHit(b, dir, at, this.objects, time);
        if (surface && surface.t <= at) {
          Object.assign(b, along(b, dir, Math.max(0, surface.t - 0.02)));
          const v = reflect({ x: b.vx, y: b.vy, z: b.vz }, surface.normal);
          Object.assign(b, { vx: v.x, vy: v.y, vz: v.z });
          b.bounces++;
          this.emit('ricochet', { at: { x: b.x, y: b.y, z: b.z } });
          continue;
        }
        Object.assign(b, along(b, dir, at));
        if (body) {
          const head = b.y > body.y - 0.25;
          this.hit(body, b.owner, head ? 100 : 34, dir, head ? 6 : 4, null);
          if (head) this.emit('headshot', { seat: body.seat, attacker: b.owner });
          b.dead = true;
        }
      }
      if (b.bounces > 16 || Math.abs(b.x) > WORLD + 5 || Math.abs(b.z) > WORLD + 5 || b.y > 80) b.dead = true;
    }
    // Rounds from different players that meet in the air cancel each other.
    for (let a = 0; a < this.bullets.length; a++)
      for (let c = a + 1; c < this.bullets.length; c++) {
        const p = this.bullets[a], q = this.bullets[c];
        if (p.dead || q.dead || p.owner === q.owner) continue;
        // Closest approach during this tick, so fast rounds can't pass through each other.
        const r0 = [p.px - q.px, p.py - q.py, p.pz - q.pz],
          dr = [p.x - q.x - r0[0], p.y - q.y - r0[1], p.z - q.z - r0[2]];
        const dd = dr[0] * dr[0] + dr[1] * dr[1] + dr[2] * dr[2];
        const u = dd ? Math.max(0, Math.min(1, -(r0[0] * dr[0] + r0[1] * dr[1] + r0[2] * dr[2]) / dd)) : 0;
        if (Math.hypot(r0[0] + dr[0] * u, r0[1] + dr[1] * u, r0[2] + dr[2] * u) < 0.6) {
          p.dead = q.dead = true;
          this.emit('cancel', { at: { x: (p.x + q.x) / 2, y: (p.y + q.y) / 2, z: (p.z + q.z) / 2 } });
        }
      }
    this.bullets = this.bullets.filter((b) => !b.dead);
  }

  get count() {
    return this.seats.filter(Boolean).length;
  }

  // Many players share one room. The second arrival starts a match; anyone
  // after that drops into the match already running instead of resetting it.
  join() {
    const seat = this.seats.indexOf(null);
    if (seat < 0) return -1;
    this.seats[seat] = { player: spawn(seat), ack: 0, queue: [], last: {} };
    if (this.count === 2 && this.phase === 'waiting') this.reset();
    return seat;
  }

  // A departure only stops the match when it leaves one player alone.
  leave(seat) {
    this.seats[seat] = null;
    this.rockets = this.rockets.filter((r) => r.owner !== seat);
    for (const it of this.items.values()) if (it.holder === seat) it.holder = -1;
    if (this.count >= 2) return;
    this.phase = 'waiting';
    this.winner = null;
    this.rockets = [];
    for (const s of this.seats.filter(Boolean)) {
      s.player = spawn(s.player.seat, s.player.life + 1);
      s.queue = [];
    }
  }

  reset() {
    this.phase = 'countdown';
    this.until = this.tick + 3 * HZ;
    this.winner = null;
    this.rockets = [];
    for (const s of this.seats.filter(Boolean)) {
      s.player = spawn(s.player.seat, s.player.life + 1);
      // Consume already received input so it cannot fire in the new round.
      s.ack = Math.max(s.ack, ...s.queue.map((c) => c.seq));
      s.queue = [];
      s.last = {};
    }
  }

  input(seat, commands) {
    const s = this.seats[seat];
    if (!s || !Array.isArray(commands) || commands.length > 6) return;
    for (const raw of commands) {
      const c = command(raw);
      if (
        !c ||
        c.seq <= s.ack ||
        c.seq > s.ack + 120 ||
        s.queue.length >= 12 ||
        s.queue.some((q) => q.seq === c.seq)
      )
        continue;
      s.queue.push(c);
    }
    s.queue.sort((a, b) => a.seq - b.seq);
  }

  emit(type, fields) {
    this.events.push({ id: ++this.eventId, tick: this.tick, type, ...fields });
    this.events = this.events.slice(-32);
  }

  step() {
    this.tick++;
    if (this.phase === 'finished' && this.tick >= this.until) this.reset();
    if (this.phase === 'countdown' && this.tick >= this.until) this.phase = 'playing';
    const shots = [];
    for (const s of this.seats.filter(Boolean)) {
      // Drain excess backlog without granting extra movement or weapon time.
      while (s.queue.length > 6) s.ack = s.queue.shift().seq;
      const c = s.queue.shift();
      if (c) {
        s.ack = c.seq;
        s.last = c;
      }
      const input = c || { yaw: s.last.yaw, pitch: s.last.pitch, buttons: 0 };
      let p = s.player;
      if (!p.alive && this.phase === 'playing' && this.tick >= p.respawnAt) {
        p = { ...spawn(p.seat, p.life + 1), score: p.score };
        s.last = {};
      }
      p.cooldown = Math.max(0, p.cooldown - 1);
      // Waiting is a movement warmup; the match countdown locks both spawns.
      if (this.phase === 'playing' || this.phase === 'waiting')
        p = move(p, input, this.objects, this.tick / HZ);
      else if (c) {
        p.yaw = c.yaw;
        p.pitch = c.pitch;
      }
      s.player = p;
      if (c && (c.use || c.drop)) this.useItem(s, c);
      if (c?.chalk) this.addChalk(p.seat, c.chalk);
      if (c) this.chalking[p.seat] = !!c.chalk;
      if (
        c?.fire &&
        p.alive &&
        !p.cooldown &&
        (this.phase === 'playing' || this.phase === 'waiting')
      )
        shots.push(s);
    }
    for (const s of shots) this.shoot(s);
    if (this.phase === 'playing' || this.phase === 'waiting') {
      this.stepRockets();
      this.stepItems();
      this.stepBullets();
    }
  }

  shoot(s) {
    const p = s.player,
      dir = direction(p.yaw, p.pitch);
    p.cooldown = FIRE_COOLDOWN;
    this.rockets.push({
      ...along(p, dir, 0.05),
      dir,
      owner: p.seat,
      born: this.tick,
      id: ++this.eventId,
    });
    this.emit('launch', { seat: p.seat });
  }

  damage(seat, amount, attacker) {
    const p = this.seats[seat]?.player;
    if (!p?.alive || this.phase !== 'playing') return;
    p.health = Math.max(0, p.health - amount);
    if (p.health) return;
    p.alive = false;
    p.respawnAt = this.tick + HZ;
    const killer = this.seats[attacker]?.player;
    if (killer) killer.score += attacker === seat ? -1 : 1;
    this.emit('frag', { seat, attacker });
    if (killer?.score >= FRAG_LIMIT) {
      this.phase = 'finished';
      this.winner = attacker;
      this.until = this.tick + 5 * HZ;
      this.rockets = [];
    }
  }

  explode(r, direct) {
    this.emit('blast', { at: { x: r.x, y: r.y, z: r.z }, seat: r.owner });
    for (const s of this.seats.filter(Boolean)) {
      const p = s.player;
      if (!p.alive) continue;
      const dx = p.x - r.x,
        dy = p.y - 0.7 - r.y,
        dz = p.z - r.z;
      const distance = Math.hypot(dx, dy, dz);
      if (distance >= 4 && p.seat !== direct) continue;
      const dir =
        distance > 0.001
          ? { x: dx / distance, y: dy / distance, z: dz / distance }
          : { x: 0, y: 1, z: 0 };
      if (
        p.seat !== direct &&
        wallDistance(r, dir, distance, this.objects, this.tick / HZ) < distance - 0.05
      )
        continue;
      const force = Math.max(0, 1 - distance / 4);
      p.vx += dir.x * force * 15;
      p.vz += dir.z * force * 15;
      p.vy += Math.max(0.3, dir.y) * force * 15;
      p.onGround = false;
      this.damage(p.seat, p.seat === direct ? 100 : Math.ceil(80 * force), r.owner);
    }
  }

  stepRockets() {
    const remaining = [];
    for (const r of this.rockets) {
      if (this.phase !== 'playing' && this.phase !== 'waiting') break;
      const travel = 28 * DT;
      let distance = wallDistance(r, r.dir, travel, this.objects, this.tick / HZ),
        direct = -1;
      for (const s of this.seats.filter(Boolean)) {
        const p = s.player;
        if (!p.alive || p.seat === r.owner) continue;
        const d = rayBox(r, r.dir, bodyBox(p), travel);
        if (d < distance) {
          distance = d;
          direct = p.seat;
        }
      }
      // Keep explosions just outside the wall so cover occlusion works from the impact.
      const next = along(r, r.dir, Math.max(0, distance - (distance < travel ? 0.01 : 0)));
      Object.assign(r, next);
      if (distance < travel || this.tick - r.born > 3 * HZ) this.explode(r, direct);
      else remaining.push(r);
    }
    this.rockets = this.phase === 'playing' || this.phase === 'waiting' ? remaining : [];
  }

  snapshot() {
    return {
      type: 'snapshot',
      tick: this.tick,
      epoch: Date.now(),
      phase: this.phase,
      until: this.until,
      winner: this.winner,
      players: this.seats.map((s) => s && { ...s.player }),
      acks: this.seats.map((s) => s?.ack || 0),
      rockets: this.rockets.map((r) => ({ ...r })),
      items: [...this.items.values()].map((it) => ({
        id: it.id, x: it.x, y: it.y, z: it.z, holder: it.holder, spin: it.spin,
      })),
      bullets: this.bullets.map((b) => ({ id: b.id, x: b.x, y: b.y, z: b.z, vx: b.vx, vy: b.vy, vz: b.vz })),
      events: this.events.map((e) => ({ ...e })),
    };
  }
}
