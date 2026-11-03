import {
  HZ,
  DT,
  FRAG_LIMIT,
  FIRE_COOLDOWN,
  spawn,
  move,
  direction,
  rayBox,
  wallDistance,
  bodyBox,
  along,
} from './world.mjs';
import { command } from './net.mjs';

export class Match {
  constructor() {
    this.tick = 0;
    this.objects = [];
    this.phase = 'waiting';
    this.until = 0;
    this.winner = null;
    this.seats = [null, null];
    this.rockets = [];
    this.events = [];
    this.eventId = 0;
    // Items: transient pose and physics for objects that can be picked up.
    this.items = new Map();
    this.bullets = [];
  }

  // Solid objects collide; items are carried, thrown and run over instead.
  setObjects(list = []) {
    this.objects = list.filter((o) => !o.item);
    const ids = new Set();
    for (const o of list.filter((o) => o.item)) {
      ids.add(o.id);
      const it = this.items.get(o.id);
      if (it) it.kind = o.item;
      else
        this.items.set(o.id, {
          id: o.id, kind: o.item, x: o.x, y: 0, z: o.z,
          vx: 0, vy: 0, vz: 0, spin: 0, holder: -1, thrownBy: -1, cool: [0, 0], ready: 0,
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
      this.bullets.push({ ...h, vx: f.x * 70, vy: f.y * 70, vz: f.z * 70, owner: p.seat, born: this.tick, id: ++this.eventId });
      this.emit('shot', { seat: p.seat, at: h });
    } else if (it.kind === 'throw') {
      this.release(it, p, 17, 3.5);
      it.thrownBy = p.seat;
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

  stepItems() {
    const G = 22, time = this.tick / HZ;
    for (const it of this.items.values()) {
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
        const dir = { x: it.vx / speed, y: it.vy / speed, z: it.vz / speed };
        const travel = speed * DT;
        const block = wallDistance(it, dir, travel, this.objects, time);
        if (block < travel) {
          Object.assign(it, along(it, dir, Math.max(0, block - 0.05)));
          // Reflect the dominant horizontal axis, lose energy.
          if (Math.abs(dir.x) > Math.abs(dir.z)) it.vx *= -0.45;
          else it.vz *= -0.45;
          it.vy *= 0.6;
        } else Object.assign(it, along(it, dir, travel));
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
      it.x = Math.max(-18, Math.min(18, it.x));
      it.z = Math.max(-18, Math.min(18, it.z));
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
              this.hit(q, it.thrownBy, 30, { x: it.vx / d, y: 0.3, z: it.vz / d }, 7, it.id);
              it.vx *= -0.3;
              it.vz *= -0.3;
              it.thrownBy = -1;
              break;
            }
          }
      }
      // Run over it to pick it up, one item per hand.
      if (it.y < 1.2 && Math.hypot(it.vx, it.vy, it.vz) < 6)
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

  stepBullets() {
    const remaining = [], time = this.tick / HZ;
    for (const b of this.bullets) {
      b.vy -= 6 * DT; // a little drop, so long shots arc
      const speed = Math.hypot(b.vx, b.vy, b.vz),
        dir = { x: b.vx / speed, y: b.vy / speed, z: b.vz / speed },
        travel = speed * DT;
      let distance = wallDistance(b, dir, travel, this.objects, time),
        target = null;
      for (const s of this.seats.filter(Boolean)) {
        const q = s.player;
        if (!q.alive || q.seat === b.owner) continue;
        const d = rayBox(b, dir, bodyBox(q), travel);
        if (d < distance) {
          distance = d;
          target = q;
        }
      }
      Object.assign(b, along(b, dir, Math.min(distance, travel)));
      if (target) this.hit(target, b.owner, 25, dir, 4, null);
      else if (distance < travel || b.y < 0) this.emit('spark', { at: { x: b.x, y: Math.max(0, b.y), z: b.z } });
      if (!target && distance >= travel && b.y >= 0 && this.tick - b.born < 1.5 * HZ) remaining.push(b);
    }
    this.bullets = remaining;
  }

  join() {
    const seat = this.seats.indexOf(null);
    if (seat < 0) return -1;
    this.seats[seat] = { player: spawn(seat), ack: 0, queue: [], last: {} };
    if (this.seats.every(Boolean)) this.reset();
    return seat;
  }

  leave(seat) {
    this.seats[seat] = null;
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
      bullets: this.bullets.map((b) => ({ x: b.x, y: b.y, z: b.z, vx: b.vx, vy: b.vy, vz: b.vz })),
      events: this.events.map((e) => ({ ...e })),
    };
  }
}
