import { HZ, DT, FRAG_LIMIT, FIRE_COOLDOWN, spawn, move,
  direction, rayBox, wallDistance, bodyBox, along } from './world.mjs';
import { command } from './net.mjs';

export class Match {
  constructor() {
    this.tick = 0;
    this.phase = 'waiting';
    this.until = 0;
    this.winner = null;
    this.seats = [null, null];
    this.rockets = [];
    this.events = [];
    this.eventId = 0;
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
      s.ack = Math.max(s.ack, ...s.queue.map(c => c.seq));
      s.queue = [];
      s.last = {};
    }
  }

  input(seat, commands) {
    const s = this.seats[seat];
    if (!s || !Array.isArray(commands) || commands.length > 6) return;
    for (const raw of commands) {
      const c = command(raw);
      if (!c || c.seq <= s.ack || c.seq > s.ack + 120 || s.queue.length >= 12 ||
          s.queue.some(q => q.seq === c.seq)) continue;
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
      if (c) { s.ack = c.seq; s.last = c; }
      const input = c || { yaw: s.last.yaw, pitch: s.last.pitch, buttons: 0 };
      let p = s.player;
      if (!p.alive && this.phase === 'playing' && this.tick >= p.respawnAt) {
        p = { ...spawn(p.seat, p.life + 1), score: p.score };
        s.last = {};
      }
      p.cooldown = Math.max(0, p.cooldown - 1);
      // Waiting is a movement warmup; the match countdown locks both spawns.
      if (this.phase === 'playing' || this.phase === 'waiting') p = move(p, input);
      else if (c) { p.yaw = c.yaw; p.pitch = c.pitch; }
      s.player = p;
      if (c?.fire && p.alive && !p.cooldown && (this.phase === 'playing' || this.phase === 'waiting')) shots.push(s);
    }
    for (const s of shots) this.shoot(s);
    if (this.phase === 'playing' || this.phase === 'waiting') this.stepRockets();
  }

  shoot(s) {
    const p = s.player, dir = direction(p.yaw, p.pitch);
    p.cooldown = FIRE_COOLDOWN;
    this.rockets.push({ ...along(p, dir, 0.05), dir, owner: p.seat, born: this.tick,
      id: ++this.eventId });
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
      const dx = p.x - r.x, dy = p.y - 0.7 - r.y, dz = p.z - r.z;
      const distance = Math.hypot(dx, dy, dz);
      if (distance >= 4 && p.seat !== direct) continue;
      const dir = distance > 0.001 ? { x: dx / distance, y: dy / distance, z: dz / distance } : { x: 0, y: 1, z: 0 };
      if (p.seat !== direct && wallDistance(r, dir, distance) < distance - 0.05) continue;
      const force = Math.max(0, 1 - distance / 4);
      p.vx += dir.x * force * 15; p.vz += dir.z * force * 15;
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
      let distance = wallDistance(r, r.dir, travel), direct = -1;
      for (const s of this.seats.filter(Boolean)) {
        const p = s.player;
        if (!p.alive || p.seat === r.owner) continue;
        const d = rayBox(r, r.dir, bodyBox(p), travel);
        if (d < distance) { distance = d; direct = p.seat; }
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
    return { type: 'snapshot', tick: this.tick, phase: this.phase,
      until: this.until, winner: this.winner,
      players: this.seats.map(s => s && { ...s.player }),
      acks: this.seats.map(s => s?.ack || 0),
      rockets: this.rockets.map(r => ({ ...r })), events: this.events.map(e => ({ ...e })) };
  }
}
