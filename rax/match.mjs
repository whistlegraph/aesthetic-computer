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
 HORIZON, FALL_Y, muzzleOf, muzzleAt } from './world.mjs';
import { command } from './net.mjs';
import { objectBoxes } from './objects.mjs';
import { RACK_RETURN_SECONDS } from './armory.mjs';
import { BOP, DRONE, lands, landsOnDrone, arenaSpawn, drones, stepDrone } from './bop.mjs';

// Input from a real network arrives late and in bursts. A batch carries the
// newest commands again so a dropped message costs nothing; the queue holds
// what a burst can bring; the grace is how long a silent player is held
// still and owed their time before the body runs on without them.
// (INPUT_SLACK: how many may wait once the budget is spent.)
// 🔫 Rounds in a gun. The last one fired, it is dropped and breaks.
export const CLIP = 12;
export const STAINS = 160; // 🩸 the most stains a floor keeps
// 👹 A flying head: its size, its speed (and the fastest a late wave flies), how near it must come to bite, how far outside the room it comes from, the seconds between two of a wave and between waves, how many the first wave has less one, and how long a bitten body lies before it stands again.
export const HEAD = { r: 0.42, speed: 3.2, fastest: 7, bite: 0.75, from: 14, gap: 0.9, rest: 4, first: 1, down: 3 };
export const INPUT_BATCH = 12, INPUT_QUEUE = HORIZON, INPUT_GRACE = HORIZON, INPUT_SLACK = 3;
// 🩸 Where on a body a round struck (gore.mjs takes it apart there): the
// head, an arm or leg by side, or the torso. Heights from the feet, as the
// body is drawn (view.mjs fighters): legs to 0.68, shoulders at 1.27, the
// head from 1.38; the arms hang 0.3 to either side.
export function hitZone(p, at) {
  const feet = p.y - 1.7, h = at.y - feet;
  const yaw = (p.yaw * Math.PI) / 180, side = (at.x - p.x) * Math.cos(yaw) - (at.z - p.z) * Math.sin(yaw); // (+ is the body's left: direction(yaw + 90))
  const which = side >= 0 ? 'left' : 'right';
  if (h >= 1.38) return 'head';
  if (h < 0.68) return `leg-${which}`;
  if (h > 0.8 && Math.abs(side) > 0.22) return `arm-${which}`;
  return 'torso';
}

export class Match {
  constructor() {
    this.tick = 0;
    this.objects = [];
    this.walls = [];
    this.open = null; // The sides that lead to a neighbour (world.mjs move).
    this.half = WORLD; // The room's half-width (world.mjs halfOf); the server sets it.
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
    // 🍄 botbop's arena (setArena): its ledges, or null in any other room.
    this.arena = null;
    this.drones = [];
    // 🎯 Things built to be hit (a scarecrow, a dummy, a zombie): they
    // stand, take every hit and say how much (setObjects).
    this.targets = [];
    // 🚁 Those looking on from their drones, [{ eye, x, y, z }], set each
    // tick by the server: a round that meets one blows it up.
    this.watchers = [];
    this.zombies = []; // 🧟 the horde's walkers standing here, set each tick (horde.mjs inRoom)
    this.zombieKills = [];
    // 👹 Heads (stepHeads): on where `waves` is set. They fly in from outside the room in waves and make for the nearest body; a round ends one, and one that reaches a body ends the body, which comes apart.
    this.waves = false;
    this.heads = [];
    this.wave = 0; // the wave now flying (0: none yet)
    this.headsOwed = 0; // still to come in this wave
    this.headAt = 0; // the tick the next one comes, or the next wave starts
    this.headId = 0;
    this.random = Math.random;
    // 🩸 Stains: the roof's own record of what was spilled on it, the same for everyone there. Each [x, z, size, shade]; the oldest go as new ones come (STAINS at the most). Told as they are made (a `stain` event) and whole now and then in the snapshot, so whoever comes later sees the floor as it is.
    // 🕰️ Waves on the bell: with `bellMs` set (the stage's clock: world.mjs BELL_MS) a wave starts only as the clock strikes, by `now` (the server's own clock; a test's).
    this.bellMs = 0;
    this.now = Date.now;
    this.bellWas = null;
    this.stains = [];
    this.stainsNew = false; // …and the ids a round has met, for the server to take off the horde
    // 🏙️ Instagib: a body a round meets comes apart, and comes back on the
    // tower's roof. The server says whether there is a roof to go to
    // (roofBound) and takes the seats queued here up to it after the tick.
    this.roofBound = false;
    this.toRoof = [];
    // 🎒 Carried-in things left lying long enough to go home (server.mjs takes them).
    this.goingHome = [];
    // 🔫 Guns that fired their last round, fell and broke: the server takes each out of the room (it is in what was removed, to be put back).
    this.broken = [];
    // ☠️ Bodies that died this tick, [{ seat, attacker }]: the server counts each against its handle (deaths.mjs).
    this.deaths = [];
    // 🌐 Rounds that flew out over an open side, for the room beside (server.mjs).
    this.outbound = [];
    // 🎯 Kills made here by rounds from other rooms, owed to their shooters there.
    this.credits = [];
    this.nameOf = null; // (seat → who fired, for a round that leaves: server.mjs sets it)
  }

  // 🍄 A side-on arena: bodies keep to one plane, the ledges are the map,
  // and play never stops for a countdown or a round.
  setArena(ledges) {
    this.arena = ledges;
    this.walls = ledges;
    this.drones = drones(ledges);
    this.phase = 'playing';
  }
  // A fresh body for a seat, set down where this room sets bodies down.
  fresh(seat, life = 1) {
    const p = spawn(seat, life, this.half);
    // A room with fixed things in it (the tower's car) says where bodies land.
    if (this.spawnAt) Object.assign(p, this.spawnAt(seat));
    return this.arena ? Object.assign(p, arenaSpawn(seat, life)) : p;
  }
  // 🔗 One more bop in the air: the k-th of a chain is worth k.
  chain(s) {
    s.combo = (s.combo || 0) + 1;
    s.pending = (s.pending || 0) + s.combo;
  }
  // 🍄 Whoever `s` is coming down on is bopped: the stomper bounces and
  // counts it. In the arena the bopped body is gone for a second; anywhere
  // else it only feels it.
  bop(s) {
    const a = s.player;
    // 🔗 Touching down ends the chain and banks what it earned; a body that
    // is bopped out of the air loses it.
    if (s.pending && (a.onGround || !a.alive)) {
      if (a.alive) {
        s.bops = (s.bops || 0) + s.pending;
        s.best = Math.max(s.best || 0, s.combo);
        this.emit('bank', { seat: a.seat, points: s.pending, combo: s.combo, count: s.bops });
      }
      s.pending = s.combo = 0;
    }
    if (!a.alive || a.onGround) return;
    // 🤖 A drone under the feet is a bop too.
    for (const d of this.drones) {
      if (!landsOnDrone(a, d)) continue;
      a.vy = BOP.bounce;
      a.onGround = false;
      this.chain(s);
      d.alive = false;
      d.backAt = this.tick + DRONE.away;
      this.emit('bop', { drone: d.id, attacker: a.seat, combo: s.combo, points: s.pending, at: { x: d.x, y: d.y + DRONE.tall, z: 0 } });
      return;
    }
    for (const o of this.seats) {
      if (!o || o === s || this.tick < (o.bopRest || 0) || !lands(a, o.player)) continue;
      const q = o.player;
      a.vy = BOP.bounce;
      a.onGround = false;
      this.chain(s);
      o.bopRest = this.tick + BOP.rest;
      this.emit('bop', { seat: q.seat, attacker: a.seat, combo: s.combo, points: s.pending, at: { x: q.x, y: q.y + 0.2, z: q.z } });
      if (this.arena) {
        q.alive = false;
        q.respawnAt = this.tick + HZ;
        o.pending = o.combo = 0; // bopped out of the air: the chain is lost
      }
      return;
    }
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
  // The place's own solid boxes: a block's house walls.
  setWalls(boxes = []) {
    this.walls = this.arena ? [...this.arena, ...boxes] : boxes;
    this.slab = boxes.find((b) => b.slab) || null; // 🧱 a platform's block (world.mjs slabOf): rounds meet it and nothing round it
  }
  setObjects(list = []) {
    this.objects = list.filter((o) => !o.item);
    // One box around each target's parts: what a round, a poke or a throw meets.
    this.targets = list.filter((o) => o.target && !o.item).map((o) => {
      const boxes = objectBoxes([o], 0);
      if (!boxes.length) return null;
      const box = { xMin: Infinity, yMin: Infinity, zMin: Infinity, xMax: -Infinity, yMax: -Infinity, zMax: -Infinity };
      for (const b of boxes) for (const k of ['x', 'y', 'z']) {
        box[k + 'Min'] = Math.min(box[k + 'Min'], b[k + 'Min']);
        box[k + 'Max'] = Math.max(box[k + 'Max'], b[k + 'Max']);
      }
      const was = this.targets?.find((t) => t.id === o.id);
      return { id: o.id, box, total: was?.total || 0 };
    }).filter(Boolean);
    const ids = new Set();
    for (const o of list.filter((o) => o.item)) {
      ids.add(o.id);
      const it = this.items.get(o.id);
      if (it) Object.assign(it, { kind: o.item, effect: o.effect || null, home: { x: o.x, z: o.z }, house: !!o.house, carried: !!o.carried, muzzle: o.item === 'shoot' ? muzzleOf(o.geometry) : null });
      else
        this.items.set(o.id, {
          muzzle: o.item === 'shoot' ? muzzleOf(o.geometry) : null, ammo: o.item === 'shoot' ? CLIP : 0, spent: false,
          id: o.id, kind: o.item, effect: o.effect || null, home: { x: o.x, z: o.z }, house: !!o.house, carried: !!o.carried, fuseAt: 0, awayUntil: 0,
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
      const h = muzzleAt(p, it.muzzle); // (out of the barrel's end, where the model shows its hole)
      // 🎯 At a head the round goes where the crosshair is, not beside it: from the barrel to the head the eye's own line meets. It used to fly level with the aim from the hand, half a metre low and to the right, and so passed under a head aimed at dead centre at any range (a body is tall enough not to notice).
      const g = this.aimFrom(p, f, h);
      // Visible, dodgeable rounds; at most 24 in the air.
      this.bullets.push({ ...h, vx: g.x * 32, vy: g.y * 32, vz: g.z * 32, owner: p.seat, born: this.tick,
        safeUntil: this.tick + Math.round(HZ * 0.25), bounces: 0, id: ++this.eventId });
      while (this.bullets.length > 24) this.bullets.shift();
      this.emit('shot', { seat: p.seat, at: h });
      // 🔫 The last round: the gun leaves the hand, and breaks where it lands (stepItems).
      if (--it.ammo <= 0) {
        it.spent = true;
        this.release(it, p, 1.5, 1);
        this.emit('drop', { seat: p.seat, item: it.id });
      }
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
      for (const t of this.targets) {
        const c = this.targetCentre(t), dx = c.x - p.x, dz = c.z - p.z;
        const reach = Math.max(t.box.xMax - t.box.xMin, t.box.zMax - t.box.zMin) / 2 + 1.6, d = Math.hypot(dx, dz);
        if (d > reach || (dx * f.x + dz * f.z) / Math.max(0.01, d) < 0.4) continue;
        this.strike(t, p.seat, 34, { x: c.x, y: Math.min(t.box.yMax, p.y - 0.3), z: c.z });
      }
    }
  }

  // The way from the barrel's end `h` to the head the eye at `p`, looking along `f`, has in its crosshair; with no head there, `f` as it always was (level with the aim, from the hand: what the room's other shooting is tuned to).
  aimFrom(p, f, h) {
    let reach = Infinity;
    for (const z of this.heads) {
      const d = rayBox(p, f, { xMin: z.x - HEAD.r, xMax: z.x + HEAD.r, yMin: z.y - HEAD.r, yMax: z.y + HEAD.r, zMin: z.z - HEAD.r, zMax: z.z + HEAD.r }, 80);
      if (d < reach) reach = d;
    }
    if (!(reach < 80)) return f;
    reach += 0.05; // (to the very point of the head's face the crosshair is on, a hair in: where it is struck is the shooter's doing)
    const dx = p.x + f.x * reach - h.x, dy = p.y + f.y * reach - h.y, dz = p.z + f.z * reach - h.z, n = Math.hypot(dx, dy, dz);
    return n > 0.2 ? { x: dx / n, y: dy / n, z: dz / n } : f;
  }

  // 🩸 Blood come down on the floor at (x, z), thrown on a little the way `dir` went: a main stain and a few small ones round it. Only where there is floor (the roof, in a roof room).
  stain(x, z, size = 1, dir = null) {
    const lay = (sx, sz, s) => {
      if (Math.abs(sx) > this.half - 0.2 || Math.abs(sz) > this.half - 0.2) return;
      const row = [Math.round(sx * 100) / 100, Math.round(sz * 100) / 100, Math.round(s * 100) / 100, Math.floor(this.random() * 3)];
      this.stains.push(row);
      if (this.stains.length > STAINS) this.stains.shift();
      this.emit('stain', { stain: row });
      this.stainsNew = true;
    };
    const dx = dir?.x || 0, dz = dir?.z || 0;
    lay(x + dx * 0.6, z + dz * 0.6, size);
    for (let k = 0; k < 4; k++) lay(x + dx * (1 + this.random() * 2) + (this.random() - 0.5) * 1.6, z + dz * (1 + this.random() * 2) + (this.random() - 0.5) * 1.6, size * (0.25 + this.random() * 0.35));
  }

  // 🎯 A target takes a hit: nothing moves it and nothing ends it; it only
  // says how much, where it was struck (the page pops the number).
  strike(t, attacker, amount, at) {
    t.total += amount;
    const top = t.box.yMax;
    this.emit('damage', { target: t.id, attacker, amount, crit: amount >= 100,
      at: { x: at?.x ?? (t.box.xMin + t.box.xMax) / 2, y: Math.min(top, (at?.y ?? top) + 0.2), z: at?.z ?? (t.box.zMin + t.box.zMax) / 2 } });
  }
  targetCentre(t) {
    return { x: (t.box.xMin + t.box.xMax) / 2, y: (t.box.yMin + t.box.yMax) / 2, z: (t.box.zMin + t.box.zMax) / 2 };
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
      // 🔫 A house gun (armory.mjs) left lying away from its slot goes back to
      // it; 🎒 a thing carried in from another room, left lying, goes home there.
      if ((it.house || it.carried) && it.holder < 0) {
        const away = it.carried || Math.hypot(it.x - it.home.x, it.z - it.home.z) > 0.05 || it.y > 0.01;
        if (!away) it.lyingSince = 0;
        else if (!it.lyingSince) it.lyingSince = this.tick;
        else if (it.carried && this.tick - it.lyingSince > RACK_RETURN_SECONDS * HZ) {
          this.emit('dust', { at: { x: it.x, y: it.y, z: it.z } });
          if (!this.goingHome.includes(it.id)) this.goingHome.push(it.id);
          it.awayUntil = this.tick + 10 * HZ; // (out of sight until the server lists it at home)
          Object.assign(it, { y: -50, vx: 0, vy: 0, vz: 0 });
          continue;
        } else if (!it.carried && this.tick - it.lyingSince > RACK_RETURN_SECONDS * HZ) {
          this.emit('dust', { at: { x: it.x, y: it.y, z: it.z } });
          Object.assign(it, { x: it.home.x, y: 0, z: it.home.z, vx: 0, vy: 0, vz: 0, spin: 0, thrownBy: -1, lyingSince: 0 });
          this.emit('dust', { at: { x: it.x, y: 0.2, z: it.z } });
          continue;
        }
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
          const hit = surfaceHit(it, dir, travel, this.objects, time, this.half);
          if (hit) {
            Object.assign(it, along(it, dir, Math.max(0, hit.t - 0.02)));
            const v = reflect({ x: it.vx, y: it.vy, z: it.vz }, hit.normal, 0.42, 0.85);
            Object.assign(it, { vx: v.x, vy: v.y, vz: v.z });
            it.spin = (it.spin || 0) * 0.7 + sp * 0.4;
            if (sp > 3) this.emit('bounce', { item: it.id, at: { x: it.x, y: it.y, z: it.z } });
          } else Object.assign(it, along(it, dir, travel));
        }
      }
      if (it.spent && it.y < 0.06) { // (the floor holds a falling thing a hair over it)
        // 🔫 Empty, it breaks on the ground. A house gun is back on its rack, loaded, in a moment; anything else is gone from the room, to be made or put back again.
        this.emit('removed', { at: { x: it.x, y: 0.3, z: it.z } });
        this.emit('dust', { at: { x: it.x, y: 0.1, z: it.z } });
        Object.assign(it, { spent: false, ammo: CLIP, vx: 0, vy: 0, vz: 0, spin: 0, thrownBy: -1, lyingSince: 0, x: it.home.x, y: -50, z: it.home.z });
        if (it.house) it.awayUntil = this.tick + 3 * HZ;
        else { it.awayUntil = this.tick + 600 * HZ; this.broken.push(it.id); } // (out of sight until the server has taken it out)
        continue;
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
      it.x = Math.max(-this.half + 0.5, Math.min(this.half - 0.5, it.x));
      it.z = Math.max(-this.half + 0.5, Math.min(this.half - 0.5, it.z));
      // A thrown item hits the first other player it reaches while fast.
      if (it.thrownBy >= 0) {
        const struck = Math.hypot(it.vx, it.vy, it.vz) >= 5 && this.targets.find((t) =>
          it.x > t.box.xMin - 0.2 && it.x < t.box.xMax + 0.2 && it.z > t.box.zMin - 0.2 && it.z < t.box.zMax + 0.2 && it.y < t.box.yMax + 0.2);
        if (struck) {
          if (it.effect === 'explode') this.detonate(it);
          else {
            this.strike(struck, it.thrownBy, 30, it);
            it.vx *= -0.3;
            it.vz *= -0.3;
            it.thrownBy = -1;
          }
        } else if (Math.hypot(it.vx, it.vy, it.vz) < 5) it.thrownBy = -1;
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
      if (!it.fuseAt && !it.spent && it.y < 1.2 && Math.hypot(it.vx, it.vy, it.vz) < 6)
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
        let body = null, target = null, at = travel;
        for (const s of this.seats.filter(Boolean)) {
          const q = s.player;
          if (!q.alive || (q.seat === b.owner && this.tick < b.safeUntil)) continue;
          const d = rayBox(b, dir, bodyBox(q), travel);
          if (d < at) {
            at = d;
            body = q;
          }
        }
        for (const t of this.targets) {
          const d = rayBox(b, dir, t.box, travel);
          if (d < at) {
            at = d;
            body = null;
            target = t;
          }
        }
        let watcher = null;
        for (const w of this.watchers) {
          const d = rayBox(b, dir, { xMin: w.x - 0.4, xMax: w.x + 0.4, yMin: w.y - 0.25, yMax: w.y + 0.25, zMin: w.z - 0.4, zMax: w.z + 0.4 }, travel);
          if (d < at) {
            at = d;
            body = null;
            target = null;
            watcher = w;
          }
        }
        let zombie = null;
        for (const z of this.zombies) {
          const d = rayBox(b, dir, z.box, travel);
          if (d < at) {
            at = d;
            body = null;
            target = null;
            watcher = null;
            zombie = z;
          }
        }
        const surface = surfaceHit(b, dir, at, this.objects, time, this.half, this.open, this.slab);
        // (A target is solid too: met at the same spot, the target takes the round.)
        if (surface && (target || watcher || zombie ? surface.t < at - 0.01 : surface.t <= at)) {
          Object.assign(b, along(b, dir, Math.max(0, surface.t - 0.02)));
          const v = reflect({ x: b.vx, y: b.vy, z: b.vz }, surface.normal);
          Object.assign(b, { vx: v.x, vy: v.y, vz: v.z });
          b.bounces++;
          this.emit('ricochet', { at: { x: b.x, y: b.y, z: b.z } });
          continue;
        }
        Object.assign(b, along(b, dir, at));
        if (target) {
          // The top of it is its head: a round there counts for everything.
          const head = b.y > target.box.yMax - 0.35;
          this.strike(target, b.owner, head ? 100 : 34, b);
          b.dead = true;
        }
        if (zombie) {
          // 🧟 Any round ends a zombie: it comes down or comes apart like a body (seat 3's green).
          this.zombies = this.zombies.filter((z) => z !== zombie);
          this.zombieKills.push(zombie.id);
          // (👹 A flying head has no body to come apart as: it bursts where it is.)
          // (`off`: where on the head the round struck, from its middle, each way -1 to 1: its bits are thrown from that side.)
          if (zombie.head) this.stain(zombie.x, zombie.z, 0.9, dir);
          if (zombie.head) this.emit('headpop', { at: { x: zombie.x, y: zombie.y, z: zombie.z }, attacker: b.owner, dir: { x: dir.x, y: dir.y, z: dir.z }, yaw: zombie.yaw || 0,
            off: { x: Math.max(-1, Math.min(1, (b.x - zombie.x) / HEAD.r)), y: Math.max(-1, Math.min(1, (b.y - zombie.y) / HEAD.r)), z: Math.max(-1, Math.min(1, (b.z - zombie.z) / HEAD.r)) } });
          else this.emit('gib', { seat: 3, zombie: true, attacker: b.owner, kind: 'shot', zone: hitZone(zombie, b),
            at: { x: zombie.x, y: zombie.y - 0.8, z: zombie.z }, hit: { x: b.x, y: b.y, z: b.z }, dir: { x: dir.x, y: dir.y, z: dir.z } });
          b.dead = true;
        }
        if (watcher) {
          // 🚁 A drone a round meets goes up in a burst; its watcher is set down again (client.mjs 'downed').
          this.emit('blast', { at: { x: watcher.x, y: watcher.y, z: watcher.z }, seat: b.owner });
          this.emit('downed', { eye: watcher.eye, attacker: b.owner, at: { x: watcher.x, y: watcher.y, z: watcher.z } });
          this.watchers = this.watchers.filter((w) => w !== watcher);
          b.dead = true;
        }
        if (body) {
          // 🏙️ Instagib: any round that meets a body ends it, and the body comes apart.
          const head = b.y > body.y - 0.25;
          this.hit(body, b.owner, 100, dir, 6, null);
          if (head) this.emit('headshot', { seat: body.seat, attacker: b.owner });
          // 🎯 A round from the room beside: the kill is its shooter's, there (server.mjs credits it).
          if (!body.alive && b.owner < 0 && b.from) this.credits.push({ from: b.from, by: b.by });
          if (!body.alive) {
            body.gibbed = true;
            this.stain(body.x, body.z, 1.2, dir);
            this.emit('gib', { seat: body.seat, attacker: b.owner, kind: 'shot', zone: hitZone(body, b),
              at: { x: body.x, y: body.y - 0.8, z: body.z }, hit: { x: b.x, y: b.y, z: b.z }, dir: { x: dir.x, y: dir.y, z: dir.z } });
          }
          b.dead = true;
        }
      }
      // 🌐 Over an open side, into the room beside (server.mjs takes it on, there or on another host).
      const side = b.dead ? null : b.x > this.half ? 'east' : b.x < -this.half ? 'west' : b.z > this.half ? 'north' : b.z < -this.half ? 'south' : null;
      if (side && this.open?.includes(side)) {
        // (Who fired it, and from where: its first room, however many seams it has crossed.)
        this.outbound.push({ side, x: b.x, y: b.y, z: b.z, vx: b.vx, vy: b.vy, vz: b.vz, bounces: b.bounces,
          by: b.by ?? this.nameOf?.(b.owner) ?? null, from: b.from ?? (b.owner >= 0 ? { seat: b.owner } : null) });
        b.dead = true;
      }
      // (Off a platform a round flies on into the open a good way before it is gone.)
      const out = this.half + (this.slab ? 60 : 5);
      if (b.bounces > 16 || Math.abs(b.x) > out || Math.abs(b.z) > out || b.y > 80 || b.y < FALL_Y) b.dead = true;
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
    this.seats[seat] = { player: this.fresh(seat), ack: 0, queue: [], last: {} };
    // Someone arriving never moves or stops whoever is already here: the
    // match is simply on (a round that ends still resets, with its countdown).
    if (this.count === 2 && this.phase === 'waiting') this.phase = 'playing';
    return seat;
  }

  // A seat passes to a new connection of the same person, body and all. The
  // newcomer counts its commands from one, so the count kept for the old
  // connection goes: left standing, every command up to it was dropped and
  // the body would not move until the new count had passed the old.
  handover(seat) {
    const s = this.seats[seat];
    if (s) Object.assign(s, { ack: 0, queue: [], last: {} });
  }

  // A departure only stops the match when it leaves one player alone.
  leave(seat) {
    this.seats[seat] = null;
    this.rockets = this.rockets.filter((r) => r.owner !== seat);
    for (const it of this.items.values()) if (it.holder === seat) it.holder = -1;
    if (this.count >= 2 || this.arena) return;
    this.phase = 'waiting';
    this.winner = null;
    this.rockets = [];
    // Whoever is left keeps walking where they stand; only the score goes.
    for (const s of this.seats.filter(Boolean)) Object.assign(s.player, { score: 0, health: 100 });
  }

  reset() {
    this.phase = 'countdown';
    this.until = this.tick + 3 * HZ;
    this.winner = null;
    this.rockets = [];
    for (const s of this.seats.filter(Boolean)) {
      s.player = this.fresh(s.player.seat, s.player.life + 1);
      // Consume already received input so it cannot fire in the new round.
      s.ack = Math.max(s.ack, ...s.queue.map((c) => c.seq));
      s.queue = [];
      s.last = {};
    }
  }

  input(seat, commands) {
    const s = this.seats[seat];
    if (!s || !Array.isArray(commands) || commands.length > INPUT_BATCH) return;
    for (const raw of commands) {
      const c = command(raw);
      if (
        !c ||
        c.seq <= s.ack ||
        c.seq > s.ack + HORIZON ||
        s.queue.length >= INPUT_QUEUE ||
        s.queue.some((q) => q.seq === c.seq)
      )
        continue;
      s.queue.push(c);
    }
    s.queue.sort((a, b) => a.seq - b.seq);
  }

  // 🚶 A body set down from another room lived `ticks` on the way, steered
  // by commands that are still to come: they are run when they arrive.
  owe(seat, ticks) {
    const s = this.seats[seat];
    if (!s) return;
    s.owed = true;
    s.quiet = 0;
    s.budget = Math.max(s.budget || 0, Math.min(INPUT_GRACE + 1, Math.max(0, ticks)));
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
      // ⏱️ A player moves by their own commands, one per tick of their own
      // time, as in Quake: a command that arrives late is still run, in the
      // order sent, so the server ends where the client predicted and a
      // jittery link never drags anyone back. The budget is real time: every
      // tick adds one command's worth, a tick with nothing to run keeps it
      // (up to INPUT_GRACE, the HORIZON), and a late burst spends it all at
      // once. Nobody gains time by it, and nothing waits in the queue after:
      // a backlog would be latency on every input for the rest of the visit.
      s.budget = Math.min(INPUT_GRACE + 1, (s.budget || 0) + 1);
      const take = Math.min(s.queue.length, Math.floor(s.budget));
      let commands = s.queue.splice(0, take);
      if (take) {
        s.budget -= take;
        s.quiet = 0;
      } else if ((s.ack || s.owed) && (s.quiet || 0) < INPUT_GRACE) {
        s.quiet = (s.quiet || 0) + 1; // Late, not gone: hold still, keep the time.
      } else {
        // Nothing for a while (or never): the body runs on with no input,
        // and that tick is spent.
        s.budget = Math.max(0, s.budget - 1);
        commands = [null];
      }
      // A few left over (clocks differ a little): run them off slowly, a
      // twentieth faster than real time. More than a few is time the body
      // already lived through without them: dropped, never run.
      if (s.queue.length) s.budget = Math.min(INPUT_GRACE + 1, s.budget + 0.05);
      while (s.queue.length > INPUT_SLACK) s.ack = s.queue.shift().seq;
      let p = s.player;
      if (!p.alive && (this.phase === 'playing' || this.phase === 'waiting') && this.tick >= p.respawnAt) {
        if (p.gibbed && this.roofBound && !p.roofAsked) {
          // Up to the roof (the server moves the seat after this tick); if it
          // cannot, the body comes back here a second later all the same.
          p.roofAsked = true;
          p.respawnAt = this.tick + HZ;
          this.toRoof.push(p.seat);
        } else {
          p = { ...this.fresh(p.seat, p.life + 1), score: p.score };
          s.last = {};
        }
      }
      p.cooldown = Math.max(0, p.cooldown - 1);
      let fire = false;
      for (const c of commands) {
        if (c) {
          s.ack = c.seq;
          s.last = c;
        }
        const input = c || { yaw: s.last.yaw, pitch: s.last.pitch, buttons: 0 };
        s.moves = (s.moves || 0) + 1; // ⏱️ the body's own time: one for each step it has lived (snapshot `moves`)
        // Waiting is a movement warmup; the match countdown locks both spawns.
        if (this.phase === 'playing' || this.phase === 'waiting') {
          p = move(p, input, this.objects, this.tick / HZ, this.walls, !!this.arena, this.open, this.half);
          // 🧱 Fallen off a platform: stood back where bodies arrive, the score kept.
          // 🏙️ Fallen off the roof, all the way to the street: the body comes apart there, is seen a moment, and stands on the roof again (☠️ a death, counted as a fall).
          if (p.alive && p.y < FALL_Y) {
            p = { ...p, y: FALL_Y, health: 0, alive: false, gibbed: true, vx: 0, vy: 0, vz: 0, respawnAt: this.tick + Math.round(HZ * (this.waves ? HEAD.down : 2.5)) };
            this.deaths.push({ seat: p.seat, attacker: -1, fell: true });
            this.emit('frag', { seat: p.seat, attacker: -1 });
            this.emit('gib', { seat: p.seat, attacker: -1, kind: 'blast', zone: 'legs', at: { x: p.x, y: p.y - 0.8, z: p.z }, hit: { x: p.x, y: p.y - 1.6, z: p.z }, dir: { x: 0, y: -1, z: 0 } });
            s.last = {};
          }
        } else if (c) {
          p.yaw = c.yaw;
          p.pitch = c.pitch;
        }
        s.player = p;
        // Checked after every command, so a late burst cannot fall through a head.
        if (this.phase === 'playing' || this.phase === 'waiting') this.bop(s);
        if (c && (c.use || c.drop)) this.useItem(s, c);
        if (c?.chalk) this.addChalk(p.seat, c.chalk);
        if (c) this.chalking[p.seat] = !!c.chalk;
        fire ||= !!c?.fire;
      }
      s.player = p;
      if (
        fire &&
        p.alive &&
        !p.cooldown &&
        (this.phase === 'playing' || this.phase === 'waiting')
      )
        shots.push(s);
    }
    for (const d of this.drones) stepDrone(d, this.tick);
    for (const s of shots) this.shoot(s);
    if (this.phase === 'playing' || this.phase === 'waiting') {
      this.stepRockets();
      this.stepItems();
      if (this.waves) this.aimHeads();
      this.stepBullets();
      if (this.waves) this.stepHeads();
    }
  }

  // 👹 Before the rounds fly: the heads stand among what a round can meet (as the horde's walkers do).
  aimHeads() {
    for (const h of this.heads) h.box = { xMin: h.x - HEAD.r, xMax: h.x + HEAD.r, yMin: h.y - HEAD.r, yMax: h.y + HEAD.r, zMin: h.z - HEAD.r, zMax: h.z + HEAD.r };
    this.zombies = [...this.zombies.filter((z) => !z.head), ...this.heads];
  }
  // …and after: the ones a round met are gone; the rest fly on, each at the nearest living body.
  stepHeads() {
    const shot = this.heads.filter((h) => !this.zombies.includes(h));
    if (shot.length) {
      this.heads = this.heads.filter((h) => this.zombies.includes(h));
      this.zombieKills = this.zombieKills.filter((id) => !shot.some((h) => h.id === id));
    }
    const bodies = this.seats.filter(Boolean).map((s) => s.player).filter((p) => p.alive);
    // Nobody to come for: the sky empties and the count starts over.
    if (!bodies.length) { this.heads = []; this.wave = 0; this.headsOwed = 0; this.headAt = 0; return; }
    // The first wave waits for someone to take the gun up; after that they keep coming.
    const armed = this.wave > 0 || [...this.items.values()].some((it) => it.kind === 'shoot' && it.holder >= 0);
    const bell = this.bellMs ? Math.floor(this.now() / this.bellMs) : null, struck = bell === null || (this.bellWas !== null && bell !== this.bellWas);
    this.bellWas = bell;
    if (armed && struck && !this.heads.length && !this.headsOwed && this.tick >= this.headAt) {
      this.wave++;
      this.headsOwed = HEAD.first + this.wave;
      this.headAt = this.tick;
      this.emit('wave', { wave: this.wave, heads: this.headsOwed });
    }
    if (this.headsOwed && this.tick >= this.headAt) {
      const a = this.random() * Math.PI * 2, far = this.half + HEAD.from;
      this.heads.push({ id: `head-${++this.headId}`, head: true, x: Math.sin(a) * far, y: 2.5 + this.random() * 4, z: Math.cos(a) * far, vx: 0, vy: 0, vz: 0, yaw: 0 });
      this.headsOwed--;
      this.headAt = this.tick + Math.round(HZ * (this.headsOwed ? HEAD.gap : HEAD.rest));
    }
    const speed = Math.min(HEAD.fastest, HEAD.speed + this.wave * 0.25);
    for (const h of this.heads) {
      let to = null, near = Infinity;
      for (const p of bodies) { const d = Math.hypot(p.x - h.x, p.y - h.y, p.z - h.z); if (d < near) { near = d; to = p; } }
      const dx = to.x - h.x, dy = to.y - h.y, dz = to.z - h.z, d = near || 1;
      // (It leans into its turn: a head that is dodged swings wide and comes round again.)
      h.vx += ((dx / d) * speed - h.vx) * 0.045; h.vy += ((dy / d) * speed - h.vy) * 0.045; h.vz += ((dz / d) * speed - h.vz) * 0.045;
      h.x += h.vx * DT; h.y += h.vy * DT; h.z += h.vz * DT;
      h.yaw = (Math.atan2(h.vx, h.vz) * 180) / Math.PI;
      if (near < HEAD.bite && to.alive) {
        const dir = { x: dx / d, y: dy / d, z: dz / d };
        this.hit(to, -1, 100, dir, 6, null);
        if (!to.alive) {
          to.gibbed = true;
          to.respawnAt = this.tick + Math.round(HZ * HEAD.down); // (long enough to see it)
          this.stain(to.x, to.z, 1.4, dir);
          this.emit('gib', { seat: to.seat, attacker: -1, kind: 'head', zone: 'head', at: { x: to.x, y: to.y - 0.8, z: to.z }, hit: { x: h.x, y: h.y, z: h.z }, dir });
        }
        h.done = true;
      }
    }
    // 🎱 Heads do not pass through one another: two that meet are put apart and each takes the other's push along the line between them, so a pack jostles and spreads instead of flying as one.
    for (let a = 0; a < this.heads.length; a++)
      for (let b = a + 1; b < this.heads.length; b++) {
        const p = this.heads[a], q = this.heads[b];
        let dx = q.x - p.x, dy = q.y - p.y, dz = q.z - p.z, d = Math.hypot(dx, dy, dz);
        const need = HEAD.r * 2;
        if (d >= need) continue;
        if (d < 1e-6) { dx = 1; dy = 0; dz = 0; d = 1; } // (one on top of the other: apart along x)
        const nx = dx / d, ny = dy / d, nz = dz / d, over = (need - d) / 2;
        p.x -= nx * over; p.y -= ny * over; p.z -= nz * over;
        q.x += nx * over; q.y += ny * over; q.z += nz * over;
        const closing = (p.vx - q.vx) * nx + (p.vy - q.vy) * ny + (p.vz - q.vz) * nz;
        if (closing > 0) {
          p.vx -= closing * nx; p.vy -= closing * ny; p.vz -= closing * nz;
          q.vx += closing * nx; q.vy += closing * ny; q.vz += closing * nz;
          if (closing > 0.6) this.emit('headbump', { at: { x: (p.x + q.x) / 2, y: (p.y + q.y) / 2, z: (p.z + q.z) / 2 } });
        }
      }
    if (this.heads.some((h) => h.done)) {
      // It got someone: the wave is over and the count starts again, after a breath.
      this.heads = []; this.headsOwed = 0; this.wave = 0;
      this.headAt = this.tick + Math.round(HZ * (HEAD.down + 1));
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

  // 🌐 A round in from the room beside: it flies on from where it crossed,
  // nobody's here (its shooter is in another room), dangerous at once.
  takeRound(r) {
    if (!['x', 'y', 'z', 'vx', 'vy', 'vz'].every((k) => Number.isFinite(r[k]))) return;
    // (Someone alone in a room can be shot from the next one: a round from outside starts the match.)
    if (this.phase === 'waiting' && this.count >= 1) this.phase = 'playing';
    this.bullets.push({ x: r.x, y: r.y, z: r.z, vx: r.vx, vy: r.vy, vz: r.vz, owner: -1, born: this.tick, safeUntil: 0,
      bounces: r.bounces || 0, id: ++this.eventId, by: typeof r.by === 'string' ? r.by.slice(0, 40) : null,
      from: r.from && typeof r.from.room === 'string' && Number.isInteger(r.from.seat) ? { room: r.from.room, seat: r.from.seat } : null });
    while (this.bullets.length > 24) this.bullets.shift();
  }

  // 🎯 A kill this seat made in another room with a round that flew over: it counts here.
  credit(seat) {
    const killer = this.seats[seat]?.player;
    if (!killer) return false;
    killer.score += 1;
    this.emit('frag', { seat: -1, attacker: seat, far: true });
    return true;
  }

  damage(seat, amount, attacker) {
    const p = this.seats[seat]?.player;
    // (Alone in a room, waiting, a body is hurt all the same: its own round come back off a wall ends it.)
    if (!p?.alive || (this.phase !== 'playing' && this.phase !== 'waiting')) return;
    p.health = Math.max(0, p.health - amount);
    if (p.health) return;
    p.alive = false;
    p.respawnAt = this.tick + (this.waves ? Math.round(HZ * HEAD.down) : HZ); // (at the stage every death is seen again before the body stands: client.mjs replay)
    const killer = this.seats[attacker]?.player;
    if (killer) killer.score += attacker === seat ? -1 : 1;
    this.deaths.push({ seat, attacker });
    this.emit('frag', { seat, attacker });
    if (this.phase === 'playing' && killer?.score >= FRAG_LIMIT) {
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
        wallDistance(r, dir, distance, this.objects, this.tick / HZ, this.half) < distance - 0.05
      )
        continue;
      const force = Math.max(0, 1 - distance / 4);
      p.vx += dir.x * force * 15;
      p.vz += dir.z * force * 15;
      p.vy += Math.max(0.3, dir.y) * force * 15;
      p.onGround = false;
      this.damage(p.seat, p.seat === direct ? 100 : Math.ceil(80 * force), r.owner);
      // 🩸 Close to a blast, a body comes all apart (gore.mjs); further off, it is thrown.
      if (!p.alive && !p.gibbed) {
        p.gibbed = true;
        this.emit('gib', { seat: p.seat, attacker: r.owner, kind: distance < 2.2 || p.seat === direct ? 'blast' : 'shot', zone: 'torso',
          at: { x: p.x, y: p.y - 0.8, z: p.z }, hit: { x: r.x, y: r.y, z: r.z }, dir: { x: dir.x, y: dir.y, z: dir.z } });
      }
    }
    for (const t of this.targets) {
      const c = this.targetCentre(t), distance = Math.hypot(c.x - r.x, c.y - r.y, c.z - r.z);
      if (distance < 4) this.strike(t, r.owner, Math.ceil(80 * Math.max(0.25, 1 - distance / 4)), { x: c.x, y: t.box.yMax, z: c.z });
    }
  }

  stepRockets() {
    const remaining = [];
    for (const r of this.rockets) {
      if (this.phase !== 'playing' && this.phase !== 'waiting') break;
      const travel = 28 * DT;
      let distance = wallDistance(r, r.dir, travel, this.objects, this.tick / HZ, this.half),
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

  // ✈️ What is in the air, or lies where it was thrown: put by when a room
  // empties or the server stops, taken up again when the room is next made.
  // Time stands still in between. Nobody's seat is remembered: a round
  // that flies again is nobody's.
  flight() {
    const loose = [...this.items.values()].filter((it) => it.holder < 0 && !it.awayUntil &&
      (it.y > 0 || it.fuseAt || Math.hypot(it.x - it.home.x, it.z - it.home.z) > 0.01));
    if (!this.rockets.length && !this.bullets.length && !loose.length) return null;
    return {
      rockets: this.rockets.map((r) => ({ x: r.x, y: r.y, z: r.z, dir: { ...r.dir }, age: this.tick - r.born })),
      bullets: this.bullets.map((b) => ({ x: b.x, y: b.y, z: b.z, vx: b.vx, vy: b.vy, vz: b.vz, bounces: b.bounces })),
      items: loose.map((it) => ({ id: it.id, x: it.x, y: it.y, z: it.z, vx: it.vx, vy: it.vy, vz: it.vz, spin: it.spin,
        fuse: it.fuseAt ? Math.max(1, it.fuseAt - this.tick) : 0 })),
    };
  }
  resume(kept) {
    const real = (o, keys) => !!o && keys.every((k) => Number.isFinite(o[k]));
    for (const r of kept?.rockets || [])
      if (real(r, ['x', 'y', 'z', 'age']) && real(r.dir, ['x', 'y', 'z']))
        this.rockets.push({ x: r.x, y: r.y, z: r.z, dir: { ...r.dir }, owner: -1, born: this.tick - r.age, id: ++this.eventId });
    for (const b of kept?.bullets || [])
      if (real(b, ['x', 'y', 'z', 'vx', 'vy', 'vz']))
        this.bullets.push({ x: b.x, y: b.y, z: b.z, vx: b.vx, vy: b.vy, vz: b.vz, owner: -1, born: this.tick, safeUntil: 0,
          bounces: b.bounces || 0, id: ++this.eventId });
    for (const k of kept?.items || []) {
      const it = this.items.get(k.id);
      if (it && real(k, ['x', 'y', 'z', 'vx', 'vy', 'vz']))
        Object.assign(it, { x: k.x, y: k.y, z: k.z, vx: k.vx, vy: k.vy, vz: k.vz, spin: k.spin || 0, holder: -1, thrownBy: -1,
          fuseAt: k.fuse ? this.tick + k.fuse : 0 });
    }
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
      // 👹 [id, x, y, z, yaw] of each head in the air, and the wave they are.
      ...(this.waves ? { heads: this.heads.map((h) => [h.id, Math.round(h.x * 100) / 100, Math.round(h.y * 100) / 100, Math.round(h.z * 100) / 100, Math.round(h.yaw)]), wave: this.wave, left: this.heads.length + this.headsOwed, armed: this.wave > 0 || [...this.items.values()].some((it) => it.kind === 'shoot' && it.holder >= 0) } : {}),
      // 🩸 (All the stains, every two seconds and whenever one was just made: the events carry them as they happen, this is for whoever was not here.)
      ...(this.stains.length && (this.tick % 120 === 0 || this.stainsNew) ? { stains: (this.stainsNew = false, this.stains) } : {}),
      acks: this.seats.map((s) => s?.ack || 0),
      // ⏱️ How many steps each body has lived. A body moves by its own commands, none on a tick when they are late and several on the next, so in the server's ticks it stops and leaps; in its own steps it walks evenly. Whoever watches plays it back by these (net.mjs interpolate, `own`).
      moves: this.seats.map((s) => s?.moves || 0),
      bops: this.seats.map((s) => s?.bops || 0),
      // 🔗 [how many in the chain, what it is worth so far], by seat, while in the air.
      combos: this.seats.map((s) => (s?.combo ? [s.combo, s.pending] : 0)),
      drones: this.drones.filter((d) => d.alive).map((d) => ({ id: d.id, x: Math.round(d.x * 1000) / 1000, y: d.y, dir: d.dir })),
      rockets: this.rockets.map((r) => ({ ...r })),
      items: [...this.items.values()].map((it) => ({
        id: it.id, kind: it.kind, x: it.x, y: it.y, z: it.z, holder: it.holder, spin: it.spin, ...(it.kind === 'shoot' ? { ammo: it.ammo } : {}),
      })),
      bullets: this.bullets.map((b) => ({ id: b.id, owner: b.owner, x: b.x, y: b.y, z: b.z, vx: b.vx, vy: b.vy, vz: b.vz })),
      events: this.events.map((e) => ({ ...e })),
    };
  }
}
