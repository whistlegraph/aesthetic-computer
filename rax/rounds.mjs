// 🔫 Rounds in flight, as this page sees them.
// The server rules every hit (match.mjs stepBullets). Between its snapshots
// a round used to be carried forward a tenth of a second and then stood
// still in the air, so on a far connection every gap froze the fight. Here
// each round flies on by its own velocity, ricocheting off the same
// surfaces the server uses, until the next snapshot sets it right; the way
// the ball is drawn from its motion (ball.mjs ballAt) and not its last spot.
// A round of your own leaves your hand the moment you fire: the page flies
// it at once, and when the server's round for that shot arrives it is
// hidden behind this one, which ends when the server's does.
import { direction, surfaceHit, reflect, rayBox } from './world.mjs';
import { objectBoxes } from './objects.mjs';

export const ROUND_SPEED = 32; // (match.mjs useItem: f * 32)
const COOLDOWN_MS = 300; // (match.mjs: ready after HZ * 0.3)
const FREE_FLIGHT_MS = 600; // How long a round flies on with no word of it.
const UNCLAIMED_MS = 1200; // A shot of ours the server never answered was refused.
const LIFE_MS = 3000;
const MAX_BOUNCES = 16;

// Where a round leaves the hand (match.mjs hand()).
export function handOf(p) {
  const f = direction(p.yaw, p.pitch),
    r = direction(p.yaw - 90);
  return { x: p.x + r.x * 0.3 + f.x * 0.45, y: p.y - 0.55 + f.y * 0.45, z: p.z + r.z * 0.3 + f.z * 0.45 };
}

export class Rounds {
  constructor() {
    this.flying = new Map(); // id → the server's round, flown on here
    this.mine = []; // rounds this page fired and is flying itself
    this.readyAt = 0;
    this.count = 0;
  }

  // A snapshot's rounds: each one set right, new ones of ours paired with
  // the shot this page already shows, gone ones dropped (and their twins).
  heard(list = [], seat = -1, now = performance.now()) {
    const seen = new Set();
    for (const b of list) {
      seen.add(b.id);
      let r = this.flying.get(b.id);
      if (!r) {
        r = { id: b.id, owner: b.owner, bounces: 0, flown: true };
        this.flying.set(b.id, r);
        if (seat >= 0 && b.owner === seat) {
          const twin = this.mine.find((m) => m.twin === undefined && !m.dead);
          if (twin) { twin.twin = b.id; r.twin = twin; }
        }
      }
      Object.assign(r, { x: b.x, y: b.y, z: b.z, vx: b.vx, vy: b.vy, vz: b.vz, heardAt: now });
    }
    for (const [id, r] of this.flying)
      if (!seen.has(id)) {
        this.flying.delete(id);
        if (r.twin) r.twin.dead = true;
      }
  }

  // This page fired: show the round now. False while the gun is cooling.
  fire(p, now = performance.now()) {
    if (now < this.readyAt || !p) return false;
    this.readyAt = now + COOLDOWN_MS;
    const f = direction(p.yaw, p.pitch), h = handOf(p);
    this.mine.push({ id: `mine-${++this.count}`, ...h, vx: f.x * ROUND_SPEED, vy: f.y * ROUND_SPEED, vz: f.z * ROUND_SPEED,
      bornAt: now, bounces: 0, flown: true });
    return true;
  }

  // Fly everything on to `now`; return what to draw.
  step(now, objects = [], time = 0, half, open = null) {
    // A target (a scarecrow…) stops a round where it strikes: one box each, as the server has it.
    if (objects !== this.boxesFrom) {
      this.boxesFrom = objects;
      this.stops = objects.filter((o) => o.target).map((o) => {
        const box = { xMin: Infinity, yMin: Infinity, zMin: Infinity, xMax: -Infinity, yMax: -Infinity, zMax: -Infinity };
        for (const b of objectBoxes([o], 0)) for (const k of ['x', 'y', 'z']) {
          box[k + 'Min'] = Math.min(box[k + 'Min'], b[k + 'Min']);
          box[k + 'Max'] = Math.max(box[k + 'Max'], b[k + 'Max']);
        }
        return box;
      }).filter((b) => Number.isFinite(b.xMin));
    }
    const stops = this.stops || [];
    const dtOf = (r) => {
      const dt = Math.max(0, Math.min(0.05, (now - (r.steppedAt ?? now)) / 1000));
      r.steppedAt = now;
      return dt;
    };
    const out = [];
    for (const r of this.flying.values()) {
      const dt = dtOf(r);
      if (r.twin) continue; // (ours: drawn as the round this page fired)
      if (now - r.heardAt < FREE_FLIGHT_MS && !r.stopped) fly(r, dt, objects, time, half, stops, open);
      if (!r.stopped) out.push(r);
    }
    this.mine = this.mine.filter((m) => !m.dead && !m.stopped && now - m.bornAt < LIFE_MS && m.bounces <= MAX_BOUNCES &&
      (m.twin !== undefined || now - m.bornAt < UNCLAIMED_MS));
    for (const m of this.mine) {
      fly(m, dtOf(m), objects, time, half, stops, open);
      out.push(m);
    }
    return out;
  }

  clear() {
    this.flying.clear();
    this.mine = [];
  }
}

// One frame of flight: along the velocity, off whatever it meets.
// (`open`: the sides that lead on; a round goes out through one and is the next room's to show.)
function fly(r, dt, objects, time, half, stops = [], open = null) {
  let left = Math.hypot(r.vx, r.vy, r.vz) * dt;
  for (let k = 0; k < 4 && left > 1e-6; k++) {
    const speed = Math.hypot(r.vx, r.vy, r.vz);
    if (!speed) return;
    const dir = { x: r.vx / speed, y: r.vy / speed, z: r.vz / speed };
    const hit = surfaceHit(r, dir, left, objects, time, half, open);
    let stop = Infinity;
    for (const box of stops) stop = Math.min(stop, rayBox(r, dir, box, left));
    if (stop <= left && (!hit || stop <= hit.t)) {
      r.x += dir.x * stop; r.y += dir.y * stop; r.z += dir.z * stop;
      r.stopped = true;
      return;
    }
    if (!hit) {
      r.x += dir.x * left; r.y += dir.y * left; r.z += dir.z * left;
      if (half && (Math.abs(r.x) > half || Math.abs(r.z) > half)) r.stopped = true; // (over the seam: the room beside shows it now)
      return;
    }
    const t = Math.max(0, hit.t - 0.02);
    r.x += dir.x * t; r.y += dir.y * t; r.z += dir.z * t;
    const v = reflect({ x: r.vx, y: r.vy, z: r.vz }, hit.normal);
    Object.assign(r, { vx: v.x, vy: v.y, vz: v.z });
    r.bounces++;
    left -= hit.t;
  }
}
