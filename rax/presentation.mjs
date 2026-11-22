import { clamp, HZ, INTERP_TICKS } from './world.mjs';

const axes = ['x', 'y', 'z'];
export const AHEAD_TICKS = 12; // a fifth of a second of a walk carried on past the newest snapshot

// Presentation alone runs between simulation ticks. Never feed these positions
// back into prediction, collision, commands or authoritative hit detection.
export class LocalPresentation {
  reset(player) {
    this.current = this.previous = player;
    this.error = { x: 0, y: 0, z: 0 };
  }

  // Both interpolation endpoints describe the same walk on either side of a block line.
  translate(dx, dz, dy = 0) {
    if (!this.current) return;
    const shift = (p) => ({ ...p, x: p.x + dx, y: p.y + dy, z: p.z + dz });
    this.previous = shift(this.previous);
    this.current = shift(this.current);
  }

  step(player) {
    if (!this.current || this.current.life !== player.life || this.current.alive !== player.alive) {
      this.reset(player);
      return;
    }
    this.previous = this.current;
    this.current = player;
  }

  correct(player, discontinuity = false) {
    const old = this.current;
    if (
      !old ||
      discontinuity ||
      old.life !== player.life ||
      old.alive !== player.alive ||
      Math.hypot(old.x - player.x, old.y - player.y, old.z - player.z) > 1
    ) {
      this.reset(player);
      return;
    }
    // Rebase both endpoints so a correction cannot pop the interpolation pair.
    // Retain the opposite visual offset and let sample() decay it in real time.
    this.previous = { ...this.previous };
    for (const axis of axes) {
      const delta = player[axis] - old[axis];
      this.previous[axis] += delta;
      this.error[axis] -= delta;
    }
    this.current = player;
    if (Math.hypot(this.error.x, this.error.y, this.error.z) > 1) this.reset(player);
  }

  sample(alpha, seconds) {
    if (!this.current) return null;
    const p = { ...this.current },
      t = clamp(alpha, 0, 1);
    const decay = Math.exp(-Math.max(0, seconds) / 0.07);
    for (const axis of axes) {
      this.error[axis] *= decay;
      p[axis] =
        this.previous[axis] + (this.current[axis] - this.previous[axis]) * t + this.error[axis];
    }
    return p;
  }
}

// Packet arrival changes the target playback rate, never the rendered tick.
// Both players sample one monotonic timeline; starvation freezes at known data.
export class SnapshotClock {
  reset() {
    this.tick = this.latest = this.at = this.sampledAt = undefined;
    this.late = 0;
    this.ahead = 0;
  }

  // How far behind the newest word the picture plays, in ticks: the usual
  // few, and more on a link that delivers in clumps (the worst lateness
  // lately, which fades over a few seconds), so others glide instead of
  // stopping and jumping.
  get delay() {
    return INTERP_TICKS + Math.min(30, this.late || 0);
  }

  receive(tick, now) {
    if (this.latest !== undefined && tick <= this.latest) return;
    // What was guessed while the wire was quiet has been lived: playback goes
    // on from there, not back over ticks the picture has already shown.
    if (this.ahead) { this.tick = Math.min(tick, this.tick + this.ahead); this.ahead = 0; }
    if (this.latest !== undefined && now - this.at < 2000)
      this.late = Math.max((this.late || 0) * 0.995, ((now - this.at) * HZ) / 1000 - (tick - this.latest));
    this.latest = tick;
    this.at = now;
    if (this.tick === undefined) {
      this.tick = tick - INTERP_TICKS;
      this.sampledAt = now;
    }
  }

  sample(now) {
    if (this.tick === undefined) return 0;
    const elapsed = Math.max(0, now - this.sampledAt) / 1000;
    this.sampledAt = now;
    const target = Math.min(
      this.latest,
      this.latest + ((now - this.at) * HZ) / 1000 - this.delay,
    );
    if (elapsed > 0.25 || target - this.tick > 20) {
      this.tick = Math.max(this.tick, target);
    } else {
      const rate = 1 + clamp((target - this.tick) * 0.1, -0.1, 0.1);
      this.tick = Math.min(this.latest, this.tick + elapsed * HZ * rate);
    }
    // 🕰️ Out of snapshots: how far past the newest the picture has run, for
    // net.mjs `interpolate` to carry a walk on with (a fifth of a second at
    // the most; after that it stands where it was guessed to).
    this.ahead = this.tick >= this.latest && elapsed <= 0.25 ? Math.min(AHEAD_TICKS, (this.ahead || 0) + elapsed * HZ) : 0;
    return this.tick;
  }
}

// A guess is put right over a moment, not at once: when the wire speaks again
// each body is where the snapshots say, and what the picture showed instead is
// kept as an offset that fades. Ground only; a new life or a long way is a cut.
export class RemoteEase {
  constructor() { this.seats = new Map(); }
  reset() { this.seats.clear(); }
  sample(seat, p, guessed, seconds) {
    if (!p) return void this.seats.delete(seat), p;
    let s = this.seats.get(seat);
    if (!s || s.life !== p.life) this.seats.set(seat, (s = { life: p.life, ex: 0, ez: 0, guessed: false, x: p.x, z: p.z }));
    const decay = Math.exp(-Math.max(0, seconds) / 0.12);
    s.ex *= decay; s.ez *= decay;
    if (s.guessed && !guessed) {
      const ex = s.x - p.x, ez = s.z - p.z;
      if (Math.hypot(ex, ez) < 2) { s.ex = ex; s.ez = ez; } else s.ex = s.ez = 0;
    }
    s.guessed = guessed;
    s.x = p.x + s.ex; s.z = p.z + s.ez;
    return s.ex || s.ez ? { ...p, x: s.x, z: s.z } : p;
  }
}
