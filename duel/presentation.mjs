import { clamp, HZ, INTERP_TICKS } from './world.mjs';

const axes = ['x', 'y', 'z'];

// Presentation alone runs between simulation ticks. Never feed these positions
// back into prediction, collision, commands or authoritative hit detection.
export class LocalPresentation {
  reset(player) {
    this.current = this.previous = player;
    this.error = { x: 0, y: 0, z: 0 };
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
  }

  receive(tick, now) {
    if (this.latest !== undefined && tick <= this.latest) return;
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
      this.latest + ((now - this.at) * HZ) / 1000 - INTERP_TICKS,
    );
    if (elapsed > 0.25 || target - this.tick > 20) {
      this.tick = Math.max(this.tick, target);
    } else {
      const rate = 1 + clamp((target - this.tick) * 0.1, -0.1, 0.1);
      this.tick = Math.min(this.latest, this.tick + elapsed * HZ * rate);
    }
    return this.tick;
  }
}
