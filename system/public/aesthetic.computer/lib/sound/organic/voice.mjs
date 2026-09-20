// 🐾 Organic voice 2026.09.19
// What every organic generator shares: the lifecycle the worklet mixer
// expects (`playing`, `fading`, `next`, `pan`, `kill`, `update`), a clock in
// seconds, and numeric params that glide when updated. A generator extends
// this and writes one method: `render(t)` → a sample before volume.

export default class Voice {
  playing = true;
  fading = false;
  fadeGain = 1; // The mixer reads `volume` and `fadeGain` for auto-mixing.
  id;
  p; // Current params — the glided values a `render` reads.
  t = 0; // Seconds since the voice began.
  dt = 1 / sampleRate;

  #glides = []; // { key, step, left }
  #fadeStep = 0;
  #stiff; // Param names that snap instead of glide.

  constructor(params, id, defaults, stiff = []) {
    this.id = id;
    this.p = { volume: 1, pan: 0, ...defaults };
    for (const k in params) if (params[k] !== undefined) this.p[k] = params[k];
    if (this.p.duration === "🔁") this.p.duration = Infinity;
    this.#stiff = new Set(["duration", ...stiff]);
  }

  get volume() {
    return this.p.volume;
  }

  // Numeric params glide over `duration` seconds; anything else (a vowel, a
  // direction) snaps, then `retune` lets the generator react.
  update({ duration = 0.1, ...props }) {
    const n = Math.max(1, Math.round(duration * sampleRate));
    for (const key in props) {
      const v = props[key];
      if (v === undefined) continue;
      if (typeof v === "number" && typeof this.p[key] === "number" && !this.#stiff.has(key)) {
        this.#glides = this.#glides.filter((g) => g.key !== key);
        this.#glides.push({ key, step: (v - this.p[key]) / n, left: n });
      } else this.p[key] = v;
    }
    this.retune?.(props, duration);
  }

  next() {
    if (!this.playing) return 0;

    for (let i = this.#glides.length - 1; i >= 0; i--) {
      const g = this.#glides[i];
      this.p[g.key] += g.step;
      if (--g.left <= 0) this.#glides.splice(i, 1);
    }

    let out = this.render(this.t) * this.p.volume;
    this.t += this.dt;

    if (this.fading) {
      this.fadeGain -= this.#fadeStep;
      if (this.fadeGain <= 0) {
        this.fadeGain = 0;
        this.fading = false;
        this.playing = false;
        return 0;
      }
      out *= this.fadeGain;
    } else if (this.t >= this.p.duration) {
      this.playing = false;
      return 0;
    }

    // Generators scale for headroom; this only catches a wild param.
    return out > 1 ? 1 : out < -1 ? -1 : out;
  }

  pan(channel, frame) {
    const p = this.p.pan;
    if (channel === 0 && p > 0) return frame * (1 - p);
    if (channel === 1 && p < 0) return frame * (1 + p);
    return frame;
  }

  kill(fade = 0.025) {
    if (!fade) {
      this.playing = false;
      return;
    }
    this.fading = true;
    this.#fadeStep = this.fadeGain / (fade * sampleRate);
  }
}
