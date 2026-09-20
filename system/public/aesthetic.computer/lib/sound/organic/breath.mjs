// 🐾 Breath 2026.09.19
// Filtered noise with a lung-shaped envelope: an exhale rises fast and
// falls long, an inhale swells slowly and stops short. Pressure sets the
// level and opens the cutoff a little as the breath moves.
import Voice from "./voice.mjs";
import { Noise, Biquad, envelope, smooth, clamp } from "./parts.mjs";

const { exp, pow, abs, sqrt } = Math;
const GAIN = 0.8;

export default class Breath extends Voice {
  #noise = new Noise(5);
  #lp = new Biquad();
  #chest = new Biquad().bandpass(650, 2);
  #cutoff = 0; // Cutoff the lowpass was last built for.

  constructor(params, id) {
    super(params, id, { pressure: 0.6, cutoff: 1200, direction: "out", duration: 0.8 });
  }

  // The reference length: a held breath keeps the first 0.8 s of shape.
  #length() {
    return this.p.duration === Infinity ? 0.8 : this.p.duration;
  }

  #shape(t) {
    const L = this.#length();
    if (this.p.direction === "in") {
      const rise = 0.85 * L;
      return t < rise ? pow(smooth(t / rise), 1.5) : 1;
    }
    const rise = 0.12 * L;
    return t < rise ? smooth(t / rise) : 0.3 + 0.7 * exp(-(t - rise) / (0.35 * L));
  }

  render(t) {
    const p = this.p;
    const pressure = clamp(p.pressure, 0, 1);
    const e = this.#shape(t);

    const fc = p.cutoff * (1 + 0.3 * pressure * e);
    if (abs(fc - this.#cutoff) > this.#cutoff * 0.01) {
      this.#lp.lowpass(fc, 1.4);
      this.#cutoff = fc;
    }

    const n = this.#noise.next();
    const out = this.#lp.process(n) + this.#chest.process(n) * 0.5;
    const release = p.direction === "in" ? 0.05 : 0.15 * this.#length();
    // Noise gets louder as the lowpass opens (level ∝ √bandwidth), so a wide
    // cutoff is pulled back to the same headroom as the default.
    const even = fc > 1200 ? sqrt(1200 / fc) : 1;
    return out * e * envelope(t, 0, release, p.duration) * (0.3 + 0.7 * pressure) * even * GAIN;
  }
}
