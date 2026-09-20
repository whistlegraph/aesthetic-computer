// 🐾 Chirp 2026.09.19
// `count` quick sine sweeps from `pitch` to `slide`, one every 1/rate
// seconds, each with a snap attack and a short decay. Ends after the last
// chirp or at `duration`, whichever comes first.
import Voice from "./voice.mjs";
import { Noise, glide, smooth } from "./parts.mjs";

const { sin, exp, floor, min, PI } = Math;
const GAIN = 0.6;

export default class Chirp extends Voice {
  #phase = 0;
  #noise = new Noise(13);

  constructor(params, id) {
    super(params, id, { pitch: 2500, slide: 4200, count: 3, rate: 8, duration: 0.5, noise: 0.05 }, ["count", "rate"]);
  }

  render(t) {
    const p = this.p;
    const gap = 1 / p.rate;
    const len = min(gap * 0.6, 0.09);
    const i = floor(t / gap);
    const tau = t - i * gap;
    if (i >= p.count || (i === p.count - 1 && tau >= len)) {
      this.playing = false;
      return 0;
    }
    if (tau >= len) return 0; // rest between chirps

    const f = glide(p.pitch, p.slide, tau / len);
    this.#phase += (2 * PI * f) / sampleRate;
    if (this.#phase > 2 * PI) this.#phase -= 2 * PI;

    const env = smooth(tau / 0.004) * exp(-tau / (len * 0.35)) * smooth((len - tau) / 0.005);
    return (sin(this.#phase) + this.#noise.next() * p.noise) * env * GAIN;
  }
}
