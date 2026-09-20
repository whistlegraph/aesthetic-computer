// 🐾 Growl 2026.09.19
// A low, rough glottal buzz in a big chest: pulse train plus aspiration
// through an "o" body scaled by size, a fast irregular tremor on top, and
// a pitch that settles downward as the animal leans into it.
import Voice from "./voice.mjs";
import { Glottis, Noise, Smoother, Biquad, envelope, clamp } from "./parts.mjs";
import { FormantBank, vowelBands } from "../formant.mjs";

const { sin, exp, PI } = Math;
const GAIN = 1.1;

export default class Growl extends Voice {
  #glottis = new Glottis(7);
  #noise = new Noise(11);
  #wander = new Smoother(2); // Slow drift of the tremor rate.
  #body;
  #chest = new Biquad(); // Keeps the fundamental the formants can't pass.
  #tremor = 0;

  constructor(params, id) {
    super(params, id, { pitch: 55, rasp: 0.6, size: 1, tremor: 0.5, duration: 1.2 }, ["size"]);
    this.#glottis.oq = 0.5;
    this.#body = new FormantBank(vowelBands("o", 1 / this.p.size));
    this.#chest.lowpass(220 / this.p.size, 1.2);
  }

  retune(props) {
    if (!("size" in props)) return;
    this.#body.set(vowelBands("o", 1 / this.p.size));
    this.#chest.lowpass(220 / this.p.size, 1.2);
  }

  render(t) {
    const p = this.p;
    const rasp = clamp(p.rasp, 0, 1);
    this.#glottis.jitter = rasp;
    this.#glottis.sub = rasp * 0.5;

    const f = p.pitch * (1 + 0.18 * exp(-t / 0.08)); // settles in ~200 ms
    const pulse = this.#glottis.next(f);
    const hiss = this.#noise.next() * this.#glottis.open;
    const src = pulse * (1 - 0.4 * rasp) + hiss * rasp * 0.7;

    const rate = 25 + this.#wander.next(this.#noise.next()) * 400;
    this.#tremor += (2 * PI * rate) / sampleRate;
    const trem = 1 - clamp(p.tremor, 0, 1) * 0.6 * (0.5 + 0.5 * sin(this.#tremor));

    const out = this.#body.process(src) * 0.7 + this.#chest.process(src) * 0.6;
    return out * trem * envelope(t, 0.06, 0.25, p.duration) * GAIN;
  }
}
