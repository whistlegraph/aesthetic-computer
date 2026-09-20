// 🐾 Howl 2026.09.19
// The general animal call: a glottal source through a vowel tract, with a
// pitch that glides from `pitch` to `slide` across the sustain, a small
// onset overshoot, vibrato that fades in, and a little random drift so no
// two seconds are the same. Wolf by default; bray, roar and yelp are params.
import Voice from "./voice.mjs";
import { Glottis, Noise, Smoother, envelope, glide, clamp } from "./parts.mjs";
import { FormantBank, vowelBands } from "../formant.mjs";

const { sin, exp, pow, max, PI } = Math;
const GAIN = 1.3;

export default class Howl extends Voice {
  #glottis = new Glottis(3);
  #noise = new Noise(9);
  #drift = new Smoother(0.7);
  #tract;

  constructor(params, id) {
    super(
      params,
      id,
      { pitch: 220, slide: 330, vowel: "o", scale: 1, vibrato: 0.4, rasp: 0.1, attack: 0.08, release: 0.3, duration: 1.5 },
      ["scale", "attack", "release"],
    );
    this.#glottis.oq = 0.65;
    this.#tract = new FormantBank(this.#bands());
  }

  #bands() {
    return vowelBands(this.p.vowel, this.p.scale) || vowelBands("a", this.p.scale);
  }

  retune(props, duration) {
    if ("vowel" in props || "scale" in props) this.#tract.set(this.#bands());
    // A new pitch without a new slide is a new resting note, not a new glide.
    if ("pitch" in props && !("slide" in props)) this.update({ slide: props.pitch, duration });
  }

  render(t) {
    const p = this.p;
    const rasp = clamp(p.rasp, 0, 1);
    this.#glottis.jitter = 0.05 + rasp * 0.6;
    this.#glottis.sub = rasp * 0.45;

    const sustain = p.duration === Infinity ? 1 : max(0.05, p.duration - p.attack - p.release);
    let f = glide(p.pitch, p.slide, (t - p.attack) / sustain);
    const k = t / 0.05;
    f *= 1 + 0.05 * k * exp(1 - k); // overshoot peaking at 50 ms, gone by ~200
    const vib = clamp((t - 0.2) / 0.3, 0, 1) * p.vibrato * sin(2 * PI * 5.5 * t);
    const drift = this.#drift.next(this.#noise.next()) * 25;
    f *= pow(2, (vib + drift) / 12);

    const pulse = this.#glottis.next(f);
    const hiss = this.#noise.next() * this.#glottis.open;
    const src = pulse * (1 - 0.35 * rasp) + hiss * rasp * 0.5;
    const out = this.#tract.process(src) + src * 0.08; // a little raw source keeps the fundamental
    return out * envelope(t, p.attack, p.release, p.duration) * GAIN;
  }
}
