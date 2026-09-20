// 🐾 Organic parts 2026.09.19
// The small DSP pieces every organic voice is built from. All of them read
// the bare AudioWorklet `sampleRate` global, like the other voices do.
const { sin, cos, exp, log, PI, min, max } = Math;

export const clamp = (x, lo, hi) => (x < lo ? lo : x > hi ? hi : x);
export const smooth = (x) => (x <= 0 ? 0 : x >= 1 ? 1 : x * x * (3 - 2 * x));

// Exponential glide from `a` to `b` — equal ratio per step, so pitch moves
// the way an ear hears it. `u` runs 0 → 1.
export function glide(a, b, u) {
  if (u <= 0) return a;
  if (u >= 1) return b;
  return a * exp(u * log(b / a));
}

// Attack / sustain / release in seconds. Sustain is flat at 1; the release
// leans on `duration`, so an Infinity duration never releases (kill does).
export function envelope(t, attack, release, duration) {
  const a = attack > 0 ? smooth(t / attack) : 1;
  const r = duration === Infinity || release <= 0 ? 1 : smooth((duration - t) / release);
  return a * r;
}

// xorshift32 — deterministic, so the same call sounds the same twice.
export class Noise {
  #s;
  constructor(seed = 0x9e3779b9) {
    this.#s = seed >>> 0 || 1;
  }
  next() {
    let x = this.#s;
    x ^= x << 13; x >>>= 0;
    x ^= x >>> 17;
    x ^= x << 5; x >>>= 0;
    this.#s = x;
    return x / 2147483648 - 1;
  }
}

// One-pole lowpass. As a smoother it turns steps into slopes; fed noise it
// makes the slow wander that keeps a pitch from sounding machined.
export class Smoother {
  #a;
  y = 0;
  constructor(cutoff) {
    this.#a = 1 - exp((-2 * PI * cutoff) / sampleRate);
  }
  next(x) {
    this.y += this.#a * (x - this.y);
    return this.y;
  }
}

// RBJ biquad, Direct Form I. Only the two shapes the voices need.
export class Biquad {
  #b0 = 1; #b1 = 0; #b2 = 0; #a1 = 0; #a2 = 0;
  #x1 = 0; #x2 = 0; #y1 = 0; #y2 = 0;

  lowpass(freq, q = 0.707) {
    const w0 = (2 * PI * min(freq, sampleRate * 0.45)) / sampleRate;
    const alpha = sin(w0) / (2 * q);
    const c = cos(w0);
    const a0 = 1 + alpha;
    this.#b0 = (1 - c) / 2 / a0;
    this.#b1 = (1 - c) / a0;
    this.#b2 = this.#b0;
    this.#a1 = (-2 * c) / a0;
    this.#a2 = (1 - alpha) / a0;
    return this;
  }

  bandpass(freq, q = 1) {
    const w0 = (2 * PI * min(freq, sampleRate * 0.45)) / sampleRate;
    const alpha = sin(w0) / (2 * q);
    const a0 = 1 + alpha;
    this.#b0 = alpha / a0;
    this.#b1 = 0;
    this.#b2 = -this.#b0;
    this.#a1 = (-2 * cos(w0)) / a0;
    this.#a2 = (1 - alpha) / a0;
    return this;
  }

  process(x) {
    const y = this.#b0 * x + this.#b1 * this.#x1 + this.#b2 * this.#x2
      - this.#a1 * this.#y1 - this.#a2 * this.#y2;
    this.#x2 = this.#x1; this.#x1 = x;
    this.#y2 = this.#y1; this.#y1 = y;
    return y;
  }
}

// Glottal source — the derivative of a Rosenberg flow pulse, which is what
// the vocal tract actually hears: a soft rise while the folds open and a
// sharp negative spike when they snap shut. Peak is ±1, mean is zero.
// `jitter` perturbs each period (rasp), `sub` weakens every other pulse so
// the pitch reads an octave down and rough (growl, roar).
export class Glottis {
  #phase = 0;
  #noise;
  #period = 1; // Per-cycle pitch multiplier, redrawn at each closure.
  #cycle = 0;
  open = 0; // 0..1 — how far the folds are apart, for aspiration noise.
  oq = 0.6; // Open quotient: fraction of the period the folds are open.
  jitter = 0;
  sub = 0;

  constructor(seed = 1) {
    this.#noise = new Noise(seed);
  }

  next(freq) {
    const tp = this.oq * 0.7; // Opening ends here, closing ends at oq.
    const tn = this.oq - tp;
    const p = this.#phase;
    let out;
    if (p < tp) {
      const s = sin((PI * p) / tp);
      this.open = 0.5 * (1 - cos((PI * p) / tp));
      out = (tn / tp) * s;
    } else if (p < this.oq) {
      const q = (p - tp) / tn;
      this.open = cos((PI * q) / 2);
      out = -sin((PI * q) / 2);
    } else {
      this.open = 0;
      out = 0;
    }
    if (this.#cycle & 1) out *= 1 - this.sub;

    this.#phase += (freq * this.#period) / sampleRate;
    if (this.#phase >= 1) {
      this.#phase -= 1;
      this.#cycle += 1;
      this.#period = 1 + this.jitter * 0.08 * this.#noise.next();
    }
    return out;
  }
}
