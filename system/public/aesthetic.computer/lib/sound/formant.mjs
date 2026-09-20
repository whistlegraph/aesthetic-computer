// Shared formant filter bank — a few resonant bandpasses in parallel, the
// way a vocal tract shapes a buzzing source into a vowel. Used by synth.mjs
// (`formant` option) and by the organic generators, so the interface stays
// small: VOWELS, vowelBands(), FormantBank.
const { sin, cos, PI, max, min } = Math;

// Adult-male vowel formants (Peterson & Barney, 1952) — freq/bw in Hz, gain
// as amplitude. F1 carries the vowel; F2/F3 sit 6–16 dB under it.
export const VOWELS = {
  a: [{ freq: 730, bw: 80, gain: 1 }, { freq: 1090, bw: 90, gain: 0.5 }, { freq: 2440, bw: 120, gain: 0.25 }],
  e: [{ freq: 530, bw: 60, gain: 1 }, { freq: 1840, bw: 90, gain: 0.5 }, { freq: 2480, bw: 120, gain: 0.2 }],
  i: [{ freq: 270, bw: 60, gain: 1 }, { freq: 2290, bw: 90, gain: 0.35 }, { freq: 3010, bw: 100, gain: 0.2 }],
  o: [{ freq: 570, bw: 80, gain: 1 }, { freq: 840, bw: 90, gain: 0.5 }, { freq: 2410, bw: 120, gain: 0.2 }],
  u: [{ freq: 300, bw: 60, gain: 1 }, { freq: 870, bw: 90, gain: 0.35 }, { freq: 2240, bw: 120, gain: 0.15 }],
};

// `spec` is a vowel name or an array of bands. `scale` moves every formant
// together: < 1 lengthens the tract (bigger animal), > 1 shortens it.
export function vowelBands(spec, scale = 1) {
  const bands = typeof spec === "string" ? VOWELS[spec.toLowerCase()] : spec;
  if (!bands) return null;
  return bands.map(({ freq, bw, gain = 1 }) => ({ freq: freq * scale, bw, gain }));
}

export class FormantBank {
  #n = 0;
  #sr;
  #b0; #a1; #a2; // RBJ constant-peak bandpass: b1 = 0, b2 = -b0.
  #gain;
  #x1; #x2; #y1; #y2; // Direct Form I history, per band.
  #norm = 1;

  // `sr` defaults to the AudioWorklet global; pass one when running elsewhere.
  constructor(bands, sr = globalThis.sampleRate) {
    this.#sr = sr;
    this.set(bands);
  }

  set(bands) {
    const n = bands.length;
    if (n !== this.#n) {
      this.#n = n;
      this.#b0 = new Float64Array(n); this.#a1 = new Float64Array(n); this.#a2 = new Float64Array(n);
      this.#gain = new Float64Array(n);
      this.#x1 = new Float64Array(n); this.#x2 = new Float64Array(n);
      this.#y1 = new Float64Array(n); this.#y2 = new Float64Array(n);
    }
    const nyq = this.#sr * 0.45; // keep a scaled-up formant below the fold
    let peak = 0;
    for (let i = 0; i < n; i++) {
      const freq = min(max(bands[i].freq, 20), nyq);
      const bw = max(bands[i].bw, 1);
      const w0 = (2 * PI * freq) / this.#sr;
      const alpha = sin(w0) * (bw / freq) / 2; // Q = freq / bw
      const a0 = 1 + alpha;
      this.#b0[i] = alpha / a0;
      this.#a1[i] = (-2 * cos(w0)) / a0;
      this.#a2[i] = (1 - alpha) / a0;
      const gain = bands[i].gain ?? 1;
      this.#gain[i] = gain;
      peak = max(peak, gain);
    }
    // Each band passes its centre at unity, so the loudest band sets the
    // scale; bands are narrow enough that their overlap barely stacks.
    this.#norm = peak > 0 ? 1 / peak : 1;
  }

  process(x) {
    let out = 0;
    for (let i = 0; i < this.#n; i++) {
      const y = this.#b0[i] * (x - this.#x2[i]) - this.#a1[i] * this.#y1[i] - this.#a2[i] * this.#y2[i];
      this.#x2[i] = this.#x1[i]; this.#x1[i] = x;
      this.#y2[i] = this.#y1[i]; this.#y1[i] = y;
      out += y * this.#gain[i];
    }
    return out * this.#norm;
  }
}
