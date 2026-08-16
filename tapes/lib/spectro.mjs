// spectrogram codec — an image painted into the magnitude spectrum of audio,
// read back with an fft. deliberately the *dumb* scheme: no error correction,
// no phase, nothing that a lossy codec or a tape head can destroy. it is the
// floor we measure everything else against.

import { fft, spectrum, hann, blackmanHarris, bestLag } from "./fft.mjs";

const { cos, sin, PI, log10, pow, round, max, min, abs, floor } = Math;

export const defaults = {
  rate: 44100,
  n: 2048, // fft size
  hop: null, // defaults to n — no column shares audio with its neighbour
  loHz: 400,
  hiHz: 14000,
  spacing: 2, // bins per image row; the spare bin is a guard against leakage
  range: 48, // dB between black and white
  mode: "gray", // or "rgb", which spends three columns per image column
};

const binOf = (hz, { n, rate }) => round((hz * n) / rate);

// where each part of the signal lives, derived once so encode and decode agree.
export function plan(opts = {}) {
  const o = { ...defaults, ...opts };
  o.hop = o.hop ?? o.n;
  const lo = binOf(o.loHz, o),
    hi = binOf(o.hiHz, o);
  return {
    ...o,
    lo,
    hi,
    // far enough from the data that a peak search for a pilot can never
    // wander onto a tone and mistake it for one. the search span the decoder
    // uses must stay under this, and wow moves a top-band tone by
    // `wow × hiHz / binHz` bins — 0.7% at 14kHz is about 4.5.
    pilotLo: lo - (o.guard ?? 10),
    pilotHi: hi + (o.guard ?? 10),
    rows: floor((hi - lo) / o.spacing) + 1,
    colRate: o.rate / o.hop,
  };
}

// row 0 is the top of the picture, which is the top of the band.
const binOfRow = (r, p) => p.hi - r * p.spacing;

// deterministic per-bin start phase, so a frame isn't an impulse.
export function startPhase(k) {
  const x = sin(k * 12.9898) * 43758.5453;
  return (x - floor(x)) * 2 * PI;
}

// synthesis window: flat in the middle, cosine ramps as wide as the overlap.
// sums to a constant for any hop from n/2 up to n. at hop = n it is just a
// rectangle and each column gets a stretch of audio entirely to itself —
// which is what keeps a vertical edge from smearing into its neighbour.
function trapezoid(n, hop) {
  const ramp = n - hop;
  const w = new Float64Array(n).fill(1);
  for (let i = 0; i < ramp; i += 1) {
    const t = 0.5 - 0.5 * cos((PI * (i + 0.5)) / ramp);
    w[i] = t;
    w[n - 1 - i] = t;
  }
  return w;
}

const toMag = (v, p) => pow(10, ((min(1, max(0, v)) - 1) * p.range) / 20);
const toVal = (db, p) => min(1, max(0, 1 + db / p.range));

// 🎼 a sync chirp the decoder can find after a codec has shifted everything.
export function chirp(p) {
  const len = round(p.rate * (p.chirpSecs ?? 0.4)); // longer sweep, more gain
  const out = new Float64Array(len);
  const f0 = 400, // stays inside what a cassette and a 128k codec both pass,
    f1 = 11000; // or the picture survives a route that sync doesn't.
  for (let i = 0; i < len; i += 1) {
    const t = i / p.rate,
      k = (f1 - f0) / (len / p.rate);
    const w = 0.5 - 0.5 * cos((2 * PI * i) / len); // taper, or it clicks
    out[i] = w * sin(2 * PI * (f0 * t + (k * t * t) / 2));
  }
  return out;
}

// ── encode ────────────────────────────────────────────────────────────────

export function encode(img, opts = {}) {
  const p = plan(opts);
  if (img.height > p.rows)
    throw new Error(`image is ${img.height} rows; band holds ${p.rows}`);

  const cols = columnsOf(img, p);
  const body = new Float64Array((cols.length + 4) * p.hop + p.n);

  const w = trapezoid(p.n, p.hop);
  const re = new Float64Array(p.n),
    im = new Float64Array(p.n);
  const phase = new Float64Array(p.n / 2 + 1);
  for (let k = 0; k <= p.n / 2; k += 1) phase[k] = startPhase(k);

  cols.forEach((col, x) => {
    re.fill(0);
    im.fill(0);
    place(re, im, p.pilotLo, 1, phase, p.n);
    place(re, im, p.pilotHi, 1, phase, p.n);
    for (let r = 0; r < col.length; r += 1)
      place(re, im, binOfRow(r, p), toMag(col[r], p), phase, p.n);

    fft(re, im, true);
    const at = x * p.hop;
    for (let i = 0; i < p.n; i += 1) body[at + i] += re[i] * w[i];

    // advance every bin by the phase one hop implies, so overlapping frames
    // stay coherent and the tones come out clean instead of warbling.
    for (let k = 0; k <= p.n / 2; k += 1)
      phase[k] = (phase[k] + (2 * PI * p.hop * k) / p.n) % (2 * PI);
  });

  // the body gets its own headroom so the chirp can stay the loudest thing in
  // the file — sync should never have to compete with the picture.
  let peak = 0;
  for (let i = 0; i < body.length; i += 1) peak = max(peak, abs(body[i]));
  for (let i = 0; i < body.length; i += 1) body[i] = (body[i] / peak) * 0.7;

  const pre = chirp(p);
  const gap = round(p.rate * 0.05);
  const head = pre.length + gap;
  const out = new Float64Array(head + body.length);
  out.set(pre, 0);
  out.set(body, head);
  return { samples: out, plan: p, cols: cols.length, start: head };
}

// hermitian pair, scaled so a magnitude of 1 is a unit-amplitude sinusoid.
export function place(re, im, k, mag, phase, n) {
  const a = (mag * n) / 2;
  const c = a * cos(phase[k]),
    s = a * sin(phase[k]);
  re[k] += c;
  im[k] += s;
  re[n - k] += c;
  im[n - k] -= s;
}

function columnsOf(img, p) {
  const cols = [];
  const px = (x, y) => {
    const i = (y * img.width + x) * 4;
    return [img.data[i], img.data[i + 1], img.data[i + 2]];
  };
  for (let x = 0; x < img.width; x += 1) {
    if (p.mode === "rgb") {
      for (let c = 0; c < 3; c += 1) {
        const col = new Float64Array(img.height);
        for (let y = 0; y < img.height; y += 1) col[y] = px(x, y)[c] / 255;
        cols.push(col);
      }
    } else {
      const col = new Float64Array(img.height);
      for (let y = 0; y < img.height; y += 1) {
        const [r, g, b] = px(x, y);
        col[y] = (0.299 * r + 0.587 * g + 0.114 * b) / 255;
      }
      cols.push(col);
    }
  }
  return cols;
}

// ── decode ────────────────────────────────────────────────────────────────

export function decode(samples, { width, height, at, syncWide, ...opts } = {}) {
  const p = plan(opts);
  const start = at ?? sync(samples, p, syncWide);
  const per = p.mode === "rgb" ? 3 : 1;
  const data = new Uint8Array(width * height * 4).fill(255);
  const w = blackmanHarris(p.n);
  const frame = new Float64Array(p.n);

  for (let x = 0; x < width; x += 1) {
    for (let c = 0; c < per; c += 1) {
      const at = start + (x * per + c) * p.hop;
      if (at + p.n > samples.length) continue;
      for (let i = 0; i < p.n; i += 1) frame[i] = samples[at + i] * w[i];
      const mag = spectrum(frame);

      // the two pilots were sent flat; whatever tilt they came back with is
      // the channel's, so subtract it across the band.
      const gLo = db(mag[p.pilotLo]),
        gHi = db(mag[p.pilotHi]);
      const span = p.pilotHi - p.pilotLo;

      for (let y = 0; y < height; y += 1) {
        const k = binOfRow(y, p);
        const g = gLo + ((gHi - gLo) * (k - p.pilotLo)) / span;
        const v = toVal(db(mag[k]) - g, p);
        const i = (y * width + x) * 4;
        if (per === 1) data[i] = data[i + 1] = data[i + 2] = round(v * 255);
        else data[i + c] = round(v * 255);
      }
    }
  }
  return { width, height, data, plan: p, start };
}

const db = (m) => 20 * log10(max(m, 1e-12));

// find the chirp, then step past it and the gap to the first column.
function sync(samples, p, wide = false) {
  const ref = chirp(p);
  const window = samples.subarray(0, min(samples.length, p.rate * 3));
  const lag = bestLag(window, ref, { wide });
  return lag + ref.length + round(p.rate * 0.05);
}
