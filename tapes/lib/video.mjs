// moving pictures. the picture codec spends one audio column per image column,
// which caps you under 1 fps. video instead treats the whole tone grid as a
// pipe: flatten every frame to a stream of greys and pour it through, so all
// ~632 bins carry picture instead of the handful one frame row would need.
//
// the ceiling is the gabor limit — about rate/2 independent cells per second.
// at 44.1k with hop = n that is 632 rows × 21.5 col/s ≈ 13,600 greys/sec, and
// fps = 13,600 / (w × h). everything here is arithmetic against that number.

import { fft, spectrum, blackmanHarris } from "./fft.mjs";
import { plan, chirp, startPhase, place } from "./spectro.mjs";

const { cos, sin, PI, log10, pow, round, max, min, abs, floor } = Math;

export const videoDefaults = { spacing: 1, range: 42, win: "rect" };

export const budget = (opts = {}) => {
  const p = plan({ ...videoDefaults, ...opts });
  return { cells: p.rows * p.colRate, rows: p.rows, colRate: p.colRate };
};

export const fpsFor = (w, h, opts) => budget(opts).cells / (w * h);

const toMag = (v, p) => pow(10, ((min(1, max(0, v)) - 1) * p.range) / 20);
const toVal = (dbv, p) => min(1, max(0, 1 + dbv / p.range));
const db = (m) => 20 * log10(max(m, 1e-12));

// frames: array of Float64Array, each w*h greys in 0..1, raster order.
export function encode(frames, opts = {}) {
  const p = plan({ ...videoDefaults, ...opts });
  const values = new Float64Array(frames.length * frames[0].length);
  frames.forEach((f, i) => values.set(f, i * f.length));

  const cols = Math.ceil(values.length / p.rows);
  const body = new Float64Array((cols + 2) * p.hop + p.n);
  const re = new Float64Array(p.n),
    im = new Float64Array(p.n);
  const phase = new Float64Array(p.n / 2 + 1);
  for (let k = 0; k <= p.n / 2; k += 1) phase[k] = startPhase(k);

  for (let x = 0; x < cols; x += 1) {
    re.fill(0);
    im.fill(0);
    place(re, im, p.pilotLo, 1, phase, p.n);
    place(re, im, p.pilotHi, 1, phase, p.n);
    for (let r = 0; r < p.rows; r += 1) {
      const v = values[x * p.rows + r] ?? 0;
      place(re, im, p.hi - r * p.spacing, toMag(v, p), phase, p.n);
    }
    fft(re, im, true);
    for (let i = 0; i < p.n; i += 1) body[x * p.hop + i] += re[i];
    for (let k = 0; k <= p.n / 2; k += 1)
      phase[k] = (phase[k] + (2 * PI * p.hop * k) / p.n) % (2 * PI);
  }

  let peak = 0;
  for (let i = 0; i < body.length; i += 1) peak = max(peak, abs(body[i]));
  for (let i = 0; i < body.length; i += 1) body[i] = (body[i] / peak) * 0.7;

  const pre = chirp(p);
  const gap = round(p.rate * 0.05);
  const out = new Float64Array(pre.length + gap + body.length);
  out.set(pre, 0);
  out.set(body, pre.length + gap);
  return {
    samples: out,
    plan: p,
    start: pre.length + gap,
    cols,
    secs: out.length / p.rate,
  };
}

export function decode(samples, { at, count, size, ...opts } = {}) {
  const p = plan({ ...videoDefaults, ...opts });
  const w = p.win === "rect" ? new Float64Array(p.n).fill(1) : blackmanHarris(p.n);
  const frame = new Float64Array(p.n);
  const need = count * size;
  const values = new Float64Array(need);

  let pos = at,
    scale = 1,
    at_ = 0;
  const span = p.pilotHi - p.pilotLo;

  while (at_ < need) {
    const off = round(pos);
    if (off < 0 || off + p.n > samples.length) break;
    for (let i = 0; i < p.n; i += 1) frame[i] = samples[off + i] * w[i];
    const mag = spectrum(frame);

    // pilots say how fast the playback ran; their positions give both the
    // frequency stretch and, through it, the block length.
    const lo = peakNear(mag, round(p.pilotLo * scale), p.span ?? 3),
      hi = peakNear(mag, round(p.pilotHi * scale), p.span ?? 3);
    const s = (hi - lo) / span;
    if (isFinite(s) && s > 0.9 && s < 1.1) scale = scale * 0.6 + s * 0.4;
    const gLo = db(mag[round(lo)]),
      gHi = db(mag[round(hi)]);

    for (let r = 0; r < p.rows && at_ < need; r += 1) {
      const k = p.hi - r * p.spacing;
      const g = gLo + ((gHi - gLo) * (k - p.pilotLo)) / span;
      values[at_] = toVal(db(lerp(mag, lo + (k - p.pilotLo) * scale)) - g, p);
      at_ += 1;
    }
    pos += p.hop / scale;
  }

  const frames = [];
  for (let i = 0; i < count; i += 1)
    frames.push(values.subarray(i * size, (i + 1) * size));
  return frames;
}

function peakNear(mag, k, span) {
  let best = k,
    peak = -1;
  for (let i = k - span; i <= k + span; i += 1)
    if ((mag[i] ?? 0) > peak) {
      peak = mag[i];
      best = i;
    }
  const a = mag[best - 1] ?? 0,
    b = mag[best],
    c = mag[best + 1] ?? 0;
  const d = a - 2 * b + c;
  return d === 0 ? best : best + (0.5 * (a - c)) / d;
}

function lerp(mag, at) {
  const i = floor(at),
    f = at - i;
  return (mag[i] ?? 0) * (1 - f) + (mag[i + 1] ?? 0) * f;
}

void cos;
void sin;
