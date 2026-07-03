#!/usr/bin/env node
// analyze.mjs — WAV → spectral-feature JSON for novelizer voice renders.
//
//   node bin/analyze.mjs out/myvoice-rising.wav [more.wav ...]
//
// Prints one JSON object per file: level stats, zero-cross rate, and
// frame-averaged spectral features (centroid, spread, rolloff, flux,
// flatness) plus an f0 + inharmonicity estimate from the loudest frame.
// Zero deps; mono or first-channel of 16-bit PCM WAVs.

import { readFileSync } from "node:fs";

const FRAME = 2048;
const HOP = 1024;

function readWav(path) {
  const b = readFileSync(path);
  if (b.toString("ascii", 0, 4) !== "RIFF" || b.toString("ascii", 8, 12) !== "WAVE")
    throw new Error(`${path}: not a RIFF/WAVE file`);
  let off = 12, sr = 0, ch = 1, bits = 16, data = null;
  while (off + 8 <= b.length) {
    const id = b.toString("ascii", off, off + 4);
    const len = b.readUInt32LE(off + 4);
    if (id === "fmt ") {
      ch = b.readUInt16LE(off + 10);
      sr = b.readUInt32LE(off + 12);
      bits = b.readUInt16LE(off + 22);
    } else if (id === "data") {
      data = b.subarray(off + 8, off + 8 + len);
    }
    off += 8 + len + (len & 1);
  }
  if (!data) throw new Error(`${path}: no data chunk`);
  if (bits !== 16) throw new Error(`${path}: only 16-bit PCM supported (got ${bits})`);
  const n = Math.floor(data.length / 2 / ch);
  const out = new Float64Array(n);
  for (let i = 0; i < n; i++) out[i] = data.readInt16LE(i * 2 * ch) / 32768;
  return { samples: out, sr };
}

// iterative radix-2 FFT, in-place on re/im
function fft(re, im) {
  const n = re.length;
  for (let i = 1, j = 0; i < n; i++) {
    let bit = n >> 1;
    for (; j & bit; bit >>= 1) j ^= bit;
    j ^= bit;
    if (i < j) { [re[i], re[j]] = [re[j], re[i]]; [im[i], im[j]] = [im[j], im[i]]; }
  }
  for (let len = 2; len <= n; len <<= 1) {
    const ang = (-2 * Math.PI) / len;
    const wr = Math.cos(ang), wi = Math.sin(ang);
    for (let i = 0; i < n; i += len) {
      let cr = 1, ci = 0;
      for (let k = 0; k < len / 2; k++) {
        const j = i + k, l = i + k + len / 2;
        const tr = re[l] * cr - im[l] * ci;
        const ti = re[l] * ci + im[l] * cr;
        re[l] = re[j] - tr; im[l] = im[j] - ti;
        re[j] += tr; im[j] += ti;
        const ncr = cr * wr - ci * wi;
        ci = cr * wi + ci * wr; cr = ncr;
      }
    }
  }
}

const hann = new Float64Array(FRAME);
for (let i = 0; i < FRAME; i++) hann[i] = 0.5 - 0.5 * Math.cos((2 * Math.PI * i) / (FRAME - 1));

function mag(frame) {
  const re = Float64Array.from(frame, (v, i) => v * hann[i]);
  const im = new Float64Array(FRAME);
  fft(re, im);
  const m = new Float64Array(FRAME / 2);
  for (let i = 0; i < FRAME / 2; i++) m[i] = Math.hypot(re[i], im[i]);
  return m;
}

function f0Autocorr(frame, sr) {
  const n = frame.length;
  const minLag = Math.floor(sr / 2000), maxLag = Math.floor(sr / 40);
  let e0 = 0;
  for (let i = 0; i < n; i++) e0 += frame[i] * frame[i];
  if (e0 < 1e-9) return 0;
  const top = Math.min(maxLag, n - 1);
  const corr = new Float64Array(top + 1);
  let best = 0;
  for (let lag = minLag; lag <= top; lag++) {
    let s = 0;
    for (let i = 0; i < n - lag; i++) s += frame[i] * frame[i + lag];
    corr[lag] = s / (n - lag); // normalize so long (low-f0) lags compete fairly
    if (corr[lag] > best) best = corr[lag];
  }
  if (best / (e0 / n) < 0.3) return 0; // unvoiced / noisy
  // every multiple of the true period correlates ~equally; take the
  // smallest LOCAL-MAX lag within tolerance of the max so neither
  // subharmonic multiples nor near-peak slopes win
  for (let lag = minLag + 1; lag < top; lag++)
    if (corr[lag] >= 0.95 * best && corr[lag] >= corr[lag - 1] && corr[lag] >= corr[lag + 1])
      return sr / lag;
  return 0;
}

function analyze(path) {
  const { samples, sr } = readWav(path);
  const n = samples.length;
  let peak = 0, sq = 0, zc = 0;
  for (let i = 0; i < n; i++) {
    const a = Math.abs(samples[i]);
    if (a > peak) peak = a;
    sq += samples[i] * samples[i];
    if (i > 0 && samples[i - 1] < 0 !== samples[i] < 0) zc++;
  }
  const rms = Math.sqrt(sq / n);

  const cents = [], flats = [], rolls = [], fluxes = [];
  let prev = null, loudest = null, loudestE = -1;
  for (let off = 0; off + FRAME <= n; off += HOP) {
    const frame = samples.subarray(off, off + FRAME);
    let e = 0;
    for (let i = 0; i < FRAME; i++) e += frame[i] * frame[i];
    if (e < 1e-7) { prev = null; continue; } // skip silence
    const m = mag(frame);
    if (e > loudestE) { loudestE = e; loudest = frame; }
    let sum = 0, wsum = 0, logsum = 0, cnt = 0;
    for (let i = 1; i < m.length; i++) {
      sum += m[i];
      wsum += m[i] * ((i * sr) / FRAME);
      logsum += Math.log(m[i] + 1e-12);
      cnt++;
    }
    if (sum <= 0) continue;
    cents.push(wsum / sum);
    flats.push(Math.exp(logsum / cnt) / (sum / cnt)); // geometric/arithmetic
    let acc = 0, roll = 0;
    for (let i = 1; i < m.length; i++) { acc += m[i]; if (acc >= 0.85 * sum) { roll = (i * sr) / FRAME; break; } }
    rolls.push(roll);
    if (prev) {
      let fx = 0;
      for (let i = 1; i < m.length; i++) { const d = m[i] - prev[i]; if (d > 0) fx += d; }
      fluxes.push(fx / sum);
    }
    prev = m;
  }
  const mean = (a) => (a.length ? a.reduce((x, y) => x + y, 0) / a.length : 0);
  const std = (a) => { const mu = mean(a); return Math.sqrt(mean(a.map((v) => (v - mu) ** 2))); };

  // inharmonicity: from the loudest frame, measure how far spectral peaks
  // sit from integer multiples of the autocorrelation f0 (0 = harmonic).
  let f0 = 0, inharm = null;
  if (loudest) {
    f0 = f0Autocorr(loudest, sr);
    if (f0 > 0) {
      const m = mag(loudest);
      const binHz = sr / FRAME;
      let devSum = 0, k = 0;
      for (let h = 1; h <= 10; h++) {
        const target = f0 * h;
        if (target > sr / 2 - binHz * 4) break;
        const c = Math.round(target / binHz);
        let bi = c, bm = -1;
        for (let i = Math.max(1, c - 3); i <= Math.min(m.length - 1, c + 3); i++)
          if (m[i] > bm) { bm = m[i]; bi = i; }
        devSum += Math.abs(bi * binHz - target) / target;
        k++;
      }
      inharm = k ? devSum / k : null;
    }
  }

  return {
    file: path,
    sr,
    seconds: +(n / sr).toFixed(3),
    peak: +peak.toFixed(4),
    rmsDb: +(20 * Math.log10(rms + 1e-12)).toFixed(2),
    crestDb: +(20 * Math.log10(peak / (rms + 1e-12))).toFixed(2),
    zeroCrossHz: +((zc / (n / sr)) / 2).toFixed(1),
    centroidHz: +mean(cents).toFixed(1),
    centroidStdHz: +std(cents).toFixed(1),
    rolloff85Hz: +mean(rolls).toFixed(1),
    fluxMean: +mean(fluxes).toFixed(4),
    fluxStd: +std(fluxes).toFixed(4),
    flatness: +mean(flats).toFixed(4), // 0 tonal .. 1 noise
    f0LoudestHz: +f0.toFixed(2),
    inharmonicity: inharm === null ? null : +inharm.toFixed(4),
  };
}

const files = process.argv.slice(2);
if (!files.length) {
  console.error("usage: node bin/analyze.mjs <file.wav> [...]");
  process.exit(2);
}
for (const f of files) console.log(JSON.stringify(analyze(f)));
