#!/usr/bin/env node
// diff-spectrum.mjs — whole-track spectral + dynamic analysis comparing
// JS vs C pre.wav renders. Reports per-band RMS, peak distribution,
// LUFS-ish loudness, and which audio character properties match/diverge.
//
// Per-track waveform analysis (not per-bar like diff-per-bar.mjs).
// Spectral analysis uses 4 octave bands derived from a simple cascade
// of one-pole filters (no FFT needed; faster, sufficient resolution).
//
// Usage:  node diff-spectrum.mjs                       (default paths)
//         node diff-spectrum.mjs <js.wav> <c.wav>

import { readFileSync } from "node:fs";

const SR = 48_000;
const JS_PATH = process.argv[2] || "/Users/jas/Documents/Shelf/hellsine/.hellsine-js-pre.wav";
const C_PATH  = process.argv[3] || "/Users/jas/Documents/Shelf/hellsine/.hellsine-c-pre.wav";

function readWav(p) {
  const buf = readFileSync(p);
  let pos = 12, fmtCode = 0, channels = 2, bits = 32;
  while (pos < buf.length - 8) {
    const id = buf.toString("ascii", pos, pos + 4);
    const sz = buf.readUInt32LE(pos + 4);
    if (id === "fmt ") {
      fmtCode = buf.readUInt16LE(pos + 8);
      channels = buf.readUInt16LE(pos + 10);
      bits = buf.readUInt16LE(pos + 22);
    } else if (id === "data") {
      const bytesPerSample = bits / 8;
      const stride = bytesPerSample * channels;
      const N = Math.floor(sz / stride);
      const L = new Float32Array(N), R = new Float32Array(N);
      for (let i = 0; i < N; i++) {
        const base = pos + 8 + i * stride;
        if (fmtCode === 3 && bits === 32) {
          L[i] = buf.readFloatLE(base);
          R[i] = buf.readFloatLE(base + 4);
        } else if (bits === 16) {
          L[i] = buf.readInt16LE(base) / 32768;
          R[i] = buf.readInt16LE(base + 2) / 32768;
        } else if (bits === 24) {
          const r24 = (o) => {
            const b0 = buf.readUInt8(o), b1 = buf.readUInt8(o + 1);
            const b2 = buf.readInt8(o + 2);
            return (b0 | (b1 << 8) | (b2 << 16)) / 8388608;
          };
          L[i] = r24(base);
          R[i] = r24(base + 3);
        } else {
          throw new Error(`unsupported wav fmt=${fmtCode} bits=${bits} (${p})`);
        }
      }
      return { L, R, N };
    }
    pos += 8 + sz;
  }
}

// Simple state-variable filter for band analysis — for a given cutoff/Q
// give back lowpass + highpass simultaneously. Used to split into bands.
function bandRMS(buf, lo, hi) {
  // Naive: HPF at lo + LPF at hi via biquads (1-pole cascade for speed)
  const w1 = Math.tan(Math.PI * lo / SR);
  const w2 = Math.tan(Math.PI * hi / SR);
  const a1 = (1 - w1) / (1 + w1);
  const a2 = w2 / (1 + w2);
  let yHP = 0, prev = 0;
  let yLP = 0;
  let sumSq = 0; let n = 0;
  for (let i = 0; i < buf.length; i++) {
    // 1-pole HP at lo
    yHP = a1 * (yHP + buf[i] - prev);
    prev = buf[i];
    // 1-pole LP at hi (on the HP output)
    yLP = yLP + a2 * (yHP - yLP);
    sumSq += yLP * yLP;
    n++;
  }
  return Math.sqrt(sumSq / n);
}

function fullRMS(buf) {
  let s = 0;
  for (let i = 0; i < buf.length; i++) s += buf[i] * buf[i];
  return Math.sqrt(s / buf.length);
}
function peak(buf) {
  let p = 0;
  for (let i = 0; i < buf.length; i++) {
    const a = Math.abs(buf[i]);
    if (Number.isFinite(a) && a > p) p = a;
  }
  return p;
}
function lufsIsh(L, R) {
  // K-weighting approximation: shelving boost above 1.5 kHz + HPF at 60.
  // Not real ITU-1770 but gives a useable comparison number.
  let sumSq = 0; let n = 0;
  let xL1 = 0, yL1 = 0, xR1 = 0, yR1 = 0;
  const hpA = 0.997;     // 1-pole HP at ~38 Hz @ 48k
  for (let i = 0; i < L.length; i++) {
    const xl = L[i], xr = R[i];
    if (!Number.isFinite(xl) || !Number.isFinite(xr)) continue;
    yL1 = hpA * (yL1 + xl - xL1); xL1 = xl;
    yR1 = hpA * (yR1 + xr - xR1); xR1 = xr;
    sumSq += yL1 * yL1 + yR1 * yR1;
    n++;
  }
  const ms = sumSq / (2 * n);
  const lufs = -0.691 + 10 * Math.log10(ms + 1e-12);
  return lufs;
}

console.log(`reading ${JS_PATH}...`);
const js = readWav(JS_PATH);
console.log(`reading ${C_PATH}...`);
const c  = readWav(C_PATH);

const min = Math.min(js.N, c.N);
// Strip NaN for fair RMS (JS has a known NaN bug at climax/coda)
function clean(buf) {
  const out = new Float32Array(min);
  for (let i = 0; i < min; i++) out[i] = Number.isFinite(buf[i]) ? buf[i] : 0;
  return out;
}
const jsL = clean(js.L), jsR = clean(js.R);
const cL  = clean(c.L),  cR  = clean(c.R);
const jsM = new Float32Array(min), cM = new Float32Array(min);
for (let i = 0; i < min; i++) {
  jsM[i] = (jsL[i] + jsR[i]) * 0.5;
  cM[i]  = (cL[i]  + cR[i])  * 0.5;
}

console.log("");
console.log("─".repeat(80));
console.log("WHOLE-TRACK STATISTICS");
console.log("─".repeat(80));

const jsPk = Math.max(peak(jsL), peak(jsR));
const cPk  = Math.max(peak(cL),  peak(cR));
const jsRMS = (fullRMS(jsL) + fullRMS(jsR)) * 0.5;
const cRMS  = (fullRMS(cL)  + fullRMS(cR))  * 0.5;
const jsLUFS = lufsIsh(jsL, jsR);
const cLUFS  = lufsIsh(cL,  cR);
const jsCrest = jsPk / jsRMS;
const cCrest  = cPk  / cRMS;

console.log(`           JS         C        ratio  ΔdB`);
const fmt = (a, b) => `  ratio ${(b/a).toFixed(3).padStart(6)}  ΔdB ${(20*Math.log10(b/a)).toFixed(2).padStart(6)}`;
console.log(`peak       ${jsPk.toFixed(4)}    ${cPk.toFixed(4)}${fmt(jsPk, cPk)}`);
console.log(`rms        ${jsRMS.toFixed(4)}    ${cRMS.toFixed(4)}${fmt(jsRMS, cRMS)}`);
console.log(`LUFS-ish   ${jsLUFS.toFixed(2)}    ${cLUFS.toFixed(2)}      Δ ${(cLUFS - jsLUFS).toFixed(2)} dB`);
console.log(`crest      ${jsCrest.toFixed(2)}     ${cCrest.toFixed(2)}    (peak/rms ratio — higher = more dynamic)`);

// Stereo width (mid/side ratio)
let jsMidSq = 0, jsSideSq = 0, cMidSq = 0, cSideSq = 0;
for (let i = 0; i < min; i++) {
  const jsm = (jsL[i] + jsR[i]) * 0.5, jss = (jsL[i] - jsR[i]) * 0.5;
  const cm  = (cL[i]  + cR[i])  * 0.5, cs  = (cL[i]  - cR[i])  * 0.5;
  jsMidSq += jsm * jsm; jsSideSq += jss * jss;
  cMidSq  += cm  * cm;  cSideSq  += cs  * cs;
}
const jsWidth = Math.sqrt(jsSideSq / jsMidSq);
const cWidth  = Math.sqrt(cSideSq  / cMidSq);
console.log(`stereo W   ${jsWidth.toFixed(3)}    ${cWidth.toFixed(3)}   (side/mid ratio — higher = wider)`);

console.log("");
console.log("─".repeat(80));
console.log("PER-BAND RMS (mono sum)");
console.log("─".repeat(80));
console.log(`band              JS rms     C rms     ratio   ΔdB    verdict`);

const bands = [
  { name: "sub        20-80",   lo: 20,   hi: 80 },
  { name: "low        80-250",  lo: 80,   hi: 250 },
  { name: "low-mid    250-800", lo: 250,  hi: 800 },
  { name: "mid        800-2.5k",lo: 800,  hi: 2500 },
  { name: "hi-mid     2.5-6k",  lo: 2500, hi: 6000 },
  { name: "air        6-16k",   lo: 6000, hi: 16000 },
];
for (const b of bands) {
  const jsr = bandRMS(jsM, b.lo, b.hi);
  const cr  = bandRMS(cM,  b.lo, b.hi);
  const r   = cr / jsr;
  const dB  = 20 * Math.log10(r || 1e-9);
  const verdict = Math.abs(dB) < 1 ? "tight" :
                  Math.abs(dB) < 3 ? "close" :
                  Math.abs(dB) < 6 ? "DRIFT" :
                                     "BIG DRIFT";
  console.log(`${b.name.padEnd(18)} ${jsr.toExponential(2)}  ${cr.toExponential(2)}  ${r.toFixed(2).padStart(5)}   ${dB.toFixed(2).padStart(6)}   ${verdict}`);
}

console.log("");
console.log("─".repeat(80));
console.log("PEAK DISTRIBUTION (% of samples above threshold)");
console.log("─".repeat(80));
const thresholds = [0.05, 0.1, 0.2, 0.4, 0.6, 0.8];
console.log(`thresh   JS%      C%      ΔpP   notes`);
for (const t of thresholds) {
  let jsCount = 0, cCount = 0;
  for (let i = 0; i < min; i++) {
    if (Math.abs(jsL[i]) > t || Math.abs(jsR[i]) > t) jsCount++;
    if (Math.abs(cL[i])  > t || Math.abs(cR[i])  > t) cCount++;
  }
  const jsP = 100 * jsCount / min;
  const cP  = 100 * cCount  / min;
  const dP  = cP - jsP;
  console.log(`|.| > ${t.toFixed(2)}  ${jsP.toFixed(2).padStart(6)}%  ${cP.toFixed(2).padStart(6)}%   ${dP > 0 ? "+" : ""}${dP.toFixed(2)}`);
}

console.log("");
console.log("─".repeat(80));
console.log("DC OFFSET + CLIPPING");
console.log("─".repeat(80));
let jsDC = 0, cDC = 0;
for (let i = 0; i < min; i++) { jsDC += jsL[i] + jsR[i]; cDC += cL[i] + cR[i]; }
jsDC /= (2 * min); cDC /= (2 * min);
let jsClip = 0, cClip = 0;
for (let i = 0; i < min; i++) {
  if (Math.abs(jsL[i]) >= 0.99 || Math.abs(jsR[i]) >= 0.99) jsClip++;
  if (Math.abs(cL[i])  >= 0.99 || Math.abs(cR[i])  >= 0.99) cClip++;
}
console.log(`DC offset  js ${jsDC.toExponential(2)}   c ${cDC.toExponential(2)}`);
console.log(`clipped    js ${jsClip} samples (${(100*jsClip/min).toFixed(4)}%)   c ${cClip} (${(100*cClip/min).toFixed(4)}%)`);
