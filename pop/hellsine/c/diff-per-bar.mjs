#!/usr/bin/env node
// diff-per-bar.mjs — read JS + C pre.wav, compute per-bar RMS diff,
// peak ratios, and flag bars that diverge most. Helps localize where the
// C port drifts from the JS reference compositionally.
//
// Usage:  node diff-per-bar.mjs                       (default paths)
//         node diff-per-bar.mjs <js.wav> <c.wav>
//
// Note: the JS engine uses Math.random() in many spots so it's not
// fully deterministic — even rendering JS twice produces small diffs.
// Treat the per-bar RMS as a *relative* indicator: bars with much
// higher RMS than their neighbors are the real drift sites.

import { readFileSync } from "node:fs";

const SR = 48_000;
const BPM = 182;
const SPB = 60 / BPM;
const SPBAR = 4 * SPB;          // 1.319 s per bar
const TOTAL_BARS = 124;

const JS_PATH = process.argv[2] || "/Users/jas/Documents/Shelf/hellsine/.hellsine-js-pre.wav";
const C_PATH  = process.argv[3] || "/Users/jas/Documents/Shelf/hellsine/.hellsine-c-pre.wav";

function readWav(p) {
  const buf = readFileSync(p);
  if (buf.toString("ascii", 0, 4) !== "RIFF") throw new Error(`bad RIFF: ${p}`);
  let pos = 12, fmtCode = 0, channels = 2, bits = 32;
  while (pos < buf.length - 8) {
    const id = buf.toString("ascii", pos, pos + 4);
    const sz = buf.readUInt32LE(pos + 4);
    if (id === "fmt ") {
      fmtCode = buf.readUInt16LE(pos + 8);
      channels = buf.readUInt16LE(pos + 10);
      bits = buf.readUInt16LE(pos + 22);
    } else if (id === "data") {
      const stride = (bits / 8) * channels;
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
        } else throw new Error(`unsupported wav fmt=${fmtCode} bits=${bits}`);
      }
      return { L, R, N };
    }
    pos += 8 + sz;
  }
  throw new Error(`no data chunk: ${p}`);
}

// PLAN — needed to label each bar with its section
const PLAN = [
  { name: "overture",  bars: 12 },
  { name: "statement", bars: 24 },
  { name: "bridge",    bars: 24 },
  { name: "develop",   bars: 24 },
  { name: "climax",    bars: 24 },
  { name: "coda",      bars: 16 },
];

function sectionForBar(bar) {
  let acc = 0;
  for (const s of PLAN) {
    if (bar < acc + s.bars) return { name: s.name, secBar: bar - acc };
    acc += s.bars;
  }
  return { name: "tail", secBar: bar - acc };
}

console.log(`reading ${JS_PATH}...`);
const js = readWav(JS_PATH);
console.log(`reading ${C_PATH}...`);
const c  = readWav(C_PATH);

const sampsPerBar = Math.round(SPBAR * SR);
const minN = Math.min(js.N, c.N);

console.log("");
console.log("─".repeat(112));
console.log("bar   sec      time   section   jsPeak  jsRMS   cPeak   cRMS    diffRMS  diffPk   peakRatio  verdict");
console.log("─".repeat(112));

let worstBars = [];
for (let bar = 0; bar < TOTAL_BARS; bar++) {
  const start = bar * sampsPerBar;
  const end = Math.min(start + sampsPerBar, minN);
  if (end <= start) break;
  const n = end - start;
  let jsPeak = 0, cPeak = 0, jsSq = 0, cSq = 0, dSq = 0, dPk = 0, valid = 0;
  for (let i = start; i < end; i++) {
    const jl = js.L[i], jr = js.R[i];
    const cl = c.L[i], cr = c.R[i];
    if (!Number.isFinite(jl) || !Number.isFinite(jr) ||
        !Number.isFinite(cl) || !Number.isFinite(cr)) continue;
    valid++;
    const ja = Math.abs(jl), jb = Math.abs(jr);
    const ca = Math.abs(cl), cb = Math.abs(cr);
    if (ja > jsPeak) jsPeak = ja; if (jb > jsPeak) jsPeak = jb;
    if (ca > cPeak)  cPeak  = ca; if (cb > cPeak)  cPeak  = cb;
    jsSq += jl*jl + jr*jr;
    cSq  += cl*cl + cr*cr;
    const dL = jl - cl, dR = jr - cr;
    dSq += dL*dL + dR*dR;
    const ad = Math.max(Math.abs(dL), Math.abs(dR));
    if (ad > dPk) dPk = ad;
  }
  const jsRMS = Math.sqrt(jsSq / (2 * valid));
  const cRMS  = Math.sqrt(cSq  / (2 * valid));
  const dRMS  = Math.sqrt(dSq  / (2 * valid));
  const peakRatio = jsPeak > 0 ? cPeak / jsPeak : 0;
  const section = sectionForBar(bar);
  const verdict = (dRMS < 0.005) ? "tight" :
                  (dRMS < 0.02) ? "close" :
                  (dRMS < 0.06) ? "DRIFT" :
                  (dRMS < 0.15) ? "BIG DRIFT" :
                                  "DIVERGE";
  worstBars.push({ bar, dRMS, section: section.name, secBar: section.secBar });
  console.log(
    `${String(bar).padStart(3)}  ${section.name.padEnd(10)} ${(bar * SPBAR).toFixed(1).padStart(5)} ${String(section.secBar).padStart(3)}      ` +
    `${jsPeak.toFixed(3)}  ${jsRMS.toFixed(3)}   ${cPeak.toFixed(3)}  ${cRMS.toFixed(3)}    ` +
    `${dRMS.toFixed(3)}    ${dPk.toFixed(3)}     ${peakRatio.toFixed(2).padStart(4)}      ${verdict}`
  );
}

console.log("─".repeat(112));

worstBars.sort((a, b) => b.dRMS - a.dRMS);
console.log(`\nTop 10 most-divergent bars:`);
for (const w of worstBars.slice(0, 10)) {
  console.log(`  bar ${String(w.bar).padStart(3)}  ${w.section.padEnd(10)} (sec-bar ${w.secBar})  ${(w.bar * SPBAR).toFixed(1)}s   rms ${w.dRMS.toFixed(3)}`);
}

console.log(`\nTop 10 most-aligned bars:`);
for (const w of worstBars.slice(-10).reverse()) {
  console.log(`  bar ${String(w.bar).padStart(3)}  ${w.section.padEnd(10)} (sec-bar ${w.secBar})  ${(w.bar * SPBAR).toFixed(1)}s   rms ${w.dRMS.toFixed(3)}`);
}

// Per-section summary
const sectionSum = {};
for (const w of worstBars) {
  const k = w.section;
  if (!sectionSum[k]) sectionSum[k] = { sum: 0, n: 0, max: 0 };
  sectionSum[k].sum += w.dRMS;
  sectionSum[k].n++;
  if (w.dRMS > sectionSum[k].max) sectionSum[k].max = w.dRMS;
}
console.log(`\nPer-section average diff:`);
for (const s of PLAN) {
  const r = sectionSum[s.name];
  if (!r) continue;
  console.log(`  ${s.name.padEnd(10)}  ${s.bars} bars · avg rms ${(r.sum/r.n).toFixed(3)}  · max ${r.max.toFixed(3)}`);
}
