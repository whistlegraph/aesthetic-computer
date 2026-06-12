#!/usr/bin/env node
// analyze-whistlegraph.mjs — pull the sung melody out of a whistlegraph
// performance video's audio: a dependency-free YIN f0 tracker → segmented
// notes → a structural + dynamic summary used to shape mombobasleep's arc.
//
//   ffmpeg -i <piece>-web.mp4 -ac 1 -ar 16000 out.wav   (caller does this)
//   node analyze-whistlegraph.mjs out.wav

import { readFileSync } from "node:fs";

const path = process.argv[2];
if (!path) { console.error("usage: analyze-whistlegraph.mjs <mono16k.wav>"); process.exit(1); }

// ── WAV (mono 16-bit PCM) → Float32 ──────────────────────────────────────
function readWav(p) {
  const buf = readFileSync(p);
  let off = 12, sr = 16000, dataOff = 0, dataLen = 0;
  while (off + 8 <= buf.length) {
    const id = buf.toString("ascii", off, off + 4), sz = buf.readUInt32LE(off + 4);
    if (id === "fmt ") sr = buf.readUInt32LE(off + 12);
    else if (id === "data") { dataOff = off + 8; dataLen = sz; }
    off += 8 + sz + (sz & 1);
  }
  const n = Math.floor(dataLen / 2), out = new Float32Array(n);
  for (let i = 0; i < n; i++) out[i] = buf.readInt16LE(dataOff + i * 2) / 32768;
  return { x: out, sr };
}

// ── YIN f0 on one frame (returns Hz or 0 if unvoiced) ────────────────────
function yin(frame, sr, thr = 0.12) {
  const W = frame.length, tauMax = W >> 1;
  const d = new Float32Array(tauMax);
  for (let tau = 1; tau < tauMax; tau++) {
    let s = 0;
    for (let i = 0; i < tauMax; i++) { const diff = frame[i] - frame[i + tau]; s += diff * diff; }
    d[tau] = s;
  }
  // cumulative mean normalized difference
  const cmnd = new Float32Array(tauMax); cmnd[0] = 1;
  let run = 0;
  for (let tau = 1; tau < tauMax; tau++) { run += d[tau]; cmnd[tau] = d[tau] * tau / (run || 1); }
  // absolute threshold → first dip below thr
  let tau = -1;
  for (let t = 2; t < tauMax; t++) {
    if (cmnd[t] < thr) { while (t + 1 < tauMax && cmnd[t + 1] < cmnd[t]) t++; tau = t; break; }
  }
  if (tau === -1) return 0;
  // parabolic interpolation
  const x0 = tau > 1 ? cmnd[tau - 1] : cmnd[tau], x2 = tau + 1 < tauMax ? cmnd[tau + 1] : cmnd[tau];
  const a = (x0 + x2 - 2 * cmnd[tau]) / 2, b = (x2 - x0) / 2;
  const better = a ? tau - b / (2 * a) : tau;
  return sr / better;
}

const { x, sr } = readWav(path);
const WIN = 1024, HOP = 160;                 // ~64 ms window, 10 ms hop
const frames = [];
const rms = (s, e) => { let a = 0; for (let i = s; i < e; i++) a += x[i] * x[i]; return Math.sqrt(a / (e - s)); };
let peakRms = 0;
for (let i = 0; i + WIN < x.length; i += HOP) {
  const r = rms(i, i + WIN); if (r > peakRms) peakRms = r;
  let f = r > 0.012 ? yin(x.subarray(i, i + WIN), sr) : 0;       // energy gate
  if (f < 70 || f > 1000) f = 0;                                 // vocal band
  frames.push({ t: i / sr, hz: f, r });
}

// median-filter f0 (kill octave jumps / spurious frames)
const med = (arr) => { const s = [...arr].sort((a, b) => a - b); return s[s.length >> 1]; };
for (let i = 2; i < frames.length - 2; i++) {
  const w = [frames[i - 2], frames[i - 1], frames[i], frames[i + 1], frames[i + 2]].map((f) => f.hz).filter((h) => h > 0);
  if (w.length >= 3) frames[i].hzf = med(w); else frames[i].hzf = 0;
}
const hzToMidi = (hz) => 69 + 12 * Math.log2(hz / 440);

// ── segment voiced frames into NOTES ─────────────────────────────────────
const notes = [];
let cur = null;
for (const fr of frames) {
  const m = fr.hzf > 0 ? hzToMidi(fr.hzf) : null;
  if (m == null) { if (cur) { notes.push(cur); cur = null; } continue; }
  if (cur && Math.abs(m - cur.mAcc / cur.n) < 1.2) { cur.mAcc += m; cur.n++; cur.end = fr.t; cur.rSum += fr.r; }
  else { if (cur) notes.push(cur); cur = { start: fr.t, end: fr.t, mAcc: m, n: 1, rSum: fr.r }; }
}
if (cur) notes.push(cur);
const clean = notes
  .map((nt) => ({ start: +nt.start.toFixed(3), dur: +(nt.end - nt.start).toFixed(3), midi: Math.round(nt.mAcc / nt.n), level: nt.rSum / nt.n }))
  .filter((nt) => nt.dur >= 0.06);

// ── summarize ────────────────────────────────────────────────────────────
const mids = clean.map((n) => n.midi);
const lo = Math.min(...mids), hi = Math.max(...mids);
const NOTE = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const name = (m) => NOTE[m % 12] + (Math.floor(m / 12) - 1);
console.log(`\n=== ${path.split("/").pop()} ===`);
console.log(`duration ${(x.length / sr).toFixed(1)}s · ${clean.length} sung notes · range ${name(lo)}..${name(hi)} (${hi - lo} st) · peakRMS ${(20 * Math.log10(peakRms)).toFixed(1)} dB`);
console.log(`\nmelody (onset · dur · note · rel-level):`);
for (const n of clean) console.log(`  ${n.start.toFixed(2).padStart(6)}s  ${n.dur.toFixed(2)}s  ${name(n.midi).padEnd(4)}  ${"█".repeat(Math.max(1, Math.round(n.level / peakRms * 12)))}`);

// register/dynamic arc in N equal time-bins (for mapping onto movements)
const BINS = 9, T = x.length / sr;
const bins = Array.from({ length: BINS }, () => ({ mSum: 0, mN: 0, lSum: 0, lN: 0 }));
for (const n of clean) { const b = Math.min(BINS - 1, Math.floor((n.start / T) * BINS)); bins[b].mSum += n.midi; bins[b].mN++; bins[b].lSum += n.level; bins[b].lN++; }
console.log(`\n${BINS}-bin arc (avg register · density):`);
bins.forEach((b, i) => {
  const reg = b.mN ? name(Math.round(b.mSum / b.mN)) : "—";
  const lv = b.lN ? (b.lSum / b.lN / peakRms) : 0;
  console.log(`  bin ${i}: ${b.mN.toString().padStart(2)} notes  reg ${reg.padEnd(4)}  ${"▓".repeat(Math.round(lv * 16))}`);
});
const json = { duration: +T.toFixed(3), notes: clean.map((n) => ({ ...n, level: +(n.level / peakRms).toFixed(3) })), range: [lo, hi] };
console.log("\nJSON:" + JSON.stringify(json));
