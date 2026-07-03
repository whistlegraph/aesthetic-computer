#!/usr/bin/env node
// jingle.mjs — synthesize the podcast's intro/outro stings.
//
// A short pentatonic bell motif: ascending on the way in (an opening bell),
// resolving downward on the way out (a closing bell). Deterministic, no
// samples, $0. Bells are a sum of inharmonic partials under an exponential
// decay — the pop DSP posture (phase-increment sines) kept self-contained.
//
// Usage:
//   node bin/jingle.mjs            # writes assets/intro.wav + assets/outro.wav
//   import { renderJingles } from "./jingle.mjs"

import { writeFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const SR = 44100;

// Tempo of the bed — exported so produce.mjs can quantize speech onsets to
// the same grid (beat-align).
export const BED_BPM = 72;

// ── tiny WAV (PCM16 stereo) encoder ────────────────────────────────────
function encodeWav(left, right) {
  const n = left.length;
  const buf = Buffer.alloc(44 + n * 4);
  buf.write("RIFF", 0);
  buf.writeUInt32LE(36 + n * 4, 4);
  buf.write("WAVE", 8);
  buf.write("fmt ", 12);
  buf.writeUInt32LE(16, 16);
  buf.writeUInt16LE(1, 20);          // PCM
  buf.writeUInt16LE(2, 22);          // stereo
  buf.writeUInt32LE(SR, 24);
  buf.writeUInt32LE(SR * 4, 28);     // byte rate
  buf.writeUInt16LE(4, 32);          // block align
  buf.writeUInt16LE(16, 34);         // bits
  buf.write("data", 36);
  buf.writeUInt32LE(n * 4, 40);
  let o = 44;
  for (let i = 0; i < n; i++) {
    const l = Math.max(-1, Math.min(1, left[i]));
    const r = Math.max(-1, Math.min(1, right[i]));
    buf.writeInt16LE((l * 32767) | 0, o); o += 2;
    buf.writeInt16LE((r * 32767) | 0, o); o += 2;
  }
  return buf;
}

// A struck bell at frequency f, hit at time `at` (s), ringing for `ring` (s).
// Inharmonic partial stack + exponential decay + a touch of stereo spread.
function addBell(L, R, f, at, ring, gain, pan = 0, bright = 1) {
  // `bright` scales the upper (inharmonic) partials — 1 = full shimmer,
  // lower = warmer/darker (used for the bed so it sits under the voice).
  const partials = [
    [1.0, 1.0], [2.01, 0.5 * bright], [2.99, 0.34 * bright],
    [4.18, 0.22 * bright], [5.43, 0.12 * bright],
  ];
  const start = Math.floor(at * SR);
  const len = Math.floor(ring * SR);
  const gl = gain * (1 - Math.max(0, pan)) * 0.5;
  const gr = gain * (1 + Math.min(0, pan)) * 0.5;
  for (let i = 0; i < len; i++) {
    const t = i / SR;
    const env = Math.exp(-t * 3.6) * (1 - Math.exp(-t * 400)); // decay + soft attack
    let s = 0;
    for (const [mult, amp] of partials) {
      s += amp * Math.sin(2 * Math.PI * f * mult * t);
    }
    s *= env / 2.2;
    const idx = start + i;
    if (idx < L.length) { L[idx] += s * (gl * 2); R[idx] += s * (gr * 2); }
  }
}

// A soft sine pad under the motif for warmth.
function addPad(L, R, f, at, dur, gain) {
  const start = Math.floor(at * SR);
  const len = Math.floor(dur * SR);
  for (let i = 0; i < len; i++) {
    const t = i / SR;
    const env = Math.min(1, t * 6) * Math.min(1, (dur - t) * 3); // fade in/out
    const s = (Math.sin(2 * Math.PI * f * t) + 0.5 * Math.sin(2 * Math.PI * f * 2 * t)) * env * gain;
    if (start + i < L.length) { L[start + i] += s; R[start + i] += s; }
  }
}

function normalize(L, R, peak = 0.9) {
  let max = 0;
  for (let i = 0; i < L.length; i++) { max = Math.max(max, Math.abs(L[i]), Math.abs(R[i])); }
  if (max < 1e-6) return;
  const g = peak / max;
  for (let i = 0; i < L.length; i++) { L[i] *= g; R[i] *= g; }
}

// C-major pentatonic (a bright, unresolved-then-resolved feel).
const N = { C4: 261.63, D4: 293.66, E4: 329.63, G4: 392.0, A4: 440.0,
           C5: 523.25, D5: 587.33, E5: 659.25, G5: 783.99, A5: 880.0, C6: 1046.5 };

function renderIntro() {
  const dur = 3.4;
  const L = new Float32Array(Math.floor(dur * SR));
  const R = new Float32Array(L.length);
  addPad(L, R, N.C4 / 2, 0.0, dur, 0.10);          // low warmth
  const motif = [N.C5, N.E5, N.G5, N.A5, N.C6];    // ascending open bell
  motif.forEach((f, i) => addBell(L, R, f, 0.02 + i * 0.19, 2.2, 0.9, (i % 2 ? 0.3 : -0.3)));
  addBell(L, R, N.G5, 1.15, 2.0, 0.4, 0.0);        // sympathetic fifth
  normalize(L, R, 0.92);
  return encodeWav(L, R);
}

function renderOutro() {
  const dur = 3.2;
  const L = new Float32Array(Math.floor(dur * SR));
  const R = new Float32Array(L.length);
  addPad(L, R, N.C4 / 2, 0.0, dur, 0.11);
  const motif = [N.C6, N.G5, N.E5, N.C5];          // resolving descent
  motif.forEach((f, i) => addBell(L, R, f, 0.02 + i * 0.2, 2.2, 0.85, (i % 2 ? -0.3 : 0.3)));
  // final grounded chord
  addBell(L, R, N.C4, 0.9, 2.3, 0.55, 0);
  addBell(L, R, N.G4, 0.92, 2.3, 0.4, 0.2);
  addBell(L, R, N.C5, 0.94, 2.3, 0.4, -0.2);
  normalize(L, R, 0.92);
  return encodeWav(L, R);
}

export function renderJingles(outDir) {
  mkdirSync(outDir, { recursive: true });
  const intro = resolve(outDir, "intro.wav");
  const outro = resolve(outDir, "outro.wav");
  writeFileSync(intro, renderIntro());
  writeFileSync(outro, renderOutro());
  return { intro, outro };
}

// ── the bed: a soft lo-fi underscore for under the reading ──────────────
// Slow chord pad + sparse pentatonic melody + a gentle kick/hat pulse.
// Deterministic (seeded LCG) so re-runs are byte-identical.
function lcg(seed) { let s = seed >>> 0; return () => (s = (s * 1664525 + 1013904223) >>> 0) / 4294967296; }

function addKick(L, R, at, gain, opt = {}) {
  const { f0 = 105, f1 = 45, pdrop = 28, adec = 8.5, len = 0.3 } = opt;
  const start = Math.floor(at * SR), n = Math.floor(len * SR);
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const f = f1 + (f0 - f1) * Math.exp(-t * pdrop);
    const env = Math.exp(-t * adec);
    const s = Math.sin(2 * Math.PI * f * t) * env * gain;
    const idx = start + i;
    if (idx < L.length) { L[idx] += s; R[idx] += s; }
  }
}
// Filtered-noise percussion (hat / shaker / brush). `hp` mixes in a crude
// high-pass (1 = bright tick, 0 = fuller brush); `dec` sets the decay rate.
function addNoise(L, R, at, gain, rnd, { dur = 0.05, dec = 90, hp = 1 } = {}) {
  const start = Math.floor(at * SR), n = Math.floor(dur * SR);
  let prev = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const env = Math.exp(-t * dec);
    const white = rnd() * 2 - 1;
    const filt = hp * (white - prev) + (1 - hp) * white; prev = white;
    const s = filt * env * gain;
    const idx = start + i;
    if (idx < L.length) { L[idx] += s * 0.85; R[idx] += s; }
  }
}
// Short pitched click — woodblock / rim / tick.
function addTone(L, R, at, freq, gain, { dur = 0.09, dec = 45 } = {}) {
  const start = Math.floor(at * SR), n = Math.floor(dur * SR);
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const s = Math.sin(2 * Math.PI * freq * t) * Math.exp(-t * dec) * gain;
    const idx = start + i;
    if (idx < L.length) { L[idx] += s; R[idx] += s; }
  }
}

// ── drum kits — the swappable rhythm layer ─────────────────────────────
export const KITS = ["felt", "brush", "wood", "eight0eight", "pulse"];

function drums(L, R, dur, rnd, beat, kit) {
  for (let t = 0, b = 0; t < dur; t += beat, b++) {
    const ib = b % 4, half = beat / 2;
    switch (kit) {
      case "brush": // soft kick on 1, brushed backbeat on 2 & 4, quiet shaker
        if (ib === 0) addKick(L, R, t, 0.42, { len: 0.34, adec: 7 });
        if (ib === 1 || ib === 3) addNoise(L, R, t, 0.12, rnd, { dur: 0.16, dec: 26, hp: 0.35 });
        addNoise(L, R, t + half, 0.04, rnd, { dur: 0.06, dec: 80 });
        break;
      case "wood": // wooden kick + syncopated woodblock
        if (ib === 0 || ib === 2) addKick(L, R, t, 0.4, { f0: 180, f1: 70, len: 0.16, adec: 16 });
        addTone(L, R, t + half, 820, 0.12, { dur: 0.06, dec: 70 });
        if (ib === 2) addTone(L, R, t + half * 1.5, 1180, 0.08, { dur: 0.05, dec: 80 });
        break;
      case "eight0eight": // 808: long sub kick + tight 16th ticks
        if (ib === 0) addKick(L, R, t, 0.6, { f0: 80, f1: 34, pdrop: 16, adec: 4.5, len: 0.55 });
        if (ib === 2) addKick(L, R, t + half, 0.5, { f0: 80, f1: 34, pdrop: 16, adec: 4.5, len: 0.55 });
        for (let q = 0; q < 4; q++) addNoise(L, R, t + q * (beat / 4), q % 2 ? 0.05 : 0.08, rnd, { dur: 0.03, dec: 120 });
        break;
      case "pulse": // heartbeat — two soft kicks, no cymbals
        if (ib === 0) { addKick(L, R, t, 0.5); addKick(L, R, t + beat * 0.55, 0.3); }
        break;
      case "felt": // default lo-fi: kick on 1 & 3, shaker on eighths
      default:
        if (ib === 0 || ib === 2) addKick(L, R, t, 0.5);
        addNoise(L, R, t, 0.05, rnd, { dur: 0.05, dec: 90 });
        addNoise(L, R, t + half, 0.085, rnd, { dur: 0.05, dec: 90 });
        break;
    }
  }
}

export function renderBed(durationSec, outPath, opts = {}) {
  const kit = opts.kit || "felt";
  const dur = Math.max(4, durationSec);
  const L = new Float32Array(Math.ceil(dur * SR));
  const R = new Float32Array(L.length);
  const rnd = lcg(0x51ED9);
  const bpm = BED_BPM, beat = 60 / bpm, bar = beat * 4;

  // Chord pad — I · vi · IV · V, two bars each, gently overlapping.
  const prog = [
    [130.81, 164.81, 196.00], // C  (C3 E3 G3)
    [110.00, 130.81, 164.81], // Am (A2 C3 E3)
    [ 87.31, 110.00, 130.81], // F  (F2 A2 C3)
    [ 98.00, 123.47, 146.83], // G  (G2 B2 D3)
  ];
  let ci = 0;
  for (let t = 0; t < dur; t += bar * 2) {
    const [r, th, fi] = prog[ci++ % prog.length];
    const d = Math.min(bar * 2 + 0.7, dur - t + 0.7);
    addPad(L, R, r, t, d, 0.055);
    addPad(L, R, th, t, d, 0.035);
    addPad(L, R, fi, t, d, 0.042);
  }

  // Rhythm — swappable drum kit (default "felt" = the current sound).
  drums(L, R, dur, rnd, beat, kit);

  // Melody — sparse pentatonic music-box line, a random walk in C. Kept an
  // octave down (C4–C5) and timbrally warm so it sits under the voice.
  const penta = [261.63, 293.66, 329.63, 392.00, 440.00, 523.25];
  let mi = 2;
  for (let t = 0.5; t < dur; t += beat * (rnd() < 0.5 ? 2 : 3)) {
    mi = Math.max(0, Math.min(penta.length - 1, mi + (rnd() < 0.5 ? -1 : 1) * (rnd() < 0.3 ? 2 : 1)));
    addBell(L, R, penta[mi], t, 3.4, 0.24, rnd() * 0.6 - 0.3, 0.4); // bright=0.4 → warm
  }

  normalize(L, R, 0.72);
  writeFileSync(outPath, encodeWav(L, R));
  return outPath;
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const HERE = dirname(fileURLToPath(import.meta.url));
  const out = resolve(HERE, "..", "assets");
  const { intro, outro } = renderJingles(out);
  console.log(`✓ ${intro}`);
  console.log(`✓ ${outro}`);
}
