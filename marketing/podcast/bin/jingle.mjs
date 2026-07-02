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
function addBell(L, R, f, at, ring, gain, pan = 0) {
  const partials = [
    [1.0, 1.0], [2.01, 0.5], [2.99, 0.34], [4.18, 0.22], [5.43, 0.12],
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

if (import.meta.url === `file://${process.argv[1]}`) {
  const HERE = dirname(fileURLToPath(import.meta.url));
  const out = resolve(HERE, "..", "assets");
  const { intro, outro } = renderJingles(out);
  console.log(`✓ ${intro}`);
  console.log(`✓ ${outro}`);
}
