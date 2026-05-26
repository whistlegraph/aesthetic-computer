#!/usr/bin/env node
// gen-melody-guide.mjs — render the mantra melody as a clean piano-bell
// guide WAV (no words, no vocals). Plays back as the wave-wizard melody
// prompt so jeffrey hears the TUNE before singing the words.

import { writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const SR = 48000;
const BPM = 182;
const SPB = 60 / BPM;
const ROOT_MEL = 62;                                          // D4
const TAU = Math.PI * 2;
const m2f = (m) => 440 * Math.pow(2, (m - 69) / 12);

// First 11 notes of THEME — matches the 11 mantra syllables.
// "that" lengthened 0.5 → 1.0β; v2 ("ney") sustain trimmed 2.0 → 1.5β.
const THEME_11 = [
  [-5, 1], [0, 1.5], [3, 1.0], [7, 1],
  [7, 1], [8, 1], [7, 1], [5, 1],
  [3, 1.5], [2, 1], [0, 1],
];

const totalBeats = THEME_11.reduce((a, [, b]) => a + b, 0);
const totalSec = totalBeats * SPB + 0.5;
const N = Math.floor(totalSec * SR);
const buf = new Float32Array(N);

// Brass-like additive voice — matches the engine's voice() partials
// for the brass theme as closely as a quick guide can: 7 partials with
// slight drive (saturation), vibrato at 5.6 Hz, ADSR with sustain so
// the long notes don't decay away like bells.
let beatCursor = 0;
for (const [off, beats] of THEME_11) {
  const midi = ROOT_MEL + off;
  const f = m2f(midi);
  const start = Math.floor(beatCursor * SPB * SR);
  const dur = beats * SPB * 0.96;
  const dN = Math.floor(dur * SR);
  const atk = 0.018;
  const rel = 0.10;
  // 7-partial brass spectrum (matches engine ULTIMATE brass)
  const parts = [[1, 1], [2, 0.6], [3, 0.42], [4, 0.24], [5, 0.13], [6, 0.05], [7, 0.02]];
  for (let i = 0; i < dN; i++) {
    const t = i / SR;
    // ADSR — quick attack, sustained body, quick release.
    let env = 1.0;
    if (t < atk) env = t / atk;
    else if (t > dur - rel) env = Math.max(0, (dur - t) / rel);
    // Vibrato — light pitch wobble at 5.6 Hz, 7 cents.
    const vib = 1 + 0.004 * Math.sin(TAU * 5.6 * t);
    let v = 0;
    for (const [n, g] of parts) v += g * Math.sin(TAU * f * n * vib * t);
    // Mild drive — push the sum through tanh for brass-like harmonics.
    v = Math.tanh(v * 0.9);
    if (start + i < N) buf[start + i] += env * 0.5 * v;
  }
  beatCursor += beats;
}

// Normalize to -3 dB peak.
let peak = 0;
for (const v of buf) if (Math.abs(v) > peak) peak = Math.abs(v);
const norm = peak > 0 ? (Math.pow(10, -3 / 20) / peak) : 1;

// Write 16-bit mono WAV.
const samples = Buffer.alloc(N * 2);
for (let i = 0; i < N; i++) {
  const v = Math.max(-32768, Math.min(32767, Math.round(buf[i] * norm * 32767)));
  samples.writeInt16LE(v, i * 2);
}
const hdr = Buffer.alloc(44);
hdr.write("RIFF", 0); hdr.writeUInt32LE(36 + samples.length, 4);
hdr.write("WAVEfmt ", 8); hdr.writeUInt32LE(16, 16);
hdr.writeUInt16LE(1, 20); hdr.writeUInt16LE(1, 22);
hdr.writeUInt32LE(SR, 24); hdr.writeUInt32LE(SR * 2, 28);
hdr.writeUInt16LE(2, 32); hdr.writeUInt16LE(16, 34);
hdr.write("data", 36); hdr.writeUInt32LE(samples.length, 40);

const outPath = `${LANE}/samples/mantra-melody-guide.wav`;
writeFileSync(outPath, Buffer.concat([hdr, samples]));
console.log(`✓ ${outPath} (${(N / SR).toFixed(2)}s, ${THEME_11.length} notes @ ${BPM} BPM)`);
