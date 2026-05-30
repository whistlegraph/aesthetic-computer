#!/usr/bin/env node
// gen-melody.mjs — render a 6-syllable bell melody for "i love computer club"
// as a guide WAV played by wave-wizard before each take.

import { writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48000;
const BPM = 120;
const SPB = 60 / BPM;
const ROOT = 62; // D4
const TAU = Math.PI * 2;
const m2f = (m) => 440 * Math.pow(2, (m - 69) / 12);

// "i / love / com-pu-ter / club" — leap up on love, tumble down to club.
// [semitone offset from ROOT, beats]
const THEME = [
  [12, 0.5],   // i      D5
  [19, 0.75],  // love   A5 (leap)
  [17, 0.5],   // com    G5
  [16, 0.25],  // pu     F#5
  [14, 0.5],   // ter    E5
  [12, 1.5],   // club   D5 (land)
];

const totalBeats = THEME.reduce((a, [, b]) => a + b, 0);
const totalSec = totalBeats * SPB + 0.5;
const N = Math.floor(totalSec * SR);
const buf = new Float32Array(N);

let beatCursor = 0;
for (const [off, beats] of THEME) {
  const f = m2f(ROOT + off);
  const start = Math.floor(beatCursor * SPB * SR);
  const dur = beats * SPB * 0.94;
  const dN = Math.floor(dur * SR);
  const atk = 0.012;
  const rel = 0.08;
  // bell-ish partials with mild brass body
  const parts = [[1, 1.0], [2, 0.55], [3, 0.30], [4, 0.16], [5, 0.08], [6, 0.03]];
  for (let i = 0; i < dN; i++) {
    const t = i / SR;
    let env = 1.0;
    if (t < atk) env = t / atk;
    else if (t > dur - rel) env = Math.max(0, (dur - t) / rel);
    const vib = 1 + 0.003 * Math.sin(TAU * 5.4 * t);
    let v = 0;
    for (const [n, g] of parts) v += g * Math.sin(TAU * f * n * vib * t);
    v = Math.tanh(v * 0.85);
    if (start + i < N) buf[start + i] += env * 0.5 * v;
  }
  beatCursor += beats;
}

let peak = 0;
for (const v of buf) if (Math.abs(v) > peak) peak = Math.abs(v);
const norm = peak > 0 ? (Math.pow(10, -3 / 20) / peak) : 1;

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

const outPath = resolve(HERE, "melody.wav");
writeFileSync(outPath, Buffer.concat([hdr, samples]));
console.log(`✓ ${outPath} (${(N / SR).toFixed(2)}s, ${THEME.length} notes @ ${BPM} BPM)`);
