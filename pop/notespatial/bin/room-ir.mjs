#!/usr/bin/env node
// room-ir.mjs — a synthetic stereo room for the binaural print: 18 ms pre-delay,
// a dozen decorrelated early reflections in the first 70 ms, then a noise tail
// whose decay is faster the higher the band (RT60 1.7 s at 250 Hz, 0.9 s at 8 kHz).
// Unit energy per channel so a mix weight is a level.  node room-ir.mjs out.wav
import { writeFileSync } from 'node:fs';
const RT = +(process.argv[3] || 1); // tail scale: 1 = the room, 2 = a hall
const SR = 44100, LEN = Math.round(SR * 2.4 * RT), PRE = Math.round(SR * .018);
let seed = 20260925; const rnd = () => (seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296 * 2 - 1;
const ch = () => new Float32Array(LEN);
const L = ch(), R = ch();
// early reflections: time (ms), gain, side (-1 left, 1 right) — a room wider than deep
const early = [[21,.55,-.6],[27,.5,.7],[34,.42,-.2],[41,.38,.9],[48,.33,-.8],[53,.3,.3],[59,.26,-.5],[66,.22,.6],[72,.19,-.9],[79,.16,.1]];
for (const [ms, g, side] of early) { const i = PRE + Math.round(SR * ms / 1000); L[i] += g * (1 - .5 * side); R[i] += g * (1 + .5 * side); }
// late tail: four bands of decaying noise, each its own RT60, decorrelated per ear
const bands = [[0, 250, 1.7 * RT], [250, 1500, 1.5 * RT], [1500, 5000, 1.2 * RT], [5000, 20000, .9 * RT]];
for (const [lo, hi, rt] of bands) {
  for (const T of [L, R]) {
    // band-limited noise by crude two-pole shaping: white noise → lowpass at hi minus lowpass at lo
    const a1 = Math.exp(-2 * Math.PI * hi / SR), a0 = Math.exp(-2 * Math.PI * lo / SR);
    let y1 = 0, y0 = 0;
    for (let n = PRE + Math.round(SR * .02); n < LEN; n++) {
      const w = rnd(); y1 = (1 - a1) * w + a1 * y1; y0 = lo > 0 ? (1 - a0) * w + a0 * y0 : 0;
      const t = (n - PRE) / SR, env = Math.pow(10, -3 * t / rt) * Math.min(1, t / .06);
      T[n] += (y1 - y0) * env * .35;
    }
  }
}
const norm = T => { let e = 0; for (const v of T) e += v * v; const g = 1 / Math.sqrt(e); for (let i = 0; i < T.length; i++) T[i] *= g; };
norm(L); norm(R);
const b = Buffer.alloc(44 + LEN * 8);
b.write('RIFF', 0); b.writeUInt32LE(36 + LEN * 8, 4); b.write('WAVE', 8); b.write('fmt ', 12); b.writeUInt32LE(16, 16); b.writeUInt16LE(3, 20); b.writeUInt16LE(2, 22); b.writeUInt32LE(SR, 24); b.writeUInt32LE(SR * 8, 28); b.writeUInt16LE(8, 32); b.writeUInt16LE(32, 34); b.write('data', 36); b.writeUInt32LE(LEN * 8, 40);
for (let i = 0; i < LEN; i++) { b.writeFloatLE(L[i], 44 + i * 8); b.writeFloatLE(R[i], 48 + i * 8); }
writeFileSync(process.argv[2], b); console.log(process.argv[2]);
