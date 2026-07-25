#!/usr/bin/env node
// Original Aesthetic Wing music + sound-design bed.
// Deterministic bottom-up synthesis: no stock audio and no samples.

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ALIGNMENT = join(HERE, "vo-v1.mp3.alignment.json");
const OUT = join(HERE, "aesthetic-wing-sound.wav");
const SR = 48_000;
const words = JSON.parse(readFileSync(ALIGNMENT, "utf8")).words;
const total = words.at(-1).toMs / 1000 + 1.6;
const N = Math.ceil(total * SR);
const left = new Float32Array(N);
const right = new Float32Array(N);

const ends = [14, 21, 42, 52, 58].map((i) => words[i].toMs / 1000);
const [roadEnd, unfoldEnd, liftEnd, settleEnd] = ends;
let seed = 0x41e57e71;
const random = () => {
  seed ^= seed << 13; seed ^= seed >>> 17; seed ^= seed << 5;
  return ((seed >>> 0) / 0xffffffff) * 2 - 1;
};
const clamp = (x) => Math.max(-1, Math.min(1, x));
const ease = (x) => x <= 0 ? 0 : x >= 1 ? 1 : x * x * (3 - 2 * x);
const add = (t, l, r = l) => {
  const i = Math.floor(t * SR);
  if (i >= 0 && i < N) { left[i] += l; right[i] += r; }
};

// Quiet modal bed: long hand-tuned sine/triangle strata in D, with the
// lift section opening upward. This is the musical KidLisp field.
const midi = (n) => 440 * 2 ** ((n - 69) / 12);
const chordAt = (t) => {
  const p = t / total;
  if (t < roadEnd) return [38, 45, 52, 57];       // D/A/E/A
  if (t < unfoldEnd) return [40, 47, 54, 59];     // E/B/F#/B
  if (t < liftEnd) return [43, 50, 57, 62, 66];   // G/D/A/D/F#
  if (t < settleEnd) return [38, 45, 52, 57];
  return [38, 45, 50, 54, 57];                    // D/A/D/F#/A
};
for (let i = 0; i < N; i++) {
  const t = i / SR;
  const notes = chordAt(t);
  let s = 0;
  for (let j = 0; j < notes.length; j++) {
    const f = midi(notes[j]);
    s += Math.sin(2 * Math.PI * f * t + j * 0.61) * (0.050 / (1 + j * 0.35));
    s += Math.sin(2 * Math.PI * f * 2.003 * t + j) * (0.008 / (1 + j));
  }
  const breathing = 0.72 + 0.28 * Math.sin(2 * Math.PI * 0.071 * t);
  const fade = ease(t / 1.2) * ease((total - t) / 1.5);
  left[i] += s * breathing * fade * (0.96 + 0.04 * Math.sin(2 * Math.PI * 0.11 * t));
  right[i] += s * breathing * fade * (0.96 + 0.04 * Math.sin(2 * Math.PI * 0.13 * t + 1));
}

// Bell/cursor event: soft inharmonic prompt tones.
function bell(at, note, gain = 0.14) {
  const f = midi(note);
  const dur = 1.8;
  for (let n = 0; n < dur * SR; n++) {
    const t = n / SR;
    const e = (1 - Math.exp(-t * 80)) * Math.exp(-t * 3.5);
    const s = (Math.sin(2 * Math.PI * f * t) +
      0.24 * Math.sin(2 * Math.PI * f * 2.41 * t + 0.4) +
      0.10 * Math.sin(2 * Math.PI * f * 3.97 * t + 1.2)) * e * gain;
    add(at + t, s * 0.92, s);
  }
}
bell(0.08, 74, 0.11);
bell(Math.max(0.2, roadEnd - 0.15), 78, 0.15);
bell(unfoldEnd - 0.10, 81, 0.14);
bell(liftEnd - 0.25, 74, 0.10);
bell(settleEnd + 0.05, 69, 0.09);

// Wheel texture in road mode: filtered noise + a rubber contact pulse.
let lp = 0;
for (let i = 0; i < Math.min(N, Math.floor(roadEnd * SR)); i++) {
  const t = i / SR;
  lp += 0.018 * (random() - lp);
  const pulse = 0.45 + 0.55 * Math.max(0, Math.sin(2 * Math.PI * 6.2 * t));
  const g = ease(t / 0.5) * ease((roadEnd - t) / 0.6);
  const s = lp * pulse * g * 0.12 + Math.sin(2 * Math.PI * 42 * t) * g * 0.012;
  left[i] += s; right[i] += s * 0.92;
}

// Guarded-fan/downwash layer: rises through unfold, holds in lift, and
// decays through settle. Colored noise plus a calm six-blade harmonic.
let fanLp = 0;
const fanStart = Math.max(0, roadEnd - 0.35);
for (let i = Math.floor(fanStart * SR); i < Math.min(N, Math.floor((settleEnd + 0.35) * SR)); i++) {
  const t = i / SR;
  const up = ease((t - fanStart) / Math.max(0.5, unfoldEnd - fanStart));
  const down = ease((settleEnd + 0.25 - t) / Math.max(0.5, settleEnd - liftEnd + 0.25));
  const env = t < liftEnd ? up : down;
  fanLp += 0.055 * (random() - fanLp);
  const rotor = Math.sin(2 * Math.PI * (76 + 4 * Math.sin(t * 0.7)) * t) * 0.025 +
    Math.sin(2 * Math.PI * 228 * t + 0.4) * 0.007;
  const air = fanLp * 0.17;
  const pan = 0.08 * Math.sin(2 * Math.PI * 0.19 * t);
  left[i] += (rotor + air) * env * (1 - pan);
  right[i] += (rotor + air) * env * (1 + pan);
}

// Gentle tire kiss and folding latch.
const touch = Math.max(liftEnd + 0.2, settleEnd - 0.55);
for (let n = 0; n < 0.55 * SR; n++) {
  const t = n / SR;
  const e = Math.exp(-t * 13);
  const thump = Math.sin(2 * Math.PI * (58 - 25 * t) * t) * e * 0.24;
  const click = (n < 0.025 * SR ? random() * Math.exp(-t * 90) * 0.07 : 0);
  add(touch + t, thump + click);
}

// Gentle soft clip and write 16-bit stereo WAV.
for (let i = 0; i < N; i++) {
  left[i] = Math.tanh(left[i] * 1.18) * 0.72;
  right[i] = Math.tanh(right[i] * 1.18) * 0.72;
}
const dataBytes = N * 4;
const wav = Buffer.alloc(44 + dataBytes);
wav.write("RIFF", 0); wav.writeUInt32LE(36 + dataBytes, 4); wav.write("WAVE", 8);
wav.write("fmt ", 12); wav.writeUInt32LE(16, 16); wav.writeUInt16LE(1, 20);
wav.writeUInt16LE(2, 22); wav.writeUInt32LE(SR, 24); wav.writeUInt32LE(SR * 4, 28);
wav.writeUInt16LE(4, 32); wav.writeUInt16LE(16, 34); wav.write("data", 36);
wav.writeUInt32LE(dataBytes, 40);
for (let i = 0; i < N; i++) {
  wav.writeInt16LE(Math.round(clamp(left[i]) * 32767), 44 + i * 4);
  wav.writeInt16LE(Math.round(clamp(right[i]) * 32767), 46 + i * 4);
}
writeFileSync(OUT, wav);
console.log(`✓ ${OUT} · ${total.toFixed(2)}s original music + wheel/lift/landing sound design`);
