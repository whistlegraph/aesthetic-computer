#!/usr/bin/env node
// sacred-drone.mjs — Sacred-Harp-style 3-voice human-hum-like drone:
//   • Root G3 (≈196 Hz), 5th D4 (≈293.66 Hz), Octave G4 (≈392 Hz)
//   • No chord changes — the drone holds the tonic throughout
//   • Each voice = 2 sine oscillators detuned ±4 cents for natural
//     "two singers on the same note" chorus thickness
//   • Each voice has its OWN slow amp-LFO (≈0.11 / 0.17 / 0.23 Hz)
//     so the three never bloom in phase — gives the soft swell-and-
//     fade of a hummed congregation breath
//   • Soft stereo spread (root center, 5th slightly left, octave
//     slightly right) so the chord opens up across the field
//   • Long attack (4 s) + long release (15 s); soft tanh saturation;
//     peak-normalize to -3 dB headroom
//
// Output: stereo 16-bit WAV at 44.1 kHz.
//
// Usage: node pop/big-pictures/bin/sacred-drone.mjs
//          --duration 84 --root-midi 55
//          --out /tmp/amazing-sacred-drone.wav

import { writeFileSync } from "node:fs";
import { resolve } from "node:path";

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
    else flags[a.slice(2)] = true;
  }
}

const DUR = Number(flags.duration ?? 84);
const ROOT = Number(flags["root-midi"] ?? 55); // G3
const SR = 44100;
const OUT = resolve(String(flags.out || "/tmp/amazing-sacred-drone.wav").replace(/^~/, process.env.HOME));
const N = Math.floor(DUR * SR);

const midiToHz = (m) => 440 * Math.pow(2, (m - 69) / 12);
const cents = (c) => Math.pow(2, c / 1200);

// 3 voices: root, 5th, octave (each with 2 detuned sub-oscs).
// Gains tuned so the drone sits CLEARLY under a sung lead — strong
// enough to read as a continuous "instrument" the whole song, never
// disappearing behind the voice.
const voices = [
  { midi: ROOT,      panL: 0.62, panR: 0.62, lfoHz: 0.11, lfoPhase: 0.0,           gain: 0.95 },
  { midi: ROOT + 7,  panL: 0.78, panR: 0.42, lfoHz: 0.17, lfoPhase: Math.PI * 0.6, gain: 0.75 },
  { midi: ROOT + 12, panL: 0.42, panR: 0.78, lfoHz: 0.23, lfoPhase: Math.PI * 1.3, gain: 0.60 },
];

const L = new Float32Array(N);
const R = new Float32Array(N);

const ATTACK = 4.0;
const RELEASE = 15.0;

for (const v of voices) {
  const f1 = midiToHz(v.midi) * cents(-4);
  const f2 = midiToHz(v.midi) * cents(+4);
  // Per-voice phase init = small random offset so the two detuned
  // oscs don't start in phase (would emphasize the beat frequency).
  // Use a deterministic offset derived from the midi # so the file
  // is reproducible.
  const ph1Off = (v.midi % 17) / 17 * Math.PI * 2;
  const ph2Off = (v.midi % 13) / 13 * Math.PI * 2;
  for (let i = 0; i < N; i++) {
    const t = i / SR;
    // global envelope
    let env = v.gain;
    if (t < ATTACK) env *= t / ATTACK;
    if (t > DUR - RELEASE) env *= Math.max(0, (DUR - t) / RELEASE);
    // per-voice slow amp-LFO (gentle breath swell). Range tightened
    // to [0.92, 1.0] so the drone never drops out audibly under the
    // vocal — it still breathes, but it's always present.
    const lfo = 0.92 + 0.08 * Math.sin(2 * Math.PI * v.lfoHz * t + v.lfoPhase);
    env *= lfo;
    const s = (Math.sin(2 * Math.PI * f1 * t + ph1Off) +
               Math.sin(2 * Math.PI * f2 * t + ph2Off)) * 0.5 * env;
    L[i] += s * v.panL;
    R[i] += s * v.panR;
  }
}

// Soft saturation + peak-norm to -3 dB
let peak = 0;
for (let i = 0; i < N; i++) {
  L[i] = Math.tanh(L[i] * 1.05);
  R[i] = Math.tanh(R[i] * 1.05);
  peak = Math.max(peak, Math.abs(L[i]), Math.abs(R[i]));
}
const target = Math.pow(10, -3 / 20);
const gain = peak > 0 ? target / peak : 1;
for (let i = 0; i < N; i++) { L[i] *= gain; R[i] *= gain; }
console.log(`▸ synth peak ${peak.toFixed(3)} → gain ${gain.toFixed(3)} (target -3 dB)`);

// 16-bit PCM stereo WAV
const dataSize = N * 2 * 2;
const buf = Buffer.alloc(44 + dataSize);
buf.write("RIFF", 0);
buf.writeUInt32LE(36 + dataSize, 4);
buf.write("WAVE", 8);
buf.write("fmt ", 12);
buf.writeUInt32LE(16, 16);
buf.writeUInt16LE(1, 20);
buf.writeUInt16LE(2, 22);
buf.writeUInt32LE(SR, 24);
buf.writeUInt32LE(SR * 2 * 2, 28);
buf.writeUInt16LE(4, 32);
buf.writeUInt16LE(16, 34);
buf.write("data", 36);
buf.writeUInt32LE(dataSize, 40);
let off = 44;
for (let i = 0; i < N; i++) {
  buf.writeInt16LE(Math.max(-32768, Math.min(32767, Math.round(L[i] * 32767))), off); off += 2;
  buf.writeInt16LE(Math.max(-32768, Math.min(32767, Math.round(R[i] * 32767))), off); off += 2;
}
writeFileSync(OUT, buf);
console.log(`✓ wrote ${OUT} (${(dataSize/1024/1024).toFixed(1)} MB · ${DUR}s · ${SR}Hz · stereo)`);
