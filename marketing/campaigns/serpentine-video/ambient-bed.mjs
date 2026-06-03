#!/usr/bin/env node
// ambient-bed.mjs — slow, sustained, drifting sine-bell pad (Eno "Music for
// Airports" feel): overlapping bell tones on prime-period loops over a warm
// open chord, with a quiet low drone. No meter, no oom-pah-pah.
// Usage: node ambient-bed.mjs [durationSec] [outMp3]
import { writeFileSync, unlinkSync } from "node:fs";
import { spawnSync } from "node:child_process";

const SR = 48000;
const DUR = Number(process.argv[2] || 180);
const OUT = process.argv[3] || "/tmp/ambient-bed.mp3";
const N = Math.floor(DUR * SR);
const out = new Float32Array(N);
const TAU = Math.PI * 2;
const midiToFreq = (m) => 440 * Math.pow(2, (m - 69) / 12);

// gentle, long-ringing bell — fundamental dominant, soft inharmonic shimmer
const PARTIALS = [
  { ratio: 0.5, amp: 0.35, t60: 12 },
  { ratio: 1.0, amp: 1.0,  t60: 10 },
  { ratio: 2.0, amp: 0.20, t60: 5.5 },
  { ratio: 3.0, amp: 0.06, t60: 2.6 },
  { ratio: 4.2, amp: 0.03, t60: 1.6 },
];
const ATTACK = 0.04 * SR; // soft 40ms bloom

function strike(midi, startSec, gain, detuneCents = 0) {
  const f0 = midiToFreq(midi) * Math.pow(2, detuneCents / 1200);
  const start = Math.floor(startSec * SR);
  const ps = PARTIALS.map((p) => ({
    omega: (TAU * f0 * p.ratio) / SR,
    amp: p.amp,
    decay: Math.exp(-Math.log(1000) / (p.t60 * SR)),
    len: Math.floor(p.t60 * 1.4 * SR),
  }));
  const maxLen = Math.max(...ps.map((p) => p.len));
  for (let i = 0; i < maxLen; i++) {
    const dst = start + i;
    if (dst < 0 || dst >= N) { if (dst >= N) break; else continue; }
    let s = 0;
    for (const p of ps) {
      const env = p.amp * Math.pow(p.decay, i);
      if (env < 1e-5) continue;
      s += Math.sin(p.omega * i) * env;
    }
    let att = 1;
    if (i < ATTACK) att = 0.5 - 0.5 * Math.cos((Math.PI * i) / ATTACK);
    out[dst] += s * att * gain;
  }
}

// warm, open chord (C lydian-ish / add9-6) across octaves — no harsh seconds
const VOICES = [
  { midi: 48, period: 23.0, phase: 0.0,  gain: 0.55 }, // C3
  { midi: 55, period: 19.1, phase: 5.0,  gain: 0.5  }, // G3
  { midi: 60, period: 17.3, phase: 2.0,  gain: 0.5  }, // C4
  { midi: 64, period: 13.7, phase: 8.0,  gain: 0.45 }, // E4
  { midi: 67, period: 29.0, phase: 11.0, gain: 0.42 }, // G4
  { midi: 69, period: 21.7, phase: 3.5,  gain: 0.4  }, // A4
  { midi: 74, period: 31.0, phase: 15.0, gain: 0.34 }, // D5 (the 9, sparkle)
];

// deterministic tiny PRNG for slight humanization
let seed = 1337;
const rnd = () => ((seed = (seed * 1103515245 + 12345) & 0x7fffffff) / 0x7fffffff);

for (const v of VOICES) {
  for (let t = v.phase; t < DUR - 1; t += v.period) {
    const g = v.gain * (0.7 + 0.5 * rnd());
    const det = (rnd() - 0.5) * 8; // ±4 cents shimmer
    const jitter = (rnd() - 0.5) * 0.8;
    strike(v.midi, Math.max(0, t + jitter), g, det);
  }
}

// quiet sustained drone (C2 + G2) with very slow tremolo for warmth
for (let i = 0; i < N; i++) {
  const tt = i / SR;
  const trem = 0.85 + 0.15 * Math.sin(TAU * 0.045 * tt);
  const fade = Math.min(1, tt / 4) * Math.min(1, (DUR - tt) / 6); // ease in/out
  out[i] += (Math.sin((TAU * midiToFreq(36) * i) / SR) * 0.045
           + Math.sin((TAU * midiToFreq(43) * i) / SR) * 0.032) * trem * fade;
}

// normalize to ~ -6 dBFS (it's a bed; compose ducks it further)
let peak = 0;
for (let i = 0; i < N; i++) { const a = Math.abs(out[i]); if (a > peak) peak = a; }
if (peak > 0) { const norm = 0.5 / peak; for (let i = 0; i < N; i++) out[i] *= norm; }

const raw = "/tmp/ambient-bed.f32";
writeFileSync(raw, Buffer.from(out.buffer));
const ff = spawnSync("ffmpeg", [
  "-y", "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", raw,
  "-c:a", "libmp3lame", "-q:a", "4", OUT,
], { stdio: "inherit" });
try { unlinkSync(raw); } catch {}
console.log(ff.status === 0 ? `✓ ${OUT} (${DUR}s)` : `✗ ffmpeg failed`);
