#!/usr/bin/env node
// nsscore-bake-audio — offline render of an .nsscore to stereo wav.
// Built for the single-emitter spatial tests: the rotation ribbon is
// read as ORBIT SPEED and integrated into the emitter's angle (K = 0.5
// laps/sec at ribbon 1.0 — the diagram renderer integrates the same
// curve, so the pan you hear is the position you see). Constant-power
// L/R pan plus a gentle front/back amplitude cue. Sine/triangle/noise
// voices, raised-cosine event envelopes.
//
//   node nsscore-bake-audio.mjs <score.nsscore> [out.wav]

import { readFileSync, writeFileSync } from "node:fs";

const [, , inPath, outArg] = process.argv;
if (!inPath) { console.error("usage: nsscore-bake-audio.mjs <score.nsscore> [out.wav]"); process.exit(1); }
const S = JSON.parse(readFileSync(inPath, "utf8"));

const SR = 48000, TAIL = 2;
const K = 0.5; // laps per second at ribbon value 1.0 — shared with the diagram
const n = Math.ceil((S.dur + TAIL) * SR);
const L = new Float64Array(n), Rc = new Float64Array(n);

// integrate the ribbon once into a per-sample angle table (radians)
const angle = new Float64Array(n);
{
  const env = S.rotation || [0];
  let a = 0;
  for (let i = 0; i < n; i++) {
    const τ = i / SR;
    const e = env[Math.max(0, Math.min(env.length - 1, Math.round((τ / S.dur) * (env.length - 1))))] || 0;
    a += (e * K * 2 * Math.PI) / SR;
    angle[i] = a;
  }
}

const osc = (wave, ph) =>
  wave === "triangle" ? 2 * Math.abs(2 * (ph - Math.floor(ph + 0.5))) - 1
  : wave === "noise" ? Math.random() * 2 - 1
  : Math.sin(2 * Math.PI * ph);

for (const lane of S.lanes) {
  for (const e of lane.events) {
    const s0 = Math.floor(e.t * SR);
    const len = Math.floor(e.dur * SR);
    const atk = Math.min(0.25 * e.dur, 0.8) * SR;
    const rel = Math.min(0.35 * e.dur, 1.5) * SR;
    let ph = 0;
    for (let i = 0; i < len && s0 + i < n; i++) {
      const env = i < atk ? 0.5 - 0.5 * Math.cos(Math.PI * i / atk)
                : i > len - rel ? 0.5 - 0.5 * Math.cos(Math.PI * (len - i) / rel)
                : 1;
      ph += (e.hz || 220) / SR;
      const v = osc(e.wave, ph) * env * e.g * 0.55;
      const a = angle[s0 + i];
      const pan = Math.sin(a);                     // -1 left … +1 right
      const depth = 0.82 + 0.18 * Math.cos(a);    // faintly quieter behind
      const th = (pan + 1) * Math.PI / 4;          // constant-power law
      L[s0 + i] += v * Math.cos(th) * depth;
      Rc[s0 + i] += v * Math.sin(th) * depth;
    }
  }
}

// 16-bit stereo wav
const pcm = Buffer.alloc(n * 4);
for (let i = 0; i < n; i++) {
  pcm.writeInt16LE(Math.max(-32767, Math.min(32767, Math.round(L[i] * 32767))), i * 4);
  pcm.writeInt16LE(Math.max(-32767, Math.min(32767, Math.round(Rc[i] * 32767))), i * 4 + 2);
}
const hdr = Buffer.alloc(44);
hdr.write("RIFF", 0); hdr.writeUInt32LE(36 + pcm.length, 4); hdr.write("WAVE", 8);
hdr.write("fmt ", 12); hdr.writeUInt32LE(16, 16); hdr.writeUInt16LE(1, 20);
hdr.writeUInt16LE(2, 22); hdr.writeUInt32LE(SR, 24); hdr.writeUInt32LE(SR * 4, 28);
hdr.writeUInt16LE(4, 32); hdr.writeUInt16LE(16, 34);
hdr.write("data", 36); hdr.writeUInt32LE(pcm.length, 40);

const dest = outArg || inPath.replace(/\.nsscore$/, "") + ".wav";
writeFileSync(dest, Buffer.concat([hdr, pcm]));
console.log(`${dest} — ${(n / SR).toFixed(1)}s stereo, orbit-panned (K=${K})`);
