#!/usr/bin/env node
// nsscore-bake-audio — offline binaural render of an .nsscore.
//
// Full 3D over headphones, not a stereo pan: this is a JS port of the
// house spatializer `pop/nullabye/c/ac_hrtf.h` — Woodworth-scale ITD
// (~650 μs max) on a fractional-delay ring, moving head-shadow lowpass
// on the far ear, and elevation-dependent pinna comb notches, so ABOVE
// and BELOW read distinctly the way Devil Daggers' HRTF path does
// (OpenAL Soft + .mhr filter sets; same cue set, measured there,
// procedural here). Headphones strongly recommended.
//
// Position comes from the score: the `rotation` ribbon is orbit SPEED
// (integrated to azimuth, K laps/sec at 1.0 — the diagram integrates
// the same curve), the optional `elevation` ribbon is height in
// -1..1 → ∓90°, and `distance` breathes proximity.
//
//   node nsscore-bake-audio.mjs <score.nsscore> [out.wav]
//        [--spread 1.6]   lateral drama (1 = physical, >1 exaggerated)
//        [--laps 0.5]     laps/sec at ribbon 1.0

import { readFileSync, writeFileSync } from "node:fs";

const args = process.argv.slice(2);
const opt = (k, d) => { const i = args.indexOf("--" + k); return i >= 0 ? parseFloat(args[i + 1]) : d; };
const [inPath, outArg] = args.filter(a => !a.startsWith("--") && isNaN(parseFloat(a)));
if (!inPath) { console.error("usage: nsscore-bake-audio.mjs <score.nsscore> [out.wav] [--spread 1.6] [--laps 0.5]"); process.exit(1); }
const SPREAD = opt("spread", 1.6);
const K = opt("laps", 0.5);
const S = JSON.parse(readFileSync(inPath, "utf8"));

const SR = 48000, TAIL = 2;
const RING = 256, MASK = RING - 1;
const n = Math.ceil((S.dur + TAIL) * SR);
const L = new Float64Array(n), Rc = new Float64Array(n);

// ── ac_hrtf.h, ported ────────────────────────────────────────────────
class Hrtf {
  constructor() { this.ring = new Float64Array(RING); this.at = 0; this.sL = 0; this.sR = 0; }
  read(delay) { // cubic interpolation of the fractional tap
    let p = this.at - delay;
    while (p < 0) p += RING;
    const b = Math.floor(p) & MASK, a = (b - 1) & MASK, c = (b + 1) & MASK, d = (b + 2) & MASK;
    const f = p - Math.floor(p);
    const y0 = this.ring[a], y1 = this.ring[b], y2 = this.ring[c], y3 = this.ring[d];
    return y1 + 0.5 * f * (y2 - y0 + f * (2 * y0 - 5 * y1 + 4 * y2 - y3 + f * (3 * (y1 - y2) + y3 - y0)));
  }
  process(input, azimuth, elevation, distance) {
    // compressed inverse-distance: proximity stays musical, no cliffs
    const near = 0.012 + 0.988 / (1 + 0.18 * distance * distance);
    const side = Math.max(-1, Math.min(1, Math.sin(azimuth) * SPREAD));
    this.at = (this.at + 1) & MASK;
    this.ring[this.at] = input * near;
    const itd = Math.abs(side) * 31.2;           // ≈650 μs at 48 kHz
    const dl = side > 0 ? itd : 0, dr = side < 0 ? itd : 0;
    let l = this.read(dl), r = this.read(dr);
    // pinna reflections move with elevation — above is a short tap,
    // below a longer one; the subtraction makes the direction notch
    const en = Math.max(-1, Math.min(1, elevation / (Math.PI * 0.5)));
    const tap1 = 15 - en * 8, tap2 = 31 - en * 10;
    l -= this.read(dl + tap1) * (0.25 + 0.05 * side); l += this.read(dl + tap2) * 0.11;
    r -= this.read(dr + tap1) * (0.25 - 0.05 * side); r += this.read(dr + tap2) * 0.11;
    // far-ear head shadow: a moving one-pole mixed against the direct ear
    const shadow = 0.18 + 0.62 * Math.abs(side), a = 0.72 + 0.20 * Math.abs(side);
    this.sL = (1 - a) * l + a * this.sL;
    this.sR = (1 - a) * r + a * this.sR;
    if (side > 0) l = l * (1 - shadow) + this.sL * shadow;
    else r = r * (1 - shadow) + this.sR * shadow;
    const eg = 1 + 0.08 * Math.abs(en);          // pinna-notch loudness trim
    return [l * eg, r * eg];
  }
}

// ── position tables: azimuth integrated from the ribbon, elevation read
const azTable = new Float64Array(n), elTable = new Float64Array(n), diTable = new Float64Array(n);
{
  const spin = S.rotation || [0];
  const elev = S.elevation || null;
  const dist = S.distance || null;
  const sample = (arr, τ) => arr[Math.max(0, Math.min(arr.length - 1, Math.round((τ / S.dur) * (arr.length - 1))))] || 0;
  let a = 0;
  for (let i = 0; i < n; i++) {
    const τ = i / SR;
    a += (sample(spin, τ) * K * 2 * Math.PI) / SR;
    azTable[i] = a;
    elTable[i] = elev ? sample(elev, τ) * (Math.PI / 2) : 0;
    diTable[i] = dist ? 0.5 + sample(dist, τ) * 2.5 : 1.1;
  }
}

const osc = (wave, ph) =>
  wave === "triangle" ? 2 * Math.abs(2 * (ph - Math.floor(ph + 0.5))) - 1
  : wave === "noise" ? Math.random() * 2 - 1
  : Math.sin(2 * Math.PI * ph);

for (const lane of S.lanes) {
  for (const e of lane.events) {
    const h = new Hrtf();                        // per-voice state, per event
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
      const v = osc(e.wave, ph) * env * e.g * 0.62;
      const [l, r] = h.process(v, azTable[s0 + i] + (e.azOffset || 0), elTable[s0 + i], diTable[s0 + i]);
      L[s0 + i] += l; Rc[s0 + i] += r;
    }
  }
}

// peak-normalize to -1 dBFS — the HRTF's combs cost level unevenly
let peak = 0;
for (let i = 0; i < n; i++) peak = Math.max(peak, Math.abs(L[i]), Math.abs(Rc[i]));
const g = peak > 0 ? 0.89 / peak : 1;

const pcm = Buffer.alloc(n * 4);
for (let i = 0; i < n; i++) {
  pcm.writeInt16LE(Math.max(-32767, Math.min(32767, Math.round(L[i] * g * 32767))), i * 4);
  pcm.writeInt16LE(Math.max(-32767, Math.min(32767, Math.round(Rc[i] * g * 32767))), i * 4 + 2);
}
const hdr = Buffer.alloc(44);
hdr.write("RIFF", 0); hdr.writeUInt32LE(36 + pcm.length, 4); hdr.write("WAVE", 8);
hdr.write("fmt ", 12); hdr.writeUInt32LE(16, 16); hdr.writeUInt16LE(1, 20);
hdr.writeUInt16LE(2, 22); hdr.writeUInt32LE(SR, 24); hdr.writeUInt32LE(SR * 4, 28);
hdr.writeUInt16LE(4, 32); hdr.writeUInt16LE(16, 34);
hdr.write("data", 36); hdr.writeUInt32LE(pcm.length, 40);

const dest = outArg || inPath.replace(/\.nsscore$/, "") + ".wav";
writeFileSync(dest, Buffer.concat([hdr, pcm]));
console.log(`${dest} — ${(n / SR).toFixed(1)}s binaural (ITD + shadow + pinna elevation), spread ${SPREAD}, ${K} laps/s`);
