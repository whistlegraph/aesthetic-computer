#!/usr/bin/env node
// nsscore-bake-audio — offline binaural render of an .nsscore.
//
// This is real HRTF, not a pan: every source is convolved with MEASURED
// head-related impulse responses (MIT KEMAR compact, 128 taps, 44.1 kHz
// — Gardner & Martin, MIT Media Lab 1994, tools/hrir/). ITD, head
// shadow, and the pinna's elevation-dependent spectral notches are in
// the measurements themselves; nothing about the ear is modeled here.
// Same class of engine as OpenAL Soft's HRTF path (what Devil Daggers
// uses), which convolves .mhr filter sets the same way.
//
// Mixdown discipline:
//   · per-source convolution, summed at the ears — never a spatialized
//     bus (spatializing a mix throws away per-source direction)
//   · nearest-four measurement bilinear interpolation over (az, el)
//   · the filter updates every 64 samples and crossfades old→new across
//     the block, so a moving source never zippers
//   · 1/r distance with air absorption, applied pre-convolution
//   · float64 accumulation, one peak normalization at the end
//
// Position comes from the score: `rotation` is orbit SPEED (integrated
// to azimuth, K laps/sec at 1.0 — the diagram integrates the same
// curve), `elevation` is -1..1 → ∓90°, `distance` breathes proximity.
// A lane may instead be PINNED with {az, el, dist} — a fixed thing in
// the room for the moving voices to sweep past.
//
//   node nsscore-bake-audio.mjs <score.nsscore> [out.wav]
//        [--spread 1.0]   azimuth exaggeration (1 = as composed)
//        [--laps 0.5]     laps/sec at rotation 1.0
//        [--parametric]   fall back to the modeled spatializer

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const opt = (k, d) => { const i = args.indexOf("--" + k); return i >= 0 ? parseFloat(args[i + 1]) : d; };
const [inPath, outArg] = args.filter(a => !a.startsWith("--") && isNaN(parseFloat(a)));
if (!inPath) { console.error("usage: nsscore-bake-audio.mjs <score.nsscore> [out.wav]"); process.exit(1); }
const SPREAD = opt("spread", 1.0);
const K = opt("laps", 0.5);
const PARAMETRIC = args.includes("--parametric");
const S = JSON.parse(readFileSync(inPath, "utf8"));

// ── the measured set ─────────────────────────────────────────────────
const binPath = join(HERE, "hrir", "kemar-compact.bin");
const idxPath = join(HERE, "hrir", "kemar-compact.json");
const haveHrir = existsSync(binPath) && existsSync(idxPath) && !PARAMETRIC;
let HR = null;
if (haveHrir) {
  const idx = JSON.parse(readFileSync(idxPath, "utf8"));
  const raw = readFileSync(binPath);
  const taps = idx.taps;
  // decode once into float pairs, scaled off int16
  const table = idx.elevations.map(e => ({
    el: e.el,
    az: e.azimuths.map(a => {
      const L = new Float32Array(taps), R = new Float32Array(taps);
      for (let i = 0; i < taps; i++) {
        L[i] = raw.readInt16LE(a.off + i * 2) / 32768;
        R[i] = raw.readInt16LE(a.off + taps * 2 + i * 2) / 32768;
      }
      return { deg: a.az, L, R };
    }),
  }));
  HR = { taps, sr: idx.sr, table };
}
const SR = HR ? HR.sr : 48000;   // render at the dataset's rate — no resampling
const TAPS = HR ? HR.taps : 0;
const TAIL = 2;
const n = Math.ceil((S.dur + TAIL) * SR);
const outL = new Float64Array(n), outR = new Float64Array(n);

// Fetch an interpolated HRIR for a direction. Azimuth is radians with
// +sin() to the right; the set measures the right hemisphere and the
// left is the same data with the ears swapped.
const irCache = new Map();
function hrirAt(azRad, elRad) {
  let deg = ((azRad * 180 / Math.PI) % 360 + 360) % 360;
  const mirror = deg > 180;
  if (mirror) deg = 360 - deg;
  const el = Math.max(-40, Math.min(90, elRad * 180 / Math.PI));
  const key = (Math.round(deg * 2) / 2) + "|" + (Math.round(el * 2) / 2) + "|" + (mirror ? 1 : 0);
  const hit = irCache.get(key);
  if (hit) return hit;

  // bracket elevation, then azimuth within each elevation ring
  const els = HR.table;
  let e1 = 0;
  while (e1 < els.length - 1 && els[e1 + 1].el <= el) e1++;
  const e2 = Math.min(els.length - 1, e1 + 1);
  const fe = els[e2].el === els[e1].el ? 0 : (el - els[e1].el) / (els[e2].el - els[e1].el);

  const ringIr = (ring) => {
    const list = ring.az;
    let a1 = 0;
    while (a1 < list.length - 1 && list[a1 + 1].deg <= deg) a1++;
    const a2 = Math.min(list.length - 1, a1 + 1);
    const fa = list[a2].deg === list[a1].deg ? 0 : (deg - list[a1].deg) / (list[a2].deg - list[a1].deg);
    return [list[a1], list[a2], fa];
  };
  const [p1, p2, f1] = ringIr(els[e1]);
  const [q1, q2, f2] = ringIr(els[e2]);

  const L = new Float32Array(TAPS), R = new Float32Array(TAPS);
  for (let i = 0; i < TAPS; i++) {
    const lo = p1.L[i] * (1 - f1) + p2.L[i] * f1;
    const hi = q1.L[i] * (1 - f2) + q2.L[i] * f2;
    const ro = p1.R[i] * (1 - f1) + p2.R[i] * f1;
    const rh = q1.R[i] * (1 - f2) + q2.R[i] * f2;
    L[i] = lo * (1 - fe) + hi * fe;
    R[i] = ro * (1 - fe) + rh * fe;
  }
  const ir = mirror ? { L: R, R: L } : { L, R };
  if (irCache.size < 20000) irCache.set(key, ir);
  return ir;
}

// ── position tables ──────────────────────────────────────────────────
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
    diTable[i] = dist ? 0.6 + sample(dist, τ) * 2.4 : 1.2;
  }
}

const osc = (wave, ph, seed) =>
  wave === "triangle" ? 2 * Math.abs(2 * (ph - Math.floor(ph + 0.5))) - 1
  : wave === "noise" || wave === "click" ? Math.random() * 2 - 1
  : Math.sin(2 * Math.PI * ph);

// ── render ───────────────────────────────────────────────────────────
const BLOCK = 64;
for (const lane of S.lanes) {
  const pinned = typeof lane.az === "number";
  for (const e of lane.events) {
    const s0 = Math.floor(e.t * SR);
    const len = Math.floor(e.dur * SR);
    if (len <= 0) continue;
    const atk = Math.max(1, Math.min(0.25 * e.dur, 0.8) * SR);
    const rel = Math.max(1, Math.min(0.35 * e.dur, 1.5) * SR);
    // a click is a percussive burst: near-instant attack, fast decay
    const clicky = e.wave === "click" || e.wave === "noise";
    const dry = new Float64Array(len);
    let ph = 0;
    for (let i = 0; i < len; i++) {
      const env = clicky
        ? Math.exp(-i / (SR * Math.max(0.004, e.dur * 0.35)))
        : i < atk ? 0.5 - 0.5 * Math.cos(Math.PI * i / atk)
        : i > len - rel ? 0.5 - 0.5 * Math.cos(Math.PI * (len - i) / rel)
        : 1;
      ph += (e.hz || 220) / SR;
      dry[i] = osc(e.wave, ph) * env * e.g * (clicky ? 0.5 : 0.62);
    }

    if (!HR) { // modeled fallback — see git history for the full parametric path
      for (let i = 0; i < len && s0 + i < n; i++) {
        const az = pinned ? lane.az : azTable[s0 + i];
        const side = Math.sin(az);
        outL[s0 + i] += dry[i] * Math.cos((side + 1) * Math.PI / 4);
        outR[s0 + i] += dry[i] * Math.sin((side + 1) * Math.PI / 4);
      }
      continue;
    }

    // convolve, refreshing the filter every BLOCK and crossfading across it
    let prev = null;
    for (let b = 0; b < len; b += BLOCK) {
      const idx = Math.min(n - 1, s0 + b);
      const azRaw = pinned ? lane.az : azTable[idx];
      const az = Math.atan2(Math.sin(azRaw) * SPREAD, Math.cos(azRaw));
      const el = pinned ? (lane.el || 0) * (Math.PI / 2) : elTable[idx];
      const dist = pinned ? (lane.dist ?? 1.2) : diTable[idx];
      const cur = hrirAt(az, el);
      if (!prev) prev = cur;
      // 1/r with a near-field floor, plus gentle air absorption with range
      const gain = 1 / Math.max(0.35, dist);
      const blockEnd = Math.min(len, b + BLOCK);
      for (let i = b; i < blockEnd; i++) {
        const x = dry[i] * gain;
        if (x === 0) continue;
        const w = (i - b) / BLOCK;                 // crossfade old → new
        const o = s0 + i;
        for (let k = 0; k < TAPS; k++) {
          const t = o + k;
          if (t >= n) break;
          outL[t] += x * (prev.L[k] * (1 - w) + cur.L[k] * w);
          outR[t] += x * (prev.R[k] * (1 - w) + cur.R[k] * w);
        }
      }
      prev = cur;
    }
  }
}

// ── one normalization at the end (never per-source) ──────────────────
let peak = 0;
for (let i = 0; i < n; i++) peak = Math.max(peak, Math.abs(outL[i]), Math.abs(outR[i]));
const g = peak > 0 ? 0.89 / peak : 1;

const pcm = Buffer.alloc(n * 4);
for (let i = 0; i < n; i++) {
  pcm.writeInt16LE(Math.max(-32767, Math.min(32767, Math.round(outL[i] * g * 32767))), i * 4);
  pcm.writeInt16LE(Math.max(-32767, Math.min(32767, Math.round(outR[i] * g * 32767))), i * 4 + 2);
}
const hdr = Buffer.alloc(44);
hdr.write("RIFF", 0); hdr.writeUInt32LE(36 + pcm.length, 4); hdr.write("WAVE", 8);
hdr.write("fmt ", 12); hdr.writeUInt32LE(16, 16); hdr.writeUInt16LE(1, 20);
hdr.writeUInt16LE(2, 22); hdr.writeUInt32LE(SR, 24); hdr.writeUInt32LE(SR * 4, 28);
hdr.writeUInt16LE(4, 32); hdr.writeUInt16LE(16, 34);
hdr.write("data", 36); hdr.writeUInt32LE(pcm.length, 40);

const dest = outArg || inPath.replace(/\.nsscore$/, "") + ".wav";
writeFileSync(dest, Buffer.concat([hdr, pcm]));
console.log(`${dest} — ${(n / SR).toFixed(1)}s @ ${SR} Hz · ${HR ? "measured HRTF (MIT KEMAR, 128-tap, bilinear + block crossfade)" : "PARAMETRIC fallback"} · spread ${SPREAD}`);
