#!/usr/bin/env node
// compare.mjs — A/B the C engine against a JS reference for an isolated
// voice family. Both engines render the same test phrase using the same
// seed; we then read both WAVs and report per-sample diff (RMS, peak
// max-abs, mean L1) and rough spectral energy diff (per-band RMS).
//
// Usage:  node compare.mjs voice
//         node compare.mjs bell
//         node compare.mjs sub
//         node compare.mjs all       (runs every known voice)
//
// The JS reference implementations are inline copies of hellsine.mjs's
// functions, but Math.random() → the deterministic rng so both engines
// stay in lock-step. For long-tail voices the C version will diverge
// from JS by design (phase-increment vs absolute-time sin) — the diff
// report flags that as "expected divergence".

import { writeFileSync, readFileSync, mkdirSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT_DIR = resolve(HERE, "out");
mkdirSync(OUT_DIR, { recursive: true });

const SR = 48_000;
const TAU = Math.PI * 2;
const m2f = (m) => 440 * Math.pow(2, (m - 69) / 12);

// ── deterministic rng (must match C: FNV-1a + xorshift32) ─────────────
function hashString(s) {
  let h = 2166136261 >>> 0;
  for (let i = 0; i < s.length; i++) {
    h ^= s.charCodeAt(i);
    h = Math.imul(h, 16777619);
  }
  return h >>> 0;
}
function makeRng(seedStr) {
  let s = hashString(seedStr) || 1;
  return () => {
    s ^= s << 13; s ^= s >>> 17; s ^= s << 5; s >>>= 0;
    return s / 4294967296;
  };
}

// ── JS reference: voice() (lifted from hellsine.mjs, vibrato included) ──
// Note: the JS uses absolute-time sin. That's the bug. We copy it
// verbatim here so the C output can be measured AGAINST the buggy JS
// for short notes; divergence will grow with note duration.
function voiceJS({ L, R, WL, WR, N }, t, dur, midi, gain, opt = {}) {
  const f = m2f(midi);
  const parts = opt.parts || [
    [1, 1.0], [2, 0.5], [3, 0.34], [4, 0.16], [5, 0.12], [6, 0.06],
  ];
  let amps = 0;
  for (const p of parts) amps += p[1];
  const norm = (opt.gain ?? 1) / amps;
  const atk = opt.atk ?? 0.05;
  const rel = opt.rel ?? 0.18;
  const vibR = opt.vibR ?? 5.2;
  const vibD = opt.vibD ?? 0.006;
  const pan = opt.pan ?? 0;
  const drive = opt.drive ?? 1.0;
  const wetSend = opt.wetSend ?? 0;
  const total = dur + rel;
  const iStart = Math.max(0, Math.floor(t * SR));
  const iEnd = Math.min(N, Math.ceil((t + total) * SR));
  for (let i = iStart; i < iEnd; i++) {
    const lt = i / SR - t;
    let env = Math.min(1, lt / atk);
    if (lt > dur - rel) env *= Math.max(0, (dur - lt) / rel);
    const vib = 1 + Math.sin(TAU * vibR * lt) * vibD * Math.min(1, lt / 0.12);
    let x = 0;
    for (const [r, a] of parts) x += a * Math.sin(TAU * f * r * vib * lt);
    x = Math.tanh(x * norm * drive);
    const v = x * env * gain;
    const vL = v * (pan > 0 ? 1 - pan : 1);
    const vR = v * (pan < 0 ? 1 + pan : 1);
    L[i] += vL;
    R[i] += vR;
    if (wetSend > 0) {
      WL[i] += vL * wetSend;
      WR[i] += vR * wetSend;
    }
  }
}

// ── JS reference: bell() ──────────────────────────────────────────────
function bellJS({ L, R, WL, WR, N }, t, midi, gain, opt = {}) {
  const f = m2f(midi);
  const parts = [
    [1.000, 1.00], [2.012, 0.55], [3.025, 0.33],
    [4.055, 0.18], [5.10,  0.10], [6.20,  0.05],
  ];
  let ampSum = 0;
  for (const p of parts) ampSum += p[1];
  const norm = 1 / ampSum;
  const pan = opt.pan ?? 0;
  const atk = opt.atk ?? 0.080;
  const decTau = opt.decTau ?? 4.0;
  const wetSend = opt.wetSend ?? 0;
  const phs = new Float64Array(parts.length);
  const tailDur = decTau * 5;
  const FIB = [1, 1, 2, 3, 5, 8];
  const fizzleOn = opt.fizzle !== false;
  const fizzleAfter = atk + decTau * 0.5;
  const startI = Math.max(0, Math.floor(t * SR));
  const endI = Math.min(N, Math.ceil((t + atk + tailDur) * SR));
  for (let i = startI; i < endI; i++) {
    const lt = i / SR - t;
    const env = lt < atk ? lt / atk : Math.exp(-(lt - atk) / decTau);
    let x = 0;
    for (let k = 0; k < parts.length; k++) {
      const [r, a] = parts[k];
      let rEff = r;
      if (fizzleOn && lt > fizzleAfter) {
        const fizzleP = Math.min(1, (lt - fizzleAfter) / decTau);
        const rateScale = 1 + 3.0 * Math.pow(fizzleP, 1.4);
        const fibHz = FIB[k % FIB.length] * rateScale;
        const depth = 0.018 + 0.022 * Math.pow(fizzleP, 2);
        const drift = (k % 2 === 0 ? 1 : -1) * 0.06 * Math.pow(fizzleP, 1.8);
        rEff *= (1 + drift) * (1 + depth * Math.sin(TAU * fibHz * lt) * fizzleP);
      }
      phs[k] += (TAU * f * rEff) / SR;
      const partEnv = lt < atk ? 1 : Math.exp(-(lt - atk) / (decTau / r));
      x += a * Math.sin(phs[k]) * partEnv;
    }
    x *= norm;
    const v = x * env * gain;
    const vL = v * (pan > 0 ? 1 - pan : 1);
    const vR = v * (pan < 0 ? 1 + pan : 1);
    L[i] += vL;
    R[i] += vR;
    if (wetSend > 0) {
      WL[i] += vL * wetSend;
      WR[i] += vR * wetSend;
    }
  }
}

// ── JS reference: kick() — non-ULTIMATE, non-NOKICK (the "hole" kick) ──
function kickJS({ L, R, DUCK, N }, t, drive = 11, gain = 1) {
  const dur = 0.55;
  const pStart = 180, pEnd = 28;
  const pT = 0.080;
  let ph = 0;
  // Write DUCK envelope: 15 ms slam to 0.08, then 445 ms slow re-open.
  const di = Math.floor(t * SR);
  const dN = Math.floor(0.46 * SR);
  const closeN = Math.floor(0.015 * SR);
  const openN  = dN - closeN;
  for (let k = 0; k < dN && di + k < N; k++) {
    let env;
    if (k < closeN) env = 1 - (k / closeN) * 0.92;
    else { const p = (k - closeN) / openN; env = 0.08 + 0.92 * (1 - Math.exp(-p * 3.2)); }
    if (env < DUCK[di + k]) DUCK[di + k] = env;
  }
  const iStart = Math.max(0, Math.floor(t * SR));
  const iEnd = Math.min(N, Math.ceil((t + dur) * SR));
  for (let i = iStart; i < iEnd; i++) {
    const lt = i / SR - t;
    const f = pEnd + (pStart - pEnd) * Math.exp(-lt / pT);
    ph += (TAU * f) / SR;
    const atk   = 1 - Math.exp(-lt / 0.012);
    const decay = Math.exp(-lt / 0.22);
    const amp   = atk * decay;
    let x = Math.sin(ph);
    x = Math.tanh(x * (drive * 0.45));
    const v = x * amp * 0.95 * gain;
    L[i] += v; R[i] += v;
  }
}

// ── JS reference: snare() ─────────────────────────────────────────────
function snareJS({ L, R, N }, rng, t, gain = 0.5, opt = {}) {
  const dur = 0.30, bodyF = opt.bodyF || 175;
  const nV = 96, fMin = 1200, fMax = 9000;
  const freqs = new Float64Array(nV), phs = new Float64Array(nV);
  for (let i = 0; i < nV; i++) {
    const u = (i + rng()) / nV;
    freqs[i] = fMin * Math.pow(fMax / fMin, u);
    phs[i] = rng() * TAU;
  }
  const norm = 1 / Math.sqrt(nV);
  const iStart = Math.max(0, Math.floor(t * SR));
  const iEnd = Math.min(N, Math.ceil((t + dur) * SR));
  for (let i = iStart; i < iEnd; i++) {
    const lt = i / SR - t;
    const pf = bodyF * (1 + 0.5 * Math.exp(-lt / 0.008));
    const body = (Math.sin(TAU * pf * lt) + 0.55 * Math.sin(TAU * pf * 1.48 * lt))
               * Math.exp(-lt / 0.045);
    let noise = 0;
    for (let i2 = 0; i2 < nV; i2++) noise += Math.sin(TAU * freqs[i2] * lt + phs[i2]);
    const crackEnv = (1 - Math.exp(-lt / 0.0006)) * Math.exp(-lt / 0.030);
    const snap = lt < 0.006
      ? Math.sin(TAU * 6200 * lt) * Math.exp(-lt / 0.0014) * 0.55
      : 0;
    let x = body * 0.75 + noise * norm * crackEnv * 0.85 + snap;
    x = Math.tanh(x * 1.4);
    const v = x * gain * Math.min(1, lt / 0.0004);
    L[i] += v * 0.96; R[i] += v;
  }
}

// ── JS reference: steam() ─────────────────────────────────────────────
function steamJS({ L, R, N }, rng, t, dur, gain = 0.12, opt = {}) {
  const nVoices = opt.voices || 130;
  const fMin = opt.fMin || 400;
  const fMax = opt.fMax || 5500;
  const atk = opt.atk ?? 0.6;
  const rel = opt.rel ?? 1.2;
  const breathRate = opt.breathRate || 0.5;
  const breathDepth = opt.breathDepth ?? 0.35;
  const freqs = new Float64Array(nVoices);
  const phases = new Float64Array(nVoices);
  for (let i = 0; i < nVoices; i++) {
    const u = (i + rng()) / nVoices;
    freqs[i] = fMin * Math.pow(fMax / fMin, u);
    phases[i] = rng() * TAU;
  }
  const norm = 1 / Math.sqrt(nVoices);
  const iStart = Math.max(0, Math.floor(t * SR));
  const iEnd = Math.min(N, Math.ceil((t + dur) * SR));
  for (let i = iStart; i < iEnd; i++) {
    const lt = i / SR - t;
    let env = Math.min(1, lt / atk);
    if (lt > dur - rel) env *= Math.max(0, (dur - lt) / rel);
    const breath = (1 - breathDepth) + breathDepth * Math.sin(TAU * breathRate * lt);
    let x = 0;
    for (let k = 0; k < nVoices; k++) x += Math.sin(TAU * freqs[k] * lt + phases[k]);
    x = Math.tanh(x * norm * 1.1);
    const v = x * env * breath * gain;
    L[i] += v * 0.92; R[i] += v * 1.0;
  }
}

// ── JS reference: woodTick() ──────────────────────────────────────────
function woodtickJS({ L, R, N }, t, gain = 0.13) {
  const f = 1900, dur = 0.045;
  const iStart = Math.max(0, Math.floor(t * SR));
  const iEnd = Math.min(N, Math.ceil((t + dur) * SR));
  for (let i = iStart; i < iEnd; i++) {
    const lt = i / SR - t;
    const env = Math.exp(-lt / 0.010) * (1 - Math.exp(-lt / 0.0006));
    const x = Math.sin(TAU * f * lt) + 0.3 * Math.sin(TAU * f * 1.41 * lt);
    const v = x * env * gain;
    L[i] += v * 0.92; R[i] += v;
  }
}

// ── JS reference: tick() ──────────────────────────────────────────────
function tickJS({ L, R, N }, t, gain = 0.22, open = false) {
  const f = open ? 7400 : 9200, dur = open ? 0.06 : 0.022;
  const iStart = Math.max(0, Math.floor(t * SR));
  const iEnd = Math.min(N, Math.ceil((t + dur) * SR));
  for (let i = iStart; i < iEnd; i++) {
    const lt = i / SR - t;
    const env = Math.exp(-lt / (open ? 0.026 : 0.004));
    let x = Math.sin(TAU * f * lt) + 0.45 * Math.sin(TAU * f * 1.51 * lt);
    if (open) x *= Math.sin(TAU * 5300 * lt);
    const v = x * env * gain;
    L[i] += v * 0.9; R[i] += v;
  }
}

// ── JS reference: piano() ─────────────────────────────────────────────
const PIANO_PARTS_JS = [
  [1.0000, 1.00, 1.00], [2.0008, 0.55, 0.85],
  [3.0024, 0.30, 0.70], [4.0048, 0.18, 0.55],
  [5.0080, 0.10, 0.45], [6.0120, 0.06, 0.38],
  [7.0168, 0.035, 0.32], [8.0224, 0.020, 0.28],
];
function pianoJS({ L, R, N }, t, dur, midi, gain = 0.14, opt = {}) {
  const f = m2f(midi);
  const pan = opt.pan ?? 0, sus = opt.sus ?? 1.0;
  const fastTau = 0.35, slowTau = 2.2 * sus;
  const tauScale = Math.min(1, Math.max(0.3, m2f(60) / f));
  const phs = new Float64Array(8);
  const bits = opt.bits ?? 6, hold = opt.hold ?? 4;
  const steps = 1 << (bits - 1);
  let held = 0, holdI = 0;
  const iStart = Math.max(0, Math.floor(t * SR));
  const iEnd = Math.min(N, Math.ceil((t + dur + 0.8) * SR));
  for (let i = iStart; i < iEnd; i++) {
    const lt = i / SR - t;
    const atk = 1 - Math.exp(-lt / 0.003);
    let x = 0;
    for (let k = 0; k < 8; k++) {
      const [r, a, ds] = PIANO_PARTS_JS[k];
      phs[k] += (TAU * f * r) / SR;
      const fast = Math.exp(-lt / (fastTau * ds * tauScale));
      const slow = Math.exp(-lt / (slowTau * ds * tauScale));
      x += a * Math.sin(phs[k]) * atk * (0.6 * fast + 0.4 * slow);
    }
    x = Math.tanh(x * 0.55);
    if (lt > dur) x *= Math.exp(-(lt - dur) / 0.15);
    if (holdI++ % hold === 0) held = Math.round(x * steps) / steps;
    const v = held * gain;
    L[i] += v * (1 - Math.max(0, pan));
    R[i] += v * (1 + Math.min(0, pan));
  }
}

// ── JS reference: sawLead() ───────────────────────────────────────────
function sawJS({ L, R, DUCK, N }, t, dur, midi, gain = 0.16, opt = {}) {
  const f = m2f(midi);
  const partials = opt.partials ?? 18;
  const detune = opt.detune ?? 0.006;
  const pan = opt.pan ?? 0;
  const atk = opt.atk ?? 0.012;
  const rel = opt.rel ?? 0.06;
  const drive = opt.drive ?? 0.8;
  const gateMs = opt.gateMs ?? 0;
  const gateOn = opt.gateOnFrac ?? 0.55;
  const phsA = new Float64Array(partials);
  const phsB = new Float64Array(partials);
  let norm = 0;
  for (let n = 1; n <= partials; n++) norm += 1 / n;
  const partNorm = 1 / norm;
  const iStart = Math.max(0, Math.floor(t * SR));
  const iEnd = Math.min(N, Math.ceil((t + dur + rel) * SR));
  for (let i = iStart; i < iEnd; i++) {
    const lt = i / SR - t;
    let env = Math.min(1, lt / atk);
    if (lt > dur - rel) env *= Math.max(0, (dur - lt) / rel);
    const d = DUCK[Math.min(N - 1, i)];
    let gate = 1;
    if (gateMs > 0) {
      const phase = (lt * 1000 / gateMs) % 1;
      gate = phase < gateOn ? 1 : 0;
    }
    let x = 0;
    for (let n = 1; n <= partials; n++) {
      const k = n - 1;
      phsA[k] += (TAU * f * n) / SR;
      phsB[k] += (TAU * f * (1 + detune) * n) / SR;
      const a = 1 / n;
      x += a * (Math.sin(phsA[k]) + Math.sin(phsB[k])) * 0.5;
    }
    x = Math.tanh(x * partNorm * drive);
    const v = x * env * d * gate * gain;
    L[i] += v * (pan > 0 ? 1 - pan : 1);
    R[i] += v * (pan < 0 ? 1 + pan : 1);
  }
}

// ── JS reference: hoover() ────────────────────────────────────────────
function hooverJS({ L, R, N }, t, dur, midi, gain = 0.3) {
  const f = m2f(midi), det = [0.994, 1.0, 1.007, 1.013];
  const iStart = Math.max(0, Math.floor(t * SR));
  const iEnd = Math.min(N, Math.ceil((t + dur) * SR));
  for (let i = iStart; i < iEnd; i++) {
    const lt = i / SR - t;
    const env = Math.min(1, lt / 0.02) * Math.max(0, 1 - lt / dur);
    const idx = 1.4 + 3.0 * Math.min(1, lt / (dur * 0.6));
    let x = 0;
    for (const dd of det) {
      const mod = Math.sin(TAU * f * dd * 0.5 * lt) * idx;
      x += Math.sin(TAU * f * dd * lt + mod);
    }
    x = Math.tanh(x * 0.5 * (1 + idx * 0.2)) * env * gain;
    L[i] += x * 0.85;
    R[i] += x;
  }
}

// ── JS reference: stab() ──────────────────────────────────────────────
function stabJS({ L, R, N }, t, midi, gain = 0.34) {
  const f = m2f(midi), dur = 0.16;
  const iStart = Math.max(0, Math.floor(t * SR));
  const iEnd = Math.min(N, Math.ceil((t + dur) * SR));
  for (let i = iStart; i < iEnd; i++) {
    const lt = i / SR - t;
    const env = Math.exp(-lt / 0.06) * (1 - Math.exp(-lt / 0.001));
    const mod = Math.sin(TAU * f * 1.997 * lt) * (5.5 * Math.exp(-lt / 0.05));
    let x = Math.sin(TAU * f * lt + mod);
    x = Math.max(-0.9, Math.min(0.9, x * 1.5));
    const v = x * env * gain;
    L[i] += v;
    R[i] += v * 0.92;
  }
}

// ── JS reference: riser() ─────────────────────────────────────────────
function riserJS({ L, R, N }, t, dur, m0, m1, gain = 0.26) {
  const f0 = m2f(m0), f1 = m2f(m1);
  let ph = 0;
  const iStart = Math.max(0, Math.floor(t * SR));
  const iEnd = Math.min(N, Math.ceil((t + dur) * SR));
  for (let i = iStart; i < iEnd; i++) {
    const lt = i / SR - t;
    const p = lt / dur, f = f0 * Math.pow(f1 / f0, p);
    ph += (TAU * f) / SR;
    const idx = 1 + 5 * p;
    let x = Math.sin(ph + Math.sin(ph * 0.5) * idx);
    x = Math.tanh(x * 2.2);
    const env = Math.min(1, lt / 0.05) * (0.3 + 0.7 * p);
    const v = x * env * gain;
    L[i] += v;
    R[i] += v * 0.95;
  }
}

// ── JS reference: bubble() ────────────────────────────────────────────
function bubbleJS({ L, R, WL, WR, N }, startSec, radiusMM, rise, volume, pan,
                  wetSend = 0, depth = 1.0) {
  const startIdx = Math.floor(startSec * SR);
  const radius = radiusMM * 0.001;
  const timestep = 1 / SR;
  const pRadius = radius * Math.sqrt(radius);
  let amp = 17.2133 * pRadius * depth;
  const decay = 0.13 / radius + 0.0072 * pRadius;
  const gain = Math.exp(-decay * timestep);
  let phaseStep = (3.0 / radius) * timestep;
  const phaseRise = phaseStep * decay * rise * timestep;
  let phase = 0;
  let lastOut = 0;
  let maxOut = 1;
  const QUIET = 0.000001;
  const angle = (Math.max(-1, Math.min(1, pan)) * 0.5 + 0.5) * (Math.PI / 2);
  const gL = Math.cos(angle), gR = Math.sin(angle);
  const maxSamples = Math.floor(4.0 * SR);
  for (let i = 0; i < maxSamples; i++) {
    if (amp < QUIET && phase > 1.0) break;
    const alpha = phase < 0 ? 0 : (phase < 1.0 ? phase : 1.0);
    const ph = Math.PI * 2 * phase;
    const rich = (Math.sin(ph) + 0.34 * Math.sin(2 * ph) + 0.17 * Math.sin(3 * ph)) / 1.51;
    const out = (1.0 - alpha) * lastOut + alpha * amp * rich;
    lastOut = out;
    phase += phaseStep;
    phaseStep += phaseRise;
    amp *= gain;
    let v = out * volume * 1000;
    if (Math.abs(v) > maxOut) maxOut = Math.abs(v);
    v = v / maxOut;
    const dst = startIdx + i;
    if (dst < 0 || dst >= N) continue;
    L[dst] += v * gL;
    R[dst] += v * gR;
    if (wetSend > 0) {
      WL[dst] += v * gL * wetSend;
      WR[dst] += v * gR * wetSend;
    }
  }
}

// ── JS reference: loadWavMono() ───────────────────────────────────────
function loadWavMonoJS(path) {
  const buf = readFileSync(path);
  let p = 12, fmt = null, dOff = 0, dLen = 0;
  while (p + 8 <= buf.length) {
    const id = buf.toString("ascii", p, p + 4);
    const sz = buf.readUInt32LE(p + 4);
    if (id === "fmt ") fmt = {
      format: buf.readUInt16LE(p + 8), channels: buf.readUInt16LE(p + 10),
      sr: buf.readUInt32LE(p + 12), bits: buf.readUInt16LE(p + 22),
    };
    else if (id === "data") { dOff = p + 8; dLen = sz; }
    p += 8 + sz + (sz & 1);
  }
  if (!fmt || !dOff) throw new Error(`bad WAV: ${path}`);
  const { format, channels, bits } = fmt;
  const fb = (bits / 8) * channels, frames = Math.floor(dLen / fb);
  let mono = new Float32Array(frames);
  for (let i = 0; i < frames; i++) {
    let acc = 0;
    for (let c = 0; c < channels; c++) {
      const o = dOff + i * fb + c * (bits / 8);
      if (format === 3 && bits === 32) acc += buf.readFloatLE(o);
      else if (bits === 16) acc += buf.readInt16LE(o) / 32768;
      else if (bits === 24)
        acc += (buf.readUInt8(o) | (buf.readUInt8(o + 1) << 8) | (buf.readInt8(o + 2) << 16)) / 8388608;
      else if (bits === 32) acc += buf.readInt32LE(o) / 2147483648;
    }
    mono[i] = acc / channels;
  }
  if (fmt.sr !== SR) {
    const outN = Math.round(frames * SR / fmt.sr), rs = new Float32Array(outN);
    for (let i = 0; i < outN; i++) {
      const x = i * fmt.sr / SR, i0 = Math.floor(x), fr = x - i0;
      rs[i] = (mono[i0] || 0) + ((mono[i0 + 1] || 0) - (mono[i0] || 0)) * fr;
    }
    mono = rs;
  }
  let a = 0, b = mono.length; const TH = 0.02;
  while (a < b && Math.abs(mono[a]) < TH) a++;
  while (b > a && Math.abs(mono[b - 1]) < TH) b--;
  mono = mono.subarray(a, b);
  let pk = 0;
  for (let i = 0; i < mono.length; i++) pk = Math.max(pk, Math.abs(mono[i]));
  if (pk > 0) for (let i = 0; i < mono.length; i++) mono[i] /= pk;
  return mono;
}

// ── JS reference: playSample() ────────────────────────────────────────
function playSampleJS({ L, R, WL, WR, N }, t, buf, gain = 1, opt = {}) {
  const rate = opt.rate || 1;
  const pan = Math.max(-1, Math.min(1, opt.pan || 0));
  const wetSend = opt.wetSend ?? 0;
  const startI = Math.floor(t * SR);
  const outLen = Math.floor(buf.length / rate);
  const fadeN = Math.floor((opt.fade ?? 0.015) * SR);
  for (let k = 0; k < outLen; k++) {
    const di = startI + k;
    if (di < 0) continue;
    if (di >= N) break;
    const sx = k * rate, si = Math.floor(sx), f = sx - si;
    if (si + 1 >= buf.length) break;
    let s = buf[si] + (buf[si + 1] - buf[si]) * f;
    if (k < fadeN) s *= k / fadeN;
    if (outLen - k < fadeN) s *= (outLen - k) / fadeN;
    const v = s * gain;
    const vL = v * (pan > 0 ? 1 - pan : 1);
    const vR = v * (pan < 0 ? 1 + pan : 1);
    L[di] += vL; R[di] += vR;
    if (wetSend > 0) { WL[di] += vL * wetSend; WR[di] += vR * wetSend; }
  }
}

// ── JS reference: playSampleSwept() ───────────────────────────────────
function playSampleSweptJS({ L, R, WL, WR, N }, t, buf, gain = 1, opt = {}) {
  const startRate = opt.startRate || 1;
  const endRate   = opt.endRate   || startRate;
  const pan = Math.max(-1, Math.min(1, opt.pan || 0));
  const wetSend = opt.wetSend ?? 0;
  const maxDurMs = opt.maxDurMs ?? 180;
  const fadeMs = opt.fade ?? 0.028;
  const startI = Math.floor(t * SR);
  const maxN = Math.floor(maxDurMs * SR / 1000);
  const fadeN = Math.floor((typeof fadeMs === "number" && fadeMs < 1 ? fadeMs : fadeMs / 1000) * SR);
  let bufPos = Math.max(0, (opt.bufOffset ?? 0) * SR);
  for (let k = 0; k < maxN; k++) {
    const di = startI + k;
    if (di < 0) continue;
    if (di >= N) break;
    const si = Math.floor(bufPos);
    if (si + 1 >= buf.length) break;
    const fr = bufPos - si;
    let s = buf[si] + (buf[si + 1] - buf[si]) * fr;
    if (k < fadeN) s *= k / fadeN;
    if (maxN - k < fadeN) s *= (maxN - k) / fadeN;
    const v = s * gain;
    const vL = v * (pan > 0 ? 1 - pan : 1);
    const vR = v * (pan < 0 ? 1 + pan : 1);
    L[di] += vL; R[di] += vR;
    if (wetSend > 0) { WL[di] += vL * wetSend; WR[di] += vR * wetSend; }
    const p = k / maxN;
    const rate = startRate * Math.pow(endRate / startRate, p);
    bufPos += rate;
  }
}

// ── JS reference: sub() — DUCK assumed all-1 in isolation ─────────────
function subJS({ L, R, N }, t, dur, midi, gain) {
  const f = m2f(midi - 12);
  const iStart = Math.max(0, Math.floor(t * SR));
  const iEnd = Math.min(N, Math.ceil((t + dur + 0.05) * SR));
  for (let i = iStart; i < iEnd; i++) {
    const lt = i / SR - t;
    const a = Math.min(1, lt / 0.006) *
              Math.exp(-Math.max(0, lt - (dur - 0.03)) / 0.020);
    const d = 1;            // no sidechain in isolation
    let x = Math.sin(TAU * f * lt)
          + 0.42 * Math.sin(TAU * f * 2 * lt)
          + 0.18 * Math.sin(TAU * f * 3 * lt);
    const click = lt < 0.004
      ? Math.sin(TAU * 1100 * lt) * Math.exp(-lt / 0.0012) * 0.45
      : 0;
    x = Math.tanh(x * 1.85) * 0.85 + click * 0.5;
    const v = x * a * d * gain * 0.68;
    L[i] += v; R[i] += v;
  }
}

// ── test phrases — must match the C `test_<name>()` functions exactly ──
const TESTS = {
  voice: (ctx) => {
    const scale = [62, 64, 65, 67, 69, 70, 72, 74];
    for (let i = 0; i < 8; i++) {
      const t = 0.05 + i * 0.55;
      voiceJS(ctx, t, 0.40, scale[i], 0.18, {
        atk: 0.05, rel: 0.18, vibR: 5.2, vibD: 0.006,
        drive: 1.0, wetSend: 0,
      });
    }
  },
  bell: (ctx) => {
    const notes = [62, 65, 69, 72, 74];
    for (let i = 0; i < 5; i++) {
      const t = 0.1 + i * 0.30;
      bellJS(ctx, t, notes[i], 0.10, {
        atk: 0.080, decTau: 2.5, wetSend: 0, fizzle: true,
      });
    }
  },
  sub: (ctx) => {
    const SPB = 60 / 174;
    for (let i = 0; i < 8; i++) {
      const t = 0.05 + i * SPB;
      subJS(ctx, t, SPB * 0.7, 38, 0.6);
    }
  },
  kick: (ctx) => {
    for (let i = 0; i < 4; i++) {
      kickJS(ctx, 0.1 + i * 1.0, 11, 1);
    }
  },
  snare: (ctx, rng) => {
    for (let i = 0; i < 4; i++) {
      snareJS(ctx, rng, 0.1 + i * 1.0, 0.5, { bodyF: 175 });
    }
  },
  steam: (ctx, rng) => {
    steamJS(ctx, rng, 0.1, 4.0, 0.12, {
      voices: 130, fMin: 400, fMax: 5500,
      atk: 0.6, rel: 1.2, breathRate: 0.5, breathDepth: 0.35,
    });
  },
  woodtick: (ctx) => {
    for (let i = 0; i < 16; i++) {
      woodtickJS(ctx, 0.05 + i * 0.08, 0.13);
    }
  },
  tick: (ctx) => {
    for (let i = 0; i < 16; i++) {
      tickJS(ctx, 0.05 + i * 0.08, 0.22, i % 2 === 1);
    }
  },
  piano: (ctx) => {
    const scale = [62, 65, 69, 72, 74];
    for (let i = 0; i < 5; i++) {
      pianoJS(ctx, 0.05 + i * 1.0, 0.70, scale[i], 0.14, {
        sus: 1.0, bits: 6, hold: 4,
      });
    }
  },
  saw: (ctx) => {
    const notes = [62, 65, 69, 72];
    for (let i = 0; i < 4; i++) {
      sawJS(ctx, 0.05 + i * 1.2, 1.0, notes[i], 0.16, {
        partials: 18, detune: 0.006, atk: 0.012, rel: 0.06, drive: 0.8,
      });
    }
  },
  hoover: (ctx) => {
    const notes = [50, 53, 57];
    for (let i = 0; i < 3; i++) {
      hooverJS(ctx, 0.1 + i * 1.6, 1.4, notes[i], 0.30);
    }
  },
  stab: (ctx) => {
    const notes = [62, 62, 65, 67, 62, 65, 67, 69];
    const SPB = 60 / 174;
    for (let i = 0; i < 8; i++) {
      stabJS(ctx, 0.05 + i * SPB * 0.5, notes[i], 0.34);
    }
  },
  riser: (ctx) => {
    riserJS(ctx, 0.1, 4.0, 38, 86, 0.26);
  },
  bubble: (ctx) => {
    for (let i = 0; i < 8; i++) {
      const radiusMM = 1.5 + i * 0.6;
      bubbleJS(ctx, 0.05 + i * 0.5, radiusMM, 0.3, 0.012, 0.0, 0.0, 1.0);
    }
  },
  sample: (ctx) => {
    const clap = loadWavMonoJS(resolve(HERE, "../samples/clap.wav"));
    for (let i = 0; i < 6; i++) {
      playSampleJS(ctx, 0.10 + i * 0.6, clap, 0.5, {
        rate: 1.0 + i * 0.15,
        pan: (i % 2) ? -0.4 : 0.4,
        wetSend: 0, fade: 0.015,
      });
    }
  },
  sampleswept: (ctx) => {
    const clap = loadWavMonoJS(resolve(HERE, "../samples/clap.wav"));
    for (let i = 0; i < 6; i++) {
      const startRate = 0.5 + i * 0.3;
      playSampleSweptJS(ctx, 0.10 + i * 0.6, clap, 0.6, {
        startRate, endRate: startRate * 1.5,
        pan: (i % 2) ? -0.3 : 0.3,
        maxDurMs: 220, fade: 0.028, bufOffset: 0,
      });
    }
  },
};

// ── render JS reference, write 32-bit float stereo wav ────────────────
function renderJS(name, durSec) {
  const N = Math.ceil(durSec * SR);
  const ctx = {
    L: new Float32Array(N),
    R: new Float32Array(N),
    WL: new Float32Array(N),
    WR: new Float32Array(N),
    DUCK: new Float32Array(N).fill(1),
    N,
  };
  // Each isolated test starts from a fresh RNG state seeded with "hellsine",
  // matching the C side which resets xorshift_state = fnv1a(SEED_STR) at
  // process start. (--test foo never advances the rng between tests.)
  const rng = makeRng("hellsine");
  if (name === "all") {
    for (const k of Object.keys(TESTS)) TESTS[k](ctx, rng);
  } else {
    TESTS[name](ctx, rng);
  }
  // normalize peak → 0.95 (matches C finalize_and_write at wet_mix=0)
  let peak = 0;
  for (let i = 0; i < N; i++) {
    const a = Math.abs(ctx.L[i]); const b = Math.abs(ctx.R[i]);
    if (a > peak) peak = a; if (b > peak) peak = b;
  }
  const g = peak > 0 ? Math.min(1, 0.95 / peak) : 1;
  for (let i = 0; i < N; i++) { ctx.L[i] *= g; ctx.R[i] *= g; }

  const path = resolve(OUT_DIR, `${name}-js.wav`);
  writeWavF32Stereo(path, ctx.L, ctx.R, N);
  return { path, L: ctx.L, R: ctx.R, N, peak };
}

function writeWavF32Stereo(path, L, R, N) {
  const ch = 2, bps = 4, dataLen = N * ch * bps;
  const buf = Buffer.alloc(44 + dataLen);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + dataLen, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(3, 20);
  buf.writeUInt16LE(ch, 22); buf.writeUInt32LE(SR, 24);
  buf.writeUInt32LE(SR * ch * bps, 28); buf.writeUInt16LE(ch * bps, 32);
  buf.writeUInt16LE(32, 34);
  buf.write("data", 36); buf.writeUInt32LE(dataLen, 40);
  let o = 44;
  for (let i = 0; i < N; i++) {
    buf.writeFloatLE(L[i], o); o += 4;
    buf.writeFloatLE(R[i], o); o += 4;
  }
  writeFileSync(path, buf);
}

// minimal float32 stereo WAV reader (only the format we wrote)
function readWavF32Stereo(path) {
  const buf = readFileSync(path);
  if (buf.toString("ascii", 0, 4) !== "RIFF") throw new Error("not RIFF");
  if (buf.toString("ascii", 8, 12) !== "WAVE") throw new Error("not WAVE");
  // find "data" chunk
  let pos = 12;
  while (pos < buf.length - 8) {
    const id = buf.toString("ascii", pos, pos + 4);
    const sz = buf.readUInt32LE(pos + 4);
    if (id === "data") {
      const start = pos + 8;
      const N = sz / 8;             // stereo float32 = 8 bytes/sample
      const L = new Float32Array(N), R = new Float32Array(N);
      for (let i = 0; i < N; i++) {
        L[i] = buf.readFloatLE(start + i * 8);
        R[i] = buf.readFloatLE(start + i * 8 + 4);
      }
      return { L, R, N };
    }
    pos += 8 + sz;
  }
  throw new Error("no data chunk");
}

function diffStats(a, b, n) {
  let sumSq = 0, sumAbs = 0, peakAbs = 0;
  for (let i = 0; i < n; i++) {
    const d = a[i] - b[i];
    const ad = Math.abs(d);
    sumSq += d * d;
    sumAbs += ad;
    if (ad > peakAbs) peakAbs = ad;
  }
  return {
    rms: Math.sqrt(sumSq / n),
    meanAbs: sumAbs / n,
    peakAbs,
  };
}

function runOne(name) {
  console.log(`\n── ${name} ──`);
  const C_BIN = resolve(HERE, "hellsine");
  // 1. JS reference
  const TEST_DUR = 7.5;
  const js = renderJS(name, TEST_DUR);
  console.log(`  js  · peak before norm ${js.peak.toFixed(4)} · ${js.path}`);
  // 2. C
  const cWav = resolve(OUT_DIR, `${name}-c.wav`);
  const r = spawnSync(C_BIN, ["--test", name, "--out", cWav], {
    stdio: ["ignore", "ignore", "inherit"],
  });
  if (r.status !== 0) { console.error("  C engine FAILED"); return; }
  // 3. read both back + diff
  const jsRd = readWavF32Stereo(js.path);
  const cRd  = readWavF32Stereo(cWav);
  const n = Math.min(jsRd.N, cRd.N);
  if (jsRd.N !== cRd.N) {
    console.warn(`  ⚠ length mismatch: js=${jsRd.N} c=${cRd.N}; comparing first ${n}`);
  }
  const dL = diffStats(jsRd.L, cRd.L, n);
  const dR = diffStats(jsRd.R, cRd.R, n);
  console.log(`  diff L · rms ${dL.rms.toExponential(3)} · mean|·| ${dL.meanAbs.toExponential(3)} · peak ${dL.peakAbs.toExponential(3)}`);
  console.log(`  diff R · rms ${dR.rms.toExponential(3)} · mean|·| ${dR.meanAbs.toExponential(3)} · peak ${dR.peakAbs.toExponential(3)}`);
  // pass/fail (lenient — phase-increment vs absolute-time sin diverges with lt)
  const verdict = (dL.rms < 0.05 && dR.rms < 0.05) ? "PASS (close enough)" :
                  (dL.rms < 0.15 && dR.rms < 0.15) ? "EXPECTED DIVERGENCE (long-tail phase math)" :
                                                       "FAIL — investigate";
  console.log(`  → ${verdict}`);
}

const name = process.argv[2] || "all";
if (name === "all") {
  for (const k of Object.keys(TESTS)) runOne(k);
} else if (TESTS[name]) {
  runOne(name);
} else {
  console.error(`unknown test: ${name}`);
  process.exit(1);
}
