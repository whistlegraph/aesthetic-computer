#!/usr/bin/env node
// sleephellsine.mjs — the 15-minute minimal-sine sleep mix.
//
// Forked from pop/hellsine/bin/hellsine.mjs but stripped to its purest
// form: pure-sine pad triads + bell layer + soft theme + counter, the
// same D-minor leitmotif from hellsine slowed to a near-stationary
// crawl, heavy Schroeder reverb, no kicks (or very rare sub heartbeats),
// no samples, no vocals, no brass / saw / hoover / stab / steam.
//
// A single calm wash. No drops, no buildups. Lullaby flow.
//
// Output: a 32-bit float WAV (pre-master, headroom intact). bake.mjs
// runs the gentle loudnorm finalize.
//
// Usage:
//   node pop/sleephellsine/bin/sleephellsine.mjs --out OUT.wav
//        [--bpm 42] [--seed sleephellsine] [--heartbeat off|on]

import { writeFileSync, mkdirSync } from "node:fs";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";

// ── flags ─────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  if (argv[i].startsWith("--")) {
    const k = argv[i].slice(2);
    const v = argv[i + 1] && !argv[i + 1].startsWith("--") ? argv[++i] : "true";
    flags[k] = v;
  }
}
const SR = 48_000;
const BPM = Number(flags.bpm ?? 42);            // 1/4 of hellsine's 182
const SEED_STR = flags.seed || "sleephellsine";
const HEARTBEAT = (flags.heartbeat || "on") !== "off";
const OUT = flags.out || `${process.env.HOME}/Documents/Working Desktop/sleephellsine/.sleephellsine-pre.wav`;

const SPB = 60 / BPM;          // seconds per beat (≈ 1.43s @42 BPM)
const SPBAR = 4 * SPB;         // 4/4 ≈ 5.71s per bar

// ── deterministic rng (humanization only) ─────────────────────────────
function hashString(s) { let h = 2166136261 >>> 0; for (let i = 0; i < s.length; i++) { h ^= s.charCodeAt(i); h = Math.imul(h, 16777619); } return h >>> 0; }
function makeRng(seedStr) { let s = hashString(seedStr) || 1; return () => { s ^= s << 13; s ^= s >>> 17; s ^= s << 5; s >>>= 0; return s / 4294967296; }; }
const rng = makeRng(SEED_STR);
const hum = (amt) => (rng() * 2 - 1) * amt;

const m2f = (m) => 440 * Math.pow(2, (m - 69) / 12);
const TAU = Math.PI * 2;

// ── the principal theme (leitmotif) — same as hellsine ────────────────
// Heroic Dm leitmotif. Semitone offsets from D4 (62). Stays identical
// to hellsine; the tempo halving makes every note ~4× longer so each
// pitch hangs in the air like a slow lullaby.
const ROOT_MEL = 62;
const THEME = [
  [-5, 1], [0, 1.5], [3, 0.5], [7, 1],
  [7, 1], [8, 1], [7, 1], [5, 1],
  [3, 2], [2, 1], [0, 1],
  [0, 2], [-2, 1], [-5, 1],
  [0, 1], [3, 1], [7, 1], [10, 1],
  [12, 2], [10, 1], [8, 1],
  [7, 2], [5, 1], [3, 1],
  [0, 4],
];
// B theme — the calm stepwise companion.
const BTHEME = [
  [-5, 2], [-2, 1], [0, 1],
  [3, 2], [2, 1], [0, 1],
  [-2, 2], [0, 1], [2, 1],
  [0, 4],
  [0, 1], [2, 1], [3, 1], [5, 1],
  [7, 2], [5, 1], [3, 1],
  [2, 2], [0, 1], [-2, 1],
  [0, 4],
];
// the counterpoint — held tones + occasional flourishes.
const COUNTER = [
  [-9, 2], [-5, 2],
  [-5, 2], [-9, 2],
  [-12, 1], [-9, 1], [-4, 2],
  [-9, 1], [-7, 1], [-5, 2],
  [-5, 2], [-2, 2],
  [-9, 1], [-5, 1], [-2, 1], [-5, 1],
  [-7, 1], [-10, 1], [-2, 2],
  [-10, 2], [-7, 1], [-5, 1],
];

// ── chords (low register, root/third/fifth as sines) ──────────────────
const CHORD = {
  Dm: { root: 38, q: [0, 3, 7] }, Bb: { root: 46, q: [0, 4, 7] },
  F:  { root: 41, q: [0, 4, 7] }, C:  { root: 48, q: [0, 4, 7] },
  Gm: { root: 43, q: [0, 3, 7] }, A:  { root: 45, q: [0, 4, 7] },
};
const cyc = (arr, n) => arr[n % arr.length];

// ── arrangement — gentle tonic-dominant-tonic descending washes ───────
// All sections are intentionally similar in intensity. No climax, no
// drop. Just a long, calm reading of the harmony.
// At BPM=42, SPBAR ≈ 5.71s. 24 bars ≈ 137s per section. 7 sections × 24
// bars + tail ≈ 960s (~16 min). Truncated to ~15 min by the bake fade.
const PLAN = [
  { name: "drift-in",  bars: 24, chords: ["Dm","Dm","Dm","Bb","Bb","F","C","Dm"], theme: "soft" },
  { name: "lullaby-a", bars: 24, chords: ["Dm","Bb","F","C","Dm","Bb","F","Dm"], theme: "bsoft" },
  { name: "wash-a",    bars: 24, chords: ["Gm","Gm","Dm","Dm","C","C","Bb","A"], theme: "soft" },
  { name: "restate",   bars: 24, chords: ["Dm","Dm","Bb","Bb","F","F","C","Dm"], theme: "soft" },
  { name: "lullaby-b", bars: 24, chords: ["Gm","Gm","Dm","Dm","Bb","Bb","C","C"], theme: "bsoft" },
  { name: "wash-b",    bars: 24, chords: ["Dm","Bb","Gm","C","F","C","Dm","Dm"], theme: "soft" },
  { name: "dissolve",  bars: 24, chords: ["Dm","Dm","Gm","Gm","C","C","Dm","Dm"], theme: "dissolve" },
];
const TOTAL_BARS = PLAN.reduce((a, s) => a + s.bars, 0);
const TAIL = 14;                          // long fade-out tail (10-15s)
const totalSec = TOTAL_BARS * SPBAR + TAIL;
const N = Math.ceil(totalSec * SR);
const L = new Float32Array(N), Rb = new Float32Array(N);
// wet reverb send bus
const WL = new Float32Array(N), WR = new Float32Array(N);

// ── core sine-only voices ─────────────────────────────────────────────
function write(t, fn, dur) {
  let i = Math.max(0, Math.floor(t * SR));
  const e = Math.min(N, Math.ceil((t + dur) * SR));
  for (; i < e; i++) {
    const lt = i / SR - t;
    const s = fn(lt);
    if (s) { L[i] += s[0]; Rb[i] += s[1]; }
  }
}

// Sub — barely-there heartbeat. Smooth, no click. Pure low sine + a
// touch of 2nd harmonic for body.
function sub(t, dur, midi, gain = 0.16) {
  const f = m2f(midi - 12);
  write(t, (lt) => {
    const atk = Math.min(1, lt / 0.20);                     // 200 ms soft swell
    const rel = Math.max(0, 1 - Math.max(0, lt - (dur - 0.6)) / 0.6);
    const env = atk * rel;
    let x = Math.sin(TAU * f * lt) + 0.22 * Math.sin(TAU * f * 2 * lt);
    x = Math.tanh(x * 1.1);
    const v = x * env * gain;
    return [v, v];
  }, dur);
}

// Pad / theme voice — additive sine partials, slow attacks + long
// releases, gentle vibrato, optional wet-bus send.
function voice(t, dur, midi, gain, opt = {}) {
  const f = m2f(midi);
  const parts = opt.parts || [
    [1, 1.0], [2, 0.42], [3, 0.20], [4, 0.10], [5, 0.05],
  ];
  let amps = 0; for (const p of parts) amps += p[1];
  const norm = 1 / amps;
  const atk = opt.atk ?? 0.55;
  const rel = opt.rel ?? 0.85;
  const vibR = opt.vibR ?? 4.4, vibD = opt.vibD ?? 0.005;
  const pan = opt.pan ?? 0;
  const wetSend = opt.wetSend ?? 0.65;
  write(t, (lt) => {
    let env = Math.min(1, lt / atk);
    if (lt > dur - rel) env *= Math.max(0, (dur - lt) / rel);
    const vib = 1 + Math.sin(TAU * vibR * lt) * vibD * Math.min(1, lt / 0.2);
    let x = 0;
    for (const [r, a] of parts) x += a * Math.sin(TAU * f * r * vib * lt);
    x *= norm;
    const v = x * env * gain;
    const vL = v * (pan > 0 ? 1 - pan : 1);
    const vR = v * (pan < 0 ? 1 + pan : 1);
    return [vL, vR];
  }, dur + rel);
  // wet-bus send — copy a parallel quieter version into WL/WR for the
  // reverb tail.
  if (wetSend > 0) {
    write(t, (lt) => {
      let env = Math.min(1, lt / atk);
      if (lt > dur - rel) env *= Math.max(0, (dur - lt) / rel);
      const vib = 1 + Math.sin(TAU * vibR * lt) * vibD * Math.min(1, lt / 0.2);
      let x = 0;
      for (const [r, a] of parts) x += a * Math.sin(TAU * f * r * vib * lt);
      x *= norm;
      const v = x * env * gain * wetSend;
      const vL = v * (pan > 0 ? 1 - pan : 1);
      const vR = v * (pan < 0 ? 1 + pan : 1);
      // route directly to WL/WR
      const i = Math.floor((t + lt) * SR);
      if (i >= 0 && i < N) { WL[i] += vL; WR[i] += vR; }
      return null;
    }, dur + rel);
  }
}

// Sine bell — additive partials with slight inharmonic stretch. Slow
// attack, very long exponential decay (multi-second).
function bell(t, midi, gain = 0.06, opt = {}) {
  const f = m2f(midi);
  const parts = [
    [1.000, 1.00], [2.012, 0.50], [3.025, 0.28],
    [4.055, 0.14], [5.10,  0.07], [6.20,  0.03],
  ];
  let ampSum = 0; for (const p of parts) ampSum += p[1];
  const norm = 1 / ampSum;
  const pan = opt.pan ?? 0;
  const atk = opt.atk ?? 0.10;
  const decTau = opt.decTau ?? 5.5;
  const wetSend = opt.wetSend ?? 0.90;
  const phs = new Float64Array(parts.length);
  const tailDur = decTau * 5;
  const startI = Math.max(0, Math.floor(t * SR));
  const endI   = Math.min(N, Math.ceil((t + atk + tailDur) * SR));
  for (let i = startI; i < endI; i++) {
    const lt = i / SR - t;
    const env = lt < atk ? lt / atk : Math.exp(-(lt - atk) / decTau);
    let x = 0;
    for (let k = 0; k < parts.length; k++) {
      const [r, a] = parts[k];
      phs[k] += (TAU * f * r) / SR;
      const partEnv = lt < atk ? 1 : Math.exp(-(lt - atk) / (decTau / r));
      x += a * Math.sin(phs[k]) * partEnv;
    }
    x *= norm;
    const v = x * env * gain;
    const vL = v * (pan > 0 ? 1 - pan : 1);
    const vR = v * (pan < 0 ? 1 + pan : 1);
    L[i]  += vL;
    Rb[i] += vR;
    if (wetSend > 0) {
      WL[i] += vL * wetSend;
      WR[i] += vR * wetSend;
    }
  }
}

// ── render the arrangement ────────────────────────────────────────────
let bar = 0;
for (const sec of PLAN) {
  const startSec = bar * SPBAR;

  // pad triad on each bar — soft attacks, long releases, lots of vibrato
  for (let b = 0; b < sec.bars; b++) {
    const tBar = (bar + b) * SPBAR;
    const ch = CHORD[cyc(sec.chords, b)];
    const root = ch.root;
    // phase-based gentle fade-ins on tiered layers so the texture
    // breathes without ever climbing in intensity.
    const tier = Math.floor(b / 8);

    const padG = sec.name === "dissolve" ? 0.075 : 0.090;
    // pad = sine triad with soft mid-weight partials
    for (const semi of ch.q) {
      voice(tBar + hum(0.008), SPBAR * 1.02, root + semi + 12, padG,
        { atk: 0.55, rel: 0.85, vibR: 3.8, vibD: 0.004,
          parts: [[1, 1], [2, 0.40], [3, 0.18], [4, 0.08]] });
    }
    // high sparkle pad — octave up, much quieter; fades in by tier
    if (tier >= 1) {
      voice(tBar + hum(0.010), SPBAR * 1.02, root + ch.q[0] + 24, padG * 0.28,
        { atk: 0.85, rel: 0.95, vibR: 3.2, vibD: 0.005,
          parts: [[1, 1], [2, 0.22], [3, 0.08]] });
    }
    // low body voice — gentle root drone, fades in by tier
    if (tier >= 1) {
      voice(tBar + hum(0.006), SPBAR * 1.02, root, padG * 0.50,
        { atk: 0.45, rel: 0.70, parts: [[1, 1], [2, 0.30], [3, 0.10]] });
    }

    // optional sub heartbeat — one quiet pulse every 8 bars only, very
    // sleepy. Skipped entirely in dissolve so the track relaxes.
    if (HEARTBEAT && sec.name !== "dissolve" && b % 8 === 0) {
      sub(tBar + 0.4, SPBAR * 0.6, root, 0.085);
    }

    // bell ping on the downbeat of every 4th bar — sparse high accents
    // that hang in the reverb hall. Different chord-tone pitches keep
    // the wash gently moving.
    if (b % 4 === 0) {
      const bellMidi = root + ch.q[(b / 4) % ch.q.length] + 36;
      bell(tBar + hum(0.020), bellMidi, sec.name === "dissolve" ? 0.045 : 0.055,
        { pan: ((b / 4) % 2 === 0) ? -0.30 : 0.30,
          decTau: 6.0, atk: 0.030, wetSend: 0.95 });
    }
  }

  // gentle melodic layer — VERY soft theme statements. Each section
  // shows just a fragment, played near-pianissimo so it floats over the
  // pad wash rather than leading it.
  const layTheme = (notes, baseGain, regOff = 0, t0 = startSec) => {
    let beatPos = 0;
    for (const [off, beats] of notes) {
      const tN = t0 + beatPos * SPB;
      const midi = ROOT_MEL + off + regOff;
      const dur = beats * SPB * 0.96;
      voice(tN + hum(0.020), dur, midi, baseGain, {
        atk: 0.45, rel: 0.55, vibR: 4.4, vibD: 0.006,
        parts: [[1, 1], [2, 0.35], [3, 0.16], [4, 0.06]] });
      beatPos += beats;
    }
    return beatPos;
  };
  const layCounter = (notes, baseGain, t0 = startSec) => {
    let beatPos = 0;
    for (const [off, beats] of notes) {
      const tN = t0 + beatPos * SPB;
      voice(tN + hum(0.025), beats * SPB * 0.98, ROOT_MEL + off, baseGain, {
        atk: 0.50, rel: 0.65, vibR: 4.0, vibD: 0.005, pan: -0.30,
        parts: [[1, 1], [2, 0.42], [3, 0.20], [4, 0.08]] });
      beatPos += beats;
    }
  };

  // theme entry — wait for the chord wash to settle before the melody
  // floats in (4 bars in). All gains very quiet.
  const themeT0 = startSec + 4 * SPBAR;
  if (sec.theme === "soft") {
    // first half of THEME only — antecedent, the "calm question"
    layTheme(THEME.slice(0, 11), 0.062, 0, themeT0);
    // soft counter from bar 12 in
    if (sec.bars >= 16) layCounter(COUNTER.slice(0, 4), 0.040, themeT0 + 8 * SPBAR);
  } else if (sec.theme === "bsoft") {
    layTheme(BTHEME, 0.062, 0, themeT0);
    layTheme(BTHEME, 0.038, -12, themeT0);                  // octave down doubling
  } else if (sec.theme === "dissolve") {
    // final section: a single fragment, then a long held tonic drone
    // (D4 + D3) into the tail. No counter — let the bells carry it.
    layTheme(THEME.slice(0, 4), 0.052, 0, themeT0);
    voice(startSec + 12 * SPBAR, 12 * SPBAR + TAIL, ROOT_MEL, 0.045,
      { atk: 2.0, rel: 4.5, parts: [[1, 1], [2, 0.30], [3, 0.12], [4, 0.04]] });
    voice(startSec + 12 * SPBAR, 12 * SPBAR + TAIL, ROOT_MEL - 12, 0.038,
      { atk: 2.0, rel: 4.5, parts: [[1, 1], [2, 0.25]] });
  }

  bar += sec.bars;
}

// ── Schroeder reverb on the wet bus → mix back into L/Rb ──────────────
// 4 parallel feedback combs + 2 series allpasses per channel. Longer
// delays + higher feedback than hellsine → ~6 s tail (cathedral, not
// hall). Wet send floats throughout the whole track.
const COMB_L = [0.0437, 0.0541, 0.0641, 0.0727];
const COMB_R = [0.0451, 0.0557, 0.0663, 0.0741];
const COMB_FB = 0.91;
const AP_D = [0.0089, 0.0027];
const AP_FB = 0.5;
const processCh = (buf, combDelays) => {
  const combs = combDelays.map((d) => ({ line: new Float32Array(Math.floor(d * SR)), idx: 0 }));
  const aps   = AP_D.map((d) => ({ line: new Float32Array(Math.floor(d * SR)), idx: 0 }));
  const out = new Float32Array(buf.length);
  for (let i = 0; i < buf.length; i++) {
    let combOut = 0;
    for (const c of combs) {
      const delayed = c.line[c.idx];
      c.line[c.idx] = buf[i] + delayed * COMB_FB;
      c.idx = (c.idx + 1) % c.line.length;
      combOut += delayed;
    }
    combOut *= 0.25;
    let apOut = combOut;
    for (const a of aps) {
      const delayed = a.line[a.idx];
      const newS = apOut + delayed * AP_FB;
      a.line[a.idx] = newS;
      apOut = delayed - newS * AP_FB;
      a.idx = (a.idx + 1) % a.line.length;
    }
    out[i] = apOut;
  }
  return out;
};
const wetL = processCh(WL, COMB_L);
const wetR = processCh(WR, COMB_R);
const WET_MIX = 0.58;            // HEAVY wet — sleep mix wants the hall
// Smooth wet fade-in over the first 8 s so the reverb blooms in.
const revFadeEnd = 8.0;
for (let i = 0; i < N; i++) {
  const t = i / SR;
  const wet = t < revFadeEnd ? WET_MIX * (t / revFadeEnd) : WET_MIX;
  L[i]  += wetL[i] * wet;
  Rb[i] += wetR[i] * wet;
}
console.log(`→ reverb · Schroeder 4-comb + 2-allpass · wet ${WET_MIX} · long tail`);

// ── master sum: very gentle bus glue, generous headroom ────────────────
// Quieter than hellsine — leaves dynamic range for the lullaby breath.
let peak = 0;
for (let i = 0; i < N; i++) {
  const a = Math.abs(L[i]); const b = Math.abs(Rb[i]);
  if (a > peak) peak = a; if (b > peak) peak = b;
}
const g = peak > 0 ? Math.min(1, 0.55 / peak) : 1;          // scale to 0.55 peak (was 0.89)
for (let i = 0; i < N; i++) {
  L[i] = L[i] * g;                                          // no tanh — preserve sine purity
  Rb[i] = Rb[i] * g;
}

// ── write 32-bit float WAV ────────────────────────────────────────────
mkdirSync(dirname(OUT), { recursive: true });
const ch = 2, bps = 4, dataLen = N * ch * bps;
const buf = Buffer.alloc(44 + dataLen);
buf.write("RIFF", 0); buf.writeUInt32LE(36 + dataLen, 4); buf.write("WAVE", 8);
buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(3, 20); // IEEE float
buf.writeUInt16LE(ch, 22); buf.writeUInt32LE(SR, 24);
buf.writeUInt32LE(SR * ch * bps, 28); buf.writeUInt16LE(ch * bps, 32);
buf.writeUInt16LE(32, 34);
buf.write("data", 36); buf.writeUInt32LE(dataLen, 40);
let o = 44;
for (let i = 0; i < N; i++) { buf.writeFloatLE(L[i], o); o += 4; buf.writeFloatLE(Rb[i], o); o += 4; }
writeFileSync(OUT, buf);

console.log(`sleephellsine · ${BPM} BPM · ${TOTAL_BARS} bars · ${totalSec.toFixed(1)}s (${(totalSec/60).toFixed(2)} min)`);
console.log(`→ wav · ${OUT}`);
