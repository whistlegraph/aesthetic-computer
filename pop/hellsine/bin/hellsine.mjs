#!/usr/bin/env node
// hellsine.mjs — the all-sine hardcore engine.
//
// THE LAW (amended 2026-05-22 by the queen): the synthesis engine is
// all-sine — every generated sample is a sum of Math.sin() terms, or a
// memoryless waveshaping (tanh / hard clip) of such a sum. No noise, no
// saw, no square. The gabber kick is a pitch-enveloped sine clipped past
// its skin — that distorted sine IS hardcore. The "hell" is the drive.
//
// THE ONE EXCEPTION: @jeffrey's recorded rattle — a single organic
// sampled layer (pop/hellsine/samples/rattle.wav, captured via
// `node pop/bin/rfa.mjs --sample --track hellsine --name rattle`) that
// rides on top of the sine engine. Absent the file, the render is pure
// all-sine, unchanged. Density: --rattle off|sparse|drive.
//
// It carries a hand-composed John Williams structure (a heroic D-minor
// leitmotif: stated → developed → transposed → restated) and is built
// to run as epic study music: continuous, no dead air, loop-friendly
// tail under a film arc.
//
// Output: a 32-bit float WAV (pre-master, headroom intact) + a
// scratch-mix-compatible struct.json (bpm + sections + kick/snare
// onset grid). bake.mjs runs the post-FX + Spotify finalize.
//
// Usage:
//   node pop/hellsine/bin/hellsine.mjs --out OUT.wav [--struct S.json]
//        [--bpm 182] [--hell 11] [--seed hellsine]
//        [--rattle off|sparse|drive] [--rattle-gain 0.5]
import { writeFileSync, mkdirSync, readFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
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
const BPM = Number(flags.bpm ?? 182);
const HELL = Number(flags.hell ?? 11);          // base gabber drive ("hell knob")
const NOKICK = !!flags.nokick;                  // --nokick: drop the kick + its ducking
const SEED_STR = flags.seed || "hellsine";
const OUT = flags.out || `${process.env.HOME}/Documents/Working Desktop/hellsine/.hellsine-pre.wav`;
const STRUCT = flags.struct || `${OUT.replace(/\.wav$/, "")}.assets/struct.json`;
const HERE = dirname(fileURLToPath(import.meta.url));

const SPB = 60 / BPM;          // seconds per beat
const SPBAR = 4 * SPB;         // 4/4

// ── deterministic rng (humanization only — the music is composed) ─────
function hashString(s) { let h = 2166136261 >>> 0; for (let i = 0; i < s.length; i++) { h ^= s.charCodeAt(i); h = Math.imul(h, 16777619); } return h >>> 0; }
function makeRng(seedStr) { let s = hashString(seedStr) || 1; return () => { s ^= s << 13; s ^= s >>> 17; s ^= s << 5; s >>>= 0; return s / 4294967296; }; }
const rng = makeRng(SEED_STR);
const hum = (amt) => (rng() * 2 - 1) * amt;     // ± humanization

const m2f = (m) => 440 * Math.pow(2, (m - 69) / 12);
const TAU = Math.PI * 2;

// D natural minor reference: D E F G A Bb C → 0 2 3 5 7 8 10. Theme +
// chord pitches below are written as explicit semitone offsets.

// ── the principal theme (leitmotif) ───────────────────────────────────
// Hand-written, singable, heroic. Semitone offsets from melodic root
// D4. Antecedent = a rising Dm-arpeggio "call" + an upper-neighbour
// swell + a lyric descent that ends OPEN on the 5th (a question).
// Consequent = climbs the full arpeggio to a soaring high D (the apex)
// and resolves home (a strong cadence). Durations in beats.
const ROOT_MEL = 62;   // D4
const THEME = [
  // antecedent (bars 1–4)
  [-5, 1], [0, 1.5], [3, 0.5], [7, 1],            // A3 → D4 F4 A4   (the call)
  [7, 1], [8, 1], [7, 1], [5, 1],                 // A4 Bb4 A4 G4    (swell)
  [3, 2], [2, 1], [0, 1],                         // F4 . E4 D4      (descent)
  [0, 2], [-2, 1], [-5, 1],                       // D4 . C4 A3      (open — question)
  // consequent (bars 5–8)
  [0, 1], [3, 1], [7, 1], [10, 1],                // D4 F4 A4 C5     (climb)
  [12, 2], [10, 1], [8, 1],                       // D5 . C5 Bb4     (the apex)
  [7, 2], [5, 1], [3, 1],                         // A4 . G4 F4      (descent)
  [0, 4],                                          // D4              (resolve home)
];
// B theme — lyrical, narrow, stepwise: the study calm. 8 bars.
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

// ── the countermelody ─────────────────────────────────────────────────
// An independent 8-bar horn line set AGAINST the principal THEME (not a
// unison double). Tenor register, semitone offsets from D4. The rule of
// the line: hold long tones where the theme is busy (bars 1, 2, 5), and
// move — arpeggiated flourishes — where the theme sustains (bars 3, 6,
// 7, 8). Chord-tone consonant; contrary motion into the cadences.
const COUNTER = [
  [-9, 2], [-5, 2],                       // 1 Dm — F3 .  A3 .   held under the call
  [-5, 2], [-9, 2],                       // 2 Dm — A3 .  F3 .   descent under the swell
  [-12, 1], [-9, 1], [-4, 2],             // 3 Bb — D3 F3 Bb3 .  moves under held F4
  [-9, 1], [-7, 1], [-5, 2],              // 4 Bb — F3 G3 A3 .   rises under held D4
  [-5, 2], [-2, 2],                       // 5 F  — A3 .  C4 .   long tones under the climb
  [-9, 1], [-5, 1], [-2, 1], [-5, 1],     // 6 F  — F3 A3 C4 A3  flourish under the apex
  [-7, 1], [-10, 1], [-2, 2],             // 7 C  — G3 E3 C4 .   under the held A4
  [-10, 2], [-7, 1], [-5, 1],             // 8 C  — E3 .  G3 A3  carries the cadence home
];

// ── radical melodic-variation strategies ──────────────────────────────
// --strategy <name> applies a transform to the principal leitmotif. The
// B-theme, chords + form stay fixed — a constant frame to A/B the
// variation against. Every transform PRESERVES the theme's 32-beat /
// 8-bar length, so it drops straight into the section loop. "none" = the
// composed track. 16 strategies, deliberately extreme:
//   inversion        melodic mirror — every interval flipped
//   retrograde       the theme played backwards
//   retro-inversion  backwards AND upside-down
//   interval-x2      every interval doubled — contour blown wide open
//   interval-half    every interval halved — contour collapsed to a chant
//   octave-scatter   each note hurled into a random octave
//   whole-tone       re-quantized to the whole-tone scale (no tonic)
//   chromatic-climb  pitches replaced by a relentless chromatic spiral
//   phrygian         recolored to D-phrygian (the dark "metal" mode)
//   mirror-canon     theme + its live inversion, simultaneously
//   stretto          the theme chases itself an octave down, one bar late
//   crab-canon       theme forward + theme backward, at once
//   scramble         the pitch sequence shuffled (rhythm intact)
//   slow-motion      note-pairs fused — the theme in augmentation
//   double-time      every note split in two — frantic diminution
//   atonal           pitches fully randomized — melody to pitched chaos
const STRATEGY = flags.strategy || "none";
const sRng = makeRng("hellsine-strat:" + STRATEGY);
const sShuffle = (a) => {
  const r = a.slice();
  for (let i = r.length - 1; i > 0; i--) {
    const j = Math.floor(sRng() * (i + 1));
    [r[i], r[j]] = [r[j], r[i]];
  }
  return r;
};
// scale-aware helpers for the busier-melody set (D natural minor)
const DMIN = [0, 2, 3, 5, 7, 8, 10];
const inDmin = (o) => DMIN.includes(((o % 12) + 12) % 12);
const scaleStep = (o, dir) => { let x = o + dir; for (let i = 0; i < 12 && !inDmin(x); i++) x += dir; return x; };
// busier-melody transforms — each DENSIFIES the leitmotif (more, shorter
// notes) while preserving its 32-beat / 8-bar total length:
//   ornament    a passing tone threaded after each note
//   arpeggiate  each note bursts into a 4-note triad figure
//   sixteenths  every note subdivided into a 16th-note run
//   turns       a classical 4-note turn on each note
//   run-fill    long notes filled with rising scalar runs
//   tremolo     strict rapid alternation with the upper neighbour
//   perpetual   nonstop 8th-note motion off theme + chord tones
//   mordent     a fast grace-note flick at each note's onset
function sOrnament(t) {
  const r = [];
  t.forEach(([o, d], i) => {
    const nxt = t[i + 1] ? t[i + 1][0] : o;
    if (d >= 1) { r.push([o, d * 0.75]); r.push([scaleStep(o, nxt >= o ? 1 : -1), d * 0.25]); }
    else r.push([o, d]);
  });
  return r;
}
function sArpeggiate(t) {
  const r = [];
  for (const [o, d] of t) {
    const b = scaleStep(scaleStep(o, 1), 1), c = scaleStep(scaleStep(b, 1), 1);
    const seq = [o, b, c, b], dd = d / 4;
    for (let k = 0; k < 4; k++) r.push([seq[k], dd]);
  }
  return r;
}
function sSixteenths(t) {
  const r = [];
  for (const [o, d] of t) {
    const n = Math.max(1, Math.round(d * 4)), dd = d / n;
    for (let k = 0; k < n; k++) r.push([k % 2 ? scaleStep(o, 1) : o, dd]);
  }
  return r;
}
function sTurns(t) {
  const r = [];
  for (const [o, d] of t) {
    const seq = [scaleStep(o, 1), o, scaleStep(o, -1), o], dd = d / 4;
    for (let k = 0; k < 4; k++) r.push([seq[k], dd]);
  }
  return r;
}
function sRunFill(t) {
  const r = [];
  for (const [o, d] of t) {
    if (d >= 2) {
      const n = Math.round(d / 0.5), dd = d / n;
      let p = o;
      for (let k = 0; k < n; k++) { r.push([p, dd]); p = scaleStep(p, 1); }
    } else r.push([o, d]);
  }
  return r;
}
function sTremolo(t) {
  const r = [];
  for (const [o, d] of t) {
    const n = Math.max(2, Math.round(d * 4)), dd = d / n, hi = scaleStep(o, 1);
    for (let k = 0; k < n; k++) r.push([k % 2 ? hi : o, dd]);
  }
  return r;
}
function sPerpetual(t) {
  const r = [];
  for (const [o, d] of t) {
    const n = Math.max(1, Math.round(d * 2)), dd = d / n;
    const b = scaleStep(scaleStep(o, 1), 1), c = scaleStep(scaleStep(b, 1), 1);
    const seq = [o, b, c];
    for (let k = 0; k < n; k++) r.push([seq[k % 3], dd]);
  }
  return r;
}
function sMordent(t) {
  const r = [];
  for (const [o, d] of t) {
    if (d >= 0.5) { r.push([o, 0.08]); r.push([scaleStep(o, 1), 0.08]); r.push([o, d - 0.16]); }
    else r.push([o, d]);
  }
  return r;
}
const STRATEGIES = {
  none:              (t) => t,
  inversion:         (t) => t.map(([o, d]) => [-o, d]),
  retrograde:        (t) => [...t].reverse(),
  "retro-inversion": (t) => [...t].reverse().map(([o, d]) => [-o, d]),
  "interval-x2":     (t) => t.map(([o, d]) => [o * 2, d]),
  "interval-half":   (t) => t.map(([o, d]) => [Math.round(o / 2), d]),
  "octave-scatter":  (t) => t.map(([o, d]) => [o + (Math.floor(sRng() * 4) - 1) * 12, d]),
  "whole-tone":      (t) => t.map(([o, d]) => [2 * Math.round(o / 2), d]),
  "chromatic-climb": (t) => t.map(([, d], i) => [((i * 2) % 26) - 7, d]),
  phrygian:          (t) => t.map(([o, d]) => [(((o % 12) + 12) % 12) === 2 ? o - 1 : o, d]),
  "mirror-canon":    (t) => t,           // COUNTER_V becomes the live inversion
  stretto:           (t) => t,           // a 2nd entry chases it (brass branch)
  "crab-canon":      (t) => t,           // COUNTER_V becomes the retrograde
  scramble:          (t) => { const o = sShuffle(t.map((x) => x[0])); return t.map(([, d], i) => [o[i], d]); },
  "slow-motion":     (t) => { const r = []; for (let i = 0; i < t.length; i += 2) r.push(i + 1 < t.length ? [t[i][0], t[i][1] + t[i + 1][1]] : t[i]); return r; },
  "double-time":     (t) => { const r = []; t.forEach(([o, d], i) => { r.push([o, d / 2]); r.push([o + (i % 2 ? 2 : -2), d / 2]); }); return r; },
  atonal:            (t) => t.map(([, d]) => [Math.floor(sRng() * 22) - 7, d]),
  ornament: sOrnament, arpeggiate: sArpeggiate, sixteenths: sSixteenths,
  turns: sTurns, "run-fill": sRunFill, tremolo: sTremolo,
  perpetual: sPerpetual, mordent: sMordent,
};
const applyStrat = STRATEGIES[STRATEGY] || STRATEGIES.none;
const THEME_V = applyStrat(THEME);
let COUNTER_V = COUNTER;
if (STRATEGY === "mirror-canon") COUNTER_V = THEME.map(([o, d]) => [-o, d]);
else if (STRATEGY === "crab-canon") COUNTER_V = [...THEME].reverse();

// --strategy ultimate — the composed-through showcase: a different
// technique per section, and per restatement inside the brass sections,
// so the leitmotif accumulates ornamentation as the arc escalates, then
// returns home plain. The horn counterpoint runs under it throughout;
// the climax swaps the counter for an octave-down stretto canon.
const ULTIMATE = STRATEGY === "ultimate";
const ULTIMATE_MAP = {
  overture:  ["none"],                                   // bare hint
  statement: ["none", "ornament", "turns"],              // stated, then ornamented
  develop:   ["sixteenths"],                             // busy fragmentation
  climax:    ["arpeggiate", "double-time", "sixteenths"], // maximal density
  coda:      ["none"],                                   // resolve home, plain
};
const ultThemeAt = (name, lp = 0) => {
  const seq = ULTIMATE_MAP[name] || ["none"];
  return (STRATEGIES[seq[Math.min(lp, seq.length - 1)]] || STRATEGIES.none)(THEME);
};

// ── chords (low register, root/third/fifth as sines) ──────────────────
const CHORD = {
  Dm: { root: 38, q: [0, 3, 7] }, Bb: { root: 46, q: [0, 4, 7] },
  F:  { root: 41, q: [0, 4, 7] }, C:  { root: 48, q: [0, 4, 7] },
  Gm: { root: 43, q: [0, 3, 7] }, A:  { root: 45, q: [0, 4, 7] },
};
const cyc = (arr, n) => arr[n % arr.length];

// ── arrangement: ordered sections, each N bars ────────────────────────
// Longer sections + halftime beat = each kick lands like a HOLE, not a
// punch. Spread out + heavy ducking = the negative space carries the
// hardcore weight. "Study music under a film arc" still — continuous,
// no dead air, more sine voices layered for body.
const PLAN = [
  { name: "overture",  bars: 12, kick: "none",     drive: 0,         chords: ["Dm","Dm","Bb","Bb","F","F","C","A","Dm","Bb","F","C"], theme: "soft" },
  { name: "statement", bars: 24, kick: "halftime", drive: HELL,      chords: ["Dm","Dm","Bb","Bb","F","F","C","C"], theme: "brass" },
  { name: "bridge",    bars: 24, kick: "pulse",    drive: HELL*0.7,  chords: ["Gm","Gm","Dm","Dm","C","C","Bb","A"], theme: "bsoft" },
  { name: "develop",   bars: 24, kick: "halftime", drive: HELL*1.15, chords: ["Dm","Dm","F","F","Gm","Gm","A","A","Bb","Bb","C","C","Dm","A","Dm","A"], theme: "frag" },
  { name: "climax",    bars: 24, kick: "halfhard", drive: HELL*1.3, transpose: 2, chords: ["Dm","Dm","Bb","Bb","F","F","C","C"], theme: "brass" },
  { name: "coda",      bars: 16, kick: "fade",     drive: HELL*0.6,  chords: ["Dm","Dm","Gm","Gm","C","C","Dm","Dm"], theme: "dissolve" },
];
const TOTAL_BARS = PLAN.reduce((a, s) => a + s.bars, 0);
const TAIL = 3.2;
const totalSec = TOTAL_BARS * SPBAR + TAIL;
const N = Math.ceil(totalSec * SR);
const L = new Float32Array(N), Rb = new Float32Array(N);
// sidechain duck (1 = open) — kicks pull it down; sub/pad/strings ride it.
const DUCK = new Float32Array(N).fill(1);

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

// ── WAV loader — the ONE sampled exception. Reads PCM16/24/32 or float
// WAV → mono Float32 @ SR, silence-trimmed, normalized to peak 1.
function loadWavMono(path) {
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
  if (fmt.sr !== SR) {                                  // linear resample → SR
    const outN = Math.round(frames * SR / fmt.sr), rs = new Float32Array(outN);
    for (let i = 0; i < outN; i++) {
      const x = i * fmt.sr / SR, i0 = Math.floor(x), fr = x - i0;
      rs[i] = (mono[i0] || 0) + ((mono[i0 + 1] || 0) - (mono[i0] || 0)) * fr;
    }
    mono = rs;
  }
  let a = 0, b = mono.length; const TH = 0.02;           // trim near-silence
  while (a < b && Math.abs(mono[a]) < TH) a++;
  while (b > a && Math.abs(mono[b - 1]) < TH) b--;
  mono = mono.subarray(a, b);
  let pk = 0;                                            // normalize → peak 1
  for (let i = 0; i < mono.length; i++) pk = Math.max(pk, Math.abs(mono[i]));
  if (pk > 0) for (let i = 0; i < mono.length; i++) mono[i] /= pk;
  return mono;
}

// Place a sample buffer at time t — linear-interp resample for pitch,
// simple pan, short declick fades. Mixes into the L/Rb bus like a voice.
function playSample(t, buf, gain = 1, opt = {}) {
  const rate = opt.rate || 1;
  const pan = Math.max(-1, Math.min(1, opt.pan || 0));
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
    if (k < fadeN) s *= k / fadeN;                       // declick in
    if (outLen - k < fadeN) s *= (outLen - k) / fadeN;   // declick out
    const v = s * gain;
    L[di]  += v * (pan > 0 ? 1 - pan : 1);
    Rb[di] += v * (pan < 0 ? 1 + pan : 1);
  }
}

// The HOLE kick — no punch, no click, no transient. A slow-blooming
// sub-only sine pitch-drop that lives mostly through what it removes
// from the surrounding mix. Each hit is a vacuum carved by the duck
// (15 ms slam-shut → 450 ms slow re-open); the kick itself is just the
// bass pressure filling the hole. Still pure sine, still saturated for
// hardcore body, just stripped of its bright attack.
function kick(t, drive = HELL, gain = 1) {
  if (NOKICK) return;                        // --nokick: melodies fully exposed
  const dur = 0.55;                          // longer body (was 0.26)
  const pStart = 180, pEnd = 28;             // start lower, end deep-sub (was 240→47)
  const pT = 0.080;                          // slow pitch sweep (was 0.034)
  let ph = 0;
  // The HOLE: instant slam-shut (15 ms), then slow re-open over 450 ms.
  // Ducks the sub/pad/strings/theme to 8% — they all vacuum out around
  // each kick, which is what makes the kick FEEL like a hole.
  const di = Math.floor(t * SR);
  const dN = Math.floor(0.46 * SR);
  const closeN = Math.floor(0.015 * SR);     // 15 ms slam
  const openN  = dN - closeN;                // 445 ms re-open
  for (let k = 0; k < dN && di + k < N; k++) {
    let env;
    if (k < closeN) {
      env = 1 - (k / closeN) * 0.92;         // 1.0 → 0.08 over 15 ms
    } else {
      const p = (k - closeN) / openN;
      env = 0.08 + 0.92 * (1 - Math.exp(-p * 3.2));   // smooth re-open
    }
    if (env < DUCK[di + k]) DUCK[di + k] = env;
  }
  write(t, (lt) => {
    const f = pEnd + (pStart - pEnd) * Math.exp(-lt / pT);
    ph += (TAU * f) / SR;
    // Slow bloom (12 ms attack — was 0.8 ms!) + long body
    const atk   = 1 - Math.exp(-lt / 0.012);
    const decay = Math.exp(-lt / 0.22);       // longer tail
    const amp   = atk * decay;
    let x = Math.sin(ph);
    x = Math.tanh(x * (drive * 0.45));         // gentler saturation (was full)
    const v = x * amp * 0.95 * gain;
    return [v, v];
  }, dur);
}

// ── snare — all-sine: tonal shell + additive-sine "noise" crack ───────
// THE LAW HOLDS. Body = a pitch-blipped detuned sine pair (~188 Hz)
// with a fast decay — the drum's wooden shell. Crack = the steam trick:
// a dense bank of log-spaced detuned sines across 1.5–8 kHz summed to
// broadband noise, near-instant attack + ~60 ms decay — the bright
// snap. tanh fuses body + crack into one hardcore backbeat hit. The
// HOLE kick deliberately drops its transient; the snare carries it.
function snare(t, gain = 0.5, opt = {}) {
  const dur = 0.30, bodyF = opt.bodyF || 150;
  const nV = 64, fMin = 900, fMax = 4200;
  const freqs = new Float64Array(nV), phs = new Float64Array(nV);
  for (let i = 0; i < nV; i++) {
    const u = (i + rng()) / nV;
    freqs[i] = fMin * Math.pow(fMax / fMin, u);
    phs[i] = rng() * TAU;
  }
  const norm = 1 / Math.sqrt(nV);
  write(t, (lt) => {
    // body — detuned sine pair with a short downward pitch blip
    const pf = bodyF * (1 + 0.6 * Math.exp(-lt / 0.012));
    const body = (Math.sin(TAU * pf * lt) + 0.7 * Math.sin(TAU * pf * 1.48 * lt))
               * Math.exp(-lt / 0.068);
    // crack — additive-sine noise, sharp attack, fast decay
    let noise = 0;
    for (let i = 0; i < nV; i++) noise += Math.sin(TAU * freqs[i] * lt + phs[i]);
    const crackEnv = (1 - Math.exp(-lt / 0.0008)) * Math.exp(-lt / 0.040);
    let x = body * 0.9 + noise * norm * crackEnv * 0.5;
    x = Math.tanh(x * 1.5);                        // hardcore glue — body-forward, deep
    const v = x * gain * Math.min(1, lt / 0.0006);
    return [v * 0.97, v];                          // faint stereo widen
  }, dur);
}

// ── steam release — additive-sine "white noise" breath ───────────────
// THE LAW HOLDS: a dense bank of detuned sines summed IS broadband noise
// (Fourier identity). Frequencies are log-spaced + jittered across the
// air band so the result reads as steam/breath, not as pitched chord.
// Stays inside the all-sine constraint and gives the track the hiss the
// pure-sine mix otherwise lacks.
function steam(t, dur, gain = 0.12, opt = {}) {
  const nVoices = opt.voices || 130;       // dense enough to fuse → smooth wash, not a buzzy comb
  const fMin = opt.fMin || 400;          // more body, less hiss
  const fMax = opt.fMax || 5500;         // pulled out of the buzz band (was 9500)
  const atk  = opt.atk  ?? 0.6;              // slow steam release
  const rel  = opt.rel  ?? 1.2;
  const breathRate = opt.breathRate || 0.5;  // gentle steam pulsing
  const breathDepth = opt.breathDepth ?? 0.35;
  const freqs = new Float64Array(nVoices);
  const phases = new Float64Array(nVoices);
  for (let i = 0; i < nVoices; i++) {
    const u = (i + rng()) / nVoices;
    freqs[i] = fMin * Math.pow(fMax / fMin, u);
    phases[i] = rng() * TAU;
  }
  const norm = 1 / Math.sqrt(nVoices);
  write(t, (lt) => {
    let env = Math.min(1, lt / atk);
    if (lt > dur - rel) env *= Math.max(0, (dur - lt) / rel);
    const breath = (1 - breathDepth) + breathDepth * Math.sin(TAU * breathRate * lt);
    let x = 0;
    for (let i = 0; i < nVoices; i++) x += Math.sin(TAU * freqs[i] * lt + phases[i]);
    x = Math.tanh(x * norm * 1.1);
    const v = x * env * breath * gain;
    return [v * 0.92, v * 1.00];             // slight pan offset → stereo hiss
  }, dur + rel);
}

// Sub — low sine at the chord root, rides the duck (the pump).
function sub(t, dur, midi, gain = 0.5) {
  const f = m2f(midi - 12);
  write(t, (lt) => {
    const a = Math.min(1, lt / 0.008) * Math.exp(-Math.max(0, lt - (dur - 0.06)) / 0.04);
    const d = DUCK[Math.min(N - 1, Math.floor((t + lt) * SR))];
    // fundamental + a touch of 2nd harmonic, then driven hard for a
    // deep, FELT pressure-bass — audible on small speakers, not just felt
    let x = Math.sin(TAU * f * lt) + 0.3 * Math.sin(TAU * f * 2 * lt);
    x = Math.tanh(x * 2.05) * 0.88;               // deep pressure drive
    const v = x * a * d * gain;
    return [v, v];
  }, dur);
}

// Additive-sine brass / strings — carries the leitmotif. Odd-harmonic
// emphasis = brass bite; slow attack + many partials = strings.
function voice(t, dur, midi, gain, opt = {}) {
  const f = m2f(midi);
  const parts = opt.parts || [
    [1, 1.0], [2, 0.5], [3, 0.34], [4, 0.16], [5, 0.12], [6, 0.06],
  ];
  let amps = 0; for (const p of parts) amps += p[1];
  const norm = (opt.gain ?? 1) / amps;
  const atk = opt.atk ?? 0.05, rel = opt.rel ?? 0.18;
  const vibR = opt.vibR ?? 5.2, vibD = opt.vibD ?? 0.006;
  const pan = opt.pan ?? 0, drive = opt.drive ?? 1.0;
  write(t, (lt) => {
    let env = Math.min(1, lt / atk);
    if (lt > dur - rel) env *= Math.max(0, (dur - lt) / rel);
    const vib = 1 + Math.sin(TAU * vibR * lt) * vibD * Math.min(1, lt / 0.12);
    let x = 0;
    for (const [r, a] of parts) x += a * Math.sin(TAU * f * r * vib * lt);
    x = Math.tanh(x * norm * drive);              // soft sine glue
    const v = x * env * gain;
    return [v * (1 - Math.max(0, pan)), v * (1 + Math.min(0, pan))];
  }, dur + rel);
}

// Hoover — detuned sine stack + rising sine-on-sine FM index. All sine,
// reads aggressive after the saturation.
function hoover(t, dur, midi, gain = 0.3) {
  const f = m2f(midi), det = [0.994, 1.0, 1.007, 1.013];
  write(t, (lt) => {
    const env = Math.min(1, lt / 0.02) * Math.max(0, 1 - lt / dur);
    const idx = 1.4 + 3.0 * Math.min(1, lt / (dur * 0.6));
    let x = 0;
    for (const dd of det) {
      const mod = Math.sin(TAU * f * dd * 0.5 * lt) * idx;
      x += Math.sin(TAU * f * dd * lt + mod);
    }
    x = Math.tanh(x * 0.5 * (1 + idx * 0.2)) * env * gain;
    return [x * 0.85, x];
  }, dur);
}

// Hardcore stab — 2-op sine FM (sine modulates sine), high index, clipped.
function stab(t, midi, gain = 0.34) {
  const f = m2f(midi), dur = 0.16;
  write(t, (lt) => {
    const env = Math.exp(-lt / 0.06) * (1 - Math.exp(-lt / 0.001));
    const mod = Math.sin(TAU * f * 1.997 * lt) * (5.5 * Math.exp(-lt / 0.05));
    let x = Math.sin(TAU * f * lt + mod);
    x = Math.max(-0.9, Math.min(0.9, x * 1.5));
    const v = x * env * gain;
    return [v, v * 0.92];
  }, dur);
}

// Perc — the "hat" re-derived as a very high, sub-ms sine blip. Metallic
// open-hat = a ring-modded sine pair (product of two sines → sines).
function tick(t, gain = 0.22, open = false) {
  const f = open ? 7400 : 9200, dur = open ? 0.06 : 0.022;
  write(t, (lt) => {
    const env = Math.exp(-lt / (open ? 0.026 : 0.004));
    let x = Math.sin(TAU * f * lt) + 0.45 * Math.sin(TAU * f * 1.51 * lt);
    if (open) x *= Math.sin(TAU * 5300 * lt);     // ring-mod metal
    const v = x * env * gain;
    return [v * 0.9, v];
  }, dur);
}

// Scream / riser — a sine swept up over the span with a rising FM index
// and the same hard saturation. The build.
function riser(t, dur, m0, m1, gain = 0.26) {
  const f0 = m2f(m0), f1 = m2f(m1);
  let ph = 0;
  write(t, (lt) => {
    const p = lt / dur, f = f0 * Math.pow(f1 / f0, p);
    ph += (TAU * f) / SR;
    const idx = 1 + 5 * p;
    let x = Math.sin(ph + Math.sin(ph * 0.5) * idx);
    x = Math.tanh(x * 2.2);
    const env = Math.min(1, lt / 0.05) * (0.3 + 0.7 * p);
    const v = x * env * gain;
    return [v, v * 0.95];
  }, dur);
}

// ── render the arrangement ────────────────────────────────────────────
const kickEvents = [], snareEvents = [], sectionRanges = [];
let bar = 0;
for (const sec of PLAN) {
  const tr = sec.transpose || 0;
  const startSec = bar * SPBAR;
  sectionRanges.push({ name: sec.name, startBar: bar, endBar: bar + sec.bars,
    startSec, endSec: (bar + sec.bars) * SPBAR });

  // chords / sub / pad — more sine voices for body. Each chord tone gets
  // an 8-partial pad voice (was 5); we add a high octave-up sparkle pad
  // (5 partials) for shimmer, and a low octave-down body voice on root
  // only. Total pad sine count per chord ≈ 4 voices × 8 partials = 32
  // sines, plus 5 sparkle partials + 5 body partials = ~42 sines/chord.
  for (let b = 0; b < sec.bars; b++) {
    const tBar = (bar + b) * SPBAR;
    const ch = CHORD[cyc(sec.chords, b)];
    const root = ch.root + tr;
    const padGain = sec.name === "overture" || sec.name === "coda" ? 0.085 : 0.072;
    const tier = Math.floor(b / 8);              // 8-bar phase — layers fade in by tier
    // pad = sine triad with mid-weight partials (no upper-mid sizzle —
    // dropped partials 7+8, tamed 5+6 to keep the wash smooth, not buzzy)
    for (const semi of ch.q) {
      voice(tBar + hum(0.004), SPBAR * 0.98, root + semi + 12, padGain,
        { atk: 0.35, rel: 0.4, vibD: 0.003,
          parts: [[1, 1], [2, 0.45], [3, 0.22], [4, 0.12], [5, 0.06]] });
    }
    // high sparkle pad — octave up, much quieter + fewer partials so it
    // doesn't pile into the 2-4 kHz buzz band
    if (sec.name !== "overture" && tier >= 1) {
      voice(tBar + hum(0.005), SPBAR * 0.98, root + 24, padGain * 0.22,
        { atk: 0.65, rel: 0.65, vibR: 4.2, vibD: 0.004,
          parts: [[1, 1], [2, 0.25], [3, 0.10]] });
    }
    // body voice — low root warmth; fades in by tier (texture ins/outs)
    if (tier >= 1 || sec.bars < 24)
      voice(tBar + hum(0.003), SPBAR * 0.98, root, padGain * 0.55,
        { atk: 0.25, rel: 0.35, parts: [[1, 1], [2, 0.35], [3, 0.14], [4, 0.06]] });
    // the bass — now in EVERY section (the pressure floor, overture too)
    sub(tBar, SPBAR * 0.99, root, sec.name === "bridge" ? 0.46 : 0.58);
  }

  // steam-release: long breath voice spanning the whole section. Quiet
  // backbone hiss + an extra crescendo blast across the LAST 4 bars
  // into the next section (the "release"). Coda gets a long dissolve.
  const secDur = sec.bars * SPBAR;
  // Overture steam is fully exposed (no kick to mask it) — give it a far
  // denser bank + a lifted fMin so the partials fuse into a smooth wash
  // instead of a comb of beating sines (the "weird buzzes" in the open).
  steam(startSec, secDur,
    sec.name === "bridge" ? 0.030 : sec.name === "overture" ? 0.032 : 0.022,
    sec.name === "overture"
      ? { atk: 2.4, rel: 1.8, voices: 220, fMin: 760 }
      : { atk: 1.4, rel: 1.8 });
  if (sec.name !== "coda") {
    steam(startSec + Math.max(0, secDur - 4 * SPBAR), 4 * SPBAR + 0.5,
      sec.name === "develop" ? 0.065 : 0.045,
      { atk: 3.0, rel: 0.6, breathRate: 0.8, breathDepth: 0.45 });
  } else {
    // coda: long final steam dissipation under the tail
    steam(startSec + 6 * SPBAR, 10 * SPBAR + TAIL, 0.035,
      { atk: 3.5, rel: 4.5, breathRate: 0.35, breathDepth: 0.55 });
  }

  // kick + perc grid — HALFTIME by default (beats 1+3 only) so each hit
  // lands like a hole with space around it. Climax adds a sparse ghost
  // on the "and-of-4" of every other bar; that's the only density bump.
  for (let b = 0; b < sec.bars; b++) {
    const tBar = (bar + b) * SPBAR;
    const dr = sec.drive;
    const lastBar = b === sec.bars - 1;
    if (sec.kick === "halftime" || sec.kick === "halfhard") {
      for (const beat of [0, 2]) {                       // beats 1 + 3
        const tk = tBar + beat * SPB + hum(0.003);
        kick(tk, dr, 1); kickEvents.push({ t: +tk.toFixed(4) });
      }
      if (sec.kick === "halfhard" && b % 2 === 1) {      // climax: ghost on every other bar
        const tk = tBar + 3.5 * SPB + hum(0.003);
        kick(tk, dr * 0.7, 0.7); kickEvents.push({ t: +tk.toFixed(4) });
      }
      if (lastBar && sec.name === "develop") {           // pre-climax: 4 kicks across the last bar (was 16th roll)
        for (let r = 0; r < 4; r++) {
          const tk = tBar + r * SPB;
          kick(tk, dr * (0.65 + r * 0.10), 0.85);
          kickEvents.push({ t: +tk.toFixed(4) });
        }
      }
    } else if (sec.kick === "pulse") {                   // study zone: halftime (same as default now)
      for (const beat of [0, 2]) {
        const tk = tBar + beat * SPB; kick(tk, dr, 0.85);
        kickEvents.push({ t: +tk.toFixed(4) });
      }
    } else if (sec.kick === "fade") {
      if (b < 4) { const tk = tBar; kick(tk, dr * (1 - b / 5), 0.8);
        kickEvents.push({ t: +tk.toFixed(4) }); }
    }
    // perc tick — also widened: 8th-positions 1 + 5 only (the "and-of-1" and "and-of-3")
    // so the hi-hats mirror the halftime kicks instead of fighting them.
    if (sec.kick !== "none" && sec.name !== "coda") {
      const bridgeSparse = sec.name === "bridge";
      const ptier = Math.floor(b / 8);                          // perc fades in by phase
      tick(tBar + 1 * (SPB / 2), bridgeSparse ? 0.14 : 0.20);   // and-of-1
      if (ptier >= 1) tick(tBar + 5 * (SPB / 2), bridgeSparse ? 0.14 : 0.20);  // and-of-3 IN
      if (!bridgeSparse && ptier >= 1) tick(tBar + 3.5 * SPB, 0.22, true);     // open hat IN
      // backbeat snare — beats 2 + 4, the hardcore crack the HOLE kick
      // omits. Softer in the bridge (study calm), hottest in the climax.
      const snGain = bridgeSparse ? 0.30
                   : sec.name === "climax" ? 0.56
                   : sec.name === "develop" ? 0.50 : 0.48;
      for (const beat of [1, 3]) {
        if (lastBar && sec.name === "develop" && beat === 3) continue; // roll covers it
        const ts = tBar + beat * SPB + hum(0.003);
        snare(ts, snGain); snareEvents.push({ t: +ts.toFixed(4) });
      }
      // pre-climax fill: a half-bar 16th snare roll crescendo across the
      // last 2 beats of develop, locking with the 4-kick run into climax.
      if (lastBar && sec.name === "develop") {
        for (let r = 0; r < 8; r++) {
          const ts = tBar + 2 * SPB + r * (SPB / 4);
          snare(ts, 0.26 + r * 0.045);
          snareEvents.push({ t: +ts.toFixed(4) });
        }
      }
    }
  }

  // melodic layer
  const layTheme = (notes, baseGain, brass, regOff = 0, t0 = sectionStartT) => {
    let beatPos = 0;
    for (const [off, beats] of notes) {
      const tN = t0 + beatPos * SPB;
      const midi = ROOT_MEL + off + tr + regOff;
      const dur = beats * SPB * 0.96;
      if (brass) {
        voice(tN + hum(0.004), dur, midi, baseGain, {
          atk: 0.018, rel: 0.10, vibR: 5.6, vibD: 0.007, drive: 1.25,
          parts: [[1, 1], [2, 0.6], [3, 0.45], [4, 0.28], [5, 0.2], [6, 0.12], [7, 0.07]] });
        voice(tN + hum(0.004), dur, midi - 12, baseGain * 0.5, {
          atk: 0.03, rel: 0.12, parts: [[1, 1], [2, 0.4], [3, 0.2]] }); // oct double
      } else {
        voice(tN + hum(0.004), dur, midi, baseGain, {
          atk: 0.12, rel: 0.22, vibR: 4.6, vibD: 0.005,
          parts: [[1, 1], [2, 0.4], [3, 0.22], [4, 0.1]] });
      }
      beatPos += beats;
    }
    return beatPos;
  };
  // independent countermelody — a warm horn line under the leitmotif.
  // Distinct timbre (rounder, fewer high partials than the brass theme)
  // and panned a touch left so the two voices read as separate lines.
  const layCounter = (notes, baseGain, t0 = sectionStartT) => {
    let beatPos = 0;
    for (const [off, beats] of notes) {
      const tN = t0 + beatPos * SPB;
      voice(tN + hum(0.005), beats * SPB * 0.98, ROOT_MEL + off + tr, baseGain, {
        atk: 0.045, rel: 0.18, vibR: 4.8, vibD: 0.006, drive: 1.05, pan: -0.28,
        parts: [[1, 1], [2, 0.55], [3, 0.3], [4, 0.14], [5, 0.06]] });
      beatPos += beats;
    }
  };
  const sectionStartT = startSec;

  if (sec.theme === "soft") {
    layTheme((ULTIMATE ? ultThemeAt(sec.name) : THEME_V).slice(0, 11), 0.085, false);
  } else if (sec.theme === "brass") {
    // principal leitmotif + the independent horn countermelody, the
    // 8-bar pair looped to fill the whole section — real two-voice
    // counterpoint, not a unison double. --strategy reshapes the lead.
    const loops = Math.max(1, Math.floor(sec.bars / 8));
    for (let lp = 0; lp < loops; lp++) {
      const t0 = sectionStartT + lp * 8 * SPBAR;
      const tv = ULTIMATE ? ultThemeAt(sec.name, lp) : THEME_V;
      const canon = STRATEGY === "stretto" || (ULTIMATE && sec.name === "climax");
      // tiered arrangement — loop 0 SPOTLIGHTS the theme alone; the
      // counterpoint enters on loop 1; loop 2 adds an octave sparkle.
      layTheme(tv, lp === 0 ? 0.175 : 0.155, true, 0, t0);
      if (canon) layTheme(tv, 0.11, true, -12, t0 + SPBAR); // octave-down canon, 1 bar late
      else if (lp >= 1) layCounter(COUNTER_V, 0.108, t0);   // counter IN on the restatement
      if (lp >= 2 && !canon) layTheme(tv, 0.052, true, 12, t0); // octave sparkle, last pass
    }
  } else if (sec.theme === "bsoft") {
    layTheme(BTHEME, 0.11, false);                        // B theme, lyrical
    layTheme(BTHEME, 0.10, false, -0)
  } else if (sec.theme === "frag") {
    // development: fragment the head (first 4 notes), sequence it up the
    // scale every 2 bars, hoover doubling, riser into the climax.
    const head = (ULTIMATE ? ultThemeAt(sec.name) : THEME_V).slice(0, 4);
    for (let seg = 0; seg < 8; seg++) {
      const t0 = sectionStartT + seg * 2 * SPBAR;
      let bp = 0;
      for (const [off, beats] of head) {
        const midi = ROOT_MEL + off + tr + (seg % 4) * 2;
        voice(t0 + bp * SPB + hum(0.004), beats * SPB * 0.9, midi, 0.135, {
          atk: 0.016, rel: 0.09, drive: 1.3,
          parts: [[1, 1], [2, 0.6], [3, 0.4], [4, 0.24], [5, 0.16]] });
        if (seg >= 4) hoover(t0 + bp * SPB, beats * SPB * 0.8, midi - 12, 0.13);
        bp += beats;
      }
    }
    riser(sectionStartT + (sec.bars - 2) * SPBAR, 2 * SPBAR,
      ROOT_MEL - 12, ROOT_MEL + 14, 0.28);                // scream into climax
  } else if (sec.theme === "dissolve") {
    // coda: theme fragment, each restatement softer + longer, strings
    // bloom and hold → continuous loop-friendly tail (no dead air).
    layTheme((ULTIMATE ? ultThemeAt(sec.name) : THEME_V).slice(0, 8), 0.10, true);
    layCounter(COUNTER_V.slice(0, 4), 0.058);             // the counter, dissolving too
    voice(sectionStartT + 6 * SPBAR, 6 * SPBAR + TAIL, ROOT_MEL + tr, 0.07,
      { atk: 1.2, rel: 2.4, parts: [[1, 1], [2, 0.35], [3, 0.18], [5, 0.06]] });
    voice(sectionStartT + 6 * SPBAR, 6 * SPBAR + TAIL, ROOT_MEL + tr - 12, 0.06,
      { atk: 1.2, rel: 2.4, parts: [[1, 1], [2, 0.3]] });
  }

  // stabs — climax only, on the offbeats (the hardcore "ear")
  if (sec.name === "climax") {
    for (let b = 0; b < sec.bars; b++) {
      const tBar = (bar + b) * SPBAR;
      const ch = CHORD[cyc(sec.chords, b)];
      stab(tBar + 1.5 * SPB, ch.root + tr + 24, 0.30);
      stab(tBar + 3.5 * SPB, ch.root + tr + 24 + 7, 0.26);
    }
  }
  bar += sec.bars;
}

// ── rattle — @jeffrey's recorded shake, the one sampled voice ─────────
// THE LAW is amended (see header): this single organic layer rides on
// top of the all-sine engine. Sparse by default — a pickup shake into
// the downbeat every other bar — so the halftime holes survive. "drive"
// lays a steady offbeat-8th shaker through the driven sections. Absent
// the sample file, the render stays pure all-sine.
const RATTLE_MODE = flags.rattle || "sparse";
const RATTLE_PATH = flags["rattle-path"] || resolve(HERE, "../samples/rattle.wav");
if (RATTLE_MODE !== "off" && existsSync(RATTLE_PATH)) {
  const rat = loadWavMono(RATTLE_PATH);
  const ratGain = Number(flags["rattle-gain"] ?? 0.5);
  let placed = 0;
  for (const sr of sectionRanges) {
    const driven = sr.name === "statement" || sr.name === "develop" || sr.name === "climax";
    if (!driven && sr.name !== "bridge") continue;
    for (let b = sr.startBar; b < sr.endBar; b++) {
      const tBar = b * SPBAR;
      if (RATTLE_MODE === "drive" && driven) {
        for (const beat of [0.5, 1.5, 2.5, 3.5]) {           // offbeat 8ths
          playSample(tBar + beat * SPB + hum(0.004), rat, ratGain * 0.5,
            { rate: 1 + hum(0.03), pan: hum(0.35) });
          placed++;
        }
      } else if ((b - sr.startBar) % 2 === 1) {              // pickup every 2 bars
        playSample(tBar + 3.5 * SPB + hum(0.004), rat,
          driven ? ratGain : ratGain * 0.6,
          { rate: 1 + hum(0.03), pan: hum(0.3) });
        placed++;
      }
    }
  }
  console.log(`→ rattle · ${RATTLE_MODE} · ${placed} hits · sampled (THE LAW amended)`);
} else if (RATTLE_MODE !== "off") {
  console.log(`· rattle · no sample at ${RATTLE_PATH.replace(process.env.HOME, "~")} — pure all-sine`);
}

// ── master sum: gentle bus glue, no clipping (pre-master headroom) ─────
let peak = 0;
for (let i = 0; i < N; i++) { const a = Math.abs(L[i]); const b = Math.abs(Rb[i]); if (a > peak) peak = a; if (b > peak) peak = b; }
const g = peak > 0 ? Math.min(1, 0.89 / peak) : 1;
for (let i = 0; i < N; i++) {
  L[i] = Math.tanh(L[i] * g * 1.04);
  Rb[i] = Math.tanh(Rb[i] * g * 1.04);
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

// ── struct.json (scratch-mix grid + tooling) ──────────────────────────
mkdirSync(dirname(STRUCT), { recursive: true });
writeFileSync(STRUCT, JSON.stringify({
  engine: "hellsine", allSine: true, meter: 4, bpm: BPM,
  scale: "minor", rootMidi: 50, totalBars: TOTAL_BARS, totalSec,
  sections: sectionRanges,
  counts: { kick: kickEvents.length, snare: snareEvents.length },
  events: { kick: kickEvents, snare: snareEvents },
}, null, 2));

console.log(`hellsine · ${BPM} BPM · hell=${HELL} · strategy=${STRATEGY} · ${TOTAL_BARS} bars · ${totalSec.toFixed(1)}s`);
console.log(`→ wav    · ${OUT}`);
console.log(`→ struct · ${STRUCT}`);
