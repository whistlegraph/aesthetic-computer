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
import { writeFileSync, mkdirSync, readFileSync, readdirSync, existsSync } from "node:fs";
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
// humEager — a personality-bearing humanizer for MELODIC content (never
// kicks). Distribution skews EARLY (eager / nervous / forward), tight on
// the beat sometimes, occasionally hesitating/stumbling LATE, and rarely
// "forgetting" — a full miss that lands well after the beat.
const humEager = (amt) => {
  const r = rng();
  if (r < 0.58) return -amt * (0.40 + rng() * 0.70);   // eager forward push
  if (r < 0.80) return (rng() * 2 - 1) * amt * 0.30;   // tight, nervously on
  if (r < 0.94) return amt * (1.0 + rng() * 1.4);      // hesitating / stumbly
  return amt * (2.2 + rng() * 2.0);                    // forgetting — way late
};

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
  // "that" lengthened 0.5 → 1.0β (was too short for the vocal to land);
  // compensated by trimming the v2 sustain "ney" from 2 → 1.5β so the
  // total antecedent still sums to 16 beats and the "hope" 1.5β
  // syncopation stays intact.
  [-5, 1], [0, 1.5], [3, 1.0], [7, 1],            // A3 → D4 F4 A4   (the call)
  [7, 1], [8, 1], [7, 1], [5, 1],                 // A4 Bb4 A4 G4    (swell)
  [3, 1.5], [2, 1], [0, 1],                       // F4 . E4 D4      (descent — v2 shortened)
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
  // octave-skips — every ~4 bars, a high note jumps an octave up. The
  // next 2 notes "skipaloo" back down via larger-than-stepwise leaps
  // (5th down, then 3rd down), so the line climbs out then skips home.
  "octave-skips": (t) => {
    const r = [];
    let jumpAt = -10;
    for (let i = 0; i < t.length; i++) {
      const [o, d] = t[i];
      if (o >= 5 && i - jumpAt >= 4) {
        r.push([o + 12, d]);   // octave UP
        jumpAt = i;
      } else if (i === jumpAt + 1) {
        r.push([o + 7, d]);    // skip down a fifth from the upper octave
      } else if (i === jumpAt + 2) {
        r.push([o + 3, d]);    // another skip down (a third)
      } else {
        r.push([o, d]);
      }
    }
    return r;
  },
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
  statement: ["none", "ornament", "octave-skips"],       // stated, then ornamented, then octave-skip variation
  develop:   ["sixteenths"],                             // busy fragmentation
  climax:    ["arpeggiate", "octave-skips", "sixteenths"], // arp → skipaloos → 16ths
  coda:      ["none"],                                   // resolve home, plain
};
// ULTIMATE — bar-by-bar voicing rotation. Cycle through root pos / 1st
// inv / 2nd inv, then add a b7 color on the 4th bar of each 4-bar phrase
// so the bass-chord wash audibly evolves instead of restating identically.
const ultVoicing = (q, b4) => {
  if (!ULTIMATE) return q;
  if (b4 === 1) return [q[1], q[2], q[0] + 12];
  if (b4 === 2) return [q[2], q[0] + 12, q[1] + 12];
  if (b4 === 3) return [...q, 10];
  return q;
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
  { name: "develop",   bars: 24, kick: "halftime", drive: HELL*0.75, chords: ["Dm","Dm","F","F","Gm","Gm","A","A","Bb","Bb","C","C","Dm","A","Dm","A"], theme: "frag" },
  { name: "climax",    bars: 24, kick: "halfhard", drive: HELL*1.3, transpose: 14, chords: ["Dm","Dm","Bb","Bb","F","F","C","C"], theme: "brass" },
  { name: "coda",      bars: 16, kick: "fade",     drive: HELL*0.6,  chords: ["Dm","Dm","Gm","Gm","C","C","Dm","Dm"], theme: "dissolve" },
];
const TOTAL_BARS = PLAN.reduce((a, s) => a + s.bars, 0);
const TAIL = 3.2;
const totalSec = TOTAL_BARS * SPBAR + TAIL;
// AC_STAMP_TIME — where the "aesthetic dot computer" stamp lands (3.5 s
// before the climax drop). Kicks AFTER this time switch to a shorter +
// higher-pitched variant (octave up, half duration) per @jeffrey.
let _climaxBarOffset = 0;
for (const _s of PLAN) { if (_s.name === "climax") break; _climaxBarOffset += _s.bars; }
const CLIMAX_START_SEC = _climaxBarOffset * SPBAR;
const AC_STAMP_TIME = CLIMAX_START_SEC - 3.5;
const N = Math.ceil(totalSec * SR);
const L = new Float32Array(N), Rb = new Float32Array(N);
// shake-only wet bus — playSample writes here when opt.wetSend > 0; we
// run a Schroeder reverb over this bus at the end and mix back into L/Rb.
const WL = new Float32Array(N), WR = new Float32Array(N);
// sidechain duck (1 = open) — kicks pull it down; sub/pad/strings ride it.
const DUCK = new Float32Array(N).fill(1);

// ── SDT physical bubble (ported from pop/marimba/bin/render-marimbaba.mjs)
// Writes a single self-normalising water-bubble pop into L/Rb (with
// optional wet-bus send). Used in the coda for the "kicks dissipate into
// bubbles + bubble field as train fades in" outro.
//   radiusMM — bubble radius. 3 = tiny ping ~1 kHz, 30 = deep gloop ~100 Hz.
//   rise     — upward pitch glide from surface tension. 0 = static,
//              0.2–0.8 = gentle bath, 4+ = boiling.
function bubble(startSec, radiusMM, rise, volume, pan, wetSend = 0, depth = 1.0) {
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
    L[dst]  += v * gL;
    Rb[dst] += v * gR;
    if (wetSend > 0) {
      WL[dst] += v * gL * wetSend;
      WR[dst] += v * gR * wetSend;
    }
  }
}

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

// Pitch-swept variant of playSample — plays the sample with a
// continuous exponential rate sweep from startRate → endRate over
// the playback window. Used for the kick rattles so they warp out of
// (or fall into) the kick body, mirroring the kick's pitch sweep.
// `maxDurMs` caps the output length so the rattle stays contained
// inside the kick's body. Routes to wet bus via wetSend like playSample.
function playSampleSwept(t, buf, gain = 1, opt = {}) {
  const startRate = opt.startRate || 1;
  const endRate   = opt.endRate   || startRate;
  const pan = Math.max(-1, Math.min(1, opt.pan || 0));
  const wetSend = opt.wetSend ?? 0;
  const maxDurMs = opt.maxDurMs ?? 180;
  // Longer default fade — 28 ms — to kill the start-of-roll pops users
  // reported around 2:30, where slow-rate swept rattles begin reading
  // from buf[0] and any low-level DC offset became audible at the edge.
  const fadeMs = opt.fade ?? 0.028;
  const startI = Math.floor(t * SR);
  const maxN = Math.floor(maxDurMs * SR / 1000);
  const fadeN = Math.floor((typeof fadeMs === "number" && fadeMs < 1 ? fadeMs : fadeMs / 1000) * SR);
  // bufOffset (seconds) — start reading from inside the sample, not buf[0].
  // Useful when the sample has multiple events (e.g. the crow file holds
  // 5 caws spaced ~0.7 s apart) so each chop can lift a different event.
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
    L[di]  += vL;
    Rb[di] += vR;
    if (wetSend > 0) {
      WL[di] += vL * wetSend;
      WR[di] += vR * wetSend;
    }
    // exponential rate sweep — startRate → endRate over maxN samples
    const p = k / maxN;
    const rate = startRate * Math.pow(endRate / startRate, p);
    bufPos += rate;
  }
}

// Place a sample buffer at time t — linear-interp resample for pitch,
// simple pan, short declick fades. Mixes into the L/Rb bus like a voice.
// opt.wetSend (0..1) also writes the panned sample to WL/WR for the
// shake reverb bus.
function playSample(t, buf, gain = 1, opt = {}) {
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
    if (k < fadeN) s *= k / fadeN;                       // declick in
    if (outLen - k < fadeN) s *= (outLen - k) / fadeN;   // declick out
    const v = s * gain;
    const vL = v * (pan > 0 ? 1 - pan : 1);
    const vR = v * (pan < 0 ? 1 + pan : 1);
    L[di]  += vL;
    Rb[di] += vR;
    if (wetSend > 0) {
      WL[di] += vL * wetSend;
      WR[di] += vR * wetSend;
    }
  }
}

// The HOLE kick — no punch, no click, no transient. A slow-blooming
// sub-only sine pitch-drop that lives mostly through what it removes
// from the surrounding mix. Each hit is a vacuum carved by the duck
// (15 ms slam-shut → 450 ms slow re-open); the kick itself is just the
// bass pressure filling the hole. Still pure sine, still saturated for
// hardcore body, just stripped of its bright attack.
function kick(t, drive = HELL, gain = 1, thin = 0) {
  if (NOKICK) return;                        // --nokick: melodies fully exposed
  // ULTIMATE swaps in a punch kick — fast attack + brief sine click +
  // higher pitch start + lighter ducking. Front of mix, not a vacuum.
  // The hole-kick design (slow bloom, 8% slam duck) stays for non-ULTIMATE.
  // `thin` (0..1) shortens the body decay + softens the click so the kick
  // thins out across each section, leaving room as motifs accumulate.
  if (ULTIMATE) {
    // After the "aesthetic dot computer" stamp (3.5 s before climax),
    // kicks become much SHORTER + HIGHER per @jeffrey: half duration,
    // pitch sweep doubled (octave up). Front-of-mix tight clicks
    // instead of the long-tailed punch.
    const shortHigh = t >= AC_STAMP_TIME;
    // GLOBAL THINNING — from t=0 up to AC_STAMP_TIME, kicks tighten
    // progressively. @jeffrey: "more and more tight lipped from the
    // start up until the aesthetic.computer stamp". By the stamp, thin
    // saturates at ~0.95 → very short body. Past the stamp, shortHigh
    // takes over with its own tight character.
    const globalTighten = shortHigh ? 0 : Math.min(0.95, t / AC_STAMP_TIME * 0.95);
    const effThin = Math.max(thin, globalTighten);
    const dur = shortHigh ? 0.15 : 0.30;
    const pStart = shortHigh ? 520 : 260;
    const pEnd   = shortHigh ? 100 : 50;
    const pT = shortHigh ? 0.020 : 0.030;    // faster sweep when short
    const bodyTauBase = shortHigh ? 0.060 : 0.14;
    const bodyTau = bodyTauBase * (1 - effThin * 0.55);
    const clickAmp = (shortHigh ? 0.40 : 0.55) * (1 - effThin * 0.65);
    let ph = 0;
    // Lighter duck — 8 ms close to 35%, 180 ms re-open. Sub still pumps,
    // chords/strings aren't sucked away, so the kick fights forward.
    const di = Math.floor(t * SR);
    const dN = Math.floor(0.18 * SR);
    const closeN = Math.floor(0.008 * SR);
    const openN  = dN - closeN;
    for (let k = 0; k < dN && di + k < N; k++) {
      let env;
      if (k < closeN) env = 1 - (k / closeN) * 0.65;
      else { const p = (k - closeN) / openN; env = 0.35 + 0.65 * (1 - Math.exp(-p * 3.5)); }
      if (env < DUCK[di + k]) DUCK[di + k] = env;
    }
    write(t, (lt) => {
      const f = pEnd + (pStart - pEnd) * Math.exp(-lt / pT);
      ph += (TAU * f) / SR;
      const atk   = 1 - Math.exp(-lt / 0.0008);             // 0.8 ms snap
      const decay = Math.exp(-lt / bodyTau);                // thinned body
      const amp   = atk * decay;
      // click — short 2.4 kHz sine, decays in ~4 ms (still all-sine)
      const click = lt < 0.006
        ? Math.sin(TAU * 2400 * lt) * Math.exp(-lt / 0.0015) * clickAmp
        : 0;
      let x = Math.sin(ph);
      x = Math.tanh(x * drive * 0.65);                      // harder gabber drive
      const v = (x * amp + click) * 0.92 * gain;
      return [v, v];
    }, dur);
    // BOOwub — reverse-kick supersample right after the forward kick.
    // @jeffrey: only AFTER the aesthetic.computer stamp (not in the
    // first breaks). Forward kick stays at its tight short-high pitch
    // (one pitch); the reverse-kick gets pitch-shifted up OR down by a
    // varied amount per kick — chosen from [-oct, -fifth, +fifth, +oct]
    // — giving the wub character varied tonal answers ("wub one pitch,
    // reverse kick pitch shifted up or down").
    if (shortHigh) {
      // Deterministic pitch flow — beat-indexed cycle through a 4-step
      // sequence so consecutive wubs trace a repeatable shape, not noise.
      // [-oct, +fifth, -fifth, +oct] alternates down/up/down/up — a
      // dub-style wubwub. Swap the order or extend the sequence to
      // re-flow the beatz.
      const revPitchOpts = [0.5, 1.5, 0.667, 2.0];
      const beatIdx = Math.floor(t / SPB);
      const revPitch = revPitchOpts[beatIdx % revPitchOpts.length];
      const revPStart = pStart * revPitch;
      const revPEnd   = pEnd   * revPitch;
      const revDur = dur * 0.85;
      let phR = 0;
      write(t + dur, (lt) => {
        const rlt = revDur - lt;                            // reversed local time
        const f = revPEnd + (revPStart - revPEnd) * Math.exp(-rlt / pT);
        phR += (TAU * f) / SR;
        const atk   = 1 - Math.exp(-rlt / 0.0008);
        const decay = Math.exp(-rlt / bodyTau);
        const amp   = atk * decay;
        let x = Math.sin(phR);
        x = Math.tanh(x * drive * 0.55);
        const v = x * amp * 0.92 * gain * 0.55;
        return [v, v];
      }, revDur);
    }
    return;
  }
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
  // SLAP + SNAP snare: brighter crack band (up to 9 kHz), denser partial
  // bank (96 voices), tighter body decay, and an extra HF snap blip in
  // the first 6 ms for the attack click.
  const dur = 0.30, bodyF = opt.bodyF || 175;
  const nV = 96, fMin = 1200, fMax = 9000;
  const freqs = new Float64Array(nV), phs = new Float64Array(nV);
  for (let i = 0; i < nV; i++) {
    const u = (i + rng()) / nV;
    freqs[i] = fMin * Math.pow(fMax / fMin, u);
    phs[i] = rng() * TAU;
  }
  const norm = 1 / Math.sqrt(nV);
  write(t, (lt) => {
    // body — tighter pitch blip + faster decay → tighter punch
    const pf = bodyF * (1 + 0.5 * Math.exp(-lt / 0.008));
    const body = (Math.sin(TAU * pf * lt) + 0.55 * Math.sin(TAU * pf * 1.48 * lt))
               * Math.exp(-lt / 0.045);
    // crack — brighter + denser additive-sine, fast attack, fast decay
    let noise = 0;
    for (let i = 0; i < nV; i++) noise += Math.sin(TAU * freqs[i] * lt + phs[i]);
    const crackEnv = (1 - Math.exp(-lt / 0.0006)) * Math.exp(-lt / 0.030);
    // HF snap blip — a brief 6 kHz sine click that gives the transient
    // its "snap" — disappears in ~6 ms
    const snap = lt < 0.006
      ? Math.sin(TAU * 6200 * lt) * Math.exp(-lt / 0.0014) * 0.55
      : 0;
    let x = body * 0.75 + noise * norm * crackEnv * 0.85 + snap;
    x = Math.tanh(x * 1.4);
    const v = x * gain * Math.min(1, lt / 0.0004);
    return [v * 0.96, v];
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

// Sub — PUNCHY: tighter release, less sustained fundamental, more
// 2nd + 3rd harmonic for definition + a brief click for transient. The
// overall gain is also pulled down (×0.68) so the sub doesn't smother
// the rest of the mix in headphones.
function sub(t, dur, midi, gain = 0.5) {
  const f = m2f(midi - 12);
  write(t, (lt) => {
    // Faster release for tightness — 30 ms instead of 60 ms tail
    const a = Math.min(1, lt / 0.006) * Math.exp(-Math.max(0, lt - (dur - 0.03)) / 0.020);
    const d = DUCK[Math.min(N - 1, Math.floor((t + lt) * SR))];
    // Boosted 2nd+3rd harmonic gives the sub MORE mid presence + bite,
    // less just-low-rumble. Click in first 4 ms = transient definition.
    let x = Math.sin(TAU * f * lt)
          + 0.42 * Math.sin(TAU * f * 2 * lt)
          + 0.18 * Math.sin(TAU * f * 3 * lt);
    const click = lt < 0.004
      ? Math.sin(TAU * 1100 * lt) * Math.exp(-lt / 0.0012) * 0.45
      : 0;
    x = Math.tanh(x * 1.85) * 0.85 + click * 0.5;
    const v = x * a * d * gain * 0.68;             // overall sub pulled back
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

// Piano — all-sine additive grand-piano emulation. Felted 3 ms hammer
// attack, two-stage decay (fast initial knee + long sympathetic tail),
// 8 stretched partials (real-piano inharmonicity B ≈ 0.0004 → upper
// partials sit sharp of pure integer ratios). Bitcrushed at the output
// (6-bit + ×4 SR reduction) for the lo-fi grand-piano grit the queen
// asked for. THE LAW HOLDS: every sample is still a sum of sines, just
// quantized in amplitude + time.
function piano(t, dur, midi, gain = 0.14, opt = {}) {
  const f = m2f(midi);
  // [ratio, amp, decay-scale (higher partials decay faster)]
  const parts = [
    [1.0000, 1.00, 1.00], [2.0008, 0.55, 0.85],
    [3.0024, 0.30, 0.70], [4.0048, 0.18, 0.55],
    [5.0080, 0.10, 0.45], [6.0120, 0.06, 0.38],
    [7.0168, 0.035, 0.32], [8.0224, 0.020, 0.28],
  ];
  const pan = opt.pan ?? 0, sus = opt.sus ?? 1.0;
  const fastTau = 0.35, slowTau = 2.2 * sus;
  // High notes decay faster — energy redistributes upward in a real piano
  const tauScale = Math.min(1, Math.max(0.3, m2f(60) / f));
  const phs = new Float64Array(parts.length);
  const bits = opt.bits ?? 6, hold = opt.hold ?? 4;
  const steps = 1 << (bits - 1);
  let held = 0, holdI = 0;
  write(t, (lt) => {
    const atk = 1 - Math.exp(-lt / 0.003);
    let x = 0;
    for (let i = 0; i < parts.length; i++) {
      const [r, a, ds] = parts[i];
      phs[i] += (TAU * f * r) / SR;
      const fast = Math.exp(-lt / (fastTau * ds * tauScale));
      const slow = Math.exp(-lt / (slowTau * ds * tauScale));
      x += a * Math.sin(phs[i]) * atk * (0.6 * fast + 0.4 * slow);
    }
    x = Math.tanh(x * 0.55);
    if (lt > dur) x *= Math.exp(-(lt - dur) / 0.15);
    // bitcrush + sample-and-hold (SR reduction) — the lo-fi grand piano
    if (holdI++ % hold === 0) held = Math.round(x * steps) / steps;
    const v = held * gain;
    return [v * (1 - Math.max(0, pan)), v * (1 + Math.min(0, pan))];
  }, dur + 0.8);
}

// Sine bell — additive partials with slight inharmonic stretch (bell-
// like spectrum: pure fundamental + harmonics 2..6, higher partials
// decay faster — the classic bell energy-redistribution). Slow attack,
// very long exponential decay (multi-second). Routes both dry (L/Rb)
// and wet (WL/WR, the shake reverb bus) so the bells bloom in the same
// hall as the rattles.
function bell(t, _dur, midi, gain = 0.085, opt = {}) {
  const f = m2f(midi);
  const parts = [
    [1.000, 1.00], [2.012, 0.55], [3.025, 0.33],
    [4.055, 0.18], [5.10,  0.10], [6.20,  0.05],
  ];
  let ampSum = 0; for (const p of parts) ampSum += p[1];
  const norm = 1 / ampSum;
  const pan = opt.pan ?? 0;
  const atk = opt.atk ?? 0.080;
  const decTau = opt.decTau ?? 4.0;
  const wetSend = opt.wetSend ?? 0.8;
  const phs = new Float64Array(parts.length);
  const tailDur = decTau * 5;        // essentially silent past 5τ
  // Fibonaccian fizzle on the tail — per-partial pitch wobble at
  // Fibonacci-spaced rates (1, 1, 2, 3, 5, 8 Hz), depth ramping in after
  // 50 % of the decay so the bell edges fishtail as they fizzle out.
  const FIB = [1, 1, 2, 3, 5, 8];
  const fizzleOn = opt.fizzle !== false;
  const fizzleAfter = atk + decTau * 0.5;
  const startI = Math.max(0, Math.floor(t * SR));
  const endI   = Math.min(N, Math.ceil((t + atk + tailDur) * SR));
  for (let i = startI; i < endI; i++) {
    const lt = i / SR - t;
    const env = lt < atk ? lt / atk : Math.exp(-(lt - atk) / decTau);
    let x = 0;
    for (let k = 0; k < parts.length; k++) {
      const [r, a] = parts[k];
      let rEff = r;
      if (fizzleOn && lt > fizzleAfter) {
        const fizzleP = Math.min(1, (lt - fizzleAfter) / decTau);
        // The bell tail should sound like wiggling worms falling UP or
        // DOWN at the end of decay. Two compounded effects:
        //   1. directional DRIFT — odd partials slide up, even slide down,
        //      so the tail spreads in pitch as it fades.
        //   2. accelerating WIGGLE — fibHz scaled up by fizzleP^1.4 so the
        //      wobble gets ~4× faster by the very end; depth also widens.
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
    L[i]  += vL;
    Rb[i] += vR;
    if (wetSend > 0) {
      WL[i] += vL * wetSend;
      WR[i] += vR * wetSend;
    }
  }
}

// All-sine power saw — additive partials Σ sin(2π·n·f·t)/n is a true
// saw wave (Fourier series), so THE LAW holds. We use ~18 partials,
// stacked with a slightly detuned twin per partial (supersaw width),
// sidechain-ducked by the kick, and optionally 16th-note GATED for the
// classic trance "flashing through" effect. Octave-stack the call site
// for the supersaw thickness.
function sawLead(t, dur, midi, gain = 0.16, opt = {}) {
  const f = m2f(midi);
  const partials = opt.partials ?? 18;
  const detune = opt.detune ?? 0.006;
  const pan = opt.pan ?? 0;
  const atk = opt.atk ?? 0.012;
  const rel = opt.rel ?? 0.06;
  const drive = opt.drive ?? 0.8;
  const gateMs = opt.gateMs ?? 0;             // 0 → no gate
  const gateOn = opt.gateOnFrac ?? 0.55;
  const phsA = new Float64Array(partials);
  const phsB = new Float64Array(partials);
  let norm = 0;
  for (let n = 1; n <= partials; n++) norm += 1 / n;
  const partNorm = 1 / norm;
  write(t, (lt) => {
    let env = Math.min(1, lt / atk);
    if (lt > dur - rel) env *= Math.max(0, (dur - lt) / rel);
    const d = DUCK[Math.min(N - 1, Math.floor((t + lt) * SR))];
    let gate = 1;
    if (gateMs > 0) {
      const phase = (lt * 1000 / gateMs) % 1;
      gate = phase < gateOn ? 1 : 0;
    }
    let x = 0;
    for (let n = 1; n <= partials; n++) {
      const i = n - 1;
      phsA[i] += (TAU * f * n) / SR;
      phsB[i] += (TAU * f * (1 + detune) * n) / SR;
      const a = 1 / n;
      x += a * (Math.sin(phsA[i]) + Math.sin(phsB[i])) * 0.5;
    }
    x = Math.tanh(x * partNorm * drive);
    const v = x * env * d * gate * gain;
    return [v * (pan > 0 ? 1 - pan : 1), v * (pan < 0 ? 1 + pan : 1)];
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

// Wood-block sine blip — lower, shorter, softer than the hi-hat tick.
// Used by the ULTIMATE polyrhythm so 3-against-4 sits clave-like under
// the 4-beat grid instead of fighting the 7.4-9.2 kHz hat band.
function woodTick(t, gain = 0.13) {
  const f = 1900, dur = 0.045;
  write(t, (lt) => {
    const env = Math.exp(-lt / 0.010) * (1 - Math.exp(-lt / 0.0006));
    const x = Math.sin(TAU * f * lt) + 0.3 * Math.sin(TAU * f * 1.41 * lt);
    const v = x * env * gain;
    return [v * 0.92, v];
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
    // ULTIMATE overture — organs from the VERY beginning, but VERY quiet
    // (0.08 at bar 0), fading up slowly over the full overture so the
    // percussion still owns the early space.
    const overtureFade = (ULTIMATE && sec.name === "overture")
      ? Math.min(1, 0.03 + Math.pow(b / sec.bars, 2) * 0.97)   // 0.03 at b=0 → 0.27 mid → 1.0 by end (quadratic, stays quiet)
      : 1;
    const tBar = (bar + b) * SPBAR;
    const ch = CHORD[cyc(sec.chords, b)];
    const root = ch.root + tr;
    const padGain = (sec.name === "overture" || sec.name === "coda" ? 0.085 : 0.072)
                  * overtureFade;
    const tier = Math.floor(b / 8);              // 8-bar phase — layers fade in by tier
    const padQ = ultVoicing(ch.q, b % 4);
    // pad = sine triad with mid-weight partials (no upper-mid sizzle —
    // dropped partials 7+8, tamed 5+6 to keep the wash smooth, not buzzy)
    for (const semi of padQ) {
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
    // the bass — pressure floor in every section. ULTIMATE: on the 4th
    // bar of each 4-bar group the sub walks (root → leading tone toward
    // the next chord), so the harmonic motion is felt, not just heard.
    // Boosted subG for climax/develop — @jeffrey wants DEEEEP bass after 2:00.
    const subBoost = (sec.name === "climax" || sec.name === "develop") ? 1.45 : 1.0;
    const subG = (sec.name === "bridge" ? 0.46 : 0.58) * overtureFade * subBoost;
    if (ULTIMATE && b % 4 === 3 && b + 1 < sec.bars) {
      const nextRoot = CHORD[cyc(sec.chords, b + 1)].root + tr;
      sub(tBar, SPBAR * 0.5, root, subG);
      sub(tBar + SPBAR * 0.5, SPBAR * 0.49, nextRoot - 1, subG * 0.85);
    } else {
      sub(tBar, SPBAR * 0.99, root, subG);
    }
    // DEEEEP sub-octave layer for climax — drops another octave below the
    // primary sub (effective root - 24). Pulled back so it adds rumble
    // not muddy mid energy.
    if (ULTIMATE && sec.name === "climax") {
      sub(tBar, SPBAR * 0.99, root - 12, subG * 0.55);
    }

    // ULTIMATE Bachian piano — two voices running all the way through:
    // bass walks quarter-notes on chord tones (R, 5th, R+oct, 5th),
    // treble runs 8 eighth-notes per bar arpeggiating each chord with
    // an octave-crossing figure. Continuous perpetual-motion texture
    // sitting under the brass theme, bitcrushed for the lo-fi grit.
    if (ULTIMATE) {
      const bachG = (({
        overture: 0.052, statement: 0.072, bridge: 0.088,
        develop:  0.058, climax:    0.072, coda:    0.064,
      })[sec.name] || 0.060) * overtureFade;
      // Bitcrush per section — overture AND coda stay clean (the bitcrush
      // sample-and-hold quantizes pad amplitudes to discrete steps which
      // are audible as tiny pops over the quiet ends of the track). Only
      // the loud middle sections get the lo-fi grit.
      const bachOpts = (sec.name === "overture" || sec.name === "coda")
        ? { bits: 16, hold: 1 }
        : { bits: 6,  hold: 4 };
      const fifth = root + ch.q[2];
      // bass voice — quarter notes, low register, panned slightly left
      piano(tBar + 0 * SPB,        SPB * 0.96, root - 12,  bachG * 0.95, { pan: -0.20, sus: 0.95, ...bachOpts });
      piano(tBar + 1 * SPB,        SPB * 0.96, fifth - 12, bachG * 0.82, { pan: -0.20, sus: 0.85, ...bachOpts });
      piano(tBar + 2 * SPB,        SPB * 0.96, root,       bachG * 0.92, { pan: -0.20, sus: 0.95, ...bachOpts });
      piano(tBar + 3 * SPB,        SPB * 0.96, fifth - 12, bachG * 0.82, { pan: -0.20, sus: 0.85, ...bachOpts });
      // treble voice — 8 eighth-notes, two octaves up, panned slightly right
      const mid = root + 24, eighth = SPB / 2;
      const seq = [
        mid,             mid + ch.q[1],      mid + ch.q[2],  mid + ch.q[1] + 12,
        mid + 12,        mid + ch.q[2],      mid + ch.q[1],  mid + ch.q[2] - 12,
      ];
      const tG = bachG * 0.58;
      for (let k = 0; k < 8; k++) {
        piano(tBar + k * eighth + hum(0.004), eighth * 0.88, seq[k], tG,
          { pan: 0.20, sus: 0.55, ...bachOpts });
      }
    }
  }

  // steam-release: long breath voice spanning the whole section. Quiet
  // backbone hiss + an extra crescendo blast across the LAST 4 bars
  // into the next section (the "release"). Coda gets a long dissolve.
  const secDur = sec.bars * SPBAR;
  // Overture steam is fully exposed (no kick to mask it) — give it a far
  // denser bank + a lifted fMin so the partials fuse into a smooth wash
  // instead of a comb of beating sines (the "weird buzzes" in the open).
  // ULTIMATE pulls back the steam wash — the additive-sine breath bank is
  // the biggest "buzz" contributor outside the brass partials; cutting it
  // ~45% lets the melody + bass-chord motion sit forward without losing
  // the air entirely. Overture gets a deeper cut (the "rush of noise" at
  // the top): only ~15% of normal, so the opening reads as space, not buzz.
  const stMul = ULTIMATE
    ? (sec.name === "overture" ? 0.15 : 0.55)
    : 1.0;
  // ULTIMATE overture: steam waits until bar 6 (phase-3 reverb-in).
  const steamStartT = (ULTIMATE && sec.name === "overture")
    ? startSec + 6 * SPBAR
    : startSec;
  const steamDur = (ULTIMATE && sec.name === "overture")
    ? Math.max(0, secDur - 6 * SPBAR)
    : secDur;
  steam(steamStartT, steamDur,
    (sec.name === "bridge" ? 0.030 : sec.name === "overture" ? 0.032 : 0.022) * stMul,
    sec.name === "overture"
      ? { atk: 2.4, rel: 1.8, voices: 220, fMin: 760 }
      : { atk: 1.4, rel: 1.8 });
  if (sec.name !== "coda") {
    steam(startSec + Math.max(0, secDur - 4 * SPBAR), 4 * SPBAR + 0.5,
      (sec.name === "develop" ? 0.065 : 0.045) * stMul,
      { atk: 3.0, rel: 0.6, breathRate: 0.8, breathDepth: 0.45 });
  } else {
    // coda: long final steam dissipation under the tail
    steam(startSec + 6 * SPBAR, 10 * SPBAR + TAIL, 0.035 * stMul,
      { atk: 3.5, rel: 4.5, breathRate: 0.35, breathDepth: 0.55 });
  }

  // kick + perc grid — HALFTIME by default (beats 1+3 only) so each hit
  // lands like a hole with space around it. Climax adds a sparse ghost
  // on the "and-of-4" of every other bar; that's the only density bump.
  // ULTIMATE: per-section "thin out" arc for the kick. Body decay
  // shortens across each section so the kick gradually evaporates as
  // the music gets denser. Climax stays full so the peak still lands.
  const sectionThinMax = { statement: 0.40, develop: 0.55, bridge: 0.50, climax: 0.00, coda: 0.0, overture: 0 };
  for (let b = 0; b < sec.bars; b++) {
    const tBar = (bar + b) * SPBAR;
    const dr = sec.drive;
    const lastBar = b === sec.bars - 1;
    const thin = ULTIMATE
      ? (b / Math.max(1, sec.bars - 1)) * (sectionThinMax[sec.name] || 0)
      : 0;
    if (sec.kick === "halftime" || sec.kick === "halfhard") {
      for (const beat of [0, 2]) {                       // beats 1 + 3
        const tk = tBar + beat * SPB + hum(0.003);
        kick(tk, dr, 1, thin); kickEvents.push({ t: +tk.toFixed(4) });
      }
      // ULTIMATE: a syncopated push-kick on the "and-of-3" once per 4-bar
      // phrase in driven non-climax sections — breaks the strict 1+3 hole
      // pattern so the rhythm leans forward into the next bar.
      if (ULTIMATE && b % 4 === 3 &&
          (sec.name === "statement" || sec.name === "develop")) {
        const tk = tBar + 2.5 * SPB + hum(0.003);
        kick(tk, dr * 0.55, 0.55, thin); kickEvents.push({ t: +tk.toFixed(4) });
      }
      if (sec.kick === "halfhard" && b % 2 === 1) {      // climax: ghost on every other bar
        const tk = tBar + 3.5 * SPB + hum(0.003);
        kick(tk, dr * 0.7, 0.7, thin); kickEvents.push({ t: +tk.toFixed(4) });
      }
      if (lastBar && sec.name === "develop") {           // pre-climax: 4 kicks across the last bar (was 16th roll)
        for (let r = 0; r < 4; r++) {
          const tk = tBar + r * SPB;
          kick(tk, dr * (0.65 + r * 0.10), 0.85, thin);
          kickEvents.push({ t: +tk.toFixed(4) });
        }
      }
    } else if (sec.kick === "pulse") {                   // study zone: halftime (same as default now)
      for (const beat of [0, 2]) {
        const tk = tBar + beat * SPB; kick(tk, dr, 0.85, thin);
        kickEvents.push({ t: +tk.toFixed(4) });
      }
    } else if (sec.kick === "fade") {
      // CODA KICK EVOLUTION — bars 0-2: REVERSE-FEEL swell (slow-attack sub
      // on chord root, builds INTO the downbeat like a backwards kick).
      // Bars 3-5: lerp swell → forward kick (both at lowering gain).
      // Bars 6+: punchy SINE-CHORDAL kick (kick body + root/3rd/5th
      // sub stack) — a clean dance kick with the chord baked in.
      const ch = CHORD[cyc(sec.chords, b)];
      const chRoot = ch.root + tr;
      const tk = tBar;
      if (b < 3) {
        // Reverse swell — slow-attack sub, releases on the downbeat
        write(tk, (lt) => {
          const dur = SPBAR * 0.95;
          const env = Math.min(1, Math.pow(lt / dur, 1.4));        // backward-feel ramp
          const f = m2f(chRoot - 12);
          const v = Math.sin(TAU * f * lt) * env * 0.40;
          return [v, v];
        }, SPBAR);
      } else if (b < 6) {
        // Blend: swell quieter + forward kick fading in
        const blend = (b - 3) / 3;
        write(tk, (lt) => {
          const dur = SPBAR * 0.85;
          const env = Math.min(1, Math.pow(lt / dur, 1.4)) * (1 - blend);
          const f = m2f(chRoot - 12);
          const v = Math.sin(TAU * f * lt) * env * 0.35;
          return [v, v];
        }, SPBAR);
        kick(tk, dr * 0.85, 0.55 * blend, 0.40);
        kickEvents.push({ t: +tk.toFixed(4) });
      } else if (b < 13) {
        // Punchy sine-chordal kick — kick body + root + 3rd + 5th subs
        kick(tk, dr * 1.15, 0.90, 0.30);
        sub(tk, 0.32, chRoot - 12, 0.50);                   // root
        sub(tk, 0.22, chRoot - 12 + ch.q[1], 0.32);         // 3rd
        sub(tk, 0.22, chRoot - 12 + ch.q[2], 0.26);         // 5th
        kickEvents.push({ t: +tk.toFixed(4) });
      } else if (b === 13) {
        // Break-apart starts — kicks fragment to 4 staggered, pitch-drifting
        for (let s = 0; s < 4; s++) {
          const ts = tk + s * (SPBAR / 4);
          const pitchDrop = s * 2;                          // 2 semis lower per fragment
          kick(ts, dr * (1 - s * 0.18), 0.75 - s * 0.13, 0.45);
          sub(ts, 0.20, chRoot - 12 - pitchDrop, 0.40 * (1 - s * 0.20));
          kickEvents.push({ t: +ts.toFixed(4) });
        }
      } else if (b === 14) {
        // Stutter — 6 fast micro-kicks at irregular subdivisions
        const subdivs = [0, 0.18, 0.31, 0.52, 0.71, 0.88];
        for (let s = 0; s < subdivs.length; s++) {
          const ts = tk + subdivs[s] * SPBAR;
          const drop = s * 1.5;
          kick(ts, dr * (0.85 - s * 0.10), 0.55 - s * 0.07, 0.55);
          sub(ts, 0.16, chRoot - 12 - drop, 0.30 * (1 - s * 0.12));
          kickEvents.push({ t: +ts.toFixed(4) });
        }
      } else {
        // Final bar (b=15): one long sub-swell pitching DOWN to silence
        write(tk, (lt) => {
          const dur = SPBAR * 1.4;
          const env = Math.exp(-lt / (dur * 0.55));
          const pitchDriftSemis = -lt * 6;                  // 6 semis/sec down
          const f = m2f(chRoot - 12 + pitchDriftSemis);
          const v = Math.sin(TAU * f * lt) * env * 0.55;
          return [v, v];
        }, SPBAR * 1.4);
      }
    }
    // perc tick — also widened: 8th-positions 1 + 5 only (the "and-of-1" and "and-of-3")
    // so the hi-hats mirror the halftime kicks instead of fighting them.
    // Coda now keeps the ticks going (was excluded) but linearly tapers
    // them across its 16 bars so they don't fall off the cliff at climax
    // → coda. @jeffrey wanted to "keep some of that going" at master 2:28.
    if (sec.kick !== "none") {
      const bridgeSparse = sec.name === "bridge";
      const ptier = Math.floor(b / 8);                          // perc fades in by phase
      // Coda taper: 1.0 at bar 0, 0.15 at bar 15. Other sections unchanged.
      const codaFade = sec.name === "coda"
        ? Math.max(0.15, 1.0 - 0.85 * (b / Math.max(1, sec.bars - 1)))
        : 1.0;
      // ULTIMATE drops the closed-hi-hat clicks at the top of statement
      // — placeholder space for the rattle-intro sample (see THE LAW
      // amendment below). The open hat + the and-of-3 still come in at
      // ptier >= 1 so the groove fills out by bar 8.
      const tickEarly = !(ULTIMATE && sec.name === "statement" && b < 8);
      if (tickEarly) tick(tBar + 1 * (SPB / 2), (bridgeSparse ? 0.14 : 0.20) * codaFade);    // and-of-1
      if (ptier >= 1) tick(tBar + 5 * (SPB / 2), (bridgeSparse ? 0.14 : 0.20) * codaFade);   // and-of-3 IN
      // OPEN HATS — playful additions: bumped gain so they actually read,
      // plus a SECOND open hat on the and-of-2 every other bar for
      // syncopated lift. Develop + climax get a THIRD on the and-of-4
      // for a busy hi-hat texture. Coda keeps open hats but quieter.
      if (!bridgeSparse && (ptier >= 1 || sec.name === "coda")) {
        tick(tBar + 3.5 * SPB, 0.42 * codaFade, true);                       // open hat on and-of-3
        if (b % 2 === 0) tick(tBar + 1.5 * SPB, 0.32 * codaFade, true);      // open hat on and-of-2, every other bar
        if (sec.name === "develop" || sec.name === "climax") {
          tick(tBar + 7.5 * SPB, 0.28, true);                                // open hat on and-of-4 in driven sections
        }
      }
      // ULTIMATE polyrhythm — 3-against-4 wood-block sine blips per bar
      // in the dense driven sections. Wood-block pitch (1.9 kHz) keeps
      // it OUT of the 7.4-9.2 kHz hat band; fades in over 4 bars from
      // bar 8 so it doesn't slam on alongside the open-hat entrance.
      if (ULTIMATE && (sec.name === "develop" || sec.name === "climax")) {
        const polyG = Math.max(0, Math.min(1, (b - 8) / 4)) * 0.12;
        if (polyG > 0.006) {
          for (let p = 0; p < 3; p++) {
            woodTick(tBar + (p * 4 / 3) * SPB + hum(0.003), polyG);
          }
        }
      }
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

  // ULTIMATE notification-bell pings — high "dingsszzz" on accent beats
  // through the brass-driven sections. One bell on beat 1 of every 2nd
  // bar, at chord root + 4 octaves (root vs 5th alternating), pan flips
  // L/R, 4.5 s decay tau. With 80% wet send to the reverb hall they
  // pile up into a perpetual ping carpet above the melody.
  if (ULTIMATE && (sec.theme === "brass" || sec.name === "bridge" || sec.name === "develop")) {
    for (let bb = 0; bb < sec.bars; bb += 2) {
      // Skip the high bell pings during climax loop 1 (bars 8-15,
      // master 2:00-2:10) — the same breathing window the brass loop
      // strips back. Bells return for loop 2.
      if (sec.name === "climax" && bb >= 8 && bb < 16) continue;
      const tBar = (bar + bb) * SPBAR;
      const ch2 = CHORD[cyc(sec.chords, bb)];
      const root2 = ch2.root + tr;
      const offset = (bb % 4 === 0) ? 48 : 48 + ch2.q[2];
      const midiBell = root2 + offset;
      if (midiBell < 120) {
        const bellG = sec.name === "climax" ? 0.072 : 0.058;
        const panBase = ((bb / 2) % 2 === 0) ? -0.30 : 0.30;
        bell(tBar + hum(0.006), 4 * SPB, midiBell, bellG,
          { pan: panBase, decTau: 4.5, atk: 0.020, wetSend: 0.85 });
        // CHORD MODE — from engine time ≥ 75 s (master ≈ 1:15) the high
        // bells stop being single pings and pile into a chord: root + 5th
        // above, with a few-ms stagger so the strike spreads horizontally
        // and the 5th sparkles a hair later than the root. A discreet
        // octave-up ping on every 4th bar adds top-air without piling.
        if (tBar >= 75.0) {
          const fifth = midiBell + 7;
          if (fifth < 120) {
            bell(tBar + 0.014 + hum(0.004), 4 * SPB, fifth, bellG * 0.78,
              { pan: -panBase * 0.60, decTau: 4.0, atk: 0.022, wetSend: 0.85 });
          }
          if ((bb % 4) === 0 && midiBell + 12 < 124) {
            bell(tBar + 0.028 + hum(0.004), 4 * SPB, midiBell + 12, bellG * 0.42,
              { pan: panBase * 0.35, decTau: 3.0, atk: 0.018, wetSend: 0.90 });
          }
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
        // ULTIMATE: trim partials 5/6/7 (the 2–6 kHz "sizzle" that reads
        // as buzz on a long-form listen) — the melody pushes via gain,
        // not via upper-mid harmonic energy.
        const bParts = ULTIMATE
          ? [[1, 1], [2, 0.6], [3, 0.42], [4, 0.24], [5, 0.13], [6, 0.05], [7, 0.02]]
          : [[1, 1], [2, 0.6], [3, 0.45], [4, 0.28], [5, 0.2], [6, 0.12], [7, 0.07]];
        voice(tN + humEager(0.022), dur, midi, baseGain, {
          atk: 0.018, rel: 0.10, vibR: 5.6, vibD: 0.007, drive: 1.25,
          parts: bParts });
        voice(tN + hum(0.004), dur, midi - 12, baseGain * 0.5, {
          atk: 0.03, rel: 0.12, parts: [[1, 1], [2, 0.4], [3, 0.2]] }); // oct double
      } else {
        voice(tN + humEager(0.022), dur, midi, baseGain, {
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
      voice(tN + humEager(0.026), beats * SPB * 0.98, ROOT_MEL + off + tr, baseGain, {
        atk: 0.045, rel: 0.18, vibR: 4.8, vibD: 0.006, drive: 1.05, pan: -0.28,
        parts: [[1, 1], [2, 0.55], [3, 0.3], [4, 0.14], [5, 0.06]] });
      beatPos += beats;
    }
  };
  const sectionStartT = startSec;

  if (sec.theme === "soft") {
    // ULTIMATE: phase the soft theme to enter at bar 6 (phase 3) so the
    // opening is dry percussion → organs → theme + reverb.
    const themeT0 = ULTIMATE ? sectionStartT + 6 * SPBAR : sectionStartT;
    layTheme((ULTIMATE ? ultThemeAt(sec.name) : THEME_V).slice(0, 11), 0.085, false, 0, themeT0);
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
      // ULTIMATE lifts every melodic gain — the theme and counter are
      // what the listener should chase, not the buzz/drive bed.
      // Lead + counter pumped — the sample layers (crow, splash, crowd)
      // were overshadowing the actual melodic content. Theme up ~55%,
      // counter up ~60% so the harmony reads clearly through the mix.
      const meldG0 = ULTIMATE ? 0.34  : 0.27;
      const meldG1 = ULTIMATE ? 0.30  : 0.24;
      const cntG   = ULTIMATE ? 0.21  : 0.17;
      const canG   = ULTIMATE ? 0.21  : 0.17;
      const sprG   = ULTIMATE ? 0.10  : 0.082;
      // CLIMAX LOOP 1 BREATHING ROOM — @jeffrey: "2:00-2:20 mix way too
      // busy, drop out + bring others in, create more space". For the
      // 8 bars covering master 2:00-2:10 (climax loop 1), strip back
      // to JUST the theme — no octave-up counter shadow, no saw lead,
      // and a quieter primary counter. The full counterpoint stack
      // returns for loop 2 (master 2:10-2:20) and the saw lead returns
      // with it, so the section reads as: tutti → tacet (loop 1) →
      // tutti (loop 2) instead of one flat wall.
      const climaxBreathe = sec.name === "climax" && ULTIMATE && lp === 1;
      layTheme(tv, lp === 0 ? meldG0 : meldG1, true, 0, t0);
      if (sec.name === "climax" && ULTIMATE) {
        const cntScale = climaxBreathe ? 0.55 : 1.55;
        layCounter(COUNTER_V, cntG * cntScale, t0);
        if (!climaxBreathe) {
          // octave-up counter shadow — only when NOT in the breathing
          // loop, otherwise the texture re-thickens immediately.
          const counterUp = COUNTER_V.map(([off, beats]) => [off + 12, beats]);
          layCounter(counterUp, cntG * 0.85, t0);
        }
      } else if (canon) layTheme(tv, canG, true, -12, t0 + SPBAR); // octave-down canon, 1 bar late
      else if (lp >= 1) layCounter(COUNTER_V, cntG, t0);    // counter IN on the restatement
      if (lp >= 2 && !canon) layTheme(tv, sprG, true, 12, t0); // octave sparkle, last pass
      // ULTIMATE power-saw lead — trance flash doubling the leitmotif
      // in statement loops 1+ and the climax. 16th-note gate (82 ms at
      // 182 BPM) for the "flashing through" stutter, octave-stacked.
      // SKIPPED during climax loop 1 (master 2:00-2:10) to give the
      // section breathing room — returns for loop 2 (2:10-2:20).
      if (ULTIMATE && lp >= 1 && !climaxBreathe &&
          (sec.name === "statement" || sec.name === "climax")) {
        // Climax goes MAXIMAL — saw lead nearly doubled vs statement.
        const sawG = sec.name === "climax" ? 0.175 : 0.080;
        let bp = 0;
        for (const [off, beats] of tv) {
          const tN = t0 + bp * SPB;
          const midi = ROOT_MEL + off + tr;
          const dur = beats * SPB * 0.96;
          sawLead(tN, dur, midi, sawG,
            { atk: 0.008, rel: 0.05, gateMs: 82, gateOnFrac: 0.55, drive: 0.85 });
          sawLead(tN, dur, midi + 12, sawG * 0.5,
            { atk: 0.010, rel: 0.05, gateMs: 82, gateOnFrac: 0.55, detune: 0.009 });
          bp += beats;
        }
      }
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
        // ULTIMATE skips the hoover doubling — its rising FM index is the
        // single biggest "buzz" source in develop, and the fragmented
        // brass + polyrhythm already carry the build.
        if (seg >= 4 && !ULTIMATE) hoover(t0 + bp * SPB, beats * SPB * 0.8, midi - 12, 0.13);
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
      stab(tBar + 1.5 * SPB, ch.root + tr + 24, ULTIMATE ? 0.18 : 0.30);
      stab(tBar + 3.5 * SPB, ch.root + tr + 24 + 7, ULTIMATE ? 0.16 : 0.26);
    }
  }
  bar += sec.bars;
}

// ── grenade KICK — every 8th kick gets a grenade.wav layered on top so
// it lands like a bomb. Soft enough that it doesn't drown the kick body;
// loud enough to push the down-feel each cycle. Uses the same Boom
// Explosion sample loaded at t=12.9.
const GRENADE_KICK_PATH = resolve(HERE, "../samples/grenade.wav");
if (existsSync(GRENADE_KICK_PATH) && kickEvents.length > 0) {
  const grenBuf = loadWavMono(GRENADE_KICK_PATH);
  let gkCount = 0;
  for (let i = 0; i < kickEvents.length; i++) {
    if (i % 8 !== 7) continue;             // every 8th (0-indexed: 7, 15, 23, …)
    const kt = kickEvents[i].t;
    playSample(kt, grenBuf, 0.42, { rate: 0.92, pan: 0, wetSend: 0.20, fade: 0.005 });
    gkCount++;
  }
  console.log(`→ grenade-kick · ${gkCount} layered every-8th-kick`);
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

// ── water — ULTIMATE only, optional. Replaces the cut overture steam.
// If pop/hellsine/samples/water.wav exists (drop in a freesound water
// spray / spigot recording), it plays under the overture as the ambient
// bed. Absent the file, the overture stays cleanly dry. THE LAW: same
// amendment posture as the rattle — one organic layer riding the engine.
const WATER_PATH = flags["water-path"] || resolve(HERE, "../samples/water.wav");
const WATER_GAIN = Number(flags["water-gain"] ?? 0.35);
if (ULTIMATE && existsSync(WATER_PATH)) {
  const water = loadWavMono(WATER_PATH);
  const overture = sectionRanges.find((s) => s.name === "overture");
  if (overture) {
    const wDur = water.length / SR;
    const tgt = overture.endSec - overture.startSec;
    // Play once, time-stretched via linear-interp rate, with long fades
    // so it dissolves cleanly into the chord wash entering at statement.
    playSample(overture.startSec, water, WATER_GAIN, {
      rate: wDur / tgt, pan: 0, fade: 1.2,
    });
    console.log(`→ water · ${wDur.toFixed(2)}s → ${tgt.toFixed(2)}s overture · sampled (THE LAW amended)`);
  }
} else if (ULTIMATE) {
  console.log(`· water · no sample at ${WATER_PATH.replace(process.env.HOME, "~")} — overture dry`);
}

// ── drum break — ULTIMATE only. The recorded drums + scratches read as
// BREAK material (chopped one-shots), not as on-grid groove. So they
// only appear in the intro break + at section transitions. Each drum is
// paired with a scratch that lands directly after the drum body — the
// pair reads as a single chopped "couplet" event.
if (ULTIMATE) {
  const DRUM_FILES = {
    hit1:  "drum-1",          hit2:  "drum-2",
    low1:  "drum-1-low",      low2:  "drum-2-low",
    scrA1: "drum-1-scratch-a", scrB1: "drum-1-scratch-b",
    scrA2: "drum-2-scratch-a", scrB2: "drum-2-scratch-b",
  };
  const drums = {};
  for (const [key, name] of Object.entries(DRUM_FILES)) {
    const p = resolve(HERE, "../samples/" + name + ".wav");
    if (existsSync(p)) drums[key] = loadWavMono(p);
  }
  const drumCount = Object.keys(drums).length;
  const DING_PATH = resolve(HERE, "../samples/imessage-ding.wav");
  const dingBuf = existsSync(DING_PATH) ? loadWavMono(DING_PATH) : null;
  // Cards — @jeffrey's recorded fingers-through-a-deck flaps (wave-wizard,
  // spec at samples/wave-wizard-cards.json). 6 takes shaped by the analyzer
  // for placement: cards-slow (4.76 s, 13 evenly-spaced clicks, zcr 3318 —
  // darkest, perfect for distant background) and cards-medium (3.95 s, 4
  // sparse hits, zcr 7408) → overture distant-birdies layer; cards-hard
  // (0.26 s snap), cards-burst-a (15 chops, zcr 8348), cards-burst-b (35
  // chops, zcr 9365 — brightest) → wall-of-sound 3rd perc layer. The high-
  // crest takes (≥31 dB) sit between the sine-engine's sustained voices
  // and the kick's hole transients without masking either.
  const CARD_NAMES = ["cards-fast", "cards-medium", "cards-slow", "cards-hard", "cards-burst-a", "cards-burst-b"];
  const cards = {};
  for (const n of CARD_NAMES) {
    const p = resolve(HERE, "../samples/" + n + ".wav");
    if (existsSync(p)) cards[n] = loadWavMono(p);
  }
  const cardCount = Object.keys(cards).length;

  // ── overture stays CLEAN — drum + scratch couplets gutted (@jeffrey:
  // no clicky perc through the first ~16 s). The drums + scratches all
  // return AT THE DROP as the WALL OF SOUND slam (see statement block
  // below). Only the Tri-Tone ding survives in here — it's glitch-
  // stuttered into the first kick of statement as the beat-drop anchor.
  if (drumCount > 0) {
    let placed = 0;
    if (dingBuf) {
      // Tri-Tone as the BEAT-DROP — glitch stutters then a half-speed
      // main playthrough + 3 decaying echo taps, anchored exactly to
      // the first kick of statement so the Tri-Tone "drops" with the beat.
      const stSec = sectionRanges.find((s) => s.name === "statement")?.startSec || 0;
      // Tri-Tone glitches LEAD INTO the drop — 4 stutters before stSec,
      // so the MAIN ding lands EXACTLY on the drop beat (not a beat late).
      const glitchN = 4, glitchStep = 0.075;
      for (let g = 0; g < glitchN; g++) {
        const tg = stSec - (glitchN - g) * glitchStep;     // -0.30, -0.225, -0.15, -0.075
        playSampleSwept(tg, dingBuf, 0.70, {
          startRate: 0.70, endRate: 0.70,
          maxDurMs: 80,
          pan: g % 2 === 0 ? -0.25 : 0.25,
          wetSend: 0.45,
        });
      }
      const mainStart = stSec;        // MAIN ding ON the drop
      playSample(mainStart, dingBuf, 1.10,
        { rate: 0.5, pan: 0.0, wetSend: 0.55 });
      const echoes = [
        { dt: 0.42, g: 0.34, pan: -0.45 },
        { dt: 0.86, g: 0.21, pan:  0.45 },
        { dt: 1.42, g: 0.13, pan: -0.30 },
      ];
      for (const e of echoes) {
        playSample(mainStart + e.dt, dingBuf, 0.55 * e.g,
          { rate: 0.5, pan: e.pan, wetSend: 0.85 });
      }
      placed += 1 + glitchN + echoes.length;
    }
    console.log(`→ overture clean · ${placed} drop-stamps${dingBuf ? " (Tri-Tone)" : ""}`);
  }

  // ── MELON STAB + SQUISH SWEETENER — wet sound-design layer.
  // Butcher-knife-into-watermelon (#464626) lands on the splash impact at
  // t=3 for a visceral edge on the cannonball. The longer spooky-squish
  // sweetener (#365167) drapes across the bubble field as a wet textural
  // bed so the outro bubbles feel like they're inside something.
  const STAB_PATH   = resolve(HERE, "../samples/melon-stab.wav");
  const SQUISH_PATH = resolve(HERE, "../samples/squish-sweetener.wav");
  if (existsSync(STAB_PATH)) {
    const stab = loadWavMono(STAB_PATH);
    // Lands right after the last typewriter key (t=2.34s + ~0.27s sample) —
    // gives the intro a "knife-slice-on-the-paper" punctuation right
    // before the splash begins to rush in.
    playSample(2.45, stab, 0.85, { rate: 1.0, pan: 0.10, wetSend: 0.85, fade: 0.003 });
    console.log(`→ melon stab at 2.45s w/ heavy reverb tail (space + depth)`);
  }
  // squish sweetener pulled too — the splashy/squelchy textures at end were
  // reading repetitive. The melon stab at 2.65s is the only wet sound left.

  // ── TAPE NOISE BED — analog-tape-warmth hiss across the first 24 s,
  // fading from full intro warmth into the post-drop mix. Built from
  // two filtered noise streams: a brighter "tape hiss" + a lower "warm
  // room rumble" + slow LFO "breathing" + tiny stereo decorrelation
  // (left + right use slightly different filter states) so it reads
  // wide and analog, not stamped-down mono.
  {
    const bedDur = 24.0;
    const noiseEndI = Math.min(N, Math.floor(bedDur * SR));
    let n1L=0,n2L=0,n3L=0, n1R=0,n2R=0,n3R=0;
    let hL=0, hR=0;                        // tape-hiss highpass states
    for (let i = 0; i < noiseEndI; i++) {
      // pink-ish warm rumble (cascaded LPs)
      const wL = rng() * 2 - 1, wR = rng() * 2 - 1;
      n1L = 0.99*n1L + 0.01*wL; n2L = 0.96*n2L + 0.04*n1L; n3L = 0.86*n3L + 0.14*n2L;
      n1R = 0.99*n1R + 0.01*wR; n2R = 0.96*n2R + 0.04*n1R; n3R = 0.86*n3R + 0.14*n2R;
      const warmL = n3L * 6, warmR = n3R * 6;
      // tape-hiss high band via single-pole highpass
      const hssL = wL - hL; hL = hL * 0.85 + wL * 0.15;
      const hssR = wR - hR; hR = hR * 0.85 + wR * 0.15;
      const t = i / SR;
      // Envelope: full intro warmth from 0-15.82, gentle taper to 0 by 24s
      let env;
      if (t < 2.0)        env = t / 2.0;                       // 2s fade-in
      else if (t < 15.82) env = 1.0;                            // full through drop
      else                env = Math.max(0, 1 - (t - 15.82) / 8.18);  // 8.18s taper to 0
      const breathe = 0.88 + 0.12 * Math.sin(TAU * 0.11 * t);
      // Halved hiss gain — was too prominent on headphones (was 0.034/0.012).
      const vL = (warmL * 0.017 + hssL * 0.005) * env * breathe;
      const vR = (warmR * 0.017 + hssR * 0.005) * env * breathe;
      L[i]  += vL;
      Rb[i] += vR;
      WL[i] += vL * 0.30;
      WR[i] += vR * 0.30;
    }
  }

  // ── TYPEWRITER KEYS — first two bars (0 to ~2.64s) get a handful of
  // mechanical punch-key transients. Picks up the "starting the document"
  // feel before everything else rushes in.
  const TYPER_PATH = resolve(HERE, "../samples/typewriter-key.wav");
  if (existsSync(TYPER_PATH)) {
    const typer = loadWavMono(TYPER_PATH);
    // [t, rate, gain, pan]
    const keys = [
      [0.18, 1.00, 0.65, -0.30],
      [0.42, 1.08, 0.60,  0.35],
      [0.78, 0.96, 0.62, -0.20],
      [1.12, 1.04, 0.58,  0.30],
      [1.55, 1.10, 0.55, -0.40],
      [1.92, 0.98, 0.52,  0.25],
      [2.34, 1.06, 0.48,  0.00],
    ];
    // FADE-IN to loudness — first punch enters at 18% of its written gain
    // and ramps up linearly across the seven keys. The opening "starting
    // the document" gesture should creep in, not slap; the existing gains
    // describe the destination, not the entrance.
    for (let k = 0; k < keys.length; k++) {
      const [t, rate, g, pan] = keys[k];
      const fadeIn = 0.18 + (1 - 0.18) * (k / (keys.length - 1));
      const tJ = t + hum(0.012);
      playSample(tJ, typer, g * fadeIn, { rate, pan, wetSend: 0.25, fade: 0.003 });
      // VERY soft sub pulse under each typewriter punch — just a hint of
      // depth, not a kick. The previous kick+sub stack was way too big
      // for the dry overture.
      sub(tJ, 0.18, 38, 0.14 * fadeIn);   // D2 sub follows the same ramp
    }
    console.log(`→ typewriter · ${keys.length} key punches w/ soft sub thumps across first 2 bars`);
  }

  // ── splash @ t=3 — cannonball-into-drowning, very quiet but RUSHING in.
  // The 4-second declick fade ramps the sample from silent up to its
  // already-low playback gain, so the cannonball impact reads as a
  // distant whoosh growing into the foreground, peaking under the meows
  // before the drop hits. Two scratch flicks ride the underwater bed
  // between the meows. Sources: Freesound darcydunes #273834 + tyops #474624.
  const SPLASH_PATH = resolve(HERE, "../samples/splash-intro.wav");
  const splashT = 3.00;
  if (existsSync(SPLASH_PATH)) {
    const splash = loadWavMono(SPLASH_PATH);
    playSample(splashT, splash, 0.18, { rate: 1.0, pan: 0, wetSend: 0.75, fade: 4.0 });
    console.log(`→ splash rush · ${(splash.length / SR).toFixed(2)}s cannonball→drowning, quiet rush-in at ${splashT.toFixed(2)}s`);
    if (drumCount > 0) {
      const flickA = drums.scrA1 || drums.scrA2;
      const flickB = drums.scrB2 || drums.scrB1;
      if (flickA) playSample(splashT + 5.50, flickA, 0.14, { rate: 1.10, pan: -0.80, wetSend: 0.80, fade: 0.04 });
      if (flickB) playSample(splashT + 8.10, flickB, 0.12, { rate: 0.92, pan:  0.80, wetSend: 0.85, fade: 0.04 });
    }
  }

  // ── crow caw — @jeffrey: a single crow lands on a high sine-bell ping
  // in the back half of statement (38–45 s window). Snapped to the bar 32
  // bell (bb=20 → t = statement.startSec + 20·SPBAR), pan matches the
  // bell's -0.30 L, heavy wet send so the caw blooms with the same hall
  // tail. Source: Freesound acclivity #20224 (carrion crow, CC).
  const CROW_PATH = resolve(HERE, "../samples/crow.wav");
  const statementSecForCrow = sectionRanges.find((s) => s.name === "statement");
  if (existsSync(CROW_PATH) && statementSecForCrow) {
    const crow = loadWavMono(CROW_PATH);
    const crowT = statementSecForCrow.startSec + 20 * SPBAR;
    playSample(crowT, crow, 0.58, { rate: 1.0, pan: 0.00, wetSend: 0.65, fade: 0.05 });
    // Bones — cards-bursts + rattle-intro clatter the next two bars after
    // the crow, so it reads like the bird dropped a pile of bones from
    // its perch. Cards = brittle bone-flick texture, rattle = sticks.
    let boneCount = 0;
    const bursts = [];
    if (cardCount > 0) {
      if (cards["cards-burst-a"]) bursts.push({ buf: cards["cards-burst-a"], dt: 0.45, g: 0.18, pan:  0.55, rate: 1.00, wet: 0.70 });
      if (cards["cards-hard"])    bursts.push({ buf: cards["cards-hard"],    dt: 0.95, g: 0.22, pan: -0.45, rate: 1.10, wet: 0.65 });
      if (cards["cards-burst-b"]) bursts.push({ buf: cards["cards-burst-b"], dt: 1.55, g: 0.16, pan:  0.40, rate: 0.92, wet: 0.75 });
      if (cards["cards-hard"])    bursts.push({ buf: cards["cards-hard"],    dt: 2.20, g: 0.14, pan: -0.55, rate: 1.20, wet: 0.80 });
    }
    for (const b of bursts) {
      playSample(crowT + b.dt, b.buf, b.g, { rate: b.rate, pan: b.pan, wetSend: b.wet, fade: 0.03 });
      boneCount++;
    }
    const RATTLE_BONES_PATH = resolve(HERE, "../samples/rattle-intro.wav");
    if (existsSync(RATTLE_BONES_PATH)) {
      const rb = loadWavMono(RATTLE_BONES_PATH);
      playSample(crowT + 0.65, rb, 0.22, { rate: 1.05, pan:  0.15, wetSend: 0.70, fade: 0.05 });
      playSample(crowT + 1.80, rb, 0.18, { rate: 0.88, pan: -0.25, wetSend: 0.80, fade: 0.05 });
      boneCount += 2;
    }
    // Crow PERC BREAK — 6 bars (bb=18..23) with the FULL caw revealed at
    // bb=20. The two bars before the caw (bb=18, 19) are mystery chops
    // sourced from caws 2-4 at varied rates so the listener can't tell
    // it's a crow yet — they sound like distorted vinyl scratches. The
    // reveal at bb=20 (42.20 s) recontextualizes the intro chops. The
    // three bars after (bb=21, 22, 23) explode into Fibonacci-gap 16ths:
    // gaps go 1,2,3,5,8,13 (expansion) then 13,8,5,3,2,1 (contraction)
    // so the scratch pattern arcs out and back — golden-spiral feel on
    // a 16th grid. Each chop reads a different caw via bufOffset.
    const SP16 = SPB / 4;                     // 16th-note step
    const C1 = 0.10, C2 = 0.85, C3 = 1.55, C4 = 2.30, C5 = 3.05;

    // INTRO CHOPS (mystery, before the caw) — bars bb=18, 19 → 39.57 to 42.20s.
    // Lower gain (≤ 0.85) + heavily pitched so they don't sound like a crow yet.
    const introChops = [
      // [barRelToCaw, i16, cawOff, startRate, endRate, durMs, gain, pan]
      [-2,  4, C2, 1.85, 1.50, 130, 0.75, -0.55],   // pitched-up sliver
      [-2, 10, C3, 0.55, 0.55, 240, 0.80,  0.50],   // slow drag low-pitch
      [-2, 14, C4, 1.70, 1.20, 110, 0.75,  0.30],
      [-1,  2, C5, 0.45, 0.45, 280, 0.82, -0.45],   // even slower drag
      [-1,  8, C2, 2.00, 1.40, 100, 0.78,  0.55],
      [-1, 12, C3, 0.70, 0.50, 230, 0.85, -0.30],   // pitch-down hint
    ];

    // FIBONACCI scratch block — bb=21, 22, 23 (3 bars after the caw).
    // Gap sequence in 16ths (expansion → contraction): 1,2,3,5,8,13,13,8,5,3,2,1.
    // Sum = 64 sixteenths = exactly 4 bars; we cap chops to bb=23 (3 bars
    // → 48 sixteenths) and roll the leftover off the edge into bridge.
    const FIB_GAPS = [1, 2, 3, 5, 8, 13, 13, 8, 5, 3, 2, 1];
    const caws = [C5, C4, C3, C2, C1, C2, C3, C4, C5, C4, C3, C5];
    const fibPattern = [];
    let stepCursor = 0;
    for (let i = 0; i < FIB_GAPS.length; i++) {
      const gap = FIB_GAPS[i];
      stepCursor += i === 0 ? 0 : FIB_GAPS[i - 1];
      const barOff = 1 + Math.floor(stepCursor / 16);
      const i16    = stepCursor % 16;
      if (barOff > 3) break;
      // Big gaps → slow drag chops, small gaps → tight chops
      const isBigDrag = gap >= 8;
      const isSmallChop = gap <= 2;
      const rate = isBigDrag ? 0.45 : isSmallChop ? 1.55 : 1.00;
      const ms   = isBigDrag ? 360 : isSmallChop ? 130 : 200;
      const g    = isBigDrag ? 1.35 : 1.25;
      const pan  = (i % 2 === 0) ? -0.50 : 0.50;
      fibPattern.push([barOff, i16, caws[i], rate, rate, ms, g, pan]);
    }
    // Final screwed-down crawl at bb=23 end → pitch-down into bridge
    fibPattern.push([3, 14, C5, 0.95, 0.18, 720, 1.40, 0.00]);

    // BRIDGE EXTENSION — keep the scratching going through the bridge as
    // a sparser Fibonacci pass (gain reduced so the soft B-theme breathes).
    // Bars bb=4..11 (in caw-relative bar numbers) = bridge bars 0..7.
    // The pattern thins out approaching t=60s so the crowd entrance lands
    // on a clean swell, not a chop-cluttered surface.
    // BRIDGE → CROWD blend — chops here serve as the swishy connective
    // tissue between the crow break and the stadium crowd that enters
    // around t=53s. Slow rates (textural), longer chops, MUCH heavier wet
    // send so the reverb tail fuses with the crowd's onset.
    const BRIDGE_FIB = [3, 5, 8, 13, 21, 13, 8, 5];
    const bridgeCaws = [C1, C3, C5, C4, C2, C3, C5, C4];
    let bcursor = 0;
    let bridgeCount = 0;
    for (let i = 0; i < BRIDGE_FIB.length; i++) {
      const gap = BRIDGE_FIB[i];
      bcursor += i === 0 ? 0 : BRIDGE_FIB[i - 1];
      const barOff = 4 + Math.floor(bcursor / 16);
      const i16    = bcursor % 16;
      if (barOff > 12) break;
      const isBig = gap >= 13;
      // All bridge chops slow → textural swish blending with crowd
      const rate  = isBig ? 0.35 : 0.65;
      const ms    = isBig ? 520  : 320;
      const g     = isBig ? 0.95 : 0.75;
      const pan   = (i % 2 === 0) ? -0.65 : 0.65;
      fibPattern.push([barOff, i16, bridgeCaws[i], rate, rate, ms, g, pan]);
      bridgeCount++;
    }

    const breakChops = [...introChops, ...fibPattern];
    let breakCount = 0;
    // Crow chops globally scaled DOWN (gain × 0.55) and LONGER (ms × 1.5)
    // — earlier render had the chops dominating the mix. Now they sit
    // back, ring longer, and bleed naturally into the crowd entrance at
    // t=60 s. Wet send pushed higher so they reverb-tail across bars.
    for (const ev of breakChops) {
      const [barOff, i16, cawOff, sr, er, ms, g, pan] = ev;
      const t = crowT + barOff * SPBAR + i16 * SP16 + hum(0.005);
      playSampleSwept(t, crow, g * 0.65, {
        startRate: sr, endRate: er,
        maxDurMs: Math.round(ms * 1.5),
        pan, wetSend: 0.75,
        bufOffset: cawOff, fade: 0.018,
      });
      breakCount++;
    }
    console.log(`→ crow break · ${introChops.length} intro chops · 1 caw reveal · ${fibPattern.length} Fibonacci-gap 16ths · ${breakCount} total chops across 12 bars (into bridge)`);
  }

  // ── TTS SINGING MANTRA — alternating macOS voices sing "i hope that you
  // get all the mo-ney you want" continuously, back-to-back, from statement
  // start through the bridge/develop until the climax (chorus). 7 voice
  // tracks rotate so each pass feels like a new singer in a relay choir.
  // Generator: bin/gen-tts-singing.mjs (run once to render the 7 WAVs).
  const stForTTS = sectionRanges.find((s) => s.name === "statement")?.startSec ?? 15.82;
  // End vocals a few bars BEFORE the AC stamp (climax.startSec - 3.5s),
  // so the crew goes quiet before the "aesthetic dot computer" announcement.
  const acStampSec = (sectionRanges.find((s) => s.name === "climax")?.startSec ?? 110.77) - 3.5;
  // Vocals END at master 1:42 (which is also pre-master ~102s since we're
  // before the climax atempo speed-up). Earlier limit was AC stamp − 3 bars.
  const cxForTTS = Math.min(acStampSec - 3 * SPBAR, 102.0);
  const ttsVoices = ["Zarvox", "Albert", "Fred", "Alex", "Samantha", "Daniel", "Bells"];
  const ttsVariants = ["money", "honey", "bunnies"];
  // Load 4 pitch tiers per voice+variant for SATB voicing:
  //   Bass (-12), Tenor (0), Alto (+7), Soprano (+12). Pre-baked via
  //   rubberband -p N -F -c 6 (formant-preserving, no speed change).
  const ttsBufs = {};
  const ttsCombos = [];                            // tenor combos for compatibility
  const ttsTiers = { B: [], T: [], A: [], S: [] };  // keyed by SATB tier
  for (const v of ttsVoices) {
    for (const variant of ttsVariants) {
      const base = `tts-singing-${v}-${variant}`;
      const key = `${v}-${variant}`;
      const tenor = resolve(HERE, `../samples/${base}.wav`);
      const bass  = resolve(HERE, `../samples/${base}-dn12.wav`);
      const alto  = resolve(HERE, `../samples/${base}-up7.wav`);
      const sop   = resolve(HERE, `../samples/${base}-up12.wav`);
      if (existsSync(tenor)) { ttsBufs[`T:${key}`] = loadWavMono(tenor); ttsTiers.T.push(`T:${key}`); ttsCombos.push(key); }
      if (existsSync(bass))  { ttsBufs[`B:${key}`] = loadWavMono(bass);  ttsTiers.B.push(`B:${key}`); }
      if (existsSync(alto))  { ttsBufs[`A:${key}`] = loadWavMono(alto);  ttsTiers.A.push(`A:${key}`); }
      if (existsSync(sop))   { ttsBufs[`S:${key}`] = loadWavMono(sop);   ttsTiers.S.push(`S:${key}`); }
    }
  }
  if (ttsCombos.length > 0) {
    // CHOIR — scales 6 → 21 voices through the build. Enters 2 bars
    // AFTER the drop so the wall-of-sound owns the first 2 bars, then
    // the chorus floods in. Each pass is bar-locked, panned wide L↔R
    // with small pitch detunes for chorus width. Jeffrey-pvc lead is
    // layered ON TOP in the separate block below.
    // Align vocals to brass-theme LOOP boundaries (every 8 bars). The
    // brass theme plays an 8-bar antecedent+consequent loop starting at
    // sectionStart; the vocal mantra IS the antecedent melody. Vocals
    // SKIP loop 0 (the first instrumental drop) and enter on loop 1's
    // antecedent so each pass actually sings ALONG with the brass.
    const vocalDelay = 0;                                    // start ON the drop, loop 0 antecedent
    // LOOP-LOCKED CHORUS — at each 8-bar loop start, voices
    // stack at different pans (L / center / R) singing different
    // voice+variant combos simultaneously. The combo offsets per layer
    // are co-prime strides into the 21-combo pool so each bar gets a
    // fresh L/C/R trio. Slight rate detune per layer gives the layered
    // voices a real chorus-effect width.
    // 4-bar step keeps vocals continuous: even passes (0,2,4,...) align
    // with brass antecedent; odd passes (1,3,5,...) sing antecedent over
    // the consequent's brass — still in the same D-min chord progression
    // so it reads as a chord-tone harmony, not a clash. Fills the silence
    // between antecedent loops.
    const STEP = 4 * SPBAR;
    let ttsCount = 0;
    let i = 0;
    for (let t = stForTTS + vocalDelay; t < cxForTTS - 0.5; t += STEP, i++) {
      const sinceStart = t - stForTTS;
      const tilEnd = cxForTTS - t;
      // Choir pulled back HARD — @jeffrey: "still can't hear jeffrey
      // clearly". The 0.42 envG + tier-1.15/1.20 boost was masking the
      // lead because chorus and lead share the same lyric + range.
      // Now envG=0.18 + tier weights neutralized so the chorus is a
      // wash behind jeffrey, not a parallel voice fighting with him.
      let envG = 0.18;
      if (tilEnd    < 8)   envG *= Math.max(0.20, tilEnd / 8);
      const wet = Math.min(0.90, 0.70 + sinceStart * 0.003);
      const buildP = Math.max(0, Math.min(1, (t - stForTTS) / (cxForTTS - stForTTS)));

      // PROGRESSIVE SATB ARRANGEMENT — voices enter tier-by-tier as the
      // build progresses, with antiphonal alternation (even passes lean
      // tenor+alto = mid range; odd passes lean bass+soprano = extremes)
      // so the choir reads as a real conversation, not a uniform block.
      //
      // Pass schedule — @jeffrey: "i want the female right out of the
      // first drop singing!". The soprano (and alto) tiers are now
      // doubled from i=0, so the first utterance lands with female
      // voices already at full strength instead of building over passes.
      //   i=0   : T=1, A=2, S=2, B=1  (female-forward from the drop)
      //   i=1   : T=2, A=2, S=2, B=1
      //   i=2   : T=2, A=2, S=3, B=2
      //   i=3+  : alternating doublings
      const isEven = i % 2 === 0;
      let plan;
      if (i === 0)      plan = { T: 1, A: 2, S: 2, B: 1 };
      else if (i === 1) plan = { T: 2, A: 2, S: 2, B: 1 };
      else if (i === 2) plan = { T: 2, A: 2, S: 3, B: 2 };
      else if (isEven)  plan = { T: 3, A: 2, S: 2, B: 1 };
      else              plan = { T: 2, A: 2, S: 3, B: 2 };
      const totalVoices = plan.T + plan.A + plan.S + plan.B;
      const perVoiceG = envG * Math.pow(Math.max(1, totalVoices), -0.22);

      // Pan layout: bass spread WIDE L+R, tenor centered, alto inner-side, soprano centered
      const tierPans = { B: [-0.85, 0.85], T: [0.0, -0.15, 0.15], A: [-0.45, 0.45], S: [0.0, 0.10, -0.10] };
      const tierWet  = { B: wet,            T: wet + 0.05,         A: wet + 0.10,    S: wet + 0.15 };
      // Female (S + A) tiers BUMPED — @jeffrey wants the females louder.
      // S 0.75 → 1.20, A 0.90 → 1.15. Bass + tenor stay where they were.
      const tierGMul = { B: 0.85,           T: 1.00,               A: 1.15,          S: 1.20 };
      // tierDelay zeroed — all voices fire on the beat with jeffrey for
      // sync. The "bloom" effect was at the cost of alignment.
      const tierDelay = { B: 0.0,           T: 0.0,                A: 0.0,           S: 0.0 };

      for (const tier of ["B", "T", "A", "S"]) {
        const want = plan[tier];
        const pool = ttsTiers[tier];
        if (!pool.length) continue;
        for (let n = 0; n < want; n++) {
          const combo = pool[((i * 5) + n * 7) % pool.length];
          const buf = ttsBufs[combo];
          if (!buf) continue;
          const pans = tierPans[tier];
          const pan = pans[n % pans.length] + (rng() * 0.08 - 0.04);
          const g = perVoiceG * tierGMul[tier];
          // Small detune per voice for chorus thickness within the tier
          const detune = ((n - (want - 1) / 2) / Math.max(1, want - 1)) * 0.015;
          playSample(t + tierDelay[tier], buf, g, {
            rate: 1.0 + detune,
            pan: Math.max(-0.97, Math.min(0.97, pan)),
            wetSend: Math.min(0.85, tierWet[tier]),
            fade: 0.008,       // sharper attack to match jeffrey
          });
          ttsCount++;
        }
      }
    }
    console.log(`→ TTS mantra · ${ttsCount} layered voices (3 per 4-bar position) chorus-stacked L/C/R from ${stForTTS.toFixed(2)}s → ${cxForTTS.toFixed(2)}s (chorus)`);
  }

  // ── JEFFREY-PVC LEAD VOCAL (ElevenLabs) — the actual human-cloned
  // jeffrey-pvc voice singing the mantra. Lands at statement.startSec
  // ("I HOPE" on the drop). Layered ON TOP of the macOS-TTS chorus stack
  // as the LEAD vocal — the chorus reads as background harmony, the
  // jeffrey-pvc reads as the singer. Re-render via:
  //   node pop/bin/say.mjs /tmp/hellsine-mantra-lyric.txt --section hook \
  //     --out /tmp/jeffrey-mantra.mp3 --stability 0.55 --similarity 0.9 --style 0.4
  // (memory: stability ≥ 0.5 keeps voice identity).
  // jeffrey-pvc renders in 3 lyric variants matching the chorus — rotate
  // through money/honey/bunnies so the lead vocal also stays ambiguous.
  const jeffreyVariants = ["money", "honey", "bunnies"];
  // @jeffrey: words split + aligned to notes + pitch-shifted to match,
  // verified via whisper. Load the rendered jeffrey-vocal-*.wav files
  // (gen-jeffrey-whisper output: per-syllable sliced from live takes,
  // each beat-aligned + autotuned to theme − 7 semis).
  const jvocBufs = {};
  for (const variant of jeffreyVariants) {
    jvocBufs[variant] = {};
    for (const [key, suffix] of [
      ["that", ""], ["you", "-you"], ["we", "-we"],
    ]) {
      const p = resolve(HERE, `../samples/jeffrey-vocal-${variant}${suffix}.wav`);
      if (existsSync(p)) jvocBufs[variant][key] = loadWavMono(p);
    }
  }
  // FOLEY tail for each variant — fires AFTER each full mantra pass:
  //   money   → cash-register cha-ching
  //   honey   → lollipop lick
  //   bunnies → spring boing
  // Skips the teaser pass (j === 0, "that"-only — no foley after a
  // single-syllable announcement).
  const foleyPaths = {
    money:   resolve(HERE, "../samples/cash-register.wav"),
    honey:   resolve(HERE, "../samples/lollipop-lick.wav"),
    bunnies: resolve(HERE, "../samples/spring-boing.wav"),
  };
  const foleyBufs = {};
  for (const [k, p] of Object.entries(foleyPaths)) {
    if (existsSync(p)) foleyBufs[k] = loadWavMono(p);
  }
  // Foley dropped from 0.55-0.65 to 0.32-0.38 — the cha-ching / lick /
  // boing samples are LONG (1-2.5 s) and were sustaining straight into
  // the next jeffrey pass, masking it. Now they read as punctuation,
  // not as a competing layer.
  const foleyGain = { money: 0.34, honey: 0.38, bunnies: 0.32 };
  const foleyPan  = { money:  0.15, honey: -0.20, bunnies: 0.05 };
  if (Object.keys(jvocBufs).length > 0) {
    let jvCount = 0;
    let foleyCount = 0;
    // @jeffrey: "i wanna be singing right after the drop". One bar of
    // brass theme plays first, then jeffrey enters on the downbeat of
    // bar 2. All subsequent passes shift by the same amount so the
    // rotation stays bar-locked.
    const jvocDelay = SPBAR;
    for (let t = stForTTS + jvocDelay, j = 0; t < cxForTTS - 0.5; t += 4 * SPBAR, j++) {
      const variant = jeffreyVariants[j % jeffreyVariants.length];
      const bufsForVariant = jvocBufs[variant];
      if (!bufsForVariant) continue;
      // Rotate through all three phrasings (that → you → we → that…)
      // so the mantra varies pass-to-pass.
      const phrasings = ["that", "you", "we"];
      const phrasingKey = phrasings[j % phrasings.length];
      const jvoc = bufsForVariant[phrasingKey] || bufsForVariant.that
                || bufsForVariant.you || bufsForVariant.we;
      if (!jvoc) continue;
      const tilEnd = cxForTTS - t;
      // Jeffrey is the LEAD — strong but not overdriven. The 2.80 stack
      // was likely clipping the L/Rb summation so hard that the limiter
      // pumped everything (including subsequent jeffrey passes) down.
      // Pull back to a sane 1.65 and run a SINGLE voice (no triple
      // stack) so the rest of the mix can breathe and jeffrey stays
      // continuous pass-to-pass.
      let g = 1.65;
      if (tilEnd < 8) g *= Math.max(0.20, tilEnd / 8);
      const pan = Math.sin(j * 0.7) * 0.10;
      if (j === 0) {
        // TEASER ENTRY — first pass sings just ONE word: "that". The
        // earlier draft also fired "we want" at the end of the bar but
        // @jeffrey wanted that dropped too — the first utterance is a
        // single syllable announcement; the full mantra arrives on pass 1.
        // mantra timing: that@0.82s (single 200 ms slice).
        playSampleSwept(t + 2.5 * SPB, jvoc, g * 1.05, {
          startRate: 1.0, endRate: 1.0,
          maxDurMs: 200,                                // "that"
          bufOffset: 0.82, pan, wetSend: 0.40, fade: 0.03,
        });
        jvCount += 1;
      } else {
        // LEAD — single voice, dry-ish, with SIDECHAIN DUCK on brass +
        // sub so the busy mid range steps out of jeffrey's way without
        // pulling the choir down too far. Triple-stacking at g=2.80
        // was clipping the buffer and apparently making the limiter
        // suppress subsequent passes; one clean lead + sidechain is
        // the readable solution.
        // Tighter timing — humDt collapsed from ±10 ms to ±3 ms and
        // fade-in halved (40 → 8 ms) so the attack lands sharply on
        // the bar instead of ramping in late.
        const humDt = (rng() * 2 - 1) * 0.003;
        playSample(t + humDt, jvoc, g, {
          rate: 1.0, pan, wetSend: 0.15, fade: 0.008,
        });
        // Sidechain duck — pull DUCK[] down across the 4 s mantra so
        // every voice() + sub() voice steps back. playSample bypasses
        // DUCK, so choir + foley + jeffrey himself are unaffected.
        const duckStart = Math.floor((t + humDt) * SR);
        const duckEnd   = Math.min(N, Math.floor((t + humDt + 4.0) * SR));
        const duckFloor = 0.65;
        const duckFadeN = Math.floor(0.080 * SR);
        for (let di = duckStart; di < duckEnd; di++) {
          let dg = duckFloor;
          if (di - duckStart < duckFadeN) dg = 1 - (1 - duckFloor) * ((di - duckStart) / duckFadeN);
          else if (duckEnd - di < duckFadeN) dg = duckFloor + (1 - duckFloor) * (1 - (duckEnd - di) / duckFadeN);
          if (dg < DUCK[di]) DUCK[di] = dg;
        }
        jvCount++;
        // Foley tail after "we want" lands — mantra ends near t + 4.0 s.
        // Aim 0.18 s after "want" so the foley reads as the punctuation,
        // not a layered hit.
        const fb = foleyBufs[variant];
        if (fb) {
          const fT = t + 4.05;
          if (fT + fb.length / SR < cxForTTS + 6) {
            playSample(fT, fb, foleyGain[variant], {
              rate: 1.0,
              pan: foleyPan[variant] + Math.sin(j * 0.7) * 0.05,
              wetSend: 0.45,
              fade: 0.015,
            });
            foleyCount++;
          }
        }
      }
    }
    console.log(`→ jeffrey-pvc lead vocal · ${jvCount} passes + ${foleyCount} foley tails (money→cha-ching, honey→lick, bunnies→boing) from ${stForTTS.toFixed(2)}s → ${cxForTTS.toFixed(2)}s`);
  }

  // ── grenade @ t=14s — right before the drop. Close-mic, dry, big punch
  // that the 2-s reverb fade-in catches the tail of so the explosion's
  // body is upfront and its decay blooms into the wall-of-sound slam.
  // Source: Freesound "Boom Explosion" #523783.
  const GRENADE_PATH = resolve(HERE, "../samples/grenade.wav");
  if (existsSync(GRENADE_PATH)) {
    const gren = loadWavMono(GRENADE_PATH);
    // BIGGER feel — main hit at higher gain, slightly slower rate for body
    // depth, plus a layered sub-octave copy + a deep sub() pulse for the
    // low-end shockwave.
    // Gains pulled back — earlier stack was slamming the master limiter
    // hard, causing a perceived "global ducking" wash for ~100ms after.
    // Still BIG but in proportion.
    playSample(12.90, gren, 0.85, { rate: 0.88, pan: 0, wetSend: 0.30, fade: 0.005 });
    playSample(12.90, gren, 0.35, { rate: 0.50, pan: 0, wetSend: 0.45, fade: 0.005 }); // octave-down body
    sub(12.90, 0.55, 26, 0.65);    // D1 sub shockwave — lighter
    console.log(`→ grenade · ${(gren.length / SR).toFixed(2)}s explosion at 12.90s (gain-reduced — no longer slams limiter)`);

    // ── CARD-FLIP LEAD-IN + GRENADE on the LAST DROP — 4 cards-burst/
    // hard chops zip into the drop, then grenade lands EXACTLY on the
    // climax downbeat at 110.77s. Cards build tension; grenade is the
    // arrival.
    if (cardCount > 0) {
      const flipPool = [cards["cards-burst-a"], cards["cards-hard"], cards["cards-burst-b"], cards["cards-fast"]].filter(Boolean);
      const flipOffsets = [-0.62, -0.46, -0.30, -0.14];     // 4 zips leading INTO the drop
      for (let f = 0; f < flipOffsets.length && f < flipPool.length; f++) {
        const buf = flipPool[f % flipPool.length];
        const t = 110.77 + flipOffsets[f];
        const rate = [1.10, 1.30, 1.00, 1.40][f];
        const pan  = [(-0.6), (0.55), (-0.4), (0.5)][f];
        playSample(t, buf, 0.55, { rate, pan, wetSend: 0.50, fade: 0.02 });
      }
    }
    playSample(110.77, gren, 1.15, { rate: 0.88, pan: 0, wetSend: 0.25, fade: 0.005 });
    playSample(110.77, gren, 0.55, { rate: 0.50, pan: 0, wetSend: 0.45, fade: 0.005 });
    sub(110.77, 0.65, 26, 1.05);
    console.log(`→ card-flip lead-in (4 zips) + grenade #2 on the LAST DROP at 110.77s`);
  }

  // ── stadium crowd @ t=60s — the bridge has escalated with the crow
  // scratching; at 60s a stadium roar enters arpeggiated. A chord-tone
  // arpeggio (D minor scale, rates derived from MIDI intervals) plays
  // short slices of crowd-roar at different pitches over 8 beats, giving
  // the crowd a "musical" quality — like the audience is humming a tune.
  // The full crowd-roar plays at low gain underneath as the sustained bed,
  // and the win-cheer overlay surges at t=63s as the harmonic peak.
  // Sources: Freesound benfree #130568 + FoolBoyMedia #397434.
  const CROWD_ROAR_PATH = resolve(HERE, "../samples/crowd-roar.wav");
  const CROWD_WIN_PATH  = resolve(HERE, "../samples/crowd-win.wav");
  if (existsSync(CROWD_ROAR_PATH)) {
    const roar = loadWavMono(CROWD_ROAR_PATH);
    // Sustained bed enters at 53s with a 7-s fade-in — peaks under the
    // last crow chops so they fuse into one continuous swishy mass before
    // becoming the foreground. Compositional crossfade: density rises in
    // the crowd as the crow chops thin and slow.
    playSample(53.00, roar, 0.22, { rate: 1.0, pan: 0, wetSend: 0.70, fade: 7.0 });
    // Arpeggio enters at 58s (overlapping the late crow chops). D-minor:
    // 0, 3, 7, 12, 15, 12, 7, 3, 0. First note pitches up to common-tone
    // with the late crow chop pitches so the transition is musical.
    const arpSemis = [0, 3, 7, 12, 15, 12, 7, 3, 0];
    let arpCount = 0;
    for (let i = 0; i < arpSemis.length; i++) {
      const t = 58.00 + i * SPB;
      const rate = Math.pow(2, arpSemis[i] / 12);
      const pan  = (i % 2 === 0) ? -0.65 : 0.65;
      const off  = 1.5 + (i * 1.8) % 14;
      // Gain ramps from low → full across the arpeggio so it emerges
      const arpG = 0.15 + (i / arpSemis.length) * 0.25;
      playSampleSwept(t, roar, arpG, {
        startRate: rate, endRate: rate, maxDurMs: 380, pan,
        wetSend: 0.70, bufOffset: off, fade: 0.04,
      });
      arpCount++;
    }
    console.log(`→ crowd roar · bed fade-in at 53.00s + ${arpCount}-note D-minor arpeggio emerging at 58.00s`);
  }
  if (existsSync(CROWD_WIN_PATH)) {
    const win = loadWavMono(CROWD_WIN_PATH);
    playSample(63.00, win, 0.18, { rate: 1.0, pan: 0, wetSend: 0.50, fade: 1.5 });
    console.log(`→ crowd win cheer overlay at 63.00s`);
  }

  // ── SKID SNARES @ statement 20-47s — on beats 2 + 4 of every bar
  // through the main vocal section, with EVERY 4TH BAR replacing the
  // straight clap with a SKID: playSampleSwept with a rate ramp (1.0 →
  // 0.55 or 1.0 → 1.6) that smears the clap into a turntablist
  // pitch-skid. Reads as record-scratch on the backbeat.
  const SKID_PATH = resolve(HERE, "../samples/clap.wav");
  if (existsSync(SKID_PATH)) {
    const clap = loadWavMono(SKID_PATH);
    const skidStart = 19.78;        // ~4 bars past the drop
    const skidEnd   = 47.0;         // end of statement
    let skidCount = 0, regularCount = 0;
    const startBar = Math.ceil(skidStart / SPBAR);
    const endBar   = Math.floor(skidEnd / SPBAR);
    for (let b = startBar; b <= endBar; b++) {
      const tBar = b * SPBAR;
      const isSkidBar = (b % 4 === 0);    // every 4th bar = skid
      if (isSkidBar) {
        // Skidded snare — rate-swept clap on beat 2 (downward) + beat 4 (upward)
        playSampleSwept(tBar + 1 * SPB + hum(0.005), clap, 0.50, {
          startRate: 1.00, endRate: 0.45, maxDurMs: 360,
          pan: -0.20, wetSend: 0.50, fade: 0.012,
        });
        playSampleSwept(tBar + 3 * SPB + hum(0.005), clap, 0.50, {
          startRate: 1.00, endRate: 1.70, maxDurMs: 280,
          pan:  0.20, wetSend: 0.50, fade: 0.012,
        });
        skidCount += 2;
      } else {
        // Regular clap on 2 + 4
        playSample(tBar + 1 * SPB + hum(0.004), clap, 0.42, { rate: 1.0, pan: -0.10, wetSend: 0.40, fade: 0.01 });
        playSample(tBar + 3 * SPB + hum(0.004), clap, 0.42, { rate: 1.0, pan:  0.10, wetSend: 0.40, fade: 0.01 });
        regularCount += 2;
      }
    }
    console.log(`→ snare pattern · ${regularCount} regular + ${skidCount} SKID hits across statement bars ${startBar}-${endBar}`);
  }

  // ── 808 CLAP pattern 72-95s — classic 808 clap on the 2 and 4 of every
  // bar, fading in over the first 4 bars and out over the last 3, so it
  // emerges from the bridge as a steady backbeat and dissolves back into
  // the develop ramp. Source: Freesound Sorinious_Genious #561113.
  const CLAP_PATH = resolve(HERE, "../samples/clap.wav");
  if (existsSync(CLAP_PATH)) {
    const clap = loadWavMono(CLAP_PATH);
    const clapStart = 72.0, clapEnd = 95.0;
    const clapFadeInBars = 4, clapFadeOutBars = 3;
    let clapCount = 0;
    const startBar = Math.ceil(clapStart / SPBAR);
    const endBar   = Math.floor(clapEnd / SPBAR);
    for (let b = startBar; b <= endBar; b++) {
      const tBar = b * SPBAR;
      // Fade-in curve over first N bars, fade-out over last N bars
      const bIn  = b - startBar;
      const bOut = endBar - b;
      let gFade = 1;
      if (bIn < clapFadeInBars)   gFade = Math.min(gFade, (bIn + 1) / clapFadeInBars);
      if (bOut < clapFadeOutBars) gFade = Math.min(gFade, (bOut + 1) / (clapFadeOutBars + 1));
      const g = 0.42 * gFade;
      // beat 2 + beat 4 — slight hum and pan jitter for a human feel
      playSample(tBar + 1 * SPB + hum(0.004), clap, g, { rate: 1.0, pan: -0.08 + hum(0.05), wetSend: 0.50, fade: 0.01 });
      playSample(tBar + 3 * SPB + hum(0.004), clap, g, { rate: 1.0, pan:  0.08 + hum(0.05), wetSend: 0.50, fade: 0.01 });
      clapCount += 2;
    }
    console.log(`→ 808 clap · ${clapCount} hits on 2+4 from ${clapStart.toFixed(0)}-${clapEnd.toFixed(0)}s, fade in/out`);
  }

  // ── crow + crowd ambient blend through develop + climax — both elements
  // recur as low-gain textural beds so they're "always there" not just at
  // one transition. Sparse crow chops every ~5s, plus a sustained crowd
  // wash through climax that fades before the whip at 132s.
  if (existsSync(CROW_PATH) && existsSync(CROWD_ROAR_PATH)) {
    const crowAmb = loadWavMono(CROW_PATH);
    const roarAmb = loadWavMono(CROWD_ROAR_PATH);
    const C1A = 0.10, C2A = 0.85, C3A = 1.55, C4A = 2.30, C5A = 3.05;
    // Crow chops 80-128s. Sparse early, DENSE from 1:27 (87s) onward —
    // the develop section builds + the user wants lots of crow scratching
    // bleeding into the climax. Louder gains overall too.
    // CROWS HEAVILY THINNED — @jeffrey heard the 26-chop dense bed as
    // overcooked before 2:00. Keep only a handful of sparse landmark
    // chops as connective texture; the EVERYTHING BLAST at 2:00 stays
    // for the section turn.
    const ambChops = [
      [ 80.0, C2A, 0.55, 320,  0.40, -0.55 ],
      [ 88.0, C5A, 0.45, 380,  0.40,  0.55 ],
      [ 98.0, C3A, 0.60, 320,  0.40, -0.40 ],
      [108.5, C5A, 0.40, 460,  0.40,  0.45 ],
      [115.5, C1A, 1.45, 170,  0.40,  0.55 ],
    ];
    for (const [t, off, rate, ms, g, pan] of ambChops) {
      playSampleSwept(t + hum(0.04), crowAmb, g, {
        startRate: rate, endRate: rate,
        maxDurMs: ms, pan, wetSend: 0.85,
        bufOffset: off, fade: 0.025,
      });
    }
    // Crowd bed sustains across climax — low gain, long fade-in + fade-out
    // so it lives under the brass + sub. Cuts before the whip at 132 s.
    playSample(108.00, roarAmb, 0.13, { rate: 0.92, pan: -0.20, wetSend: 0.55, fade: 5.5 });
    playSample(115.00, roarAmb, 0.11, { rate: 1.08, pan:  0.25, wetSend: 0.55, fade: 5.5 });

    // EVERYTHING BLAST REMOVED — @jeffrey: "mix way too busy from 2:00
    // to 2:20". The 8-chop crow burst + 3 overlapping roar beds + win
    // cheer all stacked at master 2:00-2:01 was the densest moment in
    // the track. Now master 2:00 lands cleanly on the climax brass +
    // grand piano portal, with no extra crowd/crow content on top.
    const blastChops = [];
    console.log(`→ crow+crowd blend · ${ambChops.length} sparse chops 80-128s + 3 crowd beds (EVERYTHING BLAST removed for breathing room)`);
  }


  // ── trumpet REMOVED — the fluidsynth+VintageDreamsWaves GM render
  // sounded bad either way (jagged or limp). The generator still lives at
  // bin/gen-trumpet.mjs in case we revisit with a better soundfont.

  // ── WHIP → GALLOP → NEIGH → TRAIN at 2:12 — climax transitions into
  // a western-caboose outro. Whip crack at 132 s, horse gallop runs 20 s
  // beneath the coda, a single whinny at 138 s as the rider's voice,
  // then a passing freight train sweeps in at 152 s and carries the
  // track out. Sources: Freesound JayRom01 #615761 (whip), Max_Headroom
  // #175356 (gallop), n_audioman #321957 (neigh), InspectorJ #328114 (train).
  const WHIP_PATH  = resolve(HERE, "../samples/whip.wav");
  const NEIGH_PATH = resolve(HERE, "../samples/neigh.wav");
  const TRAIN_PATH = resolve(HERE, "../samples/train.wav");
  if (existsSync(WHIP_PATH)) {
    const whip = loadWavMono(WHIP_PATH);
    // Whip HI-HAT BURST — "t t t t t t t neighhhh". 7 short whip chops on
    // a 16th grid, ping-ponging L/R, slight rate jitter so the t's aren't
    // identical. Each chop ~80 ms (just the crack snap). The neigh lands
    // on the 8th step like the punch-line.
    const SP16W = SPB / 4;
    for (let i = 0; i < 7; i++) {
      const tW = 132.00 + i * SP16W;
      const rate = 1.0 + (i % 2 === 0 ? -0.05 : 0.05);
      const pan  = (i % 2 === 0) ? -0.55 : 0.55;
      // Cranked up — was getting buried under the climax wall-of-sound.
      const g    = 2.00 - i * 0.10;          // 2.0 → 1.4 across the burst
      playSampleSwept(tW, whip, g, {
        startRate: rate, endRate: rate, maxDurMs: 95, pan,
        wetSend: 0.20, fade: 0.003,
      });
    }
    console.log(`→ whip · 7-chop hi-hat burst 132.00-132.50s (LOUD)`);
  }
  // ── GRAND PIANO PORTAL — @jeffrey: around master 2:00, the
  // orchestration briefly "portals" into clean grand piano playing the
  // same chord progression, then vortexes back. 2-bar window at climax
  // bars 7-8 (pre-master ~120-123s). Big sustained chord on each
  // downbeat + arpeggio of chord tones across 8 eighth-notes. Clean
  // piano (bits=16, hold=1, no bitcrush) so the timbre clearly stands
  // apart from the surrounding distorted brass + saw lead.
  const climaxForPiano = sectionRanges.find((s) => s.name === "climax");
  if (climaxForPiano) {
    const portalStart = climaxForPiano.startSec + 7 * SPBAR;
    // Climax chord cycle (with +14 transpose) at bars 7-8: C → Dm → D major + E minor.
    const portalChords = [
      { root: 50, q: [0, 4, 7] },   // D major
      { root: 52, q: [0, 3, 7] },   // E minor
    ];
    for (let b = 0; b < 2; b++) {
      const tBar = portalStart + b * SPBAR;
      const ch = portalChords[b];
      // Sustained triad on downbeat
      for (const semi of ch.q) {
        piano(tBar + hum(0.004), SPBAR * 0.98, ch.root + semi + 12, 0.22,
          { bits: 16, hold: 1, pan: hum(0.15) });
      }
      // Bass octave for weight
      piano(tBar, SPBAR * 0.95, ch.root, 0.16, { bits: 16, hold: 1, pan: -0.30 });
      // Arpeggio — 8 eighth-notes, chord tones climbing + descending
      const seq = [
        ch.root, ch.root + ch.q[1], ch.root + ch.q[2], ch.root + 12,
        ch.root + ch.q[1] + 12, ch.root + ch.q[2], ch.root + ch.q[1], ch.root,
      ];
      for (let k = 0; k < 8; k++) {
        const tk = tBar + k * (SPB / 2) + hum(0.004);
        piano(tk, SPB * 0.55, seq[k] + 24, 0.14,
          { bits: 16, hold: 1, pan: (k % 2 === 0 ? -0.35 : 0.35) });
      }
    }
    // Brief lead-in shimmer (pre-portal): a fast rising arpeggio in
    // the half-bar BEFORE the portal, like opening the portal door.
    const leadInStart = portalStart - SPBAR * 0.5;
    const leadInSeq = [50, 54, 57, 62, 66, 69, 74, 78];     // D3 → F#5 climb
    for (let k = 0; k < leadInSeq.length; k++) {
      piano(leadInStart + k * 0.07, 0.40, leadInSeq[k], 0.10,
        { bits: 16, hold: 1, pan: (k - 4) * 0.10 });
    }
    // Vortex tail (post-portal): a descending sparkle as we drop back
    // into orchestration. Cascades down over the half-bar AFTER.
    const vortexStart = portalStart + portalDurBars();
    function portalDurBars() { return 2 * SPBAR; }
    const vortexSeq = [86, 81, 78, 74, 69, 66, 62, 57];     // top-down cascade
    for (let k = 0; k < vortexSeq.length; k++) {
      piano(vortexStart + k * 0.06, 0.35, vortexSeq[k], 0.10,
        { bits: 16, hold: 1, pan: (4 - k) * 0.10 });
    }
    console.log(`→ grand piano PORTAL · 2 bars at climax bars 7-8 (pre-master ${portalStart.toFixed(2)}s) + lead-in arpeggio + vortex cascade`);
  }

  // ── DRIPPY FLOWER TRANSITION — the climax → coda hand-off at ~142s
  // (pre-master) was reading abrupt. A cascade of slowly descending
  // bell pings + a long descending pad fall down through the boundary
  // smear the seams. The bell()'s worm-wiggle drift naturally pitches
  // them down as they decay — we just stack a chord of them descending.
  const drippyT0 = 140.5;
  const drippyNotes = [98, 93, 89, 86, 82, 78, 74];   // D7 → A6 → F6 → D6 → Bb5 → F#5 → D5
  for (let i = 0; i < drippyNotes.length; i++) {
    const t = drippyT0 + i * 0.45;                    // slow drip every 450ms
    const m = drippyNotes[i] + 0.15;
    bell(t, 4 * SPB, m, 0.070 - i * 0.005, {
      pan: i % 2 === 0 ? -0.35 : 0.35,
      decTau: 3.5 - i * 0.3,
      atk: 0.030, wetSend: 0.92,
    });
  }
  // LONG transition pad — overlapping descending pad notes that sustain
  // THROUGH the climax→coda boundary, smearing the seams so the orchestra
  // doesn't cut off abruptly at ~2:20 master (pre-master 141.75 s). Each
  // note 4-6 s with long releases that bleed into the next note.
  const padFall = [
    { t: 138.0, m: 62, dur: 5.0 },   // D4 — starts well before the boundary
    { t: 140.0, m: 58, dur: 5.5 },   // A#3
    { t: 142.0, m: 55, dur: 6.0 },   // G3
    { t: 144.5, m: 50, dur: 7.0 },   // D3 — bridges into coda
    { t: 147.0, m: 45, dur: 8.0 },   // A2 — deep sustain through outro
  ];
  for (const p of padFall) {
    voice(p.t, p.dur, p.m, 0.090, {
      atk: 1.20, rel: 2.50, vibR: 3.2, vibD: 0.008, drive: 0.9,
      parts: [[1, 1], [2, 0.45], [3, 0.20], [4, 0.08]],
      pan: 0,
    });
    // Octave-up sparkle on the same notes for shimmer
    voice(p.t + 0.05, p.dur, p.m + 12, 0.045, {
      atk: 1.60, rel: 2.20,
      parts: [[1, 1], [2, 0.30], [3, 0.10]],
      pan: 0.30,
    });
  }
  if (existsSync(NEIGH_PATH)) {
    const ne = loadWavMono(NEIGH_PATH);
    // Cranked + DRY — the punch-line after the whip burst.
    playSample(132.65, ne, 1.60, { rate: 1.0, pan: 0.10, wetSend: 0.25, fade: 0.02 });
    console.log(`→ horse neigh at 132.65s (punch-line, LOUD + DRY)`);
  }
  // ── CHURCH BELL GONG @ "DOT" — @jeffrey: align the bell with the
  // "dot" in "aesthetic dot computer". Whisper places "dot" at
  // 0.480-0.590s into the stamp sample; the main stamp plays at rate
  // 1.18, so "dot" lands at engine_stamp_start + 0.480/1.18 ≈ +0.407s.
  // Stamp starts at climaxSec.startSec - 3.5 = 107.27, so the dot lands
  // at engine 107.68. Pitched DOWN a 4th (rate 0.65) for deep gong.
  const BELL_PATH = resolve(HERE, "../samples/church-bell.wav");
  if (existsSync(BELL_PATH)) {
    const bell8 = loadWavMono(BELL_PATH);
    const climaxForBell = sectionRanges.find((s) => s.name === "climax");
    const bellT = (climaxForBell?.startSec ?? 110.77) - 3.5 + (0.480 / 1.18);
    playSample(bellT, bell8, 0.80, { rate: 0.65, pan: 0.0, wetSend: 0.65, fade: 0.05 });
    console.log(`→ church bell gong at ${bellT.toFixed(2)}s · aligned with "dot" in AC stamp`);
  }

  // ── CHAIN DRAG @ 1:45 — heavy metal chain rattle, dramatic mid-track
  // accent. Master 1:45 = pre-master 105s. Source: Freesound #611669.
  const CHAIN_PATH = resolve(HERE, "../samples/chain.wav");
  if (existsSync(CHAIN_PATH)) {
    const chain = loadWavMono(CHAIN_PATH);
    playSample(105.00, chain, 0.55, { rate: 1.0, pan: -0.20, wetSend: 0.45, fade: 0.05 });
    console.log(`→ chain drag at 105.00s (master 1:45)`);
  }

  // ── iOS KEYBOARD CLICKS @ 1:22 — a burst of iPhone text-key taps,
  // dropped into the develop section. Picks up the typewriter motif from
  // the intro and brings it into the modern era. Source: Freesound
  // jordanielmills #640365.
  const IOS_PATH = resolve(HERE, "../samples/ios-click.wav");
  if (existsSync(IOS_PATH)) {
    const ios = loadWavMono(IOS_PATH);
    // Play the first ~3s of clicks at 82s, pitched up slightly for crispness
    playSample(82.00, ios, 0.55, { rate: 1.10, pan: -0.20, wetSend: 0.35, fade: 0.02 });
    console.log(`→ iOS keyboard clicks at 82s (master 1:22)`);
  }

  // ── CROWD WOOOO @ 2:19 — after the horse moment (whip+neigh at 132s),
  // the crowd ROARS in for the final coda transition. Pre-master 140.7s
  // maps to master 2:19 (after the post-110.77 atempo=1.06 compression).
  // CROWD SCRATCHED + PITCH-SHIFTED @ 2:20 — instead of clean crowd
  // playback, scratch the WOOO + roar with rate-shifts + chops + stutter
  // for a turntablist climax-departure moment. @jeffrey: "highly skifin
  // and scratching and pitch shifting around" + "FUX IT UPPP".
  if (existsSync(CROWD_WIN_PATH)) {
    const wooo = loadWavMono(CROWD_WIN_PATH);
    // Stutter-burst of pitched WOOO chops. Smoothed @ master 2:24 — the
    // tail chops (sr 0.55 deep + sr 1.80 cranked) were piling into each
    // other as harsh aliased artifacts; tame the extremes, lengthen the
    // fades, lower the tail gains so the burst breathes out instead of
    // clipping into the pad fall.
    const wooo_chops = [
      { t: 140.30, sr: 0.65, ms: 250, pan: -0.55, g: 0.70 },   // slow pitched-down
      { t: 140.55, sr: 1.40, ms: 180, pan:  0.55, g: 0.72 },   // pitched up
      { t: 140.75, sr: 0.85, ms: 220, pan: -0.30, g: 0.70 },
      { t: 140.97, sr: 1.45, ms: 180, pan:  0.55, g: 0.62 },   // pulled in from 1.55
      { t: 141.18, sr: 1.00, ms: 320, pan:  0.00, g: 0.80 },   // BIG center
      { t: 141.60, sr: 0.70, ms: 380, pan:  0.30, g: 0.55 },   // softer crawl (was 0.55/0.78)
      { t: 142.02, sr: 1.50, ms: 140, pan: -0.55, g: 0.50 },   // tame the cranked top (was 1.80/0.72)
    ];
    for (const c of wooo_chops) {
      playSampleSwept(c.t, wooo, c.g, {
        startRate: c.sr, endRate: c.sr * (0.95 + rng() * 0.10),  // narrower sweep
        maxDurMs: c.ms, pan: c.pan, wetSend: 0.55,
        fade: 0.035,                                              // longer fade-out smooths the seam
      });
    }
    console.log(`→ crowd WOOO scratched · ${wooo_chops.length} pitched chops 140.3-142.1s`);
  }
  if (existsSync(CROWD_ROAR_PATH)) {
    const roar = loadWavMono(CROWD_ROAR_PATH);
    const roar_chops = [
      { t: 140.40, sr: 0.60, ms: 420, off: 2.0, pan: -0.65, g: 0.48 },
      { t: 140.90, sr: 1.30, ms: 280, off: 5.0, pan:  0.65, g: 0.48 },
      { t: 141.35, sr: 0.78, ms: 380, off: 1.0, pan: -0.40, g: 0.48 },
      { t: 141.80, sr: 1.10, ms: 320, off: 8.0, pan:  0.40, g: 0.50 },
      // FINAL CRAWL — was the worst offender for 2:24 glitch chatter
      // (rate 0.40 over 540 ms aliased into the pad fall). Pulled the
      // rate up, shortened the chop, ducked the gain, and lengthened the
      // fade so it dissolves into the coda instead of breaking up.
      { t: 142.20, sr: 0.55, ms: 360, off: 3.0, pan:  0.00, g: 0.42 },
    ];
    for (const c of roar_chops) {
      playSampleSwept(c.t, roar, c.g, {
        startRate: c.sr, endRate: c.sr * (0.92 + rng() * 0.16),    // narrower sweep
        maxDurMs: c.ms, pan: c.pan, wetSend: 0.70,
        bufOffset: c.off, fade: 0.045,                               // longer fade-out
      });
    }
    console.log(`→ crowd-roar scratched · ${roar_chops.length} pitched chops 140.4-142.7s`);
  }

  // ── ZIPPER + SIPPER @ 1:16 — fly zipper + a sip of a drink. The zipper
  // opens the moment, the sip closes it. Late-bridge moment, sets up the
  // build into develop. Sources: Freesound atha89 #79161 + Natty23 #257277.
  const ZIPPER_PATH = resolve(HERE, "../samples/zipper.wav");
  const SIPPER_PATH = resolve(HERE, "../samples/sipper.wav");
  if (existsSync(ZIPPER_PATH)) {
    const zip = loadWavMono(ZIPPER_PATH);
    playSample(76.00, zip, 0.65, { rate: 1.0, pan: -0.25, wetSend: 0.30, fade: 0.02 });
    console.log(`→ zipper at 76.00s`);
  }
  if (existsSync(SIPPER_PATH)) {
    const sip = loadWavMono(SIPPER_PATH);
    playSample(77.40, sip, 0.55, { rate: 1.0, pan:  0.30, wetSend: 0.30, fade: 0.02 });
    console.log(`→ sipper at 77.40s`);
  }

  // ── ACCORDION @ master 2:16-2:20 — late-climax fill, Toriana old-
  // accordion chord phrase (#187053, 4.38s). Pre-master t=137.5s after the
  // post-climax atempo=1.06 maps to master 2:16. Adds a folk-pump warmth
  // under the chaos right before coda.
  const ACCORDION_PATH = resolve(HERE, "../samples/accordion.wav");
  if (existsSync(ACCORDION_PATH)) {
    const acc = loadWavMono(ACCORDION_PATH);
    playSample(137.50, acc, 0.45, { rate: 1.0, pan: 0.10, wetSend: 0.45, fade: 0.25 });
    console.log(`→ accordion chord phrase at 137.5s (≈ master 2:16-2:20)`);
  }

  // ── BUBBLES + SQUISH PULLED — @jeffrey: the splashy outro reads
  // repetitive and "weird". The train carries the western-caboose outro
  // cleanly without the wet bubble layer.

  if (existsSync(TRAIN_PATH)) {
    const train = loadWavMono(TRAIN_PATH);
    // Train enters quietly at 150 s, runs at normal pitch through 155 s,
    // then PITCH-ARPEGGIATES DOWN in chromatic-ish steps to a deep sub
    // crawl, lasting after the gallop has faded. Fade-out at the very end.
    playSample(150.00, train, 0.45, { rate: 1.0, pan: 0, wetSend: 0.40, fade: 3.0 });
    // pitch-down arpeggio — 6 chops at descending rates, each 1.5-2 s
    const trainArp = [
      [154.5, 0.94, 1600, 0.45,  0.15],   // semitone down
      [156.2, 0.84, 1700, 0.48, -0.25],   // whole tone
      [157.8, 0.71, 1800, 0.50,  0.20],   // minor 3rd
      [159.3, 0.59, 1900, 0.50, -0.20],   // perfect 4th
      [160.7, 0.50, 1800, 0.50,  0.10],   // tritone
      [162.0, 0.38, 1600, 0.55,  0.00],   // deep sub — track end
    ];
    for (const [t, rate, ms, g, pan] of trainArp) {
      playSampleSwept(t, train, g, {
        startRate: rate, endRate: rate, maxDurMs: ms, pan,
        wetSend: 0.55, fade: 0.20,
      });
    }
    console.log(`→ train · 150s entry + 6-chop pitch-down arpeggio 154-162s (chromatic descent into sub)`);
  }

  // ── three kitten meows clustered through 7-13s — three different
  // cats from three different uploaders/recordings, panned slightly so
  // it reads as a small chorus of cats over the rushing-in splash. All
  // play quiet and dry. Pitches were YIN-detected on each so future
  // ornaments can lock to them if needed.
  // Sources: Freesound byronabadia #725264 ("12 Kitty meowing", G#5),
  // taylorelorenz #528743 ("meow.wav"), and meow_sample #163286.
  const MEOW_FILES = [
    { path: "meow.wav",   t: 7.00,  gain: 0.10, pan: -0.55, wet: 0.40 },  // studio kitten — back to natural
    { path: "meow-2.wav", t: 10.00, gain: 0.09, pan:  0.45, wet: 0.55 },  // bigger meow — right
    { path: "meow-3.wav", t: 12.40, gain: 0.10, pan: -0.25, wet: 0.50 },  // short mew — slight left
  ];
  let meowCount = 0;
  for (const m of MEOW_FILES) {
    const p = resolve(HERE, "../samples/" + m.path);
    if (!existsSync(p)) continue;
    const buf = loadWavMono(p);
    playSample(m.t, buf, m.gain, { rate: 1.0, pan: m.pan, wetSend: m.wet, fade: 0.05 });
    meowCount++;
  }
  console.log(`→ ${meowCount} kitten meows clustered 7-13s · 3 different cats`);

  // ── overture OPENING + distant BIRDIES — @jeffrey: "begin with the fast
  // roll cards". So the very first sound at t=0 is a featured cards-fast
  // roll, louder + present (the 1.52 s, 7-transient take is shaped like
  // a riffle through a deck — perfect opener). After it tails, distant
  // pitched birdies (cards-slow + cards-medium, zcr 3318/7408) pan far
  // L↔R as the chirpy background, very low gain + very wet, fading out
  // as the overture organs slowly fade in.
  if (cardCount > 0) {
    const birdyBuf = cards["cards-slow"] || cards["cards-medium"] || cards["cards-fast"];
    const birdyAlt = cards["cards-medium"] || cards["cards-fast"] || birdyBuf;
    const rollBuf  = cards["cards-fast"]  || birdyBuf;
    // [t_sec, buf, startRate, endRate, pan, gain, wet, maxMs]
    const birdies = [
      // OPENING fast roll — present but roomy: gain back up so the deck
      // riffle reads alongside the typewriter, with heavy wet so it sits
      // in the space, not in your face.
      [ 0.00, rollBuf,  1.00, 1.05,  0.00, 0.30,  0.70, 1500 ],
      // Distant birdies fade in afterward
      [ 1.40, birdyAlt, 2.20, 2.10,  0.85, 0.050, 0.90, 500 ], // R answer
      [ 2.20, birdyBuf, 1.75, 1.90, -0.75, 0.055, 0.88, 700 ], // L
      [ 3.00, birdyAlt, 2.30, 2.50,  0.90, 0.045, 0.92, 450 ], // R chirp up
      [ 3.80, birdyBuf, 1.80, 1.70, -0.85, 0.050, 0.90, 650 ], // L drifting
      [ 4.60, birdyAlt, 2.00, 2.20,  0.70, 0.048, 0.92, 550 ], // R
      [ 5.40, birdyBuf, 1.95, 2.05, -0.95, 0.042, 0.93, 500 ], // far far L
      [ 6.20, birdyAlt, 2.40, 2.30,  0.80, 0.038, 0.94, 450 ], // R, faintest
    ];
    let birdyCount = 0;
    for (const [t, buf, sr1, sr2, pan, g, wet, ms] of birdies) {
      if (!buf) continue;
      playSampleSwept(t + hum(0.02), buf, g, {
        startRate: sr1, endRate: sr2, pan, wetSend: wet,
        maxDurMs: ms, fade: 0.040,
      });
      birdyCount++;
    }
    console.log(`→ overture distant birdies · ${birdyCount} pitched card flaps panning L↔R`);
  }

  // ── statement WALL OF SOUND drop — drums + scratches slam in HUGE on
  // the first kick of statement and ride 4 bars. Stacked, wide-panned
  // 16th-note grid; downbeat steps fire hit + low + scratch simultaneously
  // so the kit reads as one fat slab. Heavy reverb send turns the room
  // into a hall. A pitched CC0 Star Wars blaster (freesound #466867,
  // MikeE63, CC0) rides on top as the perc-break ornament @jeffrey asked
  // for — same one-shot resampled across two octaves so it reads as a
  // family of zaps. THE LAW amended: another sampled layer on the sine engine.
  //   B = hit1 + low1 + scrB1 + scrA2 stacked (downbeat slam)
  //   K = low2 + scrA2 (off-downbeat answer)
  //   h = hit2 (mid attack)
  //   t = scratch tick (hi-hat-like)
  //   . = rest
  const statementSec = sectionRanges.find((s) => s.name === "statement");
  const BLASTER_PATH = resolve(HERE, "../samples/starwars-blaster.wav");
  const blasterBuf = existsSync(BLASTER_PATH) ? loadWavMono(BLASTER_PATH) : null;
  if (statementSec && drumCount > 0) {
    const stT = statementSec.startSec;
    const wallPat = [
      "Bhth tKth Bhth tKth",   // bar 1 — full slam
      "Bhth tKth BhBh tKtK",   // bar 2 — denser answer
      "Bhth tKth Bhth tKth",   // bar 3 — slam again
      "BhBh KhBh BhBh KKKK",   // bar 4 — fill
      "Bhth tKth Bhth tKth",   // bar 5 — ghostly slam (fade ~0.08)
      "BhBh KhBh BhBh KKKK",   // bar 6 — tail fill (fade ~0.05)
    ].map((s) => s.replace(/\s+/g, ""));
    let hits = 0;
    // Exponential fade across the 6-bar break: gain starts at 1.0 at the
    // first downbeat (the slam) and decays so by step 96 (end of bar 6)
    // it's ~5%. @jeffrey: drum break should fade out, not just stop,
    // and last a bit longer. tau=32 → fade ≈ 0.61 at end of bar 1, 0.37
    // bar 2, 0.22 bar 3, 0.14 bar 4, 0.08 bar 5, 0.05 bar 6.
    const wallFade = (barIdx, stepIdx) => Math.exp(-(barIdx * 16 + stepIdx) / 32);
    // Arpeggio pitch ratios — minor triad cycle (D F A D higher octave),
    // climbing as the fade descends. Each consecutive hit advances to
    // the next arp note, giving the break a melodic/pitched character
    // that lifts against the fade. Semis: 0, 3, 7, 12 → rates by 2^(s/12).
    const arpSemis = [0, 3, 7, 12, 0, 3, 7, 12, 15, 19, 12, 7, 3, 12, 7, 0];
    const arpRate = (idx) => Math.pow(2, arpSemis[idx % arpSemis.length] / 12);
    let arpIdx = 0;
    for (let barIdx = 0; barIdx < wallPat.length; barIdx++) {
      const pat = wallPat[barIdx];
      for (let i = 0; i < pat.length; i++) {
        const ch = pat[i];
        if (ch === "." || ch === " ") continue;
        const stepT = stT + barIdx * SPBAR + i * (SPB / 4);
        const ws = 0.65;
        const fade = wallFade(barIdx, i);
        const r = arpRate(arpIdx);
        arpIdx++;
        if (ch === "B") {
          if (drums.hit1)  playSample(stepT + hum(0.003), drums.hit1,  0.88 * fade,
            { rate: r, pan: -0.15 + hum(0.05), wetSend: ws });
          if (drums.low1)  playSample(stepT + hum(0.005), drums.low1,  0.82 * fade,
            { rate: r, pan:  0.15 + hum(0.05), wetSend: ws });
          if (drums.scrB1) playSample(stepT + 0.012 + hum(0.004), drums.scrB1, 0.72 * fade,
            { rate: r, pan: -0.55 + hum(0.06), wetSend: ws + 0.10 });
          if (drums.scrA2) playSample(stepT + 0.014 + hum(0.004), drums.scrA2, 0.72 * fade,
            { rate: r, pan:  0.55 + hum(0.06), wetSend: ws + 0.10 });
          hits += 4;
        } else if (ch === "K") {
          if (drums.low2)  playSample(stepT + hum(0.004), drums.low2,  0.80 * fade,
            { rate: r, pan:  0.20 + hum(0.05), wetSend: ws });
          if (drums.scrA2) playSample(stepT + 0.010 + hum(0.003), drums.scrA2, 0.68 * fade,
            { rate: r, pan: -0.45 + hum(0.06), wetSend: ws + 0.05 });
          hits += 2;
        } else if (ch === "h") {
          if (drums.hit2)  playSample(stepT + hum(0.003), drums.hit2,  0.62 * fade,
            { rate: r, pan: hum(0.35), wetSend: ws });
          hits++;
        } else if (ch === "t") {
          const sc = (i % 2 === 0 ? drums.scrB1 : drums.scrA2)
                  || drums.scrA2 || drums.scrB1;
          if (sc) playSample(stepT + hum(0.003), sc, 0.52 * fade,
            { rate: r, pan: (i % 2 === 0 ? -0.42 : 0.42) + hum(0.05), wetSend: ws });
          hits++;
        }
      }
    }
    // Star Wars blaster — two-part flanged signature:
    //   1) SLOW REVERSE riser leading INTO the drop. Plays a reversed +
    //      flanged copy at rate 0.45 so it lasts ~3.7 s and ENDS exactly
    //      at stT. Reversing the natural decay→attack shape creates a
    //      built-in crescendo into the kick.
    //   2) ONE FORWARD hit, flanged, slightly faster (rate 1.2), dead-
    //      center on the drop, less wet so it punches.
    // Flange = feedforward comb filter modulated by a slow cos LFO —
    // depth 5 ms, slow 0.3 Hz on the reverse (long swoosh) / 1.2 Hz on
    // the forward (faster shimmer). @jeffrey: "a bit flanged out".
    const flange = (buf, depthMs, rateHz, mix) => {
      const n = buf.length;
      const out = new Float32Array(n);
      const dMax = depthMs * SR / 1000;
      let peak = 0;
      for (let i = 0; i < n; i++) {
        const lfo = (1 - Math.cos(2 * Math.PI * rateHz * i / SR)) * 0.5; // 0..1
        const d = dMax * lfo + 1;
        const di = Math.floor(d);
        const f = d - di;
        const i0 = i - di, i1 = i0 - 1;
        const s0 = i0 >= 0 ? buf[i0] : 0;
        const s1 = i1 >= 0 ? buf[i1] : 0;
        const delayed = s0 * (1 - f) + s1 * f;
        out[i] = buf[i] + mix * delayed;
        const a = Math.abs(out[i]);
        if (a > peak) peak = a;
      }
      if (peak > 1) for (let i = 0; i < n; i++) out[i] /= peak;
      return out;
    };
    let blasterHits = 0;
    if (blasterBuf) {
      const blasterRev = new Float32Array(blasterBuf.length);
      for (let i = 0; i < blasterBuf.length; i++) {
        blasterRev[i] = blasterBuf[blasterBuf.length - 1 - i];
      }
      const revFlanged = flange(blasterRev, 6, 0.3, 0.7);  // slow deep flange on the riser
      const fwdFlanged = flange(blasterBuf, 4, 1.2, 0.6);  // faster shimmer on the hit
      const revRate = 0.45;
      const revDur = revFlanged.length / SR / revRate;
      playSample(stT - revDur + hum(0.004), revFlanged, 0.60,
        { rate: revRate, pan: 0.0, wetSend: 0.85, fade: 0.050 });
      blasterHits++;
      playSample(stT + hum(0.004), fwdFlanged, 0.85,
        { rate: 1.2, pan: 0.0, wetSend: 0.55 });
      blasterHits++;
    }
    // ── Cards 3rd layer — staccato flap fills syncopated against the
    // downbeat slams. The high-crest burst/snap takes (cards-hard 19.4 dB
    // crest, cards-burst-b 22.3 dB, cards-burst-a 31 dB) sit ABOVE the
    // sustained sine voices and BETWEEN the kick holes, providing the
    // staccato flapping texture without masking either. Each hit is
    // capped via maxDurMs so the longer burst takes don't smear across
    // the next downbeat. Light pitch variation, wide pan.
    let cardHits = 0;
    if (cardCount > 0) {
      const hard  = cards["cards-hard"];
      const bA    = cards["cards-burst-a"];
      const bB    = cards["cards-burst-b"];
      const fast  = cards["cards-fast"];
      // [beatOffset, buf, rate, pan, gain, wet, maxMs] — beats 0..24 over 6 bars
      const cardPlan = [
        [ 0.50, hard, 1.00,  0.00, 0.62, 0.50, 220 ], // bar1 1.5 — snap
        [ 1.50, bA,   1.15, -0.55, 0.55, 0.55, 350 ], // bar1 2&
        [ 2.75, bB,   1.00,  0.55, 0.58, 0.55, 380 ], // bar1 3.75
        [ 3.50, hard, 1.40, -0.30, 0.60, 0.50, 200 ], // bar1 4&
        [ 4.50, bB,   0.90,  0.45, 0.55, 0.55, 480 ], // bar2 1.5
        [ 5.75, fast, 1.10, -0.50, 0.55, 0.55, 550 ], // bar2 2.75
        [ 6.50, hard, 1.00,  0.20, 0.62, 0.50, 240 ], // bar2 b3.5
        [ 7.50, bA,   1.30, -0.45, 0.55, 0.55, 400 ], // bar2 4.5
        [ 8.50, bB,   1.00,  0.50, 0.55, 0.55, 480 ], // bar3 1.5
        [ 9.75, hard, 1.20,  0.00, 0.65, 0.50, 220 ], // bar3 2.75
        [10.50, bA,   0.95, -0.30, 0.55, 0.55, 400 ], // bar3 b3.5
        [11.50, bB,   1.40,  0.40, 0.55, 0.60, 460 ], // bar3 4.5
        [12.50, fast, 1.00, -0.35, 0.55, 0.55, 650 ], // bar4 1.5
        [13.75, hard, 1.50,  0.30, 0.60, 0.50, 200 ], // bar4 2.75
        [14.50, bB,   1.10,  0.00, 0.60, 0.60, 580 ], // bar4 b3.5
        [16.50, hard, 1.30, -0.40, 0.50, 0.55, 220 ], // bar5 1.5 — quieter
        [17.75, bA,   1.20,  0.50, 0.45, 0.60, 380 ], // bar5 2.75
        [19.50, bB,   1.10, -0.30, 0.45, 0.65, 460 ], // bar5 4.5
        [20.50, hard, 1.60,  0.30, 0.45, 0.65, 200 ], // bar6 1.5
        [22.50, bA,   1.40,  0.00, 0.40, 0.70, 380 ], // bar6 3.5 — last whisper
      ];
      // Same exponential fade as the drums so they tail together.
      const cardFadeBeats = (beat) => Math.exp(-beat / 8.0);
      for (const [beat, buf, rate, pan, gain, wet, ms] of cardPlan) {
        if (!buf) continue;
        const f = cardFadeBeats(beat);
        playSampleSwept(stT + beat * SPB + hum(0.006), buf, gain * f, {
          startRate: rate, endRate: rate, pan: pan + hum(0.04),
          wetSend: wet, maxDurMs: ms, fade: 0.012,
        });
        cardHits++;
      }
    }
    console.log(`→ statement WALL OF SOUND · ${hits} drum hits across 6 bars (fading)` +
      (blasterHits ? ` + ${blasterHits} pitched blaster ornaments` : ` (no blaster sample at ${BLASTER_PATH.replace(process.env.HOME, "~")})`) +
      (cardHits ? ` + ${cardHits} card flaps (fading)` : ``));
  }

  // ── Pre-drop voice stamps — "aesthetic dot computer" then "I need you"
  // land right before the climax drop. "I need you" is pitched UP (rate
  // 1.22, brighter) and glitch-stuttered into existence; the AC stamp
  // is clean + dry-ish so the words read.
  const INEED_PATH = resolve(HERE, "../samples/i-need-you.wav");
  const ACSTAMP_PATH = resolve(HERE, "../samples/aesthetic-dot-computer.wav");
  const climaxSec = sectionRanges.find((s) => s.name === "climax");
  if (climaxSec) {
    const cT = climaxSec.startSec;
    if (existsSync(ACSTAMP_PATH)) {
      const acBuf = loadWavMono(ACSTAMP_PATH);
      // Stamp is pitched UP (rate 1.18 ≈ +2.9 semis) + harmonized at
      // a 5th and an octave + heavy reverb send + a slight slow main
      // body via 0.92× rate twin underneath. Triadic harmony stack.
      const stampT = cT - 3.5;
      // Main: pitched up — restored to original mix-friendly gain
      playSample(stampT,          acBuf, 0.45,
        { rate: 1.18, pan: 0.0,  wetSend: 0.75 });
      // Harmony — perfect fifth up (×1.5 freq = ×1.5 rate) at half gain
      playSample(stampT + 0.025,  acBuf, 0.28,
        { rate: 1.18 * 1.5, pan: -0.35, wetSend: 0.85 });
      // Harmony — octave up at lower gain (×2 rate)
      playSample(stampT + 0.045,  acBuf, 0.18,
        { rate: 1.18 * 2.0, pan: 0.35, wetSend: 0.85 });
      // Slow body underneath — adds weight + slight smear (rate 0.78)
      playSample(stampT + 0.060,  acBuf, 0.22,
        { rate: 0.78, pan: 0.0, wetSend: 0.90 });
      console.log(`→ "aesthetic dot computer" stamp at ${stampT.toFixed(2)}s · pitched+harmonized+wet`);
    }
    if (existsSync(INEED_PATH)) {
      const ineed = loadWavMono(INEED_PATH);
      const startT = cT - 1.5;          // glitch begins 1.5s before drop
      const glitchN = 3, glitchStep = 0.080;
      for (let g = 0; g < glitchN; g++) {
        playSampleSwept(startT + g * glitchStep, ineed, 0.34, {
          startRate: 1.10, endRate: 1.10,
          maxDurMs: 90,
          pan: g % 2 === 0 ? -0.28 : 0.28,
          wetSend: 0.55,
        });
      }
      const mainT = startT + glitchN * glitchStep;
      // Higher-pitched main playthrough — rate 1.22 ≈ +3.5 semitones.
      playSample(mainT, ineed, 0.55,
        { rate: 1.22, pan: 0.0, wetSend: 0.65 });
      const ineedEchoes = [
        { dt: 0.40, g: 0.30, pan: -0.45 },
        { dt: 0.85, g: 0.18, pan:  0.45 },
      ];
      for (const e of ineedEchoes) {
        playSample(mainT + e.dt, ineed, 0.55 * e.g,
          { rate: 1.22, pan: e.pan, wetSend: 0.85 });
      }
      console.log(`→ "I need you" higher-pitched at ${startT.toFixed(2)}s (pre-climax drop)`);
    }
  }
}

// ── shake pool — ULTIMATE only. cut-shakes.mjs slices recorded rattle
// takes into samples/shakes/shake-NN-DDDDms.wav (sorted by length).
// We bucket by length and rotate through medium-to-long shakes for the
// pickup-into-downbeat slot in the first 8 bars of statement. Falls
// back to the single rattle-intro.wav if shakes/ is empty.
const SHAKES_DIR = resolve(HERE, "../samples/shakes");
const RATTLE_INTRO_PATH = flags["rattle-intro-path"] ||
  resolve(HERE, "../samples/rattle-intro.wav");
const RATTLE_INTRO_GAIN = Number(flags["rattle-intro-gain"] ?? 0.48);
if (ULTIMATE) {
  let pool = [];
  if (existsSync(SHAKES_DIR)) {
    const files = readdirSync(SHAKES_DIR)
      .filter((f) => /^shake-\d+-(\d+)ms\.wav$/.test(f))
      .map((f) => {
        const m = f.match(/^shake-\d+-(\d+)ms\.wav$/);
        return { path: resolve(SHAKES_DIR, f), durMs: Number(m[1]) };
      });
    // Pickup wants meat — medium-to-long shakes only (≥300 ms). Falls back
    // to anything ≥150 ms if the pool is sparse.
    let picks = files.filter((x) => x.durMs >= 300);
    if (picks.length < 4) picks = files.filter((x) => x.durMs >= 150);
    pool = picks.map((x) => ({ ...x, buf: loadWavMono(x.path) }));
  }
  if (pool.length) {
    // Kick-triggered rattle warps — each kick fires 1-2 rattles that
    // SWEEP in pitch (start brighter, end deeply pitched-down) over a
    // window capped to the kick's body length (~140 ms). The rattle is
    // contained inside the kick's "hole" instead of trailing past it.
    // Incremental fade-in across statement, full through develop+climax,
    // fade-out across coda.
    const ballPool  = pool.filter((x) => x.durMs <= 250);
    const sweepPool = pool.filter((x) => x.durMs >= 400);
    const ball  = ballPool.length  ? ballPool  : pool;
    const sweep = sweepPool.length ? sweepPool : pool;
    const tRng = makeRng("hellsine-rattle-warp-" + SEED_STR);
    const semiRate = (s) => Math.pow(2, s / 12);
    // Per-section sweep — startSemi → endSemi over the rattle's body.
    // overture: kicks already absent here so no rattles fire. statement
    // ramps in. climax is the brightest, with a wider sweep span.
    const SEC_RATTLE = {
      // Pitched DOWN harder + longer (was bottoming around -7 to -19, now
      // -22 to -28) so the rattles read as deep WHOOMP swooshes rather
      // than mid-range clicks.
      overture:  { startSemi:  -5, endSemi: -26, n: 1, gain: 0.20, maxMs: 180 },
      statement: { startSemi:  -2, endSemi: -24, n: 1, gain: 0.34, maxMs: 180 },
      bridge:    { startSemi:   0, endSemi: -22, n: 1, gain: 0.30, maxMs: 180 },
      develop:   { startSemi:   2, endSemi: -22, n: 2, gain: 0.40, maxMs: 170 },
      climax:    { startSemi:   4, endSemi: -20, n: 2, gain: 0.46, maxMs: 170 },
      coda:      { startSemi:  -2, endSemi: -24, n: 1, gain: 0.26, maxMs: 190 },
    };
    const sectionFor = (t) => {
      for (const sec of sectionRanges) {
        if (t >= sec.startSec && t < sec.endSec) return sec;
      }
      return sectionRanges.find((s) => s.name === "statement");
    };
    // Incremental ramp factor per kick — fades in across statement,
    // stays at 1.0 through develop+climax, fades out across coda.
    const ramp = (sec, t) => {
      const local = (t - sec.startSec) / (sec.endSec - sec.startSec);
      switch (sec.name) {
        case "statement": return 0.30 + 0.70 * local;
        // Coda used to drop to 0.35 by end — too fast a fall-off; the
        // hi-hat ticks already fade across coda and they were the ONLY
        // perc still going. Hold the rattle warps higher and longer so
        // the train's pitch-down outro keeps some percussive shimmer.
        case "coda":      return 1.0 - 0.30 * local;
        default:          return 1.0;
      }
    };
    let warps = 0;
    for (const ke of kickEvents) {
      const t = ke.t;
      const sec = sectionFor(t);
      const cfg = SEC_RATTLE[sec.name] || SEC_RATTLE.statement;
      const r = ramp(sec, t);
      const baseG = cfg.gain * RATTLE_INTRO_GAIN * 1.8 * r;
      for (let i = 0; i < cfg.n; i++) {
        const shake = ball[Math.floor(tRng() * ball.length)];
        // per-rattle pitch jitter on the start + end of the sweep
        const startR = semiRate(cfg.startSemi + (tRng() * 2 - 1) * 1.5);
        const endR   = semiRate(cfg.endSemi   + (tRng() * 2 - 1) * 1.5);
        const offset = i === 0 ? 0.002 : 0.018;
        const pan = ((i & 1) ? 1 : -1) * (0.40 + tRng() * 0.35);
        const g = baseG * (0.80 + tRng() * 0.30);
        playSampleSwept(t + offset, shake.buf, g, {
          startRate: startR, endRate: endR,
          maxDurMs: cfg.maxMs + Math.floor(tRng() * 20),
          pan, wetSend: 0.95,                             // deeper reverb wash (was 0.55)
        });
        warps++;
      }
      // climax: every 4th kick gets a deeply-warped long sweep that
      // smears underneath — fills the "hole" with a drone gust.
      if (sweep.length && sec.name === "climax" && warps % 16 === 0) {
        const sw = sweep[Math.floor(tRng() * sweep.length)];
        playSampleSwept(t + 0.005, sw.buf, baseG * 0.55, {
          startRate: semiRate(-2),
          endRate:   semiRate(-22),
          maxDurMs:  280,
          pan: (tRng() * 2 - 1) * 0.5,
          wetSend: 1.0,
        });
        warps++;
      }
    }
    console.log(`→ kick rattle warps · ${warps} swept rattles inside ${kickEvents.length} kick bodies · pool ${pool.length} (${ball.length} short, ${sweep.length} long)`);

    // ── PERC ACCENT @ master 2:04 — 2-bar window where extra percussion
    // is LAYERED ON TOP of the climax mix. The earlier implementation
    // *ducked* the mix to spotlight the perc, but @jeffrey heard every
    // duck as a glitch ("weird dip", "whole track goes down"). Now we
    // just play the perc on top — the existing climax bed stays at full
    // level, the new perc adds energy at master 2:04-2:06.
    {
      const breakStart = 124.80;
      const breakEnd   = 127.45;
      // Now write fresh percussion ON TOP of the FULL mix — kicks on
      // every beat, ticks + woodTicks for groove, chord-tone subs for
      // weight. Pure perc, no melody.
      const breakSPBAR = SPBAR;
      const breakSPB   = SPB;
      let breakKicks = 0;
      for (let bar = 0; bar < 2; bar++) {
        const tBar = breakStart + bar * breakSPBAR;
        // Kick on beats 1 + 3 (halftime)
        kick(tBar,                      HELL * 1.10, 0.95, 0.30);
        kick(tBar + 2 * breakSPB,       HELL * 1.10, 0.95, 0.30);
        // Snare-y open hat tick on beats 2 + 4
        tick(tBar + 1 * breakSPB,       0.45, true);
        tick(tBar + 3 * breakSPB,       0.45, true);
        // 16th-note closed-hat shaker pattern
        for (let s = 0; s < 16; s++) {
          if (s % 4 !== 0) tick(tBar + s * (breakSPB / 4), 0.16, false);
        }
        // Chord-tone sub on the downbeat — gives the break some root
        const chordIdx = (10 + bar) % 8;
        const ch = CHORD[["Dm","Dm","Bb","Bb","F","F","C","C"][chordIdx]];
        if (ch) {
          const tr14 = 14;
          sub(tBar, 0.30, ch.root + tr14 - 12, 0.55);
        }
        // Wood-block tick polyrhythm (3-against-4) for spice
        for (let p = 0; p < 3; p++) {
          woodTick(tBar + (p * 4 / 3) * breakSPB, 0.18);
        }
        breakKicks += 2;
      }
      console.log(`→ PERC ACCENT · 2 bars at ${breakStart.toFixed(2)}-${breakEnd.toFixed(2)}s (master 2:04) · additive ${breakKicks} kicks + hats + sub on top of climax`);
    }

    // ── POST-2:00 ADVENTURE — additive only. The earlier draft tried
    // stutter-gate + half-bar duck + reverse-swell ducks, but every
    // duck-based gesture read as an audio error to @jeffrey ("weird
    // dip", "the whole track goes down"). Adventure now comes from
    // ADDED percussion + sub punches LAYERED ON TOP of the climax mix,
    // not from carving holes in it. Two accent points: an extra kick
    // burst @ engine 121, a sub-sweep + woodtick triplet @ engine 131.
    {
      // (A) KICK BURST @ engine 120.5 — 4 fast kicks in 0.5 s, a
      // "stuttering on top of the climax" feel WITHOUT muting anything.
      const kbT = 120.50;
      for (let s = 0; s < 4; s++) {
        kick(kbT + s * (SPB / 4), HELL * 0.95, 0.78, 0.28);
      }

      // (B) SUB PUNCH + TICK TRIPLET @ engine 131.0 — a sub punch on D2
      // plus three rapid woodticks across 0.5 s. Adds weight + spice
      // without ducking.
      const pB_start = 131.00;
      kick(pB_start,                HELL * 1.10, 0.92, 0.30);
      sub(pB_start,                 0.40, 38, 0.55);
      woodTick(pB_start + 0.10,     0.30);
      woodTick(pB_start + 0.22,     0.28);
      woodTick(pB_start + 0.36,     0.26);
      tick(pB_start + 0.48,         0.45, true);

      console.log(`→ POST-2:00 adventure · kick burst @ ${kbT.toFixed(2)}s + sub+tick punch @ ${pB_start.toFixed(2)}s (additive, no ducking)`);
    }

    // ── Schroeder reverb on the shake-wet bus → mix back into L/Rb ─────
    // 4 parallel feedback combs + 2 series allpasses per channel, with
    // slightly different delays per side for stereo width. Tail ~2 s.
    const COMB_L = [0.0297, 0.0371, 0.0411, 0.0437];
    const COMB_R = [0.0307, 0.0381, 0.0421, 0.0447];
    const COMB_FB = 0.84;
    const AP_D = [0.005, 0.0017];
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
    const WET_MIX = 0.42;
    // Time-varying wet mix — reverb fades in gradually across the entire
    // intro starting at t=0, so even the opening cards roll has soft-room
    // bloom. Hits full WET_MIX right at the drop.
    const dropSec = sectionRanges.find((s) => s.name === "statement")?.startSec ?? 16.0;
    const revFadeStart = 0.0;
    const revFadeEnd   = dropSec;
    for (let i = 0; i < N; i++) {
      const t = i / SR;
      let wet;
      if (t < revFadeStart) wet = 0;
      else if (t < revFadeEnd) wet = WET_MIX * (t - revFadeStart) / (revFadeEnd - revFadeStart);
      else wet = WET_MIX;
      L[i]  += wetL[i] * wet;
      Rb[i] += wetR[i] * wet;
    }
    console.log(`→ shake reverb · Schroeder 4-comb + 2-allpass · wet ${WET_MIX} (dry 0→${revFadeStart.toFixed(1)}s, fade-in to ${revFadeEnd.toFixed(1)}s)`);
  } else if (existsSync(RATTLE_INTRO_PATH)) {
    // Fallback: legacy single-file rattle-intro
    const rat = loadWavMono(RATTLE_INTRO_PATH);
    let placed = 0;
    if (statement) {
      for (let b = statement.startBar; b < statement.startBar + 8; b++) {
        const tBar = b * SPBAR;
        const fadeIn = Math.min(1, (b - statement.startBar) / 6);
        playSample(tBar + 3.5 * SPB + hum(0.004), rat,
          RATTLE_INTRO_GAIN * (0.55 + 0.45 * fadeIn),
          { rate: 1 + hum(0.025), pan: hum(0.3) });
        placed++;
      }
    }
    console.log(`→ rattle-intro · ${placed} hits across statement bars 0-7 (single-file fallback)`);
  } else {
    console.log(`· rattle · no shakes/ pool and no rattle-intro.wav — intro perc empty`);
  }
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
