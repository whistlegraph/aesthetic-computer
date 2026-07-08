#!/usr/bin/env node
// render-flatterbop180.mjs — FLATTERBOP180 COMPOSITION + SCORE BAKER.
//
// fluttabap360's fast, high-energy little cousin: HALF the length, a NEW time
// signature, and a ZOO. The canonical renderer is the C engine
// (pop/marimba/c/flatterbop180.c — the fluttabap orchestra + a sampled zoo
// voice); this file COMPOSES the piece and BAKES the score. The JS mixdown
// below is reference/A-B only, and it cannot hear the zoo (zoo is bake-only,
// like flute/fembell/gong).
//
// Canonical render (album cut + seamless loop + DistroKid wav):
//   node bin/render-flatterbop180.mjs --bake out/flatterbop180.score.txt
//   node c/run-c.mjs out/flatterbop180.score.txt --engine flatterbop180 --out out/flatterbop180.mp3 --wav
//   node c/run-c.mjs out/flatterbop180.score.txt --engine flatterbop180 --out out/flatterbop180-loop.mp3 --loop --wav
//
// THE NUMEROLOGY — everything is 180:
//   • exactly 180 s (3:00.000, hard-clamped like the 360's 6:00)
//   • 3/4 time (fluttabap is 4/4 — this one WALTZES, fast)
//   • 180 BPM → one bar of 3/4 = exactly ONE SECOND → the form is 180 bars
//
// Same butterfly-park DNA, re-phrased: the flutterbap melodic blocks
// (butterfly / pal-of-mine / mommy-wow / slinky / fly / land) are re-sung in
// 3 — same contours, new rhythm — over a driving stomp-waltz kit. The key
// journey climbs C → E♭ → (home cave breath) → F → G. And the park got
// a ZOO: freshly-sourced animal one-shots (bin/fetch-zoo.mjs, CC0) answer the
// scratch calls in the menagerie rides, a lion prowls the cave, and a parrot
// gets the last word.
//
// Run:
//   node pop/marimba/bin/render-flatterbop180.mjs            # JS reference mix
//   node pop/marimba/bin/render-flatterbop180.mjs --bake out/flatterbop180.score.txt

import { mixEventMarimba, MARIMBA_PRESETS } from "../synths/marimba.mjs";
import { mixKick, mixBassPerc, mixSnare, mixHat, mixShaker, mixReverseBell, mixReverseKick, mixScratch, mixScream, renderHat } from "../synths/perc.mjs";
import { writeFileSync, mkdirSync, unlinkSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";
import { satParams } from "../../lib/substrate.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;
const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };

// the SUBSTRATE — the medium the track is printed onto (tape / vinyl / …).
const SUBSTRATE = _argi("--substrate") || "tape";
const SAT = satParams(SUBSTRATE);

// ── note table ────────────────────────────────────────────────────────
const N = {
  C1: 24, E1: 28, F1: 29, G1: 31, A1: 33,
  C2: 36, D2: 38, E2: 40, F2: 41, G2: 43, A2: 45,
  C3: 48, E3: 52, F3: 53, G3: 55, A3: 57, B3: 59,
  C4: 60, D4: 62, E4: 64, F4: 65, G4: 67, A4: 69, B4: 71,
  C5: 72, D5: 74, E5: 76, F5: 77, G5: 79, A5: 81, B5: 83,
  C6: 84, D6: 86, E6: 88, F6: 89, G6: 91,
};
const mf = (m) => 440 * Math.pow(2, (m - 69) / 12);

// ── KEY — global transpose in semitones (the dispatch sets this per section).
// C → E♭ → (home) → F → G across the three minutes. ────────────────────────
let gKey = 0;
const TR = (m) => m + gKey;

// ── note-event telemetry (visualizer lanes, same as fluttabap360) ──────────
const NOTE_EVENTS = { rosewood: [], vibraphone: [], kalimba: [], bass: [], bubbles: [] };
function EV(lane, t0, midi, beats) {
  NOTE_EVENTS[lane].push({ t: +t0.toFixed(4), midi: TR(midi), dur: +(beats * BEAT).toFixed(4) });
}

// ══════════════════════════════════════════════════════════════════════
//  ARRANGEMENT — 3/4 bars. MUST sum to 180 so that at 180 BPM the form is
//  exactly 180 s (1 bar = 1 s). opts: { lvl, ride, gMul, key, zoo }.
// ══════════════════════════════════════════════════════════════════════
const ARRANGEMENT = [
  ["intro", 10, { key: 0 }],                                          // zoo-park dawn, fast
  // ── PASS A — establish the bop in C ──
  ["butterfly", 8, { lvl: 0 }], ["palofmine", 8, { lvl: 1 }], ["mommywow", 6],
  ["slinky", 8, { lvl: 1 }],    ["fly", 8, { lvl: 2 }],
  ["ride", 8, { lvl: 2 }],                                            // menagerie fight I
  // ── PASS B — busier — LIFT to E♭ (+3, a darker shimmer the 360 never visits) ──
  ["butterfly", 8, { lvl: 1, ride: true, key: 3 }], ["palofmine", 8, { lvl: 1, ride: true }],
  ["slinky", 8, { lvl: 2 }],   ["fly", 8, { lvl: 3 }],
  ["ride", 8, { lvl: 3 }],                                            // menagerie fight II
  ["cave", 8, { key: 0 }],                                            // home breath — the lion's den
  // ── PASS C — brightest — LIFT to F (+5) ──
  ["butterfly", 8, { lvl: 2, ride: true, key: 5 }], ["mommywow", 6], ["fly", 8, { lvl: 3 }],
  ["ride", 8, { lvl: 3 }],                                            // menagerie fight III
  // FINAL CHORUS — euphoric jump up to G (+7, the dominant), the whole zoo out
  ["butterfly", 8, { lvl: 2, ride: true, key: 7 }], ["palofmine", 8, { lvl: 2, ride: true }],
  ["slinky", 8, { lvl: 2 }],   ["fly", 8, { lvl: 3, zoo: true }],
  ["progression", 8], ["land", 4], ["button", 2],
];

const TOTAL_BARS = ARRANGEMENT.reduce((s, [, bars]) => s + bars, 0);
const TARGET_SEC = Number(_argi("--sec")) || 180;
// back-solve BPM so TOTAL_BARS bars of 3/4 == TARGET_SEC. With the canonical
// 180-bar form this lands EXACTLY on 180 BPM (the numerology holds itself up).
const BPM = Number(_argi("--bpm")) || (TOTAL_BARS * 3 * 60) / TARGET_SEC;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT;                       // ← 3/4: THE time-signature change
const totalSec = TOTAL_BARS * BAR + 3.0;    // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// ── loop mode / raw keep (same as fluttabap360) ────────────────────────
const LOOP = process.argv.includes("--loop");
const KEEP_RAW = process.argv.includes("--keep-raw");

// ── C-ENGINE BAKE ──────────────────────────────────────────────────────
const BAKE_PATH = _argi("--bake");
const BAKING = BAKE_PATH != null;
const SOLO = (() => { const v = _argi("--solo"); return v ? new Set(v.split(",").map((s) => s.trim())) : null; })();
const soloed = (name) => !SOLO || SOLO.has(name);
const SCORE = [];
function rec(line) { if (BAKING) SCORE.push(line); }

function recMarimba(lane, ev, opts) {
  if (!BAKING) return;
  const P = { ...(MARIMBA_PRESETS[ev.preset] || MARIMBA_PRESETS.rosewood), ...(opts.params || {}) };
  const attack = opts.attack ?? P.attack ?? 0.0005;
  const release = opts.decay ?? P.decay ?? 0.2;
  const decayMul = opts.decayMul ?? ev.decayMul ?? 1.0;
  rec(`marimba ${lane} ${ev.startSec.toFixed(8)} ${ev.midi} ${ev.durSec.toFixed(8)} ${ev.gain.toFixed(6)} `
    + `${decayMul} ${attack} ${release} ${P.mallet ?? 0.002} ${P.resQ ?? 18} ${P.resGain ?? 0.6} `
    + `${P.strike ?? 0.5} ${P.noise ?? 0} ${P.tremHz ?? 0} ${P.tremDepth ?? 0} `
    + `${P.partials.length} ${P.partials.join(" ")} ${P.amps.join(" ")} ${P.decays.join(" ")}`);
}

function recMarimbaBus(tag, ev) {
  if (!BAKING) return;
  const P = MARIMBA_PRESETS[ev.preset] || MARIMBA_PRESETS.rosewood;
  const attack = P.attack ?? 0.0005;
  const release = P.decay ?? 0.2;
  const decayMul = ev.decayMul ?? 1.0;
  rec(`${tag} ${ev.startSec.toFixed(8)} ${ev.midi} ${ev.durSec.toFixed(8)} ${ev.gain.toFixed(6)} `
    + `${decayMul} ${attack} ${release} ${P.mallet ?? 0.002} ${P.resQ ?? 18} ${P.resGain ?? 0.6} `
    + `${P.strike ?? 0.5} ${P.noise ?? 0} ${P.tremHz ?? 0} ${P.tremDepth ?? 0} `
    + `${P.partials.length} ${P.partials.join(" ")} ${P.amps.join(" ")} ${P.decays.join(" ")}`);
}

// ── swing — DEEP (0.66): the techno pulse holds the floor dead straight on
// the beats while everything between them lopes hard. ──
const SWING = Math.min(0.72, Math.max(0.5, Number(_argi("--swing")) || 0.66));
function swingBeat(x) {
  const q = Math.floor(x + 1e-9), r = x - q;
  return q + (r < 0.5 ? r * 2 * SWING : SWING + (r - 0.5) * 2 * (1 - SWING));
}
const t = (bar, beat = 0) => bar * BAR + swingBeat(beat) * BEAT;

// ── humanize ─────────────────────────────────────────────────────────
const HUMANIZE = !process.argv.includes("--robotic");
let _hs = 0x1a2b3c4d >>> 0;
function hrand() {
  _hs = (_hs + 0x6d2b79f5) >>> 0;
  let x = Math.imul(_hs ^ (_hs >>> 15), 1 | _hs);
  x = (x + Math.imul(x ^ (x >>> 7), 61 | x)) ^ x;
  return ((x ^ (x >>> 14)) >>> 0) / 4294967296;
}
const hr2 = () => hrand() * 2 - 1;
const jit = (t0, ms) => (HUMANIZE ? Math.max(0, t0 + hr2() * ms / 1000) : t0);
const vel = (g, amt) => (HUMANIZE ? Math.max(0, g * (1 + hr2() * amt)) : g);

// ── output buffers — one mono bus per voice (JS reference mix only) ─────
const leadBuf  = new Float32Array(ns);
const bassBuf  = new Float32Array(ns);
const sparkBuf = new Float32Array(ns);
const bellBuf  = new Float32Array(ns);
const vibBuf   = new Float32Array(ns);
const deepBuf  = new Float32Array(ns);
const riseBuf  = new Float32Array(ns);
const scrBuf   = new Float32Array(ns);
const screamBuf = new Float32Array(ns);
const kickBuf  = new Float32Array(ns);
const subBuf   = new Float32Array(ns);
const snareBuf = new Float32Array(ns);
const hatBuf   = new Float32Array(ns);
const shakBuf  = new Float32Array(ns);
const caveBuf  = new Float32Array(ns);
const padBuf   = new Float32Array(ns);
const dropBuf  = new Float32Array(ns);
const streamBuf = new Float32Array(ns);

// ── MARIMBA MORPH — the lead's physical CHARACTER evolves across the track,
// the way the dynabell's waveform does: a warm WOODEN bar in the dawn that
// turns steadily GLASSY as the form climbs, peaking vitreous in the G chorus
// and settling back for the land. Sampled per strike. ──
const MARIMBA_KEYS = [
  [0,   0.05],   // dawn — pure rosewood
  [45,  0.15],
  [88,  0.30],   // E♭ pass — the glass creeps in
  [96,  0.20],   // the den — back toward wood
  [104, 0.45],
  [126, 0.60],   // F pass — half glass
  [134, 0.90],   // FINAL G CHORUS — vitreous, ringing
  [166, 0.75],
  [180, 0.55],
];
function marimbaMorph(sec) {
  if (sec <= MARIMBA_KEYS[0][0]) return MARIMBA_KEYS[0][1];
  for (let i = 1; i < MARIMBA_KEYS.length; i++) {
    const [t1, v1] = MARIMBA_KEYS[i], [t0, v0] = MARIMBA_KEYS[i - 1];
    if (sec <= t1) return v0 + (v1 - v0) * ((sec - t0) / (t1 - t0));
  }
  return MARIMBA_KEYS[MARIMBA_KEYS.length - 1][1];
}
// ── LEAD — the PHYSICALLY-MODELED marimba (physmar in the C engine: an
// FEM-style modal bar, cousin of the fembell). Beepy: short notes get gated
// to quick bright blips; mallet hardness rides the morph. The JS reference
// mix approximates it with a snappy xylophone (physmar is C-only). ──
function lead(t0, midi, beats, gain = 1.0) {
  EV("rosewood", t0, midi, beats);
  const start = jit(t0, 24), g = vel(gain, 0.2);
  const beep = beats <= 1 ? 0.55 : 0.8;              // shorter faster beep tones
  const dur = beats * BEAT * beep;
  const morph = marimbaMorph(start);
  const strike = Math.min(1, 0.35 + 0.5 * morph + 0.1 * g);
  rec(`physmar ${start.toFixed(8)} ${mf(TR(midi)).toFixed(6)} ${dur.toFixed(8)} ${g.toFixed(6)} ${morph.toFixed(6)} ${strike.toFixed(6)}`);
  const ev = { startSec: start, midi: TR(midi), durSec: dur, gain: g, preset: "xylophone", decayMul: 0.75 };
  mixEventMarimba(ev, leadBuf, { sampleRate: SR, attack: 0.0012, params: { mallet: 0.002, noise: 0.0, amps: [1.0, 0.4, 0.18, 0.06], resGain: 0.42 } });
}
function bass(t0, midi, beats, gain = 0.85) {
  EV("bass", t0, midi, beats);
  const ev = { startSec: jit(t0, 16), midi: TR(midi), durSec: beats * BEAT, gain: vel(gain, 0.18), preset: "bass", decayMul: 0.55 };
  recMarimba("bass", ev, { sampleRate: SR });
  mixEventMarimba(ev, bassBuf, { sampleRate: SR });
}
function spark(t0, midi, beats, gain = 0.55) {
  EV("kalimba", t0, midi, beats);
  const ev = { startSec: jit(t0, 26), midi: TR(midi), durSec: beats * BEAT, gain: vel(gain, 0.24), preset: "kalimba", decayMul: 0.7 };
  recMarimba("spark", ev, { sampleRate: SR });
  mixEventMarimba(ev, sparkBuf, { sampleRate: SR });
}
// ── DYNABELL — same evolving sine→triangle→square morph, rescaled to 3:00:
// round early, reedy through the middle, ~30 s of buzzy square in the F
// chorus, eased back for the land. ──
const BELL_KEYS = [
  [0,    0.00],   // dawn + pass A — round sine bells
  [45,   0.00],
  [100,  0.50],   // pass B / C — warm triangle bells
  [130,  0.50],
  [136,  1.00],   // FINAL G CHORUS — buzzy square bells
  [162,  1.00],
  [168,  0.60],   // land / button — settle back toward triangle
  [180,  0.55],
];
function bellShape(sec) {
  if (sec <= BELL_KEYS[0][0]) return BELL_KEYS[0][1];
  for (let i = 1; i < BELL_KEYS.length; i++) {
    const [t1, v1] = BELL_KEYS[i], [t0, v0] = BELL_KEYS[i - 1];
    if (sec <= t1) return v0 + (v1 - v0) * ((sec - t0) / (t1 - t0));
  }
  return BELL_KEYS[BELL_KEYS.length - 1][1];
}
function bellSpectrum(shape, nHarm) {
  const amps = new Float64Array(nHarm + 1);
  const seg = Math.min(1, Math.max(0, shape)) * 2;
  let k = 0;
  for (let n = 1; n <= nHarm; n += 2, k++) {
    const sine = n === 1 ? 1 : 0;
    const tri = (8 / (Math.PI * Math.PI)) * (1 / (n * n)) * (k % 2 === 0 ? 1 : -1);
    const sq  = (4 / Math.PI) * (1 / n);
    amps[n] = seg <= 1 ? sine + (tri - sine) * seg : tri + (sq - tri) * (seg - 1);
  }
  let e = 0; for (let n = 1; n <= nHarm; n += 2) e += amps[n] * amps[n];
  const norm = e > 0 ? 1 / Math.sqrt(e) : 1;
  for (let n = 1; n <= nHarm; n += 2) amps[n] *= norm;
  return amps;
}
function dynabell(t0, midi, beats, gain = 0.65, opts = {}) {
  EV("bubbles", t0, midi, beats);
  const start = jit(t0, 18), g = vel(gain, 0.16);
  const f = mf(TR(midi)), dur = beats * BEAT;
  const shape = bellShape(start);
  const bend = opts.bend ?? 0, bendTime = opts.bendTime ?? 0.09;
  const vibD = opts.vib ?? 0, vibHz = opts.vibHz ?? 5.5;
  const tail = opts.tail ?? 1;
  rec(`dynabell ${start.toFixed(8)} ${f.toFixed(6)} ${dur.toFixed(8)} ${g.toFixed(6)} ${shape.toFixed(6)} `
    + `${bend.toFixed(6)} ${bendTime.toFixed(6)} ${vibD.toFixed(6)} ${vibHz.toFixed(6)} ${tail.toFixed(6)}`);
  const nHarm = Math.max(1, Math.min(31, (Math.floor((SR * 0.45) / f) | 1)));
  const amps = bellSpectrum(shape, nHarm);
  const n0 = Math.floor(start * SR);
  const len = Math.ceil((dur * 1.35 * tail + 0.08) * SR);
  const attS = Math.max(1, Math.floor(0.003 * SR));
  const relA = Math.log(900) / (dur * 1.35 * tail);
  const dt = 1 / SR;
  let bph = 0;
  for (let i = 0; i < len; i++) {
    const d = n0 + i; if (d < 0 || d >= ns) continue;
    const tsec = i * dt;
    let semis = 0;
    if (bend && tsec < bendTime) semis += bend * (1 - tsec / bendTime);
    if (vibD) semis += vibD * Math.sin(2 * Math.PI * vibHz * tsec) * Math.min(1, tsec / 0.12);
    bph += (f * Math.pow(2, semis / 12)) * dt;
    const att = i < attS ? i / attS : 1;
    let s = 0;
    for (let n = 1; n <= nHarm; n += 2) {
      const a = amps[n]; if (a === 0) continue;
      const env = Math.exp(-relA * tsec * (1 + (n - 1) * 0.08));
      s += a * env * Math.sin(2 * Math.PI * n * bph);
    }
    bellBuf[d] += s * att * g * 0.62;
  }
}
function bellRun(ab, beat0, notes, { gain = 0.5, bend = -2.0, vib = 0.12, tail = 1 } = {}) {
  for (const [bo, midi, beats] of notes)
    dynabell(t(ab, beat0 + bo), midi, beats, gain, { bend, bendTime: 0.07, vib, vibHz: 6, tail });
}
function bassBell(ab, beat, midi, beats, gain = 0.6, tail = 1.5) {
  dynabell(t(ab, beat), midi, beats, gain, { bend: -5, bendTime: 0.22, vib: 0.18, vibHz: 4.5, tail });
}
function revbell(landSec, midi, beats, gain = 0.5) {
  const f = mf(TR(midi)), dur = beats * BEAT, shape = bellShape(landSec);
  rec(`revbell ${landSec.toFixed(8)} ${f.toFixed(6)} ${dur.toFixed(8)} ${gain.toFixed(6)} ${shape.toFixed(6)}`);
  const nHarm = Math.max(1, Math.min(31, (Math.floor((SR * 0.45) / f) | 1)));
  const amps = bellSpectrum(shape, nHarm);
  const len = Math.ceil((dur * 1.35 + 0.08) * SR);
  const attS = Math.max(1, Math.floor(0.003 * SR));
  const relA = Math.log(900) / (dur * 1.35), dt = 1 / SR;
  const tmp = new Float32Array(len);
  for (let i = 0; i < len; i++) {
    const tsec = i * dt, att = i < attS ? i / attS : 1;
    let s = 0;
    for (let n = 1; n <= nHarm; n += 2) {
      const a = amps[n]; if (a === 0) continue;
      const env = Math.exp(-relA * tsec * (1 + (n - 1) * 0.08));
      s += a * env * Math.sin(2 * Math.PI * f * n * tsec);
    }
    tmp[i] = s * att * 0.62;
  }
  const endIdx = Math.floor(landSec * SR);
  for (let i = 0; i < len; i++) { const d = endIdx - i; if (d < 0 || d >= ns) continue; bellBuf[d] += tmp[i] * gain; }
}
const FEMBELL_PRESET = { handbell: 0, church: 1, bowl: 2 };
function fembell(t0, midi, gain = 0.55, preset = "handbell", tauScale = 1.0) {
  EV("bubbles", t0, midi, 4);
  const start = jit(t0, 8), f0 = mf(TR(midi));
  rec(`fembell ${start.toFixed(8)} ${f0.toFixed(6)} ${vel(gain, 0.1).toFixed(6)} ${FEMBELL_PRESET[preset] ?? 0} ${tauScale.toFixed(6)}`);
}
const FLUTE_PRESET = { flute: 0, panflute: 1 };
function flute(t0, midi, beats, gain = 0.5, { preset = "flute", attack = 0.06, release = 0.3 } = {}) {
  const start = jit(t0, 6), f0 = mf(TR(midi)), dur = beats * BEAT;
  rec(`flute ${start.toFixed(8)} ${f0.toFixed(6)} ${dur.toFixed(8)} ${vel(gain, 0.08).toFixed(6)} `
    + `${FLUTE_PRESET[preset] ?? 0} ${attack.toFixed(6)} ${release.toFixed(6)}`);
}
function gong(t0, midi, gain = 0.8, tauScale = 1.0) {
  const start = jit(t0, 6), f0 = mf(TR(midi));
  rec(`gong ${start.toFixed(8)} ${f0.toFixed(6)} ${vel(gain, 0.06).toFixed(6)} ${tauScale.toFixed(6)}`);
}
function boowoop(ab, beat, midi, beats = 2, gain = 0.55) {
  const land = t(ab, beat);
  revbell(land, midi, Math.min(beats, 2.5), gain * 0.85);
  dynabell(land, midi, beats, gain, { bend: -3, bendTime: 0.16, vib: 0.16, vibHz: 5 });
}
const bell = dynabell;
function vib(t0, midi, beats, gain = 0.4) {
  EV("vibraphone", t0, midi, beats);
  const ev = { startSec: jit(t0, 28), midi: TR(midi), durSec: beats * BEAT, gain: vel(gain, 0.16), preset: "vibraphone_off", decayMul: 1.0 };
  recMarimba("vib", ev, { sampleRate: SR });
  mixEventMarimba(ev, vibBuf, { sampleRate: SR });
}
// ── BUTTER SUB ─────────────────────────────────────────────────────────
function SUBSINE(ab, beat, midi, beats, g = 0.5) {
  const start = jit(t(ab, beat), 4);
  const f = mf(TR(midi)), dur = beats * BEAT;
  rec(`sub ${start.toFixed(8)} ${f.toFixed(6)} ${dur.toFixed(8)} ${g.toFixed(6)}`);
  const n0 = Math.floor(start * SR);
  const len = Math.ceil((dur * 1.2 + 0.05) * SR);
  const attS = Math.floor(0.008 * SR), relA = Math.log(1000) / (dur * 0.9);
  const dt = 1 / SR; let phase = 0;
  for (let i = 0; i < len; i++) {
    const d = n0 + i; if (d < 0 || d >= ns) continue;
    phase += f * dt;
    let env = Math.exp(-relA * (i / SR));
    if (i < attS) env *= i / attS;
    deepBuf[d] += Math.sin(2 * Math.PI * phase) * env * g;
  }
}

// ── HELD PAD ───────────────────────────────────────────────────────────
function PAD(ab, midis, bars, g = 0.4, attFrac = 0.42) {
  const start = t(ab, 0), dur = bars * BAR;
  rec(`pad ${start.toFixed(8)} ${dur.toFixed(8)} ${g.toFixed(6)} ${attFrac.toFixed(4)} ${midis.length} ${midis.map(TR).join(" ")}`);
  const n0 = Math.floor(start * SR);
  const len = Math.ceil((dur + 0.5) * SR);
  const att = Math.max(1, Math.floor(dur * attFrac * SR));
  const rel = Math.floor(0.7 * SR);
  const dt = 1 / SR;
  for (const m of midis) {
    const f = mf(TR(m)); let phase = 0;
    for (let i = 0; i < len; i++) {
      const d = n0 + i; if (d < 0 || d >= ns) continue;
      phase += f * dt;
      let env = 1;
      if (i < att) env = i / att; else if (i > len - rel) env = Math.max(0, (len - i) / rel);
      const s = Math.sin(2 * Math.PI * phase)
              + 0.5 * Math.sin(2 * Math.PI * phase * 1.5)
              + 0.35 * Math.sin(2 * Math.PI * phase * 0.5);
      padBuf[d] += s * env * g * 0.45;
    }
  }
}

// ── WATER DROP ─────────────────────────────────────────────────────────
function drop(t0, f0, g = 0.45, dur = 0.24) {
  rec(`drop ${t0.toFixed(8)} ${f0.toFixed(6)} ${g.toFixed(6)} ${dur.toFixed(8)}`);
  const n0 = Math.floor(t0 * SR), ln = Math.ceil(dur * SR), dt = 1 / SR;
  let ph = 0;
  for (let i = 0; i < ln; i++) {
    const d = n0 + i; if (d < 0 || d >= ns) continue;
    const u = i / ln;
    const f = f0 * (1 + 1.7 * Math.min(1, u * 3.4));
    ph += f * dt;
    let env = Math.exp(-u * 6.2);
    if (i < 20) env *= i / 20;
    const s = Math.sin(2 * Math.PI * ph) + 0.16 * Math.sin(4 * Math.PI * ph);
    dropBuf[d] += s * env * g * 0.6;
  }
  NOTE_EVENTS.bubbles.push({ t: +t0.toFixed(4), midi: Math.round(69 + 12 * Math.log2(f0 / 440)), dur: +dur.toFixed(4) });
}

// ── WATER STREAM — the park brook, unchanged (still the DNA) ────────────
function waterStream(gateAt) {
  let ws = 0x9e3779b1 >>> 0;
  const wrand = () => {
    ws = (ws + 0x6d2b79f5) >>> 0;
    let x = Math.imul(ws ^ (ws >>> 15), 1 | ws);
    x = (x + Math.imul(x ^ (x >>> 7), 61 | x)) ^ x;
    return ((x ^ (x >>> 14)) >>> 0) / 4294967296;
  };
  const noise = () => wrand() * 2 - 1;
  const fc = 950, q = 0.7;
  const f = 2 * Math.sin(Math.PI * fc / SR), damp = 1 / q;
  let lp = 0, bp = 0;
  const lfo = [
    { r: 0.07, p: 0.0 }, { r: 0.13, p: 1.7 }, { r: 0.031, p: 4.2 }, { r: 0.21, p: 2.9 },
  ];
  const NB = 5;
  const burble = [];
  for (let k = 0; k < NB; k++) burble.push({
    fc: 500 + wrand() * 1800,
    target: 500 + wrand() * 1800,
    lp: 0, bp: 0,
    amp: 0.5 + wrand() * 0.5,
    lfoR: 0.05 + wrand() * 0.25, lfoP: wrand() * 6.283,
  });
  const dt = 1 / SR;
  for (let i = 0; i < ns; i++) {
    const tt = i * dt;
    const gate = gateAt(tt);
    if (gate <= 0) continue;
    let amR = 1;
    for (const L of lfo) amR *= 0.78 + 0.22 * Math.sin(2 * Math.PI * L.r * tt + L.p);
    const n = noise();
    lp += f * bp;
    const hp = n - lp - damp * bp;
    bp += f * hp;
    let s = bp * 0.32 * amR;
    for (const v of burble) {
      v.fc += (v.target - v.fc) * 0.00006;
      if (Math.abs(v.fc - v.target) < 8) v.target = 420 + wrand() * 2000;
      const vf = 2 * Math.sin(Math.PI * Math.min(6000, v.fc) / SR);
      const vn = noise();
      v.lp += vf * v.bp;
      const vhp = vn - v.lp - 0.18 * v.bp;
      v.bp += vf * vhp;
      const pulse = 0.45 + 0.55 * Math.max(0, Math.sin(2 * Math.PI * v.lfoR * tt + v.lfoP));
      s += v.bp * 0.16 * v.amp * pulse;
    }
    streamBuf[i] += s * gate * 0.5;
  }
  const HZ = [880, 1180, 1480, 990, 1320, 760, 1620, 1040];
  let hi = 0;
  for (let tt = 1.0; tt < ns / SR - 1.0; ) {
    tt += 0.18 + wrand() * 1.4;
    const g = gateAt(tt);
    if (g > 0.15 && wrand() < g) {
      const f0 = HZ[hi++ % HZ.length] * (1 + (wrand() * 2 - 1) * 0.06);
      const pg = (0.06 + 0.10 * g) * g, pdur = 0.14 + wrand() * 0.10;
      const n0 = Math.floor(tt * SR), ln = Math.ceil(pdur * SR);
      let ph = 0;
      for (let j = 0; j < ln; j++) {
        const d = n0 + j; if (d < 0 || d >= ns) continue;
        const u = j / ln;
        const pf = f0 * (1 + 1.7 * Math.min(1, u * 3.4));
        ph += pf * dt;
        let env = Math.exp(-u * 6.4);
        if (j < 20) env *= j / 20;
        streamBuf[d] += (Math.sin(2 * Math.PI * ph) + 0.16 * Math.sin(4 * Math.PI * ph)) * env * pg * 0.6;
      }
    }
  }
}
function plink(t0, midi, beats, g = 0.5) {
  EV("bubbles", t0, midi, beats);
  const ev = { startSec: t0, midi: TR(midi), durSec: beats * BEAT, gain: g, preset: "glockenspiel", decayMul: 4.2 };
  recMarimbaBus("plink", ev);
  mixEventMarimba(ev, dropBuf, { sampleRate: SR });
}

// ── ZOO — the fresh-sourced animal one-shots (bake-only: the C engine is the
// only renderer that can play them; the JS reference mix stays animal-free).
// Roles map to slots registered as `zoosample <idx> <path>` in the header.
// `rate` repitches (2^(semis/12)); `pan` places the call in the stereo park. ──
const ZOO_ROLES = ["monkey", "parrot", "elephant", "sealion", "lion", "hyena"];
const ZOO_DIR = resolve(HERE, "..", "samples", "zoo");
const ZOO_MANIFEST = (() => {
  try { return JSON.parse(readFileSync(resolve(ZOO_DIR, "zoo.json"), "utf8")); }
  catch { return {}; }
})();
for (const r of ZOO_ROLES) if (!ZOO_MANIFEST[r])
  console.warn(`! zoo role '${r}' missing from ${ZOO_DIR}/zoo.json — run bin/fetch-zoo.mjs ${r}`);
const zooSemis = (s) => Math.pow(2, s / 12);
function zoo(t0, role, gain = 0.4, { rate = 1, pan = 0, jitMs = 6 } = {}) {
  const idx = ZOO_ROLES.indexOf(role);
  if (idx < 0) { console.warn(`! zoo: unknown role '${role}'`); return; }
  rec(`zoo ${jit(t0, jitMs).toFixed(8)} ${idx} ${vel(gain, 0.1).toFixed(6)} ${rate.toFixed(6)} ${pan.toFixed(6)}`);
}

// ── kit routes ──────────────────────────────────────────────────────────
function KICK(ab, beat, g = 0.95, o = {}) {
  // TECHNO thump: lower start, faster pitch drop, tighter body, harder drive.
  const ev = { startSec: jit(t(ab, beat), 8), gain: vel(g, 0.14), fStart: 120, fEnd: 45, ampDecay: 0.36, pitchDecay: 0.050, click: 0.42, drive: 2.3, ...o };
  rec(`kick ${ev.startSec.toFixed(8)} ${ev.gain.toFixed(6)} ${(ev.fStart ?? 150).toFixed(4)} ${ev.fEnd.toFixed(4)} ${ev.pitchDecay.toFixed(6)} ${ev.ampDecay.toFixed(6)} ${(ev.click ?? 0.55).toFixed(4)} ${ev.drive.toFixed(6)}`);
  mixKick(ev, kickBuf, { sampleRate: SR });
}
function SNR(ab, beat, g = 0.7, o = {}) {
  const ev = { startSec: jit(t(ab, beat), 14), gain: vel(g, 0.22), ...o };
  rec(`snare ${ev.startSec.toFixed(8)} ${ev.gain.toFixed(6)} ${(ev.toneDecay ?? 0.10).toFixed(6)} `
    + `${(ev.noiseDecay ?? 0.16).toFixed(6)} ${(ev.tone ?? 0.55).toFixed(6)} ${(ev.noise ?? 0.9).toFixed(6)} `
    + `${(ev.f1 ?? 185).toFixed(4)} ${(ev.f2 ?? 330).toFixed(4)}`);
  mixSnare(ev, snareBuf, { sampleRate: SR });
}
function HAT(ab, beat, g = 0.28, o = {}) {
  const ev = { startSec: jit(t(ab, beat), 16), gain: vel(g, 0.34), ...o };
  rec(`hat ${ev.startSec.toFixed(8)} ${ev.gain.toFixed(6)} ${(ev.decay ?? (ev.open ? 0.26 : 0.045)).toFixed(6)}`);
  mixHat(ev, hatBuf, { sampleRate: SR });
}
function SHK(ab, beat, g = 0.2) {
  const ev = { startSec: jit(t(ab, beat), 18), gain: vel(g, 0.30) };
  rec(`shaker ${ev.startSec.toFixed(8)} ${ev.gain.toFixed(6)} ${(0.06).toFixed(6)} ${(0.008).toFixed(6)}`);
  mixShaker(ev, shakBuf, { sampleRate: SR });
}
function BP(ab, beat, midi, durBeats, g = 0.8, o = {}) {
  const ev = { startSec: jit(t(ab, beat), 10), midi: TR(midi), durSec: durBeats * BEAT, gain: vel(g, 0.16), sub: 0.58, drive: 2.0, ...o };
  rec(`subperc ${ev.startSec.toFixed(8)} ${ev.midi} ${ev.durSec.toFixed(8)} ${ev.gain.toFixed(6)} `
    + `${(ev.pitchUp ?? 5).toFixed(4)} ${(ev.pitchDecay ?? 0.022).toFixed(6)} ${(ev.decay ?? 0.5).toFixed(6)} `
    + `${(ev.drive ?? 2.2).toFixed(6)} ${(ev.sub ?? 0.35).toFixed(6)} ${(ev.click ?? 0.18).toFixed(6)}`);
  mixBassPerc(ev, subBuf, { sampleRate: SR });
}
function rise(landBar, dur, midis, g) {
  const ev = { landSec: t(landBar, 0), dur, midis: midis.map(TR), gain: g };
  rec(`rise ${ev.landSec.toFixed(8)} ${ev.dur.toFixed(8)} ${ev.gain.toFixed(6)} `
    + `${(ev.decay ?? 2.6).toFixed(6)} ${(ev.ratio ?? 3.5).toFixed(6)} ${ev.midis.length} ${ev.midis.join(" ")}`);
  mixReverseBell(ev, riseBuf, { sampleRate: SR });
}
function riseKick(landBar, dur, g) {
  if (!soloed("revkick")) return;
  const ev = { landSec: t(landBar, 0), dur, gain: g };
  rec(`revkick ${ev.landSec.toFixed(8)} ${ev.dur.toFixed(8)} ${ev.gain.toFixed(6)} `
    + `${(ev.fStart ?? 190).toFixed(4)} ${(ev.fEnd ?? 48).toFixed(4)} ${(ev.pitchDecay ?? 0.13).toFixed(6)} `
    + `${(ev.ampDecay ?? dur * 0.55).toFixed(6)} ${(ev.drive ?? 1.6).toFixed(6)}`);
  mixReverseKick(ev, kickBuf, { sampleRate: SR });
}
function rhat(targetBuf, landAb, landBeat, g = 0.32) {
  if (BAKING) {
    const tag = targetBuf === caveBuf ? "cavehat" : "rhat";
    rec(`${tag} ${t(landAb, landBeat).toFixed(8)} ${g.toFixed(6)}`);
  }
  const seg = renderHat({ gain: g, open: true }, { sampleRate: SR });
  const startIdx = Math.floor(t(landAb, landBeat) * SR) - seg.length;
  for (let i = 0; i < seg.length; i++) {
    const d = startIdx + i;
    if (d >= 0 && d < targetBuf.length) targetBuf[d] += seg[seg.length - 1 - i];
  }
}
function reverbInPlace(buf, { wet = 0.6, decay = 0.8 } = {}) {
  const combDelays = [0.0297, 0.0371, 0.0411, 0.0437], apDelays = [0.005, 0.0017];
  const out = new Float32Array(buf.length);
  for (const cd of combDelays) {
    const d = Math.max(1, Math.floor(cd * SR)), ring = new Float32Array(d);
    for (let i = 0; i < buf.length; i++) { const y = ring[i % d]; out[i] += y; ring[i % d] = buf[i] + y * decay; }
  }
  for (let i = 0; i < out.length; i++) out[i] /= combDelays.length;
  for (const ad of apDelays) {
    const d = Math.max(1, Math.floor(ad * SR)), ring = new Float32Array(d), g = 0.7;
    for (let i = 0; i < out.length; i++) { const bd = ring[i % d], y = -g * out[i] + bd; ring[i % d] = out[i] + g * y; out[i] = y; }
  }
  for (let i = 0; i < buf.length; i++) buf[i] = buf[i] * (1 - wet) + out[i] * wet;
}
function delayInPlace(buf, { time, fb = 0.45, wet = 0.5 } = {}) {
  const d = Math.max(1, Math.floor(time * SR));
  const out = new Float32Array(buf.length);
  for (let i = 0; i < buf.length; i++) {
    let y = buf[i];
    if (i >= d) y += out[i - d] * fb;
    out[i] = y;
  }
  for (let i = 0; i < buf.length; i++) buf[i] = buf[i] * (1 - wet) + out[i] * wet;
}
const SCR = {
  baby:      { rate: 5.5, depth: 2.0, tone: 0.5, dur: 0.30 },
  scribble:  { rate: 14,  depth: 1.6, tone: 0.6, dur: 0.40 },
  transform: { rate: 5,   depth: 2.2, gate: 12, gateDuty: 0.5,  tone: 0.55, dur: 0.42 },
  chirp:     { rate: 7,   depth: 3.0, gate: 7,  gateDuty: 0.55, tone: 0.6,  dur: 0.26 },
};
function SC(ab, beat, kind, g = 0.5, midi = 50) {
  const ev = { startSec: jit(t(ab, beat), 18), midi: TR(midi), gain: vel(g, 0.22), ...SCR[kind] };
  rec(`scratch ${ev.startSec.toFixed(8)} ${ev.midi} ${ev.gain.toFixed(6)} ${(ev.dur ?? 0.32).toFixed(6)} `
    + `${(ev.rate ?? 7).toFixed(6)} ${(ev.depth ?? 2.6).toFixed(6)} ${(ev.gate ?? 0).toFixed(6)} `
    + `${(ev.gateDuty ?? 0.5).toFixed(6)} ${(ev.tone ?? 0.55).toFixed(6)}`);
  mixScratch(ev, scrBuf, { sampleRate: SR });
}
function SCREAM(ab, beat, midi, g = 0.4, o = {}) {
  const ev = { startSec: jit(t(ab, beat), 20), midi: TR(midi), gain: vel(g, 0.22), ...o };
  rec(`scream ${ev.startSec.toFixed(8)} ${ev.midi} ${ev.gain.toFixed(6)} ${(ev.dur ?? 0.5).toFixed(6)} `
    + `${(ev.bend ?? 7).toFixed(6)} ${(ev.rasp ?? 0.65).toFixed(6)}`);
  mixScream(ev, screamBuf, { sampleRate: SR });
}
function vortex(t0, midi, dur, opts = {}) {
  const start = t0, f0 = mf(TR(midi));
  const wubHz = opts.wub ?? (opts.wubDiv ?? 2) / BEAT;
  const depthOct = opts.depth ?? 2.6;
  const g = opts.gain ?? 0.5, q = opts.q ?? 0.88;
  rec(`vortex ${start.toFixed(8)} ${f0.toFixed(6)} ${dur.toFixed(8)} ${g.toFixed(6)} ${wubHz.toFixed(6)} ${depthOct.toFixed(6)} ${q.toFixed(6)}`);
  const n0 = Math.floor(start * SR), ln = Math.ceil(dur * SR), dt = 1 / SR;
  let phase = 0, lp = 0, bp = 0;
  const damp = Math.min(1.8, 1 / q);
  for (let i = 0; i < ln; i++) {
    const d = n0 + i; if (d < 0 || d >= ns) continue;
    const u = i / ln, tsec = i * dt;
    const dive = Math.pow(Math.sin(Math.PI * u), 0.55);
    const snap = 0.12 * Math.sin(Math.PI * u) * Math.sin(2 * Math.PI * u);
    const ripple = 0.22 * Math.sin(2 * Math.PI * 3 * u);
    const pf = Math.pow(2, depthOct * -dive + snap + ripple);
    phase += f0 * pf * dt;
    const ph = phase - Math.floor(phase);
    const src = 2 * ph - 1;
    const wub = 0.5 + 0.5 * Math.sin(2 * Math.PI * wubHz * tsec);
    const fcW = (160 + 3200 * pf) * (0.42 + 0.58 * wub);
    const fco = Math.min(0.49, 2 * Math.sin(Math.PI * Math.min(fcW, SR * 0.45) / SR));
    lp += fco * bp;
    const hp = src - lp - damp * bp;
    bp += fco * hp;
    const swell = Math.sin(Math.PI * u);
    scrBuf[d] += lp * (0.35 + 0.65 * wub) * swell * g;
  }
}
// ── 3/4 hat figures ─────────────────────────────────────────────────────
function hats6(ab, g = 0.26, openLast = false) {           // six 8ths per bar
  for (let e = 0; e < 6; e++) HAT(ab, e * 0.5, e % 2 === 1 ? g : g * 0.55, { open: openLast && e === 5 });
}
function rideHats3(ab, g = 0.18) {          // accented swung 8ths — no skips, no rush
  for (let e = 0; e < 6; e++) HAT(ab, e * 0.5, e % 3 === 1 ? g * 1.35 : g * 0.7);
}

// ── shared 3/4 groove + fat bass for a span of bars ─────────────────────
// EASY TECHNO IN 3 — the kick walks every beat, dead simple (the rush came
// out and the deep swing went in everywhere else). One easy backbeat; a small
// snare lift only every 4th bar at peak. lvl scales WEIGHT, not clutter.
function layFatGroove(ab, bars, { lvl = 1, subRoots = [N.C2], ride = false, kg = 0.94 } = {}) {
  for (let i = 0; i < bars; i++) {
    const b = ab + i, r = subRoots[i % subRoots.length], r5 = r === N.C2 ? N.G1 : r;
    KICK(b, 0, kg);                                      // the pulse —
    if (lvl >= 1) { KICK(b, 1, kg * 0.78); KICK(b, 2, kg * 0.82); }   // — on the floor
    SNR(b, 1, lvl >= 2 ? 0.5 : 0.42);                    // one easy backbeat
    if (lvl >= 3 && i % 4 === 3) { SNR(b, 2.5, 0.3); SNR(b, 2.75, 0.38); }   // small lift, sparingly
    if (ride) rideHats3(b, lvl >= 2 ? 0.18 : 0.14); else hats6(b, lvl >= 1 ? 0.22 : 0.15, lvl >= 2);
    if (lvl >= 1) for (let e = 0; e < 3; e++) SHK(b, e + 0.5, 0.13);
    BP(b, 0, r, 1.0, 0.82); BP(b, 2, r5, 0.8, 0.68);
    SUBSINE(b, 0, r, 1.8, 0.5);
    if (lvl >= 3) { SUBSINE(b, 0, r - 12, 2.0, 0.42); SUBSINE(b, 2, r5 - 12, 0.9, 0.3); }
  }
}
function bassBounce(ab, bars, roots) {
  for (let i = 0; i < bars; i++) { const r = roots[i % roots.length]; bass(t(ab + i, 0), r, 1.2); bass(t(ab + i, 2), r === N.C3 ? N.G3 : r, 0.9); }
}
function vibTriad(ab, beat, midis, beats, g = 0.32) {
  for (const m of midis) vib(t(ab, beat), m, beats, g);
  vib(t(ab, beat), midis[0] - 12, beats, g * 0.72);
}
const TRIAD = { C: [N.C4, N.E4, N.G4], F: [N.F3, N.A3, N.C4], G: [N.G3, N.B3, N.D4], Am: [N.A3, N.C4, N.E4] };

// ══════════════════════════════════════════════════════════════════════
//  LEAD MELODY DATA — the flutterbap tunes RE-SUNG IN 3: same contours
//  (the hook still climbs E→G→C, the soar still hangs on high C), new
//  rhythm. [barInBlock, beat, note, durBeats, gain?]; beats ∈ [0,3).
// ══════════════════════════════════════════════════════════════════════
const LEAD = {
  butterfly: [
    [0, 0, "E5", 1], [0, 1, "G5", 1], [0, 2, "C6", 1],
    [1, 0, "C6", 2], [1, 2, "G5", 1],
    [2, 0, "C6", 1], [2, 1, "E6", 1], [2, 2, "C6", 1],
    [3, 0, "C6", 3],
    [4, 0, "D5", 1], [4, 1, "G5", 1], [4, 2, "B5", 1],
    [5, 0, "B5", 2], [5, 2, "G5", 1],
    [6, 0, "C6", 2], [6, 2, "G5", 1],
    [7, 0, "C6", 3],
  ],
  palofmine: [
    [0, 0, "E5", 1], [0, 1, "G5", 1], [0, 2, "C6", 1],
    [1, 0, "C6", 3],
    [2, 0, "A5", 1], [2, 1, "G5", 1], [2, 2, "F5", 1],
    [3, 0, "F5", 3],
    [4, 0, "E5", 1], [4, 1, "F5", 1], [4, 2, "G5", 1],
    [5, 0, "A5", 3],
    [6, 0, "C6", 3],
    [7, 0, "C6", 3, 0.85],
  ],
  mommywow: [
    [0, 0, "G5", 3, 0.9],
    [1, 0, "C6", 1], [1, 1, "E6", 1], [1, 2, "G6", 1],
    [2, 0, "G6", 3],
    [3, 0, "G5", 3, 0.9],
    [4, 0, "G6", 1], [4, 1, "E6", 1], [4, 2, "C6", 1],
    [5, 0, "C6", 3],
  ],
  slinky: [
    [0, 0, "C5", 1], [0, 1, "E5", 1], [0, 2, "G5", 1],
    [1, 0, "G5", 1], [1, 1, "F5", 1], [1, 2, "E5", 0.5], [1, 2.5, "D5", 0.5],
    [2, 0, "C5", 1], [2, 1, "E5", 1], [2, 2, "G5", 1],
    [3, 0, "C5", 0.5], [3, 0.5, "D5", 0.5], [3, 1, "E5", 0.5], [3, 1.5, "F5", 0.5], [3, 2, "G5", 0.5], [3, 2.5, "A5", 0.5],
    [4, 0, "B5", 0.5], [4, 0.5, "C6", 0.5], [4, 1, "B5", 0.5], [4, 1.5, "A5", 0.5], [4, 2, "G5", 0.5], [4, 2.5, "F5", 0.5],
    [5, 0, "E5", 0.5], [5, 0.5, "D5", 0.5], [5, 1, "C5", 1], [5, 2, "E5", 1],
    [6, 0, "C6", 1], [6, 1, "A5", 1], [6, 2, "F5", 1],
    [7, 0, "E5", 1], [7, 1, "G5", 1], [7, 2, "C5", 1],
  ],
  fly: [
    [0, 0, "C5", 1], [0, 1, "E5", 1], [0, 2, "G5", 1],
    [1, 0, "C6", 3],
    [2, 0, "E5", 1], [2, 1, "G5", 1], [2, 2, "A5", 1],
    [3, 0, "D6", 3],
    [4, 0, "E6", 1], [4, 1, "D6", 1], [4, 2, "C6", 1],
    [5, 0, "A5", 1], [5, 1, "C6", 1], [5, 2, "G6", 1],
    [6, 0, "G6", 1], [6, 1, "E6", 1], [6, 2, "C6", 1],
    [7, 0, "G5", 1], [7, 1, "E5", 1], [7, 2, "C5", 1],
  ],
  land: [
    [0, 0, "C6", 1], [0, 1, "G5", 1], [0, 2, "E5", 1],
    [1, 0, "D5", 1], [1, 1, "C5", 2],
    [2, 0, "E5", 1], [2, 1, "D5", 1], [2, 2, "C5", 1],
    [3, 0, "C4", 3, 0.9],
  ],
};
function playLead(ab, name, gMul = 1) {
  for (const [bb, beat, note, dur, g] of LEAD[name]) lead(t(ab + bb, beat), N[note], dur, (g ?? 1.0) * gMul);
}
function twinkle(ab, bars) {
  for (let i = 0; i < bars; i++) { spark(t(ab + i, 1.5), N.E6, 0.5, 0.4); spark(t(ab + i, 2.5), N.C6, 0.5, 0.4); }
}

// ══════════════════════════════════════════════════════════════════════
//  BLOCKS
// ══════════════════════════════════════════════════════════════════════
const BLOCKS = {
  intro(ab, bars) {
    // ZOO-PARK DAWN, compressed — the 360's slow water-drop dawn at a jog: a
    // warm pad, drops + plinks in the echo, AND the zoo waking up around the
    // brook (a parrot far right, a monkey far left, one distant lion). The
    // groove creeps in after just a few bars — this dawn is in a hurry.
    const amb = Math.max(3, bars - 4);
    PAD(ab, [N.C3, N.G3, N.C4, N.E4], bars + 1, 0.26, 0.12);   // fast bloom — the bed is THERE from the first second
    SUBSINE(ab, 0, N.C2, amb * 3, 0.18);

    const HZ = [1760, 2090, 1320, 1900, 1500, 2350, 1150, 1980, 1640, 2210];
    let di = 0;
    for (let i = 0; i < bars; i++) {
      const b = ab + i;
      const nd = i < amb ? (i % 2 ? 2 : 1) : 1;
      for (let k = 0; k < nd; k++) {
        const beat = (k * 1.15 + (i % 3) * 0.6 + hrand() * 0.5) % 3;
        drop(t(b, beat), HZ[di++ % HZ.length] * (1 + hr2() * 0.05), Math.max(0.18, 0.44 - i * 0.02));
      }
    }
    // beepier dawn melody — shorter, quicker plinks (the long ring now lives
    // in the glock's stretched decay, not in held durations)
    const MEL = [[0, 1, "G4", 1], [0, 2.5, "C5", 0.5], [1, 1, "E5", 1], [1, 2.5, "G5", 0.5],
                 [2, 0.5, "C5", 0.5], [2, 1.5, "E5", 1], [3, 0, "D5", 0.5], [3, 1.5, "G4", 0.5],
                 [4, 0, "G5", 1], [4, 2, "E5", 0.5], [5, 0.5, "C5", 0.5], [5, 1.5, "E5", 1], [5, 2.5, "G5", 1]];
    for (const [bb, beat, note, dur] of MEL) if (bb < bars) plink(t(ab + bb, beat), N[note], dur, Math.max(0.34, 0.48 - bb * 0.012));
    bassBell(ab + 1, 1, N.C4, 4, 0.42, 1.8); bassBell(ab + 4, 0, N.G3, 4, 0.36, 1.8);
    flute(t(ab + 1, 0), N.C5, 5, 0.34, { preset: "panflute", attack: 0.6, release: 1.0 });
    flute(t(ab + 4, 1), N.G5, 4, 0.30, { preset: "panflute", attack: 0.7, release: 0.9 });
    // ── the zoo wakes with the park — properly awake now, not shy ──
    zoo(t(ab + 2, 1.6), "parrot", 0.30, { rate: 1.0, pan: 0.55 });
    zoo(t(ab + 4, 0.8), "monkey", 0.26, { rate: 0.92, pan: -0.6 });
    zoo(t(ab + 6, 0.2), "lion",   0.24, { rate: 0.75, pan: -0.2 });
    zoo(t(ab + 7, 1.8), "parrot", 0.26, { rate: 1.12, pan: -0.5 });

    for (let i = amb; i < bars; i++) {
      const b = ab + i, j = i - amb;
      KICK(b, 0, 0.55 + j * 0.08);
      if (j >= 1) { KICK(b, 1, 0.45 + j * 0.06); KICK(b, 2, 0.48 + j * 0.06); }   // the techno pulse arrives
      BP(b, 0, N.C2, 1.2, 0.55 + j * 0.06); BP(b, 2, N.G1, 0.8, 0.5 + j * 0.05);
      SUBSINE(b, 0, N.C2, 1.8, 0.42);
      for (let e = 0; e < 3; e++) SHK(b, e + 0.5, 0.10 + j * 0.02);
      if (j >= 1) hats6(b, 0.14 + j * 0.03);
      if (j >= 2) { SNR(b, 1, 0.44); SNR(b, 2, 0.4); }
    }
    const TEASE = [[bars - 3, 0, "E5", 1], [bars - 3, 1, "G5", 1], [bars - 3, 2, "C6", 1],
                   [bars - 2, 0, "C6", 2], [bars - 2, 2, "G5", 1]];
    for (const [bb, beat, note, dur] of TEASE) lead(t(ab + bb, beat), N[note], dur, 0.5);
    SNR(ab + bars - 1, 2.5, 0.45); SNR(ab + bars - 1, 2.75, 0.55);   // an easy two-hit fill, not a roll
    spark(t(ab + bars - 1, 2.5), N.C6, 0.5, 0.5);
    rise(ab + bars, 1.6 * BAR, [N.G5, N.C6, N.E6, N.G6], 0.46);
    riseKick(ab + bars, 1.1 * BAR, 0.55);
  },
  butterfly(ab, bars, o = {}) {
    playLead(ab, "butterfly");
    bassBounce(ab, bars, [N.C3]);
    vibTriad(ab, 0, TRIAD.C, 3 * bars / 4, 0.3);
    twinkle(ab, bars);
    layFatGroove(ab, bars, { lvl: o.lvl ?? 0, subRoots: [N.C2], ride: o.ride });
    dynabell(t(ab, 0), N.C6, 3, 0.5, { tail: 2.2 }); bell(t(ab, 0.05), N.E6, 3, 0.38); bell(t(ab, 0.1), N.G6, 3, 0.3);
    if ((o.lvl ?? 0) >= 1) {   // the parrot answers the hook
      zoo(t(ab + 3, 2.5), "parrot", 0.36, { rate: 1.0, pan: 0.5 });
      zoo(t(ab + 7, 2.5), "parrot", 0.34, { rate: 1.12, pan: -0.5 });
    }
  },
  palofmine(ab, bars, o = {}) {
    playLead(ab, "palofmine");
    bassBounce(ab, bars, [N.C3, N.F3, N.G3, N.C3]);
    const ch = [TRIAD.C, TRIAD.F, TRIAD.G, TRIAD.C];
    for (let i = 0; i < bars; i++) vibTriad(ab + i, 0, ch[i % 4], 3, 0.3);
    twinkle(ab, bars);
    layFatGroove(ab, bars, { lvl: o.lvl ?? 1, subRoots: [N.C2, N.F1, N.G1, N.C2], ride: o.ride });
    zoo(t(ab + 5, 1.5), "monkey", 0.32, { rate: 1.06, pan: 0.45 });
  },
  mommywow(ab, bars) {
    // HUSH — six bars of held wonder; the kit thins, the butter sub holds.
    playLead(ab, "mommywow");
    for (let i = 0; i < bars; i++) bass(t(ab + i, 0), N.C3, 3, 0.5);
    vibTriad(ab, 0, TRIAD.C, 3 * bars, 0.28);
    PAD(ab, [N.C4, N.E4, N.G4, N.C5], bars, 0.42);
    for (let i = 0; i < bars; i++) {
      KICK(ab + i, 0, 0.6);
      BP(ab + i, 0, N.C2, 2.6, 0.6, { drive: 1.6, decay: 0.9 });
      SUBSINE(ab + i, 0, N.C2, 2.6, 0.46);
      HAT(ab + i, 1.5, 0.18, { open: true });
      for (let e = 0; e < 3; e++) SHK(ab + i, e + 0.5, 0.1);
    }
    bell(t(ab, 0), N.E6, 3 * bars, 0.36); bell(t(ab + 1, 1), N.G6, 2, 0.7);
    bell(t(ab + 3, 0), N.G6, 1, 0.7); bell(t(ab + 3, 2), N.C6, 1, 0.6);
    bassBell(ab, 0, N.C4, 3 * bars, 0.55); bassBell(ab + 2, 0, N.G3, 5, 0.46);
    const FMEL = [[0, 0, "G5", 2], [0, 2, "E5", 1], [1, 0, "C6", 3], [2, 0, "A5", 2], [2, 2, "G5", 1], [3, 0, "E5", 3], [4, 0, "C6", 2], [4, 2, "E6", 1], [5, 0, "G5", 3]];
    for (const [bb, beat, note, beats] of FMEL) if (bb < bars) flute(t(ab + bb, beat), N[note], beats, 0.50, { preset: "flute", attack: 0.12, release: 0.45 });
    zoo(t(ab + 4, 1), "parrot", 0.24, { rate: 0.9, pan: 0.5 });   // drifting through the hush
    SNR(ab + bars - 1, 2, 0.4); SNR(ab + bars - 1, 2.5, 0.5); SNR(ab + bars - 1, 2.75, 0.6);
  },
  slinky(ab, bars, o = {}) {
    playLead(ab, "slinky");
    const lvl = o.lvl ?? 2;
    for (let i = 0; i < bars; i++) {
      const b = ab + i;
      KICK(b, 0, 0.94);
      if (lvl >= 1) { KICK(b, 1, 0.74); KICK(b, 2, 0.78); }
      SNR(b, 1, 0.5);
      if (o.ride) rideHats3(b, 0.18); else hats6(b, 0.22, lvl >= 2);
      for (let e = 0; e < 3; e++) SHK(b, e + 0.5, 0.13);
      BP(b, 0, N.C2, 0.8, 0.82); BP(b, 2, N.G1, 0.7, 0.7);
      SUBSINE(b, 0, N.C2, 1.3, 0.46);
      bass(t(b, 0), N.C3, 1.2); bass(t(b, 1.5), N.G3, 1.0);
      SC(b, 2, i % 2 ? "baby" : "scribble", 0.46, 48 + (i % 3) * 4);
    }
    twinkle(ab, bars);
    // the sea lion is the springiest thing in the zoo — it barks the slinky
    zoo(t(ab + 3, 2.5), "sealion", 0.5, { rate: 1.0, pan: 0.45 });
    zoo(t(ab + 5, 1.5), "sealion", 0.42, { rate: 0.94, pan: 0.1 });
    zoo(t(ab + 7, 2.5), "sealion", 0.52, { rate: 1.12, pan: -0.45 });
    rise(ab, 1.8 * BAR, [N.E5, N.G5, N.C6, N.E6], 0.5);
    riseKick(ab, 1.3 * BAR, 0.66);
    SCREAM(ab, 0, 88, 0.42, { bend: 8, rasp: 0.6, dur: 0.5 });
  },
  fly(ab, bars, o = {}) {
    playLead(ab, "fly");
    const roots3 = [N.C3, N.C3, N.F3, N.F3, N.C3, N.A3, N.G3, N.C3];
    const roots1 = [N.C2, N.C2, N.F1, N.F1, N.C2, N.A1, N.G1, N.C2];
    for (let i = 0; i < bars; i++) bassBounce(ab + i, 1, [roots3[i % 8]]);
    layFatGroove(ab, bars, { lvl: o.lvl ?? 3, subRoots: roots1, ride: o.ride, kg: 0.96 });
    const flvl = o.lvl ?? 3;
    bellRun(ab + 1, 0, [[0, N.G5, 1], [1, N.C6, 2]], { gain: 0.52, bend: -2, tail: 2.4 });
    if (flvl >= 1)
      bellRun(ab + 3, 0, [[0, N.C6, 0.5], [0.5, N.E6, 0.5], [1, N.G6, 0.5], [1.5, N.C6 + 12, 1.5]], { gain: 0.48, bend: -2 });
    if (flvl >= 2)
      bellRun(ab + 5, 0, [[0, N.G6, 0.5], [0.5, N.E6, 0.5], [1, N.C6, 0.5], [1.5, N.A5, 0.5], [2, N.G5, 1]], { gain: 0.46, bend: -1.5, vib: 0.14, tail: 1.6 });
    if (flvl >= 3) {
      bellRun(ab + 6, 0, [
        [0, N.G5, 0.5], [0.5, N.C6, 0.25], [0.75, N.E6, 0.25], [1, N.G6, 0.5],
        [1.5, N.E6, 0.25], [1.75, N.C6, 0.5], [2.25, N.A5, 0.5],
      ], { gain: 0.40, bend: -1.5, vib: 0.14, tail: 1.8 });
      dynabell(t(ab + 7, 0), N.C6, 1, 0.52, { bend: -2, vib: 0.16, tail: 2.9 });
    }
    flute(t(ab + 1, 0), N.G5, 3, 0.34, { preset: "flute", attack: 0.22, release: 0.6 });
    flute(t(ab + 5, 1), N.C6, 2.5, 0.32, { preset: "flute", attack: 0.18, release: 0.5 });
    twinkle(ab, bars);
    rise(ab, 1.5 * BAR, [N.G5, N.C6, N.E6, N.G6], 0.42);
    riseKick(ab, 1.0 * BAR, 0.5);
    SCREAM(ab, 0, 90, 0.42, { bend: 7, rasp: 0.6, dur: 0.5 });
    SCREAM(ab + 3, 2.5, 86, 0.38, { bend: 10, rasp: 0.6, dur: 0.42 });
    if (o.zoo) {   // FINAL soar only — the hyena finds the climb hilarious
      zoo(t(ab + 4, 0.5), "hyena", 0.52, { rate: 1.0, pan: 0.5 });
      zoo(t(ab + 6, 1.5), "monkey", 0.46, { rate: 1.12, pan: -0.5 });
    }
    if ((o.lvl ?? 3) >= 3) zoo(t(ab + 1, 1.5), "parrot", 0.3, { rate: 1.06, pan: 0.35 });   // riding the soar
    if ((o.lvl ?? 3) >= 3) for (const fb of [0, 0.25, 0.5, 0.75]) SNR(ab + bars - 1, 2 + fb, 0.35 + fb * 0.7);
  },
  ride(ab, bars, o = {}) {
    // MENAGERIE RIDE — the 360's squeak-fight, except now the ANIMALS answer.
    // The scratch still sings its climbing call; on alternating bars a zoo
    // one-shot (monkey / parrot / sealion / hyena, repitched to the motif)
    // takes the answer instead of the squeak. Same hump envelope — the whole
    // menagerie swells in, peaks (with the vortex + a hyena cackle), and slips
    // back out under the stomp.
    const lvl = o.lvl ?? 3;
    layFatGroove(ab, bars, { lvl, subRoots: [N.C2, N.C2, N.G1, N.A1], ride: true, kg: 0.96 });
    zoo(t(ab, 0), "elephant", 0.6, { rate: 1.0, pan: -0.15 });   // the trumpet opens the fight
    const CALL   = [[0.0, N.C5, "scribble"], [0.5, N.E5, "chirp"], [1.0, N.G5, "scribble"]];
    const ANSWER = [[1.5, N.G5], [2.0, N.C6], [2.5, N.E6]];
    const ANIMALS = ["monkey", "parrot", "sealion", "hyena"];
    for (let i = 0; i < bars; i++) {
      const swell = Math.sin(((i + 0.5) / bars) * Math.PI);
      const g = 0.15 + 0.32 * swell;   // easier on the ear than rev-1
      const motif = (i % 2 ? 2 : 0) + (i === bars - 1 ? 4 : 0);
      for (const [beat, midi, kind] of CALL)
        SC(ab + i, beat, kind, g, midi + motif);
      if (i % 2 === 0) {
        // animal answer — two calls, repitched to the motif, opposite sides
        const an = ANIMALS[(i / 2) % ANIMALS.length];
        zoo(t(ab + i, 1.5), an, g * 1.15, { rate: zooSemis(motif), pan: 0.55 });
        zoo(t(ab + i, 2.25), an, g * 0.95, { rate: zooSemis(motif + 3), pan: -0.55 });
      } else {
        for (const [beat, midi] of ANSWER)
          SCREAM(ab + i, beat, midi + motif, g * 0.85, { bend: 4, rasp: 0.32, dur: 0.30 });
      }
      spark(t(ab + i, 2.75), N.E6, 0.5, 0.36 * swell);
    }
    const peakBar = Math.floor((bars - 1) / 2);
    vortex(t(ab + peakBar, 0.5), N.C4, 2.5 * BEAT, { gain: 0.46 + 0.06 * lvl, depth: 2.8, wubDiv: 2 });
    vortex(t(ab + peakBar + 1, 1.5), N.G3, 1.2 * BEAT, { gain: 0.38 + 0.05 * lvl, depth: 2.4, wubDiv: 4 });
    zoo(t(ab + peakBar, 0), "hyena", 0.55, { rate: 1.06, pan: 0.2 });   // peak cackle
    rise(ab, 1.2 * BAR, [N.C6, N.E6, N.G6], 0.4);
  },
  cave(ab, bars) {
    // the lion's den — the dub breakdown, home key, one real lion in it.
    const caveVib = (tt, m, beats, g) => { const ev = { startSec: tt, midi: TR(m), durSec: beats * BEAT, gain: g, preset: "vibraphone_off", decayMul: 1.5 }; recMarimbaBus("cavem", ev); mixEventMarimba(ev, caveBuf, { sampleRate: SR }); };
    const caveLead = (tt, m, beats, g) => { const ev = { startSec: tt, midi: TR(m), durSec: beats * BEAT, gain: g, preset: "xylophone", decayMul: 1.4 }; recMarimbaBus("cavem", ev); mixEventMarimba(ev, caveBuf, { sampleRate: SR }); };
    const caveKick = (ev) => {
      rec(`cavekick ${ev.startSec.toFixed(8)} ${(ev.gain ?? 1).toFixed(6)} ${(ev.fStart ?? 150).toFixed(4)} ${(ev.fEnd ?? 48).toFixed(4)} ${(ev.pitchDecay ?? 0.055).toFixed(6)} ${(ev.ampDecay ?? 0.34).toFixed(6)} ${(ev.click ?? 0.55).toFixed(4)} ${(ev.drive ?? 1.6).toFixed(6)}`);
      mixKick(ev, caveBuf, { sampleRate: SR });
    };
    for (let half = 0; half < bars; half += 4) {
      for (const m of TRIAD.Am) caveVib(t(ab + half, 0), m, 5.5, 0.30);
      for (const m of TRIAD.F) caveVib(t(ab + half + 2, 0), m, 5.5, 0.28);
      caveLead(t(ab + half, 1), N.E5, 1, 0.48); caveLead(t(ab + half, 2), N.A5, 1, 0.44);
      caveLead(t(ab + half + 2, 1), N.C6, 1, 0.44); caveLead(t(ab + half + 2, 2), N.G5, 1, 0.4);
      {
        const ev = { landSec: t(ab + half, 0), dur: 1.4 * BAR, midis: [N.A5, N.C6, N.E6].map(TR), gain: 0.38 };
        rec(`caverise ${ev.landSec.toFixed(8)} ${ev.dur.toFixed(8)} ${ev.gain.toFixed(6)} ${(2.6).toFixed(6)} ${(3.5).toFixed(6)} ${ev.midis.length} ${ev.midis.join(" ")}`);
        mixReverseBell(ev, caveBuf, { sampleRate: SR });
      }
      caveKick({ startSec: t(ab + half, 0), gain: 0.45, fEnd: 38, ampDecay: 0.6 });
      caveKick({ startSec: t(ab + half + 1, 1.5), gain: 0.45, fEnd: 38, ampDecay: 0.6 });
      rhat(caveBuf, ab + half + 1, 0, 0.32);
      SUBSINE(ab + half, 0, N.A1, 3, 0.4); SUBSINE(ab + half + 2, 0, N.F1, 3, 0.4);
    }
    zoo(t(ab, 0.5), "lion", 0.5, { rate: 0.8, pan: -0.25 });   // the den's keeper, slowed to giant
    zoo(t(ab + 5, 1), "lion", 0.4, { rate: 0.72, pan: 0.3 });    // and its echo-mate, even deeper
    SCREAM(ab + bars - 1, 0, 88, 0.36, { bend: 8, rasp: 0.55, dur: 0.5 });
  },
  progression(ab, bars) {
    // Am → F → C → G (vi–IV–I–V — rotated away from the 360's I–V–vi–IV),
    // two bars each, walking it home in 3 — with a farewell animal per chord.
    const PROG = [
      { root1: N.A1, root3: N.A3, triad: TRIAD.Am, top: N.A5 },
      { root1: N.F1, root3: N.F3, triad: TRIAD.F, top: N.A5 },
      { root1: N.C2, root3: N.C3, triad: TRIAD.C, top: N.C6 },
      { root1: N.G1, root3: N.G3, triad: TRIAD.G, top: N.B5 },
    ];
    const PARADE = ["monkey", "sealion", "parrot", "hyena"];
    for (let i = 0; i < bars; i++) {
      const b = ab + i, ch = PROG[Math.floor(i / 2) % 4];
      KICK(b, 0, 0.9); KICK(b, 1, 0.72); KICK(b, 2, 0.76); SNR(b, 1, 0.5);
      hats6(b, 0.22); for (let e = 0; e < 3; e++) SHK(b, e + 0.5, 0.13);
      BP(b, 0, ch.root1, 1.1, 0.78); BP(b, 2, ch.root1, 0.7, 0.66);
      SUBSINE(b, 0, ch.root1, 1.8, 0.48);
      vibTriad(b, 0, ch.triad, 3, 0.34);
      lead(t(b, 0), ch.top, 1, 0.9); lead(t(b, 1), ch.triad[2], 1, 0.8); lead(t(b, 2), ch.triad[1] + 12, 1, 0.82);
      spark(t(b, 2.5), N.E6, 0.5, 0.4);
      bell(t(b, 0), ch.triad[2] + 12, 2, 0.4);
      if (i % 2 === 0) zoo(t(b, 2.5), PARADE[(i / 2) % 4], 0.35, { rate: 1.0, pan: i % 4 ? -0.5 : 0.5 });
    }
  },
  land(ab, bars) {
    playLead(ab, "land");
    for (let i = 0; i < bars; i++) {
      const b = ab + i;
      KICK(b, 0, 0.85 - i * 0.12);
      if (i < 2) { SNR(b, 1, 0.5); SNR(b, 2, 0.44); hats6(b, 0.2); }
      BP(b, 0, N.C2, 1.8 - i * 0.3, 0.74 - i * 0.1);
      SUBSINE(b, 0, N.C2, 2.0 - i * 0.3, 0.46 - i * 0.06);
      bass(t(b, 0), N.C3, 1.2); bass(t(b, 1.5), N.G3, 1.0);
    }
    vibTriad(ab, 0, TRIAD.C, 3, 0.3);
  },
  button(ab) {
    // final resolved C hit + ring — and the parrot gets the last word.
    KICK(ab, 0, 1.0); BP(ab, 0, N.C2, 2.6, 0.88, { decay: 1.4 }); SUBSINE(ab, 0, N.C2, 2.8, 0.55);
    SNR(ab, 0, 0.5);
    bell(t(ab, 0), N.C6, 3, 0.5); bell(t(ab, 0.06), N.E6, 3, 0.4); bell(t(ab, 0.12), N.G6, 3, 0.35);
    vibTriad(ab, 0, TRIAD.C, 3, 0.4);
    lead(t(ab, 0), N.C6, 3, 0.85);
    rhat(hatBuf, ab, 0, 0.42);
    zoo(t(ab + 1, 0.5), "parrot", 0.55, { rate: 1.06, pan: 0.35 });
  },
};

// ── DISPATCH ────────────────────────────────────────────────────────────
let cursor = 0;
const placed = [];
let firstKeySet = false;
for (const [name, bars, opts] of ARRANGEMENT) {
  const o = opts ?? {};
  if (o.key !== undefined) {
    const changed = o.key !== gKey;
    gKey = o.key;
    if (changed && firstKeySet) PAD(cursor, [N.C4, N.E4, N.G4], Math.min(bars, 4), 0.5, 0.3);
    firstKeySet = true;
  }
  BLOCKS[name](cursor, bars, o);
  placed.push({ name, bar0: cursor, bars, key: gKey });
  cursor += bars;
}

// ── BOOWOOP FLOURISHES — reverse-suck "boo" into a bendy "woop" — sprinkled
// in the dawn, the den, and every few bars of the euphoric F chorus. ──
function keyAtBar(bar) { for (const s of placed) if (bar >= s.bar0 && bar < s.bar0 + s.bars) return s.key; return 0; }
const BOOWOOPS = [
  [3, 1.5, "C5", 2, 0.40], [7, 1.0, "G5", 2, 0.40],       // dawn
  [100, 1.0, "E5", 2, 0.42],                              // the den
  // FINAL G CHORUS (bars 134–166) — one every few bars
  [138, 0.0, "C6", 2, 0.46], [144, 1.5, "G5", 2, 0.44],
  [150, 0.0, "E6", 2, 0.46], [156, 1.5, "C6", 2, 0.44],
  [162, 0.0, "G6", 2, 0.48],
];
for (const [bar, beat, note, beats, g] of BOOWOOPS) { gKey = keyAtBar(bar); boowoop(bar, beat, N[note], beats, g); }

// ── FEMBELL ACCENTS ─────────────────────────────────────────────────────
const FEMBELLS = [
  [4,   0, "C5", 0.40, "bowl",     0.55],  // dawn — pure shimmer under the drops
  [34,  0, "G5", 0.36, "bowl",     0.50],  // the pass-A hush
  [134, 0, "C5", 0.52, "church",   1.00],  // F → G lift: the big toll
  [136, 0, "C6", 0.46, "handbell", 1.00],  // final chorus — bright handbell
  [178, 0, "C4", 0.58, "church",   1.20],  // the BUTTON — deep toll into the fade
];
for (const [bar, beat, note, g, preset, ts] of FEMBELLS) { gKey = keyAtBar(bar); fembell(t(bar, beat), N[note], g, preset, ts); }

// ── GONGS ───────────────────────────────────────────────────────────────
const GONGS = [
  [96,  0, "C2", 0.78, 1.15],   // the den opens
  [134, 0, "G1", 0.90, 1.25],   // the G-CHORUS LIFT
  [178, 0, "C2", 0.85, 1.30],   // the BUTTON
];
for (const [bar, beat, note, g, ts] of GONGS) { gKey = keyAtBar(bar); gong(t(bar, beat), N[note], g, ts); }
gKey = 0;

// ── WATER-STREAM bed — the park brook still runs through the zoo. Same gate
// map; the fast passes hold it lower so the stomp stays dry + punchy. ──────
{
  const GATE_FOR = {
    intro: 1.0, mommywow: 0.78, cave: 1.0, land: 0.5, button: 0.4,
    slinky: 0.30, butterfly: 0.28, palofmine: 0.28, progression: 0.26,
    fly: 0.20, ride: 0.18,
  };
  const HZ = 20, span = Math.ceil(totalSec * HZ), env = new Float32Array(span);
  for (let k = 0; k < span; k++) {
    const tt = k / HZ; let lvl = 0.3;
    for (const s of placed) {
      if (tt >= t(s.bar0, 0) && tt < t(s.bar0 + s.bars, 0)) { lvl = GATE_FOR[s.name] ?? 0.3; break; }
    }
    env[k] = lvl;
  }
  const a = 0.05;
  for (let k = 1; k < span; k++) env[k] = env[k] * a + env[k - 1] * (1 - a);
  for (let k = span - 2; k >= 0; k--) env[k] = env[k] * a + env[k + 1] * (1 - a);
  waterStream((tt) => env[Math.max(0, Math.min(span - 1, Math.floor(tt * HZ)))]);
}

// ── C-ENGINE SCORE ──────────────────────────────────────────────────────
if (BAKING) {
  if (soloed("stream")) {
    const streamRaw = BAKE_PATH + ".stream.f32";
    const sb = Buffer.alloc(ns * 4);
    for (let i = 0; i < ns; i++) sb.writeFloatLE(streamBuf[i], i * 4);
    writeFileSync(streamRaw, sb);
    rec(`streamraw ${streamRaw}`);
  }
  const fF = placed.find((s) => s.key === 7);
  const finalLift = fF ? t(fF.bar0, 0) : -1;
  const head = ["# flatterbop180 baked score — one event per line; the C engine replays them",
    `sr ${SR}`, `beat ${BEAT.toFixed(10)}`, `bar ${BAR.toFixed(10)}`,
    `totalbars ${TOTAL_BARS}`, `targetsec ${TARGET_SEC}`, `finallift ${finalLift.toFixed(8)}`,
    `substrate ${SUBSTRATE}`, `sat ${SAT.drive} ${SAT.bias} ${SAT.hiss}`,
    `ns ${ns}`];
  // the zoo roster — slot registrations the engine loads before the events
  const zooLines = ZOO_ROLES.map((r, i) => `zoosample ${i} ${resolve(ZOO_DIR, r + ".wav")}`);
  const sectionLines = placed.map((s) => `section ${s.name} ${s.bar0} ${s.bars}`);
  writeFileSync(BAKE_PATH, head.concat(zooLines, sectionLines, SCORE).join("\n") + "\n");
  console.log(`✓ baked ${SCORE.length} events → ${BAKE_PATH} (sr=${SR} ns=${ns} · 3/4 @ ${BPM.toFixed(1)} BPM · ${TOTAL_BARS} bars)`);
  process.exit(0);
}

// ══════════════════════════════════════════════════════════════════════
//  STEREO MIXDOWN (JS reference — no zoo; the C engine is canonical)
// ══════════════════════════════════════════════════════════════════════
const outL = new Float32Array(ns);
const outR = new Float32Array(ns);
function pan(p) { const a = (p + 1) * Math.PI / 4; return [Math.cos(a), Math.sin(a)]; }
function place(buf, p, gain) {
  const [lg, rg] = pan(p);
  for (let i = 0; i < ns; i++) { const s = buf[i] * gain; outL[i] += s * lg; outR[i] += s * rg; }
}
if (SOLO) {
  const VOICE = {
    sub:  [deepBuf, 0.00, 0.92],
    kick: [kickBuf, 0.00, 1.02],
    lead: [leadBuf, 0.00, 1.00], bass: [bassBuf, 0.00, 0.66],
    spark:[sparkBuf, 0.44, 0.86], bell: [bellBuf, -0.42, 0.84], vib: [vibBuf, -0.24, 0.88],
    snare:[snareBuf, 0.04, 0.80], hat: [hatBuf, 0.34, 0.56], shaker: [shakBuf, -0.34, 0.48],
    subperc:[subBuf, 0.00, 1.00], scratch:[scrBuf, 0.34, 0.60], scream:[screamBuf, -0.48, 0.48],
    rise: [riseBuf, 0.00, 0.85], revkick:[kickBuf, 0.00, 1.02],
    pad:  [padBuf, 0.00, 0.60], drop: [dropBuf, 0.00, 0.84], stream: [streamBuf, -0.30, 0.30],
    cave: [caveBuf, 0.00, 0.92],
  };
  for (const name of SOLO) {
    const v = VOICE[name];
    if (v) place(v[0], v[1], v[2]);
    else console.warn(`  ! solo: voice '${name}' not mapped in the JS reference (zoo/flute/fembell/gong are C-only) — skipped`);
  }
} else {
// flatterbop's OWN priorities (mirrors flatterbop180.c): bells/vib forward,
// beepy lead just behind, ethereal room. (No zoo here — C-only.)
place(leadBuf,   0.00, 0.90);
place(bassBuf,   0.00, 0.62);
place(vibBuf,   -0.28, 0.98);
place(deepBuf,   0.00, 0.94);
place(riseBuf,   0.00, 0.88);
place(scrBuf,    0.34, 0.52);
place(screamBuf,-0.48, 0.42);
place(sparkBuf,  0.48, 0.90);
place(bellBuf,  -0.46, 1.00);
place(kickBuf,   0.00, 1.06);
place(subBuf,    0.00, 0.98);
place(snareBuf,  0.04, 0.70);
place(hatBuf,    0.34, 0.50);
place(shakBuf,  -0.34, 0.44);
reverbInPlace(caveBuf, { wet: 0.56, decay: 0.80 });
place(caveBuf,   0.00, 0.94);

reverbInPlace(streamBuf, { wet: 0.36, decay: 0.62 });
place(streamBuf, -0.30, 0.32);
place(streamBuf,  0.30, 0.32);

place(padBuf,    0.00, 0.60);

delayInPlace(dropBuf, { time: BEAT * 0.75, fb: 0.42, wet: 0.48 });
reverbInPlace(dropBuf, { wet: 0.52, decay: 0.74 });
place(dropBuf,   0.00, 0.84);

// ethereal — mirrors scene_amt() in flatterbop180.c (~1.6× the 360's sends;
// even the driving passes keep a wash)
const REVERB_SCENES = {
  intro:      { bell: 0.48, spark: 0.30, vib: 0.32, pad: 0.48, lead: 0.22, rise: 0.28, scream: 0.24 },
  butterfly:  { bell: 0.30, spark: 0.22, lead: 0.14, scream: 0.22, vib: 0.18 },
  palofmine:  { bell: 0.28, spark: 0.18, lead: 0.14, vib: 0.18 },
  mommywow:   { bell: 0.52, pad: 0.48, vib: 0.30, lead: 0.20, spark: 0.26 },
  slinky:     { scream: 0.20, bell: 0.18, lead: 0.10 },
  fly:        { bell: 0.20, lead: 0.12, vib: 0.16 },
  ride:       { scream: 0.46, scr: 0.42, bell: 0.18, rise: 0.22 },
  cave:       { scream: 0.36, vib: 0.32, pad: 0.34, lead: 0.18 },
  progression:{ bell: 0.26, spark: 0.18, lead: 0.12 },
  land:       { bell: 0.38, pad: 0.32, lead: 0.18 },
  button:     { bell: 0.44, pad: 0.36 },
};
const SEND_VOICES = { lead: leadBuf, bell: bellBuf, spark: sparkBuf, vib: vibBuf, pad: padBuf, scr: scrBuf, scream: screamBuf, rise: riseBuf };
const spaceSend = new Float32Array(ns);
const RVZ = 100;
const rvSpan = Math.ceil((ns / SR) * RVZ) + 1;
const sceneAt = (name, voice) => (REVERB_SCENES[name] ?? {})[voice] ?? 0;
for (const [vname, vbuf] of Object.entries(SEND_VOICES)) {
  const env = new Float32Array(rvSpan);
  for (let k = 0; k < rvSpan; k++) {
    const tt = k / RVZ;
    let amt = 0;
    for (const s of placed) if (tt >= s.bar0 * BAR && tt < (s.bar0 + s.bars) * BAR) { amt = sceneAt(s.name, vname); break; }
    env[k] = amt;
  }
  const a2 = 0.04;
  for (let k = 1; k < rvSpan; k++) env[k] = env[k] * a2 + env[k - 1] * (1 - a2);
  for (let k = rvSpan - 2; k >= 0; k--) env[k] = env[k] * a2 + env[k + 1] * (1 - a2);
  for (let i = 0; i < ns; i++) { const g = env[Math.min(rvSpan - 1, Math.floor((i / SR) * RVZ))]; if (g > 0.0004) spaceSend[i] += vbuf[i] * g; }
}
reverbInPlace(spaceSend, { wet: 1.0, decay: 0.78 });
place(spaceSend, 0.00, 0.50);
}

// scrub non-finite
let nan = 0;
for (let i = 0; i < ns; i++) {
  if (!Number.isFinite(outL[i])) { outL[i] = 0; nan++; }
  if (!Number.isFinite(outR[i])) { outR[i] = 0; nan++; }
}
if (nan) console.warn(`     ! scrubbed ${nan} non-finite samples`);

// loop fold
let outN = ns;
if (LOOP) {
  const loopN = Math.round(TOTAL_BARS * BAR * SR);
  for (let i = loopN; i < ns; i++) { const d = i - loopN; if (d < loopN) { outL[d] += outL[i]; outR[d] += outR[i]; } }
  outN = loopN;
}

let peak = 0;
for (let i = 0; i < outN; i++) { const a = Math.abs(outL[i]); if (a > peak) peak = a; const b = Math.abs(outR[i]); if (b > peak) peak = b; }
if (peak > 0 && !SOLO) { const nrm = 0.84 / peak; for (let i = 0; i < outN; i++) { outL[i] *= nrm; outR[i] *= nrm; } }

// FINAL-CHORUS LIFT — +2.4 dB into the F chorus (linear master only)
if (!LOOP && !SOLO) {
  const fF = placed.find((s) => s.key === 7);
  if (fF) {
    const s0 = Math.floor(t(fF.bar0, 0) * SR), ramp = Math.floor(2.0 * SR);
    const gMax = Math.pow(10, 2.4 / 20);
    for (let i = s0; i < outN; i++) {
      const g = 1 + (gMax - 1) * Math.min(1, (i - s0) / ramp);
      outL[i] *= g; outR[i] *= g;
    }
  }
}

let trimN;
if (SOLO) {
  trimN = outN;
} else {
  if (LOOP) {
    trimN = outN;
  } else {
    let lastLoud = outN - 1;
    while (lastLoud > 0 && Math.abs(outL[lastLoud]) < 0.004 && Math.abs(outR[lastLoud]) < 0.004) lastLoud--;
    trimN = Math.min(outN, lastLoud + Math.floor(0.6 * SR));
    // HARD-CLAMP to exactly TARGET_SEC — a player reads "3:00.000", never "3:01"
    trimN = Math.min(trimN, Math.round(TARGET_SEC * SR));
  }
  let _hss = 0x51ed2701 >>> 0;
  const _hr = () => { _hss = (_hss + 0x6d2b79f5) >>> 0; let x = Math.imul(_hss ^ (_hss >>> 15), 1 | _hss); x = (x + Math.imul(x ^ (x >>> 7), 61 | x)) ^ x; return ((x ^ (x >>> 14)) >>> 0) / 4294967296 * 2 - 1; };
  let hlpL = 0, hlpR = 0; const HLP = 0.22, HISS = 0.0055;
  const drive = 1.5, dnorm = Math.tanh(drive);
  for (let i = 0; i < trimN; i++) {
    let l = Math.tanh(outL[i] * drive) / dnorm;
    let r = Math.tanh(outR[i] * drive) / dnorm;
    hlpL += (_hr() - hlpL) * HLP; hlpR += (_hr() - hlpR) * HLP;
    outL[i] = l + hlpL * HISS; outR[i] = r + hlpR * HISS;
  }
  if (!LOOP) {
    // quick arrival — the dawn is AT the door, not down the road
    const fadeIn = Math.floor(1.5 * SR), fadeOut = Math.floor(1.4 * SR);
    for (let i = 0; i < fadeIn && i < trimN; i++) { const g = Math.pow(i / fadeIn, 1.4); outL[i] *= g; outR[i] *= g; }
    for (let i = 0; i < fadeOut && i < trimN; i++) { const idx = trimN - 1 - i, g = i / fadeOut; outL[idx] *= g; outR[idx] *= g; }
  } else {
    const FLOOR = 0.45, swell = Math.floor(2.5 * SR);
    for (let i = 0; i < swell && i < trimN; i++) { const g = FLOOR + (1 - FLOOR) * Math.pow(i / swell, 1.4); outL[i] *= g; outR[i] *= g; }
    const edge = Math.floor(0.06 * SR);
    for (let i = 0; i < edge && i < trimN; i++) { const g = i / edge; const j = trimN - 1 - i; outL[j] *= g; outR[j] *= g; }
  }
}

console.log(`→ flatterbop180 · ${TOTAL_BARS} bars · 3/4 @ ${BPM.toFixed(1)} BPM · ${(trimN / SR).toFixed(3)} s · modulates C→E♭→(home)→F→G · easy techno-waltz · physmar lead (C-only) · menagerie loud · deep swing=${SWING.toFixed(2)}${LOOP ? " · LOOP" : ""}`);

// ── write out (JS reference master) ─────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", LOOP ? "flatterbop180-loop.mp3" : "flatterbop180.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) { b.writeFloatLE(outL[i], i * 8); b.writeFloatLE(outR[i], i * 8 + 4); }
writeFileSync(rawPath, b);

const MASTER = [
  "highpass=f=26",
  "equalizer=f=52:t=q:w=0.9:g=2.2",
  "equalizer=f=100:t=q:w=1.0:g=1.6",
  "equalizer=f=240:t=q:w=1.0:g=1.7",
  "equalizer=f=420:t=q:w=1.3:g=0.9",
  "equalizer=f=900:t=q:w=1.2:g=-0.6",
  "equalizer=f=2600:t=q:w=1.4:g=0.2",
  "equalizer=f=11000:t=q:w=0.7:g=2.6",
  "treble=g=1.6:f=13500",
  "lowpass=f=18000",
  "vibrato=f=0.6:d=0.45",
  "vibrato=f=5.0:d=0.16",
  "acompressor=threshold=-18dB:ratio=2.5:attack=18:release=210:makeup=2.4:knee=8",
  "alimiter=limit=0.96:attack=5:release=90",
].join(",");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER, "-c:a", "libmp3lame", "-q:a", "2", outPath], { stdio: "inherit" });
if (ff.status !== 0) { try { unlinkSync(rawPath); } catch {} console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath} (JS reference master · ${(trimN / SR).toFixed(1)} s)`);

const wavOut = outPath.replace(/\.mp3$/i, "") + ".distrokid.wav";
const ffw = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", `${MASTER},loudnorm=I=-14:TP=-1.5:LRA=11`,
  "-ar", "44100", "-ac", "2", "-c:a", "pcm_s16le", wavOut], { stdio: "inherit" });
if (!KEEP_RAW) { try { unlinkSync(rawPath); } catch {} }
else console.log(`  (kept pre-master raw → ${rawPath})`);
if (ffw.status === 0) console.log(`✓ ${wavOut} (DistroKid master · 44.1 kHz / 16-bit · -14 LUFS)`);
else console.error("✗ DistroKid WAV master failed");

// struct.json — section map
{
  const structTotalSec = trimN / SR;
  const struct = {
    _comment: "Section map for flatterbop180 (the easy 3/4 techno zoo-bop). 3/4 @ 180 BPM; one bar = one second; the form == 180 s exactly. Modulates C→E♭→(home C)→F→G; keyOffset is semitones above the C home key.",
    meter: 3, bpm: +BPM.toFixed(3), scale: "major", rootMidi: 60,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: placed.map((s) => ({ name: s.name, keyOffset: s.key, startSec: +t(s.bar0, 0).toFixed(6), endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6) })),
    events: Object.fromEntries(Object.entries(NOTE_EVENTS).map(
      ([lane, evs]) => [lane, evs.filter((e) => e.t < structTotalSec).sort((a, b) => a.t - b.t)])),
  };
  writeFileSync(resolve(HERE, "..", "out", "flatterbop180.struct.json"), JSON.stringify(struct, null, 2) + "\n");
}
