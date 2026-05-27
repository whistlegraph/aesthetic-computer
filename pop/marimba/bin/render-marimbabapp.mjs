#!/usr/bin/env node
// render-marimbabapp.mjs — render marimbaba++ to mp3.
//
// The disco-shuffle remix of marimbaba. The score doc lives in
// pop/marimba/marimbabapp.np; this file holds the same music as
// scheduled events + procedural drum/bass DSP.
//
// Meter is 6/8 compound: ~115 BPM dotted-quarter, bar ≈ 1.043 s,
// 6 eighths per bar. The marimba hook keeps marimbaba's triple feel
// (6/8 is a compound 3/4) over a four-on-the-floor kick.
//
// Voices:
//   rosewood        — marimba hook lead + "the float" bridge
//   vibraphone      — disco chicken-scratch chord stabs
//   vibraphone_off  — held pad chords
//   kalimba         — high offbeat sparkle + drop counter-melody
//   staccato        — section-closing 16th fills
//   drums + bass    — procedural DSP (kick / clap / hats / octave bass)
//
// Run:
//   node pop/marimba/bin/render-marimbabapp.mjs
//   node pop/marimba/bin/render-marimbabapp.mjs --out ~/marimbabapp.mp3

import { mixEventMarimba } from "../synths/marimba.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;

// ── timing — 6/8 compound disco-shuffle ───────────────────────────────
const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };
const BPM = Number(_argi("--bpm")) || 115;   // dotted-quarter pulse
const EIGHTH = 60 / BPM / 3;                 // one eighth note (s)
const BAR = 6 * EIGHTH;                      // 6/8 bar (s)
const TOTAL_BARS = 352;
const totalSec = TOTAL_BARS * BAR + 2.4;     // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// bar/eighth → seconds. e may be fractional (16th = 0.5).
const tE = (bar, e = 0) => bar * BAR + e * EIGHTH;

function midiToFreq(m) { return 440 * Math.pow(2, (m - 69) / 12); }

// ── note table ────────────────────────────────────────────────────────
const N = {
  Bb1: 34, C2: 36, D2: 38, E2: 40, F2: 41, G2: 43, A2: 45, Bb2: 46,
  C3: 48, D3: 50, E3: 52, F3: 53, G3: 55, A3: 57, Bb3: 58,
  C4: 60, D4: 62, E4: 64, F4: 65, G4: 67, A4: 69, Bb4: 70,
  C5: 72, D5: 74, E5: 76, F5: 77, G5: 79, A5: 81, Bb5: 82,
  C6: 84, D6: 86, E6: 88, F6: 89, G6: 91,
};

// ── deterministic RNG (so re-renders are bit-identical) ───────────────
let _rng = 0x2f6b1e9d >>> 0;
function rnd() {
  _rng = (Math.imul(_rng, 1664525) + 1013904223) >>> 0;
  return _rng / 0xffffffff;
}
const noise = () => rnd() * 2 - 1;

// ── output buffers — one mono bus per group, panned at mixdown ────────
const drumBuf   = new Float32Array(ns);   // kick / clap / hats / crash / riser
const bassBuf   = new Float32Array(ns);   // disco octave bass
const leadBuf   = new Float32Array(ns);   // marimba hook + bridge (rosewood)
const compBuf   = new Float32Array(ns);   // vibraphone chord stabs
const sparkBuf  = new Float32Array(ns);   // kalimba sparkle
const padBuf    = new Float32Array(ns);   // vibraphone_off held pad

// kick onset times — drive the sidechain pump
const kickTimes = [];

// ═══════════════════════════════════════════════════════════════════
//  PROCEDURAL DRUM / BASS DSP
// ═══════════════════════════════════════════════════════════════════

// kick — body sine with a 110→47 Hz pitch drop + a short beater click.
function kick(t0, gain = 0.95) {
  kickTimes.push(t0);
  const i0 = Math.floor(t0 * SR);
  const dur = 0.20, nK = Math.floor(dur * SR);
  let ph = 0;
  for (let i = 0; i < nK; i++) {
    const dst = i0 + i; if (dst < 0 || dst >= ns) continue;
    const x = i / SR;
    const f = 47 + 63 * Math.exp(-x * 52);          // pitch drop
    ph += f / SR;
    const body = Math.sin(2 * Math.PI * ph);
    const ampEnv = Math.exp(-x / 0.052);            // punchy body decay
    let s = body * ampEnv;
    if (x < 0.006) s += noise() * 0.5 * (1 - x / 0.006);  // beater click
    drumBuf[dst] += s * gain;
  }
}

// generic band-limited noise hit (snare / hats / crash) — white noise
// through one-pole highpass then one-pole lowpass, exp amplitude env.
function noiseHit(t0, durSec, gain, hpHz, lpHz, tau) {
  const i0 = Math.floor(t0 * SR);
  const nH = Math.ceil(durSec * SR);
  const hpA = Math.exp(-2 * Math.PI * hpHz / SR);
  const lpA = 1 - Math.exp(-2 * Math.PI * lpHz / SR);
  let xPrev = 0, hp = 0, lp = 0;
  for (let i = 0; i < nH; i++) {
    const dst = i0 + i; if (dst < 0 || dst >= ns) continue;
    const x = i / SR;
    const w = noise();
    hp = hpA * (hp + w - xPrev); xPrev = w;
    lp += lpA * (hp - lp);
    drumBuf[dst] += lp * Math.exp(-x / tau) * gain;
  }
}

// disco clap — four quick noise spits, then a short room tail.
function clap(t0, gain = 0.5) {
  const offs = [0, 0.011, 0.022, 0.034];
  for (const o of offs) noiseHit(t0 + o, 0.05, gain * 0.85, 1100, 4200, 0.018);
  noiseHit(t0 + 0.034, 0.18, gain * 0.55, 1300, 3600, 0.060);   // tail
}
const hatClosed = (t0, g = 0.17) => noiseHit(t0, 0.05, g, 7000, 17000, 0.013);
const hatOpen   = (t0, g = 0.20) => noiseHit(t0, 0.22, g, 6800, 17000, 0.085);
const crash     = (t0, g = 0.42) => {
  noiseHit(t0, 1.6, g, 2600, 16000, 0.62);
  noiseHit(t0, 0.30, g * 0.6, 5000, 17000, 0.10);
};

// white-noise riser — rising lowpass + rising amplitude over durSec.
function riser(t0, durSec, gain = 0.34) {
  const i0 = Math.floor(t0 * SR);
  const nR = Math.ceil(durSec * SR);
  let lp = 0;
  for (let i = 0; i < nR; i++) {
    const dst = i0 + i; if (dst < 0 || dst >= ns) continue;
    const frac = i / nR;
    const fc = 400 + 9000 * frac * frac;
    const lpA = 1 - Math.exp(-2 * Math.PI * fc / SR);
    lp += lpA * (noise() - lp);
    drumBuf[dst] += lp * frac * frac * gain;
  }
}

// disco octave bass — sawtooth pluck, lowpass-decay per note.
function bassNote(t0, midi, durSec, gain) {
  const f = midiToFreq(midi);
  const i0 = Math.floor(t0 * SR);
  const nB = Math.ceil((durSec + 0.04) * SR);
  const durN = Math.floor(durSec * SR);
  const attN = Math.floor(0.004 * SR);
  let ph = 0, lp = 0;
  for (let i = 0; i < nB; i++) {
    const dst = i0 + i; if (dst < 0 || dst >= ns) continue;
    const x = i / SR;
    ph += f / SR; if (ph >= 1) ph -= 1;
    const saw = 2 * ph - 1;
    // cutoff sweeps bright→dark for a plucked attack
    const fc = Math.min(4800, 1.4 * f + 2400 * Math.exp(-x / 0.055));
    const lpA = 1 - Math.exp(-2 * Math.PI * fc / SR);
    lp += lpA * (saw - lp);
    let env = i < attN ? i / attN : Math.exp(-(i - attN) / SR / 0.12);
    if (i > durN) env *= Math.exp(-(i - durN) / SR / 0.02);   // note-off
    bassBuf[dst] += lp * env * gain;
  }
}

// ═══════════════════════════════════════════════════════════════════
//  MARIMBA VOICE SCHEDULING
// ═══════════════════════════════════════════════════════════════════
// decayMul per role — disco wants drier, snappier marimba than the
// lullaby's dreamy ring (render-marimbaba uses 1.4–1.8).
function lead(t0, midi, durSec, gain)  { mixEventMarimba({ startSec: t0, midi, durSec, gain, preset: "rosewood",       decayMul: 0.85 }, leadBuf,  { sampleRate: SR }); }
function comp(t0, midi, durSec, gain)  { mixEventMarimba({ startSec: t0, midi, durSec, gain, preset: "vibraphone",     decayMul: 0.34 }, compBuf,  { sampleRate: SR }); }
function spark(t0, midi, durSec, gain) { mixEventMarimba({ startSec: t0, midi, durSec, gain, preset: "kalimba",        decayMul: 0.70 }, sparkBuf, { sampleRate: SR }); }
function pad(t0, midi, durSec, gain)   { mixEventMarimba({ startSec: t0, midi, durSec, gain, preset: "vibraphone_off", decayMul: 1.55 }, padBuf,   { sampleRate: SR }); }
function chop(t0, midi, durSec, gain)  { mixEventMarimba({ startSec: t0, midi, durSec, gain, preset: "staccato",       decayMul: 0.55 }, leadBuf,  { sampleRate: SR }); }

// ═══════════════════════════════════════════════════════════════════
//  HARMONY
// ═══════════════════════════════════════════════════════════════════
// 8-bar cycle, 2 bars per chord: F vi(Dm) IV(Bb) V(C). chordIdx 0..3.
function chordOf(bar) { return Math.floor((bar % 8) / 2); }
// key lift: bars >= 264 transpose +2 (F major → G major).
const transposeAt = (bar) => (bar >= 264 ? 2 : 0);

// bass roots (low octave) per chord, key of F
const BASS_ROOT = [N.F2, N.D2, N.Bb1, N.C2];
// vibraphone comp chord tones per chord, around C4–C5
const COMP_TONES = [
  [N.F4, N.A4, N.C5],   // F
  [N.D4, N.F4, N.A4],   // Dm
  [N.Bb3, N.D4, N.F4],  // Bb
  [N.C4, N.E4, N.G4],   // C
];
// sparkle arpeggio pitches per chord, upper octave
const SPARK_TONES = [
  [N.C6, N.F6, N.A5],   // F
  [N.D6, N.A5, N.F6],   // Dm
  [N.Bb5, N.F6, N.D6],  // Bb
  [N.C6, N.G6, N.E6],   // C
];

// ═══════════════════════════════════════════════════════════════════
//  THE HOOK — 8 bars, derived from marimbaba [twinkle] + [ba-ba-ba bap]
// ═══════════════════════════════════════════════════════════════════
// each entry: [hookBar, eighth, midi, durEighths, gain]
const HOOK = [
  [0, 0, N.F5, 2, 0.52], [0, 2, N.A5, 1, 0.55], [0, 3, N.C6, 2, 0.58], [0, 5, N.A5, 1, 0.48],
  [1, 0, N.G5, 3, 0.50], [1, 3, N.F5, 2, 0.50], [1, 5, N.A5, 1, 0.46],
  [2, 0, N.F5, 1, 0.50], [2, 1, N.G5, 1, 0.50], [2, 2, N.A5, 1, 0.54], [2, 3, N.G5, 1, 0.50], [2, 4, N.F5, 2, 0.50],
  [3, 0, N.F5, 6, 0.50],
  [4, 0, N.A5, 1, 0.56], [4, 1, N.G5, 1, 0.52], [4, 2, N.A5, 1, 0.56], [4, 3, N.F5, 3, 0.52],
  [5, 0, N.C6, 1, 0.58], [5, 1, N.Bb5, 1, 0.52], [5, 2, N.C6, 1, 0.58], [5, 3, N.A5, 3, 0.52],
  [6, 0, N.G5, 2, 0.52], [6, 2, N.F5, 2, 0.50], [6, 4, N.E5, 2, 0.48],
  [7, 0, N.F5, 6, 0.50],
];

// "the float" — the NEW bridge melody for the breakdown (8 bars).
const FLOAT = [
  [0, 0, N.C6, 3, 0.40], [0, 3, N.D6, 3, 0.42],
  [1, 0, N.C6, 2, 0.40], [1, 2, N.A5, 4, 0.38],
  [2, 0, N.Bb5, 3, 0.40], [2, 3, N.G5, 3, 0.38],
  [3, 0, N.A5, 6, 0.40],
  [4, 0, N.D6, 3, 0.42], [4, 3, N.C6, 3, 0.40],
  [5, 0, N.A5, 2, 0.40], [5, 2, N.G5, 4, 0.38],
  [6, 0, N.F5, 3, 0.40], [6, 3, N.A5, 3, 0.40],
  [7, 0, N.G5, 6, 0.40],
];

// ═══════════════════════════════════════════════════════════════════
//  PATTERN HELPERS
// ═══════════════════════════════════════════════════════════════════

// drum kit for one bar. hat: "full" | "half" | "sparse" | "none".
function kit(bar, { doKick = true, doClap = true, hat = "full", openEvery = 2, g = 1 } = {}) {
  if (doKick) { kick(tE(bar, 0), 0.95 * g); kick(tE(bar, 3), 0.92 * g); }
  if (doClap) clap(tE(bar, 3), 0.50 * g);
  if (hat === "full") {
    for (const e of [1, 2, 4, 5]) hatClosed(tE(bar, e), (e === 2 || e === 5 ? 0.20 : 0.15) * g);
    if (openEvery && bar % openEvery === 1) hatOpen(tE(bar, 5), 0.20 * g);
  } else if (hat === "half") {
    hatClosed(tE(bar, 2), 0.16 * g); hatClosed(tE(bar, 5), 0.18 * g);
  } else if (hat === "sparse") {
    hatClosed(tE(bar, 5), 0.13 * g);
  }
}

// disco octave bass for one bar — low/high alternating eighths.
function discoBass(bar, { g = 1 } = {}) {
  const root = BASS_ROOT[chordOf(bar)] + transposeAt(bar);
  for (let e = 0; e < 6; e++) {
    const midi = root + (e % 2 === 1 ? 12 : 0);
    const accent = (e === 0 || e === 3) ? 1.0 : 0.72;
    bassNote(tE(bar, e), midi, EIGHTH * 0.92, 0.46 * accent * g);
  }
}

// vibraphone chicken-scratch chord stabs on the offbeats.
function compStabs(bar, { g = 1, full = true } = {}) {
  const tones = COMP_TONES[chordOf(bar)];
  const tr = transposeAt(bar);
  const slots = full ? [1, 2, 4, 5] : [2, 5];
  for (const e of slots) {
    const accent = (e === 2 || e === 5) ? 1.0 : 0.7;
    for (const m of tones) comp(tE(bar, e), m + tr, EIGHTH * 0.7, 0.085 * accent * g);
  }
}

// kalimba sparkle — offbeat upper-octave arpeggio.
function sparkleArp(bar, { g = 1, density = 1 } = {}) {
  const tones = SPARK_TONES[chordOf(bar)];
  const tr = transposeAt(bar);
  const slots = density >= 1 ? [1.5, 3.5, 5] : [3.5];
  tones; // (silence lint — tones used below)
  for (let i = 0; i < slots.length; i++) {
    spark(tE(bar, slots[i]), tones[i % tones.length] + tr, EIGHTH * 1.2, 0.20 * g);
  }
}

// play one bar of the 8-bar hook into leadBuf.
function hookBar(bar, hb, { g = 1 } = {}) {
  const tr = transposeAt(bar);
  for (const [h, e, midi, dur, gn] of HOOK) {
    if (h !== hb) continue;
    lead(tE(bar, e), midi + tr, EIGHTH * dur, gn * g);
  }
}

// play one bar of "the float" bridge into leadBuf.
function floatBar(bar, fb, { g = 1 } = {}) {
  for (const [h, e, midi, dur, gn] of FLOAT) {
    if (h !== fb) continue;
    lead(tE(bar, e), midi, EIGHTH * dur, gn * g);
  }
}

// held pad chord across [bar0, bar0+bars).
function padChord(bar0, bars, { g = 1 } = {}) {
  const tones = COMP_TONES[chordOf(bar0)];
  const tr = transposeAt(bar0);
  for (const m of tones) pad(tE(bar0, 0), m + tr, BAR * bars, 0.085 * g);
}

// section-closing staccato 16th tumble across one bar.
function fill16(bar, { g = 1 } = {}) {
  const run = [N.C6, N.Bb5, N.A5, N.G5, N.F5, N.E5, N.D5, N.C5,
               N.A5, N.G5, N.F5, N.D5];
  const tr = transposeAt(bar);
  for (let i = 0; i < run.length; i++) {
    chop(tE(bar, i * 0.5), run[i] + tr, EIGHTH * 0.4, 0.34 * g);
  }
}

// snare-roll accelerando across [bar0, bar0+bars) — hits get denser.
function snareRoll(bar0, bars) {
  for (let b = 0; b < bars; b++) {
    const bar = bar0 + b;
    const subdiv = b < bars * 0.4 ? 2 : b < bars * 0.75 ? 3 : 6;
    for (let s = 0; s < subdiv; s++) {
      const e = (s / subdiv) * 6;
      const g = 0.18 + 0.32 * (b / bars);
      noiseHit(tE(bar, e), 0.07, g, 1400, 4800, 0.026);
    }
  }
}

// ═══════════════════════════════════════════════════════════════════
//  ARRANGEMENT — the disco club arc
// ═══════════════════════════════════════════════════════════════════

const SECTIONS = [
  { name: "intro",     bar0: 0,   bars: 16  },
  { name: "grooveA",   bar0: 16,  bars: 48  },
  { name: "break",     bar0: 64,  bars: 24  },
  { name: "grooveB",   bar0: 88,  bars: 64  },
  { name: "breakdown", bar0: 152, bars: 40  },
  { name: "build",     bar0: 192, bars: 24  },
  { name: "drop",      bar0: 216, bars: 96  },
  { name: "outro",     bar0: 312, bars: 40  },
];

// ── §0 intro — kick + hats fade up, bass joins ────────────────────────
for (let b = 0; b < 16; b++) {
  const bar = b;
  const g = Math.min(1, 0.3 + b / 14);
  if (b < 4)        kit(bar, { doClap: false, hat: "sparse", g });
  else if (b < 8)   kit(bar, { doClap: false, hat: "half",   g });
  else              kit(bar, { hat: "full", openEvery: 4,    g });
  if (b >= 8) discoBass(bar, { g: g * 0.85 });
  if (b === 15) fill16(bar, { g: 0.8 });
}

// ── §1 groove A — full kit + bass + chicken-scratch stabs ─────────────
for (let b = 0; b < 48; b++) {
  const bar = 16 + b;
  kit(bar, { hat: "full" });
  discoBass(bar);
  compStabs(bar, { full: b >= 8 });
  if (b % 4 === 2) sparkleArp(bar, { density: 0 });
  // hook teaser — first two bars of the hook every 8 bars
  if (b % 8 >= 4 && b % 8 <= 5) hookBar(bar, b % 8 - 4, { g: 0.7 });
  if (b % 8 === 7) fill16(bar, { g: 0.7 });
}

// ── §2 break — drums drop out, pad blooms, filtered feel ──────────────
for (let b = 0; b < 24; b++) {
  const bar = 64 + b;
  if (b % 2 === 0) padChord(bar, 2, { g: 1.0 });
  compStabs(bar, { full: false, g: 0.6 });
  if (b % 4 === 1) sparkleArp(bar, { density: 0, g: 0.8 });
  hatClosed(tE(bar, 5), 0.10);            // a ghost of the groove
  if (b >= 8 && b < 20) discoBass(bar, { g: 0.4 + 0.5 * ((b - 8) / 12) });
  if (b >= 16) clap(tE(bar, 3), 0.30 + 0.2 * ((b - 16) / 8));
  if (b >= 20) riser(tE(bar, 0), BAR, 0.10 + 0.18 * ((b - 20) / 4));
  if (b === 23) crash(tE(bar + 1, 0));    // crash into groove B
}

// ── §3 groove B — the FULL marimba hook, looping ──────────────────────
for (let b = 0; b < 64; b++) {
  const bar = 88 + b;
  kit(bar, { hat: "full" });
  discoBass(bar);
  compStabs(bar, { full: true });
  hookBar(bar, b % 8);
  if (b % 8 >= 6) sparkleArp(bar, { density: 1 });
  if (b % 16 === 15) fill16(bar);
  if (b % 32 === 0) crash(tE(bar, 0), 0.34);
}

// ── §4 breakdown — drums out, "the float" bridge (NEW melody) ─────────
for (let b = 0; b < 40; b++) {
  const bar = 152 + b;
  if (b % 2 === 0) padChord(bar, 2, { g: 1.1 });
  floatBar(bar, b % 8, { g: 1.0 });
  if (b % 4 === 2) sparkleArp(bar, { density: 0, g: 0.9 });
  // a slow heartbeat kick keeps a pulse without breaking the calm
  if (b >= 24 && b % 2 === 0) kick(tE(bar, 0), 0.5 + 0.3 * ((b - 24) / 16));
}

// ── §5 build — snare-roll accelerando + riser ─────────────────────────
snareRoll(192, 24);
for (let b = 0; b < 24; b++) {
  const bar = 192 + b;
  if (b < 16 && b % 2 === 0) kick(tE(bar, 0), 0.6 + 0.3 * (b / 16));
  if (b >= 16) kit(bar, { doClap: false, hat: "half", g: 0.5 + 0.5 * ((b - 16) / 8) });
  if (b % 2 === 0) padChord(bar, 2, { g: 0.7 });
  compStabs(bar, { full: false, g: 0.4 + 0.5 * (b / 24) });
  if (b >= 8) riser(tE(bar, 0), BAR, 0.10 + 0.30 * ((b - 8) / 16));
}
crash(tE(216, 0), 0.5);

// ── §6 drop — everything, max; key lifts +2 to G major at bar 264 ─────
for (let b = 0; b < 96; b++) {
  const bar = 216 + b;
  kit(bar, { hat: "full", openEvery: 2 });
  discoBass(bar);
  compStabs(bar, { full: true });
  hookBar(bar, b % 8, { g: 1.05 });
  sparkleArp(bar, { density: 1 });
  // after the key lift, kalimba doubles a counter-melody an octave up
  if (bar >= 264 && b % 2 === 1) {
    const tones = SPARK_TONES[chordOf(bar)];
    spark(tE(bar, 0.5), tones[0] + transposeAt(bar) - 12, EIGHTH * 2, 0.16);
    spark(tE(bar, 3.5), tones[1] + transposeAt(bar) - 12, EIGHTH * 2, 0.16);
  }
  if (b % 16 === 15) fill16(bar);
}
crash(tE(264, 0), 0.5);   // crash on the key change

// ── §7 outro — elements peel away, marimba rings out ──────────────────
for (let b = 0; b < 40; b++) {
  const bar = 312 + b;
  const fade = Math.max(0, 1 - b / 38);
  if (b < 16) {
    kit(bar, { hat: b < 8 ? "full" : "half", openEvery: 4, g: fade });
    discoBass(bar, { g: fade });
    compStabs(bar, { full: false, g: fade });
    hookBar(bar, b % 8, { g: fade });
  } else if (b < 28) {
    if (b % 2 === 0) kick(tE(bar, 0), 0.5 * fade);
    compStabs(bar, { full: false, g: 0.5 * fade });
    hookBar(bar, b % 8, { g: 0.7 * fade });
    if (b % 2 === 0) padChord(bar, 2, { g: 0.8 });
  } else {
    if (b % 2 === 0) padChord(bar, 2, { g: 0.9 });
    if (b % 8 === 0) hookBar(bar, 7, { g: 0.5 });
  }
}
// the final F-major chord — the lullaby tucked back in.
pad(tE(350, 0), N.F4, BAR * 4, 0.16);
pad(tE(350, 0), N.A4, BAR * 4, 0.14);
pad(tE(350, 0), N.C5, BAR * 4, 0.13);
lead(tE(350, 0), N.F5, BAR * 3, 0.34);

// ═══════════════════════════════════════════════════════════════════
//  SIDECHAIN PUMP
// ═══════════════════════════════════════════════════════════════════
// build a duck curve: 0 at each kick onset, recovering toward 1.
kickTimes.sort((a, b) => a - b);
const duck = new Float32Array(ns);
{
  let ki = 0, lastKick = -1e9;
  const tau = 0.075;
  for (let i = 0; i < ns; i++) {
    const t = i / SR;
    while (ki < kickTimes.length && kickTimes[ki] <= t) { lastKick = kickTimes[ki]; ki++; }
    const x = t - lastKick;
    duck[i] = x < 0 ? 1 : 1 - Math.exp(-x / tau);
  }
}
// apply, with per-group depth (how hard the kick pumps each bus).
function pump(buf, depth) {
  for (let i = 0; i < ns; i++) buf[i] *= (1 - depth) + depth * duck[i];
}
pump(bassBuf,  0.62);
pump(compBuf,  0.52);
pump(padBuf,   0.66);
pump(leadBuf,  0.30);
pump(sparkBuf, 0.32);

// ═══════════════════════════════════════════════════════════════════
//  STEREO MIXDOWN
// ═══════════════════════════════════════════════════════════════════
const outL = new Float32Array(ns);
const outR = new Float32Array(ns);

// equal-power pan: p in [-1,1].
function pan(p) {
  const a = (p + 1) * Math.PI / 4;
  return [Math.cos(a), Math.sin(a)];
}
function place(buf, p, gain) {
  const [lg, rg] = pan(p);
  for (let i = 0; i < ns; i++) { const s = buf[i] * gain; outL[i] += s * lg; outR[i] += s * rg; }
}
place(drumBuf,  0.00, 0.95);
place(bassBuf,  0.00, 0.90);
place(compBuf, -0.32, 0.95);
place(leadBuf,  0.12, 1.00);
place(sparkBuf, 0.42, 0.85);
// pad gets a 13 ms haas widen on the right channel
{
  const [lg, rg] = [0.72, 0.72];
  const haas = Math.floor(0.013 * SR);
  for (let i = 0; i < ns; i++) {
    outL[i] += padBuf[i] * lg;
    if (i - haas >= 0) outR[i] += padBuf[i - haas] * rg;
  }
}

// scrub non-finite, normalise to -1.3 dBFS peak
let nan = 0;
for (let i = 0; i < ns; i++) {
  if (!Number.isFinite(outL[i])) { outL[i] = 0; nan++; }
  if (!Number.isFinite(outR[i])) { outR[i] = 0; nan++; }
}
if (nan) console.warn(`     ! scrubbed ${nan} non-finite samples`);
let peak = 0;
for (let i = 0; i < ns; i++) {
  const a = Math.abs(outL[i]); if (a > peak) peak = a;
  const b = Math.abs(outR[i]); if (b > peak) peak = b;
}
if (peak > 0) { const nrm = 0.86 / peak; for (let i = 0; i < ns; i++) { outL[i] *= nrm; outR[i] *= nrm; } }

// auto-trim trailing silence + fades
let lastLoud = ns - 1;
while (lastLoud > 0 && Math.abs(outL[lastLoud]) < 0.004 && Math.abs(outR[lastLoud]) < 0.004) lastLoud--;
const trimN = Math.min(ns, lastLoud + Math.floor(0.4 * SR));
const fadeIn = Math.floor(0.004 * SR);
const fadeOut = Math.floor(2.6 * SR);   // longer disco-style fade
for (let i = 0; i < fadeIn && i < trimN; i++) { const g = i / fadeIn; outL[i] *= g; outR[i] *= g; }
for (let i = 0; i < fadeOut && i < trimN; i++) {
  const idx = trimN - 1 - i, g = i / fadeOut;
  outL[idx] *= g; outR[idx] *= g;
}

console.log(`→ marimbabapp · ${TOTAL_BARS} bars · F→G major 6/8 @ ${BPM} BPM dotted · ${kickTimes.length} kicks · ${(trimN / SR).toFixed(1)} s`);

// ── write out ─────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", "marimbabapp.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) {
  b.writeFloatLE(outL[i], i * 8);
  b.writeFloatLE(outR[i], i * 8 + 4);
}
writeFileSync(rawPath, b);

// pop master chain — punchier than the lullaby: tighter glue comp,
// a low-shelf lift for the disco kick + bass, air on top, brickwall.
const MASTER = [
  "highpass=f=28",
  "acompressor=threshold=-18dB:ratio=3:attack=12:release=160:makeup=2.6:knee=5",
  "bass=g=2.0:f=90",
  "treble=g=1.6:f=8000",
  "alimiter=limit=0.96:attack=3:release=55",
].join(",");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER, "-c:a", "libmp3lame", "-q:a", "2", outPath], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath} (pop-mastered · ${(trimN / SR).toFixed(1)} s)`);

// ── struct.json — the club arc, for any future visualizer ─────────────
// Parallels marimbaba.struct.json. Section bounds are derived from the
// SECTIONS table; preview tooling reads totalSec + sections[].
{
  const structTotalSec = trimN / SR;
  const struct = {
    _comment: "Section map for marimbaba++ (the disco-shuffle remix). 6/8 compound, 115 BPM dotted-quarter → bar ≈ 1.043 s. F major lifting to G major at bar 264. Derived from render-marimbabapp.mjs SECTIONS.",
    meter: 6, bpm: BPM, scale: "major", rootMidi: 65,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: SECTIONS.map((s) => ({
      name: s.name,
      startSec: +tE(s.bar0, 0).toFixed(6),
      endSec: +Math.min(structTotalSec, tE(s.bar0 + s.bars, 0)).toFixed(6),
    })),
  };
  writeFileSync(resolve(HERE, "..", "out", "marimbabapp.struct.json"),
    JSON.stringify(struct, null, 2) + "\n");
}
