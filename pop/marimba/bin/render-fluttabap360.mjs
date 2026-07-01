#!/usr/bin/env node
// render-fluttabap360.mjs — FLUTTABAP360 COMPOSITION + SCORE BAKER.
//
// ⚠ AUDIO PATH DEPRECATED (2026-06): for fluttabap360 the canonical renderer is
// now the C engine (pop/marimba/c/fluttabap360.c). This file's job is to
// COMPOSE the piece and BAKE the score; the C engine does ALL the DSP, mixdown,
// reverb-scene automation, finalize, and master. The JS mixdown/finalize below
// is kept only for reference/A-B and is NO LONGER the release path.
//
// Canonical render (album cut + seamless loop + DistroKid wav):
//   node bin/render-fluttabap360.mjs --bake out/fluttabap360.score.txt
//   node c/run-c.mjs out/fluttabap360.score.txt --out out/fluttabap360.mp3 --wav
//   node c/run-c.mjs out/fluttabap360.score.txt --out out/fluttabap360-loop.mp3 --loop --wav
//
// a fatter, butterier, six-minute cousin of flutterbap. Same butterfly-park DNA
// + synthesized rhythm section (../synths/perc.mjs + ../synths/marimba.mjs), but:
//
//   • exactly 360 s — the form is hard-clamped to 6:00.000 so a player reads
//     "6:00", never "6:01" (the final ring fades out right on the boundary)
//   • a KEY-CHANGE JOURNEY — the form modulates C → D → E → (home) → F across
//     the four passes so the song keeps lifting (see ARRANGEMENT `key:`)
//   • FATTER low end — buttery: a clean sine sub layered under a grittier,
//     sub-reinforced bass-perc, warmer master tilt
//   • more POPPIN + GROOVIN — busier kits, 16th "ride" hats, four-on-floor
//     lifts, escalating intensity across four passes of the hook
//   • SQUEAK-RIDES that SING — short melodic scratch+squeak duets that swell
//     IN and OUT of the fight (a hump envelope), each voice on its own
//     pitched riff, blooming in a shared space-reverb send
//   • HELD pad swells at every modulation + a deeper overall room
//
// The flutterbap melodic material (butterfly / pal-of-mine / mommy-wow /
// slinky / fly / land) is transcribed into reusable BLOCKS and arranged
// over four escalating passes with ride breaks, cave breakdowns, a chord
// progression and a buttoned finish.
//
// Run:
//   node pop/marimba/bin/render-fluttabap360.mjs
//   node pop/marimba/bin/render-fluttabap360.mjs --loop      # seam-perfect
//   node pop/marimba/bin/render-fluttabap360.mjs --out ~/x.mp3
//
// 4/4. bar = 4·60/BPM. BPM is solved so TOTAL_BARS bars == 360 s.

import { mixEventMarimba, MARIMBA_PRESETS } from "../synths/marimba.mjs";
import { mixKick, mixBassPerc, mixSnare, mixHat, mixShaker, mixReverseBell, mixReverseKick, mixScratch, mixScream, renderHat } from "../synths/perc.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";
import { satParams } from "../../lib/substrate.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;
const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };

// the SUBSTRATE — the medium the track is printed onto (tape / vinyl / …). Its
// per-sample print params (drive/bias/hiss) are baked into the score so the C
// engine prints with them; its name is baked so run-c builds the right master.
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

// ── KEY — global transpose in semitones. The dispatch sets this per section
// so the whole form MODULATES across the six minutes (C → D → E → home → F).
// Every PITCHED route adds gKey; the unpitched kit (kick/snare/hat/shaker)
// ignores it so the groove stays put while the melody climbs. ──────────────
let gKey = 0;
const TR = (m) => m + gKey;

// ── note-event telemetry for the storyline visualizer. struct.events is
// keyed by the visualizer's voice lanes; each event is { t, midi, dur } in
// seconds, recording the SOUNDING pitch (post-transpose). ──────────────────
const NOTE_EVENTS = { rosewood: [], vibraphone: [], kalimba: [], bass: [], bubbles: [] };
function EV(lane, t0, midi, beats) {
  NOTE_EVENTS[lane].push({ t: +t0.toFixed(4), midi: TR(midi), dur: +(beats * BEAT).toFixed(4) });
}

// ══════════════════════════════════════════════════════════════════════
//  ARRANGEMENT — summed in bars first; the BPM is back-solved from this
//  so the whole form is exactly TARGET_SEC. opts: { lvl, ride, gMul }.
//  lvl = groove intensity (0 light → 3 peak). ride = 16th ride-hats.
// ══════════════════════════════════════════════════════════════════════
// `key` (semitones) modulates from here on; sections without it inherit it.
const ARRANGEMENT = [
  ["intro", 10, { key: 0 }],                                         // ── home: C — slow water-drop dawn → build
  // ── PASS A — establish the hop (light → driving), in C ──
  ["butterfly", 4, { lvl: 0 }], ["palofmine", 4, { lvl: 1 }], ["mommywow", 4],
  ["slinky", 8, { lvl: 1 }],    ["fly", 8, { lvl: 2 }],
  ["ride", 4, { lvl: 2 }],
  // ── PASS B — fatter, busier — LIFT to D (+2) ──
  ["butterfly", 4, { lvl: 1, ride: true, key: 2 }], ["palofmine", 4, { lvl: 1, ride: true }], ["mommywow", 4],
  ["slinky", 8, { lvl: 2 }],   ["fly", 8, { lvl: 3 }],
  ["ride", 4, { lvl: 3 }],                                            // (was 8 — the long 2:00 fight, halved + melodic)
  ["cave", 8],
  // ── PASS C — biggest, brightest — LIFT to E (+4) ──
  ["butterfly", 4, { lvl: 2, ride: true, key: 4 }], ["palofmine", 4, { lvl: 2, ride: true }], ["mommywow", 4],
  ["slinky", 8, { lvl: 2 }],   ["fly", 8, { lvl: 3 }],
  ["ride", 4, { lvl: 3 }],                                            // (was 8 — halved)
  // ── PASS D — second wind in E, drop HOME for a breath, then FINAL lift ──
  ["butterfly", 4, { lvl: 2, ride: true }], ["palofmine", 4, { lvl: 2, ride: true }],
  ["slinky", 8, { lvl: 2 }],   ["fly", 8, { lvl: 3 }],
  ["ride", 4, { lvl: 3 }],
  ["cave", 8, { key: 0 }],                                           // home breakdown — the calm before the lift
  // FINAL CHORUS — euphoric jump up to F (+5), and ride it out
  ["butterfly", 4, { lvl: 2, ride: true, key: 5 }], ["palofmine", 4, { lvl: 2, ride: true }], ["mommywow", 4],
  ["slinky", 8, { lvl: 2 }],   ["fly", 8, { lvl: 3 }],
  ["progression", 4], ["land", 4], ["button", 1],
];

const TOTAL_BARS = ARRANGEMENT.reduce((s, [, bars]) => s + bars, 0);
const TARGET_SEC = Number(_argi("--sec")) || 360;
// back-solve BPM so TOTAL_BARS bars == TARGET_SEC (override with --bpm).
const BPM = Number(_argi("--bpm")) || (TOTAL_BARS * 4 * 60) / TARGET_SEC;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const totalSec = TOTAL_BARS * BAR + 3.0;     // + tail for the final ring
const ns = Math.ceil(totalSec * SR);

// ── loop mode (same fold as flutterbap) ────────────────────────────────
const LOOP = process.argv.includes("--loop");
// --keep-raw → keep the pre-master f32 (for the C-engine parity harness,
// pop/marimba/c/compare.mjs, which diffs JS vs C pre-master sample-for-sample).
const KEEP_RAW = process.argv.includes("--keep-raw");

// ── C-ENGINE BAKE — `--bake <file>` records every (already humanized) voice
// event into a flat score the C engine (pop/marimba/c/fluttabap360.c) replays,
// then exits before mixdown. `--solo a,b` restricts BOTH the JS mix and the
// bake to named voice types and skips the finalize stage (fuzz/hiss/normalize/
// lift/fades) so a partial C port can be A/B'd voice-by-voice against JS.
// Voices are ported (and added to the score) one milestone at a time. ───────
const BAKE_PATH = _argi("--bake");
const BAKING = BAKE_PATH != null;
const SOLO = (() => { const v = _argi("--solo"); return v ? new Set(v.split(",").map((s) => s.trim())) : null; })();
const soloed = (name) => !SOLO || SOLO.has(name);
const SCORE = [];   // baked event lines
function rec(line) { if (BAKING) SCORE.push(line); }

// bake a modal-marimba voice: resolve the preset⊕overrides EXACTLY as
// renderMarimba does, then emit every param explicitly (partials/amps/decays
// arrays included) so the C engine needs no preset table. lane → place args.
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

// bake a modal-marimba voice that routes to a NON-lane bus (plink → dropBuf,
// cave vib/lead → caveBuf). `tag` is the C score token; the engine reads the
// same field layout as `marimba <lane>` minus the lane. ev carries preset +
// decayMul; default opts (no params/attack override). ───────────────────────
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

// ── swing ──────────────────────────────────────────────────────────────
const SWING = Math.min(0.72, Math.max(0.5, Number(_argi("--swing")) || 0.60));
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

// ── output buffers — one mono bus per voice, panned at mixdown ─────────
const leadBuf  = new Float32Array(ns);
const bassBuf  = new Float32Array(ns);
const sparkBuf = new Float32Array(ns);
const bellBuf  = new Float32Array(ns);
const vibBuf   = new Float32Array(ns);
const deepBuf  = new Float32Array(ns);   // butter sine sub — the "like butter" low end
const riseBuf  = new Float32Array(ns);
const scrBuf   = new Float32Array(ns);
const screamBuf = new Float32Array(ns);
const kickBuf  = new Float32Array(ns);
const subBuf   = new Float32Array(ns);   // bass-perc
const snareBuf = new Float32Array(ns);
const hatBuf   = new Float32Array(ns);
const shakBuf  = new Float32Array(ns);
const caveBuf  = new Float32Array(ns);
const padBuf   = new Float32Array(ns);   // long held swells — the sustained "held notes"
const dropBuf  = new Float32Array(ns);   // water-drop intro — echo + big reverb at mixdown
const streamBuf = new Float32Array(ns);  // continuous flowing-brook bed — band-passed noise + bubble voices, reverbed wide at mixdown

// ── mallet voice routes ────────────────────────────────────────────────
// MAIN MARIMBA LEAD — softened: a longer, rounder attack, a longer mallet
// contact (low-passes the bright transient), the stick-click noise removed,
// and the glassy upper partials pulled back. Softer touch, less harsh, with
// a touch more ring. (Per-call opts so only the lead is softened.)
function lead(t0, midi, beats, gain = 1.0) {
  EV("rosewood", t0, midi, beats);
  const ev = { startSec: jit(t0, 13), midi: TR(midi), durSec: beats * BEAT, gain: vel(gain, 0.14), preset: "xylophone", decayMul: 1.3 };
  const opts = { sampleRate: SR, attack: 0.0032, params: { mallet: 0.0032, noise: 0.0, amps: [1.0, 0.34, 0.13, 0.04], resGain: 0.42 } };
  recMarimba("lead", ev, opts);
  mixEventMarimba(ev, leadBuf, opts);
}
function bass(t0, midi, beats, gain = 0.85) {
  EV("bass", t0, midi, beats);
  const ev = { startSec: jit(t0, 9), midi: TR(midi), durSec: beats * BEAT, gain: vel(gain, 0.12), preset: "bass", decayMul: 0.55 };
  recMarimba("bass", ev, { sampleRate: SR });
  mixEventMarimba(ev, bassBuf, { sampleRate: SR });
}
function spark(t0, midi, beats, gain = 0.55) {
  EV("kalimba", t0, midi, beats);
  const ev = { startSec: jit(t0, 14), midi: TR(midi), durSec: beats * BEAT, gain: vel(gain, 0.16), preset: "kalimba", decayMul: 0.95 };
  recMarimba("spark", ev, { sampleRate: SR });
  mixEventMarimba(ev, sparkBuf, { sampleRate: SR });
}
// ── DYNABELL — a struck bell whose WAVEFORM SHAPE evolves across the track:
// round SINE early → reedy TRIANGLE through the body → ~30 s of buzzy SQUARE
// in the euphoric F chorus → eased back. One global automation, bellShape(sec),
// is read at each strike, so the whole bell choir morphs together. Additive
// odd-harmonic model (sine = fundamental only; triangle = odd/n², alternating
// sign; square = odd/n, same sign), RMS-normalised so loudness holds steady as
// the spectrum opens up. Higher partials damp faster → it still rings like a
// real bell (bright transient, mellow tail) while wearing whatever shape the
// automation calls for. ──
const BELL_KEYS = [
  [0,    0.00],   // dawn + pass A — round sine bells
  [90,   0.00],
  [200,  0.50],   // pass B / C — warm triangle bells
  [305,  0.50],
  [312,  1.00],   // FINAL F CHORUS — ~30 s of buzzy square bells
  [340,  1.00],
  [346,  0.60],   // land / button — settle back toward triangle
  [360,  0.55],
];
function bellShape(sec) {
  if (sec <= BELL_KEYS[0][0]) return BELL_KEYS[0][1];
  for (let i = 1; i < BELL_KEYS.length; i++) {
    const [t1, v1] = BELL_KEYS[i], [t0, v0] = BELL_KEYS[i - 1];
    if (sec <= t1) return v0 + (v1 - v0) * ((sec - t0) / (t1 - t0));
  }
  return BELL_KEYS[BELL_KEYS.length - 1][1];
}
// odd-harmonic partial amplitudes for a given morph position (0=sine, 0.5=tri, 1=sq)
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
// dynabell is now PHASE-ACCUMULATED so it can BEND: `bend` semitones glides
// into the target pitch over `bendTime` s (a scoopy attack), and `vib`/`vibHz`
// add a swelling vibrato after the strike — the "pitch-bendy" bells.
function dynabell(t0, midi, beats, gain = 0.65, opts = {}) {
  EV("bubbles", t0, midi, beats);
  const start = jit(t0, 10), g = vel(gain, 0.12);
  const f = mf(TR(midi)), dur = beats * BEAT;
  const shape = bellShape(start);
  const bend = opts.bend ?? 0, bendTime = opts.bendTime ?? 0.09;
  const vibD = opts.vib ?? 0, vibHz = opts.vibHz ?? 5.5;
  const tail = opts.tail ?? 1;   // >1 = longer ring (lengthen the decay on big melody bells)
  // bake: dynabell → bellBuf. C replays from (start f dur g shape bend bendTime vib vibHz tail).
  rec(`dynabell ${start.toFixed(8)} ${f.toFixed(6)} ${dur.toFixed(8)} ${g.toFixed(6)} ${shape.toFixed(6)} `
    + `${bend.toFixed(6)} ${bendTime.toFixed(6)} ${vibD.toFixed(6)} ${vibHz.toFixed(6)} ${tail.toFixed(6)}`);
  const nHarm = Math.max(1, Math.min(31, (Math.floor((SR * 0.45) / f) | 1)));   // odd, under Nyquist
  const amps = bellSpectrum(shape, nHarm);
  const n0 = Math.floor(start * SR);
  const len = Math.ceil((dur * 1.35 * tail + 0.08) * SR);
  const attS = Math.max(1, Math.floor(0.003 * SR));
  const relA = Math.log(900) / (dur * 1.35 * tail);   // long bell ring (tail stretches it)
  const dt = 1 / SR;
  let bph = 0;   // fundamental phase in CYCLES (accumulated so pitch can move)
  for (let i = 0; i < len; i++) {
    const d = n0 + i; if (d < 0 || d >= ns) continue;
    const tsec = i * dt;
    // pitch movement in semitones: a glide from `bend` → 0 over bendTime, plus
    // a vibrato that eases in over the first 120 ms.
    let semis = 0;
    if (bend && tsec < bendTime) semis += bend * (1 - tsec / bendTime);
    if (vibD) semis += vibD * Math.sin(2 * Math.PI * vibHz * tsec) * Math.min(1, tsec / 0.12);
    bph += (f * Math.pow(2, semis / 12)) * dt;
    const att = i < attS ? i / attS : 1;
    let s = 0;
    for (let n = 1; n <= nHarm; n += 2) {
      const a = amps[n]; if (a === 0) continue;
      const env = Math.exp(-relA * tsec * (1 + (n - 1) * 0.08));   // higher partials decay faster
      s += a * env * Math.sin(2 * Math.PI * n * bph);
    }
    bellBuf[d] += s * att * g * 0.62;
  }
}
// ── BELL RUN — an elaborate, optionally FAST + bendy bell figure: a run of
// dynabell notes, each scooping up into pitch. `notes` is [beatOffset, midi,
// beats] triples relative to (ab, beat0). Used to make the bell line sing in
// quick complex flurries that grow busier as the track climbs. ──
function bellRun(ab, beat0, notes, { gain = 0.5, bend = -2.0, vib = 0.12, tail = 1 } = {}) {
  for (const [bo, midi, beats] of notes)
    dynabell(t(ab, beat0 + bo), midi, beats, gain, { bend, bendTime: 0.07, vib, vibHz: 6, tail });
}
// BASS BELL — a low, long, deeply pitch-bendy dynabell (a scoop up from a
// fourth/fifth below into a sustained low ring) for body under the spacious bits.
function bassBell(ab, beat, midi, beats, gain = 0.6) {
  dynabell(t(ab, beat), midi, beats, gain, { bend: -5, bendTime: 0.22, vib: 0.18, vibHz: 4.5 });
}
// ── REVERSE DYNABELL ("boo") — render a dynabell forward, then write it
// REVERSED so it swells UP and sucks INTO the land point: the reverse-sucky
// flourish, wearing the morphing bell shape. Non-bendy (closed-form phase) so
// it stays bit-exact across the C port. ──
function revbell(landSec, midi, beats, gain = 0.5) {
  const f = mf(TR(midi)), dur = beats * BEAT, shape = bellShape(landSec);
  // bake: revbell land f dur g shape  (C renders forward → writes reversed)
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
  const endIdx = Math.floor(landSec * SR);   // i=0 (loud attack) lands HERE; swell builds before it
  for (let i = 0; i < len; i++) { const d = endIdx - i; if (d < 0 || d >= ns) continue; bellBuf[d] += tmp[i] * gain; }
}
// ── FEMBELL — a physically-modeled bell (pop/bell's FEM shell model) as an
// accent voice: real inharmonic partials, retuned per note. Bake-only (the C
// engine holds the modal tables + synthesis, routed to the bell bus). presets:
// "handbell" (bright struck), "church" (big deep toll), "bowl" (pure shimmer). ─
const FEMBELL_PRESET = { handbell: 0, church: 1, bowl: 2 };
function fembell(t0, midi, gain = 0.55, preset = "handbell", tauScale = 1.0) {
  EV("bubbles", t0, midi, 4);
  const start = jit(t0, 8), f0 = mf(TR(midi));
  rec(`fembell ${start.toFixed(8)} ${f0.toFixed(6)} ${vel(gain, 0.1).toFixed(6)} ${FEMBELL_PRESET[preset] ?? 0} ${tauScale.toFixed(6)}`);
}
// ── FLUTE — the AC physically-modeled flute (gm_synth's Cook jet-waveguide,
// ported into the C engine). Bake-only here. preset "flute" or "panflute"
// (breathier); slow attack/release = AMBIENT sustained flute, fast = melodic. ──
const FLUTE_PRESET = { flute: 0, panflute: 1 };
function flute(t0, midi, beats, gain = 0.5, { preset = "flute", attack = 0.06, release = 0.3 } = {}) {
  const start = jit(t0, 6), f0 = mf(TR(midi)), dur = beats * BEAT;
  rec(`flute ${start.toFixed(8)} ${f0.toFixed(6)} ${dur.toFixed(8)} ${vel(gain, 0.08).toFixed(6)} `
    + `${FLUTE_PRESET[preset] ?? 0} ${attack.toFixed(6)} ${release.toFixed(6)}`);
}
// ── GONG — a gnarly, long-blooming gong (dense FEM steel/glass spectrum +
// shimmer bloom + crash, in the C engine, routed to the cave reverb). Bake-only.
// tauScale stretches the ring; gongs sit LOW so the wash is felt. ──
function gong(t0, midi, gain = 0.8, tauScale = 1.0) {
  const start = jit(t0, 6), f0 = mf(TR(midi));
  rec(`gong ${start.toFixed(8)} ${f0.toFixed(6)} ${vel(gain, 0.06).toFixed(6)} ${tauScale.toFixed(6)}`);
}
// BOOWOOP — the reverse "boo" sucking up into a bendy dynabell "woop".
function boowoop(ab, beat, midi, beats = 2, gain = 0.55) {
  const land = t(ab, beat);
  revbell(land, midi, Math.min(beats, 2.5), gain * 0.85);                         // the "boo" suck-in
  dynabell(land, midi, beats, gain, { bend: -3, bendTime: 0.16, vib: 0.16, vibHz: 5 });  // the "woop"
}
// bell() now wears the evolving shape — every existing bell call morphs with it.
const bell = dynabell;
function vib(t0, midi, beats, gain = 0.4) {
  EV("vibraphone", t0, midi, beats);
  const ev = { startSec: jit(t0, 18), midi: TR(midi), durSec: beats * BEAT, gain: vel(gain, 0.10), preset: "vibraphone_off", decayMul: 1.0 };
  recMarimba("vib", ev, { sampleRate: SR });
  mixEventMarimba(ev, vibBuf, { sampleRate: SR });
}
// ── BUTTER SUB — a clean enveloped sine an octave anchor under the root.
// This is the "like butter" weight: smooth, round, felt more than heard. ─
function SUBSINE(ab, beat, midi, beats, g = 0.5) {
  const start = jit(t(ab, beat), 4);
  const f = mf(TR(midi)), dur = beats * BEAT;
  // bake: the C engine replays this exact sine-env voice from (start, f, dur, g)
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

// ── HELD PAD — a slow-attack sustained chord that BLOOMS across `bars`.
// These are the held notes that open each new key and float under the hush:
// a soft choir of detuned partials (root + octave + fifth-ish) routed to
// padBuf, which is fed into the space reverb at mixdown so it sits deep. ──
function PAD(ab, midis, bars, g = 0.4) {
  const start = t(ab, 0), dur = bars * BAR;
  // bake: held-pad sine choir → padBuf. midis are post-transpose (TR applied
  // per-partial below); record the SOUNDING midis so C reproduces them.
  rec(`pad ${start.toFixed(8)} ${dur.toFixed(8)} ${g.toFixed(6)} ${midis.length} ${midis.map(TR).join(" ")}`);
  const n0 = Math.floor(start * SR);
  const len = Math.ceil((dur + 0.5) * SR);
  const att = Math.max(1, Math.floor(dur * 0.42 * SR));   // slow swell IN
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
              + 0.5 * Math.sin(2 * Math.PI * phase * 1.5)        // a fifth of shimmer
              + 0.35 * Math.sin(2 * Math.PI * phase * 0.5);      // sub octave warmth
      padBuf[d] += s * env * g * 0.45;
    }
  }
}

// ── WATER DROP — a short pitched "plink": a sine that chirps UP fast then
// decays away, with a tiny click attack. Pitched high + placed irregularly =
// rain falling on a still pond. Routed to dropBuf, which gets a tempo-synced
// echo + a big reverb at mixdown (the "lots of echoes" of the dawn). ──
function drop(t0, f0, g = 0.45, dur = 0.24) {
  // bake: water-drop chirp → dropBuf.
  rec(`drop ${t0.toFixed(8)} ${f0.toFixed(6)} ${g.toFixed(6)} ${dur.toFixed(8)}`);
  const n0 = Math.floor(t0 * SR), ln = Math.ceil(dur * SR), dt = 1 / SR;
  let ph = 0;
  for (let i = 0; i < ln; i++) {
    const d = n0 + i; if (d < 0 || d >= ns) continue;
    const u = i / ln;
    const f = f0 * (1 + 1.7 * Math.min(1, u * 3.4));   // fast upward chirp = the plink
    ph += f * dt;
    let env = Math.exp(-u * 6.2);
    if (i < 20) env *= i / 20;                          // tiny click attack
    const s = Math.sin(2 * Math.PI * ph) + 0.16 * Math.sin(4 * Math.PI * ph);
    dropBuf[d] += s * env * g * 0.6;
  }
  // telemetry: water drops light the bubbles lane (pitch from the chirp f0)
  NOTE_EVENTS.bubbles.push({ t: +t0.toFixed(4), midi: Math.round(69 + 12 * Math.log2(f0 / 440)), dur: +dur.toFixed(4) });
}

// ── WATER STREAM — a continuous flowing-brook BED, all synthesized into
// streamBuf (no samples). It is AMBIENT, never a note: it stays out of
// NOTE_EVENTS / the visualizer lanes. Three layers, summed sample-by-sample:
//
//   1. RUSH  — white noise through a state-variable band-pass (~300 Hz–3 kHz)
//      for the broadband hiss of moving water, slowly amplitude-modulated by a
//      cluster of incommensurate LFOs so it surges and eases like real current.
//   2. BURBLE — a handful of narrow resonant band-pass "bubble" voices whose
//      centre frequencies wander (random-walk) through the gloop range, fed by
//      their own noise — the trickles and gurgles riding on top of the rush.
//   3. POPS  — sparse, random little pitched bubble-pops (adapted from drop(),
//      at low gain) so the bed reads as a STREAM, not just filtered hiss.
//
// `gateAt(t)` returns 0→1: how present the stream is at time t (the caller
// gates it UP in the quiet sections, DOWN under the busy ones). A dedicated
// RNG keeps it deterministic without disturbing the humanize stream. ──
function waterStream(gateAt) {
  // its own deterministic noise source (don't perturb hrand's timing stream)
  let ws = 0x9e3779b1 >>> 0;
  const wrand = () => {
    ws = (ws + 0x6d2b79f5) >>> 0;
    let x = Math.imul(ws ^ (ws >>> 15), 1 | ws);
    x = (x + Math.imul(x ^ (x >>> 7), 61 | x)) ^ x;
    return ((x ^ (x >>> 14)) >>> 0) / 4294967296;
  };
  const noise = () => wrand() * 2 - 1;

  // ── layer 1: the broadband RUSH (one band-pass state-variable filter) ──
  // SVF coefficients for a wide band centred ~950 Hz, gentle Q.
  const fc = 950, q = 0.7;
  const f = 2 * Math.sin(Math.PI * fc / SR), damp = 1 / q;
  let lp = 0, bp = 0;
  // slow surge LFOs — incommensurate rates so the swell never loops audibly
  const lfo = [
    { r: 0.07, p: 0.0 }, { r: 0.13, p: 1.7 }, { r: 0.031, p: 4.2 }, { r: 0.21, p: 2.9 },
  ];

  // ── layer 2: wandering BURBLE band-pass voices (the gloops/trickles) ──
  const NB = 5;
  const burble = [];
  for (let k = 0; k < NB; k++) burble.push({
    fc: 500 + wrand() * 1800,        // centre wanders in the gloop range
    target: 500 + wrand() * 1800,
    lp: 0, bp: 0,
    amp: 0.5 + wrand() * 0.5,
    lfoR: 0.05 + wrand() * 0.25, lfoP: wrand() * 6.283,   // each gurgle pulses
  });

  const dt = 1 / SR;
  for (let i = 0; i < ns; i++) {
    const tt = i * dt;
    const gate = gateAt(tt);
    if (gate <= 0) continue;

    // RUSH: amplitude-modulated band-passed noise
    let amR = 1;
    for (const L of lfo) amR *= 0.78 + 0.22 * Math.sin(2 * Math.PI * L.r * tt + L.p);
    const n = noise();
    lp += f * bp;
    const hp = n - lp - damp * bp;
    bp += f * hp;
    let s = bp * 0.32 * amR;

    // BURBLE: a few narrow resonant voices, centres slowly wandering
    for (const v of burble) {
      // random-walk the centre toward a target, repick on arrival
      v.fc += (v.target - v.fc) * 0.00006;
      if (Math.abs(v.fc - v.target) < 8) v.target = 420 + wrand() * 2000;
      const vf = 2 * Math.sin(Math.PI * Math.min(6000, v.fc) / SR);
      const vn = noise();
      v.lp += vf * v.bp;
      const vhp = vn - v.lp - 0.18 * v.bp;   // high Q (0.18 damp ≈ Q 5.5) = resonant gloop
      v.bp += vf * vhp;
      const pulse = 0.45 + 0.55 * Math.max(0, Math.sin(2 * Math.PI * v.lfoR * tt + v.lfoP));
      s += v.bp * 0.16 * v.amp * pulse;
    }

    streamBuf[i] += s * gate * 0.5;
  }

  // ── layer 3: sparse pitched bubble-POPS — the same chirp shape as drop()
  // but written straight into streamBuf at low gain (NO telemetry: the stream
  // is ambient, it never lights a visualizer lane). Random across the whole
  // span, denser where the gate is open. ──
  const HZ = [880, 1180, 1480, 990, 1320, 760, 1620, 1040];
  let hi = 0;
  for (let tt = 1.0; tt < ns / SR - 1.0; ) {
    tt += 0.18 + wrand() * 1.4;                       // irregular spacing
    const g = gateAt(tt);
    if (g > 0.15 && wrand() < g) {
      const f0 = HZ[hi++ % HZ.length] * (1 + (wrand() * 2 - 1) * 0.06);
      const pg = (0.06 + 0.10 * g) * g, pdur = 0.14 + wrand() * 0.10;
      const n0 = Math.floor(tt * SR), ln = Math.ceil(pdur * SR);
      let ph = 0;
      for (let j = 0; j < ln; j++) {
        const d = n0 + j; if (d < 0 || d >= ns) continue;
        const u = j / ln;
        const pf = f0 * (1 + 1.7 * Math.min(1, u * 3.4));   // fast upward chirp
        ph += pf * dt;
        let env = Math.exp(-u * 6.4);
        if (j < 20) env *= j / 20;                          // tiny click attack
        streamBuf[d] += (Math.sin(2 * Math.PI * ph) + 0.16 * Math.sin(4 * Math.PI * ph)) * env * pg * 0.6;
      }
    }
  }
}
// ── PLINK — a long-decay glassy tone into dropBuf (rides the same echo +
// reverb): the slow intro's sparse melody with the "long decaaaays". ──
function plink(t0, midi, beats, g = 0.5) {
  EV("bubbles", t0, midi, beats);
  const ev = { startSec: t0, midi: TR(midi), durSec: beats * BEAT, gain: g, preset: "glockenspiel", decayMul: 2.8 };
  recMarimbaBus("plink", ev);   // glock → dropBuf
  mixEventMarimba(ev, dropBuf, { sampleRate: SR });
}

// ── kit routes ──────────────────────────────────────────────────────────
function KICK(ab, beat, g = 0.95, o = {}) {
  const ev = { startSec: jit(t(ab, beat), 4), gain: vel(g, 0.10), fEnd: 40, ampDecay: 0.48, pitchDecay: 0.064, drive: 1.85, ...o };
  // bake: resolve renderKick's defaults (fStart 150, click 0.55) so the C
  // engine has every param explicitly — no hidden defaults across languages.
  rec(`kick ${ev.startSec.toFixed(8)} ${ev.gain.toFixed(6)} ${(ev.fStart ?? 150).toFixed(4)} ${ev.fEnd.toFixed(4)} ${ev.pitchDecay.toFixed(6)} ${ev.ampDecay.toFixed(6)} ${(ev.click ?? 0.55).toFixed(4)} ${ev.drive.toFixed(6)}`);
  mixKick(ev, kickBuf, { sampleRate: SR });
}
function SNR(ab, beat, g = 0.7, o = {}) {
  const ev = { startSec: jit(t(ab, beat), 5), gain: vel(g, 0.14), ...o };
  // bake: resolve renderSnare's defaults so the C engine has every param.
  rec(`snare ${ev.startSec.toFixed(8)} ${ev.gain.toFixed(6)} ${(ev.toneDecay ?? 0.10).toFixed(6)} `
    + `${(ev.noiseDecay ?? 0.16).toFixed(6)} ${(ev.tone ?? 0.55).toFixed(6)} ${(ev.noise ?? 0.9).toFixed(6)} `
    + `${(ev.f1 ?? 185).toFixed(4)} ${(ev.f2 ?? 330).toFixed(4)}`);
  mixSnare(ev, snareBuf, { sampleRate: SR });
}
function HAT(ab, beat, g = 0.28, o = {}) {
  const ev = { startSec: jit(t(ab, beat), 6), gain: vel(g, 0.24), ...o };
  // bake: decay resolves from `open` (open? 0.26 : 0.045).
  rec(`hat ${ev.startSec.toFixed(8)} ${ev.gain.toFixed(6)} ${(ev.decay ?? (ev.open ? 0.26 : 0.045)).toFixed(6)}`);
  mixHat(ev, hatBuf, { sampleRate: SR });
}
function SHK(ab, beat, g = 0.2) {
  const ev = { startSec: jit(t(ab, beat), 7), gain: vel(g, 0.22) };
  rec(`shaker ${ev.startSec.toFixed(8)} ${ev.gain.toFixed(6)} ${(0.06).toFixed(6)} ${(0.008).toFixed(6)}`);
  mixShaker(ev, shakBuf, { sampleRate: SR });
}
// FAT bass-perc — extra sub reinforcement + grit by default.
function BP(ab, beat, midi, durBeats, g = 0.8, o = {}) {
  const ev = { startSec: jit(t(ab, beat), 5), midi: TR(midi), durSec: durBeats * BEAT, gain: vel(g, 0.12), sub: 0.58, drive: 2.0, ...o };
  // bake: resolve renderBassPerc's defaults (pitchUp 5, pitchDecay 0.022,
  // decay 0.5, click 0.18). durSec is explicit. C replays subperc → subBuf.
  rec(`subperc ${ev.startSec.toFixed(8)} ${ev.midi} ${ev.durSec.toFixed(8)} ${ev.gain.toFixed(6)} `
    + `${(ev.pitchUp ?? 5).toFixed(4)} ${(ev.pitchDecay ?? 0.022).toFixed(6)} ${(ev.decay ?? 0.5).toFixed(6)} `
    + `${(ev.drive ?? 2.2).toFixed(6)} ${(ev.sub ?? 0.35).toFixed(6)} ${(ev.click ?? 0.18).toFixed(6)}`);
  mixBassPerc(ev, subBuf, { sampleRate: SR });
}
function rise(landBar, dur, midis, g) {
  const ev = { landSec: t(landBar, 0), dur, midis: midis.map(TR), gain: g };
  // bake: reverse-bell → riseBuf. Defaults: decay 2.6, ratio 3.5. midis explicit.
  rec(`rise ${ev.landSec.toFixed(8)} ${ev.dur.toFixed(8)} ${ev.gain.toFixed(6)} `
    + `${(ev.decay ?? 2.6).toFixed(6)} ${(ev.ratio ?? 3.5).toFixed(6)} ${ev.midis.length} ${ev.midis.join(" ")}`);
  mixReverseBell(ev, riseBuf, { sampleRate: SR });
}
function riseKick(landBar, dur, g) {
  // reverse-kick shares kickBuf with KICK; gate it off when soloing so the
  // 'kick' parity buffer stays pure (reverse-kick is its own later milestone).
  if (!soloed("revkick")) return;
  const ev = { landSec: t(landBar, 0), dur, gain: g };
  // bake: reverse-kick → kickBuf. Defaults: fStart 190, fEnd 48, pitchDecay
  // 0.13, ampDecay dur*0.55, drive 1.6.
  rec(`revkick ${ev.landSec.toFixed(8)} ${ev.dur.toFixed(8)} ${ev.gain.toFixed(6)} `
    + `${(ev.fStart ?? 190).toFixed(4)} ${(ev.fEnd ?? 48).toFixed(4)} ${(ev.pitchDecay ?? 0.13).toFixed(6)} `
    + `${(ev.ampDecay ?? dur * 0.55).toFixed(6)} ${(ev.drive ?? 1.6).toFixed(6)}`);
  mixReverseKick(ev, kickBuf, { sampleRate: SR });
}
function rhat(targetBuf, landAb, landBeat, g = 0.32) {
  // bake: reverse-hat (renderHat startSec 0 → seed, written reversed at
  // land−len). Route token by target bus: hatBuf → 'rhat', caveBuf → 'cavehat'.
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
// feedback delay (echo). `time` in seconds; `fb` = feedback amount (→ how many
// repeats); `wet` = dry/echo mix. Used to drench the water-drop intro.
function delayInPlace(buf, { time, fb = 0.45, wet = 0.5 } = {}) {
  const d = Math.max(1, Math.floor(time * SR));
  const out = new Float32Array(buf.length);
  for (let i = 0; i < buf.length; i++) {
    let y = buf[i];
    if (i >= d) y += out[i - d] * fb;     // feed the delayed output back in
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
  const ev = { startSec: jit(t(ab, beat), 8), midi: TR(midi), gain: vel(g, 0.15), ...SCR[kind] };
  // bake: resolve renderScratch's defaults (rate 7, depth 2.6, gate 0,
  // gateDuty 0.5, tone 0.55, dur 0.32). C replays scratch → scrBuf.
  rec(`scratch ${ev.startSec.toFixed(8)} ${ev.midi} ${ev.gain.toFixed(6)} ${(ev.dur ?? 0.32).toFixed(6)} `
    + `${(ev.rate ?? 7).toFixed(6)} ${(ev.depth ?? 2.6).toFixed(6)} ${(ev.gate ?? 0).toFixed(6)} `
    + `${(ev.gateDuty ?? 0.5).toFixed(6)} ${(ev.tone ?? 0.55).toFixed(6)}`);
  mixScratch(ev, scrBuf, { sampleRate: SR });
}
function SCREAM(ab, beat, midi, g = 0.4, o = {}) {
  const ev = { startSec: jit(t(ab, beat), 10), midi: TR(midi), gain: vel(g, 0.16), ...o };
  // bake: resolve renderScream's defaults (dur 0.5, bend 7, rasp 0.65).
  rec(`scream ${ev.startSec.toFixed(8)} ${ev.midi} ${ev.gain.toFixed(6)} ${(ev.dur ?? 0.5).toFixed(6)} `
    + `${(ev.bend ?? 7).toFixed(6)} ${(ev.rasp ?? 0.65).toFixed(6)}`);
  mixScream(ev, screamBuf, { sampleRate: SR });
}
// ── VORTEX — a performed turntable WARBLE: the player rides ON TOP of the
// track, super-samples it, and yanks it into a vortex. A rich saw whose pitch
// is sucked DOWN-and-back-UP in one big "throooop" (half-cosine dip + a faster
// ripple riding on it) while a resonant low-pass is wobbled by a rhythmic LFO
// — the wub-wub. The wob also gates the amplitude, so the whole gesture pulses
// in time and ripples back out. Routed to scrBuf → space reverb, so it blooms
// in the room alongside the squeaks instead of grinding flat up front. ──
function vortex(t0, midi, dur, opts = {}) {
  const start = t0, f0 = mf(TR(midi));
  const wubHz = opts.wub ?? (opts.wubDiv ?? 2) / BEAT;   // wobble rate, default 8ths
  const depthOct = opts.depth ?? 2.6;                    // DEEP suck — octaves of plunge
  const g = opts.gain ?? 0.5, q = opts.q ?? 0.88;        // higher Q = the filter SINGS the shoop
  // bake token (C parity): start f0 dur gain wubHz depthOct q
  rec(`vortex ${start.toFixed(8)} ${f0.toFixed(6)} ${dur.toFixed(8)} ${g.toFixed(6)} ${wubHz.toFixed(6)} ${depthOct.toFixed(6)} ${q.toFixed(6)}`);
  const n0 = Math.floor(start * SR), ln = Math.ceil(dur * SR), dt = 1 / SR;
  let phase = 0, lp = 0, bp = 0;
  const damp = Math.min(1.8, 1 / q);
  for (let i = 0; i < ln; i++) {
    const d = n0 + i; if (d < 0 || d >= ns) continue;
    const u = i / ln, tsec = i * dt;
    // the SHOOOOP — pitch sucks down HARD into the vortex and hangs low before
    // ripping back. skewed (^0.55) so it plunges fast and lingers in the pit;
    // a small overshoot at the tail snaps it back up.
    const dive = Math.pow(Math.sin(Math.PI * u), 0.55);  // fuller, lingering dip
    const snap = 0.12 * Math.sin(Math.PI * u) * Math.sin(2 * Math.PI * u);  // tail overshoot
    const ripple = 0.22 * Math.sin(2 * Math.PI * 3 * u); // the large-scale warble
    const pf = Math.pow(2, depthOct * -dive + snap + ripple);
    phase += f0 * pf * dt;
    const ph = phase - Math.floor(phase);
    const src = 2 * ph - 1;                              // naive saw — rich harmonics to filter
    // rhythmic WUB — and the cutoff ALSO TRACKS the pitch dive (pf), so the
    // brightness sucks down WITH the pitch: that's the shoopy vocal whoosh.
    const wub = 0.5 + 0.5 * Math.sin(2 * Math.PI * wubHz * tsec);
    const fc = (160 + 3200 * pf) * (0.42 + 0.58 * wub);
    const fco = Math.min(0.49, 2 * Math.sin(Math.PI * Math.min(fc, SR * 0.45) / SR));
    lp += fco * bp;
    const hp = src - lp - damp * bp;
    bp += fco * hp;
    const swell = Math.sin(Math.PI * u);                 // blooms in and out of the fight
    scrBuf[d] += lp * (0.35 + 0.65 * wub) * swell * g;
  }
}
function hats8(ab, g = 0.26, openLast = false) {
  for (let e = 0; e < 8; e++) HAT(ab, e * 0.5, e % 2 === 1 ? g : g * 0.55, { open: openLast && e === 7 });
}
function rideHats(ab, g = 0.18) {            // steady 16ths — the "ridin"
  for (let e = 0; e < 16; e++) HAT(ab, e * 0.25, e % 4 === 2 ? g * 1.35 : g * 0.7);
}

// ── shared groove + fat bass for a span of bars ─────────────────────────
function layFatGroove(ab, bars, { lvl = 1, subRoots = [N.C2], ride = false, kg = 0.94 } = {}) {
  for (let i = 0; i < bars; i++) {
    const b = ab + i, r = subRoots[i % subRoots.length], r5 = r === N.C2 ? N.G1 : r;
    KICK(b, 0, kg); KICK(b, 2, kg * 0.84);
    if (lvl >= 2) KICK(b, 3.5, kg * 0.6);
    if (lvl >= 3) KICK(b, 1, 0.55);                      // four-on-floor lift
    if (lvl >= 1) { SNR(b, 1, 0.6); SNR(b, 3, 0.66); }
    // EXCITING-PART SNARE — at the top energy, drive ghost snares + a building
    // 16th roll out of every other bar so the climaxes really push.
    if (lvl >= 3) {
      SNR(b, 2.5, 0.22); SNR(b, 3.5, 0.30);              // ghost notes between the backbeats
      if (i % 2 === 1) for (const s of [0, 0.25, 0.5, 0.75]) SNR(b, 3 + s, 0.26 + s * 0.95);   // snare roll → next bar
    }
    if (ride) rideHats(b, lvl >= 2 ? 0.2 : 0.15); else hats8(b, lvl >= 1 ? 0.26 : 0.16, lvl >= 2);
    if (lvl >= 1) for (let e = 0; e < 4; e++) SHK(b, e + 0.5, 0.14);
    // FAT bass-perc bounce + butter sine sub on the downbeat
    BP(b, 0, r, 1.4, 0.84); BP(b, 1.5, r, 0.5, 0.62); BP(b, 2.5, r5, 1.1, 0.74);
    SUBSINE(b, 0, r, 2.2, 0.5); SUBSINE(b, 2.5, r5, 1.2, 0.4);
    // THICKER DEEP BASS at the heavy parts — a sub-octave sine under the root
    // for chest-weight (the "bring in a thicker deep bass at times").
    if (lvl >= 3) { SUBSINE(b, 0, r - 12, 2.6, 0.42); SUBSINE(b, 2.5, r5 - 12, 1.4, 0.32); }
  }
}
function bassBounce(ab, bars, roots) {
  for (let i = 0; i < bars; i++) { const r = roots[i % roots.length]; bass(t(ab + i, 0), r, 1.6); bass(t(ab + i, 2), r === N.C3 ? N.G3 : r, 1.4); }
}
function vibTriad(ab, beat, midis, beats, g = 0.32) {
  for (const m of midis) vib(t(ab, beat), m, beats, g);
  vib(t(ab, beat), midis[0] - 12, beats, g * 0.72);       // octave-down warmth
}
const TRIAD = { C: [N.C4, N.E4, N.G4], F: [N.F3, N.A3, N.C4], G: [N.G3, N.B3, N.D4], Am: [N.A3, N.C4, N.E4] };

// ══════════════════════════════════════════════════════════════════════
//  LEAD MELODY DATA (per block; [barInBlock, beat, note, durBeats, gain?])
// ══════════════════════════════════════════════════════════════════════
const LEAD = {
  butterfly: [
    [0, 0, "E5", 1], [0, 1, "G5", 1], [0, 2, "C6", 2],
    [1, 0, "C6", 1], [1, 1, "E6", 1], [1, 2, "C6", 2],
    [2, 0, "D5", 1], [2, 1, "G5", 1], [2, 2, "B5", 2],
    [3, 0, "C6", 2], [3, 2, "G5", 2],
  ],
  palofmine: [
    [0, 0, "E5", 1], [0, 1, "G5", 1], [0, 2, "C6", 2],
    [1, 0, "A5", 1], [1, 1, "G5", 1], [1, 2, "F5", 2],
    [2, 0, "E5", 1], [2, 1, "F5", 1], [2, 2, "G5", 1], [2, 3, "A5", 1],
    [3, 0, "C6", 4],
  ],
  mommywow: [
    [0, 0, "G5", 4, 0.9],
    [1, 0, "C6", 1], [1, 1, "E6", 1], [1, 2, "G6", 2],
    [2, 0, "G5", 4, 0.9],
    [3, 0, "G6", 1], [3, 1, "E6", 1], [3, 2, "C6", 1], [3, 3, "G6", 1],
  ],
  slinky: [
    [0, 0, "C5", 1], [0, 1, "E5", 1], [0, 2, "G5", 2],
    [1, 0, "G5", 1], [1, 1, "F5", 1], [1, 2, "E5", 1], [1, 3, "D5", 1],
    [2, 0, "C5", 1], [2, 1, "E5", 1], [2, 2, "G5", 2],
    [3, 0, "C5", 0.5], [3, 0.5, "D5", 0.5], [3, 1, "E5", 0.5], [3, 1.5, "F5", 0.5], [3, 2, "G5", 0.5], [3, 2.5, "A5", 0.5], [3, 3, "B5", 0.5], [3, 3.5, "C6", 0.5],
    [4, 0, "C6", 0.5], [4, 0.5, "B5", 0.5], [4, 1, "A5", 0.5], [4, 1.5, "G5", 0.5], [4, 2, "F5", 0.5], [4, 2.5, "E5", 0.5], [4, 3, "D5", 0.5], [4, 3.5, "C5", 0.5],
    [5, 0, "C5", 1], [5, 1, "E5", 1], [5, 2, "G5", 1], [5, 3, "C6", 1],
    [6, 0, "C6", 1], [6, 1, "A5", 1], [6, 2, "F5", 1], [6, 3, "D5", 1],
    [7, 0, "E5", 1], [7, 1, "G5", 1], [7, 2, "C5", 2],
  ],
  fly: [
    [0, 0, "C5", 1], [0, 1, "D5", 1], [0, 2, "E5", 1], [0, 3, "G5", 1],
    [1, 0, "C6", 4],
    [2, 0, "E5", 1], [2, 1, "F5", 1], [2, 2, "G5", 1], [2, 3, "A5", 1],
    [3, 0, "D6", 4],
    [4, 0, "E6", 1], [4, 1, "D6", 1], [4, 2, "C6", 1], [4, 3, "G5", 1],
    [5, 0, "A5", 1], [5, 1, "C6", 1], [5, 2, "G6", 2],
    [6, 0, "G6", 1], [6, 1, "E6", 1], [6, 2, "C6", 1], [6, 3, "A5", 1],
    [7, 0, "G5", 1], [7, 1, "E5", 1], [7, 2, "C5", 2],
  ],
  land: [
    [0, 0, "C6", 1], [0, 1, "G5", 1], [0, 2, "E5", 1], [0, 3, "C5", 1],
    [1, 0, "D5", 1], [1, 1, "C5", 3],
    [2, 0, "E5", 1], [2, 1, "D5", 1], [2, 2, "C5", 2],
    [3, 0, "C4", 4, 0.9],
  ],
};
function playLead(ab, name, gMul = 1) {
  for (const [bb, beat, note, dur, g] of LEAD[name]) lead(t(ab + bb, beat), N[note], dur, (g ?? 1.0) * gMul);
}
function twinkle(ab, bars) {            // kalimba sparkle on phrase tails
  for (let i = 0; i < bars; i++) { spark(t(ab + i, 0.5), N.E6, 0.5, 0.4); spark(t(ab + i, 2.5), N.C6, 0.5, 0.4); }
}

// ══════════════════════════════════════════════════════════════════════
//  BLOCKS — each schedules everything for its span starting at abs bar ab
// ══════════════════════════════════════════════════════════════════════
const BLOCKS = {
  intro(ab, bars) {
    // SLOW WATER-DROP DAWN → gather into the hook. The first stretch is
    // BEATLESS: a warm held pad, a soft sub drone, and irregular water-drop
    // plinks with long decays — all drenched in echo + reverb (dropBuf is
    // delayed + reverbed at mixdown). A sparse, wandering, non-repeating
    // melody drifts upward over it. Only in the last few bars does the groove
    // creep in and build into the first chorus. Slow to start, lots of space.
    const amb = Math.max(3, bars - 5);               // beatless ambient bars
    PAD(ab, [N.C3, N.G3, N.C4, N.E4], bars + 1, 0.26);   // warm held bed
    SUBSINE(ab, 0, N.C2, amb * 4, 0.18);                 // soft sub drone under the dawn (eased in at mixdown)

    // ── water drops — sparse + IRREGULAR (off the grid, never cyclical) ──
    const HZ = [1760, 2090, 1320, 1900, 1500, 2350, 1150, 1980, 1640, 2210];
    let di = 0;
    for (let i = 0; i < bars; i++) {
      const b = ab + i;
      const nd = i < amb ? (i % 2 ? 3 : 2) : 1;          // a few per bar early, thinning as the groove enters
      for (let k = 0; k < nd; k++) {
        const beat = (k * 1.45 + (i % 3) * 0.7 + hrand() * 0.6) % 4;   // irregular placement
        drop(t(b, beat), HZ[di++ % HZ.length] * (1 + hr2() * 0.05), Math.max(0.18, 0.46 - i * 0.02));
      }
    }
    // ── a slow, sparse, NON-repeating melody (long decays) that gathers ──
    const MEL = [[0, 1, "G4", 3], [1, 3, "C5", 4], [3, 0.5, "E5", 4], [4, 2, "D5", 3],
                 [amb - 1, 1, "G5", 3], [amb, 0, "C5", 3], [amb + 1, 2, "E5", 3],
                 [amb + 2, 0, "G5", 2], [amb + 2, 2.5, "A5", 2]];
    for (const [bb, beat, note, dur] of MEL) if (bb < bars) plink(t(ab + bb, beat), N[note], dur, Math.max(0.34, 0.5 - bb * 0.012));
    // exposed bendy bass bells in the dawn — a clean look at the dynabell
    // (round sine here; it scoops up from a fifth below and rings long)
    bassBell(ab + 1, 2, N.C4, 5, 0.42); bassBell(ab + 4, 0, N.G3, 5, 0.36);
    // AMBIENT FLUTE — slow breathy pan-flute long-tones drift through the dawn
    flute(t(ab + 1, 0), N.C5, 6, 0.34, { preset: "panflute", attack: 0.7, release: 1.1 });
    flute(t(ab + 4, 2), N.G5, 5, 0.30, { preset: "panflute", attack: 0.8, release: 1.0 });

    // ── groove creeps in for the last (bars-amb) bars and builds ──
    for (let i = amb; i < bars; i++) {
      const b = ab + i, j = i - amb;
      KICK(b, 0, 0.55 + j * 0.07); if (j >= 1) KICK(b, 2, 0.5 + j * 0.06);
      BP(b, 0, N.C2, 1.6, 0.55 + j * 0.06); BP(b, 2.5, N.G1, 1.0, 0.5 + j * 0.05);
      SUBSINE(b, 0, N.C2, 2.4, 0.42);
      for (let e = 0; e < 4; e++) SHK(b, e + 0.5, 0.10 + j * 0.02);
      if (j >= 1) hats8(b, 0.14 + j * 0.025);
      if (j >= 2) { SNR(b, 1, 0.42); SNR(b, 3, 0.48); }
      if (j >= 3) rideHats(b, 0.16);
    }
    // ── foreshadow the butterfly hook on the (now softer) lead, late ──
    const TEASE = [[bars - 3, 0, "E5", 1], [bars - 3, 1, "G5", 1], [bars - 3, 2, "C6", 2],
                   [bars - 2, 0, "G5", 1], [bars - 2, 2, "E5", 2]];
    for (const [bb, beat, note, dur] of TEASE) lead(t(ab + bb, beat), N[note], dur, 0.5);
    for (const fb of [0, 0.25, 0.5, 0.75]) SNR(ab + bars - 1, 3 + fb, 0.4 + fb * 0.8);   // fill into the hook
    spark(t(ab + bars - 1, 3.5), N.C6, 0.5, 0.5);
    rise(ab + bars, 1.6 * BAR, [N.G5, N.C6, N.E6, N.G6], 0.46);   // → THE START
    riseKick(ab + bars, 1.1 * BAR, 0.55);
  },
  butterfly(ab, bars, o = {}) {
    playLead(ab, "butterfly");
    bassBounce(ab, bars, [N.C3]);
    vibTriad(ab, 0, TRIAD.C, 4 * bars / 4, 0.3);
    twinkle(ab, bars);
    layFatGroove(ab, bars, { lvl: o.lvl ?? 0, subRoots: [N.C2], ride: o.ride });
    // a bright bell chime on the downbeat — the hook clearly lands, top bell
    // rings LONG so the hook sustains over the bar.
    dynabell(t(ab, 0), N.C6, 3, 0.5, { tail: 2.2 }); bell(t(ab, 0.05), N.E6, 3, 0.38); bell(t(ab, 0.1), N.G6, 3, 0.3);
  },
  palofmine(ab, bars, o = {}) {
    playLead(ab, "palofmine");
    bassBounce(ab, bars, [N.C3, N.F3, N.G3, N.C3]);
    const ch = [TRIAD.C, TRIAD.F, TRIAD.G, TRIAD.C];
    for (let i = 0; i < bars; i++) vibTriad(ab + i, 0, ch[i % 4], 4, 0.3);
    twinkle(ab, bars);
    layFatGroove(ab, bars, { lvl: o.lvl ?? 1, subRoots: [N.C2, N.F1, N.G1, N.C2], ride: o.ride });
  },
  mommywow(ab, bars) {
    // HUSH — pull the kit back for the held wonder, keep the butter sub long
    playLead(ab, "mommywow");
    for (let i = 0; i < bars; i++) bass(t(ab + i, 0), N.C3, 4, 0.5);
    vibTriad(ab, 0, TRIAD.C, 4 * bars, 0.28);
    PAD(ab, [N.C4, N.E4, N.G4, N.C5], bars, 0.42);   // a long held chord blooms under the wonder
    for (let i = 0; i < bars; i++) {
      KICK(ab + i, 0, 0.6);
      BP(ab + i, 0, N.C2, 3.6, 0.6, { drive: 1.6, decay: 0.9 });
      SUBSINE(ab + i, 0, N.C2, 3.6, 0.46);
      HAT(ab + i, 2, 0.18, { open: true });
      for (let e = 0; e < 4; e++) SHK(ab + i, e + 0.5, 0.1);
    }
    bell(t(ab, 0), N.E6, 4 * bars, 0.36); bell(t(ab + 1, 2), N.G6, 2, 0.7);
    bell(t(ab + 3, 0), N.G6, 1, 0.7); bell(t(ab + 3, 3), N.C6, 1, 0.6);
    // BASS BELLS — long, low, deeply pitch-bendy scoops blooming under the hush
    bassBell(ab, 0, N.C4, 4 * bars, 0.55); bassBell(ab + 2, 0, N.G3, 6, 0.46);
    // FLUTE — the hush SINGS on the AC physically-modeled flute (breathy + warm)
    const FMEL = [[0, 0, "G5", 2], [0, 2.5, "E5", 1.5], [1, 1, "C6", 3], [2, 0, "A5", 2], [2, 2.5, "G5", 1.5], [3, 0, "E5", 4]];
    for (const [bb, beat, note, beats] of FMEL) flute(t(ab + bb, beat), N[note], beats, 0.50, { preset: "flute", attack: 0.12, release: 0.45 });
    SNR(ab + bars - 1, 3, 0.4); SNR(ab + bars - 1, 3.5, 0.5); SNR(ab + bars - 1, 3.75, 0.6);   // fill out of the hush
  },
  slinky(ab, bars, o = {}) {
    playLead(ab, "slinky");
    const lvl = o.lvl ?? 2;
    // springy, syncopated FAT bass
    for (let i = 0; i < bars; i++) {
      const b = ab + i;
      KICK(b, 0, 0.94); KICK(b, 2, 0.8);
      if (lvl >= 2) KICK(b, 3.5, 0.56);
      SNR(b, 1, 0.6); SNR(b, 3, 0.66);
      if (o.ride) rideHats(b, 0.2); else hats8(b, 0.26, lvl >= 2);
      for (let e = 0; e < 4; e++) SHK(b, e + 0.5, 0.14);
      BP(b, 0, N.C2, 0.9, 0.84); BP(b, 1.5, N.C2, 0.5, 0.62); BP(b, 2, N.G1, 0.9, 0.76); BP(b, 3.5, N.C2, 0.5, 0.6);
      SUBSINE(b, 0, N.C2, 1.6, 0.46);
      bass(t(b, 0), N.C3, 1.6); bass(t(b, 2), N.G3, 1.4);
      SC(b, 3, i % 2 ? "baby" : "scribble", 0.46, 48 + (i % 3) * 4);
    }
    twinkle(ab, bars);
    rise(ab, 1.8 * BAR, [N.E5, N.G5, N.C6, N.E6], 0.5);   // the slinky drop
    riseKick(ab, 1.3 * BAR, 0.66);
    SCREAM(ab, 0, 88, 0.42, { bend: 8, rasp: 0.6, dur: 0.5 });
  },
  fly(ab, bars, o = {}) {
    playLead(ab, "fly");
    const roots3 = [N.C3, N.C3, N.F3, N.F3, N.C3, N.A3, N.G3, N.C3];
    const roots1 = [N.C2, N.C2, N.F1, N.F1, N.C2, N.A1, N.G1, N.C2];
    for (let i = 0; i < bars; i++) bassBounce(ab + i, 1, [roots3[i % 8]]);
    layFatGroove(ab, bars, { lvl: o.lvl ?? 3, subRoots: roots1, ride: o.ride, kg: 0.96 });
    // ELABORATE BELL RUNS — bendy dynabell flurries that grow busier + FASTER
    // as the track climbs (o.lvl). Each note scoops up into pitch; the morphing
    // shape (sine→tri→square) rides along, so late soars buzz where early ones glow.
    const flvl = o.lvl ?? 3;
    bellRun(ab + 1, 0, [[0, N.G5, 1.5], [1.5, N.C6, 2.5]], { gain: 0.52, bend: -2, tail: 2.4 });   // LONG-ringing halo
    if (flvl >= 1)
      bellRun(ab + 3, 0, [[0, N.C6, 0.5], [0.5, N.E6, 0.5], [1, N.G6, 0.5], [1.5, N.C6 + 12, 2]], { gain: 0.48, bend: -2 });
    if (flvl >= 2)
      bellRun(ab + 5, 0, [[0, N.G6, 0.5], [0.5, N.E6, 0.5], [1, N.C6, 0.5], [1.5, N.A5, 0.5], [2, N.G5, 1.5]], { gain: 0.46, bend: -1.5, vib: 0.14, tail: 1.6 });
    if (flvl >= 3) {   // the showpiece — a flowing PENTATONIC cascade that RINGS
      // into itself (tail) and is phrased (mixed lengths) so it sings instead of
      // blipping: arcs up to the top and resolves down onto a held C.
      bellRun(ab + 6, 0, [
        [0, N.G5, 0.5], [0.5, N.C6, 0.25], [0.75, N.E6, 0.25], [1, N.G6, 0.75],
        [1.75, N.E6, 0.25], [2, N.C6, 0.5], [2.5, N.A5, 0.5],
      ], { gain: 0.40, bend: -1.5, vib: 0.14, tail: 1.8 });
      // …then one BIG long-decay bell rings out over the soar
      dynabell(t(ab + 7, 0), N.C6, 1, 0.52, { bend: -2, vib: 0.16, tail: 2.9 });
    }
    // FLUTE soaring over the climb — sustained, breathy, riding the lift
    flute(t(ab + 1, 0), N.G5, 4, 0.34, { preset: "flute", attack: 0.22, release: 0.6 });
    flute(t(ab + 5, 2), N.C6, 3, 0.32, { preset: "flute", attack: 0.18, release: 0.5 });
    twinkle(ab, bars);
    rise(ab, 1.5 * BAR, [N.G5, N.C6, N.E6, N.G6], 0.42);   // lift into the soar
    riseKick(ab, 1.0 * BAR, 0.5);
    SCREAM(ab, 0, 90, 0.42, { bend: 7, rasp: 0.6, dur: 0.5 });
    SCREAM(ab + 3, 3.5, 86, 0.38, { bend: 10, rasp: 0.6, dur: 0.42 });
    if (o.lvl >= 3) for (const fb of [0, 0.25, 0.5, 0.75]) SNR(ab + bars - 1, 3 + fb, 0.35 + fb * 0.7);
  },
  ride(ab, bars, o = {}) {
    // SQUEAK-RIDE — a SHORT, melodic scratch+squeak duet that SWELLS in and
    // out of the fight instead of grinding flat the whole time. The scratch
    // SINGS a little climbing call (a pitched, buzzy "aah"), the squeak
    // ANSWERS it up high; both bloom in the space reverb (scrBuf/screamBuf →
    // space send), so the fight sits BACK in the mix and breathes. The fat
    // groove + 16th ride hats hold the floor underneath.
    const lvl = o.lvl ?? 3;
    layFatGroove(ab, bars, { lvl, subRoots: [N.C2, N.C2, N.G1, N.A1], ride: true, kg: 0.96 });
    // a real two-bar phrase: the scratch calls, the squeak answers, on pitches.
    const CALL   = [[0.0, N.C5, "scribble"], [0.75, N.E5, "chirp"], [1.5, N.G5, "scribble"]];
    const ANSWER = [[2.0, N.G5], [2.75, N.C6], [3.5, N.E6]];           // squeak tops the call
    for (let i = 0; i < bars; i++) {
      // a hump envelope across the span — quiet at the edges, loud in the
      // middle — so the duet DROPS IN, peaks, and DROPS OUT of the groove.
      const swell = Math.sin(((i + 0.5) / bars) * Math.PI);            // 0 → 1 → 0
      const g = 0.18 + 0.40 * swell;
      const motif = (i % 2 ? 2 : 0) + (i === bars - 1 ? 4 : 0);        // a touch of internal motion
      for (const [beat, midi, kind] of CALL)
        SC(ab + i, beat, kind, g, midi + motif);
      for (const [beat, midi] of ANSWER)
        SCREAM(ab + i, beat, midi + motif, g * 0.85, { bend: 4, rasp: 0.32, dur: 0.30 });
      spark(t(ab + i, 3.75), N.E6, 0.5, 0.36 * swell);
    }
    // VORTEX — one big performed warble per fight, landing at the swell peak:
    // the player rides ON TOP, sucks the track into a vortex and lets it ripple
    // back, wub-wubbing in the reverb with the squeaks (the "1:20" gesture).
    const peakBar = Math.floor((bars - 1) / 2);
    vortex(t(ab + peakBar, 0.5), N.C4, 3.5 * BEAT, { gain: 0.46 + 0.06 * lvl, depth: 2.8, wubDiv: 2 });
    // a second, shorter shoop on the off-beat so the fight has two warps
    vortex(t(ab + peakBar, 3.0), N.G3, 1.5 * BEAT, { gain: 0.38 + 0.05 * lvl, depth: 2.4, wubDiv: 4 });
    rise(ab, 1.2 * BAR, [N.C6, N.E6, N.G6], 0.4);
  },
  cave(ab, bars) {
    // fat dub breakdown — sparse + drenched (routed into caveBuf → reverb).
    const caveVib = (tt, m, beats, g) => { const ev = { startSec: tt, midi: TR(m), durSec: beats * BEAT, gain: g, preset: "vibraphone_off", decayMul: 1.5 }; recMarimbaBus("cavem", ev); mixEventMarimba(ev, caveBuf, { sampleRate: SR }); };
    const caveLead = (tt, m, beats, g) => { const ev = { startSec: tt, midi: TR(m), durSec: beats * BEAT, gain: g, preset: "xylophone", decayMul: 1.4 }; recMarimbaBus("cavem", ev); mixEventMarimba(ev, caveBuf, { sampleRate: SR }); };
    const caveKick = (ev) => {
      // bake: cave kick → caveBuf. resolve renderKick defaults (fStart 150,
      // pitchDecay 0.055, click 0.55, drive 1.6).
      rec(`cavekick ${ev.startSec.toFixed(8)} ${(ev.gain ?? 1).toFixed(6)} ${(ev.fStart ?? 150).toFixed(4)} ${(ev.fEnd ?? 48).toFixed(4)} ${(ev.pitchDecay ?? 0.055).toFixed(6)} ${(ev.ampDecay ?? 0.34).toFixed(6)} ${(ev.click ?? 0.55).toFixed(4)} ${(ev.drive ?? 1.6).toFixed(6)}`);
      mixKick(ev, caveBuf, { sampleRate: SR });
    };
    for (let half = 0; half < bars; half += 4) {
      for (const m of TRIAD.Am) caveVib(t(ab + half, 0), m, 7, 0.30);
      for (const m of TRIAD.F) caveVib(t(ab + half + 2, 0), m, 7, 0.28);
      caveLead(t(ab + half, 1), N.E5, 1, 0.48); caveLead(t(ab + half, 2.5), N.A5, 1.5, 0.44);
      caveLead(t(ab + half + 2, 1), N.C6, 1, 0.44); caveLead(t(ab + half + 2, 2.5), N.G5, 1.5, 0.4);
      {
        const ev = { landSec: t(ab + half, 0), dur: 1.4 * BAR, midis: [N.A5, N.C6, N.E6].map(TR), gain: 0.38 };
        rec(`caverise ${ev.landSec.toFixed(8)} ${ev.dur.toFixed(8)} ${ev.gain.toFixed(6)} ${(2.6).toFixed(6)} ${(3.5).toFixed(6)} ${ev.midis.length} ${ev.midis.join(" ")}`);
        mixReverseBell(ev, caveBuf, { sampleRate: SR });
      }
      caveKick({ startSec: t(ab + half, 0), gain: 0.45, fEnd: 38, ampDecay: 0.6 });
      caveKick({ startSec: t(ab + half + 1, 2), gain: 0.45, fEnd: 38, ampDecay: 0.6 });
      rhat(caveBuf, ab + half + 1, 0, 0.32);   // reverse-hat → caveBuf (baked inside rhat)
      // keep a soft butter sub pulsing so the dub still grooves
      SUBSINE(ab + half, 0, N.A1, 4, 0.4); SUBSINE(ab + half + 2, 0, N.F1, 4, 0.4);
    }
    SCREAM(ab + bars - 1, 0, 88, 0.36, { bend: 8, rasp: 0.55, dur: 0.5 });   // soft cry out of the cave
  },
  progression(ab, bars) {
    // C → G → Am → F (I–V–vi–IV) walking it home.
    const PROG = [
      { root1: N.C2, root3: N.C3, triad: TRIAD.C, top: N.C6 },
      { root1: N.G1, root3: N.G3, triad: TRIAD.G, top: N.B5 },
      { root1: N.A1, root3: N.A3, triad: TRIAD.Am, top: N.A5 },
      { root1: N.F1, root3: N.F3, triad: TRIAD.F, top: N.A5 },
    ];
    for (let i = 0; i < bars; i++) {
      const b = ab + i, ch = PROG[i % 4];
      KICK(b, 0, 0.9); KICK(b, 2, 0.78); SNR(b, 1, 0.6); SNR(b, 3, 0.66);
      hats8(b, 0.24); for (let e = 0; e < 4; e++) SHK(b, e + 0.5, 0.14);
      BP(b, 0, ch.root1, 1.5, 0.78); BP(b, 2, ch.root1, 1.0, 0.7); BP(b, 3, ch.root1, 0.8, 0.6);
      SUBSINE(b, 0, ch.root1, 2.2, 0.48);
      vibTriad(b, 0, ch.triad, 4, 0.34);
      lead(t(b, 0), ch.top, 1, 0.9); lead(t(b, 1), ch.triad[2], 1, 0.8); lead(t(b, 2), ch.triad[1] + 12, 2, 0.82);
      spark(t(b, 3.5), N.E6, 0.5, 0.4);
      bell(t(b, 0), ch.triad[2] + 12, 2, 0.4);
    }
  },
  land(ab, bars) {
    playLead(ab, "land");
    for (let i = 0; i < bars; i++) {
      const b = ab + i;
      KICK(b, 0, 0.85 - i * 0.12);
      if (i < 2) { SNR(b, 1, 0.5); SNR(b, 3, 0.5); hats8(b, 0.2); }
      BP(b, 0, N.C2, 2.2 - i * 0.4, 0.74 - i * 0.1);
      SUBSINE(b, 0, N.C2, 2.4 - i * 0.4, 0.46 - i * 0.06);
      bass(t(b, 0), N.C3, 1.6); bass(t(b, 2), N.G3, 1.4);
    }
    vibTriad(ab, 0, TRIAD.C, 4, 0.3);
  },
  button(ab) {
    // final resolved C hit + ring
    KICK(ab, 0, 1.0); BP(ab, 0, N.C2, 3.4, 0.88, { decay: 1.4 }); SUBSINE(ab, 0, N.C2, 3.6, 0.55);
    SNR(ab, 0, 0.5);
    bell(t(ab, 0), N.C6, 4, 0.5); bell(t(ab, 0.06), N.E6, 4, 0.4); bell(t(ab, 0.12), N.G6, 4, 0.35);
    vibTriad(ab, 0, TRIAD.C, 4, 0.4);
    lead(t(ab, 0), N.C6, 4, 0.85);
    rhat(hatBuf, ab, 0, 0.42);
  },
};

// ── DISPATCH — lay every block at its cumulative bar offset ─────────────
let cursor = 0;
const placed = [];
let firstKeySet = false;
for (const [name, bars, opts] of ARRANGEMENT) {
  const o = opts ?? {};
  if (o.key !== undefined) {
    const changed = o.key !== gKey;
    gKey = o.key;
    // on a real modulation (not the opening key set), open the new key with a
    // held tonic-triad swell so the change lands as a bloom of held notes.
    if (changed && firstKeySet) PAD(cursor, [N.C4, N.E4, N.G4], Math.min(bars, 4), 0.5);
    firstKeySet = true;
  }
  BLOCKS[name](cursor, bars, o);
  placed.push({ name, bar0: cursor, bars, key: gKey });
  cursor += bars;
}

// ── BOOWOOP FLOURISHES — reverse-bell "boo" sucking up into a bendy dynabell
// "woop". Sprinkled in exposed intro/dub spots and every few bars through the
// LAST MINUTE (the final F chorus). gKey is restored per-flourish from the
// placed section map so each one transposes to its section's key. ──
function keyAtBar(bar) { for (const s of placed) if (bar >= s.bar0 && bar < s.bar0 + s.bars) return s.key; return 0; }
const BOOWOOPS = [
  // intro dawn + exposed dub breakdowns — cool reverse sucks in the quiet
  [3, 2.0, "C5", 2.5, 0.40], [7, 1.0, "G5", 2.5, 0.40],
  [78, 0.0, "A5", 2, 0.42], [146, 2.0, "E5", 2, 0.42],
  // LAST MINUTE — a boowoop every few bars through the euphoric F chorus
  [156, 0.0, "C6", 2, 0.46], [160, 2.0, "G5", 2, 0.44],
  [164, 0.0, "E6", 2, 0.46], [167, 2.0, "C6", 2, 0.44],
  [171, 0.0, "G6", 2, 0.48], [175, 2.0, "E6", 2, 0.46],
  [179, 0.0, "C6", 2, 0.46], [182, 0.0, "G5", 3, 0.48],
];
for (const [bar, beat, note, beats, g] of BOOWOOPS) { gKey = keyAtBar(bar); boowoop(bar, beat, N[note], beats, g); }

// ── FEMBELL ACCENTS — a few physically-modeled bells (pop/bell) placed where
// they shine: a long pure singing-bowl in the dawn, a big church toll on the
// F-chorus lift + the final button, a bright handbell opening the final chorus.
// [bar, beat, note, gain, preset, tauScale] — key restored per accent.
const FEMBELLS = [
  [4,   0,   "C5", 0.40, "bowl",     0.55],  // intro dawn — pure shimmer (~12 s ring) under the drops
  [54,  0,   "G5", 0.36, "bowl",     0.50],  // a hush bowl (end of pass-A mommywow)
  [150, 0,   "C5", 0.52, "church",   1.00],  // home → F lift: a big church bell tolls the change (~3 s)
  [154, 0,   "C6", 0.46, "handbell", 1.00],  // final chorus — bright handbell accent
  [186, 0,   "C4", 0.58, "church",   1.20],  // the BUTTON — a deep church bell tolls into the fade
];
for (const [bar, beat, note, g, preset, ts] of FEMBELLS) { gKey = keyAtBar(bar); fembell(t(bar, beat), N[note], g, preset, ts); }

// ── GONGS — a few gnarly, long-blooming gongs at the big structural turns, low
// and drenched so the wash is truly felt. [bar, beat, note, gain, tauScale].
const GONGS = [
  [142, 0, "C2", 0.78, 1.15],   // home-cave breakdown opens — a deep gong swells through it
  [150, 0, "G1", 0.90, 1.25],   // the F-CHORUS LIFT — the biggest gong tolls the key change
  [186, 0, "C2", 0.85, 1.30],   // the BUTTON — a final gong wash rings the track out
];
for (const [bar, beat, note, g, ts] of GONGS) { gKey = keyAtBar(bar); gong(t(bar, beat), N[note], g, ts); }
gKey = 0;   // restore (defensive — nothing pitched runs after the flourishes)

// ── WATER-STREAM bed — a continuous natural brook woven UNDER the music. It
// is the bedroom-pop "padding": gate it UP in the open beats (the dawn, the
// held mommy-wow hushes, the cave breakdowns) and DOWN under the busy passes,
// so the room never feels vacuous but the stream never crowds the groove.
// Built at 20 Hz from the section map, one-pole smoothed so section steps
// glide instead of clicking. ──────────────────────────────────────────────
{
  const GATE_FOR = {
    intro: 1.0, mommywow: 0.78, cave: 1.0, land: 0.5, button: 0.4,
    slinky: 0.34, butterfly: 0.32, palofmine: 0.32, progression: 0.28,
    fly: 0.22, ride: 0.20,
  };
  const HZ = 20, span = Math.ceil(totalSec * HZ), env = new Float32Array(span);
  for (let k = 0; k < span; k++) {
    const tt = k / HZ; let lvl = 0.3;
    for (const s of placed) {
      if (tt >= t(s.bar0, 0) && tt < t(s.bar0 + s.bars, 0)) { lvl = GATE_FOR[s.name] ?? 0.3; break; }
    }
    env[k] = lvl;
  }
  const a = 0.05;   // one-pole both directions = smooth glides between sections
  for (let k = 1; k < span; k++) env[k] = env[k] * a + env[k - 1] * (1 - a);
  for (let k = span - 2; k >= 0; k--) env[k] = env[k] * a + env[k + 1] * (1 - a);
  waterStream((tt) => env[Math.max(0, Math.min(span - 1, Math.floor(tt * HZ)))]);
}

// ── C-ENGINE SCORE — emit the baked voice events + exit before mixdown.
// Header carries sr + ns so the C engine allocates an identical buffer. ──
if (BAKING) {
  // the water-stream bed is a per-sample, 3-layer noise process keyed off the
  // section gate map — impractical to bake as discrete events. Instead bake the
  // finished mono streamBuf as a raw side-file the C engine loads verbatim, so
  // the full mix matches without re-deriving the stochastic bed in C. (When the
  // stream isn't in the solo set, skip the side-file entirely.)
  if (soloed("stream")) {
    const streamRaw = BAKE_PATH + ".stream.f32";
    const sb = Buffer.alloc(ns * 4);
    for (let i = 0; i < ns; i++) sb.writeFloatLE(streamBuf[i], i * 4);
    writeFileSync(streamRaw, sb);
    rec(`streamraw ${streamRaw}`);
  }
  // header: the C mixdown needs BEAT/BAR (drop echo), the form clamp + bar
  // count, and the final-chorus-lift boundary (start of the key==5 section).
  const fF = placed.find((s) => s.key === 5);
  const finalLift = fF ? t(fF.bar0, 0) : -1;
  const head = ["# fluttabap360 baked score — one event per line; C engine replays them",
    `sr ${SR}`, `beat ${BEAT.toFixed(10)}`, `bar ${BAR.toFixed(10)}`,
    `totalbars ${TOTAL_BARS}`, `targetsec ${TARGET_SEC}`, `finallift ${finalLift.toFixed(8)}`,
    // SUBSTRATE — the print medium: name (→ run-c master) + per-sample sat params
    `substrate ${SUBSTRATE}`, `sat ${SAT.drive} ${SAT.bias} ${SAT.hiss}`,
    `ns ${ns}`];
  // section map → the C engine rebuilds the reverb-scene send automation from it
  const sectionLines = placed.map((s) => `section ${s.name} ${s.bar0} ${s.bars}`);
  writeFileSync(BAKE_PATH, head.concat(sectionLines, SCORE).join("\n") + "\n");
  console.log(`✓ baked ${SCORE.length} events → ${BAKE_PATH} (sr=${SR} ns=${ns})`);
  process.exit(0);
}

// ══════════════════════════════════════════════════════════════════════
//  STEREO MIXDOWN
// ══════════════════════════════════════════════════════════════════════
const outL = new Float32Array(ns);
const outR = new Float32Array(ns);
function pan(p) { const a = (p + 1) * Math.PI / 4; return [Math.cos(a), Math.sin(a)]; }
function place(buf, p, gain) {
  const [lg, rg] = pan(p);
  for (let i = 0; i < ns; i++) { const s = buf[i] * gain; outL[i] += s * lg; outR[i] += s * rg; }
}
if (SOLO) {
  // ── PARITY MODE — only the soloed (already-ported) voices, DRY, at their
  // canonical pan/gain. Lets the C engine be A/B'd voice-by-voice as the port
  // grows. Names map to the buffer + place args used in the full mix below. ──
  const VOICE = {
    sub:  [deepBuf, 0.00, 0.92],
    kick: [kickBuf, 0.00, 1.02],
    lead: [leadBuf, 0.00, 1.00], bass: [bassBuf, 0.00, 0.66],
    spark:[sparkBuf, 0.44, 0.86], bell: [bellBuf, -0.42, 0.84], vib: [vibBuf, -0.24, 0.88],
    // ── milestone 4 — perc + scratch + scream ──
    snare:[snareBuf, 0.04, 0.80], hat: [hatBuf, 0.34, 0.56], shaker: [shakBuf, -0.34, 0.48],
    subperc:[subBuf, 0.00, 1.00], scratch:[scrBuf, 0.34, 0.60], scream:[screamBuf, -0.48, 0.48],
    rise: [riseBuf, 0.00, 0.85], revkick:[kickBuf, 0.00, 1.02],
    // ── milestone 5 — ambience (placed dry in solo; mix-bus DSP at full mix) ──
    pad:  [padBuf, 0.00, 0.60], drop: [dropBuf, 0.00, 0.84], stream: [streamBuf, -0.30, 0.30],
    cave: [caveBuf, 0.00, 0.92],
    // (more added here as voices are ported: perc, ambience…)
  };
  for (const name of SOLO) {
    const v = VOICE[name];
    if (v) place(v[0], v[1], v[2]);
    else console.warn(`  ! solo: voice '${name}' not yet ported/mapped — skipped`);
  }
} else {
place(leadBuf,   0.00, 1.00);
place(bassBuf,   0.00, 0.66);
place(vibBuf,   -0.24, 0.88);
place(deepBuf,   0.00, 0.92);   // butter sub, dead centre
place(riseBuf,   0.00, 0.85);
place(scrBuf,    0.34, 0.60);   // a touch drier here — the space send does the rest
place(screamBuf,-0.48, 0.48);   // the squeaks, wide-left, proud-but-musical, sitting back
place(sparkBuf,  0.44, 0.86);
place(bellBuf,  -0.42, 0.84);
place(kickBuf,   0.00, 1.02);
place(subBuf,    0.00, 1.00);   // bass-perc — the fat backbone
place(snareBuf,  0.04, 0.80);
place(hatBuf,    0.34, 0.56);
place(shakBuf,  -0.34, 0.48);
reverbInPlace(caveBuf, { wet: 0.50, decay: 0.74 });   // tighter — intimate, not cavernous
place(caveBuf,   0.00, 0.92);

// ── WATER STREAM — wide + intimate (only a touch of room) under everything ──
reverbInPlace(streamBuf, { wet: 0.28, decay: 0.55 });
place(streamBuf, -0.30, 0.30);
place(streamBuf,  0.30, 0.30);   // doubled L/R for a wide brook bed

// ── HELD PAD — place the sustained swells, centred and warm ──
place(padBuf,    0.00, 0.60);

// ── WATER-DROP INTRO — tempo-synced (dotted-8th) echoes + a long reverb tail,
// so the drops + sparse plinks ring out and overlap like rain on a pond. ──
delayInPlace(dropBuf, { time: BEAT * 0.75, fb: 0.42, wet: 0.48 });
reverbInPlace(dropBuf, { wet: 0.52, decay: 0.74 });   // dawn still rings, but closer + less washy
place(dropBuf,   0.00, 0.84);

// ── SPACE — the room is now a COMPOSITIONAL ELEMENT. Each section picks a
// "reverb scene": which voices bloom into the shared room and how much. The
// dawn opens wide; the squeak-FIGHTS throw the scratches + screams way back
// (the squeaks live in the reverb); the HUSH floats the bells; and the driving
// slinky/fly passes go BONE-DRY so the music snaps right up to your face —
// nothing reverbed, all very close. Smoothed across section boundaries so the
// room breathes open and shut instead of switching. (REVERB_SCENES is the score.)
const REVERB_SCENES = {
  intro:      { bell: 0.34, spark: 0.20, vib: 0.20, pad: 0.34, lead: 0.12, rise: 0.18, scream: 0.18 },
  butterfly:  { bell: 0.18, spark: 0.13, lead: 0.07, scream: 0.16 },
  palofmine:  { bell: 0.16, spark: 0.11, lead: 0.07 },
  mommywow:   { bell: 0.38, pad: 0.34, vib: 0.18, lead: 0.12, spark: 0.16 },   // the hush — bells FLOAT
  slinky:     { scream: 0.14 },                                                // nearly DRY — close + punchy
  fly:        { bell: 0.08 },                                                  // DRY — the soar snaps up front
  ride:       { scream: 0.38, scr: 0.34, bell: 0.10, rise: 0.14 },             // the FIGHT blooms (squeaks live here)
  cave:       { scream: 0.28, vib: 0.20, pad: 0.22, lead: 0.10 },              // dub bloom
  progression:{ bell: 0.15, spark: 0.11, lead: 0.07 },
  land:       { bell: 0.24, pad: 0.20, lead: 0.10 },                           // closing ring opens back up
  button:     { bell: 0.30, pad: 0.24 },
};
const SEND_VOICES = { lead: leadBuf, bell: bellBuf, spark: sparkBuf, vib: vibBuf, pad: padBuf, scr: scrBuf, scream: screamBuf, rise: riseBuf };
const spaceSend = new Float32Array(ns);
const RVZ = 100;                                     // automation grid (Hz)
const rvSpan = Math.ceil((ns / SR) * RVZ) + 1;
const sceneAt = (name, voice) => (REVERB_SCENES[name] ?? {})[voice] ?? 0;
for (const [vname, vbuf] of Object.entries(SEND_VOICES)) {
  // a coarse per-voice send envelope across the whole timeline …
  const env = new Float32Array(rvSpan);
  for (let k = 0; k < rvSpan; k++) {
    const tt = k / RVZ;
    let amt = 0;
    for (const s of placed) if (tt >= s.bar0 * BAR && tt < (s.bar0 + s.bars) * BAR) { amt = sceneAt(s.name, vname); break; }
    env[k] = amt;
  }
  // … smoothed both directions so the room glides open/closed (≈0.4 s glide).
  const a2 = 0.04;
  for (let k = 1; k < rvSpan; k++) env[k] = env[k] * a2 + env[k - 1] * (1 - a2);
  for (let k = rvSpan - 2; k >= 0; k--) env[k] = env[k] * a2 + env[k + 1] * (1 - a2);
  for (let i = 0; i < ns; i++) { const g = env[Math.min(rvSpan - 1, Math.floor((i / SR) * RVZ))]; if (g > 0.0004) spaceSend[i] += vbuf[i] * g; }
}
reverbInPlace(spaceSend, { wet: 1.0, decay: 0.70 });   // SHORTER tail — intimate room, not a cavern
place(spaceSend, 0.00, 0.34);
}

// scrub non-finite
let nan = 0;
for (let i = 0; i < ns; i++) {
  if (!Number.isFinite(outL[i])) { outL[i] = 0; nan++; }
  if (!Number.isFinite(outR[i])) { outR[i] = 0; nan++; }
}
if (nan) console.warn(`     ! scrubbed ${nan} non-finite samples`);

// loop fold (exact whole-bar length; tail folded onto the head)
let outN = ns;
if (LOOP) {
  const loopN = Math.round(TOTAL_BARS * BAR * SR);
  for (let i = loopN; i < ns; i++) { const d = i - loopN; if (d < loopN) { outL[d] += outL[i]; outR[d] += outR[i]; } }
  outN = loopN;
}

// SOLO/parity mode skips ALL finalize (normalize, lift, tape-fuzz, fades) so
// the raw is the pure summed voices — directly comparable to the C engine's
// pre-finalize output. The finalize stage is its own (later) port milestone.
let peak = 0;
for (let i = 0; i < outN; i++) { const a = Math.abs(outL[i]); if (a > peak) peak = a; const b = Math.abs(outR[i]); if (b > peak) peak = b; }
if (peak > 0 && !SOLO) { const nrm = 0.84 / peak; for (let i = 0; i < outN; i++) { outL[i] *= nrm; outR[i] *= nrm; } }

// ── FINAL-CHORUS LIFT — the form modulates up to F for the last chorus but
// renders at the same level as the E passes, so the payoff lands soft. Push
// the whole final-F stretch ~+2.4 dB into the master limiter (which catches
// the peaks) so it reads as the loudest, biggest moment. Smooth 2 s ramp-in
// at the cave→F boundary so the lift swells in rather than jumping. ──
if (!LOOP && !SOLO) {
  const fF = placed.find((s) => s.key === 5);
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
  trimN = outN;   // parity: full untrimmed length, no fuzz/hiss/fades (matches the C engine)
} else {
  if (LOOP) {
    // SEAMLESS LOOP — the tail (button ring + reverb past 360 s) was already
    // folded onto the head above, so the length is the exact 187-bar loopN. No
    // trim, no fades: the end flows straight back into the start on the grid.
    trimN = outN;
  } else {
    let lastLoud = outN - 1;
    while (lastLoud > 0 && Math.abs(outL[lastLoud]) < 0.004 && Math.abs(outR[lastLoud]) < 0.004) lastLoud--;
    trimN = Math.min(outN, lastLoud + Math.floor(0.6 * SR));
    // HARD-CLAMP to exactly TARGET_SEC so a player reads "6:00.000", never "6:01"
    // (the final ring just fades out right on the six-minute boundary).
    trimN = Math.min(trimN, Math.round(TARGET_SEC * SR));
  }
  // ── BEDROOM-POP TEXTURE — trade cavernous reverb for INTIMATE grit: a soft
  // tape saturation (gentle tanh fuzz + harmonics = warmth, not a clean hi-fi
  // sheen) and a low, low-passed tape-HISS bed so the room feels PADDED, never
  // vacuous. Applied to BOTH the linear master and the seamless loop. ──
  let _hss = 0x51ed2701 >>> 0;
  const _hr = () => { _hss = (_hss + 0x6d2b79f5) >>> 0; let x = Math.imul(_hss ^ (_hss >>> 15), 1 | _hss); x = (x + Math.imul(x ^ (x >>> 7), 61 | x)) ^ x; return ((x ^ (x >>> 14)) >>> 0) / 4294967296 * 2 - 1; };
  let hlpL = 0, hlpR = 0; const HLP = 0.22, HISS = 0.0055;   // warm (low-passed) hiss, ~−45 dB
  const drive = 1.5, dnorm = Math.tanh(drive);
  for (let i = 0; i < trimN; i++) {
    let l = Math.tanh(outL[i] * drive) / dnorm;   // tape fuzz
    let r = Math.tanh(outR[i] * drive) / dnorm;
    hlpL += (_hr() - hlpL) * HLP; hlpR += (_hr() - hlpR) * HLP;
    outL[i] = l + hlpL * HISS; outR[i] = r + hlpR * HISS;
  }
  // FADES / LOOP BREATH.
  if (!LOOP) {
    // linear master — long eased intro swell from silence + a clean outro fade.
    const fadeIn = Math.floor(6.0 * SR), fadeOut = Math.floor(1.6 * SR);
    for (let i = 0; i < fadeIn && i < trimN; i++) { const g = Math.pow(i / fadeIn, 1.7); outL[i] *= g; outR[i] *= g; }
    for (let i = 0; i < fadeOut && i < trimN; i++) { const idx = trimN - 1 - i, g = i / fadeOut; outL[idx] *= g; outR[idx] *= g; }
  } else {
    // SEAMLESS-BUT-STILL-AN-INTRO loop — a GENTLE breath: the dawn dips to a
    // floor (not silence) and swells back up over ~3.5 s, so each cycle still
    // FEELS like an intro emerging without the groove ever dropping out. A short
    // anti-click micro-fade on both seam edges keeps the wrap glitch-free.
    const FLOOR = 0.45, swell = Math.floor(3.5 * SR);
    for (let i = 0; i < swell && i < trimN; i++) { const g = FLOOR + (1 - FLOOR) * Math.pow(i / swell, 1.4); outL[i] *= g; outR[i] *= g; }
    const edge = Math.floor(0.06 * SR);   // ~60 ms — inaudible, just kills the seam click
    for (let i = 0; i < edge && i < trimN; i++) { const g = i / edge; const j = trimN - 1 - i; outL[j] *= g; outR[j] *= g; }
  }
}

console.log(`→ fluttabap360 · ${TOTAL_BARS} bars · 4/4 @ ${BPM.toFixed(1)} BPM · ${(trimN / SR).toFixed(3)} s · modulates C→D→E→(home)→F · FAT bass-perc + butter sub · ride hats · melodic squeak-rides + space reverb · swing=${SWING.toFixed(2)}${LOOP ? " · LOOP" : ""}`);

// ── write out ────────────────────────────────────────────────────────
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}
const outPath = expandHome(_argi("--out")) || resolve(HERE, "..", "out", LOOP ? "fluttabap360-loop.mp3" : "fluttabap360.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(trimN * 2 * 4);
for (let i = 0; i < trimN; i++) { b.writeFloatLE(outL[i], i * 8); b.writeFloatLE(outR[i], i * 8 + 4); }
writeFileSync(rawPath, b);

// BEDROOM-POP master chain — warm, PADDED low-mids, the hi-fi air rolled OFF
// for a cozy/lofi top, and gentle cushioned glue. Trades the bright open room
// for an intimate, fuzzy, close sound. (Pairs with the JS tape fuzz + hiss +
// the dialed-back reverb sends above.)
const MASTER = [
  "highpass=f=26",
  "equalizer=f=52:t=q:w=0.9:g=2.2",         // sub
  "equalizer=f=100:t=q:w=1.0:g=1.6",        // low body / kick
  "equalizer=f=240:t=q:w=1.0:g=1.7",        // PADDED warmth — fill the low-mids
  "equalizer=f=420:t=q:w=1.3:g=0.9",        // cushion
  "equalizer=f=900:t=q:w=1.2:g=-0.6",       // scoop a touch of boxiness
  "equalizer=f=2600:t=q:w=1.4:g=0.2",       // presence pulled back (still not harsh)
  "equalizer=f=11000:t=q:w=0.7:g=2.6",      // AIR — breathy top-end sheen
  "treble=g=1.6:f=13500",                   // gentle air shelf — open, not lofi-rolled
  "lowpass=f=18000",                        // let the air through
  "vibrato=f=0.6:d=0.45",                   // WOW — slow warped-record sag
  "vibrato=f=5.0:d=0.16",                   // FLUTTER — fast wobble-wobble
  "acompressor=threshold=-18dB:ratio=2.5:attack=18:release=210:makeup=2.4:knee=8",  // gentle padded glue
  "alimiter=limit=0.96:attack=5:release=90",
].join(",");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER, "-c:a", "libmp3lame", "-q:a", "2", outPath], { stdio: "inherit" });
if (ff.status !== 0) { try { unlinkSync(rawPath); } catch {} console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath} (butter-mastered · ${(trimN / SR).toFixed(1)} s)`);

// DistroKid WAV master — -14 LUFS / -1.5 TP, 44.1 kHz / 16-bit.
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
    _comment: "Section map for fluttabap360 (the fat six-minute butterfly hop). 4/4. BPM back-solved + hard-clamped so the form == 360 s exactly. Modulates C→D→E→(home C)→F; keyOffset is semitones above the C home key.",
    meter: 4, bpm: +BPM.toFixed(3), scale: "major", rootMidi: 60,
    totalSec: +structTotalSec.toFixed(6), prerollSec: 0,
    sections: placed.map((s) => ({ name: s.name, keyOffset: s.key, startSec: +t(s.bar0, 0).toFixed(6), endSec: +Math.min(structTotalSec, t(s.bar0 + s.bars, 0)).toFixed(6) })),
    // per-note events for the storyline visualizer (lanes ride the playhead
    // string). Keyed by voice lane; each { t, midi, dur } in seconds.
    events: Object.fromEntries(Object.entries(NOTE_EVENTS).map(
      ([lane, evs]) => [lane, evs.filter((e) => e.t < structTotalSec).sort((a, b) => a.t - b.t)])),
  };
  writeFileSync(resolve(HERE, "..", "out", "fluttabap360.struct.json"), JSON.stringify(struct, null, 2) + "\n");
}
