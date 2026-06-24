#!/usr/bin/env node
// render-fluttabap360.mjs — FLUTTABAP360: a fatter, butterier, six-minute
// cousin of flutterbap. Same butterfly-park DNA + synthesized rhythm
// section (../synths/perc.mjs + ../synths/marimba.mjs), but:
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

import { mixEventMarimba } from "../synths/marimba.mjs";
import { mixKick, mixBassPerc, mixSnare, mixHat, mixShaker, mixReverseBell, mixReverseKick, mixScratch, mixScream, renderHat } from "../synths/perc.mjs";
import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;
const _argi = (k) => { const i = process.argv.indexOf(k); return i >= 0 ? process.argv[i + 1] : null; };

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

// ── mallet voice routes ────────────────────────────────────────────────
// MAIN MARIMBA LEAD — softened: a longer, rounder attack, a longer mallet
// contact (low-passes the bright transient), the stick-click noise removed,
// and the glassy upper partials pulled back. Softer touch, less harsh, with
// a touch more ring. (Per-call opts so only the lead is softened.)
function lead(t0, midi, beats, gain = 1.0) {
  EV("rosewood", t0, midi, beats);
  mixEventMarimba(
    { startSec: jit(t0, 13), midi: TR(midi), durSec: beats * BEAT, gain: vel(gain, 0.14), preset: "xylophone", decayMul: 1.3 },
    leadBuf,
    { sampleRate: SR, attack: 0.0032, params: { mallet: 0.0032, noise: 0.0, amps: [1.0, 0.34, 0.13, 0.04], resGain: 0.42 } },
  );
}
function bass(t0, midi, beats, gain = 0.85) {
  EV("bass", t0, midi, beats);
  mixEventMarimba({ startSec: jit(t0, 9), midi: TR(midi), durSec: beats * BEAT, gain: vel(gain, 0.12), preset: "bass", decayMul: 0.55 }, bassBuf, { sampleRate: SR });
}
function spark(t0, midi, beats, gain = 0.55) {
  EV("kalimba", t0, midi, beats);
  mixEventMarimba({ startSec: jit(t0, 14), midi: TR(midi), durSec: beats * BEAT, gain: vel(gain, 0.16), preset: "kalimba", decayMul: 0.95 }, sparkBuf, { sampleRate: SR });
}
function bell(t0, midi, beats, gain = 0.65) {
  EV("bubbles", t0, midi, beats);
  mixEventMarimba({ startSec: jit(t0, 10), midi: TR(midi), durSec: beats * BEAT, gain: vel(gain, 0.12), preset: "glockenspiel", decayMul: 1.35 }, bellBuf, { sampleRate: SR });
}
function vib(t0, midi, beats, gain = 0.4) {
  EV("vibraphone", t0, midi, beats);
  mixEventMarimba({ startSec: jit(t0, 18), midi: TR(midi), durSec: beats * BEAT, gain: vel(gain, 0.10), preset: "vibraphone_off", decayMul: 1.0 }, vibBuf, { sampleRate: SR });
}
// ── BUTTER SUB — a clean enveloped sine an octave anchor under the root.
// This is the "like butter" weight: smooth, round, felt more than heard. ─
function SUBSINE(ab, beat, midi, beats, g = 0.5) {
  const start = jit(t(ab, beat), 4);
  const f = mf(TR(midi)), dur = beats * BEAT;
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
// ── PLINK — a long-decay glassy tone into dropBuf (rides the same echo +
// reverb): the slow intro's sparse melody with the "long decaaaays". ──
function plink(t0, midi, beats, g = 0.5) {
  EV("bubbles", t0, midi, beats);
  mixEventMarimba({ startSec: t0, midi: TR(midi), durSec: beats * BEAT, gain: g, preset: "glockenspiel", decayMul: 2.8 }, dropBuf, { sampleRate: SR });
}

// ── kit routes ──────────────────────────────────────────────────────────
function KICK(ab, beat, g = 0.95, o = {}) {
  mixKick({ startSec: jit(t(ab, beat), 4), gain: vel(g, 0.10), fEnd: 40, ampDecay: 0.48, pitchDecay: 0.064, drive: 1.85, ...o }, kickBuf, { sampleRate: SR });
}
function SNR(ab, beat, g = 0.7, o = {}) {
  mixSnare({ startSec: jit(t(ab, beat), 5), gain: vel(g, 0.14), ...o }, snareBuf, { sampleRate: SR });
}
function HAT(ab, beat, g = 0.28, o = {}) {
  mixHat({ startSec: jit(t(ab, beat), 6), gain: vel(g, 0.24), ...o }, hatBuf, { sampleRate: SR });
}
function SHK(ab, beat, g = 0.2) {
  mixShaker({ startSec: jit(t(ab, beat), 7), gain: vel(g, 0.22) }, shakBuf, { sampleRate: SR });
}
// FAT bass-perc — extra sub reinforcement + grit by default.
function BP(ab, beat, midi, durBeats, g = 0.8, o = {}) {
  mixBassPerc({ startSec: jit(t(ab, beat), 5), midi: TR(midi), durSec: durBeats * BEAT, gain: vel(g, 0.12), sub: 0.58, drive: 2.0, ...o }, subBuf, { sampleRate: SR });
}
function rise(landBar, dur, midis, g) {
  mixReverseBell({ landSec: t(landBar, 0), dur, midis: midis.map(TR), gain: g }, riseBuf, { sampleRate: SR });
}
function riseKick(landBar, dur, g) {
  mixReverseKick({ landSec: t(landBar, 0), dur, gain: g }, kickBuf, { sampleRate: SR });
}
function rhat(targetBuf, landAb, landBeat, g = 0.32) {
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
  mixScratch({ startSec: jit(t(ab, beat), 8), midi: TR(midi), gain: vel(g, 0.15), ...SCR[kind] }, scrBuf, { sampleRate: SR });
}
function SCREAM(ab, beat, midi, g = 0.4, o = {}) {
  mixScream({ startSec: jit(t(ab, beat), 10), midi: TR(midi), gain: vel(g, 0.16), ...o }, screamBuf, { sampleRate: SR });
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
    if (ride) rideHats(b, lvl >= 2 ? 0.2 : 0.15); else hats8(b, lvl >= 1 ? 0.26 : 0.16, lvl >= 2);
    if (lvl >= 1) for (let e = 0; e < 4; e++) SHK(b, e + 0.5, 0.14);
    // FAT bass-perc bounce + butter sine sub on the downbeat
    BP(b, 0, r, 1.4, 0.84); BP(b, 1.5, r, 0.5, 0.62); BP(b, 2.5, r5, 1.1, 0.74);
    SUBSINE(b, 0, r, 2.2, 0.5); SUBSINE(b, 2.5, r5, 1.2, 0.4);
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
    // a bright bell chime on the downbeat — the hook clearly lands
    bell(t(ab, 0), N.C6, 3, 0.5); bell(t(ab, 0.05), N.E6, 3, 0.38); bell(t(ab, 0.1), N.G6, 3, 0.3);
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
    // bell halos on the soar
    bell(t(ab + 1, 0), N.G6, 4, 0.55); bell(t(ab + 3, 0), N.G6, 4, 0.55);
    bell(t(ab + 5, 2), N.G6, 2, 0.5);
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
    rise(ab, 1.2 * BAR, [N.C6, N.E6, N.G6], 0.4);
  },
  cave(ab, bars) {
    // fat dub breakdown — sparse + drenched (routed into caveBuf → reverb).
    const caveVib = (tt, m, beats, g) => mixEventMarimba({ startSec: tt, midi: TR(m), durSec: beats * BEAT, gain: g, preset: "vibraphone_off", decayMul: 1.5 }, caveBuf, { sampleRate: SR });
    const caveLead = (tt, m, beats, g) => mixEventMarimba({ startSec: tt, midi: TR(m), durSec: beats * BEAT, gain: g, preset: "xylophone", decayMul: 1.4 }, caveBuf, { sampleRate: SR });
    for (let half = 0; half < bars; half += 4) {
      for (const m of TRIAD.Am) caveVib(t(ab + half, 0), m, 7, 0.30);
      for (const m of TRIAD.F) caveVib(t(ab + half + 2, 0), m, 7, 0.28);
      caveLead(t(ab + half, 1), N.E5, 1, 0.48); caveLead(t(ab + half, 2.5), N.A5, 1.5, 0.44);
      caveLead(t(ab + half + 2, 1), N.C6, 1, 0.44); caveLead(t(ab + half + 2, 2.5), N.G5, 1.5, 0.4);
      mixReverseBell({ landSec: t(ab + half, 0), dur: 1.4 * BAR, midis: [N.A5, N.C6, N.E6].map(TR), gain: 0.38 }, caveBuf, { sampleRate: SR });
      mixKick({ startSec: t(ab + half, 0), gain: 0.45, fEnd: 38, ampDecay: 0.6 }, caveBuf, { sampleRate: SR });
      mixKick({ startSec: t(ab + half + 1, 2), gain: 0.45, fEnd: 38, ampDecay: 0.6 }, caveBuf, { sampleRate: SR });
      rhat(caveBuf, ab + half + 1, 0, 0.32);
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
reverbInPlace(caveBuf, { wet: 0.62, decay: 0.82 });
place(caveBuf,   0.00, 0.92);

// ── HELD PAD — place the sustained swells, centred and warm ──
place(padBuf,    0.00, 0.60);

// ── WATER-DROP INTRO — tempo-synced (dotted-8th) echoes + a long reverb tail,
// so the drops + sparse plinks ring out and overlap like rain on a pond. ──
delayInPlace(dropBuf, { time: BEAT * 0.75, fb: 0.5, wet: 0.6 });
reverbInPlace(dropBuf, { wet: 0.7, decay: 0.9 });
place(dropBuf,   0.00, 0.92);

// ── SPACE — a shared room-reverb SEND to deepen the whole field. The kit +
// sub stay DRY (fat, punchy, up front); the melodic + squeak voices bloom
// into a big room so the scratches sit BACK and everything gets depth. ──
const spaceSend = new Float32Array(ns);
const addSend = (buf, g) => { for (let i = 0; i < ns; i++) spaceSend[i] += buf[i] * g; };
addSend(leadBuf,   0.16);
addSend(bellBuf,   0.30);
addSend(sparkBuf,  0.26);
addSend(vibBuf,    0.24);
addSend(padBuf,    0.46);
addSend(scrBuf,    0.46);    // scratches bloom in the room
addSend(screamBuf, 0.52);    // squeaks sit back with reverb
addSend(riseBuf,   0.20);
reverbInPlace(spaceSend, { wet: 1.0, decay: 0.88 });   // 100%-wet return
place(spaceSend, 0.00, 0.52);

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

let peak = 0;
for (let i = 0; i < outN; i++) { const a = Math.abs(outL[i]); if (a > peak) peak = a; const b = Math.abs(outR[i]); if (b > peak) peak = b; }
if (peak > 0) { const nrm = 0.84 / peak; for (let i = 0; i < outN; i++) { outL[i] *= nrm; outR[i] *= nrm; } }

let trimN;
if (LOOP) {
  trimN = outN;
} else {
  let lastLoud = outN - 1;
  while (lastLoud > 0 && Math.abs(outL[lastLoud]) < 0.004 && Math.abs(outR[lastLoud]) < 0.004) lastLoud--;
  trimN = Math.min(outN, lastLoud + Math.floor(0.6 * SR));
  // HARD-CLAMP to exactly TARGET_SEC so a player reads "6:00.000", never "6:01"
  // (the final ring just fades out right on the six-minute boundary).
  trimN = Math.min(trimN, Math.round(TARGET_SEC * SR));
  // a long EASED intro swell so the water-drop dawn emerges from silence and
  // BUILDS (instead of opening at full sub-drone level). pow > 1 = slow start.
  const fadeIn = Math.floor(6.0 * SR), fadeOut = Math.floor(1.6 * SR);
  for (let i = 0; i < fadeIn && i < trimN; i++) { const g = Math.pow(i / fadeIn, 1.7); outL[i] *= g; outR[i] *= g; }
  for (let i = 0; i < fadeOut && i < trimN; i++) { const idx = trimN - 1 - i, g = i / fadeOut; outL[idx] *= g; outR[idx] *= g; }
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

// BUTTER master chain — warmer low-mid tilt, softer (less harsh) top, more
// sub weight + glue. Fat and round, still poppin.
const MASTER = [
  "highpass=f=24",
  "equalizer=f=52:t=q:w=0.9:g=2.6",        // deep sub butter
  "equalizer=f=100:t=q:w=1.0:g=1.9",        // fat low body / kick punch
  "equalizer=f=200:t=q:w=1.2:g=1.1",        // warmth
  "equalizer=f=320:t=q:w=1.1:g=-1.0",       // de-mud
  "equalizer=f=2600:t=q:w=1.4:g=0.8",       // mallet/pop presence
  "treble=g=0.8:f=9000",                    // gentle air (butter = not harsh)
  "acompressor=threshold=-17dB:ratio=3.0:attack=12:release=160:makeup=2.6:knee=6",
  "alimiter=limit=0.96:attack=4:release=80",
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
try { unlinkSync(rawPath); } catch {}
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
