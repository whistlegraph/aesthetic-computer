// deepbaba.mjs — the marimbaba lullaby sunk into the bass-marimba register,
// then developed as a single AUGMENTATION → FRAGMENTATION arc.
//
// Identity kept: F DORIAN, the bass-marimba lead (warm low "bass" voice),
// the womb ~48 BPM rocking-chair feel, and the marimbaba DNA — the hush
// descent (C5-A4-F4) and the sleep cadence still thread through.
//
// THE DEVELOPMENT: the piece is one long melodic transformation.
//   I.  AUGMENTATION — the hush theme stated VAST and slow: each note a deep
//       low bell held many beats, a grand statement where a heartbeat lives.
//   II. FRAGMENTATION — the long tune is broken into shorter, more active
//       bass cells that converse: the theme's intervals get diminished into
//       quicker runs, sequenced up by step, inverted, tossed call-and-response
//       between a low and a slightly-higher bass voice (left/right), the
//       density accelerating until the cells trill and tumble.
//   III. RE-AUGMENTATION — the fragments slow and re-fuse: the sleep cadence
//       returns, hugely stretched again, putting the tune back to bed.
// A rocking-chair F2->C2 sway breathes under the whole arc.
//
// Run:  node variations/deepbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 48;
const BEAT = 60 / BPM;

// ── mode remap: fold a midi to the nearest degree of F dorian ──
const ROOT = 5; // F
const DORIAN = [0, 2, 3, 5, 7, 9, 10];
function toDorian(midi) {
  const rel = ((midi - ROOT) % 12 + 12) % 12;
  let best = DORIAN[0], bestD = 99;
  for (const d of DORIAN) {
    for (const c of [d, d - 12, d + 12]) {
      const dist = Math.abs(rel - c);
      if (dist < bestD) { bestD = dist; best = c; }
    }
  }
  return midi + (best - rel);
}

// ── motif helpers: a cell is [[noteName, beats], ...] ──────────────────────
const toMidi = (cell) => cell.map(([n, b]) => [m(n), b]);
const transpose = (cell, semis) => cell.map(([mi, b]) => [mi + semis, b]);
const diminish = (cell, f) => cell.map(([mi, b]) => [mi, b * f]);
const retrograde = (cell) => [...cell].reverse();
function invert(cell) {
  // mirror intervals around the first note's pitch.
  const pivot = cell[0][0];
  return cell.map(([mi, b]) => [pivot - (mi - pivot), b]);
}

const events = [];
const F = (cell) => cell.map(([mi, b]) => [toDorian(mi), b]); // snap to mode

// emit a melodic cell of [midi, beats] onto the bass voice, returning end time.
function emitCell(cell, startSec, opts = {}) {
  const { gain = 0.5, ring = 1.25, pan = 0, octave = 0, slur = 1.0, decayMul } = opts;
  let t = startSec;
  for (const [mi, beats] of cell) {
    const dur = beats * BEAT;
    if (mi != null) {
      events.push({
        preset: "bass",
        startSec: t,
        midi: mi + 12 * octave,
        durSec: dur * slur * ring,
        gain,
        decayMul: decayMul ?? 1.45,
        pan,
      });
    }
    t += dur;
  }
  return t;
}

// the marimbaba hush theme, in the low register (the seed sat -12; we go a bit
// lower still so the grand statement sits in the chest).
const HUSH = F(transpose(toMidi(MOTIFS.hush), -24));    // C3-A2-F2-F2(held)
const SLEEP = F(transpose(toMidi(MOTIFS.sleep), -24));  // settle cadence
const TWINKLE = F(transpose(toMidi(MOTIFS.twinkle), -24));
const BABA = F(transpose(toMidi(MOTIFS.baba), -24));

// ════════════════════════════════════════════════════════════════════════
//  MOVEMENT I — AUGMENTATION.  The hush theme stated vast: each note stretched
//  ~5x, deep and slow, a grand low bell. ~0–22s
// ════════════════════════════════════════════════════════════════════════
let t = 0;
const AUG = 5.0; // every hush note swells to five times its length
// add a low fifth shimmer under the very first note to announce the register.
events.push({ preset: "bass", startSec: 0, midi: m("F1"), durSec: 9 * BEAT, gain: 0.42, decayMul: 1.9, pan: 0 });
events.push({ preset: "bass", startSec: 0, midi: m("C2"), durSec: 7 * BEAT, gain: 0.24, decayMul: 1.8, pan: 0.05 });
t = emitCell(diminish(HUSH, AUG), 0, { gain: 0.6, ring: 1.35, pan: -0.04, decayMul: 1.7, slur: 0.95 });
// a vibraphone halo under the augmented statement — Fm9 womb cushion.
for (const [note, g, pan] of [["F3", 0.11, -0.12], ["Ab3", 0.09, 0.1], ["C4", 0.09, -0.07], ["G3", 0.07, 0.13]]) {
  events.push({ preset: "vibraphone", startSec: 0, midi: m(note), durSec: t, gain: g, decayMul: 1.8, pan });
}

// ════════════════════════════════════════════════════════════════════════
//  MOVEMENT II — FRAGMENTATION.  The long tune breaks into shorter, quicker
//  bass cells that converse — diminished, sequenced, inverted, tossed L/R,
//  density accelerating. ~22–110s
// ════════════════════════════════════════════════════════════════════════

// Stage A — the hush, now at "normal" length, but split into a question (first
// two notes) and an answer (last two), tossed between low-left and higher-right
// bass voices. Sequence the pair up by step three times (F dorian climb).
const q = F([toDorian(m("C3")), toDorian(m("A2"))].map((mi) => [mi, 1]));
const a = F([toDorian(m("F2")), toDorian(m("G2"))].map((mi) => [mi, 1]));
let stageT = t + 0.5 * BEAT;
for (let s = 0; s < 4; s++) {
  const up = 2 * s; // step the conversation up the mode
  stageT = emitCell(transpose(q, up), stageT, { gain: 0.5, pan: -0.18, ring: 1.1, octave: 0 });
  stageT += 0.15 * BEAT;
  // the answer comes back a touch brighter and to the right (a 2nd voice).
  stageT = emitCell(transpose(a, up), stageT, { gain: 0.46, pan: 0.2, ring: 1.05, octave: 1 });
  stageT += 0.35 * BEAT;
}

// Stage B — diminish the twinkle climb into a faster run and invert its return:
// the contour rises (fragmented from the tune) then mirrors back down.
const twDim = diminish(TWINKLE, 0.6);
stageT = emitCell(twDim, stageT + 0.3 * BEAT, { gain: 0.5, pan: -0.1, ring: 1.0, octave: 1 });
stageT = emitCell(diminish(invert(TWINKLE), 0.6), stageT + 0.1 * BEAT, { gain: 0.46, pan: 0.16, ring: 0.95, octave: 1 });

// keep the rocking sway alive under stages A–B.
for (let bar = 0; bar < 8; bar++) {
  const b0 = t + bar * 3 * BEAT;
  events.push({ preset: "bass", startSec: b0, midi: m("F2"), durSec: 2.1 * BEAT, gain: 0.34, decayMul: 1.55, pan: -0.05 });
  events.push({ preset: "bass", startSec: b0 + 2 * BEAT, midi: m("C2"), durSec: 1.4 * BEAT, gain: 0.26, decayMul: 1.5, pan: 0.05 });
}

// Stage C — the baba "slinky" cell becomes the engine of the fastest section:
// diminished hard, sequenced down a step each pass, with a call (low-left) and
// an echo a beat later (right, octave up) — a little stretto canon. Density
// climbs as the slur tightens.
const babaDim = diminish(BABA, 0.5);
let fastT = stageT + 0.4 * BEAT;
const swayStart = fastT;
for (let pass = 0; pass < 5; pass++) {
  const down = -1 * pass; // drift the canon downward
  const cell = transpose(babaDim, down);
  const slur = 0.95 - pass * 0.08; // tighten = more active / staccato
  emitCell(cell, fastT, { gain: 0.5, pan: -0.2, ring: 0.95, octave: 0, slur });
  // the canon voice: a beat behind, octave up, to the right — a conversation.
  emitCell(cell, fastT + 0.5 * BEAT, { gain: 0.34, pan: 0.22, ring: 0.85, octave: 1, slur });
  fastT += (3.8 - pass * 0.4) * BEAT; // passes crowd closer: accelerating density
}

// trill flurry at the peak: a fast oscillation on the 5th, the most active point
// before things relax. retrograde of a tiny hush fragment, repeated quick.
const trillCell = [[toDorian(m("C3")), 0.25], [toDorian(m("D3")), 0.25]];
let trillT = fastT - 1.0 * BEAT;
for (let i = 0; i < 10; i++) {
  emitCell(trillCell, trillT, { gain: 0.3 - i * 0.005, pan: i % 2 ? 0.18 : -0.18, ring: 0.7, octave: 1, slur: 0.9 });
  trillT += 0.5 * BEAT;
}

// rocking sway under the fast canon section, but quicker (the chair speeds up).
for (let i = 0; i < 14; i++) {
  const b0 = swayStart + i * 1.5 * BEAT;
  const note = i % 2 ? "C2" : "F2";
  events.push({ preset: "bass", startSec: b0, midi: m(note), durSec: 1.3 * BEAT, gain: 0.3, decayMul: 1.45, pan: i % 2 ? 0.06 : -0.06 });
}

// ════════════════════════════════════════════════════════════════════════
//  MOVEMENT III — RE-AUGMENTATION.  Fragments slow and re-fuse: the sleep
//  cadence returns hugely stretched, putting the tune back to bed.
// ════════════════════════════════════════════════════════════════════════
let coda = trillT + 1.2 * BEAT;
// one last fragmented breath — the hush question, slowing (ritard) into the cadence.
let ritT = coda;
for (let i = 0; i < 3; i++) {
  const stretch = 1.4 + i * 0.6; // each step longer: the chair coming to rest
  ritT = emitCell(diminish(q, stretch), ritT, { gain: 0.42 - i * 0.03, pan: i % 2 ? 0.08 : -0.08, ring: 1.4, octave: 0, decayMul: 1.7 });
  ritT += 0.4 * BEAT;
}

// the sleep cadence, re-augmented (~3.4x) — vast and final, mirroring Movement I.
const RE_AUG = 2.9;
const codaStart = ritT + 0.4 * BEAT;
// deep root pedal under the coda.
events.push({ preset: "bass", startSec: codaStart, midi: m("F1"), durSec: 14 * BEAT, gain: 0.4, decayMul: 1.95, pan: 0 });
events.push({ preset: "bass", startSec: codaStart, midi: m("C2"), durSec: 9 * BEAT, gain: 0.22, decayMul: 1.85, pan: 0.05 });
// vibraphone halo returns, bookending the opening cushion.
for (const [note, g, pan] of [["F3", 0.1, -0.1], ["Ab3", 0.08, 0.12], ["C4", 0.08, -0.06]]) {
  events.push({ preset: "vibraphone", startSec: codaStart, midi: m(note), durSec: 16 * BEAT, gain: g, decayMul: 1.9, pan });
}
const endT = emitCell(diminish(SLEEP, RE_AUG), codaStart, { gain: 0.5, ring: 1.4, pan: -0.03, decayMul: 1.8, slur: 0.98 });

// final sway slowing to a stop on the deepest F.
events.push({ preset: "bass", startSec: endT, midi: m("F1"), durSec: 10 * BEAT, gain: 0.36, decayMul: 2.0, pan: 0 });

const { mp3, durationSec } = renderLullaby(events, {
  name: "deepbaba",
  here: HERE,
  title: "deepbaba (F dorian, bass register)",
  reverb: { wet: 0.3, decay: 0.86, damp: 0.5 },
  fadeIn: 1.5,
  fadeOut: 6.0,
  tailSec: 6.0,
  peak: 0.84,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
