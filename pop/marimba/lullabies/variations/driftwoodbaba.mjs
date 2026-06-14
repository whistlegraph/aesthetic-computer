// driftwoodbaba.mjs — a deep, drifting riff on the marimbaba lullaby, carried
// on a slow current.
//
// Direction: F DORIAN (F G Ab Bb C D Eb), bass + gamelan in the LOW register,
// ~46 BPM. Drifting and deep, like driftwood on a slow tide — the tune does
// not repeat, it WANDERS and STRETCHES, sinking ever lower as it goes.
//
// DEVELOPMENT STRATEGY — SLOW GENERATIVE DRIFT + AUGMENTATION:
//  - A single deterministic "drift" walker steps through F-dorian scale degrees
//    with a seeded, weighted bias (gentle downward pull, occasional lift), so
//    the melody wanders a new path every pass yet always sounds like the same
//    hand. The hush sigh seeds the very first steps so the marimbaba contour is
//    legible at the surface before the current takes it.
//  - AUGMENTATION: each successive phrase plays SLOWER and LONGER than the last.
//    A `stretch` factor grows pass over pass (1.0 → ~2.4×), so durations dilate
//    like wood waterlogging — the line literally drifts toward stillness.
//  - DESCENT: the walker's register center sinks pass over pass (octave drops +
//    a downward step bias), so the wood travels deeper into the bass/gamelan
//    register. The final cadence is the marimbaba "sleep" settle, augmented and
//    transposed down two octaves — the driftwood coming to rest on the seabed.
//  - The recognizable thread: opening hush sigh seeds the drift; the slinky
//    "baba" wobble surfaces mid-current as a gamelan eddy; the sleep cadence
//    closes it, all in F (the marimbaba home key, now its dorian sibling).
//
// Run:  node variations/driftwoodbaba.mjs       (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 46;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT; // 3/4, like the marimbaba

// ── F DORIAN scale (root pc = 5). Degrees: F G Ab Bb C D Eb. ────────────────
const ROOT = 5; // F
const DORIAN = [0, 2, 3, 5, 7, 9, 10];
function snap(midi) {
  const pc = ((midi % 12) + 12) % 12;
  const rel = ((pc - ROOT) % 12 + 12) % 12;
  let best = DORIAN[0], bestD = 99;
  for (const s of DORIAN) {
    const d = Math.min(Math.abs(s - rel), 12 - Math.abs(s - rel));
    if (d < bestD) { bestD = d; best = s; }
  }
  const base = midi - rel;
  let cand = base + best;
  if (cand - midi > 6) cand -= 12;
  if (midi - cand > 6) cand += 12;
  return cand;
}

// ── degree-walk in F dorian. A note is an absolute scale "index" where 0 is
//    F2 and each +1 climbs one dorian degree (so the walker thinks in steps,
//    not semitones, and stays in the mode by construction). ──────────────────
const F2 = m("F2");
function degToMidi(deg) {
  const oct = Math.floor(deg / 7);
  const i = ((deg % 7) + 7) % 7;
  return F2 + oct * 12 + DORIAN[i];
}

// deterministic LCG so every drift is repeatable (driftwood follows the tide,
// not chaos).
function lcg(seed) {
  let s = (seed * 2654435761) >>> 0;
  return () => ((s = (s * 1103515245 + 12345) >>> 0) / 4294967296);
}

// ── THE DRIFT WALKER ────────────────────────────────────────────────────────
// drift(opts) returns a cell of [midi, beats] pairs. It steps through dorian
// degrees with a gentle downward bias and a stretching note-length, so the
// melody wanders, lengthens, and sinks. `seed` keys the path; `seedSteps`
// optionally forces the opening intervals (we seed it with the hush sigh).
function drift(out, {
  startBar, startDeg, steps, voice = "gamelan", baseBeats = 1.0, stretch = 1.0,
  downBias = 0.5, gain = 0.42, panBase = 0, decayMul = 1.7, seed = 1,
  seedSteps = null, gapBeats = 0.0,
} = {}) {
  const rnd = lcg(seed);
  let deg = startDeg;
  let beat = 0;
  // step menu (in dorian degrees) and weights, biased downward by downBias.
  const stepMenu = [-3, -2, -1, 0, +1, +2, +3];
  for (let i = 0; i < steps; i++) {
    let step;
    if (seedSteps && i < seedSteps.length) {
      step = seedSteps[i];
    } else {
      // weight: notes near the center, biased down by downBias (0..1).
      const r = rnd();
      // build a biased pick: blend two rolls so middle steps dominate, then
      // shade the result downward.
      const blend = (rnd() + rnd()) / 2; // triangular, centered ~0.5
      let idx = Math.floor(blend * stepMenu.length);
      idx = Math.max(0, Math.min(stepMenu.length - 1, idx));
      step = stepMenu[idx];
      if (r < downBias && step > 0) step = -step; // pull descending
    }
    deg += step;
    // keep the wood in a sane low-mid band: gently reflect off the floor/ceiling
    if (deg > startDeg + 6) deg -= 7;
    if (deg < startDeg - 8) deg += 7;

    const beats = baseBeats * stretch * (0.85 + 0.3 * rnd()); // length jitter
    const midi = degToMidi(deg);
    // pan drifts slowly with the index — the wood sliding across the stereo tide
    const pan = Math.max(-0.5, Math.min(0.5, panBase + Math.sin(i * 0.6) * 0.22));
    out.push({
      preset: voice,
      startSec: startBar * BAR + beat * BEAT,
      midi,
      durSec: beats * BEAT,
      gain,
      decayMul: (DECAY[voice] ?? 1.3) * decayMul,
      pan,
    });
    beat += beats + gapBeats;
  }
  return { endBeat: beat, endDeg: deg }; // for chaining
}

// ── soft, deep bass root under a span (kept clear and low). ──────────────────
function bass(out, startBar, name, lenBars = 2, gain = 0.4) {
  out.push({
    preset: "bass",
    startSec: startBar * BAR,
    midi: snap(m(name)),
    durSec: lenBars * BAR,
    gain,
    decayMul: DECAY.bass * 1.5,
    pan: 0,
  });
}

// ── low gamelan pad/eddy — a sustained dorian dyad/triad breathing under a
//    span, deep in the register so it feels like underwater light. ───────────
const padDeg = (deg, oct) =>
  snap(m(["F", "G", "Ab", "Bb", "C", "D", "Eb"][deg] + oct));
function eddy(out, startBar, lenBars, triad, baseOct, gain = 0.16) {
  const pans = [-0.24, 0.0, 0.24];
  for (let i = 0; i < triad.length; i++) {
    out.push({
      preset: "vibraphone_off",
      startSec: startBar * BAR,
      midi: padDeg(triad[i], baseOct),
      durSec: lenBars * BAR + BEAT,
      gain,
      decayMul: 2.0,
      pan: pans[i] ?? 0,
    });
  }
}

// ── a single low gamelan "knock" — a struck point of light. ──────────────────
function knock(out, { startBar, beat, name, beats, gain = 0.22, pan = 0 }) {
  out.push({
    preset: "gamelan",
    startSec: startBar * BAR + beat * BEAT,
    midi: snap(m(name)),
    durSec: beats * BEAT,
    gain,
    decayMul: (DECAY.gamelan ?? 1.3) * 1.8,
    pan,
  });
}

const events = [];

// ════════════════════════════════════════════════════════════════════════
//  ARC: driftwood on a slow current — each pass slower, longer, lower.
//  The hush sigh seeds the drift; the current carries it down through a
//  gamelan eddy of the slinky "baba"; it settles on the seabed (sleep).
// ════════════════════════════════════════════════════════════════════════

// hush sigh as DORIAN-degree steps, to seed the walker so the marimbaba
// contour opens the piece: C5 A4 F4 F4 → roughly degrees down a 3rd, down a
// 3rd, hold. In degree-steps from the start that reads as a descending sigh.
const HUSH_SEED = [-2, -2, 0, +1, -1]; // gentle falling open, slight lift, settle

// ── PASS 0 (bars 0–5): the hush sigh, near the surface. stretch 1.0, the
//    walker barely wandering yet — the tune as we first hear it. Mid register
//    (startDeg ~ degree of C4-ish). ──────────────────────────────────────────
{
  // start around C4 in degree-space: C is dorian degree 4 (F G Ab Bb C),
  // two octaves up from F2 → deg = 2*7 + 4 = 18.
  drift(events, {
    startBar: 0, startDeg: 18, steps: 8, voice: "gamelan",
    baseBeats: 1.1, stretch: 1.0, downBias: 0.45, gain: 0.44,
    decayMul: 1.7, seed: 3, seedSteps: HUSH_SEED, gapBeats: 0.15,
  });
  bass(events, 0, "F2", 3, 0.42);
  bass(events, 3, "F2", 3, 0.42);
  eddy(events, 0, 6, [0, 2, 4], 3, 0.15); // F-Ab-C low pad
  knock(events, { startBar: 2, beat: 1.5, name: "C4", beats: 2, gain: 0.16, pan: 0.28 });
}

// ── PASS 1 (bars 6–12): the current takes it. stretch ~1.3, the walker
//    wanders freely now, register center dropped a step. A little deeper, a
//    little slower. ───────────────────────────────────────────────────────────
{
  drift(events, {
    startBar: 6, startDeg: 16, steps: 9, voice: "gamelan",
    baseBeats: 1.1, stretch: 1.35, downBias: 0.55, gain: 0.42,
    decayMul: 1.8, seed: 11, gapBeats: 0.2,
  });
  bass(events, 6, "F2", 3, 0.38);
  bass(events, 9, "Bb2", 3, 0.38); // subdominant drift (dorian IV is major)
  eddy(events, 6, 7, [3, 5, 0], 3, 0.14); // Bb-D-F
  knock(events, { startBar: 7, beat: 2.0, name: "Bb3", beats: 2.5, gain: 0.15, pan: -0.26 });
  knock(events, { startBar: 11, beat: 0.5, name: "D4", beats: 2, gain: 0.14, pan: 0.3 });
}

// ── PASS 2 (bars 13–20): MID-CURRENT EDDY — the slinky "baba" wobble surfaces,
//    now in the low gamelan register and augmented (slow). The recognizable
//    thread, made deep and unhurried. stretch ~1.7. Register sinking further. ─
{
  // seed the walker with the baba contour as degree-steps (its up-down wobble).
  // baba is roughly: A5 G5 A5 F5 C6 Bb5 C6 A5 → up/down small steps.
  const BABA_SEED = [-1, +1, -2, +4, -1, +1, -2, -1];
  drift(events, {
    startBar: 13, startDeg: 13, steps: 9, voice: "gamelan",
    baseBeats: 1.0, stretch: 1.7, downBias: 0.5, gain: 0.4,
    decayMul: 1.9, seed: 23, seedSteps: BABA_SEED, gapBeats: 0.25,
  });
  bass(events, 13, "Ab2", 3, 0.36);
  bass(events, 16, "Eb2", 3, 0.36); // bVI region — deep dorian colour
  bass(events, 19, "F2", 3, 0.36);
  eddy(events, 13, 4, [2, 4, 6], 3, 0.15); // Ab-C-Eb
  eddy(events, 17, 4, [0, 2, 4], 3, 0.14); // back toward F
  knock(events, { startBar: 14, beat: 1.0, name: "Eb4", beats: 3, gain: 0.14, pan: -0.3 });
  knock(events, { startBar: 18, beat: 1.5, name: "Ab3", beats: 3, gain: 0.13, pan: 0.28 });
}

// ── PASS 3 (bars 21–28): the slowest, deepest WANDER — stretch ~2.1, the wood
//    waterlogged, drifting in long low tones. Register dropped a whole octave
//    in spirit (startDeg low), the walker barely moving, sinking. ─────────────
{
  drift(events, {
    startBar: 21, startDeg: 9, steps: 8, voice: "gamelan",
    baseBeats: 1.0, stretch: 2.1, downBias: 0.62, gain: 0.4,
    decayMul: 2.0, seed: 37, gapBeats: 0.35,
  });
  bass(events, 21, "F2", 3, 0.36);
  bass(events, 24, "Eb2", 3, 0.34);
  bass(events, 27, "F2", 3, 0.34);
  eddy(events, 21, 8, [0, 2, 4], 2, 0.15); // very low F pad — the seabed glow
  knock(events, { startBar: 23, beat: 1.0, name: "C3", beats: 4, gain: 0.13, pan: 0.24 });
}

// ── PASS 4 (bars 29–36): SEABED — the marimbaba "sleep" cadence, augmented
//    (stretch ~2.4) and transposed down two octaves, played near-flat. The
//    driftwood comes to rest. The recognizable settle, very deep, very slow. ──
{
  // sleep motif: C5 A4 G4 F4 F4 F4 → settling descent. Lay it as degree-steps,
  // way down low, with long augmented durations and almost no wandering.
  const SLEEP_SEED = [-2, -1, -1, 0, 0, -1]; // descend and rest
  drift(events, {
    startBar: 29, startDeg: 8, steps: 7, voice: "gamelan",
    baseBeats: 1.0, stretch: 2.4, downBias: 0.7, gain: 0.42,
    decayMul: 2.2, seed: 5, seedSteps: SLEEP_SEED, gapBeats: 0.4,
  });
  // a final hush echo, the lowest of all — wood touching bottom.
  bass(events, 29, "F2", 3, 0.36);
  bass(events, 32, "F2", 3, 0.34);
  bass(events, 35, "F2", 3, 0.34);
  eddy(events, 29, 8, [0, 2, 4], 2, 0.16); // long resolving low F
  knock(events, { startBar: 31, beat: 1.0, name: "F3", beats: 5, gain: 0.13, pan: -0.22 });
  knock(events, { startBar: 34, beat: 0.5, name: "Ab2", beats: 6, gain: 0.12, pan: 0.2 });
  knock(events, { startBar: 35, beat: 1.5, name: "F2", beats: 5, gain: 0.12, pan: 0 });
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "driftwoodbaba",
  here: HERE,
  title: "driftwoodbaba",
  reverb: { wet: 0.44, decay: 0.9, damp: 0.3 }, // deep, wide underwater room
  fadeIn: 2.0,
  fadeOut: 6.0,
  tailSec: 5.0,
  peak: 0.84,
  healingHz: 396, // Solfeggio UT, low + grounding — suits the deep dorian
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
