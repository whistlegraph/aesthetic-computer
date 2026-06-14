// moonbaba.mjs — a moonlit, weightless riff on the marimbaba lullaby, now
// SHATTERED ACROSS THE REGISTERS.
//
// Direction: still Eb major, ~50 BPM, vibraphone-led dream haze, with the
// vibraphone_off pad breathing underneath. But the MELODY no longer sits in
// one register: it is scattered across 3–4 octaves, Webern-style pointillism
// softened for vibraphone. The same tune leaps high↔low so it feels vast and
// re-discovered on every pass.
//
// DEVELOPMENT STRATEGY — OCTAVE-DISPLACEMENT & REGISTER EXPLOSION:
//  - The whistlegraph contour (hush → twinkle → wow → baba → sleep) is kept
//    as the melodic DNA, but each note is octave-displaced. Consecutive notes
//    jump registers, so the moon's tune scatters into starlight.
//  - The displacement WIDENS across the piece: passes start near (±1 oct),
//    then explode to ±2/3 octaves at the apex (the "wow" float becomes a
//    register supernova), then COLLAPSE back into a close low register for
//    the moonset — the moon sinking into a single quiet octave.
//  - A recognizable thread survives: the descending hush sigh opens close,
//    the final cadence settles to Eb in the home octave, and the contour
//    (up-and-falling waves, the wobble, the slinky bap) is intact under the
//    octave scatter.
//
// Run:  node variations/moonbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 50;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT; // 3/4

// ── Eb major scale-fold (root pc = 3). Snap melodic voices to the mode. ──
const ROOT = 3; // Eb
const MAJOR = [0, 2, 4, 5, 7, 9, 11];
function snap(midi) {
  const pc = ((midi % 12) + 12) % 12;
  const rel = ((pc - ROOT) % 12 + 12) % 12;
  let best = MAJOR[0], bestD = 99;
  for (const s of MAJOR) {
    const d = Math.min(Math.abs(s - rel), 12 - Math.abs(s - rel));
    if (d < bestD) { bestD = d; best = s; }
  }
  const base = midi - rel;
  let cand = base + best;
  if (cand - midi > 6) cand -= 12;
  if (midi - cand > 6) cand += 12;
  return cand;
}

const LEAD = "vibraphone";

// ── motif → Eb-folded MIDI cells. The marimbaba MOTIFS are written in F;
//    transpose -2 into Eb territory then snap into the mode. Each cell is a
//    list of [midi, beats] pairs we can octave-displace freely. ──────────────
function cell(motif) {
  return MOTIFS[motif].map(([name, beats]) => [snap(m(name) - 2), beats]);
}

// ── OCTAVE-DISPLACEMENT ENGINE ─────────────────────────────────────────────
// scatter(cell, opts) returns a fresh cell whose notes are nudged by whole
// octaves. `spread` is the max number of octaves a note may jump (the
// register width); `center` shifts the whole cloud up/down; `seed` makes the
// scatter deterministic so a pass is repeatable; `pattern` (optional) forces
// the per-note octave offsets so we can hand-shape the contour.
function lcg(seed) {
  let s = (seed * 2654435761) >>> 0;
  return () => ((s = (s * 1103515245 + 12345) >>> 0) / 4294967296);
}
function scatter(c, { spread = 1, center = 0, seed = 1, pattern = null } = {}) {
  const rnd = lcg(seed);
  return c.map(([midi, beats], i) => {
    let oct;
    if (pattern) {
      oct = pattern[i % pattern.length];
    } else {
      // bias alternate notes to opposite extremes so the line zig-zags hard
      const reach = Math.round(rnd() * spread);
      const dir = i % 2 === 0 ? 1 : -1;
      oct = dir * reach;
    }
    return [midi + center * 12 + oct * 12, beats];
  });
}

// transpose a whole cell by an interval (for sequencing) before scattering.
function tpose(c, semis) { return c.map(([midi, beats]) => [snap(midi + semis), beats]); }
// invert a cell around its first note (mirror the intervals).
function invert(c) {
  const axis = c[0][0];
  return c.map(([midi, beats]) => [snap(axis - (midi - axis)), beats]);
}
// retrograde — play the cell backwards.
function retro(c) { return [...c].reverse(); }

// ── lay a cell into events at a bar, voice, with optional ornament tail. ─────
function lay(out, c, { startBar, voice = LEAD, gain = 0.5, panSpread = 0.34, decayMul = 1.6, beat0 = 0 } = {}) {
  let beat = beat0;
  const baseBar = startBar;
  for (let i = 0; i < c.length; i++) {
    const [midi, beats] = c[i];
    // pan follows register: high notes drift right, low notes left → the
    // scatter is spatial as well as registral.
    const reg = (midi - 63) / 24; // ~ -1..+1 across the explosion
    const pan = Math.max(-0.5, Math.min(0.5, reg * panSpread));
    out.push({
      preset: voice,
      startSec: baseBar * BAR + beat * BEAT,
      midi,
      durSec: beats * BEAT,
      gain,
      decayMul: (DECAY[voice] ?? 1) * decayMul,
      pan,
    });
    beat += beats;
  }
  return beat; // beats consumed (for chaining)
}

// glockenspiel/kalimba "stars" — a single high sparkle.
function star(out, { startBar, beat, name, beats, voice = "glockenspiel", gain = 0.13, pan = 0.3 }) {
  out.push({
    preset: voice,
    startSec: startBar * BAR + beat * BEAT,
    midi: snap(m(name)),
    durSec: beats * BEAT,
    gain,
    decayMul: (DECAY[voice] ?? 1) * 1.6,
    pan,
  });
}

// vibraphone_off pad (Eb-folded triad) breathing under a span of bars.
const padDeg = (deg, oct) => snap(m(["Eb", "F", "G", "Ab", "Bb", "C", "D"][deg] + oct));
function pad(out, startBar, lenBars, triad, baseOct, gain = 0.13) {
  for (let i = 0; i < triad.length; i++) {
    out.push({
      preset: "vibraphone_off",
      startSec: startBar * BAR,
      midi: padDeg(triad[i], baseOct),
      durSec: lenBars * BAR + BEAT,
      gain,
      decayMul: 2.0,
      pan: i === 0 ? -0.22 : i === 1 ? 0.0 : 0.22,
    });
  }
}

// soft bass root under a bar (kept low + clear).
function bass(out, startBar, name, lenBars = 1, gain = 0.4) {
  out.push({
    preset: "bass",
    startSec: startBar * BAR,
    midi: snap(m(name)),
    durSec: lenBars * BAR,
    gain,
    decayMul: DECAY.bass * 1.4,
    pan: 0,
  });
}

const events = [];

// ════════════════════════════════════════════════════════════════════════
//  ARC: close → widening → register explosion → collapse to moonset.
// ════════════════════════════════════════════════════════════════════════

// ── PASS 0 (bars 0–3): the hush sigh, almost CLOSE — the moon's tune as we
//    first hear it. Only a tiny ±1-oct displacement on the last note so the
//    recognizable descending sigh stays legible before it scatters. ─────────
{
  const hush = cell("hush");
  lay(events, scatter(hush, { pattern: [0, 0, 0, -1] }), { startBar: 0, gain: 0.5, decayMul: 1.7 });
  bass(events, 0, "Eb2", 2, 0.42);
  bass(events, 2, "Eb2", 2, 0.42);
  pad(events, 0, 4, [0, 2, 4], 4, 0.12); // Eb pad bed
  star(events, { startBar: 2, beat: 2.2, name: "Bb5", beats: 1.5, gain: 0.09, pan: 0.3 });
}

// ── PASS 1 (bars 4–9): twinkle, beginning to SCATTER (±1 oct). The climbing
//    wave still climbs but each step hops a register — first glints of the
//    explosion. ───────────────────────────────────────────────────────────
{
  const tw = cell("twinkle");
  lay(events, scatter(tw, { spread: 1, seed: 7 }), { startBar: 4, gain: 0.48, panSpread: 0.4 });
  // answer: the same wave, sequenced up a 3rd, scattered the other way (canon-ish)
  lay(events, scatter(tpose(tw, 4), { spread: 1, seed: 13 }), { startBar: 6, gain: 0.42, panSpread: 0.42 });
  // bars 8–9: a flyHigh fragment, scatter ±1, as a high lift
  lay(events, scatter(cell("flyHigh"), { spread: 1, seed: 21, center: 0 }), { startBar: 8, gain: 0.44, panSpread: 0.45 });
  bass(events, 4, "Eb2", 2, 0.36); bass(events, 6, "Ab2", 2, 0.36); bass(events, 8, "Bb2", 2, 0.36);
  pad(events, 4, 3, [0, 2, 4], 4, 0.12);
  pad(events, 7, 3, [3, 5, 0], 4, 0.12); // Ab
  // kalimba grace flurries answering the climbs
  star(events, { startBar: 5, beat: 1.0, name: "G5", beats: 1, voice: "kalimba", gain: 0.13, pan: -0.26 });
  star(events, { startBar: 9, beat: 0.5, name: "C6", beats: 1.5, voice: "glockenspiel", gain: 0.12, pan: 0.34 });
}

// ── PASS 2 (bars 10–15): THE REGISTER EXPLOSION. The "wow" wobble is no
//    longer a held dyad — it detonates across 3 octaves, then the baba
//    slinky-bap is hurled even wider (±3 oct). This is the apex: vast,
//    re-discovered, weightless. Cascading glock starlight overhead. ─────────
{
  // wow, scattered ±2 octaves with a hand-shaped zig-zag for a clear leap arc
  const wow = cell("wow");
  lay(events, scatter(wow, { pattern: [0, 2, -1, 2, -2, 1] }), { startBar: 10, gain: 0.42, panSpread: 0.5, decayMul: 1.9 });
  // an inverted echo of the wow, one bar later, scattered the opposite way —
  // the tune answering itself across the void (stretto).
  lay(events, scatter(invert(wow), { pattern: [-2, 1, -1, 2, 0, 2] }), { startBar: 12, gain: 0.34, panSpread: 0.5, decayMul: 1.9 });
  // baba slinky-bap flung the WIDEST — ±3 octaves, the supernova
  const baba = cell("baba");
  lay(events, scatter(baba, { spread: 3, seed: 41 }), { startBar: 14, gain: 0.4, panSpread: 0.5, decayMul: 1.7 });

  // cascading glockenspiel + kalimba starlight raining through the explosion
  const cascade = [
    [10, 2.0, "Eb6", "glockenspiel", 0.34], [11, 0.5, "Bb5", "kalimba", -0.28],
    [11, 1.6, "G6", "glockenspiel", 0.38], [12, 2.2, "Eb5", "kalimba", -0.3],
    [13, 0.8, "C6", "glockenspiel", 0.32], [13, 2.0, "Ab6", "glockenspiel", 0.4],
    [14, 1.2, "F6", "glockenspiel", 0.36], [15, 0.6, "Bb6", "glockenspiel", 0.42],
    [15, 2.0, "G5", "kalimba", -0.26],
  ];
  for (const [b, bt, n, v, pn] of cascade) star(events, { startBar: b, beat: bt, name: n, beats: 1.5, voice: v, gain: 0.11, pan: pn });

  // bass holds the harmonic floor clear under the chaos
  bass(events, 10, "Bb2", 2, 0.34); bass(events, 12, "Eb2", 2, 0.34); bass(events, 14, "Ab2", 2, 0.34);
  pad(events, 10, 4, [4, 6, 1], 4, 0.12); // Bb (dominant) under the float
  pad(events, 14, 2, [3, 5, 0], 4, 0.12); // Ab
}

// ── PASS 3 (bars 16–19): the explosion begins COLLAPSING. The twinkle wave
//    returns in retrograde (the tune folding back on itself), displacement
//    narrowing from ±2 to ±1, pulling the scattered stars back together. ────
{
  const twR = retro(cell("twinkle"));
  lay(events, scatter(twR, { spread: 2, seed: 55 }), { startBar: 16, gain: 0.4, panSpread: 0.42, decayMul: 1.7 });
  lay(events, scatter(cell("wow"), { spread: 1, seed: 61 }), { startBar: 18, gain: 0.36, panSpread: 0.34, decayMul: 1.7 });
  bass(events, 16, "Ab2", 2, 0.34); bass(events, 18, "Bb2", 2, 0.34);
  pad(events, 16, 4, [4, 6, 1], 4, 0.12); // Bb resolving toward home
  star(events, { startBar: 17, beat: 1.5, name: "Eb6", beats: 2, gain: 0.1, pan: 0.32 });
}

// ── PASS 4 (bars 20–25): MOONSET. The sleep cadence collapses into a single
//    CLOSE, LOW register — the moon sinking into one quiet octave. No
//    displacement now: the tune comes home, recognizable and settled. ───────
{
  const sleep = cell("sleep");
  // pull the whole cadence down into the low-mid register (center -1) and
  // play it flat (no scatter) — the collapse made literal.
  lay(events, tpose(sleep, -12), { startBar: 20, gain: 0.46, panSpread: 0.18, decayMul: 1.9 });
  // a final, faint, close hush echo to close the frame (recognizable thread)
  lay(events, tpose(cell("hush"), -12), { startBar: 23, gain: 0.4, panSpread: 0.16, decayMul: 2.0 });
  bass(events, 20, "Eb2", 2, 0.36); bass(events, 22, "Bb2", 2, 0.34); bass(events, 24, "Eb2", 2, 0.34);
  pad(events, 20, 6, [0, 2, 4], 4, 0.13); // Eb — long resolving moonset
  // one last, very high, very soft star fading out over the home octave
  star(events, { startBar: 23, beat: 1.0, name: "Eb6", beats: 4, gain: 0.08, pan: 0.3 });
  star(events, { startBar: 24, beat: 1.5, name: "Bb5", beats: 3, voice: "kalimba", gain: 0.09, pan: -0.24 });
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "moonbaba",
  here: HERE,
  title: "moonbaba",
  reverb: { wet: 0.42, decay: 0.88, damp: 0.34 }, // deep, glassy moon-room
  fadeIn: 1.6,
  fadeOut: 6.0,
  tailSec: 6.0,
  peak: 0.82,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
