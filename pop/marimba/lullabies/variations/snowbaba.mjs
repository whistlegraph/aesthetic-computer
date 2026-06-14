// snowbaba.mjs — a falling-snow riff on the marimbaba lullaby, dissolved into
// DRIFTING SINGLE NOTES that sink through the registers and settle to silence.
//
// Direction: Bb major, glockenspiel, ~50 BPM. The whistlegraph tune (hush →
// twinkle → wow → baba → sleep) is scattered into solitary, octave-displaced
// flakes — each note drifts down a register or two from the last, the line
// ever sparser and slower, until the snow has settled and only the room rings.
//
// DEVELOPMENT STRATEGY — DOWNWARD OCTAVE-DISPLACED POINTILLISM:
//  - The marimbaba contour is kept as the melodic DNA, but the tune is broken
//    into single drifting notes — no chords in the lead, just flakes.
//  - Each successive note is biased DOWNWARD: a descending octave-walk runs
//    under the cells so the whole field of notes sinks through the registers
//    as the piece proceeds (the snow falling from sky to ground).
//  - The motion THINS and SLOWS: early passes drift in regular eighths/quarters,
//    later passes stretch the inter-onset gaps and drop notes, so the flurry
//    becomes the last few flakes, then one, then the bed alone.
//  - A recognizable thread survives: the descending hush sigh opens it (snow
//    starting), the wow wobble and slinky baba are legible inside the drift,
//    and the final sleep cadence settles to Bb in the low home octave —
//    the snow at rest on the ground.
//
// Run:  node variations/snowbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 50;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT; // 3/4

// ── Bb major scale-fold (root pc = 10). Snap melodic voices to the mode. ──
const ROOT = 10; // Bb
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

const LEAD = "glockenspiel";

// ── motif → Bb-folded MIDI cells. The marimbaba MOTIFS are written in F;
//    transpose +5 up into Bb territory, snap into the mode. Each cell is a
//    list of [midi, beats] pairs we drift downward and re-rhythm freely. ──
function cell(motif) {
  return MOTIFS[motif].map(([name, beats]) => [snap(m(name) + 5), beats]);
}

// transpose a whole cell (for sequencing) and re-snap into Bb.
function tpose(c, semis) { return c.map(([midi, beats]) => [snap(midi + semis), beats]); }
// retrograde — play the cell backwards (the snow re-falling).
function retro(c) { return [...c].reverse(); }

// ── DOWNWARD DRIFT ENGINE ────────────────────────────────────────────────
// drift(cell, opts) scatters each note into a SINGLE drifting flake whose
// octave sinks as the cell proceeds. `start` is the octave offset of the first
// flake; `fall` is how many octaves the whole cell descends across its length;
// `jitter` adds gentle per-note wobble (deterministic via seed) so flakes
// don't fall in a straight column. The pitches stay in the mode after the
// octave shift.
function lcg(seed) {
  let s = (seed * 2654435761) >>> 0;
  return () => ((s = (s * 1103515245 + 12345) >>> 0) / 4294967296);
}
function drift(c, { start = 0, fall = 1, jitter = 0, seed = 1 } = {}) {
  const rnd = lcg(seed);
  const n = Math.max(1, c.length - 1);
  return c.map(([midi, beats], i) => {
    // smooth descent across the cell + small octave wobble on some flakes
    const sink = -Math.round((i / n) * fall);
    const wob = jitter && rnd() < jitter ? (rnd() < 0.5 ? -1 : 1) : 0;
    return [snap(midi + (start + sink + wob) * 12), beats];
  });
}

// ── lay a cell as drifting flakes: gaps OPEN as `spacing` grows, so the field
//    thins over the piece. `spacing` multiplies the rests between flakes;
//    `slow` multiplies each flake's own beats (longer ring as snow settles).
//    Pan follows register: high flakes drift right, low flakes left, so the
//    fall is spatial as well as registral. ─────────────────────────────────
function lay(out, c, { startBar, beat0 = 0, voice = LEAD, gain = 0.46, panSpread = 0.42, decayMul = 1.7, spacing = 1, slow = 1 } = {}) {
  let beat = beat0;
  for (let i = 0; i < c.length; i++) {
    const [midi, beats] = c[i];
    const reg = (midi - 70) / 24; // ~ -1..+1 across the fall
    const pan = Math.max(-0.5, Math.min(0.5, reg * panSpread));
    out.push({
      preset: voice,
      startSec: startBar * BAR + beat * BEAT,
      midi,
      durSec: beats * slow * BEAT,
      gain,
      decayMul: (DECAY[voice] ?? 1) * decayMul,
      pan,
    });
    beat += beats * spacing;
  }
  return beat;
}

// a lone flake — a single drifting glockenspiel/kalimba note.
function flake(out, { startBar, beat, name, beats, voice = LEAD, gain = 0.2, pan = 0.3, decayMul = 1.8, oct = 0 }) {
  out.push({
    preset: voice,
    startSec: startBar * BAR + beat * BEAT,
    midi: snap(m(name) + oct * 12),
    durSec: beats * BEAT,
    gain,
    decayMul: (DECAY[voice] ?? 1) * decayMul,
    pan,
  });
}

// vibraphone_off pad (Bb-folded triad) breathing under a span of bars — the
// quiet grey sky behind the snow.
const padDeg = (deg, oct) => snap(m(["Bb", "C", "D", "Eb", "F", "G", "A"][deg] + oct));
function pad(out, startBar, lenBars, triad, baseOct, gain = 0.12) {
  for (let i = 0; i < triad.length; i++) {
    out.push({
      preset: "vibraphone_off",
      startSec: startBar * BAR,
      midi: padDeg(triad[i], baseOct),
      durSec: lenBars * BAR + BEAT,
      gain,
      decayMul: 2.1,
      pan: i === 0 ? -0.22 : i === 1 ? 0.0 : 0.22,
    });
  }
}

// soft bass root under a bar (kept low + clear) — the ground gathering snow.
function bass(out, startBar, name, lenBars = 1, gain = 0.38) {
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
//  ARC: first flakes from the sky → a sinking flurry → ever sparser, slower
//        → the last flakes → settled on the ground, silence.
// ════════════════════════════════════════════════════════════════════════

// ── PASS 0 (bars 0–3): FIRST FLAKES. The hush sigh as single high glints,
//    each a register lower than the last — snow beginning to fall from a
//    high, still sky. Dense-ish, regular drift. ─────────────────────────────
{
  const hush = cell("hush");
  lay(events, drift(hush, { start: 1, fall: 1, jitter: 0.25, seed: 3 }),
    { startBar: 0, gain: 0.42, decayMul: 1.7, spacing: 1, slow: 1 });
  // a couple of stray early flakes overhead
  flake(events, { startBar: 1, beat: 0.6, name: "F6", beats: 1.5, gain: 0.16, pan: 0.34 });
  flake(events, { startBar: 2, beat: 1.4, name: "Bb6", beats: 1.5, gain: 0.14, pan: -0.3, voice: "kalimba" });
  bass(events, 0, "Bb2", 2, 0.36);
  bass(events, 2, "Bb2", 2, 0.36);
  pad(events, 0, 4, [0, 2, 4], 4, 0.11); // Bb sky
}

// ── PASS 1 (bars 4–9): the flurry THICKENS, twinkle climbing then sinking.
//    The climbing wave still climbs in pitch-class but each step is dragged a
//    register downward — the snow swirling up briefly then falling. Two
//    sequenced passes drift further down. ───────────────────────────────────
{
  const tw = cell("twinkle");
  lay(events, drift(tw, { start: 1, fall: 1, jitter: 0.3, seed: 7 }),
    { startBar: 4, gain: 0.44, panSpread: 0.44, spacing: 1, slow: 1 });
  // answer a 3rd below, drifting one octave lower — the field deepening
  lay(events, drift(tpose(tw, -3), { start: 0, fall: 1, jitter: 0.3, seed: 11 }),
    { startBar: 6, gain: 0.4, panSpread: 0.44, spacing: 1.1, slow: 1.05 });
  // a flyHigh fragment, but DRIFTING DOWN — the highest flakes already sinking
  lay(events, drift(cell("flyHigh"), { start: 0, fall: 2, jitter: 0.2, seed: 17 }),
    { startBar: 8, gain: 0.4, panSpread: 0.45, spacing: 1.1, slow: 1.1 });
  bass(events, 4, "Bb2", 2, 0.32); bass(events, 6, "Eb2", 2, 0.32); bass(events, 8, "F2", 2, 0.32);
  pad(events, 4, 3, [0, 2, 4], 4, 0.11);
  pad(events, 7, 3, [3, 5, 0], 4, 0.11); // Eb
  flake(events, { startBar: 5, beat: 2.2, name: "G6", beats: 1.5, gain: 0.13, pan: 0.34 });
  flake(events, { startBar: 9, beat: 0.6, name: "D6", beats: 2, gain: 0.13, pan: -0.28, voice: "kalimba" });
}

// ── PASS 2 (bars 10–15): the wow wobble and slinky baba, now DRIFTING DOWN
//    through the middle registers. Still recognizable, but each gesture sinks
//    a full octave or two over its length, and the gaps begin to OPEN — the
//    flurry starting to thin as the snow finds its weight. ──────────────────
{
  const wow = cell("wow");
  lay(events, drift(wow, { start: 0, fall: 2, jitter: 0.25, seed: 23 }),
    { startBar: 10, gain: 0.42, panSpread: 0.42, decayMul: 1.8, spacing: 1.15, slow: 1.1 });
  // slinky baba, drifting one octave lower again, gaps opening further
  const baba = cell("baba");
  lay(events, drift(tpose(baba, -5), { start: 0, fall: 2, jitter: 0.2, seed: 29 }),
    { startBar: 13, gain: 0.4, panSpread: 0.42, decayMul: 1.8, spacing: 1.25, slow: 1.15 });
  // scattered low flakes catching the light as they pass
  flake(events, { startBar: 11, beat: 2.0, name: "Bb4", beats: 2, gain: 0.16, pan: -0.24, voice: "kalimba" });
  flake(events, { startBar: 14, beat: 1.5, name: "D5", beats: 2.5, gain: 0.15, pan: 0.22 });
  bass(events, 10, "F2", 2, 0.3); bass(events, 12, "Bb2", 2, 0.3); bass(events, 14, "Eb2", 2, 0.3);
  pad(events, 10, 3, [4, 6, 1], 4, 0.11); // F (dominant)
  pad(events, 13, 3, [0, 2, 4], 4, 0.11); // Bb
}

// ── PASS 3 (bars 16–21): EVER SPARSER, SLOWER. The twinkle wave returns in
//    retrograde (the snow re-falling, settling), pulled down into the low-mid
//    register; the spacing stretches wide and each flake rings longer. Only a
//    few flakes left aloft now. ─────────────────────────────────────────────
{
  const twR = retro(cell("twinkle"));
  lay(events, drift(twR, { start: -1, fall: 1, jitter: 0.15, seed: 37 }),
    { startBar: 16, gain: 0.38, panSpread: 0.3, decayMul: 1.9, spacing: 1.5, slow: 1.4 });
  // a sparse hush echo, very low and slow — almost the last of it
  lay(events, drift(tpose(cell("hush"), -12), { start: 0, fall: 1, jitter: 0 }),
    { startBar: 19, gain: 0.36, panSpread: 0.2, decayMul: 2.0, spacing: 1.7, slow: 1.6 });
  bass(events, 16, "Eb2", 2, 0.3); bass(events, 18, "F2", 2, 0.3); bass(events, 20, "Bb2", 2, 0.3);
  pad(events, 16, 3, [3, 5, 0], 4, 0.11); // Eb
  pad(events, 19, 4, [4, 6, 1], 4, 0.11); // F resolving toward home
  flake(events, { startBar: 17, beat: 2.0, name: "Bb5", beats: 3, gain: 0.11, pan: 0.3 });
  flake(events, { startBar: 20, beat: 1.0, name: "F5", beats: 3, gain: 0.1, pan: -0.24, voice: "kalimba" });
}

// ── PASS 4 (bars 22–27): SETTLED. The sleep cadence comes home, low and
//    close, in slow single flakes — the snow at rest on the ground. The last
//    notes drift wide apart, the very last one alone, then only the bed rings
//    to silence. ────────────────────────────────────────────────────────────
{
  const sleep = cell("sleep");
  // the cadence flat in the low home octave, very slow, gaps wide — the
  // collapse to stillness made literal.
  lay(events, drift(tpose(sleep, -12), { start: 0, fall: 0, jitter: 0 }),
    { startBar: 22, gain: 0.44, panSpread: 0.16, decayMul: 2.0, spacing: 1.9, slow: 1.7 });
  bass(events, 22, "Bb2", 2, 0.32); bass(events, 24, "F2", 2, 0.3); bass(events, 26, "Bb2", 2, 0.3);
  pad(events, 22, 6, [0, 2, 4], 4, 0.12); // Bb — long resolving ground

  // the final few flakes, ever farther apart, the last one alone over silence.
  flake(events, { startBar: 24, beat: 2.0, name: "Bb4", beats: 4, gain: 0.13, pan: 0.26 });
  flake(events, { startBar: 26, beat: 1.0, name: "F4", beats: 4, gain: 0.11, pan: -0.2, voice: "kalimba" });
  flake(events, { startBar: 27, beat: 2.0, name: "Bb3", beats: 5, gain: 0.1, pan: 0.0 }); // last flake, settled
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "snowbaba",
  here: HERE,
  title: "snowbaba",
  reverb: { wet: 0.4, decay: 0.86, damp: 0.36 }, // soft, snow-muffled room
  fadeIn: 1.4,
  fadeOut: 6.5,
  tailSec: 6.5,
  peak: 0.82,
  healingHz: 852, // Solfeggio LA — high, crystalline, suits the snow + Bb
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
