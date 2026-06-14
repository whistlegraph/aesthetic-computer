// mistbaba.mjs — a whole-tone MIST riff on the marimbaba seed, now GENERATIVE.
//
// Identity unchanged: ~50 BPM, F whole-tone scale {0,2,4,6,8,10} (no perfect
// fifths, no leading tone — nothing resolves), foregrounded vibraphone_off pad
// clouds, sparse rosewood lead + kalimba glints, long smeary reverb. You fall
// asleep inside one breathing fog.
//
// What changed: the melody no longer restates the hush/sleep contours and
// stops. Instead a single seed phrase — the hush descent (C5-A4-F4) snapped to
// whole-tone — is fed through a DETERMINISTIC ALEATORIC WANDER. Each phrase is
// the previous one with one or two notes nudged a whole-tone step (sometimes an
// octave displaced), chosen by a seeded PRNG so it is reproducible but never
// repeats. Over ~24 phrases the tune drifts clean across the whole-tone field,
// always becoming something else — dreamlike, unmoored. The hush contour is the
// genome; everything you hear is a mutation of it. It comes home at the end:
// the wander is gently pulled back toward the original hush + sleep cadence so
// the mist exhales into the recognizable seed before fading.
//
// Run:  node variations/mistbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 50;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT;

// ── F whole-tone: root pitch-class F (5), scale {0,2,4,6,8,10} ───────────────
const ROOT_PC = m("F4") % 12; // 5
const WHOLETONE = [0, 2, 4, 6, 8, 10];

// Fold a midi note to the nearest pitch in the whole-tone scale (keeps the
// octave region; ties resolve downward so things settle / sink into sleep).
function snap(midi, rootPc = ROOT_PC, scale = WHOLETONE) {
  let best = midi, bestD = Infinity;
  for (let oct = -1; oct <= 1; oct++) {
    for (const deg of scale) {
      const pc = (rootPc + deg) % 12;
      const base = Math.round((midi - pc) / 12) * 12 + pc + oct * 12;
      const d = Math.abs(base - midi);
      if (d < bestD - 1e-6 || (Math.abs(d - bestD) < 1e-6 && base < best)) {
        best = base; bestD = d;
      }
    }
  }
  return best;
}

// ── seeded PRNG (mulberry32) — deterministic so the wander is reproducible ───
function rng(seed) {
  let a = seed >>> 0;
  return () => {
    a |= 0; a = (a + 0x6D2B79F5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const rand = rng(0x6D157BABA & 0x7fffffff); // "mistbaba" seed

// ── the whole-tone ladder the wander walks ──────────────────────────────────
// All whole-tone pitches across the lead's comfortable register, as a sorted
// index space. Mutations step ±1 / ±2 rungs (and occasionally ±octave) so the
// tune always stays on the scale but never settles.
const LADDER = [];
for (let mid = m("D4"); mid <= m("D6"); mid++) {
  const s = snap(mid);
  if (!LADDER.includes(s)) LADDER.push(s);
}
LADDER.sort((a, b) => a - b);
const ladderIdx = (midi) => {
  const s = snap(midi);
  let bi = 0, bd = Infinity;
  for (let i = 0; i < LADDER.length; i++) {
    const d = Math.abs(LADDER[i] - s);
    if (d < bd) { bd = d; bi = i; }
  }
  return bi;
};
const clampIdx = (i) => Math.max(0, Math.min(LADDER.length - 1, i));

// ── seed genome: the hush sigh, snapped to whole-tone, as ladder indices ─────
// [idx, beats] cells. This is what gets mutated, generation after generation.
const seedHush = MOTIFS.hush.map(([note, beats]) => [ladderIdx(m(note)), beats]);

// mutate(phrase) — copy the phrase, nudge one or two notes by a whole-tone step
// (rarely an octave leap), and sometimes re-rhythm a single cell. Pure of the
// previous phrase; deterministic via the shared rand().
function mutate(phrase, intensity = 1) {
  const next = phrase.map((c) => c.slice());
  const nNudge = 1 + (rand() < 0.45 * intensity ? 1 : 0);
  for (let k = 0; k < nNudge; k++) {
    const i = Math.floor(rand() * next.length);
    const roll = rand();
    let step;
    if (roll < 0.12 * intensity) step = (rand() < 0.5 ? -1 : 1) * 5; // octave-ish leap (5 rungs ≈ 8ve)
    else if (roll < 0.5) step = (rand() < 0.5 ? -1 : 1);             // ±1 whole-tone
    else step = (rand() < 0.5 ? -2 : 2);                            // ±2 whole-tones
    next[i][0] = clampIdx(next[i][0] + step);
  }
  // occasional gentle re-rhythm: stretch or split one cell's duration.
  if (rand() < 0.3 * intensity) {
    const i = Math.floor(rand() * next.length);
    next[i][1] = Math.max(0.5, next[i][1] + (rand() < 0.5 ? -0.5 : 1));
  }
  return next;
}

// pull(phrase, target, amount) — nudge each note partway back toward a target
// phrase's contour (used to bring the wander home at the end). amount in rungs.
function pull(phrase, target, amount) {
  return phrase.map((c, i) => {
    const t = target[i % target.length][0];
    const cur = c[0];
    const dir = Math.sign(t - cur);
    const stepped = clampIdx(cur + dir * Math.min(amount, Math.abs(t - cur)));
    return [stepped, c[1]];
  });
}

const events = [];

// ── 1) the pad mist (UNCHANGED identity) — overlapping whole-tone clouds ──────
// Slow vibraphone_off stacks, each started before the last decays, so the fog
// never clears. These hold the harmonic ground the melody drifts over. The
// chain is longer now to give the wander room to travel.
function cloud(startBar, beats, notes, gain, pan) {
  for (const n of notes) {
    events.push({
      preset: "vibraphone_off",
      startSec: startBar * BAR,
      midi: snap(m(n)),
      durSec: beats * BEAT,
      gain,
      decayMul: (DECAY.vibraphone_off ?? 1.4) * 1.6,
      pan,
    });
  }
}
cloud(0, 15, ["F3", "G3", "A4", "B4"], 0.2, -0.22);
cloud(4, 15, ["A3", "B3", "C5", "D5"], 0.18, 0.2);
cloud(8, 15, ["G3", "A3", "C5", "Eb5"], 0.18, -0.18);
cloud(12, 15, ["F3", "Ab3", "B4", "C5"], 0.17, 0.22);
cloud(16, 16, ["D3", "F3", "A4", "B4"], 0.17, -0.2);
cloud(20, 16, ["G3", "B3", "C5", "D5"], 0.16, 0.18);
cloud(24, 16, ["Ab3", "B3", "Eb5", "F5"], 0.16, -0.21);
cloud(28, 16, ["F3", "A3", "B4", "C5"], 0.16, 0.2);
cloud(32, 16, ["D3", "G3", "A4", "G4"], 0.16, -0.16); // settle, low

// ── 2) the GENERATIVE WANDER — the melody that never repeats ──────────────────
// Start from the hush genome and emit one mutated phrase per ~2 bars, walking
// continuously through the mist. Each phrase = the previous one, nudged. The
// lead voice subtly cross-fades rosewood/kalimba/vibraphone as it drifts so the
// timbre wanders too. Notes ring long and overlap, smearing into the pad.
const LEAD_VOICES = ["rosewood", "rosewood", "kalimba", "rosewood", "vibraphone"];

function emitPhrase(phrase, startBar, voice, gain, pan, beatScale, durMul, octaveShift) {
  let bar = startBar, beat = 0;
  for (const [idx, beats] of phrase) {
    const span = beats * beatScale;
    const midi = LADDER[clampIdx(idx)] + octaveShift;
    events.push({
      preset: voice,
      startSec: bar * BAR + beat * BEAT,
      midi,
      durSec: span * durMul * BEAT,
      gain,
      decayMul: (DECAY[voice] ?? 1.7) * 1.5,
      pan,
    });
    beat += span;
    while (beat >= 3) { beat -= 3; bar += 1; }
  }
  return bar + (beat > 0 ? 1 : 0);
}

// Drift across bars 1..27. ~14 generations, ~1.85 bars apart (overlapping).
let phrase = seedHush.slice();
const GENERATIONS = 14;
const GEN_STEP = 1.85;
const startBars = [];
for (let g = 0; g < GENERATIONS; g++) startBars.push(Math.round((1 + g * GEN_STEP) * 10) / 10);

for (let g = 0; g < GENERATIONS; g++) {
  // mutate from the previous phrase (the seed is generation 0, played as-is).
  if (g > 0) {
    // intensity swells toward the middle then eases — an arc of strangeness.
    const t = g / (GENERATIONS - 1);
    const intensity = 0.6 + 1.1 * Math.sin(Math.PI * t); // 0.6 → ~1.7 → 0.6
    phrase = mutate(phrase, intensity);
  }
  const startBar = startBars[g];
  // wander the timbre + register so it feels like the tune is travelling.
  const voice = LEAD_VOICES[g % LEAD_VOICES.length];
  const pan = 0.34 * Math.sin(g * 1.31); // slow stereo drift
  // octave displacement drifts the whole line up into chimes, then sinks back.
  const t = g / (GENERATIONS - 1);
  const octaveShift = (Math.sin(Math.PI * t - 0.3) > 0.6 ? 12 : 0) - (t > 0.85 ? 12 : 0);
  // gentler gains for kalimba/vibraphone so the lead breathes evenly.
  const baseGain = voice === "kalimba" ? 0.2 : voice === "vibraphone" ? 0.24 : 0.3;
  const gain = baseGain * (0.85 + 0.3 * Math.sin(g * 0.7)); // subtle dynamic swell
  const beatScale = 2.0 + 0.5 * Math.sin(g * 0.9);  // breathe the tempo of phrases
  const durMul = 1.3 + 0.2 * (rand() - 0.5);
  emitPhrase(phrase, startBar, voice, gain, pan, beatScale, durMul, octaveShift);
}

// ── 3) coming home — the wander is pulled back toward the seed cadence ─────────
// After the long drift, gently haul the current phrase back toward the original
// hush, then state the seed's sleep settle as the last recognizable word. The
// mist exhales into the tune it was always derived from.
let homing = phrase.slice();
homing = pull(homing, seedHush, 2);
emitPhrase(homing, 28, "rosewood", 0.26, -0.1, 2.4, 1.4, 0);

homing = pull(homing, seedHush, 4);
emitPhrase(homing, 31, "rosewood", 0.24, 0.1, 2.5, 1.4, 0);

// the seed's sleep settle, whole-tone-snapped — last word in the fog.
const sleepCells = MOTIFS.sleep.map(([note, beats]) => [ladderIdx(m(note)), beats]);
emitPhrase(sleepCells, 34, "rosewood", 0.22, 0, 2.2, 1.5, 0);

// ── 4) far kalimba glints — a few distant chimes catching light in the fog ────
// Single whole-tone tonics/9ths up high, scattered so they never form a phrase
// of their own — just sparkle threading through the wandering tune.
const glints = [
  [7, "F6", 0.15, 0.34],
  [14, "G6", 0.13, 0.3],
  [21, "B5", 0.15, 0.36],
  [27, "F6", 0.12, 0.28],
  [33, "Ab5", 0.13, 0.32],
];
for (const [bar, note, gain, pan] of glints) {
  events.push({
    preset: "kalimba",
    startSec: bar * BAR + (rand() < 0.5 ? 0 : 1.5) * BEAT,
    midi: snap(m(note)),
    durSec: 4 * BEAT,
    gain,
    decayMul: (DECAY.kalimba ?? 1.75) * 1.6,
    pan,
  });
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "mistbaba",
  here: HERE,
  title: "mistbaba",
  reverb: { wet: 0.4, decay: 0.88, damp: 0.32 },
  fadeIn: 2.0,
  fadeOut: 6.0,
  tailSec: 6.0,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
