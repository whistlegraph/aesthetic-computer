// spiralbaba.mjs — a hypnotic, endlessly-climbing riff on the marimbaba
// lullaby, built as a CONTINUOUS TRANSPOSITION SPIRAL.
//
// Direction: A minor, kelon-led, ~58 BPM. The whistlegraph motif climbs by a
// constant interval on every repetition — a Shepard-tone illusion in marimba:
// each pass starts a fixed step higher than the last, while voices fade IN at
// the bottom and fade OUT at the top so the ear never finds the seam. The
// spiral rises and rises (forever, it seems), reaches a hovering apex, then
// UNWINDS — the same constant step, now descending — and finally settles home
// on A.
//
// DEVELOPMENT STRATEGY — CONTINUOUS TRANSPOSITION SPIRAL (Shepard-ish):
//  - One short "spiral cell" (a folded hush→twinkle contour) is the seed.
//  - Each repetition is transposed by a CONSTANT interval (STEP = +2 semis,
//    a whole tone) — pass k sits at k*STEP above the seed.
//  - To sustain the endless-rise illusion, every pass is voiced as a STACK of
//    octave copies (a Shepard chord). A slow triangular spectral window
//    weights each octave by gain: octaves near the window center are loud,
//    octaves at the extremes are near-silent. As the cell climbs, copies
//    crossing the top edge fade OUT while fresh copies fade IN at the bottom —
//    so the perceived register stays put while the pitch class keeps rising.
//  - The bass/healing root also spirals (a slow circle-of-A drone) but folds
//    octave so it never actually leaves the low register.
//  - At the apex the spiral HOVERS (a held shimmer), then the same engine runs
//    with STEP negated: the tune unwinds, descending endlessly, until it lands
//    on home A and the room exhales.
//
// Recognizable thread: the descending hush sigh opens it (close, legible), the
// twinkle climb is the spiral's DNA, and the final cadence is the marimbaba
// sleep settle, home on A.
//
// Run:  node variations/spiralbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 58;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT; // 3/4, like the marimbaba

// ── A natural-minor scale-fold (root pc = 9, A). Snap melodic voices. ───────
const ROOT = 9; // A
const MINOR = [0, 2, 3, 5, 7, 8, 10]; // natural minor
function snap(midi) {
  const pc = ((midi % 12) + 12) % 12;
  const rel = ((pc - ROOT) % 12 + 12) % 12;
  let best = MINOR[0], bestD = 99;
  for (const s of MINOR) {
    const d = Math.min(Math.abs(s - rel), 12 - Math.abs(s - rel));
    if (d < bestD) { bestD = d; best = s; }
  }
  const base = midi - rel;
  let cand = base + best;
  if (cand - midi > 6) cand -= 12;
  if (midi - cand > 6) cand += 12;
  return cand;
}

const LEAD = "kelon";

// ── the SPIRAL SEED CELL — a folded hush→twinkle contour, in A-minor land.
//    The marimbaba MOTIFS are written in F; transpose +4 to lift toward A and
//    snap into the minor mode. We keep this as [pitchClass-ish midi, beats]
//    pairs and let the spiral engine octave-stack them. Each note carries a
//    contour STEP (semitones above the cell's own root) so we can rebuild the
//    chromatic class freely at any spiral height. ──────────────────────────
function cell(motif) {
  return MOTIFS[motif].map(([name, beats]) => [snap(m(name) + 4), beats]);
}

// The seed: a compact, hypnotic rise — three steps up then a gentle fall,
// looped feeling. Built from hush (sigh) + the head of twinkle (climb), so the
// whistlegraph thread is audible. We reduce to a single octave of "pitch
// content" (the engine supplies the octaves).
const SEED = (() => {
  const h = cell("hush");      // C5 A4 F4 F4  → folded to A-minor sigh
  const t = cell("twinkle");   // F5 A5 C6 A5 G5 → climbing wave head
  // a 6-note hypnotic loop: down-sigh then up-climb, all in beats of 1–1.5
  return [
    [h[0][0], 1], [h[1][0], 1], [h[2][0], 1],   // sigh down
    [t[1][0], 1], [t[2][0], 1], [t[4][0], 1.5], // climb up + linger
  ];
})();

// ── SHEPARD SPECTRAL WINDOW ─────────────────────────────────────────────────
// Given a midi pitch, weight by how close it is to the window center. A
// triangular window over [lo, hi] in midi: peak gain at center, 0 at edges.
// This is what fades octaves in at the bottom and out at the top as the
// spiral climbs — the engine of the endless-rise illusion.
const WIN_LO = m("A2");   // 45
const WIN_HI = m("A6");   // 93
const WIN_CTR = (WIN_LO + WIN_HI) / 2;
const WIN_HALF = (WIN_HI - WIN_LO) / 2;
function shepardWeight(midi) {
  const d = Math.abs(midi - WIN_CTR) / WIN_HALF; // 0 center .. 1 edge
  if (d >= 1) return 0;
  // raised-cosine for a smoother, dreamier crossfade than a hard triangle
  return 0.5 + 0.5 * Math.cos(Math.PI * d);
}

// ── lay one SPIRAL PASS: the seed cell transposed by `transpose` semitones,
//    voiced as a stack of octave copies, each weighted by the Shepard window.
//    Octaves whose weight is ~0 are skipped (silent edges). Returns nothing;
//    pushes events. The perceived register stays centered while the pitch
//    class spirals. ───────────────────────────────────────────────────────
function spiralPass(out, { startBar, transpose, voice = LEAD, gainMul = 1, panBase = 0, decayMul = 1.7, octaves = [-2, -1, 0, 1, 2] }) {
  let beat = 0;
  for (let i = 0; i < SEED.length; i++) {
    const [seedMidi, beats] = SEED[i];
    const base = snap(seedMidi + transpose);
    const t = startBar * BAR + beat * BEAT;
    // pan drifts gently with position in the cell so the spiral feels like it
    // turns in space as it climbs.
    const pan = Math.max(-0.5, Math.min(0.5, panBase + (i / SEED.length - 0.5) * 0.5));
    for (const oct of octaves) {
      const midi = base + oct * 12;
      const w = shepardWeight(midi);
      if (w < 0.06) continue; // skip the faded-out edges
      const gain = 0.5 * w * gainMul;
      if (gain < 0.025) continue;
      out.push({
        preset: voice,
        startSec: t,
        midi,
        durSec: beats * BEAT,
        gain,
        decayMul: (DECAY[voice] ?? 1) * decayMul,
        pan,
      });
    }
    beat += beats;
  }
}

// ── a low, octave-folded root drone that follows the spiral's harmonic
//    center but never leaves the low register (keeps the bottom clear). ──────
function root(out, { startBar, transpose, lenBars = 1, gain = 0.4 }) {
  // fold the spiral root into the A1..A2 octave so it spirals in pitch-class
  // while staying anchored low.
  let r = snap(m("A2") + ((transpose % 12) + 12) % 12);
  while (r > m("A2")) r -= 12;
  if (r < m("A1")) r += 12;
  out.push({
    preset: "bass",
    startSec: startBar * BAR,
    midi: r,
    durSec: lenBars * BAR,
    gain,
    decayMul: DECAY.bass * 1.4,
    pan: 0,
  });
}

// ── a vibraphone_off shimmer pad (A-minor triad, octave-folded into a fixed
//    mid register) breathing under a span — the haze the spiral turns inside. ─
function pad(out, { startBar, lenBars, transpose, gain = 0.12 }) {
  // a minor triad rooted at the (folded) spiral root, kept in one register so
  // it reads as a steady haze, not a moving chord.
  const r = snap(m("A4") + (((transpose % 12) + 12) % 12));
  const triad = [r, snap(r + 3), snap(r + 7)];
  for (let i = 0; i < triad.length; i++) {
    let v = triad[i];
    while (v > m("C5")) v -= 12; // keep the pad mid, not bright
    out.push({
      preset: "vibraphone_off",
      startSec: startBar * BAR,
      midi: v,
      durSec: lenBars * BAR + BEAT,
      gain,
      decayMul: 2.0,
      pan: i === 0 ? -0.2 : i === 1 ? 0.0 : 0.2,
    });
  }
}

// ── a single high "spiral glint" sparkle (glockenspiel / kalimba). ──────────
function glint(out, { startBar, beat, midi, beats = 1.5, voice = "glockenspiel", gain = 0.1, pan = 0.3 }) {
  out.push({
    preset: voice,
    startSec: startBar * BAR + beat * BEAT,
    midi: snap(midi),
    durSec: beats * BEAT,
    gain,
    decayMul: (DECAY[voice] ?? 1) * 1.7,
    pan,
  });
}

const events = [];

// ════════════════════════════════════════════════════════════════════════
//  ARC: legible hush → endless ascent spiral → hovering apex → endless
//       descent (unwind) → settle home on A.
//  Each pass is 3 bars (one cell = 6.5 beats ≈ a little over 2 bars; we give
//  it a 3-bar slot so passes overlap-ring and breathe). STEP = whole tone.
// ════════════════════════════════════════════════════════════════════════

const STEP = 2;       // constant spiral interval — a whole tone per pass
const PASS_BARS = 3;  // bars per spiral pass

// ── INTRO (bars 0–2): the hush sigh, CLOSE and legible, no spiral yet — so
//    the marimbaba thread registers before the illusion begins. ─────────────
{
  const h = cell("hush");
  let beat = 0;
  for (const [midi, beats] of h) {
    events.push({
      preset: LEAD, startSec: 0 * BAR + beat * BEAT, midi: snap(midi),
      durSec: beats * BEAT, gain: 0.5, decayMul: DECAY[LEAD] * 1.8, pan: -0.05,
    });
    beat += beats;
  }
  root(events, { startBar: 0, transpose: 0, lenBars: 2, gain: 0.4 });
  pad(events, { startBar: 0, lenBars: 3, transpose: 0, gain: 0.1 });
  glint(events, { startBar: 2, beat: 1.2, midi: m("E6"), beats: 1.5, gain: 0.08, pan: 0.3 });
}

// ── ASCENT SPIRAL (bars 3 …): the seed climbs by STEP every pass. The
//    Shepard window keeps the perceived register steady while the pitch class
//    rises endlessly. Gain swells slightly toward the apex. ─────────────────
const ASCENT_PASSES = 6; // 6 * whole tone = ascending through ~an octave of class
let bar = 3;
for (let k = 0; k < ASCENT_PASSES; k++) {
  const transpose = k * STEP;
  // gain breathes up toward the apex, then the apex pass is the fullest.
  const gainMul = 0.85 + 0.15 * (k / (ASCENT_PASSES - 1));
  spiralPass(events, {
    startBar: bar,
    transpose,
    voice: LEAD,
    gainMul,
    panBase: (k % 2 === 0 ? -0.08 : 0.08), // alternate the turn direction
    decayMul: 1.7,
  });
  // the spiraling low root (octave-folded) under each pass
  root(events, { startBar: bar, transpose, lenBars: 2, gain: 0.36 });
  if (k % 2 === 0) {
    root(events, { startBar: bar + 1, transpose: transpose + STEP, lenBars: 1, gain: 0.3 });
  }
  // haze pad refreshed every couple passes
  if (k % 2 === 0) pad(events, { startBar: bar, lenBars: 2, transpose, gain: 0.11 });
  // rising glints — a kalimba/glock sparkle that itself climbs by STEP,
  // reinforcing the spiral overhead. Octave-fold the glint into a fixed-ish
  // high band so it too "rises forever".
  let g = m("A5") + transpose;
  while (g > m("C6")) g -= 12; // keep glints in a steady high band
  glint(events, {
    startBar: bar, beat: 2.0, midi: g + 12,
    beats: 1.5, voice: k % 3 === 0 ? "glockenspiel" : "kalimba",
    gain: 0.1, pan: k % 2 === 0 ? 0.32 : -0.28,
  });
  bar += PASS_BARS;
}

// ── APEX HOVER (≈ 2 bars): the spiral stops climbing and HOVERS — a held
//    Shepard shimmer at the top of the ascent, the tune suspended. ──────────
const apexT = ASCENT_PASSES * STEP;
{
  // sustain the seed's top notes as a shimmering chord across octaves
  const topMidi = snap(SEED[3][0] + apexT - STEP); // the climb peak class
  for (const oct of [-2, -1, 0, 1, 2]) {
    const mi = topMidi + oct * 12;
    const w = shepardWeight(mi);
    if (w < 0.06) continue;
    events.push({
      preset: "vibraphone",
      startSec: bar * BAR,
      midi: mi,
      durSec: 2 * BAR + BEAT,
      gain: 0.4 * w,
      decayMul: 1.6,
      pan: oct % 2 === 0 ? -0.18 : 0.18,
    });
  }
  root(events, { startBar: bar, transpose: apexT, lenBars: 2, gain: 0.34 });
  pad(events, { startBar: bar, lenBars: 2, transpose: apexT, gain: 0.12 });
  glint(events, { startBar: bar, beat: 1.0, midi: m("E6"), beats: 2.5, gain: 0.1, pan: 0.34 });
  glint(events, { startBar: bar + 1, beat: 0.5, midi: m("A5"), beats: 2, voice: "kalimba", gain: 0.09, pan: -0.26 });
  bar += 2;
}

// ── UNWIND / DESCENT SPIRAL (bars …): the same engine, STEP negated. The
//    tune unwinds, the pitch class spiraling DOWN while the Shepard window
//    holds the register — an endless fall back toward home. Gain eases off. ──
const DESCENT_PASSES = 5;
for (let k = 0; k < DESCENT_PASSES; k++) {
  const transpose = apexT - (k + 1) * STEP;
  const gainMul = 0.85 - 0.18 * (k / (DESCENT_PASSES - 1)); // settling
  spiralPass(events, {
    startBar: bar,
    transpose,
    voice: LEAD,
    gainMul,
    panBase: (k % 2 === 0 ? 0.08 : -0.08),
    decayMul: 1.8,
  });
  root(events, { startBar: bar, transpose, lenBars: 2, gain: 0.34 });
  if (k % 2 === 1) pad(events, { startBar: bar, lenBars: 2, transpose, gain: 0.1 });
  // falling glints
  let g = m("A5") + transpose;
  while (g < m("A4")) g += 12;
  while (g > m("C6")) g -= 12;
  glint(events, {
    startBar: bar, beat: 1.5, midi: g + 12,
    beats: 1.5, voice: k % 3 === 0 ? "glockenspiel" : "kalimba",
    gain: 0.085, pan: k % 2 === 0 ? -0.3 : 0.3,
  });
  bar += PASS_BARS;
}

// ── HOME SETTLE (final bars): the marimbaba sleep cadence, transpose 0 — the
//    spiral lands home on A, recognizable and resolved. No octave stack now:
//    a single, close, low register. The room exhales. ───────────────────────
{
  const sleep = cell("sleep");
  let beat = 0;
  for (const [midi, beats] of sleep) {
    let v = snap(midi);
    while (v > m("C5")) v -= 12; // bring the cadence down, intimate
    events.push({
      preset: LEAD,
      startSec: bar * BAR + beat * BEAT,
      midi: v,
      durSec: beats * BEAT,
      gain: 0.46,
      decayMul: DECAY[LEAD] * 1.9,
      pan: -0.04,
    });
    beat += beats;
  }
  // a final faint hush echo — the thread closing the frame, home on A
  const h = cell("hush");
  let b2 = 0;
  const echoBar = bar + 3;
  for (const [midi, beats] of h) {
    let v = snap(midi);
    while (v > m("A4")) v -= 12;
    events.push({
      preset: LEAD,
      startSec: echoBar * BAR + b2 * BEAT,
      midi: v,
      durSec: beats * BEAT,
      gain: 0.38,
      decayMul: DECAY[LEAD] * 2.0,
      pan: 0.04,
    });
    b2 += beats;
  }
  root(events, { startBar: bar, transpose: 0, lenBars: 2, gain: 0.34 });
  root(events, { startBar: bar + 2, transpose: 0, lenBars: 2, gain: 0.3 });
  root(events, { startBar: echoBar, transpose: 0, lenBars: 2, gain: 0.28 });
  pad(events, { startBar: bar, lenBars: 6, transpose: 0, gain: 0.12 }); // long home haze
  // one last very high, very soft glint fading over the home octave
  glint(events, { startBar: bar + 1, beat: 1.0, midi: m("A6"), beats: 4, gain: 0.07, pan: 0.3 });
  glint(events, { startBar: echoBar, beat: 1.5, midi: m("E6"), beats: 3, voice: "kalimba", gain: 0.07, pan: -0.24 });
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "spiralbaba",
  here: HERE,
  title: "spiralbaba",
  reverb: { wet: 0.4, decay: 0.88, damp: 0.36 }, // deep, hypnotic spiral-room
  healingHz: 852,   // Solfeggio LA / "intuition" — a high A-ish glow over A minor
  fadeIn: 1.6,
  fadeOut: 6.0,
  tailSec: 6.0,
  peak: 0.82,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
