// prismbaba.mjs — the marimbaba hush/twinkle phrase REFRACTED THROUGH MODES,
// a prism held up to the lullaby so the same light bends a new color each pass.
//
// Direction: E modal home, kelon lead, ~58 BPM. The tune keeps ONE fixed
// scale-degree contour (the whistlegraph hush → twinkle → wow → baba → sleep
// shapes, reduced to degrees) and re-illuminates it by swapping the MODE
// underneath: Ionian → Lydian → Dorian → Aeolian → back to Ionian. Same steps
// of the staircase, repainted — a TRUE mode remap, never a transposition, so
// degree 3 brightens to major and dims to minor, degree 4 lifts to the Lydian
// #4 and settles back, degree 6/7 darken into Aeolian, all while the melodic
// contour stays instantly recognizable.
//
// DEVELOPMENT STRATEGY — MODAL REFRACTION:
//  - Phrases are stored as SCALE DEGREES (1-based) with octave marks, not notes.
//  - deg→midi(mode, root) maps a degree through whichever 7-note mode is lit.
//  - Each pass restates the same contour through a new mode; the listener hears
//    "the same tune" wearing a new color of light. The arc:
//      P0 Ionian  (warm noon)   — hush, stated plainly so we learn the contour
//      P1 Lydian  (bright dawn) — twinkle, the #4 raising the roof
//      P2 Dorian  (cool dusk)   — wow + baba, minor-but-hopeful, the b3 cooling
//      P3 Aeolian (deep night)  — twinkle inverted, the b6/b7 darkest
//      P4 Ionian  (return)      — sleep cadence comes home, the prism set down
//  - A drone pedal on E threads every pass so the modes are heard as colors of
//    ONE home, not as key changes — that pedal IS the prism's white light.
//
// Run:  node variations/prismbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 58;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT; // 3/4 — the marimbaba waltz lilt

const LEAD = "kelon";
const ROOT = m("E4"); // E modal home

// ── THE MODES (semitone offsets for degrees 1..7) ──────────────────────────
// Same degree numbers, different light. This is the prism: one contour, many
// colorings. Degree d (1-based) → MODE[mode][d-1] semitones above the root.
const MODES = {
  ionian:  [0, 2, 4, 5, 7, 9, 11], // major — warm noon
  lydian:  [0, 2, 4, 6, 7, 9, 11], // #4 — bright dawn (the raised roof)
  dorian:  [0, 2, 3, 5, 7, 9, 10], // b3 b7 — cool, minor-hopeful dusk
  aeolian: [0, 2, 3, 5, 7, 8, 10], // b3 b6 b7 — deep night
};

// ── degree cell → midi cell, refracted through a mode ───────────────────────
// A degree is written "d" or "dᵒ" via a {deg, oct} record; we store them as
// [degree, octaveShift, beats]. octaveShift is in octaves relative to ROOT's
// octave. refract() bends the whole cell through the lit mode.
function refract(cell, mode) {
  const tab = MODES[mode];
  return cell.map(([deg, oct, beats]) => {
    const idx = ((deg - 1) % 7 + 7) % 7;
    const extraOct = Math.floor((deg - 1) / 7); // degrees > 7 wrap up octaves
    return [ROOT + tab[idx] + 12 * (oct + extraOct), beats];
  });
}

// ── reduce the marimbaba MOTIFS to mode-free DEGREE contours (in F major, the
//    motif key). We read each motif note's scale degree relative to F so the
//    whistlegraph shapes survive as pure contour, ready to be re-lit on E. ────
const F_ROOT = m("F4");
const F_MAJOR = [0, 2, 4, 5, 7, 9, 11];
function noteToDegree(midi) {
  // nearest degree of F major; returns [degree(1-based, can exceed 7), oct]
  const rel = midi - F_ROOT;
  const pc = ((rel % 12) + 12) % 12;
  let bestI = 0, bestD = 99;
  for (let i = 0; i < 7; i++) {
    const d = Math.min(Math.abs(F_MAJOR[i] - pc), 12 - Math.abs(F_MAJOR[i] - pc));
    if (d < bestD) { bestD = d; bestI = i; }
  }
  const oct = Math.round((rel - F_MAJOR[bestI]) / 12);
  return [bestI + 1, oct]; // degree (1..7), octave shift relative to F4
}
function contourOf(motif) {
  return MOTIFS[motif].map(([name, beats]) => {
    const [deg, oct] = noteToDegree(m(name));
    return [deg, oct, beats];
  });
}

// the five whistlegraph contours, stored as pure scale degrees on E now:
const C = {
  hush:    contourOf("hush"),
  twinkle: contourOf("twinkle"),
  flyHigh: contourOf("flyHigh"),
  wow:     contourOf("wow"),
  baba:    contourOf("baba"),
  sleep:   contourOf("sleep"),
};

// ── contour transforms (operate on DEGREE cells, before refraction) ─────────
// invert a degree-contour around its first degree (mirror, stays diatonic).
function invert(cell) {
  const [d0, o0] = [cell[0][0], cell[0][1]];
  const abs0 = (d0 - 1) + 7 * o0;
  return cell.map(([deg, oct, beats]) => {
    const abs = (deg - 1) + 7 * oct;
    const mir = 2 * abs0 - abs;
    return [(((mir % 7) + 7) % 7) + 1, Math.floor(mir / 7), beats];
  });
}
// retrograde — same contour, walked backwards.
function retro(cell) { return [...cell].reverse(); }
// sequence — shift the whole contour up/down by N diatonic degrees.
function seq(cell, steps) {
  return cell.map(([deg, oct, beats]) => {
    const abs = (deg - 1) + 7 * oct + steps;
    return [(((abs % 7) + 7) % 7) + 1, Math.floor(abs / 7), beats];
  });
}
// gently lengthen final note for a breathing cadence.
function breathe(cell, extra = 1) {
  const c = cell.map((x) => [...x]);
  c[c.length - 1][2] += extra;
  return c;
}

// ── lay a refracted midi cell into events ──────────────────────────────────
function lay(out, midiCell, { startBar, beat0 = 0, voice = LEAD, gain = 0.5, decayMul = 1.5, panSpread = 0.28 } = {}) {
  let beat = beat0;
  for (const [midi, beats] of midiCell) {
    const reg = (midi - ROOT) / 24; // register → pan, the prism fans in space
    const pan = Math.max(-0.45, Math.min(0.45, reg * panSpread));
    out.push({
      preset: voice,
      startSec: startBar * BAR + beat * BEAT,
      midi,
      durSec: beats * BEAT,
      gain,
      decayMul: (DECAY[voice] ?? 1) * decayMul,
      pan,
    });
    beat += beats;
  }
  return beat;
}

// ── E pedal drone (the prism's white light) — vibraphone_off, soft + long. ──
function pedal(out, startBar, lenBars, mode, gain = 0.13) {
  const tab = MODES[mode];
  // root + 5th always; the colored 3rd of the lit mode sits on top so the
  // pedal itself faintly carries the pass's color.
  const triad = [ROOT - 12, ROOT - 12 + tab[4], ROOT + tab[2]];
  const pans = [-0.2, 0.0, 0.22];
  for (let i = 0; i < triad.length; i++) {
    out.push({
      preset: "vibraphone_off",
      startSec: startBar * BAR,
      midi: triad[i],
      durSec: lenBars * BAR + BEAT,
      gain,
      decayMul: 2.0,
      pan: pans[i],
    });
  }
}

// ── soft bass root, kept low + clear ───────────────────────────────────────
function bass(out, startBar, midi, lenBars = 2, gain = 0.38) {
  out.push({
    preset: "bass",
    startSec: startBar * BAR,
    midi,
    durSec: lenBars * BAR,
    gain,
    decayMul: DECAY.bass * 1.4,
    pan: 0,
  });
}

// ── a single sparkle in the lit mode (glockenspiel/kalimba prism glints) ────
function glint(out, { startBar, beat, deg, oct, beats, mode, voice = "glockenspiel", gain = 0.11, pan = 0.3 }) {
  const tab = MODES[mode];
  const idx = ((deg - 1) % 7 + 7) % 7;
  const extraOct = Math.floor((deg - 1) / 7);
  out.push({
    preset: voice,
    startSec: startBar * BAR + beat * BEAT,
    midi: ROOT + tab[idx] + 12 * (oct + extraOct),
    durSec: beats * BEAT,
    gain,
    decayMul: (DECAY[voice] ?? 1) * 1.7,
    pan,
  });
}

const events = [];

// ════════════════════════════════════════════════════════════════════════
//  ARC: one contour, refracted Ionian → Lydian → Dorian → Aeolian → Ionian.
// ════════════════════════════════════════════════════════════════════════

// ── PASS 0 · IONIAN (bars 0–5): warm noon. Hush stated plainly so the ear
//    learns the contour before the prism turns. ────────────────────────────
{
  const mode = "ionian";
  lay(events, refract(C.hush, mode), { startBar: 0, gain: 0.52, decayMul: 1.7 });
  // a soft answering hush, sequenced up a 3rd (degrees), same mode
  lay(events, refract(seq(C.hush, 2), mode), { startBar: 2, gain: 0.42, decayMul: 1.7 });
  lay(events, refract(breathe(C.hush, 1), mode), { startBar: 4, gain: 0.46, decayMul: 1.8 });
  bass(events, 0, ROOT - 24, 2, 0.4); bass(events, 2, ROOT - 24, 2, 0.4); bass(events, 4, ROOT - 24, 2, 0.38);
  pedal(events, 0, 6, mode, 0.12);
  glint(events, { startBar: 1, beat: 1.5, deg: 5, oct: 1, beats: 1.5, mode, gain: 0.09, pan: 0.3 });
}

// ── PASS 1 · LYDIAN (bars 6–11): bright dawn. The twinkle climbing-wave, now
//    the #4 lifting the roof — the same wave reaches a brighter ceiling. ────
{
  const mode = "lydian";
  lay(events, refract(C.twinkle, mode), { startBar: 6, gain: 0.5, panSpread: 0.36 });
  // answer: twinkle sequenced up a 4th (lands the #4 high → max shimmer)
  lay(events, refract(seq(C.twinkle, 3), mode), { startBar: 8, gain: 0.44, panSpread: 0.4 });
  // flyHigh fragment — the butterfly catching the Lydian light
  lay(events, refract(C.flyHigh, mode), { startBar: 10, gain: 0.44, panSpread: 0.42 });
  bass(events, 6, ROOT - 24, 2, 0.35); bass(events, 8, ROOT - 12 + MODES[mode][4], 2, 0.34); bass(events, 10, ROOT - 24, 2, 0.34);
  pedal(events, 6, 6, mode, 0.12);
  glint(events, { startBar: 7, beat: 1.0, deg: 4, oct: 2, beats: 1.5, mode, voice: "kalimba", gain: 0.12, pan: -0.26 }); // the #4 glinting
  glint(events, { startBar: 11, beat: 0.5, deg: 1, oct: 2, beats: 1.5, mode, gain: 0.11, pan: 0.34 });
}

// ── PASS 2 · DORIAN (bars 12–17): cool dusk. The wow wobble + baba slinky-bap
//    refracted minor-but-hopeful — the b3 cools the tune, the natural 6 keeps
//    it from going fully sad. The same wobble, a cloud crossing the sun. ─────
{
  const mode = "dorian";
  lay(events, refract(C.wow, mode), { startBar: 12, gain: 0.46, decayMul: 1.6, panSpread: 0.3 });
  // baba slinky-bap, refracted dorian
  lay(events, refract(C.baba, mode), { startBar: 14, gain: 0.44, decayMul: 1.5, panSpread: 0.34 });
  // a sequenced echo of the wow a step down — dusk deepening
  lay(events, refract(seq(C.wow, -1), mode), { startBar: 16, gain: 0.38, decayMul: 1.6, panSpread: 0.28 });
  bass(events, 12, ROOT - 24, 2, 0.34); bass(events, 14, ROOT - 24 + MODES[mode][3], 2, 0.34); bass(events, 16, ROOT - 24, 2, 0.34);
  pedal(events, 12, 6, mode, 0.12);
  glint(events, { startBar: 13, beat: 2.0, deg: 3, oct: 2, beats: 1.5, mode, gain: 0.1, pan: 0.3 });  // the cooled b3
  glint(events, { startBar: 15, beat: 1.5, deg: 6, oct: 1, beats: 1.5, mode, voice: "kalimba", gain: 0.11, pan: -0.28 });
}

// ── PASS 3 · AEOLIAN (bars 18–23): deep night, the darkest light. Twinkle
//    INVERTED (the wave folding back into itself) and refracted aeolian so the
//    b6/b7 pull it lowest — the same contour seen from the far side of the
//    prism, in shadow. ──────────────────────────────────────────────────────
{
  const mode = "aeolian";
  lay(events, refract(invert(C.twinkle), mode), { startBar: 18, gain: 0.44, decayMul: 1.6, panSpread: 0.3 });
  // retrograde hush, aeolian — the opening sigh walked backwards in the dark
  lay(events, refract(retro(C.hush), mode), { startBar: 20, gain: 0.4, decayMul: 1.7 });
  // wow once more, aeolian, lowest color, sequenced down — night's deepest point
  lay(events, refract(seq(C.wow, -2), mode), { startBar: 22, gain: 0.36, decayMul: 1.7, panSpread: 0.26 });
  bass(events, 18, ROOT - 24, 2, 0.34); bass(events, 20, ROOT - 24 + MODES[mode][5], 2, 0.32); bass(events, 22, ROOT - 24, 2, 0.32);
  pedal(events, 18, 6, mode, 0.12);
  glint(events, { startBar: 19, beat: 1.5, deg: 6, oct: 1, beats: 2, mode, gain: 0.09, pan: 0.32 });  // the b6 in shadow
  glint(events, { startBar: 23, beat: 1.0, deg: 7, oct: 1, beats: 2, mode, voice: "kalimba", gain: 0.09, pan: -0.24 });
}

// ── PASS 4 · IONIAN RETURN (bars 24–29): the prism set down. The sleep cadence
//    comes home to warm noon — the tune resolved, recognizable, settled. A
//    final close hush echo closes the frame. ───────────────────────────────
{
  const mode = "ionian";
  lay(events, refract(C.sleep, mode), { startBar: 24, gain: 0.48, decayMul: 1.9, panSpread: 0.18 });
  // a final, faint, close hush echo — the recognizable thread, last light.
  lay(events, refract(breathe(C.hush, 2), mode), { startBar: 27, gain: 0.42, decayMul: 2.0, panSpread: 0.16 });
  bass(events, 24, ROOT - 24, 2, 0.36); bass(events, 26, ROOT - 12 + MODES[mode][4], 2, 0.33); bass(events, 28, ROOT - 24, 2, 0.33);
  pedal(events, 24, 6, mode, 0.13); // long resolving E pedal — prism back to white
  glint(events, { startBar: 27, beat: 1.0, deg: 1, oct: 2, beats: 4, mode, gain: 0.08, pan: 0.3 });
  glint(events, { startBar: 28, beat: 1.5, deg: 5, oct: 1, beats: 3, mode, voice: "kalimba", gain: 0.09, pan: -0.24 });
}

const { mp3, durationSec } = renderLullaby(events, {
  name: "prismbaba",
  here: HERE,
  title: "prismbaba",
  reverb: { wet: 0.4, decay: 0.86, damp: 0.36 }, // glassy prism-room
  healingHz: 639, // Solfeggio FA — sits kindly under E modal home
  fadeIn: 1.4,
  fadeOut: 6.0,
  tailSec: 6.0,
  peak: 0.84,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
