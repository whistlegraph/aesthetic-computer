// musicboxbaba.mjs — a wind-up nursery music box that ACCELERATES.
//
// C major, ~74 BPM, high register. STRATEGY: ACCELERATING DIMINUTION.
// The box winds UP. The twinkle theme is first stated plainly in slow
// quarter notes on the warm kelon body. Then each restatement HALVES the
// note values — quarters → 8ths → 16ths → 32nd flurries — and grows turns
// and mordents, the tempo of the comb tightening, until a dizzying
// glockenspiel spiral whirls at the peak. Then the spring catches: a sudden
// ritard UNWINDS it back through 16ths and 8ths to slow single notes, the
// last tine clicking once as the mainspring runs out.
//
// The seed is marimbaba (F major, 3/4, ~56 BPM) — here remapped to C major
// and re-voiced as a wind-up box. The recognizable thread: the twinkle
// climbing wave and the hush/sleep descending sigh-cadence survive every
// diminution; only their SPEED and ornamentation transform.
//
// Run:  node variations/musicboxbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

// ── mode remap: fold any midi to the nearest pitch in C major ──────────────
const ROOT = 0; // C
const MAJOR = [0, 2, 4, 5, 7, 9, 11];
function snap(midi) {
  const pc = ((midi - ROOT) % 12 + 12) % 12;
  let best = MAJOR[0], bestD = 99;
  for (const d of MAJOR) {
    const dist = Math.min(Math.abs(d - pc), 12 - Math.abs(d - pc));
    if (dist < bestD) { bestD = dist; best = d; }
  }
  return midi + (best - pc);
}
const snapName = (name, oct = 0) => snap(m(name) + oct * 12);

// next scale tone above/below within C major (for turns & mordents) ─────────
function scaleStep(midi, dir) {
  let x = midi + dir;
  for (let i = 0; i < 3; i++) { if (snap(x) === x) return x; x += dir; }
  return snap(x);
}

// ── timing ─────────────────────────────────────────────────────────────────
const BPM = 74;
const BEAT = 60 / BPM;

const ev = [];
let t = 0; // running cursor in seconds

const KELON_DEC = (DECAY.kelon ?? 1.3) * 1.5;
const GLOCK_DEC = 1.7;

// ── voices ─────────────────────────────────────────────────────────────────
// warm wooden box body
function kel(at, midi, dur, gain, pan = -0.08, decMul = KELON_DEC) {
  ev.push({ preset: "kelon", startSec: at, midi, durSec: dur, gain, decayMul: decMul, pan });
}
// bright comb tine
function tine(at, midi, gain = 0.24, dur = 0.4 * BEAT, pan = 0.28) {
  ev.push({ preset: "glockenspiel", startSec: at, midi, durSec: dur, gain, decayMul: GLOCK_DEC, pan });
}
// wound base
function root(name, gain = 0.32, dur = 3 * BEAT * 1.1, at = t) {
  ev.push({ preset: "bass", startSec: at, midi: m(name), durSec: dur, gain, decayMul: 1.7, pan: 0 });
}

// ── the recognizable thread: a single melodic line (scale degrees, C major) ─
// Drawn from MOTIFS.twinkle (climbing wave) + the hush sigh tail, flattened
// into ONE contour of note names. Every diminution re-renders THIS line at a
// different note-value, so the tune is always the same — only faster.
const THEME = ["C5", "E5", "G5", "C6", "A5", "G5", "E5", "G5",   // twinkle climb + curl
               "F5", "A5", "C6", "B5", "A5", "G5", "F5", "E5"];  // wave + sigh down
// cadence tag (the hush/sleep descending sigh — survives to the very end)
const CADENCE = ["G5", "F5", "E5", "C5"];

// ── ornament generators (turns / mordents) on a scale tone ─────────────────
// mordent: note, lower-neighbor, note  (three quick grace strikes)
function mordent(midi) {
  return [midi, scaleStep(midi, -1), midi];
}
// turn: upper-neighbor, note, lower-neighbor, note (four-note curl)
function turn(midi) {
  return [scaleStep(midi, 1), midi, scaleStep(midi, -1), midi];
}

// ── render one diminution pass of THEME ────────────────────────────────────
// noteVal = seconds per theme note. voice "kel" (slow) or "tine" (fast).
// ornament = none | "mordent" | "turn"  applied to a few accent notes.
// octShift moves the whole pass; gain shapes loudness.
function pass(notes, noteVal, { voice = "kel", ornament = null, octShift = 0,
                                gain = 0.4, accentEvery = 4, pan } = {}) {
  let cur = t;
  notes.forEach((name, i) => {
    const base = snapName(name, octShift);
    const accent = (i % accentEvery === 0);
    if (ornament && accent && noteVal < 0.55 * BEAT) {
      // squeeze the ornament into this note's slot
      const orn = ornament === "turn" ? turn(base) : mordent(base);
      const g = noteVal / orn.length;
      orn.forEach((mi, k) => {
        const at = cur + k * g;
        if (voice === "tine") tine(at, mi + 12, gain * (k === orn.length - 1 ? 1 : 0.7), Math.max(0.12, g * 1.2), pan);
        else kel(at, mi, Math.max(0.12, g * 1.4), gain * (k === orn.length - 1 ? 1 : 0.72), pan);
      });
    } else {
      if (voice === "tine") tine(cur, base + 12, gain * (accent ? 1 : 0.8), Math.max(0.14, noteVal * 1.2), pan);
      else kel(cur, base, noteVal * 1.25, gain * (accent ? 1 : 0.85), pan);
    }
    cur += noteVal;
  });
  return cur; // end time
}

// =====================================================================
// WIND-UP — the key turning: a fast fragile rising clockwork sparkle.
// =====================================================================
root("C2", 0.3, 3 * BEAT * 1.2);
["C5", "E5", "G5", "C6", "E6", "G6"].forEach((n, i) =>
  tine(t + i * 0.5 * BEAT, snapName(n, 1) - 12, 0.16 + i * 0.012));
t += 3 * BEAT;

// one more half-turn of the key, a touch faster
root("G2", 0.28, 2.2 * BEAT);
["G5", "B5", "D6", "G6"].forEach((n, i) =>
  tine(t + i * 0.4 * BEAT, snapName(n, 1) - 12, 0.16));
t += 2.2 * BEAT;

// =====================================================================
// STATEMENT — quarter notes. The theme stated plainly, slow & warm.
// kelon body, one tine ghost per phrase-head. This is the box at rest.
// =====================================================================
root("C2", 0.32, 8 * BEAT);
root("A2", 0.3, 8 * BEAT, t + 8 * BEAT);
t = pass(THEME, 1.0 * BEAT, { gain: 0.42, accentEvery: 8 });
// a lone answering tine at the cadence head
tine(t - 8 * BEAT, snapName("C6"), 0.18);

// gentle plain cadence (slow sigh)
root("G2", 0.3, 4 * BEAT);
t = pass(CADENCE, 1.0 * BEAT, { gain: 0.4, accentEvery: 99 });

// STATEMENT (repeat) — same plain quarters, an octave-down warm answer so
// the theme is fully memorable before the winding begins.
root("F2", 0.3, 8 * BEAT);
root("C2", 0.3, 8 * BEAT, t + 8 * BEAT);
t = pass(THEME, 1.0 * BEAT, { gain: 0.4, octShift: -1, accentEvery: 8, pan: 0.06 });
tine(t - 6 * BEAT, snapName("G6") - 12, 0.13);
root("G2", 0.3, 4 * BEAT);
t = pass(CADENCE, 1.0 * BEAT, { gain: 0.38, octShift: -1, accentEvery: 99, pan: 0.06 });

// =====================================================================
// DIMINUTION 1 — eighth notes. Twice as fast. First mordents bloom.
// kelon still leads but tines start doubling the accents.
// =====================================================================
root("C2", 0.3, 8 * BEAT);
root("F2", 0.3, 8 * BEAT, t + 4 * BEAT);
const d1End = pass(THEME, 0.5 * BEAT, { gain: 0.4, ornament: "mordent", accentEvery: 4 });
// tine ghost doubling the climb, an octave up
let gt = t;
["C5", "E5", "G5", "C6"].forEach((n, i) => { tine(gt + i * BEAT, snapName(n, 1), 0.13); });
t = d1End;

// =====================================================================
// DIMINUTION 2 — sixteenth notes. Turns now. Voice splits: kelon body +
// glockenspiel tine line interlocking like the comb teeth speeding up.
// =====================================================================
root("C2", 0.28, 8 * BEAT);
root("G2", 0.28, 8 * BEAT, t + 4 * BEAT);
const startD2 = t;
// kelon takes the line at 16ths with turns on accents
const d2End = pass(THEME, 0.25 * BEAT, { gain: 0.36, ornament: "turn", accentEvery: 4 });
// glockenspiel shadows it a 16th late, octave up, sparser (offset canon)
let ct = startD2 + 0.125 * BEAT;
THEME.forEach((name, i) => {
  if (i % 2 === 0) tine(ct, snapName(name, 1), 0.12, 0.22 * BEAT);
  ct += 0.25 * BEAT;
});
t = d2End;

// =====================================================================
// PEAK — 32nd-note FLURRY + a dizzying glockenspiel spiral.
// The comb is whirring. THEME compressed into 32nds on tines, octave up,
// while kelon holds a shimmering pedal and a spiraling arpeggio cascades.
// =====================================================================
root("C3", 0.3, 6 * BEAT);
// shimmering held kelon pedal chord (C major) under the whirl
kel(t, snapName("E4"), 6 * BEAT, 0.22, -0.1, KELON_DEC * 1.3);
kel(t, snapName("G4"), 6 * BEAT, 0.2, 0.0, KELON_DEC * 1.3);
kel(t, snapName("C5"), 6 * BEAT, 0.18, 0.1, KELON_DEC * 1.3);

const peakStart = t;
// THEME at 32nds on the tines — the dizzy flurry, octave up
t = pass(THEME, 0.125 * BEAT, { voice: "tine", gain: 0.2, ornament: "turn", accentEvery: 4 });
// THEME again, retrograde, still 32nds — the spiral doubles back
const RETRO = [...THEME].reverse();
t = pass(RETRO, 0.125 * BEAT, { voice: "tine", gain: 0.19, octShift: 0, accentEvery: 4 });

// the glockenspiel SPIRAL: a continuous up-down arpeggio cascade over the
// flurry — C-major across three octaves, accelerating then settling.
const spiralPitches = ["C5","E5","G5","B5","C6","E6","G6","E6","C6","B5","G5","E5","C5","E5","G5","C6"];
let sp = peakStart;
let step = 0.16 * BEAT;
spiralPitches.forEach((n, i) => {
  tine(sp, snapName(n, 1) - (i > 7 ? 12 : 0), 0.14, 0.2 * BEAT, i % 2 ? 0.34 : -0.34);
  sp += step;
  step *= 0.97; // wind tighter
});

// a top-of-the-spiral shimmer burst — the highest, fastest tines
let bs = peakStart + 3 * BEAT;
["G6","C6","E6","G6","C6","A5","G6","E6"].forEach((n, i) => {
  tine(bs, snapName(n, 1) - 12, 0.13 - i * 0.004, 0.18 * BEAT, i % 2 ? 0.3 : -0.3);
  bs += 0.09 * BEAT;
});

// =====================================================================
// UNWIND — the spring catches. Sudden ritard, note-values DOUBLING back:
// 16ths → 8ths → quarters → slow single notes. Ornaments fall away.
// The hush/sleep cadence returns, slower each time, as it runs down.
// =====================================================================
// (re-render shrinking fragments of THEME, each pass slower & quieter)

// unwind A — 16ths, descending fragment, ornament thinning
root("C2", 0.28, 4 * BEAT);
const unA = ["C6","B5","A5","G5","F5","E5","G5","E5"];
t = pass(unA, 0.25 * BEAT, { gain: 0.3, ornament: "mordent", accentEvery: 8 });

// unwind B — 8ths, slowing, plainer
root("F2", 0.26, 4 * BEAT);
const unB = ["A5","G5","F5","E5","C5","E5"];
t = pass(unB, 0.5 * BEAT * 1.12, { gain: 0.3, accentEvery: 99 });

// unwind C — quarters, the cadence sigh, plain
root("G2", 0.26, 5 * BEAT);
t = pass(CADENCE, 1.0 * BEAT * 1.25, { gain: 0.3, accentEvery: 99 });
// faint tine echoes trailing behind, slowing
tine(t - 3.0 * BEAT, snapName("G6") - 12, 0.1);
tine(t - 1.6 * BEAT, snapName("E6") - 12, 0.09);

// unwind D — slow single notes, the spring nearly stopped (deep ritard)
root("C2", 0.26, 6 * BEAT);
kel(t, snapName("C5"), 2.6 * BEAT, 0.3, -0.06, KELON_DEC * 1.3); t += 2.0 * BEAT;
kel(t, snapName("A4"), 2.8 * BEAT, 0.28, 0.02, KELON_DEC * 1.35); t += 2.4 * BEAT;
tine(t - 0.5 * BEAT, snapName("A5"), 0.1);

// final lone resolve — the mainspring stops
root("C2", 0.24, 6 * BEAT);
kel(t, snapName("C4"), 5 * BEAT, 0.3, 0.0, KELON_DEC * 1.5);
kel(t, snapName("E4"), 5 * BEAT, 0.22, 0.06, KELON_DEC * 1.5);
kel(t, snapName("G4"), 5 * BEAT, 0.18, -0.06, KELON_DEC * 1.5);
tine(t + 0.2 * BEAT, snapName("C6"), 0.12);
// the last click of the comb, off the grid, very soft
tine(t + 2.4 * BEAT, snapName("E5"), 0.09, 0.6 * BEAT, 0.2);
t += 5 * BEAT;

// ── render ─────────────────────────────────────────────────────────────────
const { mp3, durationSec } = renderLullaby(ev, {
  name: "musicboxbaba",
  here: HERE,
  title: "musicboxbaba",
  reverb: { wet: 0.34, decay: 0.84, damp: 0.34 },
  fadeIn: 0.5,
  fadeOut: 4.5,
  tailSec: 5.0,
  peak: 0.82,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
