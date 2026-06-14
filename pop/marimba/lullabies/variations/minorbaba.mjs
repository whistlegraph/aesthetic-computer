// minorbaba.mjs — the marimbaba hush-theme put through the four mirror forms.
//
// Identity kept: F natural minor, rosewood lead, ~52 BPM lullaby feel, the soft
// Fm vibraphone pad, kalimba droplets. But the MELODY now develops by canon-
// table transformation: the hush "descending sigh" theme is stated as a PRIME,
// then answered by its INVERSION (intervals mirrored — the sigh climbs instead
// of falls), its RETROGRADE (the phrase played backwards), and the inverted-
// retrograde, the four forms calling to one another as the bass sinks F → Eb →
// Db → C. At the darkest point (the C / lowered area) the contour literally
// turns upside-down: the inversion takes the lead and the sigh becomes a rise.
// The sleep cadence returns at the end as a recognizable thread, mirrored once
// more before it finally settles to the true descending hush.
//
// Run:  node variations/minorbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 52;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT;

// ── F natural minor scale machinery ─────────────────────────────────────────
// We work in scale-degree space so inversion mirrors *diatonic* intervals (the
// sigh stays in-mode when flipped, no chromatic mush). Degree 0 = F.
const ROOT = m("F4");                 // 65 — degree 0 anchor
const STEPS = [0, 2, 3, 5, 7, 8, 10]; // F natural minor semitone offsets
const PCS = STEPS;                     // pitch-classes of the mode

// midi -> diatonic degree index (… can go negative / >6 across octaves)
function toDegree(midi) {
  const rel = midi - ROOT;
  const oct = Math.floor(rel / 12);
  const pc = ((rel % 12) + 12) % 12;
  // nearest scale pc
  let best = 0, bestD = 99;
  for (let i = 0; i < PCS.length; i++) {
    const d = Math.abs(PCS[i] - pc);
    if (d < bestD) { bestD = d; best = i; }
  }
  return oct * 7 + best;
}
// diatonic degree index -> midi
function toMidi(deg) {
  const oct = Math.floor(deg / 7);
  const idx = ((deg % 7) + 7) % 7;
  return ROOT + oct * 12 + STEPS[idx];
}

// ── theme as [noteName, beats] cells, snapped into degree space ──────────────
// PRIME = the hush sigh, the recognizable DNA (C5 A4 F4 / settle).
const cell = (pairs) => pairs.map(([n, b]) => [toDegree(m(n)), b]);
const HUSH = cell(MOTIFS.hush);   // [[deg(C5),1],[deg(A4),1],[deg(F4),1],[deg(F4),3]]
const SLEEP = cell(MOTIFS.sleep); // the settle cadence

// ── the four classical forms, operating on [degree, beats] cells ────────────
// Inversion: mirror each interval around the first note's degree (axis).
function invert(cells, axis = cells[0][0]) {
  return cells.map(([d, b]) => [axis - (d - axis), b]);
}
// Retrograde: reverse note order, keep each note's own duration with it.
function retrograde(cells) {
  return cells.slice().reverse();
}
// Retrograde-inversion: do both.
function retroInvert(cells, axis = cells[0][0]) {
  return retrograde(invert(cells, axis));
}
// Transpose by diatonic steps (sequence the form up/down the mode).
function seq(cells, steps) {
  return cells.map(([d, b]) => [d + steps, b]);
}
// Octave displace some notes for wilder leaps (every other note up/down).
function displace(cells, pattern) {
  return cells.map(([d, b], i) => [d + (pattern[i % pattern.length] ?? 0) * 7, b]);
}
// Augment / diminish durations.
function scaleDur(cells, k) { return cells.map(([d, b]) => [d, b * k]); }

// ── event helpers ───────────────────────────────────────────────────────────
const events = [];
function play(cells, startSec, opts = {}) {
  const { preset = "rosewood", gain = 0.5, decayMul = 1.4, pan = 0, gap = 1.0, octave = 0 } = opts;
  let t = startSec;
  for (const [deg, beats] of cells) {
    const dur = beats * BEAT;
    events.push({
      preset,
      startSec: t,
      midi: toMidi(deg) + octave * 12,
      durSec: dur * 1.05, // slight ring overlap
      gain,
      decayMul,
      pan,
    });
    t += dur * gap;
  }
  return t; // end time
}
const bass = (note, startSec, beats, gain = 0.4) =>
  events.push({ preset: "bass", startSec, midi: m(note), durSec: beats * BEAT, gain, decayMul: 1.55, pan: 0 });
const padFm = (root, startSec, beats, gain = 0.15, pan = 0) => {
  // a soft Fm-color triad rooted on `root` (degree-built minor triad).
  const base = toDegree(m(root));
  for (const off of [0, 2, 4]) {
    events.push({
      preset: "vibraphone",
      startSec, midi: toMidi(base + off),
      durSec: beats * BEAT, gain: gain - off * 0.004,
      decayMul: 1.6, pan,
    });
  }
};
const drop = (note, startSec, gain = 0.15, pan = 0.3) =>
  events.push({ preset: "kalimba", startSec, midi: toMidi(toDegree(m(note))), durSec: 1.2 * BEAT, gain, decayMul: 1.7, pan });

// ════════════════════════════════════════════════════════════════════════════
//  SECTION I — STATEMENT.  The prime hush, spoken plainly.  (bars 0–3)
// ════════════════════════════════════════════════════════════════════════════
padFm("F3", 0, 4 * BAR, 0.15, 0.14);
bass("F2", 0, 3, 0.46); bass("F2", 1 * BAR, 3, 0.46);
bass("F2", 2 * BAR, 3, 0.44); bass("F2", 3 * BAR, 3, 0.4);
let cur = play(HUSH, 0.5 * BEAT, { gain: 0.55, decayMul: 1.45, pan: -0.05 });
// echo the sigh a sixth down, very quiet (kalimba), so the contour imprints.
play(seq(HUSH, -3), 2 * BAR + 0.5 * BEAT, { preset: "kalimba", gain: 0.2, decayMul: 1.7, pan: 0.34 });

// ════════════════════════════════════════════════════════════════════════════
//  SECTION II — ANSWER BY INVERSION.  The sigh turns and climbs.  (bars 4–7)
//  Harmony begins to sink: bass slips F → Eb.
// ════════════════════════════════════════════════════════════════════════════
let s2 = 4 * BAR;
bass("F2", s2, 3, 0.44); bass("F2", s2 + BAR, 3, 0.42);
bass("Eb2", s2 + 2 * BAR, 3, 0.44); bass("Eb2", s2 + 3 * BAR, 3, 0.42);
padFm("Ab3", s2 + 2 * BAR, 2 * BAR, 0.13, -0.12);
// inversion of hush — the descending sigh now ASCENDS. answered against the
// fading prime: a stretto where the prime (low) and its inversion (high) overlap.
play(scaleDur(HUSH, 1.1), s2 + 0.0, { gain: 0.34, decayMul: 1.5, pan: -0.25, octave: -1 }); // shadow prime, low
play(invert(HUSH), s2 + BEAT, { gain: 0.55, decayMul: 1.4, pan: 0.2 });                      // bright inversion
// grace-note flurry trailing the inversion's peak (diminished run up the mode).
let g2 = s2 + 2.2 * BAR;
for (const st of [0, 1, 2, 3, 4]) {
  events.push({ preset: "kalimba", startSec: g2, midi: toMidi(toDegree(m("F5")) + st), durSec: 0.9 * BEAT, gain: 0.18 - st * 0.012, decayMul: 1.6, pan: 0.32 });
  g2 += 0.28 * BEAT;
}

// ════════════════════════════════════════════════════════════════════════════
//  SECTION III — RETROGRADE.  The phrase runs backwards.  (bars 8–11)
//  Bass sinks further: Eb → Db.  A second voice answers in canon (retrograde
//  chasing the prime by one beat).
// ════════════════════════════════════════════════════════════════════════════
let s3 = 8 * BAR;
bass("Db2", s3, 3, 0.44); bass("Db2", s3 + BAR, 3, 0.42);
bass("Db2", s3 + 2 * BAR, 3, 0.42); bass("Db2", s3 + 3 * BAR, 3, 0.4);
padFm("Db3", s3, 4 * BAR, 0.13, 0.12);
// retrograde of hush in the lead — F F A C reversed contour, but extended via
// augmentation so the backwards sigh is long and searching.
const RH = scaleDur(retrograde(HUSH), 1.0);
play(RH, s3 + 0.0, { gain: 0.52, decayMul: 1.5, pan: -0.1 });
// canon: the *prime* chases it a beat later, an octave up, quieter (the memory
// of the original answering its own reversal).
play(HUSH, s3 + BEAT, { gain: 0.3, decayMul: 1.5, pan: 0.28, octave: 1 });
// re-rhythmed retrograde fragment (hemiola: the 3/4 grouped in 2s) on kelon.
const frag = retrograde(HUSH).slice(0, 3); // just the turn
let h3 = s3 + 2.4 * BAR;
for (const [deg, b] of [...frag, ...seq(frag, -2)]) {
  events.push({ preset: "kelon", startSec: h3, midi: toMidi(deg), durSec: 0.8 * BEAT, gain: 0.3, decayMul: 1.3, pan: 0.05 });
  h3 += 0.5 * BEAT; // 2-against-3
}

// ════════════════════════════════════════════════════════════════════════════
//  SECTION IV — THE DARKEST POINT.  Bass on the lowered C; the contour turns
//  fully upside down — the RETROGRADE-INVERSION leads, the most transformed
//  form, the sigh now a backwards rise.  (bars 12–16)
// ════════════════════════════════════════════════════════════════════════════
let s4 = 12 * BAR;
bass("C2", s4, 3, 0.46); bass("C2", s4 + BAR, 3, 0.44);
bass("C2", s4 + 2 * BAR, 3, 0.42); bass("Ab1", s4 + 3 * BAR, 3, 0.42);
bass("Ab1", s4 + 4 * BAR, 3, 0.4);
// held minor pad — Cm color over the trough.
padFm("C4", s4, 3 * BAR, 0.15, -0.1);
padFm("Ab3", s4 + 3 * BAR, 2 * BAR, 0.14, 0.1);
// retrograde-inversion in the lead, augmented, brooding low then leaping high
// via octave displacement (wild upside-down leaps at the trough).
const RI = displace(scaleDur(retroInvert(HUSH), 1.15), [0, 1, 0, 1]);
play(RI, s4 + 0.5 * BEAT, { gain: 0.5, decayMul: 1.55, pan: 0.0 });
// all four forms briefly stacked as a quiet stretto haze (the "everything at
// once" climax of the development) — each entering a beat apart, far apart in
// register, hushed so it stays musical not muddy.
const stack = [
  [HUSH, 0, 0.2, -0.3, "rosewood"],
  [invert(HUSH), 1, 1.0, 0.3, "kalimba"],
  [retrograde(HUSH), 2, -1.0, -0.2, "vibraphone"],
  [retroInvert(HUSH), 3, 0.0, 0.2, "kelon"],
];
let sb = s4 + 2 * BAR;
for (const [form, beatOff, oct, pan, preset] of stack) {
  play(scaleDur(form, 1.0), sb + beatOff * BEAT, { preset, gain: preset === "rosewood" ? 0.3 : 0.18, decayMul: 1.55, pan, octave: oct });
}
// a single bright droplet at the very bottom — a star over the trough.
drop("C6", s4 + 3.5 * BAR, 0.16, 0.34);

// ════════════════════════════════════════════════════════════════════════════
//  SECTION V — RECOVERY & RETURN.  The harmony lifts back C → Eb → F; the four
//  forms relax back toward the prime, the sleep cadence returns mirrored once,
//  then rights itself into the true descending hush.  (bars 17–24)
// ════════════════════════════════════════════════════════════════════════════
let s5 = 17 * BAR;
bass("Eb2", s5, 3, 0.42); bass("Eb2", s5 + BAR, 3, 0.4);
bass("F2", s5 + 2 * BAR, 3, 0.42);
padFm("Eb3", s5, 2 * BAR, 0.13, -0.1);
padFm("F3", s5 + 2 * BAR, 2 * BAR, 0.14, 0.1);
// sleep cadence first appears INVERTED (still rising — not yet at peace) …
play(invert(SLEEP), s5 + 0.5 * BEAT, { gain: 0.42, decayMul: 1.5, pan: 0.15 });
// quiet inversion-of-hush answer fading, the last upside-down gesture.
play(invert(HUSH), s5 + 1.8 * BAR, { preset: "kalimba", gain: 0.18, decayMul: 1.7, pan: 0.32 });

// … then the cadence RIGHTS itself: the true descending sleep, the recognizable
// thread, low and warm, putting the contour back the right way up. (bars 20–24)
let s6 = 20 * BAR;
bass("F2", s6, 6, 0.38);
bass("C2", s6 + 2 * BAR, 3, 0.36);
bass("F2", s6 + 3 * BAR, 6, 0.34);
padFm("F3", s6, 5 * BAR, 0.14, 0.12);
// the prime sleep settle, an octave low, the home statement.
let endT = play(SLEEP, s6 + 0.5 * BEAT, { gain: 0.46, decayMul: 1.5, pan: 0, octave: -1 });
// faint kalimba droplets tracing the final descent, snapped to minor.
for (const [n, off] of [["F5", 0], ["Eb5", 1.4], ["Db5", 2.6], ["C5", 3.8], ["F4", 5.0]]) {
  drop(n, s6 + 1.2 * BAR + off * BEAT, 0.15, 0.3);
}
// last low Fm root to put it to bed.
bass("F2", endT - BEAT, 8, 0.3);
padFm("F2", endT - BEAT, 8, 0.12, 0);

const { mp3, durationSec } = renderLullaby(events, {
  name: "minorbaba",
  here: HERE,
  title: "minorbaba (F natural minor — four mirror forms)",
  reverb: { wet: 0.34, decay: 0.85, damp: 0.42 },
  fadeIn: 1.2,
  fadeOut: 5.5,
  tailSec: 5.5,
  peak: 0.82,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
