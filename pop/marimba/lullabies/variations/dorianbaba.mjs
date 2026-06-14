// dorianbaba.mjs — a D-dorian folk ROUND on the marimbaba seed, developed as
// CANON & STRETTO. The lead (kelon) sings a wistful dorian thread drawn from
// the marimbaba motifs; a second marimba (kalimba — the humming voice) chases
// it as a strict canon, entering a bar later at the 4th/5th. Then the chase
// TIGHTENS: each return crowds the answer closer to the leader (stretto) —
// a bar, then half a bar, then a beat, then a single beat-and-a-half overlap —
// until the two voices braid into a dense contrapuntal weave. Finally the
// imitation relaxes: the answers fall away and a single line tucks the round
// in, alone, as a folk goodnight.
//
// Identity kept: D dorian (root D, raised-6th B natural = the "hope"), kelon
// lead, ~56 BPM lullaby lilt in 3, kalimba as the second/answering voice, the
// hush "descending sigh" as the recurring head of the round, and a re-rooted
// D/G/A folk bass. The MELODY now travels hard: the head is sequenced,
// inverted, augmented and diminished as it passes between the two voices.
//
// Run:  node variations/dorianbaba.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 56;
const BEAT = 60 / BPM;
const BAR = 3 * BEAT;

// ── D dorian: root pitch-class D (2), scale {0,2,3,5,7,9,10} ─────────────────
const ROOT_PC = m("D4") % 12;            // 2
const DORIAN = [0, 2, 3, 5, 7, 9, 10];   // D E F G A B C

// All seven scale degrees as absolute pitch classes, for snapping & for
// transposing strictly along the mode (a "diatonic" transpose, so the canon
// answer at the 4th/5th stays in D dorian rather than going chromatic).
const SCALE_PCS = DORIAN.map((d) => (ROOT_PC + d) % 12);

// Fold a midi note to the nearest pitch in D dorian (ties resolve downward so
// the tender minor color is favored).
function snap(midi) {
  let best = midi, bestD = Infinity;
  for (let oct = -1; oct <= 1; oct++) {
    for (const deg of DORIAN) {
      const pc = (ROOT_PC + deg) % 12;
      const base = Math.round((midi - pc) / 12) * 12 + pc + oct * 12;
      const d = Math.abs(base - midi);
      if (d < bestD - 1e-6 || (Math.abs(d - bestD) < 1e-6 && base < best)) {
        best = base; bestD = d;
      }
    }
  }
  return best;
}

// Give a midi its scale-degree index (0..6) within D dorian, plus octave, so
// we can move it by N scale steps and land on a real dorian pitch.
function degreeOf(midi) {
  const snapped = snap(midi);
  const pc = ((snapped % 12) + 12) % 12;
  const idx = SCALE_PCS.indexOf(pc);
  const oct = Math.floor((snapped - SCALE_PCS[idx]) / 12);
  return { idx, oct };
}

// Diatonic transpose: move a midi note up/down by `steps` scale degrees,
// staying inside D dorian. steps=+3 => up a 4th, +4 => up a 5th (folk round).
function diatonic(midi, steps) {
  const { idx, oct } = degreeOf(midi);
  const total = idx + steps;
  const o = oct + Math.floor(total / 7);
  const i = ((total % 7) + 7) % 7;
  return SCALE_PCS[i] + (o + 1) * 12;
}

// ── the ROUND HEAD: a single dorian line, as [note, beats] cells ────────────
// Built from the marimbaba DNA: the hush sigh, the twinkle climb, the baba
// wobble, the sleep settle — folded to dorian and re-strung into one singable
// subject that we can hand back and forth as a canon. ~8 beats long.
const cell = (m, b) => [m, b];
function fold(motif) { return motif.map(([n, b]) => [snap(m(n)), b]); }

// HEAD — the subject of the round (the thing the second voice will chase).
// hush sigh -> a little rise -> resolve. Recognizably the marimbaba hush.
const HEAD = [
  ...fold(MOTIFS.hush).slice(0, 3),       // C5 A4 F4  (descending sigh)
  cell(snap(m("G4")), 1), cell(snap(m("A4")), 1), // rise back up
  cell(snap(m("F4")), 2),                 // settle
];

// invert intervals around the first note (mirror up/down) — a fresh contour
// from the same DNA, used when the round turns over.
function invert(line) {
  const pivot = line[0][0];
  return line.map(([n, b], i) => (i === 0 ? [n, b] : [snap(2 * pivot - n), b]));
}
// retrograde — play the line backwards.
function retro(line) { return [...line].reverse(); }
// augment / diminish durations.
function scaleDur(line, k) { return line.map(([n, b]) => [n, b * k]); }
// diatonic sequence: move the whole line by N scale steps.
function seq(line, steps) { return line.map(([n, b]) => [diatonic(n, steps), b]); }
// octave displace selected notes for wild leaps.
function octave(line, k = 1) { return line.map(([n, b]) => [n + 12 * k, b]); }

// ── event emitter for a melodic line ────────────────────────────────────────
const events = [];
function play(line, startBeat, { preset = "kelon", gain = 0.34, pan = 0, decayMul = 1.4, durMul = 1.05, gapafter = 0 } = {}) {
  let t = startBeat;
  for (const [midi, beats] of line) {
    events.push({
      preset,
      startSec: t * BEAT,
      midi,
      durSec: beats * durMul * BEAT,
      gain,
      decayMul: (DECAY[preset] ?? 1.4) * decayMul,
      pan,
    });
    t += beats;
  }
  return t + gapafter; // returns the beat where the line ends
}

// ── BASS: a slow D / G / A dorian folk pedal under the whole round ───────────
function bassNote(noteName, startBeat, beats, gain = 0.46) {
  events.push({
    preset: "bass",
    startSec: startBeat * BEAT,
    midi: m(noteName),
    durSec: beats * BEAT,
    gain,
    decayMul: (DECAY.bass ?? 1.8) * 1.25,
    pan: 0,
  });
}

// ── soft dorian pad to seat the harmony (vibraphone_off) ────────────────────
function pad(notes, startBeat, beats, gain = 0.12, pan = -0.2) {
  for (const n of notes) {
    events.push({
      preset: "vibraphone_off",
      startSec: startBeat * BEAT,
      midi: m(n),
      durSec: beats * BEAT,
      gain,
      decayMul: (DECAY.vibraphone_off ?? 1.4) * 1.4,
      pan,
    });
  }
}

// ════════════════════════════════════════════════════════════════════════════
//  THE ARC — one continuous beat clock; sections measured in beats (3/bar).
// ════════════════════════════════════════════════════════════════════════════
let B = 0; // global beat clock

// ── SECTION 1 — the SUBJECT, alone (a simple statement) ─────────────────────
// The lead sings the round head once, unaccompanied but for the bass, so the
// ear learns the tune before the chase begins.
bassNote("D2", B, 6, 0.42);
play(HEAD, B + 0, { preset: "kelon", gain: 0.36, pan: -0.12 });
B += 8; // head is ~8 beats

// ── SECTION 2 — CANON at the distance of a BAR (the round opens) ────────────
// Lead restates the head (now with a small extension); the second voice
// (kalimba) enters ONE BAR (3 beats) later at the 4th above — strict canon,
// the classic folk-round entry. We do this twice so the round circles.
function dux(line, at, opts) { return play(line, at, { preset: "kelon", gain: 0.34, pan: -0.14, ...opts }); }
function comes(line, at, steps, opts) {
  // the answer: same line, diatonically transposed (4th/5th), kalimba voice.
  return play(seq(line, steps), at, { preset: "kalimba", gain: 0.24, pan: 0.34, decayMul: 1.45, ...opts });
}

bassNote("D2", B, 9, 0.4);
bassNote("G2", B + 9, 9, 0.4);
// pass A: lead head, kalimba answer a BAR later at the 4th (+3 steps)
dux(HEAD, B);
comes(HEAD, B + 3, 3, { gain: 0.22 });
// a sparse Dm pad glow under the round
pad(["D4", "F4", "A4"], B, 9, 0.11);
B += 9;
// pass B: lead head sequenced UP a step (the tune travels); answer at the 5th.
bassNote("A2", B, 9, 0.4);
const headUp = seq(HEAD, 1);
dux(headUp, B, { pan: -0.16 });
comes(headUp, B + 3, 4, { gain: 0.22 }); // answer a 5th above
pad(["E4", "G4", "B4"], B, 9, 0.11); // Em-ish color, B natural = the hope
B += 9;

// ── SECTION 3 — STRETTO: the answers crowd closer and closer ────────────────
// Same head, but each restatement shortens the gap before the answer enters:
// a full bar -> half a bar -> a beat -> a beat-and-a-half overlap that braids
// the two voices into one shimmering weave. We also diminish (compress) the
// head a little each time so the lines accelerate — the round tightening.
function strettoPass(at, gapBeats, durMul, headLine, leadSteps, answerSteps, leadPan, ansPan) {
  const lead = headLine;
  dux(scaleDur(lead, durMul), at, { pan: leadPan, durMul: 1.0 });
  comes(scaleDur(lead, durMul), at + gapBeats, answerSteps, { pan: ansPan, gain: 0.23, durMul: 1.0 });
  return at;
}

bassNote("D2", B, 6, 0.4);
// gap = a full bar (3 beats), tune travels down a step
strettoPass(B, 3, 1.0, seq(HEAD, -1), 0, 4, -0.18, 0.36);
pad(["C4", "E4", "G4"], B, 6, 0.1);
B += 6;

bassNote("G2", B, 6, 0.4);
// gap = half a bar (1.5 beats); diminish the head to 0.8 — it speeds up
strettoPass(B, 1.5, 0.82, HEAD, 0, 3, -0.2, 0.38);
pad(["G4", "Bb4", "D5"], B, 6, 0.1);
B += 6;

bassNote("A2", B, 6, 0.4);
// gap = one beat; invert the head so the second turn of the round mirrors
const invHead = invert(HEAD);
strettoPass(B, 1.0, 0.7, invHead, 0, 4, -0.22, 0.4);
pad(["D4", "A4", "E5"], B, 6, 0.1);
B += 5;

bassNote("D2", B, 6, 0.4);
// gap = a beat-and-a-half overlap, but now a THIRD voice (vibraphone) joins —
// the densest braid: three entries inside two bars, the round at full weave.
const tightHead = scaleDur(HEAD, 0.6); // strongly diminished — quick run
dux(tightHead, B, { pan: -0.2, durMul: 0.95 });
comes(tightHead, B + 1.0, 3, { pan: 0.36, gain: 0.22 });   // 4th, one beat behind
play(seq(tightHead, 4), B + 1.5,                            // 5th, on vibraphone
  { preset: "vibraphone", gain: 0.16, pan: 0.05, decayMul: 1.2, durMul: 0.95 });
pad(["D4", "F4", "A4", "C5"], B, 7, 0.1);
B += 6;

// ── SECTION 4 — the WEAVE peaks: cascading canon of fragments ───────────────
// Fragment the head to its first 3 notes (the hush sigh) and fire it as a
// quick rising sequence in close canon — bright glockenspiel sparks chasing
// the kelon, a flurry that lifts the round to its highest point before it
// relaxes. Octave-displaced for a couple of wide, surprising leaps.
const frag = HEAD.slice(0, 3);                 // C5 A4 F4 sigh, diminished
const sigh = scaleDur(frag, 0.5);
bassNote("D3", B, 6, 0.38);
let f = B;
for (let i = 0; i < 4; i++) {
  const up = seq(sigh, i);                      // sequence the sigh upward
  dux(i === 3 ? octave(up, 1) : up, f, { pan: -0.18 + i * 0.04, gain: 0.3 - i * 0.01 });
  // a glockenspiel spark answers a 5th up, three-quarters of a beat behind
  play(seq(up, 4), f + 0.75,
    { preset: "glockenspiel", gain: 0.14, pan: 0.3, decayMul: 1.3, durMul: 0.9 });
  f += 1.5;
}
pad(["G4", "B4", "D5", "A5"], B, 8, 0.11); // G(add9) — the hopeful B-natural glow
B += 8;

// ── SECTION 5 — the round RELAXES back to a single line (resolution) ─────────
// The imitation falls away. The lead sings the head once more, augmented
// (stretched, calm), in its home register — alone now, the second voice only
// humming the final resting note an octave up, like the round closing.
bassNote("D2", B, 12, 0.4);
pad(["D4", "F4", "A4"], B, 12, 0.1);
const calm = scaleDur(HEAD, 1.25);             // augmented — slow goodnight
const closeEnd = dux(calm, B, { gain: 0.32, pan: -0.06, durMul: 1.15, decayMul: 1.5 });
// one soft kalimba answer of just the last note, an octave up — the round's
// final echo, then silence.
const lastMidi = HEAD[HEAD.length - 1][0];
events.push({
  preset: "kalimba",
  startSec: (closeEnd - 1) * BEAT,
  midi: lastMidi + 12,
  durSec: 3.2 * BEAT,
  gain: 0.2,
  decayMul: (DECAY.kalimba ?? 1.75) * 1.5,
  pan: 0.3,
});
// a low D bell, tucking in.
events.push({
  preset: "kalimba",
  startSec: (closeEnd + 0.6) * BEAT,
  midi: m("D4"),
  durSec: 3.6 * BEAT,
  gain: 0.18,
  decayMul: (DECAY.kalimba ?? 1.75) * 1.55,
  pan: 0.24,
});

const { mp3, durationSec } = renderLullaby(events, {
  name: "dorianbaba",
  here: HERE,
  title: "dorianbaba",
  reverb: { wet: 0.34, decay: 0.85, damp: 0.36 },
  fadeIn: 1.0,
  fadeOut: 4.5,
  tailSec: 5.0,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
