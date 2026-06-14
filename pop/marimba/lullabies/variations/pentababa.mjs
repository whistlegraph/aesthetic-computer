// pentababa.mjs — F-major-pentatonic PROCESS music on the marimbaba DNA.
//
// Pentatonic drops the 4th (Bb) and 7th (E) from F major, leaving only the
// five most consonant degrees — F G A C D. Nothing can clash, so the same
// five tones can be recombined endlessly without ever needing to resolve.
//
// DEVELOPMENT STRATEGY — PERMUTATIONAL RECOMBINATION (minimalist process):
// We treat the five pitches as a SET and systematically recombine them across
// the whole piece — rotate the cell, reorder it, interleave TWO simultaneous
// orderings (a phasing canon), and shift which note is emphasized each phrase.
// A single 5-note cell becomes an ever-shifting kaleidoscope: the melody
// travels and mutates the entire time instead of restating the tune. We keep
// the marimbaba thread by opening and closing on the hush sigh (C-A-F, all
// in-scale) and by keeping the kalimba lead, ~60 BPM feel, big open gamelan
// room, and F-pedal bass — the same MOOD, a wildly different melody.
//
// Run:  node variations/pentababa.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby, m } from "../lib/core.mjs";
import { MOTIFS, DECAY } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BPM = 60;
const BEAT = 60 / BPM;

// ── F major pentatonic spelled across octaves: F G A C D ─────────────────────
// The SET we permute. Octave 4 = the "home" register; we lift/drop by ±12.
const SET = ["F4", "G4", "A4", "C5", "D5"]; // 5 pitch classes, ascending
const N = SET.length;

// ── permutation helpers (the process engine) ─────────────────────────────────
const rotate = (arr, k) => arr.map((_, i) => arr[(i + k) % arr.length]);
const retro = (arr) => [...arr].reverse();
// "interleave" two orderings into one stream a0 b0 a1 b1 ... (stretto / weave)
const interleave = (a, b) => {
  const out = [];
  for (let i = 0; i < Math.max(a.length, b.length); i++) {
    if (i < a.length) out.push(a[i]);
    if (i < b.length) out.push(b[i]);
  }
  return out;
};
// move a note up/down whole octaves
const oct = (note, n) => {
  const mm = m(note) + 12 * n;
  return mm;
};

const LEAD = "kalimba";
const events = [];
let cursor = 0; // running seconds cursor for the lead voice

// emit one lead note; advance cursor by `beats`.
function lead(note, beats, gain = 0.34, opts = {}) {
  const midi = typeof note === "number" ? note : m(note);
  events.push({
    preset: opts.preset ?? LEAD,
    startSec: cursor,
    midi,
    durSec: beats * BEAT * (opts.ring ?? 1),
    gain,
    decayMul: (DECAY[opts.preset ?? LEAD] ?? 1.75) * (opts.decayMul ?? 1.5),
    pan: opts.pan ?? 0.18,
  });
  cursor += beats * BEAT;
}

// a free "answer" voice at an absolute time (canon / bonang ping, no cursor).
function ping(atSec, note, beats, gain = 0.2, opts = {}) {
  const midi = typeof note === "number" ? note : m(note);
  events.push({
    preset: opts.preset ?? "gamelan",
    startSec: atSec,
    midi,
    durSec: beats * BEAT * (opts.ring ?? 1),
    gain,
    decayMul: (DECAY[opts.preset ?? "gamelan"] ?? 1.75) * (opts.decayMul ?? 1.6),
    pan: opts.pan ?? -0.3,
  });
}

// a sustained F pedal bass that holds the room together (octave per call).
function pedal(atSec, beats, note = "F2", gain = 0.42) {
  events.push({
    preset: "bass",
    startSec: atSec,
    midi: m(note),
    durSec: beats * BEAT,
    gain,
    decayMul: (DECAY.bass ?? 1.8) * 1.2,
    pan: 0,
  });
}

// =============================================================================
// PHASE 0 — STATEMENT: the marimbaba hush sigh, slow & spacious, on kalimba.
//   The recognizable thread. C-A-F descending, all in pentatonic.
// =============================================================================
pedal(cursor, 12, "F2", 0.4);
for (const [note, beats] of MOTIFS.hush) {
  lead(note, beats * 1.2, 0.3, { ring: 1.3, pan: -0.06 });
}
ping(cursor - BEAT * 1.5, oct("C6", 0), 3, 0.16, { pan: 0.34 }); // a far chime echo
cursor += BEAT * 1.0; // breathe

// =============================================================================
// PHASE 1 — ROTATIONS: the bare set, ascending, then rotated one step each
//   pass. Each pass lands its emphasis (a louder, longer note) on a new degree
//   so the "tonic of the phrase" keeps shifting — same 5 notes, new center.
// =============================================================================
pedal(cursor, 15, "F2", 0.38);
{
  let passStart = cursor;
  for (let k = 0; k < N; k++) {
    const cell = rotate(SET, k);          // ascending order, rotated start
    cell.forEach((note, i) => {
      const emph = i === 0;               // emphasize the rotation's head note
      lead(note, emph ? 0.75 : 0.5, emph ? 0.4 : 0.28, {
        pan: -0.2 + (i / N) * 0.4,        // sweep the cell L→R
        ring: emph ? 1.4 : 1.0,
      });
    });
    // a sparse gamelan answer an octave up on the head of the next rotation
    ping(passStart + 0.25 * BEAT, oct(rotate(SET, (k + 1) % N)[0], 1), 2, 0.14, {
      pan: 0.32,
    });
    passStart = cursor;
  }
}
cursor += BEAT * 0.5;

// =============================================================================
// PHASE 2 — TWO-VOICE WEAVE (stretto): interleave an ASCENDING ordering with a
//   simultaneous DESCENDING (retrograde) ordering into one rippling stream.
//   Octave-displace the descending member so the line leaps wildly while still
//   only ever using the five tones — the kaleidoscope opening up.
// =============================================================================
pedal(cursor, 18, "F3", 0.34);
{
  const up = SET;                          // F G A C D
  const down = retro(SET);                 // D C A G F
  // weave them, lifting the descending member up an octave for big leaps
  const woven = [];
  for (let i = 0; i < N; i++) {
    woven.push([up[i], 0]);
    woven.push([down[i], 1]);              // [note, octaveShift]
  }
  woven.forEach(([note, o], i) => {
    const fast = i % 2 === 1;              // the leaping voice is shorter/brighter
    lead(oct(note, o), fast ? 0.375 : 0.5, fast ? 0.24 : 0.34, {
      pan: fast ? 0.34 : -0.28,           // the two voices sit on opposite sides
      ring: fast ? 0.9 : 1.2,
    });
  });
  // bonang canon: echo the whole weave a beat later, an octave up, quieter
  let echoT = cursor - woven.length * 0.4375 * BEAT + BEAT * 0.5;
  for (const [note, o] of woven) {
    ping(echoT, oct(note, o + 1), 1.5, 0.12, { pan: 0.38 });
    echoT += 0.4375 * BEAT;
  }
}
cursor += BEAT * 0.5;

// =============================================================================
// PHASE 3 — DIMINUTION / ACCELERATION: compress the rotating cell into faster
//   and faster runs (a hemiola of 5 against 3), driving toward a peak. This is
//   the most "insane" travelling — the same set spun into cascading sextuplets.
// =============================================================================
pedal(cursor, 12, "F2", 0.34);
{
  const speeds = [0.5, 0.375, 0.3, 0.25, 0.2]; // beats/note — accelerating
  speeds.forEach((dur, pass) => {
    const cell = rotate(SET, pass * 2 % N);    // rotate by 2 (a different cycle)
    // run the cell up, then its retrograde down — a wave per pass
    const run = [...cell, ...retro(cell).slice(1)];
    run.forEach((note, i) => {
      // climb octaves as we accelerate so the peak literally rises
      const o = pass >= 3 ? 1 : 0;
      const g = 0.22 + pass * 0.02;
      lead(oct(note, o), dur, Math.min(g, 0.3), {
        pan: -0.3 + ((i % N) / N) * 0.6,
        ring: 0.8,
      });
    });
  });
  // a high gamelan peak crowning the acceleration
  ping(cursor, oct("D5", 1), 4, 0.22, { pan: 0.3, ring: 1.4 });
  ping(cursor + 0.5 * BEAT, oct("A4", 1), 3.5, 0.18, { pan: -0.32, ring: 1.4 });
}
cursor += BEAT * 1.0;

// =============================================================================
// PHASE 4 — AUGMENTED RECOMBINATION (the come-down): take the SAME set but
//   stretch it way out, reordered by a "skip" permutation (every other note),
//   landing emphasis on F again to steer the ear home. Sparse, wide, gamelan.
// =============================================================================
pedal(cursor, 21, "F2", 0.38);
{
  // skip-permutation: indices 0,2,4,1,3 → F A D G C — a fresh contour, same set
  const skip = [0, 2, 4, 1, 3].map((i) => SET[i]);
  skip.forEach((note, i) => {
    const emph = note === "F4";
    lead(note, emph ? 3 : 2, emph ? 0.34 : 0.26, {
      ring: 1.5,
      pan: -0.1 + (i / skip.length) * 0.2,
    });
    // a far octave answer between the long notes
    ping(cursor - BEAT * 1.0, oct(skip[(i + 2) % skip.length], 1), 3, 0.13, {
      pan: i % 2 ? 0.36 : -0.34,
    });
  });
}
cursor += BEAT * 1.0;

// =============================================================================
// PHASE 5 — RETURN: the hush sigh again, the spine, now high & far as a
//   wondrous echo floating up to rest — bookending the whole process.
// =============================================================================
pedal(cursor, 12, "F2", 0.34);
for (const [note, beats] of MOTIFS.hush) {
  lead(oct(note, 1), beats * 1.4, 0.18, {
    preset: "gamelan",
    ring: 1.5,
    pan: 0.3,
    decayMul: 1.8,
  });
}
// one last low F to settle the room
ping(cursor - BEAT * 2, "F4", 4, 0.16, { preset: "kalimba", pan: -0.1, ring: 1.4 });

// =============================================================================
const { mp3, durationSec } = renderLullaby(events, {
  name: "pentababa",
  here: HERE,
  title: "pentababa",
  reverb: { wet: 0.4, decay: 0.88, damp: 0.32 }, // big open room for the gaps
  fadeIn: 1.0,
  fadeOut: 5.0,
  tailSec: 6.0,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
