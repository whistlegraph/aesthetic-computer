// render-waltz.mjs — an original charming WALTZ promo for "Menu Band",
// the colorful macOS menubar piano app.
//
// Built on the AC /pop lullaby engine (pop/marimba/lullabies/lib/core.mjs):
// the same modal-mallet marimba synth, mixed + reverbed + mastered to mp3.
// The lead voice is a glockenspiel (a tiny music box / bell), riding a
// classic waltz "oom-pah-pah" bass + chord stabs, in a bright C MAJOR.
//
// FORM — rounded ABA so it opens, develops, and lands resolved (good loop):
//   A  (bars 0-7)   the music-box tune, plain & singable, ends half-open on V.
//   B  (bars 8-15)  a lift — the tune sequenced up, brighter register, a turn.
//   A' (bars 16-23) the tune returns home and cadences cleanly on tonic C.
//
// One `events` list feeds BOTH the audio render AND the notes.json the UI sim
// reads to light keys — each event carries a `lane` ("lead"|"bass"|"harmony").
//
// Run:  node pop/menuband/bin/render-waltz.mjs        (from repo root)

import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { writeFileSync, existsSync } from "node:fs";
import { renderLullaby, m } from "../../marimba/lullabies/lib/core.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT_DIR = resolve(HERE, "..", "out");

// ── tempo: a lilting waltz at 138 BPM ────────────────────────────────────
const BPM = 138;
const BEAT = 60 / BPM; // quarter-note
const BAR = 3 * BEAT; // a 3/4 bar

// ── voices ────────────────────────────────────────────────────────────────
const LEAD = "glockenspiel"; // music-box bell — the singable tune
const BASS = "bass"; // oom on beat 1
const STAB = "staccato"; // light pah-pah chord stabs on 2 & 3

// ── C MAJOR chord table: [bassNote, [stabNoteA, stabNoteB]] ────────────────
const CH = {
  C: ["C2", ["E4", "G4"]],
  G: ["G2", ["D4", "G4"]],
  G7: ["G2", ["F4", "B3"]],
  F: ["F2", ["A3", "C4"]],
  Am: ["A2", ["C4", "E4"]],
  Dm: ["D2", ["F4", "A3"]],
  Em: ["E2", ["G4", "B3"]],
};

// the one events list both the audio and the notes.json are derived from.
const events = [];

function push(lane, preset, startSec, note, durSec, gain, pan = 0, decayMul = 1.2) {
  events.push({
    lane,
    preset,
    startSec,
    midi: typeof note === "number" ? note : m(note),
    durSec,
    gain,
    pan,
    decayMul,
  });
}

// ── one waltz accompaniment bar: oom (b1) — pah (b2) — pah (b3) ─────────────
function accompany(bar, chord, g = 1) {
  const [bn, [a, b]] = CH[chord];
  const t = bar * BAR;
  // OOM — warm bass downbeat, rings about a beat.
  push("bass", BASS, t + 0 * BEAT, bn, 1.05 * BEAT, 0.4 * g, 0);
  // PAH — two light chord stabs on beats 2 and 3, gently panned apart.
  push("harmony", STAB, t + 1 * BEAT, a, 0.42 * BEAT, 0.15 * g, -0.16, 0.9);
  push("harmony", STAB, t + 1 * BEAT, b, 0.42 * BEAT, 0.15 * g, -0.16, 0.9);
  push("harmony", STAB, t + 2 * BEAT, a, 0.42 * BEAT, 0.14 * g, 0.16, 0.9);
  push("harmony", STAB, t + 2 * BEAT, b, 0.42 * BEAT, 0.14 * g, 0.16, 0.9);
}

// ── lead-melody emitter: a cell of [noteName|null, beats] pairs ─────────────
// null = a rest. Notes ring a little past their beat for music-box bloom.
function melody(startBar, cell, { gain = 0.5, octave = 0, pan = 0.04, ring = 1.25 } = {}) {
  let t = startBar * BAR;
  for (const [note, beats] of cell) {
    const dur = beats * BEAT;
    if (note != null) {
      const midi = (typeof note === "number" ? note : m(note)) + 12 * octave;
      push("lead", LEAD, t, midi, dur * ring, gain, pan, 1.3);
    }
    t += dur;
  }
  return t;
}

// ════════════════════════════════════════════════════════════════════════
// SECTION A — the music-box tune (bars 0-7). Plain, singable, warm.
//   A rising curtsy (G→C→E→G), a gentle turn, a little arch, then a
//   poised half-cadence hanging on the dominant (D over G) — "more to come".
// chords:  C  C  F  C  Dm  G  C  G
// ════════════════════════════════════════════════════════════════════════
const PROG_A = ["C", "C", "F", "C", "Dm", "G", "C", "G"];
PROG_A.forEach((c, i) => accompany(i, c));

// the tune. Each [note, beats]; bars are 3 beats. Anacrusis-free, lands on 1.
const THEME_A = [
  // bar 0: pickup-feel rising into the bar
  ["G4", 1], ["C5", 1], ["E5", 1],
  // bar 1: arch up and settle
  ["G5", 1], ["E5", 1], ["C5", 1],
  // bar 2: over F — a gentle reach
  ["A4", 1], ["C5", 1], ["F5", 1],
  // bar 3: a sweet falling turn back home
  ["E5", 1], ["D5", 0.5], ["C5", 0.5], ["D5", 1],
  // bar 4: over Dm — lilt
  ["F5", 1], ["E5", 1], ["D5", 1],
  // bar 5: over G — step up to a poised tone
  ["E5", 1], ["D5", 1], ["B4", 1],
  // bar 6: resolve briefly to C, a little skip
  ["C5", 1], ["E5", 1], ["G4", 1],
  // bar 7: half-cadence — hang on D (the 5th of G), open
  ["A4", 1], ["B4", 1], ["D5", 1],
];
melody(0, THEME_A);

// ════════════════════════════════════════════════════════════════════════
// SECTION B — the lift (bars 8-15). The tune climbs into a brighter register
//   and develops with a little sequence + a turn, building anticipation, then
//   a dominant prep that leans back toward home.
// chords:  Am  Em  F   C   Dm  G7  C   G7
// ════════════════════════════════════════════════════════════════════════
const PROG_B = ["Am", "Em", "F", "C", "Dm", "G7", "C", "G7"];
PROG_B.forEach((c, i) => accompany(8 + i, c, 0.95));

const THEME_B = [
  // bar 8: over Am — a brighter opening up high
  ["E5", 1], ["A5", 1], ["G5", 1],
  // bar 9: over Em — sequence down a step, sighing
  ["E5", 1], ["G5", 1], ["B5", 1],
  // bar 10: over F — a turning figure
  ["A5", 1], ["G5", 0.5], ["F5", 0.5], ["E5", 1],
  // bar 11: over C — settle, twinkle
  ["G5", 1], ["E5", 1], ["C5", 1],
  // bar 12: over Dm — rise again
  ["D5", 1], ["F5", 1], ["A5", 1],
  // bar 13: over G7 — a bright tension reach
  ["B5", 1], ["A5", 0.5], ["G5", 0.5], ["F5", 1],
  // bar 14: over C — a brief touch home
  ["E5", 1], ["G5", 1], ["C5", 1],
  // bar 15: over G7 — lean on the dominant, pulling back to A'
  ["D5", 1], ["F5", 1], ["B4", 1],
];
melody(8, THEME_B, { gain: 0.46, pan: -0.04, ring: 1.2 });

// a sparkling high counter-shimmer through B (an octave up, far + quiet) so
// the lift feels colorful — tagged harmony so the sim keeps key-lighting on lead.
const SHIMMER_B = [
  ["C6", 1.5], ["E6", 1.5],
  ["G6", 1.5], ["E6", 1.5],
  ["F6", 1.5], ["C6", 1.5],
  ["G6", 3],
];
{
  let t = 8 * BAR;
  for (const [note, beats] of SHIMMER_B) {
    push("harmony", LEAD, t, m(note), beats * BEAT * 1.3, 0.1, 0.34, 1.5);
    t += beats * BEAT;
  }
}

// ════════════════════════════════════════════════════════════════════════
// SECTION A' — the return (bars 16-23). The opening tune comes back home, but
//   this time the ending is rewritten to a CLEAN, RESOLVED authentic cadence
//   (G7 → C) so the loop lands firmly on tonic.
// chords:  C  C  F  C  Dm  G7  C  C
// ════════════════════════════════════════════════════════════════════════
const PROG_A2 = ["C", "C", "F", "C", "Dm", "G7", "C", "C"];
PROG_A2.forEach((c, i) => accompany(16 + i, c));

const THEME_A2 = [
  // bars 16-19: same beloved opening (curtsy + arch + reach + turn)
  ["G4", 1], ["C5", 1], ["E5", 1],
  ["G5", 1], ["E5", 1], ["C5", 1],
  ["A4", 1], ["C5", 1], ["F5", 1],
  ["E5", 1], ["D5", 0.5], ["C5", 0.5], ["D5", 1],
  // bar 20: over Dm — lilt up
  ["F5", 1], ["E5", 1], ["D5", 1],
  // bar 21: over G7 — the cadential gather, a graceful step-down
  ["F5", 1], ["E5", 1], ["D5", 1],
  // bar 22: over C — RESOLVE: land on tonic, a warm chiming arrival
  ["C5", 1], ["E5", 1], ["G5", 1],
  // bar 23: the final tonic — a settled high C with a soft echo below
  ["C6", 2], ["C5", 1],
];
melody(16, THEME_A2);

// a low tonic bloom under the very last bar to seat the cadence.
push("bass", BASS, 23 * BAR, "C2", 3 * BEAT, 0.34, 0);
// one last far bell — the music box winding down to rest.
push("harmony", LEAD, 23 * BAR + 0.5 * BEAT, m("G5"), 2.5 * BEAT, 0.12, 0.3, 1.5);

// ════════════════════════════════════════════════════════════════════════
// RENDER — audio mp3 + notes.json, both from the SAME events list.
// ════════════════════════════════════════════════════════════════════════
const outMp3 = resolve(OUT_DIR, "menuband-waltz.mp3");
const { mp3, durationSec } = renderLullaby(events, {
  name: "menuband-waltz",
  here: HERE,
  out: outMp3,
  title: "Menu Band Waltz",
  healing: false, // a bright dance, not a healing bed — keep it clean.
  reverb: { wet: 0.26, decay: 0.82, damp: 0.38 },
  fadeIn: 0.4,
  fadeOut: 2.0,
  tailSec: 3.0,
});

// derive the notes array straight from the events (no hand-duplication).
const notes = events
  .map((e) => ({
    t: +e.startSec.toFixed(4),
    dur: +e.durSec.toFixed(4),
    midi: e.midi,
    vel: +e.gain.toFixed(3),
    lane: e.lane,
  }))
  .sort((a, b) => a.t - b.t || a.midi - b.midi);

const notesJson = {
  bpm: BPM,
  beatSec: +BEAT.toFixed(6),
  barSec: +BAR.toFixed(6),
  durationSec: +durationSec.toFixed(4),
  leadPreset: LEAD,
  notes,
};

const outNotes = resolve(OUT_DIR, "menuband-waltz.notes.json");
writeFileSync(outNotes, JSON.stringify(notesJson, null, 2));

// confirm both files exist.
const leadCount = notes.filter((n) => n.lane === "lead").length;
const okMp3 = existsSync(mp3);
const okNotes = existsSync(outNotes);
console.log(`✓ mp3   : ${mp3} ${okMp3 ? "(exists)" : "(MISSING)"} · ${durationSec.toFixed(1)}s`);
console.log(`✓ notes : ${outNotes} ${okNotes ? "(exists)" : "(MISSING)"}`);
console.log(`  bpm ${BPM} · key C major · lead "${LEAD}"`);
console.log(`  ${notes.length} notes total · ${leadCount} lead`);
if (!okMp3 || !okNotes) process.exitCode = 1;
