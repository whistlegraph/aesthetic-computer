#!/usr/bin/env node
// render-polyrhythm-jingle.mjs — the Menu Band POLYRHYTHM reel's jingle,
// on the same /pop lullaby engine as render-waltz.mjs / render-jingles.mjs.
//
// The rhythmic identity IS the content: every rhythm is its own lane with
// its own timbre, and the sim lights one side-by-side circle per lane from
// that lane's notes — so the circles and the audio agree to the sample.
//
//   solo   0.0– 4.8   "3"      three (vibraphone) establishes the pulse
//   duo    4.8–12.0   "3:2"    two (gamelan) lands against it
//   trio  12.0–19.2   "3:4"    four (kalimba) swaps in — same 3, new grid
//   full  19.2–26.4   "3:4:5"  five (glockenspiel) on top — three grids
//   end   26.4        every grid coincides: one unison C, ringing out
//
// The shared cycle is 2.4s — exactly the app's PolyrhythmTrainerClock
// default (75 BPM on the outer 3: 60/75*3 = 2.4). Every pitch is a WHITE
// KEY so the strip rig's real captured menu-bar piano lights pixel-real.
//
// Writes: out/menuband-polyrhythm.mp3
//         out/menuband-polyrhythm.notes.json   (lanes preserved)
//         out/menuband-polyrhythm.score.json   (sections + rhythms per lane)
//
// Run:  node pop/menuband/bin/render-polyrhythm-jingle.mjs   (from repo root)

import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { writeFileSync } from "node:fs";
import { renderLullaby, m } from "../../marimba/lullabies/lib/core.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT_DIR = resolve(HERE, "..", "out");

const CYCLE = 2.4;                       // one full polyrhythm cycle (75 BPM · 3)
const BPM = 75;                          // the app's trainer default

// Chord tone tables — one pitch cell per lane per chord, all white keys.
// Each cell has exactly `count` pitches: hit i of a cycle plays cell[i].
const CHORDS = {
  C:  { three: ["C5", "E5", "G5"], two: ["C3", "G3"], four: ["C4", "E4", "G4", "C5"], five: ["C6", "D6", "E6", "G6", "A6"], bass: "C2" },
  G:  { three: ["B4", "D5", "G5"], two: ["G2", "D3"], four: ["G3", "B3", "D4", "G4"], five: ["G5", "A5", "B5", "D6", "G6"], bass: "G2" },
  Am: { three: ["A4", "C5", "E5"], two: ["A2", "E3"], four: ["A3", "C4", "E4", "A4"], five: ["A5", "B5", "C6", "E6", "A6"], bass: "A2" },
  F:  { three: ["A4", "C5", "F5"], two: ["F2", "C3"], four: ["F3", "A3", "C4", "F4"], five: ["F5", "G5", "A5", "C6", "F6"], bass: "F2" },
};

// One lane per rhythm: count, timbre, and the icon-palette color the sim's
// circle wears (reel-lib KEY_COLORS — brand-consistent, per-rhythm).
const RHYTHMS = {
  three: { count: 3, preset: "vibraphone",   gain: 0.42, ring: 1.15, decayMul: 1.25, color: [255, 77, 107] },
  two:   { count: 2, preset: "gamelan",      gain: 0.30, ring: 0.95, decayMul: 1.10, color: [97, 158, 255] },
  four:  { count: 4, preset: "kalimba",      gain: 0.24, ring: 0.90, decayMul: 1.00, color: [255, 153, 46] },
  five:  { count: 5, preset: "glockenspiel", gain: 0.14, ring: 1.30, decayMul: 1.30, color: [51, 209, 179] },
};

// The escalation: which lanes sound, over which chords, per section.
const SECTIONS = [
  { name: "solo", label: "3",     lanes: ["three"],                 chords: ["C", "C"] },
  { name: "duo",  label: "3:2",   lanes: ["three", "two"],          chords: ["C", "C", "G"] },
  { name: "trio", label: "3:4",   lanes: ["three", "four"],         chords: ["Am", "F", "Am"] },
  { name: "full", label: "3:4:5", lanes: ["three", "four", "five"], chords: ["F", "G", "C"] },
];

const events = [];
const push = (lane, preset, startSec, note, durSec, gain, pan = 0, decayMul = 1.2) => {
  events.push({ lane, preset, startSec: +startSec.toFixed(4), midi: m(note), durSec: +durSec.toFixed(4), gain, pan, decayMul });
};

// Pan each lane toward where its circle sits in that section's row.
const lanePan = (lanes, lane) => {
  if (lanes.length < 2) return 0;
  const i = lanes.indexOf(lane);
  return +(((i / (lanes.length - 1)) - 0.5) * 0.44).toFixed(3);
};

let t = 0;
const sections = [];
for (const S of SECTIONS) {
  const t0 = t;
  S.chords.forEach((chordName, c) => {
    const chord = CHORDS[chordName];
    const ct = t0 + c * CYCLE;
    // the bed: a bass root each cycle + a soft woodblock tick on the downbeat
    push("bed", "bass", ct, chord.bass, 2.0, 0.26, 0, 1.1);
    if (S.name !== "solo") push("tick", "woodblock", ct, "E5", 0.12, 0.055, 0.1, 0.9);
    // the rhythms: lane k plays `count` evenly spaced hits per cycle
    for (const lane of S.lanes) {
      const R = RHYTHMS[lane];
      const step = CYCLE / R.count;
      for (let i = 0; i < R.count; i++) {
        push(lane, R.preset, ct + i * step, chord[lane][i], step * R.ring, R.gain, lanePan(S.lanes, lane), R.decayMul);
      }
    }
  });
  t = t0 + S.chords.length * CYCLE;
  sections.push({
    name: S.name, label: S.label, t0: +t0.toFixed(4), t1: +t.toFixed(4),
    cycles: S.chords.length,
    rhythms: S.lanes.map((lane) => ({ lane, ...RHYTHMS[lane], preset: RHYTHMS[lane].preset })),
  });
}

// ── the unison — every grid's dot 0, at once: one spread C, ringing out ────
const END = t;                                            // 26.4
push("bed", "bass", END, "C2", 3.4, 0.32, 0, 1.5);
for (const [i, n] of ["C5", "E5", "G5"].entries()) push("three", "vibraphone", END + i * 0.05, n, 3.0, 0.36, -0.12 + i * 0.12, 1.6);
for (const [i, n] of ["C4", "E4", "G4"].entries()) push("four", "kalimba", END + i * 0.04, n, 2.4, 0.20, 0.1, 1.4);
push("five", "glockenspiel", END, "C6", 3.2, 0.15, 0.26, 1.6);
push("five", "glockenspiel", END + 0.08, "G6", 2.8, 0.09, 0.34, 1.6);

// ── master + scores ────────────────────────────────────────────────────────
const NAME = "menuband-polyrhythm";
const { mp3, durationSec } = renderLullaby(events, {
  name: NAME, here: HERE, out: resolve(OUT_DIR, `${NAME}.mp3`),
  healing: false, reverb: { wet: 0.24, decay: 0.8, damp: 0.4 },
  fadeIn: 0.3, fadeOut: 2.0, tailSec: 2.4,
  title: "Menu Band Polyrhythm Trainer",
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);

const notes = events
  .map((e) => ({ t: e.startSec, dur: e.durSec, midi: e.midi, vel: +e.gain.toFixed(3), lane: e.lane }))
  .sort((a, b) => a.t - b.t || a.midi - b.midi);
writeFileSync(resolve(OUT_DIR, `${NAME}.notes.json`), JSON.stringify({
  bpm: BPM, cycleSec: CYCLE, durationSec: +durationSec.toFixed(4), notes,
}, null, 2));
writeFileSync(resolve(OUT_DIR, `${NAME}.score.json`), JSON.stringify({
  bpm: BPM, cycleSec: CYCLE, durationSec: +durationSec.toFixed(4),
  end: +END.toFixed(4), sections,
}, null, 2));
console.log(`  ${sections.length} sections · unison @ ${END.toFixed(1)}s · ${notes.length} notes`);
