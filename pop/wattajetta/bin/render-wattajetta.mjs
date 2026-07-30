#!/usr/bin/env node
// render-wattajetta.mjs — the composition. A fighter jet made entirely
// of water, ~3 minutes. Starts ON the drop. The bell runs are our
// physically-modeled FEM bells and over the flight the water hardens:
// glass → bronze → steel → stone (a granite lithophone material added
// to the engine for this track), decays tightening as it goes. A
// turntablist rides the whole thing — scratch gestures scrub slices of
// the track itself, a global warp pass drags the platter and wobbles
// the record out.
//
// Novel voices this cut:
//   chimes     — tubular-geometry bells in quick clusters (wind chimes)
//   bloops     — rising sine chirps, actual water-drip percussion
//   water choir— formant-shaped sine stacks breathing "ooh" in the breaths
//   underwater — bells resampled through a slow LFO, heard through water
//   church     — one huge church-geometry toll at the coda
//
//   node pop/wattajetta/bin/render-wattajetta.mjs          → out/wattajetta.mp3
//   node pop/wattajetta/bin/render-wattajetta.mjs --world  → separate spatial/rhythmic audition
//   node pop/wattajetta/bin/render-wattajetta.mjs --next   → 1:24 pickup / powersaw audition
//   node pop/wattajetta/bin/render-wattajetta.mjs --score  → print the engine score
//
// Arc (bars):
//   0–11   drop A   glass bells · silver bowls
//   12–15  breath 1 chimes · flyby · platter drag · baby-scratch at 11.5
//   16–27  drop B   glass + bloops · super scratch at 15
//   28–31  breath 2 water choir enters · bronze toll · slow scrub
//   32–43  drop C   bronze, tighter decays · brass bowls · bloops denser
//   44–47  breath 3 platter drag · steel toll · choir swells
//   48–59  drop D   steel, tight runs · super scratch 2 at 47 · flyby
//   60–63  breath 4 underwater bells · stone toll · choir
//   64–75  drop E   STONE — granite runs + stone bowls, everything in
//   76–79  coda     church bell tolls · kick thins to downbeats
//   80–95  mist     stone tolls · vinyl wobble · underwater scrub ·
//                   one last glass bell closing the circle

import { writeFileSync, readFileSync, mkdirSync, unlinkSync, existsSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { renderBell } from "../../lib/bell.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../out");
mkdirSync(OUT, { recursive: true });
const BELL_CACHE = resolve(OUT, ".wattajetta-bell-cache-v1");
mkdirSync(BELL_CACHE, { recursive: true });
const NEXT = process.argv.includes("--next");
const WORLD = NEXT || process.argv.includes("--world");

const SR = 48000;
const TAU = 2 * Math.PI;
const BPM = 138;
const BEAT = 60 / BPM;
const BAR = BEAT * 4;
const INTRO_BARS = WORLD ? 4 : 0;
const BARS = WORLD ? 88 : 96;
const DUR = (INTRO_BARS + BARS) * BAR + (WORLD ? 3.2 : 5.5);

// deterministic sprinkle — same track every bake
let _s = 0xa7757e77;
const rnd = () => ((_s = (_s * 1664525 + 1013904223) >>> 0) / 4294967296);
let _human = 0xa9aa0e11;
const humanRnd = () => ((_human = (_human * 1664525 + 1013904223) >>> 0) / 4294967296);

// e minor pentatonic — the bell runs live here. One octave down from
// the first cuts: E5–E6 read piercing-tangy on laptop speakers
const DROP_NOTES = ["E4", "G4", "A4", "B4", "D5", "E5"];
const CHIME_NOTES = ["E5", "G5", "A5", "B5", "D6"];
const BLOOP_HZ = [164.81, 196.0, 220.0, 246.94, 293.66]; // e3 pentatonic
// sub roots: e1 g1 a1 d2 under each 2-bar phrase
const ROOTS = [41.203, 48.999, 55.0, 73.416];
const ROOT_BELLS = ["E4", "G4", "A4", "D5"];
const NOTE_PC = { C: 0, "C#": 1, D: 2, "D#": 3, E: 4, F: 5, "F#": 6,
                  G: 7, "G#": 8, A: 9, "A#": 10, B: 11 };
function noteHz(note) {
  const m = /^([A-G](?:#)?)(-?\d)$/.exec(note);
  const midi = (Number(m[2]) + 1) * 12 + NOTE_PC[m[1]];
  return 440 * 2 ** ((midi - 69) / 12);
}

const kicks = [];  // t0 f0 f1 sweep ampDb hole decay
const sines = [];  // t0 dur f0 f1 ampDb atk rel pan0 pan1 vibHz vibCents
const noises = []; // t0 dur f0 f1 q peakDb atk rel pan
const bells = [];  // { t, note, vel, pan, gain, material, geometry, dur, warp? }

const bar = (n) => (n + INTRO_BARS) * BAR;
const introBar = (n) => n * BAR;

// the flight plan: each drop hardens the water a little more
const DROPS = [
  { a: 0,  z: 12, mat: "glass",  bowl: "silver", durs: [3.2, 3.2, 1.4], density: 0.85, gainDb: -14, bloops: false },
  { a: 16, z: 28, mat: "glass",  bowl: "silver", durs: [3.2, 1.4, 0.9], density: 0.9,  gainDb: -13, bloops: 0.4 },
  { a: 32, z: 44, mat: "bronze", bowl: "brass",  durs: [1.8, 1.8, 1.0], density: 0.9,  gainDb: -13, bloops: 0.6 },
  { a: 48, z: 60, mat: "steel",  bowl: "gold",   durs: [1.2, 1.2, 0.7], density: 0.95, gainDb: -12.5, bloops: 0.6 },
  { a: 64, z: 76, mat: "stone",  bowl: "stone",  durs: [1.6, 1.0, 0.7], density: 0.95, gainDb: -11.5, bloops: 0.75 },
];
const BREATHS = [12, 28, 44, 60]; // 4-bar rests between drops

// The world audition keeps a pulse in every breath and adds weather without
// importing samples: rain, thunder, bubbles, hats, and snares are all made by
// the same sine/noise vocabulary as the water engine.
if (WORLD) {
  const hat = (t, open = false, db = -19) =>
    noises.push([t, open ? 0.22 : 0.045, 7200, open ? 9800 : 8100,
                 open ? 0.7 : 1.7, db, 0.001, open ? 0.18 : 0.035, (rnd() * 2 - 1) * 0.45]);
  const snare = (t, db = -9) => {
    noises.push([t, 0.095, 4300, 1700, 0.8, db, 0.001, 0.075, (rnd() * 2 - 1) * 0.12]);
    noises.push([t + 0.006, 0.055, 6900, 3300, 1.4, db - 3, 0.001, 0.035, (rnd() * 2 - 1) * 0.2]);
    sines.push([t, 0.075, 230, 155, db - 7, 0.002, 0.055, 0, 0, 0, 0]);
  };
  const bubble = (t, lift = 1, db = -21, requestedDur = null) => {
    const f = (420 + rnd() * 720) * lift;
    const dur = requestedDur ?? (0.045 + rnd() ** 1.6 * 0.32);
    const pan0 = (rnd() * 2 - 1) * 0.9;
    const pan1 = Math.max(-0.95, Math.min(0.95, pan0 + (rnd() * 2 - 1) * 0.55));
    sines.push([t, dur, f * (0.48 + rnd() * 0.15), f * (1.3 + rnd() * 1.15),
                db, Math.min(0.012, dur * 0.18), Math.max(0.025, dur * 0.72),
                pan0, pan1, 0, 0]);
  };

  // Four bars establish the world before the original opening drop: distant
  // rain, a submerged E pedal, a bell crossing into staccato form, bubbles,
  // then an eighth-note hat pickup that hands the listener to bar zero.
  for (const [mul, db] of [[1, -25], [2, -31], [3, -35], [4, -39]])
    sines.push([introBar(0), 4 * BAR, 82.407 * mul, 82.407 * mul, db,
                1.3, 1.5, -0.25 + mul * 0.1, 0.25 - mul * 0.08, 2.1, 5]);
  bells.push({ t: introBar(0.35), note: "E4", vel: 0.68, pan: -0.35,
               gain: Math.pow(10, -15 / 20), material: "glass",
               geometry: "bowl", dur: 6.2, morph: true });
  for (let b = 0; b < 4; b++) {
    bubble(introBar(b) + (1.1 + 0.55 * b) * BEAT, 1 + b * 0.12, -23 + b);
    if (b >= 2) for (let e = 0; e < 8; e++)
      hat(introBar(b) + e * 0.5 * BEAT, e === 7, -22 + 2 * (b - 2));
  }

  // Closed eighths keep time through every nominal break; the last upbeat
  // opens like spray. They become more present around the 1:40 passage.
  for (const b of BREATHS) {
    for (let e = 0; e < 8; e++)
      hat(bar(b) + e * 0.5 * BEAT + (NEXT && e % 2 ? 0.105 * BEAT : 0)
          + (NEXT ? (humanRnd() - 0.5) * 0.011 : 0), false, b >= 44 ? -16.5 : -19);
    for (let e = 0; e < 8; e++)
      hat(bar(b + 2) + e * 0.5 * BEAT + (NEXT && e % 2 ? 0.105 * BEAT : 0)
          + (NEXT ? (humanRnd() - 0.5) * 0.011 : 0), e === 7, b >= 44 ? -15.5 : -18);
  }
  for (let b = 53; b < 64; b++)
    for (let e = 0; e < 8; e++)
      hat(bar(b) + e * 0.5 * BEAT + (NEXT && e % 2 ? 0.105 * BEAT : 0)
          + (NEXT ? (humanRnd() - 0.5) * 0.011 : 0), e === 7, -17);

  if (NEXT) {
    // "t t sssssssss tt t": two planted ticks, a tightening 32nd-note
    // spray, then a three-hit answer. The phrase repeats through the steel
    // drop without replacing its four-on-the-floor footing.
    const trapSteps = [0, 4, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 13, 14, 15];
    for (let b = 48; b < 60; b += 2)
      for (const step of trapSteps)
        hat(bar(b) + step * 0.25 * BEAT
            + (Math.floor(step) % 4 >= 2 ? 0.055 * BEAT : 0)
            + (humanRnd() - 0.5) * 0.009,
            step === 12, step >= 8 && step <= 12 ? -14 : -16.5);
  }

  // A clipped snare voice enters at 1:25 in the four-bar-intro cut, then
  // persists on 2/4 into the next breath so the beat never disappears.
  for (let b = 45; b < 64; b++) {
    snare(bar(b) + BEAT, b < 57 ? -9.5 : -8.5);
    snare(bar(b) + 3 * BEAT, b < 57 ? -9 : -8);
  }

  // Particle clouds: deeper bodies, many lengths, and occasional high
  // pinpricks. The clustered timing feels like material moving through water.
  for (let b = 12; b < BARS; b += 2) {
    const count = b % 8 === 4 ? 9 : b >= 44 ? 6 : 4;
    const origin = bar(b) + (0.7 + rnd() * 1.7) * BEAT;
    const spread = (0.45 + rnd() * 0.95) * BEAT;
    for (let p = 0; p < count; p++) {
      const highPinprick = p === count - 1 && b % 4 === 0;
      const lift = highPinprick ? 1.8 + rnd() * 0.7 : 0.65 + rnd() * 0.8;
      bubble(origin + rnd() * spread, lift, -22 - rnd() * 5,
             0.045 + rnd() ** 1.7 * (highPinprick ? 0.12 : 0.34));
    }
  }

  // Feed-forward storm gestures: broad rain bands and low thunder swells.
  noises.push([0, DUR, 9200, 3600, 0.42, -38, 3.0, 5.0, -0.35]);
  noises.push([0, DUR, 7600, 2800, 0.5, -39, 5.0, 7.0, 0.35]);
  for (const b of [14, 30, 46, 61.5, 78, 86]) {
    noises.push([bar(b), 3.4, 180, 52, 0.48, -23, 0.08, 2.8, 0]);
    sines.push([bar(b), 3.2, 62, 27, -19, 0.08, 2.7, -0.12, 0.12, 1.7, 5]);
  }
}

// ── kicks: halftime 1+3 early (the part we loved), then the flight
//    ramps — drop C flips to four-on-the-floor halfway, and the steel
//    and stone drops drive full trance 4/4 ────────────────────────────
const kickAt = (t) => kicks.push([
  t, WORLD ? 108 : 118, WORLD ? 34 : 41, WORLD ? 0.095 : 0.075,
  WORLD ? -2.4 : -2, 0.012, WORLD ? 0.38 : 0.28,
]);
for (const d of DROPS)
  for (let b = d.a; b < d.z; b++) {
    const fourFloor = d.a >= 48 || (d.a === 32 && b >= 38);
    if (fourFloor) for (let k = 0; k < 4; k++) kickAt(bar(b) + k * BEAT);
    else { kickAt(bar(b)); kickAt(bar(b) + 2 * BEAT); }
  }
// coda: the downbeat only, letting the stone ring between hits
for (let b = 76; b < 80; b++) kickAt(bar(b));
// Reverse-kick inhalations pull into the major world-cut landings.
if (WORLD) for (const b of [12, 16, 28, 32, 44, 48, 60, 64, 76]) {
  const land = bar(b);
  sines.push([land - 0.62, 0.62, 34, 108, -12.5, 0.56, 0.012, 0, 0, 0, 0]);
  noises.push([land - 0.48, 0.48, 170, 1250, 0.55, -25, 0.42, 0.01, 0]);
}

// ── sub bass: the fuselage — root + quiet octave, one note per 2 bars ─
function subNote(t, dur, freq, db) {
  sines.push([t, dur, freq, freq, db, 0.06, 0.25, 0, 0, 0, 0]);
  sines.push([t, dur, freq * 2, freq * 2, db - 7, 0.06, 0.25, 0, 0, 4.5, 6]);
}
for (const d of DROPS)
  for (let b = d.a; b < d.z; b += 2) {
    const root = ROOTS[((b - d.a) / 2) % 4];
    if (d.a >= 48) {
      // trance gallop: offbeat eighth stabs — the sidechain makes the pump
      for (let bb = b; bb < b + 2; bb++)
        for (let k = 0; k < 4; k++)
          sines.push([bar(bb) + (k + 0.5) * BEAT, 0.16, root * 2, root * 2, -8, 0.005, 0.08, 0, 0, 0, 0]);
      sines.push([bar(b), 2 * BAR - 0.08, root, root, -9, 0.06, 0.25, 0, 0, 0, 0]); // sub floor stays
    } else {
      subNote(bar(b), 2 * BAR - 0.08, root, -7.5);
    }
  }
subNote(bar(76), 4 * BAR - 0.1, ROOTS[0], -9); // coda holds the root
// each breath: the sub lets go and rises an octave into vapor
for (const b of BREATHS)
  sines.push([bar(b), 4 * BAR, 41.203, 82.407, -11, 1.0, 2.2, 0, 0, 0, 0]);

if (NEXT) {
  // Throat/thwub bass: a centered pitch dive carries a moving harmonic mouth.
  // The sub floor stays mono; the upper partials make the bass pronounce each
  // beat instead of merely sitting beneath it.
  const throatThwub = (t, root, db = -6.5) => {
    const dur = 0.48;
    sines.push([t, dur, root * 1.55, root, db, 0.004, 0.28, 0, 0, 2.8, 5]);
    for (let h = 2; h <= 9; h++) {
      const hz = root * h;
      const mouth = Math.exp(-(((hz - 260) / 170) ** 2))
                  + 0.72 * Math.exp(-(((hz - 720) / 310) ** 2));
      const hdb = db - 8 + 20 * Math.log10(Math.max(0.035, mouth) / h);
      if (hdb > -48)
        sines.push([t, dur * 0.88, hz * 1.7, hz, hdb, 0.003, 0.2, 0, 0, 7.2, 11]);
    }
    noises.push([t, dur * 0.75, 620, 145, 2.2, db - 12, 0.003, 0.24, 0]);
  };
  for (let b = 48; b < 64; b++) {
    const root = ROOTS[Math.floor((b - 48) / 2) % ROOTS.length];
    for (const beat of [0, 1.5, 2.5, 3.5]) throatThwub(bar(b) + beat * BEAT, root, b === 48 ? -4.8 : -6.5);
  }

  // The new cold-open lands on bar 48. Give it a sub impact and a broadband
  // metal-water crash after a short reverse inhale.
  sines.push([bar(48), 1.15, 68, 29, -1.5, 0.002, 0.72, 0, 0, 0, 0]);
  sines.push([bar(48), 2.4, 41.203, 41.203, -5.2, 0.008, 1.9, 0, 0, 1.7, 4]);
  noises.push([bar(48), 1.8, 11200, 520, 0.55, -10.5, 0.001, 1.35, 0]);
}

// ── bell runs: eighth-note pentatonic random walks; decay varies per
//    strike (a few land choked), material follows the flight plan ──────
function bellRun(b, d) {
  let idx = Math.floor(rnd() * DROP_NOTES.length);
  for (let e = 0; e < 8; e++) {
    idx = Math.max(0, Math.min(DROP_NOTES.length - 1, idx + (rnd() < 0.5 ? -1 : 1)));
    if (rnd() < d.density * (WORLD ? 0.82 : 1))
      bells.push({
        t: bar(b) + e * 0.5 * BEAT,
        note: DROP_NOTES[idx],
        vel: 0.55 + rnd() * 0.3,
        pan: (rnd() * 2 - 1) * 0.7,
        gain: Math.pow(10, d.gainDb / 20),
        material: d.mat, geometry: "glass",
        dur: WORLD ? [0.42, 0.68, 0.95][Math.floor(rnd() * 3)]
                   : d.durs[Math.floor(rnd() * d.durs.length)],
        ...(WORLD ? { renderDur: 0.95 } : {}),
        crush: WORLD && rnd() < 0.18,
      });
  }
}
for (const d of DROPS) for (let b = d.a; b < d.z; b++) bellRun(b, d);

// ── bloops: actual water-drip percussion — a sine chirping UP into its
//    note the way a drip rings a pool, on the swung offbeats ────────────
function bloop(t, hz, db) {
  sines.push([t, 0.11, hz * 0.55, hz, db, 0.006, 0.07, (rnd() * 2 - 1) * 0.5, 0, 0, 0]);
}
for (const d of DROPS) {
  if (!d.bloops) continue;
  for (let b = d.a; b < d.z; b++)
    for (const slot of [1.75, 3.25, 3.75])
      if (rnd() < d.bloops) bloop(bar(b) + slot * BEAT, BLOOP_HZ[Math.floor(rnd() * BLOOP_HZ.length)], -17);
}

// ── water choir: formant-shaped sine stacks — an "ooh" breathed out of
//    pure sines. Harmonic amplitudes follow /u/ vowel resonances;
//    detuned pairs beat slowly like the surface of held water ──────────
const FORMANTS = [[300, 170], [870, 260], [2250, 430]]; // [center, bandwidth] — wide = breathy
function choirNote(t, dur, f0, db, pan) {
  for (let h = 1; h <= 8; h++) {
    const fh = f0 * h;
    let a = 0;
    for (const [fc, bw] of FORMANTS) a += Math.exp(-(((fh - fc) / bw) ** 2));
    a = Math.max(a, 0.04) / h; // spectral tilt keeps it breathy, not brassy
    const hdb = db + 20 * Math.log10(a);
    if (hdb < -60) continue;
    for (const det of [-5, 0, 5]) // detuned trio — thick slow beating
      sines.push([t, dur, fh * Math.pow(2, det / 1200), fh * Math.pow(2, det / 1200),
                  hdb - 9, dur * 0.35, dur * 0.4, pan, pan, 4.8, 7]);
  }
}
function choirChord(t, dur, db) {
  choirNote(t, dur, 82.407, db - 2, 0);      // e2
  choirNote(t, dur, 123.47, db - 3, -0.35);  // b2
  choirNote(t, dur, 164.81, db, 0.35);       // e3
  choirNote(t, dur, 196.0, db - 4, -0.15);   // g3 — the minor color
}
choirChord(bar(28), 4 * BAR, -19);
choirChord(bar(44), 4 * BAR, -17);
choirChord(bar(60), 4 * BAR, -17);
choirChord(bar(80), 8 * BAR, -20); // the mist hums it one last time

// ── chimes: tubular bells in quick clusters — wind chimes on the
//    canopy rail, breath sections and the mist ─────────────────────────
function chimeCluster(t, count, db) {
  let tt = t;
  for (let i = 0; i < count; i++) {
    bells.push({ t: tt, note: CHIME_NOTES[Math.floor(rnd() * CHIME_NOTES.length)],
                 vel: 0.35 + rnd() * 0.25, pan: (rnd() * 2 - 1) * 0.8,
                 gain: Math.pow(10, db / 20),
                 material: "aluminum", geometry: "tubular", dur: WORLD ? 1.35 : 2.2,
                 crush: WORLD && i % 3 === 1 });
    tt += 0.03 + rnd() * 0.09;
  }
}
for (const b of BREATHS) { chimeCluster(bar(b) + BEAT, 5, -20); chimeCluster(bar(b + 2) + 2 * BEAT, 4, -22); }
for (const b of [82, 86, 91]) chimeCluster(bar(b), 3, -23);

// Sparse upper-register "bing bing" answers: short steel/glass glocks,
// locked to E minor pentatonic so sparkle never becomes broadband haze.
if (WORLD) {
  const glockNotes = ["E6", "G6", "A6", "B6", "D7"];
  for (let b = 6, phrase = 0; b < 80; b += 4, phrase++) {
    const first = glockNotes[phrase % glockNotes.length];
    const second = glockNotes[(phrase + (phrase % 2 ? 2 : 1)) % glockNotes.length];
    const t0 = bar(b) + (phrase % 2 ? 1.5 : 0.75) * BEAT;
    bells.push({ t: t0, note: first, vel: 0.5, pan: -0.48,
                 gain: Math.pow(10, -21 / 20), material: "steel", geometry: "glass",
                 dur: 0.68, renderDur: 0.68, crush: phrase % 3 === 1 });
    bells.push({ t: t0 + 0.5 * BEAT, note: second, vel: 0.46, pan: 0.48,
                 gain: Math.pow(10, -22 / 20), material: "steel", geometry: "glass",
                 dur: 0.54, renderDur: 0.68, crush: phrase % 3 === 1 });
  }
}

// ── bowls: a low anchor each 2-bar downbeat, material morphing too ────
for (const d of DROPS)
  for (let b = d.a; b < d.z; b += 2)
    bells.push({ t: bar(b), note: ROOT_BELLS[((b - d.a) / 2) % 4],
                 vel: 0.7, pan: (b / 2) % 2 === 0 ? -0.2 : 0.2,
                 gain: Math.pow(10, -16 / 20),
                 material: WORLD ? "silver" : d.bowl, geometry: "bowl", dur: WORLD ? 2.75 : 4.5,
                 morph: WORLD && ((b - d.a) % 4 === 0),
                 crush: WORLD && ((b - d.a) / 2) % 3 === 1 });

// ── breaths, coda, mist: long bowls + tolls; the next material always
//    tolls once before its drop arrives; breath 4 bells come through
//    water (warp = slow LFO resample) ──────────────────────────────────
const longBowl = (t, note, mat, db, dur = 7, extra = {}) =>
  bells.push({ t, note, vel: 0.8, pan: 0, gain: Math.pow(10, db / 20),
               material: mat, geometry: "bowl", dur: WORLD ? Math.min(dur, 4.4) : dur, ...extra });
longBowl(bar(12), "E4", "silver", -15, 7, { morph: WORLD });
longBowl(bar(44), "E4", "brass", -15, 7, { morph: WORLD });
longBowl(bar(60), "E4", "gold", -15, 7, { warp: { depth: 0.03, hz: 0.7 }, morph: WORLD });
longBowl(bar(76), "E3", "stone", -14, 8, { morph: WORLD });
longBowl(bar(80), "E4", "silver", -17, WORLD ? 6 : 9,
         { warp: { depth: 0.035, hz: 0.55 }, morph: WORLD });
// foreshadow tolls — the ear learns each material before its drop
const foreshadow = [[30, "E5", "bronze"], [46, "B5", "steel"], [62, "G5", "stone"]];
for (const [b, n, m] of foreshadow)
  bells.push({ t: bar(b), note: n, vel: 0.6, pan: 0.3, gain: Math.pow(10, -16 / 20),
               material: m, geometry: "glass", dur: WORLD ? 1.6 : 2.5, crush: WORLD });
// breath sparkles — breath 4's come through water
for (const b of [13.5, 14.5, 29, 45.5, 61, 62.5])
  bells.push({ t: bar(b), note: DROP_NOTES[Math.floor(rnd() * DROP_NOTES.length)],
               vel: 0.5, pan: (rnd() * 2 - 1) * 0.5, gain: Math.pow(10, -17 / 20),
               material: "glass", geometry: "glass", dur: WORLD ? 1.8 : 3.2,
               ...(b >= 60 ? { warp: { depth: 0.04, hz: 0.8 } } : {}) });
// coda: the church bell — one huge toll, then its echo
bells.push({ t: bar(76), note: "E3", vel: 0.9, pan: 0, gain: Math.pow(10, -13 / 20),
             material: "bronze", geometry: "church", dur: WORLD ? 5 : 9, crush: WORLD });
bells.push({ t: bar(78), note: "E3", vel: 0.55, pan: 0.15, gain: Math.pow(10, -17 / 20),
             material: "bronze", geometry: "church", dur: WORLD ? 4 : 7 });
// mist: stone tolls slowing, underwater, one last glass bell full circle
const MIST_TOLLS = WORLD
  ? [[80, "E5"], [81, "G5"], [82.5, "A5"], [84, "E5"], [86, "D6"]]
  : [[80, "E5"], [81, "G5"], [82.5, "A5"], [84, "E5"], [86, "D6"], [88.5, "G5"], [91, "A5"]];
for (const [b, n] of MIST_TOLLS)
  bells.push({ t: bar(b), note: n, vel: 0.55, pan: (rnd() * 2 - 1) * 0.4,
               gain: Math.pow(10, (-16 - (b - 80) * 0.4) / 20),
               material: "stone", geometry: "glass", dur: WORLD ? 1.25 : 2.0,
               warp: { depth: 0.03, hz: 0.6 }, crush: WORLD && b % 2 === 0 });
bells.push({ t: bar(WORLD ? 87 : 93), note: "E6", vel: 0.45, pan: 0, gain: Math.pow(10, -19 / 20),
             material: "glass", geometry: "glass", dur: WORLD ? 2.2 : 3.2,
             warp: { depth: 0.05, hz: 0.5 } });

if (NEXT) {
  // One passage of particles follows the exact bell contour: each selected
  // strike is approached from below, lands on its pitch, and releases upward.
  for (const strike of bells.filter((s) => s.t >= bar(52) && s.t < bar(56)
                                             && s.geometry === "glass")) {
    const target = noteHz(strike.note);
    const dur = 0.09 + 0.08 * rnd();
    sines.push([strike.t + 0.035, dur, target * (0.46 + 0.08 * rnd()), target * 1.04,
                -16.5 - 3 * rnd(), 0.004, dur * 0.72, strike.pan * 0.8, -strike.pan * 0.55, 0, 0]);
  }
}

// ── flybys: sine dopplers at the seams, quiet ─────────────────────────
function flyby(t, dir) {
  const d = 2 * BAR;
  sines.push([t, d, 760, 185, -17, d * 0.4, d * 0.45, -dir, dir, 0, 0]);
}
flyby(bar(13), 1);
flyby(bar(29), -1);
flyby(bar(53), 1);
flyby(bar(70), -1);

// ── spray, whisper level: a steam breath at each drop, an exhale out ──
for (const d of DROPS) noises.push([bar(d.a), 2.0, 2000, 300, 0.8, -23, 0.015, 1.8, 0]);
noises.push([bar(80), (WORLD ? 8 : 16) * BAR + 2, 3000, 500, 0.8, -27,
             2.0, (WORLD ? 6 : 14) * BAR, 0]);
noises.push([0, DUR, 1100, 1100, 0.6, -41, 4, 4, 0]);

// ── bake + render the engine part ─────────────────────────────────────
const fmt = (rows) => rows.map((r) => "  " + r.map((v) => +v.toFixed(5)).join(" ")).join("\n");
const score = [
  `sr ${SR}`,
  `dur ${DUR.toFixed(3)}`,
  `normpeak 0.82`,
  `fadein 0.004`,
  `fadeout 4.0`,
  `sidechain 0.015 0.2 -6`,
  `kick ${kicks.length}`, fmt(kicks),
  `sine ${sines.length}`, fmt(sines),
  `noise ${noises.length}`, fmt(noises),
].join("\n") + "\n";

const scorePath = resolve(OUT, "wattajetta.score.txt");
writeFileSync(scorePath, score);
if (process.argv.includes("--score")) { console.log(score); process.exit(0); }
console.log(`baked ${kicks.length} kicks, ${sines.length} sines, ${noises.length} sprays, ${bells.length} bells`);

const rawPath = resolve(OUT, "wattajetta.f32.raw");
const kickPath = resolve(OUT, "wattajetta.kick.f32.raw");
const r = spawnSync("node", [resolve(HERE, "../c/run-c.mjs"), scorePath, "--raw", rawPath, "--kickraw", kickPath], { stdio: "inherit" });
if (r.status !== 0) process.exit(1);

// ── mix the bells over the engine render ──────────────────────────────
// The kick rides its own bus: scratches and ducks only ever touch the
// musical bus, so the instinctual rhythm is never destroyed — we layer,
// we don't overwrite. One FEM render per unique voicing (cached),
// copies mixed per strike with equal-power pan. A strike with `warp` is
// resampled through a slow sine LFO first — a bell heard through water.
const raw = readFileSync(rawPath);
const mix = new Float32Array(raw.buffer, raw.byteOffset, raw.length / 4);
const kraw = readFileSync(kickPath);
const kickBus = new Float32Array(kraw.buffer, kraw.byteOffset, kraw.length / 4);
const ns = mix.length / 2;
const smooth = (p) => p * p * (3 - 2 * p);

// ── crunch: the water hardens sonically too — a tanh waveshaper whose
//    blend ramps in across drops C→E, eases off for the coda and mist.
//    It runs HERE, on the engine bus and the kick only, BEFORE the
//    bells and vocals layer in — distorted sines and kicks get teeth,
//    the bells stay pure (crunched bells read tangy on small speakers) ─
{
  const DRIVE = 2.4;
  const norm = Math.tanh(DRIVE);
  const blendAt = (t) => {
    if (t < bar(24)) return 0;
    if (t < bar(64)) return 0.85 * smooth((t - bar(24)) / (bar(64) - bar(24)));
    if (t < bar(76)) return 0.85;
    if (t < bar(80)) return 0.85 - 0.6 * smooth((t - bar(76)) / (4 * BAR));
    return 0.15;
  };
  for (let f = 0; f < ns; f++) {
    const m = blendAt(f / SR);
    if (m <= 0) continue;
    for (let ch = 0; ch < 2; ch++) {
      const x = mix[2 * f + ch];
      mix[2 * f + ch] = x * (1 - m) + (Math.tanh(x * DRIVE) / norm) * m;
      const k = kickBus[2 * f + ch];
      kickBus[2 * f + ch] = k * (1 - m) + (Math.tanh(k * DRIVE) / norm) * m;
    }
  }
}

const bank = new Map();
const bellFor = ({ note, material, geometry, dur, renderDur = dur }) => {
  const k = `${note}/${material}/${geometry}/${renderDur}`;
  if (!bank.has(k)) {
    const stem = k.replace(/[^a-z0-9.-]+/gi, "-");
    const leftPath = resolve(BELL_CACHE, `${stem}-L.f32`);
    const rightPath = resolve(BELL_CACHE, `${stem}-R.f32`);
    if (existsSync(leftPath) && existsSync(rightPath)) {
      const readFloat = (path) => {
        const b = readFileSync(path);
        return new Float32Array(b.buffer.slice(b.byteOffset, b.byteOffset + b.byteLength));
      };
      bank.set(k, { L: readFloat(leftPath), R: readFloat(rightPath) });
      console.log(`  bell ${note} (${material}/${geometry} ×${renderDur}s) cache`);
    } else {
      console.log(`  bell ${note} (${material}/${geometry} ×${renderDur}s)…`);
      const rendered = renderBell({ note, material, geometry, dur: renderDur });
      writeFileSync(leftPath, Buffer.from(rendered.L.buffer, rendered.L.byteOffset, rendered.L.byteLength));
      writeFileSync(rightPath, Buffer.from(rendered.R.buffer, rendered.R.byteOffset, rendered.R.byteLength));
      bank.set(k, rendered);
    }
  }
  return bank.get(k);
};

function underwater(buf, depth, hz) {
  const out = new Float32Array(buf.length);
  let pos = 0;
  for (let i = 0; i < out.length; i++) {
    const j = Math.floor(pos);
    if (j >= buf.length - 1) break;
    const fr = pos - j;
    out[i] = buf[j] * (1 - fr) + buf[j + 1] * fr;
    pos += 1 + depth * Math.sin(TAU * hz * (i / SR));
  }
  return out;
}

function bellMorphAmount(p) {
  if (p < 0.16 || p > 0.9) return 0;
  if (p < 0.36) return smooth((p - 0.16) / 0.2);
  if (p < 0.62) return 1;
  return 1 - smooth((p - 0.62) / 0.28);
}

// The staccato state is not a mute over the ringing bell. It repeatedly
// excites the bell's own opening waveform, then crossfades back into the
// uninterrupted resonant body, so the listener hears one object change form.
function transformedBellSample(buf, i, n, enabled) {
  if (!enabled || n < 8) return buf[i];
  const m = bellMorphAmount(i / n);
  if (m <= 0) return buf[i];
  const period = Math.max(1, Math.floor(BEAT * 0.5 * SR));
  const phase = i % period;
  const attackN = Math.min(buf.length, Math.floor(0.19 * SR));
  const src = Math.min(attackN - 1, phase);
  const pulse = phase < attackN ? buf[src] * Math.exp(-phase / (0.055 * SR)) * 1.45 : 0;
  return buf[i] * Math.sqrt(1 - m) + pulse * Math.sqrt(m);
}

function worldBellSample(buf, i, n, s) {
  let x = transformedBellSample(buf, i, n, s.morph);
  if (s.crush) {
    const heldAt = Math.min(n - 1, Math.floor(i / 3) * 3);
    const held = transformedBellSample(buf, heldAt, n, s.morph);
    const crushed = Math.round(held * 64) / 64;
    x = x * 0.72 + crushed * 0.28;
  }
  if (WORLD) {
    const time = i / SR;
    const attack = Math.min(1, time / 0.012);
    const body = Math.exp(-time / Math.max(0.45, Math.min(1.8, s.dur * 0.38)));
    const f = noteHz(s.note);
    const tonal = (Math.sin(TAU * f * time) + 0.3 * Math.sin(TAU * f * 2 * time)
                  + 0.12 * Math.sin(TAU * f * 3 * time)) * attack * body;
    x = x * 0.9 + tonal * 0.1;
  }
  return x;
}

for (const s of bells) {
  let { L, R } = bellFor(s);
  if (s.warp) { L = underwater(L, s.warp.depth, s.warp.hz); R = underwater(R, s.warp.depth, s.warp.hz); }
  const at = Math.floor(s.t * SR);
  const a = (s.pan + 1) * 0.25 * Math.PI;
  const gl = Math.cos(a) * s.gain * s.vel;
  const gr = Math.sin(a) * s.gain * s.vel;
  const n = Math.min(L.length, Math.floor(s.dur * SR), ns - at);
  const fade = Math.min(Math.floor(0.08 * SR), Math.floor(n * 0.2)); // choked strikes land soft, never click
  for (let i = 0; i < n; i++) {
    const env = i > n - fade ? (n - i) / fade : 1;
    mix[2 * (at + i)] += worldBellSample(L, i, n, s) * gl * env;
    mix[2 * (at + i) + 1] += worldBellSample(R, i, n, s) * gr * env;
  }
}

// Power saw enters only on the second melodic pass of the new cut. It follows
// the existing bell score, built from two slightly detuned finite saw stacks;
// the first pass therefore remains all throat bass, bubbles, and metal.
function addPowerSaw(buf) {
  const strikes = bells.filter((s) => s.t >= bar(54) && s.t < bar(60)
                                     && s.geometry === "glass"
                                     && DROP_NOTES.includes(s.note));
  for (const s of strikes) {
    const at = Math.floor(s.t * SR);
    const n = Math.min(Math.floor(0.42 * SR), ns - at);
    const f0 = noteHz(s.note) * 0.5;
    const gain = Math.pow(10, -23 / 20);
    let lowL = 0, lowR = 0;
    const lp = 1 - Math.exp(-TAU * 3200 / SR);
    for (let i = 0; i < n; i++) {
      const t = i / SR;
      const a = Math.min(1, t / 0.018);
      const r = Math.min(1, (n - i) / (0.1 * SR));
      const env = a * r * (0.78 + 0.22 * Math.sin(Math.PI * i / n));
      let l = 0, rr = 0;
      const harmonics = Math.min(11, Math.floor(5000 / (f0 * 1.004)));
      for (let h = 1; h <= harmonics; h++) {
        l += Math.sin(TAU * f0 * 0.996 * h * t + 0.17 * h) / h;
        rr += Math.sin(TAU * f0 * 1.004 * h * t - 0.13 * h) / h;
      }
      lowL += lp * (l - lowL);
      lowR += lp * (rr - lowR);
      const pan = 0.28 * Math.sin(TAU * t / 0.84 + s.pan);
      buf[2 * (at + i)] += lowL * env * gain * (0.82 - pan);
      buf[2 * (at + i) + 1] += lowR * env * gain * (0.82 + pan);
    }
  }
}
if (NEXT) addPowerSaw(mix);

let aquaukeBus = null;
let aquaukeMixBus = null;

// Four physically modeled nylon strings in baritone ukulele tuning (D3 G3 B3
// E4). Each pluck is generated by a damped Karplus–Strong waveguide, then
// sampled with a tiny attack-sharp bend and per-fret intonation offset. Quiet
// body modes keep it woody rather than reading as another bright synth.
function addUkuleleString(buf, atSec, midi, gainDb, pan, seed, slideFromMidi = null) {
  const stableFreq = 440 * 2 ** ((midi - 69) / 12);
  const delay = Math.max(8, Math.round(SR / stableFreq));
  const dur = 2.15;
  const rawFrames = Math.floor((dur + 0.08) * SR);
  const raw = new Float32Array(rawFrames);
  const ring = new Float32Array(delay);
  let state = seed >>> 0;
  const localRnd = () => ((state = (state * 1664525 + 1013904223) >>> 0) / 4294967296);
  let excitation = 0;
  for (let i = 0; i < delay; i++) {
    const white = localRnd() * 2 - 1;
    excitation += 0.34 * (white - excitation); // soft nylon/fingertip spectrum
    const pickNotch = 0.72 + 0.28 * Math.sin(Math.PI * i / delay);
    ring[i] = excitation * pickNotch;
  }
  let head = 0, bridge = 0;
  const loss = 0.99935 - Math.min(0.00022, stableFreq / 2200000);
  for (let i = 0; i < rawFrames; i++) {
    const y = ring[head];
    const next = ring[(head + 1) % delay];
    bridge += 0.58 * (((y + next) * 0.5) - bridge);
    ring[head] = bridge * loss;
    raw[i] = y;
    head = (head + 1) % delay;
  }

  const at = Math.floor(atSec * SR);
  const frames = Math.min(Math.floor(dur * SR), ns - at);
  const cents = (localRnd() * 2 - 1) * 2.8; // each fretted sample settles differently
  const gain = Math.pow(10, gainDb / 20);
  const angle = (pan + 1) * Math.PI * 0.25;
  const gl = Math.cos(angle), gr = Math.sin(angle);
  let position = 0, fingerState = (seed ^ 0xf17e2d31) >>> 0, fingerLow = 0;
  const slideCents = slideFromMidi == null ? 0 : (slideFromMidi - midi) * 100;
  for (let i = 0; i < frames; i++) {
    const t = i / SR;
    const j = Math.min(rawFrames - 2, Math.floor(position));
    const fraction = position - j;
    const string = raw[j] * (1 - fraction) + raw[j + 1] * fraction;
    const body = 0.13 * Math.sin(TAU * 132 * t) * Math.exp(-t / 0.52)
               + 0.07 * Math.sin(TAU * 271 * t + 0.4) * Math.exp(-t / 0.36)
               + 0.035 * Math.sin(TAU * 428 * t + 0.9) * Math.exp(-t / 0.25);
    fingerState = (fingerState * 1664525 + 1013904223) >>> 0;
    const fingerWhite = fingerState / 4294967296 * 2 - 1;
    fingerLow += 0.22 * (fingerWhite - fingerLow);
    const fingerThump = 0.19 * Math.sin(TAU * (84 - 21 * Math.min(1, t / 0.07)) * t)
                      * Math.exp(-t / 0.052);
    const fretScrape = (fingerWhite - fingerLow) * (slideFromMidi == null ? 0.018 : 0.065)
                     * Math.exp(-t / (slideFromMidi == null ? 0.025 : 0.14));
    const attack = Math.min(1, t / 0.0025);
    const release = Math.min(1, (frames - i) / (0.07 * SR));
    const sample = (string * 0.74 + body + fingerThump + fretScrape) * attack * release * gain;
    buf[2 * (at + i)] += sample * gl;
    buf[2 * (at + i) + 1] += sample * gr;
    const attackSharp = 7.5 * Math.exp(-t / 0.065);
    const fretSlide = slideCents * Math.exp(-t / 0.12);
    position += 2 ** ((cents + attackSharp + fretSlide) / 1200);
  }
}

if (NEXT) {
  const ukeBus = new Float32Array(mix.length);
  aquaukeBus = ukeBus;
  const renderedUke = new Float32Array(mix.length);
  aquaukeMixBus = renderedUke;
  const strings = [50, 55, 59, 64]; // D3 G3 B3 E4 — baritone uke
  const stringPan = [-0.28, -0.09, 0.09, 0.28];
  const shapes = [
    [2, 0, 0, 0], // Em7: E3 G3 B3 E4
    [0, 0, 0, 3], // G:    D3 G3 B3 G4
    [2, 2, 1, 0], // Am:   E3 A3 C4 E4
    [0, 2, 3, 0], // Dsus2:D3 A3 D4 E4
  ];
  const picking = [1, 2, 0, 3, 2, 1, 3, 0];
  const singleMotif = [52, 55, 57, 59, 62, 59, 57, 55]; // E3 G3 A3 B3 D4 B3 A3 G3
  let grooveSeed = 0xa9aa0e11;
  const grooveRnd = () => ((grooveSeed = (grooveSeed * 1664525 + 1013904223) >>> 0) / 4294967296);
  let phrase = 0;
  for (const b of [...Array.from({ length: 8 }, (_, i) => 52 + i),
                   ...Array.from({ length: 8 }, (_, i) => 66 + i)]) {
    const shape = shapes[phrase % shapes.length];
    const singleVoice = b >= 52 && b < 56;
    if (singleVoice) {
      let previousMidi = singleMotif[(phrase * 2 + 7) % singleMotif.length];
      for (let step = 0; step < 8; step++) {
        const midi = singleMotif[(phrase * 2 + step) % singleMotif.length];
        const swing = step % 2 ? 0.105 * BEAT : 0;
        const humanTime = (grooveRnd() - 0.5) * 0.018;
        const slide = step === 2 || step === 6 ? previousMidi : null;
        addUkuleleString(ukeBus, bar(b) + step * 0.5 * BEAT + swing + humanTime,
                         midi, -17.8 + (grooveRnd() - 0.5) * 1.2,
                         0.22 * Math.sin((phrase * 8 + step) * 0.47),
                         0x511000 + phrase * 37 + step * 11, slide);
        previousMidi = midi;
      }
      phrase++;
      continue;
    }
    // A human downstroke every second bar establishes the chord before the
    // individual strings answer. Low-to-high order follows the physical hand.
    if (phrase % 2 === 0) {
      const downstroke = [1, 2, 0, 3];
      let spread = 0;
      for (let stroke = 0; stroke < downstroke.length; stroke++) {
        const string = downstroke[stroke];
        spread += 0.017 + grooveRnd() * 0.012;
        addUkuleleString(ukeBus, bar(b) + spread, strings[string] + shape[string],
                         -18.8 + (grooveRnd() - 0.5) * 1.4,
                         stringPan[string], 0xa11000 + phrase * 43 + stroke * 13);
      }
    }
    for (let step = 0; step < picking.length; step++) {
      const string = picking[step];
      const midi = strings[string] + shape[string];
      const accent = step === 0 || step === 4 ? 1.8 : 0;
      const swing = step % 2 ? 0.105 * BEAT : 0;
      const humanTime = (grooveRnd() - 0.5) * 0.022;
      const humanDb = (grooveRnd() - 0.5) * 1.8;
      addUkuleleString(ukeBus, bar(b) + step * 0.5 * BEAT + swing + humanTime, midi,
                       -20.5 + accent + humanDb, stringPan[string],
                       0x51a000 + phrase * 31 + step * 7);
    }
    phrase++;
  }

  // Aquauke reflections: two short, slowly breathing water paths around the
  // dry nylon center. Cross-fed echoes preserve the hand while making the
  // modeled body feel submerged in Wattajetta's world.
  const dryUke = ukeBus.slice();
  let waterL = 0, waterR = 0;
  const waterA = 1 - Math.exp(-TAU * 3900 / SR);
  for (let f = 0; f < ns; f++) {
    const t = f / SR;
    const dl = Math.round((0.027 + 0.0045 * Math.sin(TAU * 0.31 * t)) * SR);
    const dr = Math.round((0.041 + 0.006 * Math.sin(TAU * 0.23 * t + 1.3)) * SR);
    const leftAt = f - dl, rightAt = f - dr;
    const echoL = leftAt >= 0 ? dryUke[2 * leftAt + 1] : 0;
    const echoR = rightAt >= 0 ? dryUke[2 * rightAt] : 0;
    waterL += waterA * (echoL - waterL);
    waterR += waterA * (echoR - waterR);
    const renderedL = ukeBus[2 * f] + waterL * 0.24;
    const renderedR = ukeBus[2 * f + 1] + waterR * 0.24;
    renderedUke[2 * f] = renderedL;
    renderedUke[2 * f + 1] = renderedR;
    mix[2 * f] += renderedL;
    mix[2 * f + 1] += renderedR;
  }
}

// ── super scratching: a hand scrubs the record — the playhead scrubs
//    a slice of the track itself. Position follows smoothstep gestures
//    (zero velocity at the turnarounds, like a real wrist), a
//    transformer gate chops, and the mix ducks under the hand ──────────
const scratchSource = mix.slice();
function scratchAt(tOut, srcT, srcDur, gestures, gainDb) {
  const src0 = Math.floor(srcT * SR);
  const srcN = Math.floor(srcDur * SR);
  const gain = Math.pow(10, gainDb / 20);
  let t = tOut;
  for (const g of gestures) {
    const n = Math.floor(g.dur * SR);
    const at = Math.floor(t * SR);
    for (let i = 0; i < n; i++) {
      const p = smooth(i / n);
      const pos = (g.from + (g.to - g.from) * p) * srcN;
      const j = src0 + Math.floor(pos);
      if (j < 0 || j >= ns - 1 || at + i >= ns) continue;
      const fr = pos - Math.floor(pos);
      // 5ms gate ramps so the chop never clicks
      const edge = Math.min(i, n - i) / (0.005 * SR);
      const gate = (g.gate ?? 1) * Math.min(1, edge);
      const duck = 1 - 0.65 * gate;
      for (let ch = 0; ch < 2; ch++) {
        const v = scratchSource[2 * j + ch] * (1 - fr) + scratchSource[2 * (j + 1) + ch] * fr;
        mix[2 * (at + i) + ch] = mix[2 * (at + i) + ch] * duck + v * gate * gain;
      }
    }
    t += g.dur;
  }
}

// In the world cut the platter is a performed voice, not an occasional FX
// fill. Every beat receives a forward/back syllable made from that bar's own
// musical material. The band-limited voice stays clear of kick and sub.
function scratchVoiceGesture(tOut, srcT, from, to, gainDb, pan, durBeats = 0.22) {
  const n = Math.floor(durBeats * BEAT * SR);
  const at = Math.floor(tOut * SR);
  const src0 = Math.floor(srcT * SR);
  const srcN = Math.floor(BAR * SR);
  const gain = Math.pow(10, gainDb / 20);
  const aLo = 1 - Math.exp(-TAU * 230 / SR);
  const aHi = 1 - Math.exp(-TAU * 6200 / SR);
  const angle = (pan + 1) * 0.25 * Math.PI;
  const gl = Math.cos(angle), gr = Math.sin(angle);
  let low = 0, band = 0;
  for (let i = 0; i < n && at + i < ns; i++) {
    const p = smooth(i / n);
    const pos = (from + (to - from) * p) * srcN;
    const j = src0 + Math.floor(pos);
    if (j < 0 || j >= ns - 1) continue;
    const fr = pos - Math.floor(pos);
    const a = (scratchSource[2 * j] + scratchSource[2 * j + 1]) * 0.5;
    const b = (scratchSource[2 * (j + 1)] + scratchSource[2 * (j + 1) + 1]) * 0.5;
    const x = a * (1 - fr) + b * fr;
    low += aLo * (x - low);
    const hp = x - low;
    band += aHi * (hp - band);
    const edge = Math.min(1, Math.min(i, n - i) / (0.006 * SR));
    const vowel = 0.72 + 0.28 * Math.sin(Math.PI * i / n);
    const v = band * edge * vowel * gain;
    mix[2 * (at + i)] += v * gl;
    mix[2 * (at + i) + 1] += v * gr;
  }
}

if (WORLD) {
  for (let b = -INTRO_BARS; b < BARS; b++) {
    if (NEXT && b < 60) continue;
    const tBar = bar(b);
    const inBreath = BREATHS.some((x) => b >= x && b < x + 4);
    const inMist = b >= 80;
    const gainDb = b < 0 ? -22 : inMist ? -22 : inBreath ? -18.5 : b >= 48 ? -19 : -21.5;
    // Sixteenth-grid phrases sample the current bar in quarter- or half-beat
    // steps. Deterministic gaps scatter the hand without abandoning pulse;
    // downbeats remain present, while the smaller playhead moves make the
    // scratches articulate the song instead of behaving like transition FX.
    for (let step = 0; step < 16; step++) {
      const downbeat = step % 4 === 0;
      const density = inBreath ? 0.72 : inMist ? 0.48 : 0.62;
      if (!downbeat && rnd() > density) continue;
      const t = tBar + step * 0.25 * BEAT;
      const move = (rnd() < 0.42 ? 2 : 1) / 16; // half- or quarter-beat of the bar
      const direction = rnd() < 0.46 ? -1 : 1;
      const from = Math.min(0.94, Math.max(0.01, step / 16 + (rnd() - 0.5) / 32));
      const to = Math.min(0.98, Math.max(0.01, from + direction * move));
      const pan = 0.45 * Math.sin((b * 16 + step) * 0.41);
      scratchVoiceGesture(t, tBar, from, to, gainDb + (downbeat ? 1 : 0), pan,
                          downbeat ? 0.24 : 0.18 + rnd() * 0.06);
    }
  }
}
const E = BEAT / 2; // an eighth
const SUPER = [
  { dur: E * 2, from: 0, to: 0.8 }, { dur: E * 2, from: 0.8, to: 0.05 },
  { dur: E, from: 0.05, to: 0.7 }, { dur: E, from: 0.7, to: 0.1, gate: 0.7 },
  { dur: E * 0.5, from: 0.1, to: 0.5 }, { dur: E * 0.5, from: 0.5, to: 0.15 },
  { dur: E * 0.5, from: 0.15, to: 0.6, gate: 0.8 }, { dur: E * 0.5, from: 0.6, to: 0.05 },
  { dur: E * 0.25, from: 0.05, to: 0.3 }, { dur: E * 0.25, from: 0.3, to: 0.1 },
  { dur: E * 0.25, from: 0.1, to: 0.35 }, { dur: E * 0.25, from: 0.35, to: 0 },
];
// end of drop A: quick baby-scratch fill on the opening slice
scratchAt(bar(11) + 2 * BEAT, bar(0), BEAT, [
  { dur: E, from: 0, to: 0.6 }, { dur: E, from: 0.6, to: 0.1 },
  { dur: E * 0.5, from: 0.1, to: 0.5 }, { dur: E * 0.5, from: 0.5, to: 0 },
  { dur: E, from: 0, to: 1 },
], -4);
// the SUPER scratches: each one scrubs the NEXT drop's downbeat in
scratchAt(bar(15), bar(16), BEAT, SUPER, -3);
if (!NEXT) scratchAt(bar(47), bar(48), BEAT, SUPER, -3);
else scratchAt(bar(69), bar(64), BEAT, SUPER, -4);
// breath 2: slow half-speed scrub, barely gated
scratchAt(bar(29.5), bar(16), 2 * BEAT, [
  { dur: BEAT, from: 0, to: 0.5, gate: 0.5 }, { dur: BEAT, from: 0.5, to: 0.15, gate: 0.4 },
], -10);
// mist: one last underwater scrub of the stone drop
scratchAt(bar(84.5), bar(64), 2 * BEAT, [
  { dur: BEAT * 1.5, from: 0, to: 0.4, gate: 0.45 }, { dur: BEAT, from: 0.4, to: 0.1, gate: 0.35 },
], -11);

// ── vocals: jeffrey-pvc drops (pop/bin/say.mjs → assets/). Mixed after
//    the scratches so the hand never chews them, before crunch + warp so
//    they harden and accelerate with the record. `rate` re-pitches
//    (1 = as recorded); `warp` sends one underwater ────────────────────
function loadVocal(file) {
  const p = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error",
    "-i", resolve(HERE, "../assets", file),
    "-f", "f32le", "-ar", String(SR), "-ac", "2", "-"],
    { maxBuffer: 64 * 1024 * 1024 });
  if (p.status !== 0) { console.error(`✗ vocal decode failed: ${file}`); process.exit(1); }
  const b = p.stdout; // copy into an aligned buffer — stdout offset isn't guaranteed %4
  return new Float32Array(b.buffer.slice(b.byteOffset, b.byteOffset + b.length - (b.length % 4)));
}
const VOCALS = [
  { t: bar(0), file: "wattajetta.mp3", db: -8, rate: 1 },              // title drop over the steam
  { t: bar(15) - 0.15, file: "wayer.mp3", db: -6, rate: 1 },           // riding super scratch 1
  { t: bar(47) - 0.15, file: "wayer.mp3", db: -6, rate: 0.94 },        // into steel, a shade deeper
  { t: bar(63.5), file: "wayer.mp3", db: -8, rate: 0.8,
    warp: { depth: 0.05, hz: 0.9 } },                                  // underwater, into stone
  { t: bar(76) + 2 * BEAT, file: "wattajetta.mp3", db: -12, rate: 0.9 }, // coda echo under the church bell
];
const vocalCache = new Map();
for (const v of VOCALS) {
  if (!vocalCache.has(v.file)) vocalCache.set(v.file, loadVocal(v.file));
  let buf = vocalCache.get(v.file);
  if (v.rate !== 1 || v.warp) {
    const out = new Float32Array(Math.floor(buf.length / v.rate / 2) * 2);
    let pos = 0;
    for (let f = 0; f * 2 + 3 < out.length; f++) {
      const wob = v.warp ? 1 + v.warp.depth * Math.sin(TAU * v.warp.hz * (f / SR)) : 1;
      const j = Math.floor(pos) * 2;
      if (j + 3 >= buf.length) break;
      const fr = pos - Math.floor(pos);
      out[2 * f] = buf[j] * (1 - fr) + buf[j + 2] * fr;
      out[2 * f + 1] = buf[j + 1] * (1 - fr) + buf[j + 3] * fr;
      pos += v.rate * wob;
    }
    buf = out;
  }
  const g = Math.pow(10, v.db / 20);
  const at = Math.floor(v.t * SR);
  const n = Math.min(buf.length / 2, ns - at);
  for (let i = 0; i < n; i++) {
    mix[2 * (at + i)] += buf[2 * i] * g;
    mix[2 * (at + i) + 1] += buf[2 * i + 1] * g;
  }
}

// A moving water-world return. The source is high-passed before entering the
// room, so the kick and fuselage sub retain a stable physical center. Three
// modulated feed-forward paths supply changing distance and reflection shape;
// most of the return is antisymmetric and therefore disappears in mono,
// leaving the dry song as an intact anchor.
function addWaterWorld(buf) {
  const ringN = 16384;
  const ring = new Float32Array(ringN);
  const mask = ringN - 1;
  let at = 0, low = 0, air = 0;
  const hpA = 1 - Math.exp(-TAU * 190 / SR);
  const lpA = 1 - Math.exp(-TAU * 6800 / SR);
  const delayed = (frames) => ring[(at - frames) & mask];
  for (let f = 0; f < ns; f++) {
    const t = f / SR;
    const mid = (buf[2 * f] + buf[2 * f + 1]) * 0.5;
    low += hpA * (mid - low);
    const hp = mid - low;
    air += lpA * (hp - air);
    ring[at] = air;

    const driftA = Math.round(19 * Math.sin(TAU * 0.071 * t));
    const driftB = Math.round(27 * Math.sin(TAU * 0.047 * t + 1.7));
    const driftC = Math.round(13 * Math.sin(TAU * 0.093 * t + 3.1));
    const left = delayed(1488 + driftA) * 0.58 + delayed(3816 - driftB) * 0.34
               - delayed(6672 + driftC) * 0.22;
    const right = delayed(1776 - driftA) * 0.58 - delayed(4272 + driftB) * 0.34
                + delayed(7248 - driftC) * 0.22;
    const side = (left - right) * 0.5;
    const roomMid = (left + right) * 0.5;
    const originalBar = t / BAR - INTRO_BARS;
    const breath = BREATHS.some((b) => originalBar >= b && originalBar < b + 4);
    const amount = originalBar < 0 ? 0.24 : originalBar >= 80 ? 0.25 : breath ? 0.22 : 0.14;
    buf[2 * f] += side * amount + roomMid * 0.035;
    buf[2 * f + 1] -= side * amount - roomMid * 0.035;
    at = (at + 1) & mask;
  }
}

// Headphone stage: widen only the musical side field, with very little width
// below 220 Hz. The centered kick is added afterward, so low-end localization
// stays physical while bells, scratches, and water occupy distinct positions.
function headphoneWidth(buf) {
  let sideLow = 0;
  const sideA = 1 - Math.exp(-TAU * 220 / SR);
  for (let f = 0; f < ns; f++) {
    const l = buf[2 * f], r = buf[2 * f + 1];
    const mid = (l + r) * 0.5;
    const side = (l - r) * 0.5;
    sideLow += sideA * (side - sideLow);
    const highSide = side - sideLow;
    const placed = sideLow * 0.35 + highSide * 1.32;
    buf[2 * f] = mid + placed;
    buf[2 * f + 1] = mid - placed;
  }
}

// A physical moving-source return, adapted from Special Sign's dry-anchor /
// wet-rotation idea. Ear-to-source delays move in opposite directions, which
// produces a small Doppler bend as the orbit accelerates. Distance changes the
// level and spectral damping. Only the interaural difference returns to the
// mix, so the centered song and low end remain intact in mono.
function addPhysicalSpin(buf, start, dur, turns) {
  const source = buf.slice();
  const startFrame = Math.max(256, Math.floor(start * SR));
  const endFrame = Math.min(ns, Math.floor((start + dur) * SR));
  let low = 0;
  const hp = 1 - Math.exp(-TAU * 260 / SR);
  const readMono = (frame) => {
    const j = Math.max(0, Math.min(ns - 2, Math.floor(frame)));
    const fr = frame - j;
    const a = (source[2 * j] + source[2 * j + 1]) * 0.5;
    const b = (source[2 * (j + 1)] + source[2 * (j + 1) + 1]) * 0.5;
    return a * (1 - fr) + b * fr;
  };
  for (let f = startFrame; f < endFrame; f++) {
    const p = (f - startFrame) / Math.max(1, endFrame - startFrame);
    const edge = Math.min(1, p / 0.08, (1 - p) / 0.12);
    const orbit = TAU * (turns * smooth(p)
                + 0.085 * Math.sin(TAU * 2.7 * p)
                + 0.035 * Math.sin(TAU * 6.2 * p + 0.8));
    const azimuth = Math.sin(orbit);
    const distance = 0.82 + 0.42 * (0.5 + 0.5 * Math.cos(orbit));
    const baseDelay = 190 + 74 * distance;
    const itd = 31 * azimuth;
    const left = readMono(f - baseDelay - itd);
    const right = readMono(f - baseDelay + itd);
    const difference = (left - right) * 0.5;
    low += hp * (difference - low);
    const rotatingSide = (difference - low) * edge * (0.48 / distance);
    buf[2 * f] += rotatingSide;
    buf[2 * f + 1] -= rotatingSide;
  }
}

if (WORLD) {
  addWaterWorld(mix);
  headphoneWidth(mix);
  if (NEXT) addPhysicalSpin(mix, bar(64), 12 * BAR, 1.5);
}

// ── the kick returns: layered back on top, untouched by every hand ────
for (let i = 0; i < mix.length; i++) mix[i] += kickBus[i];

// ── warping: the whole record is wet — a global variable-rate pass.
//    The platter drags mid-breath (a hand on the vinyl), the tempo
//    RAMPS ~6% faster through the back half (138 → ~146 effective BPM,
//    kick and all — the rhythm stays locked because everything warps
//    together), and the mist rides a warped-record wobble out ──────────
{
  const rate = (t) => {
    let r = 1;
    for (const c of [bar(13.5), bar(45.5)]) {
      const d = (t - c) / 1.4;
      if (d > -1 && d < 1) r *= 1 - 0.38 * (0.5 + 0.5 * Math.cos(Math.PI * d)) ** 2;
    }
    // the trance accelerando: ramp in across drop C, hold through stone,
    // let go across the coda
    if (t >= bar(28) && t < bar(48)) r *= 1 + 0.06 * smooth((t - bar(28)) / (bar(48) - bar(28)));
    else if (t >= bar(48) && t < bar(76)) r *= 1.06;
    else if (t >= bar(76) && t < bar(80)) r *= 1 + 0.06 * (1 - smooth((t - bar(76)) / (4 * BAR)));
    if (t > bar(80)) {
      const w = Math.min(1, (t - bar(80)) / (2 * BAR));
      r *= 1 + 0.012 * w * Math.sin(TAU * 0.8 * t);
    }
    return r;
  };
  const warped = new Float32Array(mix.length);
  let pos = 0;
  for (let i = 0; i < ns; i++) {
    const j = Math.floor(pos);
    if (j >= ns - 1) break;
    const fr = pos - j;
    warped[2 * i] = mix[2 * j] * (1 - fr) + mix[2 * (j + 1)] * fr;
    warped[2 * i + 1] = mix[2 * j + 1] * (1 - fr) + mix[2 * (j + 1) + 1] * fr;
    pos += rate(i / SR);
  }
  mix.set(warped);
}

// The five seconds removed from the front are not thrown away. Later, the
// platter remembers them in quarter-beat cells: alternating cells run forward
// and backward with soft transformer-gate edges, so the old pickup material
// becomes a rhythmic scratch voice after the melody has established itself.
if (NEXT) {
  const memoryStart = Math.floor(84 * SR);
  const memoryFrames = Math.floor(5 * SR);
  const memory = mix.slice(memoryStart * 2, (memoryStart + memoryFrames) * 2);
  const returnAt = Math.floor(123 * SR);
  const cellFrames = Math.floor(0.25 * BEAT * SR);
  const returnFrames = Math.min(memoryFrames, ns - returnAt);
  const gain = Math.pow(10, -9 / 20);
  for (let i = 0; i < returnFrames; i++) {
    const cell = Math.floor(i / cellFrames);
    const inCell = i % cellFrames;
    const reverse = cell % 3 === 1;
    const sourceCell = (cell * 5) % Math.max(1, Math.floor(memoryFrames / cellFrames));
    const sourceInCell = reverse ? cellFrames - 1 - inCell : inCell;
    const sourceFrame = Math.min(memoryFrames - 1, sourceCell * cellFrames + sourceInCell);
    const edge = Math.min(1, inCell / (0.008 * SR), (cellFrames - inCell) / (0.008 * SR));
    const phrase = Math.min(1, i / (0.18 * SR), (returnFrames - i) / (0.35 * SR));
    const g = gain * edge * phrase;
    mix[2 * (returnAt + i)] += memory[2 * sourceFrame] * g;
    mix[2 * (returnAt + i) + 1] += memory[2 * sourceFrame + 1] * g;
  }
}

// re-peak after the layers so the master sees a sane level
let peak = 0;
for (let i = 0; i < mix.length; i++) { const v = Math.abs(mix[i]); if (v > peak) peak = v; }
if (peak > 0.9) { const g = 0.9 / peak; for (let i = 0; i < mix.length; i++) mix[i] *= g; }

// The listener's chosen front door is five seconds into the first pickup cut:
// 1:29 in the accepted world audition. The 2:30 sequence revisits rather than
// stretches: impact → rotation → discarded intro/scratch material → deeper-saw
// hook reprise → coda. Two-second equal-power joins keep the chapters physical.
let outputMix = mix;
if (NEXT) {
  const targetFrames = 150 * SR;
  outputMix = new Float32Array(targetFrames * 2);
  const overlapFrames = 2 * SR;
  let writtenFrames = 0;
  const appendChapter = (startSec, endSec) => {
    const sourceStart = Math.floor(startSec * SR);
    const sourceFrames = Math.min(Math.floor((endSec - startSec) * SR), ns - sourceStart);
    const overlap = writtenFrames === 0 ? 0 : Math.min(overlapFrames, writtenFrames, sourceFrames);
    const destination = writtenFrames - overlap;
    for (let f = 0; f < overlap; f++) {
      const p = f / Math.max(1, overlap - 1);
      const oldGain = Math.cos(p * Math.PI * 0.5);
      const newGain = Math.sin(p * Math.PI * 0.5);
      outputMix[2 * (destination + f)] *= oldGain;
      outputMix[2 * (destination + f) + 1] *= oldGain;
      outputMix[2 * (destination + f)] += mix[2 * (sourceStart + f)] * newGain;
      outputMix[2 * (destination + f) + 1] += mix[2 * (sourceStart + f) + 1] * newGain;
    }
    const remaining = Math.min(sourceFrames - overlap, targetFrames - destination - overlap);
    for (let f = 0; f < remaining; f++) {
      outputMix[2 * (destination + overlap + f)] = mix[2 * (sourceStart + overlap + f)];
      outputMix[2 * (destination + overlap + f) + 1] = mix[2 * (sourceStart + overlap + f) + 1];
    }
    writtenFrames = Math.min(targetFrames, destination + overlap + remaining);
  };
  // Enter the source on bar 51 exactly. The prior 89-second cut began 0.7
  // beats into that bar, so the new prelude and inherited groove disagreed
  // about where the pulse lived.
  appendChapter(bar(51), bar(51) + 55); // one full aligned bar before impact
  appendChapter(59, 89);  // the discarded intro and its scratch ancestry return
  appendChapter(107, 140);// tighter deeper-saw hook reprise
  appendChapter(142, 156);// coda retained after making room for the slow build

  // A single readable staircase leads to the inherited drop: isolated nylon
  // notes, a straight hat count, sparse FEM answers, and one counted bar of
  // quiet. The source Aquauke stem is removed from the inherited body so the
  // prelude does not become a competing harmony after the landing.
  const alignedDropAt = 15 * BAR;
  if (aquaukeBus) {
    const body = outputMix.slice();
    const introFrames = Math.floor(14 * BAR * SR);
    if (aquaukeMixBus) {
      const sourceStart = Math.floor(bar(51) * SR);
      const firstChapterFrames = Math.min(Math.floor(55 * SR), targetFrames);
      for (let f = 0; f < firstChapterFrames; f++) {
        const source = sourceStart + f;
        if (source >= ns) break;
        body[2 * f] -= aquaukeMixBus[2 * source];
        body[2 * f + 1] -= aquaukeMixBus[2 * source + 1];
      }
    }
    for (let f = targetFrames - 1; f >= introFrames; f--) {
      outputMix[2 * f] = body[2 * (f - introFrames)];
      outputMix[2 * f + 1] = body[2 * (f - introFrames) + 1];
    }
    outputMix.fill(0, 0, introFrames * 2);

    // Keep the inherited bar 51 barely present under the count. Its amplitude
    // rises only 3 dB before the vacuum; the actual velocity jump belongs to
    // bar 52, not to a false landing inside the break.
    for (let f = introFrames; f < Math.floor(alignedDropAt * SR); f++) {
      const p = (f / SR - 14 * BAR) / BAR;
      const g = 0.1 + 0.04 * smooth(p);
      outputMix[2 * f] *= g;
      outputMix[2 * f + 1] *= g;
    }

    // One monophonic E-minor-pentatonic line. Density and velocity rise in
    // separate, simple stages; it never becomes a post-drop chord bed.
    const ukeMotif = [52, 55, 57, 59, 62, 59, 57, 55];
    let preludeState = 0x5a17c411;
    const preludeRnd = () => ((preludeState = (preludeState * 1664525 + 1013904223) >>> 0) / 4294967296);
    let ukePhrase = 0;
    for (let b = 0; b < 11; b++) {
      const slots = b < 4 ? [0] : b < 8 ? [0, 2] : [0, 1, 2, 3];
      for (const beat of slots) {
        const progress = (b + beat / 4) / 11;
        const handoff = b < 9 ? 0 : -2.5 * (b - 8);
        const midi = ukeMotif[ukePhrase % ukeMotif.length];
        addUkuleleString(outputMix, b * BAR + beat * BEAT + (preludeRnd() - 0.5) * 0.006,
                         midi, -25 + 6 * smooth(progress) + handoff,
                         0.2 * Math.sin(ukePhrase * 0.61), 0x91a000 + ukePhrase * 73);
        ukePhrase++;
      }
    }

    // Straight hats expose 138 BPM: half-time offbeats, then off-eighths,
    // then two bars of eighths. The break returns to four soft quarter counts.
    const tickRnd = () => ((tickState = (tickState * 1664525 + 1013904223) >>> 0) / 4294967296);
    let tickState = 0x71cc901d;
    for (let b = 4; b < 15; b++) {
      const slots = b < 8 ? [1, 3]
                  : b < 12 ? [0.5, 1.5, 2.5, 3.5]
                  : b < 14 ? [0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5]
                  : [0, 1, 2, 3];
      for (let step = 0; step < slots.length; step++) {
        const slot = slots[step];
        const when = b * BAR + slot * BEAT + (tickRnd() - 0.5) * 0.004;
        const at = Math.floor(when * SR);
        const open = b === 13 && step === slots.length - 1;
        const n = Math.min(Math.floor((open ? 0.12 : 0.032) * SR), targetFrames - at);
        const pan = (tickRnd() * 2 - 1) * 0.48;
        const angle = (pan + 1) * Math.PI * 0.25;
        const progress = Math.min(1, (b - 4 + slot / 4) / 10);
        const db = b === 14 ? -24 : -28 + 11 * smooth(progress);
        const gain = Math.pow(10, db / 20);
        let previous = 0;
        for (let i = 0; i < n; i++) {
          const white = tickRnd() * 2 - 1;
          const high = white - previous;
          previous = white;
          const x = high * Math.exp(-i / ((open ? 0.05 : 0.012) * SR)) * gain;
          outputMix[2 * (at + i)] += x * Math.cos(angle);
          outputMix[2 * (at + i) + 1] += x * Math.sin(angle);
        }
      }
    }

    // FEM steel answers twice per bar, inheriting the line without doubling its
    // rhythm. The final E rings across the counted break.
    const bellMotif = ["E4", "G4", "A4", "B4", "D5", "B4", "A4", "G4"];
    let bellPhrase = 0;
    const addPreludeBell = (when, note, db, pan, long = false) => {
      const spec = { note, material: "steel", geometry: "glass",
                     dur: long ? 2.2 : 0.95, renderDur: long ? 2.2 : 0.95 };
      const rendered = bellFor(spec);
      const at = Math.floor(when * SR);
      const n = Math.min(rendered.L.length, Math.floor(spec.dur * SR), targetFrames - at);
      const gain = Math.pow(10, db / 20);
      const angle = (pan + 1) * Math.PI * 0.25;
      for (let i = 0; i < n; i++) {
        const release = Math.min(1, (n - i) / (0.055 * SR));
        outputMix[2 * (at + i)] += rendered.L[i] * gain * Math.cos(angle) * release;
        outputMix[2 * (at + i) + 1] += rendered.R[i] * gain * Math.sin(angle) * release;
      }
    };
    for (let b = 10; b < 13; b++) for (const beat of [0, 2]) {
      const note = bellMotif[bellPhrase % bellMotif.length];
      const progress = bellPhrase / 7;
      addPreludeBell(b * BAR + beat * BEAT, note, -24 + 5 * smooth(progress),
                     0.46 * Math.sin(bellPhrase * 0.72));
      bellPhrase++;
    }
    addPreludeBell(13 * BAR, "B4", -19, -0.18);
    addPreludeBell(13 * BAR + 2 * BEAT, "E5", -16, 0.18, true);
  }

  // The source enters on bar 51 and lands exactly on bar 52. Nothing is added
  // after the downbeat: the existing kick, bass, thwubs, and bells own it.
  const dropAt = alignedDropAt;
  const vacuumStart = dropAt - 0.18;
  for (let f = Math.floor(vacuumStart * SR); f < Math.floor(dropAt * SR); f++) {
    const p = (f / SR - vacuumStart) / (dropAt - vacuumStart);
    const g = 1 - 0.82 * smooth(p);
    outputMix[2 * f] *= g;
    outputMix[2 * f + 1] *= g;
  }

  // At 0:45 the water world begins corroding into an original industrial
  // vocabulary: inharmonic machine strikes orbit slowly while low white-noise
  // waves periodically cover and muffle the song. No external samples are
  // used; every impact and surge is generated from oscillators and noise.
  let industrialSeed = 0x1d057a11;
  const industrialRnd = () => ((industrialSeed = (industrialSeed * 1664525 + 1013904223) >>> 0) / 4294967296);
  const addMetalHit = (when, base, db, pan, seed) => {
    const at = Math.floor(when * SR);
    const n = Math.min(Math.floor(0.58 * SR), targetFrames - at);
    const gain = Math.pow(10, db / 20);
    const angle = (pan + 1) * Math.PI * 0.25;
    const ratios = [1, 1.417, 2.173, 3.691, 5.13];
    const phases = new Float64Array(ratios.length);
    let state = seed >>> 0, scrape = 0;
    for (let i = 0; i < n; i++) {
      const t = i / SR;
      state = (state * 1664525 + 1013904223) >>> 0;
      const white = state / 4294967296 * 2 - 1;
      scrape += 0.19 * (white - scrape);
      let metal = 0;
      for (let h = 0; h < ratios.length; h++) {
        phases[h] += TAU * base * ratios[h] * (1 + 0.014 * Math.exp(-t / 0.08)) / SR;
        metal += Math.sin(phases[h]) * Math.exp(-t / (0.13 + h * 0.06)) / (1 + h * 0.58);
      }
      const strike = Math.tanh((metal * 0.72 + scrape * Math.exp(-t / 0.035) * 0.65) * 1.9);
      const x = strike * Math.exp(-t / 0.44) * gain;
      outputMix[2 * (at + i)] += x * Math.cos(angle);
      outputMix[2 * (at + i) + 1] += x * Math.sin(angle);
    }
  };

  const industrialStart = 45;
  for (let b = 0, whenBar = industrialStart; whenBar < 140; b++, whenBar += BAR) {
    const slots = b < 7 ? [0, 2.5] : b % 4 === 3 ? [0, 0.75, 1.5, 2.25, 3] : [0, 1.5, 2.75];
    for (let hit = 0; hit < slots.length; hit++) {
      const when = whenBar + slots[hit] * BEAT + (industrialRnd() - 0.5) * 0.016;
      const age = Math.min(1, (when - industrialStart) / 42);
      const base = [74, 91, 113, 137][(b + hit) % 4] * (hit % 2 ? 1 : 0.5);
      const orbit = TAU * ((when - industrialStart) / 21
                    + 0.08 * Math.sin(TAU * (when - industrialStart) / 13));
      const pan = 0.76 * Math.sin(orbit);
      addMetalHit(when, base, -27 + 9 * smooth(age), pan, 0x1d0000 + b * 71 + hit * 17);
    }
  }

  const addDrowningWave = (when, dur, db, direction, seed) => {
    const at = Math.floor(when * SR);
    const n = Math.min(Math.floor(dur * SR), targetFrames - at);
    const gain = Math.pow(10, db / 20);
    let state = seed >>> 0, lowNoise = 0, songLowL = 0, songLowR = 0, subPhase = 0;
    for (let i = 0; i < n; i++) {
      const p = i / Math.max(1, n - 1);
      state = (state * 1664525 + 1013904223) >>> 0;
      const white = state / 4294967296 * 2 - 1;
      const noiseCutoff = 260 + 5200 * (1 - smooth(p));
      const noiseA = 1 - Math.exp(-TAU * noiseCutoff / SR);
      lowNoise += noiseA * (white - lowNoise);
      const attack = smooth(Math.min(1, p / 0.12));
      const tail = (1 - p) ** 1.35;
      const envelope = attack * tail;
      subPhase += TAU * (47 + (25 - 47) * smooth(p)) / SR;
      const pressure = lowNoise * 0.82 + white * 0.1 + Math.sin(subPhase) * 0.42;
      const azimuth = direction * Math.sin(Math.PI * p + 0.42 * Math.sin(TAU * 1.7 * p));
      const angle = (azimuth + 1) * Math.PI * 0.25;
      const frame = at + i;
      const songL = outputMix[2 * frame], songR = outputMix[2 * frame + 1];
      const drownA = 1 - Math.exp(-TAU * (720 + 480 * (1 - p)) / SR);
      songLowL += drownA * (songL - songLowL);
      songLowR += drownA * (songR - songLowR);
      const cover = envelope * (0.24 + 0.2 * smooth((when - industrialStart) / 92));
      outputMix[2 * frame] = songL * (1 - cover) + songLowL * cover
                           + pressure * envelope * gain * Math.cos(angle);
      outputMix[2 * frame + 1] = songR * (1 - cover) + songLowR * cover
                               + pressure * envelope * gain * Math.sin(angle);
    }
  };
  for (const [i, when] of [45, 68, 91, 114, 134].entries())
    addDrowningWave(when, i === 4 ? 9 : 6.5, -18 + i * 0.8, i % 2 ? -1 : 1,
                    0xd20a00 + i * 0x121);

  const fadeFrames = Math.floor(0.018 * SR);
  for (let f = 0; f < fadeFrames; f++) {
    const g = f / fadeFrames;
    outputMix[2 * f] *= g;
    outputMix[2 * f + 1] *= g;
  }
  const tailFrames = Math.floor(3 * SR);
  for (let f = 0; f < tailFrames; f++) {
    const g = 1 - smooth(f / tailFrames);
    const at = outputMix.length / 2 - tailFrames + f;
    outputMix[2 * at] *= g;
    outputMix[2 * at + 1] *= g;
  }

  // Progressive air taper: retain articulation at the opening impact, then
  // increasingly fold the field above ~5.2 kHz back into the body of the mix.
  // This is a shelf-like crossover, not a hard low-pass wall.
  let lowL = 0, lowR = 0;
  const airA = 1 - Math.exp(-TAU * 5200 / SR);
  const frames = outputMix.length / 2;
  for (let f = 0; f < frames; f++) {
    const p = f / Math.max(1, frames - 1);
    lowL += airA * (outputMix[2 * f] - lowL);
    lowR += airA * (outputMix[2 * f + 1] - lowR);
    const airGain = 0.82 - 0.24 * smooth(p);
    outputMix[2 * f] = lowL + (outputMix[2 * f] - lowL) * airGain;
    outputMix[2 * f + 1] = lowR + (outputMix[2 * f + 1] - lowR) * airGain;
  }

  // Bake the complete arrangement into one vinyl material. Pitch is carried by
  // a slow platter wow plus fine motor flutter; groove saturation and channel
  // bleed bind the stereo image; synthetic grain/crackle/rumble establish a
  // surface without importing a sample. The treatment is deliberately subtle
  // enough that kick fundamentals and bell intonation remain usable.
  const dry = outputMix.slice();
  let vinylSeed = 0x71a71e11;
  const vinylRnd = () => ((vinylSeed = (vinylSeed * 1664525 + 1013904223) >>> 0) / 4294967296);
  let position = 0, grain = 0, crackle = 0;
  const grainA = 1 - Math.exp(-TAU * 6800 / SR);
  const satNorm = Math.tanh(1.45);
  for (let f = 0; f < frames; f++) {
    const t = f / SR;
    const j = Math.min(frames - 2, Math.floor(position));
    const fraction = position - j;
    const left = dry[2 * j] * (1 - fraction) + dry[2 * (j + 1)] * fraction;
    const right = dry[2 * j + 1] * (1 - fraction) + dry[2 * (j + 1) + 1] * fraction;
    const rate = 1 + 0.00145 * Math.sin(TAU * 0.29 * t)
                   + 0.00042 * Math.sin(TAU * 5.7 * t + 0.8)
                   + 0.00018 * Math.sin(TAU * 9.3 * t + 2.1);
    position = Math.min(frames - 2.001, position + rate);

    const white = vinylRnd() * 2 - 1;
    grain += grainA * (white - grain);
    if (vinylRnd() < 11 / SR) crackle += (vinylRnd() * 2 - 1) * 0.024;
    crackle *= 0.93;
    const surface = grain * 0.00115 + crackle
                  + 0.0007 * Math.sin(TAU * 28.5 * t + 0.3 * Math.sin(TAU * 0.11 * t));
    const bleedL = left * 0.965 + right * 0.035;
    const bleedR = right * 0.965 + left * 0.035;
    const satL = Math.tanh(bleedL * 1.45) / satNorm;
    const satR = Math.tanh(bleedR * 1.45) / satNorm;
    outputMix[2 * f] = bleedL * 0.78 + satL * 0.22 + surface;
    outputMix[2 * f + 1] = bleedR * 0.78 + satR * 0.22 + surface * 0.92;
  }
  // Fade the material itself too, so surface grain ends as part of the record
  // rather than being cut off at the file boundary.
  for (let f = 0; f < tailFrames; f++) {
    const g = 1 - smooth(f / tailFrames);
    const at = frames - tailFrames + f;
    outputMix[2 * at] *= g;
    outputMix[2 * at + 1] *= g;
  }
}

const outputStem = NEXT ? "wattajetta-world-v2" : WORLD ? "wattajetta-world" : "wattajetta";
const mixedPath = resolve(OUT, `${outputStem}.mixed.f32.raw`);
writeFileSync(mixedPath, Buffer.from(outputMix.buffer, outputMix.byteOffset, outputMix.length * 4));

// ── master: firmer than the pure cut — the crunch ramp wants a spine,
//    but the opening still breathes ─────────────────────────────────────
const MASTER = [
  "highpass=f=24",
  "acompressor=threshold=-19dB:ratio=2.8:attack=10:release=150:makeup=2.2:knee=6",
  "equalizer=f=50:t=q:w=1.2:g=2.5", // the boom under the kick — no treble boost; laptop speakers read it as tang
  ...(WORLD ? ["equalizer=f=285:t=q:w=0.85:g=-1.6"] : []), // normalize low-mid buildup for headphones
  ...(WORLD ? ["highshelf=f=5500:g=-2dB"] : []), // keep the water world, lose excess upper-air haze
  "alimiter=limit=0.96:attack=4:release=70",
  ...(WORLD ? ["volume=-1.5dB"] : []), // leave the spatial audition at about -1 dBTP
  // The accelerando leaves dead source at the legacy tail. The fixed 2:30
  // vinyl arrangement already owns its ending, so preserve that exact clock.
  ...(!NEXT ? ["areverse", "silenceremove=start_periods=1:start_threshold=-70dB", "areverse"] : []),
];
const mp3 = resolve(OUT, `${outputStem}.mp3`);
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", mixedPath,
  "-af", MASTER.join(","), "-c:a", "libmp3lame", "-q:a", "2",
  "-metadata", `title=${NEXT ? "wattajetta — vinyl material audition" : WORLD ? "wattajetta — world audition" : "wattajetta"}`,
  "-metadata", "album=pixsies",
  mp3], { stdio: "inherit" });
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
if (WORLD) {
  const wav = resolve(OUT, `${outputStem}-AUDITION.wav`);
  const fw = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", mixedPath,
    "-af", MASTER.join(","), "-ar", String(SR), "-ac", "2", "-c:a", "pcm_s24le", wav],
    { stdio: "inherit" });
  if (fw.status !== 0) { console.error("✗ world WAV failed"); process.exit(1); }
  console.log(`✓ ${wav} (24-bit spatial audition)`);
}
for (const p of [rawPath, kickPath, mixedPath]) { try { unlinkSync(p); } catch {} }
console.log(`✓ ${mp3} (glass↔staccato transmogrification · rhythmic scratch voice · water world)`);
