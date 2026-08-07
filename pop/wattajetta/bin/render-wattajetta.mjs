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
//   node pop/wattajetta/bin/render-wattajetta.mjs --tiny-bells
//                                                        → out/wattajetta-tinybells.mp3
//   node pop/wattajetta/bin/render-wattajetta.mjs --stone-club
//                                                  → out/wattajetta-stone-club-audition.mp3
//   node pop/wattajetta/bin/render-wattajetta.mjs --stone-club --stems
//                                                  → out/wattajetta-stone-club-stems/*.wav
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

import { writeFileSync, readFileSync, mkdirSync, unlinkSync, existsSync, statSync, renameSync } from "node:fs";
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
const INDUSTRIAL = process.argv.includes("--industrial");
const WORLD = NEXT || process.argv.includes("--world");
// watjetsto720 — the extended mix: the club cut let out three times further
// again (scale 6, 432 bars) and clamped to exactly 720 s, the way
// fluttabap360 was back-solved onto its 360 s boundary. 138 BPM and the
// 72-bar form survive intact; the surplus comes off the long mist coda.
const STONE_720 = process.argv.includes("--stone-720")
                || process.argv.includes("--watjetsto720");
const STONE_CLUB = STONE_720 || process.argv.includes("--stone-club");
const STONE_STUDY = STONE_CLUB || process.argv.includes("--stone-study");
// The club cut is the canonical stone form let out to twice its length:
// every span doubles, so the ensemble introduction unfolds at half the
// rate and the accelerando has twice as far to climb.
const STONE_SCALE = STONE_720 ? 6 : STONE_CLUB ? 2 : 1;
const outputStem = INDUSTRIAL ? "wattajetta-industrial"
  : NEXT ? "wattajetta-world-v2" : WORLD ? "wattajetta-world"
  : STONE_720 ? "watjetsto720"
  : STONE_CLUB ? "wattajetta-stone-club" : "wattajetta";

const SR = 48000;
const TAU = 2 * Math.PI;
const BPM = 138;
const BEAT = 60 / BPM;
const BAR = BEAT * 4;
const BARS = STONE_STUDY ? 72 * STONE_SCALE : 96;
const INTRO_BARS = WORLD ? 4 : 0;
const DUR = (INTRO_BARS + BARS) * BAR + (WORLD ? 3.2 : 5.5);
const TINY_BELLS = STONE_STUDY || process.argv.includes("--tiny-bells");
const TINY_BELL_END = 45 * STONE_SCALE;
const MUTATION = 18 * STONE_SCALE; // bars per stone mutation

// deterministic sprinkle — same track every bake
let _s = 0xa7757e77;
const rnd = () => ((_s = (_s * 1664525 + 1013904223) >>> 0) / 4294967296);
let _human = 0xa9aa0e11;
const humanRnd = () => ((_human = (_human * 1664525 + 1013904223) >>> 0) / 4294967296);

// e minor pentatonic — the bell runs live here. One octave down from
// the first cuts: E5–E6 read piercing-tangy on laptop speakers
const DROP_NOTES = ["E4", "G4", "A4", "B4", "D5", "E5"];
// The club cut's mallet figures (octave stops, pickup runs, rolls) extend the
// pentatonic ladder one octave DOWN — marimba left hand — so the perc-nerd
// play never reaches above the E5 laptop-tang ceiling.
const LADDER_BASE = 5; // canonical: DROP_NOTES[i] sits at full-ladder index i + 5
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
const snares = []; // { t, strength } — also keys the independent bell duck
const bells = [];  // { t, note, vel, pan, gain, material, geometry, dur, warp? }
// Tagged pitched events that don't live in `bells` (they mix straight to the
// bus), collected only so the graphic-score exporter can analyze the track.
const scoreExtra = { uke: [], flyby: [], sub: [], disco: [] };
// Staircase notes (rushes, glisses, pickups, arps) log themselves here so a
// square-wave shadow voice can dilly-dally after them at mix time.
const squareShadow = [];
const SCORE_JSON = STONE_CLUB && process.argv.includes("--score-json");

const bar = (n) => (n + INTRO_BARS) * BAR;
const introBar = (n) => n * BAR;

// the flight plan: each drop hardens the water a little more
const ORIGINAL_DROPS = [
  { a: 0,  z: 12, mat: "glass",  bowl: "silver", durs: [3.2, 3.2, 1.4], density: 0.85, gainDb: -14, bloops: false },
  { a: 16, z: 28, mat: "glass",  bowl: "silver", durs: [3.2, 1.4, 0.9], density: 0.9,  gainDb: -13, bloops: 0.4 },
  { a: 32, z: 44, mat: "bronze", bowl: "brass",  durs: [1.8, 1.8, 1.0], density: 0.9,  gainDb: -13, bloops: 0.6 },
  { a: 48, z: 60, mat: "steel",  bowl: "gold",   durs: [1.2, 1.2, 0.7], density: 0.95, gainDb: -12.5, bloops: 0.6 },
  { a: 64, z: 76, mat: "stone",  bowl: "stone",  durs: [1.6, 1.0, 0.7], density: 0.95, gainDb: -11.5, bloops: 0.75 },
];
// Canonicalize the original bar-64 stone drop as one continuous two-minute
// form. Four seamless 18-bar mutations change density without dropping the
// floor. Material, dense bloops, galloping sine floor, and 4/4 kick are the
// exact ingredients that made the source section work.
const STONE_DROPS = [
  { a: 0,  z: 18, mat: "stone", bowl: "stone", geom: "glass", durs: [1.6, 1.0, 0.7], density: 0.92, gainDb: -11.8, bloops: 0.72 },
  { a: 18, z: 36, mat: "stone", bowl: "stone", geom: "handbell", durs: [1.6, 1.0, 0.7], density: 0.95, gainDb: -11.5, bloops: 0.78 },
  { a: 36, z: 54, mat: "stone", bowl: "stone", geom: "tubular", durs: [1.6, 1.0, 0.7], density: 0.98, gainDb: -11.2, bloops: 0.84 },
  { a: 54, z: 72, mat: "stone", bowl: "stone", geom: "glass", durs: [1.6, 1.0, 0.7], density: 1, gainDb: -10.8, bloops: 0.9 },
];
const DROPS = STONE_STUDY
  ? STONE_DROPS.map((d) => ({ ...d, a: d.a * STONE_SCALE, z: d.z * STONE_SCALE }))
  : ORIGINAL_DROPS;
const BREATHS = STONE_STUDY ? [] : [12, 28, 44, 60];

// ── watjetsto720: the wave form ───────────────────────────────────────
// Suzanne Ciani's Seven Waves was written down, not patched, and her
// long-form work never lets one identity stand for more than ~5 minutes:
// eight named scenes, build and release, then back to the ocean. The
// stone form as inherited is one chord for twelve minutes, so we borrow
// her machinery — but only the parts that survive at 138 BPM.
//
// The bass is the ocean and the chord is the wave: E1/G1/A1/D2 stays
// exactly as it is and the harmony moves ABOVE it, which buys eight
// harmonic identities without costing the fuselage its character. All
// bass motion is hoarded for a single event (wave 7) so that one
// transposition lands like a wall instead of like a habit.
//
// Cool rule, held without exception: no D#. No leading tone, no V-i, no
// B major. Colour comes from added 9ths, quartal stacks, thirds kept out
// of the bass, and flat-side motion (bVI, iv, minor v, phrygian bII).
const WAVE_BARS = 54; // eight waves across 432 bars — about 93 s each
const WAVES = [
  { name: "out of the ocean", transpose: 0,
    chord: ["E2", "B2", "E3", "F#3", "G3"],           // Em(add9) — home
    pent:  ["E4", "F#4", "G4", "B4", "D5", "E5"] },
  { name: "glass rising",     transpose: 0,
    chord: ["E2", "C3", "E3", "G3", "B3"],            // Cmaj9/E — bVI
    pent:  ["E4", "G4", "B4", "C5", "D5", "E5"] },
  { name: "the fourths",      transpose: 0,
    chord: ["E2", "A2", "C3", "G3"],                  // Am11/E — iv, no third on top
    pent:  ["E4", "G4", "A4", "C5", "D5", "E5"] },
  { name: "coldest cadence",  transpose: 0,
    chord: ["E2", "B2", "D3", "F#3"],                 // Bm/E — minor v, no leading tone
    pent:  ["E4", "F#4", "A4", "B4", "D5", "E5"] },
  { name: "mixolydian lift",  transpose: 0,
    chord: ["E2", "D3", "E3", "A3"],                  // Dsus2/E — bVII, lift not dominant
    pent:  ["E4", "F#4", "A4", "B4", "D5", "E5"] },
  { name: "phrygian ice",     transpose: 0,
    chord: ["E2", "F3", "A3", "C4"],                  // F/E — bII, the water hardening
    pent:  ["E4", "F4", "A4", "C5", "D5", "E5"] },
  { name: "the wall",         transpose: 3,           // the ONLY bass move in twelve minutes
    chord: ["G2", "D3", "G3", "A3", "A#3"],           // Gm(add9) — up a minor third
    pent:  ["E4", "G4", "A4", "A#4", "D5", "E5"] },
  { name: "back to the ocean", transpose: 0,
    chord: ["E2", "B2", "E3", "F#3", "G3", "C4", "D4"], // home, carrying what it gathered
    pent:  ["E4", "G4", "A4", "B4", "D5", "E5"] },
];
const waveAt = (b) => WAVES[Math.min(Math.floor(b / WAVE_BARS), WAVES.length - 1)];
const waveNotes = (b) => (STONE_720 ? waveAt(b).pent : DROP_NOTES);
const waveShift = (b) => (STONE_720 ? Math.pow(2, waveAt(b).transpose / 12) : 1);


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
const kickAt = (t, db = STONE_CLUB ? -0.8 : -2) => kicks.push([
  t,
  STONE_CLUB ? 126 : 118,
  STONE_CLUB ? 43 : 41,
  STONE_CLUB ? 0.088 : 0.075,
  db,
  STONE_CLUB ? 0.009 : 0.012,
  STONE_CLUB ? 0.34 : 0.28,
]);
for (const d of DROPS)
  for (let b = d.a; b < d.z; b++) {
    // techno section: the kick tightens — shorter hole, faster decay, a
    // shade louder. A bare machine pulse while every other voice steps out.
    if (inTechno(b)) {
      for (let k = 0; k < 4; k++)
        kicks.push([bar(b) + k * BEAT, 128, 45, 0.06, -0.4, 0.007, 0.2]);
      continue;
    }
    // slow build: the club intro opens halftime (1 and 3) and only commits
    // to four-on-the-floor at bar 4 — the room assembles piece by piece
    if (STONE_CLUB && b < 4) {
      kickAt(bar(b)); kickAt(bar(b) + 2 * BEAT);
      continue;
    }
    const fourFloor = STONE_STUDY || d.a >= 48 || (d.a === 32 && b >= 38);
    if (fourFloor) for (let k = 0; k < 4; k++) kickAt(bar(b) + k * BEAT);
    else { kickAt(bar(b)); kickAt(bar(b) + 2 * BEAT); }
  }

// Dry popping backbeat. Its brightness and velocity breathe over a 12-hit
// wave, but it never doubles: the dependable 2-and-4 silhouette stays intact.
if (STONE_STUDY) {
  let snareIndex = 0;
  for (let b = 4 * STONE_SCALE; b < BARS; b++) {
    if (bar(b) < TINY_BELL_END && b % 2 === 1) continue;
    for (const beat of [1, 3]) {
      const phase = (snareIndex % 12) / 12;
      const swell = 0.5 - 0.5 * Math.cos(TAU * phase);
      const t = bar(b) + beat * BEAT;
      const db = (bar(b) < TINY_BELL_END ? -15 : -13) + 3.2 * swell;
      noises.push([t, 0.085 + 0.035 * swell,
        1750 + 950 * swell, 900 + 350 * swell, 0.95 + 0.4 * swell,
        db, 0.002, 0.06 + 0.035 * swell, beat === 1 ? -0.08 : 0.08]);
      snares.push({ t, strength: 0.68 + 0.32 * swell });
      snareIndex++;
    }
  }
}

// A narrow offbeat hat makes the quarter-note grid physical without covering
// the stone and glass transients. It opens only at four-bar turns.
if (STONE_CLUB) {
  for (let b = 0; b < BARS; b++) { // the tick never stops — hats run bar 0 to the end
    for (let beat = 0; beat < 4; beat++) {
      const tech = inTechno(b);
      const early = b < 4; // pre-build: a quiet tight tick that leads the room in
      const open = beat === 3 && b % 4 === 3 && !tech && !early; // techno/intro: closed only
      const hit = b * 4 + beat;
      const t = bar(b) + (beat + 0.5) * BEAT + EAGER + (tech ? 0 : grooveJitter(hit, 73, 0.004));
      const lateLift = b >= 36 ? 1.5 : b >= 18 ? 0.8 : 0;
      const hand = tech ? 0 : (grooveUnit(hit, 91) - 0.5) * 2.6;
      noises.push([t, open ? 0.18 : tech || early ? 0.03 : 0.055,
        open ? 6900 : 7600, open ? 9800 : 8900, open ? 0.72 : 1.45,
        (open ? -17.5 : tech ? -18.5 : early ? -21 : -19.5) + lateLift + hand,
        0.001, open ? 0.14 : tech || early ? 0.02 : 0.035, drumOrbitPan(t, spatialEventPan(t, hit + 4, 0.42))]);
    }
  }

  // Swung sixteenth shakers on Euclidean necklaces (Toussaint): E(k,16) with
  // k climbing 5→7→9→11 as the material hardens, the necklace rotated by 5
  // each bar — 5 is coprime to 16, so the rotation orbit visits all sixteen
  // phases before repeating. Accents land where a counter-rotating E(3,16)
  // agrees: coincidence, not a fixed backbeat. Timing feel is unchanged:
  // eager, odd sixteenths late, ±4.5 ms hand scatter, velocity random walk.
  const euclid = (k, n, rot) => {
    const hits = [];
    for (let i = 0; i < n; i++) {
      const j = (i + n - (rot % n)) % n;
      if (Math.floor((j + 1) * k / n) > Math.floor(j * k / n)) hits.push(i);
    }
    return hits;
  };
  for (let b = 1; b < BARS; b++) {
    if (inTechno(b)) continue; // techno strips to kick/rim/closed-hat
    const lateLift = b >= 54 ? 2.4 : b >= 36 ? 1.5 : b >= 18 ? 0.7 : 0;
    const k = [5, 7, 9, 11][Math.min(3, Math.floor(b / 18))];
    const accents = new Set(euclid(3, 16, (b * 7) % 16));
    for (const step of euclid(k, 16, (b * 5) % 16)) {
      if (b < 8 && step % 3 !== 0) continue;
      const hit = b * 16 + step;
      const swing = step % 2 ? SWING_16 : 0;
      const t = bar(b) + step * (BEAT / 4) + EAGER + swing + grooveJitter(hit, 113, 0.0045);
      const accent = accents.has(step) ? 2.1 : 0;
      const hand = (grooveUnit(hit, 127) - 0.5) * 3.2;
      noises.push([t, 0.026 + grooveUnit(hit, 131) * 0.022,
        5200 + 1700 * grooveUnit(hit, 137), 7600 + 1500 * grooveUnit(hit, 139),
        1.2 + 0.7 * grooveUnit(hit, 149), -28 + lateLift + accent + hand,
        0.001, 0.018 + 0.018 * grooveUnit(hit, 151), drumOrbitPan(t, spatialEventPan(t, hit, 0.5))]);
    }
  }

  // A coprime polymeter family over the 16-grid: a rim tick every 5
  // sixteenths (realigns with the bar every 5 bars) and, once the steel
  // half begins, a deeper wood tick every 3 (dotted-eighth pulse, 3-bar
  // cycle). With the bar itself that's periods {3,4,5} — the full pattern
  // only rephases every LCM = 15 bars, so the ear never catches it looping.
  for (let s = 8 * 16; s < BARS * 16; s += 5) {
    const t = bar(0) + s * (BEAT / 4) + EAGER * 0.5 + grooveJitter(s, 211, 0.003);
    noises.push([t, 0.03, 2600, 2200, 2.4, -29.5 + (grooveUnit(s, 223) - 0.5) * 2,
      0.001, 0.02, spatialEventPan(t, 900 + s, 0.55)]);
  }
  for (let s = 36 * 16; s < BARS * 16; s += 3) {
    if (inTechno(Math.floor(s / 16))) continue;
    const t = bar(0) + s * (BEAT / 4) + DRAG * 0.5 + grooveJitter(s, 227, 0.003);
    noises.push([t, 0.042, 1050, 850, 2.0, -30.5 + (grooveUnit(s, 229) - 0.5) * 2,
      0.001, 0.03, spatialEventPan(t, 940 + s, 0.5)]);
  }

  // Fibonacci-word ghost snares: the aperiodic binary word (s_n = s_{n-1} +
  // s_{n-2}) decides which off-sixteenths get a tiny tap. Self-similar,
  // never periodic, hit density exactly 1/φ² — structure without a loop.
  let fibA = "0", fibB = "01";
  while (fibB.length < BARS * 8) [fibA, fibB] = [fibB, fibB + fibA];
  let fibCursor = 0;
  for (let b = 18; b < BARS; b++)
    for (const step of [1, 3, 5, 7, 9, 11, 13, 15]) {
      if (fibB[fibCursor++ % fibB.length] !== "1") continue;
      if (inTechno(b)) continue; // cursor still advances — the word doesn't reset
      const hit = b * 16 + step;
      const t = bar(b) + step * (BEAT / 4) + DRAG + grooveJitter(hit, 233, 0.004);
      noises.push([t, 0.05, 1500 + 400 * grooveUnit(hit, 239), 950, 1.3,
        -31 + (grooveUnit(hit, 241) - 0.5) * 2.4,
        0.001, 0.035, step < 8 ? -0.14 : 0.14]);
    }

  // "Skipadoo" fills: the pattern skips a sixteenth, then answers with a
  // tight double. Four skip shapes rotate and the tone ramp flips direction
  // on odd fills, so no two consecutive fills say the same thing.
  const SKIP_PATTERNS = [
    [2.5, 2.75, 3.25, 3.5, 3.625, 3.75],
    [2.25, 2.75, 3.0, 3.5, 3.75],
    [2.5, 3.0, 3.25, 3.375, 3.625, 3.875],
    [2.75, 3.25, 3.5, 3.625, 3.75],
  ];
  for (const [fillIndex, b] of [7, 17, 25, 35, 43, 53, 61, 69].entries()) {
    if (b >= BARS) continue;
    const pattern = SKIP_PATTERNS[fillIndex % SKIP_PATTERNS.length];
    const flip = fillIndex % 2 ? pattern.length - 1 : 0;
    for (let i = 0; i < pattern.length; i++) {
      const ramp = Math.abs(i - flip); // rises on even fills, falls on odd
      const hit = fillIndex * 16 + i;
      const t = bar(b) + pattern[i] * BEAT + EAGER + grooveJitter(hit, 181, 0.003);
      noises.push([t, 0.038, 1450 + ramp * 170, 930 + ramp * 120, 1.6,
        -22.5 + i * 0.65 + (grooveUnit(hit, 191) - 0.5) * 2,
        0.001, 0.028, spatialEventPan(t, hit + 200, 0.62)]);
    }
  }

  // Reverse-snare intakes and 32nd-note "tttttt" hat rushes announce each
  // material mutation. The rush accelerates in brightness and width. Smaller
  // cousins (no intake, half the hats, quieter) pair with the surprise bell
  // rushes mid-mutation.
  for (const [turn, targetBar] of [18, 36, 54, 68].entries()) {
    if (targetBar >= BARS) continue;
    const target = bar(targetBar);
    noises.push([target - 0.72, 0.72, 850, 5200, 0.82, -19 + turn,
      0.64, 0.025, spatialEventPan(target, 300 + turn, 0.7)]);
    const count = 12;
    const start = target - 0.75 * BEAT;
    for (let i = 0; i < count; i++) {
      const t = start + i * (0.75 * BEAT / count) + EAGER * 0.35 + grooveJitter(i, 331 + turn, 0.0015);
      noises.push([t, 0.018 + 0.012 * (i / count), 7200 + i * 210, 9100 + i * 170,
        1.45, -28 + i * 0.78 + turn * 0.7, 0.0005, 0.014,
        spatialEventPan(t, 340 + turn * 16 + i, 0.78)]);
    }
  }
  for (const [turn, targetBar] of [27, 45, 62].entries()) {
    if (targetBar >= BARS) continue;
    const target = bar(targetBar);
    const count = 6;
    const start = target - 0.4 * BEAT;
    for (let i = 0; i < count; i++) {
      const t = start + i * (0.4 * BEAT / count) + EAGER * 0.35 + grooveJitter(i, 397 + turn, 0.0015);
      noises.push([t, 0.016 + 0.010 * (i / count), 7400 + i * 240, 9200 + i * 190,
        1.45, -32 + i * 0.9, 0.0005, 0.012,
        spatialEventPan(t, 420 + turn * 16 + i, 0.7)]);
    }
  }
}
// coda: the downbeat only, letting the stone ring between hits
if (!STONE_STUDY) for (let b = 76; b < 80; b++) kickAt(bar(b));
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
    const root = ROOTS[((b - d.a) / 2) % 4] * waveShift(b);
    if (STONE_STUDY || d.a >= 48) {
      // trance gallop: offbeat eighth stabs — the sidechain makes the pump.
      // The techno strip drops the gallop and keeps only the bare floor.
      for (let bb = b; bb < b + 2; bb++) {
        if (inTechno(bb)) continue;
        for (let k = 0; k < 4; k++)
          sines.push([bar(bb) + (k + 0.5) * BEAT, 0.16, root * 2, root * 2,
            -8, 0.005, 0.08, 0, 0, 0, 0]);
      }
      sines.push([bar(b), 2 * BAR - 0.08, root, root,
        -9, 0.06, 0.25, 0, 0, 0, 0]); // sub floor stays
    } else {
      subNote(bar(b), 2 * BAR - 0.08, root, -7.5);
    }
  }
if (!STONE_STUDY) subNote(bar(76), 4 * BAR - 0.1, ROOTS[0], -9); // coda holds the root
// each breath: the sub lets go and rises an octave into vapor
for (const b of BREATHS)
  sines.push([bar(b), 4 * BAR, 41.203, 82.407, -11, 1.0, 2.2, 0, 0, 0, 0]);

// ── bell line: one continuous pentatonic clock hand across the piece.
//    The old score restarted a random walk each bar, making the runs feel
//    looped. This cursor never resets: a long contour changes direction at
//    asymmetric intervals and reflects at the register edges. ──────────
let bellIndex = 2;
let bellDirection = 1;
let bellStep = 0;
const BELL_MOVES = [1, 1, 0, 2, -1, 1, -2, 1, 0, 1, 2, -1, 0, -2, 1, 1, -1];
// Where the melodic cursor stands at each bar's downbeat, as a MALLET_LADDER
// position — the roll and pickup passes below aim at these so the perc-nerd
// figures always land on the through-composed line, never beside it.
const barLead = [];
// One player, two mallets: while a roll or a pickup run is being played the
// eighth-note line must YIELD — otherwise two melodic streams double up.
// These predicates mirror the figure placement below exactly.
const PICKUP_SEAMS = [18, 36, 54];
const MINI_RUSH = [27, 40, 45, 62]; // surprise rush drops + the bar-40 breakout
// cliff runs: the line scrawls up to the top of the ladder and slides back
// down — the walk lays out for the whole bar while the hands climb
const CLIFF_BARS = [24, 42, 56, 64];
const pickupYield = (b, e) => STONE_CLUB &&
  ((PICKUP_SEAMS.includes(b + 1) && (b + 1 === 54 ? e >= 6 : e === 7)) ||
   (MINI_RUSH.includes(b + 1) && e === 7));
// During a chorus the composed hook takes the first two bars of every four;
// the walking line answers in the other two — call and response, never both.
const hookBar = (b) => STONE_CLUB && sectionAt(b).name === "chorus" && (b - sectionAt(b).a) % 4 < 2;
// The club line breathes: two incommensurate waves (13- and 7.3-bar periods)
// make sparse valleys and dense crests that never lock to the 4-bar grid.
// In a valley the player lays out and lets single strikes RING (long variable
// FEM decays); on a crest the eighths swing harder and sprout sixteenth fills.
// the intro's wave additionally ramps from 60% — the room assembles slowly
const sectionWave = (b) => {
  const s = sectionAt(b);
  return s.name === "intro" ? s.wave * (0.6 + 0.4 * (b / Math.max(1, s.z))) : s.wave;
};
const waveAt = (b) => (STONE_CLUB ? sectionWave(b) : 1) * (0.35 + 0.9 *
  (0.6 * (0.5 + 0.5 * Math.sin(TAU * b / 13 + 1.1)) +
   0.4 * (0.5 + 0.5 * Math.sin(TAU * b / 7.3))));
const sparseAt = (b) => STONE_CLUB && waveAt(b) < 0.62;
const rollBar = (b) => STONE_CLUB && b + 1 < BARS && (b + 1) % 4 === 0 &&
  bar(b) >= 45 && (b >= 54 || (b + 1) % 8 === 0) && !sparseAt(b + 1);
function bellRun(b, d) {
  const inSparseOpening = STONE_STUDY && bar(b) < TINY_BELL_END;
  const earlyDensity = inSparseOpening ? 0.48 : 1;
  // The club walks the section's mode ladder (the scale tour); the canonical
  // cut keeps its original pentatonic window untouched.
  const mode = STONE_CLUB ? modeAt(b) : null;
  const winLen = mode ? LANES[mode].len : DROP_NOTES.length;
  const winBase = mode ? LANES[mode].start : LADDER_BASE;
  const octSteps = mode ? MODES[mode].length : 5;
  // At most two ornaments a bar, and never two on one strike — a crest full
  // of independent flams/diddles/stops/fills smears the melody into confusion.
  let ornaments = 0;
  for (let e = 0; e < 8; e++) {
    if (bellStep > 0 && (bellStep % 11 === 0 || bellStep % 29 === 0))
      bellDirection *= -1;
    const move = BELL_MOVES[(bellStep + Math.floor(b / MUTATION) * 3) % BELL_MOVES.length];
    bellIndex += move * bellDirection;
    const POOL = waveNotes(b);
    while (bellIndex < 0 || bellIndex >= POOL.length) {
      if (bellIndex < 0) bellIndex = -bellIndex;
      if (bellIndex >= POOL.length) bellIndex = 2 * (POOL.length - 1) - bellIndex;
      bellDirection *= -1;
    }
    if (e === 0) barLead[b] = winBase + bellIndex;
    // Consume the density gate every slot so the pitch walk and the engine
    // textures stay aligned even where the club line lays out.
    const gate = rnd();
    const wave = STONE_CLUB ? waveAt(b) : 1;
    const sparse = sparseAt(b);
    // One player: the line yields while the hands are rolling or sweeping,
    // steps aside for the hook's two chorus bars (call and response), lays
    // out entirely for cliff runs and the whole stripped techno section.
    const yielded = (rollBar(b) && e >= 6) || pickupYield(b, e) || hookBar(b) ||
      (STONE_CLUB && CLIFF_BARS.includes(b)) || inTechno(b) ||
      (sparse && e % 2 === 1); // valleys drop the offbeat answers entirely
    if (!yielded && gate < d.density * earlyDensity * Math.min(1, wave + 0.25)) {
      // Most opening bells remain pinpricks, but a deliberately rare FEM
      // strike becomes a structural tone with its full physical decay.
      const longTail = inSparseOpening && bellStep % 23 === 5;
      bells.push({
        t: bar(b) + e * 0.5 * BEAT,
        note: POOL[bellIndex],
        vel: 0.55 + rnd() * 0.3,
        pan: (rnd() * 2 - 1) * 0.7,
        gain: Math.pow(10, (d.gainDb + (longTail ? -2.5 : 0)) / 20),
        material: d.mat, geometry: d.geom || "glass",
        dur: openingBloom ? [7, 8.5, 10][Math.floor(bellStep / 23) % 3]
          : ringDur ||
            (STONE_CLUB && wave > 1.02 ? Math.min(baseDur, 0.7) : baseDur),
        longTail: rings,
        // the bridge is heard through water — the whole line submerges
        ...(STONE_CLUB && sectionAt(b).name === "bridge"
          ? { warp: { depth: 0.025, hz: 0.5 } } : {}),
      };
      bells.push(strike);
      // The line harmonises: on right-hand strikes a quiet partner joins a
      // diatonic third below (verses) or sixth below (choruses) from the
      // section ladder — parallel organum riding the walk. Separate from the
      // ornament budget: this is a voice, not a decoration.
      if (STONE_CLUB && !rings && !sparse && !leftHand && b >= 8 && octSteps === 7 &&
          grooveUnit(bellStep, 617) < (sectionAt(b).name === "chorus" ? 0.42 : 0.26))
        bells.push({ ...strike,
          note: lnote(mode, li - (sectionAt(b).name === "chorus" ? 5 : 2)),
          vel: strike.vel * 0.5, pan: -strike.pan * 0.8,
          dur: Math.min(strike.dur, 1.2) });
      // Ornaments are mutually exclusive per strike and budgeted per bar —
      // one flam OR diddle OR octave stop OR fill, never a pile-up.
      if (STONE_CLUB && !rings && !sparse && ornaments < 2) {
        // Flam: a soft grace note one scale step below, 30 ms ahead of the
        // downbeat strike — the mallet brushes in before the accent.
        if (e === 0 && b % 2 === 0 && b >= 4 && grooveUnit(bellStep, 421) < 0.45) {
          bells.push({ ...strike, t: strike.t - 0.030,
            note: lnote(mode, li - 1), vel: strike.vel * 0.42,
            dur: 0.45, choke: true, pan: strike.pan * 0.5 });
          ornaments++;
        // Diddle: the left hand doubles its stroke into a tight sixteenth
        // pair; in the tubular mutation the second stroke is a dead stroke —
        // the mallet stays on the bar and chokes it.
        } else if (leftHand && b >= 36 && wave > 0.8 && grooveUnit(bellStep, 431) < 0.38) {
          bells.push({ ...strike, t: strike.t + 0.25 * BEAT,
            vel: strike.vel * 0.55, dur: Math.min(strike.dur, 0.7),
            choke: b < 54, pan: strike.pan * 0.7 });
          ornaments++;
        // Octave double-stop: the right hand's accent picks up its marimba
        // left-hand partner an octave below — color on the accent, kept rare
        // and soft enough that it never reads as a second melody.
        } else if (!leftHand && b >= 18 &&
            grooveUnit(bellStep, 441) < (b >= 54 ? 0.32 : 0.18)) {
          bells.push({ ...strike, note: lnote(mode, li - octSteps),
            vel: strike.vel * 0.68, pan: -strike.pan * 0.6 });
          ornaments++;
        // Crest fill: on a dense wave the right hand tucks in a soft choked
        // sixteenth neighbor a step below — motion, not a parallel line.
        } else if (!leftHand && wave > 1.02 && grooveUnit(bellStep, 459) < 0.3) {
          bells.push({ ...strike, t: strike.t + 0.25 * BEAT,
            note: lnote(mode, li - 1), vel: strike.vel * 0.5,
            dur: 0.45, choke: true, pan: strike.pan * 0.4 });
          ornaments++;
        }
      }
    }
    bellStep++;
  }
}
for (const d of DROPS) for (let b = d.a; b < d.z; b++) bellRun(b, d);

// The classic-xylo showpiece figures, aimed at the recorded bar leads:
// tremolo rolls that crescendo across beat four into the next downbeat, and
// George-Hamilton-Green pickup runs sweeping up the ladder into each 18-bar
// mutation seam. Every strike is stone, so all of it survives the canonical
// filter; every timing choice reads the groove clock.
if (STONE_CLUB) {
  const dropAt = (b) => DROPS.find((d) => b >= d.a && b < d.z) ?? DROPS.at(-1);
  // Rolls never repeat a shape twice in a row — three variants cycle:
  //   cresc — single-note tremolo swelling into the downbeat (the classic)
  //   dyad  — two-mallet tremolo alternating the lead with its lower neighbor
  //   rush  — an accelerating 32nd ladder climb, the "rush drop" — the
  //           figure that earns its place at every appearance
  let rollIndex = 0;
  const ROLL_SHAPES = ["rush", "cresc", "dyad", "rush", "cresc", "rush"];
  const playRush = (target, lead, mode, d, count, gainDb) => {
    // Grid-perfect: exact 32nds into the landing, the pitch staircase spaced
    // evenly up the ladder. No humanization here — at this speed, machine
    // placement is what reads as intent; jitter reads as sloppiness.
    const startIdx = Math.max(0, lead - 1 - Math.ceil(count * 0.8));
    for (let i = 1; i <= count; i++) {
      const t = target - i * (BEAT / 8);
      const frac = 1 - (i - 1) / Math.max(1, count - 1); // 0 far → 1 at landing
      const idx = clamp(Math.round(startIdx + (lead - 1 - startIdx) * frac), 0, LADDERS[mode].length - 1);
      bells.push({ t, note: midiName(LADDERS[mode][idx]),
        vel: 0.30 + 0.55 * frac,
        pan: spatialEventPan(t, 620 + i, 0.55),
        gain: Math.pow(10, gainDb / 20), material: d.mat,
        geometry: d.geom || "glass", dur: 0.45, choke: true });
      squareShadow.push({ t, midi: LADDERS[mode][idx], vel: 0.3 + 0.4 * frac });
    }
  };
  for (let b = 0; b + 1 < BARS; b++) {
    if (!rollBar(b) || inTechno(b) || inTechno(b + 1)) continue;
    const d = dropAt(b);
    const m1 = modeAt(b + 1);
    const oct1 = MODES[m1].length;
    const lead = barLead[b + 1] ?? barLead[b] ?? LANES[m1].start + 2;
    const shape = ROLL_SHAPES[rollIndex++ % ROLL_SHAPES.length];
    const rollGain = Math.pow(10, (d.gainDb - 1.5) / 20);
    if (shape === "rush") {
      playRush(bar(b + 1), lead, m1, d, 10, d.gainDb - 1.5);
    } else {
      for (let i = 0; i < 8; i++) {
        const t = bar(b) + 3 * BEAT + i * (BEAT / 8); // exact 32nds — see playRush
        bells.push({ t,
          note: lnote(m1, shape === "dyad" && i % 2 ? lead - 1 : lead),
          vel: 0.34 + 0.05 * (i % 2) + 0.28 * (i / 7),
          pan: spatialEventPan(t, 600 + b, 0.4) + (i % 2 ? 0.14 : -0.14),
          gain: rollGain, material: d.mat, geometry: d.geom || "glass",
          dur: 0.45, choke: true });
      }
    }
    // the roll resolves into the line's own downbeat — punctuate it with
    // the left hand landing the octave below
    if (lead - oct1 >= 0)
      bells.push({ t: bar(b + 1), note: lnote(m1, lead - oct1), vel: 0.85,
        pan: -0.2, gain: Math.pow(10, d.gainDb / 20),
        material: d.mat, geometry: d.geom || "glass", dur: 0.7 });
  }
  // Seam pickups each get their own shape so the big sweeps evolve:
  //   18 — five-note ascending sextuplet (the introduction)
  //   36 — a descending turn from above the lead, answering 18
  //   54 — the full two-octave ladder gliss (the payoff stays)
  for (const seam of PICKUP_SEAMS) {
    const next = dropAt(seam);
    const m = modeAt(seam); // the pickup announces the ARRIVING mode
    const top = LADDERS[m].length - 1;
    const lead = barLead[seam] ?? LANES[m].start + 2;
    const gainDb = next.gainDb - 1;
    if (seam === 36) {
      const turn = [Math.min(top, lead + 2), Math.min(top, lead + 1), lead - 1, lead - 2]
        .filter((i) => i >= 0);
      for (let k = 0; k < turn.length; k++) {
        const t = bar(seam) - (turn.length - k) * (BEAT / 6); // exact sextuplets
        bells.push({ t, note: lnote(m, turn[k]),
          vel: 0.44 + 0.08 * k, pan: spatialEventPan(t, 700 + seam + k, 0.5),
          gain: Math.pow(10, gainDb / 20), material: next.mat,
          geometry: next.geom || "glass", dur: 0.45, choke: true });
        squareShadow.push({ t, midi: LADDERS[m][clamp(turn[k], 0, LADDERS[m].length - 1)], vel: 0.4 });
      }
    } else {
      const notes = seam === 54 ? Math.max(2, lead) : Math.min(5, lead);
      const spacing = seam === 54 ? BEAT / 12 : BEAT / 6;
      for (let k = notes; k >= 1; k--) {
        const t = bar(seam) - k * spacing; // exact grid — the gliss is a machine
        bells.push({ t, note: lnote(m, lead - k),
          vel: 0.44 + 0.09 * (notes - k),
          pan: spatialEventPan(t, 700 + seam + k, 0.5),
          gain: Math.pow(10, gainDb / 20),
          material: next.mat, geometry: next.geom || "glass",
          dur: 0.45, choke: true });
        squareShadow.push({ t, midi: LADDERS[m][clamp(lead - k, 0, LADDERS[m].length - 1)], vel: 0.42 });
      }
    }
  }
  // Surprise rush drops mid-mutation — the bar-54 compound rush was the
  // track's best moment, so smaller cousins land where nobody expects them,
  // skipping any that would rush into a sparse valley.
  for (const b of MINI_RUSH) {
    if (b >= BARS || sparseAt(b) || inTechno(b)) continue;
    const d = dropAt(b);
    // the breakout at 40 rushes longer and louder — it IS the switch-up
    const breakout = b === 40;
    playRush(bar(b), barLead[b] ?? LANES[modeAt(b)].start + 2, modeAt(b), d,
      breakout ? 12 : 7, d.gainDb - (breakout ? 1 : 2.5));
  }
  // ── cliff runs: the line scrawls up to the top of the cliff — a two-gear
  //    accelerating climb into the ladder's top octaves — plants ONE super-
  //    high fast-attack LONG-decay glass bell at the summit, then slides
  //    back down with a decelerating run onto the next bar's lead. High
  //    notes taper in gain the further above E5 they reach: sparkle, never
  //    tang. Placed at the crests of the complex sections. ────────────────
  for (const b of CLIFF_BARS) {
    if (b + 1 >= BARS) continue;
    const m = modeAt(b);
    const lad = LADDERS[m];
    const d = dropAt(b);
    const startIdx = barLead[b] ?? LANES[m].start + 2;
    const peakIdx = Math.min(lad.length - 1, startIdx + 12);
    const hiTaper = (idx) => Math.max(0, lad[idx] - 76) * 0.3; // dB per semitone above E5
    // ascent: four 16ths then eight 32nds — two exact gears of acceleration
    let t = bar(b);
    for (let i = 0; i < 12; i++) {
      const idx = Math.round(startIdx + (peakIdx - startIdx) * (i / 12));
      bells.push({ t, note: midiName(lad[idx]), vel: 0.5 + 0.3 * (i / 12),
        pan: spatialEventPan(t, 1000 + b * 16 + i, 0.5),
        gain: Math.pow(10, (d.gainDb - 2 - hiTaper(idx)) / 20),
        material: "stone", geometry: d.geom || "glass",
        dur: 0.45, choke: true, cliff: true });
      squareShadow.push({ t, midi: lad[idx], vel: 0.35 + 0.25 * (i / 12) });
      t += i < 4 ? BEAT / 4 : BEAT / 8;
    }
    // the summit: struck once, rings for nine seconds — glass, not granite
    bells.push({ t, note: midiName(lad[peakIdx]), vel: 0.95, pan: 0,
      gain: Math.pow(10, (d.gainDb - 6.5) / 20),
      material: "glass", geometry: "glass", dur: 9,
      longTail: true, cliff: true });
    // the slide down: a decelerating descent landing on the next downbeat
    const landIdx = barLead[b + 1] ?? startIdx;
    let td = t + BEAT / 8;
    for (let i = 1; i <= 8; i++) {
      const idx = Math.round(peakIdx + (landIdx - peakIdx) * (i / 9));
      bells.push({ t: td, note: midiName(lad[idx]), vel: 0.62 - 0.03 * i,
        pan: spatialEventPan(td, 1040 + b * 16 + i, 0.5),
        gain: Math.pow(10, (d.gainDb - 2.5 - hiTaper(idx)) / 20),
        material: "stone", geometry: d.geom || "glass",
        dur: 0.45, choke: true, cliff: true });
      td += i < 5 ? BEAT / 8 : BEAT / 4;
    }
  }
  // The chorus hook: one composed two-bar riff — the same CONTOUR every
  // chorus, so it stays hummable, but voiced in scale DEGREES so each
  // chorus's mode re-colours it: minor in dorian, brightening through
  // mixolydian, arriving major in ionian. It owns the first two bars of
  // each four (the walking line yields there and answers in the next two).
  // The last chorus doubles it an octave below. Exact grid, center-weighted.
  const HOOK = [ // [beat, laneDegree, weight] — degrees {1,3,4,5,7} of the mode
    [0, 0, 1], [0.75, 2, 0.8], [1.5, 4, 1], [2.5, 6, 0.9], [3.25, 4, 0.72],
    [3.5, 3, 0.85], [4, 0, 1], [4.75, 2, 0.8], [5.5, 3, 0.95], [6.5, 2, 0.8],
    [7, 0, 1.05],
  ];
  for (const s of CLUB_SECTIONS) {
    if (s.name !== "chorus") continue;
    const m = s.mode;
    const base = LANES[m].start;
    const oct = MODES[m].length;
    for (let p = s.a; p + 1 < s.z; p += 4) {
      const d = dropAt(p);
      for (const [beat, deg, w] of HOOK) {
        const t = bar(p) + beat * BEAT;
        bells.push({ t, note: lnote(m, base + deg), vel: Math.min(0.98, 0.85 * w),
          pan: 0.12 * Math.sin(beat), gain: Math.pow(10, (d.gainDb + 2) / 20),
          material: d.mat, geometry: d.geom || "glass", dur: 0.7 });
        if (s.a >= 54)
          bells.push({ t, note: lnote(m, base + deg - oct), vel: Math.min(0.98, 0.85 * w * 0.7),
            pan: -0.15, gain: Math.pow(10, (d.gainDb + 2) / 20),
            material: d.mat, geometry: d.geom || "glass", dur: 0.7 });
      }
    }
  }

  // Quaternion-rotated motif: a four-note cell whose contour is literally
  // rotated in 3-space by successive elements of the binary tetrahedral
  // group (2T). Each appearance spins the same intervallic seed to a new
  // orientation, requantized onto the pentatonic ladder — the melody turns
  // in a higher dimension. It answers "at times": the tail of every chorus
  // and through the bridge, where there is room to hear it. Stone, <bar 68,
  // pentatonic, under E5 — it survives the canonical filter untouched.
  // ── the lullaby: before the club, the fairy tale. The classic wattajetta
  //    hook, alone, as a music box — plain glass bells an octave up, at half
  //    speed, glinting through the crack under the door while the cap is
  //    still on. The whole arrangement is this little melody's fever dream.
  const LULLABY = [[0, "E5"], [1.5, "G5"], [3, "B5"], [5, "D6"], [6.5, "B5"],
    [7, "A5"], [8, "E5"], [9.5, "G5"], [11, "A5"], [13, "G5"], [14, "E5"]];
  for (const [beat, note] of LULLABY)
    bells.push({ t: bar(0.5) + beat * BEAT, note, vel: 0.5,
      pan: 0.25 * Math.sin(beat * 0.9), gain: Math.pow(10, -14 / 20),
      material: "glass", geometry: "glass", dur: 2.4, longTail: true, cliff: true });

  const QUAT_SPOTS = [22, 43, 46, 48, 60]; // clear of cliffs and the techno strip
  const CELL = [2, 1, 3]; // seed intervals (ladder steps) — the hook's shape
  let qk = 8;             // start on the tetrahedral vertices (true 120° turns)
  for (const b of QUAT_SPOTS) {
    if (b >= BARS || inTechno(b)) continue;
    const m = modeAt(b);
    const lad = LADDERS[m];
    const d = dropAt(b);
    const v = qRotate(QUAT_2T[qk % QUAT_2T.length], CELL);
    qk += 5; // stride the group so consecutive spots are far apart
    const base = (barLead[b] ?? LANES[m].start + 2) - 1;
    let pos = base, prev = base;
    const steps = [0, Math.round(v[0]), Math.round(v[1]), Math.round(v[2])];
    for (let i = 0; i < 4; i++) {
      pos = clamp(prev + steps[i], 0, lad.length - 1);
      prev = pos;
      const t = bar(b) + i * (BEAT / 3) + grooveJitter(b * 4 + i, 601, 0.002);
      bells.push({ t, note: midiName(lad[pos]),
        vel: 0.58 + 0.08 * i, pan: spatialEventPan(t, 900 + qk + i, 0.7),
        gain: Math.pow(10, (d.gainDb - 0.5) / 20),
        material: "stone", geometry: d.geom || "glass", dur: 1.1, quat: true });
    }
  }
}

// Clearly audible opening blooms: these are not reverbs or extended chokes,
// but full 10–14 second FEM bodies in less-damped metals. Their asymmetric
// placement lets the resonances overlap organically beneath the tiny pings.
if (STONE_STUDY) {
  const blooms = [
    [2,  "E4", "glass", "glass", 14, -12.5, -0.42],
    [7,  "B4", "glass", "glass", 12, -13.0,  0.36],
    [12, "G4", "glass", "glass", 14, -13.5, -0.18],
    [18, "D5", "glass", "glass", 12, -14.0,  0.44],
    [24, "A4", "glass", "glass", 10, -13.0, -0.32],
  ];
  for (const [b, note, material, geometry, dur, db, pan] of blooms)
    bells.push({ t: bar(b * STONE_SCALE) + 0.25 * BEAT, note, vel: 0.82, pan,
      gain: Math.pow(10, db / 20), material, geometry, dur, longTail: true });
}

// ── bloops: actual water-drip percussion — a sine chirping UP into its
//    note the way a drip rings a pool, on the swung offbeats ────────────
function bloop(t, hz, db) {
  sines.push([t, 0.11, hz * 0.55, hz, db, 0.006, 0.07, (rnd() * 2 - 1) * 0.5, 0, 0, 0]);
}
for (const d of DROPS) {
  if (!d.bloops) continue;
  for (let b = d.a; b < d.z; b++)
    for (const slot of [1.75, 3.25, 3.75])
      if (rnd() < d.bloops) {
        // the club outro drips dry — a lone chirp on the fade reads as a
        // stray boop, not water — and the techno strip has no water at all
        if (STONE_CLUB && (b >= 66 || inTechno(b))) { rnd(); continue; }
        // club drips tune to the section mode (E3-octave scale tones)
        const pool = STONE_CLUB
          ? MODES[modeAt(b)].map((s) => MIDI_HZ(52 + s))
          : BLOOP_HZ;
        bloop(bar(b) + slot * BEAT, pool[Math.floor(rnd() * pool.length)],
          -17);
      }
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
function choirChord(t, dur, db, thirdHz = 196.0) {
  choirNote(t, dur, 82.407, db - 2, 0);      // e2
  choirNote(t, dur, 123.47, db - 3, -0.35);  // b2
  choirNote(t, dur, 164.81, db, 0.35);       // e3
  choirNote(t, dur, thirdHz, db - 4, -0.15); // the chord color — minor or major third
}
function choirStack(t, dur, db, notes) {
  notes.forEach((n, i) =>
    choirNote(t, dur, noteHz(n), db - (i === 0 ? 2 : i * 1.4),
              i === 0 ? 0 : ((i % 2 ? 1 : -1) * (0.2 + 0.12 * i))));
}
// Each wave is announced by its own chord and held under the whole span,
// so the harmony reads as a place rather than as an event.
if (STONE_720) {
  WAVES.forEach((w, i) => {
    const a = i * WAVE_BARS;
    if (a >= BARS) return;
    choirStack(bar(a), Math.min(WAVE_BARS, BARS - a) * BAR, -21, w.chord);
    choirStack(bar(a), 6 * BAR, -17, w.chord); // the swell that names it
  });
}
choirChord(bar(28 * STONE_SCALE), 4 * BAR, -19);
choirChord(bar(44 * STONE_SCALE), 4 * BAR, -17);
choirChord(bar(60 * STONE_SCALE), 4 * BAR, -17);
choirChord(bar(80 * STONE_SCALE), 8 * BAR, -20); // the mist hums it one last time

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
for (const b of BREATHS) {
  chimeCluster(bar(b) + BEAT, INDUSTRIAL ? 2 : 5, INDUSTRIAL ? -24 : -20);
  chimeCluster(bar(b + 2) + 2 * BEAT, INDUSTRIAL ? 2 : 4, INDUSTRIAL ? -25 : -22);
}
if (!INDUSTRIAL) for (const b of [82, 86, 91]) chimeCluster(bar(b), 3, -23);

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
  for (let b = d.a; b < d.z; b += 2) {
    // In canonical's sparse opening, bowls arrive every four bars instead
    // of every two. The kick and sine floor remain; only bell traffic thins.
    if (STONE_STUDY && bar(b) < TINY_BELL_END && ((b - d.a) / 2) % 2 === 1) continue;
    bells.push({ t: bar(b), note: ROOT_BELLS[((b - d.a) / 2) % 4],
                 vel: 0.7, pan: (b / 2) % 2 === 0 ? -0.2 : 0.2,
                 gain: Math.pow(10, -16 / 20),
                 material: d.bowl, geometry: "bowl", dur: 4.5 });
  }

// ── breaths, coda, mist: long bowls + tolls; the next material always
//    tolls once before its drop arrives; breath 4 bells come through
//    water (warp = slow LFO resample) ──────────────────────────────────
const longBowl = (t, note, mat, db, dur = 7, extra = {}) =>
  bells.push({ t, note, vel: 0.8, pan: 0, gain: Math.pow(10, db / 20),
               material: mat, geometry: "bowl", dur: WORLD ? Math.min(dur, 4.4) : dur, ...extra });
if (!STONE_STUDY) {
  longBowl(bar(12), "E4", "silver", -15, 7, { morph: WORLD });
  longBowl(bar(44), "E4", "brass", -15, 7, { morph: WORLD });
  longBowl(bar(60), "E4", "gold", -15, 7, { warp: { depth: 0.03, hz: 0.7 }, morph: WORLD });
  longBowl(bar(76), "E3", "stone", -14, 8, { morph: WORLD });
  longBowl(bar(80), "E4", "silver", -17, WORLD ? 6 : 9,
           { warp: { depth: 0.035, hz: 0.55 }, morph: WORLD });
}
// foreshadow tolls — the ear learns each material before its drop
const foreshadow = [[30, "E5", "bronze"], [46, "B5", "steel"], [62, "G5", "stone"]];
for (const [b, n, m] of foreshadow)
  bells.push({ t: bar(b * STONE_SCALE), note: n, vel: 0.6, pan: 0.3, gain: Math.pow(10, -16 / 20),
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
if (!STONE_STUDY) for (const [b, n] of MIST_TOLLS)
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

// Chordal material gates: the solo pentatonic line occasionally resolves into
// a physical stone voicing. Tiny hand-strums keep the chord from behaving like
// a keyboard block; later voicings wobble farther through the material.
if (STONE_STUDY) {
  // Gate voicings are lane degrees, so each gate rings in its section's
  // mode — the 54 gate arrives as a major-seventh in ionian, the door to
  // the final chorus. Canonical keeps the fixed pentatonic names.
  const chordGates = [
    { b: 18, notes: ["E4", "G4", "B4"], degs: [0, 2, 4], geom: "handbell", dur: 1.6, db: -15.5, wobble: 0.010 },
    { b: 36, notes: ["E4", "A4", "D5"], degs: [0, 3, 6], geom: "tubular", dur: 1.6, db: -15.0, wobble: 0.017 },
    { b: 54, notes: ["E4", "G4", "B4", "D5"], degs: [0, 2, 4, 6], geom: "glass", dur: 1.6, db: -14.2, wobble: 0.025 },
    { b: 67, notes: ["E4", "B4", "E5"], degs: [0, 4, 7], geom: "church", dur: 2.5, db: -16.0, wobble: 0.032 },
  ];
  for (const [gateIndex, gate] of chordGates.entries()) {
    const gm = modeAt(gate.b);
    for (let i = 0; i < gate.notes.length; i++) {
      const t = bar(gate.b) + i * (0.014 + gateIndex * 0.003);
      bells.push({ t,
        note: STONE_CLUB ? lnote(gm, LANES[gm].start + gate.degs[i]) : gate.notes[i],
        vel: 0.64 + i * 0.045,
        pan: spatialEventPan(t, 500 + gateIndex * 8 + i, 0.76),
        gain: Math.pow(10, (gate.db - i * 0.45) / 20), material: "stone",
        geometry: gate.geom, dur: gate.dur, longTail: true,
        // the club keeps its gates nearly dry — ±25-cent chord wobble read
        // as the track going out of tune at the tubular mutation
        warp: { depth: gate.wobble * (STONE_CLUB ? 0.2 : 1),
                hz: 0.42 + gateIndex * 0.11 + i * 0.035 } });
    }
  }
}

if (STONE_STUDY) {
  // Remove the parent arrangement's aluminum/bronze foreshadows and its
  // out-of-range coda. The locked form is strictly the bar-64 stone vocabulary.
  const canonicalStone = bells.filter((s) => s.t < bar(68 * STONE_SCALE) && s.material === "stone");
  bells.length = 0;
  bells.push(...canonicalStone);
  bells.push({ t: bar(63 * STONE_SCALE), note: "E4", vel: 0.55, pan: 0,
    gain: Math.pow(10, -17 / 20), material: "stone",
    geometry: "church", dur: 2.5 });
}

// Redundancy sweep + line audit. Independent figures (rolls landing, octave
// stops, bowls, chord gates) can legally ask for the same note at the same
// instant — one mallet can't strike a bar twice at once, so the sweep keeps
// the stronger strike and drops the double. The audit then reports the
// breathing (strikes-per-bar spread), the long-ring count, and asserts no
// unchoked same-note near-unison survived.
if (STONE_CLUB) {
  bells.sort((x, y) => x.t - y.t);
  const dropped = new Set();
  for (let i = 0; i < bells.length; i++) {
    if (dropped.has(i)) continue;
    const s = bells[i];
    if (s.choke) continue;
    for (let j = i + 1; j < bells.length && bells[j].t - s.t < 0.04; j++) {
      const o = bells[j];
      if (dropped.has(j) || o.choke || o.note !== s.note) continue;
      dropped.add(o.gain * o.vel <= s.gain * s.vel ? j : i);
      if (dropped.has(i)) break;
    }
  }
  if (dropped.size) {
    const kept = bells.filter((_, i) => !dropped.has(i));
    bells.length = 0;
    bells.push(...kept);
  }
  const perBar = new Array(BARS).fill(0);
  let ringing = 0, doubled = 0;
  for (let i = 0; i < bells.length; i++) {
    const s = bells[i];
    perBar[Math.min(BARS - 1, Math.floor(s.t / BAR))]++;
    if (s.dur >= 2.5) ringing++;
    for (let j = i + 1; j < bells.length && bells[j].t - s.t < 0.04; j++)
      if (bells[j].note === s.note && !s.choke && !bells[j].choke) doubled++;
  }
  const counts = perBar.slice(0, 68);
  console.log(`line audit: ${bells.length} strikes (${dropped.size} doubles swept) · ${Math.min(...counts)}–${Math.max(...counts)}/bar · ${ringing} ringing ≥2.5s · ${doubled} same-note near-unisons`);
  if (doubled > 0) { console.error("✗ melody doubling above tolerance"); process.exit(1); }
}

// ── flybys: sine dopplers at the seams, quiet ─────────────────────────
function flyby(t, dir) {
  const d = 2 * BAR;
  sines.push([t, d, 760, 185, -17, d * 0.4, d * 0.45, -dir, dir, 0, 0]);
}
flyby(bar(13 * STONE_SCALE), 1);
flyby(bar(29 * STONE_SCALE), -1);
flyby(bar(53 * STONE_SCALE), 1);
flyby(bar(70 * STONE_SCALE), -1);

// ── spray, whisper level: transition breaths only. Canonical deliberately
//    has no bar-zero splash; it was too startling before the groove settled.
for (const d of DROPS)
  if (!(STONE_STUDY && d.a === 0))
    noises.push([bar(d.a), 2.0, 2000, 300, 0.8, -23, 0.015, 1.8, 0]);
if (!STONE_STUDY) noises.push([bar(80), 16 * BAR + 4, 3000, 500, 0.8, -27, 2.0, 14 * BAR, 0]);
noises.push([0, DUR, 1100, 1100, 0.6, -41, 4, 4, 0]);

// ── bake + render the engine part ─────────────────────────────────────
const fmt = (rows) => rows.map((r) => "  " + r.map((v) => +v.toFixed(5)).join(" ")).join("\n");
const score = [
  `sr ${SR}`,
  `dur ${DUR.toFixed(3)}`,
  `normpeak 0.82`,
  `fadein ${STONE_CLUB ? "0.02" : "0.004"}`,
  `fadeout ${STONE_CLUB ? "0.10" : "4.0"}`,
  `sidechain ${STONE_CLUB ? "0.008 0.17 -8" : "0.015 0.2 -6"}`,
  `kick ${kicks.length}`, fmt(kicks),
  `sine ${sines.length}`, fmt(sines),
  `noise ${noises.length}`, fmt(noises),
].join("\n") + "\n";

const scorePath = resolve(OUT, `${outputStem}.score.txt`);
writeFileSync(scorePath, score);
if (process.argv.includes("--score")) { console.log(score); process.exit(0); }
console.log(`baked ${kicks.length} kicks, ${sines.length} sines, ${noises.length} sprays, ${bells.length} bells`);

const rawPath = resolve(OUT, `${outputStem}.f32.raw`);
const kickPath = resolve(OUT, `${outputStem}.kick.f32.raw`);
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
// Club stems are true-summing premaster busses. Every linear arrangement
// operation below is mirrored onto its source bus, while the nonlinear master
// remains on the reference mix only.
const waterBus = STEMS ? new Float32Array(mix) : null;
const stemKickBus = STEMS ? new Float32Array(kickBus) : null;
const bellBus = STEMS ? new Float32Array(mix.length) : null;
const trashBus = STEMS ? new Float32Array(mix.length) : null;
// Ableton-friendly granularity: the sample voices, the synth voices, and
// the guitar each get their own true-summing stem.
const sampleBus = STEMS ? new Float32Array(mix.length) : null; // owls/phones/modem/punches/castanets/stamp
const synthBus = STEMS ? new Float32Array(mix.length) : null;  // disco bass + square shadows
const gtrBus = STEMS ? new Float32Array(mix.length) : null;    // power chords + threads
// The buffers are resident now; release their ~100 MB of temporary disk before
// the final mixed bus is written. This matters on the small fleet Macs.
try { unlinkSync(rawPath); } catch {}
try { unlinkSync(kickPath); } catch {}
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
    if (STONE_CLUB) {
      // The long cut earns its granite: glassier open, hardening steadily
      // toward the canonical 0.85-and-past by the final mutation.
      return 0.6 + 0.3 * smooth(Math.max(0, Math.min(1, t / DUR)));
    }
    if (STONE_STUDY) return 0.85;
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
      if (waterBus) {
        const w = waterBus[2 * f + ch];
        waterBus[2 * f + ch] = w * (1 - m) + (Math.tanh(w * DRIVE) / norm) * m;
      }
      const k = kickBus[2 * f + ch];
      kickBus[2 * f + ch] = k * (1 - m) + (Math.tanh(k * DRIVE) / norm) * m;
      if (stemKickBus) {
        const sk = stemKickBus[2 * f + ch];
        stemKickBus[2 * f + ch] = sk * (1 - m) + (Math.tanh(sk * DRIVE) / norm) * m;
      }
    }
  }
}

const bank = new Map();
const bellFor = ({ note, material, geometry, dur, renderDur = dur }) => {
  const k = `${note}/${material}/${geometry}/${renderDur}`;
  if (!bank.has(k)) {
    const cacheDir = resolve(OUT, ".wattajetta-bell-cache-v1");
    // FEM glass v2 has lower structural loss, a thinner profile, and its own
    // contact model. Preserve the useful old cache for every unaffected bell.
    const modelVersion = material === "glass" ? "-fem2" : "";
    const cacheKey = `${note}-${material}-${geometry}-${dur}${modelVersion}`;
    const leftPath = resolve(cacheDir, `${cacheKey}-L.f32`);
    const rightPath = resolve(cacheDir, `${cacheKey}-R.f32`);
    const validCache = existsSync(leftPath) && existsSync(rightPath)
      && statSync(leftPath).size > 0
      && statSync(leftPath).size === statSync(rightPath).size
      && statSync(leftPath).size % 4 === 0;
    if (validCache) {
      const left = readFileSync(leftPath), right = readFileSync(rightPath);
      bank.set(k, {
        L: new Float32Array(left.buffer.slice(left.byteOffset, left.byteOffset + left.byteLength)),
        R: new Float32Array(right.buffer.slice(right.byteOffset, right.byteOffset + right.byteLength)),
      });
    } else {
      try { unlinkSync(leftPath); } catch {}
      try { unlinkSync(rightPath); } catch {}
      console.log(`  bell ${note} (${material}/${geometry} ×${dur}s)…`);
      const rendered = renderBell({ note, material, geometry, dur });
      mkdirSync(cacheDir, { recursive: true });
      const leftTmp = `${leftPath}.tmp-${process.pid}`;
      const rightTmp = `${rightPath}.tmp-${process.pid}`;
      try {
        writeFileSync(leftTmp, Buffer.from(rendered.L.buffer, rendered.L.byteOffset, rendered.L.byteLength));
        writeFileSync(rightTmp, Buffer.from(rendered.R.buffer, rendered.R.byteOffset, rendered.R.byteLength));
        renameSync(leftTmp, leftPath);
        renameSync(rightTmp, rightPath);
      } finally {
        try { unlinkSync(leftTmp); } catch {}
        try { unlinkSync(rightTmp); } catch {}
      }
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
  // The hybrid's opening wants the FEM materials as percussion, not wash:
  // choke every bell before 0:45 to a tiny 65–105 ms ping. Later bells keep
  // their composed tails so the material arc can still open up dramatically.
  // `choke` is a played dead stroke (mallet held on the bar) — it stays a
  // 65–105 ms ping even after the opening's tiny-bell window opens up.
  const tiny = s.choke || (TINY_BELLS && s.t < TINY_BELL_END && !s.longTail);
  const chokeFrames = Math.floor((0.065 + 0.04 * Math.min(1, s.vel ?? 0.5)) * SR);
  const n = Math.min(L.length, ns - at, tiny ? chokeFrames : Number.MAX_SAFE_INTEGER);
  const fade = tiny
    ? Math.min(Math.floor(0.028 * SR), Math.floor(n * 0.38))
    : Math.min(Math.floor(0.08 * SR), Math.floor(n * 0.2)); // always click-safe
  for (let i = 0; i < n; i++) {
    const env = i > n - fade ? (n - i) / fade : 1;
    const now = s.t + i / SR;
    let snareDuck = 1;
    for (const snare of snares) {
      const since = now - snare.t;
      if (since >= 0 && since < 0.19) {
        const attack = 0.008;
        const shape = since < attack
          ? smooth(since / attack)
          : 1 - smooth((since - attack) / (0.19 - attack));
        snareDuck = Math.min(snareDuck, 1 - 0.58 * snare.strength * shape);
      }
    }
    const addL = L[i] * gl * env * snareDuck;
    const addR = R[i] * gr * env * snareDuck;
    if (STEMS && (!Number.isFinite(addL) || !Number.isFinite(addR))) {
      console.error("✗ non-finite bell contribution", { strike: s, frame: i,
        left: L[i], right: R[i], gl, gr, env, snareDuck });
      process.exit(1);
    }
    mix[2 * (at + i)] += addL;
    mix[2 * (at + i) + 1] += addR;
    if (bellBus) {
      bellBus[2 * (at + i)] += addL;
      bellBus[2 * (at + i) + 1] += addR;
    }
  }
}

// The local macOS Empty Trash sound becomes a small transition toy: three
// progressively slower throws land just before the 18-bar material mutations.
// Keep it on a dedicated bus so every placement can be muted or rebuilt.
const EMPTY_TRASH = "/System/Library/Components/CoreAudio.component/Contents/SharedSupport/SystemSounds/finder/empty trash.aif";
if (TRASH_SAMPLE && existsSync(EMPTY_TRASH)) {
  const decoded = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error",
    "-i", EMPTY_TRASH, "-f", "f32le", "-ar", String(SR), "-ac", "2", "-"],
    { maxBuffer: 8 * 1024 * 1024 });
  if (decoded.status !== 0) { console.error("✗ Empty Trash decode failed"); process.exit(1); }
  const b = decoded.stdout;
  const sample = new Float32Array(b.buffer.slice(b.byteOffset, b.byteOffset + b.length - (b.length % 4)));
  const throws = [
    { t: bar(3) - 0.6, rate: 0.55, db: 7, pan: -0.08 }, // early, and SLOW — the first thing thrown overboard
    { t: bar(18) - 0.72, rate: 1.18, db: 8, pan: -0.12 },
    { t: bar(36) - 0.82, rate: 0.95, db: 7, pan: 0.12 },
    { t: bar(54) - 1.00, rate: 0.78, db: 6, pan: 0 },
  ];
  for (const s of throws) {
    const at = Math.floor(s.t * SR);
    const frames = Math.floor(sample.length / 2 / s.rate);
    const a = (s.pan + 1) * 0.25 * Math.PI;
    const gl = Math.cos(a) * Math.pow(10, s.db / 20);
    const gr = Math.sin(a) * Math.pow(10, s.db / 20);
    for (let i = 0; i < frames && at + i < ns; i++) {
      const pos = i * s.rate;
      const j = Math.floor(pos);
      if (j + 1 >= sample.length / 2) break;
      const fr = pos - j;
      const l = sample[2 * j] * (1 - fr) + sample[2 * (j + 1)] * fr;
      const r = sample[2 * j + 1] * (1 - fr) + sample[2 * (j + 1) + 1] * fr;
      mix[2 * (at + i)] += l * gl;
      mix[2 * (at + i) + 1] += r * gr;
      if (trashBus) {
        trashBus[2 * (at + i)] += l * gl;
        trashBus[2 * (at + i) + 1] += r * gr;
      }
    }
  }
}

// ── prox percussion: the fleet's own dings. Two live prompt rocks — vimib
//    (a slab ping) and mugot (a slab beep, this very session) — become
//    fully dynamic physical gestures in a virtual room, never one-shot
//    clicks: THROWS (dice across the table — bounces tighten exponentially,
//    fade, pitch creeps up, and travel across the stereo field), SHAKES
//    (the cup rattle — a granular crescendo of micro-hits), and
//    shake-into-throw (rattle the cup, roll the dice). Dense conversation
//    in the intro, then a gesture every few bars for the whole flight;
//    the machine strip keeps its dice. ─────────────────────────────────────
const decodeSample = (path) => {
  const p = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error",
    "-i", path, "-f", "f32le", "-ar", String(SR), "-ac", "1", "-"],
    { maxBuffer: 32 * 1024 * 1024 });
  if (p.status !== 0) return null;
  const b = p.stdout;
  return new Float32Array(b.buffer.slice(b.byteOffset, b.byteOffset + b.length - (b.length % 4)));
};
if (STONE_CLUB) {
  const vimibSrc = decodeSample(resolve(HERE, "../assets/clicks/vimib.wav")); // tight, high
  const mugotSrc = decodeSample(resolve(HERE, "../assets/clicks/mugot.wav")); // woodier, lower
  if (vimibSrc && mugotSrc) {
    const clickFrom = (src, rate) => {
      const n = Math.min(Math.floor(0.08 * SR), Math.floor(src.length / rate));
      const out = new Float32Array(Math.max(1, n));
      const fade = Math.floor(0.012 * SR);
      for (let i = 0; i < n; i++) {
        const pos = i * rate;
        const j = Math.floor(pos), fr = pos - j;
        const v = src[j] * (1 - fr) + (src[j + 1] ?? 0) * fr;
        out[i] = v * (i > n - fade ? (n - i) / fade : 1) * Math.min(1, i / 40);
      }
      return out;
    };
    const placeClick = (t, buf, db, pan) => {
      const at = Math.floor(t * SR);
      const a = (clamp(pan, -0.95, 0.95) + 1) * 0.25 * Math.PI;
      const gl = Math.cos(a) * Math.pow(10, db / 20);
      const gr = Math.sin(a) * Math.pow(10, db / 20);
      for (let i = 0; i < buf.length && at + i < ns; i++) {
        mix[2 * (at + i)] += buf[i] * gl;
        mix[2 * (at + i) + 1] += buf[i] * gr;
        if (sampleBus) {
          sampleBus[2 * (at + i)] += buf[i] * gl;
          sampleBus[2 * (at + i) + 1] += buf[i] * gr;
        }
      }
    };
    // a die thrown across the room: bounces tighten by the restitution,
    // soften ~1 dB each, pitch creeps up, and the pan travels p0 → p1
    const bounceThrow = (t0, src, baseRate, db, p0, p1, count, rest, seed) => {
      let t = t0, dt = 0.16 + 0.1 * grooveUnit(seed, 851);
      for (let n = 0; n < count && dt > 0.016; n++) {
        placeClick(t, clickFrom(src, baseRate * Math.pow(1.04, n)),
          db - n * 1.0 + (grooveUnit(seed + n, 853) - 0.5) * 2,
          p0 + (p1 - p0) * (1 - Math.pow(rest, n)));
        t += dt; dt *= rest;
      }
      return t;
    };
    // the cup rattle: a granular stream of micro-hits, crescendo or fade
    const shakeRoll = (t0, src, baseRate, db, pan, durS, cresc, seed) => {
      let t = t0, n = 0;
      while (t < t0 + durS && n < 64) {
        const prog = (t - t0) / durS;
        const u = grooveUnit(seed + n, 857);
        placeClick(t, clickFrom(src, baseRate * (0.94 + 0.12 * u)),
          db - 7 + 7 * (cresc ? prog : 1 - prog) + (u - 0.5) * 3,
          pan + (grooveUnit(seed + n, 859) - 0.5) * 0.35);
        t += 0.02 + 0.035 * grooveUnit(seed + n, 861);
        n++;
      }
      return t;
    };
    // the gesture score: [bar, kind, timbre(0 vimib · 1 mugot), panFrom, panTo]
    const GESTURES = [
      [0.25, "shakeThrow", 0, -0.7, 0.5], [1.5, "throw", 1, 0.6, -0.4],
      [2.75, "shake", 0, -0.2, 0], [4.0, "shakeThrow", 1, 0.5, -0.6],
      [6.0, "throw", 0, -0.5, 0.7], [10.5, "throw", 1, 0.4, -0.5],
      [15.0, "shakeThrow", 0, -0.6, 0.6], [21.0, "throw", 1, 0.5, -0.3],
      [27.5, "shake", 0, 0.3, 0], [33.0, "throw", 0, -0.4, 0.6],
      [39.5, "shakeThrow", 1, 0.6, -0.6], [44.5, "throw", 0, -0.3, 0.4],
      [49.5, "shake", 1, 0.0, 0], [51.25, "throw", 0, -0.6, 0.6],
      [53.0, "shakeThrow", 1, 0.5, -0.5], [58.0, "throw", 0, -0.5, 0.5],
      [63.0, "shakeThrow", 1, 0.4, -0.6], [67.0, "throw", 0, -0.3, 0.5],
      [70.0, "shake", 1, 0.2, 0],
    ];
    for (const [gb, kind, tim, p0, p1] of GESTURES) {
      const src = tim ? mugotSrc : vimibSrc;
      const baseRate = tim ? 1.4 : 2.1;
      const db = gb < 8 ? -19 : -23;
      const seed = Math.floor(gb * 16);
      const t0 = bar(gb);
      if (kind === "throw") {
        bounceThrow(t0, src, baseRate, db, p0, p1, 9, 0.72, seed);
      } else if (kind === "shake") {
        shakeRoll(t0, src, baseRate, db, p0, BEAT, true, seed);
      } else {
        const te = shakeRoll(t0, src, baseRate, db - 2, p0, BEAT, true, seed);
        bounceThrow(te, src, baseRate, db, p0, p1, 8, 0.7, seed + 99);
      }
    }
  }
}

// ── owls: real hoots from the /pop sample sources (Freesound, cached and
//    attributed in the vault) — the night around the water. One close call
//    in the intro, a distant answer over the bridge, a last call in the
//    outro. The bridge owl is heard through the water like everything else.
if (STONE_CLUB) {
  const owlAt = (file, t, db, pan, warp = null) => {
    const src = decodeSample(resolve(HERE, "../assets/owls", file));
    if (!src) { console.log(`  (owl missing: ${file})`); return; }
    const n = Math.min(src.length, Math.floor(6.5 * SR));
    const at = Math.floor(t * SR);
    const a = (clamp(pan, -0.9, 0.9) + 1) * 0.25 * Math.PI;
    const gl = Math.cos(a) * Math.pow(10, db / 20);
    const gr = Math.sin(a) * Math.pow(10, db / 20);
    const fade = Math.floor(0.4 * SR);
    let pos = 0;
    for (let i = 0; i < n && at + i < ns; i++) {
      const wob = warp ? 1 + warp.depth * Math.sin(TAU * warp.hz * (i / SR)) : 1;
      const j = Math.floor(pos), fr = pos - j;
      if (j + 1 >= src.length) break;
      const v = src[j] * (1 - fr) + src[j + 1] * fr;
      const env = Math.min(1, i / fade, (n - i) / fade);
      mix[2 * (at + i)] += v * gl * env;
      mix[2 * (at + i) + 1] += v * gr * env;
      if (sampleBus) {
        sampleBus[2 * (at + i)] += v * gl * env;
        sampleBus[2 * (at + i) + 1] += v * gr * env;
      }
      pos += wob;
    }
  };
  owlAt("465697-owl_hoot.mp3", bar(1.5), -17, -0.4);
  owlAt("784609-birds_of_prey_barred_owl_hooting_distant.mp3", bar(46), -19, 0.4,
    { depth: 0.03, hz: 0.5 });
  owlAt("465697-owl_hoot.mp3", bar(68.5), -18, -0.25);
}

// ── telephones (Freesound, vault-attributed). The dial tone is the real
//    350+440 Hz dual tone, rate-shifted so its two tones LAND ON the
//    section's scale degrees — it harmonizes instead of clashing: ×1.1225
//    puts it on G+B (the iii dyad) over the E floor; ×0.9375 puts it on
//    E+G# for the bright modes. The rotary bell (dominant ≈1616 Hz) rings
//    pitch-MATCHED — resampled so its gong lands on the current bar lead,
//    two octaves up. A phone heard from outside the club; someone answers
//    at the bridge; one last call before the end. ─────────────────────────
if (STONE_CLUB) {
  const phoneAt = (file, t, { db, pan, rate = 1, maxDur = 6, fadeIn = 0.3, fadeOut = 0.5, warp = null }) => {
    const src = decodeSample(resolve(HERE, "../assets/phones", file));
    if (!src) { console.log(`  (phone missing: ${file})`); return; }
    const at = Math.floor(t * SR);
    const n = Math.min(Math.floor(src.length / rate), Math.floor(maxDur * SR));
    const a = (clamp(pan, -0.9, 0.9) + 1) * 0.25 * Math.PI;
    const gl = Math.cos(a) * Math.pow(10, db / 20);
    const gr = Math.sin(a) * Math.pow(10, db / 20);
    const fi = Math.floor(fadeIn * SR), fo = Math.floor(fadeOut * SR);
    let pos = 0;
    for (let i = 0; i < n && at + i < ns; i++) {
      const wob = warp ? 1 + warp.depth * Math.sin(TAU * warp.hz * (i / SR)) : 1;
      const j = Math.floor(pos), fr = pos - j;
      if (j + 1 >= src.length) break;
      const v = src[j] * (1 - fr) + src[j + 1] * fr;
      const env = Math.min(1, i / Math.max(1, fi), (n - i) / Math.max(1, fo));
      mix[2 * (at + i)] += v * gl * env;
      mix[2 * (at + i) + 1] += v * gr * env;
      if (sampleBus) {
        sampleBus[2 * (at + i)] += v * gl * env;
        sampleBus[2 * (at + i) + 1] += v * gr * env;
      }
      pos += rate * wob;
    }
  };
  const DIAL = "97789-dial_wav.mp3";           // measured 352 + 440 Hz
  const RING = "663840-1970_telephone_ring_1_4_seconds_cycle_nl_wav.mp3"; // dominant ≈1616 Hz
  // off the hook outside the club — G+B over the bare E floor, fading as
  // the door opens
  phoneAt(DIAL, bar(0.5), { db: -24, pan: 0.3, rate: 1.1225, maxDur: bar(5), fadeIn: 1.5, fadeOut: 3 });
  // the bridge answers in lydian — E+G#, heard through the water
  phoneAt(DIAL, bar(45), { db: -25, pan: -0.3, rate: 0.9375, maxDur: bar(3), fadeIn: 2, fadeOut: 2.5,
    warp: { depth: 0.03, hz: 0.45 } });
  // pitch-matched rotary calls: the gong lands on the bar lead, 2 octaves up
  const ringAt = (b, db, pan) => {
    const m = modeAt(b);
    const leadMidi = LADDERS[m][clamp((barLead[Math.floor(b)] ?? LANES[m].start + 2), 0, LADDERS[m].length - 1)];
    const target = MIDI_HZ(Math.min(100, leadMidi + 24));
    phoneAt(RING, bar(b), { db, pan, rate: clamp(target / 1616, 0.55, 1.7), maxDur: 3.2, fadeIn: 0.05, fadeOut: 0.8 });
  };
  ringAt(16.5, -21, 0.45);   // ringing as chorus 1 approaches
  ringAt(47.5, -20, -0.4);   // ringing unanswered into the machine strip
  ringAt(64.5, -22, 0.35);   // one last call at the final crest
}

// ── punches: fight-foley impacts (assets/punches/ — drop the SF6 stash in
//    the same folder and the next render uses it). They land where fists
//    belong: rimshot accents through the machine strip and the final-chorus
//    phrase changes — combo hits on the biggest turns. ────────────────────
if (STONE_CLUB) {
  const PUNCH_FILES = ["209392-kung_fu_punch_1.mp3", "209393-kung_fu_punch_2.mp3",
    "209490-kung_fu_punch_3.mp3", "209627-kung_fu_punch_4.mp3",
    "210896-punch_2.mp3", "210897-kung_fu_punch_5.mp3"];
  const punchBank = PUNCH_FILES.map((f) => decodeSample(resolve(HERE, "../assets/punches", f)));
  const punchAt = (t, which, db, pan) => {
    const src = punchBank[which % punchBank.length];
    if (!src) return;
    const at = Math.floor(t * SR);
    const n = Math.min(src.length, Math.floor(0.9 * SR));
    const a = (clamp(pan, -0.9, 0.9) + 1) * 0.25 * Math.PI;
    const gl = Math.cos(a) * Math.pow(10, db / 20);
    const gr = Math.sin(a) * Math.pow(10, db / 20);
    for (let i = 0; i < n && at + i < ns; i++) {
      const env = Math.min(1, (n - i) / (0.08 * SR));
      mix[2 * (at + i)] += src[i] * gl * env;
      mix[2 * (at + i) + 1] += src[i] * gr * env;
      if (sampleBus) {
        sampleBus[2 * (at + i)] += src[i] * gl * env;
        sampleBus[2 * (at + i) + 1] += src[i] * gr * env;
      }
    }
  };
  // machine strip: a hit on beat 2 of every other bar, harder each time
  let pi = 0;
  for (let b = 49; b < 54; b++) {
    if (b % 2 === 1) punchAt(bar(b) + BEAT, pi++, -13 + (b - 49) * 0.6, b % 4 < 2 ? -0.3 : 0.3);
  }
  // final chorus: combo hits on the phrase changes; a 1-2 on the last one
  for (let b = 54; b < 66; b += 4) {
    punchAt(bar(b), pi++, -12, 0.25);
    punchAt(bar(b) + 0.5 * BEAT, pi++, -15, -0.3); // the answer jab
  }
  punchAt(bar(64) + 3.5 * BEAT, pi++, -11.5, 0);   // the finisher into the last crest
}

// ── MODEM: the track as protocol. Samples from pop/samples/modem/ (pitch-
//    analyzed in its manifest; rates land carrier tones on E-world targets):
//    the phone DIALS behind the wall, the techno strip IS the handshake
//    negotiation (a B2-locked carrier + data-burst clicks), a 1200-baud
//    carrier whistles under the lydian water, and the AT&T disconnect hangs
//    up the whole track inside the shoegaze fade. ─────────────────────────
if (STONE_CLUB) {
  const MODEM_DIR = resolve(HERE, "../../samples/modem");
  const modemAt = (file, t, { rate = 1, rateEnd = null, db, pan = 0, maxDur = 5, fadeIn = 0.25, fadeOut = 0.6, warp = null }) => {
    const src = decodeSample(resolve(MODEM_DIR, file));
    if (!src) { console.log(`  (modem missing: ${file})`); return; }
    const at = Math.floor(t * SR);
    const avgRate = rateEnd != null ? (rate + rateEnd) / 2 : rate;
    const n = Math.min(Math.floor(src.length / avgRate), Math.floor(maxDur * SR));
    const a = (clamp(pan, -0.9, 0.9) + 1) * 0.25 * Math.PI;
    const gl = Math.cos(a) * Math.pow(10, db / 20);
    const gr = Math.sin(a) * Math.pow(10, db / 20);
    const fi = Math.max(1, Math.floor(fadeIn * SR)), fo = Math.max(1, Math.floor(fadeOut * SR));
    let pos = 0;
    for (let i = 0; i < n && at + i < ns; i++) {
      const wob = warp ? 1 + warp.depth * Math.sin(TAU * warp.hz * (i / SR)) : 1;
      const r = rateEnd != null ? rate + (rateEnd - rate) * smooth(i / n) : rate; // the takeoff glide
      const j = Math.floor(pos), fr = pos - j;
      if (j + 1 >= src.length) break;
      const v = src[j] * (1 - fr) + src[j + 1] * fr;
      const env = Math.min(1, i / fi, (n - i) / fo);
      mix[2 * (at + i)] += v * gl * env;
      mix[2 * (at + i) + 1] += v * gr * env;
      if (sampleBus) {
        sampleBus[2 * (at + i)] += v * gl * env;
        sampleBus[2 * (at + i) + 1] += v * gr * env;
      }
      pos += r * wob;
    }
  };
  // ── the door: a soft boomy TAKEOFF — the low handshake carrier tuned to
  //    E2, gliding up a full octave over the first six bars. Deep enough to
  //    pass straight through the wall's dampening (lows always do) — the
  //    runway rumble you feel before you're inside. A soft tubular RTTY
  //    carrier answers high through the crack.
  modemAt("454649-modem_3_aif.mp3", bar(0), { rate: 0.6868, rateEnd: 1.374, db: -13, pan: 0,
    maxDur: 10.5, fadeIn: 2.5, fadeOut: 3 });
  modemAt("109147-rtty_45_1000hz_ogg.mp3", bar(2), { rate: 0.654, db: -28, pan: -0.3,
    maxDur: 5, fadeIn: 1.5, fadeOut: 2 });
  // the phone dials behind the wall — B3-tuned, barely there, answering
  // the dial-tone drone before the door opens
  modemAt("658932-dial_up_sound_mp3_flac.mp3", bar(5), { rate: 0.5612, db: -26, pan: 0.35, maxDur: 4.5, fadeIn: 1 });
  // section-seam data artifacts: one tiny burst announcing each chorus
  modemAt("397079-digitalradio_noise4_wav.mp3", bar(18) - 0.4, { rate: 0.5488, db: -24, pan: -0.5, maxDur: 0.8, fadeIn: 0.05, fadeOut: 0.3 });
  modemAt("397079-digitalradio_noise4_wav.mp3", bar(36) - 0.4, { rate: 0.7325, db: -24, pan: 0.5, maxDur: 0.8, fadeIn: 0.05, fadeOut: 0.3 });
  // the 1200-baud carrier whistles under the lydian water, E6-locked
  modemAt("78657-modem1200_wav.mp3", bar(46), { rate: 0.6388, db: -27, pan: 0.25, maxDur: 4.5, fadeIn: 1.5, fadeOut: 1.5, warp: { depth: 0.03, hz: 0.5 } });
  // THE NEGOTIATION: the techno strip is a handshake — a B2-locked carrier
  // groans under the machine while data clicks spray the offbeats
  modemAt("454649-modem_3_aif.mp3", bar(49), { rate: 1.0289, db: -19, pan: 0, maxDur: 6.5, fadeIn: 0.8, fadeOut: 1 });
  for (let b = 49; b < 54; b++)
    for (const s of [3, 6, 10, 13])
      if (grooveUnit(b * 16 + s, 911) < 0.55)
        modemAt("8037-modem_1_53_wav.mp3", bar(b) + s * (BEAT / 4),
          { rate: 0.5494 * (1 + (grooveUnit(b * 16 + s, 913) - 0.5) * 0.1), db: -22,
            pan: (grooveUnit(b * 16 + s, 917) - 0.5) * 1.4, maxDur: 0.22, fadeIn: 0.005, fadeOut: 0.08 });
  // the login sequence rises into the stamp and the drop
  modemAt("49608-dialup_login_dec_2001_24_bit_wav.mp3", bar(52.5), { rate: 0.5345, db: -20, pan: -0.2, maxDur: 2.6, fadeIn: 0.4, fadeOut: 0.4 });
  // and at the very end, the protocol hangs up inside the haze
  modemAt("844723-at_t_internet_gateway_modem_ups_w_modem_disconne.mp3", bar(70.5),
    { rate: 0.5118, db: -20, pan: 0.15, maxDur: 4, fadeIn: 0.3, fadeOut: 2 });
}

// ── the aesthetic dot computer stamp — the /pop signature, placed by the
//    house convention (hellsine): 3.5 s before the climax drop, pitched up
//    ×1.18 and harmonized in a triadic stack (fifth + octave above, a slow
//    ×0.78 body underneath). Here it speaks out of the bare techno machine
//    right before the gliss slams into the major-key final chorus — and it
//    trails two protocol echoes, because this track is a protocol. ────────
if (STONE_CLUB) {
  const stampSrc = decodeSample(resolve(HERE, "../assets/aesthetic-dot-computer.wav"));
  if (stampSrc) {
    const stampAt = (t, rate, db, pan) => {
      const at = Math.floor(t * SR);
      const n = Math.floor(stampSrc.length / rate);
      const a = (clamp(pan, -0.9, 0.9) + 1) * 0.25 * Math.PI;
      const gl = Math.cos(a) * Math.pow(10, db / 20);
      const gr = Math.sin(a) * Math.pow(10, db / 20);
      let pos = 0;
      for (let i = 0; i < n && at + i < ns; i++) {
        const j = Math.floor(pos), fr = pos - j;
        if (j + 1 >= stampSrc.length) break;
        const v = stampSrc[j] * (1 - fr) + stampSrc[j + 1] * fr;
        const env = Math.min(1, i / 200, (n - i) / (0.04 * SR));
        mix[2 * (at + i)] += v * gl * env;
        mix[2 * (at + i) + 1] += v * gr * env;
        if (sampleBus) {
          sampleBus[2 * (at + i)] += v * gl * env;
          sampleBus[2 * (at + i) + 1] += v * gr * env;
        }
      }
    };
    const sT = bar(54) - 3.5;
    stampAt(sT, 1.18, -4, 0);                  // main, pitched up — front and center
    stampAt(sT + 0.025, 1.18 * 1.5, -10, -0.35); // fifth above
    stampAt(sT + 0.045, 1.18 * 2.0, -13, 0.35);  // octave above
    stampAt(sT + 0.06, 0.78, -10.5, 0);          // slow body underneath
    stampAt(sT + 0.42, 1.18, -14, 0.5);          // protocol echo 1
    stampAt(sT + 0.84, 1.18, -20, -0.5);         // protocol echo 2
  }
}

// ── power chords: distorted electric guitar for the ending high points.
//    Synthesized in-lane — three Karplus-Strong strings (root/fifth/octave;
//    no third, so the chord is mode-proof), double-tracked left/right with
//    ±6-cent detune, driven hard through tanh and a cab-style lowpass.
//    Stabs land on the final-chorus phrase changes with palm-mute chugs
//    driving into each one; the whole bus ducks under the kick. ───────────
if (STONE_CLUB) {
  const gtr = new Float32Array(ns * 2);
  // striation: after the pick, tiny noise keeps re-exciting the string — a
  // scratchy bowed fizz in the sustain instead of a clean KS decay
  const gtrString = (t, hz, dur, damp, level, ch, striation = 0.004) => {
    const period = Math.max(2, Math.round(SR / hz));
    const buf = new Float32Array(period);
    let lp = 0;
    const seed = ((t * 997) | 0) + period + ch;
    let s = (Math.imul(seed + 1, 2654435761) ^ 0x9e3779b9) >>> 0;
    const rand = () => (s = (Math.imul(s, 1664525) + 1013904223) >>> 0) / 4294967296;
    for (let j = 0; j < period; j++) {
      lp += 0.82 * ((rand() * 2 - 1) - lp); // hard pick
      buf[j] = lp;
    }
    const at = Math.floor(t * SR), n = Math.floor(dur * SR);
    for (let i = 0; i < n && at + i < ns; i++) {
      const j = i % period;
      gtr[2 * (at + i) + ch] += buf[j] * level * Math.min(1, (n - i) / (0.25 * SR));
      buf[j] = damp * 0.5 * (buf[j] + buf[(j + 1) % period])
        + (rand() * 2 - 1) * striation * (1 - i / n);
    }
  };
  // the twizzle: as a chord tapers, its timbre UNRAVELS — the plucked
  // string blooms into a triangle wave mid-decay, then dissolves into raw
  // sawtooth at the tail, while a pitch rattle (two incommensurate wobble
  // rates, deepening as the chord dies) shakes it apart. Same drive/cab
  // chain, so the morph stays inside the amp.
  const twizzle = (t, hz, dur, level, ch, detCents) => {
    const at = Math.floor(t * SR), n = Math.floor(dur * 1.25 * SR);
    const f = hz * Math.pow(2, detCents / 1200);
    let phase = 0;
    for (let i = 0; i < n && at + i < ns; i++) {
      const u = i / (dur * SR);
      const triW = Math.max(0, Math.sin(Math.PI * clamp((u - 0.25) / 0.55, 0, 1)));
      const sawW = Math.max(0, Math.sin(Math.PI * clamp((u - 0.6) / 0.65, 0, 1)));
      if (triW === 0 && sawW === 0) { phase += f / SR; continue; }
      const rat = 0.006 + 0.035 * Math.min(1.25, u);      // the rattle deepens
      const tt = i / SR;
      const wob = 1 + rat * (0.6 * Math.sin(TAU * 9.7 * tt) + 0.4 * Math.sin(TAU * 13.3 * tt + 1.7));
      phase += (f * wob) / SR;
      const p = phase - Math.floor(phase);
      const tri = 1 - 4 * Math.abs(p - 0.5);
      const saw = 2 * p - 1;
      const env = Math.exp(-u * 1.6) * level;
      gtr[2 * (at + i) + ch] += (tri * 0.4 * triW + saw * 0.34 * sawW) * env;
    }
  };
  // sine threads: quiet pure tones that peel off the chord and drift like
  // threads in the wind — their pitch envelope is CV-mediated by a slow
  // random-walk (a new gust every 50 ms, chased lazily), the drift deepening
  // as the chord unravels underneath them.
  const thread = (t, hz, dur, level, seedBase, pan) => {
    const at = Math.floor(t * SR), n = Math.floor(dur * 1.3 * SR);
    const a = (clamp(pan, -0.9, 0.9) + 1) * 0.25 * Math.PI;
    const gl = Math.cos(a), gr = Math.sin(a);
    let phase = 0, drift = 0, target = 0;
    for (let i = 0; i < n && at + i < ns; i++) {
      const u = i / (dur * SR);
      if (i % 2400 === 0) target = (grooveUnit(seedBase + ((i / 2400) | 0), 941) - 0.5) * 2.5;
      drift += (target - drift) * 0.0018; // the lazy chase — wind, not vibrato
      phase += hz * Math.pow(2, (drift * (0.3 + u)) / 12) / SR;
      const w = Math.max(0, Math.sin(Math.PI * clamp((u - 0.15) / 0.9, 0, 1)));
      const v = Math.sin(TAU * phase) * w * level * Math.exp(-u * 1.2);
      gtr[2 * (at + i)] += v * gl;
      gtr[2 * (at + i) + 1] += v * gr;
    }
  };
  // long-drain chords: five strings a side (root/fifth/octave + two ±15¢
  // haze strings), rung so each chord still sounds when the next arrives
  const powerChord = (t, rootMidi, dur, level, damp = 0.99955, striation = 0.004) => {
    for (const [off, lv] of [[0, 1], [7, 0.8], [12, 0.6]])
      for (const [ch, cents] of [[0, -6], [1, 6]]) {
        gtrString(t + (ch ? 0.006 : 0), MIDI_HZ(rootMidi + off) * Math.pow(2, cents / 1200),
          dur, damp, level * lv * 0.42, ch, striation);
        if (dur > 1) // stabs and sustains unravel; palm-mute chugs stay plucked
          twizzle(t + (ch ? 0.006 : 0), MIDI_HZ(rootMidi + off), dur, level * lv * 0.5, ch, cents);
      }
    if (dur > 1) // three threads lift off the chord tones, one per interval
      for (const [k, off] of [[0, 12], [1, 19], [2, 24]].map((x, i) => [i, x[1]]))
        thread(t + 0.3 + k * 0.22, MIDI_HZ(rootMidi + off), dur, level * 0.2,
          ((t * 131) | 0) + k * 37, k === 0 ? -0.5 : k === 1 ? 0.5 : 0);
    for (const [ch, cents] of [[0, -15], [1, 15]]) // the haze pair
      gtrString(t + 0.012, MIDI_HZ(rootMidi + 12) * Math.pow(2, cents / 1200),
        dur * 1.2, Math.min(0.99985, damp + 0.0002), level * 0.22, ch, striation * 1.6);
  };
  // final chorus: a stab on every 2-bar phrase change, chugs driving in;
  // sustains overlap the next chord — the wall starts assembling
  for (let b = 54; b < 66; b += 2) {
    const rootMidi = 40 + progSemis(b, ((b - 54) / 2) % 4); // E2 region
    for (const chug of [2.5, 3, 3.5])
      powerChord(bar(b - 1) + chug * BEAT, rootMidi, 0.16, 0.5, 0.965, 0.002); // palm mutes
    powerChord(bar(b), rootMidi, 4.6, 0.85, 0.99955, 0.004 + (b - 54) * 0.0009);
  }
  powerChord(bar(64), 40, 5.5, 1, 0.9997, 0.012); // the summit chord — already fraying
  // the outro IS the shoegaze: three long haze chords, maximum striation,
  // draining into each other all the way through the fade
  powerChord(bar(66), 40, 8, 0.8, 0.99975, 0.018);
  powerChord(bar(68), 43, 8, 0.72, 0.99978, 0.022);
  powerChord(bar(70), 40, 10, 0.66, 0.9998, 0.028);
  // drive (climbing toward the end) → cab lowpass → DC-safe highpass
  const tEnd = bar(72);
  for (let ch = 0; ch < 2; ch++) {
    let cab = 0, hpX = 0, hpY = 0;
    const aCab = 1 - Math.exp(-2 * Math.PI * 3400 / SR);
    for (let i = 0; i < ns; i++) {
      const x = gtr[2 * i + ch];
      if (x === 0 && cab === 0 && hpY === 0) continue;
      const t = i / SR;
      const drv = 7 + 6 * smooth(clamp((t - bar(54)) / (tEnd - bar(54)), 0, 1)); // 7 → 13: gnarlier as it goes
      const driven = Math.tanh(x * drv) / Math.tanh(drv);
      cab += aCab * (driven - cab);
      const y = cab - hpX + 0.996 * hpY; // ~30 Hz DC-safe highpass
      hpX = cab; hpY = y;
      gtr[2 * i + ch] = y;
    }
  }
  // MBV smear: from the last cliff onward a cross-fed feedback delay washes
  // the bus into haze — by the fade it's all bloom, no attack
  {
    const d = Math.floor(0.187 * SR);
    const from = Math.floor(bar(63) * SR);
    for (let i = from; i < ns; i++) {
      const t = i / SR;
      const fb = 0.2 + 0.42 * smooth(clamp((t - bar(63)) / (bar(70) - bar(63)), 0, 1));
      if (i - d >= 0) {
        gtr[2 * i] += gtr[2 * (i - d) + 1] * fb;       // cross-fed L←R
        gtr[2 * i + 1] += gtr[2 * (i - d)] * fb * 0.94; // R←L
      }
    }
  }
  const kickTimes2 = kicks.map((k) => k[0]).sort((x, y) => x - y);
  let ki2 = 0;
  const G = Math.pow(10, -16 / 20);
  for (let i = 0; i < ns; i++) {
    const t = i / SR;
    while (ki2 + 1 < kickTimes2.length && kickTimes2[ki2 + 1] <= t) ki2++;
    const since = t - kickTimes2[ki2];
    const duck = since >= 0 && since < 0.14 ? 1 - 0.45 * Math.exp(-since / 0.045) : 1;
    const l = gtr[2 * i] * G * duck, r = gtr[2 * i + 1] * G * duck;
    mix[2 * i] += l; mix[2 * i + 1] += r;
    if (gtrBus) { gtrBus[2 * i] += l; gtrBus[2 * i + 1] += r; }
  }
}

// ── ukulele: one stringed voice on the boat — a nylon Karplus-Strong pluck,
//    no samples. Warm open strums follow the bowl roots two bars at a time
//    (ringing longest in the valleys), choked "chnk" skanks ride the crest
//    offbeats, and a bright open E rings each mutation seam's resolution.
//    Voicings stay inside the pentatonic set and under the E5 ceiling. ────
if (STONE_CLUB) {
  // Tuned a whole octave below a real uke — baritone-nylon register, so the
  // long tails sit under the stone bells instead of tangling with them.
  // Voicings are stacked scale thirds on the progression degree, built from
  // the section ladder — the uke re-tunes with the tour (quartal-ish in the
  // pentatonic bookends, real triads in the modal middle).
  const modeChord = (b, progIdx) => {
    const m = modeAt(b);
    const lad = LADDERS[m];
    const L = MODES[m].length;
    const r = progDegs(m)[progIdx];
    return [r, r + 2, r + 4, r + L].map((i) => MIDI_HZ(lad[clamp(i, 0, lad.length - 1)]));
  };
  const ukeRand = (a) => {
    let s = (Math.imul(a + 1, 2654435761) ^ 0x9e3779b9) >>> 0;
    return () => (s = (Math.imul(s, 1664525) + 1013904223) >>> 0) / 4294967296;
  };
  const pluck = (hz, dur, damp, bright, seed) => {
    const rand = ukeRand(seed);
    const period = Math.max(2, Math.round(SR / hz));
    const n = Math.floor(dur * SR);
    const out = new Float32Array(n);
    const buf = new Float32Array(period);
    let lp = 0;
    for (let j = 0; j < period; j++) {
      lp += bright * ((rand() * 2 - 1) - lp); // nylon: pre-softened burst
      buf[j] = lp;
    }
    for (let i = 0; i < n; i++) {
      const j = i % period;
      out[i] = buf[j] * Math.min(1, (n - i) / (0.05 * SR)); // click-safe tail
      buf[j] = damp * 0.5 * (buf[j] + buf[(j + 1) % period]);
    }
    return out;
  };
  const strum = (t, chord, { db, dur, damp = 0.9965, bright = 0.55, up = false, pan = 0 }) => {
    const g = Math.pow(10, db / 20);
    const order = up ? [...chord].reverse() : chord;
    if (SCORE_JSON) for (const hz of chord)
      scoreExtra.uke.push({ t: +t.toFixed(4), hz: +hz.toFixed(2), db, dur, pan: +pan.toFixed(3) });
    for (let s = 0; s < order.length; s++) {
      const wave = pluck(order[s], dur, damp, bright, Math.floor(t * 977) + s);
      const at = Math.floor((t + s * 0.013) * SR);
      const a = (clamp(pan + (s - 1.5) * 0.09, -0.9, 0.9) + 1) * 0.25 * Math.PI;
      const gl = Math.cos(a) * g, gr = Math.sin(a) * g;
      for (let i = 0; i < wave.length && at + i < ns; i++) {
        mix[2 * (at + i)] += wave[i] * gl;
        mix[2 * (at + i) + 1] += wave[i] * gr;
        if (bellBus) {
          bellBus[2 * (at + i)] += wave[i] * gl;
          bellBus[2 * (at + i) + 1] += wave[i] * gr;
        }
      }
    }
  };
  for (const d of DROPS)
    for (let b = d.a; b < d.z; b += 2) {
      if (inTechno(b)) continue; // no strings in the machine strip
      const chord = modeChord(b, ((b - d.a) / 2) % 4);
      const w = waveAt(b);
      // 10× decay: the strings barely damp, so successive strums overlay
      // into a harp-like wash — a couple dB quieter to pay for the overlap
      strum(bar(b) + 0.01 + grooveJitter(b, 501, 0.004), chord, {
        db: w < 0.62 ? -16.5 : -20, dur: w < 0.62 ? 24 : 11,
        damp: 0.99965, pan: spatialEventPan(bar(b), 800 + b, 0.3),
      });
      for (const bb of [b, b + 1]) {
        if (waveAt(bb) <= 0.9) continue;
        for (const beat of [1.5, 3.5])
          strum(bar(bb) + beat * BEAT + EAGER + grooveJitter(bb * 4 + beat * 2, 503, 0.003),
            chord, { db: -21.5, dur: 0.09, damp: 0.86, bright: 0.8,
              up: beat > 2, pan: 0.25 });
      }
    }
  for (const seam of PICKUP_SEAMS)
    strum(bar(seam) + 0.02, modeChord(seam, 0),
      { db: -17, dur: 22, damp: 0.99965, bright: 0.65, pan: -0.15 });
  // Fibonaccian arpeggiations: in the valleys and all through the bridge the
  // uke picks single strings. The Fibonacci word (s_n = s_{n-1} + s_{n-2})
  // decides pluck-or-rest on the eighth grid and the interval steps walk the
  // Fibonacci cycle (1,1,2,3,5), reflecting at the ladder edges — the arp is
  // self-similar at every scale but never loops. Grid-perfect placement.
  let fA = "0", fB = "01";
  while (fB.length < BARS * 8) [fA, fB] = [fB, fB + fA];
  const FIB_STEPS = [1, 1, 2, 3, 5];
  let arpIdx = 4, arpDir = 1, arpStep = 0, fCursor = 0;
  for (let b = 0; b < BARS; b++) {
    if (inTechno(b) || (!sparseAt(b) && sectionAt(b).name !== "bridge")) { fCursor += 8; continue; }
    // the arp picks from the section ladder's low half — it re-tunes too
    const arpLad = LADDERS[modeAt(b)].filter((m) => m <= 80);
    for (let e = 0; e < 8; e++, fCursor++) {
      if (fB[fCursor % fB.length] !== "1") continue;
      arpIdx += FIB_STEPS[arpStep++ % FIB_STEPS.length] * arpDir;
      while (arpIdx < 0 || arpIdx >= arpLad.length) {
        if (arpIdx >= arpLad.length) arpIdx = 2 * (arpLad.length - 1) - arpIdx;
        if (arpIdx < 0) arpIdx = -arpIdx;
        arpDir *= -1;
      }
      strum(bar(b) + e * 0.5 * BEAT, [MIDI_HZ(arpLad[arpIdx])],
        { db: -22.5, dur: 4, damp: 0.9994, bright: 0.6,
          pan: e % 2 ? 0.3 : -0.3 });
      squareShadow.push({ t: bar(b) + e * 0.5 * BEAT, midi: arpLad[arpIdx], vel: 0.3 });
    }
  }
}

// ── the square shadow: a purely synthetic square wave (odd-harmonic
//    additive, so no aliasing) dilly-dallying after every staircase figure —
//    trailing a sixteenth behind the rushes, glisses, pickups, and arps,
//    skipping notes, hesitating, popping the octave. The chip kid copying
//    the percussionist's homework. ─────────────────────────────────────────
if (STONE_CLUB && squareShadow.length) {
  const sq = (t, midi, vel, db, pan) => {
    const hz = MIDI_HZ(midi);
    const at = Math.floor(t * SR), n = Math.floor(0.13 * SR);
    const a = (clamp(pan, -0.9, 0.9) + 1) * 0.25 * Math.PI;
    const g = Math.pow(10, db / 20) * vel;
    const gl = Math.cos(a) * g, gr = Math.sin(a) * g;
    for (let i = 0; i < n && at + i < ns; i++) {
      const ph = (i / SR) * hz;
      let v = 0;
      for (let k = 1; k <= 9; k += 2) v += Math.sin(2 * Math.PI * ph * k) / k;
      const env = Math.exp(-i / (0.045 * SR)) * Math.min(1, i / 90);
      mix[2 * (at + i)] += v * env * gl;
      mix[2 * (at + i) + 1] += v * env * gr;
      if (synthBus) {
        synthBus[2 * (at + i)] += v * env * gl;
        synthBus[2 * (at + i) + 1] += v * env * gr;
      }
    }
  };
  let si = 0;
  for (const sh of squareShadow) {
    si++;
    if (grooveUnit(si, 901) < 0.28) continue;             // dally: skips notes
    const hesitate = grooveUnit(si, 903) < 0.14 ? BEAT / 8 : 0;
    const t = sh.t + BEAT / 4 + hesitate;                  // a sixteenth behind
    sq(t, sh.midi, sh.vel, -25.5, si % 2 ? 0.5 : -0.5);
    if (sh.midi + 12 <= 103 && grooveUnit(si, 907) < 0.16) // octave pop
      sq(t + BEAT / 8, sh.midi + 12, sh.vel * 0.7, -28.5, si % 2 ? -0.4 : 0.4);
  }
}

// ── disco bass: the FCUKERS move — a Bernard-Edwards/Chic disco bassline
//    (octave jumps as flourish, syncopated 16ths, "chucking" ghost notes,
//    root-fifth-octave, slides), plucky filtered saw + a sub sine for body,
//    sidechained to the four-on-the-floor and sent to a high-passed plate
//    reverb so the tail blooms without mudding the low end. Sits an octave
//    above the fuselage sub, so weight and groove don't fight. ────────────
if (STONE_CLUB && !process.env.NODISCO) {
  const bassDry = new Float32Array(ns);
  const hzFrom = (rootHz, semis) => rootHz * Math.pow(2, semis / 12);
  // one repeating 16th pattern, disco-syncopated: R=root, 7=fifth, 12=octave.
  // g = ghost (muted chuck), a = accent, s = slide from the previous note.
  const _ = null;
  const DISCO = [
    { d: 0, a: 1 }, { d: 0, g: 1 }, { d: 0 }, { d: 12, s: 1 },
    _,              { d: 0 },       { d: 7 }, { d: 12 },
    { d: 0, a: 1 }, { d: 0, g: 1 }, { d: 12 },{ d: 7 },
    { d: 0 },       { d: 12, s: 1 },{ d: 0, g: 1 }, { d: 10 },
  ];
  // a sparser skeleton for the bridge — dubby, let it ring into the reverb
  const DISCO_DUB = DISCO.map((st, i) => (i % 8 === 0 ? { d: st?.d ?? 0, a: 1 } : (i === 11 ? { d: 12 } : _)));
  // render one plucked note (saw + sub, resonant SVF, drive) into bassDry
  let prevHz = null;
  const pluckBass = (t, hz, { ghost, accent, slide }) => {
    const dur = ghost ? 0.055 : accent ? 0.19 : 0.14;
    const at = Math.floor(t * SR), n = Math.floor(dur * SR);
    const g = ghost ? 0.16 : accent ? 1.0 : 0.8;
    // Cascaded one-pole lowpass — unconditionally stable (a resonant SVF
    // here blew up to NaN on high notes). Disco bass is rounded, not acid,
    // so two poles + a bright pluck envelope give the right electric tone.
    let lp1 = 0, lp2 = 0;
    const fromHz = slide && prevHz ? prevHz : hz;
    for (let i = 0; i < n && at + i < ns; i++) {
      const age = i / SR;
      const gl = slide ? Math.min(1, age / 0.05) : 1;   // 50 ms portamento
      const f = fromHz + (hz - fromHz) * gl;
      const ph = ((t + age) * f) % 1;
      const saw = 2 * ph - 1;
      const sub = Math.sin(2 * Math.PI * ((t + age) * f * 0.5)); // sub-octave body
      const env = Math.exp(-age / (dur * 0.34)) * (age < 0.003 ? age / 0.003 : 1);
      const cutoff = 700 + (accent ? 1700 : 950) * Math.exp(-age / 0.045);
      const a = 1 - Math.exp(-2 * Math.PI * Math.min(6000, cutoff) / SR);
      const inp = 0.8 * saw + 0.3 * sub;
      lp1 += a * (inp - lp1);
      lp2 += a * (lp1 - lp2);
      const shaped = Math.tanh(lp2 * 1.8) / 1.8;         // gentle grit
      bassDry[at + i] += shaped * env * g;
    }
    if (!ghost) prevHz = hz;
  };
  for (const d of DROPS)
    for (let b = d.a; b < d.z; b++) {
      const sec = sectionAt(b).name;
      if (sec === "intro") continue;                   // the bass waits outside the door
      if (sec === "techno") continue;                  // the machine strip has no disco
      const rootHz = progHz(b, ((b - d.a) / 2) % 4) * 2; // an octave above the sub, mode-tuned
      // the pattern's b7 becomes a natural 7 in the bright modes
      const seventh = MODES[modeAt(b)][MODES[modeAt(b)].length - 1];
      const pat = sec === "bridge" ? DISCO_DUB : DISCO;
      for (let s = 0; s < 16; s++) {
        const st = pat[s];
        if (!st) continue;
        if (sec === "verse" && s % 2 === 1 && !st.a && !st.g && grooveUnit(b * 16 + s, 71) < 0.4) continue;
        const t = bar(b) + s * (BEAT / 4) + EAGER * 0.4 + grooveJitter(b * 16 + s, 211, 0.003);
        const hz = hzFrom(rootHz, st.d === 10 ? seventh : st.d);
        pluckBass(t, hz, { ghost: st.g, accent: st.a, slide: st.s });
        if (SCORE_JSON && !st.g)
          scoreExtra.disco.push({ t: +t.toFixed(4), hz: +hz.toFixed(2),
            midi: +(69 + 12 * Math.log2(hz / 440)).toFixed(2), accent: st.a ? 1 : 0 });
      }
    }
  // DC-block the pluck bus: the resonant SVF on short saw fragments leaves a
  // per-note DC bias that the reverb combs would amplify into a huge offset.
  // A ~7 Hz one-pole high-pass removes it while preserving the 40 Hz sub.
  {
    let xPrev = 0, yPrev = 0;
    for (let i = 0; i < ns; i++) {
      const x = Number.isFinite(bassDry[i]) ? bassDry[i] : 0; // never propagate a stray NaN
      const y = x - xPrev + 0.999 * yPrev;
      xPrev = x; yPrev = y;
      bassDry[i] = y;
    }
  }
  // ── Schroeder plate reverb on the bass, high-passed so lows stay tight ─
  const combTune = [0.0297, 0.0371, 0.0411, 0.0437], combFb = [0.79, 0.80, 0.78, 0.81];
  const combBuf = combTune.map((s) => new Float32Array(Math.max(1, Math.floor(s * SR))));
  const combPos = combTune.map(() => 0);
  const apTune = [0.0050, 0.0017], apFb = 0.7;
  const apBuf = apTune.map((s) => new Float32Array(Math.max(1, Math.floor(s * SR))));
  const apPos = apTune.map(() => 0);
  const bassWet = new Float32Array(ns);
  let hpPrevIn = 0, hpPrevOut = 0;
  const hpA = 0.985; // ~150 Hz one-pole high-pass on the wet return
  for (let i = 0; i < ns; i++) {
    const x = bassDry[i];
    let acc = 0;
    for (let c = 0; c < combBuf.length; c++) {
      const buf = combBuf[c];
      const y = buf[combPos[c]];
      buf[combPos[c]] = x + y * combFb[c];
      combPos[c] = (combPos[c] + 1) % buf.length;
      acc += y;
    }
    acc *= 0.25;
    for (let ap = 0; ap < apBuf.length; ap++) {
      const buf = apBuf[ap];
      const bufout = buf[apPos[ap]];
      const y = -apFb * acc + bufout;
      buf[apPos[ap]] = acc + apFb * y;
      apPos[ap] = (apPos[ap] + 1) % buf.length;
      acc = y;
    }
    const hp = hpA * (hpPrevOut + acc - hpPrevIn);
    hpPrevIn = acc; hpPrevOut = hp;
    bassWet[i] = hp;
  }
  // ── sum dry + wet into the mix with a kick-pumped sidechain ────────────
  const kickTimes = kicks.map((k) => k[0]).sort((a, b) => a - b);
  const dryGain = Math.pow(10, -9.5 / 20), wetGain = Math.pow(10, -17 / 20);
  let ki = 0;
  for (let i = 0; i < ns; i++) {
    const t = i / SR;
    while (ki + 1 < kickTimes.length && kickTimes[ki + 1] <= t) ki++;
    const since = t - kickTimes[ki];
    const duck = since >= 0 && since < 0.16 ? 1 - 0.5 * Math.exp(-since / 0.05) : 1;
    const v = (bassDry[i] * dryGain + bassWet[i] * wetGain) * duck;
    mix[2 * i] += v; mix[2 * i + 1] += v;
    if (synthBus) { synthBus[2 * i] += v; synthBus[2 * i + 1] += v; }
  }
}

// ── graphic-score export: everything the composer knows about the track,
//    written as analysis data for bin/score-video.mjs. Ground truth, not a
//    re-analysis — exact times, pitches, materials, sections, drum density.
if (SCORE_JSON) {
  const NOTE_SEMI = { C: 0, D: 2, E: 4, F: 5, G: 7, A: 9, B: 11 };
  const noteMidi = (n) => {
    const m = /^([A-G])(#?)(-?\d)$/.exec(n);
    if (!m) return null;
    return 12 * (+m[3] + 1) + NOTE_SEMI[m[1]] + (m[2] ? 1 : 0);
  };
  const hzMidi = (hz) => 69 + 12 * Math.log2(hz / 440);
  // Classify each bell into a score lane by its inspectable fields.
  const bellLane = (s) => {
    if (s.cliff && s.longTail) return "cliff";
    if (s.quat) return "quat";
    if (s.geometry === "bowl") return "bowl";
    if (s.geometry === "church") return "toll";
    if (s.dur >= 2.5 || s.longTail) return "ring";
    if (s.choke) return "ornament";
    return "melody";
  };
  const scoreBells = [...bells].sort((a, b) => a.t - b.t).map((s) => ({
    t: +s.t.toFixed(4), midi: noteMidi(s.note), note: s.note,
    vel: +(s.vel ?? 0.7).toFixed(3), dur: +(s.dur ?? 1).toFixed(3),
    pan: +(s.pan ?? 0).toFixed(3), lane: bellLane(s),
    material: s.material, geometry: s.geometry,
  })).filter((s) => s.midi != null);
  // Per-16th drum density — the mathematical grid made visible.
  const STEP = BEAT / 4;
  const drumSteps = Math.ceil(DUR / STEP);
  const drumDensity = new Array(drumSteps).fill(0);
  for (const nz of noises) drumDensity[Math.floor(nz[0] / STEP)]++;
  // The playback tempo warp (same curve the audio's final resample uses):
  // rate(t_out) = 0.92 + 0.08·smoothstep(t_out/DUR). Export the cumulative
  // input position vs output time so the score video can map each composed
  // event time to when it actually SOUNDS — otherwise the marks drift.
  const rateOf = (t) => 0.92 + 0.08 * (((p) => p * p * (3 - 2 * p))(Math.max(0, Math.min(1, t / DUR))));
  const tmDt = 0.02;
  const cumInput = [];
  let cum = 0;
  for (let t = 0; t <= DUR + tmDt; t += tmDt) { cumInput.push(+cum.toFixed(4)); cum += rateOf(t) * tmDt; }
  const payload = {
    track: "wattajetta stone club",
    transport: { bpm: BPM, beat: +BEAT.toFixed(5), bar: +BAR.toFixed(5),
      bars: BARS, durationSec: +DUR.toFixed(3), sr: SR },
    tempoMap: { dt: tmDt, cumInput },
    sections: CLUB_SECTIONS.map((s) => ({ ...s, tStart: +bar(s.a).toFixed(3), tEnd: +bar(s.z).toFixed(3) })),
    spinWindows: SPIN_WINDOWS.map((w) => ({ tStart: +w.at.toFixed(3), tEnd: +(w.at + w.dur).toFixed(3), turns: w.turns })),
    kicks: kicks.map((k) => +k[0].toFixed(4)),
    drumStep: +STEP.toFixed(5), drumDensity,
    bells: scoreBells,
    uke: scoreExtra.uke.map((u) => ({ ...u, midi: +hzMidi(u.hz).toFixed(2) })),
    flyby: scoreExtra.flyby.map((f) => ({ ...f, midi: +hzMidi(f.hz).toFixed(2) })),
    sub: scoreExtra.sub.map((s) => ({ ...s, midi: +hzMidi(s.hz).toFixed(2) })),
    disco: scoreExtra.disco,
  };
  const jsonPath = resolve(OUT, "wattajetta-stone-club-score.json");
  writeFileSync(jsonPath, JSON.stringify(payload) + "\n");
  console.log(`✓ ${jsonPath} · ${scoreBells.length} bells, ${payload.uke.length} uke, ${payload.flyby.length} flyby, ${kicks.length} kicks`);
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
if (INDUSTRIAL) {
  // One compact transformer gesture every two bars: audible turntablism,
  // disciplined like a machine interlock rather than a continuous spray.
  for (let b = 2; b < 80; b += 2) {
    const tBar = bar(b);
    const reverse = (b / 2) % 2 === 0;
    scratchVoiceGesture(tBar + 1.5 * BEAT, tBar,
                        reverse ? 0.58 : 0.32, reverse ? 0.34 : 0.52,
                        -16.5, reverse ? -0.24 : 0.24, 0.19);
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
if (!STONE_STUDY) {
  // The parent Wattajetta keeps its turntablist gestures. Canonical stone is a
  // continuous machine: no scrubbed buffers that can read as audio glitches.
  scratchAt(bar(11) + 2 * BEAT, bar(0), BEAT, [
    { dur: E, from: 0, to: 0.6 }, { dur: E, from: 0.6, to: 0.1 },
    { dur: E * 0.5, from: 0.1, to: 0.5 }, { dur: E * 0.5, from: 0.5, to: 0 },
    { dur: E, from: 0, to: 1 },
  ], -4);
  scratchAt(bar(15), bar(16), BEAT, SUPER, -3);
  scratchAt(bar(47), bar(48), BEAT, SUPER, -3);
  scratchAt(bar(29.5), bar(16), 2 * BEAT, [
    { dur: BEAT, from: 0, to: 0.5, gate: 0.5 }, { dur: BEAT, from: 0.5, to: 0.15, gate: 0.4 },
  ], -10);
  scratchAt(bar(84.5), bar(64), 2 * BEAT, [
    { dur: BEAT * 1.5, from: 0, to: 0.4, gate: 0.45 }, { dur: BEAT, from: 0.4, to: 0.1, gate: 0.35 },
  ], -11);
}

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
const VOCALS = STONE_STUDY ? [] : [
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

// ── entering the room: for the first eight bars the whole track — kick
//    included — is heard from behind the wall. A heavy lowpass holds dark
//    for two bars, then the door swings open across bars 2–8: the cutoff
//    sweeps exponentially from 280 Hz to transparent while the muffled
//    −3.5 dB lifts away. By the verse you're inside. ────────────────────
if (STONE_CLUB) {
  const wallEnd = bar(8);
  // The cap comes off by MEASURE: each bar it rests one notch higher, and
  // between the downbeats the hand toys with it — two playful pumps lifting
  // toward (past) the next notch before reseating. Pressure, tease, release.
  const CAP = [0.02, 0.07, 0.15, 0.26, 0.4, 0.55, 0.72, 0.9, 1];
  const capOpen = (t) => {
    const bpos = t / BAR;
    const bi = Math.min(8, Math.floor(bpos));
    const ph = bpos - Math.floor(bpos);
    const lo = CAP[bi], hi = CAP[Math.min(8, bi + 1)];
    const lift = ph * ph;                                   // leans late in the bar
    const tease = 0.5 + 0.5 * Math.sin(TAU * (2 * ph - 0.25)); // two pumps a bar
    return clamp(lo + (hi + 0.1 - lo) * lift * (0.55 + 0.45 * tease), 0, 1);
  };
  const busses = [mix, waterBus, stemKickBus, bellBus, trashBus, sampleBus, synthBus, gtrBus].filter(Boolean);
  const aHi = 1 - Math.exp(-2 * Math.PI * 4500 / SR); // wider crack: hat bodies fit through
  for (const bus of busses) {
    let l = 0, r = 0, hiL = 0, hiR = 0;
    for (let i = 0; i < ns; i++) {
      const t = i / SR;
      if (t >= wallEnd) break;
      const open = capOpen(t);
      const fc = 280 * Math.pow(18000 / 280, open);
      const a = 1 - Math.exp(-2 * Math.PI * fc / SR);
      const dryL = bus[2 * i], dryR = bus[2 * i + 1];
      l += a * (dryL - l);
      r += a * (dryR - r);
      // the crack under the door: a sliver of 6 kHz+ glints through the
      // dampening — dice clicks and bell pinpricks sparkle while the body
      // of the music stays behind the wall; the leak folds away as it opens
      hiL += aHi * (dryL - hiL);
      hiR += aHi * (dryR - hiR);
      const leak = 0.24 * (1 - open) + 0.06; // never fully sealed — the tick punches through even at t=0
      const lvl = 0.67 + 0.33 * open;
      bus[2 * i] = l * lvl + (dryL - hiL) * leak;
      bus[2 * i + 1] = r * lvl + (dryR - hiR) * leak;
    }
  }
}

// ── canonical stone has one uninterrupted tempo story: it starts slower
//    and rises gradually, with no local scratch grabs or slowdowns. ─────
{
  const rate = (t) => {
    if (STONE_STUDY) {
      const progress = Math.max(0, Math.min(1, t / DUR));
      return 0.92 + 0.08 * smooth(progress); // ~127 → 138 BPM
    }
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
  const warpBus = (bus) => {
    const warped = new Float32Array(bus.length);
    let pos = 0;
    for (let i = 0; i < ns; i++) {
      const j = Math.floor(pos);
      if (j >= ns - 1) break;
      const fr = pos - j;
      warped[2 * i] = bus[2 * j] * (1 - fr) + bus[2 * (j + 1)] * fr;
      warped[2 * i + 1] = bus[2 * j + 1] * (1 - fr) + bus[2 * (j + 1) + 1] * fr;
      pos += rate(i / SR);
    }
    bus.set(warped);
  };
  warpBus(mix);
  for (const bus of [waterBus, stemKickBus, bellBus, trashBus, sampleBus, synthBus, gtrBus].filter(Boolean)) warpBus(bus);
}

// Narrative macro-dynamics: a slow reveal, three mutation valleys, a late
// crest, and a long outro retreat. The envelope is shared by every stem so the
// premaster sum remains exact; the gentler club compressor preserves the arc.
if (STONE_CLUB) {
  const arc = [
    [0.00, -18], [0.035, -9], [0.10, -3], [0.22, 0],
    [0.26, -3.2], [0.41, 0.6], [0.49, -4.0], [0.63, 0.7],
    [0.71, -2.8], [0.86, 1.3], [0.90, 0], [0.94, -5.5], [1.00, -48],
  ];
  const gainAt = (p) => {
    for (let i = 1; i < arc.length; i++) if (p <= arc[i][0]) {
      const [a, adb] = arc[i - 1], [b, bdb] = arc[i];
      const u = smooth((p - a) / (b - a));
      return Math.pow(10, (adb + (bdb - adb) * u) / 20);
    }
    return Math.pow(10, arc.at(-1)[1] / 20);
  };
  const busses = [mix, waterBus, stemKickBus, bellBus, trashBus, sampleBus, synthBus, gtrBus].filter(Boolean);
  for (let i = 0; i < ns; i++) {
    const g = gainAt(i / Math.max(1, ns - 1));
    for (const bus of busses) { bus[2 * i] *= g; bus[2 * i + 1] *= g; }
  }
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
if (peak > 0.9) {
  const g = 0.9 / peak;
  for (const bus of [mix, waterBus, stemKickBus, bellBus, trashBus, sampleBus, synthBus, gtrBus].filter(Boolean))
    for (let i = 0; i < bus.length; i++) bus[i] *= g;
}

if (STEMS) {
  const finiteStats = (name, bus) => {
    let bad = 0, first = -1;
    for (let i = 0; i < bus.length; i++) if (!Number.isFinite(bus[i])) {
      bad++; if (first < 0) first = i;
    }
    console.log(`${name}: ${bad} non-finite${first >= 0 ? ` (first ${first})` : ""}`);
    return bad;
  };
  const bad = [
    finiteStats("premaster", mix), finiteStats("kick stem", stemKickBus),
    finiteStats("water stem", waterBus), finiteStats("bell stem", bellBus),
    finiteStats("trash stem", trashBus), finiteStats("sample stem", sampleBus),
    finiteStats("synth stem", synthBus), finiteStats("guitar stem", gtrBus),
  ].reduce((a, b) => a + b, 0);
  if (bad) { console.error("✗ non-finite stem samples"); process.exit(1); }
  let sumDiffSq = 0, mixLSq = 0, mixRSq = 0;
  for (let i = 0; i < mix.length; i += 2) {
    const stemL = waterBus[i] + stemKickBus[i] + bellBus[i] + trashBus[i] + sampleBus[i] + synthBus[i] + gtrBus[i];
    const stemR = waterBus[i + 1] + stemKickBus[i + 1] + bellBus[i + 1] + trashBus[i + 1] + sampleBus[i + 1] + synthBus[i + 1] + gtrBus[i + 1];
    const dl = mix[i] - stemL, dr = mix[i + 1] - stemR;
    sumDiffSq += dl * dl + dr * dr;
    mixLSq += mix[i] * mix[i]; mixRSq += mix[i + 1] * mix[i + 1];
  }
  const sumError = Math.sqrt(sumDiffSq / mix.length);
  const balanceDb = 10 * Math.log10(mixRSq / mixLSq);
  console.log(`stem sum error ${sumError.toExponential(3)} · premaster R-L ${balanceDb.toFixed(2)} dB`);
  if (sumError > 1e-5) { console.error("✗ stem sum does not match premaster"); process.exit(1); }
  const stemDir = resolve(OUT, "wattajetta-stone-club-stems");
  mkdirSync(stemDir, { recursive: true });
  const stemDefs = [
    ["01-kick.wav", stemKickBus],
    ["02-water-engine.wav", waterBus],
    ["03-stone-bells-uke.wav", bellBus],
    ["04-disco-bass-squares.wav", synthBus],
    ["05-guitar.wav", gtrBus],
    ["06-samples-fx.wav", sampleBus],
    ["07-empty-trash-fx.wav", trashBus],
  ];
  for (const [name, bus] of stemDefs) {
    const rawStem = resolve(OUT, `.stem-${name}.f32.raw`);
    const wavStem = resolve(stemDir, name);
    writeFileSync(rawStem, Buffer.from(bus.buffer, bus.byteOffset, bus.byteLength));
    const encoded = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
      "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawStem,
      "-c:a", "pcm_s24le", "-metadata", `title=${name.replace(/\.wav$/, "")}`,
      wavStem], { stdio: "inherit" });
    try { unlinkSync(rawStem); } catch {}
    if (encoded.status !== 0) { console.error(`✗ stem encode failed: ${name}`); process.exit(1); }
  }
  writeFileSync(resolve(stemDir, "README.txt"), [
    "wattajetta stone club audition — 138 BPM — 48 kHz / 24-bit stereo",
    "All stems begin at 00:00 and sum to the premaster arrangement.",
    "Apply the renderer's master chain to the summed stems for the reference sound.",
    "01 kick | 02 water engine (sub, gallop, drums, bloops, choir, flybys, wub) | 03 stone bells + ukulele",
    "04 disco bass + square shadows | 05 electric guitar (power chords, threads) | 06 samples (owls, phones, modem, punches, dice, AC stamp) | 07 Empty Trash FX",
    "Kick/sub stay centered. Non-kick stems carry a side-only fixed-listener return that cancels in mono.",
    "Empty Trash source: local macOS SystemSounds/finder/empty trash.aif",
    "",
  ].join("\n"));
  console.log(`✓ ${stemDir} (${stemDefs.length} true-summing 24-bit stems)`);
}

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

const mixedPath = resolve(OUT, `${outputStem}.mixed.f32.raw`);
writeFileSync(mixedPath, Buffer.from(outputMix.buffer, outputMix.byteOffset, outputMix.length * 4));

// ── master: firmer than the pure cut — the crunch ramp wants a spine,
//    but the opening still breathes. The club cut instead masters in the
//    stone-club-audition-2 lineage: gentler compression, real dynamics
//    (≈ -13 LUFS, LRA around 5) instead of the canonical's pressed loaf ──
const CLAMP_720 = 720; // seconds, exact — the name is the spec
const MASTER = STONE_CLUB ? [
  "highpass=f=24",
  "acompressor=threshold=-16dB:ratio=1.7:attack=12:release=180:makeup=1.2:knee=8",
  "equalizer=f=50:t=q:w=1.2:g=2.5",
  "alimiter=limit=0.94:attack=4:release=80",
  "areverse", "silenceremove=start_periods=1:start_threshold=-70dB", "areverse",
  // land on the boundary the title promises, fading rather than chopping
  ...(STONE_720 ? [`afade=t=out:st=${CLAMP_720 - 6}:d=6`,
                   `atrim=end=${CLAMP_720}`, "asetpts=PTS-STARTPTS"] : []),
] : [
  "highpass=f=24",
  "acompressor=threshold=-19dB:ratio=2.8:attack=10:release=150:makeup=2.2:knee=6",
  "equalizer=f=50:t=q:w=1.2:g=2.5", // the boom under the kick — no treble boost; laptop speakers read it as tang
  ...(WORLD ? ["equalizer=f=285:t=q:w=0.85:g=-1.6"] : []), // normalize low-mid buildup for headphones
  ...(WORLD ? ["highshelf=f=5500:g=-2dB"] : []), // keep the water world, lose excess upper-air haze
  ...(INDUSTRIAL ? ["equalizer=f=240:t=q:w=1.1:g=-1.2", "highshelf=f=6200:g=-2.5dB"] : []),
  "alimiter=limit=0.96:attack=4:release=70",
  ...(WORLD ? ["volume=-1.5dB"] : []), // leave the spatial audition at about -1 dBTP
  // The accelerando leaves dead source at the legacy tail. The fixed 2:30
  // vinyl arrangement already owns its ending, so preserve that exact clock.
  ...(!NEXT ? ["areverse", "silenceremove=start_periods=1:start_threshold=-70dB", "areverse"] : []),
];
const mp3 = resolve(OUT, STONE_720
  ? "watjetsto720.mp3"
  : STONE_CLUB
  ? "wattajetta-stone-club.mp3"
  : STONE_STUDY
  ? "wattajetta-stone-canonical.mp3"
  : TINY_BELLS ? "wattajetta-tinybells.mp3" : "wattajetta.mp3");
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", mixedPath,
  "-af", MASTER.join(","), "-c:a", "libmp3lame", "-q:a", "2",
  "-metadata", `title=${STONE_720 ? "watjetsto720" : STONE_CLUB ? "wattajetta stone club" : STONE_STUDY ? "wattajetta stone canonical" : TINY_BELLS ? "wattajetta tiny bells" : "wattajetta"}`, "-metadata", "album=pixsies",
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
// --keep-mixed leaves the pre-master mix on disk for external master passes
// (release WAVs want a different chain than the canonical mp3).
const scratch = process.argv.includes("--keep-mixed") ? [rawPath, kickPath] : [rawPath, kickPath, mixedPath];
for (const p of scratch) { try { unlinkSync(p); } catch {} }
console.log(`✓ ${mp3} (${INDUSTRIAL ? "steel/stone press line · disciplined scratches · negative space" : "glass↔staccato transmogrification · rhythmic scratch voice · water world"})`);
