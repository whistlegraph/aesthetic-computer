#!/usr/bin/env node
// render-wattajetta-cutie.mjs — wattajetta cutie club. The wattajetta
// composition (the harmony plot and the one eight-bar tune from
// render-composition-study.mjs) let out to six minutes as a slow, bloopy,
// airy dance track. Still all water: sine holding a shape, spray losing
// it, bells frozen mid-ring. Everything is synthesized here except the
// two jeffrey-pvc drops already cached in assets/.
//
//   node pop/wattajetta/bin/render-wattajetta-cutie.mjs          → out/wattajetta-cutie.mp3
//   node pop/wattajetta/bin/render-wattajetta-cutie.mjs --form   print the form and exit
//   node pop/wattajetta/bin/render-wattajetta-cutie.mjs --stems  also write per-bus premaster wavs
//   node pop/wattajetta/bin/render-wattajetta-cutie.mjs --material none   skip the plastic tape (pop/lib/material.mjs)
//   node pop/wattajetta/bin/render-wattajetta-cutie.mjs --bake out/wattajetta-cutie.score.txt
//        write every note for the C engine (c/wattajetta-cutie.c) and the
//        premaster to hold it against; stops before the master
//
// The feel lives in CFG and the form in ROWS: twenty eight-bar rows,
// one line each. Retune there; the voices below only read them.

import { mkdirSync, writeFileSync, readFileSync, existsSync, statSync, openSync, writeSync, closeSync } from "node:fs";
import { createHash } from "node:crypto";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { renderBell, bellModes } from "../../lib/bell.mjs";
import * as progress from "../../lib/render-progress.mjs";
import { applyMaterial } from "../../lib/material.mjs";
import { masterGraph, COVER } from "./cutie-master.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../out");
const ASSETS = resolve(HERE, "../assets");
mkdirSync(OUT, { recursive: true });
const BELL_CACHE = resolve(OUT, ".cutie-bell-cache");
mkdirSync(BELL_CACHE, { recursive: true });
const FFMPEG = existsSync("/opt/homebrew/bin/ffmpeg") ? "/opt/homebrew/bin/ffmpeg" : "ffmpeg";
const STEM = "wattajetta-cutie";
const flag = (name, fallback) => { const i = process.argv.indexOf(name); return i > 0 ? process.argv[i + 1] : fallback; };
const MATERIAL = flag("--material", "tape");            // --material none for the bare mix
const MATERIAL_AMOUNT = Number(flag("--amount", 1));
// The jeffrey phoneme library (jeffrey-pvc, eleven_v4 syllables) lives in the vault.
const PHONEMES = resolve(process.env.AC_VAULT ?? resolve(HERE, "../../../../aesthetic-computer-vault"), "voice/jeffrey-phonemes");
const PYTHON = resolve(HERE, "../../.venv/bin/python");

// ── bake ────────────────────────────────────────────────────────────────
// The score the C engine reads: one line per note, in the order played here
// (the spray voices share one noise stream, so the order is part of the sound).
const BAKE = flag("--bake", null), baked = [], bakeIds = new Map();
const BAKE_DIR = BAKE && resolve(dirname(resolve(BAKE)), ".cutie-c-bake");
if (BAKE) mkdirSync(BAKE_DIR, { recursive: true });
const ev = BAKE ? (...a) => baked.push(a.join(" ")) : () => {};
// A sample's id in the score. With `data`, the decoded floats are written out
// once (named by the key's hash); without, `key` is a raw f32 file already on disk.
function bakeSmp(key, data) {
  if (!bakeIds.has(key)) {
    let path = key;
    if (data) {
      path = resolve(BAKE_DIR, `${createHash("sha1").update(key).digest("hex").slice(0, 12)}.f32`);
      if (!existsSync(path) || statSync(path).size !== data.byteLength) writeFileSync(path, Buffer.from(data.buffer, data.byteOffset, data.byteLength));
    }
    bakeIds.set(key, bakeIds.size);
    baked.push(`smp ${bakeIds.get(key)} ${path}`);
  }
  return bakeIds.get(key);
}

// ── feel ────────────────────────────────────────────────────────────────
const CFG = {
  bpm: 112,
  swing: 0.16,            // fraction of a sixteenth the off-sixteenths arrive late
  tail: 8,                // seconds of ring after the last bar
  // machined: a fast drop from 230 Hz for the knock, a short boxy body at 58 (it was a soft 118→46 over 45 ms, 0.2 s long)
  kick: { start: 230, end: 58, attack: 0.002, decay: 0.15, sweep: 0.03, gain: 0.6 },
  duck: { music: 0.5, bass: 0.72, bells: 0.18, wet: 0.4, release: 0.2, snare: 0.3, snareRelease: 0.16 },
  reverb: { feedback: 0.905, damp: 0.32, predelay: 0.028, level: 0.8 },
  delay: { beats: 0.75, feedback: 0.42, damp: 0.45, level: 0.8 },
  crunch: 0.85,           // tanh drive on kick + bass + drums only; bells and voices stay pure
  gain: {
    kick: 1.0, bass: 0.48, acid: 0.85, drums: 0.6, music: 1.25, bells: 1.7,
    pad: 0.13, air: 1.0, sung: 1.3, back: 0.7, choir: 0.2, arp: 0.3, bloop: 0.5, lead: 0.4, harm: 0.16,
    clap: 0.5, hat: 0.26, shaker: 0.22, rim: 0.3, vox: 0.9, fly: 0.2, riser: 0.3,
  },
};

const SR = 48_000;
const TAU = Math.PI * 2;
const BEAT = 60 / CFG.bpm;
const BAR = 4 * BEAT;
const SIX = BEAT / 4;

// ── harmony: the composition study's chords ─────────────────────────────
const CH = {
  "Em(add9)": [40, [52, 59, 64, 66, 67]], "Cmaj7/E": [40, [48, 55, 59, 64]],
  "Am9/E": [40, [45, 52, 55, 59, 64]], "B7sus4": [35, [47, 52, 54, 57]],
  "B7": [35, [47, 51, 54, 57]], "Em9": [40, [52, 55, 59, 66]],
  "D/F#": [42, [50, 54, 57, 62]], "Cmaj7": [36, [48, 52, 55, 59]],
  "Am9": [33, [45, 48, 52, 55, 59]], "B7(b9)": [35, [47, 51, 54, 57, 60]],
  "Em7": [40, [52, 55, 59, 62]], "A(add9)/E": [40, [45, 52, 57, 59, 64]],
  "Fmaj7/E": [40, [53, 57, 60, 64]], "Am/E": [40, [45, 52, 57, 60]],
  "E7sus4": [40, [52, 57, 59, 62]], "D/A": [33, [50, 54, 57, 62]],
  "A(add9)": [33, [45, 52, 57, 59, 64]], "Emaj7(#11)": [40, [52, 56, 59, 63, 66]],
  "F#7sus4": [42, [54, 59, 61, 64]], "D#m7/A#": [46, [51, 54, 58, 61]],
  "Amaj7/E": [40, [45, 52, 56, 61]], "E5": [40, [52, 59, 64]],
  "E(add9)": [40, [52, 56, 59, 66]], "B/D#": [39, [47, 51, 54, 59]],
  "C#m7": [37, [49, 52, 56, 59]], "Amaj9": [33, [45, 52, 56, 59, 64]],
  "F#m7": [30, [42, 49, 52, 57]], "Esus2": [40, [52, 54, 59, 64]],
};

// Each block is eight bars. The study's ten- and five-bar sections are
// re-phrased onto the eight so a dancer can count them.
const BLOCKS = {
  INTRO:   [["Em(add9)", 2], ["Cmaj7/E", 2], ["Am9/E", 2], ["B7sus4", 1], ["B7", 1]],
  VERSE1:  [["Em9", 2], ["D/F#", 2], ["Cmaj7", 1], ["Am9", 1], ["B7sus4", 1], ["B7(b9)", 1]],
  CHORUS1: [["Em7", 2], ["A(add9)/E", 2], ["Cmaj7", 2], ["B7sus4", 1], ["B7", 1]],
  VERSE2:  [["Em(add9)", 2], ["Fmaj7/E", 2], ["Cmaj7/E", 1], ["Am/E", 1], ["B7(b9)", 1], ["B7", 1]],
  CHORUS2: [["E7sus4", 2], ["D/A", 2], ["A(add9)", 2], ["B7sus4", 1], ["B7", 1]],
  BRIDGE:  [["Emaj7(#11)", 2], ["F#7sus4", 2], ["D#m7/A#", 2], ["Amaj7/E", 2]],
  PEDAL:   [["E5", 8]],
  FINAL_A: [["E(add9)", 2], ["B/D#", 2], ["C#m7", 2], ["Amaj9", 2]],
  FINAL_B: [["F#m7", 2], ["B7sus4", 1], ["B7", 1], ["E(add9)", 2], ["Amaj9", 1], ["B7", 1]],
  OUTRO:   [["Em(add9)", 2], ["Cmaj7/E", 2], ["Esus2", 2], ["E5", 2]],
};

// ── melody: one eight-bar tune, [beat, semitones above E4, beats, velocity] ──
// The hook, as semitones over E in the natural minor (the mode maps below
// recolour it). Refined 2026-11-20 into one line with somewhere to go: it
// used to circle the fifth and close on the tonic in its second bar, then
// leap to the high octave at the end. Now each two-bar phrase moves by step
// and hands on to the next — a rise that stays open on the second, a higher
// rise that rests on the fifth, the peak and its fall, and a last line of ten
// notes that dips, climbs by step, leans on the note above the tonic ("NOTH")
// and settles onto the upper tonic ("ing"), held across the barline. (It first walked down to the low tonic;
// sung, that ending sat in the bass's own register and read as the bass line.
// Going up keeps the voice clear of it.) The rhythm keeps the long-short rock.
const THEME_STRAIGHT = [
  [0, 0, 1.5, 1], [1.5, 2, 0.5, 0.76], [2, 3, 1.25, 0.9], [3.25, 5, 0.75, 0.8],
  [4, 7, 1.5, 0.92], [5.5, 5, 0.5, 0.74], [6, 3, 1, 0.82], [7, 2, 1, 0.84],
  [8, 5, 1, 0.8], [9, 7, 1, 0.84], [10, 9, 1.5, 0.94], [11.5, 10, 0.5, 0.78],
  [12, 12, 1, 0.96], [13, 10, 1.5, 0.86], [14.5, 9, 0.5, 0.76], [15, 7, 1, 0.86],
  [16, 12, 1.5, 1], [17.5, 14, 0.5, 0.82], [18, 15, 1.25, 1], [19.25, 14, 0.75, 0.84],
  [20, 12, 1.5, 0.92], [21.5, 10, 0.5, 0.78], [22, 8, 1, 0.84], [23, 7, 1, 0.82],
  [24, 5, 1, 0.82], [25, 7, 0.5, 0.78], [25.5, 12, 1, 0.94], [26.5, 10, 0.5, 0.8], [27, 7, 1, 0.86],
  [28, 5, 0.5, 0.82], [28.5, 7, 0.5, 0.84], [29, 9, 1, 0.9], [30, 14, 1, 1], [31, 12, 3.5, 0.96],
];
// --waltz: the same notes with a waltz's push and hesitate. Each two-bar
// phrase is four small bars of three laid over the four-four (three pulses
// to every two beats), and inside each the second pulse comes early and the
// third late: 0, 0.60, 1.40 where even would be 0, 0.67, 1.33. The stressed
// syllables take the downbeats (WHAT - a | JET-ter | CU-tie you | ARE), the
// kick stays straight underneath, and the last note waits a whole beat.
const THEME_WALTZ = [
  [0, 0, 1.4, 1], [1.4, 2, 0.6, 0.76], [2, 3, 0.6, 0.92], [2.6, 5, 1.4, 0.8],
  [4, 7, 0.6, 0.94], [4.6, 5, 0.8, 0.78], [5.4, 3, 0.6, 0.82], [6, 2, 2, 0.9],
  [8.6, 5, 0.8, 0.8], [9.4, 7, 0.6, 0.84], [10, 9, 0.6, 0.96], [10.6, 10, 1.4, 0.8],
  [12, 12, 0.6, 0.96], [12.6, 10, 0.8, 0.86], [13.4, 9, 0.6, 0.78], [14, 7, 2, 0.88],
  [16, 12, 1.4, 1], [17.4, 14, 0.6, 0.82], [18, 15, 0.6, 1], [18.6, 14, 1.4, 0.84],
  [20, 12, 0.6, 0.94], [20.6, 10, 0.8, 0.8], [21.4, 8, 0.6, 0.84], [22, 7, 2, 0.86],
  [24, 5, 1.4, 0.84], [25.4, 7, 0.6, 0.78], [26, 12, 0.6, 0.96], [26.6, 10, 0.8, 0.82], [27.4, 7, 0.6, 0.84],
  [28, 5, 0.3, 0.8], [28.3, 7, 0.3, 0.82], [28.6, 9, 1.4, 0.9], [30, 14, 1.4, 1], [31.4, 12, 3.5, 0.96],
];
const WALTZ = process.argv.includes("--waltz");
const ROCK = process.argv.includes("--rock");   // a rock kit pattern in place of four-on-the-floor
if (ROCK) { CFG.gain.drums = 0.8; CFG.kick.gain = 0.68; CFG.gain.hat = 0.33; CFG.gain.clap = 0.6; }   // and the kit up front
const THEME = WALTZ ? THEME_WALTZ : THEME_STRAIGHT;
// a mode map works on the pitch class and keeps the octave
const pcmap = (map) => (n) => { const pc = ((n % 12) + 12) % 12; return pc in map ? n - pc + map[pc] : n; };
const themed = (fn) => THEME.map(([b, n, d, v]) => [b, fn(n, b), d, v]);
const TAIL_SUS = [[24, 5, 1.5, 0.6], [26, 2, 1, 0.55], [28, -1, 2, 0.6]];
const MEL = {
  introFrag: [[2, 0, 2, 0.5], [4, 3, 1, 0.45], [6, 7, 1, 0.55], [7, 5, 1, 0.42],
              [18, 0, 2, 0.5], [20, 3, 1, 0.45], [22, 7, 1, 0.55], [23, 10, 1.5, 0.5]],
  verse1:  [[0, 0, 1.5, 0.68], [2, 3, 0.5, 0.58], [3, 7, 1, 0.7], [8, 2, 1, 0.58],
            [10, 0, 2, 0.7], [16, 3, 1, 0.62], [18, 7, 1, 0.7], ...TAIL_SUS],
  verse1b: [[0, 0, 1.5, 0.7], [2, 3, 0.5, 0.6], [3, 7, 1, 0.72], [8, 2, 1, 0.6],
            [10, 0, 2, 0.7], [12, 7, 1, 0.6], [13, 10, 1, 0.66], [14, 7, 2, 0.7],
            [16, 3, 1, 0.62], [18, 7, 1, 0.7], [20, 12, 2, 0.66], ...TAIL_SUS],
  verse2:  [[0, 0, 1.5, 0.65], [2, 1, 0.5, 0.58], [3, 7, 1, 0.68], [8, 1, 1, 0.6],
            [10, 0, 2, 0.72], [16, 3, 1, 0.65], [18, 7, 1, 0.72],
            [24, 5, 1.5, 0.6], [26, 8, 1, 0.58], [28, -1, 2, 0.6]],
  verse2b: [[0, 0, 1.5, 0.68], [2, 1, 0.5, 0.6], [3, 7, 1, 0.7], [8, 1, 1, 0.6],
            [10, 0, 2, 0.72], [12, 5, 1, 0.6], [13, 8, 1, 0.64], [14, 5, 2, 0.68],
            [16, 3, 1, 0.65], [18, 7, 1, 0.72], [20, 10, 2, 0.66],
            [24, 5, 1.5, 0.6], [26, 8, 1, 0.58], [28, -1, 2, 0.6]],
  // Dorian: the raised sixth is the colour over the A chord (bars 3-4); the line itself takes C over Cmaj7.
  hookDor: themed((n) => n),
  hookMix: themed(pcmap({ 3: 4 })),
  // Ionian: third, sixth and seventh rise. The fourth stays natural — the
  // study's raised fourth sat a semitone off the Amaj9 underneath it.
  hookIon: themed(pcmap({ 3: 4, 8: 9, 10: 11 })),
  // The four-bar cadential answer, rests under the dominant, then a tag that leans on the door.
  ionAnswer: [[0, 9, 1.5, 0.86], [1.5, 14, 0.5, 0.78], [2, 12, 1, 0.9], [3, 9, 1, 0.8],
              [4, 5, 1, 0.76], [5, 9, 1, 0.84], [6, 11, 1, 0.9], [7, 12, 2.5, 1],
              [16, 7, 2, 0.8], [18, 4, 1, 0.72], [19, 2, 1, 0.7], [20, 0, 3, 0.82],
              [24, 9, 1.5, 0.84], [25.5, 7, 0.5, 0.74], [26, 5, 1, 0.78], [27, 4, 1, 0.76],
              [28, 2, 1, 0.76], [29, 7, 1, 0.84], [30, 11, 2, 0.95]],
  bridge:  [[0, 12, 3, 0.68], [5, 11, 2.5, 0.6], [8, 9, 3, 0.62], [12, 7, 3.5, 0.6],
            [16, 6, 3, 0.62], [20, 9, 3.5, 0.6], [24, 4, 3, 0.6], [28, 0, 4, 0.66]],
  breath:  [[0, 0, 3, 0.6], [4, 3, 1, 0.5], [6, 7, 2, 0.6], [10, 10, 2, 0.55],
            [16, 7, 2, 0.58], [20, 5, 1, 0.5], [22, 2, 2, 0.55], [26, 0, 4, 0.6]],
  outro:   [[0, 7, 2, 0.68], [4, 3, 1, 0.58], [6, 2, 1, 0.54], [8, 0, 3, 0.68],
            [16, 2, 2, 0.5], [20, 0, 4, 0.6]],
  outro2:  [[0, 0, 4, 0.55], [16, 7, 2, 0.5], [24, 0, 6, 0.55]],
};
const SCALES = {
  pent: [0, 3, 5, 7, 10], aeo: [0, 2, 3, 5, 7, 8, 10], dor: [0, 2, 3, 5, 7, 9, 10],
  phr: [0, 1, 3, 5, 7, 8, 10], mix: [0, 2, 4, 5, 7, 9, 10], lyd: [0, 2, 4, 6, 7, 9, 11],
  ion: [0, 2, 4, 5, 7, 9, 11],
};

// ── form: twenty-one eight-bar rows ─────────────────────────────────────
// kick: four | half | heart | 0    bass: bounce | skel | long | pulse | 0
// arp: [level, notes per beat]     bloop: [level, hits per bar]
// voice: synth | bells | both      build: riser + roll into the next row
// acid: [level, cutoff at the row's start, cutoff at its end] (0..1)
// strings: the chord as long acid notes, phased with the line
// wub: beat-locked wobble bass under the acid
// choir: jeffrey's "oo"/"ah" on the chord     sing: jeffrey sings the line on the tune
// back: the second line chanted under it, at this gain
const FORM = [
  { name: "intro",     block: "INTRO",   mode: "pent", kick: "four", hat: 0.5, shk: 0.4, bass: "long", pad: 0.6, air: 0.8, bloop: [1, 5], mel: "introFrag", voice: "bells" },
  { name: "verse 1",   block: "VERSE1",  mode: "aeo",  kick: "four", clap: 0.6, hat: 0.7, shk: 0.5, bass: "skel", pad: 0.7, air: 0.6, arp: [0.6, 2], bloop: [0.8, 5], mel: "verse1", voice: "bells" },
  { name: "verse 1'",  block: "VERSE1",  mode: "aeo",  kick: "four", clap: 0.7, hat: 0.8, shk: 0.6, bass: "bounce", pad: 0.7, air: 0.6, arp: [0.8, 2], bloop: [0.8, 6], mel: "verse1b", voice: "both", build: 0.5 },
  { name: "chorus 1",  block: "CHORUS1", mode: "dor",  kick: "four", clap: 0.9, hat: 0.9, shk: 0.7, bass: "long", pad: 0.9, air: 0.8, arp: [0.9, 4], bloop: [0.6, 6], mel: "hookDor", voice: "synth", acid: [0.6, 0.3, 0.38], choir: 0.7, strings: 0.5, back: 0.9 },
  { name: "chorus 1'", block: "CHORUS1", mode: "dor",  kick: "four", clap: 0.9, hat: 0.9, shk: 0.7, bass: "long", pad: 0.9, air: 0.8, arp: [0.9, 4], bloop: [0.6, 6], mel: "hookDor", voice: "both", harm: true, acid: [0.7, 0.34, 0.44], choir: 0.9, sing: true, strings: 0.6, back: 0.55 },
  { name: "breath",    block: "INTRO",   mode: "pent", pad: 1, air: 1, bloop: [0.9, 3], mel: "breath", voice: "bells", build: 1, choir: 1 },
  { name: "verse 2",   block: "VERSE2",  mode: "phr",  kick: "four", clap: 0.6, hat: 0.7, shk: 0.5, rim: 0.5, bass: "skel", pad: 0.7, air: 0.6, arp: [0.7, 2], bloop: [0.8, 5], mel: "verse2", voice: "bells" },
  { name: "verse 2'",  block: "VERSE2",  mode: "phr",  kick: "four", clap: 0.7, hat: 0.8, shk: 0.6, rim: 0.5, bass: "long", pad: 0.7, air: 0.6, arp: [0.8, 2], bloop: [0.8, 6], mel: "verse2b", voice: "both", build: 0.5, acid: [0.5, 0.2, 0.3], strings: 0.4 },
  { name: "chorus 2",  block: "CHORUS2", mode: "mix",  kick: "four", clap: 0.9, hat: 0.9, shk: 0.7, bass: "long", pad: 0.9, air: 0.8, arp: [0.9, 4], bloop: [0.6, 6], mel: "hookMix", voice: "synth", acid: [0.7, 0.36, 0.46], choir: 0.7, strings: 0.6, back: 0.8 },
  { name: "chorus 2'", block: "CHORUS2", mode: "mix",  kick: "four", clap: 0.9, hat: 0.9, shk: 0.7, bass: "long", pad: 0.9, air: 0.8, arp: [0.9, 4], bloop: [0.6, 6], mel: "hookMix", voice: "both", harm: true, acid: [0.75, 0.4, 0.52], choir: 0.9, sing: true, strings: 0.7, wub: 0.5, back: 0.55 },
  { name: "bridge",    block: "BRIDGE",  mode: "lyd",  bass: "long", pad: 1, air: 1, arp: [0.5, 2], bloop: [0.6, 3], mel: "bridge", voice: "synth", under: true, choir: 1 },
  { name: "bridge'",   block: "BRIDGE",  mode: "lyd",  kick: "heart", hat: 0.4, bass: "long", pad: 1, air: 0.9, arp: [0.8, 4], bloop: [0.7, 5], mel: "bridge", voice: "both", build: 1, drop: true, acid: [0.55, 0.12, 0.4], choir: 0.8, strings: 0.7, wub: 0.5 },
  // the solo: four bars whispered with the band gone, then four bars loud, the voice alone on the tune
  { name: "solo",      block: "FINAL_A", mode: "ion",  kick: "four", kickFrom: 4, pad: 0.45, air: 0.7, bloop: [0.5, 3], solo: true, build: 1 },
  { name: "machine",   block: "PEDAL",   mode: "pent", kick: "four", tight: true, rim: 0.9, hat: 0.8, shk: 0.6, bass: "long", pad: 0.3, air: 0.4, bloop: [0.7, 9], chops: true, build: 1, drop: true, acid: [1, 0.25, 0.85], strings: 0.5, wub: 1.0, back: 1 },
  { name: "final a",   block: "FINAL_A", mode: "ion",  kick: "four", clap: 1, hat: 1, shk: 0.8, bass: "long", pad: 1, air: 0.9, arp: [1, 4], bloop: [0.6, 6], mel: "hookIon", voice: "both", acid: [0.8, 0.5, 0.6], choir: 0.8, sing: true, strings: 0.8, back: 0.5 },
  { name: "final a'",  block: "FINAL_B", mode: "ion",  kick: "four", clap: 1, hat: 1, shk: 0.8, bass: "long", pad: 1, air: 0.9, arp: [1, 4], bloop: [0.6, 6], mel: "ionAnswer", voice: "both", acid: [0.8, 0.55, 0.62], choir: 0.8, sing: true, strings: 0.8, back: 0.5 },
  { name: "final b",   block: "FINAL_A", mode: "ion",  kick: "four", clap: 1, hat: 1, shk: 0.9, bass: "long", pad: 1, air: 1, arp: [1, 4], bloop: [0.7, 7], mel: "hookIon", voice: "both", harm: true, low: true, acid: [0.85, 0.58, 0.7], choir: 1, sing: true, strings: 0.9, wub: 0.75, back: 0.6 },
  { name: "final b'",  block: "FINAL_B", mode: "ion",  kick: "four", clap: 1, hat: 1, shk: 0.9, bass: "long", pad: 1, air: 1, arp: [1, 4], bloop: [0.7, 7], mel: "ionAnswer", voice: "both", harm: true, low: true, acid: [0.85, 0.62, 0.72], choir: 1, sing: true, strings: 0.9, wub: 0.75, back: 0.6 },
  { name: "outro",     block: "OUTRO",   mode: "pent", kick: "four", hat: 0.6, shk: 0.4, bass: "long", pad: 0.8, air: 0.9, arp: [0.5, 2], bloop: [0.9, 5], mel: "outro", voice: "bells", choir: 0.7 },
  { name: "outro'",    block: "OUTRO",   mode: "pent", kick: "half", pad: 0.6, air: 1, bloop: [0.8, 4], mel: "outro2", voice: "bells", choir: 0.5 },
  { name: "mist",      block: "PEDAL",   mode: "pent", pad: 0.35, air: 1, bloop: [0.6, 3], last: true },
];

// The single is twelve of those rows, 3:34: one verse and one chorus pair each
// side, the whispered-then-loud solo into the machine, one finale pair. --long
// renders all twenty (5:50).
const SINGLE = ["intro", "verse 1'", "chorus 1", "chorus 1'", "verse 2'", "chorus 2'", "solo", "machine", "final a", "final b'", "outro'", "mist"];
// The words (@jeffrey 2026-11-20), in lyrics/cutie.json: the lead line on the
// tune, the second line chanted slowly under it. Each is sung as a whole
// spoken take stretched onto the notes (bin/sing-lines.py), so the words can
// be heard; stitched syllables could not carry them.
const LINES = JSON.parse(readFileSync(resolve(HERE, "../lyrics/cutie.json"), "utf8"));
const NSYL = Object.fromEntries(Object.entries(LINES).map(([k, v]) => [k, v.split(/[ -]/).length]));
// The inflection each line is sung in (bin/cutie-takes.mjs CADENCES). @jeffrey
// chose mischievous, and softly on some lines, by ear from
// out/cutie-inflections-lead.mp3, 2026-11-20. A
// line left out here is cast by whisper instead and takes turns.
// The title line comes twice a row (phrases one and three): the second time is the soft one
// ([warmly]: the [softly] take lost its words on the tune, whisper heard "was a gender cutie who you are").
const PIN = { lead: ["cheeky", "warm"], back: ["cheeky"], lip: ["cheeky"], nothing: ["cheeky"] };
// The lip liner goes through the rainbow, a colour each time it comes round,
// and ends on black (@jeffrey 2026-11-20). "lip" itself is the pink one.
const COLORS = ["red", "orange", "yellow", "green", "blue", "purple", "pink", "black"];
for (const c of COLORS) if (c !== "pink") PIN[`lip-${c}`] = PIN.lip;
let colorTurn = 0;
const nextColor = () => { const c = COLORS[colorTurn++ % COLORS.length]; return c === "pink" ? "lip" : `lip-${c}`; };
// yellowest and orangest have a syllable more than the phrase has notes: the
// stressed note gives up its last half beat (or the next note is halved) for it
function roomFor(notes, syllables) {
  if (syllables !== notes.length + 1) return notes;
  const n = notes.map((x) => [...x]), [a, b] = [n[2], n[3]];
  if (a[2] >= b[2] + 0.5) { a[2] -= 0.5; n.splice(3, 0, [a[0] + a[2], a[1], 0.5, a[3] * 0.9]); }
  else { b[2] /= 2; n.splice(4, 0, [b[0] + b[2], b[1], b[2], b[3]]); }
  return n;
}
const phrases = [];                    // { line, turn, t, starts, midis, end_hold, gain, pan, wet, echo }
const turns = {};
const rawTakes = [];                   // spoken takes played as they are: { slug, t, gain, pan, wet, echo }    // each time a line comes round it takes the next inflection
function singLine(line, t, starts, midis, end_hold, o) {
  const n = Math.min(starts.length, NSYL[line]);    // fewer notes than syllables: the line stops early
  if (n < 2) return;
  turns[line] ??= 0;
  phrases.push({ line, turn: turns[line]++, pin: o.pin ?? null, mode: o.mode ?? null, t, starts: starts.slice(0, n).map((x) => Math.round(x * 1000) / 1000), midis: midis.slice(0, n), end_hold: Math.round(end_hold * 1000) / 1000, ...o });
}

// The tax bracket ("break a tax bracket and then do nothing") has to be heard (@jeffrey 2026-11-20: "the
// tax bracket lyrics aren't that easy to hear / or the doooo noooothing"; then "deeper harmonics lower
// octaves", "closer to mic", "more overtones", "choral arrangements there"). The dry takes read whole;
// in the mix whisper heard none of the words, the voice 4-6 dB under everything else at 1-4 kHz. So on
// this line only: the lead is louder, nearly dry and sung "close" (bin/sing-lines.py: proximity, tighter
// level, an exciter); the same line is doubled an octave under it; and two more voices sing it on the
// chord (the next chord tone over each note, and the one over that), one per ear, wetter than the lead. All of it is the takes already in the vault, sung again by WORLD.
const taxRow = (row) => !!row.sing && !!row.mel && MEL[row.mel].filter((n) => Math.floor(n[0] / 8) === 3).length >= NSYL.nothing;
function taxLine(t, starts, midis, hold, chordOf, o = {}) {
  const g = o.gain ?? CFG.gain.sung * 2.0, own = o.pin ? { pin: o.pin } : {};
  singLine("nothing", t, starts, midis, hold, { gain: g, pan: 0, wet: 0.02, echo: 0, mode: "close", ...own });
  // the octave under: its fundamental is cut at 80 Hz by the close voice, so it adds body and
  // low harmonics without sitting on the bass. (A second octave down would be under 46 Hz: not sung.)
  for (const [down, lvl] of o.lows ?? [[12, 0.55]])
    singLine("nothing", t, starts, midis.map((m) => m - down), hold, { gain: g * lvl, pan: 0, wet: 0.02, echo: 0, mode: "close" });
  // the choir: chord tones over the lead, by the chord under each syllable
  const over = (m, c) => { const pcs = new Set(c.notes.map((x) => ((x % 12) + 12) % 12)); for (let x = m + 3; x < m + 12; x++) if (pcs.has(x % 12)) return x; return m + 7; };
  const cs = starts.map((st) => chordOf(t + st));
  const h1 = midis.map((m, i) => over(m, cs[i])), h2 = h1.map((m, i) => over(m, cs[i])), oct = midis.map((m) => m + 12);
  const c = o.choir ?? 1;
  singLine("nothing", t + 0.012, starts, h1, hold, { gain: g * 0.34 * c, pan: -0.7, wet: 0.3, echo: 0.04, mode: "close", pin: ["warm"] });
  singLine("nothing", t + 0.02, starts, h2, hold, { gain: g * 0.3 * c, pan: 0.7, wet: 0.3, echo: 0.04, mode: "close", pin: ["cheeky"] });
  // (the octave over the lead was a third choir voice; left out to keep the choir small — "more conservative")
  void oct;
}

// The conservative arrangement (@jeffrey 2026-11-20: "i wonder if the overall mix is too layered in the
// end" / "we need some more subtlety and introduction to bring people into the sound before we smash em
// with everything" / "so we can be more conservative in the final arrangement"). It is made by taking
// parts out of the table above, row by row; --dense plays the table as written. A row's `from` holds a
// part back until that bar of the row.
//   opening: air, bloops, bells and the gold gong; the pad at bar 5; then the bass; then a light kit
//            (kick on one and three, quiet hats, the snare only in the second half); the first chorus
//            is the first time everything is on
//   sung rows: jeffrey is the lead, so the chant, the oo/ah choir, the acid strings, the synth on the
//            tune and its harmony, the arp, the air, the shaker, the wub and the bloops are out
//            (the last sung row keeps the synth, its harmony and the strings: the peak)
//   verses 2: drop back — half-time kick, no snare, no acid, no strings, no arp
//   everywhere: no shaker under the hats, no acid strings or air beside the acid line
const DENSE = process.argv.includes("--dense"), LONG = process.argv.includes("--long");
// The long cut keeps the whole dance bit (@jeffrey 2026-11-20: "and what about the 5 minute version …
// with the whole dance bit the more stone club transition"): after the machine come two rows of stone
// club's minimal machine (ARRANGEMENT.md, "techno": tight kick, dry rim on the five-sixteenth tick,
// closed hats, nothing ringing but the stone bells on the walking tune, an offbeat bass), and the second
// ends the way stone club enters its last chorus: a two-octave run of stone bells that slams the door.
// The single takes one of the two rows (@jeffrey: "can the shorter version have the stone parts too … i want
// the ultimate pop version"): the second, with the bell run, between the machine and the finale. --no-stone
// leaves it out of the single.
if (!DENSE && !process.argv.includes("--no-stone")) SINGLE.splice(SINGLE.indexOf("machine") + 1, 0, "club'");
// The ending (@jeffrey 2026-11-20: "the ending is a bit unresolved too … could die out faster / be more cute …
// be more cute woosh"): the record ended on eight bars of mist over a bare E fifth and a forty-second ring.
// Now the mist row is gone; the last outro row closes on E major in its last two bars with the tune on E,
// a hand stops the gong just before the end, and the last thing heard is a small whoosh up the E chord
// with a twinkle of bloops across it. Two and a half seconds of tail.
if (!DENSE) { for (const L of [SINGLE]) { const i = L.indexOf("mist"); if (i >= 0) L.splice(i, 1); } FORM.splice(FORM.findIndex((r) => r.name === "mist"), 1); CFG.tail = 2.5; }
if (!DENSE) FORM.splice(FORM.findIndex((r) => r.name === "machine") + 1, 0,
  { name: "club", block: "PEDAL", mode: "pent", kick: "four", tight: true, rim: 0.9, hat: 0.8, bass: "pulse", mel: "verse1", voice: "bells" },
  { name: "club'", block: "PEDAL", mode: "pent", kick: "four", tight: true, rim: 0.9, hat: 0.8, bass: "pulse", mel: "verse1b", voice: "bells", build: 1, gliss: true });
if (!DENSE) for (const r of FORM) {
  const cut = (...ks) => ks.forEach((k) => delete r[k]);
  if (r.name === "intro") { cut("kick", "hat", "shk"); r.from = { pad: 4, bass: 4 }; if (LONG) cut("bass"); }
  else if (r.name === "verse 1") { cut("clap", "shk", "arp"); r.kick = "half"; r.hat = 0.4; r.from = { kick: 4, hat: 4 }; }
  else if (r.name === "verse 1'") { cut("shk", "air"); r.kick = "half"; r.hat = 0.5; r.clap = 0.5; r.from = { clap: 4, arp: 4 }; }
  else if (r.name === "verse 2" || r.name === "verse 2'") { cut("clap", "shk", "acid", "strings", "arp"); r.kick = "half"; r.hat = 0.6; }
  else if (r.name === "outro") cut("shk", "arp");   // the gong stands on this door: room for it
  else if (r.sing) {
    cut("back", "choir", "arp", "air", "shk", "wub", "bloop", "low");
    if (r.name === "final a") { r.arp = [0.45, 4]; r.until = { arp: 4 }; }   // the arpeggio the stamp sets off runs on four bars into the finale
    if (r.name === "final b" || r.name === "final b'") r.strings = 0.5; else { cut("strings", "harm"); r.voice = "bells"; }
  }
  else if (r.name === "machine") {
    // the rap bed (@jeffrey: "around 2:00 would be better to have some rap … super fast commit rap … and grand
    // piano"): the machine keeps its tight kick, the dry rim, quiet hats and the bass; the acid, the wub, the
    // bloops, the chant and the chops are out, and the grand piano and the commit rap take their place
    cut("strings", "shk", "air", "pad", "acid", "wub", "bloop", "back", "chops"); r.hat = 0.3; r.rim = 0.6; r.rap = 1;
  }
  else if (r.name === "chorus 1") {
    // no voice before the first sung row (@jeffrey 2026-11-20: "i think no lyrics until 0:50"): the first
    // chorus is the band and the synth on the hook; the chant, the oo choir and the title scream are out of it
    cut("strings", "shk", "air", "back", "choir");
  }
  else if (r.acid) { cut("strings", "shk", "air"); if (r.choir) r.choir = Math.min(r.choir, 0.4); }
}
// The rap bed has two rows. It was made for the commit rap at the speed it was rapped (@jeffrey: "the rap needs
// to be extended not compressed vocals"); then "lets remove the rap lyrics … but i love the instrumentation
// there": the rap is muted and the two rows of piano, kick, bass and rim stay exactly as they were. --rap
// puts the voice back (bin/cutie-rap.mjs has the takes; pop/bin/rapvox.py tunes them).
const RAP_VOCAL = process.argv.includes("--rap");
if (!DENSE) {
  const m = FORM.find((r) => r.name === "machine");
  FORM.splice(FORM.indexOf(m) + 1, 0, { ...m, name: "machine'", rap: 2 }); delete m.drop; delete m.build;
  if (SINGLE.includes("machine")) SINGLE.splice(SINGLE.indexOf("machine") + 1, 0, "machine'");
}
let _w = 0x51ed270b; const wonk = () => ((_w = (_w * 1664525 + 1013904223) >>> 0) / 4294967296), wonkLog = [];
const onFrom = (row, part, bar) => bar >= (row.from?.[part] ?? 0);

const ROWS = process.argv.includes("--long") ? FORM : SINGLE.map((n) => FORM.find((r) => r.name === n));
const BARS = ROWS.length * 8;
const DUR = BARS * BAR + CFG.tail;
const N = Math.ceil(DUR * SR);

// The gong's pull (@jeffrey 2026-11-20: "i want the gong to bend the pitch shape … lean everything
// towards i mean … thatd be sooo ill to hear"). While the gold gong rings, every pitched note the track
// plays is drawn to the gong's own tuning: to the nearest of its strong simulated partials (the modes
// pop/bell's FEM solve gives for this body, any octave). At the strike the note sits ON the partial;
// the pull lets go as the ring decays (e-fold `back` seconds). A note further than `reach` cents from
// every partial is left alone. Bass, pad, air, bloops, arp, the synth tune, the acid line and its
// strings all lean; the drums do not, and neither do the bells (FEM samples) or the sung voice.
// The gong itself bends too: struck hard, a gong's modes sit sharp while the metal is stretched and
// settle as it calms, so the ring is read `glide` cents sharp at the strike, easing home over `settle` s.
const GONG = { note: "E2", material: "gold", geometry: "church", ring: 40, glide: 100, settle: 0.8 };
const LEAN = { depth: 1, back: 9, reach: 60 };
const GONG_AT = [0, ...[ROWS.findIndex((r) => r.name === "outro" || r.name === "outro'")].filter((i) => i >= 0).map((i) => i * 8 * BAR)];
const GONG_MODES = (() => {
  const file = resolve(BELL_CACHE, `${GONG.note}-${GONG.material}-${GONG.geometry}.modes.json`);
  if (existsSync(file)) return JSON.parse(readFileSync(file, "utf8"));
  const m = bellModes({ note: GONG.note, material: GONG.material, geometry: GONG.geometry }).modes.map(({ m, freq, tau, part }) => ({ m, freq, tau, part }));
  mkdirSync(BELL_CACHE, { recursive: true }); writeFileSync(file, JSON.stringify(m)); return m;
})();
const GONG_PCS = GONG_MODES.filter((m, i) => i === 0 || m.part >= 0.4).map((m) => (((69 + 12 * Math.log2(m.freq / 440)) % 12) + 12) % 12);
const leanLog = {};
function leanMidi(t, midi, part) {
  if (DENSE) return midi;
  let w = 0;
  for (const ts of GONG_AT) { const age = t - ts; if (age >= 0 && age < LEAN.back * 4) w += LEAN.depth * Math.exp(-age / LEAN.back); }
  if (w <= 0) return midi;
  let pull = 99;
  for (const pc of GONG_PCS) { const d = ((((pc - midi) % 12) + 18) % 12) - 6; if (Math.abs(d) < Math.abs(pull)) pull = d; }
  if (Math.abs(pull) * 100 > LEAN.reach) return midi;
  const by = Math.round(Math.min(1, w) * pull * 1e4) / 1e4;
  if (part && Math.abs(by) * 100 > (leanLog[part] ?? 0)) leanLog[part] = Math.abs(by) * 100;
  return midi + by;
}

const clock = (s) => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, "0")}`;

if (process.argv.includes("--layers")) {   // what is sounding in each row, as JSON (with --dense: the table as written)
  const PARTS = ["kick", "clap", "hat", "shk", "rim", "bass", "pad", "air", "arp", "bloop", "acid", "strings", "wub", "choir", "back", "chops", "sing", "solo", "build"];
  console.log(JSON.stringify(ROWS.map((r, i) => ({ row: r.name, bar: i * 8, t: i * 8 * BAR,
    parts: [...PARTS.filter((k) => r[k]), ...(r.mel ? (r.voice === "both" ? ["bells", "synth"] : [r.voice]) : []), ...(r.harm ? ["harm"] : []), ...(r.low ? ["low"] : []), ...(r.rap ? ["piano"] : []),
      ...(r.sing && taxRow(r) ? ["tax choir ×2", "tax low octave"] : [])], from: r.from ?? null }))));
  process.exit(0);
}
if (process.argv.includes("--form")) {
  ROWS.forEach((r, i) => console.log(
    `${String(i * 8).padStart(3)}–${String(i * 8 + 8).padEnd(3)} ${clock(i * 8 * BAR).padEnd(5)} ${r.name.padEnd(10)} ${r.mode.padEnd(4)} ${BLOCKS[r.block].map(([c, n]) => `${c}×${n}`).join(" · ")}`));
  console.log(`${BARS} bars · ${CFG.bpm} BPM · ${clock(DUR)}`);
  process.exit(0);
}

// ── buses ───────────────────────────────────────────────────────────────
const kickBus = new Float32Array(N);
const bassBus = new Float32Array(N);
const acidBus = new Float32Array(N);
const drums = new Float32Array(N * 2);
const music = new Float32Array(N * 2);
const bells = new Float32Array(N * 2);
const rev = new Float32Array(N * 2);   // reverb send, processed in place
const dly = new Float32Array(N);       // delay send
const sung = [];                       // jeffrey notes, sung after the arrangement is known
const kicks = [];                      // [time, gain] for the sidechain
const snares = [];                     // [time, gain]: each clap ducks the voices

const mtof = (m) => 440 * 2 ** ((m - 69) / 12);
const clamp = (v, a, b) => Math.max(a, Math.min(b, v));
let _r = 0xa7757e77;
const rnd = () => ((_r = (_r * 1664525 + 1013904223) >>> 0) / 4294967296);
let _n = 0x1badcafe;
const noise = () => { _n ^= _n << 13; _n ^= _n >>> 17; _n ^= _n << 5; return (_n >>> 0) / 2147483648 - 1; };
const panL = (p) => Math.cos((clamp(p, -1, 1) + 1) * Math.PI / 4);
const panR = (p) => Math.sin((clamp(p, -1, 1) + 1) * Math.PI / 4);
// Sixteenth position → seconds within a bar, with the off-sixteenths swung late.
const six = (pos) => pos * SIX + (pos % 2 ? CFG.swing * SIX : 0);

// ── voices ──────────────────────────────────────────────────────────────
function kick(t, g = 1, tight = false) {
  const K = CFG.kick, i0 = Math.round(t * SR), n = Math.floor((tight ? 0.26 : 0.55) * SR);
  const dec = tight ? 0.09 : K.decay;
  ev("kick", t, g, tight ? 1 : 0);
  let ph = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const x = k / SR;
    ph += (K.end + (K.start - K.end) * Math.exp(-x / K.sweep)) / SR;
    kickBus[i0 + k] += Math.sin(TAU * ph) * Math.min(1, x / K.attack) * Math.exp(-x / dec) * g * K.gain;
  }
  kicks.push([t, g]);
}

// Spray: white noise through one state-variable filter. type 0 band, 1 high.
function spray(t, { fc, q = 1, type = 0, att = 0.001, dec = 0.05, len, gain, pan = 0, wet = 0 }) {
  const i0 = Math.round(t * SR), n = Math.floor((len ?? dec * 6 + att) * SR);
  ev("spray", t, fc, q, type, att, dec, len ?? dec * 6 + att, gain, pan, wet);
  const f = 2 * Math.sin(Math.PI * Math.min(fc, 7600) / SR), damp = 1 / q;
  const gl = gain * panL(pan), gr = gain * panR(pan);
  let lo = 0, bd = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const x = k / SR, hi = noise() - lo - damp * bd;
    bd += f * hi; lo += f * bd;
    const v = (type ? hi : bd) * Math.min(1, x / att) * Math.exp(-x / dec);
    const j = (i0 + k) * 2;
    drums[j] += v * gl; drums[j + 1] += v * gr;
    if (wet) { rev[j] += v * gl * wet; rev[j + 1] += v * gr * wet; }
  }
}
function clap(t, g) {
  ev("snare", t, g); snares.push([t, g]);
  for (const [o, a] of [[0, 0.5], [0.011, 0.7], [0.022, 1]])
    spray(t + o, { fc: 1500, q: 1.4, dec: o === 0.022 ? 0.085 : 0.012, gain: CFG.gain.clap * g * a, wet: 0.35 });
}
// rock snare: a short drum-head tone under two bands of noise, longer than the
// clap, and it ducks the voices the same way
function snare(t, g) {
  ev("snare", t, g); snares.push([t, g]);
  tick(t, 190, CFG.gain.clap * g * 0.9, 0); tick(t + 0.003, 330, CFG.gain.clap * g * 0.45, 0);
  spray(t, { fc: 2200, q: 0.8, dec: 0.13, gain: CFG.gain.clap * g * 1.25, wet: 0.4 });
  spray(t, { fc: 5600, q: 0.7, type: 1, dec: 0.09, gain: CFG.gain.clap * g * 0.6, wet: 0.3 });
}
const hat = (t, g, open) => spray(t, { fc: 7200, q: 0.9, type: 1, dec: open ? 0.11 : 0.022, gain: CFG.gain.hat * g, pan: 0.25 });
const shaker = (t, g, pan) => spray(t, { fc: 5200, q: 2.2, att: 0.008, dec: 0.028, gain: CFG.gain.shaker * g, pan });
function tick(t, hz, g, pan) {
  ev("tick", t, hz, g, pan);
  const i0 = Math.round(t * SR), n = Math.floor(0.05 * SR), gl = g * panL(pan), gr = g * panR(pan);
  let ph = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const x = k / SR; ph += hz / SR;
    const v = Math.sin(TAU * ph) * Math.exp(-x / 0.009), j = (i0 + k) * 2;
    drums[j] += v * gl; drums[j + 1] += v * gr; rev[j] += v * gl * 0.2; rev[j + 1] += v * gr * 0.2;
  }
}

// Bloop: a sine that rises into its note — a drip. The lane's signature percussion.
// A donk: the short hollow pitched knock of bounce music, for the intro's wonky snares (@jeffrey 2026-11-20:
// "and have a bit of donk to them"). A sine that starts 2.2 times too high and drops to its note in a few
// milliseconds (e-fold 12 ms), a touch of FM on the front (index 0.8 at 2.7× the note, gone in 30 ms) for
// the wooden edge, 2 ms attack, e-fold 60 ms: about a fifth of a second long.
function donk(t, midi, g, pan) {
  midi = leanMidi(t, midi, "donk");
  ev("donk", t, midi, g, pan);
  const f = mtof(midi), i0 = Math.round(t * SR), n = Math.floor(0.3 * SR), gl = g * panL(pan), gr = g * panR(pan);
  let ph = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const x = k / SR;
    ph += f * (1 + 1.2 * Math.exp(-x / 0.012)) / SR;
    const v = Math.sin(TAU * ph + 0.8 * Math.exp(-x / 0.03) * Math.sin(TAU * 2.7 * ph)) * Math.min(1, x / 0.002) * Math.exp(-x / 0.06);
    const j = (i0 + k) * 2;
    music[j] += v * gl; music[j + 1] += v * gr;
    rev[j] += v * gl * 0.2; rev[j + 1] += v * gr * 0.2;
  }
}
function bloop(t, midi, g, pan, len = 0.2) {
  midi = leanMidi(t, midi, "bloop");
  ev("bloop", t, midi, g, pan, len);
  const f = mtof(midi), i0 = Math.round(t * SR), n = Math.floor(len * 4 * SR);
  const a = g * CFG.gain.bloop, gl = a * panL(pan), gr = a * panR(pan);
  let ph = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const x = k / SR;
    ph += f * (1 - 0.48 * Math.exp(-x / 0.028)) / SR;
    const v = Math.sin(TAU * ph) * Math.min(1, x / 0.004) * Math.exp(-x / (len * 0.42));
    const j = (i0 + k) * 2;
    music[j] += v * gl; music[j + 1] += v * gr;
    rev[j] += v * gl * 0.45; rev[j + 1] += v * gr * 0.45;
    dly[i0 + k] += v * a * 0.42;
  }
}

function bassNote(t, dur, midi, g, soft = false) {
  midi = leanMidi(t, midi, "bass");
  ev("bass", t, dur, midi, g, soft ? 1 : 0);
  const f = mtof(midi), i0 = Math.round(t * SR), n = Math.floor((dur + 0.12) * SR);
  const att = soft ? 0.12 : 0.006;
  let ph = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const x = k / SR; ph += f / SR; if (ph > 1) ph -= 1;
    const p = TAU * ph, pl = soft ? 0.25 : Math.exp(-x / 0.14);
    const tone = Math.sin(p) + pl * (0.42 * Math.sin(2 * p) + 0.2 * Math.sin(3 * p) + 0.1 * Math.sin(4 * p));
    const env = Math.min(1, x / att) * (x < dur ? 1 : Math.max(0, 1 - (x - dur) / 0.12)) * (soft ? 1 : 0.55 + 0.45 * Math.exp(-x / 0.25));
    bassBus[i0 + k] += tone * env * g;
  }
}

// Acid: one saw through a four-pole resonant ladder whose cutoff snaps open
// on every note and harder on the accents; tied steps slide instead of
// retriggering. One continuous voice per row, so slides and filter state carry.
const ACID_PAT = [   // [ladder index, accent, slide into the next step] or [] for a rest
  [[0], [], [0], [3, 1, 1], [], [0], [], [2], [0], [], [3, 1], [0, 0, 1], [], [1], [0], [4, 1]],
  [[0], [0], [], [3, 1], [0, 0, 1], [], [2], [], [0], [3, 0, 1], [], [0], [1, 1], [], [3], [2, 0, 1]],
  // the long one: tied steps hold a note for a beat or more and slide between them like a bowed string
  [[0, 1, 1], [0, 0, 1], [0, 0, 1], [3, 0, 1], [3, 0, 1], [3], [], [2, 1, 1], [2, 0, 1], [2, 0, 1], [0, 0, 1], [0], [], [4, 1, 1], [4, 0, 1], [3]],
];
const ACID_ORDER = [0, 2, 1, 2];

// Acid strings: the chord itself as long notes — two detuned saws a tone
// through the same kind of ladder, opening slowly instead of snapping.
function acidChord(t, dur, notes, level, cut0, cut1) {
  notes = notes.map((m) => leanMidi(t, m, "strings"));
  ev("acidchord", t, dur, level, cut0, cut1, notes.length, ...notes);
  const i0 = Math.round(t * SR), rel = 0.5, n = Math.floor((dur + rel) * SR), k = 0.5 * 3.8;
  const inc = notes.flatMap((m) => [mtof(m) * 2 ** (-7 / 1200) / SR, mtof(m) * 2 ** (7 / 1200) / SR]);
  const ph = inc.map((_, i) => (i * 0.37) % 1);
  let y1 = 0, y2 = 0, y3 = 0, y4 = 0;
  for (let i = 0; i < n && i0 + i < N; i++) {
    const x0 = i / SR, u = Math.min(1, x0 / dur);
    let saw = 0;
    for (let v = 0; v < inc.length; v++) {
      const dt = inc[v]; let p = ph[v] + dt; if (p >= 1) p -= 1; ph[v] = p;
      let w = 2 * p - 1;
      if (p < dt) { const q = p / dt; w -= q + q - q * q - 1; }
      else if (p > 1 - dt) { const q = (p - 1) / dt; w -= q * q + q + q + 1; }
      saw += w;
    }
    saw /= inc.length;
    const swell = 0.5 - 0.5 * Math.cos(Math.PI * u);
    const fc = Math.min(8000, 220 * 2 ** ((cut0 + (cut1 - cut0) * swell) * 4.2 + 0.5 * Math.sin(TAU * (t + x0) / (2 * BAR))));
    const g = 1 - Math.exp(-TAU * fc / SR);
    const x = Math.tanh(saw * 1.6 - k * y4);
    y1 += g * (x - y1); y2 += g * (y1 - y2); y3 += g * (y2 - y3); y4 += g * (y3 - y4);
    acidBus[i0 + i] += y4 * 1.6 * Math.min(1, x0 / 0.09) * (x0 < dur ? 1 : Math.max(0, 1 - (x0 - dur) / rel)) * level;
  }
}
// Every step also carries its own shape: atk (how fast the amp opens), len
// (the gate, in sixteenths), fdec (how fast the filter closes again), out (a
// pitch-out: when the gate shuts the note dives or chirps that many semitones
// through a longer release), and whis (the whistle: the ladder near
// self-oscillation with its cutoff riding the pitch, so the saw thins to a tone).
function acidRow(t0, evs, level, cut0, cut1, reso = 0.78) {
  for (const e of evs) e.midi = leanMidi(e.t, e.midi, "acid");
  ev("acidrow", t0, level, cut0, cut1, reso, evs.length);
  for (const e of evs) ev(".", e.t, e.midi, e.acc, e.slide, e.atk, e.len, e.fdec, e.out, e.whis, e.lvl ?? 1);
  const i0 = Math.round(t0 * SR), n = Math.round(8 * BAR * SR), k = reso * 3.8, kw = 0.95 * 3.8;
  let ph = 0, f = 0, ft = 0, base = 0, y1 = 0, y2 = 0, y3 = 0, y4 = 0, fenv = 0, aenv = 0, gateEnd = 0, acc = 0, e = 0, tied = false;
  let atk = 0.02, fdec = 0.99988, out = 0, whis = 0, outDone = false, lvl = 1, lv = 1;   // lvl: the step's own level (the line steps back under a sung line), eased so it does not click
  for (let i = 0; i < n && i0 + i < N; i++) {
    const t = t0 + i / SR;
    while (e < evs.length && evs[e].t <= t) {
      const ev = evs[e++];
      ft = base = mtof(ev.midi);
      if (!tied || f === 0) { f = ft; fenv = 1; acc = ev.acc; }
      gateEnd = ev.t + SIX * ev.len;
      tied = ev.slide; atk = ev.atk; fdec = ev.fdec; out = ev.out; whis = ev.whis; outDone = false; lvl = ev.lvl ?? 1;
    }
    if (f === 0) continue;
    const gate = t < gateEnd;
    if (!gate && out && !outDone) { ft = base * 2 ** (out / 12); outDone = true; }
    f += (ft - f) * (gate ? 0.0008 : 0.003);
    aenv += ((gate ? 1 : 0) - aenv) * (gate ? atk : out ? 0.0007 : 0.004);
    fenv *= fdec;
    lv += (lvl - lv) * 0.002;
    const dt = f / SR;
    ph += dt; if (ph >= 1) ph -= 1;
    let saw = 2 * ph - 1;
    if (ph < dt) { const u = ph / dt; saw -= u + u - u * u - 1; }
    else if (ph > 1 - dt) { const u = (ph - 1) / dt; saw -= u * u + u + u + 1; }
    const fc = whis ? Math.min(9000, f * 2) : Math.min(9000, 150 * 2 ** ((cut0 + (cut1 - cut0) * (i / n)) * 4.5 + (1.6 + acc) * fenv));
    const g = 1 - Math.exp(-TAU * fc / SR), kk = whis ? kw : k;
    const x = Math.tanh(saw * (whis ? 0.25 : 0.8) - kk * y4);
    y1 += g * (x - y1); y2 += g * (y1 - y2); y3 += g * (y2 - y3); y4 += g * (y3 - y4);
    acidBus[i0 + i] += Math.tanh(y4 * (1 + kk * 0.6) * 1.4) * aenv * (0.75 + 0.35 * acc) * level * (whis ? 0.8 : 1) * lv;
  }
}

// Wub: a held saw over its own sub-octave square, the ladder's cutoff swung
// by an LFO locked to the beat. The rate changes by the bar — quarters,
// eighths, triplets, sixteenths — so the bass talks.
const WUB_RATES = [1, 1, 2, 1, 2, 2, 3, 4];     // wobbles per beat, by bar of the row
function wubRow(t0, chords, level, dropLast) {
  ev("wub", t0, level, dropLast ? 1 : 0, ...Array.from({ length: 8 }, (_, bar) => chordAt(chords, bar).bass));
  const i0 = Math.round(t0 * SR), bars = dropLast ? 7 : 8, n = Math.round(bars * BAR * SR), k = 0.55 * 3.8;
  const barN = BAR * SR, beatN = BEAT * SR;
  let ph = 0, f = 0, y1 = 0, y2 = 0, y3 = 0, y4 = 0;
  for (let i = 0; i < n && i0 + i < N; i++) {
    const bar = Math.floor(i / barN), c = chordAt(chords, bar), ft = mtof(c.bass + (c.bass < 36 ? 12 : 0));
    f = f ? f + (ft - f) * 0.0006 : ft;
    const inBar = i - bar * barN, lfo = 0.5 - 0.5 * Math.cos(TAU * WUB_RATES[bar] * inBar / beatN);
    const dt = f / SR;
    ph += dt * 0.5; if (ph >= 1) ph -= 1;                 // phase runs at the sub-octave
    const p2 = (ph * 2) % 1;
    let saw = 2 * p2 - 1;
    if (p2 < dt) { const u = p2 / dt; saw -= u + u - u * u - 1; }
    else if (p2 > 1 - dt) { const u = (p2 - 1) / dt; saw -= u * u + u + u + 1; }
    const sub = ph < 0.5 ? 0.5 : -0.5;
    const fc = 85 * 2 ** (lfo ** 1.4 * 4.9);
    const g = 1 - Math.exp(-TAU * fc / SR);
    const x = Math.tanh((saw + sub) * 1.3 - k * y4);
    y1 += g * (x - y1); y2 += g * (y1 - y2); y3 += g * (y2 - y3); y4 += g * (y3 - y4);
    const edge = Math.min(1, i / 600, (n - i) / 2400);
    acidBus[i0 + i] += Math.tanh(y4 * 2.2) * (0.55 + 0.45 * lfo) * edge * level;
  }
}

function padNote(t, dur, midi, g, pan, bright) {
  midi = leanMidi(t, midi, "pad");
  ev("pad", t, dur, midi, g, pan, bright);
  const f = mtof(midi), i0 = Math.round(t * SR), rel = 1.6, n = Math.floor((dur + rel) * SR);
  const gl = g * panL(pan), gr = g * panR(pan);
  let ph = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const x = k / SR;
    ph += f * (1 + 0.0016 * Math.sin(TAU * 0.13 * (t + x) + midi)) / SR; if (ph > 1) ph -= 1;
    const p = TAU * ph;
    const v = (0.7 * Math.sin(p) + 0.2 * Math.sin(2 * p + 0.4) + bright * 0.1 * Math.sin(3 * p))
      * Math.min(1, x / 0.35) * (x < dur ? 1 : Math.max(0, 1 - (x - dur) / rel));
    const j = (i0 + k) * 2;
    music[j] += v * gl; music[j + 1] += v * gr;
    rev[j] += v * gl * 0.5; rev[j + 1] += v * gr * 0.5;
  }
}

// Air: breath with a pitch — noise through a narrow band at the note, a
// different noise in each ear. This is the layer that keeps the room open.
function airNote(t, dur, midi, g, pan = 0) {
  midi = leanMidi(t, midi, "air");
  ev("air", t, dur, midi, g, pan);
  const fc = Math.min(mtof(midi), 6000), i0 = Math.round(t * SR), rel = 1.8, n = Math.floor((dur + rel) * SR);
  const f = 2 * Math.sin(Math.PI * fc / SR), damp = 1 / 22, a = g * CFG.gain.air * 0.05;
  const gl = a * panL(pan * 0.45), gr = a * panR(pan * 0.45);
  let l1 = 0, b1 = 0, l2 = 0, b2 = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const x = k / SR;
    let hi = noise() - l1 - damp * b1; b1 += f * hi; l1 += f * b1;
    hi = noise() - l2 - damp * b2; b2 += f * hi; l2 += f * b2;
    const env = Math.min(1, x / 0.7) * (x < dur ? 1 : Math.max(0, 1 - (x - dur) / rel))
      * (0.72 + 0.28 * Math.sin(TAU * 0.21 * (t + x) + midi));
    const j = (i0 + k) * 2;
    music[j] += b1 * env * gl; music[j + 1] += b2 * env * gr;
    rev[j] += b1 * env * gl * 0.8; rev[j + 1] += b2 * env * gr * 0.8;
  }
}

// Pluck: a hollow little square whose upper partials close fast.
function pluck(t, midi, g, pan, decay = 0.22) {
  midi = leanMidi(t, midi, "arp");
  ev("pluck", t, midi, g, pan, decay);
  const f = mtof(midi), i0 = Math.round(t * SR), n = Math.floor(decay * 5 * SR);
  const a = g * CFG.gain.arp, gl = a * panL(pan), gr = a * panR(pan);
  let ph = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const x = k / SR; ph += f / SR; if (ph > 1) ph -= 1;
    const p = TAU * ph, c = Math.exp(-x / 0.06);
    const v = (Math.sin(p) + c * (0.33 * Math.sin(3 * p) + 0.16 * Math.sin(5 * p)))
      * Math.min(1, x / 0.003) * Math.exp(-x / decay);
    const j = (i0 + k) * 2;
    music[j] += v * gl; music[j + 1] += v * gr;
    rev[j] += v * gl * 0.3; rev[j + 1] += v * gr * 0.3;
    dly[i0 + k] += v * a * 0.5;
  }
}

// Lead: the hook's voice — a bloop that stays, with a late soft vibrato.
function lead(t, dur, midi, vel, g = CFG.gain.lead, pan = 0) {
  midi = leanMidi(t, midi, "synth");
  ev("lead", t, dur, midi, vel, g, pan);
  const f = mtof(midi), i0 = Math.round(t * SR), rel = 0.45, n = Math.floor((dur + rel) * SR);
  const a = vel * g, gl = a * panL(pan), gr = a * panR(pan);
  let ph = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const x = k / SR;
    const vib = x > 0.22 ? 1 + 0.0045 * Math.min(1, (x - 0.22) / 0.3) * Math.sin(TAU * 5.1 * x) : 1;
    ph += f * (1 - 0.3 * Math.exp(-x / 0.014)) * vib / SR; if (ph > 1) ph -= 1;
    const p = TAU * ph;
    const v = (Math.sin(p) + 0.2 * Math.sin(2 * p) + 0.09 * Math.exp(-x / 0.18) * Math.sin(3 * p))
      * Math.min(1, x / 0.008) * (0.6 + 0.4 * Math.exp(-x / 0.4)) * (x < dur ? 1 : Math.max(0, 1 - (x - dur) / rel));
    const j = (i0 + k) * 2;
    music[j] += v * gl; music[j + 1] += v * gr;
    rev[j] += v * gl * 0.5; rev[j + 1] += v * gr * 0.5;
    dly[i0 + k] += v * a * 0.3;
  }
}

// Flyby: the jet, as one sine passing overhead.
function flyby(t, dir) {
  ev("flyby", t, dir);
  const dur = 2.5 * BAR, i0 = Math.round(t * SR), n = Math.floor(dur * SR);
  let ph = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const u = k / n, s = 1 / (1 + Math.exp(-(u - 0.5) * 14));
    ph += mtof(dir > 0 ? 76 : 71) * (1.06 - 0.12 * s) * (1 + 0.003 * Math.sin(TAU * 6 * u * dur)) / SR; if (ph > 1) ph -= 1;
    const v = Math.sin(TAU * ph) * Math.exp(-((u - 0.5) ** 2) / 0.03) * CFG.gain.fly, pan = dir * (2 * u - 1) * 0.9;
    const j = (i0 + k) * 2;
    music[j] += v * panL(pan); music[j + 1] += v * panR(pan);
    rev[j] += v * 0.5; rev[j + 1] += v * 0.5;
  }
}

// Riser: spray climbing through a moving band, into a seam.
// The build: cabin air, after Menu Band's air voice. Three layers of the same
// noise, a channel each side: a brown floor (the cabin), six narrow whistles
// on three low tones of the chord the build is heading for and their octaves,
// which start an octave under and glide up to arrive in tune, and one moving
// band from 110 Hz to 1.3 kHz. (It was a single band swept 350 Hz to 5.6 kHz.)
// The weights keep the old sweep's level: measured 0.46 against 0.48 rms.
function riser(t, dur, g, hz) {
  ev("riser", t, dur, g, hz[0], hz[1], hz[2]);
  const i0 = Math.round(t * SR), n = Math.floor(dur * SR);
  const S = [new Float64Array(16), new Float64Array(16)];   // per side: sweep lo, bd · brown, floor · six whistles' lo, bd
  for (let k = 0; k < n && i0 + k < N; k++) {
    const u = k / n, f = 2 * Math.sin(Math.PI * (110 * 12 ** u) / SR), up = 2 ** (u - 1);
    const a = u * u * g * CFG.gain.riser, j = (i0 + k) * 2;
    for (let ch = 0; ch < 2; ch++) {
      const s = S[ch], x = noise();
      const hi = x - s[0] - 0.3 * s[1]; s[1] += f * hi; s[0] += f * s[1];
      s[2] = s[2] * 0.995 + x * 0.05; s[3] += 0.06 * (s[2] - s[3]);
      let w = 0;
      for (let i = 0; i < 6; i++) {
        const o = 4 + i * 2, ff = 2 * Math.sin(Math.PI * hz[i >> 1] * (i & 1 ? 2 : 1) * up / SR);
        const h = x - s[o] - 0.03 * s[o + 1]; s[o + 1] += ff * h; s[o] += ff * s[o + 1];
        w += s[o + 1] * (i & 1 ? 0.35 : 1);
      }
      const v = (s[1] * 0.9 + s[3] * 1.3 + w * 0.6) * a;
      music[j + ch] += v; rev[j + ch] += v * 0.6;
    }
  }
}

// Bells: FEM strikes, cached per (note, material, geometry, length), never crunched.
const NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const noteName = (m) => `${NAMES[m % 12]}${Math.floor(m / 12) - 1}`;
const bellCache = new Map();
function bell(t, midi, vel, { material = "stone", geometry = "handbell", dur = 2.5, gain = 1, pan = 0, wet = 0.35 } = {}) {
  while (midi > 76) midi -= 12;                      // runs live at or below E5
  // One solve per (note, body): the FEM engine takes seconds a strike, so
  // every strike is a five- or nine-second ring kept on disk, and short
  // notes are the same ring choked early.
  const ring = dur > 5 ? 9 : 5, key = `${noteName(midi).replace("#", "s")}-${material}-${geometry}-${ring}`;
  let b = bellCache.get(key);
  if (!b) {
    const file = resolve(BELL_CACHE, `${key}.f32`);
    if (existsSync(file)) {
      const raw = readFileSync(file), all = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2), h = all.length >> 1;
      b = { L: all.subarray(0, h), R: all.subarray(h) };
    } else {
      const r = renderBell({ note: noteName(midi), material, geometry, dur: ring, vel: 0.8 });
      let pk = 0;
      for (let i = 0; i < r.L.length; i++) pk = Math.max(pk, Math.abs(r.L[i]), Math.abs(r.R[i]));
      const all = new Float32Array(r.L.length * 2);
      for (let i = 0; i < r.L.length; i++) { all[i] = r.L[i] / (pk || 1); all[r.L.length + i] = r.R[i] / (pk || 1); }
      writeFileSync(file, Buffer.from(all.buffer));
      b = { L: all.subarray(0, r.L.length), R: all.subarray(r.L.length) };
      console.log(`bell ${key}`);
    }
    bellCache.set(key, b);
  }
  const i0 = Math.round(t * SR), n = Math.min(b.L.length, Math.floor(dur * SR)), fade = Math.floor(0.08 * SR);
  const a = vel * gain * 0.3, gl = a * Math.min(1, 1 - pan), gr = a * Math.min(1, 1 + pan);
  ev("bell", BAKE && bakeSmp(resolve(BELL_CACHE, `${key}.f32`)), t, dur, a, pan, wet);
  for (let k = 0; k < n && i0 + k < N; k++) {
    const e = k > n - fade ? (n - k) / fade : 1, j = (i0 + k) * 2;
    const l = b.L[k] * gl * e, r = b.R[k] * gr * e;
    bells[j] += l; bells[j + 1] += r; rev[j] += l * wet; rev[j + 1] += r * wet;
  }
}

// Vocal drops: the cached jeffrey-pvc one-shots, replayed at a rate.
const voxCache = new Map();
function loadVox(name) {
  if (voxCache.has(name)) return voxCache.get(name);
  const r = spawnSync(FFMPEG, ["-v", "error", "-i", resolve(ASSETS, `${name}.mp3`), "-f", "f32le", "-ac", "1", "-ar", String(SR), "pipe:1"], { maxBuffer: 1 << 28 });
  if (r.status !== 0) throw new Error(`vocal decode failed: ${name}`);
  const buf = new Float32Array(r.stdout.buffer, r.stdout.byteOffset, r.stdout.byteLength >> 2);
  // trim to the voiced span so slices land on syllables
  let a = 0, z = buf.length - 1;
  while (a < z && Math.abs(buf[a]) < 0.02) a++;
  while (z > a && Math.abs(buf[z]) < 0.02) z--;
  const out = buf.slice(a, z + 1);
  voxCache.set(name, out);
  return out;
}
function vox(t, name, { rate = 1, gain = 1, pan = 0, from = 0, to = 1, wet = 0.4, echo = 0.3 } = {}) {
  const src = loadVox(name), s0 = from * src.length, s1 = to * src.length;
  const n = Math.floor((s1 - s0) / rate), i0 = Math.round(t * SR), fade = Math.min(400, n >> 2);
  const a = gain * CFG.gain.vox, gl = a * panL(pan), gr = a * panR(pan);
  ev("vox", BAKE && bakeSmp(`vox:${name}`, src), t, rate, gain, pan, from, to, wet, echo);
  for (let k = 0; k < n && i0 + k < N; k++) {
    const p = s0 + k * rate, q = Math.floor(p), fr = p - q;
    const v = ((src[q] ?? 0) * (1 - fr) + (src[q + 1] ?? 0) * fr) * Math.min(1, k / fade, (n - k) / fade);
    const j = (i0 + k) * 2;
    bells[j] += v * gl; bells[j + 1] += v * gr;
    rev[j] += v * gl * wet; rev[j + 1] += v * gr * wet;
    dly[i0 + k] += v * a * echo;
  }
}

// ── arrangement ─────────────────────────────────────────────────────────
// Euclid E(k,16) rotated: the hit positions bloops and shakers walk.
const euclid = (k, rot) => Array.from({ length: 16 }, (_, i) => i).filter((i) => ((i + rot) * k) % 16 < k);
const expand = (block) => { let b = 0; return BLOCKS[block].map(([name, bars]) => { const c = { name, bars, at: b, bass: CH[name][0], notes: CH[name][1] }; b += bars; return c; }); };
const chordAt = (chords, bar) => chords.find((c) => bar >= c.at && bar < c.at + c.bars);

progress.begin({ type: "audio", label: STEM });
let bloopCursor = 0, sungCount = 0;
ROWS.forEach((row, ri) => {
  const b0 = ri * 8, t0 = b0 * BAR, chords = expand(row.block), next = ROWS[ri + 1];
  const scale = SCALES[row.mode];
  // the first round of singing stays quiet and airy and has to be legible (@jeffrey: "for the first round of
  // vocal … keep it quiet airy but like legible"): the voice is not raised there; the parts around it step back
  const firstSung = !DENSE && ri === ROWS.findIndex((r) => r.sing);

  // harmony beds
  for (const c of chords) {
    const t = t0 + c.at * BAR, d = c.bars * BAR;
    if (row.pad && onFrom(row, "pad", c.at)) c.notes.forEach((m, i) => padNote(t + i * 0.03, d - 0.1, m, row.pad * CFG.gain.pad * (taxRow(row) && c.at >= 6 ? 0.6 : firstSung ? 0.6 : row.sing ? 0.75 : 1), (i - (c.notes.length - 1) / 2) * 0.32, row.pad));
    if (row.air) {
      const top = c.notes.slice(-3);
      top.forEach((m, i) => airNote(t, d - 0.2, m + 12, row.air, i - 1));
      airNote(t, d - 0.2, c.notes[0] + 24, row.air * 0.5, 0);
      airNote(t, d - 0.2, c.notes[1] + 36, row.air * 0.2, -1);
      airNote(t, d - 0.2, c.notes[2] + 36, row.air * 0.2, 1);
    }
    if (row.bass === "long" && onFrom(row, "bass", c.at)) bassNote(t, d - 0.15, c.bass + (c.bass < 36 ? 12 : 0), row.acid ? 0.5 : 0.5, true);
    // choir: three of the chord's tones inside jeffrey's range, one per ear and one in the middle
    if (row.choir && c.bars <= 2) {
      const tones = c.notes.filter((m) => m >= 50 && m <= 66).slice(-3);
      const syl = ["ion", "mix", "lyd"].includes(row.mode) ? "ah" : "oo";
      tones.forEach((m, i) => sung.push({ t: t + 0.04 * i, syl, midi: m, dur: d, hold: d - 0.15, gain: row.choir * CFG.gain.choir,
        pan: (i - (tones.length - 1) / 2) * 0.7, attack: 0.3, wet: 0.9, echo: 0 }));
    }
  }
  if (row.acid) {
    const evs = [];
    for (let bar = 0; bar < 8; bar++) {
      if (row.drop && bar === 7) break;
      const c = chordAt(chords, bar), bm = c.bass + (c.bass < 36 ? 12 : 0);
      const up = [...new Set(c.notes.map((m) => ((m - bm) % 12 + 12) % 12))].filter((v) => v).sort((a, b) => a - b);
      const ladder = [bm, bm + (up[1] ?? 7), bm + (up[2] ?? up[1] ?? 7), bm + 12, bm + 12 + (up[1] ?? 7)];
      // bars four and eight of a row end in a whistle: the line lets go of the
      // pattern for two beats and climbs a figure made of itself. Four steps up
      // the ladder, and inside each step the same four in miniature, an octave
      // over the bass, every note tied to the next so it slides.
      // where the tax bracket is sung (bars 7-8 of a sung row) the whistle is left out of bar 8 — it sat at
      // 1.5-4 kHz right on "and then do nothing" — and the line steps back 14 dB for those two bars
      const tax = taxRow(row) && bar >= 6;
      const fractal = bar % 4 === 3 && !tax;
      ACID_PAT[ACID_ORDER[bar % 4]].forEach(([idx, acc = 0, slide = 0], p) => {
        if (idx == null || (fractal && p >= 8)) return;
        // each step its own shape, drawn once: mostly the plain snap, with stabs,
        // swells, long filters and a pitch-out about one note in three
        const r = rnd(), r2 = rnd(), r3 = rnd();
        evs.push({ t: t0 + bar * BAR + six(p), midi: ladder[idx], acc, slide,
          atk: r < 0.55 ? 0.02 : r < 0.8 ? 0.006 : 0.0018,
          len: slide ? 1.1 : r2 < 0.25 ? 0.35 : r2 < 0.7 ? 0.62 : r2 < 0.9 ? 0.9 : 1.6,
          fdec: acc ? (r3 < 0.5 ? 0.99975 : 0.9999) : r3 < 0.2 ? 0.9996 : r3 < 0.75 ? 0.99988 : 0.99995,
          out: slide || rnd() > 0.34 ? 0 : [-12, -7, -5, 12, -12, 7][Math.floor(rnd() * 6)], whis: 0, lvl: tax || firstSung ? 0.2 : row.sing ? 0.45 : 1 });   // the acid sits 4 dB back under any sung row
      });
      if (fractal) {
        const big = [0, 2, 3, 4], small = [0, 3, 2, 4];
        for (let a = 0; a < 4; a++) for (let b2 = 0; b2 < 4; b2++) {
          const i = a * 4 + b2, lad = (j) => ladder[Math.min(4, j)];
          evs.push({ t: t0 + bar * BAR + (8 + i / 2) * SIX, midi: 12 + lad(big[a]) + (lad(small[b2]) - ladder[0]) * (b2 ? 1 : 0), acc: b2 === 0 ? 1 : 0,
            slide: i < 15 ? 1 : 0, atk: 0.004, len: i < 15 ? 0.6 : 2.5, fdec: 0.99995, out: i === 15 ? 12 : 0, whis: 1 });
        }
      }
    }
    acidRow(t0, evs, row.acid[0], row.acid[1], row.acid[2]);
    if (row.wub) wubRow(t0, chords, row.wub, row.drop);
    if (row.strings) for (const c of chords) {
      const tones = c.notes.filter((m) => m >= 50 && m <= 67).slice(-3);
      acidChord(t0 + c.at * BAR, Math.min(c.bars, row.drop ? 7 - c.at : c.bars) * BAR - 0.1, tones, row.strings * 1.1, row.acid[1] * 0.8, row.acid[2] + 0.12);
    }
  }

  for (let bar = 0; bar < 8; bar++) {
    const tb = t0 + bar * BAR, c = chordAt(chords, bar), abs = b0 + bar;
    const building = row.build && bar >= 6, dropped = row.drop && bar === 7;

    // floor
    if (row.kick && bar >= (row.kickFrom ?? 0) && onFrom(row, "kick", bar) && !dropped) {
      // --rock: the kick leaves the floor for one, three and a push (the and of
      // three one bar, the and of two the next); the machine keeps its four
      const rock = ROCK && row.kick === "four" && !row.tight;
      // played by a hand, and an eager one: the pushes land ahead of the grid, every hit a little off it
      if (rock) for (const bt of (bar % 2 ? [0, 1.5, 2] : [0, 2, 2.5])) kick(tb + bt * BEAT - (bt % 1 ? 0.010 : 0.002) + 0.004 * (rnd() - 0.5), bt % 1 ? 0.85 + 0.15 * rnd() : 1, row.tight);
      const beats = rock ? [] : row.kick === "four" ? [0, 1, 2, 3] : row.kick === "half" ? [0, 2] : bar < 4 ? [0, 2.5] : [0, 1, 2, 3];
      for (const bt of beats) kick(tb + bt * BEAT, row.kick === "heart" && bar < 4 ? 0.8 : 1, row.tight);
    }
    if (ROCK && !row.tight) {
      // snare on two and four, a ghost before four some bars; hats in straight
      // eighths leaning on the beat, the last one open every fourth bar. All of
      // it eager (6-8 ms ahead) and uneven in time and weight, like a player
      // who wants the next bar.
      const lean = () => -0.007 + 0.008 * (rnd() - 0.5), hit = () => 0.82 + 0.36 * rnd();
      if (row.clap && onFrom(row, "clap", bar) && !dropped) {
        for (const bt of [1, 3]) snare(tb + bt * BEAT + lean(), row.clap * hit() * (firstSung || (taxRow(row) && bar >= 6) ? 0.7 : 1));   // the kit leans back under the tax bracket
        // before the first word the snares are wonky and hint at the tune (@jeffrey 2026-11-20: "pitch around the
        // snares in the intro so they are like a bit wonky and like alluding to the melody"): under each backbeat a
        // short donk — the drum's tone — on the tune's note at that beat, an octave down, and never quite on it:
        // off by up to 35 cents, one hit in four by up to a quarter-tone. Its own seeded draw, so the rest of the
        // record's dice are as they were. The time stays the snare's.
        if (!DENSE && row.mel && ri < ROWS.findIndex((r) => r.sing)) for (const bt of [1, 3]) {
          const beat = bar * 4 + bt, note = [...MEL[row.mel]].reverse().find((n) => n[0] <= beat) ?? MEL[row.mel][0];
          const cents = Math.round((wonk() - 0.5) * (wonk() < 0.25 ? 100 : 70));
          donk(tb + bt * BEAT - 0.004, 64 + note[1] - 12 + cents / 100, row.clap * 0.22, bt === 1 ? -0.15 : 0.15);
          wonkLog.push(`${ri * 8 + bar + 1}.${bt + 1} ${noteName(64 + note[1] - 12)}${cents >= 0 ? "+" : ""}${cents}`);
        }
        if (rnd() < 0.4) snare(tb + 2.75 * BEAT + lean(), row.clap * 0.28);
      }
      if (row.hat && onFrom(row, "hat", bar)) for (let e = 0; e < 8; e++) hat(tb + e * BEAT / 2 - 0.005 + 0.010 * (rnd() - 0.5), row.hat * (e % 2 ? 0.6 : 1) * hit() * (firstSung || (taxRow(row) && bar >= 6) ? 0.45 : 1), bar % 4 === 3 && e === 7);
      if (row.shk) for (const p of euclid(5 + 2 * (ri % 3), abs * 5)) shaker(tb + six(p), row.shk * 0.5 * (p % 4 === 2 ? 1 : 0.6), -0.4 + 0.8 * rnd());
    } else if (WALTZ) {
      // the kit waltzes over the straight kick: each two beats is a small bar of
      // three with the second pulse early and the third late (0, 0.60, 1.40).
      // The kick is the oom; the hats are the two pahs, and the clap leaves the
      // backbeat for the late third pulse, so the hesitation is the loudest thing.
      if (row.clap && !dropped) for (const bt of [1.4, 3.4]) clap(tb + bt * BEAT + 0.008, row.clap);
      if (row.hat) for (const bt of [0.6, 1.4, 2.6, 3.4]) hat(tb + bt * BEAT - 0.006, row.hat * (bt % 2 < 1 ? 0.75 : 1) * (0.8 + 0.2 * rnd()), !row.tight && bar % 4 === 3 && bt === 3.4);
      if (row.shk) for (const bt of [0, 0.6, 1.4, 2, 2.6, 3.4]) shaker(tb + bt * BEAT, row.shk * (bt % 2 === 0 ? 1 : 0.6), -0.4 + 0.8 * rnd());
    } else {
      if (row.clap && !dropped) for (const bt of [1, 3]) clap(tb + bt * BEAT + 0.008, row.clap);
      if (row.hat) for (let e = 0; e < 4; e++) hat(tb + (e + 0.5) * BEAT - 0.006, row.hat * (0.8 + 0.2 * rnd()), !row.tight && bar % 4 === 3 && e === 3);
      if (row.shk) for (const p of euclid(5 + 2 * (ri % 3), abs * 5)) shaker(tb + six(p), row.shk * (p % 4 === 2 ? 1 : 0.6), -0.4 + 0.8 * rnd());
    }
    if (row.rim) for (let p = (5 - (abs * 16) % 5) % 5; p < 16; p += 5) tick(tb + six(p), 1750, CFG.gain.rim * row.rim, -0.45);   // left, across from the hats

    // bass
    const bm = c.bass + (c.bass < 36 ? 12 : 0);
    if (row.bass === "bounce" && !dropped) {
      for (const p of [2, 6, 10, 14]) bassNote(tb + six(p), SIX * 1.5, bm, 0.8);
      bassNote(tb + six(7), SIX * 0.8, bm + 12, 0.55);
      if (bar % 2) bassNote(tb + six(15), SIX * 0.8, bm + 12, 0.6);
      if (bar % 4 === 3) bassNote(tb + six(13), SIX * 0.8, bm + 7, 0.5);
    } else if (row.bass === "skel") {
      bassNote(tb + six(2), SIX * 3, bm, 0.75); bassNote(tb + six(10), SIX * 2.5, bm, 0.7);
      if (bar % 2) bassNote(tb + six(7), SIX * 0.8, bm + 12, 0.5);
    } else if (row.bass === "pulse" && !dropped) {
      for (let p = 0; p < 16; p++) if (p % 4) bassNote(tb + six(p), SIX * 0.7, bm + (p % 4 === 2 ? 12 : 0), 0.5 + 0.03 * bar);
    }

    // arp: chord tones an octave up, walking a zigzag that only repeats every three bars
    if (row.arp && onFrom(row, "arp", bar) && bar < (row.until?.arp ?? 8)) {
      const [lv, div] = row.arp, ladder = [...c.notes, c.notes[0] + 12, c.notes[1] + 12].map((m) => m + 12);
      for (let s = 0; s < 4 * div; s++) {
        const walk = (s * 2 + (s % 3) + bar * 3) % ladder.length, pos = s * (4 / div);
        if (div === 4 && s % 8 === 0) continue;       // the kick owns the downbeats
        pluck(tb + six(pos), ladder[walk], lv * (s % div === 0 ? 0.75 : 1) * (0.8 + 0.2 * rnd()),
          Math.sin(abs * 0.9 + s * 0.7) * 0.6, div === 4 ? 0.16 : 0.3);
      }
    }

    // bloops: a walking cursor over the chord, so the drips carry a line
    if (row.bloop) {
      const [lv, k] = row.bloop, tones = [...c.notes.map((m) => m + 12), c.notes[0] + 24, c.notes[1] + 24];
      for (const p of euclid(k, abs * 3 + 1)) {
        bloopCursor += rnd() < 0.72 ? 1 : rnd() < 0.5 ? -2 : 3;
        const m = tones[((bloopCursor % tones.length) + tones.length) % tones.length];
        bloop(tb + six(p), m, lv * (p % 4 === 2 ? 1 : 0.7) * (0.75 + 0.25 * rnd()), -0.75 + 1.5 * rnd(), 0.16 + 0.1 * rnd());
      }
    }

    // seams
    if (building) {
      const n = bar === 6 ? 8 : 16;
      for (let s = 0; s < n; s++) {
        const u = (bar - 6 + s / n) / 2;
        spray(tb + s * (BAR / n), { fc: 1100 + 1400 * u, q: 1.6, dec: 0.045, gain: CFG.gain.clap * row.build * (0.25 + 0.75 * u * u), wet: 0.3 });
      }
    }
  }
  if (row.build) {
    // tuned to where it is going: the next row's first chord, folded into E2-D#3
    const to = (next ? expand(next.block) : chords)[0].notes.slice(0, 3);
    while (to.length < 3) to.push(to[0] + 7);
    riser(t0 + 4 * BAR, 4 * BAR, row.build, to.map((m) => 440 * 2 ** ((40 + ((m - 40) % 12 + 12) % 12 - 69) / 12)));
  }
  // a stone bowl on every row's door, tuned to the first chord's bass
  if (ri > 0) bell(t0, 52 + ((chords[0].bass - 40 + 24) % 12), 0.7, { geometry: "bowl", dur: 9, gain: 0.8, wet: 0.5 });

  // melody
  if (row.mel) {
    const glass = row.mode === "ion" || row.mode === "mix" || row.mode === "dor";
    // jeffrey sings the line under the lead: one whole line per
    // two-bar phrase of the tune, each vowel held to the next note
    if (row.sing) {
      const groups = new Map();
      for (const note of MEL[row.mel]) { const g = Math.floor(note[0] / 8); groups.set(g, [...(groups.get(g) ?? []), note]); }
      for (const [g, group] of groups) {
        if (group.length < 6) continue;             // a short answer phrase stays the synth's
        // a row's four phrases: the title line, a colour of lip liner, the next
        // colour (the soft one, sung warm), and the tax bracket on the ten-note
        // close. The answer row opens on a colour (the last, black). A phrase
        // with fewer than eight notes takes as much of the title line as fits.
        let line = "lead", pin = null;
        if (group.length >= 8 && (g === 1 || g === 2 || (g === 0 && row.mel === "ionAnswer"))) { line = nextColor(); if (g === 2) pin = ["warm"]; }
        else if (g === 3 && group.length >= NSYL.nothing) line = "nothing";
        const notes = roomFor(group, NSYL[line]);
        const b0 = notes[0][0], last = notes[Math.min(notes.length, NSYL[line]) - 1];
        const chordOf = (tt) => chordAt(chords, Math.min(7, Math.max(0, Math.floor((tt - t0) / BAR)))) ?? chords[0];
        // Two characters. The first sung row is the angel (@jeffrey 2026-11-20: "jeffreys initial verse a bit whistley
        // angelic … less robot scary"): in the register of the rest of the singing (an octave up, in a wet room, whisper lost every word of it), sung with the slides and his own
        // movement left in (bin/sing-lines.py "angel": no hard snap, no exciter, a halo and a whistle on his pitch),
        // quiet, a little of it in the room, no octave-down double and no choir — on the tax bracket too.
        // The robot comes later, once ("that can come at some point the more mischievous one"): the row the bomb
        // opens, final a — every line hard on its note, close and driven, with the octave under it.
        const mischief = !DENSE && row.name === "final a";
        if (firstSung) { singLine(line, t0 + b0 * BEAT, notes.map(([bt]) => (bt - b0) * BEAT), notes.map(([, iv]) => 64 + iv - 24), last[2] * BEAT,
          { gain: CFG.gain.sung * 1.3, pan: 0, wet: 0.18, echo: 0.04, mode: "angel", ...(pin ? { pin } : {}) }); continue; }
        if (mischief && line !== "nothing") singLine(line, t0 + b0 * BEAT, notes.map(([bt]) => (bt - b0) * BEAT), notes.map(([, iv]) => 64 + iv - 36), last[2] * BEAT,
          { gain: CFG.gain.sung * 0.5, pan: 0, wet: 0.03, echo: 0, mode: "close", ...(pin ? { pin } : {}) });
        if (line === "nothing") taxLine(t0 + b0 * BEAT, notes.map(([bt]) => (bt - b0) * BEAT), notes.map(([, iv]) => 64 + iv - 24), last[2] * BEAT, chordOf);
        else
        // two octaves under the lead: jeffrey's own speaking range, so the pitch moves a little, not a lot.
        // Close and dry, on top of the track; every note is its written length.
        singLine(line, t0 + b0 * BEAT, notes.map(([bt]) => (bt - b0) * BEAT), notes.map(([, iv]) => 64 + iv - 24), last[2] * BEAT,
          { gain: CFG.gain.sung * 1.6, pan: 0, wet: 0.04, echo: 0, mode: mischief ? "close" : "clear", ...(pin ? { pin } : {}) });   // every sung line "clear" (bin/sing-lines.py) and nearly dry; the first round a decibel over its old level (whisper heard none of its words at that level; the band steps back instead — hats, snare, acid, pad, bells — as it does under the tax bracket), later ones 4 dB up
        // on a hold, the vowel is also thrown up the chord in eighths: the
        // pitch-shifted voice, small and wet, one ear then the other
        if (last[2] >= 1.5) {
          const bar = Math.min(7, Math.floor(last[0] / 4));
          const c = chords.find((x) => bar >= x.at && bar < x.at + x.bars) ?? chords[0];
          const tones = c.notes.filter((m) => m >= 55 && m <= 74);
          for (let k = 0; tones.length && k < Math.floor(last[2] * 2); k++)
            sung.push({ t: t0 + (last[0] + 0.5 + k * 0.5) * BEAT, syl: "ah", midi: tones[k % tones.length], dur: 0.25, hold: 0.22,
              gain: CFG.gain.sung * 0.3 * (1 - 0.1 * k), pan: k % 2 ? 0.6 : -0.6, attack: 0.01, wet: 0.5, echo: 0.3 });
        }
      }
    }
    for (const [beat, iv, beats, vel0] of MEL[row.mel]) {
      const t = t0 + beat * BEAT, d = beats * BEAT, m = 64 + iv;
      const vel = vel0 * (taxRow(row) && beat >= 24 ? 0.55 : 1) * (firstSung ? 0.5 : 1);   // the tune steps back under the tax bracket
      if (row.voice !== "bells") {
        lead(t, d, m, vel * (row.sing ? 0.5 : 1));   // the synth steps back where jeffrey sings the line
        if (row.low) lead(t, d, m - 12, vel * 0.5, CFG.gain.lead, -0.2);
        if (row.harm && scale.length === 7) {
          const deg = scale.indexOf(((iv % 12) + 12) % 12);
          if (deg >= 0) {
            const lower = deg - 5, oct = Math.floor(lower / 7);
            lead(t, d, 64 + Math.floor(iv / 12) * 12 + scale[((lower % 7) + 7) % 7] + oct * 12, vel, CFG.gain.harm, 0.3);
          }
        }
      }
      if (row.voice !== "synth") {
        bell(t, m, vel, { material: glass ? "glass" : "stone", geometry: glass ? "glass" : "handbell",
          dur: beats >= 2 ? 5 : 2.5, gain: row.voice === "both" ? 0.6 : 1, pan: Math.sin(beat * 1.3) * 0.35 });
      }
    }
  }

  // the solo. Bars 1-4: the title line and the lip liner whispered on the tune,
  // with almost nothing under them. Bars 5-8: the voice loud and alone on the tune's last two phrases,
  // the title line then the tax bracket.
  if (row.solo) {
    const groups = new Map();
    for (const note of MEL.hookIon) { const g = Math.floor(note[0] / 8); groups.set(g, [...(groups.get(g) ?? []), note]); }
    // whispered, but on the tune and at its length: each line takes its own two
    // bars of the hook, a syllable a note, breath stretched and given the pitch
    for (const [g, line] of [[0, "lead"], [1, nextColor()]]) {
      const notes = roomFor(groups.get(g), NSYL[line]), b0 = notes[0][0], last = notes[Math.min(notes.length, NSYL[line]) - 1];
      singLine(line, t0 + b0 * BEAT, notes.map(([bt]) => (bt - b0) * BEAT), notes.map(([, iv]) => 64 + iv - 24), last[2] * BEAT,
        { gain: CFG.gain.sung * 1.5, pan: 0, wet: 0.2, echo: 0.15, pin: ["whisper"], mode: "whisper" });
    }
    for (const [g, line] of [[2, "lead"], [3, "nothing"]]) {
      const notes = groups.get(g), b0 = notes[0][0], last = notes[Math.min(notes.length, NSYL[line]) - 1];
      // an octave over the rest of the song's singing: a raised voice sits higher, and down
      // in the speaking range the loud takes went rough and lost their pitch (measured 2026-11-20)
      // the tax bracket: the loud take alone read "make a text bracket", so the plain cheeky voice sings it
      // an octave under (the song's own register, where it reads whole) and again an octave under that
      if (line === "nothing") taxLine(t0 + b0 * BEAT, notes.map(([bt]) => (bt - b0) * BEAT), notes.map(([, iv]) => 64 + iv - 12), last[2] * BEAT,
        (tt) => chordAt(chords, Math.min(7, Math.max(0, Math.floor((tt - t0) / BAR)))) ?? chords[0],
        { gain: CFG.gain.sung * 1.2, pin: ["shout", "loud"], lows: [[12, 1.3], [24, 0.45]], choir: 0.6 });   // the plain voice an octave under carries the words; the loud one is the colour
      else singLine(line, t0 + b0 * BEAT, notes.map(([bt]) => (bt - b0) * BEAT), notes.map(([, iv]) => 64 + iv - 12), last[2] * BEAT,
        { gain: CFG.gain.sung * 1.5, pan: 0, wet: 0.1, echo: 0.1, pin: ["shout", "loud"] });
    }
  }

  // the second line, stretched: a syllable every half note, so the line takes
  // four bars and goes round twice a row, on the chord, swapping ears.
  if (row.back) {
    for (const lap of [0, 1]) {
      const starts = [], midis = [];
      for (let k = 0; k < 8; k++) {
        const bar = lap * 4 + Math.floor(k / 2);
        if (row.drop && bar === 7) break;
        const c = chords.find((x) => bar >= x.at && bar < x.at + x.bars) ?? chords[0];
        const tones = c.notes.filter((m) => m >= 50 && m <= 66);
        if (!tones.length) break;
        const m = tones[[0, 0, 1, 1, 0, 0, 1, 1][k] % tones.length] - 12;
        starts.push(k * 2 * BEAT); midis.push(m < 43 ? m + 12 : m);   // an octave down, kept above G2
      }
      singLine("back", t0 + lap * 4 * BAR, starts, midis, 2 * BEAT - 0.04, { gain: row.back * CFG.gain.back, pan: lap ? 0.5 : -0.5, wet: 0.1, echo: 0.05, mode: "clear" });
    }
  }
  if (!DENSE && !RAP_VOCAL && row.rap === 1) {
    // The rainbow chord (@jeffrey 2026-11-20: "i should just sing all the colors in a solo voice! … arpeggiated …
    // just colors sung … not playing the whole phrase! … but extending the colors into one another in choral
    // harmony … that whole exploration can be instead of the rap"). Where the rap was, over the two rows of
    // piano: jeffrey alone says each colour once — reddest, orangest, yellowest, greenest, bluest, purplest,
    // pinkest, blackest — a new one every bar and a half, each on the next note up an E(add9) arpeggio
    // (E2 B2 E3 G♯3 B3 E4 F♯4 G♯4), and each word's last vowel is held on its note until the second row's
    // last bar. So nothing is said twice and no words overlap; the held vowels pile up into the chord,
    // complete from the eleventh bar, and let go together where the band drops out. The words are cut from
    // the lip liner takes ("put the REDDEST lip liner on": sung as far as the colour, the clip starting at
    // the colour); the held tail is the same word sung again with its last vowel long, heard only from
    // a quarter of a second into that vowel, quieter, out to the sides and wetter than the word.
    COLORS.forEach((col, k) => {
      const line = col === "pink" ? "lip" : `lip-${col}`, nw = NSYL[line] - 6, note = [40, 47, 52, 56, 59, 64, 66, 68][k];
      const tk = t0 + k * 1.5 * BAR - 0.5 * BEAT, starts = [0, 0.25 * BEAT, ...Array.from({ length: nw }, (_, i) => (0.5 + i * 0.75) * BEAT)], midis = starts.map(() => note);
      const release = t0 + 15 * BAR - (tk + starts[starts.length - 1]);
      singLine(line, tk, starts, midis, 1.2 * BEAT, { gain: CFG.gain.sung * 1.7, pan: 0, wet: 0.06, echo: 0.04, mode: "clear", pin: ["cheeky"], cut: 2 });
      singLine(line, tk, starts, midis, release, { gain: CFG.gain.sung * 0.4, pan: (k % 2 ? 1 : -1) * (0.25 + 0.06 * k), wet: 0.45, echo: 0, pin: ["cheeky"], cut: starts.length - 1, cutPlus: 0.25 });
    });
  }
  if (row.gliss && !DENSE) {
    // the arpeggio: sixteenths up the E chord's pentatonic, an octave higher every half bar, brighter as it goes,
    // from the end of the stamp to the row's last beat (the bell run crosses it)
    const pent = [64, 68, 71, 76, 80];   // the same climb the voice just sang, carried on
    for (let s16 = 0; s16 < 24; s16++) {
      const oct = Math.floor(s16 / 8), m = pent[s16 % pent.length] + 12 * oct;
      pluck(t0 + 6.5 * BAR + s16 * SIX, m > 88 ? m - 12 : m, 0.5 + 0.02 * s16, Math.sin(s16 * 0.7) * 0.6, 0.14 + 0.004 * s16);
    }
  }
  if (row.gliss) {
    // stone club's door: two octaves of the next row's scale on stone bells, in the last two beats, left to right
    const sc = SCALES[next?.mode ?? "ion"], run = [0, 1].flatMap((o) => sc.map((iv) => 52 + 12 * o + iv)).concat(76), step = 2 * BEAT / run.length;
    run.forEach((m, i) => bell(t0 + 8 * BAR - 2 * BEAT + i * step, m, 0.55 + 0.45 * i / run.length, { material: "stone", geometry: "handbell", dur: 1.2, gain: 0.8, pan: -0.5 + i / run.length, wet: 0.3 }));
  }
  if (row.last) {
    bell(t0 + 6 * BAR, 64, 0.8, { material: "glass", geometry: "glass", dur: 9, wet: 0.7 });
  }
  progress.update(Math.round(60 * (ri + 1) / ROWS.length));
});

// one-offs, placed by row name so they follow the cut
const rowAt = (...names) => { const i = ROWS.findIndex((r) => names.includes(r.name)); return i < 0 ? null : i * 8 * BAR; };
for (const [names, bar, dir] of [[["verse 2"], 3, 1], [["verse 2'"], 3, -1], [["bridge", "bridge'"], 1, -1], [["outro", "outro'"], 2, 1]]) {
  const t = rowAt(...names);
  if (t != null) flyby(t + bar * BAR, dir);
}

// The gold gong (@jeffrey 2026-11-20: "a huge deeper fem gong bell … like GOLD simulation where it rings
// out for lonngggg time … and tempers the track"). pop/bell's finite-element shell, in gold (E 79 GPa,
// ρ 19300 kg/m³, ν 0.42, loss 9e-4) at the church-bell proportions, tuned so its strike note is E2 and
// rung for forty seconds. One strike opens the record and one stands on the door of the outro. The
// second tempers the track: everything that is not a bell falls back as it strikes and comes back up
// out of the ring (`temper`: depth, then the seconds it takes to return).
const TEMPERS = [];
function gong(t, gain = 1, wet = 0.5, damp = 0) {
  const key = `${GONG.note}-${GONG.material}-${GONG.geometry}-${GONG.ring}-glide${GONG.glide}-${GONG.settle}`, file0 = resolve(BELL_CACHE, `${key}.f32`);
  // damp: a hand on the bell after that many seconds (the ring falls away over a third of a second)
  const file = damp ? resolve(BELL_CACHE, `${key}-damp${Math.round(damp * 100)}.f32`) : file0;
  if (damp && existsSync(file0) && !existsSync(file)) {
    const raw = readFileSync(file0), src = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2), h = src.length >> 1, n = Math.min(h, Math.ceil((damp + 1.2) * SR)), out = new Float32Array(n * 2);
    for (let k = 0; k < n; k++) { const e = k / SR < damp ? 1 : Math.exp(-(k / SR - damp) / 0.11); out[k] = src[k] * e; out[n + k] = src[h + k] * e; }
    writeFileSync(file, Buffer.from(out.buffer));
  }
  if (!existsSync(file0)) {
    const r = renderBell({ note: GONG.note, material: GONG.material, geometry: GONG.geometry, dur: GONG.ring, vel: 0.9 });
    let pk = 0;
    for (let i = 0; i < r.L.length; i++) pk = Math.max(pk, Math.abs(r.L[i]), Math.abs(r.R[i]));
    // the glide: the solved ring is read at 1 + k·e^(−t/settle) times its speed (every mode sharp by the same
    // ratio — in the metal each mode shifts by its own amount; the depth and time here are chosen, not solved)
    const len = r.L.length, k = 2 ** (GONG.glide / 1200) - 1, all = new Float32Array(len * 2);
    for (let i = 0; i < len; i++) {
      const t = i / SR, pos = i + GONG.settle * k * (1 - Math.exp(-t / GONG.settle)) * SR, q = Math.floor(pos), f = pos - q;
      if (q + 1 >= len) break;
      all[i] = (r.L[q] + (r.L[q + 1] - r.L[q]) * f) / (pk || 1); all[len + i] = (r.R[q] + (r.R[q + 1] - r.R[q]) * f) / (pk || 1);
    }
    writeFileSync(file0, Buffer.from(all.buffer));
    console.log(`bell ${key}`);
    if (damp) return gong(t, gain, wet, damp);
  }
  const raw = readFileSync(file), all = new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2), h = all.length >> 1;
  const i0 = Math.round(t * SR), n = Math.min(h, Math.floor(GONG.ring * SR)), fade = Math.floor(0.08 * SR), a = gain * 0.3;
  ev("bell", BAKE && bakeSmp(file), t, GONG.ring, a, 0, wet);
  for (let k = 0; k < n && i0 + k < N; k++) {
    const e = k > n - fade ? (n - k) / fade : 1, j = (i0 + k) * 2, l = all[k] * a * e, r = all[h + k] * a * e;
    bells[j] += l; bells[j + 1] += r; rev[j] += l * wet; rev[j + 1] += r * wet;
  }
}
const END = BARS * BAR;   // the last bar line
GONG_AT.forEach((t, k) => { gong(t, k ? 1.3 : 1.5, 0.5, k && !DENSE ? END - t - 0.9 : 0); if (k) TEMPERS.push({ t, depth: 0.6, back: 6 }); });
if (!DENSE) {
  // the close: E major under the last two bars, the bells on its third and fifth, then the whoosh —
  // the track's own riser, 0.7 s long, up the E chord, and five bloops across it from left to right
  [52, 56, 59, 64].forEach((m, i) => padNote(END - 2 * BAR + i * 0.03, 2 * BAR - 0.5, m, 0.8 * CFG.gain.pad, (i - 1.5) * 0.32, 0.8));
  bell(END - 2 * BAR, 68, 0.6, { material: "glass", geometry: "glass", dur: 4, pan: -0.3 }); bell(END - BAR, 71, 0.55, { material: "glass", geometry: "glass", dur: 3, pan: 0.3 });
  riser(END - 0.7, 0.7, 0.45, [76, 80, 83].map(mtof));
  [76, 80, 83, 88, 92].forEach((m, i) => bloop(END - 0.42 + i * 0.085, m, 0.7 - 0.06 * i, -0.8 + 0.4 * i, 0.12));
}
if (wonkLog.length) console.log(`wonky snares (bar.beat note±cents): ${wonkLog.join(" · ")}`);
console.log(`gong at ${GONG_AT.map(clock).join(", ")} · lean at most (cents): ${Object.entries(leanLog).map(([k, v]) => `${k} ${v.toFixed(0)}`).join(" · ")}`);

// Every sung note is one syllable from the phoneme library, pitched and
// lengthened by sing-phonemes.py (WORLD) and kept on disk by key.
if (sung.length || phrases.length || rawTakes.length) {
  const cache = resolve(OUT, ".cutie-vox-cache"), jobsFile = resolve(OUT, ".cutie-vox-jobs.json");
  // whole lines first: sung by sing-lines.py, then placed like any other sung note
  if (phrases.length) {
    for (const ph of phrases) ph.key = "line-" + createHash("sha1").update(JSON.stringify(["v6", ph.mode, ph.pin ?? PIN[ph.line] ?? null, ph.line, LINES[ph.line], ph.turn, ph.starts, ph.midis, ph.end_hold])).digest("hex").slice(0, 12);
    const lineJobs = resolve(OUT, ".cutie-line-jobs.json");
    writeFileSync(lineJobs, JSON.stringify({ cache, sr: SR, lines: LINES, pin: PIN,
      jobs: [...new Map(phrases.map((ph) => [ph.key, { key: ph.key, line: ph.line, turn: ph.turn, ...(ph.pin ? { pin: ph.pin } : {}), ...(ph.mode ? { mode: ph.mode } : {}), starts: ph.starts, midis: ph.midis, end_hold: ph.end_hold }])).values()] }));
    const r = spawnSync(PYTHON, [resolve(HERE, "sing-lines.py"), lineJobs], { stdio: "inherit" });
    if (r.status !== 0) throw new Error("sing-lines failed");
    for (const ph of phrases) {
      const pre = JSON.parse(readFileSync(resolve(cache, `${ph.key}.json`), "utf8")).pre;
      const secs = statSync(resolve(cache, `${ph.key}.f32`)).size / 4 / SR;
      // 20 ms early: a voiced glide or nasal (w, y, l, n) opens its syllable, and the vowel is what should meet the note
      if (ph.cut) {   // only the line from its `cut`-th syllable on (the colour word): a clip of its own, starting there
        const plus = ph.cutPlus ?? -0.03, from = Math.max(0, Math.round((pre + ph.starts[ph.cut] + plus) * SR)), ck = `${ph.key}-from${ph.cut}${ph.cutPlus ? "p" : ""}`, cf = resolve(cache, `${ck}.f32`);
        if (!existsSync(cf)) {
          const rawc = readFileSync(resolve(cache, `${ph.key}.f32`)), x = new Float32Array(rawc.buffer, rawc.byteOffset, rawc.byteLength >> 2), piece = Float32Array.from(x.subarray(from));
          const fi = ph.cutPlus ? 9600 : 480;   // a held tail comes in over a fifth of a second
          for (let i = 0; i < Math.min(fi, piece.length); i++) piece[i] *= i / fi;
          writeFileSync(cf, Buffer.from(piece.buffer));
        }
        sung.push({ key: ck, t: ph.t + ph.starts[ph.cut] + plus, hold: secs - from / SR, gain: ph.gain, pan: ph.pan, attack: 0, wet: ph.wet, echo: ph.echo });
        continue;
      }
      sung.push({ key: ph.key, t: ph.t - pre - 0.02, hold: secs, gain: ph.gain, pan: ph.pan, attack: 0, wet: ph.wet, echo: ph.echo });
    }
  }
  // The screams (@jeffrey 2026-11-20: "and lets add some screaming lyrics?" … "are the screams elongated
  // and aesthetivoxed and musically placed"): the song's own lines in the frothyscreama chaos delivery
  // (bin/cutie-screams.mjs), a few of them, out to the sides, where the band is at full level and never
  // over the tax bracket or the voice alone. Each scream is SUNG: the take is cut into its syllables
  // (bin/scream-syllables.py), every syllable is put on a beat of the phrase it echoes, its vowel is held
  // to the length of its note and its pitch replaced with that note (frothyscreama's aesthetivox-scream,
  // wild mode: the consonants stay as screamed, the rasp stays in the note). Where the treatment would
  // tame a syllable the tool keeps the take's own breath under the tuned copy instead.
  // [row, bar within the row, take, gain, pan, the phrase]. The phrase is either { tune: g } — the row's
  // own tune, its g-th two-bar phrase, at the synth's pitch — or { syl: [[beat, beats, midi], …] }, each
  // midi moved to the nearest tone of the chord under it. --no-scream leaves them out.
  const SCREAM_PLAN = process.argv.includes("--no-scream") ? [] : [
    ["chorus 2'", 0, "title-b", 0.5, -0.6, { tune: 0, syl: [[0, 1, 71], [1, 0.5, 71], [1.5, 1, 74], [2.5, 0.5, 71], [3, 1, 76], [4, 1, 74], [5, 1, 71], [6, 2, 69]] }],                                                              // the title on the hook, beside the sung title line (it opened the first chorus until that row went instrumental)
    // the machine's two answer the commit rap at its phrase ends: do NO-thing in the gap after the first
    // phrase (the first row's bars 6-8), break a TAAX brack-et in the bar the band drops out of (the second row's bar 8)
    // (do NOOO-thing, twice, answered the rap from the first row's bar 6.5; with the colours sung solo there it is left out)
    ["machine'", 7.25, "tax-a", 0.65, 0.5, { syl: [[0, 0.5, 69], [0.5, 0.25, 71], [0.75, 1, 76], [1.75, 0.5, 74], [2.25, 0.75, 71]] }],
    ...["final b", "final b'"].map((r) => [r, 4, "better-a", 0.55, -0.6, { syl: [[0, 1, 69], [1, 1, 72], [2, 1, 69], [3, 1, 72], [4, 1, 69], [5, 1, 72], [6, 1, 71], [7, 1, 69]] }]),   // the chant's line, a syllable a beat
    ["final b'", 7, "cutie-a", 0.5, 0.7, { syl: [[0, 1, 72], [1, 1, 69], [2, 0.5, 72], [2.5, 1.5, 76]] }],            // cu-tie cu-TIEEE, let go on the gong's bar line
  ];
  {
    const jobs = [], placed = [];
    for (const [name, bar, id, gain, pan, phrase] of SCREAM_PLAN) {
      const ri = ROWS.findIndex((r) => r.name === name);
      if (ri < 0) continue;
      const row = ROWS[ri], t0 = ri * 8 * BAR, chords = expand(row.block);
      const mp3 = resolve(PHONEMES, "phrases", `cutie-scream-${id}.mp3`), meta = resolve(PHONEMES, "phrases", `cutie-scream-${id}.syl.json`), wav = resolve(cache, `scream-${id}.wav`);
      if (!existsSync(mp3) || !existsSync(meta)) throw new Error(`no scream take ${id} (or its syllables) — run bin/cutie-screams.mjs, then bin/scream-syllables.py`);
      if (!existsSync(wav) && spawnSync(FFMPEG, ["-v", "error", "-y", "-i", mp3, "-ac", "1", "-ar", String(SR), wav]).status !== 0) throw new Error(`could not decode scream ${id}`);
      const syls = JSON.parse(readFileSync(meta, "utf8")).syllables;
      let notes;   // [beat from the bar, beats, midi]
      const tuneNotes = phrase.tune != null && (phrase.tuneOf || row.mel) ? MEL[phrase.tuneOf ?? row.mel].filter((n) => Math.floor(n[0] / 8) === phrase.tune) : [];
      if (tuneNotes.length >= syls.length) {   // the tune has a note for every syllable; otherwise the written phrase is used
        const group = tuneNotes.slice(0, syls.length), b0 = group[0][0];
        notes = group.map(([bt, iv, len], i) => [bt - b0, i + 1 < group.length ? group[i + 1][0] - bt : len, 64 + iv]);
      } else notes = phrase.syl.map(([bt, len, m]) => {
        const c = chordAt(chords, Math.min(7, Math.floor(bar + bt / 4))) ?? chords[0];
        let best = c.notes[0]; for (const x of c.notes) for (let o = -24; o <= 36; o += 12) if (Math.abs(x + o - m) < Math.abs(best - m)) best = x + o;
        return [bt, len, best];
      });
      const n = Math.min(notes.length, syls.length), tb = t0 + bar * BAR, p = { name, bar, id, onTune: tuneNotes.length >= syls.length, t: tb, raw: syls[syls.length - 1][1] - syls[0][0], sung: (notes[n - 1][0] + notes[n - 1][1]) * BEAT, parts: [] };
      for (let k = 0; k < n; k++) {
        const [bt, len, m] = notes[k], [a, b] = syls[k], piece = resolve(cache, `scream-${id}-s${k}p.wav`);   // padded to 0.3 s: the treatment needs 41 frames
        if (!existsSync(piece) && spawnSync(FFMPEG, ["-v", "error", "-y", "-ss", String(Math.max(0, a - 0.015)), "-t", String(b - a + 0.015), "-i", wav, "-af", "apad=whole_dur=0.3", piece]).status !== 0) throw new Error(`could not cut scream ${id} syllable ${k}`);
        const dur = Math.round((len * BEAT + 0.04) * 1000) / 1000;
        const key = `scream-${id}-s${k}-avox-${createHash("sha1").update(JSON.stringify([id, k, a, b, m, dur, "sung-2"])).digest("hex").slice(0, 10)}`;
        jobs.push({ in: piece, out: resolve(cache, `${key}.f32`), dur, step: SIX, transpose: 0, segs: [[0, [m]]], mode: "wild", nat: 0.35, halo: 0.2 });
        p.parts.push({ key, t: tb + bt * BEAT, hold: len * BEAT, m, beat: bt, beats: len, gain, pan });
      }
      placed.push(p);
    }
    if (jobs.length) {
      const jf = resolve(OUT, ".cutie-scream-jobs.json");
      writeFileSync(jf, JSON.stringify({ sr: SR, jobs }));
      const r = spawnSync(PYTHON, [resolve(HERE, "../../frothyscreama/bin/aesthetivox-scream.py"), jf], { stdio: ["ignore", "ignore", "inherit"] });
      if (r.status !== 0) throw new Error("aesthetivox-scream failed");
      for (const p of placed) {
        for (const q of p.parts) {
          if (!existsSync(resolve(cache, `${q.key}.f32`))) throw new Error(`sung scream syllable missing: ${q.key}`);
          q.choice = JSON.parse(readFileSync(resolve(cache, `${q.key}.f32.json`), "utf8")).choice;
          sung.push({ key: q.key, t: q.t - 0.015, hold: q.hold, gain: q.gain, pan: q.pan, attack: 0, wet: 0.25, echo: 0.12 });
        }
        console.log(`scream ${p.name} bar ${p.bar} (${clock(p.t)}) ${p.id} · ${p.onTune ? "on the tune" : "written"} · ${p.raw.toFixed(2)} s screamed → ${p.sung.toFixed(2)} s sung · ` + p.parts.map((q) => `${q.beat}+${q.beats}:${noteName(q.m)}${q.choice === "full" ? "" : "(" + q.choice + ")"}`).join(" "));
      }
      writeFileSync(resolve(OUT, ".cutie-scream-placed.json"), JSON.stringify(placed, null, 1));
    }
  }
  // The commit rap and its grand piano, in the machine row (bin/cutie-rap.mjs has the words and the takes).
  // Each phrase is one take, chanted by rapvox and cut into
  // its words; every word starts on a sixteenth (the one nearest where it fell). The takes are NOT sped up:
  // each phrase has a row of its own (the machine, then machine') and runs about five bars of it at the
  // speed it was rapped, a scream answering in the bars left.
  // The piano is the AC grand (Salamander V3, fedac/native/samples/piano — the one aesthetina plays), baked
  // a note at a time into the cache and played as clips, so both engines read the same samples. Low and
  // middle register only: E2+B2 on one, an Em7 stab on the and of two, an Esus stab on the and of three,
  // a B pickup; in the bars where a scream answers it walks B-D-E-G up to the next bar instead.
  {
    const rapLog = [];
    for (const [ri, row] of DENSE ? [] : [...ROWS.entries()].filter(([, r]) => r.rap)) {
      const t0 = ri * 8 * BAR, rapDir = resolve(PHONEMES, "phrases");
      const pianoNote = (t, midi, dur, vel, pan = 0) => {
        const key = `piano-${midi}-${Math.round(dur * 1000)}`, file = resolve(cache, `${key}.f32`);
        if (!existsSync(file)) {
          const anchor = Math.min(96, Math.max(21, 21 + 3 * Math.round((midi - 21) / 3)));
          const src = resolve(HERE, "../../../fedac/native/samples/piano", `${anchor}.raw`);
          if (!existsSync(src)) return;
          const b = readFileSync(src), frames = b.length / 4, rate = 2 ** ((midi - anchor) / 12), n = Math.min(Math.floor((frames - 2) / rate), Math.floor((dur + 0.9) * SR)), out = new Float32Array(n);
          let pk = 1e-9;
          for (let k = 0; k < n; k++) {
            const x = k * rate, j = Math.floor(x), fr = x - j, sT = k / SR;
            const v = ((b.readInt16LE(j * 4) + b.readInt16LE(j * 4 + 2)) * (1 - fr) + (b.readInt16LE(j * 4 + 4) + b.readInt16LE(j * 4 + 6)) * fr) / 65536;
            out[k] = v * (sT < dur ? 1 : Math.exp(-(sT - dur) / 0.18)); pk = Math.max(pk, Math.abs(out[k]));
          }
          for (let k = 0; k < n; k++) out[k] /= pk;
          writeFileSync(file, Buffer.from(out.buffer));
        }
        sung.push({ key, t, hold: dur + 0.8, gain: 0.3 * vel, pan, attack: 0, wet: 0.12, echo: 0 });
      };
      for (let bar = 0; bar < 8; bar++) {
        const tb = t0 + bar * BAR, answer = row.drop ? bar === 6 : bar === 5 || bar === 7;
        if (bar === 7 && row.drop) { [40, 52].forEach((m) => pianoNote(tb, m, 3.5 * BEAT, 0.8, -0.1)); continue; }   // the drop: one low E, left to ring under the scream
        pianoNote(tb, 40, 1.4 * BEAT, 0.9, -0.15); pianoNote(tb, 47, 1.4 * BEAT, 0.7, -0.15);
        [52, 55, 59, 62].forEach((m, j) => pianoNote(tb + 1.5 * BEAT + j * 0.006, m, 0.45 * BEAT, 0.62, 0.15));
        if (answer) [47, 50, 52, 55].forEach((m, j) => pianoNote(tb + (3 + j / 4) * BEAT, m, 0.3 * BEAT, 0.6 + 0.06 * j, -0.1 + 0.08 * j));
        else { [52, 57, 59, 62].forEach((m, j) => pianoNote(tb + 2.5 * BEAT + j * 0.006, m, 0.45 * BEAT, 0.58, 0.15)); pianoNote(tb + 3.5 * BEAT, 47, 0.4 * BEAT, 0.6, -0.15); }
      }
      for (const [n, bar] of RAP_VOCAL ? [[row.rap - 1, 0]] : []) {
        const meta = resolve(rapDir, `cutie-rap-${n}.json`), mp3 = resolve(rapDir, `cutie-rap-${n}.mp3`);
        if (!existsSync(meta)) { console.log(`(no commit rap take ${n}: run bin/cutie-rap.mjs)`); continue; }
        const words = JSON.parse(readFileSync(meta, "utf8")).words, a0 = words[0].a, f = 1, n16 = Math.ceil((words[words.length - 1].b - a0) / SIX);   // f = 1: the take at the speed it was rapped; only whole words move, to the nearest sixteenth
        // rapvox (pop/bin/rapvox.py; @jeffrey: "the rap could be aesthetivoxed … lets do a rapvox"): the take fitted
        // to its bars and chanted — B2, D3 on every fourth word and before a full stop, E3 on the word that ends a
        // sentence — at strength 0.8, the last word's vowel held 0.3 s; its guard lowers the strength if whisper
        // loses more than a tenth of the words
        const vox = resolve(cache, `rapvox-${n}-${createHash("sha1").update(JSON.stringify([words, "natural", "rapvox-2"])).digest("hex").slice(0, 10)}.f32`);
        if (!existsSync(vox)) {
          const r = spawnSync(PYTHON, [resolve(HERE, "../../bin/rapvox.py"), "--take", mp3, "--words", meta, "--notes", "47,50,52", "--strength", "0.8", "--hold", "0.3", "--out", vox], { stdio: "inherit" });
          if (r.status !== 0) throw new Error("rapvox failed");
        }
        const rawv = readFileSync(vox), x = new Float32Array(rawv.buffer, rawv.byteOffset, rawv.byteLength >> 2);
        let pk = 1e-9; for (const v of x) pk = Math.max(pk, Math.abs(v));
        let prev = -1;
        words.forEach((w, i) => {
          const pos = Math.max(prev + 1, Math.round((w.a - a0) * f / SIX)); prev = pos;
          const from = Math.max(0, Math.round((w.a * f - 0.012) * SR)), to = i + 1 === words.length ? x.length : Math.min(x.length, Math.round((Math.min(w.b * f + 0.03, i + 1 < words.length ? words[i + 1].a * f + 0.012 : 1e9)) * SR));
          const key = `rap-${n}-${i}-${createHash("sha1").update(JSON.stringify([w, f, "rapvox-2"])).digest("hex").slice(0, 8)}`, file = resolve(cache, `${key}.f32`);
          if (!existsSync(file)) {
            const m = to - from, piece = new Float32Array(m + Math.floor(0.11 * SR)), fade = Math.min(240, m >> 1);   // silence after the word: the clip player fades a clip's last 100 ms
            for (let k = 0; k < m; k++) piece[k] = x[from + k] / pk * Math.min(1, k / fade, (m - 1 - k) / fade);
            writeFileSync(file, Buffer.from(piece.buffer));
          }
          sung.push({ key, t: t0 + bar * BAR + pos * SIX - 0.012, hold: (to - from) / SR + 0.1, gain: CFG.gain.sung * 2.1, pan: 0, attack: 0, wet: 0.03, echo: 0 });
          rapLog.push(`${w.w}@${(bar + pos / 16 + 1).toFixed(2)}`);
        });
        console.log(`rap ${n}: ${words.length} words from bar 1 of ${row.name} (${clock(t0)}), at its own speed, ${n16} sixteenths (${(n16 / 16).toFixed(1)} bars)`);
      }
    }
    if (rapLog.length) writeFileSync(resolve(OUT, ".cutie-rap-placed.json"), JSON.stringify({ words: rapLog }));
  }
  // Two voices from outside the song, once each (@jeffrey 2026-11-20: "lets add aesthetic dot computer
  // stamp" / "and a prutti cameo"). Both are takes already on disk, cut and filtered into the cache and
  // then played like any spoken take.
  //   the stamp: assets/aesthetic-dot-computer.wav (the catalogue's voice ID), through a telephone band,
  //     quiet, on the stone row's build — on the stone row, before the arpeggio, the bell run and the bomb.
  //   the cameo: Prutti's own typed words "you are a super star" (the second half of his Pruttivox take
  //     amail-clear, from aesthetina), spoken, in the outro once the gong has rung four bars: his answer
  //     to "what a jetter cutie you are". DRAFT like his aesthetina lines: the clone needs his approval.
  // --dense leaves both out.
  if (!DENSE) {
    const shot = (slug, src, pre) => {
      const file = resolve(cache, `raw-${slug}.f32`);
      if (existsSync(file)) return true;
      if (!existsSync(src)) { console.log(`(no ${src}: ${slug} left out)`); return false; }
      const r = spawnSync(FFMPEG, ["-v", "error", ...pre, "-f", "f32le", "-ac", "1", "-ar", String(SR), "pipe:1"], { maxBuffer: 1 << 28 });
      if (r.status !== 0) throw new Error(`could not make ${slug}`);
      const x = new Float32Array(r.stdout.buffer, r.stdout.byteOffset, r.stdout.byteLength >> 2);
      let pk = 1e-9; for (const v of x) pk = Math.max(pk, Math.abs(v));
      writeFileSync(file, Buffer.from(Float32Array.from(x, (v) => v / pk).buffer));
      return true;
    };
    const stampAt = rowAt("club'"), stampSrc = resolve(HERE, "../assets/aesthetic-dot-computer.wav");
    if (stampAt != null && shot("ac-stamp-phone", stampSrc, ["-i", stampSrc, "-af", "highpass=f=420,highpass=f=420,lowpass=f=3200,lowpass=f=3200"])) {
      rawTakes.push({ slug: "ac-stamp-phone", t: stampAt + 6 * BAR, gain: 0.34, pan: 0.35, wet: 0.45, echo: 0.3 });   // after the rainbow; the arpeggio starts as it ends
      console.log(`stamp "aesthetic dot computer" at ${clock(stampAt + 6 * BAR)} (club' bar 7)`);
    }
    const outs = ROWS.map((r, i) => (r.name === "outro'" || r.name === "outro" ? i : -1)).filter((i) => i >= 0), cameoAt = outs.length ? outs[outs.length - 1] * 8 * BAR + 4 * BAR : null;
    const pm = resolve(PHONEMES, "../prutti-phonemes/phrases/amail-clear.json"), pmp3 = pm.replace(/\.json$/, ".mp3");
    if (cameoAt != null && existsSync(pm)) {
      const syl = JSON.parse(readFileSync(pm, "utf8")).syllables, a = syl[4][0] - 0.03, b = syl[syl.length - 1][1] + 0.12;
      // the same Prutti as aesthetina's (@jeffrey: "not angelic enough needs some more reverb"): less chest, more
      // air, quieter, and most of him in the room
      if (shot("prutti-superstar-angel", pmp3, ["-ss", String(a), "-t", String(b - a), "-i", pmp3, "-af", `highpass=f=220,treble=g=4:f=6000,afade=t=in:d=0.01,afade=t=out:st=${(b - a - 0.06).toFixed(3)}:d=0.06`])) {
        rawTakes.push({ slug: "prutti-superstar-angel", t: cameoAt, gain: 0.55, pan: 0.3, wet: 0.8, echo: 0.3 });
        console.log(`cameo: Prutti "you are a super star" at ${clock(cameoAt)} (the last outro row, bar 5)`);
      }
    }
  }
  // spoken takes, decoded once into the cache and played like any other clip
  for (const rt of rawTakes) {
    const file = resolve(cache, `raw-${rt.slug}.f32`);
    if (!existsSync(file)) {
      const r = spawnSync(FFMPEG, ["-v", "error", "-i", resolve(PHONEMES, "phrases", `${rt.slug}.mp3`), "-f", "f32le", "-ac", "1", "-ar", String(SR), "pipe:1"], { maxBuffer: 1 << 28 });
      if (r.status !== 0) throw new Error(`could not decode ${rt.slug}`);
      const x = new Float32Array(r.stdout.buffer, r.stdout.byteOffset, r.stdout.byteLength >> 2);
      let pk = 1e-9; for (const v of x) pk = Math.max(pk, Math.abs(v));
      writeFileSync(file, Buffer.from(Float32Array.from(x, (v) => v / pk).buffer));
    }
    sung.push({ key: `raw-${rt.slug}`, t: rt.t, hold: statSync(file).size / 4 / SR, gain: rt.gain, pan: rt.pan, attack: 0, wet: rt.wet, echo: rt.echo });
  }
  for (const n of sung) n.key ??= `${n.syl}-${n.midi}-${Math.round(n.dur * 1000)}`;
  const jobs = [...new Map(sung.filter((n) => n.syl).map((n) => [n.key, { key: n.key, syl: n.syl, midi: n.midi, dur: n.dur }])).values()];
  writeFileSync(jobsFile, JSON.stringify({ library: PHONEMES, cache, sr: SR, jobs }));
  const r = spawnSync(PYTHON, [resolve(HERE, "sing-phonemes.py"), jobsFile], { stdio: "inherit" });
  if (r.status !== 0) throw new Error("sing-phonemes failed");
  const loaded = new Map();
  let missing = 0;
  for (const n of sung) {
    if (!loaded.has(n.key)) {
      const file = resolve(cache, `${n.key}.f32`);
      const raw = existsSync(file) ? readFileSync(file) : null;
      loaded.set(n.key, raw && new Float32Array(raw.buffer, raw.byteOffset, raw.byteLength >> 2));
    }
    const src = loaded.get(n.key);
    if (!src) { missing++; continue; }
    ev("sung", BAKE && bakeSmp(resolve(cache, `${n.key}.f32`)), n.t, n.hold, n.gain, n.pan, n.attack, n.wet, n.echo);
    const i0 = Math.round(n.t * SR), len = Math.min(src.length, Math.floor((n.hold + 0.12) * SR)), fade = Math.floor(0.1 * SR);
    const att = Math.max(1, Math.floor(n.attack * SR)), gl = n.gain * panL(n.pan), gr = n.gain * panR(n.pan);
    for (let k = 0; k < len && i0 + k < N; k++) {
      const v = src[k] * Math.min(1, k / att, (len - k) / fade), j = (i0 + k) * 2;
      bells[j] += v * gl; bells[j + 1] += v * gr;
      rev[j] += v * gl * n.wet; rev[j + 1] += v * gr * n.wet;
      dly[i0 + k] += v * n.gain * n.echo;
    }
  }
  console.log(`jeffrey: ${sung.length} sung notes, ${jobs.length} unique${missing ? `, ${missing} MISSING` : ""}`);
}

// ── post ────────────────────────────────────────────────────────────────
// Underwater: a low-pass over the music bus that closes for the bridge and
// opens again as the heartbeat returns.
ROWS.forEach((row, ri) => {
  if (!row.under) return;
  ev("under", ri);
  const a = Math.round(ri * 8 * BAR * SR), z = Math.round((ri + 1) * 8 * BAR * SR), ramp = BAR * SR;
  let l = 0, r = 0;
  for (let i = a; i < z + ramp && i < N; i++) {
    const depth = Math.min(1, (i - a) / ramp, Math.max(0, (z + ramp - i) / ramp));
    const fc = 16000 * (1 - depth) + 900 * depth, k = 1 - Math.exp(-TAU * fc / SR);
    l += k * (music[i * 2] - l); r += k * (music[i * 2 + 1] - r);
    music[i * 2] = l; music[i * 2 + 1] = r;
  }
});

// Phaser on everything acid: six swept allpass stages a side, the two ears a
// quarter-cycle apart, one sweep every four bars. Dry stays in the middle, so
// the notches move across the stereo field while the low end holds still.
{
  const rate = 1 / (4 * BAR), fb = 0.5, send = CFG.gain.acid / CFG.gain.music;
  for (let ch = 0; ch < 2; ch++) {
    const x1 = new Float64Array(6), y1 = new Float64Array(6);
    let a = 0, last = 0;
    for (let i = 0; i < N; i++) {
      if ((i & 31) === 0) {
        const l = 0.5 + 0.5 * Math.sin(TAU * rate * i / SR + ch * Math.PI / 2);
        const tn = Math.tan(Math.PI * 220 * 14 ** l / SR);
        a = (1 - tn) / (1 + tn);
      }
      const dry = acidBus[i];
      let v = dry + last * fb;
      for (let st = 0; st < 6; st++) { const y = -a * v + x1[st] + a * y1[st]; x1[st] = v; y1[st] = y; v = y; }
      last = v;
      music[i * 2 + ch] += (dry + v) * 0.85 * send;
      if (ch === 0) dly[i] += dry * 0.08 * send;
    }
  }
}

// Ping-pong delay from the mono send, into the music bus and a little into the room.
{
  const D = Math.round(CFG.delay.beats * BEAT * SR), L = new Float32Array(D), R = new Float32Array(D);
  const { feedback: fb, damp, level } = CFG.delay;
  let lpL = 0, lpR = 0;
  for (let i = 0; i < N; i++) {
    const w = i % D, oL = L[w], oR = R[w];
    lpR += (1 - damp) * (oR - lpR); lpL += (1 - damp) * (oL - lpL);
    L[w] = dly[i] + fb * lpR; R[w] = lpL;
    music[i * 2] += oL * level; music[i * 2 + 1] += oR * level;
    rev[i * 2] += oL * 0.25; rev[i * 2 + 1] += oR * 0.25;
  }
}
progress.update(70);

// Reverb: eight damped combs and four allpasses per ear, processed in place.
{
  const { feedback, damp, predelay, level } = CFG.reverb, pre = Math.round(predelay * SR);
  const scale = SR / 44100, COMB = [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617], AP = [556, 441, 341, 225];
  for (let ch = 0; ch < 2; ch++) {
    const combs = COMB.map((n) => ({ buf: new Float32Array(Math.round((n + ch * 23) * scale)), i: 0, lp: 0 }));
    const aps = AP.map((n) => ({ buf: new Float32Array(Math.round((n + ch * 23) * scale)), i: 0 }));
    const line = new Float32Array(pre + 1);
    for (let i = 0; i < N; i++) {
      const w = i % (pre + 1), x = line[w]; line[w] = rev[i * 2 + ch];
      let y = 0;
      for (const c of combs) {
        const o = c.buf[c.i]; y += o;
        c.lp = o * (1 - damp) + c.lp * damp;
        c.buf[c.i] = x + c.lp * feedback;
        if (++c.i === c.buf.length) c.i = 0;
      }
      for (const a of aps) {
        const o = a.buf[a.i];
        a.buf[a.i] = y + o * 0.5; y = o - y;
        if (++a.i === a.buf.length) a.i = 0;
      }
      rev[i * 2 + ch] = y * 0.12 * level;
    }
    progress.update(75 + ch * 8);
  }
}

// The bomb (@jeffrey 2026-11-20: "one of those spatial warble wobbly explosions … like we have in
// wannadash … or two"). This is wannadash's elasticizeBus (pop/cult/bin/render10.mjs), with its "blast"
// and "burst" settings and its body masses, directions and shards: an impulse gives every stereo bus its
// own velocity, a damped spring carries it out and back (the displacement is a propagation delay, so the
// bus bends in pitch, and its sign is the azimuth), and the first excursion holds and repeats a few
// milliseconds of the bus. The kick and the bass are mono and stay where they are.
const smooth01 = (u) => u * u * (3 - 2 * u);
const BOMBS = [];
{
  const hit = (names, o) => { const t = rowAt(...names); if (t != null) BOMBS.push({ t, ...o }); };
  hit(["final a"], { duration: 4.80, strength: 1.42, hz: 0.92, damping: 0.58, glitch: 0.34, attack: 0.030, release: 0.62, dispersion: 0.24 });   // blast: the drop into the finale
  // one hit. (A second — wannadash's "burst": duration 3.20, strength 1.55, hz 1.60, damping 0.90, glitch 0.50,
  // attack 0.020, release 0.50, dispersion 0.30 — went on the door of the outro in the first pass; the gong has that door now.)
}

const BODIES = [   // wannadash's music, drums, vox and tube bodies; the sung voice and the bells share a bus here
  { bus: music, dir: -0.86, mass: 1.10, shard: 0.021 },
  { bus: drums, dir: 0.70, mass: 1.42, shard: 0.017 },
  { bus: bells, dir: -0.42, mass: 0.74, shard: 0.026 },
  { bus: rev, dir: 0.96, mass: 0.92, shard: 0.031 },
];
function elasticize(body, bodyIndex, ex) {
  const start = Math.max(0, Math.round((ex.t - 0.08) * SR)), end = Math.min(N, Math.round((ex.t + ex.duration + 0.10) * SR));
  const len = end - start; if (len < 4) return;
  const src = body.bus.slice(start * 2, end * 2);
  const read = (ch, p) => { if (p <= 0) return src[ch]; const q = Math.floor(p); if (q + 1 >= len) return src[(len - 1) * 2 + ch]; const f = p - q; return src[q * 2 + ch] + (src[(q + 1) * 2 + ch] - src[q * 2 + ch]) * f; };
  const anchor = Math.max(0, Math.round((ex.t + 0.035 + bodyIndex * 0.009) * SR) - start), grain = Math.max(16, Math.round(body.shard * SR));
  for (let i = Math.max(start, Math.round(ex.t * SR)); i < end; i++) {
    const age = i / SR - ex.t;
    if (age < 0 || age > ex.duration) continue;
    const edge = smooth01(clamp(age / ex.attack, 0, 1)) * smooth01(clamp((ex.duration - age) / ex.release, 0, 1));
    const bodyHz = ex.hz * (1 + ex.dispersion * (bodyIndex - 2) / 2);
    const spring = ex.strength / body.mass * Math.exp(-ex.damping * age) * Math.sin(TAU * bodyHz * age) * edge;
    const travel = Math.abs(spring), delay = travel * (0.012 + 0.021 / body.mass) * SR;
    const local = i - start, pos = Math.max(0, local - delay);
    let ml = read(0, pos), mr = read(1, pos);
    const distance = (1 + 0.11 * travel) / (1 + 0.24 * travel), pan = clamp(body.dir * spring * 0.86, -0.92, 0.92);
    ml *= Math.sqrt(1 - pan) * distance; mr *= Math.sqrt(1 + pan) * distance;
    const fracture = ex.glitch * Math.exp(-2.7 * age) * smooth01(clamp(age / 0.035, 0, 1)) * edge;
    if (fracture > 0.012) {
      const gp = Math.min(len - 2, anchor + ((local - anchor + grain * 64) % grain));
      const gl = read(0, gp), gr = read(1, gp), swap = bodyIndex & 1;
      ml = ml * (1 - 0.34 * fracture) + (swap ? gr : gl) * 0.34 * fracture;
      mr = mr * (1 - 0.34 * fracture) + (swap ? gl : gr) * 0.34 * fracture;
    }
    const wet = edge * clamp(0.14 + 0.58 * travel + 0.08 * fracture, 0, 0.90);
    body.bus[i * 2] = src[local * 2] * (1 - wet) + ml * wet;
    body.bus[i * 2 + 1] = src[local * 2 + 1] * (1 - wet) + mr * wet;
  }
}
for (const ex of BOMBS) {
  ev("elastic", ex.t, ex.duration, ex.strength, ex.hz, ex.damping, ex.glitch, ex.attack, ex.release, ex.dispersion);
  BODIES.forEach((b, k) => elasticize(b, k, ex));
}
// temper: the music, the drums, the kick and the bass fall back by `depth` over 60 ms and return over
// `back` seconds (an exponential, like the ring they make room for). The bells and the voice stay.
for (const tp of TEMPERS) {
  ev("temper", tp.t, tp.depth, tp.back);
  const i0 = Math.round(tp.t * SR), n = Math.round(tp.back * 5 * SR);
  for (let k = 0; k < n && i0 + k < N; k++) {
    const age = k / SR, g = 1 - tp.depth * Math.min(1, age / 0.06) * Math.exp(-age / tp.back), i = i0 + k;
    music[i * 2] *= g; music[i * 2 + 1] *= g; drums[i * 2] *= g; drums[i * 2 + 1] *= g; kickBus[i] *= g; bassBus[i] *= g;
  }
}

if (BAKE) {
  const flat = (o, at = "") => Object.entries(o).flatMap(([k, v]) => (typeof v === "object" ? flat(v, `${at}${k}.`) : [`cfg ${at}${k} ${v}`]));
  writeFileSync(resolve(BAKE), [`sr ${SR}`, `frames ${N}`, `material ${MATERIAL} ${MATERIAL_AMOUNT}`, ...flat(CFG), ...baked].join("\n") + "\n");
  console.log(`baked ${baked.length} events → ${resolve(BAKE)}`);
}

// Sum. The kick opens a hole in everything that isn't the kick.
const stems = process.argv.includes("--stems");
const mix = music;      // summed in place
{
  const D = CFG.duck, cr = CFG.crunch, G = CFG.gain, nrm = 1 / Math.tanh(cr);
  let kp = 0, sp = 0, peak = 0;
  for (let i = 0; i < N; i++) {
    const t = i / SR;
    while (kp + 1 < kicks.length && kicks[kp + 1][0] <= t) kp++;
    let d = 0;
    if (kicks.length && kicks[kp][0] <= t) {
      const dt = t - kicks[kp][0];
      d = kicks[kp][1] * (dt < 0.012 ? dt / 0.012 : Math.exp(-(dt - 0.012) / D.release));
    }
    while (sp + 1 < snares.length && snares[sp + 1][0] <= t) sp++;
    let ds = 0;
    if (snares.length && snares[sp][0] <= t) {
      const dt = t - snares[sp][0];
      ds = Math.min(1, snares[sp][1]) * (dt < 0.008 ? dt / 0.008 : Math.exp(-(dt - 0.008) / D.snareRelease));
    }
    const k = Math.tanh(kickBus[i] * cr * 1.6) * nrm * G.kick;
    const b = Math.tanh(bassBus[i] * cr * G.bass * 1.4) * nrm * (1 - D.bass * d) * 0.7;
    for (let ch = 0; ch < 2; ch++) {
      const j = i * 2 + ch;
      const dr = Math.tanh(drums[j] * cr * 2) * nrm * 0.5 * G.drums * 2;
      const v = k + b + dr + music[j] * G.music * (1 - D.music * d)
        + bells[j] * G.bells * (1 - D.bells * d) * (1 - D.snare * ds) + rev[j] * (1 - D.wet * d);
      mix[j] = v;
      if (Math.abs(v) > peak) peak = Math.abs(v);
    }
  }
  // The record's material: the summed mix is printed to plastic tape before
  // the limiter, so every voice ends up made of the same stuff.
  if (MATERIAL !== "none") {
    const pre = 0.5 / Math.max(peak, 1e-9);           // a known level into the oxide
    for (let j = 0; j < N * 2; j++) mix[j] *= pre;
    applyMaterial(mix, SR, MATERIAL, MATERIAL_AMOUNT);
    peak = 0;
    for (let j = 0; j < N * 2; j++) if (Math.abs(mix[j]) > peak) peak = Math.abs(mix[j]);
  }
  console.log(`premaster peak ${peak.toFixed(3)} · material ${MATERIAL}`);
  const g = 0.89 / Math.max(peak, 1e-9);
  for (let j = 0; j < N * 2; j++) mix[j] *= g;
}
progress.update(90);

function writeWav(path, data, channels) {
  const bytes = data.length * 4, h = Buffer.alloc(44);
  h.write("RIFF", 0); h.writeUInt32LE(36 + bytes, 4); h.write("WAVEfmt ", 8); h.writeUInt32LE(16, 16);
  h.writeUInt16LE(3, 20); h.writeUInt16LE(channels, 22); h.writeUInt32LE(SR, 24);
  h.writeUInt32LE(SR * channels * 4, 28); h.writeUInt16LE(channels * 4, 32); h.writeUInt16LE(32, 34);
  h.write("data", 36); h.writeUInt32LE(bytes, 40);
  const fd = openSync(path, "w");
  writeSync(fd, h);
  const view = Buffer.from(data.buffer, data.byteOffset, bytes);
  for (let o = 0; o < bytes; o += 1 << 26) writeSync(fd, view, o, Math.min(1 << 26, bytes - o));
  closeSync(fd);
}

// The stereo story, handed to the master as [seconds, side gain] breakpoints:
// the machine folds toward a point over its last four bars and the finale
// opens from it; the second finale row stands wider than the first.
const WIDTH = [[0, 1]];
ROWS.forEach((row, ri) => {
  const t0 = ri * 8 * BAR, t1 = t0 + 8 * BAR;
  if (row.name === "machine") WIDTH.push([t0 + 4 * BAR, 1], [t1 - 0.06, 0.2], [t1, 1]);
  if (row.name.startsWith("final b") && !ROWS[ri - 1]?.name.startsWith("final b")) WIDTH.push([t0, 1], [t0 + BAR, 1.15]);
  if (row.name.startsWith("final b") && !ROWS[ri + 1]?.name.startsWith("final b")) WIDTH.push([t1, 1.15], [t1 + 2 * BAR, 1]);
});
writeFileSync(resolve(OUT, `${STEM}.width.json`), JSON.stringify(WIDTH));

const pre = resolve(OUT, `${STEM}-premaster.wav`);
writeWav(pre, mix, 2);
if (stems) {
  const dir = resolve(OUT, `${STEM}-stems`);
  mkdirSync(dir, { recursive: true });
  writeWav(resolve(dir, "01-kick.wav"), kickBus, 1);
  writeWav(resolve(dir, "02-bass.wav"), bassBus, 1);
  writeWav(resolve(dir, "02b-acid.wav"), acidBus, 1);
  writeWav(resolve(dir, "03-drums.wav"), drums, 2);
  writeWav(resolve(dir, "04-bells-vox.wav"), bells, 2);
  writeWav(resolve(dir, "05-room.wav"), rev, 2);
}

if (BAKE) {       // the score and the premaster are the whole job; the master on disk stays as it was
  progress.end();
  console.log(`premaster ${pre}`); process.exit(0);
}

// Master: the pop chain in cutie-master.mjs (mono lows, kick harmonics,
// presence and air, glue, one static gain into the limiter).
const mp3 = resolve(OUT, `${STEM}.mp3`), wav = resolve(OUT, `${STEM}-master.wav`);
const run = (args) => { const r = spawnSync(FFMPEG, ["-y", "-hide_banner", "-loglevel", "error", ...args], { stdio: "inherit" }); if (r.status !== 0) throw new Error("ffmpeg failed"); };
run(["-i", pre, "-filter_complex", masterGraph(SR, [DUR - 3, 3], undefined, WIDTH), "-map", "[out]", "-ar", String(SR), "-c:a", "pcm_s24le", wav]);
const cover = COVER;
run(["-i", wav, ...(existsSync(cover) ? ["-i", cover, "-map", "0:a", "-map", "1:v", "-c:v", "mjpeg", "-vf", "scale=1200:1200", "-disposition:v", "attached_pic", "-id3v2_version", "3"] : []),
  "-c:a", "libmp3lame", "-b:a", "320k", "-metadata", "title=wada jetter QD UR",
  "-metadata", "artist=Aesthetic Dot Computer", "-metadata", "album=pixsies", mp3]);
progress.update(100);
progress.end();
console.log(`${mp3}\n${BARS} bars · ${CFG.bpm} BPM · ${clock(DUR)}`);
