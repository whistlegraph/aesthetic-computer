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
//
// The feel lives in CFG and the form in ROWS: twenty-one eight-bar rows,
// one line each. Retune there; the voices below only read them.

import { mkdirSync, writeFileSync, readFileSync, existsSync, openSync, writeSync, closeSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { renderBell } from "../../lib/bell.mjs";
import * as progress from "../../lib/render-progress.mjs";
import { applyMaterial } from "../../lib/material.mjs";

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

// ── feel ────────────────────────────────────────────────────────────────
const CFG = {
  bpm: 112,
  swing: 0.16,            // fraction of a sixteenth the off-sixteenths arrive late
  tail: 8,                // seconds of ring after the last bar
  kick: { start: 118, end: 46, attack: 0.007, decay: 0.2, gain: 0.6 },
  duck: { music: 0.5, bass: 0.72, bells: 0.18, wet: 0.4, release: 0.2 },
  reverb: { feedback: 0.905, damp: 0.32, predelay: 0.028, level: 0.8 },
  delay: { beats: 0.75, feedback: 0.42, damp: 0.45, level: 0.8 },
  crunch: 0.85,           // tanh drive on kick + bass + drums only; bells and voices stay pure
  gain: {
    kick: 1.0, bass: 0.48, acid: 0.85, drums: 0.6, music: 1.25, bells: 1.7,
    pad: 0.13, air: 1.0, sung: 0.55, choir: 0.2, arp: 0.3, bloop: 0.5, lead: 0.4, harm: 0.16,
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
const THEME = [
  [0, 0, 1.5, 1], [1.5, 3, 0.5, 0.78], [2, 7, 1, 0.92], [3.25, 10, 0.6, 0.82],
  [4, 7, 1, 0.88], [5.5, 5, 0.5, 0.72], [6, 2, 1, 0.8], [7, 0, 1.6, 0.95],
  [8, 3, 1, 0.78], [9, 7, 1, 0.86], [10, 12, 1.4, 1], [11.5, 10, 0.5, 0.8],
  [12, 7, 1, 0.82], [13, 9, 0.75, 0.86], [14.5, 5, 0.5, 0.74], [15, 3, 1, 0.82],
  [16, 0, 2, 1], [18, 7, 1, 0.86], [19, 10, 1, 0.82],
  [20, 14, 0.75, 1], [21, 12, 1, 0.9], [22, 9, 1, 0.84], [23, 7, 1, 0.8],
  [24, 5, 1.5, 0.82], [25.5, 8, 0.5, 0.76], [26, 7, 1, 0.84], [27, 2, 1, 0.72],
  [28, -1, 1, 0.72], [29, 2, 1, 0.78], [30, 7, 1, 0.88], [31, 12, 2, 1],
];
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
  // Dorian: the raised sixth is the colour, except over Cmaj7 (bar 5), where it yields to C.
  hookDor: themed((n, b) => (n === 9 && b >= 20 && b < 24 ? 8 : n)),
  hookMix: themed((n) => (n === 3 ? 4 : n)),
  // Ionian: third, sixth and seventh rise. The fourth stays natural — the
  // study's raised fourth sat a semitone off the Amaj9 underneath it.
  hookIon: themed((n) => ({ 3: 4, 8: 9, 10: 11 }[n] ?? n)),
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
// choir: jeffrey's "oo"/"ah" on the chord     sing: jeffrey sings the tune in syllables
const ROWS = [
  { name: "intro a",   block: "INTRO",   mode: "pent", kick: "half", kickFrom: 4, pad: 0.4, air: 0.9, bloop: [1, 5] },
  { name: "intro b",   block: "INTRO",   mode: "pent", kick: "four", hat: 0.5, shk: 0.4, bass: "long", pad: 0.6, air: 0.8, bloop: [1, 5], mel: "introFrag", voice: "bells" },
  { name: "verse 1",   block: "VERSE1",  mode: "aeo",  kick: "four", clap: 0.6, hat: 0.7, shk: 0.5, bass: "skel", pad: 0.7, air: 0.6, arp: [0.6, 2], bloop: [0.8, 5], mel: "verse1", voice: "bells" },
  { name: "verse 1'",  block: "VERSE1",  mode: "aeo",  kick: "four", clap: 0.7, hat: 0.8, shk: 0.6, bass: "bounce", pad: 0.7, air: 0.6, arp: [0.8, 2], bloop: [0.8, 6], mel: "verse1b", voice: "both", build: 0.5 },
  { name: "chorus 1",  block: "CHORUS1", mode: "dor",  kick: "four", clap: 0.9, hat: 0.9, shk: 0.7, bass: "long", pad: 0.9, air: 0.8, arp: [0.9, 4], bloop: [0.6, 6], mel: "hookDor", voice: "synth", acid: [0.6, 0.3, 0.38], choir: 0.7, strings: 0.5 },
  { name: "chorus 1'", block: "CHORUS1", mode: "dor",  kick: "four", clap: 0.9, hat: 0.9, shk: 0.7, bass: "long", pad: 0.9, air: 0.8, arp: [0.9, 4], bloop: [0.6, 6], mel: "hookDor", voice: "both", harm: true, acid: [0.7, 0.34, 0.44], choir: 0.9, sing: true, strings: 0.6 },
  { name: "breath",    block: "INTRO",   mode: "pent", pad: 1, air: 1, bloop: [0.9, 3], mel: "breath", voice: "bells", build: 1, choir: 1 },
  { name: "verse 2",   block: "VERSE2",  mode: "phr",  kick: "four", clap: 0.6, hat: 0.7, shk: 0.5, rim: 0.5, bass: "skel", pad: 0.7, air: 0.6, arp: [0.7, 2], bloop: [0.8, 5], mel: "verse2", voice: "bells" },
  { name: "verse 2'",  block: "VERSE2",  mode: "phr",  kick: "four", clap: 0.7, hat: 0.8, shk: 0.6, rim: 0.5, bass: "long", pad: 0.7, air: 0.6, arp: [0.8, 2], bloop: [0.8, 6], mel: "verse2b", voice: "both", build: 0.5, acid: [0.5, 0.2, 0.3], strings: 0.4 },
  { name: "chorus 2",  block: "CHORUS2", mode: "mix",  kick: "four", clap: 0.9, hat: 0.9, shk: 0.7, bass: "long", pad: 0.9, air: 0.8, arp: [0.9, 4], bloop: [0.6, 6], mel: "hookMix", voice: "synth", acid: [0.7, 0.36, 0.46], choir: 0.7, strings: 0.6 },
  { name: "chorus 2'", block: "CHORUS2", mode: "mix",  kick: "four", clap: 0.9, hat: 0.9, shk: 0.7, bass: "long", pad: 0.9, air: 0.8, arp: [0.9, 4], bloop: [0.6, 6], mel: "hookMix", voice: "both", harm: true, acid: [0.75, 0.4, 0.52], choir: 0.9, sing: true, strings: 0.7, wub: 0.5 },
  { name: "bridge",    block: "BRIDGE",  mode: "lyd",  bass: "long", pad: 1, air: 1, arp: [0.5, 2], bloop: [0.6, 3], mel: "bridge", voice: "synth", under: true, choir: 1 },
  { name: "bridge'",   block: "BRIDGE",  mode: "lyd",  kick: "heart", hat: 0.4, bass: "long", pad: 1, air: 0.9, arp: [0.8, 4], bloop: [0.7, 5], mel: "bridge", voice: "both", build: 1, drop: true, acid: [0.55, 0.12, 0.4], choir: 0.8, strings: 0.7, wub: 0.5 },
  { name: "machine",   block: "PEDAL",   mode: "pent", kick: "four", tight: true, rim: 0.9, hat: 0.8, shk: 0.6, bass: "long", pad: 0.3, air: 0.4, bloop: [0.7, 9], chops: true, build: 1, drop: true, acid: [1, 0.25, 0.85], strings: 0.5, wub: 1.0 },
  { name: "final a",   block: "FINAL_A", mode: "ion",  kick: "four", clap: 1, hat: 1, shk: 0.8, bass: "long", pad: 1, air: 0.9, arp: [1, 4], bloop: [0.6, 6], mel: "hookIon", voice: "both", acid: [0.8, 0.5, 0.6], choir: 0.8, sing: true, strings: 0.8 },
  { name: "final a'",  block: "FINAL_B", mode: "ion",  kick: "four", clap: 1, hat: 1, shk: 0.8, bass: "long", pad: 1, air: 0.9, arp: [1, 4], bloop: [0.6, 6], mel: "ionAnswer", voice: "both", acid: [0.8, 0.55, 0.62], choir: 0.8, sing: true, strings: 0.8 },
  { name: "final b",   block: "FINAL_A", mode: "ion",  kick: "four", clap: 1, hat: 1, shk: 0.9, bass: "long", pad: 1, air: 1, arp: [1, 4], bloop: [0.7, 7], mel: "hookIon", voice: "both", harm: true, low: true, acid: [0.85, 0.58, 0.7], choir: 1, sing: true, strings: 0.9, wub: 0.75 },
  { name: "final b'",  block: "FINAL_B", mode: "ion",  kick: "four", clap: 1, hat: 1, shk: 0.9, bass: "long", pad: 1, air: 1, arp: [1, 4], bloop: [0.7, 7], mel: "ionAnswer", voice: "both", harm: true, low: true, acid: [0.85, 0.62, 0.72], choir: 1, sing: true, strings: 0.9, wub: 0.75 },
  { name: "outro",     block: "OUTRO",   mode: "pent", kick: "four", hat: 0.6, shk: 0.4, bass: "long", pad: 0.8, air: 0.9, arp: [0.5, 2], bloop: [0.9, 5], mel: "outro", voice: "bells", choir: 0.7 },
  { name: "outro'",    block: "OUTRO",   mode: "pent", kick: "half", pad: 0.6, air: 1, bloop: [0.8, 4], mel: "outro2", voice: "bells", choir: 0.5 },
  { name: "mist",      block: "PEDAL",   mode: "pent", pad: 0.35, air: 1, bloop: [0.6, 3], last: true },
];
const BARS = ROWS.length * 8;
const DUR = BARS * BAR + CFG.tail;
const N = Math.ceil(DUR * SR);
const clock = (s) => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, "0")}`;

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
  let ph = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const x = k / SR;
    ph += (K.end + (K.start - K.end) * Math.exp(-x / 0.045)) / SR;
    kickBus[i0 + k] += Math.sin(TAU * ph) * Math.min(1, x / K.attack) * Math.exp(-x / dec) * g * K.gain;
  }
  kicks.push([t, g]);
}

// Spray: white noise through one state-variable filter. type 0 band, 1 high.
function spray(t, { fc, q = 1, type = 0, att = 0.001, dec = 0.05, len, gain, pan = 0, wet = 0 }) {
  const i0 = Math.round(t * SR), n = Math.floor((len ?? dec * 6 + att) * SR);
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
  for (const [o, a] of [[0, 0.5], [0.011, 0.7], [0.022, 1]])
    spray(t + o, { fc: 1500, q: 1.4, dec: o === 0.022 ? 0.085 : 0.012, gain: CFG.gain.clap * g * a, wet: 0.35 });
}
const hat = (t, g, open) => spray(t, { fc: 7200, q: 0.9, type: 1, dec: open ? 0.11 : 0.022, gain: CFG.gain.hat * g, pan: 0.25 });
const shaker = (t, g, pan) => spray(t, { fc: 5200, q: 2.2, att: 0.008, dec: 0.028, gain: CFG.gain.shaker * g, pan });
function tick(t, hz, g, pan) {
  const i0 = Math.round(t * SR), n = Math.floor(0.05 * SR), gl = g * panL(pan), gr = g * panR(pan);
  let ph = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const x = k / SR; ph += hz / SR;
    const v = Math.sin(TAU * ph) * Math.exp(-x / 0.009), j = (i0 + k) * 2;
    drums[j] += v * gl; drums[j + 1] += v * gr; rev[j] += v * gl * 0.2; rev[j + 1] += v * gr * 0.2;
  }
}

// Bloop: a sine that rises into its note — a drip. The lane's signature percussion.
function bloop(t, midi, g, pan, len = 0.2) {
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
function acidRow(t0, evs, level, cut0, cut1, reso = 0.78) {
  const i0 = Math.round(t0 * SR), n = Math.round(8 * BAR * SR), k = reso * 3.8;
  let ph = 0, f = 0, ft = 0, y1 = 0, y2 = 0, y3 = 0, y4 = 0, fenv = 0, aenv = 0, gateEnd = 0, acc = 0, e = 0, tied = false;
  for (let i = 0; i < n && i0 + i < N; i++) {
    const t = t0 + i / SR;
    while (e < evs.length && evs[e].t <= t) {
      const ev = evs[e++];
      ft = mtof(ev.midi);
      if (!tied || f === 0) { f = ft; fenv = 1; acc = ev.acc; }
      gateEnd = ev.t + (ev.slide ? SIX * 1.1 : SIX * 0.62);
      tied = ev.slide;
    }
    if (f === 0) continue;
    f += (ft - f) * 0.0008;
    const gate = t < gateEnd;
    aenv += ((gate ? 1 : 0) - aenv) * (gate ? 0.02 : 0.004);
    fenv *= acc ? 0.99975 : 0.99988;
    const dt = f / SR;
    ph += dt; if (ph >= 1) ph -= 1;
    let saw = 2 * ph - 1;
    if (ph < dt) { const u = ph / dt; saw -= u + u - u * u - 1; }
    else if (ph > 1 - dt) { const u = (ph - 1) / dt; saw -= u * u + u + u + 1; }
    const fc = Math.min(9000, 150 * 2 ** ((cut0 + (cut1 - cut0) * (i / n)) * 4.5 + (1.6 + acc) * fenv));
    const g = 1 - Math.exp(-TAU * fc / SR);
    const x = Math.tanh(saw * 0.8 - k * y4);
    y1 += g * (x - y1); y2 += g * (y1 - y2); y3 += g * (y2 - y3); y4 += g * (y3 - y4);
    acidBus[i0 + i] += Math.tanh(y4 * (1 + k * 0.6) * 1.4) * aenv * (0.75 + 0.35 * acc) * level;
  }
}

// Wub: a held saw over its own sub-octave square, the ladder's cutoff swung
// by an LFO locked to the beat. The rate changes by the bar — quarters,
// eighths, triplets, sixteenths — so the bass talks.
const WUB_RATES = [1, 1, 2, 1, 2, 2, 3, 4];     // wobbles per beat, by bar of the row
function wubRow(t0, chords, level, dropLast) {
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
function riser(t, dur, g) {
  const i0 = Math.round(t * SR), n = Math.floor(dur * SR);
  let lo = 0, bd = 0, lo2 = 0, bd2 = 0;
  for (let k = 0; k < n && i0 + k < N; k++) {
    const u = k / n, f = 2 * Math.sin(Math.PI * (350 * 16 ** u) / SR);
    let hi = noise() - lo - 0.3 * bd; bd += f * hi; lo += f * bd;
    hi = noise() - lo2 - 0.3 * bd2; bd2 += f * hi; lo2 += f * bd2;
    const a = u * u * g * CFG.gain.riser, j = (i0 + k) * 2;
    music[j] += bd * a; music[j + 1] += bd2 * a;
    rev[j] += bd * a * 0.6; rev[j + 1] += bd2 * a * 0.6;
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

  // harmony beds
  for (const c of chords) {
    const t = t0 + c.at * BAR, d = c.bars * BAR;
    if (row.pad) c.notes.forEach((m, i) => padNote(t + i * 0.03, d - 0.1, m, row.pad * CFG.gain.pad, (i - (c.notes.length - 1) / 2) * 0.32, row.pad));
    if (row.air) {
      const top = c.notes.slice(-3);
      top.forEach((m, i) => airNote(t, d - 0.2, m + 12, row.air, i - 1));
      airNote(t, d - 0.2, c.notes[0] + 24, row.air * 0.5, 0);
      airNote(t, d - 0.2, c.notes[1] + 36, row.air * 0.2, -1);
      airNote(t, d - 0.2, c.notes[2] + 36, row.air * 0.2, 1);
    }
    if (row.bass === "long") bassNote(t, d - 0.15, c.bass + (c.bass < 36 ? 12 : 0), row.acid ? 0.5 : 0.5, true);
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
      ACID_PAT[ACID_ORDER[bar % 4]].forEach(([idx, acc = 0, slide = 0], p) => {
        if (idx != null) evs.push({ t: t0 + bar * BAR + six(p), midi: ladder[idx], acc, slide });
      });
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
    if (row.kick && bar >= (row.kickFrom ?? 0) && !dropped) {
      const beats = row.kick === "four" ? [0, 1, 2, 3] : row.kick === "half" ? [0, 2] : bar < 4 ? [0, 2.5] : [0, 1, 2, 3];
      for (const bt of beats) kick(tb + bt * BEAT, row.kick === "heart" && bar < 4 ? 0.8 : 1, row.tight);
    }
    if (row.clap && !dropped) for (const bt of [1, 3]) clap(tb + bt * BEAT + 0.008, row.clap);
    if (row.hat) for (let e = 0; e < 4; e++) hat(tb + (e + 0.5) * BEAT - 0.006, row.hat * (0.8 + 0.2 * rnd()), !row.tight && bar % 4 === 3 && e === 3);
    if (row.shk) for (const p of euclid(5 + 2 * (ri % 3), abs * 5)) shaker(tb + six(p), row.shk * (p % 4 === 2 ? 1 : 0.6), -0.4 + 0.8 * rnd());
    if (row.rim) for (let p = (5 - (abs * 16) % 5) % 5; p < 16; p += 5) tick(tb + six(p), 1750, CFG.gain.rim * row.rim, 0.5);

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
    if (row.arp) {
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
  if (row.build) riser(t0 + 4 * BAR, 4 * BAR, row.build);
  // a stone bowl on every row's door, tuned to the first chord's bass
  if (ri > 0) bell(t0, 52 + ((chords[0].bass - 40 + 24) % 12), 0.7, { geometry: "bowl", dur: 9, gain: 0.8, wet: 0.5 });

  // melody
  if (row.mel) {
    const glass = row.mode === "ion" || row.mode === "mix" || row.mode === "dor";
    for (const [beat, iv, beats, vel] of MEL[row.mel]) {
      const t = t0 + beat * BEAT, d = beats * BEAT, m = 64 + iv;
      if (row.voice !== "bells") {
        lead(t, d, m, vel);
        if (row.low) lead(t, d, m - 12, vel * 0.5, CFG.gain.lead, -0.2);
        if (row.harm && scale.length === 7) {
          const deg = scale.indexOf(((iv % 12) + 12) % 12);
          if (deg >= 0) {
            const lower = deg - 5, oct = Math.floor(lower / 7);
            lead(t, d, 64 + Math.floor(iv / 12) * 12 + scale[((lower % 7) + 7) % 7] + oct * 12, vel, CFG.gain.harm, 0.3);
          }
        }
      }
      // jeffrey sings the tune an octave under the lead: wat-ta-jet-ta, one syllable a note
      if (row.sing) {
        const syl = ["wah", "tah", "jeh", "tah"][sungCount++ % 4], hold = Math.max(d, 0.32);
        sung.push({ t: t - 0.03, syl, midi: m - 12, dur: Math.round(hold * 20) / 20, hold, gain: vel * CFG.gain.sung, pan: 0, attack: 0, wet: 0.45, echo: 0.2 });
      }
      if (row.voice !== "synth") {
        bell(t, m, vel, { material: glass ? "glass" : "stone", geometry: glass ? "glass" : "handbell",
          dur: beats >= 2 ? 5 : 2.5, gain: row.voice === "both" ? 0.6 : 1, pan: Math.sin(beat * 1.3) * 0.35 });
      }
    }
  }

  // machine: the title chopped into four syllables and played on the grid
  if (row.chops) {
    const pat = [[2, 0], [3, 1], [6, 2], [7, 3], [10, 0], [11, 0], [13, 1], [14, 2]];
    for (let bar = 0; bar < 7; bar++) for (const [p, s] of pat) {
      if (bar < 2 && p > 7) continue;
      vox(t0 + bar * BAR + six(p), "wattajetta", { rate: bar % 4 === 3 ? 1.78 : 1.5, from: s / 4, to: (s + 1) / 4,
        gain: 0.45 + 0.04 * bar, pan: (p % 4 - 1.5) * 0.4, wet: 0.25, echo: 0.2 });
    }
  }
  if (row.last) {
    bell(t0 + 6 * BAR, 64, 0.8, { material: "glass", geometry: "glass", dur: 9, wet: 0.7 });
    vox(t0 + 4 * BAR, "wayer", { rate: 0.75, gain: 0.3, wet: 1, echo: 0.5 });
  }
  if (next?.mel?.startsWith("hook")) vox(t0 + 8 * BAR - 1.5 * BEAT, "wattajetta", { rate: 1.5, gain: 0.8, echo: 0.35 });
  progress.update(Math.round(60 * (ri + 1) / ROWS.length));
});

// one-offs
vox(6 * 8 * BAR + BEAT, "wayer", { rate: 1.2, gain: 0.6, wet: 0.9, echo: 0.5 });   // into the breath
flyby(7 * 8 * BAR + 3 * BAR, 1); flyby(8 * 8 * BAR + 3 * BAR, -1);
flyby(11 * 8 * BAR + 1 * BAR, -1); flyby(18 * 8 * BAR + 2 * BAR, 1);

// ── jeffrey ──────────────────────────────────────────────────────────────
// Every sung note is one syllable from the phoneme library, pitched and
// lengthened by sing-phonemes.py (WORLD) and kept on disk by key.
if (sung.length) {
  const cache = resolve(OUT, ".cutie-vox-cache"), jobsFile = resolve(OUT, ".cutie-vox-jobs.json");
  for (const n of sung) n.key = `${n.syl}-${n.midi}-${Math.round(n.dur * 1000)}`;
  const jobs = [...new Map(sung.map((n) => [n.key, { key: n.key, syl: n.syl, midi: n.midi, dur: n.dur }])).values()];
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

// Sum. The kick opens a hole in everything that isn't the kick.
const stems = process.argv.includes("--stems");
const mix = music;      // summed in place
{
  const D = CFG.duck, cr = CFG.crunch, G = CFG.gain, nrm = 1 / Math.tanh(cr);
  let kp = 0, peak = 0;
  for (let i = 0; i < N; i++) {
    const t = i / SR;
    while (kp + 1 < kicks.length && kicks[kp + 1][0] <= t) kp++;
    let d = 0;
    if (kicks.length && kicks[kp][0] <= t) {
      const dt = t - kicks[kp][0];
      d = kicks[kp][1] * (dt < 0.012 ? dt / 0.012 : Math.exp(-(dt - 0.012) / D.release));
    }
    const k = Math.tanh(kickBus[i] * cr * 1.6) * nrm * G.kick;
    const b = Math.tanh(bassBus[i] * cr * G.bass * 1.4) * nrm * (1 - D.bass * d) * 0.7;
    for (let ch = 0; ch < 2; ch++) {
      const j = i * 2 + ch;
      const dr = Math.tanh(drums[j] * cr * 2) * nrm * 0.5 * G.drums * 2;
      const v = k + b + dr + music[j] * G.music * (1 - D.music * d)
        + bells[j] * G.bells * (1 - D.bells * d) + rev[j] * (1 - D.wet * d);
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

// Master: gentle glue, a little weight at 54 Hz, a limiter. No treble lift —
// it reads as tang on laptop speakers.
const mp3 = resolve(OUT, `${STEM}.mp3`), wav = resolve(OUT, `${STEM}-master.wav`);
const chain = [
  "highpass=f=26",
  "acompressor=threshold=-18dB:ratio=1.8:attack=18:release=180:makeup=1.6:knee=8",
  "equalizer=f=54:t=q:w=0.8:g=1.6",
  "alimiter=limit=0.89:attack=4:release=80:level=disabled",
  `afade=t=out:st=${(DUR - 3).toFixed(2)}:d=3`,
].join(",");
const run = (args) => { const r = spawnSync(FFMPEG, ["-y", "-hide_banner", "-loglevel", "error", ...args], { stdio: "inherit" }); if (r.status !== 0) throw new Error("ffmpeg failed"); };
run(["-i", pre, "-af", chain, "-ar", String(SR), "-c:a", "pcm_s24le", wav]);
const cover = resolve(HERE, "../canvas/cover-3000.jpg");
run(["-i", wav, ...(existsSync(cover) ? ["-i", cover, "-map", "0:a", "-map", "1:v", "-c:v", "mjpeg", "-vf", "scale=1200:1200", "-disposition:v", "attached_pic", "-id3v2_version", "3"] : []),
  "-c:a", "libmp3lame", "-b:a", "320k", "-metadata", "title=wattajetta cutie club",
  "-metadata", "artist=Whistlegraph Dot Org", "-metadata", "album=pixsies", mp3]);
progress.update(100);
progress.end();
console.log(`${mp3}\n${BARS} bars · ${CFG.bpm} BPM · ${clock(DUR)}`);
