#!/usr/bin/env node
// bake-c-dance.mjs — bake the loukeman-style melodic deep house remix
// of Amazing Grace. Renders the 4/4 dance bed via amazing-dance, mixes
// the WORLD-locked vocal stem (amazing-wavewizard.wav) on top, then
// runs the master brightening chain.
//
// Pipeline:
//   1 · build amazing-dance if missing/stale
//   2 · render bed → .amazing-dance-bed.wav (240 s, 120 BPM 4/4)
//   3 · mix vocal stem (delayed +32 s — enters at the bar-16 build)
//        + bed (heavy reverb wash) → pre.wav
//   4 · brightening polish → MASTER.wav + 320k mp3
//
// Output: ~/Documents/Shelf/amazing-grace-dance/
//   amazing-grace-dance-c.mp3
//   amazing-grace-dance-c-MASTER.wav
//   amazing-grace-dance-c-MASTER-preBright.wav  (A/B reference)
//
// Usage: node pop/big-pictures/c/bake-c-dance.mjs [outDir] [-- <engine flags>]

import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { existsSync, statSync, mkdirSync } from "node:fs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const POP  = resolve(REPO, "pop");

let args = process.argv.slice(2);
const sep = args.indexOf("--");
const engineExtra = sep >= 0 ? args.slice(sep + 1) : [];
// LENGTH MODE — default "longlong" is the 5-min ultra-stretched
// held-vowel mix (every syllable drones 6-24 s). "--short" packs all 4
// hymn lines into the first ~2 min with far shorter holds, riding the
// bed's natural intro→build→drop1→break arc. The C engine always renders
// a full 300 s bed; SHORT just sets TOTAL_SEC=120 so the bake trims to
// the first 2 min (giving intro/build/drop1/break for free) and remaps
// the vocal timing into a 14..116 s window. SHORT writes to -SHORT-*
// filenames so the longlong master is never clobbered.
const SHORT = args.includes("--short");
args = args.filter((a) => a !== "--short");
const outDir = (sep >= 0 ? args.slice(0, sep) : args).filter((a) => a !== "--short")[0] ||
  `${homedir()}/Documents/Shelf/amazing-grace-dance`;
mkdirSync(outDir, { recursive: true });

const TAG = SHORT ? "-SHORT" : "";
const ENGINE   = `${HERE}/amazing-dance`;
const ENGINE_C = `${HERE}/amazing-dance.c`;
const BED_WAV  = `${outDir}/.amazing-dance${TAG}-bed.wav`;
const PRE_WAV  = `${outDir}/.amazing-dance${TAG}-pre.wav`;
const FINAL_WAV = `${outDir}/amazing-grace-dance-c${TAG}-MASTER.wav`;
const FINAL_MP3 = `${outDir}/amazing-grace-dance-c${TAG}.mp3`;
const PRE_BRIGHT_WAV = `${outDir}/amazing-grace-dance-c${TAG}-MASTER-preBright.wav`;

// Default to the DANCE-timed stem (compressed-from-hymn takes via
// assemble-wavewizard-dance.mjs). Pass --voice wavewizard to fall back
// to the original 55 s hymn-paced stem.
let VOICE_MODE = "wavewizard-dance";
const vIdx = args.findIndex((a) => a === "--voice");
if (vIdx >= 0) { VOICE_MODE = args[vIdx + 1]; args.splice(vIdx, 2); }
const VOCAL = VOICE_MODE === "wavewizard"
  ? `${POP}/big-pictures/out/amazing-wavewizard.wav`
  : `${POP}/big-pictures/out/amazing-wavewizard-dance.wav`;
// Multi-layer choir — pre-pitched stems via rubberband CLI with formant
// preservation so each layer reads as a voice doubling jeffrey, not a
// chipmunk. Each gets stacked into every vocal pass below.
const VOCAL_UP      = VOCAL.replace(/\.wav$/, "-up.wav");        // +12
const VOCAL_UP_HIGH = VOCAL.replace(/\.wav$/, "-up-high.wav");   // +24
const VOCAL_DOWN    = VOCAL.replace(/\.wav$/, "-down.wav");      // -12
// CONSTANT HUM stem — last syllable "sound" extracted + heavily
// stretched into a continuous looped vowel that hums under the whole
// track at low volume (@jeffrey "WHOLE TRACK constant little hum").
const VOCAL_HUM     = VOCAL.replace(/\.wav$/, "-hum.wav");
// HELD "AH" stem — first syllable from the vocal stem, stretched.
const VOCAL_AH      = VOCAL.replace(/\.wav$/, "-ah.wav");
// HELD "MAY" stem — second syllable, stretched.
const VOCAL_MAY     = VOCAL.replace(/\.wav$/, "-may.wav");
// HELD "ZING" stem.
const VOCAL_ZING    = VOCAL.replace(/\.wav$/, "-zing.wav");
// HELD "GRACE" stem — fourth syllable.
const VOCAL_GRACE   = VOCAL.replace(/\.wav$/, "-grace.wav");

// ── JEFFREY-PVC (ElevenLabs TTS) layer ───────────────────────────────
// Source: the raw jeffrey-pvc utterance from the original hymn pipeline
// at pop/big-pictures/out/amazing-vocal.mp3 (7.8 s) + its word-level
// alignment JSON. Each wave-wizard syllable gets a PARALLEL jeffrey-pvc
// layer at the same mix position — doubling the machined vocal with
// jeffrey's natural ElevenLabs voice character.
//
// Multi-syllable words (only "amazing" here) get split proportionally.
const PVC_SOURCE = resolve(POP, "big-pictures/out/amazing-vocal.mp3");
// stemAt / stemDur in SECONDS (converted from the word JSON's ms).
// prefDur is the consonant attack to keep at natural speed.
const PVC_LINE1 = [
  // PVC "a" REMOVED — the 172× rubberband stretch (0.151 s → 26 s)
  // produced a metallic squeak right at t=0 ("very loud intro sound"
  // @jeffrey 2026-05-29). The wave-wizard AH already covers the
  // intro held vowel; the PVC layer joins at "ma".
  { name: "ma",    stemAt: 0.151, stemDur: 0.151, prefDur: 0.04,
    mixAt: 27.5,  holdSec: 4.0,  tremolo: 0.8, gain: 1.20 },
  { name: "zing",  stemAt: 0.302, stemDur: 0.151, prefDur: 0.05,
    mixAt: 32.0,  holdSec: 14.0, tremolo: 0.7, gain: 1.20 },
  { name: "grace", stemAt: 0.488, stemDur: 0.278, prefDur: 0.06,
    mixAt: 46.0,  holdSec: 24.0, tremolo: 0.6, gain: 1.20 },
  { name: "how",   stemAt: 0.813, stemDur: 0.139, prefDur: 0.03,
    mixAt: 74.0,  holdSec: 6.0,  tremolo: 0.8, gain: 1.10 },
  { name: "sweet", stemAt: 1.010, stemDur: 0.290, prefDur: 0.07,
    mixAt: 80.0,  holdSec: 12.0, tremolo: 0.7, gain: 1.10 },
  { name: "the",   stemAt: 1.358, stemDur: 0.082, prefDur: 0.04,
    mixAt: 92.0,  holdSec: 6.0,  tremolo: 0.8, gain: 1.10 },
  { name: "sound", stemAt: 1.486, stemDur: 0.314, prefDur: 0.025,
    mixAt: 98.0,  holdSec: 16.0, tremolo: 0.5, gain: 1.20 },
];

// PVC line 2 — from amazing-vocal-words.json:
//   that 1846-1974, saved 2020-2322, a 2357-2380, wretch 2426-2659,
//   like 2717-2891, me 2949-3251
const PVC_LINE2 = [
  { name: "that",   stemAt: 1.846, stemDur: 0.128, prefDur: 0.04,
    mixAt: 130.0, holdSec: 6.0, tremolo: 0.8, gain: 1.10 },
  { name: "saved",  stemAt: 2.020, stemDur: 0.302, prefDur: 0.05,
    mixAt: 136.0, holdSec: 12.0, tremolo: 0.7, gain: 1.10 },
  { name: "a_l2",   stemAt: 2.357, stemDur: 0.023, prefDur: 0.00,
    mixAt: 148.0, holdSec: 6.0, tremolo: 0.8, gain: 1.10 },
  { name: "wretch", stemAt: 2.426, stemDur: 0.233, prefDur: 0.06,
    mixAt: 154.0, holdSec: 6.0, tremolo: 0.7, gain: 1.10 },
  { name: "like",   stemAt: 2.717, stemDur: 0.174, prefDur: 0.04,
    mixAt: 162.0, holdSec: 6.0, tremolo: 0.8, gain: 1.10 },
  { name: "me",     stemAt: 2.949, stemDur: 0.302, prefDur: 0.03,
    mixAt: 168.0, holdSec: 14.0, tremolo: 0.5, gain: 1.20 },
];
for (const s of PVC_LINE2) {
  s.file = VOCAL.replace(/wavewizard-dance\.wav$/, `pvc-${s.name}.wav`);
}

// PVC line 3 — from amazing-vocal-words.json:
//   i 3402-3518, once 3611-3820, was 3855-3971, lost 4040-4435,
//   but 4505-4656, now 4714-4923, am 4992-5097, found 5178-5503
const PVC_LINE3 = [
  { name: "i_l3",   stemAt: 3.402, stemDur: 0.116, prefDur: 0.00,
    mixAt: 200.0, holdSec: 5.0, tremolo: 0.8, gain: 1.20 },
  { name: "once",   stemAt: 3.611, stemDur: 0.209, prefDur: 0.04,
    mixAt: 205.0, holdSec: 10.0, tremolo: 0.7, gain: 1.10 },
  { name: "was_l3", stemAt: 3.855, stemDur: 0.116, prefDur: 0.05,
    mixAt: 215.0, holdSec: 5.0, tremolo: 0.8, gain: 1.10 },
  { name: "lost",   stemAt: 4.040, stemDur: 0.395, prefDur: 0.05,
    mixAt: 220.0, holdSec: 10.0, tremolo: 0.7, gain: 1.10 },
  { name: "but_l3", stemAt: 4.505, stemDur: 0.151, prefDur: 0.04,
    mixAt: 230.0, holdSec: 4.0, tremolo: 0.8, gain: 1.10 },
  { name: "now_l3", stemAt: 4.714, stemDur: 0.209, prefDur: 0.04,
    mixAt: 234.0, holdSec: 8.0, tremolo: 0.7, gain: 1.10 },
  { name: "am",     stemAt: 4.992, stemDur: 0.105, prefDur: 0.00,
    mixAt: 242.0, holdSec: 4.0, tremolo: 0.8, gain: 1.10 },
  { name: "found",  stemAt: 5.178, stemDur: 0.325, prefDur: 0.04,
    mixAt: 246.0, holdSec: 10.0, tremolo: 0.5, gain: 1.20 },
];
for (const s of PVC_LINE3) {
  s.file = VOCAL.replace(/wavewizard-dance\.wav$/, `pvc-${s.name}.wav`);
}
// File paths for each PVC syllable.
for (const s of PVC_LINE1) {
  s.file = VOCAL.replace(/wavewizard-dance\.wav$/, `pvc-${s.name}.wav`);
}

// REST-OF-LINE-1 syllables — built via a helper that splits each
// syllable into a fast prefix consonant + stretched vowel. Each entry
// here drives both the stem build AND the mix-time placement.
// `stemAt` is where the syllable starts in amazing-wavewizard-dance.wav.
// `mixAt` is when its consonant lands in the final track.
// `holdSec` is how long it sustains; the vowel stretches to fill.
// HOLD-SECONDS EXTENDED ~1.4× (@jeffrey 2026-05-29 "lyrics should
// be longer"). mixAts mostly unchanged — natural tail overlap with
// the next syllable creates a continuous vocal flow.
const LINE1_REST = [
  { name: "how",   midi: 59, stemAt: 4.0, stemDur: 1.0, prefDur: 0.05,
    mixAt:  74.0, holdSec: 6.0, tremolo: 0.8 },
  { name: "sweet", midi: 57, stemAt: 5.0, stemDur: 1.0, prefDur: 0.08,
    mixAt:  80.0, holdSec: 12.0, tremolo: 0.7 },
  { name: "the",   midi: 55, stemAt: 6.0, stemDur: 1.0, prefDur: 0.06,
    mixAt:  92.0, holdSec: 6.0, tremolo: 0.8 },
  { name: "sound", midi: 59, stemAt: 7.0, stemDur: 1.5, prefDur: 0.025,
    mixAt:  98.0, holdSec: 16.0, tremolo: 0.5 },
];

// LINE 2 — "that saved a wretch like me". 6 syllables, ~24 s.
const LINE2 = [
  { name: "that",   midi: 50, stemAt:  8.5, stemDur: 1.0, prefDur: 0.06,
    mixAt: 130.0, holdSec: 6.0, tremolo: 0.8 },
  { name: "saved",  midi: 52, stemAt:  9.5, stemDur: 1.0, prefDur: 0.06,
    mixAt: 136.0, holdSec: 12.0, tremolo: 0.7 },
  { name: "a_l2",   midi: 50, stemAt: 10.5, stemDur: 1.0, prefDur: 0.00,
    mixAt: 148.0, holdSec: 6.0, tremolo: 0.8 },
  { name: "wretch", midi: 59, stemAt: 11.5, stemDur: 1.0, prefDur: 0.08,
    mixAt: 154.0, holdSec: 6.0, tremolo: 0.7 },
  { name: "like",   midi: 55, stemAt: 12.5, stemDur: 1.0, prefDur: 0.05,
    mixAt: 162.0, holdSec: 6.0, tremolo: 0.8 },
  { name: "me",     midi: 59, stemAt: 13.5, stemDur: 1.5, prefDur: 0.04,
    mixAt: 168.0, holdSec: 14.0, tremolo: 0.5 },
];
for (const s of LINE2) {
  s.file = VOCAL.replace(/\.wav$/, `-${s.name}.wav`);
}

// LINE 3 — "I once was lost but now am found" (starts ~1:28 = 88 s).
// Wave-wizard line-3 lives at stem 15-23.5 s.
const LINE3 = [
  // pitchSemi:+7 → D3 jumps a fifth to A3. Higher than the original
  // but not the full octave (@jeffrey 2026-05-29 "id prefer just a
  // higher note" — octave was too dramatic).
  // pitchSemi removed — @jeffrey 2026-05-29 "don't like the over
  // pitching in 'i once'". Back to natural D3.
  { name: "i_l3",   midi: 50, stemAt: 15.0, stemDur: 1.0, prefDur: 0.00,
    mixAt: 200.0, holdSec: 5.0, tremolo: 0.8 },
  { name: "once",   midi: 55, stemAt: 16.0, stemDur: 1.0, prefDur: 0.05,
    mixAt: 205.0, holdSec: 10.0, tremolo: 0.7 },
  { name: "was_l3", midi: 62, stemAt: 17.0, stemDur: 1.0, prefDur: 0.06,
    mixAt: 215.0, holdSec: 5.0, tremolo: 0.8 },
  { name: "lost",   midi: 62, stemAt: 18.0, stemDur: 1.0, prefDur: 0.06,
    mixAt: 220.0, holdSec: 10.0, tremolo: 0.7 },
  { name: "but_l3", midi: 59, stemAt: 19.0, stemDur: 1.0, prefDur: 0.05,
    mixAt: 230.0, holdSec: 4.0, tremolo: 0.8 },
  { name: "now_l3", midi: 62, stemAt: 20.0, stemDur: 1.0, prefDur: 0.05,
    mixAt: 234.0, holdSec: 8.0, tremolo: 0.7 },
  { name: "am",     midi: 57, stemAt: 21.0, stemDur: 1.0, prefDur: 0.00,
    mixAt: 242.0, holdSec: 4.0, tremolo: 0.8 },
  { name: "found",  midi: 55, stemAt: 22.0, stemDur: 1.5, prefDur: 0.05,
    mixAt: 246.0, holdSec: 10.0, tremolo: 0.5 },
];
for (const s of LINE3) {
  s.file = VOCAL.replace(/\.wav$/, `-${s.name}.wav`);
}
// File paths for each.
for (const s of LINE1_REST) {
  s.file = VOCAL.replace(/\.wav$/, `-${s.name}.wav`);
}
if (!existsSync(VOCAL)) {
  console.error(`[bake-dance] missing vocal stem: ${VOCAL}`);
  if (VOICE_MODE === "wavewizard-dance") {
    console.error(`             run: node pop/big-pictures/bin/assemble-wavewizard-dance.mjs`);
  } else {
    console.error(`             run: node pop/big-pictures/bin/assemble-wavewizard.mjs`);
  }
  process.exit(1);
}
console.log(`[bake-dance] voice = ${VOICE_MODE} (${VOCAL.replace(POP + "/", "pop/")})`);

const run = (cmd, a, label, opts = {}) => {
  console.log(`\n[bake-dance] ${label}`);
  const r = spawnSync(cmd, a, { stdio: "inherit", ...opts });
  if (r.status !== 0) { console.error(`[bake-dance] FAILED: ${label}`); process.exit(1); }
};

// Generate the 3 pitch-shifted stems if missing or stale.
const shiftStems = [
  { file: VOCAL_UP,      semi:  12, label: "+12 choir" },
  { file: VOCAL_UP_HIGH, semi:  24, label: "+24 high choir" },
  { file: VOCAL_DOWN,    semi: -12, label: "-12 bass voice" },
];
for (const s of shiftStems) {
  if (!existsSync(s.file) || statSync(VOCAL).mtimeMs > statSync(s.file).mtimeMs) {
    run("rubberband", ["--pitch", String(s.semi), "--formant", "--crisp", "5",
                       VOCAL, s.file],
        `pitch-shift vocal ${s.semi >= 0 ? "+" : ""}${s.semi} (${s.label})`);
  }
}

// Generate the CONSTANT HUM stem.
if (!existsSync(VOCAL_HUM) || statSync(VOCAL).mtimeMs > statSync(VOCAL_HUM).mtimeMs) {
  const HUM_SRC = VOCAL.replace(/\.wav$/, "-hum-src.wav");
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-sseof", "-1.2", "-i", VOCAL,
                 "-ar", "48000", "-c:a", "pcm_s16le", HUM_SRC],
      "extract last 1.2 s of vocal (the 'sound' hold) for hum stem");
  run("rubberband", ["--time", "200", "--formant", "--crisp", "1",
                     HUM_SRC, VOCAL_HUM],
      "stretch hum vowel to 240 s (constant background harmony)");
}

// Generate the HELD "AH" stem — extract the first 1.0 s of the vocal
// stem (the "a-" syllable, locked to D3 by WORLD), rubberband-stretch
// to ~8.5 s so it spans 23.5 → 32.0 s in the mix.
if (!existsSync(VOCAL_AH) || statSync(VOCAL).mtimeMs > statSync(VOCAL_AH).mtimeMs) {
  const AH_SRC = VOCAL.replace(/\.wav$/, "-ah-src.wav");
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-i", VOCAL, "-t", "1.0",
                 "-ar", "48000", "-c:a", "pcm_s16le", AH_SRC],
      "extract first 1.0 s of vocal (the 'a-' for the held ah)");
  run("rubberband", ["--time", "32", "--formant", "--crisp", "1",
                     AH_SRC, VOCAL_AH],
      "stretch held 'ah' to 32 s — fades in from the very beginning");
}

// Generate the HELD "MAY" stem — but SPLIT the M consonant from the
// AY vowel so the M plays at near-real-time while only the AY gets the
// 16-bar stretch. The "ma-" syllable starts at t=1.0 s in the stem.
// We estimate the M-to-vowel transition at ~1.10 s (100 ms of M).
//   M slice    : 1.00 → 1.10 s   (100 ms, played as-is)
//   AY slice   : 1.10 → 2.00 s   (900 ms, stretched 35× to 31.5 s)
//   Total      : 0.1 s M + 31.5 s AY = 31.6 s — basically 16 bars
if (!existsSync(VOCAL_MAY) || statSync(VOCAL).mtimeMs > statSync(VOCAL_MAY).mtimeMs) {
  const MAY_M_SRC = VOCAL.replace(/\.wav$/, "-may-m-src.wav");
  const MAY_AY_SRC = VOCAL.replace(/\.wav$/, "-may-ay-src.wav");
  const MAY_AY_STRETCHED = VOCAL.replace(/\.wav$/, "-may-ay-stretched.wav");
  // 1) M consonant — first 50 ms (was 100 ms) for a snappier attack
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-ss", "1.00", "-t", "0.05", "-i", VOCAL,
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", MAY_M_SRC],
      "extract M consonant (1.00 → 1.05 s — quick M)");
  // 2) AY vowel — 1.05 → 2.00 s (slightly longer source)
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-ss", "1.05", "-t", "0.95", "-i", VOCAL,
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", MAY_AY_SRC],
      "extract AY vowel (1.05 → 2.00 s)");
  // 3) Stretch AY by 4.2× → 4.0 s (total may = 0.05 + 4.0 = ~4 s)
  run("rubberband", ["--time", "4.2", "--formant", "--crisp", "1",
                     MAY_AY_SRC, MAY_AY_STRETCHED],
      "stretch AY vowel 4.2× to 4.0 s (faster AY)");
  // 4) Concatenate M (real-time) + stretched AY
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-i", MAY_M_SRC, "-i", MAY_AY_STRETCHED,
                 "-filter_complex", "[0:a][1:a]concat=n=2:v=0:a=1[out]",
                 "-map", "[out]",
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", VOCAL_MAY],
      "concat M + stretched AY → 'may' (M fast, AY elongated)");
}

// Generate the HELD "ZING" stem — same split idea: fast Z consonant,
// long held I vowel, brief NG tail. "zing" lives at 2.0-3.0 s in the stem.
//   Z   : 2.00 → 2.06 s   (60 ms fricative, real-time)
//   I   : 2.06 → 2.86 s   (800 ms, stretched 38.5× to 30.8 s)
//   NG  : 2.86 → 3.00 s   (140 ms, real-time)
//   Total: 60 ms + 30.8 s + 140 ms = ~31 s (≈ 16 bars at 120 BPM)
if (!existsSync(VOCAL_ZING) || statSync(VOCAL).mtimeMs > statSync(VOCAL_ZING).mtimeMs) {
  const Z_SRC  = VOCAL.replace(/\.wav$/, "-zing-z-src.wav");
  const I_SRC  = VOCAL.replace(/\.wav$/, "-zing-i-src.wav");
  const NG_SRC = VOCAL.replace(/\.wav$/, "-zing-ng-src.wav");
  const I_STRETCHED = VOCAL.replace(/\.wav$/, "-zing-i-stretched.wav");
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-ss", "2.00", "-t", "0.06", "-i", VOCAL,
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", Z_SRC],
      "extract Z fricative (2.00 → 2.06 s)");
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-ss", "2.06", "-t", "0.80", "-i", VOCAL,
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", I_SRC],
      "extract I vowel (2.06 → 2.86 s)");
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-ss", "2.86", "-t", "0.14", "-i", VOCAL,
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", NG_SRC],
      "extract NG tail (2.86 → 3.00 s)");
  run("rubberband", ["--time", "17.5", "--formant", "--crisp", "1",
                     I_SRC, I_STRETCHED],
      "stretch I vowel 17.5× to 14 s (zing total ~14 s = 32-46 in mix)");
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-i", Z_SRC, "-i", I_STRETCHED, "-i", NG_SRC,
                 "-filter_complex", "[0:a][1:a][2:a]concat=n=3:v=0:a=1[out]",
                 "-map", "[out]",
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", VOCAL_ZING],
      "concat Z + stretched I + NG → 'zing' (~6 s = 3 bars)");
}

// Generate the HELD "GRACE" stem — "grace" lives at 3.0-4.0 s in stem.
//   GR  : 3.00 → 3.08 s   (80 ms cluster, real-time)
//   AY  : 3.08 → 3.85 s   (770 ms, stretched 20× to 15.4 s)
//   S   : 3.85 → 4.00 s   (150 ms s-tail, real-time)
//   Total: 0.08 + 15.4 + 0.15 = ~15.6 s ≈ 8 bars
if (!existsSync(VOCAL_GRACE) || statSync(VOCAL).mtimeMs > statSync(VOCAL_GRACE).mtimeMs) {
  const GR_SRC  = VOCAL.replace(/\.wav$/, "-grace-gr-src.wav");
  const GAY_SRC = VOCAL.replace(/\.wav$/, "-grace-ay-src.wav");
  const GS_SRC  = VOCAL.replace(/\.wav$/, "-grace-s-src.wav");
  const GAY_STRETCHED = VOCAL.replace(/\.wav$/, "-grace-ay-stretched.wav");
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-ss", "3.00", "-t", "0.08", "-i", VOCAL,
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", GR_SRC],
      "extract GR cluster (3.00 → 3.08 s)");
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-ss", "3.08", "-t", "0.77", "-i", VOCAL,
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", GAY_SRC],
      "extract grace AY vowel (3.08 → 3.85 s)");
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-ss", "3.85", "-t", "0.15", "-i", VOCAL,
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", GS_SRC],
      "extract S tail (3.85 → 4.00 s)");
  run("rubberband", ["--time", "30.0", "--formant", "--crisp", "1",
                     GAY_SRC, GAY_STRETCHED],
      "stretch grace AY 30× to 23 s (grace total ~24 s = 46-70 in mix, 12 bars)");
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-i", GR_SRC, "-i", GAY_STRETCHED, "-i", GS_SRC,
                 "-filter_complex", "[0:a][1:a][2:a]concat=n=3:v=0:a=1[out]",
                 "-map", "[out]",
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", VOCAL_GRACE],
      "concat GR + stretched AY + S → 'grace' (~5 s)");
}

// Build a held syllable: extract prefix (consonant), extract rest
// (vowel + tail), stretch rest by ratio to hit holdSec, concat.
function buildSyllable(s) {
  if (existsSync(s.file) && statSync(VOCAL).mtimeMs <= statSync(s.file).mtimeMs) return;
  const prefSrc = VOCAL.replace(/\.wav$/, `-${s.name}-pref.wav`);
  const restSrc = VOCAL.replace(/\.wav$/, `-${s.name}-rest.wav`);
  const restStretched = VOCAL.replace(/\.wav$/, `-${s.name}-stretched.wav`);
  const concatOut = s.pitchSemi
    ? VOCAL.replace(/\.wav$/, `-${s.name}-prepitch.wav`)
    : s.file;
  const restStart = s.stemAt + s.prefDur;
  const restDur = s.stemDur - s.prefDur;
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-ss", restStart.toFixed(3), "-t", restDur.toFixed(3), "-i", VOCAL,
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", restSrc],
      `extract ${s.name} rest (${restStart.toFixed(2)} → ${(restStart+restDur).toFixed(2)} s)`);
  const stretchRatio = (s.holdSec - s.prefDur) / restDur;
  run("rubberband", ["--time", stretchRatio.toFixed(3), "--formant", "--crisp", "1",
                     restSrc, restStretched],
      `stretch ${s.name} rest ${stretchRatio.toFixed(2)}× to ${(restDur * stretchRatio).toFixed(2)} s`);
  // CRITICAL FIX: ffmpeg `-t 0.000` reads to EOF (not zero), so when
  // prefDur=0 the prefix file would contain the whole rest of the
  // stem and get concat'd → syllable played wrong content and got
  // cut off. ("line 3 never plays" diagnosis @jeffrey 2026-05-29.)
  // Skip the prefix extract + concat entirely when prefDur is zero.
  if (s.prefDur > 0) {
    run("ffmpeg", ["-y", "-loglevel", "error",
                   "-ss", s.stemAt.toFixed(3), "-t", s.prefDur.toFixed(3), "-i", VOCAL,
                   "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", prefSrc],
        `extract ${s.name} prefix (${s.stemAt.toFixed(2)} → ${(s.stemAt+s.prefDur).toFixed(2)} s)`);
    run("ffmpeg", ["-y", "-loglevel", "error",
                   "-i", prefSrc, "-i", restStretched,
                   "-filter_complex", "[0:a][1:a]concat=n=2:v=0:a=1[out]",
                   "-map", "[out]",
                   "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", concatOut],
        `concat ${s.name} prefix + stretched rest (~${s.holdSec.toFixed(1)} s at t=${s.mixAt.toFixed(1)} s)`);
  } else {
    run("cp", [restStretched, concatOut],
        `${s.name} no-prefix copy (~${s.holdSec.toFixed(1)} s at t=${s.mixAt.toFixed(1)} s)`);
  }
  if (s.pitchSemi) {
    run("rubberband", ["--pitch", String(s.pitchSemi), "--formant", "--crisp", "5",
                       concatOut, s.file],
        `pitch-shift ${s.name} ${s.pitchSemi > 0 ? "+" : ""}${s.pitchSemi} semi`);
  }
}
for (const s of LINE1_REST) buildSyllable(s);
for (const s of LINE2)      buildSyllable(s);
for (const s of LINE3)      buildSyllable(s);

// Build PVC syllables from amazing-vocal.mp3 (raw jeffrey-pvc TTS).
// Same pitch-shift hook for PVC syllables.
function buildPvcSyllable(s) {
  if (existsSync(s.file) && statSync(PVC_SOURCE).mtimeMs <= statSync(s.file).mtimeMs) return;
  const prefSrc = s.file.replace(/\.wav$/, "-pref.wav");
  const restSrc = s.file.replace(/\.wav$/, "-rest.wav");
  const restStretched = s.file.replace(/\.wav$/, "-stretched.wav");
  const restDur = s.stemDur - s.prefDur;
  const restStart = s.stemAt + s.prefDur;
  // 1) prefix (if any)
  if (s.prefDur > 0) {
    run("ffmpeg", ["-y", "-loglevel", "error",
                   "-ss", s.stemAt.toFixed(4), "-t", s.prefDur.toFixed(4),
                   "-i", PVC_SOURCE,
                   "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", prefSrc],
        `extract pvc ${s.name} prefix`);
  }
  // 2) rest (vowel)
  run("ffmpeg", ["-y", "-loglevel", "error",
                 "-ss", restStart.toFixed(4), "-t", restDur.toFixed(4),
                 "-i", PVC_SOURCE,
                 "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", restSrc],
      `extract pvc ${s.name} rest`);
  // 3) stretch rest
  const stretchRatio = (s.holdSec - s.prefDur) / restDur;
  run("rubberband", ["--time", stretchRatio.toFixed(3), "--formant",
                     "--crisp", "1", restSrc, restStretched],
      `stretch pvc ${s.name} ${stretchRatio.toFixed(2)}× to ${(restDur*stretchRatio).toFixed(2)}s`);
  // 4) concat (skip prefix if 0 dur)
  const concatOut = s.pitchSemi
    ? s.file.replace(/\.wav$/, "-prepitch.wav")
    : s.file;
  if (s.prefDur > 0) {
    run("ffmpeg", ["-y", "-loglevel", "error",
                   "-i", prefSrc, "-i", restStretched,
                   "-filter_complex", "[0:a][1:a]concat=n=2:v=0:a=1[out]",
                   "-map", "[out]",
                   "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", concatOut],
        `concat pvc ${s.name} pref+rest`);
  } else {
    run("cp", [restStretched, concatOut], `pvc ${s.name} (no prefix, copy stretched)`);
  }
  // 5) optional pitch shift
  if (s.pitchSemi) {
    run("rubberband", ["--pitch", String(s.pitchSemi), "--formant", "--crisp", "5",
                       concatOut, s.file],
        `pitch-shift pvc ${s.name} ${s.pitchSemi > 0 ? "+" : ""}${s.pitchSemi} semi`);
  }
}
for (const s of PVC_LINE1) buildPvcSyllable(s);
for (const s of PVC_LINE2) buildPvcSyllable(s);
for (const s of PVC_LINE3) buildPvcSyllable(s);

// 1 — build engine if stale
const needsBuild = !existsSync(ENGINE) ||
  statSync(ENGINE_C).mtimeMs > statSync(ENGINE).mtimeMs;
if (needsBuild) run("bash", [`${HERE}/build-dance.sh`], "build C engine");

// 2 — render bed
run(ENGINE, ["--out", BED_WAV, ...engineExtra], "C engine render (loukeman bed)");

// 3 — MIX vocal THREE TIMES across the track, each pass with a
// different vibe so the lyric keeps surfacing without sounding static.
//   Pass 1 @ 8 s  — DRY MAIN. Light aecho, the listener hears the
//                   lyric clearly the first time through.
//   Pass 2 @ 100 s — BREAK section, kick + bass dropped on bed. Heavy
//                    aecho + lowpass for a dub-y, underwater feel —
//                    the lyric floats in the atmosphere.
//   Pass 3 @ 168 s — DROP 2 return. Mid-aecho but pitched slightly
//                    bright via highshelf — the lyric punches back in
//                    with the kick.
//   (Outro 208-240 s is bed-only fade, no vocal — lets the bell tolls
//    + reverb tail close the piece.)
// Track length extended 150 → 240 s (@jeffrey 2026-05-29 "all
// syllables slower its okay if the track gets a lot longer").
// Zing 14 s, grace 24 s, lines 1-4 pushed proportionally back.
// 5:00 = 300 s (@jeffrey 2026-05-29 "still too short" after 210).
// SHORT mode trims to 120 s — the first 2 min of the bed is a complete
// intro→build→drop1→break arc, perfect for the packed-vocal short cut.
const TOTAL_SEC = SHORT ? 120.0 : 300.0;
const BED_GAIN = 0.85;

// HELD-VOWEL BEDS — the giant may/zing/grace drones ARE the "all
// stretched" character. SHORT silences them (HELD=0); the packed
// per-syllable arrays carry the full lyric instead. The intro "ah"
// stays (covers the "A" of Amazing) but hands off to "ma" at AH_OUT.
const HELD = SHORT ? 0 : 1;
const AH_OUT = SHORT ? 7.0 : 27.5;    // held "ah" hands off to "ma" (on a beat)

// VOCAL PRESENCE + DICTION — "more up front / close to the speaker" and
// "can't hear the first lines pronounced" (@jeffrey). Reverb = distance,
// so SHORT dries the vocal echoes hard and sits the words forward. For
// intelligibility the PVC voice (real recorded consonants — clear
// diction) now LEADS over the wavewizard voice (stretched vowels, mushy
// consonants), and the power-harmonies are thinned so they stop smearing
// the consonants of the lead.
const VOX_GAIN     = SHORT ? 5.6 : 5.4;                       // wavewizard (body)
const PVC_GAIN_MUL = SHORT ? 3.6 : 2.1;                       // pvc lead (diction)
const VOX_ECHO = SHORT ? "aecho=0.7:0.12:45|130:0.08|0.05"    // dry, close
                       : "aecho=0.5:0.28:100|260:0.16|0.12";  // roomy
const PVC_ECHO = SHORT ? "aecho=0.7:0.10:40|120:0.07|0.04"
                       : "aecho=0.5:0.25:80|220:0.14|0.10";
const BED_PRESENCE  = SHORT ? 0.80 : 1.0;   // bed trim so vox leads
const HARM_PRESENCE = SHORT ? 0.55 : 1.0;   // thin harmonies → clearer consonants

// Main vocal phrase DISABLED (@jeffrey 2026-05-29 "don't restart after
// zing"). Each pass's startMs is past TOTAL_SEC so the layers get
// trimmed to silence. Held syllables (ah/may/zing) carry the vocal.
// PASSES startMs pushed to 9_999_000+ so they're PERMANENTLY past
// any plausible TOTAL_SEC. The 250000-252000 values were inside the
// 300 s track and produced "tons of layered vocals at 4:11"
// (@jeffrey 2026-05-29).
const PASSES = [
  { startMs: 9999000, gain: 5.4,
    fx: "aecho=0.6:0.55:80|240:0.32|0.20",
    sc: "threshold=0.05:ratio=10:attack=4:release=180:makeup=2" },
  { startMs: 9999100, gain: 4.6,
    fx: "aecho=0.75:0.7:220|520:0.5|0.4,lowpass=f=3000,highpass=f=180",
    sc: "threshold=0.04:ratio=14:attack=8:release=380:makeup=3" },
  { startMs: 9999200, gain: 5.0,
    fx: "aecho=0.65:0.6:90|280:0.35|0.25,highshelf=f=4500:g=2",
    sc: "threshold=0.06:ratio=18:attack=2:release=90:makeup=2.5" },
];

// MULTI-LAYER VOICE STACK — each pass renders the MAIN vocal + 3
// pitched harmony layers stacked on top. Bumped choir ratio (was 0.55
// "too airy") + added high (+24) sparkle + low (-12) bass body.
const LAYERS = [
  { name: "main",   file: VOCAL,         gainRatio: 1.00,
    fxExtra: "" },                                   // dry pass-through
  { name: "choir",  file: VOCAL_UP,      gainRatio: 0.75,
    fxExtra: ",highpass=f=200,lowpass=f=6500" },     // less airy than before
  { name: "high",   file: VOCAL_UP_HIGH, gainRatio: 0.32,
    fxExtra: ",highpass=f=500,lowpass=f=7500" },     // sparkle on top
  { name: "bass",   file: VOCAL_DOWN,    gainRatio: 0.55,
    fxExtra: ",lowpass=f=2200,highpass=f=70" },      // body on bottom
];

// FIELD-RECORDED BELLS from trancepenta. Three samples (2.5 / 2.6 /
// 1.9 s) with real bell harmonic structure — much richer than the
// synth glock. Layered over INTRO (0..6 s — replaces/augments the
// synth tolls), BREAK (96..128 s — atmospheric texture), and OUTRO
// (208..240 s — closing peals).
const BELL_FILES = [
  resolve(REPO, "pop/dance/out/.bell-01.wav"),
  resolve(REPO, "pop/dance/out/.bell-02.wav"),
  resolve(REPO, "pop/dance/out/.bell-03.wav"),
];
// pitch_semi = semitone offset (0 = original pitch). chop_ms = cut to
// at most N ms (full length if null). As the bells repeat across the
// track they get more chopped + pitch-shifted for a "fun" texture
// (@jeffrey 2026-05-29).
const BELL_HITS = [
  // intro — first bell at t=0.2 s REMOVED (@jeffrey 2026-05-29 "rm
  // the first loud sample"). Track now opens softer: bed + ah swell
  // alone for the first 2.4 s, then bell 2 enters.
  { bellIdx: 1, startMs:   2400, gain: 0.78, pan: +0.20, reverb: 0.45,
    pitch_semi:  0, chop_ms: null },
  { bellIdx: 2, startMs:   4600, gain: 0.85, pan: -0.10, reverb: 0.45,
    pitch_semi:  0, chop_ms: null },
  // TRANSITION MARKER bell at 27.5 s — coincides with AH→MAY switch.
  { bellIdx: 1, startMs:  27500, gain: 0.65, pan:  0.00, reverb: 0.55,
    pitch_semi:  0, chop_ms: null },
  // break — mild pitch + chop
  { bellIdx: 1, startMs:  96000, gain: 0.42, pan: -0.30, reverb: 0.65,
    pitch_semi: -2, chop_ms: 1800 },
  { bellIdx: 0, startMs: 104000, gain: 0.44, pan: +0.30, reverb: 0.65,
    pitch_semi: +3, chop_ms: 1500 },
  { bellIdx: 2, startMs: 112000, gain: 0.42, pan: -0.20, reverb: 0.65,
    pitch_semi: -4, chop_ms: 1300 },
  { bellIdx: 1, startMs: 120000, gain: 0.46, pan: +0.20, reverb: 0.65,
    pitch_semi: +5, chop_ms: 1000 },
  // outro — heavy chop + dramatic pitch
  { bellIdx: 0, startMs: 208000, gain: 0.55, pan:  0.00, reverb: 0.70,
    pitch_semi: +7, chop_ms:  800 },
  { bellIdx: 1, startMs: 216000, gain: 0.52, pan: -0.25, reverb: 0.70,
    pitch_semi: -5, chop_ms:  600 },
  { bellIdx: 2, startMs: 224000, gain: 0.50, pan: +0.25, reverb: 0.70,
    pitch_semi: +12, chop_ms: 500 },
  { bellIdx: 0, startMs: 232000, gain: 0.58, pan:  0.00, reverb: 0.80,
    pitch_semi: -7, chop_ms: 1000 },
];

// We need the bed available BOTH as the main mix AND as N sidechain
// triggers (one per vocal pass). asplit fans out a single input into
// multiple branches.
// Input layout: [0:a]=bed, [1..3]=vocal stem (x3 for 3 passes),
// [4..6]=bell files (3 unique files), each bell hit references its file
// by its input index for the adelay.
const N_PASSES = PASSES.length;                    // 3
const N_LAYERS = LAYERS.length;                    // 4 (main/choir/high/bass)
// Input layout: [0]=bed, then for each LAYER: N_PASSES copies of the
// pitched stem, then BELL_FILES.
const FIRST_BELL_INPUT = 1 + N_LAYERS * N_PASSES;  // 13
// Returns ffmpeg input index for layer L pass P.
const passInputIdx = (layerIdx, passIdx) =>
  1 + layerIdx * N_PASSES + passIdx;

const bellChains = BELL_HITS.map((h, k) => {
  const inputIdx = FIRST_BELL_INPUT + h.bellIdx;
  // Pan: use stereo + pan filter (simpler than per-channel adelay).
  return `[${inputIdx}:a]adelay=${h.startMs}|${h.startMs},` +
         `aresample=48000,aformat=channel_layouts=stereo,` +
         `pan=stereo|c0=${(1 - 0.5 * h.pan).toFixed(3)}*c0|` +
         `c1=${(1 + 0.5 * h.pan).toFixed(3)}*c1,` +
         `volume=${h.gain},` +
         // light aecho per bell — adds reverb-y tail in proportion to its position
         `aecho=0.55:${(h.reverb * 0.65).toFixed(2)}:180|420:0.25|0.18` +
         `[bell${k}]`;
});

// Long 30 s fade-in on the bed. Each pass×layer needs its own
// sidechain branch of the bed → asplit (1 main + N_PASSES × N_LAYERS).
const N_SIDECHAIN_BRANCHES = N_PASSES * N_LAYERS;
const N_BED_BRANCHES = 1 + N_SIDECHAIN_BRANCHES;
// Tag for sidechain output: sc_<layer>_<pass>
const scTag = (layerIdx, passIdx) =>
  `sc${LAYERS[layerIdx].name}${passIdx + 1}`;
// Tag for layer-pass output: lp_<layer>_<pass>
const lpTag = (layerIdx, passIdx) =>
  `lp${LAYERS[layerIdx].name}${passIdx + 1}`;

const layerChains = [];
for (let l = 0; l < N_LAYERS; l++) {
  for (let pIdx = 0; pIdx < N_PASSES; pIdx++) {
    const p = PASSES[pIdx];
    const lay = LAYERS[l];
    const gain = (p.gain * lay.gainRatio).toFixed(3);
    const inputIdx = passInputIdx(l, pIdx);
    const preTag = `${lpTag(l, pIdx)}_pre`;
    // delay → volume → fx + layer-specific EQ
    layerChains.push(
      `[${inputIdx}:a]adelay=${p.startMs}|${p.startMs},` +
      `volume=${gain},${p.fx}${lay.fxExtra}[${preTag}]`
    );
    // sidechain ducked by the kick (bed)
    layerChains.push(
      `[${preTag}][${scTag(l, pIdx)}]sidechaincompress=${p.sc}[${lpTag(l, pIdx)}]`
    );
  }
}

const allLayerTags = [];
for (let l = 0; l < N_LAYERS; l++)
  for (let pIdx = 0; pIdx < N_PASSES; pIdx++)
    allLayerTags.push(`[${lpTag(l, pIdx)}]`);

const allScTags = [];
for (let l = 0; l < N_LAYERS; l++)
  for (let pIdx = 0; pIdx < N_PASSES; pIdx++)
    allScTags.push(`[${scTag(l, pIdx)}]`);

// Hum stem index — appended after the layer copies + bells.
const HUM_INPUT_IDX = FIRST_BELL_INPUT + BELL_FILES.length;
// AH stem index — appended after the hum stem.
const AH_INPUT_IDX  = HUM_INPUT_IDX + 1;
// MAY stem index — appended after the ah stem.
const MAY_INPUT_IDX  = AH_INPUT_IDX + 1;
// ZING stem index — appended after the may stem.
const ZING_INPUT_IDX  = MAY_INPUT_IDX + 1;
// GRACE stem index — appended after the zing stem.
const GRACE_INPUT_IDX = ZING_INPUT_IDX + 1;
// LINE1_REST input indices — appended after grace.
LINE1_REST.forEach((s, i) => { s.inputIdx = GRACE_INPUT_IDX + 1 + i; });
// LINE2 indices
const FIRST_LINE2_INPUT = GRACE_INPUT_IDX + 1 + LINE1_REST.length;
LINE2.forEach((s, i) => { s.inputIdx = FIRST_LINE2_INPUT + i; });
// LINE3 indices
const FIRST_LINE3_INPUT = FIRST_LINE2_INPUT + LINE2.length;
LINE3.forEach((s, i) => { s.inputIdx = FIRST_LINE3_INPUT + i; });
// PVC_LINE1 input indices
const FIRST_PVC1_INPUT = FIRST_LINE3_INPUT + LINE3.length;
PVC_LINE1.forEach((s, i) => { s.inputIdx = FIRST_PVC1_INPUT + i; });
// PVC_LINE2 input indices
const FIRST_PVC2_INPUT = FIRST_PVC1_INPUT + PVC_LINE1.length;
PVC_LINE2.forEach((s, i) => { s.inputIdx = FIRST_PVC2_INPUT + i; });
// PVC_LINE3 input indices
const FIRST_PVC3_INPUT = FIRST_PVC2_INPUT + PVC_LINE2.length;
PVC_LINE3.forEach((s, i) => { s.inputIdx = FIRST_PVC3_INPUT + i; });

// ─── LINE 4 — "Was blind but now I see" (118-142 s) ──────────────────
const LINE4 = [
  { name: "was_l4",   midi: 59, stemAt: 23.5, stemDur: 1.0, prefDur: 0.06,
    mixAt: 256.0, holdSec: 5.0, tremolo: 0.8 },
  { name: "blind",    midi: 62, stemAt: 24.5, stemDur: 1.0, prefDur: 0.06,
    mixAt: 261.0, holdSec: 10.0, tremolo: 0.7 },
  { name: "but_l4",   midi: 62, stemAt: 25.5, stemDur: 1.0, prefDur: 0.05,
    mixAt: 271.0, holdSec: 4.0, tremolo: 0.8 },
  { name: "now_l4",   midi: 59, stemAt: 26.5, stemDur: 1.0, prefDur: 0.05,
    mixAt: 275.0, holdSec: 8.0, tremolo: 0.7 },
  { name: "i_l4",     midi: 57, stemAt: 27.5, stemDur: 1.0, prefDur: 0.00,
    mixAt: 283.0, holdSec: 4.0, tremolo: 0.8 },
  { name: "see",      midi: 55, stemAt: 28.5, stemDur: 1.5, prefDur: 0.04,
    mixAt: 287.0, holdSec: 14.0, tremolo: 0.5 },
];
for (const s of LINE4) {
  s.file = VOCAL.replace(/\.wav$/, `-${s.name}.wav`);
  buildSyllable(s);
}
const FIRST_LINE4_INPUT = FIRST_PVC3_INPUT + PVC_LINE3.length;
LINE4.forEach((s, i) => { s.inputIdx = FIRST_LINE4_INPUT + i; });

const PVC_LINE4 = [
  { name: "was_l4",  stemAt: 5.573, stemDur: 0.116, prefDur: 0.05,
    mixAt: 256.0, holdSec: 5.0, tremolo: 0.8, gain: 1.10 },
  { name: "blind",   stemAt: 5.747, stemDur: 0.430, prefDur: 0.06,
    mixAt: 261.0, holdSec: 10.0, tremolo: 0.7, gain: 1.15 },
  { name: "but_l4",  stemAt: 6.455, stemDur: 0.174, prefDur: 0.04,
    mixAt: 271.0, holdSec: 4.0, tremolo: 0.8, gain: 1.10 },
  { name: "now_l4",  stemAt: 6.676, stemDur: 0.232, prefDur: 0.04,
    mixAt: 275.0, holdSec: 8.0, tremolo: 0.7, gain: 1.15 },
  { name: "i_l4",    stemAt: 6.989, stemDur: 0.035, prefDur: 0.00,
    mixAt: 283.0, holdSec: 4.0, tremolo: 0.8, gain: 1.10 },
  { name: "see",     stemAt: 7.140, stemDur: 0.662, prefDur: 0.04,
    mixAt: 287.0, holdSec: 14.0, tremolo: 0.5, gain: 1.25 },
];
for (const s of PVC_LINE4) {
  s.file = VOCAL.replace(/wavewizard-dance\.wav$/, `pvc-${s.name}.wav`);
  buildPvcSyllable(s);
}
const FIRST_PVC4_INPUT = FIRST_LINE4_INPUT + LINE4.length;
PVC_LINE4.forEach((s, i) => { s.inputIdx = FIRST_PVC4_INPUT + i; });

// ─── SHORT-MIX TIMING REMAP ──────────────────────────────────────────
// In SHORT mode, compress all 4 hymn lines from their longlong spread
// (mixAt 27.5 → 301 s) into a tight 14 → 116 s window with holds scaled
// ~0.37× (grace 24 s → ~9 s). The full hymn is sung in 2 min, far less
// stretched. Must run BEFORE ALL_SYLLABLES/ALL_PVC/HARMONIES read these
// values (HARMONIES copies mixAt/holdSec). stemAt/stemDur are untouched
// — the stems are already built; only placement + hold change.
if (SHORT) {
  const ORIG_START = 27.5, ORIG_END = 301.0;   // longlong vocal span
  const DST_START  = 7.0,  DST_END  = 100.0;   // tighter, more up-front window
  const SCALE = (DST_END - DST_START) / (ORIG_END - ORIG_START); // ≈0.340
  // BEAT-GRID ONSETS, LEGATO HOLDS — bed is 120 BPM 4/4, grid at t=0:
  //   beat = 0.5 s, bar = 2.0 s. Quantize each syllable's ONSET (the
  //   plosive attack at mixAt) to the beat grid so words start tight with
  //   the kick — but DO NOT clip the release short. Each syllable
  //   SUSTAINS until the next one in its line enters (legato), so the
  //   line flows as one phrase instead of choppy clipped words. The last
  //   syllable of each line keeps its (scaled) natural hold then rests.
  const BEAT = 60 / 120;                       // 0.5 s
  const q = (t) => Math.round(t / BEAT) * BEAT;
  const OVERLAP = 0.25;                         // 1/8-note bleed into next word
  const MAX_TAIL = 6.0;                         // cap the final-word drone
  const remapLine = (arr) => {
    // 1) grid-quantize onsets
    for (const s of arr)
      s.mixAt = +q(DST_START + (s.mixAt - ORIG_START) * SCALE).toFixed(3);
    // 2) legato: hold each word until the next onset (+overlap)
    for (let i = 0; i < arr.length; i++) {
      const s = arr[i];
      const next = arr[i + 1];
      const natural = Math.min(MAX_TAIL, Math.max(1.0, s.holdSec * SCALE));
      s.holdSec = next
        ? +Math.max(BEAT, next.mixAt - s.mixAt + OVERLAP).toFixed(3)
        : +natural.toFixed(3);
    }
  };
  for (const arr of [LINE1_REST, LINE2, LINE3, LINE4,
                     PVC_LINE1, PVC_LINE2, PVC_LINE3, PVC_LINE4])
    remapLine(arr);
}

const ALL_SYLLABLES = [...LINE1_REST, ...LINE2, ...LINE3, ...LINE4];
const ALL_PVC = [...PVC_LINE1, ...PVC_LINE2, ...PVC_LINE3, ...PVC_LINE4];

// ─── POWER HARMONIES ──────────────────────────────────────────────────
// Pre-baked pitched copies of every syllable via rubberband (preserves
// duration so no ghost-doubling). For each syllable we generate:
//   +7  — perfect fifth above  (bright "power")
//   +12 — octave above         (sparkle)
//   -12 — octave below         (body / weight)
// Mixed at low gain so each is a backing layer, not the lead.
// @jeffrey 2026-05-29 "bring more power harmony to the vocals"
const HARM_SEMIS = [7, 12, -12];

function buildHarmony(s, semi) {
  const tag = semi >= 0 ? `up${semi}` : `dn${-semi}`;
  const harmFile = s.file.replace(/\.wav$/, `-h${tag}.wav`);
  if (existsSync(harmFile) && statSync(s.file).mtimeMs <= statSync(harmFile).mtimeMs) {
    return harmFile;
  }
  run("rubberband",
      ["--pitch", String(semi), "--formant", "--crisp", "5", s.file, harmFile],
      `harm ${s.name} ${semi > 0 ? "+" : ""}${semi}`);
  return harmFile;
}

const HARMONIES = [];
for (const src of [...ALL_SYLLABLES, ...ALL_PVC]) {
  for (const semi of HARM_SEMIS) {
    const file = buildHarmony(src, semi);
    HARMONIES.push({
      name: `${src.name}_${semi >= 0 ? `up${semi}` : `dn${-semi}`}`,
      file, mixAt: src.mixAt, holdSec: src.holdSec, tremolo: src.tremolo,
      semi,
      // Harmony gains: octave-up sparkle quietest, fifth brightest, octave-down warmest
      gain: semi === 7 ? 0.45 : semi === 12 ? 0.30 : 0.55,
      // Slight pan spread per harmony so they don't cluster center
      pan: semi === 7 ? -0.20 : semi === 12 ? +0.25 : 0.0,
    });
  }
}

// ─── WHALES + PACHINKO ───────────────────────────────────────────────
// humpback-sfx is the chosen clean moo (BBC-curated humpback recording).
// blue-whale-atlantic1 = long low blue-whale call as sub-bass texture.
// pachinko = BBC slot-machine jackpot bells (07032199, 9 s).
// NOTE: BBC SFX is RemArc license — personal/edu only, NOT commercial.
// Swap before any DistroKid release. Wikimedia whales are PD/CC0.
const WHALES_DIR = resolve(REPO, "pop/samples/whales-wikimedia");
const OCEAN_DIR  = resolve(REPO, "pop/samples/ocean-ia-cc0");
// CC0 pachinko + antique slot (Designer's Choice UCS via Internet Archive).
// Swapped in for the BBC pachinko-bbc clips — same casino-bell vibe,
// commercially safe. The "floor" file is a 150 s window from t=30..180
// of the source recording (densest bell + coin clatter activity per FFT
// scan). Bells in the recording ring G6/C6/F6 — same key as the vocal
// (rootMel=G3). NO PITCH-SHIFT NEEDED.
const PACH_DIR   = resolve(REPO, "pop/samples/pachinko-ia-cc0");
// REAL OCEAN BED — replaces the synth pink-noise crashes. Continuous
// surf rolling under the entire track for "on the ocean" feel + two
// seagull atmospheres placed at calm moments.
const OCEAN_HITS = [
  // Continuous surf bed — Okeechobee source is 332 s, plenty for 240 s.
  // Start at 30 s of source (skip ramp-in), runs for whole track.
  { file: resolve(OCEAN_DIR, "okeechobee-surf.wav"),
    name: "surf_bed", startMs: 0, gain: 0.11, pan: 0.0,
    fx: "atrim=start=30:duration=302,asetpts=PTS-STARTPTS," +
        "lowpass=f=5500,highpass=f=120," +
        "equalizer=f=900:t=q:w=1.6:g=-2.5," +
        "volume=eval=frame:volume='min(1,t/4)*max(0.4,min(1,(300-t)/8))'" },
  // Mid-track wave crash in the line-2 → line-3 gap (~150 s)
  { file: resolve(OCEAN_DIR, "ocean-waves-crushing.wav"),
    name: "gap_wave", startMs: 190000, gain: 0.18, pan: -0.20,
    fx: "atrim=start=10:duration=14,asetpts=PTS-STARTPTS," +
        "lowpass=f=6000,highpass=f=180,aecho=0.5:0.35:80|200:0.18|0.12" },
  { file: resolve(OCEAN_DIR, "seagulls-distant-surf.wav"),
    name: "intro_gulls", startMs: 8000, gain: 0.03, pan: +0.25,
    fx: "atrim=duration=32,asetpts=PTS-STARTPTS," +
        "highpass=f=400,lowpass=f=7000," +
        "volume=eval=frame:volume='min(1,t/3)*max(0.3,min(1,(32-t)/4))'" },
  { file: resolve(OCEAN_DIR, "seagulls-distant-surf.wav"),
    name: "outro_gulls", startMs: 280000, gain: 0.04, pan: -0.20,
    fx: "atrim=start=8:duration=18,asetpts=PTS-STARTPTS," +
        "highpass=f=420,lowpass=f=7000,aecho=0.5:0.4:120|320:0.25|0.18" },
];

const WHALE_HITS = [
  { file: resolve(WHALES_DIR, "blue-whale-atlantic1.wav"),
    name: "intro_blue", startMs: 0, gain: 0.20, pan: 0.0,
    fx: "lowpass=f=380,volume=eval=frame:volume='min(1,t/6)*max(0,1-(t-18)/4)'" },
  { file: resolve(WHALES_DIR, "humpback-sfx.wav"),
    name: "predrop_moo", startMs: 22000, gain: 0.55, pan: -0.10,
    fx: "highpass=f=120,lowpass=f=4500,aecho=0.6:0.5:280|620:0.4|0.3" },
  // Mid-track gap — pushed to line-2 → line-3 boundary (~145 s)
  { file: resolve(WHALES_DIR, "humpback-sfx.wav"),
    name: "gap_moo", startMs: 192000, gain: 0.60, pan: +0.15,
    fx: "highpass=f=140,lowpass=f=4000,aecho=0.65:0.55:320|680:0.45|0.35" },
  { file: resolve(WHALES_DIR, "humpback-sfx.wav"),
    name: "outro_moo", startMs: 284000, gain: 0.40, pan: 0.0,
    fx: "lowpass=f=3200,aecho=0.7:0.6:380|780:0.5|0.4" },
];
// RECORD SCRATCH FX in the last minute — synthesized via aevalsrc
// (no sample file needed). Three quick scratches as transitions in
// the last minute: one at 90s (between line 2 and line 3), one at
// 118s (line 3 → line 4), one at 142s (line 4 → outro). Synthesized
// noise burst with a fast downward pitch sweep — classic vinyl
// scratch character.
// Record scratches now mark line boundaries in the longer 240 s
// layout: line-1→line-2 (102 s), line-2→line-3 (152 s), line-3→
// line-4 (198 s).
const SCRATCH_HITS = [
  { name: "scratch_125", startMs: 125000, gain: 0.22, pan:  0.0,  durMs: 360 },
  { name: "scratch_195", startMs: 195000, gain: 0.30, pan: -0.25, durMs: 420 },
  { name: "scratch_253", startMs: 253000, gain: 0.20, pan: +0.25, durMs: 320 },
];

const PACHINKO_HITS = [
  // FLOOR BED — full 150 s pachinko parlor ambience under the entire
  // track at very low gain. EQ favors the bell band (1.2-2.2 kHz where
  // G6/C6/F6 bells sit) and scoops the embedded electronic music bed
  // (200-600 Hz) so it doesn't fight the kick + bass. Slight slap-echo
  // for room sense.
  { file: resolve(PACH_DIR, "pachinko-floor-300s.wav"),
    name: "floor_bed", startMs: 0, gain: 0.13, pan: 0.0,
    fx: "highpass=f=350,equalizer=f=300:t=q:w=1.2:g=-3.0," +
        "equalizer=f=1600:t=q:w=1.4:g=3.0," +
        "equalizer=f=900:t=q:w=1.6:g=-2.0," +
        "aecho=0.5:0.35:60|140:0.18|0.12,volume=eval=frame:" +
        "volume='min(1,t/8)*max(0.35,min(1,(150-t)/6))'" },
  // BRIGHTER bell punch at the 32 s drop — same source, but second
  // tighter window with EQ ringing the bells harder
  { file: resolve(PACH_DIR, "pachinko-floor-300s.wav"),
    name: "drop_bells", startMs: 0, gain: 0.18, pan: +0.20,
    fx: "atrim=start=28:duration=10,asetpts=PTS-STARTPTS,adelay=32000|32000," +
        "highpass=f=900,equalizer=f=1600:t=q:w=1.2:g=4.5," +
        "aecho=0.55:0.4:90|240:0.3|0.22" },
];
const FIRST_OCEAN_INPUT = FIRST_PVC4_INPUT + PVC_LINE4.length;
OCEAN_HITS.forEach((o, i) => { o.inputIdx = FIRST_OCEAN_INPUT + i; });
const FIRST_WHALE_INPUT = FIRST_OCEAN_INPUT + OCEAN_HITS.length;
WHALE_HITS.forEach((w, i) => { w.inputIdx = FIRST_WHALE_INPUT + i; });
const FIRST_PACH_INPUT = FIRST_WHALE_INPUT + WHALE_HITS.length;
PACHINKO_HITS.forEach((p, i) => { p.inputIdx = FIRST_PACH_INPUT + i; });
const FIRST_HARM_INPUT = FIRST_PACH_INPUT + PACHINKO_HITS.length;
HARMONIES.forEach((h, i) => { h.inputIdx = FIRST_HARM_INPUT + i; });

// HARMONY chains — simple adelay + volume envelope, no flange/aecho
// (those would compound across all the harmony layers). Slow tremolo
// matches the parent syllable. Hard pan per semi keeps them spread.
const harmonyChains = HARMONIES.map((h) => {
  const fadeOut = (h.mixAt + h.holdSec - 0.15).toFixed(2);
  return `[${h.inputIdx}:a]adelay=${(h.mixAt*1000)|0}|${(h.mixAt*1000)|0},` +
    `aresample=48000,aformat=channel_layouts=stereo,` +
    `pan=stereo|c0=${(1 - 0.5 * h.pan).toFixed(3)}*c0|c1=${(1 + 0.5 * h.pan).toFixed(3)}*c1,` +
    `volume=eval=frame:volume='${(h.gain*HARM_PRESENCE).toFixed(2)}*(0.8+0.2*sin(2*PI*${h.tremolo}*t))',` +
    `afade=t=in:st=${h.mixAt}:d=0.20:curve=qsin,` +
    `afade=t=out:st=${fadeOut}:d=0.15:curve=tri,` +
    `aecho=0.4:0.20:120|280:0.10|0.08,` +
    `atrim=duration=${TOTAL_SEC}[h_${h.name}]`;
});

const oceanChains = OCEAN_HITS.map((o) =>
  `[${o.inputIdx}:a]${o.fx},adelay=${o.startMs}|${o.startMs},` +
  `aresample=48000,aformat=channel_layouts=stereo,` +
  `pan=stereo|c0=${(1 - 0.5 * o.pan).toFixed(3)}*c0|c1=${(1 + 0.5 * o.pan).toFixed(3)}*c1,` +
  `volume=${o.gain},` +
  `atrim=duration=${TOTAL_SEC}[o_${o.name}]`
);
// Synthesized record-scratch chain: aevalsrc generates a downward
// pitch-swept noise burst, then panned + delayed into place.
const scratchChains = SCRATCH_HITS.map((s) => {
  const dur = (s.durMs / 1000).toFixed(3);
  return `aevalsrc='0.85*sin(2*PI*((2400-t*5800)*max(0\\,1-t*4))*t)*` +
         `(0.7+0.3*random(${s.startMs}))*exp(-t*3.5)':` +
         `d=${dur}:s=48000,` +
         `aformat=channel_layouts=stereo,` +
         `pan=stereo|c0=${(1 - 0.5 * s.pan).toFixed(3)}*c0|c1=${(1 + 0.5 * s.pan).toFixed(3)}*c1,` +
         `volume=${s.gain},` +
         `adelay=${s.startMs}|${s.startMs},` +
         `atrim=duration=${TOTAL_SEC}[scr_${s.name}]`;
});
const whaleChains = WHALE_HITS.map((w) =>
  `[${w.inputIdx}:a]adelay=${w.startMs}|${w.startMs},` +
  `aresample=48000,aformat=channel_layouts=stereo,` +
  `pan=stereo|c0=${(1 - 0.5 * w.pan).toFixed(3)}*c0|c1=${(1 + 0.5 * w.pan).toFixed(3)}*c1,` +
  `${w.fx},volume=${w.gain},` +
  `atrim=duration=${TOTAL_SEC}[w_${w.name}]`
);
const pachChains = PACHINKO_HITS.map((p) =>
  `[${p.inputIdx}:a]adelay=${p.startMs}|${p.startMs},` +
  `aresample=48000,aformat=channel_layouts=stereo,` +
  `pan=stereo|c0=${(1 - 0.5 * p.pan).toFixed(3)}*c0|c1=${(1 + 0.5 * p.pan).toFixed(3)}*c1,` +
  `${p.fx},volume=${p.gain},` +
  `atrim=duration=${TOTAL_SEC}[p_${p.name}]`
);

const mixChain = [
  // Bed stays flat — the GLOBAL slow fade-in at the bottom of the
  // chain (afade 0..32 s) handles the "everything fades in by 32 s"
  // build. Removes the previous bed-specific fade so the muffled
  // kick + bells + sine bell + hum all share the same gentle ramp.
  `[0:a]volume=${(BED_GAIN*BED_PRESENCE).toFixed(3)},asplit=${N_BED_BRANCHES}[bed]${allScTags.join("")}`,
  ...layerChains,
  ...bellChains,
  // Constant hum: low gain, gentle reverb tail, fades in over 12 s.
  // Hum lowpassed + slower fade-in + delayed start — the rubberband-
  // 200× stretch produces a metallic squeak at the very front of the
  // file that was bleeding through at t=0 (@jeffrey 2026-05-29 "nl
  // its like a squeak / right at 00"). Start at t=4 s now.
  `[${HUM_INPUT_IDX}:a]lowpass=f=800,volume=0.32,` +
    `adelay=4000|4000,afade=t=in:st=4:d=18:curve=qsin,` +
    `aresample=48000,aformat=channel_layouts=stereo,` +
    `aecho=0.7:0.65:240|580:0.4|0.3,` +
    `atrim=duration=${TOTAL_SEC}[hum]`,
  // Held "ah" — split into TWO branches: a "FAR-AWAY CROWD" version
  // (heavy lowpass + flanger phasing + faster tremolo) audible only for
  // the first 5 s, then a clean quiet ah that fades in from 3 s onward.
  // The crowd version sounds like distant voices singing through a
  // wall, the clean version is the recognisable "ah" emerging.
  `[${AH_INPUT_IDX}:a]asplit=2[ah_crowd_in][ah_clean_in]`,
  // Branch 1 — CROWD: heavy lowpass (1200 Hz), flanger phasing,
  // wide reverb, faster 2.4 Hz tremolo. Volume bumps slightly higher
  // at start, then fades out by ~5 s.
  // CROWD AH — pulled WAY back ("the first sound is still too much"
  // @jeffrey 2026-05-29). Volume 1.3 → 0.45, fade-in 1.5s → 5s
  // (gentle entry), reverb decay 0.85 → 0.55, flanger depth 3 → 1.2.
  // AH crowd: lowpass dropped 1000 → 480 Hz to kill the vowel's F2
  // formant (~990 Hz) which was reading as a "B5 squeak" right at
  // 00:00 (@jeffrey 2026-05-29 spectral diagnosis). Also delayed
  // start by 2 s so the first 2 s is pure bed.
  `[ah_crowd_in]aresample=48000,aformat=channel_layouts=stereo,` +
    `lowpass=f=480,` +
    `flanger=delay=5:depth=1.2:regen=0:width=71:speed=0.7:shape=sinusoidal,` +
    `volume=eval=frame:volume='0.45*(0.55+0.45*sin(2*PI*2.4*t))',` +
    `adelay=2000|2000,` +
    `afade=t=in:st=2:d=5:curve=qsin,` +
    `afade=t=out:st=6:d=2:curve=tri,` +
    `aecho=0.6:0.45:280|620:0.30|0.22,` +
    `atrim=duration=${TOTAL_SEC}[ah_crowd]`,
  // Branch 2 — CLEAN AH. Extended fade-in to 21 s, cuts at 24 s when
  // MAY now enters (was 20 s).
  `[ah_clean_in]aresample=48000,aformat=channel_layouts=stereo,` +
    `volume=eval=frame:volume='2.0*(0.55+0.45*sin(2*PI*1.6*t))',` +
    `afade=t=in:st=${SHORT ? 1 : 3}:d=${SHORT ? 5 : 24}:curve=qsin,` +
    `afade=t=out:st=${AH_OUT}:d=0.5:curve=tri,` +
    `aecho=0.5:0.28:100|240:0.16|0.12,` +
    `atrim=duration=${TOTAL_SEC}[ah_clean]`,
  // Sum the two branches into the [ah] tag used downstream.
  `[ah_crowd][ah_clean]amix=inputs=2:weights='1 1':normalize=0[ah]`,
  // "may" — quick M at 27.5 s, AY sustains 4 s, ends at 31.5 s.
  `[${MAY_INPUT_IDX}:a]adelay=27500|27500,` +
    `aresample=48000,aformat=channel_layouts=stereo,` +
    `volume=eval=frame:volume='${(3.8*HELD).toFixed(2)}*(0.78+0.22*sin(2*PI*0.8*t))',` +
    `afade=t=in:st=27.5:d=0.3:curve=qsin,` +
    `afade=t=out:st=31.0:d=0.5:curve=tri,` +
    `aecho=0.5:0.28:100|260:0.16|0.12,` +
    `atrim=duration=${TOTAL_SEC}[may]`,
  // "zing" — 32-46 s (14 s = 7 bars). Long held I vowel.
  `[${ZING_INPUT_IDX}:a]adelay=32000|32000,` +
    `aresample=48000,aformat=channel_layouts=stereo,` +
    `volume=eval=frame:volume='${(3.8*HELD).toFixed(2)}*(0.78+0.22*sin(2*PI*0.7*t))',` +
    `afade=t=in:st=32:d=0.3:curve=qsin,` +
    `afade=t=out:st=45.5:d=0.5:curve=tri,` +
    `aecho=0.5:0.28:100|260:0.16|0.12,` +
    `atrim=duration=${TOTAL_SEC}[zing]`,
  // "grace" — 46-70 s (24 s = 12 bars). 12-bar dramatic hold.
  `[${GRACE_INPUT_IDX}:a]adelay=46000|46000,` +
    `aresample=48000,aformat=channel_layouts=stereo,` +
    `volume=eval=frame:volume='${(3.8*HELD).toFixed(2)}*(0.78+0.22*sin(2*PI*0.6*t))',` +
    `afade=t=in:st=46:d=0.3:curve=qsin,` +
    `afade=t=out:st=69.5:d=0.5:curve=tri,` +
    `aecho=0.5:0.28:100|260:0.16|0.12,` +
    `atrim=duration=${TOTAL_SEC}[grace]`,
  // Wave-wizard syllable layers — with LATE-SYLLABLE WAVERING.
  // Adds a faster (5-8 Hz) tremolo that ramps in over the last 55% of
  // each hold, so syllable tails wobble + warble out instead of being
  // statically held. Sounds like singer's vibrato deepening on a held
  // note. @jeffrey 2026-05-29.
  // Wave-wizard syllables — STRONGER HELD VOWELS + SHORTER DECAY.
  // (@jeffrey 2026-05-29) The loud portion of the vowel now holds for
  // ~88% of holdSec at full amp; fade-out compressed 0.4s → 0.15s so
  // each word snaps off instead of trailing. Late-syllable wavering
  // retained but pushed later (last 35%) so the strong middle reads.
  // SYLLABLE chains — split into [dry] + [flanged tail].
  // The flanged branch runs the same audio through a 4 Hz flanger
  // (= 1/8 note at 120 BPM, @jeffrey 2026-05-29 "fast like 1/8th
  // beat airy flange at the tails of words") with a volume envelope
  // that ramps in over the last 35% of the hold so only the tail
  // gets the airy sweep — the loud middle stays clean.
  // SYLLABLE chains — now with FOUR branches per syllable:
  //   • dry main
  //   • 1/8-note tail flanger (late only)
  //   • +12 OCTAVE UP harmony (in-and-out via 0.4 Hz LFO)
  //   • -12 OCTAVE DOWN harmony (in-and-out via 0.35 Hz LFO, phase π/2 off)
  // Harmonies use asetrate pitch-shift (no formant preservation, but
  // the chipmunk/baritone character is musically what we want for
  // backing layers). @jeffrey 2026-05-29 "more harmonies coming in
  // and out as the vocals go / lower and higher / same samples".
  ...ALL_SYLLABLES.map((s) => {
    const wavStart = (s.mixAt + s.holdSec * 0.65).toFixed(2);
    const wavRamp  = Math.max(0.2, s.holdSec * 0.30).toFixed(2);
    const wavRate  = (5 + (s.holdSec > 4 ? 2 : 0)).toFixed(1);
    const flStart  = (s.mixAt + s.holdSec * 0.55).toFixed(2);
    const flRamp   = Math.max(0.2, s.holdSec * 0.30).toFixed(2);
    const baseChain =
      `[${s.inputIdx}:a]adelay=${(s.mixAt*1000)|0}|${(s.mixAt*1000)|0},` +
      `aresample=48000,aformat=channel_layouts=stereo,` +
      `volume=eval=frame:volume='${VOX_GAIN}*(1.00+0.18*sin(2*PI*${s.tremolo}*t)` +
        `+0.18*max(0\\,min(1\\,(t-${wavStart})/${wavRamp}))*sin(2*PI*${wavRate}*t)` +
        `+0.18*exp(-(t-${s.mixAt})*4)*max(0\\,t-${s.mixAt}))',` +
      `afade=t=in:st=${s.mixAt}:d=0.18:curve=qsin,` +
      `afade=t=out:st=${(s.mixAt+s.holdSec-0.15).toFixed(2)}:d=0.15:curve=tri,` +
      `${VOX_ECHO},` +
      `atrim=duration=${TOTAL_SEC}`;
    // asetrate harmonies REMOVED — they double-played the syllable at
    // wrong speeds creating ghosting ("that saved a wretch like me is
    // being repeated weird" @jeffrey 2026-05-29). Pitched harmonies
    // would need rubberband (preserves duration), not asetrate. Back
    // to dry + tail-flanger only.
    return `${baseChain},asplit=2[s_${s.name}_dry][s_${s.name}_pre];` +
      `[s_${s.name}_pre]flanger=delay=4:depth=2.5:regen=15:width=78:` +
      `speed=4:shape=sinusoidal:phase=25:interp=linear,` +
      // Flanger wet dropped 0.25 → 0.08 ("get some phase shifting out"
      // @jeffrey 2026-05-29). Subtle hint of motion, not a sweep.
      `volume=eval=frame:volume='0.08*max(0\\,min(1\\,(t-${flStart})/${flRamp}))'` +
      `[s_${s.name}_wet];` +
      `[s_${s.name}_dry][s_${s.name}_wet]amix=inputs=2:weights='1 1':normalize=0[s_${s.name}]`;
  }),
  ...ALL_PVC.map((s) => {
    const wavStart = (s.mixAt + s.holdSec * 0.65).toFixed(2);
    const wavRamp  = Math.max(0.2, s.holdSec * 0.30).toFixed(2);
    const wavRate  = (6 + (s.holdSec > 4 ? 2 : 0)).toFixed(1);
    const flStart  = (s.mixAt + s.holdSec * 0.55).toFixed(2);
    const flRamp   = Math.max(0.2, s.holdSec * 0.30).toFixed(2);
    const baseChain =
      `[${s.inputIdx}:a]adelay=${(s.mixAt*1000)|0}|${(s.mixAt*1000)|0},` +
      `aresample=48000,aformat=channel_layouts=stereo,` +
      `volume=eval=frame:volume='${(s.gain*PVC_GAIN_MUL).toFixed(2)}*(0.96+0.20*sin(2*PI*${s.tremolo}*t)` +
        `+0.22*max(0\\,min(1\\,(t-${wavStart})/${wavRamp}))*sin(2*PI*${wavRate}*t)` +
        `+0.20*exp(-(t-${s.mixAt})*4)*max(0\\,t-${s.mixAt}))',` +
      `afade=t=in:st=${s.mixAt}:d=0.20:curve=qsin,` +
      `afade=t=out:st=${(s.mixAt+s.holdSec-0.15).toFixed(2)}:d=0.15:curve=tri,` +
      `${PVC_ECHO},` +
      `atrim=duration=${TOTAL_SEC}`;
    return `${baseChain},asplit=2[pvc_${s.name}_dry][pvc_${s.name}_pre];` +
      `[pvc_${s.name}_pre]flanger=delay=4:depth=2.5:regen=15:width=82:` +
      `speed=4:shape=sinusoidal:phase=25:interp=linear,` +
      `volume=eval=frame:volume='0.06*max(0\\,min(1\\,(t-${flStart})/${flRamp}))'` +
      `[pvc_${s.name}_wet];` +
      `[pvc_${s.name}_dry][pvc_${s.name}_wet]amix=inputs=2:weights='1 1':normalize=0[pvc_${s.name}]`;
  }),
  ...oceanChains,
  ...whaleChains,
  ...pachChains,
  ...scratchChains,
  ...harmonyChains,
  `[bed]${allLayerTags.join("")}` +
    `${BELL_HITS.map((_, k) => `[bell${k}]`).join("")}[hum][ah][may][zing][grace]` +
    `${ALL_SYLLABLES.map(s => `[s_${s.name}]`).join("")}` +
    `${ALL_PVC.map(s => `[pvc_${s.name}]`).join("")}` +
    `${OCEAN_HITS.map(o => `[o_${o.name}]`).join("")}` +
    `${WHALE_HITS.map(w => `[w_${w.name}]`).join("")}` +
    `${PACHINKO_HITS.map(p => `[p_${p.name}]`).join("")}` +
    `${SCRATCH_HITS.map(s => `[scr_${s.name}]`).join("")}` +
    `${HARMONIES.map(h => `[h_${h.name}]`).join("")}` +
    `amix=inputs=${1 + N_LAYERS * N_PASSES + BELL_HITS.length + 5 + ALL_SYLLABLES.length + ALL_PVC.length + OCEAN_HITS.length + WHALE_HITS.length + PACHINKO_HITS.length + SCRATCH_HITS.length + HARMONIES.length}:` +
    `duration=longest:dropout_transition=0:` +
    `weights='1${' 1'.repeat(N_LAYERS * N_PASSES)}` +
    `${' 1'.repeat(BELL_HITS.length)} 1 1 1 1 1${' 1'.repeat(ALL_SYLLABLES.length)}${' 1'.repeat(ALL_PVC.length)}${' 1'.repeat(OCEAN_HITS.length)}${' 1'.repeat(WHALE_HITS.length)}${' 1'.repeat(PACHINKO_HITS.length)}${' 1'.repeat(SCRATCH_HITS.length)}${' 1'.repeat(HARMONIES.length)}':normalize=0,` +
    `atrim=duration=${TOTAL_SEC},` +
    // Master fade-in begins at t=2 s (@jeffrey 2026-05-29 "lets start
    // the track fading in from about :02 now"). Silent 0-2 s, then
    // ramps up over 4 s to ~0.5, then continues the slow build to
    // full by t=32 s on the outer sin curve.
    `volume=eval=frame:volume='max(0\\,min(1\\,(t-2)/4))*(0.45+0.55*sin(min(1\\,t/32)*PI/2))',` +
    `afade=t=out:st=${TOTAL_SEC - 3}:d=3[out]`,
].join(";");

const ffArgs = ["-y", "-i", BED_WAV];
// One copy of each layer file per pass, in the order LAYERS × PASSES.
for (let l = 0; l < N_LAYERS; l++)
  for (let i = 0; i < N_PASSES; i++)
    ffArgs.push("-i", LAYERS[l].file);
for (const bf of BELL_FILES) ffArgs.push("-i", bf);
ffArgs.push("-i", VOCAL_HUM);
ffArgs.push("-i", VOCAL_AH);
ffArgs.push("-i", VOCAL_MAY);
ffArgs.push("-i", VOCAL_ZING);
ffArgs.push("-i", VOCAL_GRACE);
for (const s of LINE1_REST) ffArgs.push("-i", s.file);
for (const s of LINE2)      ffArgs.push("-i", s.file);
for (const s of LINE3)      ffArgs.push("-i", s.file);
for (const s of PVC_LINE1)  ffArgs.push("-i", s.file);
for (const s of PVC_LINE2)  ffArgs.push("-i", s.file);
for (const s of PVC_LINE3)  ffArgs.push("-i", s.file);
for (const s of LINE4)      ffArgs.push("-i", s.file);
for (const s of PVC_LINE4)  ffArgs.push("-i", s.file);
for (const o of OCEAN_HITS)     ffArgs.push("-i", o.file);
for (const w of WHALE_HITS)     ffArgs.push("-i", w.file);
for (const p of PACHINKO_HITS)  ffArgs.push("-i", p.file);
for (const h of HARMONIES)      ffArgs.push("-i", h.file);
ffArgs.push("-filter_complex", mixChain, "-map", "[out]",
            "-ar", "48000", "-c:a", "pcm_f32le", PRE_WAV);
run("ffmpeg", ffArgs,
    `mix bed + ${N_PASSES * N_LAYERS} vocal layers (${LAYERS.map(l=>l.name).join("/")}) ` +
    `+ ${BELL_HITS.length} bells`);

// 4 — pre-bright stash for A/B
run("ffmpeg", ["-y", "-i", PRE_WAV,
               "-ar", "44100", "-sample_fmt", "s16",
               PRE_BRIGHT_WAV],
    "pre-bright stash");

// 5 — brightening polish. Loukeman / deep-house masters sit at -10..-12
// LUFS with TIGHT dynamic range; air @ 10k, a touch of mud trim @ 220 Hz,
// soft limiter at 0.96.
// Dance-pop master chain tuned for legibility:
//   • 300 Hz mud cut (clears low-mid clutter)
//   • 3.2 kHz vocal presence push (intelligibility)
//   • 5.5 kHz clarity bump
//   • 10 kHz air, 13.5 kHz sparkle
//   • Slight de-essing via 7 kHz Q-narrow
//   • Tight alimiter for dense pop master sound
// ─── FINAL POP MASTER CHAIN ──────────────────────────────────────────
// Designed to match commercial deep-house / pop loudness + clarity.
// @jeffrey 2026-05-29 "make a final pop mix".
//
//   1. TRIM 2 s of silent head
//   2. TONAL  shaping: mud / mid-scoop / vocal presence / air / sparkle
//   3. GLUE   compression: gentle 2.4:1 over the whole mix
//   4. WIDTH  stereo expansion on the mids+highs
//   5. PRES   final saturation via volume push into the limiter
//   6. PEAK   alimiter @ -0.8 dBTP, fast attack
//   7. TARGET ~-7.5 LUFS for streaming/club readiness
const brightChain = [
  "atrim=start=2",
  "asetpts=PTS-STARTPTS",

  // ── Tonal shaping ───────────────────────────────────────────────
  "equalizer=f=80:t=q:w=1.0:g=1.2",         // sub thump (kick body)
  "equalizer=f=200:t=q:w=1.0:g=-1.8",       // tighten low-mid
  "equalizer=f=420:t=q:w=1.4:g=-1.4",       // remove boxy
  "equalizer=f=900:t=q:w=1.6:g=-0.6",       // gentle nasal scoop
  "equalizer=f=2400:t=q:w=1.8:g=-1.6",      // smoother upper mids
  "equalizer=f=3400:t=q:w=1.0:g=1.8",       // vocal presence (more)
  "equalizer=f=5500:t=q:w=1.4:g=1.2",       // clarity
  "equalizer=f=7200:t=q:w=2.0:g=-1.6",      // de-ess
  "highshelf=f=10000:g=1.8",                // air
  "highshelf=f=14000:g=1.4",                // sparkle / sheen

  // ── Glue bus compression (gentle, slow) ─────────────────────────
  // 2.4:1 ratio, ~3-4 dB GR, glues all the layers together
  "acompressor=threshold=0.18:ratio=2.4:attack=30:release=260:makeup=1.5",

  // ── Stereo widening (mid-side style via extrastereo) ────────────
  // m=1.25 = ~25% wider sides while keeping mono-compatibility
  // Stereo widening REMOVED from the linear chain — moved into the
  // mono-bass split below. The old extrastereo widened the whole
  // spectrum including the bass, producing low-frequency phase
  // smear ("get some phase shifting out of the lows" @jeffrey
  // 2026-05-29). See FINAL_STAGES below for the band-split version.

  // ── Final push into the limiter ─────────────────────────────────
  // +1.8 dB drives the alimiter for ~2-3 dB of peak reduction —
  // gives the "pop" loudness without obvious pumping
  "volume=1.8dB",
].join(",");

// MONO-BASS STEREO WIDENING — split the master into low + high bands,
// keep the LOW band perfectly mono (no widening, no phase smear), and
// widen ONLY the upper band. This gives the pop "wide" image without
// killing low-end punch or causing low-frequency phase shift on
// playback systems that sum to mono (clubs, phones, car speakers).
// Crossover at 200 Hz (4th-order Linkwitz-Riley).
// @jeffrey 2026-05-29 "get some phase shifting out of the lows".
const masterComplex =
  `[0:a]${brightChain},acrossover=split=200:order=4th[lo][hi];` +
  // LOW band: force mono → guarantees no phase issues in bass
  `[lo]pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1[lo_mono];` +
  // HIGH band: stereo widening (only affects mids+highs)
  `[hi]extrastereo=m=1.20:c=true[hi_wide];` +
  // Sum back + final brickwall limit
  `[lo_mono][hi_wide]amix=inputs=2:weights='1 1':normalize=0,` +
  `alimiter=limit=0.91:attack=3:release=150:level=disabled[out]`;
run("ffmpeg", ["-y", "-i", PRE_WAV, "-filter_complex", masterComplex,
               "-map", "[out]", "-ar", "44100", "-sample_fmt", "s16",
               FINAL_WAV], "brightening polish + mono-bass widen");

run("ffmpeg", ["-y", "-i", FINAL_WAV, "-codec:a", "libmp3lame", "-b:a", "320k",
               FINAL_MP3], "320k mp3");

spawnSync("open", ["-a", "QuickTime Player", FINAL_MP3]);
console.log(`\n[bake-dance] done`);
console.log(`             master: ${FINAL_WAV}`);
console.log(`             mp3:    ${FINAL_MP3}`);
console.log(`             A/B:    ${PRE_BRIGHT_WAV}`);
