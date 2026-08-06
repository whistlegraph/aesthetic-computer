#!/usr/bin/env node
// render-special-sine-720.mjs — twelve-minute long-form variation of Special Sign.
//
// The accepted spatial master is treated as recorded thematic material. Seven
// connected movements transform it through augmentation, diminution, fifth /
// octave canons, contrary spatial mirror motion, stretto, and a final music-box run-down. All
// transformations are linear; no saturation, groove noise, or recursive room
// feedback is introduced here.

import { existsSync, mkdirSync, rmSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const arg = (key, fallback) => {
  const i = process.argv.indexOf(key);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : fallback;
};
const input = resolve(arg("--input", resolve(HERE, "../out/review/Special-Sign-DIAMOND-AUDITION.wav")));
const out = resolve(arg("--out", resolve(HERE, "../out/review/Special-Sine-720-DIAMOND-MASTER.wav")));
const mp3 = resolve(arg("--mp3", out.replace(/\.[^.]+$/, ".mp3")));
const provenance = out.replace(/\.[^.]+$/, ".provenance.json");
const work = resolve(HERE, "../out/720-work");
const SR = 48_000;
const XFADE = 4;

if (!existsSync(input)) throw new Error(`missing thematic master: ${input}`);
mkdirSync(dirname(out), { recursive: true });
mkdirSync(work, { recursive: true });

function run(args, capture = false) {
  const r = spawnSync("ffmpeg", args, capture ? { encoding: "utf8" } : { stdio: "inherit" });
  if (r.status !== 0) throw new Error(`ffmpeg failed (${r.status})`);
  return r;
}

// Rate and pitch wind together, like tape or a music-box cylinder. This uses
// only band-limited resampling: no phase vocoder / WSOLA time-stretch chirps.
const rate = (ratio) => `asetrate=${(SR * ratio).toFixed(4)},aresample=${SR}:filter_size=64:phase_shift=10`;
const polish = "highpass=f=28,lowpass=f=15000,acompressor=threshold=.16:ratio=1.16:attack=42:release=720:knee=3:makeup=1,alimiter=limit=.91:attack=8:release=180";
const movements = [
  {
    title: "I. Far Cradle — theme in the original field",
    duration: 96,
    filter: `[0:a]atrim=0:96,asetpts=PTS-STARTPTS,volume=.90,stereotools=mlev=1:slev=1.04,${polish}[out]`,
  },
  {
    title: "II. Two-Voice Invention — fifth and octave canons",
    duration: 108,
    filter: `[0:a]atrim=0:108,asetpts=PTS-STARTPTS,asplit=3[b][f][o];` +
      `[b]volume=.76[b0];` +
      `[f]highpass=f=180,lowpass=f=2100,${rate(1.498307)},adelay=1579:all=1,volume=.092[f0];` +
      `[o]highpass=f=280,lowpass=f=1450,${rate(2)},adelay=3158:all=1,volume=.030[o0];` +
      `[b0][f0][o0]amix=inputs=3:normalize=0:duration=first,atrim=0:108,${polish}[out]`,
  },
  {
    title: "III. Wind-Up — four diminishing temporal gears",
    duration: 102,
    filter: `[0:a]asplit=4[a][b][c][d];` +
      `[a]${rate(.90)},atrim=0:25.5,asetpts=PTS-STARTPTS[a0];` +
      `[b]${rate(1.00)},atrim=0:25.5,asetpts=PTS-STARTPTS[b0];` +
      `[c]${rate(1.12)},atrim=0:25.5,asetpts=PTS-STARTPTS[c0];` +
      `[d]${rate(1.24)},atrim=0:25.5,asetpts=PTS-STARTPTS[d0];` +
      `[a0][b0][c0][d0]concat=n=4:v=0:a=1,${polish}[out]`,
  },
  {
    title: "IV. Jeffrey Vowel Garden — augmented human canon",
    duration: 112,
    filter: `[0:a]atrim=5:104,asetpts=PTS-STARTPTS,${rate(.88)},atrim=0:112,asplit=3[b][l][h];` +
      `[b]volume=.82[b0];` +
      `[l]lowpass=f=2100,${rate(.5)},adelay=2390:all=1,volume=.075[l0];` +
      `[h]highpass=f=320,lowpass=f=2200,${rate(1.498307)},adelay=4780:all=1,volume=.045[h0];` +
      `[b0][l0][h0]amix=inputs=3:normalize=0:duration=first,atrim=0:112,${polish}[out]`,
  },
  {
    title: "V. Diamond Mirror — contrary low spatial glints",
    duration: 122,
    filter: `[0:a]atrim=0:122,asetpts=PTS-STARTPTS,asplit=2[b][r];` +
      `[b]volume=.88[b0];` +
      `[r]highpass=f=180,lowpass=f=3200,${rate(.749154)},pan=stereo|c0=c1|c1=c0,volume=.052[r0];` +
      `[b0][r0]amix=inputs=2:normalize=0:duration=first,atrim=0:122,${polish}[out]`,
  },
  {
    title: "VI. Stretto Constellation — eager overlapping answers",
    duration: 138,
    filter: `[0:a]${rate(1.08)},atrim=0:138,asetpts=PTS-STARTPTS,asplit=4[b][a][f][o];` +
      `[b]volume=.70[b0];` +
      `[a]highpass=f=240,lowpass=f=2400,${rate(1.259921)},adelay=789:all=1,volume=.060[a0];` +
      `[f]highpass=f=330,lowpass=f=1900,${rate(1.498307)},adelay=1579:all=1,volume=.045[f0];` +
      `[o]highpass=f=540,lowpass=f=1350,${rate(2)},adelay=2368:all=1,volume=.020[o0];` +
      `[b0][a0][f0][o0]amix=inputs=4:normalize=0:duration=first,atrim=0:138,${polish}[out]`,
  },
  {
    title: "VII. Home Sign — music box runs out of energy",
    duration: 66,
    filter: `[0:a]atrim=51:102.48,asetpts=PTS-STARTPTS,${rate(.78)},atrim=0:66,` +
      `acompressor=threshold=.18:ratio=1.12:attack=55:release=900:knee=3:makeup=1,` +
      `highpass=f=28,lowpass=f=15800,alimiter=limit=.90:attack=10:release=220[out]`,
  },
];

const movementFiles = [];
for (let i = 0; i < movements.length; i++) {
  const m = movements[i];
  const path = resolve(work, `${String(i + 1).padStart(2, "0")}.flac`);
  console.log(`[${i + 1}/7] ${m.title} · ${m.duration}s`);
  run(["-hide_banner", "-y", "-loglevel", "error", "-stream_loop", "-1", "-i", input,
    "-filter_complex", m.filter, "-map", "[out]", "-t", String(m.duration),
    "-ar", String(SR), "-c:a", "flac", "-compression_level", "8", path]);
  movementFiles.push(path);
}

// Raw movement lengths total 744 seconds; six four-second equal-power joins
// remove exactly 24 seconds, making the assembled work exactly 720 seconds.
const premaster = resolve(work, "Special-Sine-720.premaster.flac");
const inputs = movementFiles.flatMap((p) => ["-i", p]);
const joins = [];
let left = "[0:a]";
for (let i = 1; i < movementFiles.length; i++) {
  const name = `x${i}`;
  joins.push(`${left}[${i}:a]acrossfade=d=${XFADE}:c1=qsin:c2=qsin[${name}]`);
  left = `[${name}]`;
}
joins.push(`${left}atrim=0:720,asetpts=PTS-STARTPTS[out]`);
console.log("[assemble] six spatial crossfades → exactly 720.0s");
run(["-hide_banner", "-y", "-loglevel", "error", ...inputs, "-filter_complex", joins.join(";"),
  "-map", "[out]", "-ar", String(SR), "-c:a", "flac", "-compression_level", "8", premaster]);

console.log("[master] measuring the complete arc…");
const measured = run(["-hide_banner", "-nostats", "-loglevel", "info", "-i", premaster,
  "-af", "loudnorm=print_format=json", "-f", "null", "-"], true);
const field = (name) => {
  const match = measured.stderr.match(new RegExp(`"${name}"\\s*:\\s*"([^"]+)"`));
  return match ? Number(match[1]) : NaN;
};
const inputI = field("input_i"), inputTp = field("input_tp");
const gainDb = Math.min(-15 - inputI, -1.2 - inputTp);
console.log(`  ${inputI.toFixed(2)} LUFS / ${inputTp.toFixed(2)} dBTP → linear ${gainDb >= 0 ? "+" : ""}${gainDb.toFixed(2)} dB`);
// atempo/asetrate rounding can leave the assembled stream a few milliseconds
// shy of 720. Pad before the final trim so the release has exactly 34,560,000
// frames at 48 kHz; this is a sub-frame-feeling extension of the resolved tail.
const finalFilter = `volume=${gainDb.toFixed(2)}dB,apad=pad_len=960,atrim=0:720`;
run(["-hide_banner", "-y", "-loglevel", "error", "-i", premaster, "-af", finalFilter,
  "-ar", String(SR), "-c:a", "pcm_s24le", out]);
run(["-hide_banner", "-y", "-loglevel", "error", "-i", premaster, "-af", finalFilter,
  "-ar", String(SR), "-c:a", "libmp3lame", "-b:a", "320k", mp3]);

writeFileSync(provenance, JSON.stringify({
  title: "Special Sine — 720 Mix",
  durationSeconds: 720,
  thematicMaster: input.replace(REPO + "/", ""),
  form: "seven attacca spatial variations",
  crossfadeSeconds: XFADE,
  movements,
  counterpoint: {
    procedures: ["fifth answer", "octave answer", "half-bar canon", "one-bar canon", "augmentation", "diminution", "contrary spatial mirror", "stretto"],
    sourceCounterpoint: "Special Sign's authored melody, counterline, arpeggios, Jeffrey phonemes, marimba, and spatial bodies",
  },
  spatialPipeline: {
    sourceMotionRetained: true,
    hrtfRetained: true,
    distanceAndDopplerRetained: true,
    roomReturns: "four non-recursive spatial FIR images",
    recursiveRoomFeedback: false,
  },
  material: { name: "diamond", saturation: false, grooveNoise: false, extraMasterEcho: false },
  rateTransform: { method: "band-limited sample-rate conversion", timeStretch: false, pitchCorrection: false, character: "speed and pitch wind together without phase-vocoder squeaks" },
  mastering: { targetLufs: -15, maxTruePeakDb: -1.2, measuredPremasterLufs: inputI, measuredPremasterTruePeakDb: inputTp, linearGainDb: gainDb, sampleRate: SR, bitDepth: 24 },
}, null, 2) + "\n");

rmSync(work, { recursive: true, force: true });
console.log(`✓ ${out}`);
console.log(`✓ ${mp3}`);
console.log(`✓ ${provenance}`);
