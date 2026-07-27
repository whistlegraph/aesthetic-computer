#!/usr/bin/env node
// Special Sign 720 — six-minute 72→144 BPM dance/trance remix.
//
// The locked release WAV is read as thematic material and is never modified.
// A deliberately lean arrangement surrounds it with one kick, one sub voice,
// restrained hats/claps, and one C-major trance pulse.

import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../../..");
const SR = 48_000;
const DURATION = 360;
const SLOW_BPM = 72;
const FAST_BPM = 144;
const HINGE = 120;
const TARGET_LUFS = -12;
const TARGET_TP = -1;

const arg = (name, fallback) => {
  const index = process.argv.indexOf(name);
  return index >= 0 && process.argv[index + 1] ? process.argv[index + 1] : fallback;
};
const sourcePath = resolve(arg("--source", resolve(HERE, "../../release/special-sign/special-sign-MASTER.wav")));
const coverPath = resolve(arg("--cover", resolve(HERE, "../../release/special-sign/special-sign-cover-3000.jpg")));
const outputDir = resolve(arg("--output-dir", resolve(HERE, "../../out/review/variants/special-sign-720")));
const wavPath = resolve(outputDir, "Special-Sign-720-MIX.wav");
const mp3Path = resolve(outputDir, "Special-Sign-720-MIX.mp3");
const receiptPath = resolve(outputDir, "Special-Sign-720-MIX.provenance.json");
const rawPath = resolve(outputDir, ".Special-Sign-720.f32");
const premixPath = resolve(outputDir, ".Special-Sign-720-premix.wav");

if (!existsSync(sourcePath)) throw new Error(`missing locked thematic master: ${sourcePath}`);
if (!existsSync(coverPath)) throw new Error(`missing Special Sign cover: ${coverPath}`);
mkdirSync(outputDir, { recursive: true });

const run = (args, capture = false) => {
  const result = spawnSync("ffmpeg", args, capture
    ? { encoding: "utf8", maxBuffer: 64 * 1024 * 1024 }
    : { stdio: "inherit" });
  if (result.status !== 0) throw new Error(`ffmpeg failed (${result.status})`);
  return result;
};
const decode = (path) => {
  const result = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-i", path,
    "-f", "f32le", "-ar", String(SR), "-ac", "2", "-"],
  { maxBuffer: 256 * 1024 * 1024 });
  if (result.status !== 0) throw new Error(`could not decode ${path}`);
  return new Float32Array(result.stdout.buffer, result.stdout.byteOffset, result.stdout.byteLength / 4);
};
const clamp = (value, lo = 0, hi = 1) => Math.max(lo, Math.min(hi, value));
const smooth = (value) => { const x = clamp(value); return x * x * (3 - 2 * x); };
const hz = (midi) => 440 * 2 ** ((midi - 69) / 12);
const sha256 = (path) => createHash("sha256").update(readFileSync(path)).digest("hex");

console.log(`[720] decoding locked theme ${relative(REPO, sourcePath)}`);
const source = decode(sourcePath);
const sourceFrames = source.length / 2;
const frames = DURATION * SR;
const music = new Float32Array(frames * 2);

// Long, overlapping source passes preserve the authored melody and spatial
// motion. The first pass is genuine tape-like augmentation; later passes wind
// toward pop speed without phase-vocoder artifacts.
const clips = [
  { start: 0, duration: 120, src: 0, rate: 0.74, gain: 0.72, fadeIn: 4, fadeOut: 4, tag: "72 BPM far-field augmentation" },
  { start: 116, duration: 47, src: 7, rate: 1.08, gain: 0.53, fadeIn: 4, fadeOut: 3, tag: "double-time arrival" },
  { start: 160, duration: 44, src: 35, rate: 1.12, gain: 0.49, fadeIn: 3, fadeOut: 3, tag: "first pop orbit" },
  { start: 201, duration: 42, src: 12, rate: 1.18, gain: 0.49, fadeIn: 3, fadeOut: 4, tag: "bright lift" },
  { start: 238, duration: 45, src: 50, rate: 0.88, gain: 0.60, fadeIn: 4, fadeOut: 4, tag: "air-pocket theme" },
  { start: 279, duration: 43, src: 17, rate: 1.22, gain: 0.51, fadeIn: 4, fadeOut: 3, tag: "final pop orbit" },
  { start: 319, duration: 41, src: 52, rate: 1.12, gain: 0.50, fadeIn: 3, fadeOut: 8, tag: "home sign" },
];

console.log(`[720] placing ${clips.length} spacious thematic passes`);
for (const clip of clips) {
  const outStart = Math.round(clip.start * SR);
  const count = Math.min(frames - outStart, Math.round(clip.duration * SR));
  for (let i = 0; i < count; i++) {
    const local = i / SR;
    const sf = (clip.src + local * clip.rate) * SR;
    const s0 = Math.floor(sf);
    if (s0 < 0 || s0 + 1 >= sourceFrames) continue;
    const f = sf - s0;
    const fadeIn = smooth(local / clip.fadeIn);
    const fadeOut = smooth((clip.duration - local) / clip.fadeOut);
    const env = clip.gain * Math.min(fadeIn, fadeOut);
    const out = (outStart + i) * 2;
    const input = s0 * 2;
    music[out] += (source[input] + (source[input + 2] - source[input]) * f) * env;
    music[out + 1] += (source[input + 1] + (source[input + 3] - source[input + 1]) * f) * env;
  }
}

// C, A, F, G: one uncluttered pop progression, voiced in the sub octave and
// echoed by the lone trance pulse two octaves above.
const roots = [24, 21, 17, 19];
const chordTones = [
  [48, 52, 55, 60], // C
  [45, 48, 52, 57], // Am
  [41, 45, 48, 53], // F
  [43, 47, 50, 55], // G
];
const kickSample = (age) => {
  if (age < 0 || age > 0.42) return 0;
  const tau = 0.026;
  const phase = Math.PI * 2 * (46 * age + (176 - 46) * tau * (1 - Math.exp(-age / tau)));
  return Math.sin(phase) * Math.exp(-age / 0.145)
    + 0.11 * Math.sin(Math.PI * 2 * 4100 * age) * Math.exp(-age / 0.007);
};
const noiseAt = (index) => {
  let x = (index + 0x6d2b79f5) | 0;
  x ^= x << 13; x ^= x >>> 17; x ^= x << 5;
  return (x | 0) / 2147483648;
};
const riser = (t, end, length) => {
  const x = (t - (end - length)) / length;
  return x >= 0 && x < 1 ? smooth(x) * (1 - smooth((x - 0.94) / 0.06)) : 0;
};

console.log("[720] synthesizing the lean 72→144 dance chassis");
let lowNoise = 0;
for (let frame = 0; frame < frames; frame++) {
  const t = frame / SR;
  const bpm = t < HINGE ? SLOW_BPM : FAST_BPM;
  const beat = 60 / bpm;
  const localTime = t < HINGE ? t : (t - HINGE);
  const beatFloat = localTime / beat;
  const beatIndex = Math.floor(beatFloat);
  const beatPhase = beatFloat - beatIndex;
  const beatAge = beatPhase * beat;
  const barBeat = ((beatFloat % 4) + 4) % 4;
  const fastBar = t < HINGE ? -1 : Math.floor(beatFloat / 4);
  const slowBar = Math.floor(t / (4 * 60 / SLOW_BPM));
  const chordSpan = 40 / 3; // 13.333 s: four slow bars / eight fast bars.
  const chordIndex = Math.floor(t / chordSpan) % 4;

  // The first minute is beatless. At 1:00 a half-time pulse appears; at 1:30
  // it becomes four-on-the-floor at 72, then flips cleanly to 144 at 2:00.
  let kickEnabled = t >= 60;
  let kickAge = beatAge;
  if (t < 90) {
    const twoBeat = beat * 2;
    kickAge = ((t - 60) % twoBeat + twoBeat) % twoBeat;
  }
  if (t >= 240 && t < 252) kickEnabled = false;
  if (t >= 252 && t < 268) {
    const twoBeat = beat * 2;
    kickAge = ((t - 252) % twoBeat + twoBeat) % twoBeat;
  }
  const sectionGain = t < 60 ? 0 : t < 120 ? 0.44 + 0.34 * smooth((t - 60) / 60)
    : t < 240 ? 0.86 : t < 268 ? 0.56 : t < 340 ? 0.94 : 0.82 * (1 - smooth((t - 340) / 20));
  const kick = kickEnabled ? kickSample(kickAge) * sectionGain * 0.70 : 0;
  const duck = kickEnabled && kickAge < 0.48
    ? 1 - 0.44 * Math.exp(-kickAge / 0.11) * sectionGain : 1;

  // Classic offbeat bass: a single clean sine with a restrained second
  // harmonic. It enters late in the slow build and remains the only bass.
  const offbeatAge = beatPhase >= 0.5 ? (beatPhase - 0.5) * beat : (beatPhase + 0.5) * beat;
  const bassOn = t >= 88 && !(t >= 240 && t < 254);
  const bassEnv = bassOn && offbeatAge < beat * 0.48
    ? smooth(offbeatAge / 0.012) * Math.exp(-offbeatAge / (t < HINGE ? 0.32 : 0.19)) : 0;
  const bassPhase = Math.PI * 2 * hz(roots[chordIndex]) * offbeatAge;
  let sub = (Math.sin(bassPhase) + 0.10 * Math.sin(bassPhase * 2)) * bassEnv * 0.31 * sectionGain;
  sub *= duck;

  // Pop clap on two/four after the tempo hinge. One narrow hat lane gradually
  // opens from quarter-note ticks to eighths and, only in the final lift,
  // sixteenths.
  const clapDistance = Math.min(Math.abs(barBeat - 1), Math.abs(barBeat - 3));
  const clapAge = clapDistance * beat;
  const clapOn = t >= HINGE && !(t >= 240 && t < 260);
  const clap = clapOn && clapAge < 0.12
    ? noiseAt(frame) * Math.exp(-clapAge / 0.034) * 0.085 * sectionGain : 0;
  const hatDivision = t < 90 ? 0 : t < HINGE ? 1 : t < 200 ? 2 : t < 300 ? 2 : 4;
  const hatUnit = hatDivision ? beat / hatDivision : beat;
  const hatAge = ((localTime % hatUnit) + hatUnit) % hatUnit;
  const hatOn = hatDivision && !(t >= 240 && t < 250);
  const white = noiseAt(frame * 7);
  lowNoise += 0.018 * (white - lowNoise);
  const hat = hatOn && hatAge < 0.052
    ? (white - lowNoise) * Math.exp(-hatAge / 0.016) * (t < HINGE ? 0.018 : 0.032) * sectionGain : 0;

  // One pulse voice supplies all added harmony. It waits until 3:00, leaving
  // the first fast minute spacious, and becomes an octave-bright answer only
  // for the last large section.
  let pulseL = 0, pulseR = 0;
  if (t >= 180 && !(t >= 240 && t < 264)) {
    const division = t >= 300 ? 4 : 2;
    const unit = beat / division;
    const stepFloat = localTime / unit;
    const step = Math.floor(stepFloat);
    const age = (stepFloat - step) * unit;
    const tone = chordTones[chordIndex][step % 4] + (t >= 300 && step % 8 === 7 ? 12 : 0);
    const env = smooth(age / 0.006) * Math.exp(-age / (t >= 300 ? 0.085 : 0.12));
    const phase = Math.PI * 2 * hz(tone) * age;
    const pulse = (Math.sin(phase) + 0.22 * Math.sin(phase * 2)) * env * (t >= 300 ? 0.075 : 0.052) * sectionGain;
    const pan = step % 2 ? 0.64 : 0.36;
    pulseL = pulse * Math.cos(pan * Math.PI / 2) * 1.30;
    pulseR = pulse * Math.sin(pan * Math.PI / 2) * 1.30;
  }

  // Three short filtered-noise breaths announce the hinge, air-pocket return,
  // and final lift without introducing another instrument family.
  const rise = Math.max(riser(t, 120, 12), riser(t, 280, 10), riser(t, 320, 8));
  const breath = (white - lowNoise) * rise * 0.055;
  const endFade = t < 350 ? 1 : 1 - smooth((t - 350) / 10);
  const i = frame * 2;
  let l = music[i] * duck + sub + kick + clap * 0.78 + hat + pulseL + breath;
  let r = music[i + 1] * duck + sub + kick + clap + -hat + pulseR + breath;
  // A very soft safety curve catches coincident peaks before true-peak limiting.
  l = Math.tanh(l * 0.92) / Math.tanh(0.92);
  r = Math.tanh(r * 0.92) / Math.tanh(0.92);
  music[i] = l * endFade;
  music[i + 1] = r * endFade;
}

writeFileSync(rawPath, Buffer.from(music.buffer));
run(["-hide_banner", "-y", "-loglevel", "error", "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", "highpass=f=22,lowpass=f=17500,alimiter=limit=0.89:attack=5:release=120:level=disabled",
  "-ar", String(SR), "-c:a", "pcm_f32le", premixPath]);

// Meter first, then apply only a constant gain. Dynamic loudness normalization
// would erase the deliberately quiet opening and air pocket.
const premixMeter = run(["-hide_banner", "-nostats", "-i", premixPath,
  "-af", "loudnorm=print_format=json", "-f", "null", "-"], true);
const premixJson = (premixMeter.stderr.match(/\{[\s\S]*?\}/g) || []).at(-1);
if (!premixJson) throw new Error("premix loudness receipt was not found");
const premixMeasured = JSON.parse(premixJson);
const gainDb = Math.min(TARGET_LUFS - Number(premixMeasured.input_i), TARGET_TP - Number(premixMeasured.input_tp));
const finalFilter = `volume=${gainDb.toFixed(2)}dB,apad=pad_len=960,atrim=0:${DURATION}`;
console.log(`[720] linear master ${gainDb >= 0 ? "+" : ""}${gainDb.toFixed(2)} dB (arc preserved)`);
run(["-hide_banner", "-y", "-loglevel", "error", "-i", premixPath, "-af", finalFilter,
  "-ar", String(SR), "-c:a", "pcm_s24le", wavPath]);
run(["-hide_banner", "-y", "-loglevel", "error", "-i", wavPath, "-i", coverPath,
  "-map", "0:a", "-map", "1:v", "-c:a", "libmp3lame", "-b:a", "320k", "-c:v", "mjpeg",
  "-id3v2_version", "3", "-metadata", "title=Special Sign 720", "-metadata", "artist=Aesthetic Dot Computer",
  "-metadata", "album=pixsies", "-metadata:s:v", "title=Album cover", "-metadata:s:v", "comment=Cover (front)",
  "-disposition:v", "attached_pic", mp3Path]);

const meter = run(["-hide_banner", "-nostats", "-i", wavPath,
  "-af", "loudnorm=print_format=json", "-f", "null", "-"], true);
const meterJson = (meter.stderr.match(/\{[\s\S]*?\}/g) || []).at(-1);
if (!meterJson) throw new Error("final loudness receipt was not found");
const measured = JSON.parse(meterJson);
const probeResult = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "json", wavPath],
  { encoding: "utf8", maxBuffer: 1024 * 1024 });
if (probeResult.status !== 0) throw new Error("final duration verification failed");
const probe = JSON.parse(probeResult.stdout);
const actualDuration = Number(probe.format.duration);
if (actualDuration !== DURATION) throw new Error(`duration verification failed: ${actualDuration}s`);

const arrangement = [
  [0, 60, "far sign — beatless spatial theme"],
  [60, 90, "72 BPM half-time ignition"],
  [90, 120, "72 BPM four-floor build"],
  [120, 180, "144 BPM pop-trance arrival"],
  [180, 240, "single-pulse orbital lift"],
  [240, 280, "air pocket and rebuild"],
  [280, 340, "final pop-trance flight"],
  [340, 360, "home sign"],
].map(([startSeconds, endSeconds, name]) => ({ name, startSeconds, endSeconds }));

writeFileSync(receiptPath, JSON.stringify({
  title: "Special Sign 720",
  status: "six-minute remix variant — locked Special Sign release master remains untouched",
  renderer: relative(REPO, fileURLToPath(import.meta.url)),
  source: relative(REPO, sourcePath),
  sourceSha256: sha256(sourcePath),
  sampleRate: SR,
  bitDepth: 24,
  durationSeconds: actualDuration,
  tempo: { openingBpm: SLOW_BPM, popBpm: FAST_BPM, doubleTimeHingeSeconds: HINGE },
  key: "C major",
  arrangement,
  orchestration: ["locked Special Sign spatial field", "sine sub-bass", "material kick", "noise clap", "one hat lane", "one C-major trance pulse"],
  sourceTreatment: "seven long equal-power-edged varispeed passes; no phase vocoder, recursive feedback, or added echo",
  mastering: {
    targetIntegratedLufs: TARGET_LUFS,
    targetTruePeakDb: TARGET_TP,
    measuredPremasterLufs: Number(premixMeasured.input_i),
    measuredPremasterTruePeakDb: Number(premixMeasured.input_tp),
    linearGainDb: gainDb,
    format: "24-bit / 48 kHz stereo",
  },
  verification: {
    integratedLufs: Number(measured.input_i),
    truePeakDb: Number(measured.input_tp),
    loudnessRangeLu: Number(measured.input_lra),
    thresholdLufs: Number(measured.input_thresh),
    exactFrames: frames,
    wavSha256: sha256(wavPath),
    mp3Sha256: sha256(mp3Path),
  },
  outputs: { wav: relative(REPO, wavPath), mp3: relative(REPO, mp3Path) },
}, null, 2) + "\n");

unlinkSync(rawPath);
unlinkSync(premixPath);
console.log(`✓ ${wavPath}`);
console.log(`✓ ${mp3Path}`);
console.log(`✓ ${receiptPath}`);
