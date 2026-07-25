#!/usr/bin/env node
// Special Sign — Fast Pop / Chopped + Screwed fork.
//
// An intentionally isolated, reproducible arrangement built from the current
// dry-leaning Special Sign audition. It never writes into the release package
// or any shared Special Sign render path.

import { existsSync, mkdirSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../../..");
const SR = 48_000;
const BPM = 148;
const BEAT = 60 / BPM;
const BAR = BEAT * 4;
const BARS = 136;
const DURATION = BARS * BAR;
const TARGET_LUFS = -11;
const TARGET_TP = -1.0;

const arg = (name, fallback) => {
  const index = process.argv.indexOf(name);
  return index >= 0 && process.argv[index + 1] ? process.argv[index + 1] : fallback;
};
const sourcePath = resolve(arg("--source", resolve(HERE, "../../out/review/Special-Sign-SLIGHTLY-DRIER-AUDITION.wav")));
const outputDir = resolve(arg("--output-dir", resolve(HERE, "../../out/review/variants/special-sign-fast-pop")));
const wavPath = resolve(outputDir, "Special-Sign-FAST-POP-FORK.wav");
const mp3Path = resolve(outputDir, "Special-Sign-FAST-POP-FORK.mp3");
const receiptPath = resolve(outputDir, "Special-Sign-FAST-POP-FORK.provenance.json");
const rawPath = resolve(outputDir, ".Special-Sign-FAST-POP-FORK.f32");
const premixPath = resolve(outputDir, ".Special-Sign-FAST-POP-FORK-premix.wav");

if (!existsSync(sourcePath)) throw new Error(`missing source: ${sourcePath}`);
mkdirSync(outputDir, { recursive: true });

const run = (args, capture = false) => {
  const result = spawnSync("ffmpeg", args, capture
    ? { encoding: args.includes("-") ? null : "utf8", maxBuffer: 512 * 1024 * 1024 }
    : { stdio: "inherit" });
  if (result.status !== 0) throw new Error(`ffmpeg failed (${result.status})`);
  return result;
};
const decode = (path) => {
  const result = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-i", path,
    "-f", "f32le", "-ar", String(SR), "-ac", "2", "-"],
  { maxBuffer: 512 * 1024 * 1024 });
  if (result.status !== 0) throw new Error(`could not decode ${path}`);
  return new Float32Array(result.stdout.buffer, result.stdout.byteOffset, result.stdout.byteLength / 4);
};
const clamp = (v, lo = 0, hi = 1) => Math.max(lo, Math.min(hi, v));
const smooth = (v) => { const u = clamp(v); return u * u * (3 - 2 * u); };

console.log(`[fork] decoding ${relative(REPO, sourcePath)}`);
const source = decode(sourcePath);
const sourceFrames = source.length / 2;
const frames = Math.round(DURATION * SR);
const music = new Float32Array(frames * 2);

// Each clip has tiny equal-power edges. The source can therefore be cut,
// reversed, repeated, or tape-varispeeded without discontinuity clicks.
const clips = [];
const addClip = ({ bar, beats = 0, bars, seconds, src, rate = 1, gain = 1, pan = 0, reverse = false, fade = 0.018, tag }) => {
  const start = bar * BAR + beats * BEAT;
  const duration = seconds ?? bars * BAR;
  clips.push({ start, duration, src, rate: reverse ? -Math.abs(rate) : rate, gain, pan, fade, tag });
};

// Pop architecture: immediate signature, two drops, a central chopped refrain,
// an unmistakably tape-slowed breakdown, then a longer double final chorus.
addClip({ bar: 0, bars: 8, src: 0.0, rate: 1.00, gain: 0.52, tag: "orbital intro" });
addClip({ bar: 8, bars: 16, src: 8.0, rate: 1.19, gain: 0.61, tag: "ignition" });
addClip({ bar: 24, bars: 24, src: 12.6, rate: 1.20, gain: 0.56, tag: "drop one" });
addClip({ bar: 48, bars: 16, src: 39.5, rate: 0.98, gain: 0.22, tag: "chop bed" });
addClip({ bar: 64, bars: 16, src: 56.8, rate: 0.68, gain: 0.64, tag: "screwed breakdown" });
addClip({ bar: 80, bars: 16, src: 23.0, rate: 1.22, gain: 0.48, tag: "rebuild" });
addClip({ bar: 96, bars: 32, src: 38.0, rate: 1.18, gain: 0.56, tag: "double final drop" });
addClip({ bar: 128, bars: 8, src: 84.0, rate: 1.12, gain: 0.57, tag: "orbit exit" });

// Beat-grid refrains: 1/2-beat grains alternate with held beat fragments. The
// second half answers in reverse, and the build progressively shortens to
// sixteenth-note flashes of the sine/Jeffrey field.
for (let b = 48; b < 64; b++) {
  for (let beat = 0; beat < 4; beat++) {
    const n = (b - 48) * 4 + beat;
    const reverse = n % 8 === 6 || n % 8 === 7;
    addClip({ bar: b, beats: beat, seconds: BEAT * (n % 4 === 3 ? 0.96 : 0.52),
      src: 42.1 + (n % 12) * 0.41, rate: reverse ? 0.86 : 1.18,
      gain: reverse ? 0.57 : 0.66, pan: (n % 2 ? 0.16 : -0.16), reverse, fade: 0.011, tag: "refrain grain" });
  }
}
for (let b = 80; b < 96; b++) {
  const subdivision = b < 88 ? 2 : b < 92 ? 4 : 8;
  for (let step = 0; step < subdivision * 4; step++) {
    const unit = BEAT / subdivision;
    addClip({ bar: b, beats: step / subdivision, seconds: unit * 0.61,
      src: 30.2 + ((b * 7 + step * 3) % 31) * 0.37, rate: 1.08 + step % 3 * 0.12,
      gain: 0.15 + 0.21 * ((b - 80) / 16), pan: step % 2 ? 0.28 : -0.28,
      reverse: step % 11 === 10, fade: 0.006, tag: "build microchop" });
  }
}

console.log(`[fork] arranging ${clips.length} source clips`);
for (const clip of clips) {
  const outStart = Math.max(0, Math.round(clip.start * SR));
  const count = Math.min(frames - outStart, Math.round(clip.duration * SR));
  const panAngle = (clamp(clip.pan, -1, 1) + 1) * Math.PI / 4;
  const pL = Math.cos(panAngle) * Math.SQRT2;
  const pR = Math.sin(panAngle) * Math.SQRT2;
  for (let i = 0; i < count; i++) {
    const local = i / SR;
    const srcTime = clip.rate >= 0
      ? clip.src + local * clip.rate
      : clip.src + (clip.duration - local) * -clip.rate;
    const sf = srcTime * SR;
    const s0 = Math.floor(sf);
    if (s0 < 0 || s0 + 1 >= sourceFrames) continue;
    const f = sf - s0;
    const edge = Math.min(1, local / clip.fade, (clip.duration - local) / clip.fade);
    const env = Math.sin(clamp(edge) * Math.PI / 2) * clip.gain;
    const out = (outStart + i) * 2;
    const si = s0 * 2;
    music[out] += (source[si] + (source[si + 2] - source[si]) * f) * env * pL;
    music[out + 1] += (source[si + 1] + (source[si + 3] - source[si + 1]) * f) * env * pR;
  }
}

const roots = [29, 31, 24, 21]; // F1, G1, C1, A0: original bass, one octave down.
const hz = (midi) => 440 * 2 ** ((midi - 69) / 12);
const kickSample = (age) => {
  if (age < 0 || age > 0.43) return 0;
  const f0 = 184, f1 = 46, tau = 0.028;
  const phase = Math.PI * 2 * (f1 * age + (f0 - f1) * tau * (1 - Math.exp(-age / tau)));
  const body = Math.sin(phase) * Math.exp(-age / 0.15);
  const edge = Math.sin(Math.PI * 2 * 4700 * age) * Math.exp(-age / 0.008);
  return body * 0.86 + edge * 0.15;
};
const noiseAt = (index) => {
  let x = (index + 0x9e3779b9) | 0;
  x ^= x << 13; x ^= x >>> 17; x ^= x << 5;
  return (x | 0) / 2147483648;
};
const sectionAt = (bar) => bar < 8 ? "intro" : bar < 24 ? "ignition" : bar < 48 ? "drop1"
  : bar < 64 ? "chops" : bar < 80 ? "screwed" : bar < 96 ? "build" : bar < 128 ? "drop2" : "outro";

console.log("[fork] synthesizing dance chassis and super-deep sub");
let lowNoise = 0;
for (let frame = 0; frame < frames; frame++) {
  const t = frame / SR;
  const barFloat = t / BAR;
  const bar = Math.floor(barFloat);
  const section = sectionAt(bar);
  const beatFloat = t / BEAT;
  const beatIndex = Math.floor(beatFloat);
  const beatAge = (beatFloat - beatIndex) * BEAT;
  const barBeat = ((beatFloat % 4) + 4) % 4;
  const introEnergy = smooth((barFloat - 2) / 5);
  const outroEnergy = 1 - smooth((barFloat - 130) / 6);
  const energy = section === "intro" ? introEnergy * 0.55 : section === "screwed" ? 0.62
    : section === "build" ? 0.58 + 0.42 * ((bar - 80) / 16) : section === "outro" ? outroEnergy : 1;
  const fourFloor = section !== "intro" || bar >= 4;
  const kickAge = section === "screwed" ? ((beatFloat % 2 + 2) % 2) * BEAT : beatAge;
  const kick = fourFloor ? kickSample(kickAge) * (section === "screwed" ? 0.60 : 0.72) * energy : 0;
  const duck = fourFloor && kickAge < 0.46 ? 1 - 0.46 * Math.exp(-kickAge / 0.115) * energy : 1;

  // Two-beat sub pulses follow the source's F-G-C-A loop. The sine fundamental
  // stays clean at 27.5–49 Hz; a quiet saturated harmonic keeps it audible on
  // smaller speakers without turning the mix cloudy.
  const twoBeatIndex = Math.floor(beatFloat / 2);
  const bassAge = ((beatFloat % 2 + 2) % 2) * BEAT;
  const root = roots[Math.floor(bar / 2) % roots.length];
  const bassHz = hz(root);
  const bassEnv = smooth(bassAge / 0.012) * Math.exp(-bassAge / (section === "screwed" ? 0.72 : 0.48));
  const bassPhase = Math.PI * 2 * bassHz * (t - twoBeatIndex * 2 * BEAT);
  let sub = (Math.sin(bassPhase) * 0.82 + Math.tanh(2.2 * Math.sin(bassPhase)) * 0.15) * bassEnv;
  if (section === "intro" && bar < 6) sub *= 0;
  if (section === "build") sub *= 0.45 + 0.55 * ((bar - 80) / 16);
  sub *= 0.34 * energy * duck;

  // Dry pop clap on two/four; tight eighth hats grow to sixteenths in the build.
  const clapAge = Math.min(Math.abs(barBeat - 1), Math.abs(barBeat - 3)) * BEAT;
  const clap = clapAge < 0.13 ? noiseAt(frame) * Math.exp(-clapAge / 0.037) * 0.105 * energy : 0;
  const hatRate = section === "build" && bar >= 88 ? 4 : 2;
  const hatUnit = BEAT / hatRate;
  const hatAge = ((t % hatUnit) + hatUnit) % hatUnit;
  const hat = hatAge < 0.055 && (hatRate === 4 || beatIndex % 2 || section === "drop1" || section === "drop2")
    ? (noiseAt(frame * 7) - lowNoise) * Math.exp(-hatAge / 0.018) * 0.043 * energy : 0;
  lowNoise += 0.02 * (noiseAt(frame * 7) - lowNoise);

  // Reverse-material-kick intakes announce every major transition.
  let intake = 0;
  for (const boundary of [8, 24, 48, 64, 80, 96, 128]) {
    const remaining = boundary * BAR - t;
    if (remaining >= 0 && remaining < 0.43) {
      // Descending source age is literal reverse playback: tail first, beater
      // last, terminating exactly underneath the forward transition kick.
      intake += kickSample(remaining) * smooth((0.43 - remaining) / 0.43) * 0.20;
    }
  }

  const i = frame * 2;
  let l = music[i] * duck + sub + kick + clap * 0.83 + hat * 0.75 + intake;
  let r = music[i + 1] * duck + sub + kick + clap * 1.00 - hat * 0.75 + intake;
  // Very gentle safety saturation belongs to the dance fork's mastering
  // material; it catches coincident chops before the true-peak limiter.
  l = Math.tanh(l * 0.94) / Math.tanh(0.94);
  r = Math.tanh(r * 0.94) / Math.tanh(0.94);
  const endFade = t > DURATION - 5 ? smooth((DURATION - t) / 5) : 1;
  music[i] = l * endFade;
  music[i + 1] = r * endFade;
}

writeFileSync(rawPath, Buffer.from(music.buffer));
run(["-hide_banner", "-y", "-loglevel", "error", "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", "highpass=f=22,alimiter=limit=0.89:attack=5:release=110:level=disabled",
  "-c:a", "pcm_f32le", premixPath]);

const loudnorm = `loudnorm=I=${TARGET_LUFS}:TP=${TARGET_TP}:LRA=8:print_format=summary`;
run(["-hide_banner", "-y", "-loglevel", "error", "-i", premixPath, "-af", loudnorm,
  "-ar", String(SR), "-c:a", "pcm_s24le", wavPath]);
run(["-hide_banner", "-y", "-loglevel", "error", "-i", premixPath, "-af", loudnorm,
  "-ar", String(SR), "-c:a", "libmp3lame", "-b:a", "320k", mp3Path]);
unlinkSync(rawPath);
unlinkSync(premixPath);

const meter = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-i", wavPath,
  "-af", "loudnorm=print_format=json", "-f", "null", "-"],
{ encoding: "utf8", maxBuffer: 16 * 1024 * 1024 });
if (meter.status !== 0) throw new Error("final loudness verification failed");
const meterJson = (meter.stderr.match(/\{[\s\S]*?\}/g) || []).at(-1);
if (!meterJson) throw new Error("final loudness receipt was not found");
const measured = JSON.parse(meterJson);
const sha256 = (path) => createHash("sha256").update(readFileSync(path)).digest("hex");

const sections = [
  [0, 8, "orbital intro"], [8, 24, "ignition"], [24, 48, "drop one"],
  [48, 64, "chopped refrain"], [64, 80, "tape-screwed breakdown"],
  [80, 96, "microchop rebuild"], [96, 128, "double final drop"], [128, 136, "orbit exit"],
].map(([startBar, endBar, name]) => ({ name, startBar, endBar,
  startSeconds: +(startBar * BAR).toFixed(6), endSeconds: +(endBar * BAR).toFixed(6) }));
writeFileSync(receiptPath, JSON.stringify({
  title: "Special Sign (Fast Pop / Chopped + Screwed Fork)",
  status: "exploratory variant — not the Special Sign release master",
  renderer: relative(REPO, fileURLToPath(import.meta.url)),
  source: relative(REPO, sourcePath), sampleRate: SR, bitDepth: 24, bpm: BPM,
  bars: BARS, durationSeconds: +DURATION.toFixed(6),
  arrangement: sections,
  sourceTreatment: "varispeed arrangement with equal-power-edged beat grains, reversals, and a 0.68x tape-screwed breakdown",
  bass: { rootsMidi: roots, rangeHz: roots.map(hz), description: "octave-down F-G-C-A sub with quiet saturated harmonic" },
  rhythm: "four-on-the-floor dry material kick, whole-arrangement sidechain, pop claps, accelerating hats, reverse-kick transition intakes",
  mastering: { targetIntegratedLufs: TARGET_LUFS, targetTruePeakDb: TARGET_TP, highpassHz: 22, format: "24-bit / 48 kHz stereo" },
  verification: { integratedLufs: Number(measured.input_i), truePeakDb: Number(measured.input_tp),
    loudnessRangeLu: Number(measured.input_lra), thresholdLufs: Number(measured.input_thresh),
    wavSha256: sha256(wavPath), mp3Sha256: sha256(mp3Path) },
  outputs: { wav: relative(REPO, wavPath), mp3: relative(REPO, mp3Path) },
}, null, 2) + "\n");
console.log(`✓ ${wavPath}`);
console.log(`✓ ${mp3Path}`);
console.log(`✓ ${receiptPath}`);
