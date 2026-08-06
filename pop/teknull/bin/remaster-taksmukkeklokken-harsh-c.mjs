#!/usr/bin/env node
// Re-cut and musically rebalance the spatial/orchestral harsh-C mix.
// Keeps the source immutable and adds restrained, transport-locked dance reinforcement.

import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { dirname, resolve, relative } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out");
const SOURCE = resolve(OUT, "taksmukkeklokken-spatial-orchestral-harsh-c.mp3");
const TARGET = resolve(OUT, "taksmukkeklokken-spatial-orchestral-harsh-c-remaster.mp3");
const RECEIPT = resolve(OUT, "taksmukkeklokken-spatial-orchestral-harsh-c-remaster.json");
const RAW = resolve(OUT, ".taksmukkeklokken-harsh-c-remaster-percussion.f32le");
const SR = 48_000;
const BPM = 140;
const BEAT = 60 / BPM;
// Bar 10's downbeat (zero-indexed bar 9): the musical boundary nearest 0:16.
const SOURCE_START = 9 * 4 * BEAT; // 15.428571428571429
const SOURCE_DURATION = 120;
const DURATION = SOURCE_DURATION - SOURCE_START;

mkdirSync(OUT, { recursive: true });

// Ground the inherited orchestration with a centered classic kick and a short,
// cool backbeat. These are reinforcement, not a replacement drum track.
const frames = Math.floor(DURATION * SR);
const pcm = new Float32Array(frames * 2);
let noiseState = 0x51c1c;
const noise = () => {
  noiseState = (Math.imul(noiseState, 1664525) + 1013904223) >>> 0;
  return noiseState / 0x80000000 - 1;
};
function addMono(time, length, sample) {
  const first = Math.round(time * SR);
  const count = Math.min(Math.round(length * SR), frames - first);
  for (let i = 0; i < count; i++) {
    const value = sample(i / SR, i);
    pcm[(first + i) * 2] += value;
    pcm[(first + i) * 2 + 1] += value;
  }
}
for (let beat = 0; beat * BEAT < DURATION; beat++) {
  const at = beat * BEAT;
  addMono(at, 0.34, (x) => {
    const phase = 2 * Math.PI * (49 * x + 2.7 * (1 - Math.exp(-x / 0.027)));
    const click = Math.sin(2 * Math.PI * 1450 * x) * Math.exp(-x / 0.006);
    return 0.175 * Math.sin(phase) * Math.exp(-x / 0.115) + 0.016 * click;
  });
  if (beat % 4 === 1 || beat % 4 === 3) {
    addMono(at, 0.115, (x) => {
      const env = Math.exp(-x / 0.032);
      const body = Math.sin(2 * Math.PI * 184 * x) * Math.exp(-x / 0.055);
      const grit = (noise() - 0.55 * noise()) * env;
      return 0.045 * body + 0.072 * grit;
    });
  }
}
writeFileSync(RAW, Buffer.from(pcm.buffer));

const filter = [
  // Steady the image, mono the sub foundation, and soften the painful bands.
  "[0:a]atrim=start=" + SOURCE_START.toFixed(12) + ",asetpts=PTS-STARTPTS,",
  "afade=t=in:st=0:d=0.012,afade=t=out:st=" + (DURATION - 0.12).toFixed(6) + ":d=0.12,",
  "highpass=f=27,equalizer=f=3150:t=q:w=1.15:g=-2.8,equalizer=f=6900:t=q:w=0.9:g=-2.1,",
  "equalizer=f=1150:t=q:w=0.75:g=1.45,stereotools=mlev=1.06:slev=0.78,asplit=2[main][low];",
  "[low]lowpass=f=145,pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1,volume=0.24[sub];",
  "[main][sub][1:a]amix=inputs=3:weights='1 1 0.78':normalize=0,",
  "acompressor=threshold=0.16:ratio=2.1:attack=18:release=150:makeup=1.05,",
  "loudnorm=I=-13.0:LRA=6.0:TP=-1.0:linear=false,aresample=48000[out]",
].join("");

const ffmpegArgs = [
  "-y", "-hide_banner", "-loglevel", "warning",
  "-i", SOURCE,
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", RAW,
  "-filter_complex", filter, "-map", "[out]", "-t", DURATION.toFixed(9),
  "-ar", String(SR), "-ac", "2", "-codec:a", "libmp3lame", "-b:a", "320k", TARGET,
];
const rendered = spawnSync("ffmpeg", ffmpegArgs, { stdio: "inherit" });
rmSync(RAW, { force: true });
if (rendered.status !== 0) process.exit(rendered.status ?? 1);

const sha256 = (path) => createHash("sha256").update(readFileSync(path)).digest("hex");
writeFileSync(RECEIPT, JSON.stringify({
  schema: "aesthetic.computer/pop-remaster/v1",
  source: relative(ROOT, SOURCE),
  output: relative(ROOT, TARGET),
  sourceSha256: sha256(SOURCE),
  outputSha256: sha256(TARGET),
  sourceStartSec: SOURCE_START,
  sourceStartReason: "140 BPM bar downbeat nearest requested 0:16 (bar 10 begins at 15.428571s)",
  durationSec: DURATION,
  sampleRate: SR,
  channels: 2,
  changes: [
    "12 ms boundary fade and 120 ms tail fade",
    "centered sub reinforcement below 145 Hz and restrained stereo-side level",
    "classic transport-locked kick on quarter notes and tight snare on beats 2/4",
    "upper-mid/high resonance cuts at 3.15 kHz and 6.9 kHz",
    "broad 1.15 kHz melody-presence lift",
    "gentle bus compression and -13 LUFS / -1 dBTP final loudness target",
  ],
  renderer: relative(ROOT, fileURLToPath(import.meta.url)),
  ffmpegArgs,
}, null, 2) + "\n");
console.log(TARGET);
