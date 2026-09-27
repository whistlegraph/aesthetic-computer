#!/usr/bin/env node
// Wattajetta's song before its production: harmony, bass, lead, and a quiet pulse.

import { mkdirSync, writeFileSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../out");
mkdirSync(OUT, { recursive: true });

const SR = 48_000;
const BPM = 132;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const BARS = 72;
const DUR = BARS * BAR + 4;
const mix = new Float32Array(Math.ceil(DUR * SR) * 2);
const TAU = Math.PI * 2;
const hz = (midi) => 440 * 2 ** ((midi - 69) / 12);
const db = (value) => 10 ** (value / 20);
const clamp = (value, low, high) => Math.max(low, Math.min(high, value));

const HARMONY = [
  // The door: minor home, a sideways C over E, then a dominant that commits.
  [0, 2, "Em(add9)", 40, [52, 59, 64, 66, 67]],
  [2, 2, "Cmaj7/E", 40, [48, 55, 59, 64]],
  [4, 2, "Am9/E", 40, [45, 52, 55, 59, 64]],
  [6, 1, "B7sus4", 35, [47, 52, 54, 57]],
  [7, 1, "B7", 35, [47, 51, 54, 57]],

  // Verse one descends while the bass climbs into the first cadence.
  [8, 2, "Em9", 40, [52, 55, 59, 66]],
  [10, 2, "D/F#", 42, [50, 54, 57, 62]],
  [12, 2, "Cmaj7", 36, [48, 52, 55, 59]],
  [14, 2, "Am9", 33, [45, 48, 52, 55, 59]],
  [16, 1, "B7sus4", 35, [47, 52, 54, 57]],
  [17, 1, "B7(b9)", 35, [47, 51, 54, 57, 60]],

  // Chorus one opens the Dorian window, then borrows V to pull home.
  [18, 2, "Em7", 40, [52, 55, 59, 62]],
  [20, 2, "A(add9)/E", 40, [45, 52, 57, 59, 64]],
  [22, 2, "Cmaj7", 36, [48, 52, 55, 59]],
  [24, 1, "B7sus4", 35, [47, 52, 54, 57]],
  [25, 1, "B7", 35, [47, 51, 54, 57]],

  // Verse two makes the Phrygian bII a real dramatic event.
  [26, 2, "Em(add9)", 40, [52, 59, 64, 66, 67]],
  [28, 2, "Fmaj7/E", 40, [53, 57, 60, 64]],
  [30, 2, "Cmaj7/E", 40, [48, 55, 59, 64]],
  [32, 2, "Am/E", 40, [45, 52, 57, 60]],
  [34, 1, "B7(b9)", 35, [47, 51, 54, 57, 60]],
  [35, 1, "B7", 35, [47, 51, 54, 57]],

  // Chorus two turns the same silhouette toward daylight.
  [36, 2, "E7sus4", 40, [52, 57, 59, 62]],
  [38, 2, "D/A", 33, [50, 54, 57, 62]],
  [40, 2, "A(add9)", 33, [45, 52, 57, 59, 64]],
  [42, 1, "B7sus4", 35, [47, 52, 54, 57]],
  [43, 1, "B7", 35, [47, 51, 54, 57]],

  // Five floating bars, then five bars of an undecorated E pedal.
  [44, 2, "Emaj7(#11)", 40, [52, 56, 59, 63, 66]],
  [46, 1, "F#7sus4", 42, [54, 59, 61, 64]],
  [47, 1, "D#m7/A#", 46, [51, 54, 58, 61]],
  [48, 1, "Amaj7/E", 40, [45, 52, 56, 61]],
  [49, 5, "E5 pedal", 40, [52, 59, 64]],

  // The payoff earns a familiar pop cadence, but arrives with added tones.
  [54, 2, "E(add9)", 40, [52, 56, 59, 66]],
  [56, 2, "B/D#", 39, [47, 51, 54, 59]],
  [58, 2, "C#m7", 37, [49, 52, 56, 59]],
  [60, 2, "Amaj9", 33, [45, 52, 56, 59, 64]],
  [62, 2, "F#m7", 30, [42, 49, 52, 57]],
  [64, 1, "B7sus4", 35, [47, 52, 54, 57]],
  [65, 1, "B7", 35, [47, 51, 54, 57]],

  // Home is neither fully minor nor major: the fifth remains after the story.
  [66, 2, "Em(add9)", 40, [52, 59, 64, 66, 67]],
  [68, 2, "Cmaj7/E", 40, [48, 55, 59, 64]],
  [70, 1, "Esus2", 40, [52, 54, 59, 64]],
  [71, 1, "E5", 40, [52, 59, 64]],
];

// One eight-bar tune. Its rhythm stays recognizable while mode color and
// cadence notes change. Intervals are measured from E4.
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

const lead = [];
const addTheme = (bar, transform = (interval) => interval, gain = 1) => {
  for (const [beat, interval, duration, velocity] of THEME)
    lead.push([bar * 4 + beat, 64 + transform(interval), duration, velocity * gain]);
};

// The verses expose only fragments; the choruses state the tune.
for (const note of [
  [2, 0, 2, 0.5], [4, 3, 1, 0.45], [6, 7, 1, 0.55], [7, 5, 1, 0.42],
  [32, 0, 1.5, 0.68], [34, 3, 0.5, 0.58], [35, 7, 1, 0.7],
  [40, 2, 1, 0.58], [42, 0, 2, 0.7], [48, 3, 1, 0.62], [50, 7, 1, 0.7],
  [104, 0, 1.5, 0.65], [106, 1, 0.5, 0.58], [107, 7, 1, 0.68],
  [112, 1, 1, 0.6], [114, 0, 2, 0.72], [120, 3, 1, 0.65], [122, 7, 1, 0.72],
  [176, 12, 3, 0.68], [181, 11, 2, 0.6], [188, 6, 3, 0.62],
  [196, 0, 4, 0.55], [208, 7, 2, 0.6],
]) lead.push(note);

addTheme(18); // Dorian: minor third, raised sixth.
addTheme(36, (n) => n === 3 ? 4 : n); // Mixolydian: the third turns major.
addTheme(54, (n) => ({ 3: 4, 5: 6, 8: 9, 10: 11 }[n] ?? n), 1.08); // Ionian payoff.

// Final answer extends beyond the eight-bar statement into the cadence.
for (const note of [
  [248, 9, 1.5, 0.86], [249.5, 13, 0.5, 0.78], [250, 12, 1, 0.9],
  [251, 9, 1, 0.8], [252, 6, 1, 0.76], [253, 9, 1, 0.84],
  [254, 11, 1, 0.9], [255, 12, 2, 1],
  [264, 7, 2, 0.68], [268, 3, 1, 0.58], [270, 2, 1, 0.54],
  [272, 0, 3, 0.68], [280, 2, 2, 0.5], [284, 0, 4, 0.6],
]) lead.push(note);

function addStereo(sample, pan, index) {
  if (index < 0 || index >= mix.length / 2) return;
  const angle = (clamp(pan, -1, 1) + 1) * Math.PI / 4;
  mix[index * 2] += sample * Math.cos(angle);
  mix[index * 2 + 1] += sample * Math.sin(angle);
}

function padNote(at, duration, midi, level, pan) {
  const start = Math.floor(at * SR);
  const samples = Math.floor((duration + 1.8) * SR);
  const freq = hz(midi);
  for (let i = 0; i < samples; i++) {
    const t = i / SR;
    const attack = Math.min(1, t / 0.18);
    const release = t < duration ? 1 : Math.max(0, 1 - (t - duration) / 1.8);
    const drift = 1 + 0.0015 * Math.sin(TAU * 0.11 * (at + t) + midi);
    const phase = TAU * freq * drift * t;
    const voice = 0.68 * Math.sin(phase) + 0.2 * Math.sin(phase * 2 + 0.4) + 0.12 * Math.sin(phase * 0.5);
    addStereo(voice * attack * release * level, pan, start + i);
  }
}

function bassNote(at, duration, midi) {
  const start = Math.floor(at * SR);
  const samples = Math.floor(duration * SR);
  const freq = hz(midi);
  for (let i = 0; i < samples; i++) {
    const t = i / SR;
    const env = Math.min(1, t / 0.04) * Math.min(1, (duration - t) / 0.3);
    addStereo((Math.sin(TAU * freq * t) + 0.18 * Math.sin(TAU * freq * 2 * t)) * env * db(-20), 0, start + i);
  }
}

function leadNote(at, duration, midi, velocity) {
  const start = Math.floor(at * SR);
  const seconds = duration + 0.8;
  const samples = Math.floor(seconds * SR);
  const freq = hz(midi);
  for (let i = 0; i < samples; i++) {
    const t = i / SR;
    const attack = Math.min(1, t / 0.012);
    const decay = Math.exp(-t / Math.max(0.35, duration * 0.85));
    const release = t < duration ? 1 : Math.max(0, 1 - (t - duration) / 0.8);
    const tone = 0.78 * Math.sin(TAU * freq * t) + 0.17 * Math.sin(TAU * freq * 2.01 * t) + 0.05 * Math.sin(TAU * freq * 3.98 * t);
    addStereo(tone * attack * decay * release * velocity * db(-10), 0.08 * Math.sin(at * 0.17), start + i);
  }
}

for (const [startBar, bars, name, bass, notes] of HARMONY) {
  const at = startBar * BAR;
  const duration = bars * BAR;
  bassNote(at, duration - 0.04, bass);
  notes.forEach((midi, index) => padNote(at + index * 0.025, duration - 0.08, midi, db(-27), (index - (notes.length - 1) / 2) * 0.2));
  console.log(`${String(startBar).padStart(2, "0")}–${String(startBar + bars).padStart(2, "0")}  ${name}`);
}

for (const [beat, midi, beats, velocity] of lead)
  leadNote(beat * BEAT, beats * BEAT, midi, velocity);

// A heartbeat makes phrase placement audible without deciding the dance feel.
for (let bar = 0; bar < BARS; bar++) for (const beat of [0, 2]) {
  const start = Math.floor((bar * 4 + beat) * BEAT * SR);
  const samples = Math.floor(0.18 * SR);
  for (let i = 0; i < samples; i++) {
    const t = i / SR;
    const phase = TAU * (58 * t - 18 * t * t);
    addStereo(Math.sin(phase) * Math.exp(-t / 0.055) * db(-29), 0, start + i);
  }
}

let peak = 0;
for (const sample of mix) peak = Math.max(peak, Math.abs(sample));
const gain = peak > 0 ? 0.82 / peak : 1;
for (let i = 0; i < mix.length; i++) mix[i] = Math.tanh(mix[i] * gain * 1.05) / Math.tanh(1.05);

const wavPath = resolve(OUT, "wattajetta-composition-study.wav");
const mp3Path = resolve(OUT, "wattajetta-composition-study.mp3");
const dataBytes = mix.length * 4;
const header = Buffer.alloc(44);
header.write("RIFF", 0);
header.writeUInt32LE(36 + dataBytes, 4);
header.write("WAVE", 8);
header.write("fmt ", 12);
header.writeUInt32LE(16, 16);
header.writeUInt16LE(3, 20); // IEEE float
header.writeUInt16LE(2, 22);
header.writeUInt32LE(SR, 24);
header.writeUInt32LE(SR * 8, 28);
header.writeUInt16LE(8, 32);
header.writeUInt16LE(32, 34);
header.write("data", 36);
header.writeUInt32LE(dataBytes, 40);
writeFileSync(wavPath, Buffer.concat([header, Buffer.from(mix.buffer)]));

const encoded = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wavPath,
  "-codec:a", "libmp3lame", "-b:a", "256k", "-metadata", "title=Wattajetta composition study",
  "-metadata", "comment=Harmony and melody only", mp3Path], { stdio: "inherit" });
if (encoded.status !== 0) process.exit(encoded.status ?? 1);
console.log(`\n✓ ${mp3Path}`);
