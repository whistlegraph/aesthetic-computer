#!/usr/bin/env node
// audio-to-rhythm.mjs — turn an audio loop into a .np drum sequence.
//
// Detects onsets in a WAV (audioGate from lib/analysis.mjs), classifies
// each hit low/mid/high by its zero-crossing rate, quantizes the onset
// times to a beat grid, and writes a monophonic .np where each cell's
// weight is the beats until the next hit. Feed in a loop of someone
// talking / beatboxing and get a playable rhythm back.
//
// Usage:
//   node pop/bin/audio-to-rhythm.mjs --wav loop.wav --bpm 120
//   node pop/bin/audio-to-rhythm.mjs --wav loop.wav --bpm 96 --grid 0.25 --out beat.np
//
// Flags:
//   --wav    input WAV (required)
//   --bpm    tempo for quantization        (default 120)
//   --grid   grid in beats — 0.25 = 16ths  (default 0.25)
//   --thresh onset gate level 0..1         (default 0.06)
//   --out    write .np here                (default: stdout)
//
// Leading silence before the first hit is dropped — .np durations are
// relative, so the sequence starts on the first onset.

import { writeFileSync } from "node:fs";
import { resolve, basename } from "node:path";
import { readWavMono, audioGate } from "../lib/analysis.mjs";

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
  else flags[a.slice(2)] = true;
}

if (!flags.wav) {
  console.error("provide --wav path/to/loop.wav  (see --help in the header)");
  process.exit(1);
}

const bpm    = parseFloat(flags.bpm) || 120;
const grid   = parseFloat(flags.grid) || 0.25;
const thresh = parseFloat(flags.thresh) || 0.06;
const wavPath = resolve(process.cwd(), flags.wav);

const { samples, sampleRate } = readWavMono(wavPath);
const onsets = audioGate(samples, { sampleRate, threshold: thresh });

if (onsets.length === 0) {
  console.error("no onsets detected — try a lower --thresh");
  process.exit(1);
}

// Classify a hit by the zero-crossing rate of a short window after it:
// low ZCR = bassy (kick), high ZCR = noisy/bright (hat).
function classify(startSec) {
  const start = Math.floor(startSec * sampleRate);
  const win = Math.min(1024, samples.length - start);
  if (win < 32) return { note: "C4", syll: "t" };
  let crossings = 0;
  for (let j = 1; j < win; j++) {
    if ((samples[start + j - 1] < 0) !== (samples[start + j] < 0)) crossings++;
  }
  const zcr = crossings / win;
  if (zcr < 0.06) return { note: "C3", syll: "k" }; // kick
  if (zcr < 0.18) return { note: "C4", syll: "t" }; // tom / body
  return { note: "C5", syll: "h" };                  // hat
}

// Quantize onset times → beat positions on the grid, de-duplicating
// hits that collapse onto the same step.
const beatPerSec = bpm / 60;
const hits = [];
let lastStep = -1;
for (const o of onsets) {
  const beat = o.time * beatPerSec;
  const step = Math.round(beat / grid);
  if (step === lastStep) continue;
  lastStep = step;
  hits.push({ step, ...classify(o.time) });
}

// Build cells — each weight is the beats until the next hit.
const cells = [];
for (let i = 0; i < hits.length; i++) {
  const beats = i < hits.length - 1
    ? (hits[i + 1].step - hits[i].step) * grid
    : grid; // last hit gets one grid step
  const w = Number(beats.toFixed(3));
  cells.push(`${hits[i].note}:${hits[i].syll}*${w}`);
}

// Lay cells out four-beats-per-line for readability.
const lines = [];
let lineBeats = 0, line = [];
for (let i = 0; i < cells.length; i++) {
  line.push(cells[i]);
  lineBeats += hits[i + 1] ? (hits[i + 1].step - hits[i].step) * grid : grid;
  if (lineBeats >= 4) { lines.push(line.join(" ")); line = []; lineBeats = 0; }
}
if (line.length) lines.push(line.join(" "));

const totalBeats = (hits[hits.length - 1].step - hits[0].step) * grid + grid;
const bars = Math.max(1, Math.ceil(totalBeats / 4));
const np =
  `# rhythm ${bars} — extracted from ${basename(wavPath)} @ ${bpm} bpm\n` +
  `# ${hits.length} hits · ${grid}-beat grid · k=kick t=tom h=hat\n\n` +
  lines.join("\n") + "\n";

if (flags.out) {
  const outPath = resolve(process.cwd(), flags.out);
  writeFileSync(outPath, np);
  console.log(`rhythm → ${outPath}  (${hits.length} hits, ${bars} bars)`);
} else {
  process.stdout.write(np);
}
