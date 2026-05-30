#!/usr/bin/env node
// profile-vocal.mjs — measure dryness of an ElevenLabs jeffrey-pvc render.
//
// /api/say returns one of two rolls roughly 50/50: a CLEAN dry take and
// a WET reverb-tail take. We need the dry one. This tool decodes the mp3
// to PCM via ffmpeg, frames the signal at 20ms, and computes:
//
//   floor   = median RMS of the quietest 25% of frames (between-word floor)
//   active  = median RMS of the loudest 25% of frames (peak speech level)
//   ratio   = floor / active           (higher = reverby tail bleeds into silence)
//   tail50  = mean RMS in the 50–150ms window AFTER each detected silence
//             onset, normalised to active                (room-tail decay test)
//
// PASS rule (default): ratio < 0.04. tail50 is informational only.
//
// Calibration (2026-05-26): A/B comparison on ac24-may-26 confirmed that
// floor/active ratio is the meaningful discriminator between the wet and
// dry ElevenLabs rolls; tail50 sits ~0.40 for both wet and dry takes
// (natural voice decay, not reverb). Kept tail50 in the report for
// reference but no longer hard-gates the verdict.
//
// Usage:
//   node bin/profile-vocal.mjs <mp3>
//   node bin/profile-vocal.mjs <mp3> --json
//   node bin/profile-vocal.mjs <mp3> --ratio-max 0.04

import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { resolve } from "node:path";

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2);
    const n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; }
    else flags[k] = true;
  } else positional.push(a);
}

const mp3 = positional[0] && resolve(process.cwd(), positional[0]);
if (!mp3 || !existsSync(mp3)) {
  console.error("usage: profile-vocal.mjs <mp3> [--json] [--ratio-max 0.04] [--tail-max 0.08]");
  process.exit(2);
}

const RATIO_MAX = Number(flags["ratio-max"] ?? 0.04);
// tail50 stays in the report but no longer gates; set very loose default.
const TAIL_MAX  = Number(flags["tail-max"]  ?? 1.0);
const JSON_OUT  = flags.json === true;
const SR = 16000;
const FRAME = Math.round(SR * 0.02);  // 20ms

// ── decode to mono 16k s16le ──────────────────────────────────────────
const ff = spawnSync("ffmpeg", [
  "-v", "error", "-i", mp3, "-ac", "1", "-ar", String(SR),
  "-f", "s16le", "-",
], { maxBuffer: 1024 * 1024 * 256 });
if (ff.status !== 0) {
  console.error(ff.stderr.toString());
  process.exit(1);
}
const buf = ff.stdout;
const samples = new Int16Array(buf.buffer, buf.byteOffset, buf.byteLength / 2);

// ── per-frame RMS ─────────────────────────────────────────────────────
const nFrames = Math.floor(samples.length / FRAME);
const rms = new Float64Array(nFrames);
for (let i = 0; i < nFrames; i++) {
  let sum = 0;
  const start = i * FRAME;
  for (let j = 0; j < FRAME; j++) {
    const v = samples[start + j] / 32768;
    sum += v * v;
  }
  rms[i] = Math.sqrt(sum / FRAME);
}

// ── floor / active median ─────────────────────────────────────────────
const sorted = Float64Array.from(rms).sort();
function medianRange(lo, hi) {
  const a = sorted.slice(Math.floor(lo * sorted.length), Math.floor(hi * sorted.length));
  return a[Math.floor(a.length / 2)];
}
const floor = medianRange(0.05, 0.25);    // quietest band (skip absolute zeros)
const active = medianRange(0.75, 0.95);   // peak band
const ratio = floor / Math.max(active, 1e-9);

// ── tail-after-silence-onset test ─────────────────────────────────────
// Find frames where energy drops from "speech" → "silence" and measure
// the RMS in the 50–150ms window AFTER the drop (= reverb tail).
const speechThresh = active * 0.20;
const silentThresh = active * 0.08;
const tailWindowStart = 0.05 / 0.02;   // 50ms = 2.5 frames
const tailWindowEnd   = 0.15 / 0.02;   // 150ms = 7.5 frames
const tailStart = Math.round(tailWindowStart);
const tailEnd   = Math.round(tailWindowEnd);

let tailSum = 0;
let tailN = 0;
let inSpeech = false;
for (let i = 0; i < nFrames; i++) {
  if (!inSpeech && rms[i] > speechThresh) inSpeech = true;
  else if (inSpeech && rms[i] < silentThresh) {
    inSpeech = false;
    // measure tail
    for (let k = i + tailStart; k <= i + tailEnd && k < nFrames; k++) {
      tailSum += rms[k];
      tailN++;
    }
  }
}
const tail50 = tailN > 0 ? (tailSum / tailN) / Math.max(active, 1e-9) : 0;

const pass = ratio < RATIO_MAX && tail50 < TAIL_MAX;

const result = {
  mp3,
  durationSec: samples.length / SR,
  floor: +floor.toFixed(5),
  active: +active.toFixed(5),
  ratio: +ratio.toFixed(4),
  tail50: +tail50.toFixed(4),
  thresholds: { ratioMax: RATIO_MAX, tailMax: TAIL_MAX },
  pass,
  verdict: pass ? "DRY" : "WET",
};

if (JSON_OUT) {
  process.stdout.write(JSON.stringify(result, null, 2) + "\n");
} else {
  console.log(`  file:     ${mp3}`);
  console.log(`  duration: ${result.durationSec.toFixed(2)}s`);
  console.log(`  floor:    ${result.floor}   (between-word RMS)`);
  console.log(`  active:   ${result.active}   (peak speech RMS)`);
  console.log(`  ratio:    ${result.ratio}   (< ${RATIO_MAX} → dry)`);
  console.log(`  tail50:   ${result.tail50}   (< ${TAIL_MAX} → dry)`);
  console.log(`  verdict:  ${result.verdict}${pass ? " ✓" : " ✗"}`);
}

process.exit(pass ? 0 : 1);
