#!/usr/bin/env node
// beat.mjs — synthesize a low-key kick track from the waltz timing,
// one kick on every bar's beat 1. Mixed under the narration in
// compose.fish for a touch of musical pulse.
//
// Reads:  recap/out/waltz-events.json (for bpm + totalSec)
// Writes: recap/out/beat.mp3
//
// Kick: 60Hz sine, 0.18s, exponential amplitude decay. Quiet enough
// to sit under the narration without competing.
//
// Usage: node bin/beat.mjs

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const eventsPath = `${ROOT}/out/waltz-events.json`;
const rawPath = `${ROOT}/out/beat.f32.raw`;
const outPath = `${ROOT}/out/beat.mp3`;

if (!existsSync(eventsPath)) { console.error(`✗ missing ${eventsPath}`); process.exit(1); }
const { bpm, totalSec } = JSON.parse(readFileSync(eventsPath, "utf8"));
const SR = 48000;
const beatSec = 60 / bpm;
const barSec = beatSec * 3;
const totalSamples = Math.ceil((totalSec + 1) * SR);
const out = new Float32Array(totalSamples);

// Kick voice — 60 Hz sine, exponential pitch+amp decay (drum-like).
const kickDur = 0.18;
const kickGain = 0.35;
function placeKick(startSec) {
  const start = Math.floor(startSec * SR);
  const len = Math.floor(kickDur * SR);
  for (let i = 0; i < len; i++) {
    const dst = start + i;
    if (dst < 0 || dst >= out.length) continue;
    const t = i / SR;
    // pitch sweep: 90Hz → 50Hz across the kick (helps it feel "round")
    const f = 90 - 40 * (t / kickDur);
    const env = Math.exp(-t / 0.06); // exponential amplitude decay
    out[dst] += Math.sin(2 * Math.PI * f * t) * env * kickGain;
  }
}

// Place a kick on every bar's beat 1.
const nBars = Math.floor(totalSec / barSec);
for (let b = 0; b < nBars; b++) placeKick(b * barSec);

// Soft clip / normalize to stay under -1.
let peak = 0;
for (let i = 0; i < out.length; i++) {
  const a = Math.abs(out[i]);
  if (a > peak) peak = a;
}
if (peak > 0.95) {
  const k = 0.95 / peak;
  for (let i = 0; i < out.length; i++) out[i] *= k;
}

// Write float32 raw → ffmpeg → mp3
const buf = Buffer.from(out.buffer);
writeFileSync(rawPath, buf);
const ff = "/opt/homebrew/opt/ffmpeg-full/bin/ffmpeg";
const result = spawnSync(ff, [
  "-hide_banner", "-loglevel", "error", "-y",
  "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
  "-c:a", "libmp3lame", "-q:a", "4",
  outPath,
], { stdio: "inherit" });
if (result.status !== 0) {
  console.error(`✗ ffmpeg encode failed`);
  process.exit(result.status || 1);
}
console.log(`✓ ${outPath} · ${nBars} kicks · ${bpm} bpm`);
