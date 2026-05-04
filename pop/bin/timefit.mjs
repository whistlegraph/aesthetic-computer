#!/usr/bin/env node
// timefit.mjs — uniformly time-compress (or stretch) a vocal stem to
// fit a target bar count at a target BPM. The first pass at the
// "realign" stage of the post-prod pipeline — global tempo only, no
// per-word work yet.
//
// Implements ffmpeg `atempo` chaining (atempo accepts 0.5–2.0 per
// instance, so factors outside that range cascade two passes).
// Optionally wraps the source through `--keep-pitch` (formant-
// preserving rubberband) when available; default is atempo (slight
// pitch shift, noticeable above ~1.3x).
//
// Usage:
//   node bin/timefit.mjs <stem.mp3> --bars 4 --bpm 140
//   node bin/timefit.mjs <stem.mp3> --duration 6.86
//   node bin/timefit.mjs <stem.mp3> --bars 4 --bpm 140 --keep-pitch
//   node bin/timefit.mjs <stem.mp3> --bars 4 --bpm 140 --out path.mp3

import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { resolve, dirname, basename, extname } from "node:path";

function parseArgs(argv) {
  const flags = {};
  const positional = [];
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a.startsWith("--")) {
      const k = a.slice(2);
      const next = argv[i + 1];
      if (next !== undefined && !next.startsWith("--")) { flags[k] = next; i++; }
      else flags[k] = true;
    } else positional.push(a);
  }
  return { flags, positional };
}

const { flags, positional } = parseArgs(process.argv.slice(2));

const stemPath = resolve(process.cwd(), positional[0] || "");
if (!stemPath || !existsSync(stemPath)) {
  console.error("usage: node bin/timefit.mjs <stem.mp3> --bars N --bpm BPM [--keep-pitch] [--out path.mp3]");
  process.exit(1);
}

const stemDir = dirname(stemPath);
const stemBase = basename(stemPath, extname(stemPath));

// ── Source duration ───────────────────────────────────────────────────
const probe = spawnSync(
  "ffprobe",
  ["-v", "error", "-show_entries", "format=duration",
   "-of", "default=noprint_wrappers=1:nokey=1", stemPath],
  { encoding: "utf8" }
);
const sourceDur = Number(probe.stdout.trim());
if (!(sourceDur > 0)) {
  console.error(`✗ ffprobe could not read duration of ${stemPath}`);
  process.exit(1);
}

// ── Target duration ───────────────────────────────────────────────────
let targetDur = null;
if (flags.duration !== undefined) targetDur = Number(flags.duration);
if (flags.bars !== undefined && flags.bpm !== undefined) {
  const bars = Number(flags.bars);
  const bpm = Number(flags.bpm);
  const beatsPerBar = Number(flags["beats-per-bar"] || 4);
  targetDur = bars * (60 / bpm) * beatsPerBar;
}
if (!(targetDur > 0)) {
  console.error("✗ specify --duration <sec> OR (--bars N --bpm BPM)");
  process.exit(1);
}

const factor = sourceDur / targetDur; // > 1 means speed up (compress)
console.log(`→ source ${sourceDur.toFixed(3)}s · target ${targetDur.toFixed(3)}s · factor ${factor.toFixed(3)}×`);

// ── Build ffmpeg filter chain ─────────────────────────────────────────
// atempo accepts 0.5..2.0 per instance. Cascade for factors outside.
function atempoChain(f) {
  const stages = [];
  let remaining = f;
  while (remaining > 2.0) { stages.push(2.0); remaining /= 2.0; }
  while (remaining < 0.5) { stages.push(0.5); remaining /= 0.5; }
  stages.push(remaining);
  return stages.map((s) => `atempo=${s.toFixed(6)}`).join(",");
}

const KEEP_PITCH = flags["keep-pitch"] === true;
const OUT_PATH = flags.out
  ? resolve(process.cwd(), flags.out)
  : `${stemDir}/${stemBase}-fit${factor >= 1 ? "ter" : "longer"}.mp3`;

let filter;
if (KEEP_PITCH) {
  // rubberband filter preserves pitch. Built-in if ffmpeg is built with it.
  filter = `rubberband=tempo=${factor.toFixed(6)}`;
} else {
  filter = atempoChain(factor);
}

console.log(`→ filter · ${filter}`);
console.log(`→ writing · ${OUT_PATH}`);

const ff = spawnSync(
  "ffmpeg",
  ["-hide_banner", "-y", "-loglevel", "error",
   "-i", stemPath,
   "-filter:a", filter,
   "-c:a", "libmp3lame", "-q:a", "3",
   OUT_PATH],
  { stdio: "inherit" }
);
if (ff.status !== 0) {
  if (KEEP_PITCH) {
    console.error("✗ ffmpeg failed — your ffmpeg may not have rubberband. Retry without --keep-pitch.");
  } else {
    console.error("✗ ffmpeg failed");
  }
  process.exit(1);
}

// Verify output duration
const probeOut = spawnSync(
  "ffprobe",
  ["-v", "error", "-show_entries", "format=duration",
   "-of", "default=noprint_wrappers=1:nokey=1", OUT_PATH],
  { encoding: "utf8" }
);
const outDur = Number(probeOut.stdout.trim());
const drift = outDur - targetDur;
console.log(`✓ ${OUT_PATH} · ${outDur.toFixed(3)}s (drift ${drift >= 0 ? "+" : ""}${drift.toFixed(3)}s vs target)`);
