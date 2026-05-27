#!/usr/bin/env node
// sleephellsine/bin/bake.mjs — render the engine, finalize to a quiet
// sleep master. No scratch lane, no acdsp character pass, no tempo
// ramp, no climax envelope. Just engine → loudnorm to -18 LUFS → soft
// limit → long fade-out → wav + 320 mp3.
//
// Usage: node pop/sleephellsine/bin/bake.mjs [outDir]
//        [-- <extra engine flags e.g. --bpm 40 --heartbeat off>]

import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
let args = process.argv.slice(2);
const sep = args.indexOf("--");
const engineExtra = sep >= 0 ? args.slice(sep + 1) : [];
const outDir = (sep >= 0 ? args.slice(0, sep) : args)[0] ||
  `${homedir()}/Documents/Working Desktop/sleephellsine`;

const ENGINE = `${REPO}/pop/sleephellsine/bin/sleephellsine.mjs`;
const pre = `${outDir}/.sleephellsine-pre.wav`;
const finalWav = `${outDir}/sleephellsine-MASTER.wav`;
const finalMp3 = `${outDir}/sleephellsine.mp3`;

const run = (cmd, a, label) => {
  console.log(`\n[bake] ${label}`);
  const r = spawnSync(cmd, a, { stdio: "inherit" });
  if (r.status !== 0) { console.error(`[bake] FAILED: ${label}`); process.exit(1); }
};

// 1 — render the engine
run("node", [ENGINE, "--out", pre, ...engineExtra],
  "render sleephellsine engine (pure sine sleep wash)");

// 2 — finalize: gentle loudnorm to -18 LUFS, soft limit, long fade-out.
// Very dynamic, no aggressive limiting. Target 15:00 final length (900 s).
// We let the engine render ~960 s and truncate with the fade ending at 900.
const FADE_DUR = 14.0;                // long fade tail
const TOTAL_DUR = 900.0;              // final length 15:00
const FADE_START = TOTAL_DUR - FADE_DUR;

const ffChain = [
  // very subtle low-shelf trim — kill any DC / sub-30 Hz rumble
  "highpass=f=28",
  // gentle wide air lift (sine mixes can sound a bit dull)
  "highshelf=f=8000:g=1",
  // loudnorm to quiet sleep target
  "loudnorm=I=-18:TP=-3:LRA=14",
  // soft limit to keep TP under -3 dB without crushing dynamics
  "alimiter=limit=0.75:attack=12:release=240:level=disabled",
  // long fade-out
  `afade=t=out:st=${FADE_START}:d=${FADE_DUR}`,
].join(",");

run("ffmpeg", ["-y", "-i", pre, "-af", ffChain,
  "-ar", "44100", "-sample_fmt", "s16",
  "-to", `${TOTAL_DUR}`,
  finalWav],
  "finalize → -18 LUFS sleep master (long fade-out)");

run("ffmpeg", ["-y", "-i", finalWav, "-codec:a", "libmp3lame", "-b:a", "320k",
  finalMp3], "320k mp3");

console.log(`\n[bake] done → ${finalWav}`);
console.log(`[bake]      → ${finalMp3}`);
