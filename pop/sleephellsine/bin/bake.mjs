#!/usr/bin/env node
// sleephellsine/bin/bake.mjs — build (if needed) + render the C engine,
// then finalize to a quiet sleep master. No scratch lane, no acdsp pass,
// no tempo ramp. Engine → loudnorm to -28 LUFS → soft limit → 60s fade-in
// + 14s fade-out → wav + 320 mp3, truncated to 15:00.
//
// Usage: node pop/sleephellsine/bin/bake.mjs [outDir]
//        [-- <extra engine flags e.g. --seed nightfall>]

import { spawnSync } from "node:child_process";
import { existsSync, statSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
let args = process.argv.slice(2);
const sep = args.indexOf("--");
const engineExtra = sep >= 0 ? args.slice(sep + 1) : [];
const outDir = (sep >= 0 ? args.slice(0, sep) : args)[0] ||
  `${homedir()}/Documents/Shelf/sleephellsine`;

const ENGINE_SRC = `${LANE}/c/sleephellsine.c`;
const ENGINE_BIN = `${LANE}/c/sleephellsine`;
const BUILD_SH   = `${LANE}/c/build.sh`;

const pre = `${outDir}/.sleephellsine-pre.wav`;
const finalWav = `${outDir}/sleephellsine-MASTER.wav`;
const finalMp3 = `${outDir}/sleephellsine.mp3`;

const run = (cmd, a, label) => {
  console.log(`\n[bake] ${label}`);
  const r = spawnSync(cmd, a, { stdio: "inherit" });
  if (r.status !== 0) { console.error(`[bake] FAILED: ${label}`); process.exit(1); }
};

// 0 — build the C engine if missing or older than source
const needBuild =
  !existsSync(ENGINE_BIN) ||
  statSync(ENGINE_SRC).mtimeMs > statSync(ENGINE_BIN).mtimeMs;
if (needBuild) run("bash", [BUILD_SH], "build C engine");

// 1 — render the engine to a 32-bit float pre-master
run(ENGINE_BIN, ["--out", pre, ...engineExtra],
  "C engine render (circle of fifths · two uppers crossing · root bells)");

// 2 — finalize: gentle loudnorm to -28 LUFS sleep target, soft limit,
// long fade-in + fade-out, truncate to 15:00.
const FADE_DUR = 14.0;
const TOTAL_DUR = 900.0;
const FADE_START = TOTAL_DUR - FADE_DUR;
const FADE_IN_DUR = 60.0;

const ffChain = [
  "highpass=f=28",
  // soft de-essing shelf — pure sines + bells can spike around 4-6 kHz
  // when popcorn clusters pile up in the reverb. -1.5 dB above 4 kHz
  // tames that without darkening the body of the carriers (F4 ~349 Hz).
  "highshelf=f=4000:g=-1.5",
  "loudnorm=I=-28:TP=-6:LRA=14",
  "alimiter=limit=0.35:attack=12:release=240:level=disabled",
  `afade=t=in:st=0:d=${FADE_IN_DUR}`,
  `afade=t=out:st=${FADE_START}:d=${FADE_DUR}`,
].join(",");

run("ffmpeg", ["-y", "-i", pre, "-af", ffChain,
  "-ar", "44100", "-sample_fmt", "s16",
  "-to", `${TOTAL_DUR}`,
  finalWav],
  "finalize → -28 LUFS sleep master (60s fade-in + 14s fade-out)");

run("ffmpeg", ["-y", "-i", finalWav, "-codec:a", "libmp3lame", "-b:a", "320k",
  finalMp3], "320k mp3");

console.log(`\n[bake] done → ${finalWav}`);
console.log(`[bake]      → ${finalMp3}`);
