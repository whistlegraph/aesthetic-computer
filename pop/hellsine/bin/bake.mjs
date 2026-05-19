#!/usr/bin/env node
// bake.mjs — hellsine: engine → (opt scratch post-FX) → Spotify finalize.
//
// The engine (hellsine.mjs) is itself the single all-sine source mix —
// every voice is a sine. This driver:
//   1. renders the engine ONCE → pre-master float WAV + struct.json,
//   2. (optional, --scratch) runs pop/dance/bin/scratch-mix.mjs on that
//      one buffer, beat-locked to hellsine's own struct.json grid.
//      OFF by default: the chop&screw turntable gesture fights the
//      John-Williams reading on a first listen — opt in once the
//      composition is locked.
//   3. finalizes for Spotify: gentle glue → loudnorm I=-14 / TP=-1.5 →
//      limiter → end fade → hellsine-MASTER.wav + hellsine.mp3.
//
// Pure-sine mixes render dark; hardcore saturation adds upper harmonics
// so hellsine starts brighter than the trance bed, but A/B every master
// against a brightened take before release (see pop/RELEASES.md).
//
// Usage: node pop/hellsine/bin/bake.mjs [outDir] [--scratch]
//        [-- <extra engine flags e.g. --hell 14 --bpm 186>]
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
let args = process.argv.slice(2);
const SCRATCH_ON = args.includes("--scratch");
args = args.filter((a) => a !== "--scratch");
const sep = args.indexOf("--");
const engineExtra = sep >= 0 ? args.slice(sep + 1) : [];
const outDir = (sep >= 0 ? args.slice(0, sep) : args)[0] ||
  `${homedir()}/Documents/Working Desktop/hellsine`;

const ENGINE = `${REPO}/pop/hellsine/bin/hellsine.mjs`;
const SCRATCH = `${REPO}/pop/dance/bin/scratch-mix.mjs`;
const pre = `${outDir}/.hellsine-pre.wav`;
const struct = `${pre.replace(/\.wav$/, "")}.assets/struct.json`;
const scr = `${outDir}/.hellsine-scr.wav`;
const finalWav = `${outDir}/hellsine-MASTER.wav`;
const finalMp3 = `${outDir}/hellsine.mp3`;

const run = (cmd, a, label) => {
  console.log(`\n[bake] ${label}`);
  const r = spawnSync(cmd, a, { stdio: "inherit" });
  if (r.status !== 0) { console.error(`[bake] FAILED: ${label}`); process.exit(1); }
};

// 1 — the single all-sine engine render
run("node", [ENGINE, "--out", pre, "--struct", struct, ...engineExtra],
  "render engine (all-sine source mix)");

// 2 — optional in-mix post-FX, beat-locked to struct.json (no vocal
//     stamp: the concept track stays strictly instrumental + all-sine)
let mix = pre;
if (SCRATCH_ON) {
  run("node", [SCRATCH, pre, scr, "", struct],
    "post-FX (scratch + beat-locked breakbeat + FM growls)");
  mix = scr;
}

// 3 — finalize for Spotify (Spotify normalises to -14 on playback)
// Brightening polish — pure-sine mixes render very dark (lows ~31 dB
// over the air band on the first hellsine cut). Hardcore wants it
// bright + aggressive, so this is heavier than the trance note: a big
// high-shelf for air, presence + sparkle bells, and a mud trim.
run("ffmpeg", ["-y", "-i", mix, "-af",
  "highshelf=f=7500:g=8," +
  "equalizer=f=4200:t=q:w=1.1:g=4," +
  "equalizer=f=12000:t=q:w=1.4:g=4," +
  "equalizer=f=200:t=q:w=1.0:g=-2.5," +
  "acompressor=threshold=-18dB:ratio=2:attack=15:release=240:makeup=1:knee=8," +
  "loudnorm=I=-14:TP=-1.5:LRA=11," +
  "alimiter=limit=0.94:attack=6:release=110:level=disabled," +
  "afade=t=out:st=104.5:d=4",
  "-ar", "44100", "-sample_fmt", "s16", finalWav], "finalize → -14 LUFS (bright)");
run("ffmpeg", ["-y", "-i", finalWav, "-codec:a", "libmp3lame", "-b:a", "320k",
  finalMp3], "320k mp3");
spawnSync("rm", ["-f", scr]);
spawnSync("open", ["-a", "QuickTime Player", finalWav]);
console.log(`\n[bake] done → ${finalWav}`);
