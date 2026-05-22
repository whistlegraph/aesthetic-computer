#!/usr/bin/env node
// bake.mjs — hellsine: engine → (opt scratch post-FX) → Spotify finalize.
//
// The engine (hellsine.mjs) is the source mix — all-sine, plus the one
// sampled exception (@jeffrey's rattle, if recorded). This driver:
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
// The new mix (halftime hole-kick + 8-partial pads + sparkle + steam)
// is inherently brighter than the original dark gabber-punch cut, so
// the brightening pass is now a LIGHT polish. The old +8 dB shelf +
// dual presence/sparkle boosts piled into the 2-12 kHz range and read
// as buzz hash. Keep just a gentle air lift + warm the low end.
run("ffmpeg", ["-y", "-i", mix, "-af",
  "highshelf=f=9000:g=2," +
  "equalizer=f=150:t=q:w=1.0:g=2," +              // warmth back (kick is subby now)
  "acompressor=threshold=-18dB:ratio=2:attack=15:release=240:makeup=1:knee=8," +
  "loudnorm=I=-14:TP=-1.5:LRA=11," +
  "alimiter=limit=0.94:attack=6:release=110:level=disabled," +
  "afade=t=out:st=162.5:d=4",
  "-ar", "44100", "-sample_fmt", "s16", finalWav], "finalize → -14 LUFS (light polish)");
run("ffmpeg", ["-y", "-i", finalWav, "-codec:a", "libmp3lame", "-b:a", "320k",
  finalMp3], "320k mp3");
spawnSync("rm", ["-f", scr]);
spawnSync("open", ["-a", "QuickTime Player", finalWav]);
console.log(`\n[bake] done → ${finalWav}`);
