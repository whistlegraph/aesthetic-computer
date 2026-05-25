#!/usr/bin/env node
// bake.mjs — ONE render → ONE mix → final. (jas: "bake all of this into
// a single mix ... not two baked tracks mixed down then modified ...
// many tracks we can change the individual volumes of".)
//
// The engine (recap/bin/trance.mjs --mode chill) is already a single
// all-sine, multi-BUS render whose per-track volumes ARE its gain
// flags (--drum-gain --pad-gain --lead-gain --bass-gain --bells-gain
// --piano-gain). There is NO separate sine-only render and NO
// dual-layer ffmpeg combine anymore — this driver just:
//   1. renders the engine ONCE (the single source mix),
//   2. runs the in-mix post-FX (scratch + clean ID + sparing
//      beat-locked breakbeat w/ echo/flange-out + skrill growls +
//      stochastic slides) on that one buffer, beat-locked to the
//      engine's own struct.json grid,
//   3. finalizes for Spotify (gentle glue → -14 LUFS / -1.5 dBTP /
//      18 s fade).
//
// Usage: node pop/dance/bin/bake.mjs [outDir] [-- <extra engine flags>]
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { existsSync } from "node:fs";
import { acdspAvailable, processWav, presets } from "../../lib/master.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
let args = process.argv.slice(2);
const USE_ACDSP = args.includes("--acdsp");
args = args.filter((a) => a !== "--acdsp");
const sep = args.indexOf("--");
const engineExtra = sep >= 0 ? args.slice(sep + 1) : [];
const outDir = (sep >= 0 ? args.slice(0, sep) : args)[0] ||
  `${homedir()}/Documents/Working Desktop/twi-out`;

const ENGINE = `${REPO}/recap/bin/trance.mjs`;
const SCRATCH = `${REPO}/pop/dance/bin/scratch-mix.mjs`;
const STAMP = `${REPO}/pop/dance/out/.ac-dot-stamp-vocal.mp3`;
const pre = `${outDir}/trancenwaltzi-MASTER-preBright.wav`;
const scr = `${outDir}/.trancenwaltzi-scr.wav`;
const finalWav = `${outDir}/trancenwaltzi-MASTER.wav`;
const finalMp3 = `${outDir}/trancenwaltzi.mp3`;
const struct = `${pre}.assets/struct.json`;

const run = (cmd, a, label) => {
  console.log(`\n[bake] ${label}`);
  const r = spawnSync(cmd, a, { stdio: "inherit" });
  if (r.status !== 0) { console.error(`[bake] FAILED: ${label}`); process.exit(1); }
};

// 1 — the single engine render (one mix, all buses)
run("node", [ENGINE, "--mode", "chill", "--meter", "3", "--master",
  "--out", pre, ...engineExtra], "render engine (single all-sine mix)");

// 2 — in-mix post-FX, beat-locked to the engine's own struct.json
run("node", [SCRATCH, pre, scr, existsSync(STAMP) ? STAMP : "", struct],
  "post-FX (scratch + ID + breakbeat + growls + slides)");

// 3 — finalize for Spotify. The end fade is DURATION-AWARE so any
// length / time-signature bakes correctly (an 18 s fade ending exactly
// at the track end — not a hardcoded 151 s that would silence a
// longer cut early).
const durProbe = spawnSync("ffprobe", ["-v", "error", "-show_entries",
  "format=duration", "-of", "csv=p=0", scr], { encoding: "utf8" });
const totalSec = parseFloat((durProbe.stdout || "").trim()) || 169;
const fadeD = Math.min(18, Math.max(4, totalSec * 0.1));
const fadeSt = Math.max(0, totalSec - fadeD).toFixed(3);

let finalizeIn = scr;
if (USE_ACDSP) {
  if (!acdspAvailable()) {
    console.error("[bake] --acdsp requested but pop/dsp/c/acdsp not built.");
    console.error("       build it: (cd pop/dsp/c && make)"); process.exit(1);
  }
  const acOut = `${outDir}/.trancenwaltzi-acdsp.wav`;
  const spec = presets.danceMasterBus({ in_db: -3, out_db: +3, iron: 0.5 });
  console.log(`\n[bake] acdsp character pass (1176 + EQ via C lib)\n        chain: ${spec}`);
  const r = processWav(scr, acOut, spec, { float: true });
  if (!r.ok) {
    console.error("[bake] acdsp failed:\n" + r.stderr); process.exit(1);
  }
  process.stderr.write(r.stderr);
  finalizeIn = acOut;
}

const ffChain = USE_ACDSP
  // acdsp already handled compressor + EQ — just loudness + limit + fade.
  ? "loudnorm=I=-14:TP=-1.5:LRA=11," +
    "alimiter=limit=0.94:attack=8:release=120:level=disabled," +
    `afade=t=out:st=${fadeSt}:d=${fadeD.toFixed(3)}`
  // legacy chain (kept for A/B): clean acompressor → loudnorm → limit.
  : "acompressor=threshold=-20dB:ratio=2:attack=20:release=260:makeup=1:knee=8," +
    "loudnorm=I=-14:TP=-1.5:LRA=11," +
    "alimiter=limit=0.94:attack=8:release=120:level=disabled," +
    `afade=t=out:st=${fadeSt}:d=${fadeD.toFixed(3)}`;

run("ffmpeg", ["-y", "-i", finalizeIn, "-af", ffChain,
  "-ar", "44100", "-sample_fmt", "s16", finalWav],
  `finalize → -14 LUFS (fade ${fadeSt}s +${fadeD.toFixed(1)}s)${USE_ACDSP ? " [acdsp]" : ""}`);
run("ffmpeg", ["-y", "-i", finalWav, "-codec:a", "libmp3lame", "-b:a", "320k",
  finalMp3], "320k mp3");
spawnSync("rm", ["-f", scr]);
if (USE_ACDSP) spawnSync("rm", ["-f", `${outDir}/.trancenwaltzi-acdsp.wav`]);
spawnSync("open", ["-a", "QuickTime Player", finalWav]);
console.log(`\n[bake] done → ${finalWav}${USE_ACDSP ? " (acdsp character)" : ""}`);
