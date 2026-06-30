#!/usr/bin/env node
// run-c.mjs — pop-facing entry point for the physically-modeled bell.
//
// Builds the C engine if stale, renders a strike (geometry + material -> WAV),
// masters it to mp3, and can also emit the modes JSON + the 3-D visualization.
//
// Usage:
//   node pop/bell/c/run-c.mjs --note A4 --material bronze --geometry church \
//        --out /tmp/bell.mp3 [--master bell|bright|raw] [--dur 8] [--vel 0.9]
//   ... --viz /tmp/bell.mp4   also render the 3-D mode visualization
//   ... --print-modes         print the solved partial table
//
// Materials:  bronze brass steel aluminum silver glass gold
// Geometries: church handbell tubular bowl glass

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, statSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ENGINE = resolve(HERE, "bell");
const VIZ = resolve(HERE, "..", "bin", "viz.mjs");

const args = process.argv.slice(2);
const val = (k, d = null) => {
  const i = args.indexOf(k);
  return i >= 0 && i + 1 < args.length ? args[i + 1] : d;
};
const note = val("--note", "A4");
const material = val("--material", "bronze");
const geometry = val("--geometry", "church");
const dur = val("--dur", "8");
const vel = val("--vel", "0.9");
const outMp3 = val("--out", "/tmp/bell.mp3");
const master = val("--master", "bell");
const vizOut = val("--viz", null);
const printModes = args.includes("--print-modes");

// Build engine if missing or stale.
const cSrc = resolve(HERE, "bell.c");
if (!existsSync(ENGINE) || statSync(cSrc).mtimeMs > statSync(ENGINE).mtimeMs) {
  console.log("[run-c] building bell…");
  const b = spawnSync("bash", [resolve(HERE, "build.sh")], { stdio: "inherit" });
  if (b.status !== 0) process.exit(1);
}

mkdirSync(dirname(resolve(outMp3)), { recursive: true });
const wav = `${outMp3}.tmp.wav`;
const modesJson = vizOut ? `${outMp3}.modes.json` : null;

const engArgs = [
  "--note", note, "--material", material, "--geometry", geometry,
  "--dur", dur, "--vel", vel, "--out", wav,
];
if (modesJson) engArgs.push("--modes", modesJson);
if (printModes) engArgs.push("--print-modes");
const r = spawnSync(ENGINE, engArgs, { stdio: "inherit" });
if (r.status !== 0) { console.error("✗ bell engine failed"); process.exit(1); }

// Master chains (the WAV is already peak-normalized; these add polish).
const MASTERS = {
  // Keep the long shimmering tail, gentle glue, a touch of air.
  bell: [
    "highpass=f=40",
    "acompressor=threshold=-20dB:ratio=2:attack=20:release=300:makeup=1.5:knee=8",
    "treble=g=1.2:f=9000",
    "alimiter=limit=0.95:attack=4:release=120",
  ],
  // More presence for clangier/percussive uses.
  bright: [
    "highpass=f=60",
    "equalizer=f=3500:t=q:w=1.2:g=2",
    "acompressor=threshold=-18dB:ratio=2.6:attack=8:release=180:makeup=2.5:knee=6",
    "treble=g=2:f=8500",
    "alimiter=limit=0.97:attack=3:release=80",
  ],
  raw: [],
};
const chain = MASTERS[master];
if (!chain) { console.error(`unknown master: ${master}`); process.exit(1); }

const ffArgs = ["-hide_banner", "-y", "-loglevel", "error", "-i", wav];
if (chain.length) ffArgs.push("-af", chain.join(","));
ffArgs.push("-c:a", "libmp3lame", "-q:a", "2", resolve(outMp3));
const ff = spawnSync("ffmpeg", ffArgs, { stdio: "inherit" });
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outMp3} (bell · ${geometry}/${material} · ${master}-mastered)`);

// Optional 3-D visualization (uses the rendered WAV for audio + reactive flash).
if (vizOut) {
  const v = spawnSync("node", [
    VIZ, "--modes", modesJson, "--audio", wav, "--out", resolve(vizOut),
    "--dur", dur,
  ], { stdio: "inherit" });
  if (v.status !== 0) { console.error("✗ viz failed"); process.exit(1); }
  console.log(`✓ ${vizOut} (3-D mode visualization)`);
}

try { unlinkSync(wav); } catch {}
