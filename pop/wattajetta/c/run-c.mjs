#!/usr/bin/env node
// run-c.mjs — render a baked wattajetta score through the C engine and
// master to mp3. The composition lives in bin/render-wattajetta.mjs;
// this is the shared DSP + finalize path.
//
// Usage:
//   node pop/wattajetta/c/run-c.mjs <score.txt> --out <out.mp3>
//   node pop/wattajetta/c/run-c.mjs <score.txt> --raw <out.f32>

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, unlinkSync, statSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ENGINE = resolve(HERE, "wattajetta");

const args = process.argv.slice(2);
const score = args.find((a) => !a.startsWith("--"));
const _argi = (k) => { const i = args.indexOf(k); return i >= 0 ? args[i + 1] : null; };
const outMp3 = _argi("--out");
const rawOnly = _argi("--raw");
if (!score || (!outMp3 && !rawOnly)) {
  console.error("usage: run-c.mjs <score.txt> --out <mp3> | --raw <f32>");
  process.exit(1);
}

// build the engine if missing or stale
const cSrc = resolve(HERE, "wattajetta.c");
if (!existsSync(ENGINE) || statSync(cSrc).mtimeMs > statSync(ENGINE).mtimeMs) {
  console.log("[run-c] building wattajetta…");
  const b = spawnSync("bash", [resolve(HERE, "build.sh")], { stdio: "inherit" });
  if (b.status !== 0) process.exit(1);
}

const kickRaw = _argi("--kickraw");
const rawPath = rawOnly || `${outMp3}.f32.raw`;
mkdirSync(dirname(rawPath), { recursive: true });
const engineArgs = [score, "--raw", rawPath];
if (kickRaw) engineArgs.push("--kickraw", kickRaw);
const r = spawnSync(ENGINE, engineArgs, { stdio: "inherit" });
if (r.status !== 0) { console.error("✗ wattajetta failed"); process.exit(1); }
if (rawOnly) process.exit(0);

// water master — deep sub intact, spray sparkle up top, firm ceiling
const MASTER = [
  "highpass=f=24",
  "acompressor=threshold=-19dB:ratio=3:attack=10:release=140:makeup=3:knee=5",
  "equalizer=f=50:t=q:w=1.2:g=3.5", // the boom under the kick
  "treble=g=1.8:f=8000",            // caustic glint
  "alimiter=limit=0.97:attack=3:release=60",
];

const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", "48000", "-ac", "2", "-i", rawPath,
  "-af", MASTER.join(","), "-c:a", "libmp3lame", "-q:a", "2",
  "-metadata", "title=wattajetta", "-metadata", "album=pixsies",
  outMp3], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outMp3} (C engine · water-mastered)`);
