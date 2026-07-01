#!/usr/bin/env node
// run-c.mjs — render a baked fluttabap360 score through the C engine
// (fluttabap360.c) and master to mp3 (+ DistroKid wav). Mirrors
// pop/nullabye/c/run-c.mjs. The composition lives in
// ../bin/render-fluttabap360.mjs --bake; this is the shared C-DSP + finalize.
//
// Usage:
//   node pop/marimba/c/run-c.mjs <score.txt> --out <out.mp3> [--wav]
//   node pop/marimba/c/run-c.mjs <score.txt> --raw <out.f32>   (engine only)

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, statSync, unlinkSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { masterChain } from "../../lib/substrate.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ENGINE = resolve(HERE, "fluttabap360");
const SR = 48_000;

const args = process.argv.slice(2);
const score = args.find((a) => !a.startsWith("--"));
const _argi = (k) => { const i = args.indexOf(k); return i >= 0 ? args[i + 1] : null; };
const outMp3 = _argi("--out");
const rawOnly = _argi("--raw");
const wantWav = args.includes("--wav");
if (!score || (!outMp3 && !rawOnly)) {
  console.error("usage: run-c.mjs <score.txt> --out <mp3> [--wav] | --raw <f32>");
  process.exit(1);
}

// build the engine if missing or stale
const cSrc = resolve(HERE, "fluttabap360.c");
if (!existsSync(ENGINE) || statSync(cSrc).mtimeMs > statSync(ENGINE).mtimeMs) {
  console.log("[run-c] building fluttabap360…");
  const b = spawnSync("bash", [resolve(HERE, "build.sh")], { stdio: "inherit" });
  if (b.status !== 0) process.exit(1);
}

const rawPath = rawOnly || `${outMp3}.f32.raw`;
mkdirSync(dirname(rawPath), { recursive: true });
const soloArg = _argi("--solo");   // forward voice-solo restriction to the engine (parity harness)
const loopMode = args.includes("--loop");   // seamless-loop fold (tail → head, no fades)
const engineArgs = [score, "--raw", rawPath];
if (soloArg) engineArgs.push("--solo", soloArg);
if (loopMode) engineArgs.push("--loop");
const r = spawnSync(ENGINE, engineArgs, { stdio: "inherit" });
if (r.status !== 0) { console.error("✗ fluttabap360 engine failed"); process.exit(1); }
if (rawOnly) process.exit(0);

// MASTER chain comes from the SUBSTRATE the score was printed on. The baker
// writes a `substrate <name>` line into the score header; we read it and build
// the matching master (EQ tilt + wow/flutter + tube-glue compression + ceiling)
// from pop/lib/substrate.mjs — the single source for both engines.
let substrate = "tape";
try {
  const m = readFileSync(score, "utf8").match(/^substrate\s+(\w+)/m);
  if (m) substrate = m[1];
} catch {}
const MASTER = masterChain(substrate);

const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
  "-af", MASTER, "-c:a", "libmp3lame", "-q:a", "2", outMp3], { stdio: "inherit" });
if (ff.status !== 0) { try { unlinkSync(rawPath); } catch {} console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outMp3} (C engine · bedroom-pop master)`);

if (wantWav) {
  const wavOut = outMp3.replace(/\.mp3$/i, "") + ".distrokid.wav";
  const ffw = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawPath,
    "-af", `${MASTER},loudnorm=I=-14:TP=-1.5:LRA=11`,
    "-ar", "44100", "-ac", "2", "-c:a", "pcm_s16le", wavOut], { stdio: "inherit" });
  if (ffw.status === 0) console.log(`✓ ${wavOut} (DistroKid master)`);
}
try { unlinkSync(rawPath); } catch {}
