#!/usr/bin/env node
// run-c.mjs — render a wobble bassline through the C engine (wobble.c)
// and master to mp3. Shared DSP + finalize path for the C wobble; the
// composition lives in the JS lane (emitWobbleScore from a note list).
//
// Usage:
//   node pop/dance/c/run-c.mjs --demo bomp --out ~/Desktop/wob-c.mp3
//   node pop/dance/c/run-c.mjs --score score.txt --raw out.f32   (engine only)

import { spawnSync } from "node:child_process";
import { existsSync, statSync, mkdirSync, writeFileSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

import { emitWobbleScore } from "../synths/wobble.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ENGINE = resolve(HERE, "wobble");
const SR = 48_000;
const BPM = 140;

const args = process.argv.slice(2);
const argi = (k) => { const i = args.indexOf(k); return i >= 0 ? args[i + 1] : null; };
const demo = argi("--demo");
let scorePath = argi("--score");
const outMp3 = argi("--out");
const rawOnly = argi("--raw");
const expand = (p) => (!p ? p : p.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p);

if (!demo && !scorePath) { console.error("usage: run-c.mjs --demo <preset>|--score <txt> --out <mp3>|--raw <f32>"); process.exit(1); }
if (!outMp3 && !rawOnly) { console.error("need --out <mp3> or --raw <f32>"); process.exit(1); }

// build engine if missing or stale
const cSrc = resolve(HERE, "wobble.c");
if (!existsSync(ENGINE) || statSync(cSrc).mtimeMs > statSync(ENGINE).mtimeMs) {
  console.log("[run-c] building wobble…");
  const b = spawnSync("bash", [resolve(HERE, "build.sh")], { stdio: "inherit" });
  if (b.status !== 0) process.exit(1);
}

// build a demo score from a preset if --demo was given
if (demo) {
  const beat = 60 / BPM;
  const roots = { woe: [38, 36, 34, 33], bomp: [40, 40, 43, 38], row: [33, 33, 41, 43], reese: [38, 38, 41, 36] };
  const r = roots[demo] || roots.woe;
  const events = [];
  for (let bar = 0; bar < 4; bar++)
    for (let half = 0; half < 2; half++)
      events.push({ startSec: (bar * 4 + half * 2) * beat, midi: r[bar], gain: 0.95, durSec: 2 * beat * 0.98, preset: demo });
  scorePath = resolve(HERE, `.demo-${demo}.score.txt`);
  writeFileSync(scorePath, emitWobbleScore(events, { sampleRate: SR, bpm: BPM }));
}

const rawPath = expand(rawOnly) || `${expand(outMp3)}.f32.raw`;
mkdirSync(dirname(rawPath), { recursive: true });
const r = spawnSync(ENGINE, [scorePath, "--raw", rawPath], { stdio: "inherit" });
if (demo) { try { unlinkSync(scorePath); } catch {} }
if (r.status !== 0) { console.error("✗ wobble engine failed"); process.exit(1); }
if (rawOnly) process.exit(0);

// f32 mono → loudnorm + limit → 320k mp3 (same finalize as the JS bake)
const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
  "-af", "loudnorm=I=-14:TP=-1.5:LRA=11,alimiter=limit=0.94:attack=8:release=120:level=disabled",
  "-c:a", "libmp3lame", "-b:a", "320k", expand(outMp3)], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${expand(outMp3)} (C engine)`);
