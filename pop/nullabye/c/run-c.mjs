#!/usr/bin/env node
// run-c.mjs — render a baked cancelled-noise score through the C engine
// (nullnoise.c) and master to mp3. The composition lives in the lane
// renderers (render-nuellaby.mjs --bake, pop/teknull/bin/...); this is
// the shared DSP + finalize path.
//
// Usage:
//   node pop/nullabye/c/run-c.mjs <score.txt> --out <out.mp3> [--master lullaby|techno]
//   node pop/nullabye/c/run-c.mjs <score.txt> --raw <out.f32>   (engine output only)

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, unlinkSync, statSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ENGINE = resolve(HERE, "nullnoise");

const args = process.argv.slice(2);
const score = args.find((a) => !a.startsWith("--"));
const _argi = (k) => { const i = args.indexOf(k); return i >= 0 ? args[i + 1] : null; };
const outMp3 = _argi("--out");
const rawOnly = _argi("--raw");
const masterPreset = _argi("--master") || "lullaby";
if (!score || (!outMp3 && !rawOnly)) {
  console.error("usage: run-c.mjs <score.txt> --out <mp3> [--master lullaby|techno] | --raw <f32>");
  process.exit(1);
}

// build the engine if missing or stale
const cSrc = resolve(HERE, "nullnoise.c");
if (!existsSync(ENGINE) || statSync(cSrc).mtimeMs > statSync(ENGINE).mtimeMs) {
  console.log("[run-c] building nullnoise…");
  const b = spawnSync("bash", [resolve(HERE, "build.sh")], { stdio: "inherit" });
  if (b.status !== 0) process.exit(1);
}

const rawPath = rawOnly || `${outMp3}.f32.raw`;
mkdirSync(dirname(rawPath), { recursive: true });
const r = spawnSync(ENGINE, [score, "--raw", rawPath], { stdio: "inherit" });
if (r.status !== 0) { console.error("✗ nullnoise failed"); process.exit(1); }
if (rawOnly) process.exit(0);

// master chains — lullaby mirrors render-nuellaby.mjs; techno hits harder
const MASTERS = {
  lullaby: [
    "highpass=f=30",
    "acompressor=threshold=-22dB:ratio=2.2:attack=25:release=220:makeup=2.0:knee=8",
    "treble=g=0.8:f=9000",
    "alimiter=limit=0.96:attack=5:release=80",
  ],
  techno: [
    "highpass=f=24", // let the 30–40 Hz boom through; alimiter guards
    "acompressor=threshold=-18dB:ratio=3.2:attack=8:release=120:makeup=3.5:knee=4",
    "equalizer=f=45:t=q:w=1.2:g=4", // the deeeeep shelf under the kick
    "bass=g=3:f=60",
    "treble=g=1.6:f=8500",
    "alimiter=limit=0.97:attack=3:release=50",
  ],
  // teknull needs audible drum attack without re-brightening its noise source.
  teknull: [
    "highpass=f=26",
    "lowpass=f=14500",
    "acompressor=threshold=-17dB:ratio=2.8:attack=18:release=105:makeup=3:knee=5",
    "equalizer=f=52:t=q:w=1.1:g=3",
    "bass=g=2:f=72",
    "treble=g=-1.5:f=7200",
    "alimiter=limit=0.96:attack=4:release=65",
    "volume=0.82", // MP3-safe headroom; avoids decoded/intersample overs
  ],
};
const chain = MASTERS[masterPreset];
if (!chain) { console.error(`unknown master preset: ${masterPreset}`); process.exit(1); }

const ff = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", "48000", "-ac", "2", "-i", rawPath,
  "-af", chain.join(","), "-c:a", "libmp3lame", "-q:a", "2", outMp3], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outMp3} (C engine · ${masterPreset}-mastered)`);
