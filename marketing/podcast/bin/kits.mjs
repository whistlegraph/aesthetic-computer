#!/usr/bin/env node
// kits.mjs — audition the drum kits. Renders the lo-fi bed with each kit
// (same pad+melody, different drums) back to back into one file so you can
// hear them and pick. Order is printed; pass the winner to produce with --kit.
//
// Usage: node bin/kits.mjs [--secs 9] [--play]

import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { mkdirSync, rmSync, writeFileSync } from "node:fs";
import { execFileSync, spawnSync } from "node:child_process";
import { renderBed, KITS } from "./jingle.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const argv = process.argv.slice(2);
const flag = (k, d) => { const i = argv.indexOf(`--${k}`); return i >= 0 ? argv[i + 1] : d; };
const SECS = Number(flag("secs", 9));

const outDir = resolve(ROOT, "out");
mkdirSync(outDir, { recursive: true });
const build = resolve(outDir, ".kit-audition");
rmSync(build, { recursive: true, force: true });
mkdirSync(build, { recursive: true });

console.log(`\nAuditioning ${KITS.length} kits (${SECS}s each):\n`);
const segs = [];
KITS.forEach((kit, i) => {
  const wav = resolve(build, `${String(i).padStart(2, "0")}-${kit}.wav`);
  renderBed(SECS, wav, { kit });
  // normalize each to a common format
  const norm = resolve(build, `n${String(i).padStart(2, "0")}.wav`);
  execFileSync("ffmpeg", ["-y", "-i", wav, "-ar", "44100", "-ac", "2", "-af", "loudnorm=I=-18:TP=-1.5", "-c:a", "pcm_s16le", norm], { stdio: "ignore" });
  segs.push(norm);
  const sil = resolve(build, `s${String(i).padStart(2, "0")}.wav`);
  execFileSync("ffmpeg", ["-y", "-f", "lavfi", "-i", "anullsrc=r=44100:cl=stereo", "-t", "0.8", "-c:a", "pcm_s16le", sil], { stdio: "ignore" });
  segs.push(sil);
  console.log(`  ${i + 1}. ${kit}`);
});

const list = resolve(build, "list.txt");
writeFileSync(list, segs.map((f) => `file '${f}'`).join("\n") + "\n");
const outMp3 = resolve(outDir, "kit-audition.mp3");
execFileSync("ffmpeg", ["-y", "-f", "concat", "-safe", "0", "-i", list, "-c:a", "libmp3lame", "-b:a", "220k", outMp3], { stdio: "ignore" });
rmSync(build, { recursive: true, force: true });

console.log(`\n✓ ${outMp3}`);
if (argv.includes("--play")) {
  spawnSync("open", ["-a", "QuickTime Player", outMp3]);
  spawnSync("osascript", ["-e", 'tell application "QuickTime Player" to play the front document'], { stdio: "ignore" });
}
