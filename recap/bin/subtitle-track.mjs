#!/usr/bin/env node
// subtitle-track.mjs — emit a proper subtitle-track video (constant 30 fps,
// transparent alpha) so the main compose can include subtitles via a single
// `-i subtitle-track.webm` input + one overlay filter — instead of the old
// 135-deep `movie=...` chain.
//
// Two-step process:
//   1. Build a concat-demuxer text file (`out/subtitle-track.txt`) listing
//      subtitle PNGs and blank-gap PNGs with their durations.
//   2. Run ffmpeg to convert that timeline into a 30 fps alpha WebM
//      (`out/subtitle-track.webm`). Without this pre-encode, the concat
//      demuxer feeds the main compose as a sparse PTS stream (one frame
//      per `file` entry), which ffmpeg's `overlay` interprets weirdly and
//      truncates the output. The pre-encoded webm presents continuous
//      30 fps frames just like the slides input.
//
// Output webm is small (a few hundred KB at most — mostly transparent
// frames + tiny pill images). Encode is fast (1-3 seconds).
//
// Usage: node bin/subtitle-track.mjs

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const subsPath = `${ROOT}/out/subs.json`;
const blankPath = `${ROOT}/out/subs/blank.png`;
const listPath = `${ROOT}/out/subtitle-track.txt`;
const webmPath = `${ROOT}/out/subtitle-track.webm`;
const hashFile = `${webmPath}.hash`;
const durPath = `${ROOT}/out/duration.txt`;
const force = process.argv.includes("--force");

const subs = JSON.parse(readFileSync(subsPath, "utf8"));
if (!subs.length) {
  console.error(`✗ no subtitle chunks in ${subsPath}`);
  process.exit(1);
}
if (!existsSync(blankPath)) {
  console.error(`✗ missing ${blankPath} — re-run bin/subtitles.mjs`);
  process.exit(1);
}

let total;
try { total = parseFloat(readFileSync(durPath, "utf8")); }
catch { total = subs[subs.length - 1].endSec; }

// ── 1. write the concat-demuxer timeline ───────────────────────────────
const lines = [];
let cursor = 0;
for (const s of subs) {
  if (s.startSec > cursor + 0.001) {
    lines.push(`file '${blankPath}'`);
    lines.push(`duration ${(s.startSec - cursor).toFixed(3)}`);
  }
  lines.push(`file '${s.file}'`);
  lines.push(`duration ${(s.endSec - s.startSec).toFixed(3)}`);
  cursor = s.endSec;
}
if (total > cursor + 0.001) {
  lines.push(`file '${blankPath}'`);
  lines.push(`duration ${(total - cursor).toFixed(3)}`);
}
// Concat demuxer requires the last `file` line repeated for its duration
// to apply (https://trac.ffmpeg.org/wiki/Slideshow).
const lastFileLine = [...lines].reverse().find((l) => l.startsWith("file "));
lines.push(lastFileLine);

writeFileSync(listPath, lines.join("\n") + "\n");

// ── 2. cache check + encode ─────────────────────────────────────────────
const inputHash = createHash("sha256")
  .update(JSON.stringify(subs.map((s) => ({ f: s.file, a: s.startSec, b: s.endSec }))))
  .update(`total=${total.toFixed(3)}`)
  .digest("hex")
  .slice(0, 16);

if (!force && existsSync(webmPath) && existsSync(hashFile)) {
  const cached = readFileSync(hashFile, "utf8").trim();
  if (cached === inputHash) {
    const size = (readFileSync(webmPath).length / 1024).toFixed(0);
    console.log(`✓ ${webmPath} cached (${size} KB · hash ${inputHash}) — skipping ffmpeg`);
    console.log(`✓ ${listPath} · ${subs.length} chunks · total ${total.toFixed(2)}s`);
    process.exit(0);
  }
}

// libvpx-vp9 with yuva420p preserves the alpha channel through to the
// main compose's overlay filter. -t pins the output to the cut's total
// length so any concat-demuxer rounding doesn't bleed past the end.
console.log(`→ encoding subtitle track · ${subs.length} chunks · ${total.toFixed(2)}s`);
execFileSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "concat", "-safe", "0", "-i", listPath,
  "-vf", "fps=30,format=yuva420p",
  "-c:v", "libvpx-vp9",
  "-pix_fmt", "yuva420p",
  "-b:v", "2M",
  "-deadline", "realtime",
  "-cpu-used", "8",
  "-auto-alt-ref", "0",
  "-t", String(total),
  webmPath,
], { stdio: ["ignore", "ignore", "inherit"] });

writeFileSync(hashFile, inputHash + "\n");
const size = (readFileSync(webmPath).length / 1024).toFixed(0);
console.log(`✓ ${webmPath} (${size} KB · hash ${inputHash})`);
console.log(`✓ ${listPath} · ${subs.length} chunks · total ${total.toFixed(2)}s`);
