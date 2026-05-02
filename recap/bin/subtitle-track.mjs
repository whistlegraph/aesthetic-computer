#!/usr/bin/env node
// subtitle-track.mjs — emit a concat-demuxer file (`out/subtitle-track.txt`)
// that sequences subtitle PNGs with explicit durations, so the main compose
// can include subtitles via a single `-f concat -i subtitle-track.txt`
// input + one overlay filter — instead of a 135-deep `movie=...` chain.
//
// Reads `out/subs.json` (timing + per-chunk PNG paths) and `out/subs/blank.png`
// (a fully-transparent 1080×1920 PNG produced by subtitles.mjs).
//
// The output is a plain concat-demuxer text file. ffmpeg picks it up at
// frame rate via:
//   -f concat -safe 0 -i out/subtitle-track.txt
// The `duration` directive is honored on each entry. The very last `file`
// must be repeated (concat-demuxer quirk) so the final entry's duration
// applies.
//
// Usage: node bin/subtitle-track.mjs

import { readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const subsPath = `${ROOT}/out/subs.json`;
const blankPath = `${ROOT}/out/subs/blank.png`;
const outPath = `${ROOT}/out/subtitle-track.txt`;
const durPath = `${ROOT}/out/duration.txt`;

const subs = JSON.parse(readFileSync(subsPath, "utf8"));
if (!subs.length) {
  console.error(`✗ no subtitle chunks in ${subsPath}`);
  process.exit(1);
}

// Total video duration (so the track ends at the right moment). If
// duration.txt isn't around yet, fall back to the last subtitle endSec
// (slides.mjs will have set duration.txt before this runs in the pipeline).
let total;
try { total = parseFloat(readFileSync(durPath, "utf8")); }
catch { total = subs[subs.length - 1].endSec; }

const lines = [];
let cursor = 0;
for (const s of subs) {
  if (s.startSec > cursor + 0.001) {
    // Gap before this chunk: blank.
    lines.push(`file '${blankPath}'`);
    lines.push(`duration ${(s.startSec - cursor).toFixed(3)}`);
  }
  lines.push(`file '${s.file}'`);
  lines.push(`duration ${(s.endSec - s.startSec).toFixed(3)}`);
  cursor = s.endSec;
}
// Trailing blank to fill the rest of the timeline.
if (total > cursor + 0.001) {
  lines.push(`file '${blankPath}'`);
  lines.push(`duration ${(total - cursor).toFixed(3)}`);
}
// Concat demuxer requires the last `file` line repeated for its duration
// to apply (https://trac.ffmpeg.org/wiki/Slideshow).
const lastFileLine = [...lines].reverse().find((l) => l.startsWith("file "));
lines.push(lastFileLine);

writeFileSync(outPath, lines.join("\n") + "\n");
console.log(`✓ ${outPath} · ${subs.length} chunks · total ${total.toFixed(2)}s`);
