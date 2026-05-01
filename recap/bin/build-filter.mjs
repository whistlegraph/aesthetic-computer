#!/usr/bin/env node
// build-filter.mjs — emit the ffmpeg filter_complex graph for compose.fish.
// Reads out/subs.json and stitches one overlay per subtitle chunk into the
// video chain so each chunk appears only between its [startSec, endSec].
// Subs sit at the top of the frame so they don't collide with the slide's
// bottom-third title overlay; the waveform sits at the same y, layered behind
// the pill so it animates "through" the subtitle.
// Usage: node bin/build-filter.mjs <totalSec>  (writes graph to stdout)

import { readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const TOTAL = process.argv[2];
if (!TOTAL) {
  console.error("usage: build-filter.mjs <totalSec>");
  process.exit(1);
}

const subs = JSON.parse(readFileSync(`${ROOT}/out/subs.json`, "utf8"));

// Subtitle band lives just above the progress bar at the bottom of the frame,
// below the slide's title overlay. Sub PNGs are 1080×220 with the pill centered.
// Waveform is parked at the same vertical area so the pill sits in front of
// the dancing line.
// Centered horizontally — sub PNGs are 1080×220 so x=0.
const SUB_Y = 1690;
const WAVE_Y = 1752; // 1080×96 waveform centered behind the sub pill (~y 1752–1848)

const lines = [];
lines.push(`[0:v]format=yuv420p,fps=30,scale=1080:1920,setsar=1[bg]`);
lines.push(`[1:a]apad=whole_dur=${TOTAL},asplit=2[a1][a2]`);
lines.push(`[a2]showwaves=s=1080x96:colors=0xff70d0|0x70f0e0:mode=cline:rate=30,format=rgba,colorchannelmixer=aa=0.55[wave]`);
lines.push(`[bg][wave]overlay=x=0:y=${WAVE_Y}:format=auto[bg2]`);
lines.push(`[bg2]drawbox=x=0:y=1912:w='iw*t/${TOTAL}':h=8:color=0xff69b4:t=fill[v0]`);

let prev = "v0";
for (let i = 0; i < subs.length; i++) {
  const s = subs[i];
  const srcLabel = `s${i}`;
  const nextLabel = `v${i + 1}`;
  // movie filter loads PNG with alpha; format=rgba ensures alpha is preserved.
  lines.push(`movie='${s.file}':loop=0,setpts=N/(FRAME_RATE*TB),format=rgba[${srcLabel}]`);
  lines.push(`[${prev}][${srcLabel}]overlay=x=0:y=${SUB_Y}:format=auto:enable='between(t,${s.startSec},${s.endSec})'[${nextLabel}]`);
  prev = nextLabel;
}

// Final stream needs the canonical [final] label for compose.fish -map.
lines.push(`[${prev}]null[final]`);

process.stdout.write(lines.join(";\n") + "\n");
