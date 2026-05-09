#!/usr/bin/env node
// build-filter.mjs — emit the ffmpeg filter_complex graph for compose.fish.
//
// Inputs (per compose.fish):
//   [0:v] slide concat (PNG sequence)
//   [1:a] narration mp3
//   [2:v] subtitle track concat (full-frame transparent PNG sequence — see
//         subtitle-track.mjs); a single overlay onto the slide stream
//         replaces the old 135-deep movie= chain.
//
// Subtitle PNGs are now full-frame 1080×1920 transparent images with the
// pill positioned at y=1690, so we just overlay [2:v] at (0,0). The
// concat demuxer at input #2 plays them with their stored durations,
// alternating with a fully-transparent blank frame for gaps.
//
// Usage: node bin/build-filter.mjs <totalSec>  (writes graph to stdout)

import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { existsSync, readFileSync } from "node:fs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");
const TOTAL = process.argv[2];
if (!TOTAL) {
  console.error("usage: build-filter.mjs <totalSec>");
  process.exit(1);
}

// Bottom chrome — piano fills y=1830..1920 flush bottom. Progress bar
// sits ABOVE the piano keys (NOT overlapping them) with a small gap.
// The PALS logo runs left-to-right ON TOP of the progress bar over the
// full episode, so the bar reads like a track and PALS is the runner.
const PROGRESS_Y = 1800;   // 14 tall, sits y=1800..1814 — clear of piano (1830+)
const PROGRESS_H = 14;
const PALS_RUN_Y = 1700;   // PALS bottom rests at y=1800 (= top of progress bar)
const PALS_RUN_H = 100;    // 100×100 PNG sized at compose time

const lines = [];
// IMPORTANT: include `fps=25` here so the libass `subtitles=` filters
// downstream see one frame per output tick (25 fps × 281 s = ~7000
// frames). Without it, the concat demuxer emits ONE frame per slide,
// libass renders that single frame, and the encoder dups it forward —
// so animated overlays (waltz piano-roll) only flash on the rare frames
// that happen to coincide with a slide transition. The earlier "do NOT
// add fps" warning was about adding it AFTER the libass chain or to the
// wrong stream — applied to [bg] up front, it's correct and necessary.
lines.push(`[0:v]format=yuv420p,scale=1080:1920,setsar=1,fps=25[bg]`);
lines.push(`[1:a]apad=whole_dur=${TOTAL}[a1]`);
lines.push(`[bg]null[v0]`);
// Subtitles via libass — single filter pass, no pre-encode, no alpha
// codec dance. fontsdir picks up the YWFT face shipped in the repo so
// the .ass `Style: YWFTProcessing` resolves. The .ass path is escaped
// for ffmpeg's filter parser (`:` and `\` need it inside subtitles=...).
const escFilter = (p) => p.replace(/:/g, "\\:");
const ASS = escFilter(`${ROOT}/out/subs.ass`);
const KEYS = escFilter(`${ROOT}/out/waltz-keys.ass`);
const FONTSDIR = escFilter(`${REPO}/system/public/type/webfonts`);
lines.push(`[v0]subtitles='${ASS}':fontsdir='${FONTSDIR}'[v1]`);
// Waltz piano-roll — full-width keyboard flush at the bottom of the
// frame (y=1830..1920). Drawn BEFORE the segmented progress bar so the
// progress markers layer on top of the keys.
lines.push(`[v1]subtitles='${KEYS}'[v2]`);

// Segmented progress bar — drawn LAST so it sits ON TOP of the piano
// keys. One drawbox per chapter segment. Each box fills as soon as
// the chapter starts (chapter-granular structure) AND a within-chapter
// fill grows from 0 to the segment's full width across its duration.
// Thin dark divider lines mark chapter boundaries.
const segmentsPath = `${ROOT}/out/segments.json`;
const segs = existsSync(segmentsPath)
  ? JSON.parse(readFileSync(segmentsPath, "utf8")).filter((s) => s.marker !== "__END__")
  : [{ name: "_default", startSec: 0, endSec: Number(TOTAL) }];

const W = 1080;
let prevTag = "v2";
for (let i = 0; i < segs.length; i++) {
  const s = segs[i];
  const x0 = (s.startSec / Number(TOTAL)) * W;
  const segW = ((s.endSec - s.startSec) / Number(TOTAL)) * W;
  const baseColor = ["0xff69b4@0.55", "0xff40a0@0.55", "0xff70d0@0.55", "0xff90c0@0.55", "0xff5099@0.55"][i % 5];
  const nextTag = `prog${i}`;
  lines.push(`[${prevTag}]drawbox=x=${x0.toFixed(1)}:y=${PROGRESS_Y}:w=${segW.toFixed(1)}:h=${PROGRESS_H}:color=${baseColor}:t=fill:enable='gte(t,${s.startSec})'[${nextTag}]`);
  prevTag = nextTag;

  const fillTag = `pfill${i}`;
  const dur = s.endSec - s.startSec;
  const fillExpr = `min(${segW.toFixed(1)},${segW.toFixed(1)}*(t-${s.startSec})/${dur.toFixed(2)})`;
  lines.push(`[${prevTag}]drawbox=x=${x0.toFixed(1)}:y=${PROGRESS_Y}:w='${fillExpr}':h=${PROGRESS_H}:color=0xff1493:t=fill:enable='between(t,${s.startSec},${s.endSec})'[${fillTag}]`);
  prevTag = fillTag;

  if (i < segs.length - 1) {
    const divX = (s.endSec / Number(TOTAL)) * W;
    const divTag = `div${i}`;
    lines.push(`[${prevTag}]drawbox=x=${divX.toFixed(1)}:y=${PROGRESS_Y}:w=2:h=${PROGRESS_H}:color=0x100810:t=fill[${divTag}]`);
    prevTag = divTag;
  }
}
// PALS watermarks — TWO low-opacity, 90°-rotated PALS bugs sit at the
// LEFT and RIGHT edges of every frame. They're baked into the slide
// PNG (audience photoSlide) with the per-chapter chromatic drop shadow,
// so this filter chain doesn't need to add any video-layer PALS overlays.
lines.push(`[${prevTag}]null[final]`);

process.stdout.write(lines.join(";\n") + "\n");
