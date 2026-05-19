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

// Bottom chrome — piano flush bottom y=1830..1920. Progress bar sits
// FLUSH to the top of the piano keys (its bottom = piano top = 1830)
// with a solid full-width dark background "track" that the chapter
// segments fill into.
const PROGRESS_H = 16;
const PROGRESS_Y = 1830 - PROGRESS_H; // y=1814..1830, bottom flush with piano top

const lines = [];
// IMPORTANT: include `fps=25` here so the libass `subtitles=` filters
// downstream see one frame per output tick (25 fps × 281 s = ~7000
// frames). Without it, the concat demuxer emits ONE frame per slide,
// libass renders that single frame, and the encoder dups it forward —
// so animated overlays (waltz piano-roll) only flash on the rare frames
// that happen to coincide with a slide transition. The earlier "do NOT
// add fps" warning was about adding it AFTER the libass chain or to the
// wrong stream — applied to [bg] up front, it's correct and necessary.
// Inputs (per compose.fish):
//   [0:v] photos.txt   — raw jeffrey-photos at slide durations
//   [1:v] chrome.txt   — transparent chrome PNGs (chapter prompt /
//                        cap / QR / PALS) at slide durations
//   [2:a] narration mp3
//   [3:a] waltz.mp3 (optional)
//   [4:a] beat.mp3 (optional)
//
// PHOTO STREAM — handicam treatment. jeffrey-photos are 1024×1536
// (2:3); the slide frame is 1080×1920 (9:16). We scale source up by
// height to 2112 PRESERVING ASPECT (avoiding the squashed look that
// hard-forcing 1188×2112 produced), then center-crop to a 1188×2112
// canvas so the crop-pan can drift inside without exposing borders.
// Time-driven sine offsets on the inner crop produce a subtle hand-
// held camera shake on the portraits ONLY — chrome composited on top
// stays still. fps=50 + tmix=frames=12 + fps=25 gives ~0.24s temporal
// blend at chapter cuts. Final film grain (alls=14, temporal+uniform)
// breathes.
// Input #0 is photos.mov — pre-rendered in compose.fish to a continuous
// 1188×2112 yuv420p 25fps stream. The pre-render exists to dodge an
// ffmpeg deadlock: concat-demuxer-over-PNGs as a `-i` input alongside
// any audio `-i` AND the libass `subtitles=` filter downstream stalls
// at frame=0 forever (libass per-frame ticks starve the demuxer).
// Baking photos to a real video file fixes it.
//
// ffmpeg 8.1's crop filter has no `eval` option — x/y are re-evaluated
// per frame automatically because the option carries the `T` (timeline)
// flag. Setting `eval=frame` errors as "Option not found".
lines.push(`[0:v]crop=1080:1920:x='54+24*sin(t*0.6)+12*sin(t*1.7)':y='96+18*sin(t*0.4+1.2)+10*sin(t*1.3+0.5)'[bgPan]`);
lines.push(`[bgPan]tmix=frames=12,fps=25,noise=alls=14:allf=t+u[bgFilm]`);

// CHROME STREAM — pre-rendered chrome.mov (qtrle, RGBA, 25fps,
// 1080x1920) created in compose.fish, read via `movie=` filter source
// (NOT a `-i` input). The latter deadlocks: ffmpeg's input scheduler
// stalls when the photo input is a concat-demuxer over slow PNGs AND
// the graph has libass `subtitles=` downstream — frame=0 forever. The
// `movie=` source bypasses the input scheduler entirely, so frames
// flow.
const CHROME_MOV = resolve(ROOT, "out/chrome.mov");
const escMovie = CHROME_MOV.replace(/\\/g, "\\\\").replace(/:/g, "\\:");
lines.push(`movie='${escMovie}',format=rgba,setsar=1[chr]`);

// Composite chrome on top of the handicam'd photo. The chrome stays
// dead-still while the photo zooms and shakes underneath.
lines.push(`[bgFilm][chr]overlay=format=auto[v0]`);

// Audio inputs renumbered: chrome moved from `-i` (slot 1) to a `movie=`
// filter source, so narration is now [1:a], waltz [2:a], beat [3:a].
lines.push(`[1:a]apad=whole_dur=${TOTAL}[a1]`);
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
const layoutsPath = `${ROOT}/out/layouts.json`;
const segs = existsSync(segmentsPath)
  ? JSON.parse(readFileSync(segmentsPath, "utf8")).filter((s) => s.marker !== "__END__")
  : [{ name: "_default", startSec: 0, endSec: Number(TOTAL) }];
// Per-segment chapter color from layouts.json — drives the progress
// bar fill hue so each chapter beat carries its own color.
const layoutsForColors = existsSync(layoutsPath) ? JSON.parse(readFileSync(layoutsPath, "utf8")) : {};
function segColor(name) {
  const c = layoutsForColors[name] && layoutsForColors[name].color;
  if (c && c.hex) return c.hex.replace("#", "0x");
  return "0xff69b4"; // hot pink fallback
}

const W = 1080;
let prevTag = "v2";
// Solid dark "track" background for the entire progress bar — sits
// behind the per-chapter segment fills so the bar shape reads even
// before any chapter has started.
lines.push(`[${prevTag}]drawbox=x=0:y=${PROGRESS_Y}:w=${W}:h=${PROGRESS_H}:color=0x0c1430@0.78:t=fill[ptrack]`);
prevTag = "ptrack";
for (let i = 0; i < segs.length; i++) {
  const s = segs[i];
  const x0 = (s.startSec / Number(TOTAL)) * W;
  const segW = ((s.endSec - s.startSec) / Number(TOTAL)) * W;
  const segHex = segColor(s.name);          // 0xRRGGBB
  const baseColor = `${segHex}@0.40`;        // soft "passed-through" tint
  const fillColor = segHex;                  // bright leading edge as time advances
  const nextTag = `prog${i}`;
  lines.push(`[${prevTag}]drawbox=x=${x0.toFixed(1)}:y=${PROGRESS_Y}:w=${segW.toFixed(1)}:h=${PROGRESS_H}:color=${baseColor}:t=fill:enable='gte(t,${s.startSec})'[${nextTag}]`);
  prevTag = nextTag;

  const fillTag = `pfill${i}`;
  const dur = s.endSec - s.startSec;
  const fillExpr = `min(${segW.toFixed(1)},${segW.toFixed(1)}*(t-${s.startSec})/${dur.toFixed(2)})`;
  lines.push(`[${prevTag}]drawbox=x=${x0.toFixed(1)}:y=${PROGRESS_Y}:w='${fillExpr}':h=${PROGRESS_H}:color=${fillColor}:t=fill:enable='between(t,${s.startSec},${s.endSec})'[${fillTag}]`);
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
