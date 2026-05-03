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

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");
const TOTAL = process.argv[2];
if (!TOTAL) {
  console.error("usage: build-filter.mjs <totalSec>");
  process.exit(1);
}

const WAVE_Y = 1752; // y-band for the audio waveform under the subtitle pill

const lines = [];
// NOTE: do NOT add an `fps=30` filter to inputs that come from a concat
// demuxer with `duration` directives (slides input #0). The fps filter
// mis-handles the sparse PTS the demuxer emits and truncates the output
// stream. ffmpeg honors the duration directives natively at the encoder.
lines.push(`[0:v]format=yuv420p,scale=1080:1920,setsar=1[bg]`);
lines.push(`[1:a]apad=whole_dur=${TOTAL},asplit=2[a1][a2]`);
lines.push(`[a2]showwaves=s=1080x96:colors=0xff70d0|0x70f0e0:mode=cline:rate=30,format=rgba,colorchannelmixer=aa=0.55[wave]`);
lines.push(`[bg][wave]overlay=x=0:y=${WAVE_Y}:format=auto[bg2]`);
lines.push(`[bg2]drawbox=x=0:y=1912:w='iw*t/${TOTAL}':h=8:color=0xff69b4:t=fill[v0]`);
// Subtitles via libass — single filter pass, no pre-encode, no alpha
// codec dance. fontsdir picks up the YWFT face shipped in the repo so
// the .ass `Style: YWFTProcessing` resolves. The .ass path is escaped
// for ffmpeg's filter parser (`:` and `\` need it inside subtitles=...).
// Inside ffmpeg's `filter_complex_script`, the `subtitles=` argument
// uses `:` as a filter-arg separator. Path colons must be escaped with
// `\:`. Backslashes don't appear in Linux paths so we don't worry about
// those. Single-quote the values for safety.
const escFilter = (p) => p.replace(/:/g, "\\:");
const ASS = escFilter(`${ROOT}/out/subs.ass`);
const FONTSDIR = escFilter(`${REPO}/system/public/type/webfonts`);
lines.push(`[v0]subtitles='${ASS}':fontsdir='${FONTSDIR}'[final]`);

process.stdout.write(lines.join(";\n") + "\n");
