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

const TOTAL = process.argv[2];
if (!TOTAL) {
  console.error("usage: build-filter.mjs <totalSec>");
  process.exit(1);
}

const WAVE_Y = 1752; // y-band for the audio waveform under the subtitle pill

const lines = [];
lines.push(`[0:v]format=yuv420p,fps=30,scale=1080:1920,setsar=1[bg]`);
lines.push(`[1:a]apad=whole_dur=${TOTAL},asplit=2[a1][a2]`);
lines.push(`[a2]showwaves=s=1080x96:colors=0xff70d0|0x70f0e0:mode=cline:rate=30,format=rgba,colorchannelmixer=aa=0.55[wave]`);
lines.push(`[bg][wave]overlay=x=0:y=${WAVE_Y}:format=auto[bg2]`);
lines.push(`[bg2]drawbox=x=0:y=1912:w='iw*t/${TOTAL}':h=8:color=0xff69b4:t=fill[v0]`);
// Single subtitle-track overlay. Input #2 is a pre-encoded webm with
// alpha (vp9 / yuva420p) produced by subtitle-track.mjs — see comments
// there. Format the alpha track to match the slide pipeline and overlay.
lines.push(`[2:v]fps=30,format=rgba,scale=1080:1920[subs]`);
lines.push(`[v0][subs]overlay=x=0:y=0:format=auto:shortest=0[final]`);

process.stdout.write(lines.join(";\n") + "\n");
