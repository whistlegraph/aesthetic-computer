#!/usr/bin/env node
// vertical-amazing.mjs — 1080×1920 vertical IG cut for amazing-grace
// (the 1:24 single edit: verse-1-only with cool sine intro/outro).
//
// Inputs (must already exist):
//   ~/Documents/Working Desktop/gens/amazing-grace-sections/verse{1,6}/gens/v1.png
//   ~/Desktop/amazing-grace.mp3                 (the 1:24 brightened master)
//
// Output:
//   ~/Desktop/amazing-grace-vertical.mp4        (1080×1920, H.264, AAC)
//
// Two-illy slow-blend, ken-burns drift across 1:24. verse1 illy holds
// the first ~46 s (the golden-hour arrival + the vocal verse), then a
// 6-second xfade into verse6 (the deeper-dusk pew-light frame) for
// the long sine-only outro. Title PNG (pre-rendered via magick) fades
// in over 0-3 s.
//
// Note: brew ffmpeg 8.1 here ships without libass / drawtext, so we
// pre-render the title text via ImageMagick and composite it via the
// `overlay` filter. No lyric overlay in this cut.

import { existsSync, mkdirSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const HOME = homedir();
void HERE;
const SECROOT = `${HOME}/Documents/Working Desktop/gens/amazing-grace-sections`;
const AUDIO = `${HOME}/Desktop/amazing-grace.mp3`;
const OUT = `${HOME}/Desktop/amazing-grace-vertical.mp4`;
const W = 1080;
const H = 1920;
const FPS = 30;

const TOTAL = 84.0;                // single duration
const SEG1 = 50.0;                 // verse1 illy holds for 50 s
const SEG2 = 40.0;                 // verse6 illy holds for last 40 s
const XFADE = 6.0;                 // long slow blend
const SEG1_END = SEG1;             // xfade begins at 50.0 → ends 56.0

const ILLY1 = `${SECROOT}/verse1/gens/v2.png`;
const ILLY2 = `${SECROOT}/verse6/gens/v2.png`;
for (const p of [ILLY1, ILLY2]) {
  if (!existsSync(p)) { console.error(`✗ missing illy ${p}`); process.exit(1); }
}
if (!existsSync(AUDIO)) { console.error(`✗ missing ${AUDIO}`); process.exit(1); }

const tmp = `/tmp/amazing-vertical-${process.pid}`;
mkdirSync(tmp, { recursive: true });

// ── render title PNG via ImageMagick ────────────────────────────────────
const TITLE_PNG = `${tmp}/title.png`;
{
  const args = [
    "-size", `${W}x600`, "xc:none",
    "-gravity", "center",
    "-fill", "white",
    "-font", "/System/Library/Fonts/Helvetica.ttc",
    "-pointsize", "150",
    "-stroke", "#101218", "-strokewidth", "8",
    "-annotate", "+0-60", "amazing grace",
    "-stroke", "none",
    "-fill", "#FFF4E0",
    "-font", "/System/Library/Fonts/Helvetica.ttc",
    "-pointsize", "44",
    "-annotate", "+0+90", "aesthetic dot computer  ·  pixsies",
    TITLE_PNG,
  ];
  const r = spawnSync("magick", args, { stdio: ["ignore", "inherit", "inherit"] });
  if (r.status !== 0) { console.error("✗ magick title PNG failed"); process.exit(1); }
}
console.log(`▸ title PNG → ${TITLE_PNG}`);

// ── ffmpeg filter graph ────────────────────────────────────────────────
// Each illy gets a slow ken-burns zoom over its own segment duration.
const seg1Frames = Math.round(SEG1 * FPS) + Math.round(XFADE * FPS);
const seg2Frames = Math.round(SEG2 * FPS) + Math.round(XFADE * FPS);
const seg1Dur = (SEG1 + XFADE).toFixed(3);
const seg2Dur = (SEG2 + XFADE).toFixed(3);

const illyInputs = [
  "-loop","1","-t", seg1Dur, "-r", String(FPS), "-i", ILLY1,
  "-loop","1","-t", seg2Dur, "-r", String(FPS), "-i", ILLY2,
];
const titleInput = ["-loop","1","-t","3.5","-i", TITLE_PNG];

const TITLE_IDX = 2;
const AUDIO_IDX = 3;

const filters = [];
filters.push(
  `[0:v]scale=${Math.floor(W*1.18)}:-2,` +
  `zoompan=z='min(zoom+0.00035,1.10)':d=${seg1Frames}:s=${W}x${H}:fps=${FPS},` +
  `setsar=1,format=yuv420p[v0]`
);
filters.push(
  `[1:v]scale=${Math.floor(W*1.18)}:-2,` +
  `zoompan=z='min(zoom+0.00030,1.09)':d=${seg2Frames}:s=${W}x${H}:fps=${FPS},` +
  `setsar=1,format=yuv420p[v1]`
);
// xfade at SEG1_END for XFADE seconds
filters.push(`[v0][v1]xfade=transition=fade:duration=${XFADE}:offset=${SEG1_END.toFixed(3)}[vbase]`);
// title overlay (0 → 3 s, fade in 0.4 s, fade out 0.6 s)
filters.push(
  `[${TITLE_IDX}:v]format=rgba,fade=t=in:st=0:d=0.4:alpha=1,fade=t=out:st=2.4:d=0.6:alpha=1[title]`
);
filters.push(
  `[vbase][title]overlay=x=(W-w)/2:y=820:enable='between(t,0,3)'[vfinal]`
);

const filterComplex = filters.join(";");

const args = [
  "-y", "-hide_banner", "-loglevel", "warning", "-stats",
  ...illyInputs,
  ...titleInput,
  "-i", AUDIO,
  "-filter_complex", filterComplex,
  "-map", "[vfinal]",
  "-map", `${AUDIO_IDX}:a`,
  "-c:v", "libx264", "-profile:v", "high", "-level", "4.0",
  "-pix_fmt", "yuv420p", "-r", String(FPS),
  "-preset", "medium", "-crf", "20",
  "-c:a", "aac", "-b:a", "192k", "-ar", "44100",
  "-movflags", "+faststart",
  "-t", String(TOTAL),
  "-shortest",
  OUT,
];

console.log(`▸ ffmpeg: verse1 illy (0-${SEG1_END.toFixed(1)}s) + ${XFADE}s xfade + verse6 illy → 1:24 IG cut`);
console.log(`  → ${OUT}`);
const t0 = Date.now();
const r = spawnSync("ffmpeg", args, { stdio: "inherit" });
if (r.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(r.status || 1); }
const dt = ((Date.now() - t0) / 1000).toFixed(1);
console.log(`\n✓ ${OUT}  (${dt}s)`);
