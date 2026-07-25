#!/usr/bin/env node
// Render the 12-bar Special Sign sine-system reel from the locked illys.

import { mkdirSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..", "..");
const ILLYS = resolve(HERE, "illys");
const OUT = resolve(HERE, "out");
const WORK = resolve(OUT, ".work");
const AUDIO = resolve(REPO, "pop/nullabye/release/special-sign/special-sign-MASTER.wav");
const OUTPUT = resolve(OUT, "special-sign-sine-system-reel.mp4");

const FPS = 30;
const BPM = 76;
const BAR_SECONDS = 240 / BPM;
const TOTAL_SECONDS = 12 * BAR_SECONDS;

// Rounded cumulative boundaries keep the cut within half a video frame of
// every exact bar/half-bar boundary and total exactly 1137 frames.
const beats = [
  { image: "01-quiet-code.png", bars: 2, frames: 189 },
  { image: "02-first-sine.png", bars: 1.5, frames: 143 },
  { image: "03-listen-vibe.png", bars: 1.5, frames: 142 },
  { image: "04-add-rings.png", bars: 2, frames: 189 },
  { image: "05-gremlin-build.png", bars: 2, frames: 190 },
  { image: "06-full-system.png", bars: 3, frames: 284 },
];

function run(args) {
  const result = spawnSync("ffmpeg", args, { stdio: "inherit" });
  if (result.status !== 0) process.exit(result.status ?? 1);
}

mkdirSync(WORK, { recursive: true });
mkdirSync(OUT, { recursive: true });

const clips = [];
for (const [index, beat] of beats.entries()) {
  const input = resolve(ILLYS, beat.image);
  const clip = resolve(WORK, `${String(index + 1).padStart(2, "0")}.mp4`);
  const maxFrame = Math.max(1, beat.frames - 1);
  const filter = [
    "[0:v]split=2[bg][fg]",
    "[bg]scale=1080:1920:force_original_aspect_ratio=increase,crop=1080:1920,gblur=sigma=34,eq=brightness=-0.14:saturation=0.82[bg2]",
    `[fg]scale=1200:1200,zoompan=z='1+0.035*on/${maxFrame}':x='(iw-iw/zoom)/2':y='(ih-ih/zoom)/2':d=1:s=1200x1200:fps=${FPS}[fg2]`,
    "[bg2][fg2]overlay=-60:360:shortest=1,format=yuv420p[out]",
  ].join(";");

  run([
    "-hide_banner", "-loglevel", "error", "-y",
    "-loop", "1", "-framerate", String(FPS), "-i", input,
    "-filter_complex", filter,
    "-map", "[out]", "-frames:v", String(beat.frames),
    "-c:v", "libx264", "-preset", "slow", "-crf", "17",
    "-r", String(FPS), "-g", "60", "-keyint_min", "60", "-sc_threshold", "0",
    "-an", clip,
  ]);
  clips.push(clip);
}

const concatPath = resolve(WORK, "concat.txt");
writeFileSync(concatPath, clips.map((clip) => `file '${clip.replaceAll("'", "'\\''")}'`).join("\n") + "\n");

const silent = resolve(WORK, "silent.mp4");
run([
  "-hide_banner", "-loglevel", "error", "-y",
  "-f", "concat", "-safe", "0", "-i", concatPath,
  "-c", "copy", silent,
]);

const fadeStart = TOTAL_SECONDS - 0.25;
run([
  "-hide_banner", "-loglevel", "error", "-y",
  "-i", silent, "-i", AUDIO,
  "-filter_complex", `[1:a]atrim=start=0:end=${TOTAL_SECONDS.toFixed(9)},afade=t=out:st=${fadeStart.toFixed(9)}:d=0.25[a]`,
  "-map", "0:v:0", "-map", "[a]",
  "-c:v", "copy", "-c:a", "aac", "-b:a", "256k", "-ar", "48000",
  "-movflags", "+faststart", OUTPUT,
]);

console.log(`✓ ${OUTPUT}`);
console.log(`  ${TOTAL_SECONDS.toFixed(6)}s · ${FPS} fps · 1080x1920 · 12 bars @ ${BPM} BPM`);
