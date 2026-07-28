#!/usr/bin/env node
// Recover the clean visual spine when the original July camera masters are no
// longer local. The outgoing Signal review still contains the complete edit,
// but its first-pass captions are burned into the lower third. Keep the visual
// performance above a replaceable information panel; caption-and-mix.mjs then
// fills that field with an envelope-derived chapter color and lays in the
// corrected narration, captions, chapter label, and progress bar.

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = dirname(fileURLToPath(import.meta.url));
const input = process.argv[2];
const output = resolve(ROOT, "out", "unboxing-spine-realtime.mp4");

if (!input || !existsSync(input)) {
  console.error("usage: node recover-from-signal-edit.mjs /absolute/path/to/review.mov");
  process.exit(2);
}

mkdirSync(dirname(output), { recursive: true });

// The corrected read changes the duration of several chapters. Retiming each
// chapter independently keeps the visual contribution under its own narration
// instead of letting timing drift accumulate across the whole reel.
const oldStarts = [0, 18.88, 31.92, 41.32, 51.96, 64.68, 75.08, 88.48, 101.56, 112.64, 124.08, 134.92, 148.267];
const newStarts = [0, 18.425, 31.091, 38.359, 47.786, 61.207, 71.041, 81.942, 93.68, 104.163, 116.818, 127.185, 141.084];
const count = oldStarts.length - 1;
const filters = [`[0:v]split=${count}${Array.from({ length: count }, (_, i) => `[v${i}]`).join("")}`];
for (let i = 0; i < count; i++) {
  const oldDuration = oldStarts[i + 1] - oldStarts[i];
  const newDuration = newStarts[i + 1] - newStarts[i];
  const ratio = newDuration / oldDuration;
  filters.push(`[v${i}]trim=start=${oldStarts[i]}:end=${oldStarts[i + 1]},setpts=(PTS-STARTPTS)*${ratio.toFixed(9)}[s${i}]`);
}
// The original captions occupy the lower third. Crop just above them, then
// preserve the missing area as a stable editorial field instead of stretching
// the image or hiding the correction with another floating subtitle stack. Its
// temporary color is completely replaced by caption-and-mix.mjs.
filters.push(
  `${Array.from({ length: count }, (_, i) => `[s${i}]`).join("")}concat=n=${count}:v=1:a=0,` +
  "crop=1080:1280:0:0,pad=1080:1920:0:0:color=0x101014," +
  "drawbox=x=0:y=1280:w=1080:h=3:color=white@0.22:t=fill,fps=30,format=yuv420p[outv]",
);

const result = spawnSync("ffmpeg", [
  "-y", "-hide_banner", "-loglevel", "warning",
  "-i", resolve(input),
  "-filter_complex", filters.join(";"), "-map", "[outv]", "-an", "-r", "30",
  "-c:v", "libx264", "-preset", "fast", "-crf", "17",
  "-maxrate", "20M", "-bufsize", "40M", "-pix_fmt", "yuv420p",
  "-movflags", "+faststart", output,
], { stdio: "inherit" });

if (result.status !== 0) process.exit(result.status ?? 1);
console.log(output);
