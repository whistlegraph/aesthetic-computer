#!/usr/bin/env node
// Re-time the approved 141-second picture spine to a revised narration without
// needing the archived raw camera master. Each chapter remains isolated, so a
// longer read cannot drift Thomas imagery into Banyi's caption (or vice versa).

import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { loadNarrationTimeline } from "./timing.mjs";

const ROOT = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(ROOT, "out");
const input = resolve(OUT, "unboxing-spine-realtime.mp4");
const output = resolve(OUT, "unboxing-spine-retimed.mp4");
// Exact boundaries recorded when the approved source spine was assembled.
const oldStarts = [0, 18.425, 31.091, 38.359, 47.786, 61.207, 71.041, 81.942, 93.68, 104.163, 116.818, 127.185, 141.084];
const timing = loadNarrationTimeline(ROOT);
const newStarts = [...timing.lines.map((line) => line.startSec), timing.totalDuration];
if (newStarts.length !== oldStarts.length) throw new Error("expected the same 12 narration chapters as the approved spine");

const filters = [`[0:v]split=12${Array.from({ length: 12 }, (_, i) => `[s${i}]`).join("")}`];
const labels = [];
for (let i = 0; i < 12; i += 1) {
  const oldDuration = oldStarts[i + 1] - oldStarts[i];
  const newDuration = newStarts[i + 1] - newStarts[i];
  filters.push(`[s${i}]trim=start=${oldStarts[i]}:end=${oldStarts[i + 1]},setpts=(PTS-STARTPTS)*${newDuration / oldDuration},fps=30,trim=duration=${newDuration},setpts=PTS-STARTPTS[c${i}]`);
  labels.push(`[c${i}]`);
}
filters.push(`${labels.join("")}concat=n=12:v=1:a=0[outv]`);

console.log(`retime approved spine · ${oldStarts.at(-1).toFixed(3)}s → ${timing.totalDuration.toFixed(3)}s`);
const result = spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "warning", "-i", input,
  "-filter_complex", filters.join(";"), "-map", "[outv]", "-an", "-r", "30",
  "-c:v", "libx264", "-preset", "fast", "-crf", "16", "-maxrate", "20M", "-bufsize", "40M",
  "-pix_fmt", "yuv420p", "-color_primaries", "bt709", "-color_trc", "bt709", "-colorspace", "bt709",
  "-movflags", "+faststart", output], { stdio: "inherit" });
if (result.status !== 0) process.exit(result.status ?? 1);
console.log(output);
