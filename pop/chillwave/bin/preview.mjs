#!/usr/bin/env node
// chillwave/bin/preview.mjs — render a preview mp4 from cover + audio.
//
// Single still cover (1024x1024) + audio mp3 → 1080x1080 mp4 with a
// slow ken-burns drift (subtle zoom-in across the whole track). No
// chrome, no title overlay — the cover speaks.
//
// Usage:
//   node bin/preview.mjs --slug wanderbeach
//   node bin/preview.mjs --slug wanderbeach --size 720x720
//   node bin/preview.mjs --slug wanderbeach --zoom 1.12  (default 1.08)

import { existsSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}

const SLUG = flags.slug || "helpabeach";
const SIZE = flags.size || "1080x1080";
const ZOOM_END = Number(flags.zoom ?? 1.08);    // final zoom factor
const FPS = Number(flags.fps ?? 30);

const COVER = `${LANE}/out/${SLUG}-cover.png`;
const AUDIO = `${LANE}/out/${SLUG}.mp3`;
const OUT   = `${LANE}/out/${SLUG}-preview.mp4`;

if (!existsSync(COVER)) {
  console.error(`✗ cover missing: ${COVER.replace(REPO + "/", "")}`);
  console.error(`  run: node bin/gen-illy.mjs --slug ${SLUG}`);
  process.exit(1);
}
if (!existsSync(AUDIO)) {
  console.error(`✗ audio missing: ${AUDIO.replace(REPO + "/", "")}`);
  console.error(`  run: node bin/render.mjs --slug ${SLUG}`);
  process.exit(1);
}

// Probe audio duration so we know how many frames to render.
const probe = spawnSync("ffprobe", [
  "-v", "error", "-show_entries", "format=duration",
  "-of", "default=noprint_wrappers=1:nokey=1", AUDIO,
], { encoding: "utf8" });
if (probe.status !== 0) {
  console.error(`✗ ffprobe failed: ${probe.stderr}`);
  process.exit(1);
}
const DURATION = parseFloat(probe.stdout.trim());
const FRAMES = Math.ceil(DURATION * FPS);

console.log(`▸ ${SLUG} preview · ${SIZE} · ${DURATION.toFixed(1)}s · ${FRAMES} frames`);

const [W, H] = SIZE.split("x").map(Number);

// zoompan needs the input upscaled so the zoom doesn't pixelate.
// We pre-upscale to 4x then zoompan in that space and crop down.
// Center the zoom and let it slowly drift toward an off-center point
// (slightly down-right) so the eye moves over the sand + fingers
// across the duration.
const upW = W * 4, upH = H * 4;
const filter =
  `scale=${upW}:${upH}:flags=lanczos,` +
  `zoompan=` +
    `z='1+(${(ZOOM_END - 1).toFixed(4)})*on/${FRAMES}':` +
    `x='iw/2-(iw/zoom/2) + (0.05*iw*on/${FRAMES})':` +
    `y='ih/2-(ih/zoom/2) + (0.04*ih*on/${FRAMES})':` +
    `d=1:s=${W}x${H}:fps=${FPS},` +
  `format=yuv420p`;

const r = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-loop", "1", "-framerate", String(FPS), "-i", COVER,
  "-i", AUDIO,
  "-vf", filter,
  "-c:v", "libx264", "-preset", "medium", "-crf", "20",
  "-c:a", "aac", "-b:a", "192k",
  "-pix_fmt", "yuv420p",
  "-shortest",
  "-movflags", "+faststart",
  OUT,
], { stdio: "inherit" });

if (r.status !== 0) {
  console.error(`✗ ffmpeg failed (exit ${r.status})`);
  process.exit(1);
}
console.log(`✓ ${OUT.replace(REPO + "/", "")}`);
