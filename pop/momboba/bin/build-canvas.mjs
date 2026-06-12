#!/usr/bin/env node
// momboba/bin/build-canvas.mjs — the momabobasheep Spotify Canvas.
// Canvas spec: 9:16 MP4, 3–8 s, ≥720 px wide, silent, loops forever.
// The dream take ping-pongs (4 s forward + 4 s reversed) so the loop
// never jump-cuts — the lily pond breathes in and out.
//
// Usage:
//   node pop/momboba/bin/build-canvas.mjs                 # dream take
//   node pop/momboba/bin/build-canvas.mjs --shot 6-asleep-flock --start 1.5

import { existsSync, copyFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");

const _af = (k, d) => { const i = process.argv.indexOf(k); return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : d; };
const SHOT = _af("--shot", "7-dream");
const START = Number(_af("--start", "0"));
const HALF = 4; // 4 s out + 4 s back = the spec's 8 s ceiling

const src = `${LANE}/out/motion/momabobasheep-reel-shot-${SHOT}.mp4`;
const out = `${LANE}/out/momabobasheep-canvas.mp4`;
if (!existsSync(src)) { console.error(`✗ take missing: ${src}`); process.exit(1); }

console.log(`▸ canvas · ${SHOT} @ ${START}s · ${HALF}s ping-pong → ${HALF * 2}s loop`);
const r = spawnSync("ffmpeg", [
  "-y", "-ss", String(START), "-t", String(HALF), "-i", src,
  "-filter_complex",
  "[0:v]scale=720:1280,fps=24,setpts=PTS-STARTPTS,split[a][b];[b]reverse[r];[a][r]concat=n=2:v=1[v]",
  "-map", "[v]", "-an",
  "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
  "-movflags", "+faststart", out,
], { stdio: ["ignore", "ignore", "pipe"] });
if (r.status !== 0) { console.error(`✗ ffmpeg: ${r.stderr.toString().slice(-400)}`); process.exit(1); }
console.log(`✓ ${out}`);

const shelf = `${homedir()}/Documents/Shelf/momboba`;
mkdirSync(shelf, { recursive: true });
copyFileSync(out, `${shelf}/momabobasheep-canvas.mp4`);
console.log(`✓ staged → ${shelf}/momabobasheep-canvas.mp4`);
