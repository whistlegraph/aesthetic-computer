#!/usr/bin/env node
// build-concept.mjs — assemble the 4 Seedance beats into the concept film:
// hard-cut the clips in narrative order, lay the jeffrey-pvc narrative VO on
// top, hold the last frame if the VO runs past the footage. Output concept-film.mp4.

import { existsSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const clips = ["beat-1-painter.mp4", "beat-2-awakening.mp4", "beat-3-commons.mp4", "beat-4-invitation.mp4"]
  .map((f) => join(HERE, f));
const vo = join(HERE, "vo-narrative.mp3");
const reel = join(HERE, "_reel.mp4");
const out = join(HERE, "concept-film.mp4");

for (const c of clips) if (!existsSync(c)) throw new Error(`missing clip: ${c}`);
const dur = (f) => parseFloat(execFileSync("ffprobe", ["-v", "error", "-show_entries",
  "format=duration", "-of", "default=noprint_wrappers=1:nokey=1", f]).toString().trim());

// 1) concat the 4 clips, normalized to 1920x1080 @30fps h264
console.log("▸ concat 4 beats …");
const inputs = clips.flatMap((c) => ["-i", c]);
const scale = "scale=1920:1080:force_original_aspect_ratio=increase,crop=1920:1080,setsar=1,fps=30";
const fc = clips.map((_, i) => `[${i}:v]${scale}[v${i}]`).join(";") +
  ";" + clips.map((_, i) => `[v${i}]`).join("") + `concat=n=${clips.length}:v=1:a=0[v]`;
execFileSync("ffmpeg", ["-y", ...inputs, "-filter_complex", fc, "-map", "[v]",
  "-c:v", "libx264", "-pix_fmt", "yuv420p", "-crf", "18", reel], { stdio: ["ignore", "ignore", "inherit"] });

const reelDur = dur(reel);
const voDur = existsSync(vo) ? dur(vo) : 0;
const extra = Math.max(0, voDur - reelDur + 0.6); // small tail
console.log(`  reel ${reelDur.toFixed(1)}s · vo ${voDur.toFixed(1)}s · hold last frame +${extra.toFixed(1)}s`);

// 2) hold last frame to cover the VO, mux VO audio
console.log("▸ mux VO + final encode …");
const args = ["-y", "-i", reel];
if (existsSync(vo)) args.push("-i", vo);
const vf = `tpad=stop_mode=clone:stop_duration=${extra.toFixed(2)}`;
args.push("-filter_complex", `[0:v]${vf}[v]`, "-map", "[v]");
if (existsSync(vo)) args.push("-map", "1:a", "-c:a", "aac", "-b:a", "192k");
args.push("-c:v", "libx264", "-pix_fmt", "yuv420p", "-crf", "18",
  "-t", (Math.max(reelDur, voDur) + 0.6).toFixed(2), out);
execFileSync("ffmpeg", args, { stdio: ["ignore", "ignore", "inherit"] });
console.log(`✓ concept-film.mp4 · ${dur(out).toFixed(1)}s`);
