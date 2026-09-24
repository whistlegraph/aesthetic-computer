#!/usr/bin/env node
// scene-reel.mjs — put a capture on a phone in a room: a diegetic reel shot
// rendered headless by Blender (no GUI), then muxed with finished audio.
//
//   node marketing/av-reels/bin/scene-reel.mjs base.mp4 --audio sound.mp4 --out reel.mp4
//   node marketing/av-reels/bin/scene-reel.mjs base.mp4 --still 120      # one frame to check
//
// 1. Poly Haven CC0 assets (room HDRI + table) are fetched by API and cached.
// 2. The capture is split to a constant-fps PNG sequence (frame-exact).
// 3. Blender runs scene/phone-scene.py in background mode: EEVEE by default,
//    --engine cycles for a path-traced final (OptiX on the GPU).
// 4. ffmpeg joins the rendered frames with the audio of --audio (e.g. a
//    finish-reel --sound room render), or the capture's own audio.
//
// Flags: --out PATH --audio MP4 --engine eevee|cycles --seconds N --still F
//        --fps 60 --hdri ID --table ID --blender PATH

import { existsSync, mkdirSync, readdirSync, rmSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { h264Args } from "../../../pop/lib/video-codec.mjs";
import { fetchHdri, fetchModel } from "../scene/fetch-polyhaven.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const SCENE = resolve(HERE, "../scene/phone-scene.py");
const CACHE = join(homedir(), ".cache", "ac-reel-assets");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
    else flags[a.slice(2)] = true;
  } else positional.push(a);
}

const BASE = positional[0] && resolve(positional[0]);
if (!BASE || !existsSync(BASE)) {
  console.error("usage: scene-reel.mjs base.mp4 [--audio sound.mp4] [--out reel.mp4] [--engine eevee|cycles] [--still F]");
  process.exit(1);
}
const OUT = resolve(flags.out || BASE.replace(/\.mp4$/, "-scene.mp4"));
const FPS = parseInt(flags.fps || 60, 10);
const ENGINE = flags.engine || "eevee";
const BLENDER = flags.blender || join(homedir(), ".local/bin/blender");
const WORK = OUT.replace(/\.mp4$/, ".scene-work");

function run(cmd, args) {
  const r = spawnSync(cmd, args, { stdio: ["ignore", "pipe", "inherit"], encoding: "utf8", maxBuffer: 1 << 26 });
  if (r.status !== 0) { console.error(r.stdout?.slice(-4000)); throw new Error(`${cmd} failed (${r.status})`); }
  return r.stdout || "";
}

const probe = JSON.parse(run("ffprobe", ["-v", "error", "-select_streams", "v:0",
  "-show_entries", "stream=width,height:format=duration", "-of", "json", BASE]));
const { width, height } = probe.streams[0];
const seconds = Math.min(Number(flags.seconds || Infinity), Number(probe.format.duration));
const count = Math.round(seconds * FPS);

console.log(`▸ scene-reel · ${ENGINE} · ${count} frames · ${width}×${height} capture → ${OUT}`);
const hdri = await fetchHdri(flags.hdri || "lythwood_room", "2k", CACHE);
const table = await fetchModel(flags.table || "WoodenTable_01", "2k", CACHE);

const frames = join(WORK, "screen");
if (!existsSync(join(frames, `${String(count).padStart(5, "0")}.png`))) {
  rmSync(frames, { recursive: true, force: true }); mkdirSync(frames, { recursive: true });
  run("ffmpeg", ["-hide_banner", "-loglevel", "error", "-i", BASE, "-vf", `fps=${FPS}`,
    "-frames:v", String(count), "-start_number", "1", join(frames, "%05d.png")]);
}

const rendered = join(WORK, "render");
const t0 = Date.now();
const log = run(BLENDER, ["-b", "--gpu-backend", "vulkan", "-P", SCENE, "--",
  "--frames", frames, "--count", String(count), "--out", rendered,
  "--hdri", hdri, "--table", table, "--engine", ENGINE, "--fps", String(FPS),
  "--aspect", String(width / height), ...(flags.still ? ["--still", String(flags.still)] : [])]);
const done = log.split("\n").find((l) => l.startsWith("SCENE_DONE"));
console.log(`  blender ${done || "(no summary)"} · wall ${((Date.now() - t0) / 1000).toFixed(1)}s`);

if (flags.still) {
  const still = join(rendered, `still-${String(flags.still).padStart(5, "0")}.png`);
  console.log(`✓ still → ${still}`);
  process.exit(0);
}

const got = readdirSync(rendered).filter((f) => /^\d{5}\.png$/.test(f)).length;
if (got !== count) throw new Error(`rendered ${got}/${count} frames`);
const audio = flags.audio ? resolve(flags.audio) : BASE;
run("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-framerate", String(FPS), "-i", join(rendered, "%05d.png"), "-i", audio,
  "-map", "0:v", "-map", "1:a", "-shortest",
  ...h264Args({ crf: 18, preset: "medium" }), "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "192k", "-movflags", "+faststart", OUT]);
console.log(`✓ ${OUT}`);
