#!/usr/bin/env node
// oskiewar's Steam trailer — the reel factory, turned sideways.
//
//   node xbox/steam/store-page/render-trailer.mjs            # all clips, then encode
//   node xbox/steam/store-page/render-trailer.mjs --clips=1  # one clip, to eyeball
//   node xbox/steam/store-page/render-trailer.mjs --encode   # re-encode what's rendered
//
// Nothing new records the game. `bakeReplay` is the same Replay Oven the
// Instagram reels go through three times a day — deterministic re-sim off the
// demo, one source image per frame, the game's own synthesized audio muxed
// against the demo's tick clock. The only difference here is the viewport:
// 1920x1080 instead of 1080x1920, and the game lays itself out for whatever
// shape it is handed, so this is the real thing at the real aspect rather
// than a 9:16 reel cropped into a letterbox.
//
// Clips are whole rounds. Valve's screenshot rule ("what your game is
// actually like to play") applies to the trailer too, and cutting *between*
// rounds keeps the factory's own discipline: nothing inside a round is cut.
//
// Output: assets/trailer.mp4 at Valve's reference preset (H.264 high, 20
// Mbps, AAC 192k at 48 kHz, +faststart) and assets/trailer-thumbnail.jpg,
// which must be a frame from the video itself.

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, rmSync } from "node:fs";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { bakeReplay } from "../../live/marketing/replay-oven.mjs";

const here = resolve(fileURLToPath(new URL(".", import.meta.url)));
const output = join(here, "assets");
const work = join(here, "assets", "shots", "trailer");
const flags = new Map(process.argv.slice(2).map((entry) => {
  const [key, value = "true"] = entry.replace(/^--/, "").split("=");
  return [key, value];
}));

// Seeds are the whole recording: the same string re-renders the same fight,
// so a trailer that needs one clip swapped does not re-roll the others.
const CLIPS = [
  { id: "trailer-1", door: "fight", seed: "steam-trailer#1" },
  { id: "trailer-2", door: "fight", seed: "steam-trailer#4" },
  { id: "trailer-3", door: "survival", seed: "steam-trailer#7" },
  { id: "trailer-4", door: "fight", seed: "steam-trailer#9" },
].slice(0, Number(flags.get("clips") || 4));

const log = (line) => console.log(line);
mkdirSync(work, { recursive: true });

const clips = [];
if (!flags.has("encode")) {
  for (const clip of CLIPS) {
    const out = join(work, clip.id);
    rmSync(out, { recursive: true, force: true });
    log(`\n🎬 ${clip.id} · ${clip.door} · seed "${clip.seed}"`);
    const render = await bakeReplay({
      id: clip.id, kind: "self-play", seed: clip.seed, door: clip.door,
      // The reel dress, not the full oven UI: a matchup card names both
      // fighters, the round itself plays under nothing at all, and the
      // winner is called afterwards. The full HUD drags the oven's
      // intro/fight/outro scrubber along with it, which on a store page
      // reads as a video control rather than part of the game.
      hud: "reel",
      cap: 45, width: 1920, height: 1080, theme: "dark", out,
    }, { log });
    clips.push(render.base);
    log(`   → ${render.base} (${render.frames} frames)`);
  }
} else {
  for (const clip of CLIPS) {
    const base = join(work, clip.id, "base.mp4");
    if (existsSync(base)) clips.push(base);
  }
}
if (!clips.length) throw new Error("no clips rendered — drop --encode or check the log");

// One encode, not two: the clips are concatenated in the filter graph and
// written straight to Valve's preset, so nothing is transcoded twice.
const trailer = join(output, "trailer.mp4");
const inputs = clips.flatMap((file) => ["-i", file]);
const graph = clips.map((_, index) => `[${index}:v][${index}:a]`).join("") +
  `concat=n=${clips.length}:v=1:a=1[v][a]`;
const encoded = spawnSync("ffmpeg", ["-y", ...inputs,
  "-filter_complex", graph, "-map", "[v]", "-map", "[a]",
  "-c:v", "libx264", "-preset", "slow", "-b:v", "20M", "-maxrate", "20M",
  "-bufsize", "40M", "-profile:v", "high", "-level", "4.2",
  "-pix_fmt", "yuv420p", "-r", "60",
  "-c:a", "aac", "-b:a", "192k", "-ar", "48000", "-ac", "2",
  "-movflags", "+faststart", trailer], { encoding: "utf8" });
if (encoded.status !== 0) throw new Error(encoded.stderr.slice(-1600));

// Valve requires the thumbnail to be a frame of the video. Take it a third
// of the way in, where a round is mid-fight rather than mid-countdown.
const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries",
  "format=duration", "-of", "default=nw=1:nk=1", trailer], { encoding: "utf8" });
const seconds = Number(String(probe.stdout).trim()) || 0;
const thumb = spawnSync("ffmpeg", ["-y", "-ss", (seconds / 3).toFixed(2),
  "-i", trailer, "-frames:v", "1", "-q:v", "2",
  join(output, "trailer-thumbnail.jpg")], { encoding: "utf8" });
if (thumb.status !== 0) throw new Error(thumb.stderr.slice(-800));
console.log(`\n🎞  ${trailer} · ${clips.length} rounds · ${seconds.toFixed(1)}s`);
console.log(`🖼  ${join(output, "trailer-thumbnail.jpg")} (frame at ${(seconds / 3).toFixed(1)}s)`);
