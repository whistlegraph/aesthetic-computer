#!/usr/bin/env node
// finish-reel.mjs — the "reel finish": compose a capture into Instagram's
// safe zone and fire a glaze over the whole frame. One ffmpeg pass, so the
// capture's audio rides through untouched and sync is exact by construction.
//
//   node marketing/av-reels/bin/finish-reel.mjs base.mp4 --glaze vhs --out reel.mp4
//
// Safe zone: Instagram lays its header over the top ~12% of a reel and the
// caption, audio line and action buttons over the bottom ~25% (plus a column
// of icons on the right). --box x,y,w,h is where the whole piece goes; the
// default is a centered 4:5 window (960×1200 at 60,240) so nothing is under
// the UI. Capture at the box's size (capture-av --width 960 --height 1200)
// so the piece's pixels land 1:1. The margins are a dimmed, blurred copy of
// the piece itself.
//
// Glazes are reel-only GLSL (marketing/av-reels/glazes/<name>.glsl, mpv
// user-shader format) run on the GPU by ffmpeg's libplacebo filter. They are
// deliberately separate from the live site's glazes (lib/glazes/): these
// only ever touch the finished video.
//
// Flags: --out PATH  --glaze vhs|crt|none  --box x,y,w,h  --fps 60

import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { h264Args } from "../../../pop/lib/video-codec.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const GLAZES = resolve(HERE, "../glazes");
const W = 1080, H = 1920;
export const SAFE_BOX = { x: 60, y: 240, w: 960, h: 1200 };

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
  console.error("usage: finish-reel.mjs base.mp4 [--glaze vhs|crt|none] [--box x,y,w,h] [--out reel.mp4]");
  process.exit(1);
}
const OUT = resolve(flags.out || BASE.replace(/\.mp4$/, "-finished.mp4"));
const FPS = parseInt(flags.fps || 60, 10);
const GLAZE = flags.glaze || "vhs";
const [bx, by, bw, bh] = flags.box ? String(flags.box).split(",").map(Number)
  : [SAFE_BOX.x, SAFE_BOX.y, SAFE_BOX.w, SAFE_BOX.h];

let shader = null;
if (GLAZE !== "none") {
  shader = resolve(GLAZES, `${GLAZE}.glsl`);
  if (!existsSync(shader)) { console.error(`✗ no glaze ${GLAZE} in ${GLAZES}`); process.exit(1); }
}

// neighbor scaling keeps AC's pixels square if the capture isn't box-sized.
const graph = [
  `[0:v]fps=${FPS},split[a][b]`,
  `[b]scale=${W}:${H}:force_original_aspect_ratio=increase,crop=${W}:${H},` +
    `gblur=sigma=48,eq=brightness=-0.18:saturation=0.85[bg]`,
  `[a]scale=${bw}:${bh}:flags=neighbor[fg]`,
  `[bg][fg]overlay=${bx}:${by}${shader ? "[comp]" : ",format=yuv420p[v]"}`,
  ...(shader ? [`[comp]libplacebo=w=${W}:h=${H}:custom_shader_path=${shader},format=yuv420p[v]`] : []),
].join(";");

console.log(`▸ finish-reel · glaze ${GLAZE} · box ${bx},${by} ${bw}×${bh} → ${OUT}`);
const t0 = Date.now();
const ff = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", BASE,
  "-filter_complex", graph, "-map", "[v]", "-map", "0:a?",
  ...h264Args({ crf: 18, preset: "medium" }), "-r", String(FPS),
  "-c:a", "aac", "-b:a", "192k", "-movflags", "+faststart", OUT],
  { stdio: ["ignore", "inherit", "inherit"] });
if (ff.status !== 0) { console.error("✗ finish-reel ffmpeg failed"); process.exit(1); }
console.log(`✓ ${OUT} (${((Date.now() - t0) / 1000).toFixed(1)}s)`);
