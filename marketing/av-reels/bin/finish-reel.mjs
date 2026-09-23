#!/usr/bin/env node
// finish-reel.mjs — the "reel finish": frame a capture with a uniform
// border and fire a glaze over the whole frame. One ffmpeg pass, so the
// capture's audio rides through untouched and sync is exact by construction.
//
//   node marketing/av-reels/bin/finish-reel.mjs base.mp4 --glaze vhs --out reel.mp4
//
// Frame: --box x,y,w,h is where the whole piece goes. The default is a
// tight, uniform 40 px border (1000×1840 at 40,40) — near the reel's own
// aspect, so the piece reads as the whole screen with a little air around
// it. Capture at the box's size (capture-av --width 1000 --height 1840) so
// the piece's pixels land 1:1; 1000×1840 divides evenly by density 5. The
// border is a dimmed, blurred copy of the piece itself.
//
// Glazes are reel-only GLSL (marketing/av-reels/glazes/<name>.glsl, mpv
// user-shader format) run on the GPU by ffmpeg's libplacebo filter. They are
// deliberately separate from the live site's glazes (lib/glazes/): these
// only ever touch the finished video.
//
// Sound finish (--sound): AC's synths leave the oscillators raw, which reads
// as harsh on a phone. Every preset trims the fizz above ~4.5 kHz, puts the
// sound in a small synthetic room (convolution with ~0.8 s of decaying,
// slightly decorrelated pink noise) and lands it at -14 LUFS / -1.5 dBTP,
// Instagram's own playback target. `vhs` also runs it through the linear
// audio track: 80 Hz–10 kHz, wow + flutter, soft tape saturation, hiss.
//
// Flags: --out PATH  --glaze vhs|crt|none  --sound vhs|room|none  --box x,y,w,h  --fps 60

import { existsSync, rmSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { h264Args } from "../../../pop/lib/video-codec.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const GLAZES = resolve(HERE, "../glazes");
const W = 1080, H = 1920;
export const BOX = { x: 40, y: 40, w: 1000, h: 1840 };

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
const SOUND = flags.sound || "vhs";
if (!["vhs", "room", "none"].includes(SOUND)) { console.error(`✗ --sound takes vhs, room or none`); process.exit(1); }
const [bx, by, bw, bh] = flags.box ? String(flags.box).split(",").map(Number)
  : [BOX.x, BOX.y, BOX.w, BOX.h];

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

// The room: two pink-noise tails (one per ear, different seeds) with an
// exponential fade, low-passed like soft walls. Generated in-graph.
const ROOM = (seed) => `anoisesrc=d=0.8:c=pink:r=48000:a=0.5:seed=${seed},` +
  `afade=t=out:st=0:d=0.8:curve=exp,lowpass=f=5500`;
const TAPE = "highpass=f=80,lowpass=f=10000,vibrato=f=0.55:d=0.05,vibrato=f=9:d=0.012," +
  "asoftclip=type=tanh:threshold=0.8";
const audio = SOUND === "none" ? [] : [
  `[0:a]aformat=sample_rates=48000:channel_layouts=stereo,highpass=f=40,` +
    `treble=g=-4:f=4500${SOUND === "vhs" ? "," + TAPE : ""},asplit[dry][send]`,
  `${ROOM(11)}[irl]`, `${ROOM(29)}[irr]`,
  `[irl][irr]join=inputs=2:channel_layout=stereo[ir]`,
  `[send][ir]afir[wet]`,
  `[dry][wet]amix=inputs=2:weights=1 0.28:normalize=0` +
    (SOUND === "vhs" ? `[mix]` : `,loudnorm=I=-14:TP=-1.5:LRA=11,aresample=48000[a]`),
  ...(SOUND === "vhs" ? [
    "anoisesrc=c=pink:r=48000:a=0.0035:seed=7,aformat=channel_layouts=stereo,lowpass=f=9000[hiss]",
    "[mix][hiss]amix=inputs=2:normalize=0:duration=first," +
      "loudnorm=I=-14:TP=-1.5:LRA=11,aresample=48000[a]"] : []),
];

console.log(`▸ finish-reel · glaze ${GLAZE} · sound ${SOUND} · box ${bx},${by} ${bw}×${bh} → ${OUT}`);
const t0 = Date.now();
// Sound is finished in its own pass to a float WAV: run in the same graph as
// the video, the tape chain fed the AAC encoder NaNs (fine standalone).
const WAV = OUT.replace(/\.mp4$/, ".sound.wav");
if (audio.length) {
  const sf = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", BASE,
    "-filter_complex", audio.join(";"), "-map", "[a]", "-c:a", "pcm_f32le", WAV],
    { stdio: ["ignore", "inherit", "inherit"] });
  if (sf.status !== 0) { console.error("✗ finish-reel sound pass failed"); process.exit(1); }
}
const ff = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", BASE,
  ...(audio.length ? ["-i", WAV] : []),
  "-filter_complex", graph, "-map", "[v]",
  ...(audio.length ? ["-map", "1:a", "-shortest"] : ["-map", "0:a?"]),
  ...h264Args({ crf: 18, preset: "medium" }), "-r", String(FPS),
  "-c:a", "aac", "-b:a", "192k", "-movflags", "+faststart", OUT],
  { stdio: ["ignore", "inherit", "inherit"] });
rmSync(WAV, { force: true });
if (ff.status !== 0) { console.error("✗ finish-reel ffmpeg failed"); process.exit(1); }
console.log(`✓ ${OUT} (${((Date.now() - t0) / 1000).toFixed(1)}s)`);
