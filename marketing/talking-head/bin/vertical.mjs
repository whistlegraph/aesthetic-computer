// vertical.mjs — face-tracked 9:16 crop of any clip, audio kept.
//
// The song video comes out 16:9 (it's warped original footage). This turns it
// vertical the same way reel.mjs does: Vision finds the face every frame,
// reframe.mjs smooths that into a calm pan, and we crop a 1080×1920 window that
// follows him. The audio (the whole song) rides along untouched.
//
//   node vertical.mjs <video> --out reel.mp4 [--zoom 1.0]
//
// No chrome here — this is just the reframe. Captions/stamps/timer come from the
// full compose step.

import { spawn, spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { basename, resolve } from "node:path";
import { createCanvas, createImageData } from "canvas";
import { faceTrack, buildReframer } from "../../lib/reframe.mjs";

const W = 1080;
const H = 1920;
const FPS = 30;

const argv = process.argv.slice(2);
const flag = (n, d = null) => {
  const i = argv.indexOf(`--${n}`);
  return i >= 0 && argv[i + 1] && !argv[i + 1].startsWith("--") ? argv[i + 1] : d;
};
const SRC = resolve(argv.find((a) => !a.startsWith("--")));
const OUT = resolve(flag("out", "vertical.mp4"));
const ZOOM = parseFloat(flag("zoom", "1.0"));
if (!existsSync(SRC)) { console.error(`✗ not found: ${SRC}`); process.exit(1); }

const sh = (c, a) => spawnSync(c, a, { encoding: "utf8" }).stdout;
const DUR = parseFloat(sh("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", SRC]).trim());

console.log("· tracking face (Vision)");
const track = faceTrack(SRC, { fps: FPS, cachePath: `${OUT}.track.json` });
const at = buildReframer(track, { w: W, h: H, fps: FPS, zoom: ZOOM });
console.log(`  ${track.samples.length} samples, ${((track.samples.length / Math.round(DUR * FPS)) * 100).toFixed(0)}% detection`);

const SRC_W = track.srcW;
const SRC_H = track.srcH;
const FB = SRC_W * SRC_H * 4;
const FRAMES = Math.round(DUR * FPS);

const dec = spawn("ffmpeg", ["-v", "error", "-i", SRC, "-f", "rawvideo", "-pix_fmt", "rgba",
  "-vf", `fps=${FPS}`, "-"], { stdio: ["ignore", "pipe", "inherit"] });
const enc = spawn("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
  "-i", SRC, "-map", "0:v", "-map", "1:a",
  "-c:v", "libx264", "-preset", "veryfast", "-crf", "18", "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "192k", "-shortest", "-movflags", "+faststart", OUT],
  { stdio: ["pipe", "inherit", "inherit"] });

const srcCv = createCanvas(SRC_W, SRC_H);
const sctx = srcCv.getContext("2d");
const dst = createCanvas(W, H);
const ctx = dst.getContext("2d");
ctx.imageSmoothingEnabled = true;
ctx.imageSmoothingQuality = "high";

const write = (b) => new Promise((r) => (enc.stdin.write(b) ? r() : enc.stdin.once("drain", r)));
const it = dec.stdout[Symbol.asyncIterator]();
let buf = Buffer.alloc(0);
let done = false;
const nextFrame = async () => {
  while (buf.length < FB && !done) {
    const { value, done: d } = await it.next();
    if (d) { done = true; break; }
    buf = buf.length ? Buffer.concat([buf, value]) : value;
  }
  if (buf.length < FB) return null;
  const f = buf.subarray(0, FB);
  buf = buf.subarray(FB);
  return f;
};

console.log(`· cropping ${FRAMES} frames → ${basename(OUT)}`);
for (let f = 0; f < FRAMES; f += 1) {
  const rgba = await nextFrame();
  if (!rgba) break;
  sctx.putImageData(createImageData(new Uint8ClampedArray(rgba), SRC_W, SRC_H), 0, 0);
  const { sx, sy, sw, sh } = at(f / FPS);
  ctx.drawImage(srcCv, sx, sy, sw, sh, 0, 0, W, H);
  await write(dst.toBuffer("raw"));
  if (f % 150 === 0) process.stdout.write(`\r  ${f}/${FRAMES}`);
}
enc.stdin.end();
dec.kill("SIGKILL");
await new Promise((r) => enc.on("close", r));
process.stdout.write(`\r  ${FRAMES}/${FRAMES}\n✓ ${OUT}\n`);
