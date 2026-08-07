// float-community.mjs — let public AC moods and chats rise through a finished reel.
//
// This is a post-process so the expensive talking-head song, lip sync, karaoke,
// and side chrome stay untouched. A reviewed public snapshot is supplied as JSON;
// the render never fetches live user text by itself.
//
//   node marketing/talking-head/bin/float-community.mjs \
//     marketing/talking-head/out/yc-2018-song-reel.mp4 \
//     --data marketing/talking-head/community-2026-08-05.json \
//     --out marketing/talking-head/out/yc-2018-community-reel.mp4
//
//   --start 39.8   first community line in source time
//   --from 38      begin an excerpt at source time (review renders)
//   --seconds 24   excerpt duration

import { spawn, spawnSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { basename, resolve } from "node:path";
import { createCanvas, createImageData, registerFont } from "canvas";

const W = 1080;
const H = 1920;
const FPS = 30;
const argv = process.argv.slice(2);
const flag = (name, fallback = null) => {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] && !argv[i + 1].startsWith("--")
    ? argv[i + 1]
    : fallback;
};

const SRC = resolve(argv.find((arg) => !arg.startsWith("--")) || "");
const DATA = resolve(flag("data", "marketing/talking-head/community-2026-08-05.json"));
const OUT = resolve(flag("out", "marketing/talking-head/out/community-reel.mp4"));
const START = Number(flag("start", "39.8"));
const FROM = Math.max(0, Number(flag("from", "0")));

if (!existsSync(SRC) || !existsSync(DATA)) {
  console.error("usage: float-community.mjs <reel.mp4> --data snapshot.json [--out reel.mp4]");
  process.exit(2);
}

const probe = spawnSync("ffprobe", [
  "-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", SRC,
], { encoding: "utf8" });
if (probe.status !== 0) throw new Error(probe.stderr || "ffprobe failed");
const sourceDuration = Number(probe.stdout.trim());
const requestedSeconds = flag("seconds") ? Number(flag("seconds")) : sourceDuration - FROM;
const DURATION = Math.max(0, Math.min(requestedSeconds, sourceDuration - FROM));
const FRAMES = Math.round(DURATION * FPS);
const items = JSON.parse(readFileSync(DATA, "utf8")).items || [];

const fontPath = [
  "/System/Library/Fonts/Supplemental/Arial Bold.ttf",
  "/System/Library/Fonts/HelveticaNeue.ttc",
].find(existsSync);
if (fontPath) registerFont(fontPath, { family: "Community", weight: "bold" });
const FONT = fontPath ? "Community" : "Helvetica";

const dec = spawn("ffmpeg", [
  "-v", "error", "-ss", String(FROM), "-i", SRC,
  "-t", String(DURATION), "-f", "rawvideo", "-pix_fmt", "rgba", "-vf", `fps=${FPS}`, "-",
], { stdio: ["ignore", "pipe", "inherit"] });

const enc = spawn("ffmpeg", [
  "-hide_banner", "-loglevel", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
  "-ss", String(FROM), "-t", String(DURATION), "-i", SRC,
  "-map", "0:v", "-map", "1:a:0",
  "-c:v", "libx264", "-preset", "medium", "-crf", "16", "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "192k", "-shortest", "-movflags", "+faststart", OUT,
], { stdio: ["pipe", "inherit", "inherit"] });

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
ctx.imageSmoothingEnabled = true;
ctx.imageSmoothingQuality = "high";
const FRAME_BYTES = W * H * 4;

const wrap = (text, maxWidth) => {
  ctx.font = `bold 44px ${FONT}`;
  const words = String(text).replace(/\s+/g, " ").trim().split(" ");
  const lines = [];
  let line = "";
  for (const word of words) {
    const next = line ? `${line} ${word}` : word;
    if (line && ctx.measureText(next).width > maxWidth) {
      lines.push(line);
      line = word;
    } else {
      line = next;
    }
  }
  if (line) lines.push(line);
  return lines.slice(0, 3);
};

const entries = items.map((item, index) => ({
  ...item,
  at: START + index * 1.8,
  duration: 9,
  lane: index % 2,
  phase: index * 1.73,
  lines: wrap(item.text, 360),
}));

const smoothstep = (x) => {
  const v = Math.max(0, Math.min(1, x));
  return v * v * (3 - 2 * v);
};

const overlay = createCanvas(W, H);
const overlayCtx = overlay.getContext("2d");

const drawText = (text, x, y, size, color, alpha) => {
  overlayCtx.font = `bold ${size}px ${FONT}`;
  overlayCtx.lineJoin = "round";
  overlayCtx.lineWidth = size >= 40 ? 7 : 5;
  overlayCtx.strokeStyle = `rgba(0,0,0,${0.78 * alpha})`;
  overlayCtx.fillStyle = color;
  overlayCtx.globalAlpha = alpha;
  overlayCtx.strokeText(text, x, y);
  overlayCtx.fillText(text, x, y);
  overlayCtx.globalAlpha = 1;
};

const drawCommunity = (sourceTime) => {
  overlayCtx.clearRect(0, 0, W, H);
  for (const entry of entries) {
    const life = (sourceTime - entry.at) / entry.duration;
    if (life < 0 || life > 1) continue;

    const blockHeight = 38 + entry.lines.length * 52;
    const y = H + blockHeight - life * (H + blockHeight * 2);
    const x = (entry.lane === 0 ? 148 : 578) + Math.sin(life * Math.PI * 2 + entry.phase) * 16;
    const edgeFade = Math.min(smoothstep(life / 0.09), smoothstep((1 - life) / 0.13));
    const centerY = y - blockHeight / 2;
    const alpha = 0.62 * edgeFade;
    const textColor = entry.kind === "mood" ? "rgb(72,255,176)" : "rgb(255,245,202)";
    const mark = entry.kind === "mood" ? "○" : ">";

    drawText(`${mark} ${entry.from}`, x, y, 28, "rgb(255,74,196)", alpha * 0.9);
    entry.lines.forEach((line, lineIndex) => {
      drawText(line, x, y + 52 + lineIndex * 52, 44, textColor, alpha);
    });
  }

  // The messages are atmosphere, not foreground labels. Punch the stable face
  // crop out of the overlay with a feathered ellipse so lines travel behind the
  // speaker instead of tattooing his eyes and mouth.
  overlayCtx.save();
  overlayCtx.globalCompositeOperation = "destination-out";
  overlayCtx.translate(W / 2, 585);
  overlayCtx.scale(1, 1.42);
  const faceMask = overlayCtx.createRadialGradient(0, 0, 285, 0, 0, 460);
  faceMask.addColorStop(0, "rgba(0,0,0,1)");
  faceMask.addColorStop(0.72, "rgba(0,0,0,0.96)");
  faceMask.addColorStop(1, "rgba(0,0,0,0)");
  overlayCtx.fillStyle = faceMask;
  overlayCtx.beginPath();
  overlayCtx.arc(0, 0, 460, 0, Math.PI * 2);
  overlayCtx.fill();
  overlayCtx.restore();

  // Let community lines travel continuously, but occlude them behind the baked
  // karaoke band. Feathering the edges makes the passage read as depth rather
  // than a line abruptly blinking off.
  overlayCtx.save();
  overlayCtx.globalCompositeOperation = "destination-out";
  const captionMask = overlayCtx.createLinearGradient(0, 680, 0, 1120);
  captionMask.addColorStop(0, "rgba(0,0,0,0)");
  captionMask.addColorStop(0.25, "rgba(0,0,0,1)");
  captionMask.addColorStop(0.75, "rgba(0,0,0,1)");
  captionMask.addColorStop(1, "rgba(0,0,0,0)");
  overlayCtx.fillStyle = captionMask;
  overlayCtx.fillRect(0, 680, W, 440);
  overlayCtx.restore();

  ctx.drawImage(overlay, 0, 0);
};

const write = (buffer) => new Promise((resolveWrite) => {
  if (enc.stdin.write(buffer)) resolveWrite();
  else enc.stdin.once("drain", resolveWrite);
});

const it = dec.stdout[Symbol.asyncIterator]();
let buffered = Buffer.alloc(0);
let ended = false;
const nextFrame = async () => {
  while (buffered.length < FRAME_BYTES && !ended) {
    const { value, done } = await it.next();
    if (done) { ended = true; break; }
    buffered = buffered.length ? Buffer.concat([buffered, value]) : value;
  }
  if (buffered.length < FRAME_BYTES) return null;
  const frame = buffered.subarray(0, FRAME_BYTES);
  buffered = buffered.subarray(FRAME_BYTES);
  return frame;
};

console.log(`· ${items.length} public community lines · ${START.toFixed(1)}s start`);
console.log(`· rendering ${FROM.toFixed(1)}–${(FROM + DURATION).toFixed(1)}s → ${basename(OUT)}`);
let rendered = 0;
for (let frame = 0; frame < FRAMES; frame += 1) {
  const rgba = await nextFrame();
  if (!rgba) break;
  ctx.putImageData(createImageData(new Uint8ClampedArray(rgba), W, H), 0, 0);
  drawCommunity(FROM + frame / FPS);
  await write(canvas.toBuffer("raw"));
  rendered += 1;
  if (frame % 150 === 0) process.stdout.write(`\r  ${frame}/${FRAMES}`);
}

enc.stdin.end();
await new Promise((resolveClose) => enc.on("close", resolveClose));
process.stdout.write(`\r  ${rendered}/${FRAMES}\n`);
console.log(`✓ ${OUT}`);
