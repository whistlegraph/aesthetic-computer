#!/usr/bin/env node
// amazing-grace-canvas.mjs — Spotify Canvas for `amazing grace`.
//
// A vibe over the album artwork, in the lineage of
// `pop/wattajetta/canvas/render-canvas.mjs` (photo only, 2D treatment,
// every motion an integer number of cycles per loop so the wrap is
// invisible) and `pop/bin/photo-canvas.mjs` (the ffmpeg pipe). Where the
// wattajetta one pumps a slitscan on an 18-beat grid, this one is a hymn
// at 70: the frame breathes once, drifts a small circle, rocks a hair
// once per bar of three, the evening sun on the two faces swells and
// settles, and golden motes rise through the tent light.
//
// The 9:16 window is cut from the original photograph (IMG_5775, 3024×4032)
// rather than upscaled from the 1254² cover crop, framed so the pair sit
// where the cover puts them, with the copper mugs at the foot of the frame.
//
// Loop: 3 bars of 3/4 at 70 BPM = 9 beats = 7.714 s (Canvas wants 3–8 s).
//
//   node pop/big-pictures/bin/amazing-grace-canvas.mjs [--out PATH]
//     [--photo PATH] [--still SECONDS]   (dump one frame as PNG to --out)
//
// `canvas` (node-canvas) is a root devDependency; on a machine without a
// root node_modules point CANVAS_NODE_MODULES at any dir that has one.

import { spawn } from "node:child_process";
import { existsSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { resolve } from "node:path";
import { createRequire } from "node:module";

const { createCanvas, loadImage } = await import("canvas").catch(() => {
  const from = process.env.CANVAS_NODE_MODULES;
  if (!from) {
    console.error("✗ `canvas` not resolvable — npm install at the root, or set CANVAS_NODE_MODULES");
    process.exit(1);
  }
  return createRequire(resolve(from, "resolve-from-here.js"))("canvas");
});

const flags = {};
{
  const a = process.argv.slice(2);
  for (let i = 0; i < a.length; i++) {
    if (!a[i].startsWith("--")) continue;
    const k = a[i].slice(2);
    flags[k] = i + 1 < a.length && !a[i + 1].startsWith("--") ? a[++i] : true;
  }
}
const expand = (p) =>
  typeof p !== "string" ? p : p.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p;

const PHOTO = expand(flags.photo || "~/Documents/Shelf/amazing-grace-REVIEW-2026-09-23/IMG_5775.jpg");
const OUT = expand(flags.out || "~/Documents/Shelf/amazing-grace-DISTROKID/amazing-grace-canvas.mp4");
const STILL = flags.still !== undefined ? Number(flags.still) : null;
if (!existsSync(PHOTO)) { console.error(`✗ photo missing: ${PHOTO}`); process.exit(1); }

const W = 1080, H = 1920, FPS = 30;
const BPM = 70, BEATS = 9;                 // three bars of three
const DUR = BEATS * 60 / BPM;              // 7.714 s
const FRAMES = Math.round(DUR * FPS);      // 231
const TAU = Math.PI * 2;

// ── the window into the photograph (node-canvas honours the EXIF
// rotation, so the source is 3024×4032 portrait). The cover is the 1:1
// square (841..2095, 1035..2289): mother's face ≈ (1240, 1640), Jeffrey's
// ≈ (1585, 1355). The window keeps the cover's centre line, opens a little
// tent above the heads and runs down to the copper mugs (≈ y 3140), so
// both faces sit in the upper third as they do on the cover.
const WIN_W = 1300;
const WIN_H = WIN_W * 16 / 9;              // 2311
const CX = 1468;                           // the cover's centre line
const CY = 900 + WIN_H / 2;                // top at y 900
const ZOOM = 0.03;                         // breath amplitude
const DRIFT = 26;                          // px of source, the small circle
const ROCK = 0.35;                         // degrees, once per bar

const img = await loadImage(PHOTO);
const SW = img.width, SH = img.height;

// ── sun bloom: bright-pass of the photo, kept small so redrawing it big
// is the blur. Warm pixels bloom sooner than cool ones (the tent is blue
// and should stay put; the skin and the white house should glow).
const GLOW = 360;
const glowC = createCanvas(GLOW, GLOW);
{
  const g = glowC.getContext("2d");
  g.imageSmoothingQuality = "high";
  g.drawImage(img, CX - WIN_W / 2, CY - WIN_H / 2, WIN_W, WIN_H, 0, 0, GLOW, GLOW);
  const d = g.getImageData(0, 0, GLOW, GLOW);
  const px = d.data;
  for (let i = 0; i < px.length; i += 4) {
    const r = px[i], gg = px[i + 1], b = px[i + 2];
    const luma = 0.2126 * r + 0.7152 * gg + 0.0722 * b;
    const warmth = (r - b) / 255;          // −1 … 1
    let k = (luma - 150 + warmth * 60) / 90;
    k = k <= 0 ? 0 : k >= 1 ? 1 : k * k * (3 - 2 * k);
    px[i] = r * k; px[i + 1] = gg * k * 0.92; px[i + 2] = b * k * 0.7;
  }
  g.putImageData(d, 0, 0);
}

// ── motes: golden dust rising through the tent light. Each one climbs an
// integer number of frame-heights per loop and sways an integer number
// of times, so the field wraps exactly.
let seed = 20260923;
const rnd = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296);
const MOTES = [];
for (let i = 0; i < 46; i++) {
  const depth = rnd();                     // 0 near … 1 far
  MOTES.push({
    x: rnd(),
    y0: rnd(),
    rise: 1 + (rnd() < 0.3 ? 1 : 0),       // frame-heights per loop
    sway: 0.012 + 0.03 * rnd(),
    swayCycles: 1 + Math.floor(rnd() * 3),
    swayPhase: rnd(),
    r: 4 + 16 * (1 - depth) * rnd(),
    a: 0.14 + 0.42 * (1 - depth) * rnd(),
    twinkle: 2 + Math.floor(rnd() * 4),
    twPhase: rnd(),
  });
}

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
ctx.imageSmoothingEnabled = true;
ctx.imageSmoothingQuality = "high";

const vignette = ctx.createRadialGradient(W / 2, H * 0.42, H * 0.22, W / 2, H * 0.42, H * 0.78);
vignette.addColorStop(0, "rgba(0,0,0,0)");
vignette.addColorStop(1, "rgba(10,4,0,0.34)");

function drawFrame(f) {
  const p = f / FRAMES;                    // 0→1 across the loop
  const bar = (p * 3) % 1;                 // three bars a loop

  const breath = 0.5 - 0.5 * Math.cos(TAU * p);          // 0→1→0, once
  const scale = (H / WIN_H) * (1 + ZOOM * breath);
  const dx = DRIFT * Math.sin(TAU * p);
  const dy = DRIFT * 0.45 * Math.cos(TAU * p);
  const rock = ROCK * Math.sin(TAU * bar) * (Math.PI / 180);

  ctx.fillStyle = "#000";
  ctx.fillRect(0, 0, W, H);

  const view = () => {
    ctx.translate(W / 2, H / 2);
    ctx.rotate(rock);
    ctx.scale(scale, scale);
    ctx.translate(-(CX + dx), -(CY + dy));
  };

  ctx.save();
  view();
  ctx.drawImage(img, 0, 0, SW, SH);     // whole photo, so the rock never shows an edge
  ctx.restore();

  // warm grade: a breath of amber over everything, a touch lighter in the
  // exhale so the picture seems lit from within
  ctx.globalCompositeOperation = "soft-light";
  ctx.fillStyle = "rgba(255,170,90,0.20)";
  ctx.fillRect(0, 0, W, H);

  // sun swell: two slow cycles a loop, never fully off
  ctx.globalCompositeOperation = "lighter";
  const swell = 0.5 - 0.5 * Math.cos(TAU * 2 * p + 0.9);
  ctx.globalAlpha = 0.10 + 0.16 * swell;
  ctx.save();
  view();
  ctx.drawImage(glowC, 0, 0, GLOW, GLOW, CX - WIN_W / 2, CY - WIN_H / 2, WIN_W, WIN_H);
  ctx.restore();

  // light leak from the upper right, where the sun actually is, sliding once
  const lx = W * (0.92 + 0.06 * Math.sin(TAU * p));
  const ly = H * (0.08 + 0.04 * Math.cos(TAU * p));
  const leak = ctx.createRadialGradient(lx, ly, 0, lx, ly, H * 0.55);
  leak.addColorStop(0, "rgba(255,196,120,0.30)");
  leak.addColorStop(0.5, "rgba(255,150,70,0.09)");
  leak.addColorStop(1, "rgba(255,120,40,0)");
  ctx.globalAlpha = 0.75 + 0.25 * swell;
  ctx.fillStyle = leak;
  ctx.fillRect(0, 0, W, H);

  // motes
  for (const m of MOTES) {
    const y = ((m.y0 - p * m.rise) % 1 + 1) % 1;
    const x = m.x + m.sway * Math.sin(TAU * (m.swayCycles * p + m.swayPhase));
    const tw = 0.55 + 0.45 * Math.sin(TAU * (m.twinkle * p + m.twPhase));
    const px = x * W, py = y * H;
    const g = ctx.createRadialGradient(px, py, 0, px, py, m.r);
    g.addColorStop(0, `rgba(255,222,160,${(m.a * tw).toFixed(3)})`);
    g.addColorStop(0.5, `rgba(255,200,120,${(m.a * tw * 0.45).toFixed(3)})`);
    g.addColorStop(1, "rgba(255,180,90,0)");
    ctx.globalAlpha = 1;
    ctx.fillStyle = g;
    ctx.fillRect(px - m.r, py - m.r, m.r * 2, m.r * 2);
  }

  ctx.globalAlpha = 1;
  ctx.globalCompositeOperation = "source-over";
  ctx.fillStyle = vignette;
  ctx.fillRect(0, 0, W, H);
}

// fine film grain on the raw BGRA buffer, reseeded per frame
function grain(buf, frame) {
  let s = ((frame + 1) * 2654435761) >>> 0;
  for (let i = 0; i < buf.length; i += 4) {
    s ^= s << 13; s >>>= 0; s ^= s >>> 17; s ^= s << 5; s >>>= 0;
    const n = ((s & 15) - 7) * 0.22;
    for (let c = 0; c < 3; c++) {
      const v = buf[i + c] + n;
      buf[i + c] = v < 0 ? 0 : v > 255 ? 255 : v;
    }
  }
}

if (STILL !== null) {
  const f = Math.round(STILL * FPS) % FRAMES;
  drawFrame(f);
  writeFileSync(OUT, canvas.toBuffer("image/png"));
  console.log(`✓ still frame ${f} (${(f / FPS).toFixed(2)}s) → ${OUT}`);
  process.exit(0);
}

console.log(`▸ amazing-grace canvas · ${W}x${H} · ${DUR.toFixed(3)}s (${BEATS} beats @ ${BPM}) · ${FRAMES}f`);

// Silent stereo AAC (QuickTime freezes on track-less short H.264), CFR,
// 1 s GOP, High@4.0 — the settings that have shipped on every Canvas here.
const ff = spawn("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS),
  "-i", "pipe:0",
  "-f", "lavfi", "-i", "anullsrc=r=44100:cl=stereo",
  "-map", "0:v:0", "-map", "1:a:0",
  "-c:v", "libx264", "-profile:v", "high", "-level", "4.0",
  "-pix_fmt", "yuv420p", "-preset", "slow", "-crf", "19",
  "-g", String(FPS), "-keyint_min", String(FPS), "-sc_threshold", "0",
  "-c:a", "aac", "-b:a", "96k",
  "-fps_mode", "cfr", "-r", String(FPS), "-frames:v", String(FRAMES), "-shortest",
  "-movflags", "+faststart",
  OUT,
], { stdio: ["pipe", "inherit", "inherit"] });
ff.on("error", (e) => { console.error("ffmpeg spawn error:", e); process.exit(1); });

const t0 = Date.now();
for (let f = 0; f < FRAMES; f++) {
  drawFrame(f);
  const buf = Buffer.from(canvas.toBuffer("raw"));
  grain(buf, f);
  if (!ff.stdin.write(buf)) await new Promise((r) => ff.stdin.once("drain", r));
  if (f % 30 === 0) process.stdout.write(`\r  frame ${f}/${FRAMES}`);
}
ff.stdin.end();
await new Promise((r) => ff.on("close", r));
console.log(`\n✓ canvas ${((Date.now() - t0) / 1000).toFixed(1)}s → ${OUT}`);
