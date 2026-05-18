#!/usr/bin/env node
// canvas-loop.mjs — Spotify **Canvas** output model.
//
// A Canvas is the short looping clip that plays behind a track on
// Spotify (Music → track → Canvas). Spec: 9:16, 3–8 s, SILENT, seamless
// loop, NO text/logos. This is its own pipeline output — a sibling to
// the tiktok / insta-story cuts — so it is deliberately CHROME-FREE by
// construction: this renderer only ever draws illustrations. There is
// no title, progress bar, timecode, pals watermark, karaoke, lane
// score, spinning string or rotating disc here at all.
//
// The vibe is a RAPID cycle through every section illustration (quick
// cross-dissolves, not a slow per-section pan), with a subtle shared
// breathing zoom for life. The timeline is cyclic and totalFrames maps
// exactly onto `dur`, so the last frame dissolves into the first → a
// perfect seamless loop with no reverse/concat (safe on 8 GB).
//
// Usage:
//   node canvas-loop.mjs --illustrations intro=/a.png,drop1=/b.png,... \
//     --size 1080x1920 --dur 6 --out ~/Desktop/track-canvas.mp4
//
//   --illustrations  sec=path,sec=path,…  (order = cycle order)
//   --size WxH       default 1080x1920
//   --dur SECONDS    default 6   (Spotify allows 3–8)
//   --fps N          default 30
//   --xfade SECONDS  cross-dissolve length (default: auto ≈ 0.22)
//   --out PATH       output .mp4

import { spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { resolve } from "node:path";
import { createCanvas, loadImage } from "canvas";

const flags = {};
{
  const a = process.argv.slice(2);
  for (let i = 0; i < a.length; i++) {
    if (a[i].startsWith("--")) {
      const k = a[i].slice(2);
      if (i + 1 < a.length && !a[i + 1].startsWith("--")) flags[k] = a[++i];
      else flags[k] = true;
    }
  }
}
const expand = (p) =>
  !p ? p : p.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p;

function parseSize(s) {
  const m = String(s || "1080x1920").match(/^(\d+)x(\d+)$/);
  return m ? [parseInt(m[1], 10), parseInt(m[2], 10)] : [1080, 1920];
}
const [W, H] = parseSize(flags.size);
const FPS = Number(flags.fps ?? 30);
const DUR = Math.max(3, Math.min(8, Number(flags.dur ?? 6)));
const OUT = expand(flags.out);
if (!OUT) { console.error("✗ --out required"); process.exit(1); }

const pairs = String(flags.illustrations || "")
  .split(",")
  .map((s) => s.trim())
  .filter(Boolean)
  .map((s) => {
    const eq = s.indexOf("=");
    return eq < 0 ? { sec: s, path: expand(s) } : { sec: s.slice(0, eq), path: expand(s.slice(eq + 1)) };
  })
  .filter((p) => existsSync(p.path));
if (pairs.length === 0) { console.error("✗ no valid --illustrations"); process.exit(1); }

console.log(`▸ canvas-loop · ${W}x${H} · ${DUR}s @${FPS} · ${pairs.length} illys`);

const imgs = [];
for (const p of pairs) imgs.push(await loadImage(p.path));
const N = imgs.length;

// Cover-fit (mirrors cover-video.mjs): scale to cover W×H, centre
// horizontally, bias slightly UP so faces (upper band) stay in frame.
function fit(img) {
  const s = Math.max(W / img.width, H / img.height);
  const w = img.width * s, h = img.height * s;
  return { w, h, x: (W - w) / 2, y: (H - h) * 0.40 };
}
const fits = imgs.map(fit);

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
ctx.imageSmoothingEnabled = true;
ctx.imageSmoothingQuality = "high";
// Offscreen snapshot of the OUTGOING illy for the slit shear.
const oldC = createCanvas(W, H);
const oldX = oldC.getContext("2d");
oldX.imageSmoothingEnabled = true;
oldX.imageSmoothingQuality = "high";

const totalFrames = Math.round(DUR * FPS);
const segDur = DUR / N;                                  // time per illy
// Slit-scan glitch transition window (longer than a dissolve so the
// shear is legible). 8 illys / 6 s → seg ≈ 0.75 s, XF ≈ 0.45 s.
const XF = Number(flags.xfade ?? Math.min(0.5, segDur * 0.6));
const smooth = (k) => { k = Math.max(0, Math.min(1, k)); return k * k * (3 - 2 * k); };

const STEP = 3;   // scan-band height (px) — finer = smoother slit
const MB = 8;     // macroblock snap (px) — the datamosh "compression" feel

function drawCover(c, idx, zoom) {
  const f = fits[idx];
  const cx = f.x + f.w / 2, cy = f.y + f.h / 2;
  const w = f.w * zoom, h = f.h * zoom;
  c.fillStyle = "#000";
  c.fillRect(0, 0, W, H);
  c.drawImage(imgs[idx], cx - w / 2, cy - h / 2, w, h);
}

// Slit-scan / datamosh transition (ported look from cover-video.mjs
// warpUnderString): base = the INCOMING illy drawn clean, so it
// resolves perfectly at g=1; over it the OUTGOING illy's scan-rows are
// horizontally displaced by a travelling slit profile, half macroblock-
// snapped + hash-jittered (compression artefacting), with rare datamosh
// "drag" blocks on the surge. Intensity envelopes 0→1→0 so each illy
// arrives and leaves clean and only the cut glitches.
function slitTransition(i, j, g, zoom, t) {
  drawCover(ctx, j, zoom);              // incoming — clean base (g=1 → clean)
  drawCover(oldX, i, zoom);             // outgoing — snapshot to shear
  const env = Math.sin(Math.PI * smooth(g));   // 0 → 1 → 0 glitch surge
  const amp = W * 0.11 * env;                  // max horizontal slit shift
  const ph = t * 2.3;                          // travelling band phase
  const sway = Math.sin(t * 0.7) * W * 0.013 * env;
  ctx.save();
  ctx.globalAlpha = Math.max(0, 1 - g);        // old shears away as it goes
  for (let y = 0; y < H; y += STEP) {
    const yi = (y / STEP) | 0;
    let hsh = ((yi + Math.floor(ph)) * 2654435761) >>> 0;
    hsh = ((hsh ^ (hsh >>> 15)) * 2246822519) >>> 0;
    const fr = ph - Math.floor(ph);
    let h2 = ((yi + Math.floor(ph) + 1) * 2654435761) >>> 0;
    h2 = ((h2 ^ (h2 >>> 15)) * 2246822519) >>> 0;
    const j0 = (hsh & 255) / 255 - 0.5, j1 = (h2 & 255) / 255 - 0.5;
    const jit = ((j0 * (1 - fr) + j1 * fr)) * amp * 0.5;
    let dx = Math.sin(y * 0.012 + ph) * amp + sway + jit; // smooth slit
    dx = dx * 0.55 + Math.round(dx / MB) * MB * 0.45;     // half blocky
    ctx.drawImage(oldC, 0, y, W, STEP, dx, y, W, STEP);
    // rare datamosh DRAG block (only during the surge)
    if ((hsh & 31) === 0 && env > 0.6) {
      const bw = 60 + (hsh % 90);
      const bx = (hsh >>> 5) % Math.max(1, W - bw);
      ctx.drawImage(oldC, bx, y, bw, STEP, bx + dx + 10, y, bw, STEP);
    }
  }
  ctx.restore();
}

function drawFrame(t) {
  // shared breathing zoom — seamless over the whole loop (period = DUR)
  const zoom = 1.025 + 0.025 * Math.sin((2 * Math.PI * t) / DUR);
  const seg = t / segDur;
  const i = Math.floor(seg) % N;
  const local = t - Math.floor(seg) * segDur;
  if (local > segDur - XF) {
    const g = (local - (segDur - XF)) / XF;        // 0 → 1
    slitTransition(i, (i + 1) % N, g, zoom, t);    // wraps N-1 → 0 (seamless)
  } else {
    drawCover(ctx, i, zoom);
  }
}

const ff = spawn("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS),
  "-i", "pipe:0",
  "-an",                                  // SILENT (Spotify Canvas)
  "-c:v", "libx264", "-pix_fmt", "yuv420p", "-preset", "medium", "-crf", "19",
  "-r", String(FPS), "-t", String(DUR),
  "-movflags", "+faststart",
  OUT,
], { stdio: ["pipe", "inherit", "inherit"] });
ff.on("error", (e) => { console.error("ffmpeg spawn error:", e); process.exit(1); });

const t0 = Date.now();
async function stream() {
  for (let f = 0; f < totalFrames; f++) {
    drawFrame(f / FPS);
    const buf = Buffer.from(canvas.toBuffer("raw")); // copy off the backing store
    if (!ff.stdin.write(buf)) await new Promise((r) => ff.stdin.once("drain", r));
    if (f % 30 === 0) process.stdout.write(`\r  frame ${f}/${totalFrames}`);
  }
  ff.stdin.end();
}
await stream();
await new Promise((res) => ff.on("close", res));
console.log(`\n✓ canvas ${((Date.now() - t0) / 1000).toFixed(1)}s → ${OUT}`);
