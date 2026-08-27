#!/usr/bin/env node
// photo-canvas.mjs — Spotify **Canvas** from a photo cover.
//
// Sibling to `pop/dance/bin/canvas-loop.mjs`. That one cycles a set of
// section illustrations; this one brings a single photograph to life,
// for the releases whose cover comes out of jeffrey's own library.
//
// Spec obeyed: 9:16, 3–8 s, SILENT, seamless loop, no text or logos
// drawn on top. Every motion here has period == the loop length, so the
// last frame flows into the first with no reverse/concat.
//
// The look: the frame drifts across the photo (a square cover holds far
// more image than 9:16 shows, so the drift reveals its edges), breathing
// in and out; the bright places — screens, lamps, daylight — bloom on
// the beat; a screen-refresh band climbs once a bar; snares knock the
// colour channels apart for a few frames.
//
// Usage:
//   node pop/bin/photo-canvas.mjs --photo cover.jpg --bpm 144 \
//     --out out/track-canvas.mp4
//
//   --photo PATH     source photograph (square cover works best)
//   --bpm N          track tempo — sets pulse rate (default 120)
//   --bars N         loop length in bars (default 4; keep 3–8 s)
//   --size WxH       default 1080x1920
//   --fps N          default 30
//   --pan PX         horizontal drift amplitude (default 190)
//   --zoom AMP       breathing amplitude (default 0.035)
//   --bloom AMT      beat bloom strength (default 0.26)
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
    if (!a[i].startsWith("--")) continue;
    const k = a[i].slice(2);
    flags[k] = i + 1 < a.length && !a[i + 1].startsWith("--") ? a[++i] : true;
  }
}
const expand = (p) =>
  typeof p !== "string" ? p : p.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p;

const PHOTO = expand(flags.photo);
const OUT = expand(flags.out);
if (!PHOTO || !existsSync(PHOTO)) { console.error("✗ --photo missing"); process.exit(1); }
if (!OUT) { console.error("✗ --out required"); process.exit(1); }

const sizeM = String(flags.size || "1080x1920").match(/^(\d+)x(\d+)$/);
const W = sizeM ? +sizeM[1] : 1080;
const H = sizeM ? +sizeM[2] : 1920;
const FPS = Number(flags.fps ?? 30);
const BPM = Number(flags.bpm ?? 120);
const BARS = Number(flags.bars ?? 4);
const PAN = Number(flags.pan ?? 190);
const ZOOM = Number(flags.zoom ?? 0.035);
const BLOOM = Number(flags.bloom ?? 0.26);

const BEAT = 60 / BPM;
const DUR = BARS * 4 * BEAT;
if (DUR < 3 || DUR > 8) {
  console.error(`✗ ${DUR.toFixed(2)}s loop — Spotify Canvas wants 3–8 s; change --bars`);
  process.exit(1);
}
const FRAMES = Math.round(DUR * FPS);

console.log(`▸ photo-canvas · ${W}x${H} · ${DUR.toFixed(3)}s (${BARS} bars @ ${BPM}) · ${FRAMES}f`);

const img = await loadImage(PHOTO);

// Bright-pass, held small so drawing it back up is itself the blur.
const GLOW = 320;
const glowC = createCanvas(GLOW, GLOW);
{
  const g = glowC.getContext("2d");
  g.imageSmoothingQuality = "high";
  g.drawImage(img, 0, 0, GLOW, GLOW);
  const d = g.getImageData(0, 0, GLOW, GLOW);
  const px = d.data;
  for (let i = 0; i < px.length; i += 4) {
    const luma = 0.2126 * px[i] + 0.7152 * px[i + 1] + 0.0722 * px[i + 2];
    let k = (luma - 148) / 100;                       // screens, lamp, daylight
    k = k <= 0 ? 0 : k >= 1 ? 1 : k * k * (3 - 2 * k);
    px[i] *= k; px[i + 1] *= k; px[i + 2] *= k;
  }
  g.putImageData(d, 0, 0);
}

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
ctx.imageSmoothingEnabled = true;
ctx.imageSmoothingQuality = "high";

const vignette = ctx.createRadialGradient(W / 2, H * 0.46, H * 0.20, W / 2, H * 0.46, H * 0.72);
vignette.addColorStop(0, "rgba(0,0,0,0)");
vignette.addColorStop(1, "rgba(0,0,0,0.36)");

const BAND = H * 0.20;
const band = ctx.createLinearGradient(0, 0, 0, BAND);
band.addColorStop(0, "rgba(150,190,255,0)");
band.addColorStop(0.5, "rgba(150,190,255,0.055)");
band.addColorStop(1, "rgba(150,190,255,0)");

const TAU = Math.PI * 2;

function drawFrame(t) {
  const p = t / DUR;                                  // 0→1 across the loop
  const zoom = 1 + ZOOM - ZOOM * Math.cos(TAU * p);
  const scale = Math.max(W / img.width, H / img.height) * zoom;
  const w = img.width * scale, h = img.height * scale;

  const beat = t / BEAT;
  const inBeat = beat - Math.floor(beat);
  const which = Math.floor(beat) % 4;                 // beat of the bar
  // two-step: kick opens the bar, snare answers on 3; the off-beats tick.
  const weight = which === 0 ? 1 : which === 2 ? 0.85 : 0.3;
  const hit = Math.exp(-inBeat * 9) * weight;

  const x = (W - w) / 2 + PAN * Math.sin(TAU * p);
  const y = (H - h) * 0.5 - hit * 5;

  ctx.fillStyle = "#000";
  ctx.fillRect(0, 0, W, H);
  ctx.drawImage(img, x, y, w, h);

  ctx.globalCompositeOperation = "lighter";
  // 24 shimmers a loop under the beat bloom — the screens never sit still.
  ctx.globalAlpha = 0.08 + hit * BLOOM + 0.018 * Math.sin(TAU * p * 24);
  ctx.drawImage(glowC, x, y, w, h);

  // screen-refresh band, one climb per bar, wrapped so it never pops
  const bp = (t / (4 * BEAT)) % 1;
  const by = H - bp * (H + BAND);
  ctx.globalAlpha = 1;
  for (const off of [0, H + BAND]) {
    ctx.save();
    ctx.translate(0, by + off);
    ctx.fillStyle = band;
    ctx.fillRect(0, 0, W, BAND);
    ctx.restore();
  }

  ctx.globalCompositeOperation = "source-over";
  ctx.fillStyle = vignette;
  ctx.fillRect(0, 0, W, H);

  // Chroma only on kick and snare, and decaying much faster than the
  // bloom so it lands as a one-frame flick instead of a standing fringe.
  return which === 0 || which === 2 ? Math.exp(-inBeat * 16) : 0;
}

// Channel knock + grain, done on the raw BGRA buffer — cheaper and more
// convincing than compositing tinted copies.
function post(buf, hit, frame) {
  const d = Math.round(hit * 3.4);
  if (d > 0) {
    for (let yy = 0; yy < H; yy++) {
      const row = yy * W * 4;
      for (let xx = W - 1; xx >= d; xx--) buf[row + xx * 4 + 2] = buf[row + (xx - d) * 4 + 2];
      for (let xx = 0; xx < W - d; xx++) buf[row + xx * 4] = buf[row + (xx + d) * 4];
    }
  }
  let s = (frame * 2654435761) >>> 0;
  for (let i = 0; i < buf.length; i += 4) {
    s ^= s << 13; s >>>= 0; s ^= s >>> 17; s ^= s << 5; s >>>= 0;
    const n = ((s & 15) - 7) * 0.28;
    for (let c = 0; c < 3; c++) {
      const v = buf[i + c] + n;
      buf[i + c] = v < 0 ? 0 : v > 255 ? 255 : v;
    }
  }
}

// QuickTime freezes on frame 1 of short, track-less H.264 clips, so ship
// a silent stereo AAC track (Canvas must be inaudible, not track-less)
// plus CFR and a 1 s GOP at High@4.0 — see canvas-loop.mjs.
const ff = spawn("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS),
  "-i", "pipe:0",
  "-f", "lavfi", "-i", "anullsrc=r=44100:cl=stereo",
  "-map", "0:v:0", "-map", "1:a:0",
  "-c:v", "libx264", "-profile:v", "high", "-level", "4.0",
  "-pix_fmt", "yuv420p", "-preset", "slow", "-crf", "20",
  "-g", String(FPS), "-keyint_min", String(FPS), "-sc_threshold", "0",
  "-c:a", "aac", "-b:a", "96k",
  "-vsync", "cfr", "-r", String(FPS), "-t", String(DUR), "-shortest",
  "-movflags", "+faststart",
  OUT,
], { stdio: ["pipe", "inherit", "inherit"] });
ff.on("error", (e) => { console.error("ffmpeg spawn error:", e); process.exit(1); });

const t0 = Date.now();
for (let f = 0; f < FRAMES; f++) {
  const hit = drawFrame(f / FPS);
  const buf = Buffer.from(canvas.toBuffer("raw"));
  post(buf, hit, f);
  if (!ff.stdin.write(buf)) await new Promise((r) => ff.stdin.once("drain", r));
  if (f % 25 === 0) process.stdout.write(`\r  frame ${f}/${FRAMES}`);
}
ff.stdin.end();
await new Promise((r) => ff.on("close", r));
console.log(`\n✓ canvas ${((Date.now() - t0) / 1000).toFixed(1)}s → ${OUT}`);
