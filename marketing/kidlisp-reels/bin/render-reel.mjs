#!/usr/bin/env node
// render-reel.mjs — fit a captured KidLisp frame strip into a 1080×1920 reel and
// stamp it with AC's "side-stamp" chrome: two pals watermarks hugging the
// left/right edges (rotated ±90°, looping wiggle + swivel, 4-pass seep) and the
// $code as climbing per-character title columns — the code to type in. Ported
// from pop/menuband/bin/chrome-reel.mjs, but SILENT (audio added at post time)
// so the pals' LED pulse is driven by the piece's own VISUAL motion (frame-to-
// frame luma delta) instead of an audio envelope.
//
// Reads <out>/frames/ + <out>/capture.json (from capture-kidlisp.mjs).
// Writes <out>/<slug>-reel.mp4.
//
// Usage: node marketing/kidlisp-reels/bin/render-reel.mjs '$tezz'

import { spawn } from "node:child_process";
import { readFileSync, existsSync, mkdirSync, readdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { once } from "node:events";
import { createCanvas, loadImage } from "canvas";
import { spawnSync } from "node:child_process";
import { prerenderTitleChars } from "../../../pop/lib/preview-shared.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");

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

const CODE = positional[0];
if (!CODE) { console.error("usage: render-reel.mjs '$code'"); process.exit(1); }
const SLUG = CODE.replace(/^\$/, "").replace(/[^a-z0-9_-]/gi, "-").toLowerCase() || "piece";
const OUT = resolve((flags.out || `${LANE}/out/${SLUG}`).replace(/^~/, process.env.HOME));
const FRAMES_DIR = `${OUT}/frames`;
const META = JSON.parse(readFileSync(`${OUT}/capture.json`, "utf8"));
const OUTFILE = `${OUT}/${SLUG}-reel.mp4`;
const assetsDir = `${OUT}/chrome-assets`;
mkdirSync(assetsDir, { recursive: true });

const W = 1080, H = 1920;
const FPS = META.targetFps ?? META.fps ?? 30;
const SRC_W = META.width, SRC_H = META.height;   // captured piece dimensions
const TITLE = CODE;                              // "$tezz" — the side-stamp text

// Resample the realtime-captured frames to a constant FPS by TRUE elapsed time,
// so the piece plays at its real speed (heavy pieces captured below FPS get
// frames duplicated, never time-compressed). META.frames = [{file, t}] in
// ascending t; realDuration is the actual wall-clock span we captured.
const captured = (META.frames ?? []).filter((f) => existsSync(`${FRAMES_DIR}/${f.file}`));
if (captured.length === 0) {
  // legacy capture (no timestamps): fall back to on-disk order at FPS
  const files = readdirSync(FRAMES_DIR).filter((f) => f.endsWith(".png")).sort();
  for (let i = 0; i < files.length; i++) captured.push({ file: files[i], t: i / FPS });
}
if (captured.length === 0) throw new Error(`no frames in ${FRAMES_DIR}`);
const realDuration = META.realDuration ?? captured[captured.length - 1].t;
const FRAMES = Math.max(1, Math.round(realDuration * FPS));
// frameFiles[j] = the captured PNG nearest output time j/FPS (pointer walk).
const frameFiles = [];
{
  let p = 0;
  for (let j = 0; j < FRAMES; j++) {
    const tOut = j / FPS;
    while (p < captured.length - 1 && Math.abs(captured[p + 1].t - tOut) <= Math.abs(captured[p].t - tOut)) p++;
    frameFiles.push(captured[p].file);
  }
}
console.log(`▸ resampled ${captured.length} real frames (${realDuration.toFixed(1)}s) → ${FRAMES} frames @ ${FPS}fps (true speed)`);

// Word-bounce speed — lower = slower. (@jeffrey: bounce the words slower.)
const BOUNCE_RATE = 1.5;

// Placement: if the capture already matches the 9:16 reel aspect, the piece is
// drawn full-bleed (fills the whole frame). Otherwise it sits full-width in a
// centered band over a blurred, darkened cover of itself.
const srcAspect = SRC_W / SRC_H;
const FULL_BLEED = Math.abs(srcAspect - W / H) < 0.02;
const PIECE_W = W;
const PIECE_H = Math.round(SRC_H * (W / SRC_W)); // contain-to-width
const PIECE_Y = Math.round((H - PIECE_H) / 2);

// ── title chars + pals raster ────────────────────────────────────────────────
const titleFontSize = 96;
const { chars: titleChars, totalWidth: titleTotalW } = await prerenderTitleChars({
  text: TITLE, ptSize: titleFontSize, palette: ["#FFFFFF"], shadowColor: null, assetsDir,
});
const PALS_S = 145, PALS_HALF = PALS_S / 2;
const PALS_EDGE_X = 64, CHARS_EDGE_X = PALS_EDGE_X + 12;
const CHAR_SCALE = 0.48;
const CHAR_SPAN = titleTotalW * CHAR_SCALE;
const BOUNCE_BUF = 26;
const LEFT_CHARS_CY = H * 0.78, RIGHT_CHARS_CY = H * 0.22;
const LEFT_PALS_CY = LEFT_CHARS_CY - CHAR_SPAN / 2 - BOUNCE_BUF - PALS_HALF;
const RIGHT_PALS_CY = RIGHT_CHARS_CY + CHAR_SPAN / 2 + BOUNCE_BUF + PALS_HALF;

const PALS_WM_SIZE = 212;
let palsImg = null, palsBlur = null;
{
  const svg = `${REPO}/system/public/purple-pals.svg`;
  const png = `${assetsDir}/pals-watermark.png`;
  const blurPng = `${assetsDir}/pals-watermark-blur.png`;
  const r = spawnSync("rsvg-convert", ["-w", String(PALS_WM_SIZE * 2), "-h", String(PALS_WM_SIZE * 2), "-o", png, svg]);
  if (r.status === 0 && existsSync(png)) {
    palsImg = await loadImage(png);
    const rb = spawnSync("magick", [png, "-channel", "A", "-blur", "0x1.5", "+channel", blurPng]);
    palsBlur = (rb.status === 0 && existsSync(blurPng)) ? await loadImage(blurPng) : palsImg;
  }
}
if (!palsImg) throw new Error("pals watermark failed to rasterize (need rsvg-convert)");

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const wmCanvas = createCanvas(8, 8);
const wmCtx = wmCanvas.getContext("2d");
const charTintCanvas = createCanvas(2, 2);
const charTintCtx = charTintCanvas.getContext("2d");

// tiny offscreen for cheap per-frame luma (drives motion envelope + tint sample)
const LUMA_N = 32;
const lumaCanvas = createCanvas(LUMA_N, LUMA_N);
const lumaCtx = lumaCanvas.getContext("2d");
function sampleLuma(img) {
  lumaCtx.drawImage(img, 0, 0, LUMA_N, LUMA_N);
  const d = lumaCtx.getImageData(0, 0, LUMA_N, LUMA_N).data;
  let sum = 0, rs = 0, gs = 0, bs = 0;
  const px = [];
  for (let i = 0; i < d.length; i += 4) {
    const r = d[i], g = d[i + 1], b = d[i + 2];
    rs += r; gs += g; bs += b;
    const y = 0.2126 * r + 0.7152 * g + 0.0722 * b;
    sum += y; px.push(y);
  }
  const n = d.length / 4;
  return { mean: sum / n, px, rgb: [rs / n, gs / n, bs / n] };
}

// ── reel tint: average color across first/mid/last frames, nudged saturated ──
const tintRgb = await (async () => {
  const idxs = [0, Math.floor(FRAMES / 2), FRAMES - 1];
  let r = 0, g = 0, b = 0;
  for (const i of idxs) {
    const img = await loadImage(`${FRAMES_DIR}/${frameFiles[i]}`);
    const s = sampleLuma(img);
    r += s.rgb[0]; g += s.rgb[1]; b += s.rgb[2];
  }
  r /= idxs.length; g /= idxs.length; b /= idxs.length;
  // pull toward a readable mid-bright stamp color (avoid muddy averages)
  const lift = (v) => Math.round(Math.min(255, 70 + v * 0.85));
  return [lift(r), lift(g), lift(b)];
})();

function sectionTcRgb() { return tintRgb; }

function palsTinted(src, c) {
  const ww = src.width, wh = src.height;
  wmCanvas.width = ww; wmCanvas.height = wh;
  wmCtx.clearRect(0, 0, ww, wh);
  wmCtx.globalCompositeOperation = "source-over";
  wmCtx.drawImage(src, 0, 0);
  wmCtx.globalCompositeOperation = "source-in";
  wmCtx.fillStyle = `rgb(${c[0]},${c[1]},${c[2]})`;
  wmCtx.fillRect(0, 0, ww, wh);
  wmCtx.globalCompositeOperation = "source-over";
  return wmCanvas;
}
function tintCharGlyph(img, c) {
  const w = img.width, h = img.height;
  charTintCanvas.width = w; charTintCanvas.height = h;
  charTintCtx.globalCompositeOperation = "source-over";
  charTintCtx.clearRect(0, 0, w, h);
  charTintCtx.drawImage(img, 0, 0);
  charTintCtx.globalCompositeOperation = "source-in";
  charTintCtx.fillStyle = `rgb(${c[0]},${c[1]},${c[2]})`;
  charTintCtx.fillRect(0, 0, w, h);
  charTintCtx.globalCompositeOperation = "source-over";
  return charTintCanvas;
}

function drawWatermark(t, env) {
  const s = PALS_S;
  const u = (t * FPS) / FRAMES;
  const TAU = Math.PI * 2;
  const col = sectionTcRgb();
  const e = Math.min(1, env);
  const glow = e * e;
  const ledCol = [
    Math.round(col[0] + (255 - col[0]) * glow * 0.6),
    Math.round(col[1] + (255 - col[1]) * glow * 0.6),
    Math.round(col[2] + (255 - col[2]) * glow * 0.6),
  ];
  const wig = 13 * Math.sin(TAU * 24 * u + 0.7) + 4 * Math.sin(TAU * 9 * u);
  const bob = 3 * Math.sin(TAU * 17 * u + 2.1) + 1.5 * Math.sin(TAU * 31 * u);
  const swiv = 0.05 * Math.sin(TAU * 19 * u) + 0.025 * Math.sin(TAU * 38 * u + 1.4);
  const spots = [
    { cx: PALS_EDGE_X - wig, cy: LEFT_PALS_CY + bob, rot: Math.PI / 2 + swiv },
    { cx: W - PALS_EDGE_X + wig, cy: RIGHT_PALS_CY - bob, rot: -Math.PI / 2 - swiv },
  ];
  const passes = [["multiply", 0.78], ["color-burn", 0.42], ["overlay", 0.58], ["source-over", 0.06]];
  for (const sp of spots) {
    ctx.save();
    ctx.translate(sp.cx, sp.cy);
    ctx.rotate(sp.rot);
    palsTinted(palsImg, [0, 0, 0]);
    ctx.globalCompositeOperation = "source-over";
    ctx.globalAlpha = 0.26;
    ctx.drawImage(wmCanvas, -s / 2 + 3, -s / 2 + 4, s, s);
    palsTinted(palsBlur, col);
    for (const [op, a] of passes) {
      ctx.globalCompositeOperation = op; ctx.globalAlpha = a;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    }
    ctx.globalCompositeOperation = "source-over";
    ctx.globalAlpha = 0.30;
    ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    if (glow > 0.001) {
      palsTinted(palsBlur, ledCol);
      ctx.globalCompositeOperation = "screen";
      ctx.globalAlpha = 0.14 + 0.78 * glow;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
      palsTinted(palsImg, ledCol);
      ctx.globalCompositeOperation = "source-over";
      ctx.globalAlpha = 0.18 + 0.46 * glow;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    }
    ctx.restore();
  }
  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
  drawPalsTitleChars(t, env);
}

function drawPalsTitleChars(t, env) {
  const u = (t * FPS) / FRAMES;
  const TAU = Math.PI * 2;
  const wig = 11 * Math.sin(TAU * 30 * u + 3.6) + 4 * Math.sin(TAU * 12 * u + 1.1);
  const span = titleTotalW * CHAR_SCALE;
  const palsRgb = sectionTcRgb();
  const spots = [
    { charsCx: CHARS_EDGE_X - wig, cy: LEFT_CHARS_CY, rot: Math.PI / 2 },
    { charsCx: W - CHARS_EDGE_X + wig, cy: RIGHT_CHARS_CY, rot: -Math.PI / 2 },
  ];
  const startX = -span / 2;
  for (const sp of spots) {
    ctx.save();
    ctx.translate(sp.charsCx, sp.cy);
    ctx.rotate(sp.rot);
    for (let i = 0; i < titleChars.length; i++) {
      const ch = titleChars[i];
      if (!ch.img) continue;
      const x = startX + ch.prefixWidth * CHAR_SCALE;
      const dw = ch.img.width * CHAR_SCALE;
      const dh = ch.img.height * CHAR_SCALE;
      const lift = 4 * Math.sin(t * BOUNCE_RATE + i * 0.8) * (0.3 + env);
      const y = -dh / 2 + lift;
      const charRot = 0.03 * Math.sin(t * BOUNCE_RATE * 0.8 + i * 1.15);
      ctx.save();
      ctx.translate(x + dw / 2, y + dh / 2);
      ctx.rotate(charRot);
      ctx.translate(-(x + dw / 2), -(y + dh / 2));
      const sh = 0.5 + 0.5 * Math.sin(t * BOUNCE_RATE * 0.85 - i * 0.7);
      const charRgb = [
        Math.round(palsRgb[0] + (255 - palsRgb[0]) * sh * 0.35),
        Math.round(palsRgb[1] + (255 - palsRgb[1]) * sh * 0.35),
        Math.round(palsRgb[2] + (255 - palsRgb[2]) * sh * 0.35),
      ];
      ctx.save();
      ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.26;
      ctx.drawImage(tintCharGlyph(ch.img, [0, 0, 0]), x + 3, y + 4, dw, dh);
      ctx.restore();
      const charPasses = [["multiply", 0.78], ["color-burn", 0.42], ["overlay", 0.58], ["source-over", 0.06]];
      for (const [op, a] of charPasses) {
        ctx.save();
        ctx.globalCompositeOperation = op; ctx.globalAlpha = a;
        ctx.drawImage(tintCharGlyph(ch.img, charRgb), x, y, dw, dh);
        ctx.restore();
      }
      ctx.save();
      ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.46;
      ctx.drawImage(tintCharGlyph(ch.img, palsRgb), x, y, dw, dh);
      ctx.restore();
      if (env > 0.45) {
        ctx.save();
        ctx.globalCompositeOperation = "screen";
        ctx.globalAlpha = 0.14 + 0.46 * Math.min(1, (env - 0.45) / 0.55);
        ctx.drawImage(tintCharGlyph(ch.img, charRgb), x, y, dw, dh);
        ctx.restore();
      }
      ctx.restore();
    }
    ctx.restore();
  }
}

// ── silent encoder (no audio; @jeffrey adds sound at post time) ──────────────
function spawnSilentEncode(outPath) {
  return spawn("ffmpeg", [
    "-hide_banner", "-loglevel", "error", "-y",
    "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
    "-c:v", "libx264", "-preset", "faster", "-crf", "18", "-threads", "0",
    "-pix_fmt", "yuv420p", "-movflags", "+faststart", outPath,
  ], { stdio: ["pipe", "inherit", "inherit"] });
}

// ── frame pump: each PNG → backdrop + crisp piece + chrome → encoder ─────────
console.log(`▸ kidlisp reel · ${FRAMES} frames · "${TITLE}" · tint rgb(${tintRgb.join(",")}) · silent`);
const enc = spawnSilentEncode(OUTFILE);
let prevPx = null;
let cachedFile = null, cachedImg = null;
const t0 = Date.now();
for (let i = 0; i < FRAMES; i++) {
  // resampling duplicates frames for heavy pieces — only reload on file change
  if (frameFiles[i] !== cachedFile) {
    cachedImg = await loadImage(`${FRAMES_DIR}/${frameFiles[i]}`);
    cachedFile = frameFiles[i];
  }
  const img = cachedImg;

  // motion envelope: mean abs luma delta vs previous frame, normalized
  const { px } = sampleLuma(img);
  let motion = 0;
  if (prevPx) { for (let k = 0; k < px.length; k++) motion += Math.abs(px[k] - prevPx[k]); motion /= px.length; }
  prevPx = px;
  const env = Math.min(1, motion / 24);          // ~24 luma units of delta = full pulse

  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
  ctx.fillStyle = "#000";
  ctx.fillRect(0, 0, W, H);

  if (FULL_BLEED) {
    // piece fills the whole 9:16 frame, pixel-perfect
    ctx.imageSmoothingEnabled = false;
    ctx.drawImage(img, 0, 0, W, H);
    ctx.imageSmoothingEnabled = true;
  } else {
    // blurred, darkened cover of the frame behind the centered band
    ctx.imageSmoothingEnabled = true;
    const cover = Math.max(W / SRC_W, H / SRC_H) * 1.15;
    const cw = SRC_W * cover, chh = SRC_H * cover;
    ctx.globalAlpha = 0.40;
    ctx.drawImage(img, (W - cw) / 2, (H - chh) / 2, cw, chh);
    ctx.globalAlpha = 1;
    ctx.fillStyle = "rgba(0,0,0,0.45)";
    ctx.fillRect(0, 0, W, H);
    // crisp piece, pixel-perfect, in the center band
    ctx.imageSmoothingEnabled = false;
    ctx.drawImage(img, 0, PIECE_Y, PIECE_W, PIECE_H);
    ctx.imageSmoothingEnabled = true;
  }

  // side-stamp chrome
  drawWatermark(i / FPS, env);

  if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
  if ((i + 1) % 60 === 0) console.log(`  ${i + 1}/${FRAMES} · ${((Date.now() - t0) / 1000).toFixed(0)}s`);
}
enc.stdin.end();
await new Promise((res, rej) => { enc.on("close", (c) => (c === 0 ? res() : rej(new Error(`ffmpeg exit ${c}`)))); });
console.log(`✓ ${OUTFILE} (${FRAMES} frames chromed, silent)`);
