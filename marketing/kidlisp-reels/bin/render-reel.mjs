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
import { prerenderTitleChars, setPreviewFont } from "../../../pop/lib/preview-shared.mjs";

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

// Seamless loop (--loop <sec>): crossfade the tail back into the head. The
// output is FRAMES - Xf long; its first Xf frames blend frame[j] (head) with
// frame[j + OUT_FRAMES] (the tail continuation), so the wrap is continuous —
// the tail dissolves into the head, hiding the only real jump. Needs the
// capture to run ~loop-seconds longer than the desired loop length.
// --no-stamps: clean full-bleed piece, no pals side-stamps (good for TikTok).
const NO_STAMPS = !!flags["no-stamps"];
const LOOP_SEC = flags.loop ? parseFloat(flags.loop) : 0;
const Xf = LOOP_SEC > 0 ? Math.min(FRAMES - 1, Math.round(LOOP_SEC * FPS)) : 0;
let OUT_FRAMES = FRAMES - Xf;
if (LOOP_SEC > 0) console.log(`▸ seamless loop: ${OUT_FRAMES} frames (${(OUT_FRAMES / FPS).toFixed(1)}s), ${LOOP_SEC}s tail→head crossfade`);
if (NO_STAMPS) console.log(`▸ no-stamps: clean full-bleed, no side chrome`);

// Word-bounce speed — lower = slower. (@jeffrey: bounce the words slower.)
const BOUNCE_RATE = 1.5;
// Pals side-stamp motion speed — scales the wiggle/bob/swivel frequencies.
// Lower = calmer stamps. (@jeffrey: pals logos were moving too fast.)
const PALS_SPEED = 0.18;

// Placement: if the capture already matches the 9:16 reel aspect, the piece is
// drawn full-bleed (fills the whole frame). Otherwise it sits full-width in a
// centered band over a blurred, darkened cover of itself.
const srcAspect = SRC_W / SRC_H;
const FULL_BLEED = Math.abs(srcAspect - W / H) < 0.02;
const PIECE_W = W;
const PIECE_H = Math.round(SRC_H * (W / SRC_W)); // contain-to-width
const PIECE_Y = Math.round((H - PIECE_H) / 2);

// ── title chars + pals raster ────────────────────────────────────────────────
// The $code reads in YWFT (the default geometric/computery preview font), EXCEPT
// the leading "$" sigil, which is set in Comic Relief (KidLisp's own $ typeface).
const titleFontSize = 96;
const { chars: titleChars, totalWidth: titleTotalW } = await prerenderTitleChars({
  text: TITLE, ptSize: titleFontSize, palette: ["#FFFFFF"], shadowColor: null, assetsDir,
});
const DOLLAR_FONT = `${REPO}/oven/fonts/ComicRelief-Regular.ttf`;  // lighter than the Bold
if (existsSync(DOLLAR_FONT) && titleChars.some((c) => c.char === "$")) {
  setPreviewFont(DOLLAR_FONT);
  const d = await prerenderTitleChars({ text: "$", ptSize: titleFontSize, palette: ["#FFFFFF"], shadowColor: null, assetsDir });
  setPreviewFont("ywft");  // restore for any later use
  const di = d.chars[0]?.img;
  if (di) for (const c of titleChars) if (c.char === "$") c.img = di;
}
const PALS_S = 145, PALS_HALF = PALS_S / 2;
const PALS_EDGE_X = 110, CHARS_EDGE_X = PALS_EDGE_X + 12;  // badges pulled in from the edges
const CHAR_SCALE = 0.72;                                  // code letters scaled up for readability (~pals scale)
const KERN = 0;                                           // natural YWFT kerning (the spacing that read right)
const DOLLAR_SCALE = 1.25;                                // the $ sigil LARGER than the code letters
const DOLLAR_DX = 0;                                      // $ x-nudge (the gap below handles separation)
const DOLLAR_GAP = 20;                                    // extra room after the big $ so letters don't collide
const DOLLAR_SPIN = 0.7;                                  // $ rotates on its own (rad/s), separate from the letters
const titleSpanGlyph = titleTotalW + (titleChars.length - 1) * KERN;
const CHAR_SPAN = titleSpanGlyph * CHAR_SCALE;
const BOUNCE_BUF = 26;

// KidLisp reel palette — subtle lime-green/yellow with VHS chromatic-aberration
// fringe (magenta/cyan ghosts split a few px around a lime-yellow core).
const CORE = [180, 240, 70], CORE_HI = [225, 255, 150];
const CHROMA_R = [255, 60, 120], CHROMA_B = [70, 170, 255], CHROMA_PX = 3;
const LEFT_CHARS_CY = H * 0.86, RIGHT_CHARS_CY = H * 0.14;  // pushed toward the top/bottom edges
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

// Draw one glyph as a subtle VHS stamp: soft shadow + magenta/cyan chromatic
// ghosts split ±CHROMA_PX + a lime-yellow core + a gentle env glow. `tintFn`
// returns a tinted glyph canvas (palsTinted or tintCharGlyph).
function drawChroma(tintFn, sharp, soft, dx, dy, dw, dh, env) {
  ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.20;
  ctx.drawImage(tintFn(sharp, [8, 10, 6]), dx + 2.5, dy + 3, dw, dh);
  ctx.globalCompositeOperation = "screen";
  ctx.globalAlpha = 0.30; ctx.drawImage(tintFn(soft, CHROMA_R), dx - CHROMA_PX, dy, dw, dh);
  ctx.globalAlpha = 0.30; ctx.drawImage(tintFn(soft, CHROMA_B), dx + CHROMA_PX, dy, dw, dh);
  ctx.globalAlpha = 0.60; ctx.drawImage(tintFn(sharp, CORE), dx, dy, dw, dh);
  if (env > 0.02) { ctx.globalAlpha = 0.08 + 0.18 * env; ctx.drawImage(tintFn(soft, CORE_HI), dx, dy, dw, dh); }
  ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 1;
}

function drawWatermark(t, env) {
  const s = PALS_S;
  const u = (t * FPS) / FRAMES;
  const TAU = Math.PI * 2;
  const sp = PALS_SPEED;
  const wig = 13 * Math.sin(TAU * 24 * sp * u + 0.7) + 4 * Math.sin(TAU * 9 * sp * u);
  const bob = 3 * Math.sin(TAU * 17 * sp * u + 2.1) + 1.5 * Math.sin(TAU * 31 * sp * u);
  const swiv = 0.05 * Math.sin(TAU * 19 * sp * u) + 0.025 * Math.sin(TAU * 38 * sp * u + 1.4);
  const spots = [
    { cx: PALS_EDGE_X - wig, cy: LEFT_PALS_CY + bob, rot: Math.PI / 2 + swiv },
    { cx: W - PALS_EDGE_X + wig, cy: RIGHT_PALS_CY - bob, rot: -Math.PI / 2 - swiv },
  ];
  for (const spot of spots) {
    ctx.save();
    ctx.translate(spot.cx, spot.cy);
    ctx.rotate(spot.rot);
    drawChroma((src, c) => palsTinted(src, c), palsImg, palsBlur, -s / 2, -s / 2, s, s, env);
    ctx.restore();
  }
  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
  drawPalsTitleChars(t, env);
}

function drawPalsTitleChars(t, env) {
  const u = (t * FPS) / FRAMES;
  const TAU = Math.PI * 2;
  const wig = 11 * Math.sin(TAU * 30 * PALS_SPEED * u + 3.6) + 4 * Math.sin(TAU * 12 * PALS_SPEED * u + 1.1);
  const span = CHAR_SPAN;
  const spots = [
    { charsCx: CHARS_EDGE_X - wig, cy: LEFT_CHARS_CY, rot: Math.PI / 2 },
    { charsCx: W - CHARS_EDGE_X + wig, cy: RIGHT_CHARS_CY, rot: -Math.PI / 2 },
  ];
  const startX = -(span + DOLLAR_GAP) / 2;
  for (const sp of spots) {
    ctx.save();
    ctx.translate(sp.charsCx, sp.cy);
    ctx.rotate(sp.rot);
    for (let i = 0; i < titleChars.length; i++) {
      const ch = titleChars[i];
      if (!ch.img) continue;
      const isDollar = ch.char === "$";
      const cs = isDollar ? CHAR_SCALE * DOLLAR_SCALE : CHAR_SCALE;
      // letters after the $ shift right by DOLLAR_GAP to clear the larger sigil
      const x = startX + (ch.prefixWidth + i * KERN) * CHAR_SCALE + (isDollar ? DOLLAR_DX : DOLLAR_GAP);
      const dw = ch.img.width * cs;
      const dh = ch.img.height * cs;
      const lift = 3.5 * Math.sin(t * BOUNCE_RATE + i * 0.8) * (0.6 + 0.25 * env);
      const y = -dh / 2 + lift;
      // the $ tumbles on its own continuous spin; letters keep their subtle wobble
      const charRot = isDollar ? t * DOLLAR_SPIN : 0.025 * Math.sin(t * BOUNCE_RATE * 0.8 + i * 1.15);
      ctx.save();
      ctx.translate(x + dw / 2, y + dh / 2);
      ctx.rotate(charRot);
      ctx.translate(-(x + dw / 2), -(y + dh / 2));
      drawChroma((img, c) => tintCharGlyph(img, c), ch.img, ch.img, x, y, dw, dh, env);
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

const frameCache = new Map();  // small LRU of decoded source frames
async function getFrame(file) {
  let im = frameCache.get(file);
  if (!im) {
    im = await loadImage(`${FRAMES_DIR}/${file}`);
    frameCache.set(file, im);
    if (frameCache.size > 8) frameCache.delete(frameCache.keys().next().value);
  }
  return im;
}

// Trailing-static trim: some KidLisp pieces (e.g. $tezz, $ceo) reach a near-
// static end state, so the reel would "freeze" partway. Walk backward from the
// end and drop frames until motion resumes, so a reel ALWAYS ends on movement.
// (Skipped for --loop, which has its own tail handling.)
if (LOOP_SEC === 0) {
  const STATIC = 1.2, PAD = Math.round(0.3 * FPS), MIN = Math.round(4 * FPS);
  let prev = null, lastLively = OUT_FRAMES - 1;
  for (let j = OUT_FRAMES - 1; j >= 0; j--) {
    const { px } = sampleLuma(await getFrame(frameFiles[j]));
    if (prev) {
      let m = 0; for (let k = 0; k < px.length; k++) m += Math.abs(px[k] - prev[k]); m /= px.length;
      if (m > STATIC) { lastLively = j; break; }
    }
    prev = px;
  }
  const trimmed = Math.max(MIN, Math.min(OUT_FRAMES, lastLively + 1 + PAD));
  if (trimmed < OUT_FRAMES) {
    console.log(`▸ trailing-static trim: ${OUT_FRAMES}→${trimmed} frames (${(trimmed / FPS).toFixed(1)}s — piece went static)`);
    OUT_FRAMES = trimmed;
  }
}

// ── frame pump: each PNG → backdrop + crisp piece + chrome → encoder ─────────
console.log(`▸ kidlisp reel · ${OUT_FRAMES} frames · "${TITLE}" · VHS lime-green chrome · silent`);
const enc = spawnSilentEncode(OUTFILE);
let prevPx = null;
let envSmooth = 0;  // heavily smoothed so the color/glow doesn't flicker on chaotic pieces
const blendCanvas = createCanvas(SRC_W, SRC_H);
const bctx = blendCanvas.getContext("2d");
const t0 = Date.now();
for (let i = 0; i < OUT_FRAMES; i++) {
  let img;
  if (Xf > 0 && i < Xf) {
    // crossfade region: (1-w)*tail + w*head → seamless wrap
    const w = i / Xf;
    const tail = await getFrame(frameFiles[i + OUT_FRAMES]);
    const head = await getFrame(frameFiles[i]);
    bctx.globalAlpha = 1; bctx.clearRect(0, 0, SRC_W, SRC_H);
    bctx.drawImage(tail, 0, 0, SRC_W, SRC_H);
    bctx.globalAlpha = w; bctx.drawImage(head, 0, 0, SRC_W, SRC_H);
    bctx.globalAlpha = 1;
    img = blendCanvas;
  } else {
    img = await getFrame(frameFiles[i]);
  }

  // motion envelope: mean abs luma delta vs previous frame, normalized
  const { px } = sampleLuma(img);
  let motion = 0;
  if (prevPx) { for (let k = 0; k < px.length; k++) motion += Math.abs(px[k] - prevPx[k]); motion /= px.length; }
  prevPx = px;
  const env = Math.min(1, motion / 24);          // ~24 luma units of delta = full pulse
  envSmooth = envSmooth * 0.92 + env * 0.08;     // low-pass → calm, non-flickery glow

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

  // side-stamp chrome (skipped in --no-stamps clean mode)
  if (!NO_STAMPS) drawWatermark(i / FPS, envSmooth);

  if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
  if ((i + 1) % 60 === 0) console.log(`  ${i + 1}/${OUT_FRAMES} · ${((Date.now() - t0) / 1000).toFixed(0)}s`);
}
enc.stdin.end();
await new Promise((res, rej) => { enc.on("close", (c) => (c === 0 ? res() : rej(new Error(`ffmpeg exit ${c}`)))); });
console.log(`✓ ${OUTFILE} (${OUT_FRAMES} frames${NO_STAMPS ? "" : " chromed"}${LOOP_SEC > 0 ? ", seamless loop" : ""}, silent)`);
