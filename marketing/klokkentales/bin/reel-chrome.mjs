#!/usr/bin/env node
// reel-chrome.mjs — dress a dubbed Klokkentales reel in AC's pop chrome:
// two pals watermarks hugging the edges (VHS chroma ghosts, pulsing with the
// speech envelope), a drippy melt-feedback layer + chroma fringe + scanlines
// over the footage, and karaoke subtitles in YWFT where the word being spoken
// pops lime-green. Ported from marketing/kidlisp-reels/bin/render-reel.mjs /
// pop/menuband/bin/chrome-reel.mjs.
//
// Inputs (in --dir): reel-en-dub.mp4, dub-audio.mp3, words-en.json
// Output: <dir>/<name>-chrome.mp4 (voice + --music bed mixed under it)
//
// Usage:
//   node marketing/klokkentales/bin/reel-chrome.mjs \
//     --dir marketing/klokkentales/out/reels/1000-lyttere \
//     --music pop/marimba/out/marimbaba.mp3 --name 1000-lyttere

import { spawn, spawnSync } from "node:child_process";
import { readFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { once } from "node:events";
import { createCanvas, loadImage } from "canvas";
import {
  decodeAudioMono, computeRmsEnvelope,
  magickMeasureWidth, magickRenderText,
  setPreviewFont, prerenderTitleChars,
} from "../../../pop/lib/preview-shared.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..");

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  if (argv[i].startsWith("--")) { flags[argv[i].slice(2)] = argv[i + 1]; i++; }
}
const DIR = resolve(flags.dir || ".");
const NAME = flags.name || "reel";
const MUSIC = flags.music ? resolve(flags.music) : null;
const MUSIC_VOL = parseFloat(flags["music-vol"] || "0.15");

const VIDEO = `${DIR}/reel-en-dub.mp4`;
const VOICE = `${DIR}/dub-audio.mp3`;
const WORDS_JSON = `${DIR}/words-en.json`;
const OUTFILE = `${DIR}/${NAME}-chrome.mp4`;
const assetsDir = `${DIR}/chrome-assets`;
mkdirSync(assetsDir, { recursive: true });

// ── source probe ─────────────────────────────────────────────────────────────
function probe(args) {
  const r = spawnSync("ffprobe", ["-v", "error", ...args], { encoding: "utf8" });
  return r.stdout.trim();
}
const [SRC_W, SRC_H] = probe(["-select_streams", "v", "-show_entries",
  "stream=width,height", "-of", "csv=p=0", VIDEO]).split(",").map(Number);
const DUR = parseFloat(probe(["-show_entries", "format=duration", "-of",
  "default=noprint_wrappers=1:nokey=1", VIDEO]));
const FPS = 30;
const FRAMES = Math.round(DUR * FPS);
const W = 1080, H = 1920;

// ── karaoke lines from word timestamps ───────────────────────────────────────
// Greedy-wrap words into short lines, pair lines into 2-line blocks. The block
// on screen follows the voice; the word being spoken tints lime + pops.
// MacPal's playful register: Comic Sans MS Bold for subs + the laklok columns
// (same face MacPal titles and the macneopolitan visuals use).
setPreviewFont("/System/Library/Fonts/Supplemental/Comic Sans MS Bold.ttf");

const PT = 58;
const MAX_LINE_W = 680;
const SPACE_W = Math.round(PT * 0.34);
const rawWords = JSON.parse(readFileSync(WORDS_JSON, "utf8")).words
  .filter((w) => w.type === "word");

const widthCache = new Map();
function wordWidth(text) {
  if (!widthCache.has(text)) widthCache.set(text, magickMeasureWidth(text, PT));
  return widthCache.get(text);
}

const lines = [];
{
  let line = null;
  for (const w of rawWords) {
    const ww = wordWidth(w.text);
    const gap = line ? w.start - line.words[line.words.length - 1].end : 0;
    if (!line || line.w + SPACE_W + ww > MAX_LINE_W || gap > 0.7 || line.words.length >= 5) {
      line = { words: [], w: -SPACE_W };
      lines.push(line);
    }
    line.words.push(w);
    line.w += SPACE_W + ww;
  }
}
const blocks = [];
for (let i = 0; i < lines.length; i += 2) {
  const pair = lines.slice(i, i + 2);
  blocks.push({
    lines: pair,
    t0: pair[0].words[0].start,
    t1: pair[pair.length - 1].words[pair[pair.length - 1].words.length - 1].end,
  });
}
for (let i = 0; i < blocks.length; i++) {
  blocks[i].show = Math.max(0, blocks[i].t0 - 0.15);
  blocks[i].hide = i + 1 < blocks.length
    ? Math.min(blocks[i].t1 + 0.6, blocks[i + 1].t0 - 0.04)
    : Math.min(DUR, blocks[i].t1 + 0.8);
}
console.log(`▸ ${rawWords.length} words → ${lines.length} lines → ${blocks.length} blocks`);

// ── prerender word glyphs (YWFT, white; tinted per frame) ────────────────────
// three variants per word — MacPal comic treatment: white/ink-stroked for
// inactive, lime/ink-stroked for the word being spoken, plain for ghost splits.
const LIME = "#B4F046", INK = "rgb(18,18,18)";
const wordImgs = new Map();
for (const w of rawWords) {
  if (wordImgs.has(w.text)) continue;
  const safe = w.text.replace(/[^a-z0-9]/gi, "_") + "_" + wordImgs.size;
  wordImgs.set(w.text, {
    plain: await magickRenderText(w.text, {
      ptSize: PT, fill: "#FFFFFF", outPath: `${assetsDir}/w_${safe}.png`,
    }),
    stroked: await magickRenderText(w.text, {
      ptSize: PT, fill: "#FFFFFF", stroke: INK, strokeWidth: 3,
      outPath: `${assetsDir}/ws_${safe}.png`,
    }),
    lime: await magickRenderText(w.text, {
      ptSize: PT, fill: LIME, stroke: INK, strokeWidth: 3,
      outPath: `${assetsDir}/wl_${safe}.png`,
    }),
  });
}
rawWords.forEach((w, i) => { w.idx = i; });

// MacPal's per-glyph jitter (PalCore.swift): deterministic tiny dy + rotation,
// scaled up per word for reel distance.
function jitter(i) {
  let h = 2166136261 >>> 0;
  for (const b of Buffer.from("word" + i)) { h = (h ^ b) >>> 0; h = Math.imul(h, 16777619) >>> 0; }
  const dy = ((h % 7) / 2 - 1.5) * 2;
  const rot = ((((h >>> 8) % 9)) - 4) * 0.6 * Math.PI / 180;
  return { dy, rot };
}
console.log(`▸ ${wordImgs.size} distinct word glyphs prerendered`);

// ── pals watermark raster ────────────────────────────────────────────────────
const PALS_S = 150, PALS_EDGE_X = 105;
const LEFT_PALS_CY = H * 0.14, RIGHT_PALS_CY = H * 0.86;
const PALS_SPEED = 0.18;
let palsImg = null, palsBlur = null;
{
  const svg = `${REPO}/system/public/purple-pals.svg`;
  const png = `${assetsDir}/pals-watermark.png`;
  const blurPng = `${assetsDir}/pals-watermark-blur.png`;
  const r = spawnSync("rsvg-convert", ["-w", "424", "-h", "424", "-o", png, svg]);
  if (r.status === 0 && existsSync(png)) {
    palsImg = await loadImage(png);
    const rb = spawnSync("magick", [png, "-channel", "A", "-blur", "0x1.5", "+channel", blurPng]);
    palsBlur = (rb.status === 0 && existsSync(blurPng)) ? await loadImage(blurPng) : palsImg;
  }
}
if (!palsImg) throw new Error("pals watermark failed to rasterize (need rsvg-convert)");

// 'laklok' climbing char columns next to each stamp, like the kidlisp reels'
// $code columns — set in YWFT (the brand face), NOT the comic subtitle face.
const TITLE = "laklok";
setPreviewFont("ywft");
const { chars: titleChars, totalWidth: titleTotalW } = await prerenderTitleChars({
  text: TITLE, ptSize: 72, palette: ["#FFFFFF"], shadowColor: null, assetsDir,
});
setPreviewFont("/System/Library/Fonts/Supplemental/Comic Sans MS Bold.ttf");
const CHAR_SCALE = 0.8, KERN = 2, CHAR_BUF = 34;
const CHAR_SPAN = (titleTotalW + (titleChars.length - 1) * KERN) * CHAR_SCALE;
const CHARS_EDGE_X = PALS_EDGE_X + 6;


// ── speech envelope drives the pals pulse ────────────────────────────────────
const { audio, sr } = decodeAudioMono(VOICE);
const env = computeRmsEnvelope(audio, sr, FPS, DUR);

// ── canvases ─────────────────────────────────────────────────────────────────
const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const stage = createCanvas(W, H);          // current video frame at reel size
const stageCtx = stage.getContext("2d");
const tintCanvas = createCanvas(W, H);     // full-frame chroma-fringe tints
const tintCtx = tintCanvas.getContext("2d");
const dripA = createCanvas(W, H), dripB = createCanvas(W, H);
let dripFront = dripA, dripBack = dripB;
const wmCanvas = createCanvas(8, 8);
const wmCtx = wmCanvas.getContext("2d");
const glyphTint = createCanvas(2, 2);
const glyphTintCtx = glyphTint.getContext("2d");

// scanlines pattern (1px dark line every 4px)
const scan = createCanvas(W, H + 8);
{
  const sctx = scan.getContext("2d");
  sctx.fillStyle = "rgba(0,0,0,1)";
  for (let y = 0; y < H + 8; y += 4) sctx.fillRect(0, y, W, 1);
}
// vignette so the subs + stamps pop
const vig = createCanvas(W, H);
{
  const vctx = vig.getContext("2d");
  const g = vctx.createRadialGradient(W / 2, H / 2, H * 0.32, W / 2, H / 2, H * 0.72);
  g.addColorStop(0, "rgba(0,0,0,0)");
  g.addColorStop(1, "rgba(0,0,0,0.42)");
  vctx.fillStyle = g;
  vctx.fillRect(0, 0, W, H);
}

// KidLisp-reel VHS palette: lime core, magenta/cyan ghosts.
const CORE = [180, 240, 70], CORE_HI = [225, 255, 150];
const CHROMA_R = [255, 60, 120], CHROMA_B = [70, 170, 255], CHROMA_PX = 3;

function tinted(src, c, cnv, cctx) {
  cnv.width = src.width; cnv.height = src.height;
  cctx.globalCompositeOperation = "source-over";
  cctx.clearRect(0, 0, src.width, src.height);
  cctx.drawImage(src, 0, 0);
  cctx.globalCompositeOperation = "source-in";
  cctx.fillStyle = `rgb(${c[0]},${c[1]},${c[2]})`;
  cctx.fillRect(0, 0, src.width, src.height);
  cctx.globalCompositeOperation = "source-over";
  return cnv;
}

// one glyph/stamp as a VHS sandwich: shadow + split ghosts + core + env glow
function drawChroma(sharp, soft, dx, dy, dw, dh, e, core = CORE, coreAlpha = 0.62, tintCnv = wmCanvas, tintCtx2 = wmCtx) {
  ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.22;
  ctx.drawImage(tinted(sharp, [8, 10, 6], tintCnv, tintCtx2), dx + 2.5, dy + 3, dw, dh);
  ctx.globalCompositeOperation = "screen";
  ctx.globalAlpha = 0.30; ctx.drawImage(tinted(soft, CHROMA_R, tintCnv, tintCtx2), dx - CHROMA_PX, dy, dw, dh);
  ctx.globalAlpha = 0.30; ctx.drawImage(tinted(soft, CHROMA_B, tintCnv, tintCtx2), dx + CHROMA_PX, dy, dw, dh);
  ctx.globalAlpha = coreAlpha; ctx.drawImage(tinted(sharp, core, tintCnv, tintCtx2), dx, dy, dw, dh);
  if (e > 0.02) { ctx.globalAlpha = 0.08 + 0.20 * e; ctx.drawImage(tinted(soft, CORE_HI, tintCnv, tintCtx2), dx, dy, dw, dh); }
  ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 1;
}

function drawPals(t, e) {
  const TAU = Math.PI * 2, u = t / DUR, sp = PALS_SPEED, s = PALS_S;
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
    drawChroma(palsImg, palsBlur, -s / 2, -s / 2, s, s, e, CORE, 0.62);
    ctx.restore();
  }
  drawTitleChars(t, e);
}

// laklok columns: below the left stamp, above the right stamp, climbing the
// same edges, each char bouncing gently with the speech envelope.
function drawTitleChars(t, e) {
  const TAU = Math.PI * 2, u = t / DUR;
  const wig = 11 * Math.sin(TAU * 30 * PALS_SPEED * u + 3.6) + 4 * Math.sin(TAU * 12 * PALS_SPEED * u + 1.1);
  const leftCy = LEFT_PALS_CY + PALS_S / 2 + CHAR_BUF + CHAR_SPAN / 2;
  const rightCy = RIGHT_PALS_CY - PALS_S / 2 - CHAR_BUF - CHAR_SPAN / 2;
  const spots = [
    { cx: CHARS_EDGE_X - wig, cy: leftCy, rot: Math.PI / 2 },
    { cx: W - CHARS_EDGE_X + wig, cy: rightCy, rot: -Math.PI / 2 },
  ];
  const startX = -CHAR_SPAN / 2;
  for (const sp of spots) {
    ctx.save();
    ctx.translate(sp.cx, sp.cy);
    ctx.rotate(sp.rot);
    for (let i = 0; i < titleChars.length; i++) {
      const ch = titleChars[i];
      if (!ch.img) continue;
      const x = startX + (ch.prefixWidth + i * KERN) * CHAR_SCALE;
      const dw = ch.img.width * CHAR_SCALE, dh = ch.img.height * CHAR_SCALE;
      const lift = 3.5 * Math.sin(t * 1.5 + i * 0.8) * (0.6 + 0.25 * e);
      const yy = -dh / 2 + lift;
      const rot2 = 0.03 * Math.sin(t * 1.2 + i * 1.15);
      ctx.save();
      ctx.translate(x + dw / 2, yy + dh / 2);
      ctx.rotate(rot2);
      ctx.translate(-(x + dw / 2), -(yy + dh / 2));
      drawChroma(ch.img, ch.img, x, yy, dw, dh, e, CORE, 0.78, glyphTint, glyphTintCtx);
      ctx.restore();
    }
    ctx.restore();
  }
}

// karaoke block: lines centered in the frame; the active word tints lime,
// pops in scale, and gets the full ghost sandwich. Passed words solid white,
// upcoming words dimmed.
const LINE_H = Math.round(PT * 1.42);
function drawSubs(t, e) {
  const block = blocks.find((b) => t >= b.show && t < b.hide);
  if (!block) return;
  const blockH = block.lines.length * LINE_H;
  let y = (H - blockH) / 2;
  for (const line of block.lines) {
    let x = (W - line.w) / 2;
    for (const w of line.words) {
      const pair = wordImgs.get(w.text);
      if (!pair) { x += wordWidth(w.text) + SPACE_W; continue; }
      const active = t >= w.start && t < w.end;
      const passed = t >= w.end;
      const img = pair.plain;
      const dw = img.width, dh = img.height;
      const dy = y + (LINE_H - dh) / 2;
      // MacPal comic treatment: per-word jitter, ink stroke, hard accent
      // shadow. Active word flips to lime with a magenta shadow + scale pop.
      const j = jitter(w.idx);
      const cx = x + dw / 2, cy = dy + dh / 2;
      ctx.save();
      ctx.translate(cx, cy + j.dy);
      ctx.rotate(j.rot);
      if (active) {
        const pop = Math.exp(-(t - w.start) * 5);
        const sc = 1 + 0.14 * pop;
        ctx.scale(sc, sc);
      }
      ctx.translate(-cx, -cy);
      const s = active ? pair.lime : pair.stroked;
      const sx = x - (s.width - dw) / 2, sy = dy - (s.height - dh) / 2;
      const shadowColor = active ? CHROMA_R : CORE;
      ctx.globalAlpha = active ? 0.95 : 0.85;
      ctx.drawImage(tinted(s, shadowColor, glyphTint, glyphTintCtx), sx + 4, sy + 4.5, s.width, s.height);
      if (active) {
        ctx.globalCompositeOperation = "screen";
        ctx.globalAlpha = 0.35;
        ctx.drawImage(tinted(img, CHROMA_R, glyphTint, glyphTintCtx), x - 3, dy, dw, dh);
        ctx.drawImage(tinted(img, CHROMA_B, glyphTint, glyphTintCtx), x + 3, dy, dw, dh);
        ctx.globalCompositeOperation = "source-over";
      }
      ctx.globalAlpha = active || passed ? 1 : 0.8;
      ctx.drawImage(s, sx, sy, s.width, s.height);
      ctx.globalAlpha = 1;
      ctx.restore();
      x += dw + SPACE_W;
    }
    y += LINE_H;
  }
}

// ── decoder (rawvideo in) + encoder (rawvideo out) ───────────────────────────
const dec = spawn("ffmpeg", [
  "-hide_banner", "-loglevel", "error",
  "-i", VIDEO, "-f", "rawvideo", "-pix_fmt", "rgba",
  "-s", `${SRC_W}x${SRC_H}`, "-r", String(FPS), "-",
], { stdio: ["ignore", "pipe", "inherit"] });
const SILENT = `${DIR}/${NAME}-chrome-silent.mp4`;
const enc = spawn("ffmpeg", [
  "-hide_banner", "-loglevel", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
  // punch pass: crush blacks + saturate + sharpen the composited frame
  "-vf", "eq=contrast=1.15:brightness=-0.02:saturation=1.12,unsharp=5:5:0.6",
  "-c:v", "libx264", "-preset", "faster", "-crf", "18", "-threads", "0",
  "-pix_fmt", "yuv420p", "-movflags", "+faststart", SILENT,
], { stdio: ["pipe", "inherit", "inherit"] });

const FRAME_BYTES = SRC_W * SRC_H * 4;
const srcCanvas = createCanvas(SRC_W, SRC_H);
const srcCtx = srcCanvas.getContext("2d");
const srcImageData = srcCtx.createImageData(SRC_W, SRC_H);

let leftover = Buffer.alloc(0);
async function* frames() {
  for await (const chunk of dec.stdout) {
    leftover = leftover.length ? Buffer.concat([leftover, chunk]) : chunk;
    while (leftover.length >= FRAME_BYTES) {
      yield leftover.subarray(0, FRAME_BYTES);
      leftover = leftover.subarray(FRAME_BYTES);
    }
  }
}

console.log(`▸ klokkentales chrome · ${FRAMES} frames @ ${FPS}fps · drippy VHS + pals + karaoke`);
const t0ms = Date.now();
let i = 0;
for await (const raw of frames()) {
  const t = i / FPS;
  const e = env[Math.min(env.length - 1, i)];

  srcImageData.data.set(raw);
  srcCtx.putImageData(srcImageData, 0, 0);

  // stage = full-bleed video frame at reel size
  stageCtx.drawImage(srcCanvas, 0, 0, W, H);

  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
  ctx.drawImage(stage, 0, 0);

  // chroma fringe: magenta/cyan multiply-tinted copies split ±4px (screen)
  tintCtx.globalCompositeOperation = "source-over";
  tintCtx.drawImage(stage, 0, 0);
  tintCtx.globalCompositeOperation = "multiply";
  tintCtx.fillStyle = "rgb(255,60,120)"; tintCtx.fillRect(0, 0, W, H);
  ctx.globalCompositeOperation = "screen"; ctx.globalAlpha = 0.09;
  ctx.drawImage(tintCanvas, -4, 0);
  tintCtx.globalCompositeOperation = "source-over";
  tintCtx.drawImage(stage, 0, 0);
  tintCtx.globalCompositeOperation = "multiply";
  tintCtx.fillStyle = "rgb(70,170,255)"; tintCtx.fillRect(0, 0, W, H);
  ctx.drawImage(tintCanvas, 4, 0);
  ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 1;

  // drippy melt: feedback buffer falls + fades each frame, reseeded by the
  // current frame; screen-composited so highlights leave falling trails.
  const bc = dripBack.getContext("2d");
  bc.globalCompositeOperation = "copy";
  bc.globalAlpha = 0.90;
  bc.drawImage(dripFront, 0, 6);
  bc.globalCompositeOperation = "lighten";
  bc.globalAlpha = 0.45;
  bc.drawImage(stage, 0, 0);
  bc.globalCompositeOperation = "source-over";
  bc.globalAlpha = 1;
  ctx.globalCompositeOperation = "screen";
  ctx.globalAlpha = 0.17;
  ctx.drawImage(dripBack, 0, 0);
  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
  [dripFront, dripBack] = [dripBack, dripFront];

  // scanlines (slow roll) + vignette
  ctx.globalAlpha = 0.055;
  ctx.drawImage(scan, 0, -(Math.floor(t * 18) % 4));
  ctx.globalAlpha = 1;
  ctx.drawImage(vig, 0, 0);

  drawPals(t, e);
  drawSubs(t, e);

  if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
  i++;
  if (i % 90 === 0) console.log(`  ${i}/${FRAMES} · ${((Date.now() - t0ms) / 1000).toFixed(0)}s`);
}
enc.stdin.end();
await new Promise((res, rej) => { enc.on("close", (c) => (c === 0 ? res() : rej(new Error(`encode exit ${c}`)))); });
console.log(`✓ silent chrome: ${SILENT} (${i} frames)`);

// ── mux: voice + music bed (music low, faded out at the end) ─────────────────
const mixArgs = [
  "-hide_banner", "-loglevel", "error", "-y",
  "-i", SILENT, "-i", VOICE,
];
if (MUSIC) {
  // atrim clamps the mix to the video's exact length — otherwise the audio
  // track outlives the last frame by ~50ms and players flash black at the end.
  mixArgs.push("-i", MUSIC, "-filter_complex",
    `[2:a]volume=${MUSIC_VOL},afade=t=in:st=0:d=0.8,afade=t=out:st=${(DUR - 1.4).toFixed(2)}:d=1.4[m];` +
    `[1:a][m]amix=inputs=2:duration=first:normalize=0,atrim=end=${DUR.toFixed(3)}[a]`,
    "-map", "0:v", "-map", "[a]");
} else {
  mixArgs.push("-map", "0:v", "-map", "1:a", "-af", `atrim=end=${DUR.toFixed(3)}`);
}
mixArgs.push("-c:v", "copy", "-c:a", "aac", "-b:a", "192k", "-shortest", OUTFILE);
const mix = spawnSync("ffmpeg", mixArgs, { stdio: "inherit" });
if (mix.status !== 0) throw new Error("audio mux failed");
console.log(`✓ ${OUTFILE}`);
