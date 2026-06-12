#!/usr/bin/env node
// chrome.mjs — the amaythingra side-stamp chrome, full throttle, for the
// 2FA Brush story. A verbatim port of drawWatermark + drawPalsTitleChars
// from pop/big-pictures/bin/preview-score.mjs: two pals stamps hugging the
// edges (rotated ±90°, looping wiggle + swivel, 4-pass multiply/burn/
// overlay SEEP so they stain INTO the picture, LED pulse on loud audio),
// and "Twofa" as per-character climbing columns — each char its own glyph
// with a phase-offset bounce, shimmer glint, and the same seep stack —
// all tinted per-slide and hue-cycled exactly like the reference.
//
// Runs as a post pass: build.mjs assembles build/base.mp4 (+ meta.json
// with the slide timings), this pipes every frame through node-canvas,
// draws the chrome with real blend modes, and re-encodes with the same
// audio. ~870 frames, fast.
//
// Usage: node chrome.mjs          (expects build/base.mp4 + build/meta.json)

import { spawn, spawnSync } from "node:child_process";
import { readFileSync, existsSync, copyFileSync, mkdirSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { once } from "node:events";
import { createCanvas, loadImage } from "canvas";
import {
  prerenderTitleChars, decodeAudioMono, computeRmsEnvelope, spawnFFmpegEncode,
  magickRenderText,
} from "../../../pop/lib/preview-shared.mjs";
import * as progress from "../../../pop/lib/render-progress.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const TMP = join(HERE, "build");
const BASE = join(TMP, "base.mp4");
const META = JSON.parse(readFileSync(join(TMP, "meta.json"), "utf8"));
const OUT = join(HERE, "2fa-brush-story.mp4");
const assetsDir = join(TMP, "chrome-assets");
mkdirSync(assetsDir, { recursive: true });

const W = 1080, H = 1920, FPS = 30;
const TOTAL = META.total;
const FRAMES = Math.round(TOTAL * FPS);
const TITLE = "Twofa";

// per-slide tints (the PANEL_TINTS of this ad) — each slide's accent color
const SLIDE_TINTS = {
  brush: [47, 158, 143],          // mint-teal handle
  auth: [104, 134, 196],          // cornflower LED
  coding: [154, 176, 38],         // citrus Neo
  notepat: [224, 122, 95],        // coral ring
  "brush-close": [47, 158, 143],
  sheet: [214, 84, 140],          // pals pink
};

// ── audio envelope (drives LED pulse + char bounce) ───────────────────
const { audio, sr } = decodeAudioMono(BASE);
const env = computeRmsEnvelope(audio, sr, FPS, TOTAL);
const envAt = (t) => env[Math.max(0, Math.min(env.length - 1, Math.floor(t * FPS)))] ?? 0;

// ── title chars + pals rasters (same prep as the reference) ───────────
const titleFontSize = 96;
const { chars: titleChars, totalWidth: titleTotalW } = await prerenderTitleChars({
  text: TITLE, ptSize: titleFontSize, palette: ["#FFFFFF"],
  shadowColor: null, assetsDir,
});
const PALS_S = 145;
const PALS_HALF = PALS_S / 2;
const PALS_EDGE_X = 64;
const CHARS_EDGE_X = PALS_EDGE_X + 12;
const CHAR_SCALE = 0.52;
const CHAR_SPAN = titleTotalW * CHAR_SCALE;
const BOUNCE_BUF = 26;
const LEFT_CHARS_CY = H * 0.82 - 16;
const RIGHT_CHARS_CY = H * 0.18 + 48;
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
if (!palsImg) throw new Error("pals watermark failed to rasterize");

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const wmCanvas = createCanvas(8, 8);
const wmCtx = wmCanvas.getContext("2d");
const charTintCanvas = createCanvas(2, 2);
const charTintCtx = charTintCanvas.getContext("2d");

// slide tint brightened toward white by progress through the slide
function sectionTcRgb(t) {
  const s = META.slides.find((x) => t >= x.from && t < x.to) ?? META.slides.at(-1);
  let [r, g, b] = SLIDE_TINTS[s.name] ?? [180, 180, 180];
  const lp = Math.max(0, Math.min(1, (t - s.from) / Math.max(0.001, s.to - s.from)));
  const k = 0.30 * lp;
  return [Math.round(r + (255 - r) * k), Math.round(g + (255 - g) * k), Math.round(b + (255 - b) * k)];
}
// ONE color per panel (no hue cycling) — the tint changes at slide cuts
// and brightens through each slide, like the reference's per-panel tints.
function palsFrameColor(t) {
  return sectionTcRgb(t);
}
function palsTinted(rgb, src) {
  const ww = src.width, wh = src.height;
  wmCanvas.width = ww; wmCanvas.height = wh;
  wmCtx.clearRect(0, 0, ww, wh);
  wmCtx.globalCompositeOperation = "source-over";
  wmCtx.drawImage(src, 0, 0);
  wmCtx.globalCompositeOperation = "source-in";
  wmCtx.fillStyle = `rgb(${rgb[0]},${rgb[1]},${rgb[2]})`;
  wmCtx.fillRect(0, 0, ww, wh);
  wmCtx.globalCompositeOperation = "source-over";
  return wmCanvas;
}
function tintCharGlyph(img, rgb) {
  const w = img.width, h = img.height;
  charTintCanvas.width = w; charTintCanvas.height = h;
  charTintCtx.globalCompositeOperation = "source-over";
  charTintCtx.clearRect(0, 0, w, h);
  charTintCtx.drawImage(img, 0, 0);
  charTintCtx.globalCompositeOperation = "source-in";
  charTintCtx.fillStyle = `rgb(${rgb[0]},${rgb[1]},${rgb[2]})`;
  charTintCtx.fillRect(0, 0, w, h);
  charTintCtx.globalCompositeOperation = "source-over";
  return charTintCanvas;
}

function drawWatermark(audioT) {
  const s = PALS_S;
  const u = audioT * FPS / FRAMES;
  const TAU = Math.PI * 2;
  const col = sectionTcRgb(audioT);            // one color per panel
  const e = Math.min(1, envAt(audioT));
  const glow = e * e;
  // LED pulse brightens the panel color toward white — no hue swing
  const ledCol = [
    Math.round(col[0] + (255 - col[0]) * glow * 0.6),
    Math.round(col[1] + (255 - col[1]) * glow * 0.6),
    Math.round(col[2] + (255 - col[2]) * glow * 0.6),
  ];
  // the LOGO floats on its own rhythm — different harmonics + phase from
  // the char columns (gentle: jas flagged the earlier pass as too bouncy)
  const wig = 13 * Math.sin(TAU * 24 * u + 0.7) + 4 * Math.sin(TAU * 9 * u);
  const bob = 3 * Math.sin(TAU * 17 * u + 2.1) + 1.5 * Math.sin(TAU * 31 * u);
  const swiv = 0.05 * Math.sin(TAU * 19 * u) + 0.025 * Math.sin(TAU * 38 * u + 1.4);
  const spots = [
    { cx: PALS_EDGE_X - wig, cy: LEFT_PALS_CY + bob, rot: Math.PI / 2 + swiv },
    { cx: W - PALS_EDGE_X + wig, cy: RIGHT_PALS_CY - bob, rot: -Math.PI / 2 - swiv },
  ];
  const passes = [
    ["multiply", 0.78], ["color-burn", 0.42],
    ["overlay", 0.58], ["source-over", 0.06],
  ];
  for (const sp of spots) {
    ctx.save();
    ctx.translate(sp.cx, sp.cy);
    ctx.rotate(sp.rot);
    palsTinted([0, 0, 0], palsImg);
    ctx.globalCompositeOperation = "source-over";
    ctx.globalAlpha = 0.26;
    ctx.drawImage(wmCanvas, -s / 2 + 3, -s / 2 + 4, s, s);
    palsTinted(col, palsBlur);
    for (const [op, a] of passes) {
      ctx.globalCompositeOperation = op;
      ctx.globalAlpha = a;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    }
    ctx.globalCompositeOperation = "source-over";
    ctx.globalAlpha = 0.30;
    ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    if (glow > 0.001) {
      palsTinted(ledCol, palsBlur);
      ctx.globalCompositeOperation = "screen";
      ctx.globalAlpha = 0.14 + 0.78 * glow;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
      palsTinted(ledCol, palsImg);
      ctx.globalCompositeOperation = "source-over";
      ctx.globalAlpha = 0.18 + 0.46 * glow;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    }
    ctx.restore();
  }
  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
  drawPalsTitleChars(audioT);
}

function drawPalsTitleChars(audioT) {
  const u = audioT * FPS / FRAMES;
  const TAU = Math.PI * 2;
  // the WORD's own drift — distinct harmonics/phase from the pals wig
  const wig = 11 * Math.sin(TAU * 30 * u + 3.6) + 4 * Math.sin(TAU * 12 * u + 1.1);
  const span = titleTotalW * CHAR_SCALE;
  const palsRgb = palsFrameColor(audioT);
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
      const charEnv = envAt(audioT - i * 0.03);
      const lift = 4 * Math.sin(audioT * 4.0 + i * 0.8) * (0.3 + charEnv);
      const y = -dh / 2 + lift;
      // each char also swivels a hair on its own phase
      const charRot = 0.03 * Math.sin(audioT * 3.1 + i * 1.15);
      ctx.save();
      ctx.translate(x + dw / 2, y + dh / 2);
      ctx.rotate(charRot);
      ctx.translate(-(x + dw / 2), -(y + dh / 2));
      const sh = 0.5 + 0.5 * Math.sin(audioT * 3.2 - i * 0.7);
      const charRgb = [
        Math.round(palsRgb[0] + (255 - palsRgb[0]) * sh * 0.35),
        Math.round(palsRgb[1] + (255 - palsRgb[1]) * sh * 0.35),
        Math.round(palsRgb[2] + (255 - palsRgb[2]) * sh * 0.35),
      ];
      ctx.save();
      ctx.globalCompositeOperation = "source-over";
      ctx.globalAlpha = 0.26;
      ctx.drawImage(tintCharGlyph(ch.img, [0, 0, 0]), x + 3, y + 4, dw, dh);
      ctx.restore();
      const charPasses = [
        ["multiply", 0.78], ["color-burn", 0.42],
        ["overlay", 0.58], ["source-over", 0.06],
      ];
      for (const [op, a] of charPasses) {
        ctx.save();
        ctx.globalCompositeOperation = op;
        ctx.globalAlpha = a;
        ctx.drawImage(tintCharGlyph(ch.img, charRgb), x, y, dw, dh);
        ctx.restore();
      }
      ctx.save();
      ctx.globalCompositeOperation = "source-over";
      ctx.globalAlpha = 0.46;
      ctx.drawImage(tintCharGlyph(ch.img, palsRgb), x, y, dw, dh);
      ctx.restore();
      if (charEnv > 0.45) {
        ctx.save();
        ctx.globalCompositeOperation = "screen";
        ctx.globalAlpha = 0.14 + 0.46 * Math.min(1, (charEnv - 0.45) / 0.55);
        ctx.drawImage(tintCharGlyph(ch.img, charRgb), x, y, dw, dh);
        ctx.restore();
      }
      ctx.restore();                       // close the per-char swivel
    }
    ctx.restore();
  }
}

// ── segmented progress bar — one tinted segment per slide, filled by
// playback, with the just-started slide blinking (port of amaythingra's
// drawProgressBar) ─────────────────────────────────────────────────────
const PROGRESS_BAR_H = 22, PROGRESS_BAR_Y = H - PROGRESS_BAR_H;   // flush to the bottom edge, amaythingra-exact
function drawProgressBar(audioT) {
  ctx.save();
  const playedX = Math.max(0, audioT) / TOTAL * W;
  const slides = META.slides;
  const lastI = slides.length - 1;
  let curI = slides.findIndex((s) => audioT >= s.from && audioT < s.to);
  if (curI < 0) curI = lastI;
  const sinceSlide = audioT - slides[curI].from;
  const blink = (sinceSlide >= 0 && sinceSlide < 0.22) ? (1 - sinceSlide / 0.22) : 0;
  for (let j = 0; j < slides.length; j++) {
    const P = slides[j];
    const x0 = j === 0 ? 0 : (P.from / TOTAL) * W;
    const x1 = j === lastI ? W : (P.to / TOTAL) * W;
    const [r, g, b] = SLIDE_TINTS[P.name] ?? [180, 180, 180];
    ctx.fillStyle = `rgba(${Math.round(r * 0.16)},${Math.round(g * 0.16)},${Math.round(b * 0.16)},0.85)`;
    ctx.fillRect(x0, PROGRESS_BAR_Y, x1 - x0, PROGRESS_BAR_H);
    const fx1 = Math.min(x1, playedX);
    if (fx1 > x0) {
      ctx.fillStyle = `rgba(${r},${g},${b},0.96)`;
      ctx.fillRect(x0, PROGRESS_BAR_Y, fx1 - x0, PROGRESS_BAR_H);
    }
    ctx.fillStyle = "rgba(255,253,242,0.35)";
    ctx.fillRect(x1 - 1, PROGRESS_BAR_Y, 1, PROGRESS_BAR_H);
    if (blink > 0 && j === curI) {
      ctx.fillStyle = `rgba(255,255,250,${(blink * 0.7).toFixed(3)})`;
      ctx.fillRect(x0, PROGRESS_BAR_Y, x1 - x0, PROGRESS_BAR_H);
    }
  }
  ctx.restore();
}

// ── timecode — YWFT mm:ss / total above the bar, panel-tinted, gentle
// env bounce (amaythingra's drawTimecode; strings prerendered up front
// since the frame loop is synchronous) ─────────────────────────────────
const tcFontSize = 60;
const totMm = Math.floor(TOTAL / 60), totSs = String(Math.floor(TOTAL % 60)).padStart(2, "0");
const tcCache = new Map();
for (let sec = 0; sec <= Math.ceil(TOTAL); sec++) {
  const mm = Math.floor(sec / 60), ss = String(sec - mm * 60).padStart(2, "0");
  const text = `${mm}:${ss} / ${totMm}:${totSs}`;
  const safe = text.replace(/[^0-9]/g, "_");
  tcCache.set(text, {
    img: await magickRenderText(text, {
      ptSize: tcFontSize, fill: "rgba(255,253,242,0.97)", outPath: `${assetsDir}/tc.${safe}.png`,
    }),
    shadow: await magickRenderText(text, {
      ptSize: tcFontSize, fill: "rgba(0,0,0,1)", outPath: `${assetsDir}/tc.${safe}.shadow.png`,
    }),
  });
}
const tcTint = createCanvas(8, 8), tcTintCtx = tcTint.getContext("2d");
function drawTimecode(audioT) {
  const sec = Math.min(Math.max(0, Math.floor(audioT)), Math.ceil(TOTAL));
  const mm = Math.floor(sec / 60), ss = String(sec - mm * 60).padStart(2, "0");
  const entry = tcCache.get(`${mm}:${ss} / ${totMm}:${totSs}`);
  if (!entry) return;
  const { img, shadow } = entry;
  const e = envAt(audioT);
  const x = W - img.width - 32;
  const y = PROGRESS_BAR_Y - img.height - 14 - 4 * e;
  ctx.save();
  ctx.globalAlpha = 0.95;
  ctx.drawImage(shadow, x + 3, y + 4);
  ctx.drawImage(shadow, x + 2, y + 3);
  ctx.globalAlpha = 1;
  const [tr, tg, tb] = sectionTcRgb(audioT);
  tcTint.width = img.width; tcTint.height = img.height;
  tcTintCtx.clearRect(0, 0, img.width, img.height);
  tcTintCtx.globalCompositeOperation = "source-over";
  tcTintCtx.drawImage(img, 0, 0);
  tcTintCtx.globalCompositeOperation = "source-in";
  tcTintCtx.fillStyle = `rgb(${tr},${tg},${tb})`;
  tcTintCtx.fillRect(0, 0, img.width, img.height);
  ctx.drawImage(tcTint, x, y);
  ctx.restore();
}

// ── frame pump: base.mp4 → canvas → chrome → encoder ──────────────────
console.log(`▸ chrome pass · ${FRAMES} frames · "${TITLE}" columns + pals seep`);
// heartbeat → ~/.ac-pop-renders → Slab menubar bar + the launching Claude
// session's pink `rendering` state
progress.begin({ type: "video", label: `2fa-brush story chrome · ${FRAMES} frames` });
const dec = spawn("ffmpeg", ["-loglevel", "error", "-i", BASE,
  "-f", "rawvideo", "-pix_fmt", "rgba", "-"], { stdio: ["ignore", "pipe", "inherit"] });
const enc = spawnFFmpegEncode({ audioPath: BASE, w: W, h: H, fps: FPS, outPath: OUT, crf: 18 });
const FRAME_BYTES = W * H * 4;
const fbuf = Buffer.alloc(FRAME_BYTES);
const img = ctx.createImageData(W, H);
let off = 0, fi = 0;
const t0 = Date.now();
for await (const chunk of dec.stdout) {
  let cOff = 0;
  while (cOff < chunk.length) {
    const n = Math.min(FRAME_BYTES - off, chunk.length - cOff);
    chunk.copy(fbuf, off, cOff, cOff + n);
    off += n; cOff += n;
    if (off === FRAME_BYTES) {
      off = 0;
      img.data.set(fbuf);
      ctx.putImageData(img, 0, 0);
      drawWatermark(fi / FPS);
      drawProgressBar(fi / FPS);
      drawTimecode(fi / FPS);
      if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
      fi++;
      progress.update((fi / FRAMES) * 100, { done: fi, total: FRAMES });
      if (fi % 200 === 0) console.log(`  ${fi}/${FRAMES} · ${((Date.now() - t0) / 1000).toFixed(0)}s`);
    }
  }
}
enc.stdin.end();
await new Promise((res, rej) => { enc.on("close", (c) => (c === 0 ? res() : rej(new Error(`ffmpeg exit ${c}`)))); });
progress.end();
copyFileSync(OUT, join(homedir(), "Desktop", "2fa-brush-story.mp4"));
console.log(`✓ ${OUT} (${fi} frames chromed) → ~/Desktop/2fa-brush-story.mp4`);
