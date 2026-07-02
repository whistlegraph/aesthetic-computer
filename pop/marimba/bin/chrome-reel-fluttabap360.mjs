#!/usr/bin/env node
// marimba/bin/chrome-reel-fluttabap360.mjs — the fluttabap360
// METAMORPHOSIS REEL's side-stamp chrome: a port of chrome-reel.mjs
// (marimbaba's, itself ported from momboba/twofa/amaythingra). Two pals
// stamps hugging the left/right edges (rotated ±90°, looping wiggle +
// swivel, 4-pass multiply/burn/overlay SEEP so they stain INTO the
// colored-pencil paper, LED pulse on the music's swells), and
// "Fluttabap360" as per-character climbing columns — each char its own
// glyph with a phase-offset bounce, shimmer glint, and the same seep
// stack — all tinted per-beat from the metamorphosis palette
// (dawn-jade → oat → cream → sap → amber → rose → steel → indigo).
//
// REEL-ONLY: NO progress bar and NO timecode — Instagram's Reels UI
// brings its own furniture and @jeffrey wants reels with nothing under
// them. Pals stamps + title columns only.
//
// Runs as a post pass: gen-motion-fluttabap360-reel.mjs --assemble
// writes out/motion/base-fluttabap360-reel.mp4 + meta-fluttabap360-reel.json,
// this pipes every frame through node-canvas and re-encodes with audio.
//
// Usage: node pop/marimba/bin/chrome-reel-fluttabap360.mjs

import { spawn, spawnSync } from "node:child_process";
import { readFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { once } from "node:events";
import { createCanvas, loadImage } from "canvas";
import {
  prerenderTitleChars, decodeAudioMono, computeRmsEnvelope, spawnFFmpegEncode,
} from "../../lib/preview-shared.mjs";
import * as progress from "../../lib/render-progress.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const MOTION = `${LANE}/out/motion`;
const BASE = `${MOTION}/base-fluttabap360-reel.mp4`;
const META = JSON.parse(readFileSync(`${MOTION}/meta-fluttabap360-reel.json`, "utf8"));
const OUT = `${LANE}/out/fluttabap360-reel.mp4`;
const assetsDir = `${MOTION}/chrome-assets-fluttabap360-reel`;
mkdirSync(assetsDir, { recursive: true });

const W = 1080, H = 1920, FPS = 30;
const TOTAL = META.total;
const FRAMES = Math.round(TOTAL * FPS);
const TITLE = "Fluttabap360";

// per-beat tints — the metamorphosis arc of the story: dawn-jade crack →
// warming hatch/unfurl → sap-green first dance → brightest amber laptop
// marimba → rose molt → steel-blue hardening → deep starlit indigo.
const BEAT_TINTS = {
  cocoon:     [126, 168, 152], // cool pale dawn-jade
  crack:      [140, 178, 150], // dawn-jade, a shade warmer (the seam light)
  hatchpov:   [204, 176, 130], // warming oat (first sight)
  grow:       [222, 200, 158], // warm cream
  dance:      [168, 190, 110], // sap-green + amber
  growdance:  [195, 195, 134], // cream rolling into sap (the morph)
  laptops:    [240, 200, 120], // bright amber (brightest)
  mallets:    [244, 206, 128], // strike amber
  laptopspov: [238, 196, 116], // flow amber
  molt:       [232, 168, 140], // gold turning rose
  wing:       [170, 172, 176], // frost-metal grey-blue
  drone:      [140, 160, 190], // steel-blue + orange
  spacepov:   [104, 110, 168], // indigo through the lens-eye
  space:      [90, 96, 160],   // deep indigo (dark, starlit)
};

// ── audio envelope (drives LED pulse + char bounce) ────────────────────
const { audio, sr } = decodeAudioMono(BASE);
const env = computeRmsEnvelope(audio, sr, FPS, TOTAL);
const envAt = (t) => env[Math.max(0, Math.min(env.length - 1, Math.floor(t * FPS)))] ?? 0;

// ── title chars + pals rasters ─────────────────────────────────────────
const titleFontSize = 96;
const { chars: titleChars, totalWidth: titleTotalW } = await prerenderTitleChars({
  text: TITLE, ptSize: titleFontSize, palette: ["#FFFFFF"],
  shadowColor: null, assetsDir,
});
const PALS_S = 145;
const PALS_HALF = PALS_S / 2;
const PALS_EDGE_X = 64;
const CHARS_EDGE_X = PALS_EDGE_X + 12;
const CHAR_SCALE = 0.36; // 12 chars — between marimbaba's 9 (0.46) and momboba's 13
const CHAR_SPAN = titleTotalW * CHAR_SCALE;
const BOUNCE_BUF = 26;
const LEFT_CHARS_CY = H * 0.78;
const RIGHT_CHARS_CY = H * 0.22;
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

// beat tint brightened toward white by progress through the beat
function sectionTcRgb(t) {
  const s = META.slides.find((x) => t >= x.from && t < x.to) ?? META.slides.at(-1);
  let [r, g, b] = BEAT_TINTS[s.name] ?? [180, 180, 180];
  const lp = Math.max(0, Math.min(1, (t - s.from) / Math.max(0.001, s.to - s.from)));
  const k = 0.30 * lp;
  return [Math.round(r + (255 - r) * k), Math.round(g + (255 - g) * k), Math.round(b + (255 - b) * k)];
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
  const col = sectionTcRgb(audioT);            // one color per beat
  const e = Math.min(1, envAt(audioT));
  const glow = e * e;
  // LED pulse brightens the beat color toward white — no hue swing
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
  const palsRgb = sectionTcRgb(audioT);
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

// ── frame pump: base → canvas → chrome → encoder ───────────────────────
console.log(`▸ reel chrome pass · ${FRAMES} frames · "${TITLE}" columns + pals seep · no bar/timecode`);
progress.begin({ type: "video", label: `fluttabap360 reel chrome · ${FRAMES} frames` });
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
console.log(`✓ ${OUT} (${fi} frames chromed)`);
