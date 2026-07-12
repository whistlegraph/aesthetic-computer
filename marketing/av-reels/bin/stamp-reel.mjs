#!/usr/bin/env node
// stamp-reel.mjs — wrap a base av mp4 (video+audio from capture-av.mjs) in AC's
// side-stamp chrome: two purple-pals stamps hugging the left/right edges (4-pass
// seep, LED pulse on the audio's swells) + the piece name as climbing per-char
// title columns. Reel-only: no progress bar, no timecode.
//
// A piece-agnostic port of pop/menuband/bin/chrome-reel.mjs — single scene tint
// (sampled from the piece or passed via --tint r,g,b) instead of the menuband
// sim's per-slide meta. Re-muxes the base audio through unchanged.
//
//   node marketing/av-reels/bin/stamp-reel.mjs out/notepat/base-notepat.mp4 --title notepat
//   node .../stamp-reel.mjs <base.mp4> --title bubble --tint 120,180,255 --out ~/Desktop/bubble-reel.mp4

import { spawn, spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { once } from "node:events";
import { createCanvas, loadImage } from "canvas";
import { prerenderTitleChars, decodeAudioMono, computeRmsEnvelope, spawnFFmpegEncode } from "../../../pop/lib/preview-shared.mjs";
import { buildPresses, drawFingers, drawCursorPoint } from "./finger-gesture.mjs";
import { buildRipples, applyRipples } from "./displace-ripple.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..", "..");

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[a.slice(2)] = n; i++; } else flags[a.slice(2)] = true;
  } else positional.push(a);
}
const BASE = resolve((positional[0] || "").replace(/^~/, process.env.HOME));
if (!BASE || !existsSync(BASE)) { console.error("usage: stamp-reel.mjs <base.mp4> --title NAME [--tint r,g,b] [--out path]"); process.exit(1); }
const TITLE = flags.title || basename(BASE).replace(/^base-/, "").replace(/\.mp4$/, "");
const OUT = resolve((flags.out || BASE.replace(/base-/, "").replace(/\.mp4$/, "-reel.mp4")).replace(/^~/, process.env.HOME));
mkdirSync(dirname(OUT), { recursive: true });
const assetsDir = `${dirname(BASE)}/chrome-assets`;
mkdirSync(assetsDir, { recursive: true });

const W = 1080, H = 1920, FPS = parseInt(flags.fps || 30, 10);
// total duration from the base mp4
const durProbe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", BASE], { encoding: "utf8" });
const TOTAL = parseFloat((durProbe.stdout || "0").trim()) || 10;
const FRAMES = Math.round(TOTAL * FPS);

// ── touch overlay: gesture track from capture.json (or --gestures path) ──────
const POINTER = flags.pointer || "cursor"; // cursor | ring | circle | paw | pixel | none
const DISPLACE = flags.displace !== undefined && flags.displace !== false && flags.displace !== "0";
const FINGER_TINT = flags["finger-tint"]
  ? flags["finger-tint"].split(",").map((n) => parseInt(n, 10))
  : (POINTER === "paw" ? [230, 205, 235] : [236, 240, 250]);
// The overlay lags the drawn interface a touch (input recorded before the frame
// shows it); lead the marker by CURSOR_LEAD seconds to line it up.
const CURSOR_LEAD = parseFloat(flags["cursor-lead"] ?? 0.12);
const CURSOR_SIZE = parseInt(flags["cursor-size"] || 104, 10);
let PRESSES = [];
let CURSOR_TRACK = null; // per-frame [{t,x,y,down}] — exact, no interpolation
{
  const gpath = flags.gestures ? flags.gestures.replace(/^~/, process.env.HOME) : `${dirname(BASE)}/capture.json`;
  if (!flags["no-fingers"] && existsSync(gpath)) {
    try {
      const cj = JSON.parse(readFileSync(gpath, "utf8"));
      PRESSES = buildPresses(cj.gestures || []);
      if (Array.isArray(cj.cursorTrack) && cj.cursorTrack.length && cj.cursorTrack[0].x != null) CURSOR_TRACK = cj.cursorTrack;
    } catch (e) {}
  }
}
// Cursor for output time T. The cursor track is only ~capture-fps dense, so we
// INTERPOLATE position between the bracketing samples — the reticle then moves
// smoothly at the output fps even though the base video (the piece) is choppier.
function cursorAt(T) {
  const f = CURSOR_TRACK; let lo = 0, hi = f.length - 1, ans = 0;
  while (lo <= hi) { const m = (lo + hi) >> 1; if (f[m].t <= T) { ans = m; lo = m + 1; } else hi = m - 1; }
  const a = f[ans], b = f[Math.min(f.length - 1, ans + 1)];
  if (b === a || b.t <= a.t) return { x: a.x, y: a.y, down: a.down };
  const u = Math.max(0, Math.min(1, (T - a.t) / (b.t - a.t)));
  return { x: a.x + (b.x - a.x) * u, y: a.y + (b.y - a.y) * u, down: a.down };
}

// ── substrate sharpen: snap the chunky pixels to razor-hard edges ────────────
// --sharpen D re-quantizes the piece to its D-scaled buffer grid (area-down →
// nearest-up = crisp blocks) then unsharps the edges. Default: auto from the
// capture's density (so density-4 capture → 4× hard pixels).
let SHARPEN = 0;
if (flags.sharpen !== undefined && flags.sharpen !== "0" && flags.sharpen !== false) {
  SHARPEN = flags.sharpen === true ? 4 : parseInt(flags.sharpen, 10) || 4;
} else if (flags.sharpen === undefined) {
  try { const cj = JSON.parse(readFileSync(`${dirname(BASE)}/capture.json`, "utf8")); SHARPEN = parseInt(cj.density, 10) || 0; } catch (e) {}
}

// ── scene tint: --tint r,g,b, else the piece's average color at a mid frame ──
let TINT;
if (flags.tint) TINT = flags.tint.split(",").map((n) => parseInt(n, 10));
else {
  const t = spawnSync("ffmpeg", ["-loglevel", "error", "-ss", String(TOTAL / 2), "-i", BASE, "-frames:v", "1",
    "-vf", "scale=1:1", "-f", "rawvideo", "-pix_fmt", "rgb24", "-"], { maxBuffer: 1 << 20 });
  const b = t.stdout;
  TINT = b && b.length >= 3 ? [b[0], b[1], b[2]] : [180, 180, 200];
}
// lift very-dark samples so the stamps don't vanish into a black piece
const lum = 0.2126 * TINT[0] + 0.7152 * TINT[1] + 0.0722 * TINT[2];
if (lum < 90) { const k = 90 / Math.max(lum, 1); TINT = TINT.map((v) => Math.min(255, Math.round(v * k))); }
console.log(`▸ stamp-reel "${TITLE}" · ${FRAMES} frames · tint ${TINT.join(",")} → ${OUT}`);

// ── audio envelope drives LED pulse + char bounce (flat if no audio track) ──
let env = new Array(Math.max(1, FRAMES)).fill(0);
try { const { audio, sr } = decodeAudioMono(BASE); env = computeRmsEnvelope(audio, sr, FPS, TOTAL); }
catch (e) { console.log("  (no audio track — flat LED pulse)"); }
const peak = env.reduce((m, v) => Math.max(m, v), 0.0001);
const envAt = (t) => (env[Math.max(0, Math.min(env.length - 1, Math.floor(t * FPS)))] ?? 0) / peak;

// ── side-stamp title: DYNAMIC — reflects the current command (commandTrack),
// tinted to its color, and flashes/shakes/bounces when the command changes ────
const COLOR_RGB = { red: [255, 40, 40], orange: [255, 165, 0], yellow: [235, 215, 60], green: [40, 175, 70], cyan: [40, 220, 220], blue: [70, 110, 255], purple: [165, 70, 205], white: [240, 240, 246], gray: [150, 150, 158], black: [130, 130, 145], brown: [165, 100, 45] };
function cmdColor(c) { if (!c) return [205, 205, 215]; if (COLOR_RGB[c]) return COLOR_RGB[c]; const m = String(c).match(/(\d+)\D+(\d+)\D+(\d+)/); return m ? [+m[1], +m[2], +m[3]] : [225, 225, 235]; }
function cmdDisplay(e) { if (!e || e.tool === "prompt" || e.cmd === "prompt") return "prompt"; return `${e.tool === "fill" ? "fill" : "line"} ${e.color}`; }
let COMMAND_TRACK = [];
try { const cj = JSON.parse(readFileSync(`${dirname(BASE)}/capture.json`, "utf8")); if (Array.isArray(cj.commandTrack)) COMMAND_TRACK = cj.commandTrack; } catch (e) {}
const cmdTimeline = COMMAND_TRACK.map((e) => ({ t: e.t, display: cmdDisplay(e), rgb: cmdColor(e.color) }));
if (!cmdTimeline.length || cmdTimeline[0].t > 0.1) cmdTimeline.unshift({ t: 0, display: TITLE, rgb: [205, 205, 215] });
const charSet = new Map();
for (const d of [...new Set(cmdTimeline.map((c) => c.display))]) charSet.set(d, await prerenderTitleChars({ text: d, ptSize: 96, palette: ["#FFFFFF"], shadowColor: null, assetsDir }));
const currentCmdAt = (t) => { let c = cmdTimeline[0]; for (const e of cmdTimeline) if (e.t <= t) c = e; return c; };
const flashAt = (t) => { let last = -999; for (const e of cmdTimeline) if (e.t <= t) last = e.t; const age = t - last, D = 0.75; return age >= 0 && age < D ? (1 - age / D) ** 1.4 : 0; };
console.log(`  ${cmdTimeline.length} command changes → dynamic side stamps`);

const PALS_S = 145, PALS_HALF = PALS_S / 2, PALS_EDGE_X = 64, CHARS_EDGE_X = PALS_EDGE_X + 12;
const CHAR_SCALE = 0.42;
const CHAR_SPAN = 360, BOUNCE_BUF = 26; // fixed nominal span so the pals don't jump per command
const LEFT_CHARS_CY = H * 0.78, RIGHT_CHARS_CY = H * 0.22;
const LEFT_PALS_CY = LEFT_CHARS_CY - CHAR_SPAN / 2 - BOUNCE_BUF - PALS_HALF;
const RIGHT_PALS_CY = RIGHT_CHARS_CY + CHAR_SPAN / 2 + BOUNCE_BUF + PALS_HALF;

const PALS_WM_SIZE = 212;
let palsImg = null, palsBlur = null;
{
  const svg = `${REPO}/system/public/purple-pals.svg`;
  const png = `${assetsDir}/pals-watermark.png`, blurPng = `${assetsDir}/pals-watermark-blur.png`;
  const r = spawnSync("rsvg-convert", ["-w", String(PALS_WM_SIZE * 2), "-h", String(PALS_WM_SIZE * 2), "-o", png, svg]);
  if (r.status === 0 && existsSync(png)) {
    palsImg = await loadImage(png);
    const rb = spawnSync("magick", [png, "-channel", "A", "-blur", "0x1.5", "+channel", blurPng]);
    palsBlur = (rb.status === 0 && existsSync(blurPng)) ? await loadImage(blurPng) : palsImg;
  }
}
if (!palsImg) throw new Error("pals watermark failed to rasterize (need rsvg-convert)");

// AC's real cursors for the "cursor" pointer style (precise=up, active=down).
let cursorImgs = null;
if (POINTER === "cursor") {
  cursorImgs = { size: CURSOR_SIZE };
  for (const name of ["precise", "active"]) {
    const svg = `${REPO}/system/public/aesthetic.computer/cursors/${name}.svg`;
    const png = `${assetsDir}/cursor-${name}.png`;
    const r = spawnSync("rsvg-convert", ["-w", "160", "-h", "160", "-o", png, svg]);
    if (r.status === 0 && existsSync(png)) cursorImgs[name] = await loadImage(png);
  }
  if (!cursorImgs.precise && !cursorImgs.active) { console.log("  ⚠ cursor svgs failed to rasterize"); cursorImgs = null; }
}
const EFF_POINTER = POINTER === "cursor" && !cursorImgs ? "ring" : POINTER;

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const wmCanvas = createCanvas(8, 8), wmCtx = wmCanvas.getContext("2d");
const charTintCanvas = createCanvas(2, 2), charTintCtx = charTintCanvas.getContext("2d");

const TINT_DEEPEN = 0.72;
// stamp color = the CURRENT command's color, blended toward white on a change-flash
function sectionTcRgb(t) {
  const c = currentCmdAt(t).rgb, f = flashAt(t);
  return c.map((v) => { const d = v * TINT_DEEPEN; return Math.round(d + (255 - d) * f * 0.75); });
}

function tintInto(cv, cctx, src, c) {
  const w = src.width, h = src.height;
  cv.width = w; cv.height = h;
  cctx.globalCompositeOperation = "source-over"; cctx.clearRect(0, 0, w, h); cctx.drawImage(src, 0, 0);
  cctx.globalCompositeOperation = "source-in"; cctx.fillStyle = `rgb(${c[0]},${c[1]},${c[2]})`; cctx.fillRect(0, 0, w, h);
  cctx.globalCompositeOperation = "source-over"; return cv;
}
const palsTinted = (c, src) => tintInto(wmCanvas, wmCtx, src, c);
const tintCharGlyph = (img, c) => tintInto(charTintCanvas, charTintCtx, img, c);

function drawWatermark(audioT) {
  const u = audioT * FPS / FRAMES, TAU = Math.PI * 2, col = sectionTcRgb(audioT);
  const flash = flashAt(audioT);
  const s = PALS_S * (1 + flash * 0.3);              // scale bump on command change
  const e = Math.min(1, envAt(audioT)), glow = Math.max(e * e, flash);
  const ledCol = col.map((c) => Math.round(c + (255 - c) * glow * 0.6));
  const shk = flash * 22;                             // shake on change
  const wig = 13 * Math.sin(TAU * 24 * u + 0.7) + 4 * Math.sin(TAU * 9 * u) + shk * Math.sin(audioT * 61);
  const bob = 3 * Math.sin(TAU * 17 * u + 2.1) + 1.5 * Math.sin(TAU * 31 * u) + shk * Math.sin(audioT * 53 + 1);
  const swiv = 0.05 * Math.sin(TAU * 19 * u) + 0.025 * Math.sin(TAU * 38 * u + 1.4) + flash * 0.2 * Math.sin(audioT * 47);
  const spots = [
    { cx: PALS_EDGE_X - wig, cy: LEFT_PALS_CY + bob, rot: Math.PI / 2 + swiv },
    { cx: W - PALS_EDGE_X + wig, cy: RIGHT_PALS_CY - bob, rot: -Math.PI / 2 - swiv },
  ];
  const passes = [["multiply", 0.78], ["color-burn", 0.42], ["overlay", 0.58], ["source-over", 0.06]];
  for (const sp of spots) {
    ctx.save(); ctx.translate(sp.cx, sp.cy); ctx.rotate(sp.rot);
    palsTinted([0, 0, 0], palsImg);
    ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.26;
    ctx.drawImage(wmCanvas, -s / 2 + 3, -s / 2 + 4, s, s);
    palsTinted(col, palsBlur);
    for (const [op, a] of passes) { ctx.globalCompositeOperation = op; ctx.globalAlpha = a; ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s); }
    ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.30; ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    if (glow > 0.001) {
      palsTinted(ledCol, palsBlur);
      ctx.globalCompositeOperation = "screen"; ctx.globalAlpha = 0.14 + 0.78 * glow; ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
      palsTinted(ledCol, palsImg);
      ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.18 + 0.46 * glow; ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    }
    ctx.restore();
  }
  ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 1;
  drawTitleChars(audioT);
}

function drawTitleChars(audioT) {
  const u = audioT * FPS / FRAMES, TAU = Math.PI * 2;
  const cmd = currentCmdAt(audioT), set = charSet.get(cmd.display); if (!set) return;
  const chars = set.chars, flash = flashAt(audioT), shk = flash * 16;
  const wig = 11 * Math.sin(TAU * 30 * u + 3.6) + 4 * Math.sin(TAU * 12 * u + 1.1) + shk * Math.sin(audioT * 58);
  const span = set.totalWidth * CHAR_SCALE, palsRgb = sectionTcRgb(audioT), startX = -span / 2;
  const spots = [
    { charsCx: CHARS_EDGE_X - wig, cy: LEFT_CHARS_CY, rot: Math.PI / 2 },
    { charsCx: W - CHARS_EDGE_X + wig, cy: RIGHT_CHARS_CY, rot: -Math.PI / 2 },
  ];
  for (const sp of spots) {
    ctx.save(); ctx.translate(sp.charsCx, sp.cy); ctx.rotate(sp.rot);
    const sc = 1 + flash * 0.2; ctx.scale(sc, sc);   // bounce/scale on change
    for (let i = 0; i < chars.length; i++) {
      const ch = chars[i]; if (!ch.img) continue;
      const x = startX + ch.prefixWidth * CHAR_SCALE, dw = ch.img.width * CHAR_SCALE, dh = ch.img.height * CHAR_SCALE;
      const charEnv = envAt(audioT - i * 0.03);
      const lift = 4 * Math.sin(audioT * 4.0 + i * 0.8) * (0.3 + charEnv) + flash * 11 * Math.sin(audioT * 42 + i), y = -dh / 2 + lift;
      const charRot = 0.03 * Math.sin(audioT * 3.1 + i * 1.15) + flash * 0.14 * Math.sin(audioT * 45 + i * 2);
      ctx.save(); ctx.translate(x + dw / 2, y + dh / 2); ctx.rotate(charRot); ctx.translate(-(x + dw / 2), -(y + dh / 2));
      const sh = 0.5 + 0.5 * Math.sin(audioT * 3.2 - i * 0.7);
      const charRgb = palsRgb.map((c) => Math.round(c + (255 - c) * sh * 0.35));
      ctx.save(); ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.26;
      ctx.drawImage(tintCharGlyph(ch.img, [0, 0, 0]), x + 3, y + 4, dw, dh); ctx.restore();
      for (const [op, a] of [["multiply", 0.78], ["color-burn", 0.42], ["overlay", 0.58], ["source-over", 0.06]]) {
        ctx.save(); ctx.globalCompositeOperation = op; ctx.globalAlpha = a; ctx.drawImage(tintCharGlyph(ch.img, charRgb), x, y, dw, dh); ctx.restore();
      }
      ctx.save(); ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.46; ctx.drawImage(tintCharGlyph(ch.img, palsRgb), x, y, dw, dh); ctx.restore();
      if (charEnv > 0.45) {
        ctx.save(); ctx.globalCompositeOperation = "screen"; ctx.globalAlpha = 0.14 + 0.46 * Math.min(1, (charEnv - 0.45) / 0.55);
        ctx.drawImage(tintCharGlyph(ch.img, charRgb), x, y, dw, dh); ctx.restore();
      }
      ctx.restore();
    }
    ctx.restore();
  }
}

// ── frame pump: base → canvas → chrome → encoder (re-muxes base audio) ──────
console.log(`▸ chroming ${FRAMES} frames · "${TITLE}" columns + pals seep${SHARPEN ? ` · substrate sharpen ×${SHARPEN}` : ""}${PRESSES.length ? ` · ${PRESSES.length} finger presses` : ""}`);
// substrate: area-downscale to the buffer grid then nearest-upscale = crisp fat
// pixels, then unsharp to pop the block edges. Runs on the piece only (chrome +
// fingers are drawn full-res afterward).
const fit = `scale=${W}:${H}:force_original_aspect_ratio=increase,crop=${W}:${H}`;
const substrate = SHARPEN > 1
  ? `,scale=${Math.round(W / SHARPEN)}:${Math.round(H / SHARPEN)}:flags=area,scale=${W}:${H}:flags=neighbor,unsharp=3:3:0.7:3:3:0.0`
  : "";
const dec = spawn("ffmpeg", ["-loglevel", "error", "-i", BASE, "-f", "rawvideo", "-pix_fmt", "rgba",
  "-vf", `${fit}${substrate},fps=${FPS}`, "-"], { stdio: ["ignore", "pipe", "inherit"] });
const enc = spawnFFmpegEncode({ audioPath: BASE, w: W, h: H, fps: FPS, outPath: OUT, crf: parseInt(flags.crf || 15, 10) });
const RIPPLES = DISPLACE ? buildRipples(PRESSES) : [];
const FRAME_BYTES = W * H * 4, fbuf = Buffer.alloc(FRAME_BYTES), img = ctx.createImageData(W, H);
const dbuf = DISPLACE ? new Uint8ClampedArray(FRAME_BYTES) : null;
let off = 0, fi = 0; const t0 = Date.now();
for await (const chunk of dec.stdout) {
  let cOff = 0;
  while (cOff < chunk.length) {
    const n = Math.min(FRAME_BYTES - off, chunk.length - cOff);
    chunk.copy(fbuf, off, cOff, cOff + n); off += n; cOff += n;
    if (off === FRAME_BYTES) {
      off = 0;
      // displacement warps the piece pixels (in place of a glyph); else glyph on top
      if (DISPLACE) { applyRipples(fbuf, dbuf, W, H, RIPPLES, fi / FPS); img.data.set(dbuf); }
      else img.data.set(fbuf);
      ctx.putImageData(img, 0, 0);
      // marker draws on TOP of the displacement (droplet + ripple together)
      if (POINTER !== "none") {
        if (EFF_POINTER === "cursor" && CURSOR_TRACK && cursorImgs) {
          // frame-EXACT: draw the recorded cursor for this exact frame (no interp)
          const c = cursorAt(fi / FPS);
          drawCursorPoint(ctx, c.x * W, c.y * H, c.down, cursorImgs, 1);
        } else if (PRESSES.length) {
          drawFingers(ctx, PRESSES, fi / FPS + CURSOR_LEAD, W, H, { style: EFF_POINTER, tint: FINGER_TINT, cursorImgs });
        }
      }
      drawWatermark(fi / FPS);
      if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
      fi++;
      if (fi % 120 === 0) console.log(`  ${fi}/${FRAMES} · ${((Date.now() - t0) / 1000).toFixed(0)}s`);
    }
  }
}
enc.stdin.end();
await new Promise((res, rej) => enc.on("close", (c) => (c === 0 ? res() : rej(new Error(`ffmpeg exit ${c}`)))));
console.log(`✓ ${OUT} (${fi} frames chromed)`);
