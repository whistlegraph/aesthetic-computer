#!/usr/bin/env node
// captions-train.mjs — GENERIC phrase-karaoke caption layer. ONE sentence/phrase
// is shown at a time, held STILL while it's read; each word colors in
// left-to-right (character-by-character) in the slide's accent color as it is
// spoken; when the phrase finishes the WHOLE phrase slides up and the next rises
// in from below. "slide · speak · slide · speak." Plus the bottom segmented
// progress bar. Nothing moves while a word is being spoken.
//
// Usage: node marketing/bin/captions-train.mjs <campaign-dir> [--start S --frames N]
//
// NOTE: node-canvas/freetype cannot rasterize the YWFT "WebFont" TTF (renders
// .notdef tofu) → each word is pre-rendered to a PNG via ImageMagick (cream +
// per-slide-colored), drawn downscaled (crisp at any size).
import { createCanvas, loadImage } from "canvas";
import { readFileSync, mkdirSync, existsSync } from "node:fs";
import { spawn, spawnSync, execSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import * as progress from "../../pop/lib/render-progress.mjs";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const FONT = `${process.env.HOME}/Library/Fonts/ywft-processing-bold.ttf`;
const W = 1920, H = 1080, FPS = 30;

const argv = process.argv.slice(2);
const flag = (k) => { const i = argv.indexOf(k); return i >= 0 ? argv[i + 1] : null; };
const campArg = argv.find((a) => !a.startsWith("--"));
if (!campArg) { console.error("usage: captions-train.mjs <campaign-dir> [--start S --frames N]"); process.exit(2); }
const campDir = resolve(process.cwd(), campArg);
const { default: cfg } = await import(pathToFileURL(`${campDir}/campaign.mjs`).href);

const TMP = `/tmp/wsvideo-${cfg.name}`; mkdirSync(TMP, { recursive: true });
const WORDDIR = `${TMP}/words`; mkdirSync(WORDDIR, { recursive: true });
const OUT = `${TMP}/captions.mov`;
const CREAM = cfg.palette?.cream || "#fcf7c5";
const SHADOW = cfg.palette?.shadow || "#000000";
const DEF_COLOR = cfg.palette?.green || "#3dff88";

const C = {
  renderPx: 112, phrasePx: 76, maxLineW: 1740, wordGap: 26, lineH: 104,
  phraseDy: 132, bandY: 0.83, phraseSmooth: 0.20, fadeSpan: 1.2, lead: 0.18,
  ...(cfg.caption || {}),
};
const BAND_Y = Math.round(H * C.bandY);
const sc = C.phrasePx / C.renderPx;
const clamp01 = (t) => t < 0 ? 0 : t > 1 ? 1 : t;
const smoothstep = (t) => { t = clamp01(t); return t * t * (3 - 2 * t); };

// ── segments + per-slide colors (needed before word PNGs, to color each word) ─
const segs = JSON.parse(readFileSync(`${REPO}/recap/out/segments.json`, "utf8"));
const DURATIONsegs = Math.max(...segs.map((s) => s.endSec));
const COLORS = cfg.colors || {};
const hexRgb = (h) => { h = h.replace("#", ""); return [parseInt(h.slice(0, 2), 16), parseInt(h.slice(2, 4), 16), parseInt(h.slice(4, 6), 16)]; };
const segColorHex = (t) => { const s = segs.find((x) => t >= x.startSec && t < x.endSec) || segs[segs.length - 1]; return COLORS[s.name] || DEF_COLOR; };
const EXTRACTS = cfg.extracts || {};
const extractWins = segs.filter((s) => EXTRACTS[s.name]).map((s) => [s.startSec, s.endSec]);
const inExtract = (t) => extractWins.some(([a, b]) => t >= a && t < b);

// ── word PNG (ImageMagick, YWFT + baked hard shadow), cream + colored ────────
function wordPng(out, text, fill) {
  const oy = 7, ox = 6;
  const args = ["-background", "none", "-fill", fill, "-font", FONT, "-pointsize", String(C.renderPx),
    `label:${text}`, "(", "+clone", "-background", SHADOW, "-shadow", `100x0+${ox}+${oy}`, ")",
    "+swap", "-background", "none", "-layers", "merge", "+repage", out];
  const r = spawnSync("magick", args, { encoding: "utf8" });
  if (r.status !== 0) { console.error("magick failed:", text, r.stderr); process.exit(1); }
  const [w, h] = execSync(`magick identify -format "%w %h" ${out}`).toString().trim().split(" ").map(Number);
  return { w, h };
}

const raw = JSON.parse(readFileSync(`${REPO}/recap/out/words.json`, "utf8"));
const FIX = [[/^[“”'"`]+|[“”'"`]+$/g, ""], ...(cfg.textFixes || []).map(([p, r, f]) => [new RegExp(p, f || ""), r])];
const clean = (t) => FIX.reduce((s, [a, b]) => s.replace(a, b), t).trim();
const words = raw.map((w) => ({ text: clean(w.text), from: w.fromMs / 1000, to: w.toMs / 1000, rawText: w.text }))
  .filter((w) => w.text.length > 0);

console.log(`▸ [${cfg.name}] pre-rendering word PNGs (cream + per-slide color)…`);
const cache = new Map();
async function load(text, fill) {
  const key = `${text}|${fill}`;
  let e = cache.get(key);
  if (!e) {
    const safe = text.replace(/[^a-zA-Z0-9]/g, "_").slice(0, 20) + "_" + [...text + fill].reduce((a, c) => a + c.charCodeAt(0), 0);
    const png = `${WORDDIR}/${safe}.png`;
    const dim = existsSync(png) ? (() => { const [a, b] = execSync(`magick identify -format "%w %h" ${png}`).toString().trim().split(" ").map(Number); return { w: a, h: b }; })() : wordPng(png, text, fill);
    e = { ...dim, img: await loadImage(png) };
    cache.set(key, e);
  }
  return e;
}
for (const w of words) {
  w.color = segColorHex(w.from);
  const cr = await load(w.text, CREAM);
  const co = await load(w.text, w.color);
  w.imgCream = cr.img; w.imgColor = co.img; w.iw = cr.w; w.ih = cr.h; w.dw = cr.w * sc;
}

const DURATION = Math.max(words.length ? words[words.length - 1].to + 0.6 : 1, DURATIONsegs);

// ── bottom segmented progress bar ────────────────────────────────────
const BAR_H = 12, BAR_Y = H - BAR_H;
function drawProgressBar(ctx, t) {
  const playedX = Math.max(0, t) / DURATION * W;
  for (let i = 0; i < segs.length; i++) {
    const s = segs[i];
    const x0 = i === 0 ? 0 : (s.startSec / DURATION) * W;
    const x1 = i === segs.length - 1 ? W : (s.endSec / DURATION) * W;
    const [r, g, b] = hexRgb(COLORS[s.name] || DEF_COLOR);
    ctx.globalAlpha = 1;
    ctx.fillStyle = `rgba(${Math.round(r * 0.16)},${Math.round(g * 0.16)},${Math.round(b * 0.16)},0.85)`;
    ctx.fillRect(x0, BAR_Y, x1 - x0, BAR_H);
    const fx1 = Math.min(x1, playedX);
    if (fx1 > x0) { ctx.fillStyle = `rgba(${r},${g},${b},0.96)`; ctx.fillRect(x0, BAR_Y, fx1 - x0, BAR_H); }
    ctx.fillStyle = "rgba(255,253,242,0.35)"; ctx.fillRect(x1 - 1, BAR_Y, 1, BAR_H);
  }
}

// bottom-right timecode in YWFT — render the WHOLE "M:SS / M:SS" string per
// second as ONE ImageMagick image (correct YWFT kerning, pop-style), pick per
// frame. Windowed to the render range so previews stay fast.
const totMM = Math.floor(DURATION / 60), totSS = String(Math.floor(DURATION % 60)).padStart(2, "0");
const TC_PX = 56;
const _sf = flag("--start") ? Math.round(Number(flag("--start")) * FPS) : 0;
const _nf = flag("--frames") ? Number(flag("--frames")) : (Math.ceil(DURATION * FPS) - _sf);
const tcByS = {};
for (let s = Math.floor(_sf / FPS); s <= Math.min(Math.floor(DURATION), Math.ceil((_sf + _nf) / FPS)); s++) {
  const label = `${Math.floor(s / 60)}:${String(s % 60).padStart(2, "0")} / ${totMM}:${totSS}`;
  const png = `${WORDDIR}/tc_${s}.png`;
  if (!existsSync(png)) {
    const r = spawnSync("magick", ["-background", "none", "-fill", CREAM, "-font", FONT, "-pointsize", String(TC_PX), `label:${label}`,
      "(", "+clone", "-background", SHADOW, "-shadow", "100x0+4+5", ")", "+swap", "-background", "none", "-layers", "merge", "+repage", png], { encoding: "utf8" });
    if (r.status !== 0) { console.error("tc failed", label); process.exit(1); }
  }
  tcByS[s] = await loadImage(png);
}
function drawTimecode(ctx, t) {
  const img = tcByS[Math.min(Math.max(0, Math.floor(t)), Math.floor(DURATION))];
  if (!img) return;
  ctx.globalAlpha = 0.95;
  ctx.drawImage(img, W - img.width - 34, BAR_Y - 18 - img.height);
  ctx.globalAlpha = 1;
}

// ── group words into phrases (sentence / clause sized) + layout ──────
// group into phrases that ALWAYS fit ONE line: break on sentence end, or before
// a word would overflow maxLineW (never wrap to a second row).
const endRe = /[.?!]["”]?$/;
const phrases = [];
let cur = [], curW = 0;
for (const w of words) {
  if (cur.length && curW + C.wordGap + w.dw > C.maxLineW) { phrases.push(cur); cur = []; curW = 0; }
  curW += (cur.length ? C.wordGap : 0) + w.dw;
  cur.push(w);
  if (endRe.test(w.rawText.trim())) { phrases.push(cur); cur = []; curW = 0; }
}
if (cur.length) phrases.push(cur);
// single-line layout: centre each phrase horizontally
for (const ph of phrases) {
  const tot = ph.reduce((a, w) => a + w.dw, 0) + C.wordGap * (ph.length - 1);
  let x = -tot / 2;
  for (const w of ph) { w.cx = x + w.dw / 2; w.line = 0; x += w.dw + C.wordGap; }
  ph.nLines = 1;
  ph.start = ph[0].from;
}
// advance to a phrase slightly before its first word so the slide settles in the
// gap (the word is then read while STILL).
function targetIdx(t) { let i = 0; for (let k = 0; k < phrases.length; k++) if (t >= phrases[k].start - C.lead) i = k; return i; }

// ── render ───────────────────────────────────────────────────────────
const startFrame = flag("--start") ? Math.round(Number(flag("--start")) * FPS) : 0;
const nFrames = flag("--frames") ? Number(flag("--frames")) : (Math.ceil(DURATION * FPS) - startFrame);

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
ctx.imageSmoothingEnabled = true; ctx.imageSmoothingQuality = "high";

const ff = spawn("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-", "-c:v", "qtrle", OUT],
  { stdio: ["pipe", "inherit", "inherit"] });
console.log(`▸ phrase-karaoke · ${phrases.length} phrases · ${DURATION.toFixed(1)}s · ${nFrames} frames -> ${OUT}`);
progress.begin({ type: "video", label: `${cfg.name} captions · ${nFrames} frames` });

let frame = startFrame, phScroll = null;
function drawFrame() {
  const t = frame / FPS;
  ctx.clearRect(0, 0, W, H);

  if (!inExtract(t)) {
    const ti = targetIdx(t);
    phScroll = phScroll === null ? ti : phScroll + (ti - phScroll) * C.phraseSmooth;
    const lo = Math.max(0, Math.floor(phScroll) - 1), hi = Math.min(phrases.length - 1, Math.ceil(phScroll) + 1);
    for (let j = lo; j <= hi; j++) {
      const ph = phrases[j];
      const fade = smoothstep(1 - Math.abs(j - phScroll) / C.fadeSpan);
      if (fade <= 0.01) continue;
      const baseY = BAND_Y + (j - phScroll) * C.phraseDy;
      for (const w of ph) {
        const dh = w.ih * sc, x = W / 2 + w.cx, y = baseY + (w.line - (ph.nLines - 1) / 2) * C.lineH;
        const dx = x - w.dw / 2, dy = y - dh / 2;
        const spoken = t >= w.to, active = t >= w.from && t < w.to;
        if (spoken) {                       // fully read → accent color
          ctx.globalAlpha = 0.97 * fade; ctx.drawImage(w.imgColor, dx, dy, w.dw, dh);
        } else if (active) {                // colouring in, left→right (char by char)
          const p = clamp01((t - w.from) / Math.max(0.001, w.to - w.from));
          ctx.globalAlpha = fade; ctx.drawImage(w.imgCream, dx, dy, w.dw, dh);
          ctx.save(); ctx.beginPath(); ctx.rect(dx, dy, w.dw * p, dh); ctx.clip();
          ctx.drawImage(w.imgColor, dx, dy, w.dw, dh); ctx.restore();
        } else {                            // not yet read → dim cream
          ctx.globalAlpha = 0.32 * fade; ctx.drawImage(w.imgCream, dx, dy, w.dw, dh);
        }
      }
    }
    ctx.globalAlpha = 1;
  }
  drawProgressBar(ctx, t);
  drawTimecode(ctx, t);

  const ok = ff.stdin.write(Buffer.from(canvas.toBuffer("raw")));
  frame++;
  const done = frame - startFrame;
  progress.update((done / nFrames) * 100, { done, total: nFrames });
  if (frame >= startFrame + nFrames) { ff.stdin.end(); return; }
  if (ok) drawFrame(); else ff.stdin.once("drain", drawFrame);
}
ff.on("close", (code) => { progress.end(); console.log(code === 0 ? `✓ ${OUT}` : `✗ ffmpeg exit ${code}`); });
drawFrame();
