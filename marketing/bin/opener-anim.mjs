#!/usr/bin/env node
// opener-anim.mjs — animated opening title card. A soft landscape gen (the
// "commons / planetary orchestra") pushes in behind a subtle scrim; the YWFT
// title is LIVE-rendered per frame and animates in CHARACTER-BY-CHARACTER (a
// staggered rise + fade) in cream with the AC HARD DROP SHADOW; the green brand
// dot bounces in; the subtitle fades up. Outputs opener.mp4 (video + bed audio).
//
// Usage: node marketing/bin/opener-anim.mjs <campaign-dir> [--open]
//
// YWFT can't rasterize in node-canvas → each glyph is pre-rendered via
// ImageMagick (face + shadow), drawn per frame.
import { createCanvas, loadImage } from "canvas";
import { mkdirSync, existsSync } from "node:fs";
import { spawn, execSync } from "node:child_process";
import { resolve } from "node:path";
import { pathToFileURL } from "node:url";
import * as progress from "../../pop/lib/render-progress.mjs";

const FONT = `${process.env.HOME}/Library/Fonts/ywft-processing-bold.ttf`;
const W = 1920, H = 1080, FPS = 30;

const campArg = process.argv.slice(2).find((a) => !a.startsWith("--"));
const campDir = resolve(process.cwd(), campArg || ".");
const { default: cfg } = await import(pathToFileURL(`${campDir}/campaign.mjs`).href);
const tc = cfg.titleCard || {};
const TMP = `/tmp/wsvideo-${cfg.name}`; mkdirSync(TMP, { recursive: true });
const GDIR = `${TMP}/opener_glyphs`; mkdirSync(GDIR, { recursive: true });
const OUT = `${TMP}/opener.mp4`;

const INK = tc.ink || "#fcf7c5", SHADOW = tc.shadow || "#06120c", DOT = tc.dot || "#3dff88", AMBER = tc.subInk || "#ffe7b0";
const titleParts = tc.title || cfg.wordmark || ["Aesthetic", "Computer"];
const sub = tc.subtitle || "";
const cx = tc.cx ?? Math.round(W / 2), titleY = tc.titleY ?? 150;
const titlePx = tc.titlePx ?? 116, subPx = tc.subPx ?? 54;
const DUR = tc.dur || 4, BGBLUR = tc.bgBlur ?? 4;
const SOX = 5, SOY = 6;                        // hard drop-shadow offset
const clamp01 = (t) => t < 0 ? 0 : t > 1 ? 1 : t;
const easeOut = (p) => 1 - Math.pow(1 - clamp01(p), 3);

// soft (fuzzy) landscape background
const gen = tc.gen ? resolve(campDir, tc.gen) : `${campDir}/title-card/gens/v1.png`;
if (!existsSync(gen)) { console.error(`✗ no title-card illustration at ${gen}`); process.exit(1); }
const bgPng = `${TMP}/opener_bg.png`;
execSync(`magick "${gen}" -resize 2300x -gaussian-blur 0x${BGBLUR} -modulate 94 "${bgPng}"`);
const bgImg = await loadImage(bgPng);

// ── glyphs (YWFT via ImageMagick, consistent line-height for shared baseline) ─
const gcache = new Map();
async function glyph(ch, px, fill) {
  const key = `${ch}|${px}|${fill}`;
  let e = gcache.get(key);
  if (!e) {
    const png = `${GDIR}/g_${ch.charCodeAt(0)}_${px}_${fill.replace("#", "")}.png`;
    if (!existsSync(png)) execSync(`magick -background none -fill "${fill}" -font "${FONT}" -pointsize ${px} label:"${ch}" "${png}"`);
    const [w, h] = execSync(`magick identify -format "%w %h" "${png}"`).toString().trim().split(" ").map(Number);
    e = { img: await loadImage(png), w, h };
    gcache.set(key, e);
  }
  return e;
}
// positioned chars (face + shadow), centred, with a tightening track
async function layout(str, px, fill, topY) {
  const track = Math.round(px * -0.13);
  const items = [];
  let total = 0;
  for (const ch of str) {
    if (ch === " ") { items.push({ space: true, w: Math.round(px * 0.30) }); total += Math.round(px * 0.30) + track; continue; }
    const g = await glyph(ch, px, fill), sg = await glyph(ch, px, SHADOW);
    items.push({ g, sg }); total += g.w + track;
  }
  total -= track;
  let x = Math.round(cx - total / 2);
  const out = [];
  for (const it of items) {
    if (it.space) { x += it.w + track; continue; }
    out.push({ img: it.g.img, shimg: it.sg.img, x, y: topY });
    x += it.g.w + track;
  }
  return out;
}

// measure a string's laid-out width, and lay it out left-to-right from startX
async function measure(str, px) {
  const track = Math.round(px * -0.13); let total = 0;
  for (const ch of str) {
    if (ch === " ") { total += Math.round(px * 0.30) + track; continue; }
    const g = await glyph(ch, px, INK); total += g.w + track;
  }
  return total - track;
}
async function layoutAt(str, px, fill, topY, startX) {
  const track = Math.round(px * -0.13); let x = startX; const out = [];
  for (const ch of str) {
    if (ch === " ") { x += Math.round(px * 0.30) + track; continue; }
    const g = await glyph(ch, px, fill), sg = await glyph(ch, px, SHADOW);
    out.push({ img: g.img, shimg: sg.img, x, y: topY }); x += g.w + track;
  }
  return out;
}

const [pa, pb] = titleParts;
// ONE-LINE wordmark "Aesthetic • Computer", auto-fit to the cartouche width
let tpx = titlePx, wa, wb, dotSide, hg, totalW;
const maxW = Math.round(W * 0.74);
for (let pass = 0; pass < 6; pass++) {
  wa = await measure(pa, tpx); wb = await measure(pb, tpx);
  dotSide = Math.round(tpx * 0.16); hg = Math.round(tpx * 0.30);
  totalW = wa + hg + dotSide + hg + wb;
  if (totalW <= maxW) break;
  tpx = Math.round(tpx * maxW / totalW * 0.99);
}
const lineH = Math.round(tpx * 1.18);
const startX = Math.round(cx - totalW / 2);
const l1 = await layoutAt(pa, tpx, INK, titleY, startX);
const dotX = startX + wa + hg;
const dotY = titleY + Math.round(tpx * 0.42);
const l2 = await layoutAt(pb, tpx, INK, titleY, dotX + dotSide + hg);
const subY = titleY + Math.round(tpx * 1.06) + Math.round(tpx * 0.34);
const subChars = sub ? await layout(sub, subPx, AMBER, subY) : [];

// cascade timing
const START = 0.7, STAG = 0.055, RISE = 26, ANIM = 0.42;
const seq = [];
let i = 0;
for (const c of l1) seq.push({ ...c, t0: START + (i++) * STAG });
const dotT0 = START + (i++) * STAG;
for (const c of l2) seq.push({ ...c, t0: START + (i++) * STAG });
const subStart = START + i * STAG + 0.15;
subChars.forEach((c, k) => seq.push({ ...c, t0: subStart + k * 0.022 }));

// ── render ───────────────────────────────────────────────────────────
const canvas = createCanvas(W, H), ctx = canvas.getContext("2d");
ctx.imageSmoothingQuality = "high";
const useBed = cfg.bed && existsSync(cfg.bed);
const ff = spawn("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
  ...(useBed ? ["-i", cfg.bed] : []),
  ...(useBed ? ["-filter_complex", `[1:a]atrim=0:${DUR},afade=t=in:st=0:d=1.4,afade=t=out:st=${(DUR - 0.6).toFixed(2)}:d=0.6,volume=0.6[a]`, "-map", "0:v", "-map", "[a]"] : ["-map", "0:v"]),
  "-t", String(DUR), "-c:v", "libx264", "-pix_fmt", "yuv420p", "-crf", "12", "-preset", "medium",
  ...(useBed ? ["-c:a", "aac", "-b:a", "192k"] : ["-an"]), "-movflags", "+faststart", OUT],
  { stdio: ["pipe", "inherit", "inherit"] });

console.log(`▸ animated opener · ${DUR}s · ${Math.round(DUR * FPS)} frames -> ${OUT}`);
const _openerTotal = Math.round(DUR * FPS);
progress.begin({ type: "video", label: `${cfg.name} opener · ${_openerTotal} frames` });
const scrimY = titleY + Math.round(tpx * 0.5);
let frame = 0;
function draw() {
  const t = frame / FPS;
  // background: soft landscape, cover-fill, slow push-in
  const z = 1 + 0.05 * easeOut(t / DUR);
  const scale = Math.max(W / bgImg.width, H / bgImg.height) * z;
  const gw = bgImg.width * scale, gh = bgImg.height * scale;
  ctx.globalAlpha = 1; ctx.drawImage(bgImg, (W - gw) / 2, (H - gh) / 2, gw, gh);
  // soft scrim behind the title to lift cream type off the bright sky
  const rg = ctx.createRadialGradient(cx, scrimY, 100, cx, scrimY, 820);
  rg.addColorStop(0, "rgba(6,18,12,0.40)"); rg.addColorStop(1, "rgba(6,18,12,0)");
  ctx.fillStyle = rg; ctx.fillRect(0, 0, W, H);

  // per-character title cascade — shadow then cream face
  for (const c of seq) {
    const p = easeOut((t - c.t0) / ANIM);
    if (p <= 0) continue;
    const yy = c.y + (1 - p) * RISE;
    ctx.globalAlpha = p * 0.82; ctx.drawImage(c.shimg, c.x + SOX, yy + SOY);
    ctx.globalAlpha = p; ctx.drawImage(c.img, c.x, yy);
  }
  // green brand dot — bounces in (with its own hard shadow)
  const dp = (t - dotT0) / 0.5;
  if (dp > 0) {
    const e = easeOut(dp), bounce = Math.sin(Math.min(1, dp) * Math.PI) * 12 * (1 - clamp01(dp));
    const dx = Math.round(dotX), dy = Math.round(dotY - bounce);
    ctx.globalAlpha = clamp01(e) * 0.8; ctx.fillStyle = SHADOW; ctx.fillRect(dx + SOX, dy + SOY, dotSide, dotSide);
    ctx.globalAlpha = clamp01(e); ctx.fillStyle = DOT; ctx.fillRect(dx, dy, dotSide, dotSide);
  }
  ctx.globalAlpha = 1;
  if (t < 0.6) { ctx.fillStyle = `rgba(0,0,0,${1 - t / 0.6})`; ctx.fillRect(0, 0, W, H); }

  const ok = ff.stdin.write(Buffer.from(canvas.toBuffer("raw")));
  progress.update((frame / _openerTotal) * 100, { done: frame, total: _openerTotal });
  if (++frame >= _openerTotal) { ff.stdin.end(); return; }
  if (ok) draw(); else ff.stdin.once("drain", draw);
}
ff.on("close", (code) => { progress.end(); console.log(code === 0 ? `✓ ${OUT}` : `✗ ffmpeg exit ${code}`); if (code === 0 && process.argv.includes("--open")) execSync(`open "${OUT}"`); });
draw();
