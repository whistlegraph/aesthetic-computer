#!/usr/bin/env node
// menuband/bin/sim.mjs — the Menu Band PROMO REEL simulation.
//
// A self-playing, deterministic macOS-look simulation of the Menu Band
// interface, choreographed to the /pop waltz (menuband-waltz.notes.json):
// the colorful menubar-piano keyboard plays the melody (keys light in the
// exact vivid palette from the app's About-screen icon), a guided tour
// walks the menubar → popover keyboard → About card flip → language map →
// end card. Graphics are LIFTED from the Swift About-screen geometry
// (AboutWindow.iconImage) so the reproduction matches the shipping app.
//
// Output: a chrome-less BASE at 1080×1920@30 muxed with the waltz, plus
// meta-menuband-reel.json (scene timings) — chrome-reel.mjs then stamps
// the AC pals + "menuband" title columns (NO progress bar; it's a reel).
//
// Usage: node pop/menuband/bin/sim.mjs
//   (run pop/menuband/bin/render-waltz.mjs first — it writes the audio +
//    notes the sim choreographs to.)

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { once } from "node:events";
import { spawnSync } from "node:child_process";
import { createCanvas, registerFont, loadImage } from "canvas";
import { spawnFFmpegEncode } from "../../lib/preview-shared.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const OUT = `${LANE}/out`;
const NOTES_PATH = `${OUT}/menuband-waltz.notes.json`;
const AUDIO = `${OUT}/menuband-waltz.mp3`;
const BASE = `${OUT}/base-menuband-reel.mp4`;

if (!existsSync(NOTES_PATH) || !existsSync(AUDIO)) {
  console.error(`✗ missing ${NOTES_PATH} or ${AUDIO} — run render-waltz.mjs first`);
  process.exit(1);
}

// macOS system font for an authentic menubar / UI look.
try { registerFont("/System/Library/Fonts/SFNS.ttf", { family: "MBSans" }); } catch {}
try { registerFont("/System/Library/Fonts/SFNSRounded.ttf", { family: "MBSansRounded" }); } catch {}

const W = 1080, H = 1920, FPS = 30;
const score = JSON.parse(readFileSync(NOTES_PATH, "utf8"));
const TOTAL = score.durationSec;
const FRAMES = Math.round(TOTAL * FPS);
const leadNotes = (score.notes || []).filter((n) => n.lane === "lead");

// ── the exact Menu Band icon keyboard palette (AboutWindow.swift) ──────────
// 5 chromatic keys C C♯ D D♯ E, each its own vivid tint.
const KEY_COLORS = [
  [255, 77, 107],   // C  red-pink
  [255, 153, 46],   // C♯ orange
  [255, 214, 56],   // D  yellow
  [51, 209, 179],   // D♯ teal
  [97, 158, 255],   // E  blue
];
const SQ_TL = [211, 80, 196], SQ_BR = [67, 29, 113];   // squircle gradient
const CREAM = [246, 242, 232], KEYBLACK = [27, 26, 25], EDGE = [40, 28, 22];
const AC_PURPLE = "rgb(167,139,250)";


// Real scannable QR to the landing page (qrencode → PNG → loaded once).
const QR_PNG = `${OUT}/qr-menuband.png`;
spawnSync("qrencode", ["-o", QR_PNG, "-s", "16", "-m", "1", "-l", "H", "https://prompt.ac/menuband"]);
const qrImg = existsSync(QR_PNG) ? await loadImage(QR_PNG) : null;

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const rgb = (a, al = 1) => `rgba(${a[0]},${a[1]},${a[2]},${al})`;

function roundRect(x, y, w, h, r) {
  const rr = Math.min(r, w / 2, h / 2);
  ctx.beginPath();
  ctx.moveTo(x + rr, y);
  ctx.arcTo(x + w, y, x + w, y + h, rr);
  ctx.arcTo(x + w, y + h, x, y + h, rr);
  ctx.arcTo(x, y + h, x, y, rr);
  ctx.arcTo(x, y, x + w, y, rr);
  ctx.closePath();
}

// ── the Menu Band app icon, drawn from scratch (AboutWindow.iconImage) ─────
// purple squircle + cream 5-key piano; litKeys (Set of 0..4) glow vivid.
function drawIcon(x, y, px, litKeys) {
  const R = (x0, y0, x1, y1) => [x + x0 * px, y + y0 * px, (x1 - x0) * px, (y1 - y0) * px];
  const margin = px * 0.098;
  const sq = [x + margin, y + margin, px - 2 * margin, px - 2 * margin];
  const radius = sq[2] * 0.2235;
  ctx.save();
  roundRect(sq[0], sq[1], sq[2], sq[3], radius);
  ctx.clip();
  const g = ctx.createLinearGradient(sq[0], sq[1], sq[0] + sq[2], sq[1] + sq[3]);
  g.addColorStop(0, rgb(SQ_TL)); g.addColorStop(1, rgb(SQ_BR));
  ctx.fillStyle = g; ctx.fillRect(sq[0], sq[1], sq[2], sq[3]);
  ctx.restore();

  // cream keyboard plate
  const plate = R(0.254, 0.428, 0.744, 0.570);
  roundRect(plate[0], plate[1], plate[2], plate[3], px * 0.012);
  ctx.fillStyle = rgb(CREAM); ctx.fill();

  // lit white keys
  const whites = [[0, 0.254, 0.417], [2, 0.417, 0.580], [4, 0.580, 0.744]];
  for (const [idx, x0, x1] of whites) {
    if (!litKeys.has(idx)) continue;
    ctx.fillStyle = rgb(KEY_COLORS[idx]);
    roundRect(x + x0 * px + 2, y + 0.428 * px + 2, (x1 - x0) * px - 4, 0.142 * px - 4, px * 0.01);
    ctx.fill();
  }
  // white-key dividers
  ctx.strokeStyle = rgb(EDGE); ctx.lineWidth = px * 0.006;
  for (const xf of [0.417, 0.580]) {
    ctx.beginPath();
    ctx.moveTo(x + xf * px, y + 0.432 * px);
    ctx.lineTo(x + xf * px, y + 0.566 * px);
    ctx.stroke();
  }
  // plate outline
  roundRect(plate[0], plate[1], plate[2], plate[3], px * 0.012);
  ctx.lineWidth = px * 0.008; ctx.strokeStyle = rgb(EDGE); ctx.stroke();
  // black keys
  const blacks = [[1, 0.369, 0.463], [3, 0.535, 0.629]];
  for (const [idx, x0, x1] of blacks) {
    ctx.fillStyle = litKeys.has(idx) ? rgb(KEY_COLORS[idx]) : rgb(KEYBLACK);
    roundRect(x + x0 * px, y + 0.428 * px, (x1 - x0) * px, 0.098 * px, px * 0.01);
    ctx.fill();
  }
}

// ── desktop wallpaper (soft AC purple gradient) ────────────────────────────
function drawDesktop() {
  const g = ctx.createLinearGradient(0, 0, W, H);
  g.addColorStop(0, "rgb(58,42,98)");
  g.addColorStop(0.5, "rgb(92,60,140)");
  g.addColorStop(1, "rgb(150,96,180)");
  ctx.fillStyle = g; ctx.fillRect(0, 0, W, H);
  // faint diagonal sheen
  const s = ctx.createLinearGradient(0, H, W, 0);
  s.addColorStop(0, "rgba(255,255,255,0)");
  s.addColorStop(0.6, "rgba(255,255,255,0.05)");
  s.addColorStop(1, "rgba(255,255,255,0)");
  ctx.fillStyle = s; ctx.fillRect(0, 0, W, H);
}

// ── macOS menubar — but the menu bar IS the keyboard: a live ROYGBIV
// piano (C4..B5) lives in it and lights per played note. The whole pitch
// of Menu Band, up top. ──────────────────────────────────────────────────
// A short, proper macOS menu bar holding the REAL Menu Band status item —
// captured pixel-exact from the Swift KeyboardIconRenderer via
// `MenuBand --render-menubar` (short keys + the dynamic icon on the right),
// one capture per distinct lit-note set, composited per frame.
const MBAR_H = 56;
const MENUBAR_BIN = `${REPO}/slab/menuband/.build/debug/MenuBand`;
const MB_CACHE = `${OUT}/menubar-frames`;
mkdirSync(MB_CACHE, { recursive: true });

// fold a melody MIDI note into the menubar piano's displayed range (C4..B5)
function foldToBar(m) { while (m > 83) m -= 12; while (m < 60) m += 12; return m; }
function barNotesAt(t) {
  const s = new Set();
  for (const n of leadNotes) if (t >= n.t && t < n.t + Math.max(0.12, n.dur)) s.add(foldToBar(n.midi));
  return [...s].sort((a, b) => a - b);
}
const barKey = (notes) => notes.join(",");
const menubarCache = new Map();   // note-key -> loaded image
let menubarIdle = null;

// Pre-render every distinct lit-note set the reel needs through the real app.
async function prerenderMenubars() {
  if (!existsSync(MENUBAR_BIN)) {
    console.warn(`  ⚠ MenuBand binary missing (${MENUBAR_BIN}) — run \`swift build\` in slab/menuband; menubar will be blank`);
    return;
  }
  const keys = new Set();
  for (let fi = 0; fi < FRAMES; fi++) keys.add(barKey(barNotesAt(fi / FPS)));
  console.log(`▸ capturing ${keys.size} real menubar graphics via MenuBand --render-menubar …`);
  for (const key of keys) {
    const png = `${MB_CACHE}/mb-${key === "" ? "idle" : key.replace(/,/g, "-")}.png`;
    if (!existsSync(png)) {
      const r = spawnSync(MENUBAR_BIN, ["--render-menubar", "--notes", key,
        "--voice", "Glockenspiel", "--scale", "4", "--out", png], { stdio: ["ignore", "ignore", "pipe"] });
      if (r.status !== 0) console.error(`  ✗ capture ${key}: ${r.stderr?.toString().slice(-160)}`);
    }
    if (existsSync(png)) menubarCache.set(key, await loadImage(png));
  }
  menubarIdle = menubarCache.get("") || [...menubarCache.values()][0] || null;
}

function drawMenubar(t) {
  ctx.fillStyle = "rgba(24,20,34,0.72)";
  ctx.fillRect(0, 0, W, MBAR_H);
  const cy = MBAR_H / 2;
  // Apple logo
  ctx.save();
  ctx.fillStyle = "rgba(255,255,255,0.95)";
  ctx.beginPath(); ctx.arc(34, cy, 11, 0, Math.PI * 2); ctx.fill();
  ctx.fillStyle = "rgba(24,20,34,0.96)";
  ctx.beginPath(); ctx.arc(40, cy - 4, 6.5, 0, Math.PI * 2); ctx.fill();
  ctx.restore();
  ctx.fillStyle = "rgba(255,255,255,0.96)"; ctx.textBaseline = "middle"; ctx.font = "700 22px MBSans";
  ctx.fillText("Menu Band", 56, cy + 1);
  // clock (right edge)
  ctx.textAlign = "right"; ctx.font = "500 21px MBSans"; ctx.fillStyle = "rgba(255,255,255,0.92)";
  ctx.fillText("9:41", W - 22, cy + 1); ctx.textAlign = "left";
  // the REAL captured menubar piano item, sitting left of the clock
  const img = menubarCache.get(barKey(barNotesAt(t))) || menubarIdle;
  if (img) {
    const ih = MBAR_H - 12, iw = ih * img.width / img.height;
    ctx.drawImage(img, W - 96 - iw, (MBAR_H - ih) / 2, iw, ih);
  }
}

// ── note-particle system (vector eighth-notes puffing up) ──────────────────
const particles = [];
function spawnNote(cx, cy, color) {
  particles.push({
    x: cx + (Math.sin(particles.length * 1.7) * 24), y: cy,
    vx: (Math.sin(particles.length * 2.3)) * 60, vy: -260 - (particles.length % 5) * 18,
    life: 0, ttl: 2.2, color, rot: (particles.length % 7 - 3) * 0.12,
  });
}
function stepAndDrawParticles(dt) {
  for (const p of particles) {
    p.life += dt;
    p.x += p.vx * dt; p.y += p.vy * dt; p.vy += 320 * dt;   // gravity arc
  }
  for (let i = particles.length - 1; i >= 0; i--) if (particles[i].life > particles[i].ttl) particles.splice(i, 1);
  for (const p of particles) {
    const a = Math.max(0, 1 - p.life / p.ttl);
    ctx.save();
    ctx.globalAlpha = a; ctx.translate(p.x, p.y); ctx.rotate(p.rot);
    // hard dark shadow then colored note
    drawEighthNote(2.5, 3, "rgba(0,0,0,0.5)");
    drawEighthNote(0, 0, rgb(p.color));
    ctx.restore();
  }
}
function drawEighthNote(ox, oy, fill) {
  ctx.save(); ctx.translate(ox, oy); ctx.fillStyle = fill;
  ctx.save(); ctx.translate(0, 14); ctx.rotate(-0.3); ctx.beginPath();
  ctx.ellipse(0, 0, 11, 8, 0, 0, Math.PI * 2); ctx.fill(); ctx.restore();   // notehead
  ctx.fillRect(9, -22, 4.5, 36);                                            // stem
  ctx.beginPath(); ctx.moveTo(13, -22); ctx.quadraticCurveTo(30, -14, 20, 2);
  ctx.quadraticCurveTo(26, -10, 13, -10); ctx.closePath(); ctx.fill();      // flag
  ctx.restore();
}

// ── a window panel with macOS traffic lights ───────────────────────────────
function drawWindow(x, y, w, h, title) {
  ctx.save();
  ctx.shadowColor = "rgba(0,0,0,0.45)"; ctx.shadowBlur = 60; ctx.shadowOffsetY = 24;
  roundRect(x, y, w, h, 34);
  ctx.fillStyle = "rgba(244,242,250,0.98)"; ctx.fill();
  ctx.restore();
  // titlebar
  const tb = 64;
  ctx.save(); roundRect(x, y, w, h, 34); ctx.clip();
  ctx.fillStyle = "rgba(232,228,242,0.96)"; ctx.fillRect(x, y, w, tb);
  ctx.restore();
  // traffic lights
  const lights = ["rgb(255,95,86)", "rgb(255,189,46)", "rgb(39,201,63)"];
  for (let i = 0; i < 3; i++) { ctx.beginPath(); ctx.fillStyle = lights[i]; ctx.arc(x + 34 + i * 34, y + tb / 2, 12, 0, Math.PI * 2); ctx.fill(); }
  if (title) {
    ctx.fillStyle = "rgba(60,50,80,0.8)"; ctx.font = "600 26px MBSans";
    ctx.textAlign = "center"; ctx.textBaseline = "middle";
    ctx.fillText(title, x + w / 2, y + tb / 2 + 1); ctx.textAlign = "left";
  }
  return { x, y: y + tb, w, h: h - tb };   // content rect
}

// ── lit keys from the melody at time t (lead notes → 5-key fold pc%5) ──────
function litKeysAt(t) {
  const lit = new Set();
  for (const n of leadNotes) if (t >= n.t && t < n.t + Math.max(0.12, n.dur)) lit.add(((n.midi % 12) + 12) % 12 % 5);
  return lit;
}
// onset detection per frame for particle spawns
let noteCursor = 0;
const sortedLead = [...leadNotes].sort((a, b) => a.t - b.t);
function onsetsBetween(t0, t1) {
  const hits = [];
  while (noteCursor < sortedLead.length && sortedLead[noteCursor].t <= t1) {
    if (sortedLead[noteCursor].t >= t0) hits.push(sortedLead[noteCursor]);
    noteCursor++;
  }
  return hits;
}

// ── language-map chiclets (the About-screen LanguageMapView feel) ──────────
const LANGS = [
  ["EN", [97, 158, 255]], ["ES", [255, 153, 46]], ["FR", [167, 139, 250]],
  ["DE", [51, 209, 179]], ["日本", [255, 77, 107]], ["中文", [255, 214, 56]],
  ["PT", [120, 200, 120]], ["IT", [120, 180, 255]], ["한국", [255, 120, 180]],
  ["AR", [200, 160, 90]], ["RU", [150, 150, 230]], ["हिन्दी", [255, 170, 80]],
];
function drawLanguageMap(rx, t) {
  const cols = 3, cw = (rx.w - 80) / cols, ch = 78, gap = 14;
  const rows = Math.ceil(LANGS.length / cols);
  const gw = cols * cw + (cols - 1) * gap, gh = rows * ch + (rows - 1) * gap;
  const ox = rx.x + (rx.w - gw) / 2, oy = rx.y + (rx.h - gh) / 2 + 10;
  const sel = Math.floor(t * 3) % LANGS.length;   // a flag cycles ~3/s
  ctx.textAlign = "center"; ctx.textBaseline = "middle";
  for (let i = 0; i < LANGS.length; i++) {
    const r = Math.floor(i / cols), c = i % cols;
    const x = ox + c * (cw + gap), y = oy + r * (ch + gap);
    const on = i === sel;
    roundRect(x, y, cw, ch, 16);
    ctx.fillStyle = on ? rgb(LANGS[i][1], 0.95) : rgb(LANGS[i][1], 0.22); ctx.fill();
    if (on) { ctx.lineWidth = 4; ctx.strokeStyle = "rgba(255,255,255,0.9)"; ctx.stroke(); }
    ctx.fillStyle = on ? "white" : rgb(LANGS[i][1]);
    ctx.font = "700 34px MBSansRounded";
    ctx.fillText(LANGS[i][0], x + cw / 2, y + ch / 2 + 1);
  }
  ctx.textAlign = "left";
}

// ── faux QR (finder patterns + module field) → reads as "scan me" ──────────
// the REAL scannable QR (qrencode PNG), on a white rounded tile.
function drawQR(x, y, s) {
  ctx.fillStyle = "white"; roundRect(x, y, s, s, s * 0.08); ctx.fill();
  const pad = s * 0.10;
  if (qrImg) ctx.drawImage(qrImg, x + pad, y + pad, s - pad * 2, s - pad * 2);
}

const easeOut = (u) => 1 - Math.pow(1 - Math.max(0, Math.min(1, u)), 3);

function text(str, x, y, size, color, weight = 700, align = "center", rounded = true) {
  ctx.fillStyle = color; ctx.textAlign = align; ctx.textBaseline = "middle";
  ctx.font = `${weight} ${size}px ${rounded ? "MBSansRounded" : "MBSans"}`;
  ctx.fillText(str, x, y); ctx.textAlign = "left";
}

// ── real About window captures (one per language) — the second half ────────
const ABOUT_CACHE = `${OUT}/about-frames`;
mkdirSync(ABOUT_CACHE, { recursive: true });
const ABOUT_LANGS = ["en", "es", "zh", "ja", "ru", "da"];
const aboutImgs = [];
async function prerenderAbout() {
  if (!existsSync(MENUBAR_BIN)) return;
  console.log(`▸ capturing ${ABOUT_LANGS.length} real About windows via MenuBand --render-about …`);
  for (const code of ABOUT_LANGS) {
    const png = `${ABOUT_CACHE}/about-${code}.png`;
    if (!existsSync(png)) {
      const r = spawnSync(MENUBAR_BIN, ["--render-about", "--lang", code, "--scale", "3", "--out", png],
        { stdio: ["ignore", "ignore", "pipe"] });
      if (r.status !== 0) console.error(`  ✗ about ${code}: ${r.stderr?.toString().slice(-160)}`);
    }
    if (existsSync(png)) aboutImgs.push(await loadImage(png));
  }
}

// ── scene timeline: the MENU front-and-center (first half) → the real ABOUT
// window dropping in from the middle → end card with the QR. ────────────────
const SCENES = [
  { name: "menu", from: 0.00, to: 0.50, tint: [97, 158, 255] },
  { name: "about", from: 0.50, to: 0.90, tint: [167, 139, 250] },
  { name: "end", from: 0.90, to: 1.00, tint: [255, 77, 107] },
].map((s) => ({ ...s, from: s.from * TOTAL, to: s.to * TOTAL }));
const sceneAt = (t) => SCENES.find((s) => t >= s.from && t < s.to) ?? SCENES.at(-1);

// The MENU, front and center: a floating macOS menu bar (Apple · title · clock)
// holding the REAL captured piano, scaled large and vertically centered.
function drawBigMenu(t, alpha) {
  const img = menubarCache.get(barKey(barNotesAt(t))) || menubarIdle;
  const plateW = W * 0.94, plateH = 168;
  const px = (W - plateW) / 2, py = H / 2 - plateH / 2;
  ctx.save(); ctx.globalAlpha = alpha;
  ctx.shadowColor = "rgba(0,0,0,0.45)"; ctx.shadowBlur = 55; ctx.shadowOffsetY = 20;
  roundRect(px, py, plateW, plateH, 30); ctx.fillStyle = "rgba(22,18,32,0.94)"; ctx.fill();
  ctx.shadowColor = "transparent";
  const cy = py + 40;
  ctx.fillStyle = "rgba(255,255,255,0.95)"; ctx.beginPath(); ctx.arc(px + 40, cy, 13, 0, 7); ctx.fill();
  ctx.fillStyle = "rgba(22,18,32,0.95)"; ctx.beginPath(); ctx.arc(px + 47, cy - 4, 7.5, 0, 7); ctx.fill();
  text("Menu Band", px + 138, cy, 30, "rgba(255,255,255,0.96)", 700, "left", false);
  text("9:41", px + plateW - 34, cy, 28, "rgba(255,255,255,0.9)", 500, "right", false);
  if (img) {
    const iw = plateW - 96, ih = iw * img.height / img.width;
    ctx.drawImage(img, px + 48, py + plateH - ih - 26, iw, ih);
  }
  ctx.restore();
  ctx.save(); ctx.globalAlpha = alpha;
  text("your menu bar", W / 2, py - 96, 58, "white", 800);
  text("is a synthesizer", W / 2, py - 36, 58, "white", 800);
  text("type · click · send MIDI", W / 2, py + plateH + 64, 32, AC_PURPLE, 700);
  ctx.restore();
}

// The real captured About window, framed as a macOS window (titlebar + lights).
function drawAbout(img, alpha, yOff) {
  if (!img) return;
  const tb = 42;
  const contentH = Math.min(H * 0.66, 1180);
  const aw = contentH * img.width / img.height;
  const ah = contentH + tb;
  const ax = (W - aw) / 2, ay = (H - ah) / 2 + yOff;
  ctx.save(); ctx.globalAlpha = alpha;
  ctx.shadowColor = "rgba(0,0,0,0.45)"; ctx.shadowBlur = 60; ctx.shadowOffsetY = 24;
  roundRect(ax, ay, aw, ah, 26); ctx.fillStyle = "white"; ctx.fill();
  ctx.shadowColor = "transparent";
  ctx.save(); roundRect(ax, ay, aw, ah, 26); ctx.clip();
  ctx.fillStyle = "rgb(238,235,245)"; ctx.fillRect(ax, ay, aw, tb);
  const lights = ["rgb(255,95,86)", "rgb(255,189,46)", "rgb(39,201,63)"];
  for (let i = 0; i < 3; i++) { ctx.beginPath(); ctx.fillStyle = lights[i]; ctx.arc(ax + 28 + i * 28, ay + tb / 2, 9, 0, 7); ctx.fill(); }
  ctx.drawImage(img, ax, ay + tb, aw, contentH);
  ctx.restore();
  ctx.restore();
  return { ax, ay, aw, ah };
}

function drawFrame(t) {
  drawDesktop();
  const sc = sceneAt(t);
  const local = (sc.to - sc.from) > 0 ? (t - sc.from) / (sc.to - sc.from) : 1;
  const dt = 1 / FPS;

  if (sc.name === "menu") {
    // first half — the menu, front and center, playing the melody.
    drawBigMenu(t, 1);
    for (const n of onsetsBetween(t - dt, t)) {
      const idx = ((n.midi % 12) + 12) % 12 % 5;
      spawnNote(W / 2 + Math.sin(n.t * 9) * 220, H / 2 - 110, KEY_COLORS[idx]);
    }
  } else {
    onsetsBetween(t - dt, t);   // keep the onset cursor advancing
    // second half — the REAL About window, language cycling, dropping in.
    let img, alpha = 1, yOff = 0;
    if (sc.name === "about") {
      const idx = Math.min(aboutImgs.length - 1, Math.floor(local * aboutImgs.length));
      img = aboutImgs[idx];
      const e = easeOut(Math.min(1, local * 6));   // quick drop-in at the middle
      yOff = -(H * 0.66) * (1 - e); alpha = e;
    } else {
      img = aboutImgs[0];
    }
    const rc = drawAbout(img, alpha, yOff);
    if (sc.name === "end" && rc && qrImg) {
      const e = easeOut(local * 1.8);
      ctx.save(); ctx.globalAlpha = e;
      drawQR(W / 2 - 120, rc.ay + rc.ah - 280, 240);
      text("prompt.ac/menuband", W / 2, rc.ay + rc.ah + 44, 32, "white", 800);
      ctx.restore();
    }
  }

  // the real macOS menu bar stays pinned at the very top for context.
  drawMenubar(t);
  stepAndDrawParticles(dt);

  // gentle global vignette
  const vg = ctx.createRadialGradient(W / 2, H / 2, H * 0.3, W / 2, H / 2, H * 0.75);
  vg.addColorStop(0, "rgba(0,0,0,0)"); vg.addColorStop(1, "rgba(0,0,0,0.28)");
  ctx.fillStyle = vg; ctx.fillRect(0, 0, W, H);
}

// ── render → ffmpeg (muxed with the waltz) ─────────────────────────────────
await prerenderMenubars();
await prerenderAbout();
console.log(`▸ menuband sim · ${FRAMES} frames · ${TOTAL.toFixed(1)}s · ${W}x${H}@${FPS} · ${leadNotes.length} lead notes`);
const enc = spawnFFmpegEncode({ audioPath: AUDIO, w: W, h: H, fps: FPS, outPath: BASE, crf: 18 });
const t0 = Date.now();
for (let fi = 0; fi < FRAMES; fi++) {
  drawFrame(fi / FPS);
  if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
  if (fi % 150 === 0) console.log(`  ${fi}/${FRAMES} · ${((Date.now() - t0) / 1000).toFixed(0)}s`);
}
enc.stdin.end();
await new Promise((res, rej) => { enc.on("close", (c) => (c === 0 ? res() : rej(new Error(`ffmpeg exit ${c}`)))); });

// scene timings for the chrome pass's per-scene tint
writeFileSync(`${OUT}/meta-menuband-reel.json`, JSON.stringify({
  total: TOTAL,
  slides: SCENES.map((s) => ({ name: s.name, from: s.from, to: s.to, tint: s.tint })),
}, null, 2));
console.log(`✓ base ${BASE}`);
