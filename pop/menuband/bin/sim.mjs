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
/// One 3/4 bar. The hero band bobs on it, so it breathes with the waltz.
const BAR_SEC = score.barSec || 1.3;

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



const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const rgb = (a, al = 1) => `rgba(${a[0]},${a[1]},${a[2]},${al})`;

// LIGHT MODE for the whole reel: a pale desktop and dark chrome. It matches
// the strip, which is captured with `--light`, and the About / keymap windows,
// which the app already renders light unless asked for `--dark`. Ink is the
// one dark value everything on the desktop is drawn in.
const INK = "rgba(20,18,28,0.92)";

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
  // The same purple wallpaper, lifted into light appearance — pale lilac
  // rather than aubergine, so black keys and dark chrome read against it.
  const g = ctx.createLinearGradient(0, 0, W, H);
  g.addColorStop(0, "rgb(238,232,246)");
  g.addColorStop(0.5, "rgb(216,203,234)");
  g.addColorStop(1, "rgb(190,170,214)");
  ctx.fillStyle = g; ctx.fillRect(0, 0, W, H);
  // faint diagonal sheen
  const s = ctx.createLinearGradient(0, H, W, 0);
  s.addColorStop(0, "rgba(255,255,255,0)");
  s.addColorStop(0.6, "rgba(255,255,255,0.28)");
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
const MENUBAR_BIN = `${REPO}/slab/menuband/.build/debug/MenuBand`;
const MB_CACHE = `${OUT}/menubar-frames`;
/// Where each key sits across the strip + its ROYGBIV color, straight from the
/// app's own geometry (`--emit-keys`). Written by prerenderMenubars.
const KEYS_JSON = `${OUT}/menubar-keys.json`;
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
  // Ask the app where its keys are, once. Unconditional — the captures are
  // cached by note-set, so a cache hit would otherwise skip this and leave the
  // particles with no geometry to fire from.
  const probe = spawnSync(MENUBAR_BIN, ["--render-menubar", "--notes", "",
    "--scale", "1", "--light", "--no-settings", "--emit-keys", KEYS_JSON,
    "--out", `${MB_CACHE}/.probe.png`], { stdio: ["ignore", "ignore", "pipe"] });
  if (probe.status !== 0) {
    console.error(`  ✗ key geometry: ${probe.stderr?.toString().slice(-160)}`);
  }

  const keys = new Set();
  for (let fi = 0; fi < FRAMES; fi++) keys.add(barKey(barNotesAt(fi / FPS)));
  console.log(`▸ capturing ${keys.size} real menubar graphics via MenuBand --render-menubar …`);
  for (const key of keys) {
    const png = `${MB_CACHE}/mb-${key === "" ? "idle" : key.replace(/,/g, "-")}.png`;
    if (!existsSync(png)) {
      // No `--voice`: a voiceLabel would be drawn verbatim in the badge, and
      // the badge is gone anyway — `--no-settings` strips the whole chip so
      // the band is symmetrical and centers cleanly. Light mode + a pinned
      // black accent, so the render never depends on the rendering machine's
      // system accent. Scale 6 because the bare keyboard is a smaller native
      // canvas than the full icon, and the hero draws it nearly frame-wide.
      const r = spawnSync(MENUBAR_BIN, ["--render-menubar", "--notes", key,
        "--program", "9", "--scale", "6", "--light", "--accent", "#000000",
        "--no-settings", "--key-accent", "--out", png],
        { stdio: ["ignore", "ignore", "pipe"] });
      if (r.status !== 0) console.error(`  ✗ capture ${key}: ${r.stderr?.toString().slice(-160)}`);
    }
    if (existsSync(png)) menubarCache.set(key, await loadImage(png));
  }
  menubarIdle = menubarCache.get("") || [...menubarCache.values()][0] || null;
}

// No macOS menu bar in the reel — no Apple logo, no clock, no Wi-Fi, no
// battery, no miniature strip up top. The band itself is the subject, and it
// climbs to where the menu bar would have been.

// ── note-particle system (vector eighth-notes puffing up) ──────────────────
const particles = [];
/// `down` sends the note falling instead of puffing up — for once the band has
/// climbed to the top of the screen and rains its notes over the windows
/// floating up beneath it.
function spawnNote(cx, cy, color, down = false) {
  particles.push({
    // Only a few px of scatter: the note is supposed to read as leaving THAT
    // key, and a key is only ~43px wide at this scale.
    x: cx + (Math.sin(particles.length * 1.7) * 6), y: cy,
    vx: (Math.sin(particles.length * 2.3)) * 60,
    vy: down ? 40 + (particles.length % 5) * 12 : -260 - (particles.length % 5) * 18,
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

// The icon↔QR card shuffle, captured straight out of the app. `CardStackView`
// animates through Core Animation, and a headless snapshot only ever sees model
// values — so the app exposes the same shuffle as a function of progress
// (`--card-flip`) and we grab a frame per step. Two directions: the icon
// tucking behind to reveal the QR, and the QR tucking back.
const FLIP_STEPS = 9;
const FLIP_JITTER = "0.35";     // pinned — the live shuffle picks this at random
const flipImgs = { icon: [], qr: [] };
/// Where the card sits inside the About window, as fractions of it.
let CARD = { cx: 0.5, cy: 0.16, side: 0.54 };
const CARD_JSON = `${ABOUT_CACHE}/about-card.json`;

async function prerenderFlip() {
  if (!existsSync(MENUBAR_BIN)) return;
  console.log(`▸ capturing ${FLIP_STEPS * 2} card-shuffle frames via MenuBand --card-flip …`);
  for (const from of ["icon", "qr"]) {
    for (let i = 0; i < FLIP_STEPS; i++) {
      const p = (i / (FLIP_STEPS - 1)).toFixed(4);
      const png = `${ABOUT_CACHE}/flip-${from}-${i}.png`;
      if (!existsSync(png)) {
        const r = spawnSync(MENUBAR_BIN, ["--render-about", "--lang", "en", "--scale", "3",
          "--card-front", from, "--card-flip", p, "--card-jitter", FLIP_JITTER,
          "--emit-card", CARD_JSON, "--out", png], { stdio: ["ignore", "ignore", "pipe"] });
        if (r.status !== 0) console.error(`  ✗ flip ${from} ${p}: ${r.stderr?.toString().slice(-160)}`);
      }
      if (existsSync(png)) flipImgs[from].push(await loadImage(png));
    }
  }
  if (existsSync(CARD_JSON)) CARD = JSON.parse(readFileSync(CARD_JSON, "utf8"));
}

// The REAL fullscreen keymap view (big piano + raster scope + QWERTY map),
// captured per instrument program so the family tint cycles — piano, guitar,
// violin, trumpet, flute — the same way the About pass cycles languages.
const KEYMAP_PROGRAMS = [0, 24, 40, 56, 73];
const keymapImgs = [];
async function prerenderKeymap() {
  if (!existsSync(MENUBAR_BIN)) return;
  console.log(`▸ capturing ${KEYMAP_PROGRAMS.length} real keymap views via MenuBand --render-keymap …`);
  for (const prog of KEYMAP_PROGRAMS) {
    const png = `${ABOUT_CACHE}/keymap-${prog}.png`;
    if (!existsSync(png)) {
      const r = spawnSync(MENUBAR_BIN, ["--render-keymap", "--lang", "en", "--program", String(prog), "--scale", "3", "--out", png],
        { stdio: ["ignore", "ignore", "pipe"] });
      if (r.status !== 0) console.error(`  ✗ keymap ${prog}: ${r.stderr?.toString().slice(-160)}`);
    }
    if (existsSync(png)) keymapImgs.push(await loadImage(png));
  }
}

// ── scene timeline: the MENU front-and-center (first half) → the real ABOUT
// window dropping in from the middle → end card with the QR. ────────────────
const SCENES = [
  { name: "menu", from: 0.00, to: 0.42, tint: [97, 158, 255] },
  { name: "about", from: 0.42, to: 0.66, tint: [167, 139, 250] },
  { name: "keymap", from: 0.66, to: 0.90, tint: [163, 230, 53] },
  { name: "end", from: 0.90, to: 1.00, tint: [255, 77, 107] },
].map((s) => ({ ...s, from: s.from * TOTAL, to: s.to * TOTAL }));
const sceneAt = (t) => SCENES.find((s) => t >= s.from && t < s.to) ?? SCENES.at(-1);

// The MENU, front and center: the REAL captured piano, alone on the desktop.
// No plate behind it — the captured strip already carries its own rounded
// housing, so a second dark rounded rect around it just reads as a black
// bubble. And no settings chip (`--no-settings`), which used to hang off the
// right end and pull the whole strip's weight to one side; without it the
// band is symmetrical and can simply sit centered.
const HERO_W = W * 0.96;
const HERO_X = (W - HERO_W) / 2;
/// Height of one lazy up-and-down, and how long the band takes to fall in.
const HERO_BOB = 22;
const HERO_DROP = 1.4;
/// Where the band parks once it has climbed, and how long the climb takes. It
/// finishes exactly as the About window arrives, so the band is already out of
/// the way and still playing when the other windows float up beneath it.
const HERO_TOP = 54;
const HERO_RISE = 1.2;

/// Where the hero strip sits at time `t`. The note particles read this too, so
/// they leave the band wherever it currently floats. Three motions ride on the
/// resting position: an entrance falling in from above the frame, a slow bob on
/// the waltz's bar so it breathes in 3/4, and the climb to the top of the
/// screen at the end of the first act.
function heroRect(t) {
  const img = menubarIdle;
  const h = img ? HERO_W * img.height / img.width : 66;
  const middle = H / 2 - h / 2;
  const riseEnd = SCENES[0].to;
  const rise = easeOut((t - (riseEnd - HERO_RISE)) / HERO_RISE);
  const rest = middle + (HERO_TOP - middle) * rise;

  const enter = easeOut(Math.min(1, Math.max(0, t / HERO_DROP)));
  const bob = Math.sin((t / BAR_SEC) * Math.PI * 2) * HERO_BOB * (1 - rise * 0.6);
  const y = (-h - 40) + (rest - (-h - 40)) * enter + bob * enter;
  return { x: HERO_X, y, w: HERO_W, h, risen: rise > 0.5 };
}

function drawBigMenu(t, alpha) {
  const img = menubarCache.get(barKey(barNotesAt(t))) || menubarIdle;
  if (!img) return;
  const r = heroRect(t);
  ctx.save(); ctx.globalAlpha = alpha;
  ctx.drawImage(img, r.x, r.y, r.w, r.h);
  ctx.restore();
  // (No headline / subheader here — the burned VO captions carry the words;
  // the strip plays alone, front and center.)
}

/// Key geometry from the app itself: midi → { cx (fraction of strip width),
/// hex (its ROYGBIV stripe color, null for sharps) }. Loaded after the
/// prerender writes it.
let KEYS = new Map();
function loadKeyGeometry() {
  if (!existsSync(KEYS_JSON)) return;
  for (const k of JSON.parse(readFileSync(KEYS_JSON, "utf8"))) {
    KEYS.set(k.midi, k);
  }
}

/// Exactly where on the hero strip a note's key sits. The strip lights the
/// note folded into its own octave range, so the particle has to fold too —
/// otherwise it leaves from a key that never lit.
function heroKeyX(midi, t) {
  const key = KEYS.get(foldToBar(midi));
  const r = heroRect(t);
  return r.x + r.w * (key ? key.cx : 0.5);
}

/// The color the key itself lights in, as [r,g,b]. Sharps have no stripe color;
/// they light in the accent, so their notes fly off in ink.
const INK_RGB = [20, 18, 28];
function heroKeyColor(midi) {
  const hex = KEYS.get(foldToBar(midi))?.hex;
  if (!hex) return INK_RGB;
  const v = parseInt(hex.slice(1), 16);
  return [(v >> 16) & 255, (v >> 8) & 255, v & 255];
}

/// Title bar height for the drawn window frame. Scaled to the content it sits
/// on — at 42px with 9px lights it read as a toy strip glued to a 1180px window.
const TITLEBAR_H = 72;

// ── the end card's tap: a cursor flies to the app icon and clicks it a few
// times, and the deck shuffles the QR into view. Three taps, so it lands on
// the QR (icon → qr → icon → qr) and the scan target is what's left on screen.
const TAPS = [0.75, 1.45, 2.15];        // seconds into the end scene
const TAP_DUR = 0.34;                   // one shuffle, matching the app's 2×0.15s + slack
const CURSOR_FLY = 0.62;

/// Which capture to show for the end card at `local` seconds into it, and how
/// far through a tap we are. Between taps the deck rests on whichever face the
/// last tap left forward.
function flipStateAt(local) {
  let front = "icon";                   // deck starts icon-forward
  for (const at of TAPS) {
    if (local < at) break;
    const u = (local - at) / TAP_DUR;
    if (u < 1) {
      // mid-shuffle: the sequence captured FROM this face
      const i = Math.min(FLIP_STEPS - 1, Math.max(0, Math.round(u * (FLIP_STEPS - 1))));
      return { img: flipImgs[front][i], tapping: true, sinceTap: local - at };
    }
    front = front === "icon" ? "qr" : "icon";   // that tap committed
  }
  // At rest, show the deck posed with `front` forward — that's frame 0 of the
  // sequence captured FROM that face. (The last frame is the shuffle already
  // finished, which is the other face; reaching for it lands the deck
  // backwards after every tap.)
  return { img: flipImgs[front][0], tapping: false, sinceTap: Infinity };
}

/// A macOS arrow pointer, drawn as the real one is: a black-filled outline with
/// a white stroke, so it reads on any backdrop.
function drawCursor(x, y, pressed) {
  const s = pressed ? 30 : 33;
  ctx.save();
  ctx.translate(x, y);
  ctx.beginPath();
  ctx.moveTo(0, 0);
  ctx.lineTo(0, s);
  ctx.lineTo(s * 0.27, s * 0.76);
  ctx.lineTo(s * 0.44, s * 1.12);
  ctx.lineTo(s * 0.60, s * 1.05);
  ctx.lineTo(s * 0.43, s * 0.70);
  ctx.lineTo(s * 0.70, s * 0.68);
  ctx.closePath();
  ctx.shadowColor = "rgba(0,0,0,0.35)"; ctx.shadowBlur = 10; ctx.shadowOffsetY = 3;
  ctx.fillStyle = "white"; ctx.fill();
  ctx.shadowColor = "transparent";
  ctx.lineWidth = 2.4; ctx.strokeStyle = "rgba(20,18,28,0.95)"; ctx.stroke();
  ctx.restore();
}

// The real captured About window, framed as a macOS window (titlebar + lights).
function drawAbout(img, alpha, yOff) {
  if (!img) return;
  const tb = TITLEBAR_H;
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
  for (let i = 0; i < 3; i++) { ctx.beginPath(); ctx.fillStyle = lights[i]; ctx.arc(ax + 44 + i * 46, ay + tb / 2, 15, 0, 7); ctx.fill(); }
  ctx.drawImage(img, ax, ay + tb, aw, contentH);
  ctx.restore();
  ctx.restore();
  return { ax, ay, aw, ah };
}

// The real fullscreen keymap view, width-fit (it's wider than tall) and
// framed like the About window — but borderless, since it's a fullscreen
// overlay in the app. Slides up from below as it enters.
function drawKeymap(img, alpha, yOff) {
  if (!img) return;
  const aw = W * 0.94;
  const ah = aw * img.height / img.width;
  const ax = (W - aw) / 2, ay = (H - ah) / 2 + yOff;
  ctx.save(); ctx.globalAlpha = alpha;
  ctx.shadowColor = "rgba(0,0,0,0.45)"; ctx.shadowBlur = 60; ctx.shadowOffsetY = 24;
  roundRect(ax, ay, aw, ah, 26); ctx.fillStyle = "white"; ctx.fill();
  ctx.shadowColor = "transparent";
  ctx.save(); roundRect(ax, ay, aw, ah, 26); ctx.clip();
  ctx.drawImage(img, ax, ay, aw, ah);
  ctx.restore();
  ctx.restore();
  return { ax, ay, aw, ah };
}

function drawFrame(t) {
  drawDesktop();
  const sc = sceneAt(t);
  const local = (sc.to - sc.from) > 0 ? (t - sc.from) / (sc.to - sc.from) : 1;
  const dt = 1 / FPS;

  // The band plays for the whole reel. It opens front and center, then climbs
  // to the top of the screen and keeps playing there while the windows float
  // up beneath it. Drawn before them, so they pass in front.
  const hero = heroRect(t);
  drawBigMenu(t, 1);

  // Every note leaves the exact key that lit, in that key's own color — the
  // app's geometry and palette, not a guess. Once the band has climbed, the
  // notes rain down off its underside instead of puffing up. Exactly one call
  // to `onsetsBetween` per frame: it advances a cursor.
  const edge = hero.risen ? hero.y + hero.h + 6 : hero.y - 6;
  for (const n of onsetsBetween(t - dt, t)) {
    spawnNote(heroKeyX(n.midi, t), edge, heroKeyColor(n.midi), hero.risen);
  }

  if (sc.name === "keymap" && keymapImgs.length) {
    // third act — the REAL fullscreen keymap (raster scope + big piano +
    // QWERTY map), instrument family colors cycling. No caption: it's a
    // screenshot of the real thing and it speaks for itself.
    const idx = Math.min(keymapImgs.length - 1, Math.floor(local * keymapImgs.length));
    const e = easeOut(Math.min(1, local * 6));      // quick rise-in
    drawKeymap(keymapImgs[idx], e, (H * 0.5) * (1 - e));
  } else if (sc.name !== "menu") {
    // second act — the REAL About window, language cycling, floating up.
    let img, alpha = 1, yOff = 0, flip = null;
    if (sc.name === "about") {
      const idx = Math.min(aboutImgs.length - 1, Math.floor(local * aboutImgs.length));
      img = aboutImgs[idx];
      const e = easeOut(Math.min(1, local * 6));
      yOff = (H * 0.66) * (1 - e); alpha = e;
    } else {
      // end card — a cursor taps the app icon and the deck shuffles the QR up.
      const localT = t - sc.from;
      flip = flipStateAt(localT);
      img = flip.img ?? aboutImgs[0];
    }
    const rc = drawAbout(img, alpha, yOff);

    if (flip && rc) {
      // The card's spot, straight from the app's own layout (`--emit-card`).
      const cx = rc.ax + rc.aw * CARD.cx;
      const cy = rc.ay + TITLEBAR_H + (rc.ah - TITLEBAR_H) * CARD.cy;
      const localT = t - sc.from;

      // A note bursts out of the icon on every click — the app does exactly
      // this (`emitNotesBurst` in CardStackView.flip).
      for (const at of TAPS) {
        if (localT - dt < at && localT >= at) {
          for (let i = 0; i < 5; i++) {
            spawnNote(cx + (i - 2) * 16, cy - 10, KEY_COLORS[i]);
          }
        }
      }

      // The cursor flies in from the lower right, then rests on the icon,
      // dipping on each click.
      const fly = easeOut(Math.min(1, localT / CURSOR_FLY));
      const px = W * 0.86 + (cx + 6 - W * 0.86) * fly;
      const py = H * 0.78 + (cy + 6 - H * 0.78) * fly;
      const pressed = flip.sinceTap >= 0 && flip.sinceTap < 0.12;
      drawCursor(px, py + (pressed ? 3 : 0), pressed);
    }
  }

  stepAndDrawParticles(dt);

  // gentle global vignette
  const vg = ctx.createRadialGradient(W / 2, H / 2, H * 0.3, W / 2, H / 2, H * 0.75);
  vg.addColorStop(0, "rgba(0,0,0,0)"); vg.addColorStop(1, "rgba(0,0,0,0.28)");
  ctx.fillStyle = vg; ctx.fillRect(0, 0, W, H);
}

// ── render → ffmpeg (muxed with the waltz) ─────────────────────────────────
await prerenderMenubars();
loadKeyGeometry();
await prerenderAbout();
await prerenderFlip();
await prerenderKeymap();
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
