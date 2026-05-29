#!/usr/bin/env node
// hellsine/bin/preview-score-hellsine-yt.mjs — landscape 1920x1080 cut.
// Mirror of preview-score.mjs (portrait) — same flame/sfx/punch-up
// pipeline — but consumes the `-yt-sec-*.png` landscape panel set and
// writes the YouTube-aspect output.
//
// Renders the 18 story panels (gen-sections.mjs) into a 9:16 portrait
// IG-Reel / Story mp4. Forked from marimba/bin/preview-score.mjs;
// adapted for hellsine's 2-lane percussion struct (kick + snare).
//
// Differences vs marimba:
//   • Panel filename = hellsine-p-sec-NN-<id>.png (2-digit zero-padded).
//   • LANES = [kick, snare] only (struct.events only carries these two,
//     each event is { t } — no pitch / dur / gain). We default missing
//     fields so the lane renderer still works (fixed pitch + short dur).
//   • punchTimes driven by kick onsets (was bass for marimba).
//   • SECTION_TINTS expanded to 18 panels — lava arc (cool overture →
//     red statement → cosmic bridge → kindling develop → full lava
//     climax → dawn coda).
//   • TITLE_PALETTE / BACKLIGHT swapped to lava warm.
//   • FORMS skipped (no hellsine-forms.json) — falls back to whole-
//     frame Ken-Burns. TODO: add pop/hellsine/hellsine-forms.json for
//     proper face zooms.
//   • 17 sub-section transition boundaries instead of marimba's 9.
//
// Usage:
//   node pop/hellsine/bin/preview-score.mjs --reel
//   node pop/hellsine/bin/preview-score.mjs --reel --start 110 --frames 60

import { existsSync, readFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { createCanvas, loadImage } from "canvas";
import * as progress from "../../lib/render-progress.mjs";
import { getNoteColorForOctave } from "../../../system/public/aesthetic.computer/lib/note-colors.mjs";
import {
  checkYwftAvailable, decodeAudioMono, computeRmsEnvelope,
  prerenderTitleChars, magickRenderText, drawCoverKenBurns,
  drawTitleBounce, spawnFFmpegEncode, AUDIO_SR_DEFAULT,
} from "../../lib/preview-shared.mjs";
import { makeVerletString, hexToRgb } from "../../lib/cover-engine.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}

const SLUG   = flags.slug  || "hellsine";
const TITLE  = flags.title || SLUG;
const COVER  = flags.cover  || `${LANE}/out/${SLUG}-cover.png`;
const AUDIO  = flags.audio  || "/Users/jas/Documents/Shelf/hellsine/hellsine-c-MASTER.wav";
const STRUCT = flags.struct || `${LANE}/${SLUG}.struct.json`;
const OUT    = flags.out || `${LANE}/out/${SLUG}-preview-score-landscape.mp4`;
const FPS    = Number(flags.fps ?? 30);
const SIZE   = flags.size || "1920x1080";
const [W, H] = SIZE.split("x").map(Number);
const REEL   = false;       // YT cut is always chromed (progress bar + timecode shown)
const START_T = Number(flags.start ?? 0);
const FRAMES_OVERRIDE = flags.frames ? Number(flags.frames) : null;

checkYwftAvailable();
for (const [name, p] of [["audio", AUDIO], ["struct", STRUCT]]) {
  if (!existsSync(p)) {
    console.error(`✗ ${name} missing: ${p.replace(REPO + "/", "")}`);
    if (name === "struct") console.error(`  build it: see pop/hellsine/bin/preview-score.mjs header`);
    process.exit(1);
  }
}

const struct = JSON.parse(readFileSync(STRUCT, "utf8"));
const DURATION = struct.totalSec;
const FRAMES = Math.ceil(DURATION * FPS);
const SECTIONS = struct.sections.slice().sort((a, b) => a.startSec - b.startSec);
console.log(`▸ ${SLUG} storyline visualizer · ${W}x${H} · ${DURATION.toFixed(1)}s · ${FRAMES} frames · ${SECTIONS.length} sections`);

// ── audio decode + envelope ──────────────────────────────────────────
console.log("  decoding audio …");
const { audio, sr: audioSr, audioPeak } = decodeAudioMono(AUDIO, AUDIO_SR_DEFAULT);
const envelope = computeRmsEnvelope(audio, audioSr, FPS, DURATION);
function envAt(t) {
  const idx = Math.floor(t * FPS);
  return (idx < 0 || idx >= envelope.length) ? 0 : envelope[idx];
}

// ── voice lanes — hellsine's two percussion voices ───────────────────
// kick + snare. struct.events.<lane> items are { t } only; we default
// pitch/dur/gain so the existing lane renderer (which expects those)
// degrades gracefully.
const LANES = [
  { key: "kick",  color: "#ff5a1f" },   // lava orange — the hole kick
  { key: "snare", color: "#ffd24a" },   // amber — the snare crack
  { key: "sfx",   color: "#c2a4ff" },   // soft violet — freesound SFX (blaster/clap/whip/etc.)
];
const DEFAULT_PITCH = { kick: 36, snare: 60, sfx: 72 };   // midi numbers for pitch tinting
const DEFAULT_DUR   = { kick: 0.18, snare: 0.12, sfx: 0.10 };
const DEFAULT_GAIN  = { kick: 0.85, snare: 0.65, sfx: 0.55 };
const laneEvents = {};
for (const L of LANES) {
  const raw = (struct.events?.[L.key] || []).slice().sort((a, b) => a.t - b.t);
  // backfill the marimba-shape fields the lane renderer expects
  const evs = raw.map((e) => ({
    t:    e.t,
    midi: e.midi ?? e.pitch ?? DEFAULT_PITCH[L.key],
    dur:  e.dur  ?? DEFAULT_DUR[L.key],
    gain: e.gain ?? DEFAULT_GAIN[L.key],
  }));
  // STACKROW assignment (same algorithm as marimba)
  const rowEndT = [];
  let maxRows = 1;
  for (const ev of evs) {
    let row = 0;
    while (row < rowEndT.length && rowEndT[row] > ev.t + 1e-4) row++;
    ev.stackRow = row;
    const evEnd = ev.t + (ev.dur || 0.25);
    if (row >= rowEndT.length) rowEndT.push(evEnd);
    else rowEndT[row] = evEnd;
    if (row + 1 > maxRows) maxRows = row + 1;
  }
  L.maxStackRows = maxRows;
  laneEvents[L.key] = evs;
}
const nEvents = Object.values(laneEvents).reduce((s, a) => s + a.length, 0);
console.log(`  sound elements: ${nEvents} percussion events across ${LANES.length} voice lanes`);

// kick onsets drive the "sharpen" punch envelope.
const punchTimes = (laneEvents.kick || []).map((e) => e.t);
function punchAt(t) {
  let e = 0;
  for (const pt of punchTimes) {
    if (pt > t + 0.04) break;
    const dt = t - pt;
    const v = dt < 0 ? Math.max(0, 1 + dt / 0.03) : Math.exp(-dt / 0.5);
    if (v > e) e = v;
  }
  return e;
}

// ── notepat pitch colour ─────────────────────────────────────────────
const NOTE_NAMES = ["c","c#","d","d#","e","f","f#","g","g#","a","a#","b"];
function midiToNotepatRgb(midi) {
  if (!Number.isFinite(midi)) return [220, 220, 210];
  const noteIdx = ((midi % 12) + 12) % 12;
  const octave = Math.floor(midi / 12) - 1;
  const c = getNoteColorForOctave(NOTE_NAMES[noteIdx], octave);
  return Array.isArray(c) ? c : [c.r ?? 220, c.g ?? 220, c.b ?? 210];
}

// ── hellsine identity — LAVA ARC ─────────────────────────────────────
// 18 sub-panel tints arcing from cool-night earth → red strike →
// cosmic violet/cyan fall → kindling ignition → full lava peak →
// dawn coral morning-after. Hand-picked so each section's three or
// four sub-panels evolve through their own micro-arc too.
const SECTION_TINTS = {
  // OVERTURE — cool, calm earth night, the moment before the strike.
  "overture-a":  "rgba(60,72,118,1)",     // deep dusk indigo
  "overture-b":  "rgba(82,96,140,1)",     // dusk blue
  "overture-c":  "rgba(118,130,170,1)",   // pale dusk → about to break
  // STATEMENT — the strike. Red alert.
  "statement-a": "rgba(220,72,52,1)",     // red alarm flare
  "statement-b": "rgba(232,92,44,1)",     // hot orange-red
  "statement-c": "rgba(244,116,52,1)",    // bright lava orange
  // BRIDGE — the fall. Cosmic violet/cyan.
  "bridge-a":    "rgba(116,82,168,1)",    // electric violet
  "bridge-b":    "rgba(96,108,184,1)",    // violet → cyan transit
  "bridge-c":    "rgba(76,140,200,1)",    // cool cyan plunge
  "bridge-d":    "rgba(96,168,196,1)",    // pale aqua before impact
  // DEVELOP — kindling. Ignition arc.
  "develop-a":   "rgba(196,116,60,1)",    // amber kindling
  "develop-b":   "rgba(220,128,52,1)",    // bright kindling
  "develop-c":   "rgba(240,144,48,1)",    // hot ember
  // CLIMAX — full lava-orange peak party.
  "climax-a":    "rgba(252,140,40,1)",    // lava blaze
  "climax-b":    "rgba(255,108,32,1)",    // peak orange
  "climax-c":    "rgba(248,80,40,1)",     // saturated red-orange peak
  // CODA — the morning after. Dawn coral.
  "coda-a":      "rgba(236,160,128,1)",   // soft dawn coral
  "coda-b":      "rgba(228,182,160,1)",   // pale ash-coral fade
};
// Lava warm title palette.
const TITLE_PALETTE = ["#ff6a1f", "#ff8a3c", "#ffb24a", "#ffd24a", "#ffe8a8"];
const BACKLIGHT_RGB = "255,128,48";       // deep lava amber

// ── YWFT title + per-second timecode ─────────────────────────────────
console.log("  rasterizing YWFT …");
const assetsDir = AUDIO.replace(/\.(mp3|wav|flac|aac|m4a)$/i, ".assets");
mkdirSync(assetsDir, { recursive: true });

const titleFontSize = 96;
const { chars: titleChars, totalWidth: titleTotalW } = await prerenderTitleChars({
  text: TITLE, ptSize: titleFontSize, palette: TITLE_PALETTE,
  shadowColor: null, assetsDir,
});

// Per-slide side-stamp suffix letter: each of the 18 panels gets its own
// lowercase letter (a..r) drawn inline after "hellsine" on the vertical
// stamp — same baseline + same size as the rest of the title text, so the
// stamp reads as `hellsine c` (a real AC piece prompt that jumps to the
// c section of hellsine.mjs).
const STAMP_LETTERS = "abcdefghijklmnopqrstuvwxyz".split("").slice(0, 26);
const stampLetterImgs = [];
for (let i = 0; i < Math.max(SECTIONS.length, 18); i++) {
  const ch = STAMP_LETTERS[i] || "?";
  const img = await magickRenderText(ch, {
    ptSize: titleFontSize, fill: "rgba(255,253,242,1)",
    outPath: `${assetsDir}/stampletter.${ch}.png`,
  });
  stampLetterImgs.push(img);
}
const PALS_S      = 145;
const PALS_HALF   = PALS_S / 2;
const PALS_EDGE_X  = 100;
const CHARS_EDGE_X = PALS_EDGE_X + 12;
const CHAR_SCALE  = 0.52;
const CHAR_SPAN   = titleTotalW * CHAR_SCALE;
const BOUNCE_BUF  = 26;
const LEFT_CHARS_CY  = H * 0.82 - 16;
const RIGHT_CHARS_CY = H * 0.18 + 32 + 16;
const LEFT_PALS_CY  = LEFT_CHARS_CY  - CHAR_SPAN / 2 - BOUNCE_BUF - PALS_HALF;
const RIGHT_PALS_CY = RIGHT_CHARS_CY + CHAR_SPAN / 2 + BOUNCE_BUF + PALS_HALF;
const TITLE_TOP_Y = 104;

const tcFontSize = 60;
const tcCache = new Map();
async function getTcImg(text) {
  if (tcCache.has(text)) return tcCache.get(text);
  const safe = text.replace(/[^0-9]/g, "_");
  const img = await magickRenderText(text, {
    ptSize: tcFontSize, fill: "rgba(255,253,242,0.97)",
    outPath: `${assetsDir}/tc.${safe}.png`,
  });
  const shadow = await magickRenderText(text, {
    ptSize: tcFontSize, fill: "rgba(0,0,0,1)",
    outPath: `${assetsDir}/tc.${safe}.shadow.png`,
  });
  const entry = { img, shadow };
  tcCache.set(text, entry);
  return entry;
}
const totMm = Math.floor(DURATION / 60);
const totSs = Math.floor(DURATION - totMm * 60).toString().padStart(2, "0");
for (let s = 0; s <= Math.ceil(DURATION); s++) {
  const mm = Math.floor(s / 60);
  const ss = (s - mm * 60).toString().padStart(2, "0");
  await getTcImg(`${mm}:${ss} / ${totMm}:${totSs}`);
}

// ── canvas + section panels ──────────────────────────────────────────
const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const offA = createCanvas(W, H), offACtx = offA.getContext("2d");
const offB = createCanvas(W, H), offBCtx = offB.getContext("2d");

function safeName(n) {
  return n.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}
function pad2(i) { return String(i).padStart(2, "0"); }
let coverImg = existsSync(COVER) ? await loadImage(COVER) : null;
const sectionImgs = [];
let haveImgs = 0;
for (let i = 0; i < SECTIONS.length; i++) {
  // hellsine uses 2-digit zero-padded section indexes.
  const p = `${LANE}/out/${SLUG}-yt-sec-${pad2(i)}-${safeName(SECTIONS[i].name)}.png`;
  if (existsSync(p)) { sectionImgs[i] = await loadImage(p); haveImgs++; }
  else if (coverImg) sectionImgs[i] = coverImg;
  else {
    console.error(`✗ section panel missing and no cover fallback: ${p.replace(REPO + "/", "")}`);
    console.error(`  generate panels first: node pop/hellsine/bin/gen-sections.mjs`);
    process.exit(1);
  }
}
console.log(`  section panels: ${haveImgs}/${SECTIONS.length}`);
const imgW = sectionImgs[0].width, imgH = sectionImgs[0].height;

// ── figure + face bboxes (forms.json) ────────────────────────────────
// TODO(hellsine): write pop/hellsine/hellsine-forms.json with per-panel
// face + figure boxes (group-portrait centred lower-middle figure,
// upper-middle face) for tighter Ken-Burns face zooms. For v1 we
// fall back to whole-frame Ken-Burns (empty FORMS → no faces → camera
// breathes between WHOLE_BOX and itself).
let FORMS = {};
// YT cut intentionally skips the portrait forms.json (bboxes are in
// 1024x1536 coords; YT panels are 1536x1024). Fallback default-box logic
// in renderPanel will centre the backlight glow per panel. Adapt later
// if a `hellsine-yt-forms.json` lands.
try {
  const fp = `${LANE}/${SLUG}-yt-forms.json`;
  if (existsSync(fp)) FORMS = JSON.parse(readFileSync(fp, "utf8")).sections || {};
} catch { /* no forms → centre fallback */ }
function _toBox(v) {
  if (!v) return null;
  if (Array.isArray(v)) return { x: v[0], y: v[1], w: v[2], h: v[3] };
  if (Array.isArray(v.figure)) return { x: v.figure[0], y: v.figure[1], w: v.figure[2], h: v.figure[3] };
  return null;
}
function _toFace(v) {
  if (!v || Array.isArray(v)) return null;
  if (Array.isArray(v.face)) return { x: v.face[0], y: v.face[1], w: v.face[2], h: v.face[3] };
  return null;
}
function figureBoxes(name) {
  const f = FORMS[name];
  if (!f) return [];
  return [_toBox(f.jeffrey), _toBox(f.gates)].filter(Boolean);
}
function faceBoxes(name) {
  const f = FORMS[name];
  if (!f) return [];
  return [_toFace(f.jeffrey), _toFace(f.gates)].filter(Boolean);
}
const FRAME_ASPECT = W / H;
function fitFrameAspect(b) {
  let { x, y, w, h } = b;
  if (w / h > FRAME_ASPECT) { const nh = w / FRAME_ASPECT; y -= (nh - h) / 2; h = nh; }
  else                       { const nw = h * FRAME_ASPECT; x -= (nw - w) / 2; w = nw; }
  return { x, y, w, h };
}
function faceBoxFor(face, figure) {
  if (face) return fitFrameAspect(face);
  let w = figure ? Math.max(figure.w * 0.82, imgW * 0.42) : imgW * 0.42;
  let h = w / FRAME_ASPECT;
  if (h > imgH) { h = imgH; w = h * FRAME_ASPECT; }
  const cx = figure ? figure.x + figure.w / 2 : imgW / 2;
  const cy = figure ? figure.y + figure.h * 0.18 : imgH * 0.3;
  let x = Math.max(0, Math.min(imgW - w, cx - w / 2));
  let y = Math.max(0, Math.min(imgH - h, cy - h / 2));
  return { x, y, w, h };
}
const WHOLE_BOX = { x: 0, y: 0, w: imgW, h: imgH };
function lerpBox(a, b, t) {
  return {
    x: a.x + (b.x - a.x) * t, y: a.y + (b.y - a.y) * t,
    w: a.w + (b.w - a.w) * t, h: a.h + (b.h - a.h) * t,
  };
}

// ── pals watermark — side stamps ─────────────────────────────────────
const PALS_WM_SIZE = 212;
let palsImg = null, palsBlur = null;
const wmCanvas = createCanvas(8, 8);
const wmCtx = wmCanvas.getContext("2d");
{
  const svg = `${REPO}/system/public/purple-pals.svg`;
  const png = `${assetsDir}/pals-watermark.png`;
  const blurPng = `${assetsDir}/pals-watermark-blur.png`;
  if (existsSync(svg)) {
    const r = spawnSync("rsvg-convert", ["-w", String(PALS_WM_SIZE * 2), "-h", String(PALS_WM_SIZE * 2), "-o", png, svg]);
    if (r.status === 0 && existsSync(png)) {
      palsImg = await loadImage(png);
      const rb = spawnSync("magick", [png, "-channel", "A", "-blur", "0x1.5", "+channel", blurPng]);
      palsBlur = (rb.status === 0 && existsSync(blurPng)) ? await loadImage(blurPng) : palsImg;
    }
  }
  console.log(`  pals watermark: ${palsImg ? "loaded" : "MISSING (skipped)"}`);
}
function hslToRgb(h, s, l) {
  const c = (1 - Math.abs(2 * l - 1)) * s;
  const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
  const m = l - c / 2;
  let r = 0, g = 0, b = 0;
  if (h < 60) { r = c; g = x; }
  else if (h < 120) { r = x; g = c; }
  else if (h < 180) { g = c; b = x; }
  else if (h < 240) { g = x; b = c; }
  else if (h < 300) { r = x; b = c; }
  else { r = c; b = x; }
  return [Math.round((r + m) * 255), Math.round((g + m) * 255), Math.round((b + m) * 255)];
}
function palsFrameColor(audioT) {
  const u = audioT * FPS / FRAMES;
  const hue = ((u * 360 * 4) % 360 + 360) % 360;
  const hRgb = hslToRgb(hue, 0.9, 0.62);
  const [sr, sg, sb] = sectionTcRgb(audioT);
  return [
    Math.round(sr * 0.6 + hRgb[0] * 0.4),
    Math.round(sg * 0.6 + hRgb[1] * 0.4),
    Math.round(sb * 0.6 + hRgb[2] * 0.4),
  ];
}
const charTintCanvas = createCanvas(2, 2);
const charTintCtx = charTintCanvas.getContext("2d");
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
function drawWatermark(audioT) {
  if (!palsImg) return;
  const s = 145;
  const u = audioT * FPS / FRAMES;
  const TAU = Math.PI * 2;
  const hue = ((u * 360 * 4) % 360 + 360) % 360;
  const hRgb = hslToRgb(hue, 0.9, 0.62);
  const [sr, sg, sb] = sectionTcRgb(audioT);
  const col = [
    Math.round(sr * 0.6 + hRgb[0] * 0.4),
    Math.round(sg * 0.6 + hRgb[1] * 0.4),
    Math.round(sb * 0.6 + hRgb[2] * 0.4),
  ];
  const env = Math.min(1, envAt(audioT));
  const glow = env * env;
  const hotRgb = hslToRgb(hue, 1.0, Math.min(0.88, 0.60 + 0.34 * glow));
  const ledCol = [
    Math.round(col[0] + (hotRgb[0] - col[0]) * glow),
    Math.round(col[1] + (hotRgb[1] - col[1]) * glow),
    Math.round(col[2] + (hotRgb[2] - col[2]) * glow),
  ];
  const wig  = 13 * Math.sin(TAU * 30 * u) + 4 * Math.sin(TAU * 10 * u);
  const swiv = 0.05 * Math.sin(TAU * 19 * u) + 0.025 * Math.sin(TAU * 38 * u);
  const spots = [
    { cx: PALS_EDGE_X - wig,     cy: LEFT_PALS_CY,  rot:  Math.PI / 2 + swiv },
    { cx: W - PALS_EDGE_X + wig, cy: RIGHT_PALS_CY, rot: -Math.PI / 2 - swiv },
  ];
  const passes = [
    ["multiply",   0.78], ["color-burn", 0.42],
    ["overlay",    0.58], ["source-over", 0.06],
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
  if (!palsImg) return;
  const s = 145;
  const u = audioT * FPS / FRAMES;
  const TAU = Math.PI * 2;
  const wig  = 13 * Math.sin(TAU * 30 * u) + 4 * Math.sin(TAU * 10 * u);
  const charScale = CHAR_SCALE;
  const span = titleTotalW * charScale;
  const palsRgb = palsFrameColor(audioT);
  const spots = [
    { charsCx: CHARS_EDGE_X - wig,     cy: LEFT_CHARS_CY,  rot:  Math.PI / 2 },
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
      const x = startX + ch.prefixWidth * charScale;
      const dw = ch.img.width * charScale;
      const dh = ch.img.height * charScale;
      const charEnv = envAt(audioT - i * 0.03);
      const lift = 4 * Math.sin(audioT * 4.0 + i * 0.8) * (0.3 + charEnv);
      const y = -dh / 2 + lift;
      ctx.save();
      ctx.globalCompositeOperation = "source-over";
      ctx.globalAlpha = 0.26;
      ctx.drawImage(tintCharGlyph(ch.img, [0, 0, 0]), x + 3, y + 4, dw, dh);
      ctx.restore();
      const charPasses = [
        ["multiply",    0.78],
        ["color-burn",  0.42],
        ["overlay",     0.58],
        ["source-over", 0.06],
      ];
      for (const [op, a] of charPasses) {
        ctx.save();
        ctx.globalCompositeOperation = op;
        ctx.globalAlpha = a;
        ctx.drawImage(tintCharGlyph(ch.img, palsRgb), x, y, dw, dh);
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
        ctx.drawImage(tintCharGlyph(ch.img, palsRgb), x, y, dw, dh);
        ctx.restore();
      }
    }
    // ── inline per-section letter (hellsine a … r) ───────────────────
    // Same baseline + same size as the title chars — reads as the AC
    // piece prompt `hellsine c` that would jump to hellsine.mjs section c.
    const secIdx = sectionIndexAt(audioT);
    const letterImg = stampLetterImgs[secIdx];
    if (letterImg) {
      const ldw = letterImg.width * charScale;
      const ldh = letterImg.height * charScale;
      // Use a normal inter-word space (~25% of the cap height) as the gap.
      const gap = titleFontSize * 0.25 * charScale;
      const lx = startX + span + gap;
      const supEnv = envAt(audioT);
      const lift = 4 * Math.sin(audioT * 4.0 + secIdx * 0.8) * (0.3 + supEnv);
      const ly = -ldh / 2 + lift;
      // Drop shadow (matches title chars).
      ctx.save();
      ctx.globalCompositeOperation = "source-over";
      ctx.globalAlpha = 0.26;
      ctx.drawImage(tintCharGlyph(letterImg, [0, 0, 0]), lx + 3, ly + 4, ldw, ldh);
      ctx.restore();
      // Color body, same multi-pass treatment as the title chars.
      const passes = [
        ["multiply",    0.78],
        ["color-burn",  0.42],
        ["overlay",     0.58],
        ["source-over", 0.06],
      ];
      for (const [op, a] of passes) {
        ctx.save();
        ctx.globalCompositeOperation = op;
        ctx.globalAlpha = a;
        ctx.drawImage(tintCharGlyph(letterImg, palsRgb), lx, ly, ldw, ldh);
        ctx.restore();
      }
      ctx.save();
      ctx.globalCompositeOperation = "source-over";
      ctx.globalAlpha = 0.46;
      ctx.drawImage(tintCharGlyph(letterImg, palsRgb), lx, ly, ldw, ldh);
      ctx.restore();
      if (supEnv > 0.45) {
        ctx.save();
        ctx.globalCompositeOperation = "screen";
        ctx.globalAlpha = 0.14 + 0.46 * Math.min(1, (supEnv - 0.45) / 0.55);
        ctx.drawImage(tintCharGlyph(letterImg, palsRgb), lx, ly, ldw, ldh);
        ctx.restore();
      }
    }
    ctx.restore();
  }
}
function sectionTcRgb(audioT) {
  const i = sectionIndexAt(audioT);
  const s = SECTIONS[i];
  let [r, g, b] = tintRgb(SECTION_TINTS[s.name] || "rgba(255,253,242,1)");
  const span = Math.max(0.001, s.endSec - s.startSec);
  const lp = Math.max(0, Math.min(1, (audioT - s.startSec) / span));
  const k = 0.30 * lp;
  r = Math.round(r + (255 - r) * k);
  g = Math.round(g + (255 - g) * k);
  b = Math.round(b + (255 - b) * k);
  return [r, g, b];
}

// ── the verlet string ────────────────────────────────────────────────
const PLAYHEAD_X = Math.round(W / 2);
const PX_PER_SEC = 220;                      // faster scroll — hellsine is 182 bpm
const _vs = makeVerletString(ctx, { W, H, playheadX: PLAYHEAD_X, duration: DURATION });

const LANE_TOP = 240, LANE_BOTTOM = H - 200;
const LANE_H = (LANE_BOTTOM - LANE_TOP) / LANES.length;
const laneCenterY = {};
LANES.forEach((L, i) => { laneCenterY[L.key] = LANE_TOP + i * LANE_H + LANE_H / 2; });

const GROOVE_R = Math.round(Math.min(W, H) * 0.85);

// ── illustration distortion UNDER the bent string ───────────────────
const WU_HALF = 120, WU_W = WU_HALF * 2, WU_STEP = 4, WU_BANDS = 24;
const WU_STR = 0.28, WU_BW = WU_W / WU_BANDS;
const wuWin = new Float64Array(WU_BANDS);
for (let b = 0; b < WU_BANDS; b++) {
  wuWin[b] = Math.sin(Math.PI * ((b + 0.5) / WU_BANDS)) ** 2;
}
const WU_DSZ = Math.ceil(Math.hypot(W, H)) + 4;
const wuSnap = createCanvas(W, H), wuSnapC = wuSnap.getContext("2d");
const wuCR   = createCanvas(WU_DSZ, WU_DSZ), wuCRC = wuCR.getContext("2d");
const ROT_CX = Math.round(W / 2), ROT_CY = Math.round(H / 2);
function warpUnderString(theta) {
  const { devPeak } = _vs.deflection();
  if (Math.abs(devPeak) < 2) return;
  wuSnapC.clearRect(0, 0, W, H);
  wuSnapC.drawImage(canvas, 0, 0);
  wuCRC.setTransform(1, 0, 0, 1, 0, 0);
  wuCRC.clearRect(0, 0, WU_DSZ, WU_DSZ);
  wuCRC.translate(WU_DSZ / 2, WU_DSZ / 2);
  wuCRC.rotate(-theta);
  wuCRC.translate(-W / 2, -H / 2);
  wuCRC.drawImage(wuSnap, 0, 0);
  wuCRC.setTransform(1, 0, 0, 1, 0, 0);
  const sox = (WU_DSZ - W) / 2, soy = (WU_DSZ - H) / 2;
  const x0 = Math.round(PLAYHEAD_X - WU_HALF);
  ctx.save();
  ctx.translate(ROT_CX, ROT_CY);
  ctx.rotate(theta);
  ctx.translate(-ROT_CX, -ROT_CY);
  for (let y = 0; y < H; y += WU_STEP) {
    const dx = (needleXAt(y) - PLAYHEAD_X) * WU_STR;
    for (let b = 0; b < WU_BANDS; b++) {
      const sx = b * WU_BW;
      const shift = dx * wuWin[b];
      ctx.drawImage(
        wuCR, sox + x0 + sx, soy + y, WU_BW + 1, WU_STEP,
        x0 + sx + shift, y, WU_BW + 1, WU_STEP,
      );
    }
  }
  ctx.restore();
}

// ── 3-LAYER BACKLIGHT ────────────────────────────────────────────────
const _transmissionMasks = new WeakMap();
function getTransmissionMask(img) {
  let m = _transmissionMasks.get(img);
  if (m) return m;
  const iw = img.width, ih = img.height;
  const tmp = createCanvas(iw, ih);
  const tctx = tmp.getContext("2d");
  tctx.drawImage(img, 0, 0, iw, ih);
  const id = tctx.getImageData(0, 0, iw, ih);
  const px = id.data;
  const LO = 0.30, HI = 0.92, GAMMA = 1.7, FLOOR = 0.04, CAP = 0.65;
  for (let i = 0; i < px.length; i += 4) {
    const L = (0.2126 * px[i] + 0.7152 * px[i + 1] + 0.0722 * px[i + 2]) / 255;
    let s = (L - LO) / (HI - LO);
    s = s < 0 ? 0 : s > 1 ? 1 : s;
    s = s * s * (3 - 2 * s);
    const tA = Math.min(CAP, FLOOR + (1 - FLOOR) * Math.pow(s, GAMMA));
    px[i] = 255; px[i + 1] = 255; px[i + 2] = 255;
    px[i + 3] = Math.round(tA * 255);
  }
  tctx.putImageData(id, 0, 0);
  _transmissionMasks.set(img, tmp);
  return tmp;
}
const glowCanvas = createCanvas(W, H);
const glowCtx = glowCanvas.getContext("2d");
function drawVignette(c, intensity) {
  const gx = W / 2, gy = H * 0.55;
  c.save();
  c.globalCompositeOperation = "multiply";
  const vg = c.createRadialGradient(gx, gy, H * 0.10, gx, gy, H * 0.72);
  vg.addColorStop(0,    "rgba(255,255,255,1)");
  // Warm lava-shadow surround — deep amber/maroon instead of neutral.
  vg.addColorStop(0.45, `rgba(140,60,32,${(0.62 + 0.12 * intensity).toFixed(3)})`);
  vg.addColorStop(1,    `rgba(28,10,4,${(0.94).toFixed(3)})`);
  c.fillStyle = vg;
  c.fillRect(0, 0, W, H);
  c.restore();
  c.globalCompositeOperation = "source-over";
}
// When no faces are available (we have no forms.json), the transmitted
// + leaded backlights fall through (their guard returns early). The
// vignette still bakes the heat in, and the panel itself reads as the
// scene. TODO(hellsine): a centred default face box per panel would let
// the transmitted layer still glow through the panel.
function drawTransmittedBacklight(c, sectionImg, xform, faces, intensity, audioT = 0) {
  if (!faces || !faces.length || !xform || intensity <= 0.01) return;
  const k = Math.min(1, intensity);
  const jx = 3.5 * Math.sin(audioT * 17.3) + 1.5 * Math.sin(audioT * 41.0);
  const jy = 3.5 * Math.cos(audioT * 21.7) + 1.5 * Math.cos(audioT * 37.0);
  glowCtx.globalCompositeOperation = "source-over";
  glowCtx.clearRect(0, 0, W, H);
  for (const f of faces) {
    const cx = xform.x + (f.x + f.w / 2) * xform.scale + jx;
    const cy = xform.y + (f.y + f.h / 2) * xform.scale + jy;
    const radius = Math.max(f.w, f.h) * xform.scale * 1.4;
    if (radius <= 1) continue;
    const g = glowCtx.createRadialGradient(cx, cy, 0, cx, cy, radius);
    g.addColorStop(0.0,  `rgba(${BACKLIGHT_RGB},${(0.78 * k).toFixed(3)})`);
    g.addColorStop(0.32, `rgba(${BACKLIGHT_RGB},${(0.58 * k).toFixed(3)})`);
    g.addColorStop(0.65, `rgba(${BACKLIGHT_RGB},${(0.30 * k).toFixed(3)})`);
    g.addColorStop(1.0,  `rgba(${BACKLIGHT_RGB},0)`);
    glowCtx.globalCompositeOperation = "lighter";
    glowCtx.fillStyle = g;
    glowCtx.fillRect(
      Math.max(0, cx - radius), Math.max(0, cy - radius),
      Math.min(W, radius * 2), Math.min(H, radius * 2),
    );
  }
  glowCtx.globalCompositeOperation = "destination-in";
  const mask = getTransmissionMask(sectionImg);
  glowCtx.drawImage(mask, xform.x, xform.y,
    sectionImg.width * xform.scale, sectionImg.height * xform.scale);
  glowCtx.globalCompositeOperation = "source-over";
  c.save();
  c.globalCompositeOperation = "lighter";
  c.globalAlpha = 0.45;
  c.drawImage(glowCanvas, 0, 0);
  c.globalAlpha = 0.12;
  c.drawImage(glowCanvas, -4, -4, W + 8, H + 8);
  c.restore();
  c.globalCompositeOperation = "source-over";
}
function drawLeadedContrast(c, sectionImg, xform, faces, intensity, audioT = 0) {
  if (!faces || !faces.length || !xform || intensity <= 0.01) return;
  const k = Math.min(1, intensity);
  const jx = 2.5 * Math.sin(audioT * 19.7 + 1.2) + 1.0 * Math.cos(audioT * 47.0);
  const jy = 2.5 * Math.cos(audioT * 23.1 + 0.6) + 1.0 * Math.sin(audioT * 41.5);
  glowCtx.globalCompositeOperation = "source-over";
  glowCtx.clearRect(0, 0, W, H);
  for (const f of faces) {
    const cx = xform.x + (f.x + f.w / 2) * xform.scale + jx;
    const cy = xform.y + (f.y + f.h / 2) * xform.scale + jy;
    const radius = Math.max(f.w, f.h) * xform.scale * 1.4;
    if (radius <= 1) continue;
    const g = glowCtx.createRadialGradient(cx, cy, 0, cx, cy, radius);
    g.addColorStop(0.0,  `rgba(58,28,16,${(0.55 * k).toFixed(3)})`);
    g.addColorStop(0.50, `rgba(58,28,16,${(0.30 * k).toFixed(3)})`);
    g.addColorStop(1.0,  "rgba(58,28,16,0)");
    glowCtx.globalCompositeOperation = "lighter";
    glowCtx.fillStyle = g;
    glowCtx.fillRect(
      Math.max(0, cx - radius), Math.max(0, cy - radius),
      Math.min(W, radius * 2), Math.min(H, radius * 2),
    );
  }
  glowCtx.globalCompositeOperation = "destination-out";
  const mask = getTransmissionMask(sectionImg);
  glowCtx.drawImage(mask, xform.x, xform.y,
    sectionImg.width * xform.scale, sectionImg.height * xform.scale);
  glowCtx.globalCompositeOperation = "source-over";
  c.save();
  c.globalCompositeOperation = "multiply";
  c.globalAlpha = 0.28;
  c.drawImage(glowCanvas, 0, 0);
  c.restore();
  c.globalCompositeOperation = "source-over";
}

// ── panel render ─────────────────────────────────────────────────────
const KB = { breathAmp: 0.05, breathPeriodSec: 18, wobbleAmp: 4, envWobbleAmp: 0, zoomPad: 1.0 };
const ZOOM_CYCLES = 4;                        // a bit more breath over the shorter / harder track
function zoomAt(audioT) {
  const swing = 0.5 - 0.5 * Math.cos(2 * Math.PI * ZOOM_CYCLES * audioT / DURATION);
  return 0.04 + 0.30 * swing;
}
function zoomTargetBox(faces, figs) {
  const src = faces.length ? faces : figs;
  if (!src.length) return WHOLE_BOX;
  let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
  for (const b of src) {
    x0 = Math.min(x0, b.x); y0 = Math.min(y0, b.y);
    x1 = Math.max(x1, b.x + b.w); y1 = Math.max(y1, b.y + b.h);
  }
  const padX = (x1 - x0) * 0.12, padY = (y1 - y0) * 0.12;
  return fitFrameAspect({
    x: x0 - padX, y: y0 - padY,
    w: (x1 - x0) + 2 * padX, h: (y1 - y0) + 2 * padY,
  });
}
// Default face/figure box centred upper-mid for panels without a
// hellsine-forms.json entry. Lets the transmitted backlight + leaded
// contrast layers still fire — the 3-layer separation glow that other
// pop releases (trancepenta etc.) use.
function defaultFigureBox() {
  return { x: imgW * 0.18, y: imgH * 0.16, w: imgW * 0.64, h: imgH * 0.66 };
}
function defaultFaceBox() {
  return { x: imgW * 0.30, y: imgH * 0.22, w: imgW * 0.40, h: imgH * 0.30 };
}

// Punch-up pass — push chromatic richness + contrast WITHOUT lifting
// the panel's blacks. The earlier "screen" pass crept into shadow
// territory and washed out the panels; replaced with black-preserving
// blend modes ("color" shifts hue+saturation only, "soft-light" gently
// pushes contrast and preserves both blacks AND whites). The lava
// bloom is gated through the panel's own bright-region mask so it
// only paints where there's already light to push.
function drawPanelPunchUp(c, idx, audioT, env, punch, sectionImg, xform) {
  const [sr, sg, sb] = tintRgb(SECTION_TINTS[SECTIONS[idx].name] || "rgba(255,140,40,1)");
  const heat = Math.min(1, env * 0.9 + punch * 0.5);
  // "color" — shifts hue+saturation toward the section tint; preserves
  // destination luma so blacks stay black and whites stay white.
  c.save();
  c.globalCompositeOperation = "color";
  c.globalAlpha = 0.18 + 0.22 * heat;
  c.fillStyle = `rgba(${sr},${sg},${sb},1)`;
  c.fillRect(0, 0, W, H);
  c.restore();
  // "soft-light" — gentle contrast push without crushing blacks or
  // blowing highlights (much gentler than overlay, much darker-preserving
  // than screen).
  c.save();
  c.globalCompositeOperation = "soft-light";
  c.globalAlpha = 0.32 + 0.34 * heat;
  c.fillStyle = `rgba(${sr},${sg},${sb},1)`;
  c.fillRect(0, 0, W, H);
  c.restore();
  // Lava bloom on the lower half — gated by the panel's own transmission
  // mask so dark pixels stay dark (only existing light gets amplified).
  if (heat > 0.03 && sectionImg && xform) {
    glowCtx.globalCompositeOperation = "source-over";
    glowCtx.clearRect(0, 0, W, H);
    const g = glowCtx.createLinearGradient(0, H * 0.50, 0, H);
    g.addColorStop(0, `rgba(255,120, 32,0)`);
    g.addColorStop(1, `rgba(255,120, 32,${(0.32 * heat).toFixed(3)})`);
    glowCtx.fillStyle = g;
    glowCtx.fillRect(0, H * 0.50, W, H * 0.50);
    // Gate by the panel's bright-area mask.
    glowCtx.globalCompositeOperation = "destination-in";
    const mask = getTransmissionMask(sectionImg);
    glowCtx.drawImage(mask, xform.x, xform.y,
      sectionImg.width * xform.scale, sectionImg.height * xform.scale);
    glowCtx.globalCompositeOperation = "source-over";
    c.save();
    c.globalCompositeOperation = "lighter";
    c.drawImage(glowCanvas, 0, 0);
    c.restore();
  }
  c.globalCompositeOperation = "source-over";
}

function renderPanel(c, idx, audioT, env, punch) {
  const name = SECTIONS[idx].name;
  let figs  = figureBoxes(name);
  let faces = faceBoxes(name);
  // Fallback when no hellsine-forms.json: synthesize a centred default
  // figure + face box so the transmitted / leaded backlight layers can
  // glow on the panel (matches the trancepenta 3-layer style).
  if (!figs.length)  figs  = [defaultFigureBox()];
  if (!faces.length) faces = [defaultFaceBox()];
  const z = zoomAt(audioT);
  const zoomBox = lerpBox(WHOLE_BOX, zoomTargetBox(faces, figs), z);
  const xform = drawCoverKenBurns(c, sectionImgs[idx], audioT, {
    ...KB, env, punch, zoomBox, fillBackground: true,
  });
  const flicker = 1 + 0.20 * (
      0.6 * Math.sin(audioT * 14.3 + idx * 0.7)
    + 0.3 * Math.sin(audioT * 31.7 + idx * 1.3)
    + 0.2 * Math.sin(audioT * 53.1 + idx * 0.4)
  );
  // 3-layer stained-glass intensities — pushed harder so the transmitted
  // (face/lava lit-through) + leaded (linework/darks) layers actually
  // sharpen the panel instead of being a faint wash.
  const v_i = (0.85 + 0.55 * env)               * flicker;
  const t_i = (0.45 + 1.40 * env + 1.10 * punch) * flicker;
  const l_i = (0.30 + 1.10 * env + 0.80 * punch) * flicker;
  drawPanelPunchUp(c, idx, audioT, env, punch, sectionImgs[idx], xform);
  drawTransmittedBacklight(c, sectionImgs[idx], xform, faces, t_i, audioT);
  drawLeadedContrast(c, sectionImgs[idx], xform, faces, l_i, audioT);
  drawVignette(c, v_i);
  return xform;
}

// ── TRANSITIONS — 6 types cycled across 17 sub-section boundaries ────
const TRANS_S = 0.85;
const TRANSITIONS = ["iris", "blinds", "push", "zoomPunch", "pixel", "diagonal"];
function transitionForBoundary(idx) { return TRANSITIONS[(idx - 1) % TRANSITIONS.length]; }
const _pixCanvas = createCanvas(W, H), _pixCtx = _pixCanvas.getContext("2d");
function applyTransition(kind, prevC, curC, p) {
  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
  if (kind === "iris") {
    ctx.drawImage(prevC, 0, 0);
    ctx.save();
    const r = p * Math.hypot(W, H) * 0.62;
    ctx.beginPath(); ctx.arc(W / 2, H * 0.52, r, 0, Math.PI * 2); ctx.clip();
    ctx.drawImage(curC, 0, 0);
    ctx.restore();
  } else if (kind === "blinds") {
    ctx.drawImage(prevC, 0, 0);
    const N = 9, bh = H / N;
    for (let i = 0; i < N; i++) {
      const local = Math.max(0, Math.min(1, p * 1.6 - i * (0.6 / N)));
      if (local <= 0) continue;
      const h = bh * local;
      ctx.drawImage(curC, 0, i * bh, W, h, 0, i * bh, W, h);
    }
  } else if (kind === "push") {
    const dy = Math.round(p * H);
    ctx.drawImage(prevC, 0, -dy);
    ctx.drawImage(curC, 0, H - dy);
  } else if (kind === "zoomPunch") {
    const ps = 1 + 0.5 * p, cs = 1.6 - 0.6 * p;
    ctx.globalAlpha = 1 - p;
    ctx.drawImage(prevC, (W - W * ps) / 2, (H - H * ps) / 2, W * ps, H * ps);
    ctx.globalAlpha = Math.min(1, p * 1.4);
    ctx.drawImage(curC, (W - W * cs) / 2, (H - H * cs) / 2, W * cs, H * cs);
    ctx.globalAlpha = 1;
  } else if (kind === "pixel") {
    ctx.drawImage(prevC, 0, 0);
    const cell = 84, cols = Math.ceil(W / cell), rows = Math.ceil(H / cell);
    _pixCtx.clearRect(0, 0, W, H);
    _pixCtx.drawImage(curC, 0, 0);
    for (let r = 0; r < rows; r++) for (let cx = 0; cx < cols; cx++) {
      let h = ((r * 73856093) ^ (cx * 19349663)) >>> 0;
      const thr = ((h % 1000) / 1000) * 0.85;
      if (p <= thr) continue;
      ctx.drawImage(_pixCanvas, cx * cell, r * cell, cell, cell, cx * cell, r * cell, cell, cell);
    }
  } else {
    ctx.drawImage(prevC, 0, 0);
    ctx.save();
    const e = p * (W + H);
    ctx.beginPath();
    ctx.moveTo(0, 0); ctx.lineTo(e, 0); ctx.lineTo(0, e); ctx.closePath();
    ctx.clip();
    ctx.drawImage(curC, 0, 0);
    ctx.restore();
  }
  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
}

function sectionIndexAt(t) {
  let idx = 0;
  for (let i = 0; i < SECTIONS.length; i++) if (t >= SECTIONS[i].startSec) idx = i;
  return idx;
}

function needleXAt(y) { return _vs.needleXAt(y); }
function drawLanes(audioT) {
  const halfSpan = (W * 0.62) / PX_PER_SEC;
  const FLASH_WIN = 0.18;
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  for (const L of LANES) {
    const yCBase = laneCenterY[L.key];
    const laneRgb = hexToRgb(L.color);
    const visibleRows = Math.min(L.maxStackRows ?? 1, 3);
    const subH = (LANE_BOTTOM - LANE_TOP) / LANES.length / visibleRows;
    for (const ev of laneEvents[L.key]) {
      if (ev.t > audioT + halfSpan) break;
      const dur = ev.dur || 0.25;
      if (ev.t + dur < audioT - halfSpan) continue;
      const visDur = Math.min(dur, 0.30);
      const ex = PLAYHEAD_X + (ev.t - audioT) * PX_PER_SEC;
      const ew = Math.max(4, visDur * PX_PER_SEC);
      const rowIdx = Math.min(ev.stackRow ?? 0, visibleRows - 1);
      const yC = yCBase - (LANE_BOTTOM - LANE_TOP) / LANES.length / 2
               + subH / 2 + rowIdx * subH;
      const sinceTrigger = audioT - ev.t;
      let flash = 0;
      if (sinceTrigger >= 0 && sinceTrigger < FLASH_WIN) flash = 1 - sinceTrigger / FLASH_WIN;
      const isFuture = ev.t > audioT + 0.02;
      const isPlayed = sinceTrigger >= FLASH_WIN;
      let baseAlpha = 1.0;
      if (isFuture) baseAlpha = 0.55;
      else if (isPlayed) baseAlpha = 0.82;
      if (flash > 0) baseAlpha = Math.min(1.0, baseAlpha + flash * 0.40);
      const nrgb = midiToNotepatRgb(ev.midi);
      const cr = Math.round(nrgb[0] * 0.48 + laneRgb[0] * 0.52);
      const cg = Math.round(nrgb[1] * 0.48 + laneRgb[1] * 0.52);
      const cb = Math.round(nrgb[2] * 0.48 + laneRgb[2] * 0.52);
      const rgb = `${cr},${cg},${cb}`;
      const fullH = Math.min(Math.max(20, subH - 6), 30 + 86 * Math.min(1, ev.gain ?? 0.5));
      const cols = Math.max(2, Math.floor(ew / 9));
      const blockW = Math.max(3, ew / cols - 2);
      const bend = (needleXAt(yC) - PLAYHEAD_X) * 0.5;
      const startSamp = Math.max(0, Math.floor(ev.t * audioSr));
      const endSamp = Math.min(audio.length - 1, Math.floor((ev.t + visDur) * audioSr));
      const spc = (endSamp - startSamp) / Math.max(1, cols);
      for (let c = 0; c < cols; c++) {
        const s0 = startSamp + Math.floor(c * spc);
        const s1 = Math.min(endSamp, startSamp + Math.floor((c + 1) * spc));
        let pk = 0;
        for (let s = s0; s < s1; s++) { const a = Math.abs(audio[s]); if (a > pk) pk = a; }
        pk = Math.min(1, (pk / audioPeak) * 1.6);
        const tCol = ev.t + (c / cols) * visDur;
        const dt = audioT - tCol;
        let alpha;
        if (dt < 0) alpha = 0.16;
        else if (dt < 0.05) alpha = 1.0;
        else if (dt < 0.45) alpha = 1.0 - (dt - 0.05) / 0.40 * 0.55;
        else alpha = 0.42;
        alpha *= baseAlpha;
        const half = Math.max(2, (pk * fullH) / 2);
        const bx = ex + (c / cols) * ew + bend;
        const aGroove = (bx - PLAYHEAD_X) / GROOVE_R;
        ctx.save();
        ctx.translate(ROT_CX, ROT_CY);
        ctx.rotate(aGroove);
        ctx.translate(-ROT_CX, -ROT_CY);
        ctx.fillStyle = `rgba(${rgb},${alpha.toFixed(3)})`;
        ctx.fillRect(bx, yC - half, blockW, half * 2);
        ctx.restore();
      }
    }
  }
  ctx.restore();
  ctx.globalCompositeOperation = "source-over";
}

function drawStringGlow(env, audioT) {
  const [r, g, b] = palsFrameColor(audioT);
  ctx.save();
  ctx.lineCap = "round";
  ctx.globalCompositeOperation = "source-over";
  ctx.strokeStyle = `rgba(${r},${g},${b},0.42)`;
  ctx.lineWidth = 1.6;
  ctx.beginPath();
  for (let y = -20; y <= H + 20; y += 10) {
    const x = needleXAt(y);
    if (y <= -20) ctx.moveTo(x, y); else ctx.lineTo(x, y);
  }
  ctx.stroke();
  ctx.globalCompositeOperation = "screen";
  ctx.strokeStyle = `rgba(${r},${g},${b},${(0.34 + 0.40 * env).toFixed(3)})`;
  ctx.lineWidth = 3.4;
  ctx.stroke();
  ctx.restore();
  ctx.globalCompositeOperation = "source-over";
}

// Physical pixel-flame + smoke — when a percussion event reaches the
// playhead (the verlet string), spawn a deterministic pixel-particle
// burst at that lane's y on the string. Particles obey a tiny physics
// model (initial upward thrust + drag + turbulent x-jitter), are
// rendered as snapped chunky pixels (not anti-aliased circles), and
// shift colour by temperature (white-hot → yellow → orange → red →
// embers). A delayed smoke burst trails each flame, source-over grey
// pixels expanding + dissipating upward.
//
// Deterministic spawn: every particle's per-frame state is a pure
// function of (eventTime, particleIdx, audioT) — no per-frame RNG, so
// re-renders are bit-identical and we never accumulate state leakage.
const PIX_GRID         = 3;        // pixel-block size for the flame (small + crisp)
const PIX_GRID_SMOKE   = 5;        // smoke uses chunkier pixels
const FLAME_LIFE       = 1.80;     // base seconds per particle (env-scaled per hit)
const SMOKE_LIFE       = 2.40;     // seconds per smoke particle
const SMOKE_DELAY      = 0.22;     // seconds before smoke spawns after a hit
const FLAMES_PER_HIT   = 72;
const SMOKES_PER_HIT   = 22;
const FLASH_LIFE       = 0.15;     // bright disc burst at hit moment
const EMITTER_SPREAD   = 0;        // all particles spawn at the exact activation point on the string
const HIT_WINDOW       = Math.max(FLAME_LIFE * 1.5, SMOKE_DELAY + SMOKE_LIFE);
// Per-lane base flame colour — pulled from LANES.color so kick flames
// read red-orange (#ff5a1f) and snare flames read amber-gold (#ffd24a),
// matching their waveform cells on the lane strip.
const LANE_FLAME_RGB = {};
for (const L of LANES) LANE_FLAME_RGB[L.key] = hexToRgb(L.color);

const ringSpawnCursor  = { kick: 0, snare: 0, sfx: 0 };
const activeRings      = [];       // { spawnT, lane, sx, sy }

function _hash01(seed) {
  const s = Math.sin(seed * 9301.1 + 49297.7) * 233280.0;
  return s - Math.floor(s);
}

// Compute the SCREEN-space spawn point for a hit at (spawnT, laneKey).
// At the moment a kick/snare event fires, the string is rotated by
// theta(spawnT). Particles need to anchor to that rotated screen
// position and then rise STRAIGHT UP in screen coordinates (global
// vertical gravity) — the string keeps rotating, but the flame stays
// where it bloomed. This means flame draw happens OUTSIDE withRotation.
function _spawnScreenPos(audioT, laneKey, ev) {
  // Use the CURRENT frame's theta — events can fire just before this
  // frame's audioT, but we want the spawn at the VISIBLE string position
  // right now (where the cell visually crosses the string this frame).
  const theta = (audioT / DURATION) * Math.PI * 2;
  // CRITICAL: cells are NOT drawn at laneCenterY. drawLanes uses
  // yC = yCBase - LANE_H/2 + subH/2 + stackRow * subH (multi-row stack).
  // Spawning at laneCenterY misses cells in row > 0 by up to LANE_H/2.
  // Mirror drawLanes' formula here so the flame lands on the cell.
  const L = LANES.find((x) => x.key === laneKey) || LANES[0];
  const visibleRows = Math.min(L.maxStackRows ?? 1, 3);
  const yCBase = laneCenterY[laneKey];
  const laneSpan = (LANE_BOTTOM - LANE_TOP) / LANES.length;
  const subH = laneSpan / visibleRows;
  const rowIdx = Math.min((ev && ev.stackRow) ?? 0, visibleRows - 1);
  const yU = yCBase - laneSpan / 2 + subH / 2 + rowIdx * subH;
  // Match the cell rendering's half-bend offset (see drawLanes line ~988:
  // `bend = (needleXAt(yC) - PLAYHEAD_X) * 0.5`).
  const halfBend = (needleXAt(yU) - PLAYHEAD_X) * 0.5;
  const xU = PLAYHEAD_X + halfBend;
  const dx = xU - ROT_CX;
  const dy = yU - ROT_CY;
  const cT = Math.cos(theta), sT = Math.sin(theta);
  return {
    x: ROT_CX + dx * cT - dy * sT,
    y: ROT_CY + dx * sT + dy * cT,
  };
}

function drawStringFireRings(audioT) {
  // Spawn rings for events that have crossed the playhead. Each ring
  // stashes its screen-space spawn position once (after rotation at
  // spawn time) — particles then rise from that fixed point.
  for (const lane of ["kick", "snare", "sfx"]) {
    const evs = laneEvents[lane] || [];
    while (
      ringSpawnCursor[lane] < evs.length &&
      evs[ringSpawnCursor[lane]].t <= audioT
    ) {
      const ev = evs[ringSpawnCursor[lane]];
      if (audioT - ev.t <= HIT_WINDOW) {
        const p = _spawnScreenPos(audioT, lane, ev);
        const theta = (audioT / DURATION) * Math.PI * 2;
        // Emitter geometry rotates with the string slope: tangent
        // direction (along the string) at this lane is the rotated
        // vertical (0,1) → (-sin θ, cos θ). Particles fan out along
        // THIS direction at spawn time, then rise in screen-vertical.
        const tx = -Math.sin(theta);
        const ty =  Math.cos(theta);
        // env-at-spawn modulates intensity per hit (louder → longer +
        // wider + more dynamic flames).
        const eAtSpawn = Math.min(1, envAt(audioT) * 1.5);
        activeRings.push({
          spawnT: ev.t, lane,
          sx: p.x, sy: p.y,
          tx, ty,
          env: eAtSpawn,
        });
      }
      ringSpawnCursor[lane]++;
    }
  }
  // Cull old rings.
  while (activeRings.length && audioT - activeRings[0].spawnT > HIT_WINDOW) {
    activeRings.shift();
  }
  if (!activeRings.length) return;

  // ── FLASH PASS — bright hot disc at each hit moment, fades fast ─────
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  for (const ring of activeRings) {
    const age = audioT - ring.spawnT;
    if (age < 0 || age > FLASH_LIFE) continue;
    const k = 1 - age / FLASH_LIFE;
    const [lr, lg, lb] = LANE_FLAME_RGB[ring.lane] || [255, 140, 40];
    const r = 18 + 32 * (1 - k);
    // Radial gradient with hot core, at the screen-space spawn anchor.
    const grad = ctx.createRadialGradient(ring.sx, ring.sy, 0, ring.sx, ring.sy, r);
    grad.addColorStop(0,   `rgba(255,250,220,${(0.95 * k).toFixed(3)})`);
    grad.addColorStop(0.35,`rgba(${lr},${lg},${lb},${(0.70 * k).toFixed(3)})`);
    grad.addColorStop(1,   `rgba(${lr},${lg},${lb},0)`);
    ctx.fillStyle = grad;
    ctx.beginPath();
    ctx.arc(ring.sx, ring.sy, r, 0, Math.PI * 2);
    ctx.fill();
  }
  ctx.restore();

  // ── FLAME PASS — screen composite (additive light) ───────────────────
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  for (const ring of activeRings) {
    // Per-lane base colour — kick lane (#ff5a1f) burns red-orange, snare
    // (#ffd24a) burns amber-gold. The temperature ramp interpolates from
    // white-hot → lane colour → ember.
    const [lr, lg, lb] = LANE_FLAME_RGB[ring.lane] || [255, 140, 40];
    // Per-ring intensity — louder hits → longer flames, wider emitter,
    // more dynamic velocity.
    const intensity = 0.5 + ring.env;                                // 0.5..1.5
    const ringLife = FLAME_LIFE * (0.65 + 0.70 * ring.env);          // 1.17..1.97s
    const spread   = EMITTER_SPREAD * (0.55 + 0.95 * ring.env);      // ~50..140px
    for (let i = 0; i < FLAMES_PER_HIT; i++) {
      const seedA = ring.spawnT * 17 + i * 0.137;
      const seedB = ring.spawnT * 23 + i * 0.413;
      const seedC = ring.spawnT * 29 + i * 0.911;
      const h1 = _hash01(seedA);
      const h2 = _hash01(seedB);
      const h3 = _hash01(seedC);
      const stagger = h1 * 0.10;           // 0–100ms spawn stagger
      const age = audioT - ring.spawnT - stagger;
      if (age <= 0 || age >= ringLife) continue;
      const life01 = age / ringLife;

      // EMITTER: spawn position fans out ALONG the string tangent at
      // spawn — emitter geometry rotates WITH the string slope. Bell-
      // curve falloff so most particles cluster near the centre.
      const tOff = (h3 - 0.5) * spread * (1 - 0.3 * Math.abs(h3 - 0.5));
      const sx0 = ring.sx + ring.tx * tOff;
      const sy0 = ring.sy + ring.ty * tOff;

      // Physics: vertical thrust + lateral wobble + envelope-modulated
      // velocity range. Loud hits = much taller flames, more burst.
      const burst = h1 > 0.85 ? 2.0 : 1.0;                          // 15% fast jets
      const vy0 = -(160 + h1 * 320) * burst * intensity;            // px/s upward
      const vx0 = (h2 - 0.5) * 40 * (1 + h1 * 0.4);                 // narrow lateral
      const drag = Math.exp(-age * (0.45 + h2 * 0.55));             // per-particle drag
      const turbulence =
          Math.sin(age * 9.0 + h1 * 11) * 10 * (1 - life01 * 0.3)
        + Math.sin(age * 19.0 + h2 * 17) * 5  * (1 - life01 * 0.5)
        + Math.sin(age * 31.0 + h1 * 23) * 2  * (1 - life01 * 0.7);
      const x = sx0 + vx0 * age * drag + turbulence;
      const y = sy0 + vy0 * age * drag + 22 * age * age;

      // Pixel-grid snap (chunky look).
      const px = Math.floor(x / PIX_GRID) * PIX_GRID;
      const py = Math.floor(y / PIX_GRID) * PIX_GRID;

      // Temperature ramp blended through the lane colour.
      // 0..0.18  white-hot core
      // 0.18..0.45  lane colour brightened (mix toward white)
      // 0.45..0.75  pure lane colour
      // 0.75..0.92  lane colour dimmed (mix toward dark-red)
      // 0.92..1   ember fade
      let r, g, b;
      if (life01 < 0.18) {
        r = 255; g = 250; b = 220;
      } else if (life01 < 0.45) {
        // brighten lane colour toward white-hot
        const k = (0.45 - life01) / 0.27;  // 1→0 across this band
        r = Math.round(lr + (255 - lr) * k * 0.6);
        g = Math.round(lg + (255 - lg) * k * 0.6);
        b = Math.round(lb + (220 - lb) * k * 0.6);
      } else if (life01 < 0.75) {
        r = lr; g = lg; b = lb;
      } else if (life01 < 0.92) {
        const k = (life01 - 0.75) / 0.17;  // 0→1
        r = Math.round(lr * (1 - k) + 130 * k);
        g = Math.round(lg * (1 - k) + 30  * k);
        b = Math.round(lb * (1 - k) + 12  * k);
      } else {
        r = 90; g = 20; b = 8;
      }

      const aa = Math.pow(1 - life01, 1.05) * 0.95;
      const size = PIX_GRID + (life01 < 0.4 ? PIX_GRID : 0);        // brighter cores are 2×2 blocks
      ctx.fillStyle = `rgba(${r},${g},${b},${aa.toFixed(3)})`;
      ctx.fillRect(px, py, size, size);
    }
  }
  ctx.restore();

  // ── SMOKE PASS — source-over, low alpha grey pixels rising slowly ──
  ctx.save();
  ctx.globalCompositeOperation = "source-over";
  for (const ring of activeRings) {
    const xRingC = ring.sx;
    for (let i = 0; i < SMOKES_PER_HIT; i++) {
      const seedA = ring.spawnT * 29 + i * 0.731 + 7777;
      const seedB = ring.spawnT * 37 + i * 0.913 + 8888;
      const h1 = _hash01(seedA);
      const h2 = _hash01(seedB);
      const age = audioT - ring.spawnT - SMOKE_DELAY - h1 * 0.20;
      if (age <= 0 || age >= SMOKE_LIFE) continue;
      const life01 = age / SMOKE_LIFE;

      // Slower rise + wider lateral drift.
      const vy0 = -(28 + h1 * 36);
      const vx0 = (h2 - 0.5) * 26;
      const turbulence = Math.sin(age * 3 + h1 * 7) * 18 * Math.min(1, age * 0.5);
      const x = xRingC + vx0 * age + turbulence;
      const y = ring.sy + vy0 * age;

      const px = Math.floor(x / PIX_GRID_SMOKE) * PIX_GRID_SMOKE;
      const py = Math.floor(y / PIX_GRID_SMOKE) * PIX_GRID_SMOKE;

      // Warm dark grey → cool light grey as it dissipates.
      const grey = Math.round(48 + life01 * 110);
      const aa = Math.sin(life01 * Math.PI) * 0.32;                 // ramp-up-then-fade
      const size = PIX_GRID_SMOKE + Math.floor(life01 * 14);        // expands

      ctx.fillStyle = `rgba(${grey},${Math.round(grey * 0.93)},${Math.round(grey * 0.86)},${aa.toFixed(3)})`;
      ctx.fillRect(px, py, size, size);
    }
  }
  ctx.restore();
}

// ── progress bar + timecode ──────────────────────────────────────────
const PROGRESS_BAR_H = 22, PROGRESS_BAR_Y = H - PROGRESS_BAR_H;
function tintRgb(s) {
  const m = s.match(/rgba?\((\d+),\s*(\d+),\s*(\d+)/);
  return m ? [+m[1], +m[2], +m[3]] : [200, 200, 200];
}
function drawProgressBar(audioT) {
  ctx.save();
  const playedX = Math.max(0, audioT) / DURATION * W;
  const lastI = SECTIONS.length - 1;
  for (let si = 0; si < SECTIONS.length; si++) {
    const sec = SECTIONS[si];
    const x0 = si === 0 ? 0 : (sec.startSec / DURATION) * W;
    const x1 = si === lastI ? W : (sec.endSec / DURATION) * W;
    const [r, g, b] = tintRgb(SECTION_TINTS[sec.name] || "rgba(200,200,200,1)");
    ctx.fillStyle = `rgba(${Math.round(r * 0.16)},${Math.round(g * 0.16)},${Math.round(b * 0.16)},0.85)`;
    ctx.fillRect(x0, PROGRESS_BAR_Y, x1 - x0, PROGRESS_BAR_H);
    const fx1 = Math.min(x1, playedX);
    if (fx1 > x0) {
      ctx.fillStyle = `rgba(${r},${g},${b},0.96)`;
      ctx.fillRect(x0, PROGRESS_BAR_Y, fx1 - x0, PROGRESS_BAR_H);
    }
    ctx.fillStyle = "rgba(255,253,242,0.35)";
    ctx.fillRect(x1 - 1, PROGRESS_BAR_Y, 1, PROGRESS_BAR_H);
  }
  ctx.restore();
}
const tcTint = createCanvas(8, 8), tcTintCtx = tcTint.getContext("2d");
function drawTimecode(audioT) {
  const sec = Math.min(Math.max(0, Math.floor(audioT)), Math.ceil(DURATION));
  const mm = Math.floor(sec / 60), ss = (sec - mm * 60).toString().padStart(2, "0");
  const entry = tcCache.get(`${mm}:${ss} / ${totMm}:${totSs}`);
  if (!entry) return;
  const { img, shadow } = entry;
  const env = envAt(audioT);
  const x = W - img.width - 32;
  const y = PROGRESS_BAR_Y - img.height - 14 - 14 * env;
  ctx.save();
  ctx.globalAlpha = 0.95;
  ctx.drawImage(shadow, x + 3, y + 4);
  ctx.drawImage(shadow, x + 2, y + 3);
  ctx.globalAlpha = 1;
  const [tr, tg, tb] = tintRgb(SECTION_TINTS[SECTIONS[sectionIndexAt(audioT)].name] || "rgba(255,253,242,1)");
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

// ── render loop → ffmpeg ─────────────────────────────────────────────
mkdirSync(dirname(OUT), { recursive: true });
const TEST_MODE = FRAMES_OVERRIDE !== null;
const startFrame = Math.max(0, Math.floor(START_T * FPS));
const endFrame = Math.min(FRAMES, startFrame + (FRAMES_OVERRIDE ?? FRAMES));

let ff = null;
if (!TEST_MODE) {
  ff = spawnFFmpegEncode({ audioPath: AUDIO, w: W, h: H, fps: FPS, outPath: OUT });
  ff.on("error", (e) => { console.error(`✗ ffmpeg spawn failed: ${e.message}`); process.exit(1); });
}

progress.begin({ type: "video", label: `${SLUG} ${TEST_MODE ? "test" : (REEL ? "reel" : "insta-story")} · ${endFrame - startFrame} frames` });
console.log(`  rendering frames ${startFrame}..${endFrame - 1} (${endFrame - startFrame} frames)${TEST_MODE ? " → /tmp/hellsine-test-*.png" : ""} …`);
const t0 = Date.now();
let prevNoteT = startFrame > 0 ? (startFrame / FPS) - 0.001 : -1;

for (let f = startFrame; f < endFrame; f++) {
  const audioT = f / FPS;
  const env = Math.min(1, envAt(audioT));
  const punch = punchAt(audioT);

  for (const L of LANES) {
    const yC = laneCenterY[L.key];
    const lrgb = hexToRgb(L.color);
    let amp = L.key === "kick" ? 22 : 11;
    for (const ev of laneEvents[L.key]) {
      if (ev.t <= prevNoteT) continue;
      if (ev.t > audioT) break;
      const sign = (Math.floor(ev.t * 7) % 2) ? 1 : -1;
      _vs.pluck(yC, amp, sign, lrgb);
    }
  }
  prevNoteT = audioT;
  _vs.step();

  const idx = sectionIndexAt(audioT);
  const sec = SECTIONS[idx];
  const since = audioT - sec.startSec;
  if (flags.debug) {
    // Debug mode: panel replaced with solid black so flame/string
    // alignment is unambiguous against a flat background.
    ctx.fillStyle = "rgb(0,0,0)";
    ctx.fillRect(0, 0, W, H);
  } else if (idx > 0 && since >= 0 && since < TRANS_S) {
    let p = since / TRANS_S;
    p = p * p * (3 - 2 * p);
    renderPanel(offACtx, idx - 1, audioT, env, punch);
    renderPanel(offBCtx, idx, audioT, env, punch);
    applyTransition(transitionForBoundary(idx), offA, offB, p);
  } else {
    renderPanel(ctx, idx, audioT, env, punch);
  }

  const theta = (audioT / DURATION) * Math.PI * 2;
  warpUnderString(theta);
  _vs.withRotation(theta, () => {
    drawLanes(audioT);
    _vs.draw();
    drawStringGlow(env, audioT);
  });
  // Flame + smoke render in SCREEN space (no rotation transform), so
  // particles anchor to the rotated string at spawn time then rise with
  // global vertical gravity regardless of further string rotation.
  drawStringFireRings(audioT);

  drawWatermark(audioT);
  // Progress bar in BOTH reel + insta-story (per-section colored bar,
  // no timecode). Timecode stays insta-story-only — the reel format
  // doesn't want digits ticking under the action.
  drawProgressBar(audioT);
  if (!REEL) drawTimecode(audioT);

  if (TEST_MODE) {
    const png = canvas.toBuffer("image/png");
    const fname = `/tmp/hellsine-test-${f.toString().padStart(4, "0")}.png`;
    (await import("node:fs")).writeFileSync(fname, png);
  } else {
    const buf = canvas.toBuffer("raw");
    if (!ff.stdin.write(buf)) await new Promise((r) => ff.stdin.once("drain", r));
  }

  if (f % 30 === 0 || f === endFrame - 1) {
    const done = f - startFrame + 1;
    const total = endFrame - startFrame;
    progress.update((done / Math.max(1, total)) * 100, { done, total });
    process.stdout.write(`\r  frame ${done}/${total}  `);
  }
}
if (!TEST_MODE) ff.stdin.end();
if (!TEST_MODE) await new Promise((res, rej) => {
  ff.on("close", (code) => code === 0 ? res() : rej(new Error(`ffmpeg exited ${code}`)));
});
progress.end();
console.log(`\n✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → ${OUT.replace(REPO + "/", "")}`);
