#!/usr/bin/env node
// marimba/bin/preview-score.mjs — storyline visualizer for marimbaba.
//
// Renders the 10 help-call section panels (gen-sections.mjs) into a
// 9:16 Instagram-Story mp4. Built on the shared engine
// (pop/lib/preview-shared.mjs + cover-engine.mjs). Features:
//
//   • THE STRING + SOUND ELEMENTS — a verlet-physics playhead string
//     down the centre; the marimba's 161 note events (struct.json,
//     emitted by render-marimbaba.mjs) ride past it as pitch-coloured
//     blocks across 4 voice lanes (rosewood / bass / kalimba /
//     vibraphone). Each note PLUCKS the string as it crosses.
//   • 3-LAYER BACKLIGHT — (1) a warm glow rising from BEHIND the
//     figures [screen], (2) a contrast vignette darkening the
//     periphery so they read backlit, not lit-on-top [multiply],
//     (3) a per-figure halo on the form bboxes [screen].
//   • FACE ZOOM in + out — the camera eases between a wide framing
//     and a tight face crop twice per section, alternating jeffrey /
//     gates, so it breathes in and out across the story.
//   • TRANSITIONS — 6 fresh transition types (iris / blinds / push /
//     zoom-punch / pixel-dissolve / diagonal-wipe), cycled across the
//     9 section boundaries — none of them the trancenwaltz cross-
//     dissolve / slide-fade / shred-glitch.
//   • side pals watermarks, bouncing title, segmented progress bar,
//     timecode.
//
// Usage:
//   node pop/marimba/bin/render-marimbaba.mjs    # → audio + struct.json
//   node pop/marimba/bin/gen-sections.mjs        # → the 10 panels
//   node pop/marimba/bin/preview-score.mjs       # → the mp4

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

const SLUG   = flags.slug  || "marimbaba";
const TITLE  = flags.title || SLUG;
const COVER  = flags.cover  || `${LANE}/out/${SLUG}-p-cover.png`;
const AUDIO  = flags.audio  || `${LANE}/out/${SLUG}.mp3`;
const STRUCT = flags.struct || `${LANE}/out/${SLUG}.struct.json`;
const _isReel = flags.reel === true;   // (REEL is declared below after SIZE; use the flag directly here)
const OUT    = flags.out
  || `${LANE}/out/${SLUG}-preview-score-portrait-${_isReel ? "reel" : "insta-story"}.mp4`;
const FPS    = Number(flags.fps ?? 30);
const SIZE   = flags.size || "1080x1920";
const [W, H] = SIZE.split("x").map(Number);
// --reel → IG-reel cut: no title, no segmented progress bar, no
// timecode (the pals stay). Output file gets a `-reel` infix so the
// chromed insta-story and the chrome-free reel can both coexist.
const REEL   = flags.reel === true;
// --start T --frames N → tiny test render (~1s) so layout tweaks can
// be checked in seconds instead of an 11-min full render.
const START_T = Number(flags.start ?? 0);
const FRAMES_OVERRIDE = flags.frames ? Number(flags.frames) : null;

checkYwftAvailable();
for (const [name, p] of [["audio", AUDIO], ["struct", STRUCT]]) {
  if (!existsSync(p)) {
    console.error(`✗ ${name} missing: ${p.replace(REPO + "/", "")}`);
    if (name === "struct") console.error(`  emit it: node pop/marimba/bin/render-marimbaba.mjs --no-open`);
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

// ── voice lanes — the marimba's sound elements ───────────────────────
// 5 voices from struct.events. Each lane gets a base colour; individual
// notes are tinted toward their notepat pitch-colour.
const LANES = [
  { key: "rosewood",   color: "#e8a13c" },   // warm amber — the singer / lead
  { key: "vibraphone", color: "#7fb3c8" },   // soft blue — dream-haze pad
  { key: "kalimba",    color: "#ffd24a" },   // bright gold — twinkles
  { key: "bass",       color: "#c8643c" },   // deep terracotta — the rocking pulse
  { key: "bubbles",    color: "#b88ed4" },   // soft violet — SDT bubble accents
];
const laneEvents = {};
for (const L of LANES) {
  laneEvents[L.key] = (struct.events?.[L.key] || []).slice().sort((a, b) => a.t - b.t);
}
const nEvents = Object.values(laneEvents).reduce((s, a) => s + a.length, 0);
console.log(`  sound elements: ${nEvents} note events across ${LANES.length} voice lanes`);

// bass onsets drive the "sharpen" punch (no kick in a lullaby).
const punchTimes = laneEvents.bass.map((e) => e.t);
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

// ── marimbaba identity ───────────────────────────────────────────────
// Warm lullaby pastels — one tint per story section, the help call's
// changing emotion (anticipation → hello → relief → cozy → uncertain →
// wonder → effort → teamwork → triumph → drowsy calm).
const SECTION_TINTS = {
  hush1:    "rgba(120,140,190,1)",   // cool dusk — anticipation
  hush2:    "rgba(150,170,210,1)",   // dusk blue — the doorway hello
  twinkle1: "rgba(214,200,165,1)",   // warming oat — relief
  twinkle2: "rgba(238,228,205,1)",   // warm cream — cozy
  wow1:     "rgba(206,176,150,1)",   // muted clay — uncertainty
  wow2:     "rgba(244,206,128,1)",   // bright amber — wonder
  baba1:    "rgba(196,176,150,1)",   // tense taupe — anxious effort
  baba2:    "rgba(176,196,168,1)",   // sage — teamwork
  sleep1:   "rgba(236,196,160,1)",   // warm glow — triumph
  sleep2:   "rgba(214,180,180,1)",   // soft rose — drowsy close
};
const TITLE_PALETTE = ["#f0e6cd", "#a6bcd6", "#b0c4a8", "#eec48c", "#d6b4b4"];
const BACKLIGHT_RGB = "255,196,116";       // deeper warm desk-lamp amber (was too pale)

// ── YWFT title + per-second timecode ─────────────────────────────────
console.log("  rasterizing YWFT …");
const assetsDir = AUDIO.replace(/\.mp3$/, ".assets");
mkdirSync(assetsDir, { recursive: true });

const titleFontSize = 96;
const { chars: titleChars, totalWidth: titleTotalW } = await prerenderTitleChars({
  text: TITLE, ptSize: titleFontSize, palette: TITLE_PALETTE,
  shadowColor: "rgba(0,0,0,0.78)", assetsDir,
});
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
// two offscreen layers for the transitions (prev panel / cur panel)
const offA = createCanvas(W, H), offACtx = offA.getContext("2d");
const offB = createCanvas(W, H), offBCtx = offB.getContext("2d");

function safeName(n) {
  return n.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}
let coverImg = existsSync(COVER) ? await loadImage(COVER) : null;
const sectionImgs = [];
let haveImgs = 0;
for (let i = 0; i < SECTIONS.length; i++) {
  const p = `${LANE}/out/${SLUG}-p-sec-${i}-${safeName(SECTIONS[i].name)}.png`;
  if (existsSync(p)) { sectionImgs[i] = await loadImage(p); haveImgs++; }
  else if (coverImg) sectionImgs[i] = coverImg;
  else {
    console.error(`✗ section panel missing and no cover fallback: ${p.replace(REPO + "/", "")}`);
    console.error(`  generate panels first: node pop/marimba/bin/gen-sections.mjs`);
    process.exit(1);
  }
}
console.log(`  section panels: ${haveImgs}/${SECTIONS.length}`);
const imgW = sectionImgs[0].width, imgH = sectionImgs[0].height;

// ── figure + face bboxes (forms.json) ────────────────────────────────
let FORMS = {};
try {
  const fp = `${LANE}/${SLUG}-forms.json`;
  if (existsSync(fp)) FORMS = JSON.parse(readFileSync(fp, "utf8")).sections || {};
} catch { /* no forms → centre fallback */ }
// Supports old shape `figure: [x,y,w,h]` and new shape
// `figure: { figure: [...], face: [...] }`. Backwards-compatible.
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
// Grow a box about its centre to the frame's W:H aspect so the zoom
// fills without side-crop.
function fitFrameAspect(b) {
  let { x, y, w, h } = b;
  if (w / h > FRAME_ASPECT) { const nh = w / FRAME_ASPECT; y -= (nh - h) / 2; h = nh; }
  else                       { const nw = h * FRAME_ASPECT; x -= (nw - w) / 2; w = nw; }
  return { x, y, w, h };
}
// Use the explicit face box if forms.json carries one; otherwise
// derive a generous frame-aspect box centred on the figure's head.
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

// ── pals watermark — two SIDE stamps, faithful to chillwave/dance ────
// rasterize a sharp + a slightly-blurred copy, then tint each per
// frame to the current section colour blended with a slow hue cycle;
// stamp with multiply/burn/overlay + a source-over crisp top pass +
// an LED-pulse screen glow driven by the audio amplitude. Two stamps
// hug the LEFT-BOTTOM and RIGHT-TOP edges, rotated 90° with a
// looping wiggle + swivel locked to the video timeline.
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
      // a whisper of edge softening so the vector stamps sit in the
      // hand-drawn world (not a gaussian ghost — just kills the hard edge).
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
// Compute the pals's current frame colour (section tint blended with
// the slow hue cycle) — shared by drawWatermark AND
// drawPalsTitleChars so the climbing chars track the pals exactly.
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
// Re-tint a pre-rendered title-char glyph (whose pixels have a baked
// drop shadow) to a target rgb via source-in alpha mask. Uses its own
// canvas (separate from wmCanvas) so tinting + drawing don't trample.
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
  const s = 130;
  // PERFECT-LOOP GEOMETRY: position / wiggle / swivel / hue locked to
  // the full video timeline (u: 0 → 1 across all frames), INTEGER
  // harmonics, so the last frame's pals sit exactly where frame 0's do.
  const u = audioT * FPS / FRAMES;
  const TAU = Math.PI * 2;
  const hue = ((u * 360 * 4) % 360 + 360) % 360;       // 4 hue cycles
  const hRgb = hslToRgb(hue, 0.9, 0.62);
  const [sr, sg, sb] = sectionTcRgb(audioT);
  const col = [
    Math.round(sr * 0.6 + hRgb[0] * 0.4),
    Math.round(sg * 0.6 + hRgb[1] * 0.4),
    Math.round(sb * 0.6 + hRgb[2] * 0.4),
  ];
  // LED pulse: audio amplitude brightens + saturates the pals.
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
  const inset = s * 0.34;
  // Diagonal pals: LEFT pals down at bottom-half, RIGHT pals up at top.
  const spots = [
    { cx: inset - wig,     cy: H * 0.86, rot:  Math.PI / 2 + swiv },
    { cx: W - inset + wig, cy: H * 0.16, rot: -Math.PI / 2 - swiv },
  ];
  // pushed deep INTO the image — heavy multiply/burn/overlay, only a
  // whisper of straight-source pass so it stains in.
  const passes = [
    ["multiply",   0.78], ["color-burn", 0.42],
    ["overlay",    0.58], ["source-over", 0.06],
  ];
  for (const sp of spots) {
    ctx.save();
    ctx.translate(sp.cx, sp.cy);
    ctx.rotate(sp.rot);
    // tight dark drop shadow (real black tinted glyph, offset)
    palsTinted([0, 0, 0], palsImg);
    ctx.globalCompositeOperation = "source-over";
    ctx.globalAlpha = 0.26;
    ctx.drawImage(wmCanvas, -s / 2 + 3, -s / 2 + 4, s, s);
    // 4-pass seep
    palsTinted(col, palsBlur);
    for (const [op, a] of passes) {
      ctx.globalCompositeOperation = op;
      ctx.globalAlpha = a;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    }
    // crisp top pass so the logo out-reads its own shadow
    ctx.globalCompositeOperation = "source-over";
    ctx.globalAlpha = 0.30;
    ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    // LED pulse — additive bloom + crisp hot core on loud hits
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
  // After each pals stamp, write the small "marimbaba" chars OVER it,
  // rotated −90° so they read UP the side of the frame (like vertical
  // movie-poster spine text). Each char individually bounces + glows
  // with the audio envelope, staggered.
  drawPalsTitleChars(audioT);
}
// Small "marimbaba" chars climbing the side NEXT TO each pals stamp
// (not over it — so the pals can stay compact). Each char is re-tinted
// per frame to match the pals's section+hue colour so the type reads
// as the same family as the logo.
function drawPalsTitleChars(audioT) {
  if (!palsImg) return;
  const s = 130;                            // matches drawWatermark
  const u = audioT * FPS / FRAMES;
  const TAU = Math.PI * 2;
  const wig  = 13 * Math.sin(TAU * 30 * u) + 4 * Math.sin(TAU * 10 * u);
  const inset = s * 0.34;
  // Chars sit OFFSET from each pals — toward screen centre — so they
  // form a vertical spine of type beside (not on top of) the logo.
  const offset = s / 2 + 24;
  const palsSpots = [
    { charsCx: (inset - wig) + offset,         cy: H * 0.86 },
    { charsCx: (W - inset + wig) - offset,     cy: H * 0.16 },
  ];
  const charScale = 0.46;
  const span = titleTotalW * charScale;
  const palsRgb = palsFrameColor(audioT);   // same tint the pals body uses
  const startX = -span / 2;
  for (const sp of palsSpots) {
    ctx.save();
    ctx.translate(sp.charsCx, sp.cy);
    ctx.rotate(-Math.PI / 2);                // chars climb UP the side
    for (let i = 0; i < titleChars.length; i++) {
      const ch = titleChars[i];
      if (!ch.img) continue;
      const x = startX + ch.prefixWidth * charScale;
      const dw = ch.img.width * charScale;
      const dh = ch.img.height * charScale;
      const charEnv = envAt(audioT - i * 0.03);
      const lift = 4 * Math.sin(audioT * 4.0 + i * 0.8) * (0.3 + charEnv);
      const y = -dh / 2 + lift;
      // dark shadow stamp first (contrast against the illustration)
      ctx.save();
      ctx.globalAlpha = 0.55;
      ctx.drawImage(tintCharGlyph(ch.img, [0, 0, 0]), x + 2, y + 2, dw, dh);
      ctx.restore();
      // tinted-to-pals coloured top, with a warm shadow on loud chars
      if (charEnv > 0.45) {
        ctx.save();
        ctx.shadowColor = "rgba(255,236,170,0.85)";
        ctx.shadowBlur = 8 * Math.min(1, (charEnv - 0.45) / 0.55);
        ctx.drawImage(tintCharGlyph(ch.img, palsRgb), x, y, dw, dh);
        ctx.restore();
      } else {
        ctx.drawImage(tintCharGlyph(ch.img, palsRgb), x, y, dw, dh);
      }
    }
    ctx.restore();
  }
}
// Brighten the section tint toward white by how far through the
// section we are — so the watermark colour reads both WHICH section
// and HOW FAR (progress), exactly like the chillwave/trancenwaltz cut.
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
const PX_PER_SEC = 150;                      // lullaby scroll speed
const _vs = makeVerletString(ctx, { W, H, playheadX: PLAYHEAD_X, duration: DURATION });

// 5 lanes packed into a tight LOWER-MID band so ALL tracks are visible
// together on screen (under the figures' faces, above the progress bar
// + timecode). The string still spans full height and runs through them.
const LANE_TOP = 920, LANE_BOTTOM = H - 180;
const LANE_H = (LANE_BOTTOM - LANE_TOP) / LANES.length;
const laneCenterY = {};
LANES.forEach((L, i) => { laneCenterY[L.key] = LANE_TOP + i * LANE_H + LANE_H / 2; });

// Groove radius — note blocks curve around this radius about the
// canvas centre, so the rotating disc actually reads as a record:
// straight at the needle, arcing more the further out. ~85% of min
// dimension matches chillwave/trancenwaltz.
const GROOVE_R = Math.round(Math.min(W, H) * 0.85);

// ── illustration distortion UNDER the bent string ───────────────────
// Ported faithfully from chillwave's warpUnderString: snapshot the
// upright frame, counter-rotate it by -θ into a big square buffer,
// then redraw a tall strip near the playhead under +θ with each row
// shifted by the local string deflection. Two rotations cancel for the
// CONTENT (illustration stays upright, never spins) while ONLY the
// distortion field under the string lives in the string's rotated
// frame — so the picture visibly bends where the string is bent.
const WU_HALF = 200, WU_W = WU_HALF * 2, WU_STEP = 6, WU_BANDS = 7;
const WU_STR = 0.85, WU_BW = WU_W / WU_BANDS;
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
  if (Math.abs(devPeak) < 2) return;             // string ~straight → skip
  // 1) snapshot the upright frame (illustration + backlight)
  wuSnapC.clearRect(0, 0, W, H);
  wuSnapC.drawImage(canvas, 0, 0);
  // 2) counter-rotate it by −θ, centred in the big square buffer
  wuCRC.setTransform(1, 0, 0, 1, 0, 0);
  wuCRC.clearRect(0, 0, WU_DSZ, WU_DSZ);
  wuCRC.translate(WU_DSZ / 2, WU_DSZ / 2);
  wuCRC.rotate(-theta);
  wuCRC.translate(-W / 2, -H / 2);
  wuCRC.drawImage(wuSnap, 0, 0);
  wuCRC.setTransform(1, 0, 0, 1, 0, 0);
  // 3) redraw the strip under +θ, each band shifted by the local
  //    string deflection (content upright, strip rotated with the string)
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
// Drawn onto whatever ctx the panel was drawn to (main OR an offscreen
// transition layer) so the light travels with the panel through a
// transition. Layer 1: warm glow rising from BEHIND the figures
// [screen]. Layer 2: contrast vignette — darkens the periphery so the
// lit centre reads backlit, not glow-on-top [multiply]. Layer 3 (the
// per-figure halo) is drawn by the caller via drawFormBacklight.
// ── STAINED-GLASS TRANSMISSION BACKLIGHT (trancenwaltz mechanism) ───
// The illustration is treated as a stained-glass panel: each pixel's
// LUMINANCE determines how much light passes through. Bright / light-
// pencilled regions transmit (glow with the backlight colour); dark
// linework + shadows act as leaded came — they block the light and
// stay punchy. This is why the light reads as actually coming from
// BEHIND the figures, not stamped on top. Computed once per panel
// image (cached on a WeakMap), then composited per frame.
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
  // Transmission curve: smoothstep across [LO, HI] in luminance, then
  // raised to GAMMA so the dark half is crushed toward zero (lead lines
  // fully block light → high contrast on darks). A small FLOOR lets a
  // whisper of colour bleed through near-blacks so the panel doesn't
  // read as dead matte.
  // Cap max transmission at 0.65 so the brightest pixels (white
  // typewriter, white paper, lamp highlight) don't blow out to pure
  // white when the warm glow is composited additively.
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
// Shared offscreen for the gated glow.
const glowCanvas = createCanvas(W, H);
const glowCtx = glowCanvas.getContext("2d");

// LAYER 1 — strong contrast vignette: deeply darken everything except
// a tight pool around where the figures sit. Near-black corners.
function drawVignette(c, intensity) {
  const gx = W / 2, gy = H * 0.55;
  c.save();
  c.globalCompositeOperation = "multiply";
  const vg = c.createRadialGradient(gx, gy, H * 0.10, gx, gy, H * 0.72);
  vg.addColorStop(0,    "rgba(255,255,255,1)");
  // WARM-TINTED darkening — amber/brown mid + deep-amber edges (not
  // neutral grey), so the dark surround reads as night-lamp shadow,
  // not cold blue. Preserves the original colored-pencil warmth.
  vg.addColorStop(0.45, `rgba(168,118,72,${(0.62 + 0.12 * intensity).toFixed(3)})`);
  vg.addColorStop(1,    `rgba(36,20,10,${(0.94).toFixed(3)})`);
  c.fillStyle = vg;
  c.fillRect(0, 0, W, H);
  c.restore();
  c.globalCompositeOperation = "source-over";
}
// LAYER 2 — TRANSMITTED light (stained-glass): stamp a hot radial glow
// at each face region, gate it by the panel's transmission mask, then
// composite additively. Light shines THROUGH the bright illustration
// pixels at the face regions; dark linework blocks it. Reads as light
// genuinely coming from behind the figures, never as an overlay.
function drawTransmittedBacklight(c, sectionImg, xform, faces, intensity) {
  if (!faces || !faces.length || !xform || intensity <= 0.01) return;
  const k = Math.min(1, intensity);
  // Stamp the glow into the offscreen buffer at face positions.
  glowCtx.globalCompositeOperation = "source-over";
  glowCtx.clearRect(0, 0, W, H);
  for (const f of faces) {
    const cx = xform.x + (f.x + f.w / 2) * xform.scale;
    const cy = xform.y + (f.y + f.h / 2) * xform.scale;
    const radius = Math.max(f.w, f.h) * xform.scale * 1.4;
    if (radius <= 1) continue;
    const g = glowCtx.createRadialGradient(cx, cy, 0, cx, cy, radius);
    // All WARM (no white core) — so the glow tints the bright pixels
    // with the lamp colour instead of blowing them out to pure white.
    // The desk-lamp warmth matches the illustration's own diegetic light.
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
  // GATE by the transmission mask — drawn at the same xform that drew
  // the panel, so the mask aligns pixel-perfect with the illustration.
  glowCtx.globalCompositeOperation = "destination-in";
  const mask = getTransmissionMask(sectionImg);
  glowCtx.drawImage(mask, xform.x, xform.y,
    sectionImg.width * xform.scale, sectionImg.height * xform.scale);
  glowCtx.globalCompositeOperation = "source-over";
  // Composite onto the scene — softer additive wash so the illustration
  // colours (cream paper, sage knit, blue shirt) still read.
  c.save();
  c.globalCompositeOperation = "lighter";
  c.globalAlpha = 0.45;
  c.drawImage(glowCanvas, 0, 0);
  c.globalAlpha = 0.12;
  c.drawImage(glowCanvas, -4, -4, W + 8, H + 8);
  c.restore();
  c.globalCompositeOperation = "source-over";
}
// LAYER 3 — LEADED contrast: the inverse-mask pass keeps the darks
// punchy (dark linework reads as the "leading" of the stained glass).
// Stamp the same regions in a darker tone, destination-out by the mask
// (so only the DARK parts of the panel get this treatment), multiply
// onto the scene.
function drawLeadedContrast(c, sectionImg, xform, faces, intensity) {
  if (!faces || !faces.length || !xform || intensity <= 0.01) return;
  const k = Math.min(1, intensity);
  glowCtx.globalCompositeOperation = "source-over";
  glowCtx.clearRect(0, 0, W, H);
  for (const f of faces) {
    const cx = xform.x + (f.x + f.w / 2) * xform.scale;
    const cy = xform.y + (f.y + f.h / 2) * xform.scale;
    const radius = Math.max(f.w, f.h) * xform.scale * 1.4;
    if (radius <= 1) continue;
    const g = glowCtx.createRadialGradient(cx, cy, 0, cx, cy, radius);
    g.addColorStop(0.0,  `rgba(70,52,38,${(0.55 * k).toFixed(3)})`);
    g.addColorStop(0.50, `rgba(70,52,38,${(0.30 * k).toFixed(3)})`);
    g.addColorStop(1.0,  "rgba(70,52,38,0)");
    glowCtx.globalCompositeOperation = "lighter";
    glowCtx.fillStyle = g;
    glowCtx.fillRect(
      Math.max(0, cx - radius), Math.max(0, cy - radius),
      Math.min(W, radius * 2), Math.min(H, radius * 2),
    );
  }
  // Keep ONLY where the panel is DARK (inverse of transmission).
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

// ── panel render (one section, with face-zoom + 3-layer light) ───────
// zoomPad MUST be 1.0 — drawCoverKenBurns's zoom path only guarantees
// the frame is covered when pad ≤ 1 (pad > 1 shrinks sc → black gaps).
// envWobbleAmp = 0 + small wobbleAmp = SMOOTH camera, no amplitude
// reactivity. Only the backlight beats with the audio — the camera
// glides on its own slow pacing.
const KB = { breathAmp: 0.05, breathPeriodSec: 18, wobbleAmp: 4, envWobbleAmp: 0, zoomPad: 1.0 };
// Smooth global zoom LFO — continuous across section boundaries (no
// per-section reset), no envelope coupling. ~3 gentle in/out pushes
// over the lullaby. Lower amplitude than v8 so the zoom never crops a
// figure off-frame; combined with bounding-box zoom (both heads), it
// just BREATHES between the pair instead of punching tight.
const ZOOM_CYCLES = 3;
function zoomAt(audioT) {
  const swing = 0.5 - 0.5 * Math.cos(2 * Math.PI * ZOOM_CYCLES * audioT / DURATION);
  return 0.04 + 0.30 * swing;
}
// Zoom target = bounding box of ALL faces in the section (so BOTH
// heads stay in frame at peak zoom), padded a touch and fit to the
// frame's aspect. The camera focuses BETWEEN the heads by default,
// never on a single face that crops the other one off.
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
// Render section `idx` (zoomed + 3-layer backlit) onto target ctx `c`.
// Returns the panel→frame xform.
function renderPanel(c, idx, audioT, env, punch) {
  const name = SECTIONS[idx].name;
  const figs  = figureBoxes(name);
  const faces = faceBoxes(name);
  // Always focus on the CENTRE-POINT BETWEEN both heads (bounding box
  // of all available faces) — both stay in frame at peak zoom, the
  // camera just breathes in and out between them.
  const z = zoomAt(audioT);
  const zoomBox = lerpBox(WHOLE_BOX, zoomTargetBox(faces, figs), z);
  const xform = drawCoverKenBurns(c, sectionImgs[idx], audioT, {
    ...KB, env, punch, zoomBox, fillBackground: true,
  });
  // ── 3-LAYER STAINED-GLASS BACKLIGHT (trancenwaltz mechanism) ───────
  // The illustration acts as a stained-glass panel — backlight shines
  // THROUGH bright pixels, blocked by dark linework. Light is genuinely
  // behind the figures, never overlaid on top.
  // 1: contrast vignette — darken the OUTER frame so the figures pop.
  drawVignette(c, 1);
  // 2: TRANSMITTED light — gated by the panel's own luminance mask, so
  //    the glow only shines through the bright illustration parts at
  //    each face region. Beats with the audio + note onsets.
  const blI = 0.40 + env * 0.55 + punch * 0.40;
  drawTransmittedBacklight(c, sectionImgs[idx], xform, faces, blI);
  // 3: LEADED contrast — the inverse-mask multiply pass that keeps the
  //    dark linework PUNCHY, like the lead between glass.
  drawLeadedContrast(c, sectionImgs[idx], xform, faces, blI * 0.7);
  return xform;
}

// ── TRANSITIONS — 6 fresh types, cycled across the 9 boundaries ──────
const TRANS_S = 0.85;                        // fast changes
const TRANSITIONS = ["iris", "blinds", "push", "zoomPunch", "pixel", "diagonal"];
function transitionForBoundary(idx) { return TRANSITIONS[(idx - 1) % TRANSITIONS.length]; }
const _pixCanvas = createCanvas(W, H), _pixCtx = _pixCanvas.getContext("2d");
function applyTransition(kind, prevC, curC, p) {
  // p 0→1. prevC/curC are the finished panel layers. Composites to main ctx.
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
    // prev zooms out + fades; cur punches in from oversized.
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
  } else { // diagonal wipe
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

// ── lanes + string draw (the sound elements riding the string) ───────
// GEOMETRY + HIGHLIGHTING faithfully ported from pop/dance/bin/
// cover-video.mjs (trancenwaltz):
//   • Per-block aGroove rotation — each column rotated about the disc
//     centre by (bx − playhead) / GROOVE_R, so the lanes are STRAIGHT
//     at the needle and arc more the further out — a real record groove.
//   • Flash highlighting — the whole event pumps to FULL opacity for
//     a 0.18 s flashWindow after trigger (the "now firing" pulse).
//   • Per-column press/held/played alpha is the WAVEFORM-pixel layer
//     riding under the flash bump.
//   • Audio-peak waveform: each column's amplitude comes from the
//     actual mix peak in its tiny time window (mini-DAW per event).
function needleXAt(y) { return _vs.needleXAt(y); }
function drawLanes(audioT) {
  const halfSpan = (W * 0.62) / PX_PER_SEC;
  const FLASH_WIN = 0.18;
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  for (const L of LANES) {
    const yC = laneCenterY[L.key];
    const laneRgb = hexToRgb(L.color);
    for (const ev of laneEvents[L.key]) {
      if (ev.t > audioT + halfSpan) break;
      const dur = ev.dur || 0.25;
      if (ev.t + dur < audioT - halfSpan) continue;
      // Cap visible width so a long-sustain marimba note doesn't render
      // as a 600-pixel slab (same trick trancenwaltz uses for pitched).
      const visDur = Math.min(dur, 0.30);
      const ex = PLAYHEAD_X + (ev.t - audioT) * PX_PER_SEC;
      const ew = Math.max(4, visDur * PX_PER_SEC);
      // Per-event FLASH highlighting — whole event pumps brighter on
      // trigger, decays over FLASH_WIN.
      const sinceTrigger = audioT - ev.t;
      let flash = 0;
      if (sinceTrigger >= 0 && sinceTrigger < FLASH_WIN) flash = 1 - sinceTrigger / FLASH_WIN;
      const isFuture = ev.t > audioT + 0.02;
      const isPlayed = sinceTrigger >= FLASH_WIN;
      let baseAlpha = 1.0;
      if (isFuture) baseAlpha = 0.55;
      else if (isPlayed) baseAlpha = 0.82;
      if (flash > 0) baseAlpha = Math.min(1.0, baseAlpha + flash * 0.40);
      // pitch colour, mostly the notepat note-colour, touch of voice colour
      const nrgb = midiToNotepatRgb(ev.midi);
      const cr = Math.round(nrgb[0] * 0.78 + laneRgb[0] * 0.22);
      const cg = Math.round(nrgb[1] * 0.78 + laneRgb[1] * 0.22);
      const cb = Math.round(nrgb[2] * 0.78 + laneRgb[2] * 0.22);
      const rgb = `${cr},${cg},${cb}`;
      const fullH = Math.min(96, 30 + 86 * Math.min(1, ev.gain ?? 0.5));
      const cols = Math.max(2, Math.floor(ew / 9));
      const blockW = Math.max(3, ew / cols - 2);
      const bend = (needleXAt(yC) - PLAYHEAD_X) * 0.5;
      // Audio waveform sampling — per column, pull peak from the mix.
      const startSamp = Math.max(0, Math.floor(ev.t * audioSr));
      const endSamp = Math.min(audio.length - 1, Math.floor((ev.t + visDur) * audioSr));
      const spc = (endSamp - startSamp) / cols;
      for (let c = 0; c < cols; c++) {
        const s0 = startSamp + Math.floor(c * spc);
        const s1 = Math.min(endSamp, startSamp + Math.floor((c + 1) * spc));
        let pk = 0;
        for (let s = s0; s < s1; s++) { const a = Math.abs(audio[s]); if (a > pk) pk = a; }
        pk = Math.min(1, (pk / audioPeak) * 1.6);
        const tCol = ev.t + (c / cols) * visDur;
        const dt = audioT - tCol;
        let alpha;
        if (dt < 0) alpha = 0.16;                              // future — dim
        else if (dt < 0.05) alpha = 1.0;                       // PRESSED — at needle
        else if (dt < 0.45) alpha = 1.0 - (dt - 0.05) / 0.40 * 0.55;  // held — fade
        else alpha = 0.42;                                     // played — settled
        // BAKE the event-level baseAlpha bump into the per-column alpha
        // so the flash highlight pumps the whole bar in unison.
        alpha *= baseAlpha;
        const half = Math.max(2, (pk * fullH) / 2);
        const bx = ex + (c / cols) * ew + bend;
        // GROOVE CURVE — rotate this block about the disc centre by
        // (bx − playhead) / GROOVE_R so the track arcs along the record.
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

// A bright additive overdraw of the string so the playhead spine reads
// boldly over the illustration (the engine's own draw() is subtle).
function drawStringGlow(env) {
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  ctx.lineCap = "round";
  ctx.strokeStyle = `rgba(255,246,212,${(0.34 + 0.30 * env).toFixed(3)})`;
  ctx.lineWidth = 3.2;
  ctx.beginPath();
  for (let y = -20; y <= H + 20; y += 10) {
    const x = needleXAt(y);
    if (y <= -20) ctx.moveTo(x, y); else ctx.lineTo(x, y);
  }
  ctx.stroke();
  ctx.restore();
  ctx.globalCompositeOperation = "source-over";
}

// ── title ────────────────────────────────────────────────────────────
// Title now flanks the SIDES like the pals watermarks: the letters of
// "marimbaba" stack vertically — first half down the LEFT edge, second
// half down the RIGHT edge — each bouncing and glowing with the audio
// envelope. No more horizontal banner across the top.
function drawTitle(audioT, env) {
  const total = titleChars.length;
  const half = Math.ceil(total / 2);
  const charH = Math.ceil(titleFontSize * 1.05);
  const leftH  = half * charH;
  const rightH = (total - half) * charH;
  const leftY0  = Math.round((H - leftH) / 2);
  const rightY0 = Math.round((H - rightH) / 2);
  const insetLeft  = 28;
  const insetRight = 28;
  for (let i = 0; i < total; i++) {
    const ch = titleChars[i];
    if (!ch.img) continue;
    const onLeft = i < half;
    const slot = onLeft ? i : i - half;
    const xBase = onLeft ? insetLeft : W - ch.img.width - insetRight;
    const yBase = (onLeft ? leftY0 : rightY0) + slot * charH;
    // per-char envelope sample — staggered like a spectrum bar, so
    // letters react in sequence as the audio passes.
    const charEnv = envAt(audioT - i * 0.04);
    // small horizontal wiggle (toward / away from the edge) + vertical
    // bounce, both audio-reactive but gentle.
    const wig = (onLeft ? 1 : -1) * (3 + 8 * charEnv) * Math.sin(audioT * 2.6 + i * 0.9);
    const lift = (8 + 26 * charEnv) * Math.sin(audioT * 3.2 + i * 0.7);
    const x = xBase + wig;
    const y = yBase + lift;
    // glow shadow only on loud chars (cheap when sparse)
    if (charEnv > 0.5) {
      ctx.save();
      ctx.shadowColor = "rgba(255,236,170,0.92)";
      ctx.shadowBlur = 12 * Math.min(1, (charEnv - 0.5) / 0.5);
      ctx.drawImage(ch.img, x, y);
      ctx.restore();
    } else {
      ctx.drawImage(ch.img, x, y);
    }
  }
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
// --frames N → TEST MODE: skip ffmpeg, write each rendered frame as a
// PNG to /tmp/marimba-test-NNNN.png. Pair with --start T to seek to a
// specific time. Lets layout tweaks be verified in seconds.
const TEST_MODE = FRAMES_OVERRIDE !== null;
const startFrame = Math.max(0, Math.floor(START_T * FPS));
const endFrame = Math.min(FRAMES, startFrame + (FRAMES_OVERRIDE ?? FRAMES));

let ff = null;
if (!TEST_MODE) {
  ff = spawnFFmpegEncode({ audioPath: AUDIO, w: W, h: H, fps: FPS, outPath: OUT });
  ff.on("error", (e) => { console.error(`✗ ffmpeg spawn failed: ${e.message}`); process.exit(1); });
}

progress.begin({ type: "video", label: `${SLUG} ${TEST_MODE ? "test" : "insta-story"} · ${endFrame - startFrame} frames` });
console.log(`  rendering frames ${startFrame}..${endFrame - 1} (${endFrame - startFrame} frames)${TEST_MODE ? " → /tmp/marimba-test-*.png" : ""} …`);
const t0 = Date.now();
let prevNoteT = startFrame > 0 ? (startFrame / FPS) - 0.001 : -1;

for (let f = startFrame; f < endFrame; f++) {
  const audioT = f / FPS;
  const env = Math.min(1, envAt(audioT));
  const punch = punchAt(audioT);

  // pluck the string for every note onset since the last frame
  for (const L of LANES) {
    const yC = laneCenterY[L.key];
    const lrgb = hexToRgb(L.color);
    let amp = L.key === "bass" ? 19 : L.key === "rosewood" ? 13 : 9;
    for (const ev of laneEvents[L.key]) {
      if (ev.t <= prevNoteT) continue;
      if (ev.t > audioT) break;
      const sign = (Math.floor(ev.t * 7) % 2) ? 1 : -1;
      _vs.pluck(yC, amp, sign, lrgb);
    }
  }
  prevNoteT = audioT;
  _vs.step();

  // ── scene: panel + face-zoom + 3-layer backlight, with transitions ─
  const idx = sectionIndexAt(audioT);
  const sec = SECTIONS[idx];
  const since = audioT - sec.startSec;
  if (idx > 0 && since >= 0 && since < TRANS_S) {
    let p = since / TRANS_S;
    p = p * p * (3 - 2 * p);                 // smoothstep
    renderPanel(offACtx, idx - 1, audioT, env, punch);
    renderPanel(offBCtx, idx, audioT, env, punch);
    applyTransition(transitionForBoundary(idx), offA, offB, p);
  } else {
    renderPanel(ctx, idx, audioT, env, punch);
  }

  // ── the sound elements + the string, over the scene ───────────────
  // ── disc rotation — the string + lane note-blocks rotate as one
  // record around the canvas centre, like the trancenwaltz / chillwave
  // cuts. ~1 full turn over the lullaby — calm, not dizzy.
  const theta = (audioT / DURATION) * Math.PI * 2 * 1.0;
  // warpUnderString bends the storyline illustration where the string
  // is bent — the picture stays UPRIGHT but the strip under the string
  // slumps with the bend (chillwave's "string-vibe" effect).
  warpUnderString(theta);
  // Lanes + string rotate together as one disc. The illustration
  // underneath stays upright (we don't wrap it).
  _vs.withRotation(theta, () => {
    drawLanes(audioT);
    _vs.draw();
    drawStringGlow(env);
  });

  // ── HUD ───────────────────────────────────────────────────────────
  // drawWatermark draws BOTH the pals stamps and the small rotated
  // "marimbaba" chars over each pals — the title now LIVES on the
  // pals as side watermarks. Only the progress bar + timecode below
  // are the "chrome" the reel strips.
  drawWatermark(audioT);
  if (!REEL) drawProgressBar(audioT);
  if (!REEL) drawTimecode(audioT);

  if (TEST_MODE) {
    const png = canvas.toBuffer("image/png");
    const fname = `/tmp/marimba-test-${f.toString().padStart(4, "0")}.png`;
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
