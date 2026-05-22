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
  drawFormBacklight, drawTitleBounce, spawnFFmpegEncode, AUDIO_SR_DEFAULT,
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
const OUT    = flags.out    || `${LANE}/out/${SLUG}-preview-score-portrait-insta-story.mp4`;
const FPS    = Number(flags.fps ?? 30);
const SIZE   = flags.size || "1080x1920";
const [W, H] = SIZE.split("x").map(Number);

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
const { audio, sr: audioSr } = decodeAudioMono(AUDIO, AUDIO_SR_DEFAULT);
const envelope = computeRmsEnvelope(audio, audioSr, FPS, DURATION);
function envAt(t) {
  const idx = Math.floor(t * FPS);
  return (idx < 0 || idx >= envelope.length) ? 0 : envelope[idx];
}

// ── voice lanes — the marimba's sound elements ───────────────────────
// 4 voices from struct.events. Each lane gets a base colour; individual
// notes are tinted toward their notepat pitch-colour.
const LANES = [
  { key: "rosewood",   color: "#e8a13c" },   // warm amber — the singer / lead
  { key: "vibraphone", color: "#7fb3c8" },   // soft blue — dream-haze pad
  { key: "kalimba",    color: "#ffd24a" },   // bright gold — twinkles
  { key: "bass",       color: "#c8643c" },   // deep terracotta — the rocking pulse
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
const BACKLIGHT_RGB = "255,224,170";       // warm desk-lamp glow

// ── YWFT title + per-second timecode ─────────────────────────────────
console.log("  rasterizing YWFT …");
const assetsDir = AUDIO.replace(/\.mp3$/, ".assets");
mkdirSync(assetsDir, { recursive: true });

const titleFontSize = 96;
const { chars: titleChars } = await prerenderTitleChars({
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
function figureBoxes(name) {
  const f = FORMS[name];
  if (!f) return [];
  return [f.jeffrey, f.gates].filter(Boolean).map(([x, y, w, h]) => ({ x, y, w, h }));
}
// A face box centred on a figure's head, sized to the FRAME aspect
// (9:16) so the zoom punches in evenly — and never narrower than 42% of
// the panel width, so the face zoom tops out at a believable ~2.4x
// instead of upscaling a tiny far figure to mush.
const FRAME_ASPECT = W / H;
function faceBox(figure) {
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

// ── pals watermark — two SIDE stamps (real purple-pals.svg) ──────────
const PALS_WM = 220;
let palsImg = null;
{
  const svg = `${REPO}/system/public/purple-pals.svg`;
  const png = `${assetsDir}/pals-watermark.png`;
  if (existsSync(svg)) {
    const r = spawnSync("rsvg-convert", ["-w", String(PALS_WM * 2), "-h", String(PALS_WM * 2), "-o", png, svg]);
    if (r.status === 0 && existsSync(png)) palsImg = await loadImage(png);
  }
  console.log(`  pals watermark: ${palsImg ? "loaded" : "MISSING (skipped)"}`);
}
// Two stamps rotated 90°, hugging the LEFT and RIGHT edges (bleeding a
// little off-frame), wiggling + glowing with the audio.
function drawWatermark(audioT, env) {
  if (!palsImg) return;
  const s = PALS_WM;
  const wig = 11 * Math.sin(audioT * 1.7) + 4 * Math.sin(audioT * 0.6);
  const glow = env * env;
  const inset = s * 0.32;                    // hug: ~14% off-edge
  const spots = [
    { cx: inset - wig,     cy: H * 0.30, rot:  Math.PI / 2 },
    { cx: W - inset + wig, cy: H * 0.70, rot: -Math.PI / 2 },
  ];
  for (const sp of spots) {
    ctx.save();
    ctx.translate(sp.cx, sp.cy);
    ctx.rotate(sp.rot + 0.04 * Math.sin(audioT * 1.3));
    ctx.globalAlpha = 0.34 + 0.30 * glow;
    ctx.drawImage(palsImg, -s / 2, -s / 2, s, s);
    ctx.restore();
  }
  ctx.globalAlpha = 1;
}

// ── the verlet string ────────────────────────────────────────────────
const PLAYHEAD_X = Math.round(W / 2);
const PX_PER_SEC = 150;                      // lullaby scroll speed
const _vs = makeVerletString(ctx, { W, H, playheadX: PLAYHEAD_X, duration: DURATION });

// 4 lanes spread down the canvas; the string runs through their centre.
const LANE_TOP = 250, LANE_BOTTOM = H - 150;
const LANE_H = (LANE_BOTTOM - LANE_TOP) / LANES.length;
const laneCenterY = {};
LANES.forEach((L, i) => { laneCenterY[L.key] = LANE_TOP + i * LANE_H + LANE_H / 2; });

// ── 3-LAYER BACKLIGHT ────────────────────────────────────────────────
// Drawn onto whatever ctx the panel was drawn to (main OR an offscreen
// transition layer) so the light travels with the panel through a
// transition. Layer 1: warm glow rising from BEHIND the figures
// [screen]. Layer 2: contrast vignette — darkens the periphery so the
// lit centre reads backlit, not glow-on-top [multiply]. Layer 3 (the
// per-figure halo) is drawn by the caller via drawFormBacklight.
function drawBacklight(c, env, intensity) {
  const gx = W / 2, gy = H * 0.54;           // low-centre, behind the pair
  const pulse = (0.34 + 0.52 * env) * intensity;
  c.save();
  // 1 — CONTRAST VIGNETTE: darken everything but the pool the figures
  // sit in. This is the layer that reads as "lit from behind" — the
  // colored-pencil art is already light, so the look comes from
  // darkening the surround, NOT from flooding light on top.
  c.globalCompositeOperation = "multiply";
  const vg = c.createRadialGradient(gx, gy, H * 0.10, gx, gy, H * 0.74);
  vg.addColorStop(0,    "rgba(255,255,255,1)");
  vg.addColorStop(0.5,  `rgba(196,180,170,${(0.55 + 0.18 * intensity).toFixed(3)})`);
  vg.addColorStop(1,    `rgba(26,22,34,${(0.86 + 0.12 * intensity).toFixed(3)})`);
  c.fillStyle = vg;
  c.fillRect(0, 0, W, H);
  // 2 — a WHISPER of warm glow right behind the pair (not a flood) so
  // the rim catches; tight radius, low alpha.
  c.globalCompositeOperation = "screen";
  const rg = c.createRadialGradient(gx, gy, 0, gx, gy, H * 0.34);
  rg.addColorStop(0,   `rgba(${BACKLIGHT_RGB},${(0.13 * pulse).toFixed(3)})`);
  rg.addColorStop(0.6, `rgba(${BACKLIGHT_RGB},${(0.05 * pulse).toFixed(3)})`);
  rg.addColorStop(1,   `rgba(${BACKLIGHT_RGB},0)`);
  c.fillStyle = rg;
  c.fillRect(0, 0, W, H);
  c.restore();
  c.globalCompositeOperation = "source-over";
}

// ── panel render (one section, with face-zoom + 3-layer light) ───────
// zoomPad MUST be 1.0 — drawCoverKenBurns's zoom path only guarantees
// the frame is covered when pad ≤ 1 (pad > 1 shrinks sc → black gaps).
const KB = { breathAmp: 0.05, breathPeriodSec: 16, wobbleAmp: 9, envWobbleAmp: 13, zoomPad: 1.0 };
// Face-zoom LFO — z ∈ [0,1] swings wide→face→wide TWICE per section so
// the camera visibly pushes IN and OUT across each beat.
function zoomAt(idx, audioT, env) {
  const s = SECTIONS[idx];
  const prog = Math.max(0, Math.min(1, (audioT - s.startSec) / Math.max(0.1, s.endSec - s.startSec)));
  const swing = 0.5 - 0.5 * Math.cos(2 * Math.PI * 2 * prog);   // 0→1→0→1→0
  return Math.max(0, Math.min(0.92, 0.10 + 0.66 * swing + 0.14 * env));
}
// Render section `idx` (zoomed + backlit) onto target ctx `c`. Returns
// the panel→frame xform so the per-figure halo lands on the figures.
function renderPanel(c, idx, audioT, env, punch) {
  const name = SECTIONS[idx].name;
  const figs = figureBoxes(name);
  // alternate which face the camera favours, section by section
  const fav = figs.length ? figs[idx % figs.length] : null;
  const z = zoomAt(idx, audioT, env);
  const zoomBox = lerpBox(WHOLE_BOX, faceBox(fav), z);
  const xform = drawCoverKenBurns(c, sectionImgs[idx], audioT, {
    ...KB, env, punch, zoomBox, fillBackground: true,
  });
  // layers 1+2 — glow-from-behind + contrast vignette
  drawBacklight(c, env, 1);
  // layer 3 — per-figure warm halo
  const blI = 0.30 + env * 0.55 + punch * 0.3;
  drawFormBacklight(c, figs, xform, blI, BACKLIGHT_RGB);
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
function needleXAt(y) { return _vs.needleXAt(y); }
function drawLanes(audioT) {
  const halfSpan = (W * 0.62) / PX_PER_SEC;
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  for (const L of LANES) {
    const yC = laneCenterY[L.key];
    const laneRgb = hexToRgb(L.color);
    for (const ev of laneEvents[L.key]) {
      if (ev.t > audioT + halfSpan) break;
      const dur = ev.dur || 0.25;
      if (ev.t + dur < audioT - halfSpan) continue;
      const ex = PLAYHEAD_X + (ev.t - audioT) * PX_PER_SEC;
      const ew = Math.max(5, dur * PX_PER_SEC);
      // pitch colour, mostly the notepat note-colour (a touch of the
      // voice colour) — kept saturated so the blocks read as crisp
      // elements, not pale smears.
      const nrgb = midiToNotepatRgb(ev.midi);
      const cr = Math.round(nrgb[0] * 0.8 + laneRgb[0] * 0.2);
      const cg = Math.round(nrgb[1] * 0.8 + laneRgb[1] * 0.2);
      const cb = Math.round(nrgb[2] * 0.8 + laneRgb[2] * 0.2);
      // block height ∝ note gain — COMPACT bars (cap ~96px) so the
      // sound elements ride the string instead of washing the frame.
      const fullH = Math.min(96, 30 + 86 * Math.min(1, ev.gain ?? 0.5));
      const cols = Math.max(2, Math.floor(ew / 13));
      const bw = Math.max(4, ew / cols - 2);
      const bend = needleXAt(yC) - PLAYHEAD_X;
      for (let c = 0; c < cols; c++) {
        const tCol = ev.t + (c / cols) * dur;
        const dt = audioT - tCol;
        let alpha, hot;
        // Faint scrolling field, BRIGHT hit at the needle — so the
        // notes pop as discrete sound elements instead of washing the
        // frame with a continuous streak.
        if (dt < 0) { alpha = 0.10; hot = 0; }                       // future — faint
        else if (dt < 0.07) { alpha = 1.0; hot = 1; }                // PRESSED — at the needle
        else if (dt < 0.45) { alpha = 0.95 - (dt - 0.07) / 0.38 * 0.78; hot = 0; }  // held — quick fade
        else { alpha = 0.12; hot = 0; }                              // played — faint trail
        const bx = ex + (c / cols) * ew + bend * 0.6;
        const decay = dt > 0.07 ? Math.exp(-(dt - 0.07) / 0.7) : 1;
        const hh = Math.max(3, fullH * (0.4 + 0.6 * decay)) / 2;
        ctx.fillStyle = `rgba(${cr},${cg},${cb},${alpha.toFixed(3)})`;
        ctx.fillRect(bx, yC - hh, bw, hh * 2);
        // white-hot core on the note as it strikes the string
        if (hot) {
          ctx.fillStyle = "rgba(255,253,242,0.92)";
          ctx.fillRect(bx, yC - hh * 0.42, bw, hh * 0.84);
        }
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
function drawTitle(audioT, env) {
  const titleY = TITLE_TOP_Y + Math.ceil(titleFontSize * 1.25);
  drawTitleBounce(ctx, {
    chars: titleChars, ptSize: titleFontSize, baseX: 30, baseY: titleY,
    audioT, env, getEnvAt: envAt,
    charDelay: 0.04, bounceAmp: 64, restAlpha: 1.0,
    glowThreshold: 0.5, glowMax: 18, wobbleAmp: 2.4,
  });
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
const ff = spawnFFmpegEncode({ audioPath: AUDIO, w: W, h: H, fps: FPS, outPath: OUT });
ff.on("error", (e) => { console.error(`✗ ffmpeg spawn failed: ${e.message}`); process.exit(1); });

progress.begin({ type: "video", label: `${SLUG} insta-story · ${FRAMES} frames` });
console.log(`  rendering ${FRAMES} frames …`);
const t0 = Date.now();
let prevNoteT = -1;

for (let f = 0; f < FRAMES; f++) {
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
  drawLanes(audioT);
  _vs.draw();
  drawStringGlow(env);

  // ── HUD ───────────────────────────────────────────────────────────
  drawWatermark(audioT, env);
  drawTitle(audioT, env);
  drawProgressBar(audioT);
  drawTimecode(audioT);

  const buf = canvas.toBuffer("raw");
  if (!ff.stdin.write(buf)) await new Promise((r) => ff.stdin.once("drain", r));

  if (f % 30 === 0 || f === FRAMES - 1) {
    progress.update((f / FRAMES) * 100);
    process.stdout.write(`\r  frame ${f + 1}/${FRAMES}  `);
  }
}
ff.stdin.end();
await new Promise((res, rej) => {
  ff.on("close", (code) => code === 0 ? res() : rej(new Error(`ffmpeg exited ${code}`)));
});
progress.end();
console.log(`\n✓ ${((Date.now() - t0) / 1000).toFixed(1)}s → ${OUT.replace(REPO + "/", "")}`);
