#!/usr/bin/env node
// chillwave/bin/preview-score-helpabeach-yt.mjs — 1920×1080 landscape
// YouTube fork of preview-score.mjs. Same shared engine; the YT cut
// uses the "-yt" landscape illy set (gen-illy.mjs --landscape) and
// landscape-tuned chrome positions below. No TikTok / IG-platform
// branching — this file is always insta-style chrome at 16:9.
//
// Inputs:
//   --slug helpabeach              (default)
//   --cover  path-to.png            (default: out/<slug>-yt-cover.png)
//   --audio  path-to.mp3            (default: out/<slug>.mp3)
//   --struct path-to.json           (default: out/<slug>.struct.json)
//   --out    path-to.mp4            (default: out/<slug>-preview-score-yt.mp4)
//   --size 1920x1080                (default)
//   --fps 30                        (default)

import { existsSync, readFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import * as progress from "../../lib/render-progress.mjs";
import { createCanvas, loadImage } from "canvas";
import { getNoteColorForOctave } from "../../../system/public/aesthetic.computer/lib/note-colors.mjs";
import {
  checkYwftAvailable,
  decodeAudioMono,
  computeRmsEnvelope,
  prerenderTitleChars,
  magickRenderText,
  drawCoverKenBurns,
  drawFormBacklight,
  drawTitleBounce,
  spawnFFmpegEncode,
  AUDIO_SR_DEFAULT,
} from "../../lib/preview-shared.mjs";
// Shared verlet-string + rotating-disc + string→illustration WARP —
// the SAME engine trancenwaltz uses (pop/lib/cover-engine.mjs).
import { makeVerletString } from "../../lib/cover-engine.mjs";

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

const SLUG   = flags.slug || "helpabeach";
// --title overrides the on-screen title text (defaults to capitalized SLUG).
const TITLE  = flags.title || SLUG;
// YT fork: always landscape, always insta-style chrome — no PORTRAIT /
// TIKTOK branches. Uses the "-yt" illy set (gen-illy.mjs --landscape).
const TAG    = "-yt";
const TIKTOK = false;
const COVER  = flags.cover  || `${LANE}/out/${SLUG}${TAG}-cover.png`;
const AUDIO  = flags.audio  || `${LANE}/out/${SLUG}.mp3`;
const STRUCT = flags.struct || `${LANE}/out/${SLUG}.struct.json`;
const OUT    = flags.out    || `${LANE}/out/${SLUG}-preview-score-yt.mp4`;
const FPS    = Number(flags.fps ?? 30);
const SIZE   = flags.size || "1920x1080";
const [W, H] = SIZE.split("x").map(Number);

checkYwftAvailable();
for (const [name, p] of [["cover", COVER], ["audio", AUDIO], ["struct", STRUCT]]) {
  if (!existsSync(p)) {
    console.error(`✗ ${name} missing: ${p.replace(REPO + "/", "")}`);
    process.exit(1);
  }
}

const struct = JSON.parse(readFileSync(STRUCT, "utf8"));
const DURATION = struct.totalSec;
const FRAMES = Math.ceil(DURATION * FPS);
console.log(`▸ ${SLUG} score-preview · ${W}x${H} · ${DURATION.toFixed(1)}s · ${FRAMES} frames`);

// ── audio decode + envelope ──────────────────────────────────────────
console.log("  decoding audio …");
const { audio, sr: audioSr, audioPeak } = decodeAudioMono(AUDIO, AUDIO_SR_DEFAULT);
const envelope = computeRmsEnvelope(audio, audioSr, FPS, DURATION);
function envAt(t) {
  const idx = Math.floor(t * FPS);
  if (idx < 0 || idx >= envelope.length) return 0;
  return envelope[idx];
}

// ── per-lane audio buffers (display-rate mono peak streams) ──────────
// Each lane has its own downsampled buffer (e.g. 4 kHz) written by
// render.mjs. We use these so each waveform represents that lane's
// actual mix volume, not the full mixed audio.
import { readFileSync as readFileSyncFs, existsSync as existsSyncFs } from "node:fs";
const laneAudio = {};
let laneSr = AUDIO_SR_DEFAULT;
let laneAudioPeak = 1e-6;
if (struct.laneAudio?.paths) {
  laneSr = struct.laneAudio.sampleRate || 4000;
  for (const [key, relPath] of Object.entries(struct.laneAudio.paths)) {
    const full = `${LANE}/${relPath}`;
    if (existsSyncFs(full)) {
      const buf = readFileSyncFs(full);
      const ab = buf.buffer.slice(buf.byteOffset, buf.byteOffset + buf.byteLength);
      const arr = new Float32Array(ab);
      laneAudio[key] = arr;
      for (let i = 0; i < arr.length; i++) {
        const a = Math.abs(arr[i]);
        if (a > laneAudioPeak) laneAudioPeak = a;
      }
    }
  }
  console.log(`  loaded ${Object.keys(laneAudio).length} per-lane buffers @ ${laneSr} Hz`);
}

// ── chillwave identity ───────────────────────────────────────────────
// Lane palette pushed to pure dayglo for strong color differentiation
// between lanes. Plus an FX lane so the start/tail bitcrush + flange
// show up in the timeline.
// Lane palette = a full RAINBOW spread evenly around the hue circle
// (9 lanes → 40° apart), so the dense spinning record reads as a
// rainbow disc rather than a limited palette.
const LANES = [
  { key: "bells",   color: "#ff3b3b", textColor: "#7a1f1f" },   //   0° red
  { key: "birds",   color: "#ff9b2e", textColor: "#7a4a14" },   //  40° orange
  { key: "hat",     color: "#e8e23a", textColor: "#6e6a1a" },   //  80° yellow
  { key: "bubbles", color: "#5cf04a", textColor: "#246b1f" },   // 120° green
  { key: "waves",   color: "#3df2b0", textColor: "#1f6b5a" },   // 160° spring
  { key: "kick",    color: "#3dc2ff", textColor: "#1a5a7a" },   // 200° cyan-blue
  { key: "sub",     color: "#4a6aff", textColor: "#1f2f8a" },   // 240° blue
  { key: "fx",      color: "#b04aff", textColor: "#4a1f8a" },   // 280° violet
  { key: "vox",     color: "#ff4ad8", textColor: "#7a1f63" },   // 320° magenta
];

// Title palette — soft red / white / blue / pink pastels, the Rhizome
// Health clinic register (no more beach-sunlight yellows).
const TITLE_PALETTE = [
  "#f2a6ae", "#ffffff", "#a8c4e6", "#f0c8d2",
  "#eef0f4", "#9fb8de", "#f4d0d8", "#c9d8ef",
];

// Section tints — soft red/white/blue/pink pastels, one per section,
// tracking the checkup arc. Keys MUST match struct.sections names.
const SECTION_TINTS = {
  "tide-in":      "rgba(151,193,233,1.0)",  // pastel blue — arrival
  "drift 1":      "rgba(240,183,200,1.0)",  // pastel pink — intake
  "swell 1":      "rgba(240,150,148,1.0)",  // pastel coral-red — first peak
  "deep-current": "rgba(240,236,226,1.0)",  // pastel cream-white — eye chart
  "drift 2":      "rgba(164,191,228,1.0)",  // pastel blue — handle typed
  "undertow":     "rgba(150,160,214,1.0)",  // soft periwinkle — the listen
  "swell 2":      "rgba(244,168,176,1.0)",  // warm pastel pink-red — covered
  "tide-out":     "rgba(244,240,233,1.0)",  // pastel white — wrap-up
  "ebb":          "rgba(234,201,213,1.0)",  // soft rose — the room quiets
};

// ── pre-render YWFT title chars + lane labels + section labels ───────
console.log("  rasterizing YWFT …");
const assetsDir = AUDIO.replace(/\.mp3$/, ".assets");
mkdirSync(assetsDir, { recursive: true });

const titleFontSize = 96;
const { chars: titleChars, totalWidth: titleTotalW } = await prerenderTitleChars({
  text: TITLE,
  ptSize: titleFontSize,
  palette: TITLE_PALETTE,
  // YT cut: bake NO shadow so source-in tinting yields a clean alpha
  // mask for the per-frame colour seep in drawPalsTitleChars.
  shadowColor: null,
  assetsDir,
});

// ── Title/pals geometry (matches trancepenta-yt — pals snug against
// the first char of each vertical text column, both centred on the
// same vertical line so logo + type read as one family) ─────────────
const PALS_S       = 145;                       // pals stamp size (square)
const PALS_HALF    = PALS_S / 2;
const PALS_EDGE_X  = 100;                       // cx of pals (left side)
const CHARS_EDGE_X = PALS_EDGE_X + 12;          // chars sit just inward of pals
const CHAR_SCALE   = 0.52;
const CHAR_SPAN    = titleTotalW * CHAR_SCALE;  // vertical length of rotated text column
const BOUNCE_BUF   = 26;                        // vertical gap pals → first char
const LEFT_CHARS_CY  = H * 0.82 - 16;           // bottom-left group nudged UP toward centre
const RIGHT_CHARS_CY = H * 0.18 + 32 + 16;      // top-right group nudged DOWN toward centre
const LEFT_PALS_CY  = LEFT_CHARS_CY  - CHAR_SPAN / 2 - BOUNCE_BUF - PALS_HALF;
const RIGHT_PALS_CY = RIGHT_CHARS_CY + CHAR_SPAN / 2 + BOUNCE_BUF + PALS_HALF;
const TITLE_TOP_Y = 44;       // (now only used by IG_TOP_SAFE / typing-intro fallback)
const titleBoxH   = Math.ceil(titleFontSize * 1.7);

const tcFontSize = 64;
// We'll re-render the timecode each second since it changes — too
// slow per-frame. Use a cache of per-second strings.
const tcCache = new Map();
async function getTcImg(text) {
  if (tcCache.has(text)) return tcCache.get(text);
  const safe = text.replace(/[^0-9]/g, "_");
  // node-canvas does NOT honor ctx.filter = "brightness(0)", so we
  // can't tint a copy black at draw time. Pre-render a real solid-
  // black glyph plate via ImageMagick alongside the colored one and
  // composite it offset behind for a true dark drop shadow.
  const img = await magickRenderText(text, {
    ptSize: tcFontSize,
    fill: "rgba(255,253,242,0.97)",
    outPath: `${assetsDir}/tc.${safe}.png`,
  });
  const shadow = await magickRenderText(text, {
    ptSize: tcFontSize,
    fill: "rgba(0,0,0,1)",
    outPath: `${assetsDir}/tc.${safe}.shadow.png`,
  });
  const entry = { img, shadow };
  tcCache.set(text, entry);
  return entry;
}
// Pre-bake every whole-second tc label for the full duration.
{
  const totMm = Math.floor(DURATION / 60);
  const totSs = Math.floor(DURATION - totMm * 60).toString().padStart(2, "0");
  for (let s = 0; s <= Math.ceil(DURATION); s++) {
    const mm = Math.floor(s / 60);
    const ss = (s - mm * 60).toString().padStart(2, "0");
    await getTcImg(`${mm}:${ss} / ${totMm}:${totSs}`);
  }
}

// ── lane events ──────────────────────────────────────────────────────
const laneEvents = {};
for (const lane of LANES) {
  laneEvents[lane.key] = (struct.events?.[lane.key] || []).slice().sort((a, b) => a.t - b.t);
}

// ── kick envelope (drives the illustration "sharpen" punch) ──────────
// Only the deep kick drum (kind:"kick"), not the snare. Fast ~20ms
// attack → ~130ms exponential decay. Sampled per-frame in drawScene.
const kickTimes = (struct.events?.kick || [])
  .filter((e) => e.kind === "kick").map((e) => e.t).sort((a, b) => a - b);
function kickEnvAt(t) {
  let e = 0;
  for (const kt of kickTimes) {
    if (kt > t + 0.03) break;
    const dt = t - kt;
    const v = dt < 0 ? Math.max(0, 1 + dt / 0.02) : Math.exp(-dt / 0.13);
    if (v > e) e = v;
  }
  return e;
}

// ── canvas layout ────────────────────────────────────────────────────
// Tracks float across the full canvas height over the illustration —
// no backing panel, no boxed area, no lane labels. The playhead is a
// single vertical needle that spans the whole canvas top-to-bottom.
const PANEL_PAD_X  = 24;
const LANE_GUTTER  = 3;                     // dense record — minimal space between tracks
const LANE_TOP     = 220;                   // below title — landscape fork: bumped from 200 to clear the 96pt title at the new TITLE_TOP_Y=44
const LANE_BOTTOM  = H - 80;                // leaves room for progress bar + timecode — landscape fork: tightened from 108 since 1080-tall has less air
const LANE_AREA_H  = LANE_BOTTOM - LANE_TOP;
const LANE_H       = Math.floor((LANE_AREA_H - (LANES.length - 1) * LANE_GUTTER) / LANES.length);
const LANE_X0      = PANEL_PAD_X;
const PX_PER_SEC   = 240;
const TRACK_W      = W - LANE_X0 - PANEL_PAD_X;
const PLAYHEAD_X   = LANE_X0 + Math.round(TRACK_W / 2);
// AAC encoder priming delay — visual frame at time t plays back audio
// from t - AUDIO_DELAY_SEC. Shift the visual readout so blocks cross
// the playhead at the instant the listener actually hears them.
const AUDIO_DELAY_SEC = Number(flags["audio-delay"] ?? 0.045);

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const cover = await loadImage(COVER);

// ── per-section illustrations + Star Wars slide/fade transition ──────
// Each section can have its own illy at out/<slug>-sec-<i>-<safe>.png
// (gen-illy.mjs --sections). Missing ones fall back to the main cover.
function safeName(n) {
  return n.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
}
const SECTIONS = struct.sections.slice().sort((a, b) => a.startSec - b.startSec);
const sectionImgs = [];
let haveSectionImgs = 0;
for (let i = 0; i < SECTIONS.length; i++) {
  const p = `${LANE}/out/${SLUG}${TAG}-sec-${i}-${safeName(SECTIONS[i].name)}.png`;
  if (existsSync(p)) { sectionImgs[i] = await loadImage(p); haveSectionImgs++; }
  else sectionImgs[i] = cover;
}
console.log(`  section illys: ${haveSectionImgs}/${SECTIONS.length} (rest → cover)`);

// ── pals watermark (real AC purple-pals.svg, top-right) ──────────────
// Rasterize the canonical SVG once via rsvg-convert and composite it
// small + semi-transparent in the top-right corner of every frame.
const PALS_WM_SIZE = 212;
let palsImg = null;
let palsBlur = null;
const wmCanvas = createCanvas(8, 8);
const wmCtx = wmCanvas.getContext("2d");
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
// Recolor a pals alpha-mask (`src`) to `rgb` into the offscreen.
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
{
  const palsSvg = `${REPO}/system/public/purple-pals.svg`;
  const palsPng = `${assetsDir}/pals-watermark.png`;
  const palsBlurPng = `${assetsDir}/pals-watermark-blur.png`;
  if (existsSync(palsSvg)) {
    const r = spawnSync("rsvg-convert",
      ["-w", String(PALS_WM_SIZE * 2), "-h", String(PALS_WM_SIZE * 2),
       "-o", palsPng, palsSvg]);
    if (r.status === 0 && existsSync(palsPng)) {
      palsImg = await loadImage(palsPng);
      // only a hair of edge-softening (kills the hard vector edge so it
      // sits in the hand-drawn world) — NOT a gaussian ghost.
      const rb = spawnSync("magick",
        [palsPng, "-channel", "A", "-blur", "0x1.5", "+channel", palsBlurPng]);
      palsBlur = (rb.status === 0 && existsSync(palsBlurPng))
        ? await loadImage(palsBlurPng) : palsImg;
    }
  }
  console.log(`  pals watermark: ${palsImg ? "loaded" : "MISSING"}`);
}
// YT fork: trancepenta-yt-style side stamps — pals snug against each
// vertical text column, with rotated-90° title chars climbing UP beside
// them like a movie-poster spine.
function palsFrameColor(audioT) {
  const u = (audioT + AUDIO_DELAY_SEC) * FPS / FRAMES;
  const hue = ((u * 360 * 4) % 360 + 360) % 360;
  const hRgb = hslToRgb(hue, 0.9, 0.62);
  const [sr, sg, sb] = sectionTcRgb(audioT);
  return [
    Math.round(sr * 0.6 + hRgb[0] * 0.4),
    Math.round(sg * 0.6 + hRgb[1] * 0.4),
    Math.round(sb * 0.6 + hRgb[2] * 0.4),
  ];
}
// Re-tint a pre-rendered title-char glyph (alpha mask) to a target rgb
// via source-in. Its own canvas (separate from wmCanvas) so tinting +
// drawing don't trample each other in drawPalsTitleChars.
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
function drawWatermark(audioT) {
  if (!palsImg) return;
  const s = PALS_S;
  // PERFECT-LOOP GEOMETRY — see trancepenta-yt comment block. Position,
  // wiggle, swivel, hue all derive from the whole-video phase so the
  // last frame's pals sit exactly where frame 0's do.
  const u = (audioT + AUDIO_DELAY_SEC) * FPS / FRAMES;
  const TAU = Math.PI * 2;
  const hue = ((u * 360 * 4) % 360 + 360) % 360;
  const hRgb = hslToRgb(hue, 0.9, 0.62);
  const col = palsFrameColor(audioT);
  const env  = Math.min(1, envAt(audioT));
  const glow = env * env;
  const hotRgb = hslToRgb(hue, 1.0, Math.min(0.88, 0.60 + 0.34 * glow));
  const ledCol = [
    Math.round(col[0] + (hotRgb[0] - col[0]) * glow),
    Math.round(col[1] + (hotRgb[1] - col[1]) * glow),
    Math.round(col[2] + (hotRgb[2] - col[2]) * glow),
  ];
  const wig  = 13 * Math.sin(TAU * 30 * u) + 4 * Math.sin(TAU * 10 * u);
  const swiv = 0.05 * Math.sin(TAU * 19 * u) + 0.025 * Math.sin(TAU * 38 * u);
  // pals snug against each text column (cy from title geometry, cx
  // pinned at PALS_EDGE_X). The chars climbing beside them in
  // drawPalsTitleChars share the same wiggle so logo + type move as one.
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
  // Title chars climbing UP beside each pals stamp (movie-poster spine).
  drawPalsTitleChars(audioT);
}
// "helpabeach" chars climbing VERTICALLY (rotated −90°) BESIDE each
// pals stamp — left pals at bottom: text column ABOVE it; right pals at
// top: text column BELOW it. Each char re-tinted per frame to the pals
// section+hue colour so logo + type read as one family.
function drawPalsTitleChars(audioT) {
  if (!palsImg) return;
  const u = (audioT + AUDIO_DELAY_SEC) * FPS / FRAMES;
  const TAU = Math.PI * 2;
  const wig  = 13 * Math.sin(TAU * 30 * u) + 4 * Math.sin(TAU * 10 * u);
  const span = titleTotalW * CHAR_SCALE;
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
      const x = startX + ch.prefixWidth * CHAR_SCALE;
      const dw = ch.img.width * CHAR_SCALE;
      const dh = ch.img.height * CHAR_SCALE;
      const charEnv = envAt(audioT - i * 0.03);
      const lift = 4 * Math.sin(audioT * 4.0 + i * 0.8) * (0.3 + charEnv);
      const y = -dh / 2 + lift;
      // shadow stamp matching the pals shadow (solid black, α=0.26, offset 3/4)
      ctx.save();
      ctx.globalCompositeOperation = "source-over";
      ctx.globalAlpha = 0.26;
      ctx.drawImage(tintCharGlyph(ch.img, [0, 0, 0]), x + 3, y + 4, dw, dh);
      ctx.restore();
      // 4-pass seep — same blend stack as the pals so type soaks in identically.
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
      // crisp top pass at higher α so smaller type still matches pals density
      ctx.save();
      ctx.globalCompositeOperation = "source-over";
      ctx.globalAlpha = 0.46;
      ctx.drawImage(tintCharGlyph(ch.img, palsRgb), x, y, dw, dh);
      ctx.restore();
      // LED screen pulse on loud chars (mirrors the pals LED bloom)
      if (charEnv > 0.45) {
        ctx.save();
        ctx.globalCompositeOperation = "screen";
        ctx.globalAlpha = 0.14 + 0.46 * Math.min(1, (charEnv - 0.45) / 0.55);
        ctx.drawImage(tintCharGlyph(ch.img, palsRgb), x, y, dw, dh);
        ctx.restore();
      }
    }
    ctx.restore();
  }
}

const TRANS_S = 1.5;                        // slide+fade transition length
// mirrorTile — show the WHOLE illy un-cropped (fit to width), mirror-
// tiled vertically to fill the frame; a gentle vertical drift reveals
// the illy's top/bottom edges instead of zoom-cropping them away.
const KB = { mirrorTile: true, breathAmp: 0.06, breathPeriodSec: 18,
             envWobbleAmp: 18 };

// ── figure bounding boxes → zoom targets + backlight ─────────────────
// helpabeach-forms.json carries hand-placed figure bboxes per section
// (panel-image coords). ZOOM sections punch in on jeffrey's box; every
// section casts an audio-driven backlight glow on the form regions.
let FORMS = {};
try {
  // YT fork: prefer landscape-tuned forms (-yt-forms.json) if present;
  // fall back to portrait forms only if no YT-specific file exists.
  // The portrait bboxes are tuned for 1024×1536 panels — they will be
  // wrong for the 1536×1024 landscape illys, so face-detect them fresh
  // (pop/dance/bin/detect-face.py per the trancenwaltz recipe) and
  // hand-place into helpabeach-yt-forms.json once the illys land.
  const ytFp = `${LANE}/${SLUG.replace(/-short$/, "")}-yt-forms.json`;
  const fp   = `${LANE}/${SLUG.replace(/-short$/, "")}-forms.json`;
  const pick = existsSync(ytFp) ? ytFp : (existsSync(fp) ? fp : null);
  if (pick) FORMS = JSON.parse(readFileSync(pick, "utf8")).sections || {};
} catch { /* no forms → plain mirror-tile, no zoom/backlight */ }
const ZOOM_SECTIONS = new Set(["swell 1", "deep-current", "swell 2", "tide-out"]);
const BACKLIGHT_RGB = "255,236,196";   // warm backlit-illumination glow
function sectionForms(name) {
  const f = FORMS[name];
  if (!f) return [];
  return [f.jeffrey, f.clinician].filter(Boolean)
    .map(([x, y, w, h]) => ({ x, y, w, h }));
}
function sectionIndexAt(t) {
  let idx = 0;
  for (let i = 0; i < SECTIONS.length; i++) if (t >= SECTIONS[i].startSec) idx = i;
  return idx;
}
// Section transitions are a full-frame CROSS-DISSOLVE (see drawScene)
// so the illustration never slides off and never exposes an edge.
// Draw one section panel — mirror-tile WIDE, or (on ZOOM sections)
// punched WAY into jeffrey's figure bbox — and return its panel→frame
// transform + form bboxes so the backlight can be cast on the figures.
function drawSectionPanel(img, name, audioT, env, punch, extra) {
  const fs = sectionForms(name);
  const zoomBox = ZOOM_SECTIONS.has(name) && fs[0] ? fs[0] : null;
  const xform = drawCoverKenBurns(ctx, img, audioT, {
    ...KB, env, punch, ...extra, zoomBox,
  });
  return { xform, fs };
}
function drawScene(audioT, env, punch) {
  const idx = sectionIndexAt(audioT);
  const sec = SECTIONS[idx];
  const sinceStart = audioT - sec.startSec;
  // backlight intensity — a calm base that swells with the audio
  const blI = 0.22 + env * 0.5 + punch * 0.42;
  if (idx > 0 && sinceStart >= 0 && sinceStart < TRANS_S) {
    let p = sinceStart / TRANS_S;
    p = p * p * (3 - 2 * p);                 // smoothstep ease
    // CROSS-DISSOLVE between the two panels.
    drawSectionPanel(sectionImgs[idx - 1], SECTIONS[idx - 1].name, audioT, env, punch,
      { alpha: 1, fillBackground: true });
    const cur = drawSectionPanel(sectionImgs[idx], sec.name, audioT, env, punch,
      { alpha: p, fillBackground: false });
    drawFormBacklight(ctx, cur.fs, cur.xform, blI * p, BACKLIGHT_RGB);
  } else {
    const cur = drawSectionPanel(sectionImgs[idx], sec.name, audioT, env, punch, {});
    drawFormBacklight(ctx, cur.fs, cur.xform, blI, BACKLIGHT_RGB);
  }
}


function drawTitle(audioT, env) {
  // INSTA cut: rest at the SAME anchor the typing-intro uses (glyph-top
  // = TITLE_TOP_Y, baseX = 30) so the title does NOT jump when the
  // intro hands off to the track — it stays snug at the top and bounces
  // from there.
  const titleY = TITLE_TOP_Y + Math.ceil(titleFontSize * 1.25);
  drawTitleBounce(ctx, {
    chars: titleChars,
    ptSize: titleFontSize,
    baseX: 30,
    baseY: titleY,
    audioT,
    env,
    getEnvAt: envAt,                  // letters sample envelope as a wave
    charDelay: 0.03,
    bounceAmp: 90,
    restAlpha: 1.0,                   // chars ALWAYS fully opaque — only
                                      // color + bounce + glow change,
                                      // never transparency
    glowThreshold: 0.45,
    glowMax: 22,
    wobbleAmp: 3,
  });
}

// TIKTOK cut: the title is typed into the CENTRE of screen, then when
// the track loads it JUMPS in and the individual characters BOUNCE
// (per-char traveling wave) and TINT to the current section colour
// (blended with each char's palette colour) — exactly like the
// trancenwaltz title — then after a few seconds the letters FALL down
// (staggered gravity + tumble + fade) and are GONE for good (no
// reappear). Glyph metrics match the centred typing-intro so the
// hand-off doesn't move.
const _ttCanvas = createCanvas(2, 2);
const _ttCtx = _ttCanvas.getContext("2d");
function tintTitleGlyph(img, r, g, b) {
  _ttCanvas.width = img.width; _ttCanvas.height = img.height;
  _ttCtx.globalCompositeOperation = "source-over";
  _ttCtx.clearRect(0, 0, img.width, img.height);
  _ttCtx.drawImage(img, 0, 0);
  _ttCtx.globalCompositeOperation = "source-in";
  _ttCtx.fillStyle = `rgb(${r},${g},${b})`;
  _ttCtx.fillRect(0, 0, img.width, img.height);
  _ttCtx.globalCompositeOperation = "source-over";
  return _ttCanvas;
}
function drawCenterTitle(audioT) {
  const pr = struct.prerollSec || 0;
  const beatHz = (struct.bpm || 70) / 60;
  const startX = Math.round((W - titleTotalW) / 2);
  const gyc = Math.round((H - titleBoxH) / 2);   // centred glyph-box top
  const env = envAt(audioT);
  const [tSr, tSg, tSb] = sectionTcRgb(audioT);   // current section colour
  const JUMP_T = pr;                              // title "loads in" w/ track
  const sinceJump = audioT - JUMP_T;
  const jumping = sinceJump >= 0 && sinceJump < 0.8;
  const FALL_START = pr + 4.0;                    // bounce a bit, then drop
  const baseBounce = 8, envBounce = env * 34;
  for (let i = 0; i < titleChars.length; i++) {
    const ch = titleChars[i];
    if (!ch || !ch.img) continue;
    const img = ch.img;
    const x = startX + ch.prefixWidth - 3;
    const phase = (i / titleChars.length) * 2 * Math.PI;
    const beatWave = Math.sin(2 * Math.PI * beatHz * audioT + phase);
    let y = gyc - (baseBounce + envBounce) * Math.abs(beatWave);
    if (jumping) {
      const e = 1 - sinceJump / 0.8;              // 1 → 0 over 0.8 s
      y += -e * e * 44 * Math.cos(sinceJump * 26 - i * 0.6);
    }
    // staggered gravity fall + tumble + fade, then GONE (never returns)
    const charFallT = FALL_START + i * 0.10;
    let fdy = 0, frot = 0, falpha = 1;
    if (audioT > charFallT) {
      const fe = audioT - charFallT;
      fdy = 0.5 * 2650 * fe * fe;                 // gravity (px)
      const spin = (((i * 53) % 11) - 5) * 0.55;  // deterministic ± per char
      frot = spin * fe + 0.16 * Math.sin(fe * 9 + i);
      falpha = Math.max(0, 1 - fe / 2.2);
    }
    if (falpha <= 0.001) continue;                // gone — skip
    const pal = ncHexToRgb(TITLE_PALETTE[i % TITLE_PALETTE.length]);
    const cr = Math.round(tSr * 0.55 + pal[0] * 0.45);
    const cg = Math.round(tSg * 0.55 + pal[1] * 0.45);
    const cb = Math.round(tSb * 0.55 + pal[2] * 0.45);
    const blk = tintTitleGlyph(img, 0, 0, 0);
    const drawStack = (px, py) => {
      ctx.globalCompositeOperation = "source-over";
      for (const [ox, oy, a] of [[-2,-1,0.18],[2,3,0.20],[5,7,0.20],[0,3,0.22]]) {
        ctx.globalAlpha = a * falpha; ctx.drawImage(blk, px + ox, py + oy);
      }
      for (const [ox, oy, a] of [[3,4,0.95],[4,5,0.85],[3,5,0.70]]) {
        ctx.globalAlpha = a * falpha; ctx.drawImage(blk, px + ox, py + oy);
      }
      ctx.globalAlpha = falpha;
      ctx.drawImage(tintTitleGlyph(img, cr, cg, cb), px, py);
    };
    ctx.save();
    if (fdy > 0 || frot !== 0) {
      const cx = x + img.width / 2, cy = y + img.height / 2;
      ctx.translate(cx, cy + fdy);
      ctx.rotate(frot);
      drawStack(-img.width / 2, -img.height / 2);
    } else {
      drawStack(x, y);
    }
    ctx.restore();
  }
  ctx.globalAlpha = 1;
  ctx.globalCompositeOperation = "source-over";
}

const PROGRESS_BAR_H = 22;
const PROGRESS_BAR_Y = H - PROGRESS_BAR_H;

// Offscreen used to recolor the (white) pre-rendered timecode glyphs
// to the current section's tint via source-in.
const tcTint = createCanvas(8, 8);
const tcTintCtx = tcTint.getContext("2d");
// Current section color, brightened toward white by how far through
// the section we are — so the timecode reads both WHICH section and
// HOW FAR (progress) we're at.
function sectionTcRgb(audioT) {
  const i = sectionIndexAt(audioT);
  const s = SECTIONS[i];
  let [r, g, b] = tintRgb(SECTION_TINTS[s.name] || "rgba(255,253,242,1)");
  const span = Math.max(0.001, s.endSec - s.startSec);
  const lp = Math.max(0, Math.min(1, (audioT - s.startSec) / span));
  const k = 0.30 * lp;                       // ramp toward white across section
  r = Math.round(r + (255 - r) * k);
  g = Math.round(g + (255 - g) * k);
  b = Math.round(b + (255 - b) * k);
  return [r, g, b];
}
function drawTimecode(audioT) {
  const totMm = Math.floor(DURATION / 60);
  const totSs = Math.floor(DURATION - totMm * 60).toString().padStart(2, "0");
  const sec = Math.min(Math.max(0, Math.floor(audioT)), Math.ceil(DURATION));
  const mm = Math.floor(sec / 60);
  const ss = (sec - mm * 60).toString().padStart(2, "0");
  const entry = tcCache.get(`${mm}:${ss} / ${totMm}:${totSs}`);
  if (!entry) return;
  const { img, shadow } = entry;
  // bottom-right, sits ABOVE the segmented progress bar. The timecode
  // bounces with the audio envelope (like the title) plus a small
  // breathing wobble so it stays alive between hits.
  const env = envAt(audioT);
  const bounce = 16 * env;
  const wobble = 2.2 * Math.sin(audioT * 5.5);
  const x = W - img.width - 32;
  const y = PROGRESS_BAR_Y - img.height - 14 - bounce + wobble;
  // tight hard-offset DARK drop shadow — a real pre-rendered solid
  // black glyph plate (node-canvas ignores ctx.filter), stamped twice
  // for density so it reads over the bright dayglo illustration.
  ctx.save();
  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 0.95;
  ctx.drawImage(shadow, x + 3, y + 4);
  ctx.drawImage(shadow, x + 2, y + 3);
  ctx.globalAlpha = 1;
  // colored top pass — recolor the white glyph plate to the current
  // section tint (source-in keeps the glyph alpha) so the timecode
  // itself reports section + progress, not just white.
  const [tr, tg, tb] = sectionTcRgb(audioT);
  tcTint.width = img.width;
  tcTint.height = img.height;
  tcTintCtx.clearRect(0, 0, img.width, img.height);
  tcTintCtx.globalCompositeOperation = "source-over";
  tcTintCtx.drawImage(img, 0, 0);
  tcTintCtx.globalCompositeOperation = "source-in";
  tcTintCtx.fillStyle = `rgb(${tr},${tg},${tb})`;
  tcTintCtx.fillRect(0, 0, img.width, img.height);
  ctx.drawImage(tcTint, x, y);
  ctx.restore();
}

// Segmented bottom progress bar — each section paints its own tint as a
// flat band; the played portion is brighter, unplayed is faint. The
// leading edge of the played fill IS the progress indicator.
function tintRgb(s) {
  const m = s.match(/rgba?\((\d+),\s*(\d+),\s*(\d+)/);
  return m ? [+m[1], +m[2], +m[3]] : [200, 200, 200];
}
function drawProgressBar(audioT) {
  ctx.save();
  // Each section gets a near-black, section-tinted TRACK across its
  // full width (so the whole bar always reads as a progress bar), with
  // the PLAYED portion painted in the bright saturated tint on top.
  const playedX = Math.max(0, audioT) / DURATION * W;
  const lastI = struct.sections.length - 1;
  for (let si = 0; si < struct.sections.length; si++) {
    const sec = struct.sections[si];
    // first section starts at PREROLL_SEC (the typing-intro), so without
    // this clamp the bar would begin ~3% in and never touch the LEFT
    // edge — force section 0 to x=0 (mirror of the last-section→W rule).
    const segX0 = si === 0 ? 0 : (sec.startSec / DURATION) * W;
    // the final section runs all the way to the right edge — the
    // sections end a couple seconds before the padded total, so without
    // this the bar would stop ~3% short of the screen.
    const segX1 = si === lastI ? W : (sec.endSec / DURATION) * W;
    const [r, g, b] = tintRgb(SECTION_TINTS[sec.name] || "rgba(200,200,200,1)");
    // near-black but tinted toward this section's color — the track
    ctx.fillStyle = `rgba(${Math.round(r * 0.16)},${Math.round(g * 0.16)},${Math.round(b * 0.16)},0.85)`;
    ctx.fillRect(segX0, PROGRESS_BAR_Y, segX1 - segX0, PROGRESS_BAR_H);
    // played: bright saturated tint
    const filledX1 = Math.min(segX1, playedX);
    if (filledX1 > segX0) {
      ctx.fillStyle = `rgba(${r},${g},${b},0.96)`;
      ctx.fillRect(segX0, PROGRESS_BAR_Y, filledX1 - segX0, PROGRESS_BAR_H);
    }
    // section-boundary tick
    ctx.fillStyle = "rgba(255,253,242,0.35)";
    ctx.fillRect(segX1 - 1, PROGRESS_BAR_Y, 1, PROGRESS_BAR_H);
  }
  ctx.restore();
}

function hexToRgb(hex) {
  return [parseInt(hex.slice(1, 3), 16), parseInt(hex.slice(3, 5), 16), parseInt(hex.slice(5, 7), 16)];
}

// Convert MIDI number → notepat color schematic { rgb: [r,g,b] }.
// notepat uses octave-dependent palettes — base=4, dayglo>=5, muted<=3.
const NOTE_NAMES = ["c","c#","d","d#","e","f","f#","g","g#","a","a#","b"];
function midiToNotepatRgb(midi) {
  const noteIdx = ((midi % 12) + 12) % 12;
  const octave = Math.floor(midi / 12) - 1;
  return getNoteColorForOctave(NOTE_NAMES[noteIdx], octave);
}
// ── rotating record ──────────────────────────────────────────────────
// The whole track group (lane waveforms + the playhead string) is a
// disc that rotates one plain 360° about the canvas centre across the
// full record. The string is extended well past the canvas diagonal so
// it ALWAYS cuts clear across the display at any rotation (its round
// caps never enter frame). Scene + title + bar + timecode + watermark
// stay fixed.
const ROT_CX = Math.round(W / 2);  // matches the engine's disc centre
const ROT_CY = Math.round(H / 2);
const DIAG   = Math.hypot(W, H);   // drawPanel viewport span
// Waveforms ride a virtual record groove curved about the canvas
// CENTRE (ROT_CX/ROT_CY = screen centre). A big radius reads almost
// flat (centre off one edge); a small radius reads as a tight, very
// curved spindle. ~0.80·screen-min is a SLOWER, gentler curve — the
// record still centres but the polarity isn't so pronounced.
const GROOVE_R = Number(flags["groove-r"] ?? Math.round(Math.min(W, H) * 0.85));
// The shared cover engine: verlet string + rotating disc + the LOCAL
// string→illustration WARP. Same movement system as trancenwaltz.
// The illustration is NOT rotated or skewed — only the lane+string
// disc rotates, and warp() bends the picture where the string is
// deflected (the distortion comes straight from the line).
const _vs = makeVerletString(ctx, { W, H, playheadX: PLAYHEAD_X, duration: DURATION });
const trackTheta       = _vs.trackTheta;
const withRotation     = _vs.withRotation;
const pluckNeedle      = _vs.pluck;
const stepNeedle       = _vs.step;
const needleXAt        = _vs.needleXAt;
const drawNeedleString = _vs.draw;
const ncHexToRgb       = hexToRgb;

// ── string→illustration warp, COUNTER-ROTATED ───────────────────────
// The engine's warp() samples an axis-aligned strip but, run inside
// the disc rotation, redraws it rotated → the whole picture spins.
// Instead: snapshot the upright frame, rotate it −θ into a buffer,
// then redraw the strip under +θ. The two rotations cancel for the
// CONTENT (illustration stays perfectly upright, never rotates) while
// the strip + its per-row string-bend live in the string's rotated
// frame — so ONLY the distortion field under the string turns with
// the record. Feathered bands → no seam with the un-warped surround.
const WU_HALF = 200, WU_W = WU_HALF * 2, WU_STEP = 6, WU_BANDS = 7;
const WU_STR = 0.85, WU_BW = WU_W / WU_BANDS;
const wuWin = new Float64Array(WU_BANDS);
for (let b = 0; b < WU_BANDS; b++) {
  wuWin[b] = Math.sin(Math.PI * ((b + 0.5) / WU_BANDS)) ** 2;
}
const WU_DSZ = Math.ceil(DIAG) + 4;
const wuSnap = createCanvas(W, H);
const wuSnapC = wuSnap.getContext("2d");
const wuCR = createCanvas(WU_DSZ, WU_DSZ);
const wuCRC = wuCR.getContext("2d");
function warpUnderString(theta) {
  const { devPeak } = _vs.deflection();
  if (Math.abs(devPeak) < 2) return;          // string ~straight → skip
  // 1) snapshot the upright frame (scene + backlight)
  wuSnapC.clearRect(0, 0, W, H);
  wuSnapC.drawImage(canvas, 0, 0);
  // 2) counter-rotate it by −θ, centred in the big square
  wuCRC.setTransform(1, 0, 0, 1, 0, 0);
  wuCRC.clearRect(0, 0, WU_DSZ, WU_DSZ);
  wuCRC.translate(WU_DSZ / 2, WU_DSZ / 2);
  wuCRC.rotate(-theta);
  wuCRC.translate(-W / 2, -H / 2);
  wuCRC.drawImage(wuSnap, 0, 0);
  wuCRC.setTransform(1, 0, 0, 1, 0, 0);
  // 3) redraw only the strip under +θ, each band shifted by the
  //    string's local deflection (content upright, strip rotated)
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

// ── boot / shutdown beep flashes ─────────────────────────────────────
// NOT radial — a TOTALIZING whole-frame flash per ac-native chime note
// (struct.events.sfx boot/shutdown-beep-N): the entire image blooms
// the chime colour and decays; shutdown also pulls the whole frame
// down then releases (a system collapse).
const beepEvents = (struct.events?.sfx || [])
  .filter((e) => /^(boot|shutdown)-beep-\d+$/.test(e.name))
  .map((e) => ({ t: e.t, boot: e.name.startsWith("boot") }))
  .sort((a, b) => a.t - b.t);

// ── typing-intro: first illy under AC-prompt purple, slug typed ──────
// The whole intro lives in the pre-roll [0, PREROLL_SEC): first the
// boot beeps fire, THEN the slug types in char-by-char (each keyclick
// reveals one glyph, a blinking pink caret trails), then the track
// frame takes over at PREROLL_SEC.
const keyClicks = (struct.events?.sfx || [])
  .filter((e) => /^keyclick-\d+$/.test(e.name))
  .map((e) => e.t).sort((a, b) => a - b);
const PREROLL_SEC = struct.prerollSec || 0;
// EXACT port of the trancenwaltz typing intro (pop/dance/bin/
// cover-video.mjs) — same constants + animation so the two match:
// purple AC bg, glyphs typed from titleBaseX at glyph-box-top gy,
// and the pink block IS the cursor (rides right of the typed text,
// ~3 Hz blink). No leading block, no lozenge.
const titleBaseX     = 30;
const charBoxW       = Math.ceil(titleFontSize * 1.4);
const charBoxH       = Math.ceil(titleFontSize * 1.7);
const charBaselineY  = Math.ceil(titleFontSize * 1.25);
const IG_TOP_SAFE    = TITLE_TOP_Y;
const titleY         = IG_TOP_SAFE + charBaselineY;
function drawTypingIntro(audioT) {
  // The FIRST illy (tide-in) sits UNDER a translucent purple wash so it
  // ghosts through the AC prompt and gets extra screen-time before the
  // track proper begins.
  const bg = sectionImgs[0];
  const sc = Math.max(W / bg.width, H / bg.height);
  const dw = bg.width * sc, dh = bg.height * sc;
  ctx.drawImage(bg, (W - dw) / 2, (H - dh) / 2, dw, dh);
  ctx.fillStyle = "rgba(27,0,51,0.80)";      // #1b0033 prompt purple wash
  ctx.fillRect(0, 0, W, H);
  const PINK = "#e0468c";                    // AC prompt pink
  let typed = 0;
  for (const kt of keyClicks) { if (audioT >= kt) typed++; else break; }
  typed = Math.min(typed, titleChars.length);
  const blockW = Math.round(charBoxW * 0.30);
  const blockH = Math.round(charBaselineY * 0.96);
  // INSTA: type from the left at the top safe anchor. TIKTOK: type into
  // the CENTRE of screen (same placement drawCenterTitle holds, so the
  // hand-off to the track doesn't move).
  const gy = TIKTOK
    ? Math.round((H - charBoxH) / 2)
    : titleY - charBaselineY;                 // glyph-box top (no bounce)
  const startX = TIKTOK
    ? Math.round((W - titleTotalW) / 2)
    : titleBaseX;
  const promptY = gy + Math.round((charBoxH - blockH) / 2);
  for (let i = 0; i < typed; i++) {
    const ch = titleChars[i];
    if (ch && ch.img) ctx.drawImage(ch.img, startX + ch.prefixWidth - 3, gy);
  }
  // pink block = the cursor, RIGHT of the typed text; ~3 Hz blink.
  const caretPrefix = typed < titleChars.length
    ? titleChars[typed].prefixWidth : titleTotalW;
  const caretX = startX + caretPrefix + 4;
  if (Math.floor(audioT * 6) % 2 === 0) {
    ctx.fillStyle = PINK;
    ctx.fillRect(Math.round(caretX), promptY, blockW, blockH);
  }
}
function drawBeepRipples(audioT) {
  const RIPPLE_S = 1.0;
  for (const b of beepEvents) {
    const dt = audioT - b.t;
    if (dt < 0 || dt > RIPPLE_S) continue;
    const p = dt / RIPPLE_S;                  // 0..1
    const fall = (1 - p) * (1 - p);           // fast decay
    const spike = p < 0.10 ? (1 - p / 0.10) : 0;
    const tint = b.boot ? "190,255,180" : "255,165,140";
    ctx.save();
    // whole-frame bloom — the ENTIRE image flashes the chime colour
    ctx.globalCompositeOperation = "screen";
    ctx.globalAlpha = Math.min(0.88, 0.40 * fall + 0.58 * spike);
    ctx.fillStyle = `rgb(${tint})`;
    ctx.fillRect(0, 0, W, H);
    if (!b.boot) {
      // shutdown drags the whole frame down, then releases
      ctx.globalCompositeOperation = "multiply";
      ctx.globalAlpha = 0.38 * fall;
      ctx.fillStyle = "rgb(64,50,72)";
      ctx.fillRect(0, 0, W, H);
    }
    ctx.restore();
  }
  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
}

// ── beach-sunset backlight + contrast layering ───────────────────────
// A warm low radial glow rising from behind jeffrey (lower-centre),
// pulsing with the audio envelope + the bell/sub mix, plus a soft
// vignette that deepens the contrast around it. Screen for the glow,
// multiply for the vignette — the trancenwaltz backlight idea retuned
// to a beach-sunset palette.
function drawBacklight(audioT, env) {
  const idx = sectionIndexAt(audioT);
  const warmByIdx = [                         // dawn→dusk warmth ramp
    "255,210,120", "255,196,110", "255,168,86",
    "255,138,96", "255,120,140",
  ];
  const tint = warmByIdx[Math.min(idx, warmByIdx.length - 1)];
  const gx = W / 2, gy = H * 0.74;            // behind seated jeffrey
  const pulse = 0.30 + 0.55 * env;
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  const rg = ctx.createRadialGradient(gx, gy, 0, gx, gy, H * 0.62);
  rg.addColorStop(0,   `rgba(${tint},${(0.40 * pulse).toFixed(3)})`);
  rg.addColorStop(0.5, `rgba(${tint},${(0.16 * pulse).toFixed(3)})`);
  rg.addColorStop(1,   `rgba(${tint},0)`);
  ctx.fillStyle = rg;
  ctx.fillRect(0, 0, W, H);
  // multiply vignette removed 2026-05-29 — the side darkening crushed
  // the periphery (same complaint as hellsine + marimbaba v3 +
  // trancepenta-yt). The warm screen glow above already lifts the
  // centre; we don't need the periphery darkened to make it read.
  ctx.restore();
  ctx.globalCompositeOperation = "source-over";
}

// (verlet string state + pluck/step/needleXAt/draw/warp all live in
//  the shared cover-engine `_vs` above.)
const laneCenterY = {};
for (let li = 0; li < LANES.length; li++) {
  laneCenterY[LANES[li].key] = LANE_TOP + li * (LANE_H + LANE_GUTTER) + LANE_H / 2;
}

function drawPanel(audioT) {
  ctx.save();
  // No backing strip — image shines through the timeline panel entirely.

  // viewport — span the full canvas DIAGONAL (+pad), not just the
  // width, so when the track rotates (and on tall 9:16 frames) the
  // waveforms reach the corners instead of cutting off mid-air.
  const halfSpanSec = (DIAG / 2 + 120) / PX_PER_SEC;
  const tMin = audioT - halfSpanSec;
  const tMax = audioT + halfSpanSec;

  for (let li = 0; li < LANES.length; li++) {
    const lane = LANES[li];
    const y = LANE_TOP + li * (LANE_H + LANE_GUTTER);

    // events in viewport — no outlines, no fills. The waveform itself
    // animates column-by-column as the playhead passes through it:
    //   future column      → dim (anticipation)
    //   playhead column    → brightest, with glow shadow (PRESSED)
    //   ~0.4s decay window → bright with falling alpha (HELD)
    //   past that          → mid alpha (played, settled)
    // Screen blending makes everything soak into the illustration.
    ctx.globalCompositeOperation = "screen";
    for (const ev of laneEvents[lane.key]) {
      if (ev.t > tMax) break;
      const evDur = ev.dur || 0.18;
      if (ev.t + evDur < tMin) continue;
      // playhead pinned to canvas centre regardless of the span width
      const ex = PLAYHEAD_X + (ev.t - audioT) * PX_PER_SEC;
      const ew = Math.max(3, evDur * PX_PER_SEC);
      const wfX = ex + 1, wfY = y + 3, wfW = ew - 2, wfH = LANE_H - 6;
      if (wfW <= 1) continue;

      // pick this event's "voice color" — bells use notepat note color
      const noteRgb = (lane.key === "bells" && Number.isFinite(ev.midi))
        ? midiToNotepatRgb(ev.midi)
        : hexToRgb(lane.color);

      // ── alive waveform: chunky low-rez BLOCKS, alpha tied to the
      // playhead. Wide filled bars (≈9px) instead of 1px lines so the
      // tracks read big and blocky rather than hi-res.
      const BLOCK_PX = 9;
      const cols = Math.max(2, Math.floor(wfW / BLOCK_PX));
      const blockW = Math.max(3, wfW / cols - 2);
      const midY = wfY + wfH / 2;

      // Per-column waveform painting. Varies alpha + line width to
      // express the "press / held / active" feel as the playhead moves
      // through the event. NO shadowBlur — screen blending handles the
      // glow optically without the per-pixel cost of canvas shadows.
      //
      // Source: prefer the per-lane buffer (this lane's mix-volume
      // waveform) so size reflects actual contribution. Fall back to
      // the full mixed audio if the lane buffer isn't available.
      const useLaneBuf = laneAudio[lane.key];
      const srcAudio = useLaneBuf ?? audio;
      const srcSr    = useLaneBuf ? laneSr : audioSr;
      const srcPeak  = useLaneBuf ? laneAudioPeak : audioPeak;
      const startSampL = Math.max(0, Math.floor(ev.t * srcSr));
      const endSampL   = Math.min(srcAudio.length - 1, Math.floor((ev.t + evDur) * srcSr));
      const samplesPerColL = (endSampL - startSampL) / cols;

      const rgbStr = `${noteRgb[0]},${noteRgb[1]},${noteRgb[2]}`;
      // pre-compute peak per column once (loop hoist)
      const colPeak = new Float32Array(cols);
      for (let c = 0; c < cols; c++) {
        const s0 = startSampL + Math.floor(c * samplesPerColL);
        const s1 = Math.min(endSampL, startSampL + Math.floor((c + 1) * samplesPerColL));
        let p = 0;
        for (let s = s0; s < s1; s++) {
          const a = Math.abs(srcAudio[s]);
          if (a > p) p = a;
        }
        colPeak[c] = p / srcPeak;
      }
      for (let c = 0; c < cols; c++) {
        const tCol = ev.t + (c / cols) * evDur;
        const dt = audioT - tCol;
        let alpha;
        // The needle is the LEADING EDGE: blocks to the right of the
        // needle (dt < 0) are future/dim; block under the needle (dt
        // just past 0) is PRESSED; past that decays into HELD/PLAYED.
        if (dt < 0) {
          alpha = 0.16;                            // future — dim
        } else if (dt < 0.05) {
          alpha = 1.0;                             // PRESSED — under needle
        } else if (dt < 0.45) {
          const k = (dt - 0.05) / 0.40;
          alpha = 1.0 - k * 0.55;                  // HELD — decaying
        } else {
          alpha = 0.42;                            // played — settled
        }
        const half = Math.max(2, (colPeak[c] * wfH) / 2);
        // waveform bends/twists with the string — its blocks ride the
        // string's local deflection so the displacement reads as
        // directional, not just a flat row.
        const bend = (needleXAt(midY) - PLAYHEAD_X) * 0.5;
        const bx = wfX + (c / cols) * wfW + bend;
        // …then curve the whole track around the record: rotate the
        // block about the disc centre by its time-offset / GROOVE_R, so
        // it's straight at the needle and arcs more the further out.
        const aGroove = (bx - PLAYHEAD_X) / GROOVE_R;
        ctx.save();
        ctx.translate(ROT_CX, ROT_CY);
        ctx.rotate(aGroove);
        ctx.translate(-ROT_CX, -ROT_CY);
        ctx.fillStyle = `rgba(${rgbStr},${alpha.toFixed(3)})`;
        ctx.fillRect(bx, midY - half, blockW, half * 2);
        ctx.restore();
      }
    }
  }

  // reset blending so the needle + chrome paint normally
  ctx.globalCompositeOperation = "source-over";

  // (the verlet string is drawn later, after the progress bar, so it
  // runs THROUGH the bar with no gap)
  ctx.restore();
}

// ── ffmpeg subprocess (BGRA stdin → mp4 with audio) ──────────────────
const ff = spawnFFmpegEncode({
  audioPath: AUDIO, w: W, h: H, fps: FPS, outPath: OUT, crf: 20,
});
let writeOk = true;
ff.stdin.on("error", () => { writeOk = false; });

// ── perfect-loop cross-dissolve ──────────────────────────────────────
// Snapshot frame 0; over the last LOOP_S the finished frame dissolves
// back into that first frame so the video wraps seamlessly.
const LOOP_S = 1.4;
const loopCanvas = createCanvas(W, H);
const loopCtx = loopCanvas.getContext("2d");
let loopCaptured = false;

// ── frame loop ───────────────────────────────────────────────────────
const t0 = Date.now();
let nextLog = 0.05;
let prevNeedleT = -1;
const laneList = LANES.slice();
// Heavily-smoothed scene-motion state (EMA across frames) so the
// verlet vibe DISTORTS the illustration smoothly and never jumps.
let smPunch = 0;
// progress heartbeat → ~/.ac-pop-renders/ (Slab menubar reads it)
progress.begin({ type: "video", label: `${SLUG} ·yt-landscape` });
for (let f = 0; f < FRAMES; f++) {
  const t = f / FPS;
  // Shift visual readout by AAC priming delay so blocks cross the
  // playhead at the instant the listener actually hears them.
  const audioT = t - AUDIO_DELAY_SEC;
  const env = envAt(audioT);
  const punch = kickEnvAt(audioT);

  // TYPING-INTRO window: first illy under the purple AC prompt for the
  // whole pre-roll [0, PREROLL_SEC) — the boot beeps fire FIRST, then
  // the slug types in, then the track takes over. Skips the normal
  // track render (string held) but still feeds the shared frame
  // write + loop capture, so frame 0 IS the typed-prompt opener.
  if (keyClicks.length && audioT < PREROLL_SEC) {
    drawTypingIntro(audioT);
    // Pals are STAMPED ON over the typed prompt too, so they persist
    // for the WHOLE video (typing → boot → track → shutdown). TikTok
    // cut has NO side pals. Their wiggle is a perfect loop.
    if (!TIKTOK) drawWatermark(audioT);
    // boot beeps now play DURING this window — flash them here too.
    drawBeepRipples(audioT);
    prevNeedleT = audioT;          // don't backfill plucks at hand-off
  } else {

  // pluck the verlet string for every event whose onset falls in
  // this frame (block contacting the needle at that lane's y), then
  // step the physics once.
  for (let li = 0; li < laneList.length; li++) {
    const lane = laneList[li];
    const cy = laneCenterY[lane.key];
    const lrgb = ncHexToRgb(lane.color);     // string segment glows this
    // Gentle plucks — a soft string bend reads as an ORGANIC local
    // warp of the picture; big deflections ghost/tear the strip.
    let amp = 7;
    if (lane.key === "kick" || lane.key === "sub") amp = 12;
    else if (lane.key === "bells") amp = 8;
    else if (lane.key === "hat" || lane.key === "bubbles") amp = 5;
    for (const ev of laneEvents[lane.key]) {
      if (ev.t <= prevNeedleT) continue;
      if (ev.t > audioT) break;
      const sign = ((Math.floor(ev.t * 7) + li) % 2) ? 1 : -1;
      pluckNeedle(cy, amp, sign, lrgb);
    }
  }
  prevNeedleT = audioT;
  stepNeedle();

  const theta = trackTheta(audioT);

  // Illustration: SMOOTH ken-burns only (drawScene/drawCoverKenBurns
  // already breathes + soft-kick-zooms). NO rotation, NO global skew —
  // it never jumps. The kick punch is EMA-smoothed so the zoom glides.
  smPunch = smPunch * 0.80 + punch * 0.20;
  drawScene(audioT, env, smPunch);

  // beach-sunset backlight + contrast layering over the illustration.
  drawBacklight(audioT, env);

  // ONLY the distortion field under the string rotates with the disc —
  // the illustration content itself stays upright (counter-rotated).
  warpUnderString(theta);

  // Rotating disc: the lane waveforms + the string itself.
  withRotation(theta, () => {
    drawPanel(audioT);
    drawNeedleString();
  });

  // fixed HUD, drawn OVER the rotating string.
  // INSTA: bouncing top title + section progress bar + timecode + side
  // pals. TIKTOK: centred title that slowly fades, and NOTHING else —
  // no progress bar, no timecode, no side pals.
  if (TIKTOK) {
    drawCenterTitle(audioT);
  } else {
    // YT cut: title now lives ON the pals (drawWatermark calls
    // drawPalsTitleChars at the end). No horizontal top title.
    drawProgressBar(audioT);
    drawTimecode(audioT);
    drawWatermark(audioT);
  }

  // boot/shutdown beep ripples — fullscreen, on top of everything.
  drawBeepRipples(audioT);
  }  // ── end normal-track render (else of typing-intro) ───────────────

  // LOOP TRANSITION — capture frame 0, then over the tail SHRED frame
  // 0 in over jittered horizontal bands and FLASH-BULB the final
  // frames to blank white. On the wrap the clean frame 0 snaps back →
  // reads as a hard glitchy flash cut, not a soft dissolve.
  if (!loopCaptured) {
    loopCtx.drawImage(canvas, 0, 0);
    loopCaptured = true;
  } else if (audioT > DURATION - LOOP_S) {
    let lp = (audioT - (DURATION - LOOP_S)) / LOOP_S;
    lp = Math.max(0, Math.min(1, lp));
    ctx.save();
    const bh = 22;                              // shred band height
    const seed = Math.floor(audioT * 30) >>> 0; // re-jitter cadence
    for (let by = 0; by < H; by += bh) {
      let r = ((by * 2654435761) >>> 0) ^ seed;
      r = (r ^ (r >>> 13)) >>> 0;
      const rnd = ((r % 1000) / 1000) - 0.5;    // -0.5..0.5
      const reveal = ((r >>> 7) % 1000) / 1000; // per-band entry time
      if (lp < reveal * 0.5) continue;          // bands shred in over time
      const jit = rnd * (50 + 260 * lp) * lp;   // x-shred grows with lp
      ctx.globalAlpha = Math.min(1, 0.4 + lp * 1.0);
      ctx.drawImage(loopCanvas, 0, by, W, bh, jit, by, W, bh);
    }
    if (lp > 0.6) {                             // flash-bulb white blowout
      const fp = (lp - 0.6) / 0.4;
      ctx.globalAlpha = Math.min(1, fp * fp * 1.2);
      ctx.fillStyle = "#ffffff";
      ctx.fillRect(0, 0, W, H);
    }
    ctx.restore();
  }

  const buf = canvas.toBuffer("raw");
  if (!writeOk) break;
  if (!ff.stdin.write(buf)) {
    await new Promise((res) => ff.stdin.once("drain", res));
  }
  const pct = (f + 1) / FRAMES;
  progress.update(pct * 100, { done: f + 1, total: FRAMES });
  if (pct >= nextLog) {
    const elapsed = (Date.now() - t0) / 1000;
    const eta = elapsed / pct - elapsed;
    process.stdout.write(`\r  rendering… ${(pct * 100).toFixed(0)}%  elapsed ${elapsed.toFixed(0)}s  eta ${eta.toFixed(0)}s   `);
    nextLog += 0.05;
  }
}
process.stdout.write("\n");

ff.stdin.end();
await new Promise((res) => ff.on("close", res));
progress.end();
console.log(`✓ ${OUT.replace(REPO + "/", "")}`);
