#!/usr/bin/env node
// chillwave/bin/preview-score.mjs — score-train preview mp4 for a
// chillwave track. Uses the shared rendering primitives from
// pop/lib/preview-shared.mjs (YWFT typography, ken-burns, waveform-
// inside-events, BGRA pix_fmt, audio envelope). The chillwave
// identity — lane palette, section names, structural pacing — lives
// in this file.
//
// Inputs:
//   --slug wundabeach        (default)
//   --cover  path-to.png      (default: out/<slug>-cover.png)
//   --audio  path-to.mp3      (default: out/<slug>.mp3)
//   --struct path-to.json     (default: out/<slug>.struct.json)
//   --out    path-to.mp4      (default: out/<slug>-preview-score.mp4)
//   --size 1080x1080          (default)
//   --fps 30                  (default)

import { existsSync, readFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { createCanvas, loadImage } from "canvas";
import { getNoteColorForOctave } from "../../../system/public/aesthetic.computer/lib/note-colors.mjs";
import {
  checkYwftAvailable,
  decodeAudioMono,
  computeRmsEnvelope,
  prerenderTitleChars,
  magickRenderText,
  drawCoverKenBurns,
  drawTitleBounce,
  spawnFFmpegEncode,
  AUDIO_SR_DEFAULT,
} from "../../lib/preview-shared.mjs";

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

const SLUG   = flags.slug || "undabeach";
// --portrait → vertical 9:16: 1080x1920, "-p" illustration set,
// portrait output filename.
const PORTRAIT = flags.portrait === true;
const TAG    = PORTRAIT ? "-p" : "";
const COVER  = flags.cover  || `${LANE}/out/${SLUG}${TAG}-cover.png`;
const AUDIO  = flags.audio  || `${LANE}/out/${SLUG}.mp3`;
const STRUCT = flags.struct || `${LANE}/out/${SLUG}.struct.json`;
const OUT    = flags.out    || `${LANE}/out/${SLUG}-preview-score${PORTRAIT ? "-portrait" : ""}.mp4`;
const FPS    = Number(flags.fps ?? 30);
const SIZE   = flags.size || (PORTRAIT ? "1080x1920" : "1080x1080");
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
const LANES = [
  { key: "bells",   color: "#aef240", textColor: "#3a6b50" },   // electric lime
  { key: "birds",   color: "#ffd84a", textColor: "#7a5a1f" },   // electric yellow
  { key: "hat",     color: "#4adfff", textColor: "#1f5f7a" },   // pure cyan
  { key: "bubbles", color: "#d96aff", textColor: "#553a99" },   // electric magenta
  { key: "waves",   color: "#3da3ff", textColor: "#1a3a5a" },   // electric blue
  { key: "kick",    color: "#ff4a8a", textColor: "#8a2440" },   // hot pink
  { key: "sub",     color: "#a44af5", textColor: "#3a1f6b" },   // vivid purple
  { key: "fx",      color: "#ff8a3d", textColor: "#5a4a1a" },   // neon orange
];

// Title palette — high-saturation sunlight on water: vivid yellows,
// pure white, punchy lime/seaglass green, saturated gold.
const TITLE_PALETTE = [
  "#ffe600", "#ffffff", "#9cff2e", "#ffc300",
  "#fff24a", "#5bf77c", "#ffd400", "#eaff00",
];

// Section tints — pure dayglo, like the notepat neon palette.
const SECTION_TINTS = {
  "tide-in":  "rgba(255,255,80,1.0)",     // electric neon yellow
  "drift 1":  "rgba(50,200,255,1.0)",     // electric neon cyan
  "swell":    "rgba(255,140,0,1.0)",      // blazing neon orange
  "drift 2":  "rgba(180,50,255,1.0)",     // UV neon purple
  "tide-out": "rgba(50,255,120,1.0)",     // radioactive neon green
};

// ── pre-render YWFT title chars + lane labels + section labels ───────
console.log("  rasterizing YWFT …");
const assetsDir = AUDIO.replace(/\.mp3$/, ".assets");
mkdirSync(assetsDir, { recursive: true });

const titleFontSize = 96;
const { chars: titleChars } = await prerenderTitleChars({
  text: SLUG,
  ptSize: titleFontSize,
  palette: TITLE_PALETTE,
  shadowColor: "rgba(0,0,0,0.78)",
  assetsDir,
});

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
const LANE_GUTTER  = 8;                     // tighter gutter → taller lanes
const LANE_TOP     = 200;                   // below title
const LANE_BOTTOM  = H - 108;               // leaves room for progress bar + larger timecode
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
function drawWatermark(audioT) {
  if (!palsImg) return;
  // FOUR mini sideways stamps — left & right edges, at 1/3 and 2/3
  // height (TikTok-style side tags). Hue-cycling, SEEPED into the
  // picture (multiply + color-burn + overlay + faint normal) so the
  // pencil tooth reads straight through them.
  const s = 66;
  const hue = ((audioT * 14) % 360 + 360) % 360;
  const col = hslToRgb(hue, 0.9, 0.62);
  palsTinted(col, palsBlur);                 // wmCanvas ← tinted glyph
  const m = 18;
  const spots = [
    { cx: m + s / 2,     cy: H / 3,     rot: -Math.PI / 2 }, // left  upper
    { cx: m + s / 2,     cy: 2 * H / 3, rot: -Math.PI / 2 }, // left  lower
    { cx: W - m - s / 2, cy: H / 3,     rot:  Math.PI / 2 }, // right upper
    { cx: W - m - s / 2, cy: 2 * H / 3, rot:  Math.PI / 2 }, // right lower
  ];
  const passes = [
    ["multiply",   0.62], ["color-burn", 0.30],
    ["overlay",    0.45], ["source-over", 0.12],
  ];
  for (const sp of spots) {
    ctx.save();
    ctx.translate(sp.cx, sp.cy);
    ctx.rotate(sp.rot);
    for (const [op, a] of passes) {
      ctx.globalCompositeOperation = op;
      ctx.globalAlpha = a;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    }
    ctx.restore();
  }
  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
}

const TRANS_S = 1.5;                        // slide+fade transition length
const KB = { baseScale: 1.20, breathAmp: 0.06, breathPeriodSec: 18,
             wobbleAmp: 14, envWobbleAmp: 18 };
function sectionIndexAt(t) {
  let idx = 0;
  for (let i = 0; i < SECTIONS.length; i++) if (t >= SECTIONS[i].startSec) idx = i;
  return idx;
}
// Old-style cinema PUSH: at each section boundary the outgoing illy
// stays fully opaque and slides off one edge while the incoming illy
// pushes in flush against it from the opposite edge — both at full
// opacity, no cross-fade, so the prior image is always visible and
// there is never a cut-to-black. Direction cycles through all four
// cardinals (left / right / up / down) per boundary.
const PUSH_DIRS = ["left", "right", "up", "down"];
function drawScene(audioT, env, punch) {
  const idx = sectionIndexAt(audioT);
  const sec = SECTIONS[idx];
  const sinceStart = audioT - sec.startSec;
  if (idx > 0 && sinceStart >= 0 && sinceStart < TRANS_S) {
    let p = sinceStart / TRANS_S;
    p = p * p * (3 - 2 * p);                 // smoothstep ease
    const dir = PUSH_DIRS[(idx - 1) % PUSH_DIRS.length];
    let oX = 0, oY = 0, nX = 0, nY = 0;
    if (dir === "left")  { oX = -p * W;       nX = (1 - p) * W; }
    if (dir === "right") { oX =  p * W;       nX = -(1 - p) * W; }
    if (dir === "up")    { oY = -p * H;       nY = (1 - p) * H; }
    if (dir === "down")  { oY =  p * H;       nY = -(1 - p) * H; }
    // outgoing — full opacity, slides off; incoming — full opacity,
    // pushes in flush behind it (drawn second, on top at the seam).
    drawCoverKenBurns(ctx, sectionImgs[idx - 1], audioT, {
      ...KB, env, punch, offsetX: oX, offsetY: oY, alpha: 1, fillBackground: true,
    });
    drawCoverKenBurns(ctx, sectionImgs[idx], audioT, {
      ...KB, env, punch, offsetX: nX, offsetY: nY, alpha: 1, fillBackground: false,
    });
  } else {
    drawCoverKenBurns(ctx, sectionImgs[idx], audioT, { ...KB, env, punch });
  }
}


function drawTitle(audioT, env) {
  const titleY = 64 + Math.ceil(titleFontSize * 1.25);
  drawTitleBounce(ctx, {
    chars: titleChars,
    ptSize: titleFontSize,
    baseX: 32,
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
    const segX0 = (sec.startSec / DURATION) * W;
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
const ROT_CX = PLAYHEAD_X;        // == W/2
const ROT_CY = H / 2;
const DIAG   = Math.hypot(W, H);
const TWO_PI = Math.PI * 2;
// Waveforms ride a big virtual record groove: each block is rotated
// about the disc centre by (timeOffsetPx / GROOVE_R), so the tracks
// are near-straight at the needle and curve more the further out
// (incoming/exterior) they are — like grooves on a turning record.
const GROOVE_R = 950;
function trackTheta(audioT) {
  return TWO_PI * Math.max(0, Math.min(1, audioT / DURATION));
}
let CUR_THETA = 0;                 // current track rotation (for lighting)
function withRotation(theta, fn) {
  ctx.save();
  ctx.translate(ROT_CX, ROT_CY);
  ctx.rotate(theta);
  ctx.translate(-ROT_CX, -ROT_CY);
  fn();
  ctx.restore();
}

// ── boot / shutdown beep ripples ─────────────────────────────────────
// One fullscreen radial "dink" (flash + expanding ring) per ac-native
// chime note, from struct.events.sfx (boot-beep-N / shutdown-beep-N).
const beepEvents = (struct.events?.sfx || [])
  .filter((e) => /^(boot|shutdown)-beep-\d+$/.test(e.name))
  .map((e) => ({ t: e.t, boot: e.name.startsWith("boot") }))
  .sort((a, b) => a.t - b.t);
function drawBeepRipples(audioT) {
  const RIPPLE_S = 1.1;
  for (const b of beepEvents) {
    const dt = audioT - b.t;
    if (dt < 0 || dt > RIPPLE_S) continue;
    const p = dt / RIPPLE_S;                 // 0..1
    const ease = 1 - (1 - p) * (1 - p);
    const cx = W / 2, cy = H / 2;
    const maxR = Math.hypot(W, H) / 2;
    const r = ease * maxR;
    const tint = b.boot ? "180,255,170" : "255,170,150";
    ctx.save();
    // expanding thin ring
    ctx.globalCompositeOperation = "screen";
    ctx.strokeStyle = `rgba(${tint},${(0.55 * (1 - p)).toFixed(3)})`;
    ctx.lineWidth = 3 + 6 * (1 - p);
    ctx.beginPath();
    ctx.arc(cx, cy, r, 0, TWO_PI);
    ctx.stroke();
    // brief central flash on the first ~18% of the ripple
    if (p < 0.18) {
      const fa = (1 - p / 0.18) * 0.5;
      const g = ctx.createRadialGradient(cx, cy, 0, cx, cy, maxR * 0.6);
      g.addColorStop(0, `rgba(${tint},${fa.toFixed(3)})`);
      g.addColorStop(1, `rgba(${tint},0)`);
      ctx.fillStyle = g;
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
  // contrast layer — multiply vignette darkens the periphery so the
  // backlit centre reads with punch.
  ctx.globalCompositeOperation = "multiply";
  const vg = ctx.createRadialGradient(gx, gy, H * 0.18, gx, gy, H * 0.78);
  vg.addColorStop(0, "rgba(255,255,255,1)");
  vg.addColorStop(1, "rgba(70,52,70,1)");
  ctx.fillStyle = vg;
  ctx.fillRect(0, 0, W, H);
  ctx.restore();
  ctx.globalCompositeOperation = "source-over";
}

// ── verlet-string playhead ───────────────────────────────────────────
// A pinned vertical string the waveform blocks "pluck" as they cross.
// Spans the full diagonal (+pad) so the ends sit off-canvas — drawn as
// ONE smooth curve (no visible segments, no alpha-overlap knots).
const NS_SPAN = DIAG + 120;
const NS_N  = 80;
const NS_Y0 = ROT_CY - NS_SPAN / 2;
const NS_Y1 = ROT_CY + NS_SPAN / 2;
const nsY  = new Float32Array(NS_N);
const nsX  = new Float32Array(NS_N);
const nsPX = new Float32Array(NS_N);
const nsHot = new Float32Array(NS_N);
for (let i = 0; i < NS_N; i++) {
  nsY[i] = NS_Y0 + (NS_Y1 - NS_Y0) * (i / (NS_N - 1));
  nsX[i] = PLAYHEAD_X; nsPX[i] = PLAYHEAD_X;
}
// Per-node colour — each segment glows in the colour of the waveform
// lane that last plucked it (ported from the trancenwaltz string).
const nsCR = new Float32Array(NS_N);
const nsCG = new Float32Array(NS_N);
const nsCB = new Float32Array(NS_N);
nsCR.fill(255); nsCG.fill(232); nsCB.fill(150);   // default nylon cream
function ncHexToRgb(hex) {
  const h = String(hex).replace("#", "");
  return [parseInt(h.slice(0, 2), 16) || 0,
          parseInt(h.slice(2, 4), 16) || 0,
          parseInt(h.slice(4, 6), 16) || 0];
}
const laneCenterY = {};
for (let li = 0; li < LANES.length; li++) {
  laneCenterY[LANES[li].key] = LANE_TOP + li * (LANE_H + LANE_GUTTER) + LANE_H / 2;
}
function pluckNeedle(yPix, amount, sign, rgb) {
  let idx = Math.round(((yPix - NS_Y0) / (NS_Y1 - NS_Y0)) * (NS_N - 1));
  idx = Math.max(2, Math.min(NS_N - 3, idx));
  nsX[idx]     += sign * amount;
  nsX[idx - 1] += sign * amount * 0.55;
  nsX[idx + 1] += sign * amount * 0.55;
  nsHot[idx] = 1;
  nsHot[idx - 1] = Math.max(nsHot[idx - 1], 0.7);
  nsHot[idx + 1] = Math.max(nsHot[idx + 1], 0.7);
  if (rgb) {
    for (const j of [idx - 1, idx, idx + 1]) {
      nsCR[j] = rgb[0]; nsCG[j] = rgb[1]; nsCB[j] = rgb[2];
    }
  }
}
function stepNeedle() {
  for (let i = 1; i < NS_N - 1; i++) {
    const v = (nsX[i] - nsPX[i]) * 0.90;                 // damping → ring
    nsPX[i] = nsX[i];
    nsX[i] = nsX[i] + v + (PLAYHEAD_X - nsX[i]) * 0.020; // tension home
  }
  for (let pass = 0; pass < 2; pass++) {                  // string stiffness
    for (let i = 1; i < NS_N - 1; i++) {
      nsX[i] = (nsX[i] * 2 + nsX[i - 1] + nsX[i + 1]) * 0.25;
    }
  }
  nsX[0] = nsX[NS_N - 1] = PLAYHEAD_X;
  for (let i = 0; i < NS_N; i++) nsHot[i] *= 0.90;
}
// String x at any pixel y (linear nsY mapping → interpolate nsX).
function needleXAt(yPix) {
  const f = Math.max(0, Math.min(1, (yPix - NS_Y0) / (NS_Y1 - NS_Y0))) * (NS_N - 1);
  const i = Math.floor(f), frac = f - i;
  if (i >= NS_N - 1) return nsX[NS_N - 1];
  return nsX[i] + (nsX[i + 1] - nsX[i]) * frac;
}
// Trancenwaltz-style string: single-path dark shadow, then per-segment
// nylon — dark casing → body tinted by the plucking lane's colour →
// specular core. Idle stretches stay faint + thin.
function drawNeedleString() {
  ctx.save();
  ctx.lineCap = "round";
  ctx.lineJoin = "round";

  // Fixed WORLD light from the top-right. The string is drawn inside
  // the rotating frame, so the shadow (down-left in world) and the
  // specular (toward the light in world) are the world vectors rotated
  // by -CUR_THETA into the string's local space → the lit side stays
  // top-right and the shadow angle swings as the record turns.
  const sc = Math.cos(CUR_THETA), ss = Math.sin(CUR_THETA);
  const SHX = -3.5, SHY = 4.5;               // world shadow (down-left)
  const SPX = 1.7, SPY = -1.7;               // world specular (up-right)
  const sdx = SHX * sc + SHY * ss;
  const sdy = -SHX * ss + SHY * sc;
  const spx = SPX * sc + SPY * ss;
  const spy = -SPX * ss + SPY * sc;

  // 1) drop shadow — one continuous stroke (no overlap buildup)
  ctx.strokeStyle = "rgba(0,0,0,0.5)";
  ctx.lineWidth = 5;
  ctx.beginPath();
  ctx.moveTo(nsX[0] + sdx, nsY[0] + sdy);
  for (let i = 1; i < NS_N - 1; i++) {
    const mx = (nsX[i] + nsX[i + 1]) / 2 + sdx;
    const my = (nsY[i] + nsY[i + 1]) / 2 + sdy;
    ctx.quadraticCurveTo(nsX[i] + sdx, nsY[i] + sdy, mx, my);
  }
  ctx.lineTo(nsX[NS_N - 1] + sdx, nsY[NS_N - 1] + sdy);
  ctx.stroke();

  // 2) per-segment, colour held from whatever waveform plucked it
  for (let i = 1; i < NS_N; i++) {
    const h = Math.max(nsHot[i], nsHot[i - 1]);
    const dev = Math.abs(((nsX[i] + nsX[i - 1]) / 2) - PLAYHEAD_X);
    const act = Math.min(1, Math.max(h, dev / 26));
    const x0 = nsX[i - 1], y0 = nsY[i - 1], x1 = nsX[i], y1 = nsY[i];

    ctx.strokeStyle = `rgba(60,46,32,${(0.10 + act * 0.40).toFixed(3)})`;
    ctx.lineWidth = 4 + act * 4;
    ctx.beginPath(); ctx.moveTo(x0, y0); ctx.lineTo(x1, y1); ctx.stroke();

    const cr = (nsCR[i] + nsCR[i - 1]) / 2;
    const cg = (nsCG[i] + nsCG[i - 1]) / 2;
    const cb = (nsCB[i] + nsCB[i - 1]) / 2;
    const wb = 0.16 * act;                 // keep the lane colour dominant
    const br = Math.round(cr + (255 - cr) * wb);
    const bg = Math.round(cg + (255 - cg) * wb);
    const bbl = Math.round(cb + (255 - cb) * wb);
    ctx.strokeStyle = `rgba(${br},${bg},${bbl},${(0.13 + act * 0.80).toFixed(3)})`;
    ctx.lineWidth = 2.2 + act * 3.4;
    ctx.beginPath(); ctx.moveTo(x0, y0); ctx.lineTo(x1, y1); ctx.stroke();

    const specA = act * 0.85;
    if (specA > 0.02) {
      ctx.strokeStyle = `rgba(255,255,246,${specA.toFixed(3)})`;
      ctx.lineWidth = 1;
      ctx.beginPath();
      ctx.moveTo(x0 + spx, y0 + spy); ctx.lineTo(x1 + spx, y1 + spy);
      ctx.stroke();
    }
  }
  ctx.restore();
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
for (let f = 0; f < FRAMES; f++) {
  const t = f / FPS;
  // Shift visual readout by AAC priming delay so blocks cross the
  // playhead at the instant the listener actually hears them.
  const audioT = t - AUDIO_DELAY_SEC;
  const env = envAt(audioT);
  const punch = kickEnvAt(audioT);

  // pluck the verlet string for every event whose onset falls in
  // this frame (block contacting the needle at that lane's y), then
  // step the physics once.
  for (let li = 0; li < laneList.length; li++) {
    const lane = laneList[li];
    const cy = laneCenterY[lane.key];
    const lrgb = ncHexToRgb(lane.color);     // string segment glows this
    let amp = 22;
    if (lane.key === "kick" || lane.key === "sub") amp = 40;
    else if (lane.key === "bells") amp = 26;
    else if (lane.key === "hat" || lane.key === "bubbles") amp = 16;
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
  CUR_THETA = theta;                 // string lighting follows rotation

  // string-vibe displacement of the illustration — visibly tugged by
  // the verlet vibes. Magnitude from the PEAK deflection (the punch),
  // direction along the string's world normal; plus a small rock + a
  // breath-scale so the picture clearly shoves/tilts with the string.
  let devSum = 0, devPeak = 0;
  for (let i = 1; i < NS_N - 1; i++) {
    const d = nsX[i] - PLAYHEAD_X;
    devSum += d;
    if (Math.abs(d) > Math.abs(devPeak)) devPeak = d;
  }
  const devAvg = devSum / (NS_N - 2);
  const vibe = Math.max(-60, Math.min(60, devPeak));
  const dispMag = vibe * 1.4;
  const dispX = dispMag * Math.cos(theta);
  const dispY = dispMag * Math.sin(theta);
  const sceneRot = Math.max(-0.055, Math.min(0.055, devAvg * 0.0019));
  const sceneScale = 1 + Math.min(1, Math.abs(vibe) / 60) * 0.025;

  ctx.save();
  ctx.translate(ROT_CX + dispX, ROT_CY + dispY);
  ctx.rotate(sceneRot);
  ctx.scale(sceneScale, sceneScale);
  ctx.translate(-ROT_CX, -ROT_CY);
  drawScene(audioT, env, punch);
  ctx.restore();

  // beach-sunset backlight + contrast layering over the illustration.
  drawBacklight(audioT, env);

  // the track group — lane waveforms + string — rotates as one disc,
  // a plain 360° about centre across the whole record. Not clipped, so
  // the tracks extend out past the old band as it turns.
  withRotation(theta, () => {
    drawPanel(audioT);
    drawNeedleString();
  });

  // fixed HUD, drawn OVER the rotating string.
  drawTitle(audioT, env);
  drawProgressBar(audioT);
  drawTimecode(audioT);
  drawWatermark(audioT);

  // boot/shutdown beep ripples — fullscreen, on top of everything.
  drawBeepRipples(audioT);

  // perfect loop: capture frame 0, then dissolve the tail back into it.
  if (!loopCaptured) {
    loopCtx.drawImage(canvas, 0, 0);
    loopCaptured = true;
  } else if (audioT > DURATION - LOOP_S) {
    let lp = (audioT - (DURATION - LOOP_S)) / LOOP_S;
    lp = Math.max(0, Math.min(1, lp));
    const a = lp * lp * (3 - 2 * lp) * 0.92;   // smoothstep → 0.92
    ctx.save();
    ctx.globalAlpha = a;
    ctx.drawImage(loopCanvas, 0, 0);
    ctx.restore();
  }

  const buf = canvas.toBuffer("raw");
  if (!writeOk) break;
  if (!ff.stdin.write(buf)) {
    await new Promise((res) => ff.stdin.once("drain", res));
  }
  const pct = (f + 1) / FRAMES;
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
console.log(`✓ ${OUT.replace(REPO + "/", "")}`);
