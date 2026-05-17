#!/usr/bin/env node
// cover-video.mjs — per-frame canvas-rendered "score-train" music
// video. Draws each frame programmatically via node-canvas and pipes
// raw RGBA into ffmpeg for encoding. This replaces the pre-rendered
// PNG approach so we get pixel-perfect alignment AND can animate
// events (note flashes on trigger, etc.) without resampling.
//
// Layout (top → bottom):
//   • illustration (ken-burns drift) — top 1000 px
//   • char-by-char bouncing title typography (floats over the scene)
//   • score panel (1000 → 1500 px): multi-lane piano-roll-style
//     timeline that scrolls horizontally past a fixed center playhead.
//     Notes flash brighter at their trigger time, fading back to the
//     base color over ~120 ms so the rhythm reads visually.
//   • duration label at bottom-left
//
// Inputs:
//   --illustration   path to raw illustration PNG (no typography)
//   --track          path to the rendered MP3
//   --struct         path to .struct.json (defaults to <track>.assets/struct.json)
//   --title          title string
//   --bpm            beats per minute
//   --out            output mp4 path

import { existsSync, readFileSync, mkdirSync, writeFileSync } from "node:fs";
import { spawn, spawnSync } from "node:child_process";
import { resolve } from "node:path";
import { homedir } from "node:os";
import { createCanvas, loadImage } from "canvas";

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  }
}

function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const ILLUSTRATION = expandHome(flags.illustration);
// Optional per-section illustrations — comma-separated `name=path`
// pairs. The visualizer crossfades between them as the song's
// musical section shifts. Sections not mapped fall back to the
// default --illustration.
const SECTION_ILLUSTRATIONS = (() => {
  const m = new Map();
  const raw = flags["illustrations"];
  if (!raw) return m;
  for (const pair of String(raw).split(",")) {
    const [k, v] = pair.split("=");
    if (k && v) m.set(k.trim(), expandHome(v.trim()));
  }
  return m;
})();
const TRACK = expandHome(flags.track);
function locateStruct(track) {
  if (!track) return null;
  const stem = track.replace(/\.mp3$/, "");
  const sub = `${stem}.assets/struct.json`;
  const legacy = `${stem}.struct.json`;
  if (existsSync(sub)) return sub;
  if (existsSync(legacy)) return legacy;
  return sub;
}
const STRUCT = expandHome(flags.struct) || locateStruct(TRACK);
const OUT = expandHome(flags.out) || (TRACK ? TRACK.replace(/\.mp3$/, "-cover.mp4") : null);
const TITLE = (flags.title || "track").toString();
const BPM = Number(flags.bpm ?? 137.143);
const FPS = Number(flags.fps ?? 30);
// Comma-separated lane keys to omit from the timeline (e.g. piano
// for tracks where it only fires in a small slice of the song).
const HIDE_LANES = new Set(
  (flags["hide-lanes"] || "").toString().split(",").map((s) => s.trim()).filter(Boolean)
);

if (!ILLUSTRATION || !existsSync(ILLUSTRATION)) {
  console.error(`✗ --illustration is required and must exist (got: ${ILLUSTRATION})`);
  process.exit(1);
}
if (!TRACK || !existsSync(TRACK)) {
  console.error(`✗ --track is required and must exist`);
  process.exit(1);
}
if (!STRUCT || !existsSync(STRUCT)) {
  console.error(`✗ --struct is required and must exist (default: <track>.assets/struct.json)`);
  process.exit(1);
}

// Probe track duration.
const probe = spawnSync("ffprobe", [
  "-v", "error",
  "-show_entries", "format=duration",
  "-of", "csv=p=0",
  TRACK,
], { encoding: "utf8" });
const duration = parseFloat(probe.stdout.trim()) || 60;
const struct = JSON.parse(readFileSync(STRUCT, "utf8"));

// ── pre-decode raw audio + amplitude envelope ────────────────────────
// Decode the audio at AUDIO_SR Hz mono. We keep BOTH:
//   • raw audio samples — used to draw actual waveform shapes inside
//     each SFX event rectangle so the timeline reads as a mini-DAW.
//   • per-60Hz normalized RMS — drives the title bounce + scene wobble.
const AUDIO_SR = 4000; // 4 kHz mono — gives ~125 µs resolution for the SFX waveform display
console.log(`▸ extracting audio @ ${AUDIO_SR} Hz`);
const decRaw = spawnSync("ffmpeg", [
  "-hide_banner", "-loglevel", "error",
  "-i", TRACK,
  "-f", "f32le", "-ar", String(AUDIO_SR), "-ac", "1",
  "-",
], { encoding: "buffer", maxBuffer: 1024 * 1024 * 256 });
let envelope = new Float32Array(Math.ceil(duration * 60) + 4);
let audioRaw = new Float32Array(0);
let audioPeak = 1;
if (decRaw.status === 0 && decRaw.stdout) {
  audioRaw = new Float32Array(decRaw.stdout.buffer, decRaw.stdout.byteOffset, decRaw.stdout.byteLength / 4);
  // Track peak so the waveform display can normalize amplitudes.
  for (let i = 0; i < audioRaw.length; i++) {
    const a = Math.abs(audioRaw[i]);
    if (a > audioPeak) audioPeak = a;
  }
  // Compute per-60-Hz RMS over a ~30 ms window centered on each frame.
  const ENV_OUT_SR = 60;
  const winSamples = Math.floor(0.030 * AUDIO_SR);
  const stepSamples = AUDIO_SR / ENV_OUT_SR;
  envelope = new Float32Array(Math.ceil(duration * ENV_OUT_SR) + 4);
  for (let i = 0; i < envelope.length; i++) {
    const center = Math.floor(i * stepSamples);
    let sum = 0, n = 0;
    for (let j = -winSamples; j <= winSamples; j++) {
      const idx = center + j;
      if (idx < 0 || idx >= audioRaw.length) continue;
      sum += audioRaw[idx] * audioRaw[idx];
      n++;
    }
    envelope[i] = Math.sqrt(sum / Math.max(1, n));
  }
  // Normalize envelope so peak = 1.0 (relative loudness across track).
  let envMax = 0;
  for (let i = 0; i < envelope.length; i++) if (envelope[i] > envMax) envMax = envelope[i];
  if (envMax > 0) for (let i = 0; i < envelope.length; i++) envelope[i] /= envMax;
}
function envAt(t) {
  const idx = Math.floor(t * 60);
  if (idx < 0 || idx >= envelope.length) return 0;
  return envelope[idx];
}

// YWFT-Processing's TTF only resolves correctly under ImageMagick's
// freetype path — node-canvas (cairo + fontconfig) renders it as
// missing-glyph tofu boxes. So we pre-rasterize all YWFT text via
// magick to transparent PNGs at startup and composite the resulting
// images per-frame instead of using canvas text APIs. The rest of
// the labels (section names, lane gutters, duration) use the
// default system sans which node-canvas DOES render fine.
const YWFT_PATH = `${homedir()}/Library/Fonts/ywft-processing-bold.ttf`;
// Auto-detect face rect for the illustration if a sidecar exists.
// Run detect-face.py once via:
//   recap/.venv/bin/python3 pop/dance/bin/detect-face.py <illustration>
let faceRect = null;
if (ILLUSTRATION) {
  const facePath = `${ILLUSTRATION}.face.json`;
  if (existsSync(facePath)) {
    try {
      const f = JSON.parse(readFileSync(facePath, "utf8"));
      if (f.detected !== false && f.w && f.h) {
        faceRect = f;
      }
    } catch {}
  }
}

// Per-section face + synthetic laptop bboxes, in IMAGE coords. Used
// by the lane-driven backlight (kicks light up the face region, hats
// light up the laptop region). Loaded from <illustration>.face.json
// sidecars. Laptop bbox is SYNTHESIZED below the face — color-based
// detection on the painterly green laptop is too noisy across mixed
// scenes; "below the face" is reliable for all our compositions.
function loadRectsForIllustration(path) {
  if (!path || !existsSync(path)) return null;
  const facePath = `${path}.face.json`;
  if (!existsSync(facePath)) return null;
  try {
    const f = JSON.parse(readFileSync(facePath, "utf8"));
    if (f.detected === false || !f.w || !f.h) return { face: null, laptop: null, imgW: f.imgW, imgH: f.imgH };
    const face = { x: f.x, y: f.y, w: f.w, h: f.h };
    // Synthetic laptop just below the face.
    const lx = Math.max(0, f.x - f.w * 0.30);
    const ly = Math.min(f.imgH - 1, f.y + f.h * 1.25);
    const lw = Math.min(f.imgW - lx, f.w * 1.60);
    const lh = Math.min(f.imgH - ly, f.h * 0.85);
    const laptop = { x: lx, y: ly, w: lw, h: lh };
    return { face, laptop, imgW: f.imgW, imgH: f.imgH };
  } catch { return null; }
}

// ── canvas layout ────────────────────────────────────────────────────
// The illustration fills the entire 1500×1500 canvas (NO banding).
// Score panel + title + duration all render ON TOP of it. The score
// occupies a translucent strip across the bottom so the illustration
// shows through behind the lanes.
// Canvas size — default 1500x1500 square. Pass --size 1080x1920 for
// a vertical 9:16 render (the same v8 illustrations cover-fit at this
// aspect with a small horizontal crop, no new gens needed).
function parseSize(s) {
  const m = /^(\d+)x(\d+)$/.exec(String(s).trim());
  if (!m) return [1500, 1500];
  return [Number(m[1]), Number(m[2])];
}
const [W, H] = parseSize(flags.size || "1500x1500");
// Score panel spans the FULL canvas height — events overlay the
// illustration directly with NO dark backing strip. Each event gets
// a strong outline so it reads against varied illustration colors.
// Title/progress/timecode chrome floats at the very top and bottom
// edges.
const SCORE_TOP = 40;
const SCORE_BOTTOM = H - 40;
const SCORE_H = SCORE_BOTTOM - SCORE_TOP;

// Lanes — one strip per instrument category. Every fired sound in
// the rendered track is represented in some lane; macro-textures
// (pad) are conveyed via the per-section background tints instead.
const LANES = [
  { key: "sfx",      label: "SFX",   color: "#ff9a40", lo: 0,  hi: 0  }, // boot/chime/sniper/impact/zoo/guns/drones/whistle
  { key: "vox",      label: "VOX",   color: "#f599c6", lo: 0,  hi: 0  }, // greeting/booty/screams/bye/vocal stem
  { key: "sub",      label: "SUB",   color: "#a855f7", lo: 30, hi: 60 },
  { key: "kick",     label: "KICK",  color: "#ff5a8a", lo: 30, hi: 50 },
  { key: "snare",    label: "SNARE", color: "#e7c4ff", lo: 0,  hi: 0  }, // snare-roll subdivisions (build crescendo)
  { key: "hat",      label: "HAT",   color: "#7ad9f5", lo: 60, hi: 78 },
  { key: "bells",    label: "BELLS", color: "#4ad1bf", lo: 30, hi: 90 },
  { key: "lead",     label: "LEAD",  color: "#aef240", lo: 40, hi: 84 },
  { key: "piano",    label: "PIANO", color: "#f5d240", lo: 50, hi: 90 },
  { key: "supersaw", label: "SAW",   color: "#9aff4d", lo: 40, hi: 84 },
];
// laneH is computed lazily inside drawFrame() so empty-lane filtering
// (which happens later, once event arrays are loaded) sizes lanes to
// the populated count, not the declared-lane count.
let laneH;
// Per-section timeline zoom — drop sections STRETCH out so each
// climactic beat gets visual space; intros/outros SQUASH so you
// see further ahead/behind. Transitioned smoothly at section
// boundaries (1.5 s ease). The base PX_PER_SEC is used as a
// fallback for sections not listed here.
const PX_PER_SEC = 820;
const SECTION_PX_PER_SEC = {
  intro:  600,   // squashed — see the whole opening at once
  break1: 760,
  build1: 1000,  // stretched — anticipation builds
  drop1:  1180,  // peak stretch — drop detail
  break2: 760,
  build2: 1000,
  drop2:  1300,  // most stretched — final climax
  outro:  650,   // squashed — retrospective wind-down
};
// Audio-sync compensation — the AAC encoder inserts ~21 ms priming
// at the start of the audio stream, so audio playback lags the
// visual playhead by that amount. Shifting the visual readout
// LATER by this delay realigns the visualization with what the
// listener actually hears.
const AUDIO_DELAY_SEC = Number(flags["audio-delay"] ?? 0.045);
const playheadX = Math.round(W / 2);

// Section tints — DESATURATED palette for the progress bar / dividers.
// Each section gets a muted version of its color family so the bar
// reads as a soft pastel painting of the song structure rather than
// loud neon. Alpha here is the baseline; rendering further modulates
// per-section play state.
const SECTION_TINTS = {
  intro:  "rgba(46,210,118,0.20)",    // vivid emerald
  break1: "rgba(54,150,240,0.20)",    // saturated azure
  build1: "rgba(245,168,40,0.20)",    // bright amber
  drop1:  "rgba(240,64,156,0.20)",    // hot magenta-pink
  break2: "rgba(54,150,240,0.20)",
  build2: "rgba(245,168,40,0.20)",
  drop2:  "rgba(240,64,156,0.20)",
  outro:  "rgba(150,120,205,0.20)",   // dusk violet
};

// ── pre-sort events by lane so per-frame culling is fast ─────────────
// Each lane's events also get a `stackRow` index — events that overlap
// in time within the same lane get assigned to different vertical
// rows so doubled hits don't visually merge into a single block.
const laneEvents = {};
for (const lane of LANES) {
  const evs = (struct.events?.[lane.key] || []).slice();
  // Sort by displayedT/t so the tape-stop remap is respected.
  evs.sort((a, b) => (a.displayedT ?? a.t) - (b.displayedT ?? b.t));
  // Greedy interval-graph coloring — pick the lowest row whose
  // previous event has already ended (with a small gap window).
  // Events that overlap go on different rows so the timeline shows
  // doubled hits as visually separated marks.
  const rowEndT = [];
  const STACK_GAP = 0.02; // 20 ms cluster threshold
  for (const ev of evs) {
    const startT = ev.displayedT ?? ev.t;
    const endT = startT + Math.max(0.02, ev.dur || 0.06);
    let row = 0;
    while (row < rowEndT.length && rowEndT[row] > startT - STACK_GAP) row++;
    if (row === rowEndT.length) rowEndT.push(endT);
    else rowEndT[row] = endT;
    ev.stackRow = row;
  }
  lane.maxStackRows = Math.max(1, rowEndT.length);
  laneEvents[lane.key] = evs;
}

// ── lane-population validation ───────────────────────────────────────
// Confirm every declared lane has the data we expect. Prints a table
// to stderr so missing/empty lanes are obvious BEFORE we spend 6 min
// rendering. Pitched lanes also show their MIDI range so it's clear
// whether the events will land inside or get clamped outside the lane.
// Empty lanes are DROPPED from the active LANES array so the
// visualization only shows lanes with actual data (so chill mode's
// empty SNARE/VOX slots don't sit blank).
const allLanes = LANES.slice();
console.log(`▸ lane populations:`);
for (const lane of allLanes) {
  const evs = laneEvents[lane.key];
  const note = evs.length === 0 ? "·· EMPTY ··"
             : ev => ev.midi !== undefined
                ? `midi[${Math.min(...evs.map(e => e.midi))}-${Math.max(...evs.map(e => e.midi))}]`
                : "";
  const pitchInfo = (lane.hi - lane.lo) > 0 && evs.length > 0
    ? ` pitch range ${Math.min(...evs.map(e => e.midi ?? 0))}-${Math.max(...evs.map(e => e.midi ?? 0))} (lane ${lane.lo}-${lane.hi})`
    : "";
  console.log(`  · ${lane.label.padEnd(6)} (${lane.key.padEnd(8)}) → ${String(evs.length).padStart(4)} events, stack=${lane.maxStackRows}${evs.length === 0 ? "  ·· EMPTY ·· (dropped)" : pitchInfo}`);
  void note;
}
// Filter LANES down to populated lanes (and respect --hide-lanes).
// The visualization no longer reserves space for empty slots or for
// lanes the user explicitly hid.
{
  const populated = allLanes.filter((l) => (laneEvents[l.key] || []).length > 0 && !HIDE_LANES.has(l.key));
  LANES.length = 0;
  for (const l of populated) LANES.push(l);
}
console.log(`  → rendering ${LANES.length} populated lane${LANES.length === 1 ? "" : "s"}${HIDE_LANES.size ? ` (hidden: ${[...HIDE_LANES].join(",")})` : ""}`);

// Now that LANES is finalized, compute per-lane height.
laneH = Math.floor(SCORE_H / Math.max(1, LANES.length));
const dropImpacts = (struct.events?.dropImpact || []).slice().sort((a, b) => a.t - b.t);

// ── verlet-string playhead (ported from the wanderbeach video) ──────
// The playhead is a pinned vertical segmented string. Each event
// "plucks" it at its lane's y as it crosses the centre — a horizontal
// impulse — then it oscillates and settles (leftover ring) via verlet
// damping + string-stiffness smoothing.
const NS_N  = 56;
const NS_Y0 = 0;
const NS_Y1 = H; // runs full height THROUGH the progress bar
const nsY  = new Float32Array(NS_N);
const nsX  = new Float32Array(NS_N);
const nsPX = new Float32Array(NS_N);
const nsHot = new Float32Array(NS_N);
for (let i = 0; i < NS_N; i++) {
  nsY[i] = NS_Y0 + (NS_Y1 - NS_Y0) * (i / (NS_N - 1));
  nsX[i] = playheadX; nsPX[i] = playheadX;
}
const laneCenterY = {};
for (let li = 0; li < LANES.length; li++) {
  laneCenterY[LANES[li].key] = SCORE_TOP + li * laneH + laneH / 2;
}
// Per-node colour — the string segment glows in the colour of the
// waveform/lane that last plucked it (held while that node is hot).
const nsCR = new Float32Array(NS_N);
const nsCG = new Float32Array(NS_N);
const nsCB = new Float32Array(NS_N);
nsCR.fill(255); nsCG.fill(232); nsCB.fill(150); // default nylon cream
function ncHexToRgb(hex) {
  const h = String(hex).replace("#", "");
  return [parseInt(h.slice(0, 2), 16) || 0,
          parseInt(h.slice(2, 4), 16) || 0,
          parseInt(h.slice(4, 6), 16) || 0];
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
    const v = (nsX[i] - nsPX[i]) * 0.90;                  // damping → ring
    nsPX[i] = nsX[i];
    nsX[i] = nsX[i] + v + (playheadX - nsX[i]) * 0.020;   // tension home
  }
  for (let pass = 0; pass < 2; pass++) {                   // string stiffness
    for (let i = 1; i < NS_N - 1; i++) {
      nsX[i] = (nsX[i] * 2 + nsX[i - 1] + nsX[i + 1]) * 0.25;
    }
  }
  nsX[0] = nsX[NS_N - 1] = playheadX;
  for (let i = 0; i < NS_N; i++) nsHot[i] *= 0.90;
}
function drawNeedleString() {
  ctx.save();
  ctx.lineCap = "round";
  ctx.lineJoin = "round";
  // 1) sharp dark drop shadow — whole string, hard offset, no blur.
  ctx.strokeStyle = "rgba(0,0,0,0.55)";
  ctx.lineWidth = 5;
  ctx.beginPath();
  ctx.moveTo(nsX[0] + 3, nsY[0] + 4);
  for (let i = 1; i < NS_N; i++) ctx.lineTo(nsX[i] + 3, nsY[i] + 4);
  ctx.stroke();
  // 2) per-segment nylon string: dark casing → warm body → specular.
  for (let i = 1; i < NS_N; i++) {
    const h = Math.max(nsHot[i], nsHot[i - 1]);
    const dev = Math.abs(((nsX[i] + nsX[i - 1]) / 2) - playheadX);
    const act = Math.min(1, Math.max(h, dev / 26));
    const x0 = nsX[i - 1], y0 = nsY[i - 1], x1 = nsX[i], y1 = nsY[i];
    ctx.strokeStyle = `rgba(60,46,32,${(0.10 + act * 0.40).toFixed(3)})`;
    ctx.lineWidth = 4 + act * 4;
    ctx.beginPath(); ctx.moveTo(x0, y0); ctx.lineTo(x1, y1); ctx.stroke();
    // Body tinted by the colour of the waveform that plucked this
    // segment, brightening toward white at the peak of the pluck.
    const cr = (nsCR[i] + nsCR[i - 1]) / 2;
    const cg = (nsCG[i] + nsCG[i - 1]) / 2;
    const cb = (nsCB[i] + nsCB[i - 1]) / 2;
    const wb = 0.16 * act; // keep the waveform's colour dominant
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
      ctx.moveTo(x0 - 1.2, y0); ctx.lineTo(x1 - 1.2, y1);
      ctx.stroke();
    }
  }
  ctx.restore();
}

// ── title setup ──────────────────────────────────────────────────────
const TITLE_PALETTE = [
  "#aef240", "#f54aa6", "#ffffff", "#4ad1bf",
  "#f599c6", "#9aff4d", "#a44af5", "#7fe05a",
];
const titleFontSize = 95; // smaller — chrome lives in a single flush-bottom strip
const titleChars = TITLE.toLowerCase().split("");
const beatHz = BPM / 60;

// Measure each prefix width via magick (the only renderer that
// understands this YWFT TTF correctly).
function magickMeasureWidth(text) {
  const r = spawnSync("magick", [
    "-font", YWFT_PATH,
    "-pointsize", String(titleFontSize),
    `label:${text}`,
    "-format", "%w",
    "info:",
  ], { encoding: "utf8" });
  if (r.status !== 0) return 0;
  return parseInt(r.stdout.trim(), 10) || 0;
}
const prefixWidths = [0];
let cum = "";
for (const ch of titleChars) {
  cum += ch;
  prefixWidths.push(magickMeasureWidth(cum) || prefixWidths[prefixWidths.length - 1] + titleFontSize * 0.45);
}
// Title sits bottom-left, just inside a small inset.
const titleBaseX = 30;

// ── pre-render YWFT title chars via magick → load as Images ─────────
const charBoxW = Math.ceil(titleFontSize * 1.4);
const charBoxH = Math.ceil(titleFontSize * 1.7);
const charBaselineY = Math.ceil(titleFontSize * 1.25);
const charImgs = new Array(titleChars.length).fill(null);
{
  const assetsDir = TRACK.replace(/\.mp3$/, ".assets");
  mkdirSync(assetsDir, { recursive: true });
  for (let i = 0; i < titleChars.length; i++) {
    const ch = titleChars[i];
    if (ch.trim().length === 0) continue;
    const color = TITLE_PALETTE[i % TITLE_PALETTE.length];
    const out = `${assetsDir}/titlechar.${i}.png`;
    const r = spawnSync("magick", [
      "-size", `${charBoxW}x${charBoxH}`,
      "xc:none",
      "-font", YWFT_PATH,
      "-pointsize", String(titleFontSize),
      "-fill", "rgba(0,0,0,0.78)",
      "-annotate", `+8+${charBaselineY + 5}`, ch,
      "-fill", color,
      "-annotate", `+3+${charBaselineY}`, ch,
      out,
    ]);
    if (r.status !== 0) {
      console.error(`✗ magick failed to render title char '${ch}'`);
      process.exit(1);
    }
    charImgs[i] = await loadImage(out);
  }
}

// ── pre-render section labels + lane labels via magick ───────────────
const sectionLabelImgs = new Map();
const sectionFontSize = 32;
for (const sec of struct.sections) {
  const assetsDir = TRACK.replace(/\.mp3$/, ".assets");
  const out = `${assetsDir}/section.${sec.name}.png`;
  const r = spawnSync("magick", [
    "-background", "none",
    "-fill", "rgba(255,255,255,0.85)",
    "-font", YWFT_PATH,
    "-pointsize", String(sectionFontSize),
    `label:${sec.name}`,
    out,
  ]);
  if (r.status === 0) {
    sectionLabelImgs.set(sec.name, await loadImage(out));
  }
}
const laneLabelImgs = new Map();
const laneFontSize = 22;
for (const lane of LANES) {
  const assetsDir = TRACK.replace(/\.mp3$/, ".assets");
  const out = `${assetsDir}/lane.${lane.key}.png`;
  const r = spawnSync("magick", [
    "-background", "rgba(0,0,0,0.75)",
    "-fill", lane.color,
    "-font", YWFT_PATH,
    "-pointsize", String(laneFontSize),
    `label:${lane.label}`,
    "-bordercolor", "rgba(0,0,0,0.75)",
    "-border", "8x4",
    out,
  ]);
  if (r.status === 0) {
    laneLabelImgs.set(lane.key, await loadImage(out));
  }
}

// ── pre-render the timecode like the wanderbeach video ─────────────
// The whole "mm:ss / mm:ss" string is rasterised ONCE per second as a
// near-WHITE YWFT plate plus a matching solid-BLACK plate (the drop
// shadow). At draw time the white plate is recoloured to the current
// section tint (brightened by section progress) and the black plate
// is stamped twice behind it — identical compositing to wanderbeach.
const tcFontSize = titleFontSize; // timecode matches the title size
const tcCache = new Map(); // "mm:ss / mm:ss" → { img, shadow }
{
  const assetsDir = TRACK.replace(/\.mp3$/, ".assets");
  mkdirSync(assetsDir, { recursive: true });
  const renderPlate = async (text, fill, outPath) => {
    const r = spawnSync("magick", [
      "-background", "none",
      "-fill", fill,
      "-font", YWFT_PATH,
      "-pointsize", String(tcFontSize),
      `label:${text}`,
      outPath,
    ]);
    if (r.status !== 0) { console.error(`✗ magick failed on timecode '${text}'`); process.exit(1); }
    return await loadImage(outPath);
  };
  const totMm = Math.floor(duration / 60);
  const totSs = Math.floor(duration - totMm * 60).toString().padStart(2, "0");
  for (let s = 0; s <= Math.ceil(duration); s++) {
    const mm = Math.floor(s / 60);
    const ss = (s - mm * 60).toString().padStart(2, "0");
    const text = `${mm}:${ss} / ${totMm}:${totSs}`;
    if (tcCache.has(text)) continue;
    const safe = text.replace(/[^0-9]/g, "_");
    const img = await renderPlate(text, "rgba(255,253,242,0.97)", `${assetsDir}/tc.${safe}.png`);
    const shadow = await renderPlate(text, "rgba(0,0,0,1)", `${assetsDir}/tc.${safe}.shadow.png`);
    tcCache.set(text, { img, shadow });
  }
}
// Offscreen used to recolour the white timecode plate to the section
// tint via source-in (matches wanderbeach).
const tcTint = createCanvas(8, 8);
const tcTintCtx = tcTint.getContext("2d");
function tintRgb(s) {
  const m = String(s).match(/rgba?\((\d+),\s*(\d+),\s*(\d+)/);
  return m ? [+m[1], +m[2], +m[3]] : [200, 200, 200];
}
// Section tint brightened toward white by how far through the section
// we are — the timecode reports WHICH section and HOW FAR.
function sectionTcRgb(audioT) {
  let s = struct.sections[struct.sections.length - 1];
  for (const sec of struct.sections) {
    if (audioT >= sec.startSec && audioT < sec.endSec) { s = sec; break; }
  }
  let [r, g, b] = tintRgb(SECTION_TINTS[s.name] || "rgba(255,253,242,1)");
  const span = Math.max(0.001, s.endSec - s.startSec);
  const lp = Math.max(0, Math.min(1, (audioT - s.startSec) / span));
  const k = 0.30 * lp;
  r = Math.round(r + (255 - r) * k);
  g = Math.round(g + (255 - g) * k);
  b = Math.round(b + (255 - b) * k);
  return [r, g, b];
}

// ── main canvas + load illustrations ─────────────────────────────────
// We load one image per section if mapped, else the default applies
// to every section. Crossfade between adjacent illustrations over a
// 1.5-second window at each section boundary.
console.log(`▸ loading illustration: ${ILLUSTRATION}`);
const defaultIllus = await loadImage(ILLUSTRATION);
const sectionIllusImgs = new Map(); // section.name → Image
const sectionRects    = new Map(); // section.name → { face, laptop, imgW, imgH }
const defaultRects = loadRectsForIllustration(ILLUSTRATION);
for (const [secName, path] of SECTION_ILLUSTRATIONS) {
  if (path && existsSync(path)) {
    sectionIllusImgs.set(secName, await loadImage(path));
    const r = loadRectsForIllustration(path);
    if (r) sectionRects.set(secName, r);
    console.log(`  · section "${secName}" illustration: ${path}${r?.face ? " (face+laptop bboxes loaded)" : ""}`);
  }
}
function illusForSection(secName) {
  return sectionIllusImgs.get(secName) || defaultIllus;
}
function rectsForSection(secName) {
  return sectionRects.get(secName) || defaultRects;
}
// Optional INTRO PRELUDE — a pre-music "everyone is quiet, laptops
// closed, jeffrey head-down" illustration shown ONLY during the
// opening voice prefix. Swaps to the regular intro at the SNIPER hit
// (the "first gunshot"). Wired via the --prelude flag.
const PRELUDE_PATH = expandHome(flags.prelude);
const preludeImg = PRELUDE_PATH && existsSync(PRELUDE_PATH)
  ? await loadImage(PRELUDE_PATH) : null;
// Sniper event marks the gunshot moment where laptops fly open.
const sniperEv = (struct.events?.sfx || []).find((e) => e.name === "sniper");
const PRELUDE_END_SEC = sniperEv ? sniperEv.t : 2.625;

// ── Doom-style strip melt scaffolding ────────────────────────────────
// Each vertical column gets a random "delay" (0..1). During a
// transition, the OLD illustration's column slides DOWNWARD off the
// canvas, revealing the NEW illustration underneath. Columns with
// smaller delays start moving sooner, larger delays last. Like the
// classic Doom screen-melt.
const MELT_COL_W = 25;            // 25-px wide columns → 60 columns on 1500-wide canvas
const MELT_COLS = Math.ceil(W / MELT_COL_W);
const meltDelays = new Float32Array(MELT_COLS);
{
  let s = 999331;
  const rng = () => { s = (s * 1103515245 + 12345) & 0x7fffffff; return s / 0x7fffffff; };
  for (let i = 0; i < MELT_COLS; i++) meltDelays[i] = rng() * 0.55; // up to 0.55 of melt window delay
}
// Coarse columns for the ALWAYS-ON ambient slitscan (cheaper than the
// fine melt columns — the ambient displacement is only a few px so it
// doesn't need fine resolution). Each gets a random phase so the slit
// shimmers organically rather than as one rolling wave.
const SLIT_COL_W = Math.max(40, Math.round(W / 18));
const SLIT_COLS = Math.ceil(W / SLIT_COL_W);
const slitPhase = new Float32Array(SLIT_COLS);
const slitFreq = new Float32Array(SLIT_COLS);
{
  let s = 20240517;
  const rng = () => { s = (s * 1103515245 + 12345) & 0x7fffffff; return s / 0x7fffffff; };
  for (let i = 0; i < SLIT_COLS; i++) {
    slitPhase[i] = rng() * Math.PI * 2;
    slitFreq[i] = 1.1 + rng() * 1.8;
  }
}

// HSL → "r,g,b" string (0..360 / 0..1 / 0..1). Used so the backlight
// hue can drift liberally over time instead of being locked to the
// section colour.
function hslStr(h, s, l) {
  h = ((h % 360) + 360) % 360;
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
  return `${Math.round((r + m) * 255)},${Math.round((g + m) * 255)},${Math.round((b + m) * 255)}`;
}

// Single reusable offscreen canvas for the OLD image when melting.
const meltCanvas = createCanvas(W, H);
const meltCtx = meltCanvas.getContext("2d");
// A second offscreen for the parallax BACK layer / ambient-slit source.
const backCanvas = createCanvas(W, H);
const backCtx = backCanvas.getContext("2d");

// Reusable offscreen for the shutdown CRT-squeeze pass.
const shutdownCanvas = createCanvas(W, H);
const shutdownCtx = shutdownCanvas.getContext("2d");

// ── STAINED-GLASS BACKLIGHT plumbing ────────────────────────────────
// The lane backlight is rendered as light coming from BEHIND the
// illustration, not a glow stamped on top. To do that we treat each
// illustration as a stained-glass panel: a per-image TRANSMISSION
// MASK says how much light passes through each pixel. Bright / lightly
// pencilled regions transmit (glow with the light colour); dark
// linework + deep shadows act as the leaded came — they block the
// light and stay punchy, so darks keep (even gain) contrast.
//
// The mask is computed ONCE per illustration (8 of them, not per
// frame): luminance → a steep contrast curve crushed hard in the
// shadows. Stored as an opaque-white canvas whose ALPHA is the
// transmission amount, so it can gate the glow via `destination-in`.
const glowCanvas = createCanvas(W, H);
const glowCtx = glowCanvas.getContext("2d");
const _transmissionMasks = new WeakMap(); // Image → mask Canvas
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
  // raised to GAMMA so the dark half is crushed toward zero (the lead
  // lines fully block light → high contrast on darks). A small FLOOR
  // lets a whisper of colour bleed through near-blacks so the panel
  // doesn't read as dead matte.
  const LO = 0.30, HI = 0.92, GAMMA = 1.7, FLOOR = 0.04;
  for (let i = 0; i < px.length; i += 4) {
    const L = (0.2126 * px[i] + 0.7152 * px[i + 1] + 0.0722 * px[i + 2]) / 255;
    let s = (L - LO) / (HI - LO);
    s = s < 0 ? 0 : s > 1 ? 1 : s;
    s = s * s * (3 - 2 * s);          // smoothstep
    const tA = FLOOR + (1 - FLOOR) * Math.pow(s, GAMMA);
    px[i] = 255; px[i + 1] = 255; px[i + 2] = 255;
    px[i + 3] = Math.round(tA * 255);
  }
  tctx.putImageData(id, 0, 0);
  _transmissionMasks.set(img, tmp);
  return tmp;
}

// Detect whether this track has the tape-stop ending — drives the
// video shutdown fade in drawFrame. Only normal-mode tracks have it.
const hasTapeStop = (struct.events?.sfx || []).some((e) => e.name === "tape-stop");

// Boot + shutdown beep events — rendered as FULLSCREEN dinks (radial
// flash + thin ring) on the canvas regardless of whether they're
// in the regular per-lane event loop or skipped (shutdown beeps live
// inside the tape-stop region but their audio still plays at original
// wall-clock time).
const bootBeeps = (struct.events?.sfx || [])
  .filter((e) => /^boot-beep-\d+$/.test(e.name))
  .map((e) => ({ t: e.t, kind: "boot" }));
const shutdownBeeps = (struct.events?.sfx || [])
  .filter((e) => /^shutdown-beep-\d+$/.test(e.name))
  .map((e) => ({ t: e.t, kind: "shutdown" }));
// (shutdownBeeps are visualized by the CRT-collapse shutdown
// sequence at the end of the video, not by a separate ring dink.)
void shutdownBeeps;

// ── kick-driven zoom + alternating face-spot / center aim ───────────
// Each kick pulses a dramatic zoom-in toward an aim point that
// ALTERNATES between a random spot inside the face bbox and the
// raw center of the illustration. Gives the camera a jumpy, beat-
// driven "zoom in / zoom out / zoom in" feel.
const kicksSorted = (struct.events?.kick || []).slice().sort((a, b) => (a.displayedT ?? a.t) - (b.displayedT ?? b.t));
const kickAims = []; // per-kick aim coords [{ fx, fy, isFace }]
{
  let s = 12345;
  const rng = () => { s = (s * 1103515245 + 12345) & 0x7fffffff; return s / 0x7fffffff; };
  for (let i = 0; i < kicksSorted.length; i++) {
    const isFace = (i % 2 === 0); // alternate even=face, odd=center
    if (isFace && faceRect) {
      // Random point INSIDE the face bbox in source-image pixels.
      const fx = faceRect.x + faceRect.w * (0.25 + rng() * 0.50);
      const fy = faceRect.y + faceRect.h * (0.25 + rng() * 0.50);
      kickAims.push({ fx, fy, isFace: true });
    } else {
      // Center of the illustration (in source-image pixels).
      const imgW = faceRect ? faceRect.imgW : defaultIllus.width;
      const imgH = faceRect ? faceRect.imgH : defaultIllus.height;
      kickAims.push({ fx: imgW / 2, fy: imgH / 2, isFace: false });
    }
  }
}

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");

// ── one-frame renderer ───────────────────────────────────────────────
// `t` is the visual-frame time. `audioT` is the audio time currently
// hitting the listener's ears (shifted later by AAC encoder delay).
// All event positioning + envelope readouts use audioT so the
// timeline animation lines up with what's playing.
// Hoisted bottom-chrome constants. Layout (top-to-bottom):
//   …illustration + score-train events fill from y=0 down to chromeTop
//   chromeTop = H - chromeH        ← bottom strip begins
//     title bouncing (left)        ← within strip
//     timecode (right)             ← within strip
//   progressY = H - progressH      ← progress bar flush to the very bottom
const progressH = 22; // matches the wanderbeach progress bar height
const progressY = H - progressH;
// Bottom-chrome strip — holds the bouncing title + the timecode just
// above the progress bar.
const chromeH = 130;            // enough room for ~95-px title bouncing
const chromeTop = H - progressH - chromeH;
// Title is anchored TOP-LEFT now. titleY is the glyph baseline; with
// a charBaselineY-tall box drawn from (y = titleY - charBaselineY)
// this puts the cap height ~24 px below the top edge. The per-char
// bounce is clamped so an upward swing never clips off the top.
const titleTopInset = 24;
const titleY = titleTopInset + charBaselineY;
function drawFrame(t) {
  const audioT = t - AUDIO_DELAY_SEC;
  const env = envAt(audioT);

  // ── compute per-section PX_PER_SEC with smooth crossfade ──────────
  // Lerp from current section's px/sec to the next at section
  // boundaries over a 1.5 s window. Outside the boundary, the value
  // stays at the current section's level.
  let pps = PX_PER_SEC;
  {
    let cur = null, nxt = null;
    for (let i = 0; i < struct.sections.length; i++) {
      if (audioT >= struct.sections[i].startSec && audioT < struct.sections[i].endSec) {
        cur = struct.sections[i];
        nxt = struct.sections[i + 1] || null;
        break;
      }
    }
    if (cur) {
      const curPPS = SECTION_PX_PER_SEC[cur.name] ?? PX_PER_SEC;
      const nxtPPS = nxt ? (SECTION_PX_PER_SEC[nxt.name] ?? PX_PER_SEC) : curPPS;
      const tillNext = nxt ? (cur.endSec - audioT) : 1e9;
      const xfWindow = 1.5;
      if (tillNext < xfWindow) {
        const k = 1 - tillNext / xfWindow; // 0 → 1
        // Smoothstep ease for a gentle squash/stretch transition.
        const e = k * k * (3 - 2 * k);
        pps = curPPS + (nxtPPS - curPPS) * e;
      } else {
        pps = curPPS;
      }
    }
  }

  // ── FULL illustration as background ── face-focused ken-burns ────
  // Per-section illustrations crossfade at boundaries (1.5 s window
  // before each section change). When --illustrations isn't passed,
  // every section uses the same default illustration so no crossfade
  // is visible.
  // Musically-timed Doom-melt transition — centered ON each section
  // boundary, runs for exactly 1 bar (half before / half after).
  // crossfadeT = 0 at start of melt → 1 at end.
  // We HOLD currentSec at the OLD section throughout the melt so the
  // OLD illustration is what falls away. As soon as the melt ends,
  // currentSec advances to the new one normally.
  const beatSecLocal = 60 / BPM;
  const barSecLocal = beatSecLocal * (struct.meter || 3);
  const meltWindow = barSecLocal;
  // The melt RESOLVES exactly on the musical landing — the section
  // boundary, or a dropImpact within ~0.45 bar of it (snap to the
  // drop for sample-tight sync). The whole fall happens across the
  // bar BEFORE the landing, so the NEW illustration "lands" on the
  // drop instead of the old symmetric ±half-bar straddle.
  const DROP_SNAP = barSecLocal * 0.45;
  const anchorFor = (i) => {
    const bnd = struct.sections[i].endSec;
    let a = bnd, best = DROP_SNAP;
    for (const di of dropImpacts) {
      const d = Math.abs(di.t - bnd);
      if (d < best) { best = d; a = di.t; }
    }
    return a;
  };
  let currentSec = struct.sections[0];
  let nextSec = struct.sections[1] || null;
  let crossfadeT = 0;
  let meltAnchor = null; // resolved landing time of the NEXT transition
  // Default section lookup.
  if (audioT >= struct.sections[struct.sections.length - 1].endSec) {
    currentSec = struct.sections[struct.sections.length - 1];
    nextSec = null;
  } else {
    for (let i = 0; i < struct.sections.length; i++) {
      if (audioT >= struct.sections[i].startSec && audioT < struct.sections[i].endSec) {
        currentSec = struct.sections[i];
        nextSec = struct.sections[i + 1] || null;
        break;
      }
    }
  }
  // Soonest upcoming landing → drives the pre-melt slit ramp; and if
  // we're inside its 1-bar melt window, drive the crossfade + hold OLD.
  for (let i = 0; i < struct.sections.length - 1; i++) {
    const a = anchorFor(i);
    if (a <= audioT) continue; // already past this landing
    meltAnchor = a;            // the nearest future landing
    if (audioT >= a - meltWindow) {
      currentSec = struct.sections[i];
      nextSec = struct.sections[i + 1];
      crossfadeT = Math.max(0, Math.min(1, (audioT - (a - meltWindow)) / meltWindow));
    }
    break;
  }
  let illusImg = illusForSection(currentSec.name);
  let nextIllus = nextSec ? illusForSection(nextSec.name) : null;
  // PRELUDE OVERRIDE — show the "head-down, all-laptops-closed" frame
  // from t=0 up to the sniper hit (the "first gunshot"). Then DOOM-melt
  // into the regular intro illustration over 0.6 s, peaking with a
  // white-flash punch right on the sniper.
  let preludeFlash = 0;
  if (preludeImg) {
    const meltDur = 0.6;
    if (audioT < PRELUDE_END_SEC - meltDur * 0.5) {
      illusImg = preludeImg;
      nextIllus = null;
      crossfadeT = 0;
    } else if (audioT < PRELUDE_END_SEC + meltDur * 0.5) {
      // Force a melt with prelude → real intro section.
      illusImg = preludeImg;
      nextIllus = sectionIllusImgs.get("intro") || defaultIllus;
      const localT = (audioT - (PRELUDE_END_SEC - meltDur * 0.5)) / meltDur;
      crossfadeT = Math.max(0, Math.min(1, localT));
      // White-flash punch peaks AT the sniper instant and decays.
      const dt = audioT - PRELUDE_END_SEC;
      if (dt >= -0.05 && dt < 0.35) {
        preludeFlash = Math.max(0, 1 - Math.abs(dt) / 0.35);
      }
    }
  }

  const aspect = illusImg.width / illusImg.height;
  // ── smooth slow camera ──────────────────────────────────────────
  // Cover-fit so portrait or square illustrations always fully cover
  // the canvas. baseScale is the LARGER of width-fit / height-fit, so
  // the illustration overhangs at least one axis. Then a 20 % zoom
  // factor on top gives ken-burns slack.
  const coverScale = Math.max(W / illusImg.width, H / illusImg.height);
  const baseScale = 1.20;
  // Slow zoom oscillation 1.20 → 1.32 across an ~18s cycle.
  const zoomBreath = 0.06 * (0.5 - 0.5 * Math.cos(audioT * (2 * Math.PI / 18)));
  const zoom = baseScale + zoomBreath;
  // No kick pulse — placeholder so later code that referenced it
  // doesn't break.
  const lastKickIdx = -1;
  const lastKickT = -1e9;
  void lastKickT;
  void aspect;
  const baseW = illusImg.width * coverScale * zoom;
  const baseH = illusImg.height * coverScale * zoom;
  // Wobble — frame-by-frame motion at multiple cross-frequencies so
  // the painting reads as alive rather than smoothly drifting.
  const wobbleX = 14 * Math.sin(t * 1.35) + 6 * Math.cos(t * 2.7) + env * 18 * Math.sin(t * 4.0);
  const wobbleY = 11 * Math.cos(t * 1.55) + 5 * Math.sin(t * 2.3) + env * 14 * Math.cos(t * 3.3);
  // Kick-driven aim — interpolate from the previous kick's aim to
  // the next kick's aim across the time between them. So the camera
  // glides between face spots and image center on each kick.
  let centerX = (W - baseW) / 2;
  let centerY = (H - baseH) / 2;
  const imgW = faceRect ? faceRect.imgW : illusImg.width;
  const imgH = faceRect ? faceRect.imgH : illusImg.height;
  let aimFx = imgW / 2;
  let aimFy = imgH / 2;
  if (lastKickIdx >= 0 && kickAims[lastKickIdx]) {
    aimFx = kickAims[lastKickIdx].fx;
    aimFy = kickAims[lastKickIdx].fy;
    // Glide to next kick if it exists.
    if (lastKickIdx + 1 < kicksSorted.length) {
      const nextKickT = kicksSorted[lastKickIdx + 1].displayedT ?? kicksSorted[lastKickIdx + 1].t;
      const span = Math.max(0.001, nextKickT - lastKickT);
      const glide = Math.min(1, Math.max(0, (audioT - lastKickT) / span));
      const next = kickAims[lastKickIdx + 1];
      aimFx = aimFx + (next.fx - aimFx) * glide;
      aimFy = aimFy + (next.fy - aimFy) * glide;
    }
  }
  // Translate aim point to a draw offset that puts the aim under
  // the canvas center.
  const scaledFx = aimFx * (baseW / imgW);
  const scaledFy = aimFy * (baseH / imgH);
  centerX = W / 2 - scaledFx;
  centerY = H / 2 - scaledFy;
  let drawX = centerX + wobbleX;
  let drawY = centerY + wobbleY;
  // Clamp so the illustration always covers the canvas — no black
  // bars on the sides. For a baseW × baseH image to cover W × H, the
  // draw position must satisfy: W - baseW ≤ drawX ≤ 0 (and same for Y).
  drawX = Math.min(0, Math.max(W - baseW, drawX));
  drawY = Math.min(0, Math.max(H - baseH, drawY));
  // ── parallax / depth / slit drivers ────────────────────────────
  // Layer separation gives depth + a tight shake. The slitscan is a
  // MUSIC-DRIVEN horizontal-row tear: near-silent when quiet, growing
  // with amplitude, and exploding into a FINE per-pixel-row scan in
  // the bar before each drop (preMelt).
  const tillAnchor = meltAnchor !== null ? meltAnchor - audioT : Infinity;
  const preMelt = tillAnchor > 0 && tillAnchor < barSecLocal * 1.6
    ? 1 - tillAnchor / (barSecLocal * 1.6) : 0;
  // amplitude term (env²) + a hard pre-drop swell (preMelt²)
  const slitAmp = env * env * 7 + preMelt * preMelt * 72;
  const parScale = 1.055 + env * 0.03;
  const pX = 9 * Math.sin(t * 0.55) + env * 16 * Math.sin(t * 2.6);
  const pY = 7 * Math.cos(t * 0.50) + env * 13 * Math.cos(t * 2.2);
  // Horizontal-row slitscan. Row height shrinks toward 2 px as the
  // drop approaches → "much finer grain individual pixel rows pre
  // transition". Each row's shift rides the BEAT (musical) plus a
  // per-row chaotic comb that tears it apart right before the landing.
  const beatPhase = audioT * beatHz * 2 * Math.PI;
  const paintSlit = (img, dx, dy, dw, dh, amp) => {
    if (amp < 3) { ctx.drawImage(img, dx, dy, dw, dh); return; }
    backCtx.clearRect(0, 0, W, H);
    backCtx.drawImage(img, dx, dy, dw, dh);
    const grain = Math.min(1, preMelt + env * 0.3);
    const rowH = Math.max(2, Math.round(16 - 14 * grain));
    for (let y = 0; y < H; y += rowH) {
      const n = Math.sin(y * 12.9898 + 7.233) * 43758.5453;
      const rnd = n - Math.floor(n); // stable per-row 0..1
      const wave = Math.sin(beatPhase * (0.5 + rnd) + rnd * 6.283);
      const chaos = preMelt * (rnd - 0.5) * 2; // torn rows near the drop
      const off = amp * (wave * 0.6 + chaos);
      ctx.drawImage(backCanvas, 0, y, W, rowH, off, y, W, rowH);
    }
  };
  // Dim parallax BACK plate behind the crisp front — the differential
  // motion (front fixed, back swimming on pX/pY) reads as depth.
  const drawDepth = (img, fx, fy, fw, fh, parMul) => {
    ctx.fillStyle = "#000";
    ctx.fillRect(0, 0, W, H);
    ctx.drawImage(img,
      fx - pX * parMul, fy - pY * parMul, fw * parScale, fh * parScale);
    ctx.fillStyle = "rgba(0,0,0,0.40)";
    ctx.fillRect(0, 0, W, H);
  };

  if (nextIllus && crossfadeT > 0 && nextIllus !== illusImg) {
    // ── DOOM melt WITH two-layer depth ─────────────────────────────
    // NEW is the incoming plane: its own parallax back-plate + a
    // slit-displaced front (so the two layers + slit carry through
    // the whole transition). OLD then melts down in fine columns on
    // top, each column also carrying the ambient slit so the melt
    // grows organically out of it.
    const nextCoverScale = Math.max(W / nextIllus.width, H / nextIllus.height);
    const nextBaseW2 = nextIllus.width * nextCoverScale * zoom;
    const nextBaseH2 = nextIllus.height * nextCoverScale * zoom;
    let nextX = (W - nextBaseW2) / 2 + wobbleX;
    let nextY = (H - nextBaseH2) / 2 + wobbleY;
    nextX = Math.min(0, Math.max(W - nextBaseW2, nextX));
    nextY = Math.min(0, Math.max(H - nextBaseH2, nextY));
    drawDepth(nextIllus, nextX, nextY, nextBaseW2, nextBaseH2, 0.55);
    paintSlit(nextIllus, nextX, nextY, nextBaseW2, nextBaseH2,
              Math.max(slitAmp, 4));
    // OLD column-melt on top.
    meltCtx.clearRect(0, 0, W, H);
    meltCtx.drawImage(illusImg, drawX, drawY, baseW, baseH);
    for (let c = 0; c < MELT_COLS; c++) {
      const colX = c * MELT_COL_W;
      const localT = (crossfadeT - meltDelays[c]) / Math.max(0.01, 1 - meltDelays[c]);
      if (localT >= 1) continue;
      const sc = Math.floor(colX / SLIT_COL_W) % SLIT_COLS;
      const jitter = slitAmp * 0.6 * Math.sin(t * slitFreq[sc] * 1.6 + slitPhase[sc]);
      const fall = Math.max(0, localT) * H * 1.05 + jitter;
      ctx.drawImage(meltCanvas,
        colX, 0, MELT_COL_W, H,
        colX, fall, MELT_COL_W, H);
    }
  } else {
    // No transition — depth back-plate + slit-displaced front.
    drawDepth(illusImg, drawX, drawY, baseW, baseH, 1.0);
    paintSlit(illusImg, drawX, drawY, baseW, baseH, slitAmp);
  }

  // PRELUDE → INTRO punch — a screen-blended white flash that peaks
  // right on the sniper hit, mirrors the "laptops fly open / we here"
  // moment in the audio.
  if (preludeFlash > 0.01) {
    ctx.save();
    ctx.globalCompositeOperation = "screen";
    ctx.globalAlpha = preludeFlash * 0.85;
    ctx.fillStyle = "rgba(255,255,240,1)";
    ctx.fillRect(0, 0, W, H);
    ctx.restore();
  }

  // ── LANE BACKLIGHT — kicks light up the face region; hats light up
  //                    the laptop region. Both render as soft radial
  //                    glows in lane colors, anchored to the current
  //                    section's face/laptop bboxes (transformed from
  //                    image space to live canvas space using the
  //                    ken-burns scale + offset). Each event fades over
  //                    BACKLIGHT_DECAY seconds.
  {
    const rects = rectsForSection(currentSec.name);
    if (rects && (rects.face || rects.laptop)) {
      const sx = baseW / illusImg.width;
      const sy = baseH / illusImg.height;
      // Image-rect → live canvas rect.
      const imgToCanvas = (r) => r ? {
        cx: drawX + (r.x + r.w / 2) * sx,
        cy: drawY + (r.y + r.h / 2) * sy,
        rw: r.w * sx,
        rh: r.h * sy,
      } : null;
      const faceCanvas   = imgToCanvas(rects.face);
      const laptopCanvas = imgToCanvas(rects.laptop);
      // FAST + TIGHT: a very short decay so each hit snaps on and
      // clears before the next, locked hard to event timing.
      const BACKLIGHT_DECAY = 0.12; // seconds
      // Most-recent kick / hat intensity inside the decay window.
      const recentMax = (lane) => {
        let m = 0;
        for (const ev of (struct.events?.[lane] || [])) {
          const evT = ev.displayedT ?? ev.t;
          const since = audioT - evT;
          if (since >= 0 && since < BACKLIGHT_DECAY) {
            const k = 1 - since / BACKLIGHT_DECAY;
            if (k > m) m = k;
          }
        }
        return m;
      };
      // Amplitude-reactive + sensitive: the live envelope gates the
      // flash so loud passages flare and quiet ones barely register,
      // plus a whisper of always-on env shimmer so the panel breathes.
      const ampGate = 0.18 + 1.10 * env;
      const ambient = 0.10 * env;
      const maxK = faceCanvas   ? Math.max(recentMax("kick") * ampGate, ambient) : 0;
      const maxH = laptopCanvas ? Math.max(recentMax("hat")  * ampGate, ambient) : 0;
      // Hue drifts LIBERALLY over time + amplitude — not locked to the
      // section colour. Kick + hat ride different points of the wheel.
      const kickHue = audioT * 23 + env * 90;
      const hatHue  = audioT * 17 + 140 + env * 70;
      const kHi = hslStr(kickHue, 0.95, 0.60), kLo = hslStr(kickHue, 0.95, 0.16);
      const hHi = hslStr(hatHue,  0.95, 0.58), hLo = hslStr(hatHue,  0.95, 0.15);

      if (maxK > 0.008 || maxH > 0.008) {
        // Paint a hot radial into the offscreen glow buffer. Additive
        // (`lighter`) with a near-white core so it reads as an actual
        // emitter, not a tint — overlapping face+laptop glows sum.
        const stamp = (region, rgb, k0) => {
          if (!region || k0 <= 0.008) return;
          // Punchy attack, very tight tail.
          const k = Math.pow(Math.min(1, k0), 0.45);
          const radius = Math.max(region.rw, region.rh) * 1.25;
          const g = glowCtx.createRadialGradient(
            region.cx, region.cy, 0, region.cx, region.cy, radius);
          // hot core → saturated mid → transparent edge (subtler)
          g.addColorStop(0.0, `rgba(255,255,255,${0.80 * k})`);
          g.addColorStop(0.16, `rgba(${rgb},${0.85 * k})`);
          g.addColorStop(0.34, `rgba(${rgb},${0.55 * k})`);
          g.addColorStop(0.66, `rgba(${rgb},${0.26 * k})`);
          g.addColorStop(1.0, `rgba(${rgb},0)`);
          glowCtx.globalCompositeOperation = "lighter";
          glowCtx.fillStyle = g;
          glowCtx.fillRect(
            Math.max(0, region.cx - radius),
            Math.max(0, region.cy - radius),
            Math.min(W, radius * 2),
            Math.min(H, radius * 2));
        };
        const mask = getTransmissionMask(illusImg);
        // ── PASS 1: transmitted light ──────────────────────────────
        // Light that passes THROUGH the panel. Gate the glow by the
        // transmission mask (keep it only where the illustration is
        // light/translucent) then add it to the scene. Dark linework
        // has ~0 transmission so it stays untouched & punchy.
        glowCtx.globalCompositeOperation = "source-over";
        glowCtx.clearRect(0, 0, W, H);
        stamp(faceCanvas,   kHi, maxK);
        stamp(laptopCanvas, hHi, maxH);
        glowCtx.globalCompositeOperation = "destination-in";
        glowCtx.drawImage(mask, drawX, drawY, baseW, baseH);
        ctx.save();
        ctx.globalCompositeOperation = "lighter";
        ctx.globalAlpha = 0.6;
        ctx.drawImage(glowCanvas, 0, 0);
        ctx.restore();
        // ── PASS 2: leaded contrast ────────────────────────────────
        // Where the panel BLOCKS the light (the dark came-lines near
        // the source) deepen it with a saturated multiply so the
        // darks bruise toward the light's hue instead of washing out
        // — stained-glass leading, high contrast retained.
        glowCtx.globalCompositeOperation = "source-over";
        glowCtx.clearRect(0, 0, W, H);
        stamp(faceCanvas,   kLo, maxK);
        stamp(laptopCanvas, hLo, maxH);
        glowCtx.globalCompositeOperation = "destination-out";
        glowCtx.drawImage(mask, drawX, drawY, baseW, baseH);
        ctx.save();
        ctx.globalCompositeOperation = "multiply";
        ctx.globalAlpha = 0.28 * Math.max(maxK, maxH);
        ctx.drawImage(glowCanvas, 0, 0);
        ctx.restore();
      }
    }
  }

  // No dark backdrop — events render directly over the illustration.

  // Section tints — visible only for sections within the viewport.
  // PER-LANE TINTING: in addition to the overall section band tint,
  // we color the BACKGROUND of each lane that is ACTIVE in this
  // section. So a section like break1 (no kick, no sub) leaves
  // those lanes uncolored, while drop1 (kick, hat, sub, bells, lead,
  // piano all active) lights up every lane background. The event
  // blocks then render on top as the foreground.
  // Section dividers — thin dashed verticals at section boundaries.
  // Section text labels REMOVED per user direction; the section
  // colors are conveyed by the per-section illustration crossfade
  // + the colored progress bar at the top of the canvas.
  for (const sec of struct.sections) {
    const x0 = playheadX + (sec.startSec - audioT) * pps;
    if (x0 < 0 || x0 > W) continue;
    ctx.strokeStyle = "rgba(255,255,255,0.35)";
    ctx.lineWidth = 1.5;
    ctx.setLineDash([6, 6]);
    ctx.beginPath();
    ctx.moveTo(x0, 0);
    ctx.lineTo(x0, H);
    ctx.stroke();
    ctx.setLineDash([]);
  }

  // Bar-grid ticks.
  const beatSec = 60 / BPM;
  const barSec = beatSec * (struct.meter || 3);
  const introStart = struct.sections[0]?.startSec ?? 2.7;
  ctx.strokeStyle = "rgba(255,255,255,0.08)";
  ctx.lineWidth = 1;
  for (let bar = 0; bar < struct.totalBars; bar++) {
    const tBar = introStart + bar * barSec;
    const x = playheadX + (tBar - audioT) * pps;
    if (x < 0 || x > W) continue;
    ctx.beginPath();
    ctx.moveTo(x, SCORE_TOP);
    ctx.lineTo(x, SCORE_BOTTOM);
    ctx.stroke();
  }

  // ── per-lane events ────────────────────────────────────────────────
  // Viewport time window: audioT ± (W/2) / pps seconds.
  const viewportPad = W / pps;
  const tMin = audioT - viewportPad;
  const tMax = audioT + viewportPad;
  for (let li = 0; li < LANES.length; li++) {
    const lane = LANES[li];
    const laneY0 = SCORE_TOP + li * laneH;
    // Lane labels removed — the timeline reads as pure colored
    // event lanes overlaid on the illustration with no extra chrome.

    const evs = laneEvents[lane.key];
    for (const ev of evs) {
      if (ev.skipped) continue;          // beyond tape-stop max coverage
      const evT = ev.displayedT ?? ev.t; // tape-stop remap if present
      if (evT > tMax) break;             // sorted → can short-circuit
      const evDur = ev.dur || 0.06;
      if (evT + evDur < tMin) continue;
      // Cap visible width so a long-sustain bell note doesn't render
      // as a 900-pixel slab. Drum hits keep their natural short
      // width; pitched lanes get capped to a tight 0.18 s marker.
      // SFX events show the actual mini-waveform of their underlying
      // audio so their visualization length matches the sample's true
      // duration, capped to keep on-screen footprint reasonable.
      // Cap visual duration so SFX/VOX pills/waveforms never exceed a
      // sane width. Long vocal stems (greeting: 3.7s, booty: 9.8s) at
      // section pps=600 produced pills 2200-5900 px wide — Cairo's
      // wide-pill draw path (gradient + shadow + screen-blend at
      // megapixel scale) corrupts the canvas backing buffer mid-render
      // and the canvas stops updating from that point onward. Capping
      // to 1.5s keeps every visualization within reach of the playhead
      // viewport without poisoning Cairo.
      const SAMPLE_MAX_VIS_DUR = 1.5;
      const visualDur = ev.midi !== undefined
        ? Math.min(evDur, 0.18)
        : Math.min(evDur, SAMPLE_MAX_VIS_DUR);
      const w = Math.max(3, visualDur * pps);
      const x = playheadX + (evT - audioT) * pps;

      // UNIFORM event height across all lanes — drums and pitched
      // events both use ~40% of the lane height, centered vertically
      // (pitched events shift up/down by midi within a narrower
      // band). Keeps the visualization visually consistent: no
      // "big drum slab" vs "tiny pitched note" mismatch.
      const baseNoteH = Math.max(8, Math.floor(laneH * 0.82));
      let y0 = laneY0 + Math.floor((laneH - baseNoteH) / 2);
      let yh = baseNoteH;
      if (ev.midi !== undefined) {
        const t01 = Math.min(1, Math.max(0, (ev.midi - lane.lo) / Math.max(1, lane.hi - lane.lo)));
        // Pitched lanes — same height as drums, but the y center
        // shifts by ±35% of lane height based on midi position so
        // pitch contour reads while heights stay uniform.
        const pitchOffset = Math.floor((t01 - 0.5) * (laneH - baseNoteH));
        y0 = laneY0 + Math.floor((laneH - baseNoteH) / 2) - pitchOffset;
      } else if (lane.maxStackRows > 1) {
        // Stacked drum hits (pre-roll vs in-bar hats etc.) — divide
        // the centered note-band into rows. Cap rendered depth at 3
        // to avoid crushing.
        const visibleRows = Math.min(lane.maxStackRows, 3);
        const rowH = baseNoteH / visibleRows;
        const rowIdx = Math.min(ev.stackRow, visibleRows - 1);
        y0 = laneY0 + Math.floor((laneH - baseNoteH) / 2) + rowIdx * rowH;
        yh = Math.max(4, rowH - 1);
      }

      // Trigger flash — when the event's start is just left of the
      // playhead, FLASH brighter and grow a brief halo. Reads as
      // a visual "the note is firing right now." Longer fade-tail
      // (180 ms) than before so the visual stays alive after the
      // hit, and the halo size scales with the flash energy.
      const sinceTrigger = audioT - evT;
      const flashWindow = 0.18;
      let flash = 0;
      if (sinceTrigger >= 0 && sinceTrigger < flashWindow) {
        flash = 1 - sinceTrigger / flashWindow;
      }
      // Future / upcoming events get dimmed; played events stay
      // saturated but fade slightly as they leave the playhead.
      const isFuture = evT > audioT + 0.02;
      const isPlayed = sinceTrigger >= flashWindow;
      let baseAlpha = 1.0;
      if (isFuture) baseAlpha = 0.55;       // dimmer upcoming, but visible
      else if (isPlayed) baseAlpha = 0.82;  // mostly-saturated played
      // ACTIVE pulse — when flash > 0 the bubble pumps to FULL opacity
      // (and a touch more saturated alpha for the spike) so the
      // playhead-aligned event reads as the loudest moment on screen.
      if (flash > 0) baseAlpha = Math.min(1.0, baseAlpha + flash * 0.4);

      // PLUCK the verlet string the single frame this event crosses
      // the playhead — a horizontal impulse at the event's lane y.
      if (sinceTrigger >= 0 && sinceTrigger < (1 / FPS) + 1e-4) {
        const yc = laneCenterY[lane.key] ?? (SCORE_TOP + SCORE_H / 2);
        const sign = ((Math.floor(evT * 97) + li) & 1) ? 1 : -1;
        const amount = 30 + 52 * Math.min(1, env + 0.2);
        pluckNeedle(yc, amount, sign, ncHexToRgb(lane.color));
      }

      ctx.globalAlpha = baseAlpha;
      ctx.fillStyle = lane.color;
      // SFX / VOX lanes: draw the ACTUAL waveform of the audio at
      // this event's time region. Per visible pixel column, sample
      // the audio peaks in a tiny window and draw a vertical
      // excursion from the lane's center. Gives the timeline a
      // mini-DAW feel for any time-extended sample-driven event
      // (drop impacts, guns, drones, vocals, screams, etc).
      //
      // Events tagged `point: true` are DECORRELATED markers — they
      // don't draw a waveform (the underlying audio is already shown
      // by the span event they sit inside). They render as a small
      // outlined diamond on top so the user can spot the discrete
      // chime/event even though it lives inside a bigger span.
      const isPoint = ev.point === true;
      // BEACH TRACK MODEL — every non-point event is drawn as the
      // waveform of the underlying audio at its time region (no pills).
      const showWaveform = !isPoint && audioRaw.length > 0;
      if (isPoint) {
        const cx = x + w / 2;
        const cy = y0 + yh / 2;
        const size = Math.min(yh / 2 + 2, 14);
        ctx.globalAlpha = baseAlpha;
        ctx.strokeStyle = "#ffffff";
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.moveTo(cx, cy - size);
        ctx.lineTo(cx + size, cy);
        ctx.lineTo(cx, cy + size);
        ctx.lineTo(cx - size, cy);
        ctx.closePath();
        ctx.stroke();
        ctx.fillStyle = lane.color;
        ctx.globalAlpha = baseAlpha * 0.5;
        ctx.fill();
        ctx.globalAlpha = 1.0;
      } else if (showWaveform) {
        // Beach-style: the waveform itself IS the event — a THICK
        // filled envelope, ADDITIVELY blended so when several triggers
        // overlap their waveforms COMBINE (sum + brighten) instead of
        // stacking as separate thin strips. No pill shape.
        const centerY = y0 + yh / 2;
        const halfH = yh / 2 - 1;
        const samplesPerPx = AUDIO_SR / pps;
        const xEnd = Math.min(W, x + w);
        const xStart = Math.max(0, x);
        if (xEnd > xStart) {
          // sample the envelope once, reuse for fill + outline.
          const cols = [];
          for (let px = xStart; px <= xEnd; px += 1) {
            const audioAt = evT + (px - x) / pps;
            const idxC = Math.floor(audioAt * AUDIO_SR);
            const win = Math.max(1, Math.floor(samplesPerPx));
            let maxAmp = 0;
            for (let s = -win; s <= win; s++) {
              const idx = idxC + s;
              if (idx < 0 || idx >= audioRaw.length) continue;
              const a = Math.abs(audioRaw[idx]);
              if (a > maxAmp) maxAmp = a;
            }
            // taller scaling (2.6×) + a floor so it always has body.
            cols.push(Math.min(1, (maxAmp / audioPeak) * 2.6) * 0.92 + 0.06);
          }
          ctx.save();
          ctx.globalCompositeOperation = "lighter"; // overlaps SUM
          // filled mirrored body
          ctx.beginPath();
          ctx.moveTo(xStart, centerY - cols[0] * halfH);
          for (let k = 1; k < cols.length; k++) {
            ctx.lineTo(xStart + k, centerY - cols[k] * halfH);
          }
          for (let k = cols.length - 1; k >= 0; k--) {
            ctx.lineTo(xStart + k, centerY + cols[k] * halfH);
          }
          ctx.closePath();
          ctx.globalAlpha = Math.min(1, baseAlpha * 0.55 + flash * 0.4);
          ctx.fillStyle = lane.color;
          ctx.fill();
          // bright outline so the shape pops
          ctx.globalAlpha = Math.min(1, baseAlpha + flash * 0.5);
          ctx.strokeStyle = lane.color;
          ctx.lineWidth = 1.6 + flash * 2.8;
          ctx.stroke();
          // specular core when freshly fired
          if (flash > 0.05) {
            ctx.globalAlpha = flash * 0.85;
            ctx.strokeStyle = "rgba(255,255,255,0.92)";
            ctx.lineWidth = 1.2;
            ctx.stroke();
          }
          ctx.restore();
        }
      } else {
        // DIMENSIONAL PILL — the body uses a vertical color gradient
        // (lighter at top, darker at bottom) so each capsule reads
        // as a 3D candy. Plus a soft drop shadow below, and a thin
        // highlight stroke just inside the top so the specular pop
        // reads on the curved edge. Screen blend so events glow
        // against the illustration.
        const r = Math.min(yh / 2, w / 2);
        const drawPill = (px, py, pw, ph, pr) => {
          ctx.beginPath();
          ctx.moveTo(px + pr, py);
          ctx.arcTo(px + pw, py, px + pw, py + ph, pr);
          ctx.arcTo(px + pw, py + ph, px, py + ph, pr);
          ctx.arcTo(px, py + ph, px, py, pr);
          ctx.arcTo(px, py, px + pw, py, pr);
        };
        // 1) Drop shadow — soft dark band ~2 px below.
        ctx.save();
        ctx.globalAlpha = baseAlpha * 0.45;
        ctx.shadowBlur = 4;
        ctx.shadowColor = "rgba(0,0,0,0.7)";
        ctx.shadowOffsetY = 2;
        ctx.fillStyle = "rgba(0,0,0,0.0)"; // shadow is the visible part
        ctx.fillStyle = "rgba(0,0,0,0.5)";
        drawPill(x, y0, w, yh, r);
        ctx.fill();
        ctx.restore();
        // 2) Main pill body — vertical gradient, screen blend.
        ctx.save();
        ctx.globalCompositeOperation = "screen";
        const bodyGrad = ctx.createLinearGradient(x, y0, x, y0 + yh);
        // Lighter at top → core color → slightly darker at bottom.
        // Use rgba blends of the lane color with white/black.
        bodyGrad.addColorStop(0,    lane.color + "ff"); // top — full sat
        bodyGrad.addColorStop(0.5,  lane.color + "cc"); // mid — slight pull-down
        bodyGrad.addColorStop(1,    lane.color + "55"); // bottom — darker for depth
        ctx.fillStyle = bodyGrad;
        drawPill(x, y0, w, yh, r);
        ctx.fill();
        ctx.restore();
        // 3) Top inner highlight — thin white arc just inside the top.
        ctx.save();
        ctx.globalAlpha = baseAlpha * 0.55;
        ctx.strokeStyle = "rgba(255,255,255,0.85)";
        ctx.lineWidth = 1.2;
        ctx.beginPath();
        // Draw only the upper half of the pill outline (the top arc).
        const inset = 1.2;
        const ix = x + inset, iy = y0 + inset;
        const iw = w - 2 * inset, ih = yh - 2 * inset;
        const ir = Math.min(ih / 2, iw / 2);
        // Move along the top edge: top-left curve to top-right curve.
        ctx.moveTo(ix, iy + ir);
        ctx.arcTo(ix, iy, ix + ir, iy, ir);
        ctx.lineTo(ix + iw - ir, iy);
        ctx.arcTo(ix + iw, iy, ix + iw, iy + ir, ir);
        ctx.stroke();
        ctx.restore();
      }

      // ACTIVE accent — NO pill. A soft screen-blended vertical glow
      // column over the event's waveform span when it fires, so the
      // hit reads as the waveform lighting up rather than a capsule.
      if (flash > 0.05 && !isPoint) {
        const gx = Math.max(0, x);
        const gw = Math.min(W, x + w) - gx;
        if (gw > 0) {
          ctx.save();
          ctx.globalCompositeOperation = "screen";
          const cx = gx + gw / 2;
          const grad = ctx.createLinearGradient(gx, 0, gx + gw, 0);
          grad.addColorStop(0, lane.color + "00");
          grad.addColorStop(0.5, lane.color + "ff");
          grad.addColorStop(1, lane.color + "00");
          ctx.globalAlpha = flash * 0.6;
          ctx.fillStyle = grad;
          const gy0 = laneY0 + 1;
          ctx.fillRect(gx, gy0, gw, laneH - 2);
          // bright hot core line on the strongest column
          ctx.globalAlpha = flash * 0.8;
          ctx.fillStyle = "rgba(255,255,255,0.9)";
          ctx.fillRect(cx - 1, gy0, 2, laneH - 2);
          ctx.restore();
        }
        ctx.globalAlpha = 1;
      }
      // ── NOTATION POP — particle burst on activation ───────────────
      // For freshly-fired events (flash > 0) spray ~8 small dots
      // outward from the pill's center, fading over a 240 ms window.
      // Deterministic angles seeded by lane index + event time so the
      // burst doesn't shimmer between frames.
      if (flash > 0.05 && !isPoint) {
        const burstAge = (audioT - evT) / 0.24;
        if (burstAge >= 0 && burstAge < 1) {
          const cx = x + w / 2;
          const cy = y0 + yh / 2;
          const NUM = 8;
          const seed = Math.floor(evT * 1000) + li * 31;
          let s = seed >>> 0;
          const rnd = () => { s = (s * 1664525 + 1013904223) >>> 0; return (s & 0xffffff) / 0xffffff; };
          ctx.save();
          ctx.globalCompositeOperation = "screen";
          for (let p = 0; p < NUM; p++) {
            const baseAng = (p / NUM) * Math.PI * 2;
            const jitter = (rnd() - 0.5) * 0.8;
            const ang = baseAng + jitter;
            const reach = 14 + rnd() * 26;     // max distance from center
            const dist = reach * burstAge;
            const px = cx + Math.cos(ang) * dist;
            const py = cy + Math.sin(ang) * dist;
            const radius = Math.max(0.5, 2.8 * (1 - burstAge));
            ctx.globalAlpha = (1 - burstAge) * 0.95;
            ctx.fillStyle = lane.color;
            ctx.beginPath();
            ctx.arc(px, py, radius, 0, Math.PI * 2);
            ctx.fill();
          }
          ctx.restore();
        }
      }
      ctx.globalAlpha = 1.0;
    }
  }

  // ── drop-impact markers — vertical white-on-pink bars ─────────────
  for (const imp of dropImpacts) {
    const impT = imp.displayedT ?? imp.t;
    if (impT < tMin || impT > tMax) continue;
    const x = playheadX + (impT - audioT) * pps;
    ctx.strokeStyle = "rgba(255,255,255,0.9)";
    ctx.lineWidth = 3;
    ctx.beginPath();
    ctx.moveTo(x, SCORE_TOP);
    ctx.lineTo(x, SCORE_BOTTOM);
    ctx.stroke();
    ctx.strokeStyle = "#ff5a8a";
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(x, SCORE_TOP);
    ctx.lineTo(x, SCORE_BOTTOM);
    ctx.stroke();
  }

  // ── COLOR BLASTS — full-canvas flashes on dropImpact + section
  //                   boundaries. SUPPRESSED during the shutdown
  //                   window so the ending is a calm fade-to-black
  //                   without strobing.
  const inShutdown = hasTapeStop && audioT > duration - 5;
  if (!inShutdown) {
    let blastAlpha = 0;
    let blastColor = "rgba(255,255,255,0.6)";
    if (nextSec) {
      const sinceBoundary = audioT - currentSec.endSec;
      if (sinceBoundary >= -0.04 && sinceBoundary < 0.18) {
        const t01 = Math.max(0, sinceBoundary + 0.04) / 0.22;
        blastAlpha = Math.max(blastAlpha, 0.55 * (1 - t01));
        const tint = SECTION_TINTS[nextSec.name] || "rgba(255,255,255,0.5)";
        blastColor = tint.replace(/,([0-9.]+)\)/, ",1)").replace("rgba", "rgba");
      }
    }
    for (const imp of dropImpacts) {
      const impT = imp.displayedT ?? imp.t;
      const dt = audioT - impT;
      if (dt >= 0 && dt < 0.30) {
        const t01 = dt / 0.30;
        const a = 0.65 * (1 - t01);
        if (a > blastAlpha) {
          blastAlpha = a;
          blastColor = "rgba(255,90,160,1)";
        }
      }
    }
    if (blastAlpha > 0.01) {
      ctx.save();
      ctx.globalCompositeOperation = "screen";
      ctx.globalAlpha = blastAlpha;
      ctx.fillStyle = blastColor;
      ctx.fillRect(0, 0, W, H);
      ctx.restore();
    }
  }

  // ── playhead — verlet physics string (ported from wanderbeach) ───
  // Events plucked it as they crossed (above); advance the sim one
  // step then draw the oscillating/settling nylon string.
  stepNeedle();
  drawNeedleString();

  // ── bouncing title — pre-rendered chars composited per frame ─────
  // Bounce amplitude has three components:
  //   • a small per-beat sin (always pulses with rhythm)
  //   • a multiplier tied to envelope so loud passages bounce more
  //   • per-char phase offset so letters bob in a traveling wave
  const baseBounce = 8;
  const envBounce = env * 36;
  for (let i = 0; i < titleChars.length; i++) {
    const img = charImgs[i];
    if (!img) continue;
    const x = titleBaseX + prefixWidths[i] - 3;
    const phase = (i / titleChars.length) * 2 * Math.PI;
    const beatWave = Math.sin(2 * Math.PI * beatHz * audioT + phase);
    let y = titleY - charBaselineY + (baseBounce + envBounce) * beatWave;
    // Hard ceiling: an upward bounce may never clip off the top edge.
    if (y < 2) y = 2;
    ctx.drawImage(img, x, y);
  }

  // ── segmented progress bar — identical to the wanderbeach video ──
  // FULL WIDTH: each section paints a near-black tinted TRACK across
  // its width; the PLAYED portion is the bright saturated tint on top.
  // The LAST section runs all the way to the right edge (sections end
  // a couple seconds before the padded total) so the bar spans the
  // entire display with no short gap.
  {
    const playedX = (W * Math.max(0, audioT)) / duration;
    const lastI = struct.sections.length - 1;
    for (let si = 0; si < struct.sections.length; si++) {
      const sec = struct.sections[si];
      // First section pinned to the LEFT edge, last to the RIGHT edge
      // (sections start/end a little inside the padded total) so the
      // bar spans the entire display with no gaps either side.
      const segX0 = si === 0 ? 0 : (sec.startSec / duration) * W;
      const segX1 = si === lastI ? W : (sec.endSec / duration) * W;
      const [r, g, b] = tintRgb(SECTION_TINTS[sec.name] || "rgba(200,200,200,1)");
      ctx.fillStyle = `rgba(${Math.round(r * 0.16)},${Math.round(g * 0.16)},${Math.round(b * 0.16)},0.85)`;
      ctx.fillRect(segX0, progressY, segX1 - segX0, progressH);
      const filledX1 = Math.min(segX1, playedX);
      if (filledX1 > segX0) {
        ctx.fillStyle = `rgba(${r},${g},${b},0.96)`;
        ctx.fillRect(segX0, progressY, filledX1 - segX0, progressH);
      }
      ctx.fillStyle = "rgba(255,253,242,0.35)";
      ctx.fillRect(segX1 - 1, progressY, 1, progressH);
    }
  }

  // ── timecode — identical compositing to the wanderbeach video ────
  {
    const totMm = Math.floor(duration / 60);
    const totSs = Math.floor(duration - totMm * 60).toString().padStart(2, "0");
    const sec = Math.min(Math.max(0, Math.floor(audioT)), Math.ceil(duration));
    const mm = Math.floor(sec / 60);
    const ss = (sec - mm * 60).toString().padStart(2, "0");
    const entry = tcCache.get(`${mm}:${ss} / ${totMm}:${totSs}`);
    if (entry) {
      const { img, shadow } = entry;
      // bottom-right, above the progress bar; bounces with the audio
      // envelope plus a small breathing wobble.
      const bounce = 16 * env;
      const wobble = 2.2 * Math.sin(audioT * 5.5);
      const x = W - img.width - 32;
      const y = progressY - img.height - 14 - bounce + wobble;
      ctx.save();
      ctx.globalCompositeOperation = "source-over";
      ctx.globalAlpha = 0.95;
      ctx.drawImage(shadow, x + 3, y + 4);
      ctx.drawImage(shadow, x + 2, y + 3);
      ctx.globalAlpha = 1;
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
  }

  // ── boot beeps → CRT TURN-ON / DOOR-OPENING REVEAL ───────────────
  // The 3 boot beeps (C5-E5-G5) snap a centered horizontal slit OPEN
  // in stages: pre-beep-1 the canvas is BLACK, beep-1 cracks it open
  // (33% of H), beep-2 widens it (66%), beep-3 fully reveals (100%).
  // Each step glows bright white at the slit's leading edges as if
  // the doors are sliding apart. After beep-3 the reveal stays fully
  // open for the rest of the song (illustration always visible).
  // Shutdown beeps are NOT wired here — the CRT-collapse sequence at
  // the end already handles the 3-beep door-closing.
  if (bootBeeps.length > 0) {
    const firstT = bootBeeps[0].t;
    const lastT  = bootBeeps[bootBeeps.length - 1].t;
    // Once we're well past the last beep (and the reveal has settled
    // to 100 %), skip the mask entirely — full canvas shows through.
    if (audioT < lastT + 0.18) {
      // Find the most recent beep that has fired.
      let lastBeepIdx = -1;
      for (let i = 0; i < bootBeeps.length; i++) {
        if (audioT >= bootBeeps[i].t) lastBeepIdx = i; else break;
      }
      const stepFracs = bootBeeps.map((_, i) => (i + 1) / bootBeeps.length); // [0.33, 0.66, 1.0]
      let openFrac = 0;
      if (lastBeepIdx < 0) {
        // Pre-beep-1 — fully closed.
        openFrac = 0;
      } else {
        const prevFrac = lastBeepIdx > 0 ? stepFracs[lastBeepIdx - 1] : 0;
        const targetFrac = stepFracs[lastBeepIdx];
        const since = audioT - bootBeeps[lastBeepIdx].t;
        // Snap open in 90 ms with quadratic ease.
        const u = Math.min(1, Math.max(0, since / 0.09));
        const e = u * u * (3 - 2 * u);
        openFrac = prevFrac + (targetFrac - prevFrac) * e;
      }
      const slitH = H * openFrac;
      const slitTop = (H - slitH) / 2;
      const slitBottom = slitTop + slitH;
      // Black out the area outside the slit so the illustration is
      // only revealed inside it.
      ctx.save();
      ctx.fillStyle = "#000";
      ctx.fillRect(0, 0, W, Math.max(0, slitTop));
      ctx.fillRect(0, Math.min(H, slitBottom), W, Math.max(0, H - slitBottom));
      // Bright white glow line at top + bottom edges of the slit —
      // brightest right after each beep, decaying in 200 ms. Reads as
      // the doors physically sliding apart.
      if (lastBeepIdx >= 0 && audioT > firstT) {
        const since = audioT - bootBeeps[lastBeepIdx].t;
        const glowAlpha = Math.max(0, 1 - since / 0.20);
        if (glowAlpha > 0.01 && slitH > 2) {
          ctx.globalAlpha = glowAlpha;
          ctx.fillStyle = "#aef240";
          // Two bright bars (top + bottom) traveling outward.
          ctx.fillRect(0, slitTop - 3, W, 6);
          ctx.fillRect(0, slitBottom - 3, W, 6);
          // Plus a soft white spill BEYOND the slit edges so the
          // outward expansion reads as light leaking.
          ctx.globalAlpha = glowAlpha * 0.35;
          ctx.fillStyle = "rgba(255,255,255,1)";
          const spillH = 30;
          ctx.fillRect(0, slitTop - spillH, W, spillH);
          ctx.fillRect(0, slitBottom, W, spillH);
        }
      }
      ctx.restore();
    }
  }

  // ── shutdown sequence — CRT-style collapse over the tape-stop tail.
  // Starts ~3s before duration ends. Cinematic destroy: scanline
  // sweep, vertical squeeze of the entire frame toward a thin
  // horizontal line, then the line itself fades to a single white
  // dot, then black. Destroys EVERYTHING (illustration, events,
  // title, timecode, progress bar).
  if (hasTapeStop) {
    const shutdownStart = duration - 3.5;
    if (audioT > shutdownStart) {
      const f = Math.min(1, (audioT - shutdownStart) / 3.5);
      ctx.save();
      // Phase 1 (f 0..0.55): scanline glitch + RGB shift starts subtle
      // and intensifies. Implemented as a translucent horizontal line
      // sweep + a few magenta+cyan offset strips.
      if (f < 0.85) {
        const scanFreq = 12 + f * 120; // dense scanlines
        const scanAlpha = 0.15 + f * 0.55;
        ctx.globalAlpha = scanAlpha;
        ctx.fillStyle = "rgba(0,0,0,1)";
        const stripeH = 2;
        const period = Math.max(stripeH * 2, Math.floor(H / scanFreq));
        for (let y = 0; y < H; y += period) {
          ctx.fillRect(0, y, W, stripeH);
        }
        // Bright sweeping scanline that rakes down then up faster as
        // f grows.
        const sweepY = ((audioT * (60 + f * 360)) % H);
        ctx.globalAlpha = 0.6 * (1 - f);
        ctx.fillStyle = "rgba(255,255,255,1)";
        ctx.fillRect(0, sweepY, W, 2);
      }
      ctx.restore();
      // Phase 2 (f 0.55..0.9): vertical CRT-squeeze. Copy what's
      // currently on the canvas (background → bottom strip) and
      // re-draw it scaled vertically toward the center.
      if (f > 0.55) {
        const sq = Math.min(1, (f - 0.55) / 0.30);
        // Squeeze ratio: 1 → 0.05 (almost a line)
        const squeezeR = 1 - sq * 0.95;
        const cy = H / 2;
        const newH = Math.max(2, H * squeezeR);
        const newY = cy - newH / 2;
        // Snapshot what's there, blank, then redraw squeezed.
        const snap = canvas.toBuffer("raw");
        const imageData = shutdownCtx.createImageData(W, H);
        imageData.data.set(snap);
        shutdownCtx.putImageData(imageData, 0, 0);
        ctx.fillStyle = "#000";
        ctx.fillRect(0, 0, W, H);
        ctx.drawImage(shutdownCanvas, 0, 0, W, H, 0, newY, W, newH);
        // Bright horizontal afterglow line at center
        ctx.save();
        ctx.globalAlpha = 0.4 + sq * 0.6;
        ctx.strokeStyle = "rgba(255,255,255,1)";
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.moveTo(0, cy);
        ctx.lineTo(W, cy);
        ctx.stroke();
        ctx.restore();
      }
      // Phase 3 (f 0.9..1.0): center dot fades. Already squeezed to
      // a line — now collapse that line to a tiny dot at center, then
      // fade to black.
      if (f > 0.9) {
        const dot = Math.min(1, (f - 0.9) / 0.10);
        ctx.fillStyle = `rgba(0,0,0,${(0.5 + dot * 0.5).toFixed(3)})`;
        ctx.fillRect(0, 0, W, H);
        const dotR = Math.max(0, 6 * (1 - dot));
        if (dotR > 0) {
          ctx.fillStyle = "rgba(255,255,255,1)";
          ctx.beginPath();
          ctx.arc(W / 2, H / 2, dotR, 0, Math.PI * 2);
          ctx.fill();
        }
      }
    }
  }
}

// ── assets dir for any future intermediate files ─────────────────────
const assetsDir = TRACK.replace(/\.mp3$/, ".assets");
mkdirSync(assetsDir, { recursive: true });

// ── ffmpeg encoder via stdin pipe ────────────────────────────────────
const totalFrames = Math.ceil(duration * FPS);
console.log(`▸ rendering ${OUT}`);
console.log(`  ${totalFrames} frames @ ${FPS}fps · ${W}x${H} canvas · ${LANES.length} lanes`);

// ── PROBE MODE — render a handful of still frames and exit ───────────
// `--probe-frames "5.2,18.7,46.0"` renders just those audio seconds to
// PNGs next to --out (…-probe-<t>.png) and skips ffmpeg entirely. Lets
// the stained-glass / chrome look be judged in seconds, not a 10-min
// full encode.
if (flags["probe-frames"]) {
  const ts = String(flags["probe-frames"]).split(",")
    .map((s) => Number(s.trim())).filter((n) => Number.isFinite(n));
  const base = OUT.replace(/\.mp4$/i, "");
  for (const sec of ts) {
    drawFrame(sec + AUDIO_DELAY_SEC); // undo the in-fn audio offset
    const p = `${base}-probe-${sec.toFixed(2)}.png`;
    writeFileSync(p, canvas.toBuffer("image/png"));
    console.log(`  ▸ probe ${sec.toFixed(2)}s → ${p}`);
  }
  console.log(`✓ probe complete (${ts.length} frame(s)) — no video encoded`);
  process.exit(0);
}

// node-canvas `toBuffer('raw')` emits BGRA in native byte order on
// little-endian systems (Apple silicon, x86_64). Feeding ffmpeg with
// -pix_fmt rgba was swapping the red and blue channels — switching
// to bgra resolves it.
const ff = spawn("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "rawvideo",
  "-pix_fmt", "bgra",
  "-s", `${W}x${H}`,
  "-r", String(FPS),
  "-i", "pipe:0",
  "-i", TRACK,
  "-c:v", "libx264",
  "-pix_fmt", "yuv420p",
  "-preset", "medium",
  "-crf", "20",
  // Re-encode to AAC for MP4 container compatibility (mp3-in-mp4
  // ships but QuickTime + most players drop the audio). The
  // encoder delay is small (~43 ms) and gapless metadata keeps it
  // imperceptible in playback.
  "-c:a", "aac", "-b:a", "192k",
  "-r", String(FPS),
  "-t", String(duration),
  "-shortest",
  OUT,
], { stdio: ["pipe", "inherit", "inherit"] });

ff.on("error", (e) => {
  console.error("ffmpeg spawn error:", e);
  process.exit(1);
});

// Stream frames. Pause-on-backpressure pattern so we don't OOM.
const t0 = Date.now();
let lastLog = t0;
async function streamFrames() {
  for (let f = 0; f < totalFrames; f++) {
    const t = f / FPS;
    drawFrame(t);
    // node-canvas's toBuffer("raw") returns a Buffer that shares
    // memory with the canvas pixel backing store. ffmpeg.stdin.write
    // is async — without an explicit copy the next drawFrame call
    // clobbers buffers still waiting to be consumed by ffmpeg, which
    // surfaces as pixel-identical "frozen" chunks in the output mp4.
    const buf = Buffer.from(canvas.toBuffer("raw"));
    if (!ff.stdin.write(buf)) {
      await new Promise((res) => ff.stdin.once("drain", res));
    }
    const now = Date.now();
    if (now - lastLog > 2000) {
      const pct = ((f / totalFrames) * 100).toFixed(1);
      const fps = (f / ((now - t0) / 1000)).toFixed(1);
      console.log(`  · frame ${f}/${totalFrames} (${pct}%) · ${fps} fps`);
      lastLog = now;
    }
  }
  ff.stdin.end();
}

streamFrames().catch((e) => {
  console.error("frame streaming error:", e);
  ff.kill();
  process.exit(1);
});

ff.on("exit", (code) => {
  if (code !== 0) {
    console.error(`✗ ffmpeg exited with code ${code}`);
    process.exit(1);
  }
  console.log(`✓ ${OUT}`);
});
