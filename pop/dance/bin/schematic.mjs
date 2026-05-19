#!/usr/bin/env node
// schematic.mjs — reusable compositional-analysis animation for trancenwaltzi.
//
// Renders a LABELED schematic timeline MP4 from a trance struct.json + master
// wav: one horizontal lane per instrument/element drawn from real note events,
// section blocks + names, a variable-bpm curve across the top, and a moving
// playhead synced to the muxed master audio.
//
// Lightest path on an 8GB machine: draw ONE static schematic PNG with
// node-canvas, then a single ffmpeg pass overlays a time-expression drawbox
// playhead and muxes the wav. No puppeteer, no per-frame rendering, no
// parallel ffmpeg.
//
// Usage:
//   node pop/dance/bin/schematic.mjs
//     [--struct <path>] [--wav <path>] [--out <path>] [--fps 24]
//
// Re-runnable: with no args it regenerates from the latest default files.

import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { execFileSync } from "node:child_process";
import { createRequire } from "node:module";

const require = createRequire(import.meta.url);
const { createCanvas } = require("canvas");

// ---------------------------------------------------------------------------
// Args + defaults
// ---------------------------------------------------------------------------
const HOME = os.homedir();
const args = process.argv.slice(2);
function arg(name, def) {
  const i = args.indexOf(`--${name}`);
  return i >= 0 && args[i + 1] ? args[i + 1] : def;
}

const STRUCT = arg(
  "struct",
  path.join(
    HOME,
    "Documents/Working Desktop/twi-out/trancenwaltzi-MASTER-preBright.wav.assets/struct.json",
  ),
);
let WAV = arg("wav", path.join(HOME, "Desktop/trancenwaltzi-MASTER.wav"));
const OUT = arg("out", path.join(HOME, "Desktop/trancenwaltzi-schematic.mp4"));
const FPS = parseInt(arg("fps", "24"), 10);

if (!fs.existsSync(WAV)) {
  const alt = path.join(
    HOME,
    "Documents/Working Desktop/twi-out/trancenwaltzi-MASTER.wav",
  );
  if (fs.existsSync(alt)) WAV = alt;
}
for (const [label, p] of [
  ["struct.json", STRUCT],
  ["master wav", WAV],
]) {
  if (!fs.existsSync(p)) {
    console.error(`[schematic] missing ${label}: ${p}`);
    process.exit(1);
  }
}

const s = JSON.parse(fs.readFileSync(STRUCT, "utf8"));
const events = s.events || {};
const sections = s.sections || [];
const meter = s.meter || 3;
const TOTAL = Math.max(
  s.totalSec || 0,
  ...Object.values(events).flatMap((arr) =>
    Array.isArray(arr) ? arr.map((e) => (e.t || 0) + (e.dur || 0)) : [0],
  ),
);

// ---------------------------------------------------------------------------
// Lane definitions — friendly names + which event stream feeds each.
// ---------------------------------------------------------------------------
const LANES = [
  { id: "kick", label: "kick", color: "#ff5470", kind: "tick" },
  { id: "hat", label: "hat", color: "#7bdff2", kind: "tick" },
  { id: "sub", label: "sub", color: "#b388ff", kind: "bar", lo: 28, hi: 48 },
  { id: "pad", label: "pad", color: "#9d8cff", kind: "section" },
  {
    id: "lead",
    label: "lead / bunny-bow",
    color: "#ffd166",
    kind: "bar",
    lo: 40,
    hi: 108,
  },
  { id: "piano", label: "piano", color: "#06d6a0", kind: "bar", lo: 60, hi: 86 },
  {
    id: "bells",
    label: "bells / gong",
    color: "#ffadad",
    kind: "bar",
    lo: 30,
    hi: 90,
  },
  {
    id: "supersaw",
    label: "supersaw / mosquito",
    color: "#4ea8de",
    kind: "bar",
    lo: 60,
    hi: 98,
  },
  {
    id: "sfx",
    label: "sfx / whistle / cat / impact",
    color: "#f4a261",
    kind: "label",
  },
];

function sectionActive(layerId) {
  const spans = [];
  for (const sec of sections) {
    if (sec.layers && sec.layers[layerId]) spans.push([sec.startSec, sec.endSec]);
  }
  return spans;
}

// ---------------------------------------------------------------------------
// Layout
// ---------------------------------------------------------------------------
const W = 1280;
const H = 540;
const PAD_L = 188; // label gutter
const PAD_R = 28;
const PLOT_W = W - PAD_L - PAD_R;
const BPM_TOP = 58;
const BPM_H = 54;
const LANE_TOP = BPM_TOP + BPM_H + 30;
const AXIS_H = 26;
const LANE_AREA = H - LANE_TOP - AXIS_H - 14;
const LANE_H = LANE_AREA / LANES.length;

const X = (sec) => PAD_L + (sec / TOTAL) * PLOT_W;

// ---------------------------------------------------------------------------
// Variable-bpm curve (chill tempo drifts)
// ---------------------------------------------------------------------------
const bpmPts = [];
for (const sec of sections) {
  const bars = sec.endBar - sec.startBar;
  const beats = bars * meter;
  const durMin = (sec.endSec - sec.startSec) / 60;
  const bpm = durMin > 0 ? beats / durMin : 0;
  bpmPts.push({ s0: sec.startSec, s1: sec.endSec, bpm });
}
const bpmVals = bpmPts.map((p) => p.bpm).filter((v) => v > 0);
const bpmMin = Math.min(...bpmVals) * 0.92;
const bpmMax = Math.max(...bpmVals) * 1.05;
const bpmY = (b) => BPM_TOP + BPM_H - ((b - bpmMin) / (bpmMax - bpmMin)) * BPM_H;

// ---------------------------------------------------------------------------
// Draw the static schematic with node-canvas
// ---------------------------------------------------------------------------
const canvas = createCanvas(W, H);
const c = canvas.getContext("2d");

c.fillStyle = "#0b0d12";
c.fillRect(0, 0, W, H);

// Title
c.fillStyle = "#eaf0ff";
c.font = "bold 17px Helvetica, Arial, sans-serif";
c.textAlign = "left";
c.fillText("trancenwaltzi — compositional schematic", PAD_L, 24);
c.fillStyle = "#5a647a";
c.font = "12px Helvetica, Arial, sans-serif";
c.textAlign = "right";
c.fillText(
  `${s.bpm} base bpm · ${meter}/4 · ${s.scale || ""} · ${
    s.totalBars || "?"
  } bars · ${TOTAL.toFixed(1)}s`,
  W - PAD_R,
  24,
);
c.textAlign = "left";

// legend: what the lane fills mean (light section tint vs solid event marks)
c.fillStyle = "#5a647a";
c.font = "10px Helvetica, Arial, sans-serif";
c.fillText(
  "lanes: faint band = layer active in section · bright marks = actual notes/onsets · white line = audio playhead",
  PAD_L,
  38,
);

// --- Section blocks ---
const SECT_COLORS = {
  intro: "#161a24",
  break1: "#1a2230",
  build1: "#202a3a",
  drop1: "#2a2030",
  break2: "#1a2230",
  drop2: "#2a2030",
  outro: "#161a24",
};
for (const sec of sections) {
  const xa = X(sec.startSec);
  const xb = X(sec.endSec);
  c.fillStyle = SECT_COLORS[sec.name] || "#181c26";
  c.fillRect(xa, LANE_TOP, xb - xa, LANE_AREA);

  // dashed boundary
  c.save();
  c.strokeStyle = "#3a4358";
  c.lineWidth = 1;
  c.setLineDash([2, 3]);
  c.beginPath();
  c.moveTo(xa, BPM_TOP - 6);
  c.lineTo(xa, LANE_TOP + LANE_AREA);
  c.stroke();
  c.restore();

  // name pill
  c.fillStyle = "#222a3a";
  const pw = Math.max(48, Math.min(xb - xa, 88));
  c.fillRect(xa, LANE_TOP - 19, pw, 16);
  c.fillStyle = "#cdd6ec";
  c.font = "bold 11px Helvetica, Arial, sans-serif";
  c.fillText(sec.name, xa + 5, LANE_TOP - 7);
}

// --- BPM curve ---
c.fillStyle = "#7a849a";
c.font = "11px Helvetica, Arial, sans-serif";
c.fillText("eff. bpm (tempo drifts)", 6, BPM_TOP - 8);
c.fillStyle = "#5a647a";
c.font = "9px Helvetica, Arial, sans-serif";
c.fillText(bpmMax.toFixed(0), 6, BPM_TOP + 6);
c.fillText(bpmMin.toFixed(0), 6, BPM_TOP + BPM_H);
c.strokeStyle = "#222a3a";
c.lineWidth = 1;
c.strokeRect(PAD_L, BPM_TOP, PLOT_W, BPM_H);

c.strokeStyle = "#7bdff2";
c.lineWidth = 2;
c.beginPath();
let started = false;
for (const p of bpmPts) {
  const y = bpmY(p.bpm);
  if (!started) {
    c.moveTo(X(p.s0), y);
    started = true;
  } else {
    c.lineTo(X(p.s0), y);
  }
  c.lineTo(X(p.s1), y);
}
c.stroke();
c.fillStyle = "#9fb0d0";
c.font = "9px Helvetica, Arial, sans-serif";
c.textAlign = "center";
for (const p of bpmPts) {
  c.fillText(p.bpm.toFixed(0), (X(p.s0) + X(p.s1)) / 2, bpmY(p.bpm) - 4);
}
c.textAlign = "left";

// --- Lanes ---
function hexA(hex, a) {
  const r = parseInt(hex.slice(1, 3), 16);
  const g = parseInt(hex.slice(3, 5), 16);
  const b = parseInt(hex.slice(5, 7), 16);
  return `rgba(${r},${g},${b},${a})`;
}

LANES.forEach((lane, i) => {
  const ly = LANE_TOP + i * LANE_H;
  const cy = ly + LANE_H / 2;

  c.fillStyle = i % 2 ? "rgba(16,19,28,0.55)" : "rgba(13,16,24,0.55)";
  c.fillRect(PAD_L, ly, PLOT_W, LANE_H);

  // label gutter
  c.fillStyle = "#0b0d12";
  c.fillRect(0, ly, PAD_L - 6, LANE_H);
  c.fillStyle = lane.color;
  c.fillRect(8, cy - 5, 10, 10);
  c.font = "bold 12px Helvetica, Arial, sans-serif";
  c.fillText(lane.label, 26, cy + 4);

  // section-activity underlay from layers
  const layerId = lane.id === "sfx" ? null : lane.id;
  if (layerId) {
    c.fillStyle = hexA(lane.color, 0.1);
    for (const [a, b] of sectionActive(layerId)) {
      c.fillRect(X(a), ly + 2, X(b) - X(a), LANE_H - 4);
    }
  }

  const evs = events[lane.id] || [];

  if (lane.kind === "section") {
    c.fillStyle = hexA(lane.color, 0.45);
    for (const [a, b] of sectionActive(lane.id)) {
      c.fillRect(X(a), cy - 6, X(b) - X(a), 12);
    }
  } else if (lane.kind === "tick") {
    const h = lane.id === "kick" ? LANE_H - 8 : LANE_H - 16;
    c.fillStyle = hexA(lane.color, 0.8);
    for (const e of evs) c.fillRect(X(e.t), cy - h / 2, 1.1, h);
  } else if (lane.kind === "bar") {
    const lo = lane.lo,
      hi = lane.hi;
    const band = LANE_H - 10;
    c.fillStyle = hexA(lane.color, 0.85);
    for (const e of evs) {
      const m = e.midi == null ? (lo + hi) / 2 : e.midi;
      const ny = ly + 5 + (1 - (m - lo) / (hi - lo)) * band;
      const w = Math.max(0.8, X(e.t + (e.dur || 0.05)) - X(e.t));
      c.fillRect(X(e.t), ny, w, 2.2);
    }
  } else if (lane.kind === "label") {
    const sfx = [...(events.sfx || []), ...(events.dropImpact || [])];
    for (const e of sfx) {
      const isImpact = (e.name || "").includes("drop") || e.name === undefined;
      const col = isImpact ? "#ff5470" : lane.color;
      c.fillStyle = hexA(col, 0.85);
      c.fillRect(
        X(e.t),
        cy - 7,
        Math.max(2, X(e.t + (e.dur || 0.15)) - X(e.t)),
        14,
      );
      c.fillStyle = col;
      c.font = "8px Helvetica, Arial, sans-serif";
      const tag = (e.name || "impact").replace(/^chill-drop-/, "v ");
      c.fillText(tag, X(e.t) + 3, cy - 9);
    }
  }
});

// --- Time axis ---
const axisY = LANE_TOP + LANE_AREA + 4;
c.strokeStyle = "#3a4358";
c.lineWidth = 1;
c.beginPath();
c.moveTo(PAD_L, axisY);
c.lineTo(PAD_L + PLOT_W, axisY);
c.stroke();
const step = TOTAL > 120 ? 15 : 10;
c.fillStyle = "#6a748a";
c.font = "10px Helvetica, Arial, sans-serif";
c.textAlign = "center";
for (let t = 0; t <= TOTAL + 0.001; t += step) {
  const xt = X(t);
  c.strokeStyle = "#3a4358";
  c.beginPath();
  c.moveTo(xt, axisY);
  c.lineTo(xt, axisY + 5);
  c.stroke();
  c.fillText(`${t}s`, xt, axisY + 18);
}
c.textAlign = "left";

// ---------------------------------------------------------------------------
// PNG -> MP4 (moving playhead via ffmpeg `overlay` time-expression)
// ---------------------------------------------------------------------------
// Why overlay and not drawbox: in this ffmpeg build (8.1) drawbox's x/y/w/h
// expressions are evaluated ONCE at filter-init (t=0), so a "swept" drawbox
// playhead never actually moves — it stays frozen at its t=0 column. The
// `overlay` filter, by contrast, re-evaluates its x/y expressions every
// frame and exposes the running timestamp `t`, so a small bar PNG composited
// with x='(W-w)*t/DURATION' is guaranteed to glide left→right in lock-step
// with the muxed audio. Falls back to a node-canvas PNG sequence only if the
// overlay-expression path fails.
const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), "twi-schematic-"));
const pngPath = path.join(tmpDir, "schematic.png");
fs.writeFileSync(pngPath, canvas.toBuffer("image/png"));

const phTop = BPM_TOP - 6;
const phBot = LANE_TOP + LANE_AREA;
const phH = Math.round(phBot - phTop);
const PH_W = 6; // playhead bar width incl. soft halo

// Build the playhead bar as its own PNG (bright 2px core + faint halo).
const barCanvas = createCanvas(PH_W, phH);
const bc = barCanvas.getContext("2d");
bc.clearRect(0, 0, PH_W, phH);
bc.fillStyle = "rgba(123,223,242,0.30)"; // halo
bc.fillRect(0, 0, PH_W, phH);
bc.fillStyle = "rgba(255,255,255,0.95)"; // bright core
bc.fillRect(PH_W / 2 - 1, 0, 2, phH);
const barPath = path.join(tmpDir, "playhead.png");
fs.writeFileSync(barPath, barCanvas.toBuffer("image/png"));

// Sweep the bar across the plot region [PAD_L, PAD_L+PLOT_W] over TOTAL secs.
// overlay re-evaluates x each frame; `t` is the output timestamp in seconds.
const sweepEnd = PAD_L + PLOT_W - PH_W / 2;
const overlayX = `${(PAD_L - PH_W / 2).toFixed(2)}+(${(
  sweepEnd -
  (PAD_L - PH_W / 2)
).toFixed(2)})*min(t\\,${TOTAL.toFixed(4)})/${TOTAL.toFixed(4)}`;

const filterComplex =
  `[1:v]format=rgba[bar];` +
  `[0:v][bar]overlay=x='${overlayX}':y=${phTop}:format=auto:eval=frame[v]`;

function encode(fc) {
  execFileSync(
    "ffmpeg",
    [
      "-y",
      "-loglevel",
      "error",
      "-loop",
      "1",
      "-framerate",
      String(FPS),
      "-i",
      pngPath,
      "-loop",
      "1",
      "-framerate",
      String(FPS),
      "-i",
      barPath,
      "-i",
      WAV,
      "-filter_complex",
      fc,
      "-map",
      "[v]",
      "-map",
      "2:a",
      "-t",
      TOTAL.toFixed(3),
      "-r",
      String(FPS),
      "-c:v",
      "libx264",
      "-pix_fmt",
      "yuv420p",
      "-preset",
      "veryfast",
      "-crf",
      "22",
      "-c:a",
      "aac",
      "-b:a",
      "192k",
      "-shortest",
      OUT,
    ],
    { stdio: "inherit" },
  );
}

console.log("[schematic] encoding MP4 with overlay playhead + audio …");
let usedFallback = false;
try {
  encode(filterComplex);
} catch (err) {
  // Fallback: render a per-frame PNG sequence with node-canvas (the playhead
  // baked into each frame) then image2 -> mux. Heavier but bulletproof.
  console.warn(
    "[schematic] overlay path failed, falling back to PNG sequence:",
    err?.message || err,
  );
  usedFallback = true;
  const baseImg = canvas; // already-drawn static schematic
  const nFrames = Math.ceil(TOTAL * FPS);
  const seqDir = path.join(tmpDir, "seq");
  fs.mkdirSync(seqDir);
  const fc2 = createCanvas(W, H);
  const fctx = fc2.getContext("2d");
  for (let f = 0; f < nFrames; f++) {
    const t = f / FPS;
    fctx.clearRect(0, 0, W, H);
    fctx.drawImage(baseImg, 0, 0);
    const px = PAD_L + (Math.min(t, TOTAL) / TOTAL) * PLOT_W;
    fctx.fillStyle = "rgba(123,223,242,0.30)";
    fctx.fillRect(px - 3, phTop, 6, phH);
    fctx.fillStyle = "rgba(255,255,255,0.95)";
    fctx.fillRect(px - 1, phTop, 2, phH);
    fs.writeFileSync(
      path.join(seqDir, `f${String(f).padStart(6, "0")}.png`),
      fc2.toBuffer("image/png"),
    );
  }
  execFileSync(
    "ffmpeg",
    [
      "-y",
      "-loglevel",
      "error",
      "-framerate",
      String(FPS),
      "-i",
      path.join(seqDir, "f%06d.png"),
      "-i",
      WAV,
      "-t",
      TOTAL.toFixed(3),
      "-r",
      String(FPS),
      "-c:v",
      "libx264",
      "-pix_fmt",
      "yuv420p",
      "-preset",
      "veryfast",
      "-crf",
      "22",
      "-c:a",
      "aac",
      "-b:a",
      "192k",
      "-shortest",
      OUT,
    ],
    { stdio: "inherit" },
  );
}

try {
  fs.rmSync(tmpDir, { recursive: true, force: true });
} catch {}

const sz = (fs.statSync(OUT).size / 1e6).toFixed(1);
console.log(
  `[schematic] done -> ${OUT} (${sz} MB, ${TOTAL.toFixed(1)}s @ ${FPS}fps, ` +
    `playhead via ${usedFallback ? "PNG-sequence fallback" : "overlay-expression"})`,
);
