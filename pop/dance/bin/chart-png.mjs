#!/usr/bin/env node
// chart-png.mjs — render a fully-tracked multitrack chart of a
// dance-bake's struct.json to PNG. Used to give @jeffrey a visual
// reference for commenting on specific moments in the mix.
//
// Usage:
//   node pop/dance/bin/chart-png.mjs \
//        --struct ~/Documents/Working\ Desktop/twi-out/trancepenta-MASTER-preBright.wav.assets/struct.json \
//        --title trancemarka \
//        --out ~/Desktop/trancemarka-DISTROKID/trancemarka-CHART.png
//
// Sections are pulled from the SECTION_TEMPLATES_5_CHILL layout for
// chill+meter-5 trancepenta/trancemarka. If the struct comes from a
// different track this can be adapted.

import { createCanvas } from "canvas";
import { readFileSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";
import { homedir } from "node:os";

const expandHome = (p) => p?.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p;
const flags = {};
for (let i = 2; i < process.argv.length; i += 2) {
  flags[process.argv[i].replace(/^--/, "")] = process.argv[i + 1];
}
const STRUCT_PATH = expandHome(flags.struct);
const OUT = expandHome(flags.out);
const TITLE = flags.title || "trancemarka";

const s = JSON.parse(readFileSync(STRUCT_PATH, "utf8"));
const events = s.events || {};
const TOTAL = 190.69; // 80 bars × 2.381s @ 5/4 126 BPM

// Lane order + colors (matches cover-video.mjs lane palette).
const LANES = [
  { key: "kick",     label: "KICK",  color: "#ff5a8a" },
  { key: "sub",      label: "SUB",   color: "#a855f7" },
  { key: "hat",      label: "HAT",   color: "#7ad9f5" },
  { key: "bells",    label: "BELLS", color: "#4ad1bf" },
  { key: "lead",     label: "LEAD",  color: "#aef240" },
  { key: "piano",    label: "PIANO", color: "#f5d240" },
  { key: "supersaw", label: "SAW",   color: "#9aff4d" },
  { key: "snare",    label: "SNARE", color: "#e7c4ff" },
  { key: "sfx",      label: "SFX",   color: "#ff9a40" },
  { key: "vox",      label: "VOX",   color: "#f599c6" },
];

// Sections — bars + tints. Match SECTION_TEMPLATES_5_CHILL exactly.
const BAR_SEC = 5 * 60 / 126; // 2.381s
const OPENING_PREFIX_SEC = 0.25;
const SECTIONS = [
  { name: "intro",  bars: 4,  tint: "rgba(78,96,116,0.28)" },
  { name: "break1", bars: 4,  tint: "rgba(86,118,128,0.28)" },
  { name: "build1", bars: 4,  tint: "rgba(70,108,108,0.28)" },
  { name: "drop1",  bars: 24, tint: "rgba(72,104,140,0.32)" },
  { name: "break2", bars: 4,  tint: "rgba(96,90,118,0.28)" },
  { name: "build2", bars: 4,  tint: "rgba(96,108,92,0.28)" },
  { name: "drop2",  bars: 28, tint: "rgba(80,98,128,0.32)" },
  { name: "outro",  bars: 8,  tint: "rgba(92,100,112,0.28)" },
];
let _accBar = 0;
for (const sec of SECTIONS) {
  sec.t0 = OPENING_PREFIX_SEC + _accBar * BAR_SEC;
  _accBar += sec.bars;
  sec.t1 = OPENING_PREFIX_SEC + _accBar * BAR_SEC;
}

// Canvas geometry — high-res for the annotated chart.
const W = 4000;
const H = 2400;
const MARGIN_L = 240;
const MARGIN_R = 120;
const HEADER_H = 360;     // title + section strip
const FOOTER_H = 320;     // time axis + total counts + annotation legend
const LANE_H = Math.floor((H - HEADER_H - FOOTER_H) / LANES.length);
const PLOT_W = W - MARGIN_L - MARGIN_R;
const PLOT_TOP = HEADER_H;
const PLOT_BOT = H - FOOTER_H;

const tToX = (t) => MARGIN_L + (t / TOTAL) * PLOT_W;
const mmss = (t) => {
  const m = Math.floor(t / 60), s = Math.floor(t - m * 60);
  return `${m}:${String(s).padStart(2, "0")}`;
};

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");

// Background.
ctx.fillStyle = "#0a0d12";
ctx.fillRect(0, 0, W, H);

// Section tint columns + names + start times.
for (const sec of SECTIONS) {
  const x0 = tToX(sec.t0), x1 = tToX(sec.t1);
  ctx.fillStyle = sec.tint;
  ctx.fillRect(x0, PLOT_TOP, x1 - x0, PLOT_BOT - PLOT_TOP);
  // Section divider line.
  ctx.strokeStyle = "rgba(255,255,255,0.18)";
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.moveTo(x1, PLOT_TOP);
  ctx.lineTo(x1, PLOT_BOT);
  ctx.stroke();
  // Section header strip.
  ctx.fillStyle = "rgba(255,255,255,0.07)";
  ctx.fillRect(x0, HEADER_H - 100, x1 - x0, 100);
  ctx.fillStyle = "rgba(255,255,255,0.92)";
  ctx.font = "bold 36px -apple-system, sans-serif";
  ctx.textBaseline = "middle";
  ctx.textAlign = "left";
  ctx.fillText(sec.name, x0 + 20, HEADER_H - 64);
  ctx.font = "26px -apple-system, sans-serif";
  ctx.fillStyle = "rgba(255,255,255,0.55)";
  ctx.fillText(mmss(sec.t0), x0 + 20, HEADER_H - 28);
}

// Title block.
ctx.fillStyle = "rgba(255,255,255,0.97)";
ctx.font = "bold 96px -apple-system, sans-serif";
ctx.textAlign = "left";
ctx.fillText(TITLE, MARGIN_L, 110);
ctx.font = "32px -apple-system, sans-serif";
ctx.fillStyle = "rgba(255,255,255,0.55)";
ctx.fillText("Aesthetic Dot Computer · pixsies · 3:11 · 5/4 · 126 BPM · D dorian · −14 LUFS / LRA 5.5 / −1.4 dBTP", MARGIN_L, 170);
// Total event counts on the right side of the title row.
ctx.font = "26px -apple-system, sans-serif";
ctx.fillStyle = "rgba(255,255,255,0.55)";
ctx.textAlign = "right";
const countParts = [];
for (const lane of LANES) {
  const n = (events[lane.key] || []).length;
  if (n > 0) countParts.push(`${lane.label} ${n}`);
}
ctx.fillText(countParts.join("  ·  "), W - MARGIN_R, 170);

// Lane rows.
ctx.textBaseline = "middle";
for (let i = 0; i < LANES.length; i++) {
  const lane = LANES[i];
  const yTop = PLOT_TOP + i * LANE_H;
  const yMid = yTop + LANE_H / 2;
  // Lane background.
  ctx.fillStyle = i % 2 === 0 ? "rgba(255,255,255,0.025)" : "rgba(0,0,0,0.0)";
  ctx.fillRect(MARGIN_L, yTop, PLOT_W, LANE_H);
  // Lane horizontal centerline.
  ctx.strokeStyle = "rgba(255,255,255,0.10)";
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.moveTo(MARGIN_L, yMid);
  ctx.lineTo(MARGIN_L + PLOT_W, yMid);
  ctx.stroke();
  // Lane label.
  ctx.fillStyle = lane.color;
  ctx.font = "bold 38px -apple-system, sans-serif";
  ctx.textAlign = "right";
  ctx.fillText(lane.label, MARGIN_L - 24, yMid - 10);
  // Lane count.
  const evs = events[lane.key] || [];
  ctx.fillStyle = "rgba(255,255,255,0.45)";
  ctx.font = "22px -apple-system, sans-serif";
  ctx.fillText(String(evs.length), MARGIN_L - 24, yMid + 30);
  // Events.
  ctx.fillStyle = lane.color;
  for (const ev of evs) {
    const t = ev.displayedT ?? ev.t;
    if (t < 0 || t > TOTAL) continue;
    const x = tToX(t);
    // Width based on dur (clamped). Most events are short.
    const dur = ev.dur || 0.05;
    const w = Math.max(2, (dur / TOTAL) * PLOT_W);
    ctx.fillRect(x, yMid - LANE_H * 0.32, w, LANE_H * 0.64);
  }
  // Lane top border.
  if (i > 0) {
    ctx.strokeStyle = "rgba(255,255,255,0.08)";
    ctx.beginPath();
    ctx.moveTo(MARGIN_L, yTop);
    ctx.lineTo(MARGIN_L + PLOT_W, yTop);
    ctx.stroke();
  }
}

// Time axis (every 10s).
ctx.textAlign = "center";
ctx.textBaseline = "top";
ctx.fillStyle = "rgba(255,255,255,0.55)";
ctx.font = "24px -apple-system, sans-serif";
for (let t = 0; t <= TOTAL; t += 10) {
  const x = tToX(t);
  ctx.strokeStyle = "rgba(255,255,255,0.12)";
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.moveTo(x, PLOT_BOT);
  ctx.lineTo(x, PLOT_BOT + 14);
  ctx.stroke();
  ctx.fillText(mmss(t), x, PLOT_BOT + 22);
}

// ── ANNOTATED CALLOUTS — notable moments to comment against ──
const ANNOTATIONS = [
  { t:   9.78, label: "first kick · break1 enters",                                 side: "top" },
  { t:  14.77, label: "hats ramp in",                                                side: "top" },
  { t:  19.30, label: "build1 · sub + riser",                                        side: "top" },
  { t:  28.82, label: "★ DROP1 ENTERS · supersaw wall + full kit",                   side: "top" },
  { t:  30.00, label: "scratch-FX window begins (30s → 2:25)",                       side: "bottom" },
  { t:  85.7,  label: "break2 · beat carries through",                               side: "top" },
  { t:  95.30, label: "★ AUDIO STAMP · 'aesthetic dot computer' + arpeggiated neigh", side: "bottom" },
  { t:  96.07, label: "build2 · riser + bitcrush back",                              side: "top" },
  { t: 105.60, label: "★ DROP2 ENTERS · hardest section (drumDensity 1.0, 28 bars)", side: "top" },
  { t: 149.00, label: "scratch silenced here (clean drop2 finale)",                  side: "bottom" },
  { t: 165.00, label: "boathorn (159s · the lone harbour blast)",                    side: "bottom" },
  { t: 172.27, label: "★ OUTRO · perc out, pad + bells fade",                        side: "top" },
];
for (const a of ANNOTATIONS) {
  const x = tToX(a.t);
  const isTop = a.side === "top";
  // Vertical line marker through all lanes.
  ctx.strokeStyle = a.label.startsWith("★") ? "rgba(255,255,255,0.55)" : "rgba(255,255,255,0.30)";
  ctx.lineWidth = a.label.startsWith("★") ? 2.5 : 1.5;
  ctx.setLineDash(a.label.startsWith("★") ? [] : [6, 6]);
  ctx.beginPath();
  ctx.moveTo(x, PLOT_TOP);
  ctx.lineTo(x, PLOT_BOT);
  ctx.stroke();
  ctx.setLineDash([]);
  // Label callout outside the plot region.
  ctx.textBaseline = isTop ? "alphabetic" : "top";
  ctx.textAlign = "left";
  const labelY = isTop ? PLOT_TOP - 14 : PLOT_BOT + 70;
  // Background pill for legibility.
  ctx.font = "24px -apple-system, sans-serif";
  const tw = ctx.measureText(`${mmss(a.t)}  ${a.label}`).width + 24;
  ctx.fillStyle = a.label.startsWith("★") ? "rgba(255,90,138,0.18)" : "rgba(255,255,255,0.10)";
  ctx.fillRect(x - 12, labelY - (isTop ? 32 : 0), tw, 32);
  ctx.fillStyle = a.label.startsWith("★") ? "rgba(255,180,200,0.98)" : "rgba(255,255,255,0.78)";
  ctx.fillText(`${mmss(a.t)}  ${a.label}`, x, labelY - (isTop ? 10 : -22));
}
// Major grid lines at section boundaries (drawn ON TOP of events for clarity).
ctx.strokeStyle = "rgba(255,255,255,0.18)";
ctx.lineWidth = 1.5;
for (const sec of SECTIONS) {
  const x = tToX(sec.t0);
  ctx.beginPath();
  ctx.moveTo(x, PLOT_TOP);
  ctx.lineTo(x, PLOT_BOT);
  ctx.stroke();
}

// Footer: pipeline note + TODOs.
ctx.fillStyle = "rgba(255,255,255,0.55)";
ctx.textAlign = "left";
ctx.textBaseline = "top";
ctx.font = "20px -apple-system, sans-serif";
ctx.fillText("PIPELINE", MARGIN_L, PLOT_BOT + 130);
ctx.fillStyle = "rgba(255,255,255,0.78)";
ctx.font = "20px -apple-system, sans-serif";
ctx.fillText("trance.mjs (engine · --hell 13 --gallop --chill-hats on --beat-in 12)  →  scratch-mix (0:30→2:25, bed-floor 40%)  →  silent vox bus  →  finalize (aecho 4-tap space · radio glue · −14 LUFS LRA 6)",
  MARGIN_L, PLOT_BOT + 158);

ctx.fillStyle = "rgba(255,90,138,0.85)";
ctx.font = "20px -apple-system, sans-serif";
ctx.fillText("OPEN TODOs", MARGIN_L, PLOT_BOT + 206);
ctx.fillStyle = "rgba(255,255,255,0.78)";
ctx.fillText("· kick wider (Haas-style stereo, broader lows)    · hi-hats shorter + sharper (decay envelope tuning in playPercussion 'g')    · reverb-suck on the audio stamp + recover (sidechain in finalize)",
  MARGIN_L, PLOT_BOT + 234);
ctx.fillText("· choral phoneme vocals (jeffrey-pvc, no words): hahahaha / olololololol / rerererere / babobebebebabo / ummy wummy / woo woo — layered at root+3rd+5th+oct with long reverb, sparse placement",
  MARGIN_L, PLOT_BOT + 262);

// Write.
const buf = canvas.toBuffer("image/png");
writeFileSync(OUT, buf);
console.log(`✓ chart → ${OUT} (${buf.length.toLocaleString()} bytes)`);
