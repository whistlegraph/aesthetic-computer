#!/usr/bin/env node
// gen-score-fluttabap360.mjs — graphic score for FLUTTABAP360. A landscape
// timeline that lets you SEE the whole 6:00 form at a glance: the section map,
// the C→D→E→(home C)→F key-change JOURNEY, the intensity arc across four
// escalating passes, the hush + cave dips, the euphoric final lift, and a
// piano-roll density band per voice lane.
//
// READ-ONLY of the engine: section times + per-note events come from
//   pop/marimba/out/fluttabap360.struct.json
// (emitted by render-fluttabap360.mjs). lvl/ride per section are mirrored from
// that script's ARRANGEMENT array — keep in sync if the form changes.
//
// Output: pop/marimba/out/fluttabap360-score.png + a copy on the Neo ~/Desktop.
// Usage:  node pop/marimba/bin/gen-score-fluttabap360.mjs

import { writeFileSync, mkdirSync, copyFileSync, readFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { createCanvas, registerFont } from "canvas";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "out");
const REPO = resolve(HERE, "..", "..", "..");
mkdirSync(OUT, { recursive: true });

// ── type: Berkeley Mono for labels/numbers, YWFT for display if present ────
// Berkeley Mono for labels/numbers; Georgia for the display (YWFT webfonts
// don't decode through node-canvas — they render as tofu, so we don't use them).
let MONO = "monospace";
const DISP = "Georgia, serif";
try {
  const bm = resolve(REPO, "bills/invoices/BerkeleyMonoVariable-Regular.ttf");
  if (existsSync(bm)) { registerFont(bm, { family: "Berkeley Mono" }); MONO = "Berkeley Mono"; }
} catch {}

// ── data ───────────────────────────────────────────────────────────────
const struct = JSON.parse(readFileSync(resolve(OUT, "fluttabap360.struct.json"), "utf8"));
const { bpm, totalSec, sections, events } = struct;
const TOTAL = totalSec;                                       // 360.000
const fmt = (s) => `${Math.floor(s / 60)}:${String(Math.round(s % 60)).padStart(2, "0")}`;

// lvl / ride mirrored from render-fluttabap360.mjs ARRANGEMENT, in section order.
// (undefined lvl ⇒ inherits the previous section's groove; we carry it forward.)
const ARR = [
  { lvl: undefined }, // 0 intro
  { lvl: 0 }, { lvl: 1 }, { lvl: undefined }, { lvl: 1 }, { lvl: 2 }, { lvl: 2 }, // A: butterfly..ride
  { lvl: 1, ride: 1 }, { lvl: 1, ride: 1 }, { lvl: undefined }, { lvl: 2 }, { lvl: 3 }, { lvl: 3 }, { lvl: undefined }, // B + cave
  { lvl: 2, ride: 1 }, { lvl: 2, ride: 1 }, { lvl: undefined }, { lvl: 2 }, { lvl: 3 }, { lvl: 3 }, // C
  { lvl: 2, ride: 1 }, { lvl: 2, ride: 1 }, { lvl: 2 }, { lvl: 3 }, { lvl: 3 }, { lvl: undefined }, // D + cave(home)
  { lvl: 2, ride: 1 }, { lvl: 2, ride: 1 }, { lvl: undefined }, { lvl: 2 }, { lvl: 3 }, // final chorus F
  { lvl: undefined }, { lvl: undefined }, { lvl: undefined }, // progression / land / button
];
// carry-forward lvl + tag intensity per section (intro builds; cave/mommywow dip)
let carry = 0;
const sec = sections.map((s, i) => {
  const a = ARR[i] || {};
  let lvl = a.lvl;
  if (lvl == null) lvl = carry; else carry = lvl;
  // beat-specific intensity overrides for the "feel" arc
  let intensity = (lvl + 1) / 4;                             // groove → 0..1
  if (s.name === "intro") intensity = 0.10;                  // slow dawn
  if (s.name === "mommywow") intensity = 0.18;               // held hush
  if (s.name === "cave") intensity = 0.30;                   // dub breakdown dip
  if (s.name === "progression") intensity = 0.85;
  if (s.name === "land") intensity = 0.92;
  if (s.name === "button") intensity = 0.55;                 // buttoned finish
  return { ...s, lvl, ride: !!a.ride, intensity };
});

// pass boundaries (by key change) — for the JOURNEY ribbon labels
const KEY_NAMES = { 0: "C", 2: "D", 4: "E", 5: "F" };
const KEY_FULL = { 0: "C major", 2: "D major", 4: "E major", 5: "F major" };

// ── canvas ─────────────────────────────────────────────────────────────
const W = 3400, H = 2100;
const ML = 360, MR = 80, MT = 470, MB = 150;
const PW = W - ML - MR, PH = H - MT - MB;
const cv = createCanvas(W, H);
const g = cv.getContext("2d");

const CREAM = "#FBF8F0", INK = "#181410", SUB = "#6A6258", FAINT = "#9A9286";
const x = (t) => ML + (t / TOTAL) * PW;

// background — warm cream w/ a faint vertical gradient
const bg = g.createLinearGradient(0, 0, 0, H);
bg.addColorStop(0, "#FCFAF3"); bg.addColorStop(1, "#F4EFE3");
g.fillStyle = bg; g.fillRect(0, 0, W, H);

// ── section tints — one hue per beat name, so repeats read as the same idea ─
const TINT = {
  intro:       "#DCE9EC", // misty water-blue
  butterfly:   "#F3DCE6", // pink
  palofmine:   "#E7DCF1", // lilac
  mommywow:    "#EDE7D8", // hush parchment
  slinky:      "#DCEAD9", // mint
  fly:         "#FBE6CF", // warm amber
  ride:        "#F8D9C7", // squeak coral
  cave:        "#CFD3DA", // dub slate
  progression: "#E2DCEE", // resolving lilac
  land:        "#D7E7D2", // green landing
  button:      "#E9E2D2", // closed
};
const TINT_DK = {
  intro: "#5E8C97", butterfly: "#B85C82", palofmine: "#7E5CA8", mommywow: "#9A8E66",
  slinky: "#5C9658", fly: "#C88436", ride: "#C75B3A", cave: "#5A6472",
  progression: "#7C5CA8", land: "#4F8C46", button: "#8A7C5E",
};

// ════════════════════════════════════════════════════════════════════════
//  TITLE BLOCK
// ════════════════════════════════════════════════════════════════════════
g.fillStyle = INK;
g.font = `700 124px ${DISP}`;
g.fillText("fluttabap360", ML, 150);
g.fillStyle = SUB;
g.font = `400 40px ${MONO}`;
g.fillText("graphic score", ML + 4, 205);

// stat strip (right-aligned, mono)
g.textAlign = "right";
g.font = `400 38px ${MONO}`;
g.fillStyle = INK;
const stats = [
  `${fmt(TOTAL)}  ·  ${TOTAL.toFixed(3)} s  (hard-clamped)`,
  `${bpm.toFixed(1)} BPM  ·  4/4  ·  34 sections`,
  `1223 synthesized note events  ·  5 voices`,
];
stats.forEach((s, i) => g.fillText(s, W - MR, 84 + i * 48));
g.textAlign = "left";

// one-line thesis
g.fillStyle = SUB;
g.font = `400 34px ${DISP}`;
g.fillText("a six-minute through-composed pop form: reusable melodic blocks arranged over four escalating passes, climbing a key-change journey",
  ML, 270);

// ════════════════════════════════════════════════════════════════════════
//  KEY-JOURNEY RIBBON — a stepped staircase C→D→E→(home C)→F
//  drawn just above the plot, spanning the same time axis.
// ════════════════════════════════════════════════════════════════════════
const RIB_Y = 310, RIB_H = 120;
// map keyOffset → vertical step (higher key = higher rung)
const keyRung = (k) => {
  const order = [0, 2, 4, 5];                // C D E F rungs (home C re-uses C)
  const idx = order.indexOf(k);
  return RIB_Y + RIB_H - 24 - (idx / 3) * (RIB_H - 48);
};
// build runs of constant key
const runs = [];
for (const s of sec) {
  const last = runs[runs.length - 1];
  if (last && last.key === s.keyOffset) last.b = s.endSec;
  else runs.push({ key: s.keyOffset, a: s.startSec, b: s.endSec });
}
// ribbon label (set well left of the rung-letter axis so they don't collide)
g.fillStyle = FAINT; g.font = `400 26px ${MONO}`;
g.textAlign = "left"; g.fillText("key journey", 40, RIB_Y - 18); g.textAlign = "left";
// baseline grid for rungs
g.strokeStyle = "rgba(24,20,16,0.07)"; g.lineWidth = 1;
[0, 2, 4, 5].forEach((k) => {
  const yy = keyRung(k);
  g.beginPath(); g.moveTo(ML, yy); g.lineTo(ML + PW, yy); g.stroke();
  g.fillStyle = FAINT; g.font = `400 24px ${MONO}`;
  g.textAlign = "right"; g.fillText(KEY_NAMES[k], ML - 24, yy + 8); g.textAlign = "left";
});
// stepped path + filled rungs
let prevY = null, prevXb = null;
for (const r of runs) {
  const xa = x(r.a), xb = x(r.b), yy = keyRung(r.key);
  // connecting riser between runs
  if (prevY != null) {
    g.strokeStyle = "rgba(24,20,16,0.45)"; g.lineWidth = 3.5;
    g.beginPath(); g.moveTo(prevXb, prevY); g.lineTo(xa, prevY); g.lineTo(xa, yy); g.stroke();
  }
  // rung bar
  g.fillStyle = "#1f1a14"; g.fillRect(xa, yy - 4, xb - xa, 8);
  // key tablet
  const tw = 92, th = 50;
  g.fillStyle = KEY_NAMES[r.key] === "C" && r.a > 250 ? "#5A6472" : "#1f1a14";
  g.fillRect(xa + 6, yy - th - 14, tw, th);
  g.fillStyle = "#FBF8F0"; g.font = `400 36px ${DISP}`;
  g.fillText(KEY_NAMES[r.key], xa + 28, yy - 26);
  prevY = yy; prevXb = xb;
}
// journey caption
g.fillStyle = SUB; g.font = `400 26px ${MONO}`;
g.textAlign = "right";
g.fillText("C → D → E → (home C) → F   ·   four passes, each lift +2/+2/+1 then a final euphoric +1 to F", ML + PW, RIB_Y - 18);
g.textAlign = "left";

// ════════════════════════════════════════════════════════════════════════
//  LANES — section bands behind, then per-voice piano-roll density rows,
//  with the intensity arc overlaid across the top of the plot region.
// ════════════════════════════════════════════════════════════════════════
const LANES = [
  { key: "rosewood",   label: "rosewood",   sub: "marimba lead",    color: "#C75B3A" },
  { key: "vibraphone", label: "vibraphone", sub: "held chords/pad", color: "#7E5CA8" },
  { key: "kalimba",    label: "kalimba",    sub: "high sparkle",    color: "#C88436" },
  { key: "bass",       label: "bass",       sub: "butter sine sub", color: "#3A6EA5" },
  { key: "bubbles",    label: "bubbles",    sub: "water ambience",  color: "#2A8C82" },
];
const ARC_H = 150;                                // intensity arc band
const PLOT_T = MT + ARC_H + 30;
const laneH = (PH - ARC_H - 30) / LANES.length;

// section bands (full plot height)
for (let i = 0; i < sec.length; i++) {
  const s = sec[i];
  const xa = x(s.startSec), xb = x(s.endSec);
  g.fillStyle = TINT[s.name] || "#ECECE8";
  g.globalAlpha = 0.55;
  g.fillRect(xa, MT, xb - xa, PH);
  g.globalAlpha = 1;
  g.strokeStyle = "rgba(24,20,16,0.10)"; g.lineWidth = 1;
  g.beginPath(); g.moveTo(xa, MT); g.lineTo(xa, MT + PH); g.stroke();
}

// pass-band brackets along the very top of the plot (A/B/C/D groupings by key)
const passes = [
  { label: "PASS A — establish the hop · C", a: 19.25, b: 80.9 },
  { label: "PASS B — fatter, busier · D", a: 80.9, b: 157.9 },
  { label: "PASS C — biggest, brightest · E", a: 157.9, b: 219.5 },
  { label: "PASS D — second wind · E → home → F", a: 219.5, b: 360 },
];
g.font = `400 27px ${MONO}`;
for (const p of passes) {
  const xa = x(p.a), xb = x(p.b);
  g.fillStyle = "rgba(24,20,16,0.78)";
  g.fillRect(xa, MT - 38, xb - xa - 4, 30);
  g.fillStyle = "#FBF8F0";
  g.fillText(p.label, xa + 12, MT - 16);
}
// intro tag
g.fillStyle = "rgba(94,140,151,0.92)"; g.fillRect(x(0), MT - 38, x(19.25) - x(0) - 4, 30);
g.fillStyle = "#FBF8F0"; g.fillText("INTRO", x(0) + 10, MT - 16);

// section name labels (rotated, at the top of each band)
for (let i = 0; i < sec.length; i++) {
  const s = sec[i];
  const xa = x(s.startSec);
  g.save();
  g.translate(xa + 4, PLOT_T + 8);
  g.rotate(-Math.PI / 2);
  g.fillStyle = TINT_DK[s.name] || INK;
  g.font = `400 24px ${MONO}`;
  g.textAlign = "right";
  g.fillText(s.name, 0, 18);
  g.restore();
}
g.textAlign = "left";

// ── intensity arc — a smooth filled curve over the section intensities ─────
const arcY0 = MT, arcY1 = MT + ARC_H;
g.fillStyle = FAINT; g.font = `400 24px ${MONO}`;
g.textAlign = "right"; g.fillText("intensity", ML - 24, arcY0 + ARC_H / 2); g.textAlign = "left";
// build points at section midpoints
const pts = sec.map((s) => ({ t: (s.startSec + s.endSec) / 2, v: s.intensity }));
pts.unshift({ t: 0, v: 0.05 }); pts.push({ t: TOTAL, v: 0.08 });
const arcYof = (v) => arcY1 - v * (ARC_H - 14);
g.beginPath();
g.moveTo(x(0), arcY1);
for (let i = 0; i < pts.length; i++) {
  const px = x(pts[i].t), py = arcYof(pts[i].v);
  if (i === 0) g.lineTo(px, py);
  else {
    const p0 = pts[i - 1];
    const cx = (x(p0.t) + px) / 2;
    g.quadraticCurveTo(cx, arcYof(p0.v), px, py);
  }
}
g.lineTo(x(TOTAL), arcY1); g.closePath();
const arcGrad = g.createLinearGradient(0, arcY0, 0, arcY1);
arcGrad.addColorStop(0, "rgba(199,91,58,0.42)");
arcGrad.addColorStop(1, "rgba(199,91,58,0.06)");
g.fillStyle = arcGrad; g.fill();
// arc stroke
g.beginPath();
for (let i = 0; i < pts.length; i++) {
  const px = x(pts[i].t), py = arcYof(pts[i].v);
  if (i === 0) g.moveTo(px, py);
  else { const p0 = pts[i - 1]; const cx = (x(p0.t) + px) / 2; g.quadraticCurveTo(cx, arcYof(p0.v), px, py); }
}
g.strokeStyle = "#C75B3A"; g.lineWidth = 3; g.stroke();

// ── per-voice piano-roll density rows ──────────────────────────────────────
function laneMidiRange(key) {
  const ev = events[key]; const ms = ev.map((e) => e.midi);
  return [Math.min(...ms), Math.max(...ms)];
}
for (let li = 0; li < LANES.length; li++) {
  const L = LANES[li];
  const yy = PLOT_T + li * laneH;
  const innerT = yy + 10, innerB = yy + laneH - 10, innerH = innerB - innerT;
  // row separator
  g.strokeStyle = "rgba(24,20,16,0.10)"; g.lineWidth = 1;
  g.beginPath(); g.moveTo(ML, yy + laneH); g.lineTo(ML + PW, yy + laneH); g.stroke();
  // label
  g.fillStyle = INK; g.font = `400 32px ${DISP}`; g.textAlign = "right";
  g.fillText(L.label, ML - 24, yy + laneH / 2 - 4);
  g.fillStyle = FAINT; g.font = `400 22px ${MONO}`;
  g.fillText(L.sub, ML - 24, yy + laneH / 2 + 26);
  g.textAlign = "left";
  // notes
  const [lo, hi] = laneMidiRange(L.key);
  const span = Math.max(1, hi - lo);
  const ev = events[L.key];
  for (const e of ev) {
    const nx = x(e.t);
    const nw = Math.max(2.2, (e.dur / TOTAL) * PW);
    const ny = innerB - ((e.midi - lo) / span) * innerH;
    g.fillStyle = L.color;
    g.globalAlpha = 0.78;
    g.fillRect(nx, ny - 3, nw, 6);
    g.globalAlpha = 1;
  }
}

// ════════════════════════════════════════════════════════════════════════
//  EVENT MARKERS — the special moments (modulations + dips + final lift)
// ════════════════════════════════════════════════════════════════════════
const marks = [
  { t: 19.25, label: "↑ the hop begins (in C)", c: "#B85C82" },
  { t: 34.65, label: "mommywow — held hush", c: "#9A8E66" },
  { t: 80.9,  label: "▲ LIFT to D (+2)", c: "#7E5CA8" },
  { t: 142.5, label: "cave — dub breakdown", c: "#5A6472" },
  { t: 157.9, label: "▲ LIFT to E (+4)", c: "#C88436" },
  { t: 273.4, label: "cave — home C breath", c: "#5A6472" },
  { t: 288.8, label: "★ FINAL CHORUS — euphoric jump to F (+5)", c: "#C75B3A" },
  { t: 342.7, label: "progression → land → button", c: "#4F8C46" },
];
g.setLineDash([7, 9]);
for (const m of marks) {
  const xe = x(m.t);
  g.strokeStyle = m.c + "AA"; g.lineWidth = 2.5;
  g.beginPath(); g.moveTo(xe, RIB_Y - 6); g.lineTo(xe, MT + PH); g.stroke();
  g.save(); g.translate(xe + 7, MT + PH - 14); g.rotate(-Math.PI / 2);
  g.fillStyle = m.c; g.font = `400 26px ${MONO}`;
  g.fillText(m.label, 0, 0); g.restore();
}
g.setLineDash([]);

// ════════════════════════════════════════════════════════════════════════
//  TIME AXIS + FOOTER
// ════════════════════════════════════════════════════════════════════════
g.fillStyle = SUB; g.font = `400 24px ${MONO}`; g.textAlign = "center";
for (let t = 0; t <= TOTAL; t += 30) {
  const xe = x(t);
  g.strokeStyle = "rgba(24,20,16,0.12)"; g.lineWidth = 1;
  g.beginPath(); g.moveTo(xe, MT + PH); g.lineTo(xe, MT + PH + 10); g.stroke();
  g.fillText(fmt(t), xe, MT + PH + 42);
}
g.textAlign = "left";
g.fillStyle = SUB; g.font = `400 28px ${DISP}`;
g.fillText("everything synthesized in JS DSP — marimba + perc synths, butter sine sub, water-drop & water-stream ambience, melodic squeak-rides blooming in a shared space reverb · bedroom-pop master",
  ML, H - 56);
g.fillStyle = FAINT; g.font = `400 22px ${MONO}`;
g.fillText("piano-roll rows show every sounding note (post-transpose); vertical position = pitch within each voice's range · ride breaks add 16th hats · the groove stays put while the melody climbs",
  ML, H - 22);

// ── write ────────────────────────────────────────────────────────────────
const png = resolve(OUT, "fluttabap360-score.png");
writeFileSync(png, cv.toBuffer("image/png"));
console.log(`✓ ${png}`);
try {
  const desk = resolve(homedir(), "Desktop", "fluttabap360-score.png");
  copyFileSync(png, desk);
  console.log(`✓ ${desk}`);
} catch (e) { console.log("(desktop copy skipped:", e.message, ")"); }
