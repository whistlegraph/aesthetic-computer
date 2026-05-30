#!/usr/bin/env node
// sleephellsine/bin/gen-score.mjs — graphic score for sleephellsine.
//
// Circle-of-fifths chord progression. Two upper sine voices oscillate
// in contrary motion between the M3 and P5 of the current chord. At
// every zero-crossing of their swing, a long deep root bell fires.
// The harmonic root walks the circle once every 60 seconds; the piece
// is 15 rotations of the wheel.
//
// All constants mirror c/sleephellsine.c — if the engine changes,
// change them here too.
//
// Output:
//   pop/sleephellsine/out/sleephellsine-score.png   (3000×1800)
//   pop/sleephellsine/out/sleephellsine-score.pdf   (vector)
//   pop/sleephellsine/out/sleephellsine-events.json (crossing times + roots)
//
// Usage: node pop/sleephellsine/bin/gen-score.mjs

import { writeFileSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas } from "canvas";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE_DIR = resolve(HERE, "..");
const OUT_DIR = resolve(LANE_DIR, "out");
mkdirSync(OUT_DIR, { recursive: true });

// ── engine constants (must match c/sleephellsine.c) ───────────────────
const TOTAL_SEC = 900;
const CIRCLE_OF_FIFTHS = [0, 7, 2, 9, 4, 11, 6, 1, 8, 3, 10, 5];
//                        C  G  D  A  E  B   F# C# G# D# A# F
const N_CIRCLE = 12;
const CHORD_DUR_SEC = 5.0;
const ROTATION_SEC = CHORD_DUR_SEC * N_CIRCLE;    // 60 s
const N_ROTATIONS = TOTAL_SEC / ROTATION_SEC;     // 15

const UPPER_CENTER_REF = 64;       // E4
const UPPER_AMP_ST = 1.5;
const CENTER_GLIDE_TC_MS = 520;    // engine: 1/(SR*0.00004) ≈ 520 ms

const ROOT_BELL_BASE = 36;         // C2
const ROOT_BELL_TAU = 9.0;

const NOTE_NAMES = ["C", "G", "D", "A", "E", "B", "F#", "C#", "G#", "D#", "A#", "F"];
const PC_NAMES   = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];

// ── engine model (mirrors c/sleephellsine.c) ─────────────────────────
function stepAt(t) {
  const step = Math.floor(t / CHORD_DUR_SEC);
  return ((step % N_CIRCLE) + N_CIRCLE) % N_CIRCLE;
}
function rootPcAt(t) {
  return CIRCLE_OF_FIFTHS[stepAt(t)];
}
function chordCenterTarget(t) {
  let c = rootPcAt(t) + 5.5;
  while (c < UPPER_CENTER_REF - 6) c += 12;
  while (c > UPPER_CENTER_REF + 6) c -= 12;
  return c;
}
function rootBellMidi(t) {
  return ROOT_BELL_BASE + rootPcAt(t);
}

function simulate() {
  const dt = 0.005;
  const samples = [];
  const crossings = [];
  let center = chordCenterTarget(0);
  const glideCoef = 1 - Math.exp(-dt / (CENTER_GLIDE_TC_MS / 1000));
  let prevSign = 0;
  for (let t = 0; t <= TOTAL_SEC; t += dt) {
    const tgt = chordCenterTarget(t);
    center += (tgt - center) * glideCoef;
    const swing = Math.cos(2 * Math.PI * t / CHORD_DUR_SEC);
    const a = center + UPPER_AMP_ST * swing;
    const b = center - UPPER_AMP_ST * swing;
    samples.push({ t, a, b, center });
    const diff = a - b;
    const curSign = diff >= 0 ? 1 : -1;
    if (prevSign === 0) prevSign = curSign;
    else if (curSign !== prevSign) {
      crossings.push({
        t,
        midi: rootBellMidi(t),
        rootPc: rootPcAt(t),
        chordStep: stepAt(t),
      });
      prevSign = curSign;
    }
  }
  return { samples, crossings };
}

console.log("[score] simulating…");
const { samples, crossings } = simulate();
const totalChords = Math.floor(TOTAL_SEC / CHORD_DUR_SEC);
console.log(`[score] ${totalChords} chord steps · ${crossings.length} crossings (≈${(crossings.length/totalChords).toFixed(1)} per chord)`);

// emit a JSON event list
const eventsPath = resolve(OUT_DIR, "sleephellsine-events.json");
writeFileSync(eventsPath, JSON.stringify({
  generated: new Date().toISOString(),
  totalSec: TOTAL_SEC,
  circleOfFifths: CIRCLE_OF_FIFTHS,
  chordDurSec: CHORD_DUR_SEC,
  rotationSec: ROTATION_SEC,
  rotations: N_ROTATIONS,
  upperCenterRef: UPPER_CENTER_REF,
  upperAmpST: UPPER_AMP_ST,
  rootBellBase: ROOT_BELL_BASE,
  crossings: crossings.map((c) => ({
    t: +c.t.toFixed(4),
    midi: +c.midi.toFixed(4),
    rootPc: c.rootPc,
    chordStep: c.chordStep,
    chordName: PC_NAMES[c.rootPc],
  })),
}, null, 0));
console.log(`[score] → ${eventsPath}`);

// ── geometry ──────────────────────────────────────────────────────────
const W = 3000, H = 1800;
const MARGIN_L = 140, MARGIN_R = 140;
const MARGIN_TOP = 240, MARGIN_BOT = 220;

// Two panels stacked:
//   • top    : detail of one chord (5 s) — sinusoidal swing + crossings
//   • bottom : one full rotation (60 s) — all 12 chords + chord labels
const PANEL_GAP = 80;
const PANEL_H = (H - MARGIN_TOP - MARGIN_BOT - PANEL_GAP) / 2;
const PANEL_TOP_Y = MARGIN_TOP;
const PANEL_BOT_Y = MARGIN_TOP + PANEL_H + PANEL_GAP;

const TRACK_X = MARGIN_L;
const TRACK_W = W - MARGIN_L - MARGIN_R;

// pitch axis: cover the upper-voice swing range (58..70) and root bells
// in the same vertical span by mapping them onto different sub-bands.
const UPPER_PITCH_LO = 58, UPPER_PITCH_HI = 70;

const panelTtoX = (panelStart, panelEnd) => (t) =>
  TRACK_X + ((t - panelStart) / (panelEnd - panelStart)) * TRACK_W;
const upperPitchToY = (panelY) => (m) =>
  panelY + PANEL_H * 0.78 - ((m - UPPER_PITCH_LO) / (UPPER_PITCH_HI - UPPER_PITCH_LO)) * PANEL_H * 0.62;

// ── render ────────────────────────────────────────────────────────────
function renderTo(ctx, isPdf = false) {
  // Background — deep nightfall gradient.
  const bg = ctx.createLinearGradient(0, 0, 0, H);
  bg.addColorStop(0, "#0a0e1f");
  bg.addColorStop(0.55, "#141131");
  bg.addColorStop(1, "#221538");
  ctx.fillStyle = bg;
  ctx.fillRect(0, 0, W, H);

  if (!isPdf) {
    const vg = ctx.createRadialGradient(W / 2, H / 2, W * 0.25, W / 2, H / 2, W * 0.75);
    vg.addColorStop(0, "rgba(0,0,0,0)");
    vg.addColorStop(1, "rgba(0,0,0,0.55)");
    ctx.fillStyle = vg;
    ctx.fillRect(0, 0, W, H);
  }

  // ── title ──────────────────────────────────────────────────────────
  ctx.fillStyle = "#f4e9d8";
  ctx.textBaseline = "alphabetic";
  ctx.font = "bold 96px 'Georgia', serif";
  ctx.fillText("sleephellsine", MARGIN_L, 130);

  ctx.fillStyle = "#a89db8";
  ctx.font = "italic 30px 'Georgia', serif";
  ctx.fillText(
    "a graphic score · circle of fifths · two uppers crossing · 15 minutes",
    MARGIN_L, 175,
  );

  ctx.fillStyle = "#7a708a";
  ctx.font = "20px 'Helvetica', sans-serif";
  const meta = `${totalChords} chord steps · ${crossings.length} crossings · rotation every ${ROTATION_SEC}s · ${N_ROTATIONS} rotations · root bells @ τ=${ROOT_BELL_TAU}s`;
  ctx.textAlign = "right";
  ctx.fillText(meta, W - MARGIN_R, 175);
  ctx.textAlign = "left";

  // ── two panels ─────────────────────────────────────────────────────
  drawChordDetail(ctx, PANEL_TOP_Y);
  drawRotation(ctx, PANEL_BOT_Y);

  // ── circle of fifths inset (upper-right corner) ────────────────────
  drawCircleInset(ctx);

  // ── colophon ───────────────────────────────────────────────────────
  ctx.fillStyle = "#6f6585";
  ctx.font = "14px 'Helvetica', sans-serif";
  ctx.fillText("voices", MARGIN_L, H - 70);
  legendChip(ctx, MARGIN_L +  80, H - 82, "#7fbedc", "sine A");
  legendChip(ctx, MARGIN_L + 240, H - 82, "#e0a464", "sine B");
  legendChip(ctx, MARGIN_L + 400, H - 82, "#ffeec0", "crossing (root bell trigger)");
  legendChip(ctx, MARGIN_L + 740, H - 82, "#d27a4e", "root bell decay (C2..B2)");
  legendChip(ctx, MARGIN_L +1060, H - 82, "rgba(255,255,255,0.4)", "centre voice-leading");

  ctx.fillStyle = "#5a5170";
  ctx.font = "italic 14px 'Georgia', serif";
  ctx.textAlign = "right";
  ctx.fillText(
    `pop / sleephellsine · circle of fifths × ${N_ROTATIONS} rotations · 60 s per turn`,
    W - MARGIN_R, H - 70,
  );
  ctx.textAlign = "left";
}

// ── chord-detail panel (one chord, 5 s) ──────────────────────────────
function drawChordDetail(ctx, panelY) {
  // Use the first chord (C major) starting just after t=0 so the curves
  // are at steady state. Start at t = ROTATION_SEC + 0 (chord 12, also C)
  // so the curves have settled.
  const tStart = ROTATION_SEC;
  const tEnd = tStart + CHORD_DUR_SEC;
  const xT = panelTtoX(tStart, tEnd);
  const yP = upperPitchToY(panelY);

  // frame
  ctx.strokeStyle = "rgba(255,255,255,0.10)";
  ctx.lineWidth = 1.5;
  ctx.strokeRect(TRACK_X, panelY, TRACK_W, PANEL_H);

  ctx.fillStyle = "#9b8fb5";
  ctx.font = "bold 22px 'Helvetica', sans-serif";
  ctx.fillText("ONE CHORD  ·  5 s  ·  TWO UPPERS SWING M3 ↔ P5  ·  CROSS TWICE",
    TRACK_X, panelY - 16);

  // pitch gridlines + labels in the upper-voice octave
  for (let m = UPPER_PITCH_LO; m <= UPPER_PITCH_HI; m += 2) {
    const y = yP(m);
    ctx.strokeStyle = "rgba(255,255,255,0.05)";
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(TRACK_X, y); ctx.lineTo(TRACK_X + TRACK_W, y); ctx.stroke();
    ctx.fillStyle = "#5a5170";
    ctx.font = "12px 'Helvetica', sans-serif";
    ctx.fillText(midiToName(m), TRACK_X - 36, y + 4);
  }

  // chord-tone reference lines (M3 and P5 of C major in this register: E4=64, G4=67)
  const m3y = yP(64), p5y = yP(67);
  ctx.strokeStyle = "rgba(255,238,192,0.18)";
  ctx.setLineDash([4, 6]);
  ctx.lineWidth = 1.2;
  ctx.beginPath();
  ctx.moveTo(TRACK_X, m3y); ctx.lineTo(TRACK_X + TRACK_W, m3y); ctx.stroke();
  ctx.beginPath();
  ctx.moveTo(TRACK_X, p5y); ctx.lineTo(TRACK_X + TRACK_W, p5y); ctx.stroke();
  ctx.setLineDash([]);
  ctx.fillStyle = "#ffeec0";
  ctx.font = "italic 13px 'Georgia', serif";
  ctx.textAlign = "right";
  ctx.fillText("P5 (G4)", TRACK_X + TRACK_W - 8, p5y - 6);
  ctx.fillText("M3 (E4)", TRACK_X + TRACK_W - 8, m3y + 18);
  ctx.textAlign = "left";

  // voice curves
  drawSineCurve(ctx, "#7fbedc", "a", xT, yP, tStart, tEnd, 2.4);
  drawSineCurve(ctx, "#e0a464", "b", xT, yP, tStart, tEnd, 2.4);

  // crossings (this window)
  for (const c of crossings) {
    if (c.t < tStart || c.t > tEnd) continue;
    const cx = xT(c.t);
    const cy = yP(samples.find(s => Math.abs(s.t - c.t) < 0.01)?.center ?? UPPER_CENTER_REF);
    ctx.fillStyle = "rgba(255,230,176,0.18)";
    ctx.beginPath(); ctx.arc(cx, cy, 12, 0, Math.PI * 2); ctx.fill();
    ctx.fillStyle = "#ffeec0";
    ctx.beginPath(); ctx.arc(cx, cy, 4.5, 0, Math.PI * 2); ctx.fill();
    // label
    ctx.fillStyle = "#d27a4e";
    ctx.font = "bold 12px 'Helvetica', sans-serif";
    ctx.textAlign = "center";
    ctx.fillText("⇩ root bell", cx, cy + 28);
    ctx.textAlign = "left";
  }

  // root bell decay region (visualized in the lower portion of the panel)
  drawBellTrails(ctx, panelY, xT, tStart, tEnd);

  // time axis
  ctx.strokeStyle = "rgba(255,255,255,0.30)";
  ctx.lineWidth = 1.2;
  ctx.beginPath();
  ctx.moveTo(TRACK_X, panelY + PANEL_H + 1);
  ctx.lineTo(TRACK_X + TRACK_W, panelY + PANEL_H + 1);
  ctx.stroke();
  for (let s = 0; s <= CHORD_DUR_SEC; s += 0.5) {
    const tx = xT(tStart + s);
    ctx.strokeStyle = "rgba(255,255,255,0.30)";
    ctx.beginPath();
    ctx.moveTo(tx, panelY + PANEL_H + 1);
    ctx.lineTo(tx, panelY + PANEL_H + (s % 1 === 0 ? 14 : 8));
    ctx.stroke();
    if (s % 1 === 0) {
      ctx.fillStyle = "#9b8fb5";
      ctx.font = "14px 'Helvetica', sans-serif";
      ctx.textAlign = "center";
      ctx.fillText(`${s}s`, tx, panelY + PANEL_H + 32);
    }
  }
  ctx.textAlign = "left";
}

// ── rotation panel (one full rotation, 60 s, all 12 chords) ──────────
function drawRotation(ctx, panelY) {
  const tStart = ROTATION_SEC * 2;          // 3rd rotation — settled
  const tEnd = tStart + ROTATION_SEC;
  const xT = panelTtoX(tStart, tEnd);
  const yP = upperPitchToY(panelY);

  ctx.strokeStyle = "rgba(255,255,255,0.10)";
  ctx.lineWidth = 1.5;
  ctx.strokeRect(TRACK_X, panelY, TRACK_W, PANEL_H);

  ctx.fillStyle = "#9b8fb5";
  ctx.font = "bold 22px 'Helvetica', sans-serif";
  ctx.fillText(`ONE ROTATION  ·  60 s  ·  ALL 12 ROOTS  ·  REPEATS ×${N_ROTATIONS}`,
    TRACK_X, panelY - 16);

  // chord segment boundaries + labels along the top
  for (let i = 0; i < N_CIRCLE; i++) {
    const t = tStart + i * CHORD_DUR_SEC;
    const x = xT(t);
    // segment divider
    ctx.strokeStyle = "rgba(255,255,255,0.10)";
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(x, panelY); ctx.lineTo(x, panelY + PANEL_H); ctx.stroke();
    // chord name centered in the segment
    const xMid = xT(t + CHORD_DUR_SEC * 0.5);
    ctx.fillStyle = "#c8b5e0";
    ctx.font = "bold 24px 'Georgia', serif";
    ctx.textAlign = "center";
    ctx.fillText(NOTE_NAMES[i], xMid, panelY + 28);
    // step index
    ctx.fillStyle = "#5a5170";
    ctx.font = "11px 'Helvetica', sans-serif";
    ctx.fillText(`§${i + 1}`, xMid, panelY + 44);
  }
  // closing boundary
  ctx.strokeStyle = "rgba(255,255,255,0.10)";
  ctx.beginPath();
  ctx.moveTo(xT(tEnd), panelY); ctx.lineTo(xT(tEnd), panelY + PANEL_H); ctx.stroke();
  ctx.textAlign = "left";

  // pitch gridlines
  for (let m = UPPER_PITCH_LO; m <= UPPER_PITCH_HI; m += 4) {
    const y = yP(m);
    ctx.strokeStyle = "rgba(255,255,255,0.05)";
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(TRACK_X, y); ctx.lineTo(TRACK_X + TRACK_W, y); ctx.stroke();
    ctx.fillStyle = "#5a5170";
    ctx.font = "12px 'Helvetica', sans-serif";
    ctx.fillText(midiToName(m), TRACK_X - 36, y + 4);
  }

  // bell trails
  drawBellTrails(ctx, panelY, xT, tStart, tEnd);

  // centre voice-leading curve
  ctx.strokeStyle = "rgba(255,255,255,0.18)";
  ctx.lineWidth = 1.3;
  ctx.setLineDash([4, 6]);
  ctx.beginPath();
  let started = false;
  for (let i = 0; i < samples.length; i += 6) {
    const s = samples[i];
    if (s.t < tStart) continue;
    if (s.t > tEnd) break;
    const x = xT(s.t);
    const y = yP(s.center);
    if (!started) { ctx.moveTo(x, y); started = true; } else ctx.lineTo(x, y);
  }
  ctx.stroke();
  ctx.setLineDash([]);

  // two voice curves
  drawSineCurve(ctx, "#7fbedc", "a", xT, yP, tStart, tEnd, 1.6);
  drawSineCurve(ctx, "#e0a464", "b", xT, yP, tStart, tEnd, 1.6);

  // crossings
  for (const c of crossings) {
    if (c.t < tStart || c.t > tEnd) continue;
    const cx = xT(c.t);
    // approximate the center at the crossing time
    const sx = samples.find(s => Math.abs(s.t - c.t) < 0.01);
    const cy = yP(sx?.center ?? UPPER_CENTER_REF);
    ctx.fillStyle = "rgba(255,230,176,0.18)";
    ctx.beginPath(); ctx.arc(cx, cy, 5, 0, Math.PI * 2); ctx.fill();
    ctx.fillStyle = "#ffeec0";
    ctx.beginPath(); ctx.arc(cx, cy, 2.2, 0, Math.PI * 2); ctx.fill();
  }

  // time axis
  ctx.strokeStyle = "rgba(255,255,255,0.30)";
  ctx.lineWidth = 1.2;
  ctx.beginPath();
  ctx.moveTo(TRACK_X, panelY + PANEL_H + 1);
  ctx.lineTo(TRACK_X + TRACK_W, panelY + PANEL_H + 1);
  ctx.stroke();
  for (let s = 0; s <= ROTATION_SEC; s += 5) {
    const tx = xT(tStart + s);
    ctx.strokeStyle = "rgba(255,255,255,0.30)";
    ctx.beginPath();
    ctx.moveTo(tx, panelY + PANEL_H + 1);
    ctx.lineTo(tx, panelY + PANEL_H + (s % 10 === 0 ? 14 : 8));
    ctx.stroke();
    if (s % 10 === 0) {
      ctx.fillStyle = "#9b8fb5";
      ctx.font = "14px 'Helvetica', sans-serif";
      ctx.textAlign = "center";
      ctx.fillText(`${s}s`, tx, panelY + PANEL_H + 32);
    }
  }
  ctx.textAlign = "left";
}

// Curve drawer for either voice
function drawSineCurve(ctx, color, key, xMap, yP, panelStart, panelEnd, lineWidth) {
  ctx.strokeStyle = color;
  ctx.lineWidth = lineWidth;
  ctx.beginPath();
  let started = false;
  const span = panelEnd - panelStart;
  const stride = span <= 10 ? 1 : Math.max(1, Math.floor(samples.length / 4000));
  for (let i = 0; i < samples.length; i += stride) {
    const s = samples[i];
    if (s.t < panelStart) continue;
    if (s.t > panelEnd) break;
    const x = xMap(s.t);
    const y = yP(s[key]);
    if (!started) { ctx.moveTo(x, y); started = true; } else ctx.lineTo(x, y);
  }
  ctx.stroke();
}

// Root bell trails — paint a low band of the panel with each bell's
// exponential decay, colored by chord step.
function drawBellTrails(ctx, panelY, xMap, panelStart, panelEnd) {
  const bandY = panelY + PANEL_H * 0.84;
  const bandH = PANEL_H * 0.14;
  ctx.fillStyle = "rgba(255,255,255,0.02)";
  ctx.fillRect(TRACK_X, bandY, TRACK_W, bandH);
  ctx.fillStyle = "#6f6585";
  ctx.font = "11px 'Helvetica', sans-serif";
  ctx.fillText("ROOT BELLS  (C2..B2)", TRACK_X + 8, bandY + 14);

  for (const c of crossings) {
    if (c.t + ROOT_BELL_TAU * 4 < panelStart) continue;
    if (c.t > panelEnd) continue;
    const x0 = xMap(Math.max(c.t, panelStart));
    const x1Time = Math.min(panelEnd, c.t + ROOT_BELL_TAU * 4);
    const x1 = xMap(x1Time);
    if (x1 - x0 < 1) continue;
    // chord-colored gradient — hue rotates around the circle
    const hue = (c.chordStep * (360 / N_CIRCLE) + 18) % 360;
    const grad = ctx.createLinearGradient(x0, 0, x1, 0);
    grad.addColorStop(0,   `hsla(${hue}, 60%, 60%, 0.55)`);
    grad.addColorStop(0.4, `hsla(${hue}, 60%, 50%, 0.22)`);
    grad.addColorStop(1,   `hsla(${hue}, 60%, 40%, 0.0)`);
    ctx.fillStyle = grad;
    // height varies subtly with chord step (so adjacent bells visually layer)
    const yOff = (c.chordStep % 4) * 4;
    ctx.fillRect(x0, bandY + 22 + yOff, x1 - x0, 6);
  }
}

// ── circle of fifths inset ─────────────────────────────────────────────
function drawCircleInset(ctx) {
  const cx = W - MARGIN_R - 180;
  const cy = MARGIN_TOP - 80;
  const radius = 80;

  // outer ring
  ctx.strokeStyle = "rgba(255,255,255,0.15)";
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.arc(cx, cy, radius, 0, Math.PI * 2);
  ctx.stroke();

  // inner ring (smaller)
  ctx.strokeStyle = "rgba(255,255,255,0.05)";
  ctx.beginPath();
  ctx.arc(cx, cy, radius - 18, 0, Math.PI * 2);
  ctx.stroke();

  // 12 note labels around the ring (C at top, going clockwise by 5ths)
  for (let i = 0; i < 12; i++) {
    const angle = -Math.PI / 2 + (i / 12) * Math.PI * 2;
    const lx = cx + Math.cos(angle) * radius;
    const ly = cy + Math.sin(angle) * radius;
    const hue = (i * (360 / 12) + 18) % 360;
    ctx.fillStyle = `hsl(${hue}, 50%, 70%)`;
    ctx.font = "bold 14px 'Georgia', serif";
    ctx.textAlign = "center";
    ctx.textBaseline = "middle";
    ctx.fillText(NOTE_NAMES[i], lx, ly);
  }
  ctx.textAlign = "left";
  ctx.textBaseline = "alphabetic";

  // direction arrow
  ctx.strokeStyle = "rgba(255,238,192,0.55)";
  ctx.lineWidth = 1.5;
  ctx.beginPath();
  ctx.arc(cx, cy, radius - 32, -Math.PI / 2, -Math.PI / 2 + Math.PI * 1.4, false);
  ctx.stroke();
  // arrowhead
  const ah = -Math.PI / 2 + Math.PI * 1.4;
  const ax = cx + Math.cos(ah) * (radius - 32);
  const ay = cy + Math.sin(ah) * (radius - 32);
  ctx.fillStyle = "rgba(255,238,192,0.7)";
  ctx.beginPath();
  ctx.moveTo(ax, ay);
  ctx.lineTo(ax - 7, ay - 4);
  ctx.lineTo(ax - 4, ay + 6);
  ctx.closePath();
  ctx.fill();

  // label
  ctx.fillStyle = "#a89db8";
  ctx.font = "italic 14px 'Georgia', serif";
  ctx.textAlign = "center";
  ctx.fillText("circle of 5ths", cx, cy + radius + 22);
  ctx.fillText("60 s per rotation", cx, cy + radius + 40);
  ctx.textAlign = "left";
}

function legendChip(ctx, x, y, color, label) {
  ctx.fillStyle = color;
  ctx.fillRect(x, y, 22, 8);
  ctx.fillStyle = "#a89db8";
  ctx.font = "14px 'Helvetica', sans-serif";
  ctx.fillText(label, x + 32, y + 9);
}

function midiToName(m) {
  const names = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
  const oct = Math.floor(m / 12) - 1;
  return `${names[m % 12]}${oct}`;
}

// ── render PNG ────────────────────────────────────────────────────────
const pngCanvas = createCanvas(W, H);
renderTo(pngCanvas.getContext("2d"), false);
const pngPath = resolve(OUT_DIR, "sleephellsine-score.png");
writeFileSync(pngPath, pngCanvas.toBuffer("image/png"));
console.log(`→ png · ${pngPath}`);

// ── render PDF (vector) ───────────────────────────────────────────────
const pdfCanvas = createCanvas(W, H, "pdf");
renderTo(pdfCanvas.getContext("2d"), true);
const pdfPath = resolve(OUT_DIR, "sleephellsine-score.pdf");
writeFileSync(pdfPath, pdfCanvas.toBuffer("application/pdf"));
console.log(`→ pdf · ${pdfPath}`);
