#!/usr/bin/env node
// render-notespatial-diagram.mjs — the mp4 diagram of *Note(s)pat(ial)
// Native*, drawn in the graphic-score paper style (cream, ink, red — the
// scorodeon world) as an ink technical drawing come alive:
//
//   · one VIRTUAL ACOUSTIC SPACE — a wireframe globe holding the 13
//     voices of Special Sign, spinning and manipulated per movement
//   · six AC OS ThinkPads ringed in the physical room, each a WINDOW
//     into that space (every screen renders the globe from its own seat)
//   · a mini speaker rig beside each machine — sound leaves the virtual
//     space through the actual computers
//   · the human listener, centered, in the room that mediates it all
//   · sight-lines from voice to machine show the live choreography
//     (enter / antiphony / orbit / scatter / converge / rest); the full
//     track data runs along the bottom with a sweeping playhead
//
// Frames stream raw into ffmpeg; audio is the piece rebuilt from its own
// C engine, trimmed to release bounds.
//
//   node render-notespatial-diagram.mjs <score.nsscore> <audio.wav> <out.mp4>
//   node render-notespatial-diagram.mjs <score.nsscore> --still <t> <out.png>

import { readFileSync, writeFileSync } from "node:fs";
import { spawn } from "node:child_process";
import { createCanvas, registerFont, loadImage } from "canvas";
import { magickRenderText, YWFT_PATH } from "../../../pop/lib/preview-shared.mjs";

const args = process.argv.slice(2);
const scorePath = args[0];
const still = args.includes("--still") ? parseFloat(args[args.indexOf("--still") + 1]) : null;
const audioPath = still == null ? args[1] : null;
const outPath = still == null ? args[2] : args[args.indexOf("--still") + 2];
if (!scorePath || !outPath) {
  console.error("usage: render-notespatial-diagram.mjs <score.nsscore> <audio.wav> <out.mp4> | <score.nsscore> --still <t> <out.png>");
  process.exit(1);
}
const S = JSON.parse(readFileSync(scorePath, "utf8"));

const W = 1920, H = 1080, FPS = 30;
const DUR = S.dur + 2;
const FRAMES = Math.ceil(DUR * FPS);
const SEATS = 6, RING = 2.9;

// ── the graphic-score paper ──────────────────────────────────────────
const CREAM = "#FBFAFF", INK = "#1E1E1E", LINE_RED = "#B3402E";
const ink = (a) => `rgba(30,30,30,${a})`;
const rgba = (c, a) => `rgba(${c[0]},${c[1]},${c[2]},${a})`;
try { registerFont("/System/Library/Fonts/Menlo.ttc", { family: "MenloX" }); } catch { /* default */ }
const MONO = (px, bold) => `${bold ? "bold " : ""}${px}px MenloX, monospace`;

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");

// ── camera ───────────────────────────────────────────────────────────
const sub = (a, b) => [a[0] - b[0], a[1] - b[1], a[2] - b[2]];
const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]];
const norm = (a) => { const l = Math.hypot(...a); return [a[0] / l, a[1] / l, a[2] / l]; };

function project(p, t) {
  const az = 0.4 + t * 0.05, el = 0.27, dist = 8.2, fov = 900;
  const look = [0, 1.0, 0];
  const cam = [Math.cos(az) * Math.cos(el) * dist, look[1] + Math.sin(el) * dist,
               Math.sin(az) * Math.cos(el) * dist];
  const fwd = norm(sub(look, cam));
  const right = norm(cross(fwd, [0, 1, 0]));
  const up = cross(right, fwd);
  const d = sub(p, cam);
  const z = dot(d, fwd);
  if (z < 0.5) return null;
  // scene-only anamorphic stretch: the room reads panoramic, chrome stays true
  return { x: W / 2 + dot(d, right) * fov * 1.22 / z, y: H / 2 - 145 - dot(d, up) * fov / z, s: fov / z, d: z };
}

const seatAngle = (i) => (i / SEATS) * Math.PI * 2 - Math.PI / 2;
const seatPos = (i) => [Math.cos(seatAngle(i)) * RING, 0, Math.sin(seatAngle(i)) * RING];

// ── choreography: lane → seat (the composition model) ────────────────
const M = S.movements;
function laneSeat(lane, t) {
  if (S.lanes.length <= 8) { // spatial scores: the seat you are passing
    const a = voiceAngle(lane) + spinAngle;
    return ((Math.round(((a + Math.PI / 2) / (Math.PI * 2)) * SEATS) % SEATS) + SEATS) % SEATS;
  }
  const u = (m) => (t - m.t0) / (m.t1 - m.t0);
  if (t < M[0].t1) {
    const k = u(M[0]);
    if (k < 0.34) return 0;
    if (k < 0.67) return [0, 2, 4][lane % 3];
    return lane % SEATS;
  }
  if (t < M[1].t1) {
    const q = Math.floor(u(M[1]) * 4) % SEATS;
    return [4, 5, 8, 9, 12].includes(lane) ? q : (q + 3) % SEATS;
  }
  if (t < M[2].t1) {
    const k = u(M[2]);
    return (lane + Math.floor(k * 8 * (1 + k))) % SEATS;
  }
  if (t < M[3].t1) return lane % SEATS;
  if (t < M[4].t1) return u(M[4]) * 13 > lane ? 0 : lane % SEATS;
  return 0;
}

// ── the virtual acoustic space: registered 1:1 on the room ───────────
// The field IS the ring — voices orbit at seat radius, screen height,
// passing machine to machine. Movements breathe its size, never lift it.
function globeState(t) {
  const u = (m) => Math.max(0, Math.min(1, (t - m.t0) / (m.t1 - m.t0)));
  let scale = 1, cy = 0.72, r = 0.94;
  if (t < M[0].t1) scale = 0.3 + u(M[0]) * 0.7;               // assembling
  else if (t >= M[3].t0 && t < M[3].t1) r = 0.94 + u(M[3]) * 0.18; // dilate
  else if (t >= M[4].t0 && t < M[4].t1) scale = 1 - u(M[4]) * 0.35;
  else if (t >= M[4].t1) scale = 0.65 - Math.min(1, (t - M[4].t1) / 6) * 0.5;
  return { scale, cy, r };
}
let spinAngle = 0, lastT = 0;
// Integrates the SAME curve the audio baker pans by (K = 0.5 laps/sec at
// ribbon 1.0) — what you hear crossing the field is what you see.
function spinRate(t) {
  const i = Math.min((S.rotation?.length ?? 1) - 1, Math.floor((t / S.dur) * (S.rotation?.length ?? 1)));
  const env = S.rotation?.[Math.max(0, i)] ?? 0;
  return (S.lanes.length > 1 ? 0.15 : 0) + env * Math.PI;
}
// where a voice sits: pinned lanes hold still, the rest ride the orbit
const voiceAngle = (i) => typeof S.lanes[i].az === "number"
  ? S.lanes[i].az - spinAngle          // cancels the spin added downstream
  : (i / S.lanes.length) * Math.PI * 2 + (S.lanes[i].azOffset || 0);
const isPinned = (i) => typeof S.lanes[i].az === "number";
// the elevation ribbon the baker turns into pinna notches, read here as
// literal height: overhead voices ride up, underfoot ones drop below.
function elevationAt(t) {
  const e = S.elevation;
  if (!e) return 0;
  return e[Math.max(0, Math.min(e.length - 1, Math.round((t / S.dur) * (e.length - 1))))] || 0;
}
// The global gestures: the whole field swings, tips, and breathes near
// and far around a listener who stays put. The machines never move —
// they are furniture — so a field transform is visible as everything
// else moving relative to them.
function fieldAt(t, key, dflt) {
  const a = S[key];
  if (!a) return dflt;
  return a[Math.max(0, Math.min(a.length - 1, Math.round((t / S.dur) * (a.length - 1))))] ?? dflt;
}
function voicePos(i, t) {
  const g = globeState(t);
  const a = voiceAngle(i) + spinAngle + fieldAt(t, "fieldShift", 0) * Math.PI;
  const el = Math.max(-1, Math.min(1,
    (isPinned(i) ? (S.lanes[i].el || 0) : elevationAt(t)) + fieldAt(t, "fieldTilt", 0)));
  const scale = fieldAt(t, "fieldScale", 1);
  const lane = S.lanes[i];
  const near = isPinned(i) ? (lane.dist ?? 1.2) : 1.2;
  const ringR = RING * g.r * g.scale * Math.cos(el * Math.PI / 2 * 0.8)
                * Math.max(0.25, Math.min(2.1, scale * near / 1.2));
  const y = g.cy + Math.sin(voiceAngle(i) * 3 + i) * 0.34 + el * 2.3;
  return [Math.cos(a) * ringR, y, Math.sin(a) * ringR];
}

// ── event feed ───────────────────────────────────────────────────────
const cursors = S.lanes.map(() => 0);
const stripLo = S.lanes.map(() => 0);
const ripples = [];
const glow = S.lanes.map(() => 0);

function circle3d(cx3, cy3, cz3, r, axis, t, style, width) {
  ctx.strokeStyle = style; ctx.lineWidth = width;
  ctx.beginPath();
  let started = false;
  for (let a = 0; a <= 60; a++) {
    const th = (a / 30) * Math.PI;
    let p;
    if (axis === "y") p = [cx3 + Math.cos(th) * r, cy3, cz3 + Math.sin(th) * r];
    else if (axis === "x") p = [cx3, cy3 + Math.cos(th) * r, cz3 + Math.sin(th) * r];
    else p = [cx3 + Math.cos(th) * r, cy3 + Math.sin(th) * r, cz3];
    const q = project(p, t);
    if (!q) { started = false; continue; }
    if (!started) { ctx.moveTo(q.x, q.y); started = true; } else ctx.lineTo(q.x, q.y);
  }
  ctx.stroke();
}

// ── sprites ──────────────────────────────────────────────────────────
function thinkpadSprite(p2, seat, t, screenGlow) {
  const u = Math.max(16, 52 * p2.s / 140);
  ctx.save();
  ctx.translate(p2.x, p2.y);
  // slab base — matte black parallelogram with ink outline
  ctx.fillStyle = "#26262a";
  ctx.strokeStyle = ink(0.75);
  ctx.lineWidth = Math.max(1, u * 0.05);
  ctx.beginPath();
  ctx.moveTo(-u * 1.06, 0); ctx.lineTo(u * 1.06, 0);
  ctx.lineTo(u * 1.3, u * 0.44); ctx.lineTo(-u * 1.3, u * 0.44);
  ctx.closePath(); ctx.fill(); ctx.stroke();
  // keyboard dot grid + the red TrackPoint
  ctx.fillStyle = "rgba(255,255,255,0.16)";
  for (let r = 0; r < 3; r++)
    for (let c = -4; c <= 4; c++)
      ctx.fillRect(c * u * 0.22 + r * u * 0.035 - u * 0.02, u * 0.09 + r * u * 0.1, u * 0.07, u * 0.045);
  ctx.fillStyle = LINE_RED;
  ctx.beginPath(); ctx.arc(0, u * 0.14, u * 0.045, 0, Math.PI * 2); ctx.fill();
  // lid: black bezel, near-black screen
  ctx.fillStyle = "#1b1b1f";
  ctx.strokeStyle = ink(0.75);
  ctx.fillRect(-u * 1.02, -u * 1.52, u * 2.04, u * 1.5);
  ctx.strokeRect(-u * 1.02, -u * 1.52, u * 2.04, u * 1.5);
  const sx = -u * 0.9, sy = -u * 1.42, sw = u * 1.8, sh = u * 1.28;
  ctx.fillStyle = "#0a0a10";
  ctx.fillRect(sx, sy, sw, sh);
  if (screenGlow) {
    ctx.fillStyle = rgba(screenGlow.c, 0.10 + screenGlow.a * 0.2);
    ctx.fillRect(sx, sy, sw, sh);
  }
  // the window: this seat's view INTO the virtual space — every voice
  // drawn at its azimuth relative to this seat, nearer = larger
  const sa = seatAngle(seat) + Math.PI; // machine faces the center
  for (let i = 0; i < S.lanes.length; i++) {
    const rel = voiceAngle(i) + spinAngle - sa;
    const depth = Math.cos(rel);
    if (depth < -0.35) continue;
    const px = sx + sw / 2 + Math.sin(rel) * sw * 0.42;
    const py = sy + sh * 0.24 + ((i * 37) % 100) / 100 * sh * 0.55;
    const pr = Math.max(0.8, (0.5 + depth * 0.5) * u * 0.12) * (1 + glow[i] * 1.3);
    ctx.fillStyle = rgba(S.lanes[i].color, 0.35 + glow[i] * 0.65);
    ctx.beginPath(); ctx.arc(px, py, pr, 0, Math.PI * 2); ctx.fill();
  }
  ctx.restore();
}

function rigSprite(p2, color, active) {
  const u = Math.max(10, 30 * p2.s / 140);
  ctx.save();
  ctx.translate(p2.x, p2.y);
  ctx.strokeStyle = ink(0.8);
  ctx.lineWidth = Math.max(1, u * 0.07);
  // splayed tripod legs
  ctx.beginPath();
  ctx.moveTo(0, -u * 0.4); ctx.lineTo(-u * 0.7, u * 0.5);
  ctx.moveTo(0, -u * 0.4); ctx.lineTo(u * 0.7, u * 0.5);
  ctx.moveTo(0, -u * 0.4); ctx.lineTo(0, u * 0.58);
  ctx.stroke();
  // cab
  ctx.fillStyle = "#2c2c31";
  ctx.beginPath();
  ctx.roundRect(-u * 0.62, -u * 1.9, u * 1.24, u * 1.62, u * 0.12);
  ctx.fill(); ctx.stroke();
  // drivers — woofer + tweeter, ring lit by the sounding lane
  ctx.strokeStyle = active ? rgba(active.c, 0.5 + active.a * 0.5) : "rgba(255,255,255,0.25)";
  ctx.lineWidth = Math.max(1, u * 0.1);
  ctx.beginPath(); ctx.arc(0, -u * 0.86, u * 0.34, 0, Math.PI * 2); ctx.stroke();
  ctx.beginPath(); ctx.arc(0, -u * 1.5, u * 0.16, 0, Math.PI * 2); ctx.stroke();
  ctx.restore();
}

// The listener's ears carry the live stereo image: each one blends the
// voices currently panned to its side, by the SAME law the audio baker
// uses (pan = sin(angle) — see nsscore-bake-audio.mjs), so the picture
// tells you what your headphones should be doing. L and R are labeled
// from the listener's own head, not the page.
let lastPaintT = 0;
function earMix() {
  const out = { l: [0, 0, 0, 0], r: [0, 0, 0, 0] };
  for (let i = 0; i < S.lanes.length; i++) {
    if (glow[i] < 0.02) continue;
    const pan = Math.sin(voiceAngle(i) + spinAngle + fieldAt(lastPaintT, "fieldShift", 0) * Math.PI);
    const wr = (pan + 1) / 2, wl = 1 - wr;
    const c = S.lanes[i].color;
    for (const [side, w] of [["l", wl], ["r", wr]]) {
      const k = glow[i] * w;
      out[side][0] += c[0] * k; out[side][1] += c[1] * k;
      out[side][2] += c[2] * k; out[side][3] += k;
    }
  }
  for (const side of ["l", "r"]) {
    const a = out[side][3];
    out[side] = a > 0.001
      ? [Math.round(out[side][0] / a), Math.round(out[side][1] / a), Math.round(out[side][2] / a), Math.min(1, a)]
      : [30, 30, 30, 0];
  }
  return out;
}

function palSprite(p2, t) {
  const u = Math.max(14, 46 * p2.s / 140);
  const mix = earMix();
  ctx.save();
  ctx.translate(p2.x, p2.y + Math.sin(t * 1.6) * u * 0.04);
  ctx.strokeStyle = ink(0.85);
  ctx.lineWidth = Math.max(1.2, u * 0.06);
  ctx.fillStyle = CREAM;
  // round body, bigger round head — paper pal
  ctx.beginPath(); ctx.ellipse(0, -u * 0.34, u * 0.62, u * 0.5, 0, 0, Math.PI * 2); ctx.fill(); ctx.stroke();
  ctx.beginPath(); ctx.arc(0, -u * 1.28, u * 0.62, 0, Math.PI * 2); ctx.fill(); ctx.stroke();
  // ears — filled with what each one is hearing, ringed in ink
  for (const [side, sx] of [["l", -1], ["r", 1]]) {
    const [er, eg, eb, ea] = mix[side];
    ctx.beginPath();
    ctx.arc(sx * u * 0.66, -u * 1.32, u * 0.2, 0, Math.PI * 2);
    ctx.fillStyle = ea > 0.02 ? rgba([er, eg, eb], 0.25 + ea * 0.75) : CREAM;
    ctx.fill();
    ctx.strokeStyle = ink(0.85);
    ctx.lineWidth = Math.max(1.2, u * 0.055);
    ctx.stroke();
    // the label rides the ear, so L is the listener's left
    ctx.fillStyle = ink(0.8);
    ctx.font = MONO(Math.max(9, u * 0.3), true);
    ctx.textAlign = "center";
    ctx.fillText(side.toUpperCase(), sx * u * 1.12, -u * 1.24);
    ctx.textAlign = "left";
  }
  // face: dot eyes + small open-mouth wonder
  ctx.fillStyle = INK;
  ctx.beginPath(); ctx.arc(-u * 0.2, -u * 1.32, u * 0.055, 0, Math.PI * 2); ctx.fill();
  ctx.beginPath(); ctx.arc(u * 0.2, -u * 1.32, u * 0.055, 0, Math.PI * 2); ctx.fill();
  ctx.beginPath(); ctx.arc(0, -u * 1.06, u * 0.09, 0, Math.PI * 2);
  ctx.strokeStyle = ink(0.85); ctx.lineWidth = Math.max(1, u * 0.045); ctx.stroke();
  ctx.restore();
}

// A headphone read-out beside the plan: two bars showing what each ear
// gets right now — the same mix the pal's ears wear.
function drawEarMeter(x, y, w) {
  const mix = earMix();
  ctx.fillStyle = ink(0.45);
  ctx.font = MONO(12);
  ctx.fillText("your headphones", x, y - 8);
  for (const [i, side] of ["l", "r"].entries()) {
    const [er, eg, eb, ea] = mix[side];
    const by = y + i * 18;
    ctx.fillStyle = ink(0.7);
    ctx.font = MONO(12, true);
    ctx.fillText(side.toUpperCase(), x, by + 10);
    ctx.fillStyle = ink(0.08);
    ctx.fillRect(x + 16, by, w, 12);
    ctx.fillStyle = rgba([er, eg, eb], 0.35 + ea * 0.65);
    ctx.fillRect(x + 16, by, Math.max(2, w * Math.min(1, ea)), 12);
    ctx.strokeStyle = ink(0.25);
    ctx.lineWidth = 1;
    ctx.strokeRect(x + 16, by, w, 12);
  }
}

// ── track data strip (scorodeon-kin, whole piece at once) ────────────
// scorodeon's discipline: the playhead holds still, the score flows past.
const STRIP_Y = 800, STRIP_H = 215, STRIP_X = 210, STRIP_W = W - STRIP_X - 60;
const ZOOM = 22;                 // seconds across the strip
const PH = 0.35;                 // playhead sits at 35% — more future than past
const MINI_H = 14;
const xOf = (tt, now) => STRIP_X + (PH + (tt - now) / ZOOM) * STRIP_W;

function drawStrip(t) {
  ctx.strokeStyle = ink(0.25);
  ctx.lineWidth = 1;
  ctx.beginPath(); ctx.moveTo(60, STRIP_Y - 26); ctx.lineTo(W - 60, STRIP_Y - 26); ctx.stroke();
  ctx.save();
  ctx.beginPath(); ctx.rect(STRIP_X, STRIP_Y - 26, STRIP_W, STRIP_H + 30); ctx.clip();

  if (S.rotation) { // the composed spin scrolls in the same system
    ctx.strokeStyle = "rgba(62,124,138,0.7)";
    ctx.lineWidth = 1.5;
    ctx.beginPath();
    let started = false;
    for (let i = 0; i < S.rotation.length; i++) {
      const rt = (i / (S.rotation.length - 1)) * S.dur;
      const x = xOf(rt, t);
      if (x < STRIP_X - 4 || x > STRIP_X + STRIP_W + 4) { started = false; continue; }
      const y = STRIP_Y - 6 - S.rotation[i] * 16;
      if (!started) { ctx.moveTo(x, y); started = true; } else ctx.lineTo(x, y);
    }
    ctx.stroke();
  }

  const lanesH = STRIP_H - MINI_H - 8;
  const laneH = Math.min(46, lanesH / S.lanes.length);
  const yBase = STRIP_Y + (lanesH - laneH * S.lanes.length) / 2;
  const tLo = t - PH * ZOOM - 1, tHi = t + (1 - PH) * ZOOM + 1;
  for (let i = 0; i < S.lanes.length; i++) {
    const y = yBase + i * laneH;
    const evs = S.lanes[i].events;
    // rolling window — time only moves forward, so never rescan the past
    while (stripLo[i] < evs.length && evs[stripLo[i]].t + evs[stripLo[i]].dur < tLo) stripLo[i]++;
    for (let j = stripLo[i]; j < evs.length && evs[j].t <= tHi; j++) {
      const e = evs[j];
      const x0 = xOf(e.t, t), x1 = xOf(e.t + e.dur, t);
      const sounding = t >= e.t && t < e.t + e.dur;
      ctx.fillStyle = rgba(S.lanes[i].color, (0.30 + Math.min(0.6, e.g)) * (sounding ? 1 : 0.82));
      ctx.fillRect(x0, y + laneH * 0.16, Math.max(2, x1 - x0), laneH * 0.68);
    }
  }
  for (const m of M) { // movement doors slide by with their numerals
    const x = xOf(m.t0, t);
    if (x < STRIP_X - 60 || x > STRIP_X + STRIP_W + 60) continue;
    ctx.strokeStyle = ink(0.5);
    ctx.beginPath(); ctx.moveTo(x, STRIP_Y - 4); ctx.lineTo(x, STRIP_Y + lanesH + 2); ctx.stroke();
    ctx.fillStyle = ink(0.55);
    ctx.font = MONO(14, true);
    ctx.fillText(m.name.split(" ")[0], x + 6, STRIP_Y + 12);
  }
  ctx.restore();

  // lane names hold the left gutter
  ctx.font = MONO(13);
  ctx.textAlign = "right";
  for (let i = 0; i < S.lanes.length; i++) {
    ctx.fillStyle = rgba(S.lanes[i].color, 0.9);
    ctx.fillText(S.lanes[i].name, STRIP_X - 10, yBase + i * laneH + laneH * 0.74);
  }
  ctx.textAlign = "left";

  // the fixed playhead + clock
  const px = STRIP_X + PH * STRIP_W;
  ctx.strokeStyle = LINE_RED;
  ctx.lineWidth = 2;
  ctx.beginPath(); ctx.moveTo(px, STRIP_Y - 14); ctx.lineTo(px, STRIP_Y + lanesH + 4); ctx.stroke();
  ctx.fillStyle = LINE_RED;
  ctx.font = MONO(13, true);
  const mm = Math.max(0, Math.min(S.dur, t));
  ctx.fillText(`${Math.floor(mm / 60)}:${String(Math.floor(mm % 60)).padStart(2, "0")}`, px + 6, STRIP_Y - 8);

  // minimap: the whole piece, the visible window, the position
  const my = STRIP_Y + lanesH + 8;
  ctx.fillStyle = ink(0.08);
  ctx.fillRect(STRIP_X, my, STRIP_W, MINI_H);
  for (const m of M) {
    const x = STRIP_X + (m.t0 / S.dur) * STRIP_W;
    ctx.strokeStyle = ink(0.35);
    ctx.lineWidth = 1;
    ctx.beginPath(); ctx.moveTo(x, my); ctx.lineTo(x, my + MINI_H); ctx.stroke();
  }
  const w0 = Math.max(0, (t - PH * ZOOM) / S.dur), w1 = Math.min(1, (t + (1 - PH) * ZOOM) / S.dur);
  ctx.fillStyle = ink(0.10);
  ctx.fillRect(STRIP_X + w0 * STRIP_W, my, Math.max(2, (w1 - w0) * STRIP_W), MINI_H);
  ctx.fillStyle = LINE_RED;
  ctx.fillRect(STRIP_X + Math.min(1, Math.max(0, t / S.dur)) * STRIP_W - 1, my - 2, 2, MINI_H + 4);
}

// ── plan view: the room top-down — seats, listener, emitter angle ────
function drawPlan(t, cx, cy, r) {
  ctx.strokeStyle = ink(0.35);
  ctx.lineWidth = 1.4;
  ctx.beginPath(); ctx.arc(cx, cy, r, 0, Math.PI * 2); ctx.stroke();
  ctx.strokeStyle = ink(0.12);
  ctx.beginPath(); ctx.arc(cx, cy, r * 0.55, 0, Math.PI * 2); ctx.stroke();
  // seats — tiny slabs, lit when owning a sounding voice
  for (let i = 0; i < SEATS; i++) {
    const a = seatAngle(i);
    const sx = cx + Math.cos(a) * r * 0.86, sy = cy + Math.sin(a) * r * 0.86;
    let lit = null;
    for (let v = 0; v < S.lanes.length; v++)
      if (laneSeat(v, t) === i && glow[v] > 0.08) lit = S.lanes[v].color;
    ctx.save();
    ctx.translate(sx, sy);
    ctx.rotate(a + Math.PI / 2);
    ctx.fillStyle = lit ? rgba(lit, 0.9) : "#26262a";
    ctx.fillRect(-6, -4, 12, 8);
    ctx.restore();
  }
  // the listener
  ctx.fillStyle = CREAM;
  ctx.strokeStyle = ink(0.7);
  ctx.beginPath(); ctx.arc(cx, cy, 5, 0, Math.PI * 2); ctx.fill(); ctx.stroke();
  const pm = earMix();
  for (const [side, sx] of [["l", -1], ["r", 1]]) {
    ctx.beginPath(); ctx.arc(cx + sx * 6.5, cy, 2.6, 0, Math.PI * 2);
    ctx.fillStyle = pm[side][3] > 0.02 ? rgba(pm[side].slice(0, 3), 0.35 + pm[side][3] * 0.65) : CREAM;
    ctx.fill(); ctx.stroke();
  }
  ctx.fillStyle = ink(0.55);
  ctx.font = MONO(9, true);
  ctx.textAlign = "center";
  ctx.fillText("L", cx - 13, cy + 3); ctx.fillText("R", cx + 13, cy + 3);
  ctx.textAlign = "left";
  ctx.fillStyle = INK;
  ctx.beginPath(); ctx.arc(cx - 1.8, cy - 1, 0.9, 0, Math.PI * 2); ctx.fill();
  ctx.beginPath(); ctx.arc(cx + 1.8, cy - 1, 0.9, 0, Math.PI * 2); ctx.fill();
  // voices at their true angles; the emitter drags a short trail
  const g = globeState(t);
  for (let i = 0; i < S.lanes.length; i++) {
    const a = voiceAngle(i) + spinAngle + fieldAt(t, "fieldShift", 0) * Math.PI;
    const vr = r * 0.86 * g.r * g.scale / 0.94
               * Math.max(0.3, Math.min(1.6, fieldAt(t, "fieldScale", 1)));
    const vx = cx + Math.cos(a) * vr, vy = cy + Math.sin(a) * vr;
    ctx.strokeStyle = rgba(S.lanes[i].color, 0.3);
    ctx.lineWidth = 2;
    ctx.beginPath(); ctx.arc(cx, cy, vr, a - 0.6, a); ctx.stroke();
    ctx.fillStyle = rgba(S.lanes[i].color, 0.55 + glow[i] * 0.45);
    ctx.beginPath(); ctx.arc(vx, vy, 3.5 + glow[i] * 3, 0, Math.PI * 2); ctx.fill();
  }
  ctx.fillStyle = ink(0.45);
  ctx.font = MONO(12);
  ctx.textAlign = "center";
  ctx.fillText("plan", cx, cy + r + 16);
  ctx.textAlign = "left";
}

// Side view: where the emitter sits above or below the ear line — the
// cue the pinna combs carry, drawn so you can check it against what you
// hear on headphones.
function drawElevation(t, cx, cy, h) {
  const el = elevationAt(t);
  ctx.strokeStyle = ink(0.25);
  ctx.lineWidth = 1;
  ctx.beginPath(); ctx.moveTo(cx - 26, cy); ctx.lineTo(cx + 26, cy); ctx.stroke(); // ear line
  ctx.setLineDash([2, 3]);
  ctx.beginPath(); ctx.moveTo(cx, cy - h); ctx.lineTo(cx, cy + h); ctx.stroke();
  ctx.setLineDash([]);
  // the listener in profile
  ctx.fillStyle = CREAM; ctx.strokeStyle = ink(0.7); ctx.lineWidth = 1.3;
  ctx.beginPath(); ctx.arc(cx, cy, 6, 0, Math.PI * 2); ctx.fill(); ctx.stroke();
  // the emitter at its height
  const ey = cy - el * h;
  const c = S.lanes[0].color, a = Math.max(...glow);
  ctx.fillStyle = rgba(c, 0.4 + a * 0.6);
  ctx.beginPath(); ctx.arc(cx + 16, ey, 3.5 + a * 3, 0, Math.PI * 2); ctx.fill();
  ctx.strokeStyle = rgba(c, 0.35);
  ctx.beginPath(); ctx.moveTo(cx + 6, cy); ctx.lineTo(cx + 16, ey); ctx.stroke();
  ctx.fillStyle = ink(0.45);
  ctx.font = MONO(11);
  ctx.textAlign = "center";
  ctx.fillText("above", cx, cy - h - 5);
  ctx.fillText("below", cx, cy + h + 13);
  ctx.fillText("elevation", cx, cy + h + 30);
  ctx.textAlign = "left";
}

// ── legend: the listening system, spelled out in one ink chain ───────
function drawLegend(x, y) {
  const label = (tx, s) => {
    ctx.fillStyle = ink(0.5);
    ctx.font = MONO(12);
    ctx.textAlign = "center";
    ctx.fillText(s, tx, y + 46);
    ctx.textAlign = "left";
  };
  const arrow = (ax) => {
    ctx.strokeStyle = ink(0.4);
    ctx.lineWidth = 1.4;
    ctx.beginPath(); ctx.moveTo(ax, y + 12); ctx.lineTo(ax + 26, y + 12); ctx.stroke();
    ctx.beginPath(); ctx.moveTo(ax + 20, y + 8); ctx.lineTo(ax + 26, y + 12); ctx.lineTo(ax + 20, y + 16); ctx.stroke();
  };
  ctx.lineWidth = 1.4;
  // the virtual space — wireframe circle with three voice dots
  ctx.strokeStyle = ink(0.6);
  ctx.beginPath(); ctx.arc(x, y + 10, 16, 0, Math.PI * 2); ctx.stroke();
  ctx.beginPath(); ctx.ellipse(x, y + 10, 16, 5.5, 0, 0, Math.PI * 2); ctx.stroke();
  for (const [dx, dy, c] of [[-6, 2, [255, 230, 0]], [7, 6, [77, 205, 196]], [2, 16, [130, 50, 200]]]) {
    ctx.fillStyle = rgba(c, 0.9);
    ctx.beginPath(); ctx.arc(x + dx, y + dy, 2.6, 0, Math.PI * 2); ctx.fill();
  }
  label(x, "virtual space");
  arrow(x + 26);
  // the window — thinkpad
  const tx = x + 108;
  ctx.fillStyle = "#1b1b1f";
  ctx.strokeStyle = ink(0.6);
  ctx.fillRect(tx - 14, y - 4, 28, 19); ctx.strokeRect(tx - 14, y - 4, 28, 19);
  ctx.beginPath(); ctx.moveTo(tx - 17, y + 22); ctx.lineTo(tx + 17, y + 22); ctx.lineTo(tx + 21, y + 27); ctx.lineTo(tx - 21, y + 27); ctx.closePath();
  ctx.fillStyle = "#26262a"; ctx.fill(); ctx.stroke();
  ctx.fillStyle = LINE_RED;
  ctx.beginPath(); ctx.arc(tx, y + 24.5, 1.6, 0, Math.PI * 2); ctx.fill();
  label(tx, "six windows");
  arrow(tx + 34);
  // the rig
  const rx = tx + 108;
  ctx.fillStyle = "#2c2c31";
  ctx.strokeStyle = ink(0.6);
  ctx.fillRect(rx - 9, y - 6, 18, 24); ctx.strokeRect(rx - 9, y - 6, 18, 24);
  ctx.beginPath(); ctx.arc(rx, y + 8, 4.6, 0, Math.PI * 2); ctx.stroke();
  ctx.beginPath(); ctx.arc(rx, y - 0.5, 2.3, 0, Math.PI * 2); ctx.stroke();
  ctx.beginPath(); ctx.moveTo(rx, y + 18); ctx.lineTo(rx - 7, y + 27); ctx.moveTo(rx, y + 18); ctx.lineTo(rx + 7, y + 27); ctx.stroke();
  label(rx, "room sound");
  arrow(rx + 30);
  // the listener pal
  const px = rx + 100;
  ctx.fillStyle = CREAM;
  ctx.strokeStyle = ink(0.7);
  ctx.beginPath(); ctx.ellipse(px, y + 18, 9, 7.4, 0, 0, Math.PI * 2); ctx.fill(); ctx.stroke();
  ctx.beginPath(); ctx.arc(px, y + 2, 9.4, 0, Math.PI * 2); ctx.fill(); ctx.stroke();
  const lm = earMix();
  for (const [side, sx] of [["l", -1], ["r", 1]]) {
    ctx.beginPath(); ctx.arc(px + sx * 10, y + 1, 3.2, 0, Math.PI * 2);
    ctx.fillStyle = lm[side][3] > 0.02 ? rgba(lm[side].slice(0, 3), 0.3 + lm[side][3] * 0.7) : CREAM;
    ctx.fill(); ctx.stroke();
  }
  ctx.fillStyle = INK;
  ctx.beginPath(); ctx.arc(px - 3, y + 1, 1.1, 0, Math.PI * 2); ctx.fill();
  ctx.beginPath(); ctx.arc(px + 3, y + 1, 1.1, 0, Math.PI * 2); ctx.fill();
  label(px, "the listener");
}

// ── frame ────────────────────────────────────────────────────────────
let titleImg = null;
function drawFrame(t) {
  spinAngle += spinRate(t) * (t - lastT); lastT = t; lastPaintT = t;

  for (let i = 0; i < S.lanes.length; i++) {
    const evs = S.lanes[i].events;
    while (cursors[i] < evs.length && evs[cursors[i]].t <= t) {
      const e = evs[cursors[i]++];
      if (e.g > 0.06) ripples.push({ born: t, seat: laneSeat(i, t), color: S.lanes[i].color, g: e.g });
      glow[i] = Math.min(1, glow[i] + e.g);
    }
    glow[i] *= 0.90;
  }
  while (ripples.length && t - ripples[0].born > 1.5) ripples.shift();

  ctx.fillStyle = CREAM;
  ctx.fillRect(0, 0, W, H);

  // floor: ink rings + seat ticks
  circle3d(0, 0, 0, RING * 1.32, "y", t, ink(0.14), 1.5);
  circle3d(0, 0, 0, RING, "y", t, ink(0.22), 1.5);
  circle3d(0, 0, 0, RING * 0.45, "y", t, ink(0.1), 1);

  // floor ripples — sound leaving the machines
  for (const r of ripples) {
    const age = (t - r.born) / 1.5;
    const [sx, , sz] = seatPos(r.seat);
    circle3d(sx * 0.92, 0.01, sz * 0.92, 0.12 + age * 1.7, "y", t,
             rgba(r.color, (1 - age) * 0.5 * Math.min(1, r.g * 1.5)), 2);
  }

  // the virtual acoustic field — a dome registered on the room itself
  const g = globeState(t);
  const gr = RING * g.r * g.scale * Math.max(0.3, Math.min(2.0, fieldAt(t, "fieldScale", 1)));
  if (gr > 0.1) {
    circle3d(0, g.cy, 0, gr, "y", t, ink(0.26), 1.5);          // the orbit
    circle3d(0, g.cy + 0.75, 0, gr * 0.8, "y", t, ink(0.13), 1);
    circle3d(0, g.cy + 1.4, 0, gr * 0.45, "y", t, ink(0.09), 1); // dome cap
  }

  // sight-lines: voice → its machine (the choreography made visible)
  for (let i = 0; i < S.lanes.length; i++) {
    const vp = project(voicePos(i, t), t);
    const seat = laneSeat(i, t);
    const sp = project(seatPos(seat).map((v, k) => k === 1 ? 0.5 : v * 0.98), t);
    if (!vp || !sp) continue;
    ctx.strokeStyle = rgba(S.lanes[i].color, 0.05 + glow[i] * 0.4);
    ctx.lineWidth = 1 + glow[i] * 1.5;
    ctx.beginPath();
    ctx.moveTo(vp.x, vp.y);
    ctx.quadraticCurveTo((vp.x + sp.x) / 2, Math.min(vp.y, sp.y) - 30, sp.x, sp.y);
    ctx.stroke();
  }

  // depth-sorted: thinkpads + rigs + pal + voices
  const items = [];
  for (let s = 0; s < SEATS; s++) {
    const pos = seatPos(s);
    const p2 = project(pos, t);
    if (!p2) continue;
    let best = null, bl = 0.05;
    for (let i = 0; i < S.lanes.length; i++)
      if (laneSeat(i, t) === s && glow[i] > bl) { best = { c: S.lanes[i].color, a: glow[i] }; bl = glow[i]; }
    items.push({ d: p2.d, draw: () => thinkpadSprite(p2, s, t, best) });
    const ra = seatAngle(s) + 0.42;
    const rp = project([Math.cos(ra) * RING * 1.12, 0, Math.sin(ra) * RING * 1.12], t);
    if (rp) items.push({ d: rp.d, draw: () => rigSprite(rp, null, best) });
  }
  const lp = project([0, 0, 0], t);
  if (lp) items.push({ d: lp.d, draw: () => palSprite(lp, t) });
  for (let i = 0; i < S.lanes.length; i++) {
    const p3 = voicePos(i, t);
    const p2 = project(p3, t);
    if (!p2 || gr <= 0.03) continue;
    const c = S.lanes[i].color;
    const solo = S.lanes.length <= 8 ? 1.7 : 1;
    const r = Math.max(2, (4.5 + glow[i] * 8) * solo * p2.s / 140);
    items.push({ d: p2.d, draw: () => {
      ctx.fillStyle = rgba(c, 0.45 + glow[i] * 0.55);
      ctx.beginPath(); ctx.arc(p2.x, p2.y, r, 0, Math.PI * 2); ctx.fill();
      ctx.strokeStyle = ink(0.35);
      ctx.lineWidth = 1;
      ctx.stroke();
      if (glow[i] > 0.3) {
        ctx.strokeStyle = rgba(c, glow[i] * 0.45);
        ctx.beginPath(); ctx.arc(p2.x, p2.y, r * 1.8, 0, Math.PI * 2); ctx.stroke();
      }
    }});
  }
  items.sort((a, b) => b.d - a.d);
  for (const it of items) it.draw();

  // ── chrome ──
  if (titleImg) ctx.drawImage(titleImg, 60, 44, titleImg.width * 0.5, titleImg.height * 0.5);
  const mv = M.find(m => t >= m.t0 && t < m.t1);
  ctx.fillStyle = ink(0.85);
  ctx.font = MONO(22, true);
  if (mv) ctx.fillText(mv.name + " — " + mv.sub + (S.lanes.length > 1 ? " · " +
    ["enter", "antiphony", "orbit", "scatter", "converge", "rest"][M.indexOf(mv)] : ""), 62, 150);
  ctx.fillStyle = ink(0.45);
  ctx.font = MONO(16);
  ctx.textAlign = "right";
  const mins = `${Math.floor(S.dur / 60)}:${String(Math.round(S.dur % 60)).padStart(2, "0")}`;
  ctx.fillText(S.lanes.length > 1
    ? `special sign · ${mins} → 8–12 min live via held movements`
    : `${S.name} · ${mins} · one emitter orbiting the field`, W - 60, 70);
  ctx.textAlign = "right";
  ctx.fillStyle = ink(0.35);
  ctx.font = MONO(14);
  ctx.fillText("binaural · ITD + head shadow + pinna elevation · headphones", W - 60, 92);
  ctx.textAlign = "left";
  ctx.textAlign = "left";
  drawLegend(1278, 108);

  drawPlan(t, 132, 606, 58);
  if (S.elevation) drawElevation(t, 268, 606, 44);
  drawEarMeter(78, 742, 96);
  drawStrip(t);
}

async function main() {
  titleImg = await magickRenderText("Note(s)pat(ial) Native", {
    ptSize: 88, fill: INK, font: YWFT_PATH, outPath: "/tmp/notespatial-title.png",
  });

  if (still != null) {
    // settle spin + glows by simulating up to the still time
    for (let t = 0; t <= still; t += 1 / FPS) drawFrame(t);
    writeFileSync(outPath, canvas.toBuffer("image/png"));
    console.log("wrote " + outPath);
    return;
  }

  const ff = spawn("ffmpeg", [
    "-y", "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS),
    "-i", "-", "-i", audioPath,
    "-map", "0:v", "-map", "1:a", "-c:v", "libx264", "-preset", "veryfast", "-crf", "19",
    "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "192k", "-shortest", outPath,
  ], { stdio: ["pipe", "ignore", "inherit"] });
  const write = (buf) => new Promise(res => ff.stdin.write(buf) ? res() : ff.stdin.once("drain", res));

  console.log(`rendering ${FRAMES} frames…`);
  for (let f = 0; f < FRAMES; f++) {
    drawFrame(f / FPS);
    await write(canvas.toBuffer("raw"));
    if (f % 300 === 0) console.log(`  ${f}/${FRAMES}`);
  }
  ff.stdin.end();
  await new Promise(res => ff.on("close", res));
  console.log("wrote " + outPath);
}

main();
