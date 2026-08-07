#!/usr/bin/env node
// score-video.mjs — a graphic score for wattajetta stone club. Reads the
// composer's own event export (out/wattajetta-stone-club-score.json, written
// by render-wattajetta.mjs --score-json) and animates it as a scrolling
// analytical score synced to the audition mp3: pitch on Y, time on X, a
// fixed centre playhead the music slides under, section bands across the top,
// the pitched voices as coloured marks (bells by lane, uke, flyby counterline,
// sub), and the mathematical drum grid as a density strip along the bottom.
//
//   node pop/wattajetta/bin/render-wattajetta.mjs --stone-club --score-json
//   node pop/wattajetta/bin/score-video.mjs            → out/wattajetta-stone-club-score.mp4
//   node pop/wattajetta/bin/score-video.mjs --fast     → 24 fps preview
//
// No samples, no AI: deterministic canvas frames piped to ffmpeg with the
// real audio muxed on top.

import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas } from "canvas";
import { spawnFFmpegEncode } from "../../lib/preview-shared.mjs";
import { registerFont } from "canvas";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../out");
const SCORE = JSON.parse(readFileSync(resolve(OUT, "wattajetta-stone-club-score.json")));
const AUDIO = resolve(OUT, "wattajetta-stone-club-audition.mp3");
const MP4 = resolve(OUT, "wattajetta-stone-club-score.mp4");

// node-canvas can't parse the YWFT OpenType cmap (renders tofu), so the score
// uses Menlo — a clean mono that suits an analytical grid anyway.
try { registerFont("/System/Library/Fonts/Menlo.ttc", { family: "Menlo" }); } catch {}
const FONT = "Menlo, monospace";

const W = 1920, H = 1080;
const FPS = process.argv.includes("--fast") ? 24 : 60;

// ── tempo warp: composed event time → playback (heard) time ─────────────
// The audio is resampled by rate(t)=0.92+0.08·smoothstep(t/DUR) after events
// are placed, so a mark composed at time τ actually sounds at output time t
// where cumInput(t)=τ. Invert that table once; every coordinate below maps
// through warp() so the score locks to the ear.
const TM = SCORE.tempoMap;
function warp(tau) {
  const a = TM.cumInput, hi = a.length - 1;
  if (tau <= 0) return 0;
  if (tau >= a[hi]) return hi * TM.dt;
  let lo = 0, h = hi;
  while (lo + 1 < h) { const mid = (lo + h) >> 1; if (a[mid] <= tau) lo = mid; else h = mid; }
  const frac = (tau - a[lo]) / ((a[h] - a[lo]) || 1);
  return (lo + frac) * TM.dt;
}
const PLAY_DUR = (TM.cumInput.length - 1) * TM.dt; // output timeline length

const WINDOW = 8 * SCORE.transport.bar;         // seconds visible across the width
const PLAYHEAD_X = W * 0.32;                     // fixed; time flows right→left under it
const pxPerSec = W / WINDOW;
const tToX = (t, now) => PLAYHEAD_X + (t - now) * pxPerSec;

// ── vertical layout ───────────────────────────────────────────────────
const TOP = 118;            // section band height
const BOT = H - 150;        // above the drum strip
const PITCH_LO = 40, PITCH_HI = 81;             // sub E1 region → top of the line
const midiToY = (m) => BOT - ((m - PITCH_LO) / (PITCH_HI - PITCH_LO)) * (BOT - TOP);

// ── palette: a full spectrum so every voice and lane is its own colour ──
const LANE_RGB = {
  melody:   [255, 208, 120],   // the granite line — warm gold
  ornament: [242, 138, 168],   // flams/diddles/rolls — rose
  ring:     [120, 214, 255],   // long-decay bodies — glacial blue
  bowl:     [168, 132, 232],   // low anchors — violet
  toll:     [255, 110, 74],    // church tolls — ember red
};
const UKE_RGB = [150, 235, 110];      // nylon string — lime
const FLY_RGB = [255, 96, 200];       // jet counterline — magenta
const SUB_RGB = [86, 150, 196];       // fuselage — teal
const DISCO_RGB = [255, 150, 40];     // disco bass — hot tangerine
LANE_RGB.quat = [120, 255, 236];      // quaternion-rotated motif — bright cyan-mint
const SECTION_RGB = {
  intro:  [58, 74, 118], verse: [64, 104, 128],
  chorus: [176, 104, 72], bridge: [56, 116, 128], outro: [104, 78, 132],
};
// register tint bands — bass / mid / treble washes so the field is never bare
const REGISTER_BANDS = [
  { lo: 40, hi: 55, rgb: [60, 80, 130] },   // bass — indigo
  { lo: 55, hi: 67, rgb: [70, 108, 96] },   // mid — teal-green
  { lo: 67, hi: 82, rgb: [120, 92, 70] },   // treble — warm
];
const rgb = ([r, g, b], a = 1) => `rgba(${r},${g},${b},${a})`;

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");

// pitch class of the E-minor-pentatonic set, for faint staff lines
const PENTA = new Set([4, 7, 9, 11, 2]); // E G A B D

// ── pre-warp every event to played time (tp) + played end (te) so nothing
//    is computed per-frame and everything shares one clock ───────────────
const warpEvents = (arr) => arr.map((e) => {
  const tp = warp(e.t);
  return { ...e, tp, te: warp(e.t + (e.dur ?? 0)) };
}).sort((a, b) => a.tp - b.tp);
const BELLS = warpEvents(SCORE.bells);
const UKE = warpEvents(SCORE.uke);
const FLYBY = warpEvents(SCORE.flyby);
const SUB = warpEvents(SCORE.sub);
const DISCO = warpEvents(SCORE.disco ?? []);
const KICKS = SCORE.kicks.map(warp).sort((a, b) => a - b);
// drum grid: each composed 16th-step bin remapped to its played span
const DRUM = SCORE.drumDensity.map((d, i) => ({
  d, x0: warp(i * SCORE.drumStep), x1: warp((i + 1) * SCORE.drumStep),
})).filter((b) => b.d > 0);
const SECTIONS = SCORE.sections.map((s) => ({
  ...s, pStart: warp(s.tStart), pEnd: warp(s.tEnd),
}));
const SPINS = (SCORE.spinWindows ?? []).map((w) => ({
  ...w, pStart: warp(w.tStart), pEnd: warp(w.tEnd),
}));
// the through-composed melody line, in played order, for the ribbon
const MELODY_LINE = SCORE.bells.filter((b) => b.lane === "melody")
  .map((b) => ({ tp: warp(b.t), midi: b.midi })).sort((a, b) => a.tp - b.tp);
const QUAT_LINE = SCORE.bells.filter((b) => b.lane === "quat")
  .map((b) => ({ tp: warp(b.t), midi: b.midi })).sort((a, b) => a.tp - b.tp);

// which bar index sits at a given played time (for warped bar lines)
const barLineTimes = [];
for (let b = 0; b <= SCORE.transport.bars; b++) barLineTimes.push({ b, p: warp(b * SCORE.transport.bar) });
const dy0 = BOT + 18, dh = 78;      // drum strip band (kept clear of the title row)

function drawFrame(now) {
  // backdrop — a slight vertical gradient, granite dusk
  const g = ctx.createLinearGradient(0, 0, 0, H);
  g.addColorStop(0, "#0b0d12"); g.addColorStop(1, "#05060a");
  ctx.fillStyle = g; ctx.fillRect(0, 0, W, H);

  const tL = now - PLAYHEAD_X / pxPerSec;              // played time at left edge
  const tR = now + (W - PLAYHEAD_X) / pxPerSec;        // …at right edge
  const vis = (p, d = 0) => p + d > tL - 1 && p < tR + 1;

  // ── register tint bands: bass / mid / treble washes so no lane is bare ─
  for (const band of REGISTER_BANDS) {
    const y0 = midiToY(band.hi), y1 = midiToY(band.lo);
    ctx.fillStyle = rgb(band.rgb, 0.06);
    ctx.fillRect(0, y0, W, y1 - y0);
  }
  // ── section colour wash across the whole field (structure = colour) ────
  for (const s of SECTIONS) {
    const x0 = tToX(s.pStart, now), x1 = tToX(s.pEnd, now);
    if (x1 < 0 || x0 > W) continue;
    ctx.fillStyle = rgb(SECTION_RGB[s.name] ?? [60, 60, 80], 0.13);
    ctx.fillRect(Math.max(0, x0), TOP, Math.min(W, x1) - Math.max(0, x0), BOT - TOP);
  }
  // ── spin-window bands: where the mix physically rotates, a live ⟳ tint ─
  for (const sw of SPINS) {
    const x0 = tToX(sw.pStart, now), x1 = tToX(sw.pEnd, now);
    if (x1 < 0 || x0 > W) continue;
    const spinning = now >= sw.pStart && now < sw.pEnd;
    ctx.fillStyle = rgb([140, 120, 255], spinning ? 0.14 : 0.07);
    ctx.fillRect(Math.max(0, x0), TOP, Math.min(W, x1) - Math.max(0, x0), 30);
    ctx.fillStyle = rgb([200, 190, 255], 0.8);
    ctx.font = `600 20px ${FONT}`;
    ctx.textBaseline = "middle";
    if (x1 > 40 && x0 < W - 40)
      ctx.fillText(`⟳ SPIN ×${Math.abs(sw.turns)}`, Math.max(x0 + 10, 10), TOP + 15);
  }

  // ── pitch guide lines: pentatonic rows glow, others hairline. A row
  //    LIGHTS UP while any note at that pitch is currently sounding. ──────
  const litRows = new Map(); // midi → brightness 0..1
  const noteLight = (ev, col) => {
    if (now >= ev.tp && now < ev.te) {
      const life = 1 - (now - ev.tp) / Math.max(0.05, ev.te - ev.tp);
      const m = Math.round(ev.midi);
      const b = Math.max(litRows.get(m)?.b ?? 0, 0.25 + 0.75 * life);
      litRows.set(m, { b, col });
    }
  };
  for (const s of BELLS) if (vis(s.tp, s.te - s.tp)) noteLight(s, LANE_RGB[s.lane] ?? LANE_RGB.melody);
  for (const n of UKE) if (vis(n.tp, n.te - n.tp)) noteLight(n, UKE_RGB);
  for (const n of FLYBY) if (vis(n.tp, n.te - n.tp)) noteLight(n, FLY_RGB);
  for (const n of SUB) if (vis(n.tp, n.te - n.tp)) noteLight({ ...n, midi: Math.max(PITCH_LO, n.midi) }, SUB_RGB);
  for (const n of DISCO) if (now >= n.tp && now < n.tp + 0.16) noteLight({ ...n, te: n.tp + 0.16, midi: Math.max(PITCH_LO, n.midi) }, DISCO_RGB);

  for (let m = PITCH_LO; m <= PITCH_HI; m++) {
    const y = midiToY(m);
    const on = PENTA.has(((m % 12) + 12) % 12);
    const lit = litRows.get(m);
    if (lit) {
      ctx.strokeStyle = rgb(lit.col, 0.10 + 0.30 * lit.b);
      ctx.lineWidth = 1 + 1.5 * lit.b;
    } else {
      ctx.strokeStyle = on ? "rgba(120,140,180,0.16)" : "rgba(90,100,130,0.05)";
      ctx.lineWidth = 1;
    }
    ctx.beginPath(); ctx.moveTo(0, y); ctx.lineTo(W, y); ctx.stroke();
  }

  // ── bar lines flowing under the playhead (warped to played time) ──────
  for (const { b, p } of barLineTimes) {
    if (!vis(p)) continue;
    const x = tToX(p, now);
    ctx.strokeStyle = b % 4 === 0 ? "rgba(150,170,210,0.22)" : "rgba(120,140,180,0.08)";
    ctx.lineWidth = b % 4 === 0 ? 1.5 : 1;
    ctx.beginPath(); ctx.moveTo(x, TOP); ctx.lineTo(x, BOT); ctx.stroke();
  }

  // ── section bands across the top ──────────────────────────────────────
  for (const s of SECTIONS) {
    const x0 = tToX(s.pStart, now), x1 = tToX(s.pEnd, now);
    if (x1 < 0 || x0 > W) continue;
    ctx.fillStyle = rgb(SECTION_RGB[s.name] ?? [60, 60, 80], 0.9);
    ctx.fillRect(x0, 0, x1 - x0, TOP - 8);
    ctx.fillStyle = "rgba(0,0,0,0.25)";
    ctx.fillRect(x0, TOP - 8, x1 - x0, 8);
    ctx.fillStyle = "rgba(255,255,255,0.92)";
    ctx.font = `600 34px ${FONT}`;
    ctx.textBaseline = "middle";
    // pin the label so it stays readable while the band scrolls off-left
    const lx = Math.max(x0 + 18, Math.min(18, x1 - 130));
    if (x1 > 30 && x0 < W - 30) ctx.fillText(s.name.toUpperCase(), lx, TOP / 2);
  }

  // ── sub (fuselage) — thick low bars ───────────────────────────────────
  for (const n of SUB) {
    if (!vis(n.tp, n.te - n.tp)) continue;
    const x = tToX(n.tp, now), x2 = tToX(n.te, now);
    const y = midiToY(Math.max(PITCH_LO, n.midi));
    const sounding = now >= n.tp && now < n.te;
    ctx.fillStyle = rgb(SUB_RGB, sounding ? 0.85 : 0.5);
    ctx.fillRect(x, y - 5, x2 - x, 10);
  }

  // ── disco bass — punchy staccato blocks in the bass register; accents
  //    are taller/brighter, octave-jumps read as the vertical bounce ──────
  for (const n of DISCO) {
    const dur = 0.14;
    if (!vis(n.tp, dur)) continue;
    const x = tToX(n.tp, now), w = Math.max(5, dur * pxPerSec);
    const y = midiToY(Math.max(PITCH_LO, n.midi));
    const struck = now >= n.tp, age = now - n.tp;
    const hh = n.accent ? 15 : 10;
    const a = struck ? Math.max(0.2, 1 - age / 0.22) : 0.5;
    ctx.fillStyle = rgb(DISCO_RGB, a * (n.accent ? 1 : 0.8));
    ctx.fillRect(x, y - hh / 2, w, hh);
    if (struck && age < 0.14) {
      ctx.globalAlpha = (1 - age / 0.14) * 0.8;
      ctx.fillStyle = rgb(DISCO_RGB, 1);
      ctx.beginPath(); ctx.arc(x, y, 5 + (n.accent ? 16 : 10) * (age / 0.14), 0, Math.PI * 2); ctx.fill();
      ctx.globalAlpha = 1;
    }
  }

  // ── flyby counter-melody — a bright connected line with node dots ──────
  ctx.strokeStyle = rgb(FLY_RGB, 0.85); ctx.lineWidth = 3;
  ctx.beginPath();
  let started = false;
  for (const n of FLYBY) {
    const x = tToX(n.tp, now), y = midiToY(n.midi);
    if (x < -50 || x > W + 50) { started = false; continue; }
    if (!started) { ctx.moveTo(x, y); started = true; } else ctx.lineTo(x, y);
  }
  ctx.stroke();
  for (const n of FLYBY) {
    const x = tToX(n.tp, now), y = midiToY(n.midi);
    if (x < -20 || x > W + 20) continue;
    const sounding = now >= n.tp && now < n.te;
    if (sounding) { ctx.fillStyle = rgb(FLY_RGB, 0.3); ctx.beginPath(); ctx.arc(x, y, 16, 0, Math.PI * 2); ctx.fill(); }
    ctx.fillStyle = rgb(FLY_RGB, 0.95);
    ctx.beginPath(); ctx.arc(x, y, sounding ? 8 : 5, 0, Math.PI * 2); ctx.fill();
  }

  // ── uke — soft plucked diamonds with decay whiskers ───────────────────
  for (const n of UKE) {
    if (!vis(n.tp, n.te - n.tp)) continue;
    const x = tToX(n.tp, now);
    const y = midiToY(n.midi);
    const life = Math.max(0, 1 - Math.max(0, now - n.tp) / Math.max(0.5, n.te - n.tp));
    ctx.globalAlpha = 0.22 + 0.5 * life;
    ctx.strokeStyle = rgb(UKE_RGB, 0.15 * life);
    ctx.lineWidth = 2;
    ctx.beginPath(); ctx.moveTo(x, y); ctx.lineTo(tToX(n.te, now), y); ctx.stroke();
    const sounding = now >= n.tp && now < n.te;
    if (sounding && now - n.tp < 0.4) {
      ctx.globalAlpha = (1 - (now - n.tp) / 0.4) * 0.7;
      ctx.fillStyle = rgb(UKE_RGB, 1);
      ctx.beginPath(); ctx.arc(x, y, 4 + 14 * (now - n.tp) / 0.4, 0, Math.PI * 2); ctx.fill();
    }
    ctx.globalAlpha = 0.22 + 0.5 * life;
    ctx.fillStyle = rgb(UKE_RGB, 1);
    const r = sounding ? 6 : 5;
    ctx.beginPath();
    ctx.moveTo(x, y - r); ctx.lineTo(x + r, y); ctx.lineTo(x, y + r); ctx.lineTo(x - r, y);
    ctx.closePath(); ctx.fill();
    ctx.globalAlpha = 1;
  }

  // ── melody + quaternion ribbons: connect the composed lines so the
  //    contour reads as a continuous shape, not scattered dots ───────────
  const ribbon = (line, col, width, alpha) => {
    ctx.strokeStyle = rgb(col, alpha); ctx.lineWidth = width;
    ctx.lineJoin = "round"; ctx.beginPath();
    let on = false;
    for (const n of line) {
      const x = tToX(n.tp, now);
      if (x < -60 || x > W + 60) { on = false; continue; }
      const y = midiToY(n.midi);
      if (!on) { ctx.moveTo(x, y); on = true; } else ctx.lineTo(x, y);
    }
    ctx.stroke();
  };
  ribbon(MELODY_LINE, LANE_RGB.melody, 3, 0.4);
  ribbon(QUAT_LINE, LANE_RGB.quat, 3, 0.55);

  // ── bells — the score's main body. Each mark shows its DURATION as a
  //    thick sustain bar (length = decay), a bright head at the strike, and
  //    a bloom + glow while it rings. Height encodes velocity. ───────────
  for (const s of BELLS) {
    const w = Math.max(6, (s.te - s.tp) * pxPerSec);
    if (!vis(s.tp, s.te - s.tp)) continue;
    const x = tToX(s.tp, now);
    const y = midiToY(s.midi);
    const col = LANE_RGB[s.lane] ?? LANE_RGB.melody;
    const hh = 5 + s.vel * 18;                    // thicker, clearer
    const struck = now >= s.tp;
    const age = now - s.tp;
    const sounding = struck && now < s.te;
    const ringLife = sounding ? Math.max(0, 1 - age / Math.max(0.3, s.te - s.tp)) : 0;
    // the full-duration sustain bar — flat body, so length is legible
    const bodyA = (struck ? Math.max(0.12, 1 - age / Math.max(0.6, s.te - s.tp)) : 0.5)
      * (s.lane === "ring" || s.lane === "quat" ? 0.6 : 0.42);
    ctx.fillStyle = rgb(col, bodyA + 0.3 * ringLife);
    ctx.fillRect(x, y - hh / 2, w, hh);
    // a soft rounded cap on the tail end so decays read as fading, not cut
    const grad = ctx.createLinearGradient(x + w * 0.5, 0, x + w, 0);
    grad.addColorStop(0, rgb(col, 0)); grad.addColorStop(1, rgb([5, 6, 10], 0.55));
    ctx.fillStyle = grad; ctx.fillRect(x + w * 0.5, y - hh / 2, w * 0.5, hh);
    // the strike head — a bright block at the attack
    const headA = struck ? Math.max(0.55, 1 - age * 1.2) : 0.9;
    ctx.fillStyle = rgb(col, Math.min(1, headA + 0.4 * ringLife));
    ctx.fillRect(x - 2, y - hh / 2 - 1.5, 6, hh + 3);
    // bloom flash right as it is struck under the playhead
    if (struck && age < 0.18) {
      ctx.globalAlpha = (1 - age / 0.18) * 0.9;
      ctx.fillStyle = rgb(col, 1);
      ctx.beginPath(); ctx.arc(x, y, 6 + s.vel * 24 * (age / 0.18), 0, Math.PI * 2); ctx.fill();
      ctx.globalAlpha = 1;
    }
  }

  // ── kicks: a heartbeat lane just above the drum strip, pulsing on hit ──
  const kickY = dy0 - 12;
  for (const kt of KICKS) {
    if (!vis(kt, 0.25)) continue;
    const x = tToX(kt, now);
    const age = now - kt;
    const on = age >= 0 && age < 0.16;
    ctx.fillStyle = on ? `rgba(255,${120 + 80 * (1 - age / 0.16)},120,${0.9})` : "rgba(120,90,110,0.5)";
    const r = on ? 5 + 10 * (1 - age / 0.16) : 3.5;
    ctx.beginPath(); ctx.arc(x, kickY, r, 0, Math.PI * 2); ctx.fill();
  }

  // ── drum grid density strip along the bottom (warped bins) ────────────
  ctx.fillStyle = "rgba(255,255,255,0.04)";
  ctx.fillRect(0, dy0, W, dh);
  const maxD = 4;
  for (const bin of DRUM) {
    if (!vis(bin.x0, bin.x1 - bin.x0)) continue;
    const x = tToX(bin.x0, now), xw = Math.max(1, (bin.x1 - bin.x0) * pxPerSec - 1);
    const norm = Math.min(1, bin.d / maxD);
    const hgt = 6 + norm * (dh - 10);
    const hot = 120 + norm * 135;
    // brighten the bin as it passes under the playhead
    const near = Math.abs(tToX(bin.x0, now) - PLAYHEAD_X) < 14;
    ctx.fillStyle = near
      ? `rgba(255,${210 - norm * 60},150,${0.7 + 0.3 * norm})`
      : `rgba(${hot},${180 - norm * 90},${90},${0.35 + 0.5 * norm})`;
    ctx.fillRect(x, dy0 + dh - hgt, xw, hgt);
  }
  // strip label sits inside the band, dim, so it never collides with the title
  ctx.fillStyle = "rgba(200,215,240,0.35)";
  ctx.font = `500 16px ${FONT}`;
  ctx.textBaseline = "middle";
  ctx.fillText("KICK · EUCLIDEAN · POLYMETER · FIBONACCI DRUM GRID", 20, dy0 + 13);

  // ── stereo / spin radar (top-right): the spatial field made visible.
  //    Each sounding voice is a dot placed by its pan (x) and pitch (y);
  //    during a spin window a quaternion sweep rotates and the ring pulses. ─
  const cx = W - 150, cy = TOP + 150, R = 96;
  let spinPhase = null, spinTurns = 0;
  for (const sw of SPINS) if (now >= sw.pStart && now < sw.pEnd) {
    spinPhase = 2 * Math.PI * sw.turns * ((now - sw.pStart) / (sw.pEnd - sw.pStart));
    spinTurns = sw.turns;
  }
  ctx.save();
  // panel
  ctx.fillStyle = "rgba(10,12,20,0.55)";
  ctx.beginPath(); ctx.arc(cx, cy, R + 16, 0, Math.PI * 2); ctx.fill();
  ctx.strokeStyle = spinPhase != null ? "rgba(160,150,255,0.8)" : "rgba(120,140,180,0.4)";
  ctx.lineWidth = spinPhase != null ? 3 : 1.5;
  ctx.beginPath(); ctx.arc(cx, cy, R, 0, Math.PI * 2); ctx.stroke();
  ctx.strokeStyle = "rgba(120,140,180,0.2)"; ctx.lineWidth = 1;
  ctx.beginPath(); ctx.moveTo(cx - R, cy); ctx.lineTo(cx + R, cy); ctx.stroke();
  ctx.beginPath(); ctx.moveTo(cx, cy - R); ctx.lineTo(cx, cy + R); ctx.stroke();
  // spin sweep
  if (spinPhase != null) {
    ctx.strokeStyle = "rgba(180,170,255,0.9)"; ctx.lineWidth = 2.5;
    ctx.beginPath(); ctx.moveTo(cx, cy);
    ctx.lineTo(cx + Math.cos(spinPhase - Math.PI / 2) * R, cy + Math.sin(spinPhase - Math.PI / 2) * R);
    ctx.stroke();
    ctx.fillStyle = "rgba(200,190,255,0.95)"; ctx.font = `600 20px ${FONT}`;
    ctx.textAlign = "center"; ctx.textBaseline = "middle";
    ctx.fillText(`⟳ ×${Math.abs(spinTurns)}`, cx, cy - R - 30);
    ctx.textAlign = "left";
  }
  // sounding voices as dots (pan → x, pitch → y)
  const plot = (ev, col) => {
    if (!(now >= ev.tp && now < ev.te)) return;
    const life = 1 - (now - ev.tp) / Math.max(0.1, ev.te - ev.tp);
    const pan = ev.pan ?? 0;
    const px = cx + pan * R * 0.92;
    const py = cy - ((Math.max(PITCH_LO, Math.min(PITCH_HI, ev.midi)) - PITCH_LO) / (PITCH_HI - PITCH_LO) - 0.5) * 1.7 * R;
    ctx.fillStyle = rgb(col, 0.35 + 0.6 * life);
    ctx.beginPath(); ctx.arc(px, py, 3 + 5 * life, 0, Math.PI * 2); ctx.fill();
  };
  for (const s of BELLS) if (vis(s.tp, s.te - s.tp)) plot(s, LANE_RGB[s.lane] ?? LANE_RGB.melody);
  for (const n of UKE) if (vis(n.tp, n.te - n.tp)) plot(n, UKE_RGB);
  for (const n of FLYBY) if (vis(n.tp, n.te - n.tp)) plot(n, FLY_RGB);
  for (const n of DISCO) if (now >= n.tp && now < n.tp + 0.16) plot({ ...n, te: n.tp + 0.16, pan: 0 }, DISCO_RGB);
  ctx.fillStyle = "rgba(180,200,230,0.5)"; ctx.font = `500 15px ${FONT}`;
  ctx.textBaseline = "middle";
  ctx.fillText("L", cx - R - 14, cy); ctx.fillText("R", cx + R + 6, cy);
  ctx.textAlign = "center";
  ctx.fillText("STEREO FIELD", cx, cy + R + 26);
  ctx.textAlign = "left";
  ctx.restore();

  // ── the fixed playhead ────────────────────────────────────────────────
  ctx.strokeStyle = "rgba(255,255,255,0.85)"; ctx.lineWidth = 2;
  ctx.beginPath(); ctx.moveTo(PLAYHEAD_X, TOP - 4); ctx.lineTo(PLAYHEAD_X, dy0 + dh); ctx.stroke();
  ctx.fillStyle = "rgba(255,255,255,0.9)";
  ctx.beginPath();
  ctx.moveTo(PLAYHEAD_X - 8, TOP - 4); ctx.lineTo(PLAYHEAD_X + 8, TOP - 4); ctx.lineTo(PLAYHEAD_X, TOP + 8);
  ctx.closePath(); ctx.fill();

  // ── title + clock ─────────────────────────────────────────────────────
  const titleY = dy0 + dh + 12;
  ctx.fillStyle = "rgba(255,255,255,0.92)";
  ctx.font = `700 34px ${FONT}`;
  ctx.textBaseline = "top";
  ctx.fillText("wattajetta", 24, titleY);
  ctx.font = `600 34px ${FONT}`;
  ctx.fillStyle = "rgba(255,196,120,0.95)";
  ctx.fillText("· stone club", 24 + ctx.measureText("wattajetta ").width, titleY);
  const mm = String(Math.floor(now / 60)).padStart(1, "0");
  const ss = String(Math.floor(now % 60)).padStart(2, "0");
  ctx.fillStyle = "rgba(200,215,240,0.85)";
  ctx.font = `600 34px ${FONT}`;
  ctx.textAlign = "right";
  ctx.fillText(`${mm}:${ss}`, W - 24, titleY);
  ctx.textAlign = "left";
}

// ── render ────────────────────────────────────────────────────────────
const totalFrames = Math.ceil(PLAY_DUR * FPS);       // played timeline, not composed
console.log(`score video: ${W}×${H} @ ${FPS}fps · ${PLAY_DUR.toFixed(1)}s · ${totalFrames} frames`);
const ff = spawnFFmpegEncode({ audioPath: AUDIO, w: W, h: H, fps: FPS, outPath: MP4, crf: 19 });

let frame = 0;
function pump() {
  while (frame < totalFrames) {
    drawFrame(frame / FPS);
    const buf = canvas.toBuffer("raw");
    frame++;
    if (frame % (FPS * 10) === 0) process.stdout.write(`  ${(frame / FPS).toFixed(0)}s\n`);
    if (!ff.stdin.write(buf)) { ff.stdin.once("drain", pump); return; }
  }
  ff.stdin.end();
}
ff.on("close", (code) => {
  if (code === 0) console.log(`✓ ${MP4}`);
  else { console.error(`✗ ffmpeg exited ${code}`); process.exit(1); }
});
pump();
