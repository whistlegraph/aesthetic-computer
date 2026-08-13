#!/usr/bin/env node
// sim-polyrhythm.mjs — the Menu Band POLYRHYTHM reel base.
//
// The visual centerpiece is the app's reworked polyrhythm trainer: N
// SEPARATE circles side by side — one circle per rhythm (3 next to 2), NOT
// concentric rings — each circle's beat dots lighting on its own grid, its
// own sweep hand, over the polyrhythm jingle
// (render-polyrhythm-jingle.mjs). Every circle lights from ITS OWN lane in
// menuband-polyrhythm.notes.json, so the picture and the audio agree to
// the sample — the lane's founding determinism.
//
// Acts (scene times come straight from the score's sections):
//   1 · intro — the real menu-bar strip falls in, icon + identity, and the
//       first circle ("3") already keeping time below;
//   2 · duo — 3:2, two circles side by side;
//   3 · trio — 3:4, same three, new grid;
//   4 · full — 3:4:5, three circles, then every hand reaches the top at
//       once for the unison ring-out;
//   5 · end tag — icon · "free on the Mac App Store" · menuband.app.
//
// CIRCLE RENDER — one swappable function (drawPolyCircle). Today it draws
// in node-canvas with geometry lifted from the app's own
// PolyrhythmTrainer.swift (face r=56 → ring r=45, dots 6/10, hand 52,
// hub 5, needle flash 0.12s — all scaled by u = R/56). When the Swift
// side-by-side rework + `--render-polyrhythm --pattern/--phase` CLI lands,
// pre-render per-phase captures to out/polyrhythm-frames/ (the sim.mjs
// menubar-frames pattern) and swap the body of drawPolyCircle for a blit.
//
// Output: out/base-menuband-polyrhythm.mp4 + meta-menuband-polyrhythm.json
// Then:   node pop/menuband/bin/chrome-reel.mjs menuband-polyrhythm
//
// Usage: node pop/menuband/bin/sim-polyrhythm.mjs
//        (render-polyrhythm-jingle.mjs first)

import { readFileSync, existsSync } from "node:fs";
import {
  W, H, FPS, OUT, INK, easeOut, clamp01, rgb,
  makeStage, roundRect, text, drawDesktop, vignette, drawIcon,
  makeParticles, loadStripRig, drawStrip, stripKeyX, stripKeyColor,
  loadScore, litAt, makeOnsets,
  renderVideo, writeMeta, makeScenes,
} from "./reel-lib.mjs";

const SLUG = "menuband-polyrhythm";
const score = loadScore(SLUG);
const POLY = JSON.parse(readFileSync(`${OUT}/${SLUG}.score.json`, "utf8"));
const TOTAL = score.durationSec;
const CYCLE = POLY.cycleSec;
const END = POLY.end;                       // the unison downbeat

const { canvas, ctx } = makeStage();
const rig = await loadStripRig();
const particles = makeParticles(ctx);

// ── lanes ──────────────────────────────────────────────────────────────────
const RHYTHM_LANES = ["three", "two", "four", "five"];
const laneNotes = new Map(RHYTHM_LANES.map((l) => [l, score.notes.filter((n) => n.lane === l)]));
const stripNotes = score.notes.filter((n) => RHYTHM_LANES.includes(n.lane));
const stripOnsets = makeOnsets(stripNotes);
const laneOnsets = new Map(RHYTHM_LANES.map((l) => [l, makeOnsets(laneNotes.get(l))]));

const sectionAt = (t) => {
  let s = POLY.sections[0];
  for (const sect of POLY.sections) if (t >= sect.t0) s = sect;
  return s;
};

// ── scenes — boundaries ride the score's sections; `full` holds past the
// unison so the hands-at-the-top ring-out reads before the end card. ───────
const HOLD = 1.6;
const { scenes: SCENES, sceneAt } = makeScenes([
  { name: "intro", from: 0, to: POLY.sections[1].t0 / TOTAL, tint: [255, 77, 107] },
  { name: "duo", from: POLY.sections[1].t0 / TOTAL, to: POLY.sections[2].t0 / TOTAL, tint: [97, 158, 255] },
  { name: "trio", from: POLY.sections[2].t0 / TOTAL, to: POLY.sections[3].t0 / TOTAL, tint: [255, 153, 46] },
  { name: "full", from: POLY.sections[3].t0 / TOTAL, to: (END + HOLD) / TOTAL, tint: [51, 209, 179] },
  { name: "end", from: (END + HOLD) / TOTAL, to: 1, tint: [255, 77, 107] },
], TOTAL);

// ── the hero strip (features-sim geometry): the real menu-bar piano parks
// at the very top, falls in during the first second, lit from every rhythm
// lane — the app tie-in over the trainer. ──────────────────────────────────
const HERO_W = W * 0.96, HERO_X = (W - HERO_W) / 2, HERO_TOP = 54;
function heroRect(t) {
  const h = HERO_W / rig.aspect;
  const enter = easeOut(clamp01(t / 1.0));
  const bob = Math.sin((t / CYCLE) * Math.PI * 2) * 6 * enter;
  return { x: HERO_X, y: (-h - 40) + (HERO_TOP - (-h - 40)) * enter + bob, w: HERO_W, h };
}

// ── circle state: per-rhythm phase / grid step / strike glow, all derived
// from the score clock + that rhythm's OWN lane onsets. ────────────────────
const FLASH_SEC = 0.30;                     // needle/dot pop (app: 0.12s — widened for 30fps)
function circleStates(t) {
  const sect = sectionAt(t);
  const frozen = t >= END;                  // after the unison the hands hold the top
  const local = frozen ? 0 : Math.max(0, t - sect.t0);
  const phase = frozen ? 0 : (local / CYCLE) % 1;
  return {
    sect,
    specs: sect.rhythms.map((r) => {
      const notes = laneNotes.get(r.lane) ?? [];
      let flash = 0, litStep = -1, sustain = 0;
      for (const n of notes) {
        if (n.t > t) break;
        const step = Math.round((((n.t - sect.t0) % CYCLE) / CYCLE) * r.count) % r.count;
        if (t - n.t < FLASH_SEC) { flash = Math.max(flash, 1 - (t - n.t) / FLASH_SEC); litStep = step; }
        if (t < n.t + n.dur) { sustain = Math.max(sustain, 0.55); litStep = litStep < 0 ? step : litStep; }
      }
      return {
        lane: r.lane, count: r.count, color: r.color, phase,
        activeStep: Math.floor(phase * r.count) % r.count,
        litStep, flash, glow: Math.max(flash, sustain),
      };
    }),
  };
}

// ── THE SWAPPABLE CIRCLE RENDER ────────────────────────────────────────────
// Geometry from PolyrhythmTrainer.swift, scaled by u = R/56:
//   face r 56 (white, near-opaque, hairline quiet stroke) · beat ring r 45
//   dots ∅6 idle / ∅10 active at the ring · hand length 52, lw 1.5+2.5·flash
//   hub ∅5 · center label (the count) + small bpm figure below.
// Light mode; the active dot wears the rhythm's own icon-palette color
// (in-app it's the one system accent — per-circle color is the reel's read).
// Replace this body with an out/polyrhythm-frames/ blit once the Swift
// side-by-side CLI (--pattern/--phase/--light) exists.
function drawPolyCircle(c, { cx, cy, R, count, color, phase, activeStep, litStep, flash, glow, alpha = 1 }) {
  const u = R / 56;
  const TAU = Math.PI * 2;
  const quiet = "rgba(0,0,0,0.18)";
  c.save();
  c.globalAlpha = alpha;

  // face
  c.beginPath(); c.arc(cx, cy, R, 0, TAU);
  c.fillStyle = "rgba(255,255,255,0.93)"; c.fill();
  c.lineWidth = Math.max(2, 1.2 * u); c.strokeStyle = quiet; c.stroke();

  // beat ring
  c.beginPath(); c.arc(cx, cy, 45 * u, 0, TAU);
  c.lineWidth = 1.5 * u; c.strokeStyle = quiet; c.stroke();

  // dots — clockwise from the top, one per beat of this rhythm's grid
  for (let i = 0; i < count; i++) {
    const a = -Math.PI / 2 + (i / count) * TAU;
    const px = cx + Math.cos(a) * 45 * u, py = cy + Math.sin(a) * 45 * u;
    const active = i === activeStep;
    const struck = i === litStep && glow > 0;
    const dia = (active ? 10 : 6) * u + (struck ? 4 * u * flash : 0);
    if (struck) {
      c.save();
      c.shadowColor = rgb(color, 0.9 * glow); c.shadowBlur = 22 * u * glow;
      c.beginPath(); c.arc(px, py, dia / 2, 0, TAU);
      c.fillStyle = rgb(color); c.fill();
      c.restore();
    } else {
      c.beginPath(); c.arc(px, py, dia / 2, 0, TAU);
      c.fillStyle = active ? rgb(color) : quiet; c.fill();
    }
  }

  // hand — one revolution per shared cycle; strikes brighten + thicken it
  const ha = -Math.PI / 2 + phase * TAU;
  c.beginPath(); c.moveTo(cx, cy);
  c.lineTo(cx + Math.cos(ha) * 52 * u, cy + Math.sin(ha) * 52 * u);
  c.lineWidth = (1.5 + 2.5 * flash) * u;
  c.lineCap = "round";
  c.strokeStyle = flash > 0.01 ? rgb(color, 0.78 + 0.22 * flash) : "rgba(20,18,28,0.78)";
  c.stroke();

  // hub
  c.beginPath(); c.arc(cx, cy, 2.5 * u, 0, TAU);
  c.fillStyle = INK; c.fill();

  // the count + the app's bpm figure (75 — the trainer default the jingle rides)
  text(c, String(count), cx, cy - 13 * u, 15 * u, "rgba(20,18,28,0.88)", 800);
  text(c, String(POLY.bpm), cx, cy + 14 * u, 8 * u, "rgba(20,18,28,0.58)", 600);

  c.restore();
}

// ── row layout: N circles side by side, sized to the frame ────────────────
const GAP = 44;
function rowSpecs(specs, cy, maxR = 210) {
  const n = specs.length;
  const R = Math.min(maxR, (W * 0.92 - (n - 1) * GAP) / (2 * n));
  const total = n * 2 * R + (n - 1) * GAP;
  const x0 = (W - total) / 2 + R;
  return specs.map((s, i) => ({ ...s, R, cx: x0 + i * (2 * R + GAP), cy }));
}

// ── captions ───────────────────────────────────────────────────────────────
const CAPTIONS = {
  intro: ["meet the polyrhythm trainer", "one circle per rhythm · press / to start"],
  duo: ["three against two", "each circle keeps its own grid"],
  trio: ["same three · new grid", "the left circle never changes"],
  full: ["three grids at once", "3 · 4 · 5 — one shared clock"],
};

function drawFrame(t) {
  drawDesktop(ctx);
  const sc = sceneAt(t);
  const local = (sc.to - sc.from) > 0 ? (t - sc.from) / (sc.to - sc.from) : 1;
  const dt = 1 / FPS;

  // the real strip up top, lit by every rhythm lane
  const hero = heroRect(t);
  const hRect = drawStrip(ctx, rig, litAt(stripNotes, t), hero.x, hero.y, hero.w);
  for (const n of stripOnsets(t - dt, t)) {
    particles.spawnNote(stripKeyX(rig, n.midi, hRect), hRect.y + hRect.h + 6, stripKeyColor(rig, n.midi), true);
  }

  if (sc.name === "end") {
    const e = easeOut(Math.min(1, local * 5));
    ctx.save(); ctx.globalAlpha = e;
    const ipx = 300;
    drawIcon(ctx, W / 2 - ipx / 2, H * 0.28, ipx, new Set(litAt(stripNotes, t).map((mm) => ((mm % 12) + 12) % 12 % 5)));
    text(ctx, "Menu Band", W / 2, H * 0.50, 110, INK, 800);
    text(ctx, "the polyrhythm trainer — in your menu bar", W / 2, H * 0.50 + 96, 44, "rgba(60,50,80,0.9)", 600);
    text(ctx, "free on the Mac App Store", W / 2, H * 0.50 + 182, 50, "rgba(60,50,80,0.9)", 700);
    text(ctx, "menuband.app", W / 2, H * 0.50 + 262, 58, INK, 800);
    ctx.restore();
  } else {
    const { sect, specs } = circleStates(t);
    // section-entry pop
    const enter = easeOut(clamp01((t - sect.t0) / 0.5));
    const alpha = 0.25 + 0.75 * enter;

    if (sc.name === "intro") {
      const e = easeOut(clamp01(t / 0.8));
      ctx.save(); ctx.globalAlpha = e;
      const ipx = 250;
      drawIcon(ctx, W / 2 - ipx / 2, H * 0.135, ipx, new Set(litAt(stripNotes, t).map((mm) => ((mm % 12) + 12) % 12 % 5)));
      text(ctx, "Menu Band", W / 2, H * 0.135 + ipx + 66, 100, INK, 800);
      ctx.restore();
      const row = rowSpecs(specs, H * 0.60, 205);
      for (const s of row) drawPolyCircle(ctx, { ...s, alpha });
      spawnDotParticles(row, t, dt);
    } else {
      // big pattern label
      ctx.save(); ctx.globalAlpha = alpha;
      text(ctx, sect.label, W / 2, H * 0.185, 148, INK, 800);
      ctx.restore();
      const row = rowSpecs(specs, H * 0.465);
      for (const s of row) {
        const grow = 0.9 + 0.1 * enter;
        drawPolyCircle(ctx, { ...s, R: s.R * grow, alpha });
      }
      spawnDotParticles(row, t, dt);
    }

    const cap = CAPTIONS[sc.name];
    if (cap) {
      const a = easeOut(clamp01(local * 4)) * (1 - easeOut(clamp01((local - 0.86) / 0.14)));
      if (a > 0) {
        ctx.save(); ctx.globalAlpha = a;
        text(ctx, cap[0], W / 2, H * 0.80, 82, INK, 800);
        text(ctx, cap[1], W / 2, H * 0.80 + 82, 42, "rgba(60,50,80,0.88)", 600);
        ctx.restore();
      }
    }
  }

  particles.stepAndDraw(dt);
  vignette(ctx);
}

// a note leaves the struck dot itself — each circle sprays its own color
function spawnDotParticles(row, t, dt) {
  for (const s of row) {
    for (const n of laneOnsets.get(s.lane)(t - dt, t)) {
      const step = Math.round((((n.t - sectionAt(n.t).t0) % CYCLE) / CYCLE) * s.count) % s.count;
      const a = -Math.PI / 2 + (step / s.count) * (Math.PI * 2);
      particles.spawnNote(s.cx + Math.cos(a) * (s.R / 56) * 45, s.cy + Math.sin(a) * (s.R / 56) * 45, s.color, false);
    }
  }
}

await renderVideo({
  canvas, audioPath: `${OUT}/${SLUG}.mp3`, outPath: `${OUT}/base-${SLUG}.mp4`,
  total: TOTAL, drawFrame, label: "menuband polyrhythm sim",
});
writeMeta(SLUG, TOTAL, SCENES);
