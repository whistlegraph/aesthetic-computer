#!/usr/bin/env node
// A true 9:16 Menu Band reel: only the real menu-bar piano, playing and
// bouncing. No words, windows, particles, logos, chrome, or end card.

import { existsSync, mkdirSync, statSync } from "node:fs";
import { resolve } from "node:path";
import { spawnSync } from "node:child_process";
import {
  W, H, OUT, STRIP_MIDIS, makeStage, loadStripRig, drawStrip,
  stripKeyX, stripKeyColor, makeParticles, makeOnsets,
  loadScore, litAt, renderVideo,
} from "./reel-lib.mjs";

const BIN = resolve("slab/menuband/.build/debug/MenuBand");
const REEL_FPS = 60;
const FRAME_DIR = `${OUT}/menubar-frames`;
const PERC_FRAME_DIR = `${OUT}/menubar-frames-percussion-right`;
const FULL_PERC_FRAME_DIR = `${OUT}/menubar-frames-percussion-full`;
const AUDIO_WAV = `${OUT}/menuband-graceful-piano.wav`;
const AUDIO_MASTER = `${OUT}/menuband-graceful-piano.mp3`;
mkdirSync(FRAME_DIR, { recursive: true });

function capture(path, notes = "", percussionRight = false, percussionLeft = false) {
  if (existsSync(path)) return;
  const args = [
    "--render-menubar", "--notes", notes, "--out", path,
    "--program", "78", "--voice", "Whistle", "--scale", "8", "--light",
    "--accent", "#000000", "--no-settings", "--key-accent",
  ];
  if (percussionRight) args.push("--percussion-right");
  if (percussionLeft) args.push("--percussion-left");
  const r = spawnSync(BIN, args, { encoding: "utf8" });
  if (r.status !== 0) throw new Error(`MenuBand capture failed: ${r.stderr || r.stdout}`);
  console.log(`  capture ${notes || "idle"}`);
}

if (!existsSync(BIN)) {
  throw new Error(`missing ${BIN}; build slab/menuband first`);
}

if (!existsSync(AUDIO_WAV)) {
  throw new Error(`missing ${AUDIO_WAV}; run render-graceful-piano.swift first`);
}
if (!existsSync(AUDIO_MASTER) || statSync(AUDIO_MASTER).mtimeMs < statSync(AUDIO_WAV).mtimeMs) {
  const r = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error", "-i", AUDIO_WAV,
    "-af", "highpass=f=28,equalizer=f=220:t=q:w=1.1:g=1.2,equalizer=f=3100:t=q:w=1.0:g=0.8,loudnorm=I=-17:TP=-1.5:LRA=9",
    "-c:a", "libmp3lame", "-b:a", "256k", AUDIO_MASTER,
  ], { encoding: "utf8" });
  if (r.status !== 0) throw new Error(`piano master failed: ${r.stderr || r.stdout}`);
  console.log("  mastered piano audio");
}

capture(`${FRAME_DIR}/mb-idle.png`);
for (const midi of STRIP_MIDIS) capture(`${FRAME_DIR}/mb-${midi}.png`, String(midi));
mkdirSync(PERC_FRAME_DIR, { recursive: true });
capture(`${PERC_FRAME_DIR}/mb-idle.png`, "", true);
for (const midi of STRIP_MIDIS) capture(`${PERC_FRAME_DIR}/mb-${midi}.png`, String(midi), true);
mkdirSync(FULL_PERC_FRAME_DIR, { recursive: true });
capture(`${FULL_PERC_FRAME_DIR}/mb-idle.png`, "", true, true);
for (const midi of STRIP_MIDIS) capture(`${FULL_PERC_FRAME_DIR}/mb-${midi}.png`, String(midi), true, true);

const score = loadScore("menuband-graceful-piano");
const notes = (score.notes || []).filter((note) => STRIP_MIDIS.includes(note.midi));
const total = score.durationSec;
const { canvas, ctx } = makeStage();
const rig = await loadStripRig();
const percussionRig = await loadStripRig("menubar-frames-percussion-right");
const fullPercussionRig = await loadStripRig("menubar-frames-percussion-full");
const particles = makeParticles(ctx, { fade: false });
const percussionParticles = makePercussionParticles(ctx);
const onsetsBetween = makeOnsets(notes);

const STRIP_W = W * 0.94;
const STRIP_H = STRIP_W / rig.aspect;
const BASE_Y = H * 0.50 - STRIP_H / 2;
const REVEAL_AT = 3.0;
const REVEAL_DUR = 1.15;
const EXIT_DUR = 1.1;
const EXIT_AT = total - 1.5;
const onsetTimes = [...new Set(notes.map((note) => +note.t.toFixed(4)))];

function mostRecentOnset(t) {
  let found = -Infinity;
  for (const onset of onsetTimes) {
    if (onset > t) break;
    found = onset;
  }
  return found;
}

function makePercussionParticles(context) {
  const falling = [];
  const pc = (midi) => ((midi % 12) + 12) % 12;

  function stroke(color, width = 4) {
    context.strokeStyle = `rgb(${color[0]},${color[1]},${color[2]})`;
    context.lineWidth = width;
    context.lineCap = "round";
    context.lineJoin = "round";
  }

  function drawSymbol(midi, color) {
    const kind = pc(midi);
    stroke(color);
    context.fillStyle = `rgba(${color[0]},${color[1]},${color[2]},0.30)`;

    if (kind === 0) {                         // kick: bass-drum ring
      context.beginPath(); context.arc(0, 0, 17, 0, Math.PI * 2); context.fill(); context.stroke();
      context.beginPath(); context.arc(0, 0, 7, 0, Math.PI * 2); context.stroke();
    } else if (kind === 2) {                  // snare: shallow drum
      context.beginPath(); context.ellipse(0, -8, 18, 6, 0, 0, Math.PI * 2); context.fill(); context.stroke();
      context.beginPath(); context.moveTo(-18, -8); context.lineTo(-15, 10);
      context.quadraticCurveTo(0, 16, 15, 10); context.lineTo(18, -8); context.stroke();
      context.beginPath(); context.moveTo(-12, 1); context.lineTo(12, 8); context.stroke();
    } else if (kind === 4 || kind === 5) {     // clap / snap: compact burst
      for (let i = 0; i < 8; i++) {
        const a = i * Math.PI / 4;
        context.beginPath();
        context.moveTo(Math.cos(a) * 7, Math.sin(a) * 7);
        context.lineTo(Math.cos(a) * 20, Math.sin(a) * 20);
        context.stroke();
      }
      context.beginPath(); context.arc(0, 0, 5, 0, Math.PI * 2); context.fill();
    } else {                                  // hats / ride: cymbal + stand
      context.beginPath();
      context.moveTo(-20, 1); context.quadraticCurveTo(0, -13, 20, 1);
      context.quadraticCurveTo(0, 8, -20, 1); context.closePath();
      context.fill(); context.stroke();
      context.beginPath(); context.moveTo(0, 4); context.lineTo(0, 22); context.stroke();
      if (kind === 9 || kind === 11) {
        context.beginPath(); context.moveTo(-13, 13); context.lineTo(13, 13); context.stroke();
      }
    }
  }

  return {
    clear() { falling.length = 0; },
    spawn(x, y, color, midi) {
      falling.push({
        x, y, color, midi,
        vx: Math.sin((falling.length + 1) * 2.1) * 55,
        vy: 45 + (falling.length % 4) * 12,
        age: 0,
        rot: (falling.length % 7 - 3) * 0.10,
        spin: (falling.length % 2 ? 1 : -1) * 0.45,
      });
    },
    stepAndDraw(dt) {
      for (const p of falling) {
        p.age += dt; p.x += p.vx * dt; p.y += p.vy * dt;
        p.vy += 320 * dt; p.rot += p.spin * dt;
      }
      for (let i = falling.length - 1; i >= 0; i--) if (falling[i].y > H + 90) falling.splice(i, 1);
      for (const p of falling) {
        context.save();
        context.globalAlpha = 1;
        context.translate(p.x, p.y); context.rotate(p.rot);
        context.shadowColor = "rgba(25,18,35,0.28)";
        context.shadowBlur = 5; context.shadowOffsetX = 2; context.shadowOffsetY = 3;
        drawSymbol(p.midi, p.color);
        context.restore();
      }
    },
  };
}

function drawBackground() {
  const g = ctx.createLinearGradient(0, 0, 0, H);
  g.addColorStop(0, "rgb(240,235,248)");
  g.addColorStop(0.52, "rgb(218,205,235)");
  g.addColorStop(1, "rgb(194,176,218)");
  ctx.fillStyle = g;
  ctx.fillRect(0, 0, W, H);
}

function drawPerformanceLight(t, activeRig, stripRect, visibility) {
  if (visibility <= 0.01) return;
  const active = [...new Set(litAt(notes, t, 0.18))];
  if (active.length === 0) return;
  const colors = active.map((midi) => stripKeyColor(activeRig, midi));
  const perKeyAlpha = Math.min(0.34, 0.48 / Math.sqrt(active.length)) * visibility;

  ctx.save();
  ctx.globalCompositeOperation = "screen";
  for (let i = 0; i < active.length; i++) {
    const x = stripKeyX(activeRig, active[i], stripRect);
    const y = stripRect.y + stripRect.h * 0.55;
    const c = colors[i];
    const glow = ctx.createRadialGradient(x, y, 8, x, y, 360);
    glow.addColorStop(0, `rgba(${c[0]},${c[1]},${c[2]},${perKeyAlpha})`);
    glow.addColorStop(0.34, `rgba(${c[0]},${c[1]},${c[2]},${perKeyAlpha * 0.42})`);
    glow.addColorStop(1, `rgba(${c[0]},${c[1]},${c[2]},0)`);
    ctx.fillStyle = glow;
    ctx.fillRect(0, 0, W, H);
  }

  const average = colors.reduce((sum, c) => [sum[0] + c[0], sum[1] + c[1], sum[2] + c[2]], [0, 0, 0])
    .map((v) => Math.round(v / colors.length));
  ctx.globalCompositeOperation = "soft-light";
  ctx.fillStyle = `rgba(${average[0]},${average[1]},${average[2]},${0.11 * visibility})`;
  ctx.fillRect(0, 0, W, H);
  ctx.restore();
}

function drawFrame(t) {
  const onset = mostRecentOnset(t);
  const age = t - onset;
  const struck = Number.isFinite(age) && age >= 0 && age < 0.75;
  const spring = struck ? Math.exp(-age * 5.2) * Math.sin(age * 18.5) : 0;
  const breath = Math.sin((t / score.barSec) * Math.PI * 2) * 8;
  const reveal = Math.max(0, Math.min(1, (t - REVEAL_AT) / REVEAL_DUR));
  const revealEase = 1 - Math.pow(1 - reveal, 3);
  const exit = Math.max(0, Math.min(1, (t - EXIT_AT) / EXIT_DUR));
  const exitEase = exit * exit * exit;
  const hiddenY = -STRIP_H - 24;
  const restingY = BASE_Y + breath - spring * 46;
  const enteredY = hiddenY + (restingY - hiddenY) * revealEase;
  const y = enteredY + (H + STRIP_H * 2 - enteredY) * exitEase;
  const scale = 1 + spring * 0.020;
  const transitionAge = Math.min(
    Math.abs(t - score.splitAtSec),
    Math.abs(t - score.fullPercussionAtSec),
    Math.abs(t - score.percussionOffAtSec),
  );
  const shake = transitionAge < 0.7
    ? Math.exp(-transitionAge * 5.2) * Math.sin(transitionAge * 42)
    : 0;
  const tilt = Math.sin(t * 0.72) * 0.0032 + spring * 0.0024 + shake * 0.018;
  const shakeX = shake * 22;

  let activeRig = rig;
  if (t >= score.fullPercussionAtSec && t < score.percussionOffAtSec) activeRig = fullPercussionRig;
  else if (t >= score.splitAtSec && t < score.fullPercussionAtSec) activeRig = percussionRig;
  const stripRect = { x: (W - STRIP_W) / 2 + shakeX, y, w: STRIP_W, h: STRIP_H };

  drawBackground();
  drawPerformanceLight(t, activeRig, stripRect, revealEase * (1 - exitEase));

  if (reveal > 0) {
    ctx.save();
    ctx.translate(W / 2 + shakeX, y + STRIP_H / 2);
    ctx.rotate(tilt);
    ctx.scale(scale, scale);
    ctx.translate(-(W / 2 + shakeX), -(y + STRIP_H / 2));

    // The only supporting mark is the piano's own soft cast shadow.
    ctx.save();
    ctx.globalAlpha = 0.18;
    ctx.filter = "blur(18px)";
    ctx.fillStyle = "rgb(45,28,70)";
    ctx.fillRect(stripRect.x + 24, y + 24, STRIP_W - 48, STRIP_H);
    ctx.restore();

    drawStrip(ctx, activeRig, litAt(notes, t, 0.16), stripRect.x, y, STRIP_W);
    ctx.restore();
  }

  // One falling glyph per audible onset, released from the matching key.
  for (const note of onsetsBetween(t - 1 / REEL_FPS, t)) {
    const x = stripKeyX(activeRig, note.midi, stripRect);
    const y = stripRect.y + stripRect.h + 8;
    const color = stripKeyColor(activeRig, note.midi);
    if (note.lane === "drum") percussionParticles.spawn(x, y, color, note.midi);
    else particles.spawnNote(x, y, color, true);
  }
  particles.stepAndDraw(1 / REEL_FPS);
  percussionParticles.stepAndDraw(1 / REEL_FPS);
}

await renderVideo({
  canvas,
  audioPath: AUDIO_MASTER,
  outPath: `${OUT}/menuband-piano-only-reel.mp4`,
  total,
  drawFrame,
  label: "menuband piano-only reel",
  fps: REEL_FPS,
});
