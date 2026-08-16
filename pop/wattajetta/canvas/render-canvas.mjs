#!/usr/bin/env node
// Spotify Canvas for "Wattajetta Stone Club" — real cover photo only,
// traditional 2D treatment: slitscan row displacement + zoom bounce
// pumped on an 18-beat grid (135 BPM × 8s loop, inside the track's
// 127→138 accelerando). Every phase term is an integer cycle count so
// the loop is seamless.
//
//   node pop/wattajetta/canvas/render-canvas.mjs [framesDir]
//
// Emits framesDir/0000.png … 0239.png; encode with ffmpeg afterwards.

import { createCanvas, loadImage } from "canvas";
import { writeFileSync, mkdirSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const here = dirname(fileURLToPath(import.meta.url));
const framesDir = process.argv[2] || join(here, "frames");
mkdirSync(framesDir, { recursive: true });

const W = 720;
const H = 1280;
const FPS = 30;
const SECONDS = 8;
const FRAMES = FPS * SECONDS;
const BEATS = 18; // 135 BPM over the 8s loop
const TAU = Math.PI * 2;

const img = await loadImage(join(here, "cover-3000.jpg"));
const SRC = 3000;
const SRC_W = SRC * (9 / 16); // full-height 9:16 window inside the square
const CX = 1500; // figure is horizontally centered
const CY = 1500; // face already rides the upper third of the square

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");

// Beat pump: exponential-decay kick retriggered every beat, loop-safe
// because beat positions divide the loop evenly.
function beatPump(phase) {
  const beatPhase = (phase * BEATS) % 1;
  return Math.exp(-beatPhase * 6);
}

// Slitscan amplitude envelope over output rows: quiet band where the
// face lives (~rows 0.18–0.42), full smear toward top and bottom.
function rowEnvelope(v) {
  const face = Math.exp(-Math.pow((v - 0.3) / 0.16, 2));
  return 1 - 0.85 * face;
}

const STRIP = 2;

for (let f = 0; f < FRAMES; f++) {
  const phase = f / FRAMES;

  const breathe = Math.sin(TAU * 2 * phase); // two slow zoom cycles
  const pump = beatPump(phase);

  for (let y = 0; y < H; y += STRIP) {
    const v = y / H;
    const env = rowEnvelope(v);

    // Slitscan: each row reads the zoom timeline at its own offset —
    // three vertical ripple periods drifting one full cycle per loop.
    const rowPhase = phase + env * 0.09 * Math.sin(TAU * (3 * v + phase));
    const rowBreathe = Math.sin(TAU * 2 * rowPhase);
    const rowPump = beatPump(((rowPhase % 1) + 1) % 1);

    const scale = 1.17 + 0.095 * rowBreathe + 0.025 * rowPump;

    // Horizontal ripple, two periods tall, drifting once per loop.
    const xshift = env * 30 * Math.sin(TAU * (2 * v - phase)) * (0.4 + 0.6 * rowPump);

    const winW = SRC_W / scale;
    const winH = SRC / scale;
    const stripH = (STRIP / H) * winH;
    const wx0 = Math.max(0, Math.min(SRC - winW, CX - winW / 2 + xshift));
    const wy0 = Math.max(0, Math.min(SRC - winH, CY - winH / 2));
    const sy = Math.max(0, Math.min(SRC - stripH, wy0 + v * winH));

    ctx.drawImage(img, wx0, sy, winW, stripH, 0, y, W, STRIP);
  }

  writeFileSync(join(framesDir, String(f).padStart(4, "0") + ".png"), canvas.toBuffer("image/png"));
  if (f % 30 === 0) console.log(`frame ${f}/${FRAMES}`);
}

console.log(`done → ${framesDir}`);
