#!/usr/bin/env node
// preview-hitbaker.mjs — track video for the imab hitbaker bake.
//
// Shared preview primitives (YWFT via magick, waveform-inside-events,
// RMS-driven title bounce, BGRA encode) from pop/lib/preview-shared;
// imab identity here: night-butterfly palette, the hitbaker's own form
// (mirrored from bin/hitbaker.mjs), chord letters riding the timeline,
// a fixed-center playhead the mini-DAW scrolls past.
//
//   node pop/imab/bin/preview-hitbaker.mjs
//   → out/imab-hitbaker-demo1-track.mp4   (1920x1080 @ 30)
//
//   --start <sec> --frames <n>   render a slice (layout checks)

import { existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas } from "canvas";
import * as progress from "../../lib/render-progress.mjs";
import {
  checkYwftAvailable, decodeAudioMono, computeRmsEnvelope,
  prerenderTitleChars, magickRenderText, drawTitleBounce,
  drawEventWaveform, spawnFFmpegEncode,
} from "../../lib/preview-shared.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out/imab-hitbaker-demo1-track.mp4`;
const AUDIO = `${LANE}/out/imab-hitbaker-demo1.wav`;
const ASSETS = `${process.env.HOME}/.cache/ac/imab/preview-assets`;
mkdirSync(ASSETS, { recursive: true });

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}

checkYwftAvailable();
if (!existsSync(AUDIO)) { console.error(`✗ bake first: ${AUDIO}`); process.exit(1); }

const W = 1920, H = 1080, FPS = 30;
const BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT, BARS = 104;

// ── the hitbaker form, mirrored ───────────────────────────────────────
const SEC = (bar) =>
  bar < 4 ? "intro" : bar < 20 ? "verse" : bar < 24 ? "pre"
  : bar < 40 ? "chorus" : bar < 52 ? "verse" : bar < 56 ? "pre"
  : bar < 72 ? "chorus" : bar < 76 ? "break" : bar < 100 ? "finale"
  : "peel";
const VERSE8 = ["Am", "Am", "G", "G", "C", "C", "F", "G"];
const CHOR8 = ["C", "C", "F", "F", "C", "C", "C", "C"];
const chordAt = (bar) => {
  const s = SEC(bar);
  if (s === "intro" || s === "peel") return "C";
  if (s === "verse") return VERSE8[(bar - (bar < 40 ? 4 : 40)) % 8];
  if (s === "pre") return ["F", "F", "G", "G"][bar % 4];
  if (s === "break") return "G";
  return CHOR8[(bar - (bar < 52 ? 24 : bar < 76 ? 56 : 76)) % 8];
};
const inBreak = (bar) => bar >= 72 && bar < 76;

// night-butterfly tints, one per section kind
const TINT = {
  intro: "#4a3aa7", verse: "#2a4ad6", pre: "#b05cc4",
  chorus: "#eda100", break: "#1baf7a", finale: "#e05a9e", peel: "#4a3aa7",
};
const SECTIONS = [];
{
  let start = 0, kind = SEC(0);
  for (let b = 1; b <= BARS; b++) {
    const k = b < BARS ? SEC(b) : null;
    if (k !== kind) { SECTIONS.push({ kind, b0: start, b1: b }); start = b; kind = k; }
  }
}

// ── lanes of the mini-DAW: [row, label, color, events] ───────────────
const beatsOf = (bar) => [0, 1, 2, 3].map((k) => bar * 4 + k);
const ev = (t0, dur) => ({ t0, t1: t0 + dur });
const LANES = [
  { key: "vox", color: "#ffd257", events: [24, 56, 76].flatMap((door) =>
    [ev(door * BAR, 8 * BAR)]) },
  { key: "xylo", color: "#eda100", events: (() => {
    const out = [];
    for (const base of [32, 34, 64, 66]) out.push(ev(base * BAR, 2 * BAR));
    for (let b = 84; b < 100; b += 2) out.push(ev(b * BAR, 2 * BAR));
    return out;
  })() },
  { key: "stabs", color: "#b05cc4", events: (() => {
    const out = [];
    for (let bar = 4; bar < 100; bar++) if (!inBreak(bar))
      for (const beat of [1.5, 3]) out.push(ev(bar * BAR + beat * BEAT, 0.6 * BEAT));
    return out;
  })() },
  { key: "hats", color: "#1baf7a", events: (() => {
    const out = [];
    for (let bar = 0; bar < 100; bar++) out.push(ev(bar * BAR, BAR * 0.96));
    return out;
  })() },
  { key: "clap", color: "#e05a9e", events: (() => {
    const out = [];
    for (let bar = 4; bar < 100; bar++) if (!inBreak(bar))
      for (const beat of [1, 3]) out.push(ev(bar * BAR + beat * BEAT, 0.5 * BEAT));
    return out;
  })() },
  { key: "kick", color: "#e34948", events: (() => {
    const out = [];
    for (let bar = 0; bar < 100; bar++) if (!inBreak(bar))
      for (const b of beatsOf(bar)) out.push(ev(b * BEAT, 0.45 * BEAT));
    return out;
  })() },
  { key: "bass", color: "#2a78d6", events: (() => {
    const out = [];
    for (let bar = 4; bar < 100; bar++) if (!inBreak(bar))
      for (const half of [0, 2]) out.push(ev(bar * BAR + half * BEAT, 2 * BEAT * 0.95));
    return out;
  })() },
];

// ── audio + envelope ─────────────────────────────────────────────────
console.log("decoding audio…");
const { audio, sr, audioPeak: peak } = decodeAudioMono(AUDIO);
const DURATION = audio.length / sr;
const env = computeRmsEnvelope(audio, sr, 60, DURATION);
const envAt = (t) => env[Math.max(0, Math.min(env.length - 1, Math.floor(t * 60)))] ?? 0;

// ── prerendered type ─────────────────────────────────────────────────
const TITLE_PALETTE = ["#ffd257", "#e05a9e", "#b05cc4", "#1baf7a", "#eda100"];
const title = await prerenderTitleChars({
  text: "IMAB", ptSize: 190, palette: TITLE_PALETTE,
  shadowColor: "rgba(0,0,0,0.55)", assetsDir: ASSETS,
});
const subtitle = await magickRenderText("hitbaker", {
  ptSize: 44, fill: "#cdc7ee", outPath: `${ASSETS}/subtitle.png`,
  shadow: "rgba(0,0,0,0.6)",
});
const CHORD_IMGS = {};
for (const c of ["Am", "G", "C", "F"])
  CHORD_IMGS[c] = await magickRenderText(c, {
    ptSize: 30, fill: "#ffffff", outPath: `${ASSETS}/chord-${c}.png`,
  });
const SECT_IMGS = {};
for (const kind of Object.keys(TINT))
  SECT_IMGS[kind] = await magickRenderText(kind, {
    ptSize: 22, fill: "#0b0b14", outPath: `${ASSETS}/sect-${kind}.png`,
  });

// ── layout ───────────────────────────────────────────────────────────
const BAR_H = 26;                       // section progress strip
const TL_TOP = H - 420, TL_BOT = H - 44;
const ROW_H = (TL_BOT - TL_TOP - 36) / LANES.length;
const PPS = W / (8 * BAR);              // 8 bars visible across the width
const t2x = (t, now) => W / 2 + (t - now) * PPS;

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");

function drawFrame(now) {
  const e = envAt(now);
  const bar = Math.max(0, Math.min(BARS - 1, Math.floor(now / BAR)));
  const tint = TINT[SEC(bar)];

  // night ground + section-tinted wings breathing with the envelope
  const g = ctx.createLinearGradient(0, 0, 0, H);
  g.addColorStop(0, "#0b0b14"); g.addColorStop(1, "#05050a");
  ctx.fillStyle = g; ctx.fillRect(0, 0, W, H);
  for (const [cx, cy] of [[W * 0.24, H * 0.34], [W * 0.76, H * 0.34]]) {
    const r = H * (0.34 + 0.1 * e);
    const rg = ctx.createRadialGradient(cx, cy, 0, cx, cy, r);
    rg.addColorStop(0, tint + Math.round(46 + 60 * e).toString(16).padStart(2, "0"));
    rg.addColorStop(1, tint + "00");
    ctx.fillStyle = rg;
    ctx.beginPath(); ctx.arc(cx, cy, r, 0, Math.PI * 2); ctx.fill();
  }

  // section progress strip (whole track), playhead notch
  for (const s of SECTIONS) {
    const x0 = (s.b0 * BAR / DURATION) * W, x1 = (s.b1 * BAR / DURATION) * W;
    ctx.fillStyle = TINT[s.kind] + (SEC(bar) === s.kind && bar >= s.b0 && bar < s.b1 ? "ff" : "77");
    ctx.fillRect(x0, 0, x1 - x0, BAR_H);
    const img = SECT_IMGS[s.kind];
    if (img && x1 - x0 > img.width + 10)
      ctx.drawImage(img, x0 + 6, (BAR_H - img.height) / 2 + 1);
  }
  ctx.fillStyle = "#ffffff";
  ctx.fillRect((now / DURATION) * W - 1.5, 0, 3, BAR_H + 6);

  // title
  drawTitleBounce(ctx, {
    chars: title.chars, ptSize: 190,
    baseX: (W - title.totalWidth) / 2, baseY: 330,
    audioT: now, env: e, getEnvAt: envAt, bounceAmp: 70,
  });
  ctx.globalAlpha = 0.9;
  ctx.drawImage(subtitle, (W - subtitle.width) / 2, 356);
  ctx.globalAlpha = 1;

  // timeline backdrop
  ctx.fillStyle = "rgba(5,5,12,0.72)";
  ctx.fillRect(0, TL_TOP, W, TL_BOT - TL_TOP);

  // bar lines + chord letters
  const firstBar = Math.max(0, Math.floor((now - 4.1 * BAR) / BAR));
  const lastBar = Math.min(BARS, Math.ceil((now + 4.1 * BAR) / BAR));
  for (let b = firstBar; b <= lastBar; b++) {
    const x = t2x(b * BAR, now);
    ctx.fillStyle = b % 8 === 0 ? "rgba(255,255,255,0.35)" : "rgba(255,255,255,0.12)";
    ctx.fillRect(x, TL_TOP, b % 8 === 0 ? 2 : 1, TL_BOT - TL_TOP);
    if (b < BARS) {
      const img = CHORD_IMGS[chordAt(b)];
      if (img) { ctx.globalAlpha = 0.9; ctx.drawImage(img, x + 6, TL_TOP + 4); ctx.globalAlpha = 1; }
    }
  }

  // lanes: event blocks with the actual audio inside
  LANES.forEach((lane, li) => {
    const y = TL_TOP + 36 + li * ROW_H;
    for (const evn of lane.events) {
      if (evn.t1 < now - 4 * BAR || evn.t0 > now + 4 * BAR) continue;
      const x0 = t2x(evn.t0, now), x1 = t2x(evn.t1, now);
      const active = now >= evn.t0 && now < evn.t1;
      ctx.fillStyle = lane.color + (active ? "55" : "2e");
      ctx.fillRect(x0, y + 2, x1 - x0 - 1, ROW_H - 4);
      drawEventWaveform(ctx, audio, sr, peak, x0 + 1, y + 3, x1 - x0 - 3,
        ROW_H - 6, evn.t0, evn.t1, lane.color, active ? 1 : 0.55);
    }
  });

  // fixed playhead
  ctx.fillStyle = "rgba(255,255,255,0.9)";
  ctx.fillRect(W / 2 - 1.5, TL_TOP - 8, 3, TL_BOT - TL_TOP + 8);
}

// ── encode ───────────────────────────────────────────────────────────
const startT = Number(flags.start ?? 0);
const totalFrames = flags.frames
  ? Number(flags.frames) : Math.ceil((DURATION - startT) * FPS);
const enc = spawnFFmpegEncode({ audioPath: AUDIO, w: W, h: H, fps: FPS, outPath: OUT });
progress.begin({ type: "video", label: `imab hitbaker track · ${totalFrames} frames` });
for (let f = 0; f < totalFrames; f++) {
  drawFrame(startT + f / FPS);
  const buf = canvas.toBuffer("raw");
  if (!enc.stdin.write(buf)) await new Promise((r) => enc.stdin.once("drain", r));
  if (f % 150 === 0) progress.update((f / totalFrames) * 100, { done: f, total: totalFrames });
}
enc.stdin.end();
await new Promise((r) => enc.on("close", r));
progress.end();
console.log(`✓ ${OUT}`);
