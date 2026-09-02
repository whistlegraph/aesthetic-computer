#!/usr/bin/env node
// pop/cult/viz/field.mjs — wannadash's picture, from the score receipt.
//
// The cover is three UV-powder glyphs around a stone. This draws the
// RECORD onto it: every sung dot lands as a grain of powder, every dash
// is a streak that grows for as long as the note is held, in the colour
// of whoever sang it (Camille pink · Alex green · Jeffrey orange — the
// three glyphs' colours), placed by pan (left/right) and pitch (up/down).
// The kick lifts the picture, the explosions knock it. Everything comes
// from out/cult-remix-v10.events.json mapped through the release edit
// (bin/tempo.py's warp + cut-release.sh's seams), so it is in time with
// the shipped audio to the frame.
//
// Three outputs, one drawing:
//   canvas  — Spotify Canvas: 1080×1920, 8 s (= 4 bars of the hook at
//             120), SILENT, seamless (the window wraps), NO text.
//   reel    — Instagram Reel: 1080×1920 @ 30, the shipped audio, the
//             words on rails as they are sung.
//   stills  — 1024² JPGs at the section starts, for the AC piece.
//
//   node viz/field.mjs canvas [--out out/wannadash-canvas.mp4]
//   node viz/field.mjs reel   [--from 0 --to 114.13] [--fps 30]
//   node viz/field.mjs stills --times 0,4.04,20.05,... --dir DIR

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn } from "node:child_process";
import { createCanvas, loadImage, registerFont } from "canvas";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");
const MODE = process.argv[2] || "canvas";
const flag = (k, d) => { const i = process.argv.indexOf("--" + k); return i > 0 && process.argv[i + 1] ? process.argv[i + 1] : d; };
const has = (k) => process.argv.includes("--" + k);

const COVER = flag("cover", `${process.env.HOME}/Documents/Shelf/wannadash-DISTROKID/wannadash-cover.jpg`);
const AUDIO = flag("audio", `${LANE}/out/wannadash-release.flac`);
const FONT = `${REPO}/slab/menuband/Sources/MenuBand/Resources/ywft-processing-bold.ttf`;
if (existsSync(FONT)) registerFont(FONT, { family: "YWFT" });

// ── the release edit, as a map from score seconds to shipped seconds ──
const BAR = 2.0;
const smooth = (u) => { u = Math.max(0, Math.min(1, u)); return u * u * (3 - 2 * u); };
const bpmAt = (bar) => 120 + 8 * smooth((bar - 44) / 60);
const EDGES = [0];
for (let b = 0; b < 114; b++) EDGES.push(EDGES[EDGES.length - 1] + BAR * 120 / bpmAt(b));
function warp(s) {
  let b = Math.floor(s / BAR); let f = s - b * BAR;
  if (b >= EDGES.length - 1) { b = EDGES.length - 2; f = s - b * BAR; }
  return EDGES[b] + f * 120 / bpmAt(b);
}
const SEGS = [[15.95, 20], [58, 74], [79.76, 120], [127.76, 136], [143.76, 167.95], [183.71, 192], [207.76, 224.80]];
const XF = 0.24; // cut-release.sh's constant-power seams (the first is a hard concat)
function ship(t) {
  const w = warp(t); let acc = 0;
  for (let i = 0; i < SEGS.length; i++) {
    const ws = warp(SEGS[i][0]), we = warp(SEGS[i][1]);
    if (w < ws) return null;
    if (w < we) return acc + (w - ws);
    acc += we - ws - (i > 0 ? XF : 0);
  }
  return null;
}
export const SHIPPED_TOTAL = SEGS.reduce((a, [s, e]) => a + warp(e) - warp(s), 0) - XF * 5;

// ── the receipt ───────────────────────────────────────────────────────
const receipt = JSON.parse(readFileSync(`${LANE}/out/cult-remix-v10.events.json`, "utf8"));
const WHO = {
  camille: [255, 105, 175], alex: [110, 255, 140], jeffrey: [255, 150, 70],
  none: [200, 190, 255],
};
function wordsOf(name) {
  if (typeof name !== "string") return null;
  if (/^runrealfast/.test(name)) return "run real fast";
  if (/^runitfast/.test(name)) return "run it fast";
  if (/^hideaway/.test(name)) return "hide away";
  if (/^iwannahide/.test(name)) return "i wanna hide";
  if (/^iwanna/.test(name)) return "i wanna";
  if (/^away/.test(name)) return "a—waaay";
  if (/^dotdotdash/.test(name)) return "dot dot dash";
  if (/^dotorg/.test(name)) return "whistlegraph dot org";
  if (/^(dot|voxdot)-/.test(name) || /^alt-\d+-dot/.test(name)) return "dot";
  if (/^alt-\d+-cult/.test(name) || /^(cult|cultlong)-/.test(name)) return "cult";
  if (/^alt-\d+-threeofus/.test(name) || name === "three-of-us") return "the three of us";
  if (/^(dash|dashlong|bassdash|sos-dash)/.test(name)) return "dash";
  return null;
}
// grains: every sung thing. Words: the ones the reel prints.
const GRAIN_VOICES = new Set(["dot", "dash", "cult", "lead", "alt", "stretch", "material", "sample"]);
function build(timeOf) {
  const grains = [], words = [], kicks = [], booms = [];
  receipt.events.forEach((e, i) => {
    const t0 = timeOf(e.t); if (t0 == null) return;
    const dur = Math.max(0.05, e.dur || 0.3);
    if (e.voice === "kick") { kicks.push({ t: t0, g: e.gain || 0.4 }); return; }
    if (e.voice === "spatial-explosion") { booms.push({ t: t0, dur: e.dur, s: e.strength }); return; }
    if (e.voice === "beep") { grains.push({ t: t0, dur, kind: "spark", pan: e.pan || 0, midi: 96, gain: 0.3, who: "none", i }); return; }
    if (!GRAIN_VOICES.has(e.voice)) return;
    const w = wordsOf(e.sample);
    if (e.voice === "sample" && !w) return;
    const who = e.who || (w === "i wanna" || w === "run real fast" || w === "hide away" ? "jeffrey" : "none");
    const kind = (w === "dash" || w === "cult" || w === "a—waaay") ? "dash" : w === "dot" ? "dot" : "word";
    grains.push({ t: t0, dur, kind, pan: e.pan || 0, midi: e.midi || 60, gain: e.gain || 0.2, who, i, stretch: e.stretch });
    if (w && (e.voice !== "dot" || (e.who && e.gain >= 0.2)) && e.voice !== "material")
      words.push({ t: t0, dur, w, who, voice: e.voice });
  });
  // coalesce runs of the same word by the same voice inside 0.3 s (the dot dot dot)
  words.sort((a, b) => a.t - b.t);
  const out = [];
  for (const w of words) {
    const p = out[out.length - 1];
    if (p && p.w === w.w && p.who === w.who && w.t - (p.t + p.dur) < 0.3 && w.w === "dot" && w.t - p.t < 0.12) { p.dur = Math.max(p.dur, w.t + w.dur - p.t); continue; }
    out.push({ ...w });
  }
  return { grains, kicks, booms, words: out };
}

// ── drawing ───────────────────────────────────────────────────────────
const rgba = (c, a) => `rgba(${c[0]},${c[1]},${c[2]},${a})`;
const hash = (i) => { let x = (i * 2654435761) >>> 0; x ^= x >>> 15; x = Math.imul(x, 2246822519); x ^= x >>> 13; return (x >>> 0) / 4294967296; };

function drawFrame(ctx, cover, data, T, o) {
  const W = ctx.canvas.width, H = ctx.canvas.height;
  const { grains, kicks, booms } = data;
  // kick lift: the most recent kick, decaying over 220 ms
  let lift = 0;
  for (let k = kicks.length - 1; k >= 0; k--) { const d = T - kicks[k].t; if (d < 0) continue; if (d > 0.25) break; lift = Math.max(lift, kicks[k].g * Math.exp(-d / 0.07)); }
  let boom = 0;
  for (const b of booms) { const d = T - b.t; if (d >= 0 && d < b.dur) boom = Math.max(boom, b.s * Math.exp(-d / (b.dur * 0.35))); }

  // the cover: fills the frame, drifting; a breath; kicks zoom it a hair
  const cw = cover.width, ch = cover.height;
  const fit = Math.max(W / cw, H / ch);
  const scale = fit * (1.06 + 0.02 * Math.sin(2 * Math.PI * T / o.breath) + 0.012 * lift + 0.02 * boom);
  const dw = cw * scale, dh = ch * scale;
  const driftX = o.driftX * Math.sin(2 * Math.PI * T / o.breath + 1.1);
  const shakeX = boom * 22 * Math.sin(T * 61), shakeY = boom * 12 * Math.cos(T * 47);
  const x = (W - dw) / 2 + driftX + shakeX, y = (H - dh) / 2 + shakeY;
  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
  ctx.fillStyle = "#04020c"; ctx.fillRect(0, 0, W, H);
  ctx.drawImage(cover, x, y, dw, dh);
  if (boom > 0.08) { // the explosion knocks the channels apart
    ctx.globalCompositeOperation = "lighter"; ctx.globalAlpha = 0.35 * boom;
    ctx.drawImage(cover, x + 14 * boom, y, dw, dh); ctx.drawImage(cover, x - 14 * boom, y, dw, dh);
  }
  if (lift > 0.02) { ctx.globalCompositeOperation = "lighter"; ctx.globalAlpha = 0.22 * lift; ctx.drawImage(cover, x, y, dw, dh); }
  ctx.globalAlpha = 1;

  // the powder: grains placed by pan (x) and pitch (y), jittered by index
  ctx.globalCompositeOperation = "lighter";
  const fieldW = W * 0.82, fieldH = H * (o.square ? 0.72 : 0.56), cx = W / 2, cy = H * (o.square ? 0.5 : 0.47);
  for (const g of grains) {
    const age = T - g.t; if (age < -0.02) continue;
    const life = g.kind === "dash" ? g.dur + 2.4 : g.kind === "spark" ? 0.5 : 2.2;
    if (age > life) continue;
    const j1 = hash(g.i) - 0.5, j2 = hash(g.i + 7919) - 0.5;
    const px = cx + (g.pan * 0.9 + j1 * 0.28) * fieldW / 2;
    const py = cy - ((g.midi - 62) / 16 + j2 * 0.3) * fieldH / 2;
    const col = WHO[g.who] || WHO.none;
    const fade = age < g.dur ? 1 : Math.max(0, 1 - (age - g.dur) / (life - g.dur));
    const pop = age < 0.09 ? 1 + 0.8 * (1 - age / 0.09) : 1;
    if (g.kind === "dash") {
      const len = Math.min(Math.min(age, g.dur) * o.pxPerSec * (g.stretch ? 0.35 : 1), W * 0.42);
      const ang = (j1 * 0.9) + (g.pan * 0.35);
      const w = (11 + 26 * Math.min(1, g.gain * 1.6)) * o.grain;
      ctx.save(); ctx.translate(px, py); ctx.rotate(ang);
      ctx.lineCap = "round";
      ctx.strokeStyle = rgba(col, 0.22 * fade); ctx.lineWidth = w * 2.6;
      ctx.beginPath(); ctx.moveTo(-len / 2, 0); ctx.lineTo(len / 2, 0); ctx.stroke();
      ctx.strokeStyle = rgba(col, 0.9 * fade); ctx.lineWidth = w;
      ctx.beginPath(); ctx.moveTo(-len / 2, 0); ctx.lineTo(len / 2, 0); ctx.stroke();
      ctx.strokeStyle = rgba([255, 255, 255], 0.55 * fade); ctx.lineWidth = w * 0.35;
      ctx.beginPath(); ctx.moveTo(-len / 2, 0); ctx.lineTo(len / 2, 0); ctx.stroke();
      ctx.restore();
    } else {
      const r = (g.kind === "spark" ? 5 : g.kind === "word" ? 14 : 9 + 15 * Math.min(1, g.gain * 2)) * pop * o.grain;
      ctx.fillStyle = rgba(col, 0.28 * fade); ctx.beginPath(); ctx.arc(px, py, r * 3.2, 0, 7); ctx.fill();
      ctx.fillStyle = rgba(col, 0.5 * fade); ctx.beginPath(); ctx.arc(px, py, r * 1.7, 0, 7); ctx.fill();
      ctx.fillStyle = rgba(col, 0.95 * fade); ctx.beginPath(); ctx.arc(px, py, r, 0, 7); ctx.fill();
      ctx.fillStyle = rgba([255, 255, 255], 0.7 * fade); ctx.beginPath(); ctx.arc(px, py, r * 0.42, 0, 7); ctx.fill();
      // a little scatter of dust around every sung dot
      if (g.kind === "dot" && age < 0.5) for (let s = 0; s < 5; s++) {
        const a = hash(g.i * 31 + s) * 6.283, dd = (12 + 70 * hash(g.i * 17 + s)) * (0.3 + age * 2) * o.grain;
        ctx.fillStyle = rgba(col, 0.5 * fade * (1 - age / 0.5));
        ctx.beginPath(); ctx.arc(px + Math.cos(a) * dd, py + Math.sin(a) * dd, 2.6 * o.grain, 0, 7); ctx.fill();
      }
    }
  }
  ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 1;
}

// the words, on three rails (reel only): Camille above, Alex in the middle, Jeffrey below
function drawWords(ctx, words, T) {
  const W = ctx.canvas.width, H = ctx.canvas.height;
  const live = words.filter((w) => T >= w.t && T < w.t + Math.max(w.dur, 0.42) + 0.25);
  if (!live.length) return;
  const rails = { camille: H * 0.70, none: H * 0.735, alex: H * 0.77, jeffrey: H * 0.84 };
  const seen = new Set();
  for (const w of [...live].reverse()) {
    if (seen.has(w.who)) continue; seen.add(w.who);
    const age = T - w.t, hold = Math.max(w.dur, 0.42);
    const out = age > hold ? Math.max(0, 1 - (age - hold) / 0.25) : 1;
    const pop = age < 0.08 ? 1.18 - 0.18 * age / 0.08 : 1;
    const big = w.voice === "lead" || w.voice === "dash" || w.voice === "sample" || w.voice === "stretch";
    const size = Math.round((big ? 118 : 88) * (w.w.length > 12 ? 0.62 : 1) * pop);
    const col = WHO[w.who] || WHO.none;
    ctx.font = `${size}px "YWFT", "Helvetica Neue", sans-serif`;
    ctx.textAlign = "center"; ctx.textBaseline = "middle";
    const y = rails[w.who] ?? rails.none;
    ctx.globalAlpha = out;
    ctx.lineJoin = "round"; ctx.lineWidth = size * 0.16; ctx.strokeStyle = "rgba(4,2,12,0.85)";
    ctx.strokeText(w.w, W / 2, y);
    ctx.fillStyle = rgba(col, 1); ctx.fillText(w.w, W / 2, y);
    ctx.globalAlpha = 1;
  }
}

// ── outputs ───────────────────────────────────────────────────────────
async function main() {
  const cover = await loadImage(COVER);
  if (MODE === "canvas") {
    // the hook: bars 29–33 in score time = 8.000 s of 120 BPM. Silent, so
    // score time is the loop clock; grains from the bars either side are
    // drawn wrapped so the tails cross the seam.
    const W = 1080, H = 1920, FPS = 30, LOOP = 8.0, T0 = 58.0;
    const timeOf = (t) => { const u = t - T0; return u >= -LOOP && u < 2 * LOOP ? u : null; };
    const data = build(timeOf);
    const wrap = (arr) => arr.flatMap((g) => [g, { ...g, t: g.t - LOOP }, { ...g, t: g.t + LOOP }]);
    data.grains = wrap(data.grains); data.kicks = wrap(data.kicks).sort((a, b) => a.t - b.t); data.booms = wrap(data.booms);
    const out = flag("out", `${LANE}/out/wannadash-canvas.mp4`);
    const ff = spawn("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
      "-an", "-c:v", "libx264", "-preset", "slow", "-crf", "17", "-pix_fmt", "yuv420p", "-movflags", "+faststart", out], { stdio: ["pipe", "inherit", "inherit"] });
    const cv = createCanvas(W, H), ctx = cv.getContext("2d");
    const N = LOOP * FPS;
    for (let f = 0; f < N; f++) {
      drawFrame(ctx, cover, data, f / FPS, { breath: LOOP, driftX: 90, pxPerSec: 220, grain: 1, square: false });
      if (!ff.stdin.write(cv.toBuffer("raw"))) await new Promise((r) => ff.stdin.once("drain", r));
    }
    ff.stdin.end();
    await new Promise((r) => ff.on("close", r));
    console.log(`✓ ${out}  (${N} frames, ${LOOP}s loop, silent)`);
  } else if (MODE === "reel") {
    const W = 1080, H = 1920, FPS = Number(flag("fps", 30));
    const from = Number(flag("from", 0)), to = Number(flag("to", SHIPPED_TOTAL));
    const data = build(ship);
    data.kicks.sort((a, b) => a.t - b.t);
    console.log(`${data.grains.length} grains · ${data.words.length} words · ${data.kicks.length} kicks · ${data.booms.length} booms · ${(to - from).toFixed(2)} s`);
    const out = flag("out", `${LANE}/out/wannadash-reel.mp4`);
    const ff = spawn("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-",
      "-ss", String(from), "-t", String(to - from), "-i", AUDIO,
      "-c:v", "libx264", "-preset", "faster", "-crf", "18", "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "256k", "-shortest", "-movflags", "+faststart", out], { stdio: ["pipe", "inherit", "inherit"] });
    const cv = createCanvas(W, H), ctx = cv.getContext("2d");
    const N = Math.round((to - from) * FPS);
    for (let f = 0; f < N; f++) {
      const T = from + f / FPS;
      drawFrame(ctx, cover, data, T, { breath: 16, driftX: 120, pxPerSec: 220, grain: 1, square: false });
      if (!has("no-words")) drawWords(ctx, data.words, T);
      if (f % (FPS * 10) === 0) process.stdout.write(`\r  ${T.toFixed(0)} s`);
      if (!ff.stdin.write(cv.toBuffer("raw"))) await new Promise((r) => ff.stdin.once("drain", r));
    }
    ff.stdin.end();
    await new Promise((r) => ff.on("close", r));
    console.log(`\n✓ ${out}`);
  } else if (MODE === "frame") {
    const W = 1080, H = 1920, T = Number(flag("t", 4.5));
    const data = build(ship); data.kicks.sort((a, b) => a.t - b.t);
    const cv = createCanvas(W, H), ctx = cv.getContext("2d");
    drawFrame(ctx, cover, data, T, { breath: 16, driftX: 120, pxPerSec: 220, grain: 1, square: false });
    if (!has("no-words")) drawWords(ctx, data.words, T);
    writeFileSync(flag("out", `${LANE}/out/.frame.png`), cv.toBuffer("image/png"));
  } else if (MODE === "stills") {
    const S = Number(flag("size", 1024));
    const dir = flag("dir", `${LANE}/out/stills`); mkdirSync(dir, { recursive: true });
    const times = flag("times", "0").split(",").map(Number);
    const data = build(ship); data.kicks.sort((a, b) => a.t - b.t);
    const cv = createCanvas(S, S), ctx = cv.getContext("2d");
    times.forEach((T, i) => {
      drawFrame(ctx, cover, data, T, { breath: 16, driftX: 0, pxPerSec: 110, grain: S / 1080, square: true });
      const p = `${dir}/sec-${i}.jpg`;
      writeFileSync(p, cv.toBuffer("image/jpeg", { quality: 0.86 }));
      console.log(`✓ ${p}  @ ${T}s`);
    });
  }
}
main();
