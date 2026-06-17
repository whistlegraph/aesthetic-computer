#!/usr/bin/env node
// momboba/bin/chrome-listener.mjs — the LANDSCAPE listener video for the
// full momabobasheep sleep mix (the YouTube "play the whole track" page).
//
// This is the long-form twin of bin/chrome-reel.mjs (the 9:16 promo). Where
// the reel is 56 s of full-motion felt, the listener is a 10-minute video you
// leave running: a felt still (or motion loop) breathing under a QUIET,
// LABEL-FREE scrolling score — jas's "video score" idea, dialed way down.
//
// Three layers (back → front):
//   A. background  — a felt diorama still on a slow ken-burns drift
//      (drawCoverKenBurns from preview-shared). Free; carries the runtime.
//   B. (future)    — short Seedance loops ping-ponged on key movements.
//   C. score band  — the scorodeon score (out/*.scorodeon.json) re-pitched as
//      a single quiet ribbon in the lower third: felt-tinted note pills
//      scrolling right→left past a fixed centerline. NO labels on the tracks
//      (jas, 2026-06-15) — no per-note text, no chord glyphs, no lane chips.
//   D. brand chrome — the pals side-stamps + "momabobasheep" climbing columns
//      hugging the L/R edges (ported from chrome-reel.mjs), the same mark every
//      pixsies reel/story wears. NO top-left title, NO bottom progress bar/clock
//      (jas, 2026-06-15) — just the side stamps + a quiet cross-fading movement
//      name top-right.
//
// Reuses pop/lib/preview-shared.mjs primitives (ken-burns, RMS envelope,
// YWFT typography, BGRA ffmpeg encode) — same machinery as every pop preview.
//
// DRY RUN (no Seedance, no new stills — uses the existing cover):
//   node pop/momboba/bin/scorodeon-data.mjs                 # ensure score JSON
//   node pop/momboba/bin/chrome-listener.mjs --frame 360    # one PNG to eyeball
//   node pop/momboba/bin/chrome-listener.mjs --window 320-410 --dryrun  # ~90s clip
//
// FULL (once the look is approved):
//   node pop/momboba/bin/chrome-listener.mjs --ss 2         # whole 10:00
//
// Options:
//   --bg <img>        background still (default out/momabobasheep-cover.png)
//   --score <json>    score data (default out/momabobasheep.scorodeon.json)
//   --audio <mp3>     master (default out/momabobasheep.mp3)
//   --size WxH        default 1920x1080
//   --zoom <sec>      seconds of score visible across the width (default 10)
//   --fps <n>         default 30
//   --ss <n>          supersample density. NOTE: the ffmpeg encoder is sized
//                     W*SS × H*SS, so --ss 2 emits a 4K (3840×2160) file — on
//                     the 8 GB box a full 600 s SS=2 run gets jetsam-killed
//                     (exit ~144). The plate is already 1080p so SS=2 only
//                     anti-aliases the vector chrome. Use --ss 1 for the full
//                     render; --ss 2 only for short --frame / --window checks.
//   --ribbon <a>      score band opacity 0..1 (default 0.5 — quiet)
//   --window a-b      render only seconds [a,b] (audio trimmed to match)
//   --frame <sec>     dump one PNG (out/listener-frame.png) and exit
//   --dryrun          suffix the output -dryrun (keeps the real name free)
//   --out <mp4>       explicit output path
//   --desktop         also copy the result to ~/Desktop

import {
  readFileSync, writeFileSync, existsSync, mkdtempSync, mkdirSync, rmSync, copyFileSync,
} from "node:fs";
import { resolve, dirname, join, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { tmpdir, homedir } from "node:os";
import { spawnSync, spawn } from "node:child_process";
import { once } from "node:events";
import { createCanvas, loadImage, ImageData } from "canvas";
import {
  decodeAudioMono, computeRmsEnvelope, prerenderTitleChars,
} from "../../lib/preview-shared.mjs";
import * as progress from "../../lib/render-progress.mjs";  // Slab menubar heartbeats
import { makeVerletString, hexToRgb } from "../../lib/cover-engine.mjs";  // /pop string+warp

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const rel = (p) => p.replace(REPO + "/", "");

const args = process.argv.slice(2);
const opt = (k, d) => { const i = args.indexOf(`--${k}`); return i >= 0 && args[i + 1] ? args[i + 1] : d; };
const has = (k) => args.includes(`--${k}`);

// background source: a moving PLATE video (the lush 10-min motion plate from
// assemble-listener.mjs) OR a single still on a ken-burns drift (dry runs).
const PLATE = opt("plate", null) ? resolve(process.cwd(), opt("plate", null)) : null;
const BG    = resolve(process.cwd(), opt("bg",    `${LANE}/out/momabobasheep-cover.png`));
const SCORE = resolve(process.cwd(), opt("score", `${LANE}/out/momabobasheep.scorodeon.json`));
const AUDIO = resolve(process.cwd(), opt("audio", `${LANE}/out/momabobasheep.mp3`));
const needBG = PLATE ? [[PLATE, "plate"]] : [[BG, "background"]];
for (const [p, what] of [...needBG, [SCORE, "score"], [AUDIO, "audio"]]) {
  if (!existsSync(p)) { console.error(`✗ missing ${what}: ${rel(p)}`); process.exit(1); }
}

const [W, H] = opt("size", "1920x1080").split("x").map(Number);
const ZOOM = parseFloat(opt("zoom", "10"));        // sec of score across the width
const FPS  = parseInt(opt("fps", "30"), 10);
const SS   = parseInt(opt("ss", "1"), 10);         // 1 = fast dry run, 2 = final
const WARP = has("warp") ? parseFloat(opt("warp", "1")) : 0;  // felt warp OFF by default (just the clips)
const ROT_TURNS = parseFloat(opt("turns", "1"));   // disc rotations over the track
const BOUNCE = parseFloat(opt("bounce", "0"));     // rotation bounce amount (0 = off, clean linear)
const WU_STR = parseFloat(opt("bend", "0.55"));    // base string→felt warp strength

const S = JSON.parse(readFileSync(SCORE, "utf8"));
const DUR = S.dur;

// ── render window (whole track unless --window a-b) ──────────────────────
let winStart = 0, winEnd = DUR;
const wflag = opt("window", null);
if (wflag) {
  const [a, b] = wflag.split("-").map(Number);
  winStart = Math.max(0, a); winEnd = Math.min(DUR, b);
}
const frameOnly = opt("frame", null) != null ? parseFloat(opt("frame", "0")) : null;

// ── note data — every melodic onset becomes a PLUCK on the verlet string ──
// (pitch → height on the string, gain → pluck force, lane colour → string
// tint at that point). No visible track ribbon any more (jas, 2026-06-15) —
// the music lives in how it bends the string, /pop-video style.
const notes = [];
for (const l of S.lanes) {
  for (const e of l.events) {
    if (e.pitch == null) continue;                 // beds/ribbons sit this out
    notes.push({ t: e.t, dur: e.dur, pitch: e.pitch, g: e.g ?? 0.2, color: l.color });
  }
}
notes.sort((a, b) => a.t - b.t);
let pLo = Infinity, pHi = -Infinity;
for (const n of notes) { if (n.pitch < pLo) pLo = n.pitch; if (n.pitch > pHi) pHi = n.pitch; }
const pSpan = Math.max(1, pHi - pLo);

const fmt = (t) => `${Math.floor(t / 60)}:${String(Math.floor(t % 60)).padStart(2, "0")}`;

const TMP = mkdtempSync(join(tmpdir(), "listener-"));
const cover = PLATE ? null : await loadImage(BG);

// ── audio: trim to the window so env + mux line up with the frames ───────
let audioPath = AUDIO;
if (winStart > 0 || winEnd < DUR) {
  audioPath = join(TMP, "win.wav");
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
    "-ss", String(winStart), "-t", String(winEnd - winStart),
    "-i", AUDIO, audioPath], { stdio: "inherit" });
  if (r.status !== 0) { console.error("✗ audio trim failed"); process.exit(1); }
}
const winDur = winEnd - winStart;
const { audio, sr } = decodeAudioMono(audioPath);   // mono 4 kHz over the window
const env = computeRmsEnvelope(audio, sr, FPS, winDur);

// fast shimmer of the envelope (env minus its ½-s mean) — drives the gentle
// ambient "current" that keeps the string flowing through quiet stretches.
const envFast = new Float32Array(env.length);
{
  const win = Math.max(1, Math.round(FPS * 0.5));
  let acc = 0;
  for (let i = 0; i < env.length; i++) {
    acc += env[i];
    if (i >= win) acc -= env[i - win];
    envFast[i] = Math.max(0, env[i] - acc / Math.min(i + 1, win));
  }
}

// ── canvas ───────────────────────────────────────────────────────────────
const canvas = createCanvas(W * SS, H * SS);
const ctx = canvas.getContext("2d");
ctx.scale(SS, SS);

// offscreen plate holder for the gentle analog "wavy" displacement pass.
const plateCanvas = createCanvas(W * SS, H * SS);
const pctx = plateCanvas.getContext("2d");
const WAVE = parseFloat(opt("wave", "0.45"));        // wavy displacement amount (0 = off)

// ── the /pop verlet string — a plucked physical thread down the centre that
// the music tugs (organic, watery) and that bends the felt under it. ───────
const PLAYHEAD_X = Math.round(W / 2);
const _vs = makeVerletString(ctx, { W, H, playheadX: PLAYHEAD_X, duration: DUR });

// warpUnderString — snapshot the felt, then redraw a tall strip around the
// playhead in row-bands, each sheared horizontally by the string's local
// deflection (sin² window → seamless edges). The picture stays upright but
// slumps left/right where the string is bent — the chillwave "string-vibe"
// watery flow. LEAN version: no rotated buffer (that 2200² per-frame copy was
// the memory hog that thrashed the 8 GB box); the visible string still
// rotates via withRotation, the warp itself stays upright.
const WU_HALF = 170, WU_W = WU_HALF * 2, WU_STEP = 6, WU_BANDS = 14, WU_BW = WU_W / WU_BANDS;
const wuWin = new Float64Array(WU_BANDS);
for (let b = 0; b < WU_BANDS; b++) wuWin[b] = Math.sin(Math.PI * ((b + 0.5) / WU_BANDS)) ** 2;
const WU_DSZ = Math.ceil(Math.hypot(W, H)) + 4;
const wuSnap = createCanvas(W, H), wuSnapC = wuSnap.getContext("2d");
const wuCR = createCanvas(WU_DSZ, WU_DSZ), wuCRC = wuCR.getContext("2d");
const ROT_CX = Math.round(W / 2), ROT_CY = Math.round(H / 2);
function warpUnderString(theta, strength) {
  const { devPeak } = _vs.deflection();
  if (Math.abs(devPeak) < 0.6) return;             // string ~straight → skip
  // snapshot the felt, counter-rotate by −θ so the redraw under +θ keeps the
  // CONTENT upright while the warp strip rides the rotated string.
  wuSnapC.clearRect(0, 0, W, H);
  wuSnapC.drawImage(canvas, 0, 0, W * SS, H * SS, 0, 0, W, H);  // logical snapshot
  wuCRC.setTransform(1, 0, 0, 1, 0, 0); wuCRC.clearRect(0, 0, WU_DSZ, WU_DSZ);
  wuCRC.translate(WU_DSZ / 2, WU_DSZ / 2); wuCRC.rotate(-theta); wuCRC.translate(-W / 2, -H / 2);
  wuCRC.drawImage(wuSnap, 0, 0); wuCRC.setTransform(1, 0, 0, 1, 0, 0);
  const sox = (WU_DSZ - W) / 2, soy = (WU_DSZ - H) / 2;
  const x0 = Math.round(PLAYHEAD_X - WU_HALF);
  ctx.save();
  ctx.translate(ROT_CX, ROT_CY); ctx.rotate(theta); ctx.translate(-ROT_CX, -ROT_CY);
  for (let y = 0; y < H; y += WU_STEP) {
    const dx = (_vs.needleXAt(y) - PLAYHEAD_X) * strength;
    for (let b = 0; b < WU_BANDS; b++) {
      const sx = b * WU_BW, shift = dx * wuWin[b];
      ctx.drawImage(wuCR, sox + x0 + sx, soy + y, WU_BW + 1, WU_STEP,
        x0 + sx + shift, y, WU_BW + 1, WU_STEP);
    }
  }
  ctx.restore();
}

// advance the string from the music: pluck on every note onset (organic
// accents) + a slow ambient current so it always flows like water.
let noteCursor = 0;
function pluckFromMusic(now, fi) {
  while (noteCursor < notes.length && notes[noteCursor].t <= now) {
    const n = notes[noteCursor++];
    if (n.t < winStart) continue;                  // pre-window: consume, don't pluck
    const yy = H * 0.16 + (1 - (n.pitch - pLo) / pSpan) * H * 0.68;
    const sign = (Math.floor(n.t * 7) % 2) ? 1 : -1;
    _vs.pluck(yy, (7 + n.g * 18) * WARP, sign, hexToRgb(n.color));
  }
  const eF = envFast[Math.min(envFast.length - 1, fi)] ?? 0;
  const ay = H * (0.30 + 0.40 * (0.5 + 0.5 * Math.sin(now * 0.23 + 1.0)));
  _vs.pluck(ay, (3 + eF * 8) * WARP, Math.sin(now * 0.41) >= 0 ? 1 : -1);
}

// ── the TRACK: /pop waveform note-blocks scrolling in lanes, crossing the
// string. Each note is sampled from the mix into little vertical waveform
// bars; they ghost in from the right, FLASH bright as they cross the
// playhead string (in sync with the pluck), then glow as "played". The bars
// ride the string's local bend so they slump with it. (preview-score-yt's
// drawLanes, adapted to the scorodeon lanes; no groove rotation — upright.) ──
const PX_PER_SEC = parseFloat(opt("speed", "700")); // scroll speed past the string — ZOOMED in, elements fly by
// higher-res mono JUST for the lane waveform shape — the 4 kHz env buffer is
// too smoothed, so notes read as flat blocks; 22 kHz shows real waveform.
const WAVE_SR = 22050;
const { audio: waveAudio } = decodeAudioMono(audioPath, WAVE_SR);
// push each lane colour AWAY from grey so the rows read as their own hue.
const satRgb = ([r, g, b], amt) => {
  const l = 0.3 * r + 0.59 * g + 0.11 * b;
  return [r + (r - l) * amt, g + (g - l) * amt, b + (b - l) * amt]
    .map((v) => Math.max(0, Math.min(255, Math.round(v))));
};
const LANES = S.lanes
  .map((l) => ({ name: l.name, rgb: satRgb(hexToRgb(l.color), 0.6), events: l.events.filter((e) => e.pitch != null).sort((a, b) => a.t - b.t) }))
  .filter((l) => l.events.length);
const LANE_TOP = H * 0.13, LANE_BOTTOM = H * 0.87;
const SLOT_H = (LANE_BOTTOM - LANE_TOP) / LANES.length;  // one row's vertical slot
const GROOVE_R = parseFloat(opt("groove", String(Math.round(Math.min(W, H) * 0.95))));  // record-arc radius
const DOTS = !has("bars");                           // render samples as tiny dots (default)
const DOT_SZ = parseFloat(opt("dot", "2.6"));        // tiny-dot size
const laneCenterY = {};
LANES.forEach((L, i) => { laneCenterY[L.name] = LANE_TOP + (i + 0.5) * SLOT_H; });

function drawLanes(now) {
  const halfSpan = (W * 0.62) / PX_PER_SEC;
  ctx.save();
  ctx.globalCompositeOperation = "screen";
  for (const L of LANES) {
    const yC = laneCenterY[L.name];
    const [lr, lg, lb] = L.rgb;
    // NOTE: the track does NOT follow the string bend — the warp only deforms
    // the felt underneath (jas, 2026-06-15). The waveforms stay clean.
    for (const ev of L.events) {
      if (ev.t > now + halfSpan) break;                  // sorted — rest are future
      const dur = ev.dur || 0.25;
      if (ev.t + dur < now - halfSpan) continue;
      const visDur = Math.min(dur, 0.30);                // cap sustain width
      const ex = PLAYHEAD_X + (ev.t - now) * PX_PER_SEC;
      const ew = Math.max(4, visDur * PX_PER_SEC);
      const fullH = SLOT_H * 0.62;                        // NORMALIZED row height
      const cols = Math.max(6, Math.floor(ew / 2.5));     // MORE data samples
      const blockW = Math.max(1, ew / cols - 1.2);        // THIN lines + space between
      const startSamp = Math.max(0, Math.floor((ev.t - winStart) * WAVE_SR));
      const endSamp = Math.min(waveAudio.length - 1, Math.floor((ev.t - winStart + visDur) * WAVE_SR));
      // NORMALIZE to THIS note's own peak so every note fills the row evenly
      // (quiet notes no longer read as flat slivers).
      let notePeak = 1e-5;
      for (let s = startSamp; s <= endSamp; s++) { const a = Math.abs(waveAudio[s]); if (a > notePeak) notePeak = a; }
      const norm = 1 / notePeak;
      const spc = (endSamp - startSamp) / cols;
      for (let c = 0; c < cols; c++) {
        const s0 = startSamp + Math.floor(c * spc), s1 = Math.min(endSamp, startSamp + Math.floor((c + 1) * spc));
        let pk = 0; for (let s = s0; s < s1; s++) { const a = Math.abs(waveAudio[s]); if (a > pk) pk = a; }
        pk = Math.min(1, pk * norm);                      // 0..1 within the note
        const dt = now - (ev.t + (c / cols) * visDur);
        let alpha = 0.30, bright = 0;
        if (dt >= 0 && dt < 0.06) { alpha = 1.0; bright = 0.32; }   // CROSSING — flash
        else if (dt >= 0.06) { alpha = 0.9; bright = 0.42; }        // PLAYED — glowy
        const fr = Math.round(lr + (255 - lr) * bright);            // keep the HUE
        const fg = Math.round(lg + (255 - lg) * bright);
        const fb = Math.round(lb + (255 - lb) * bright);
        const half = Math.max(DOTS ? 0 : 2, (pk * fullH) / 2);
        const bx = ex + (c / cols) * ew;
        // GROOVE CURVE — rotate each block about the disc centre by its
        // distance from the string, so the rows bow into record-groove arcs.
        const aGroove = (bx - PLAYHEAD_X) / GROOVE_R;
        ctx.save();
        ctx.translate(ROT_CX, ROT_CY); ctx.rotate(aGroove); ctx.translate(-ROT_CX, -ROT_CY);
        ctx.fillStyle = `rgba(${fr},${fg},${fb},${alpha.toFixed(3)})`;
        if (DOTS) {                                       // tiny dots tracing the waveform envelope
          ctx.fillRect(bx, yC - half - DOT_SZ / 2, DOT_SZ, DOT_SZ);
          ctx.fillRect(bx, yC + half - DOT_SZ / 2, DOT_SZ, DOT_SZ);
        } else {
          ctx.fillRect(bx, yC - half, blockW, half * 2);
        }
        ctx.restore();
      }
    }
  }
  ctx.restore();
  ctx.globalCompositeOperation = "source-over";
}

// ── D. pals side-stamp identity (ported from chrome-reel.mjs) ─────────────
// Two purple-pals logos hugging the L/R edges (rotated ±90°, drowsy wiggle +
// LED pulse on the swells) with "momabobasheep" as per-character climbing
// columns — the same brand mark every pixsies reel/story wears.
const envAtT = (t) =>
  env[Math.max(0, Math.min(env.length - 1, Math.floor((t - winStart) * FPS)))] ?? 0;

// per-movement felt tint (warm night); brightened toward white by progress
const MOVE_TINTS = {
  dusk: [150, 168, 196], drowse: [196, 162, 126], descend: [176, 110, 78],
  deep: [92, 128, 182], dream: [150, 132, 196], still: [184, 176, 160],
  vivid: [160, 136, 200], morning: [206, 168, 120], resolve: [150, 150, 176],
};
const assetsDir = join(TMP, "chrome-assets");
mkdirSync(assetsDir, { recursive: true });

const TITLE = S.title.charAt(0).toUpperCase() + S.title.slice(1);  // "Momabobasheep"
const { chars: titleChars, totalWidth: titleTotalW } = await prerenderTitleChars({
  text: TITLE, ptSize: 96, palette: ["#FFFFFF"], shadowColor: null, assetsDir,
});
const PALS_S = 118, PALS_HALF = PALS_S / 2;
const EDGE_X = 58;                                   // logical px from the edge
const CHAR_SCALE = (H * 0.30) / Math.max(1, titleTotalW);
const CHAR_SPAN = titleTotalW * CHAR_SCALE;
const BOUNCE_BUF = 20;
// staggered on Y so the two side marks aren't parallel: LEFT lower, RIGHT higher
const LCHARS_CY = H * 0.74, RCHARS_CY = H * 0.28;    // left badge low, right badge high
// pals sits BEFORE the first char on each side. Left column reads top→bottom
// (start at top → pals above); right column reads bottom→top (start at bottom
// → pals below). Right was at the top = AFTER the title; flip it under.
const LPALS_CY = LCHARS_CY - CHAR_SPAN / 2 - BOUNCE_BUF - PALS_HALF;
const RPALS_CY = RCHARS_CY + CHAR_SPAN / 2 + BOUNCE_BUF + PALS_HALF;

const PALS_WM_SIZE = 200;
let palsImg = null, palsBlur = null;
{
  const svg = `${REPO}/system/public/purple-pals.svg`;
  const png = `${assetsDir}/pals.png`, blurPng = `${assetsDir}/pals-blur.png`;
  const r = spawnSync("rsvg-convert", ["-w", String(PALS_WM_SIZE * 2), "-h", String(PALS_WM_SIZE * 2), "-o", png, svg]);
  if (r.status === 0 && existsSync(png)) {
    palsImg = await loadImage(png);
    const rb = spawnSync("magick", [png, "-channel", "A", "-blur", "0x1.5", "+channel", blurPng]);
    palsBlur = (rb.status === 0 && existsSync(blurPng)) ? await loadImage(blurPng) : palsImg;
  }
}
if (!palsImg) console.error("⚠ pals watermark failed to rasterize — chrome will skip it");

// offscreen tinting canvases (used purely as image sources)
const wmCanvas = createCanvas(8, 8), wmCtx = wmCanvas.getContext("2d");
const charTintCanvas = createCanvas(2, 2), charTintCtx = charTintCanvas.getContext("2d");

function sectionRgb(now) {
  const m = S.movements?.find((x) => now >= x.t0 && now < x.t1) ?? S.movements?.at(-1);
  const [r, g, b] = MOVE_TINTS[m?.name] ?? [180, 180, 180];
  const lp = m ? Math.max(0, Math.min(1, (now - m.t0) / Math.max(0.001, m.t1 - m.t0))) : 0;
  const k = 0.28 * lp;
  return [Math.round(r + (255 - r) * k), Math.round(g + (255 - g) * k), Math.round(b + (255 - b) * k)];
}
function palsTinted(rgb, src) {
  const ww = src.width, wh = src.height;
  wmCanvas.width = ww; wmCanvas.height = wh;
  wmCtx.clearRect(0, 0, ww, wh);
  wmCtx.globalCompositeOperation = "source-over"; wmCtx.drawImage(src, 0, 0);
  wmCtx.globalCompositeOperation = "source-in";
  wmCtx.fillStyle = `rgb(${rgb[0]},${rgb[1]},${rgb[2]})`; wmCtx.fillRect(0, 0, ww, wh);
  wmCtx.globalCompositeOperation = "source-over";
  return wmCanvas;
}
function tintCharGlyph(img, rgb) {
  const w = img.width, h = img.height;
  charTintCanvas.width = w; charTintCanvas.height = h;
  charTintCtx.globalCompositeOperation = "source-over";
  charTintCtx.clearRect(0, 0, w, h); charTintCtx.drawImage(img, 0, 0);
  charTintCtx.globalCompositeOperation = "source-in";
  charTintCtx.fillStyle = `rgb(${rgb[0]},${rgb[1]},${rgb[2]})`; charTintCtx.fillRect(0, 0, w, h);
  charTintCtx.globalCompositeOperation = "source-over";
  return charTintCanvas;
}

function drawWatermark(now, e0) {
  if (!palsImg) return;
  const s = PALS_S, TAU = Math.PI * 2, u = now / DUR;
  const col = sectionRgb(now);
  const glow = Math.min(1, e0) ** 2;
  const ledCol = [
    Math.round(col[0] + (255 - col[0]) * glow * 0.6),
    Math.round(col[1] + (255 - col[1]) * glow * 0.6),
    Math.round(col[2] + (255 - col[2]) * glow * 0.6),
  ];
  const wig = 9 * Math.sin(TAU * 26 * u + 0.7) + 3 * Math.sin(TAU * 9 * u);
  const bob = 2.2 * Math.sin(TAU * 17 * u + 2.1) + 1.1 * Math.sin(TAU * 31 * u);
  const swiv = 0.04 * Math.sin(TAU * 19 * u) + 0.02 * Math.sin(TAU * 38 * u + 1.4);
  const spots = [
    { cx: EDGE_X - wig, cy: LPALS_CY + bob, rot: Math.PI / 2 + swiv },
    { cx: W - EDGE_X + wig, cy: RPALS_CY - bob, rot: -Math.PI / 2 - swiv },
  ];
  const passes = [["multiply", 0.78], ["color-burn", 0.42], ["overlay", 0.58], ["source-over", 0.06]];
  for (const sp of spots) {
    ctx.save();
    ctx.translate(sp.cx, sp.cy); ctx.rotate(sp.rot);
    palsTinted([0, 0, 0], palsImg);                  // soft drop shadow
    ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.24;
    ctx.drawImage(wmCanvas, -s / 2 + 3, -s / 2 + 4, s, s);
    palsTinted(col, palsBlur);                       // 4-pass felt seep
    for (const [op, a] of passes) {
      ctx.globalCompositeOperation = op; ctx.globalAlpha = a;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    }
    ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.30;
    ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    if (glow > 0.001) {                              // LED pulse on the swells
      palsTinted(ledCol, palsBlur);
      ctx.globalCompositeOperation = "screen"; ctx.globalAlpha = 0.14 + 0.7 * glow;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
      palsTinted(ledCol, palsImg);
      ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.16 + 0.42 * glow;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    }
    ctx.restore();
  }
  ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 1;
  drawPalsTitleChars(now);
}

function drawPalsTitleChars(now) {
  const TAU = Math.PI * 2, u = now / DUR;
  const wig = 8 * Math.sin(TAU * 30 * u + 3.6) + 3 * Math.sin(TAU * 12 * u + 1.1);
  const span = CHAR_SPAN;
  const palsRgb = sectionRgb(now);
  const spots = [
    { charsCx: EDGE_X + 4 - wig, cy: LCHARS_CY, rot: Math.PI / 2 },
    { charsCx: W - EDGE_X - 4 + wig, cy: RCHARS_CY, rot: -Math.PI / 2 },
  ];
  const startX = -span / 2;
  for (const sp of spots) {
    ctx.save();
    ctx.translate(sp.charsCx, sp.cy); ctx.rotate(sp.rot);
    for (let i = 0; i < titleChars.length; i++) {
      const ch = titleChars[i];
      if (!ch.img) continue;
      const x = startX + ch.prefixWidth * CHAR_SCALE;
      const dw = ch.img.width * CHAR_SCALE, dh = ch.img.height * CHAR_SCALE;
      const charEnv = envAtT(now - i * 0.03);
      const lift = 3 * Math.sin(now * 3.0 + i * 0.8) * (0.3 + charEnv);
      const y = -dh / 2 + lift;
      const charRot = 0.025 * Math.sin(now * 3.1 + i * 1.15);
      ctx.save();
      ctx.translate(x + dw / 2, y + dh / 2); ctx.rotate(charRot);
      ctx.translate(-(x + dw / 2), -(y + dh / 2));
      const sh = 0.5 + 0.5 * Math.sin(now * 3.2 - i * 0.7);
      const charRgb = [
        Math.round(palsRgb[0] + (255 - palsRgb[0]) * sh * 0.35),
        Math.round(palsRgb[1] + (255 - palsRgb[1]) * sh * 0.35),
        Math.round(palsRgb[2] + (255 - palsRgb[2]) * sh * 0.35),
      ];
      ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.24;
      ctx.drawImage(tintCharGlyph(ch.img, [0, 0, 0]), x + 3, y + 4, dw, dh);
      for (const [op, a] of [["multiply", 0.78], ["color-burn", 0.42], ["overlay", 0.58], ["source-over", 0.06]]) {
        ctx.globalCompositeOperation = op; ctx.globalAlpha = a;
        ctx.drawImage(tintCharGlyph(ch.img, charRgb), x, y, dw, dh);
      }
      ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 0.46;
      ctx.drawImage(tintCharGlyph(ch.img, palsRgb), x, y, dw, dh);
      if (charEnv > 0.45) {                           // shimmer glint on swells
        ctx.globalCompositeOperation = "screen";
        ctx.globalAlpha = 0.14 + 0.46 * Math.min(1, (charEnv - 0.45) / 0.55);
        ctx.drawImage(tintCharGlyph(ch.img, charRgb), x, y, dw, dh);
      }
      ctx.restore();
    }
    ctx.restore();
  }
  ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 1;
}

// gentle analog "wavy" displacement of the felt — thin horizontal bands eased
// by a slow sine, overscanned so no edge ever shows. (jas: "wavy fuzz".)
function drawPlateWave(plate, now) {
  pctx.putImageData(plate, 0, 0);
  const BH = Math.max(2, Math.round(6 * SS));
  const amp = WAVE * 0.004 * PLATE_W;
  const Z = 1.025, ox = -(Z - 1) / 2 * PLATE_W;
  ctx.save();
  ctx.setTransform(1, 0, 0, 1, 0, 0);                // device px
  for (let y = 0; y < PLATE_H; y += BH) {
    const dh = Math.min(BH, PLATE_H - y);
    const sx = amp * Math.sin(y * 0.018 + now * 1.1) + amp * 0.4 * Math.sin(y * 0.045 - now * 0.7);
    ctx.drawImage(plateCanvas, 0, y, PLATE_W, dh, ox + sx, y, PLATE_W * Z, dh);
  }
  ctx.restore();
}

let envBounce = 0, rotSpring = 0;                     // smooth-rotation easing state
function drawFrame(now, fi, plate) {
  const e0 = env[Math.min(env.length - 1, fi)] ?? 0;

  // advance the verlet string from the music (organic, watery flow)
  pluckFromMusic(now, fi);
  _vs.step();

  // ── A. background — the felt motion plate (gentle wavy displacement) ───
  if (plate) { if (WAVE) drawPlateWave(plate, now); else ctx.putImageData(plate, 0, 0); }
  else drawCoverKB(now, e0);

  // ── B. the rotating disc — felt bends under the string, and the track
  //       (waveform note-blocks) + string spin together as one record ────
  // bouncy + smooth: steady base drift + an underdamped spring the music
  // nudges (overshoots & settles) + a slow sine bob — not a stiff clock.
  let rotMod = 0;                                        // bounce OFF by default
  if (BOUNCE) {
    envBounce += (e0 - envBounce) * 0.04;
    rotSpring += (envBounce - rotSpring) * 0.08;
    rotMod = (rotSpring * 0.5 + 0.02 * Math.sin(now * 0.4)) * BOUNCE;
  }
  const theta = (now / DUR) * Math.PI * 2 * ROT_TURNS + rotMod;
  if (WARP) warpUnderString(theta, WU_STR * WARP);        // felt still bends
  // string + track removed (jas, 2026-06-16) — the (invisible) plucked string
  // keeps deforming the felt; toggle the overlay back on with --overlay.
  if (has("overlay")) _vs.withRotation(theta, () => { drawLanes(now); _vs.draw(); });

  // ── C. pals side-stamps + climbing title columns (the brand mark) ──────
  drawWatermark(now, e0);
}

// ken-burns wrapper — square cover into landscape, gentle and sleepy
function drawCoverKB(now, e0) {
  // inline (preview-shared.drawCoverKenBurns expects device-px canvas; our
  // ctx is logical-scaled, so call it against the logical W/H via a shim)
  drawKB(ctx, cover, now, {
    baseScale: 1.06, breathAmp: 0.05, breathPeriodSec: 24,
    env: e0, wobbleAmp: 10, envWobbleAmp: 12,
  });
}

// a logical-space ken-burns (mirrors preview-shared, sized to W×H not canvas px)
function drawKB(c, img, t, o) {
  const baseScale = o.baseScale, breathAmp = o.breathAmp, breathT = o.breathPeriodSec;
  const e = o.env ?? 0, wAmp = o.wobbleAmp, wEnv = o.envWobbleAmp;
  const coverScale = Math.max(W / img.width, H / img.height);
  const breath = breathAmp * (0.5 - 0.5 * Math.cos(t * (2 * Math.PI / breathT)));
  const zoom = baseScale + breath;
  const bw = img.width * coverScale * zoom, bh = img.height * coverScale * zoom;
  const wx = wAmp * Math.sin(t * 0.5) + wAmp * 0.4 * Math.cos(t * 0.9) + e * wEnv * Math.sin(t * 1.6);
  const wy = wAmp * 0.7 * Math.cos(t * 0.6) + e * wEnv * 0.7 * Math.cos(t * 1.3);
  let dx = (W - bw) / 2 + wx, dy = (H - bh) / 2 + wy;
  dx = Math.min(0, Math.max(W - bw, dx));
  dy = Math.min(0, Math.max(H - bh, dy));
  c.fillStyle = "#0b0712"; c.fillRect(0, 0, W, H);
  c.drawImage(img, dx, dy, bw, bh);
}

// ── plate frame source (the lush motion background) ──────────────────────
const PLATE_W = W * SS, PLATE_H = H * SS, PLATE_BYTES = PLATE_W * PLATE_H * 4;

// decode the plate to raw RGBA frames at device resolution, from winStart.
function openPlateStream() {
  return spawn("ffmpeg", ["-hide_banner", "-loglevel", "error",
    ...(winStart > 0 ? ["-ss", String(winStart)] : []),
    "-i", PLATE, "-f", "rawvideo", "-pix_fmt", "rgba",
    "-s", `${PLATE_W}x${PLATE_H}`, "-r", String(FPS), "-"],
    { stdio: ["ignore", "pipe", "ignore"] });
}

// a frame reader over a paused stream: returns the next frame as ImageData,
// or null at end-of-stream (caller holds the last frame to the finish).
function makeFrameReader(stream) {
  let buf = Buffer.alloc(0), ended = false;
  stream.on("end", () => { ended = true; });
  return async function next() {
    while (buf.length < PLATE_BYTES) {
      const chunk = stream.read();
      if (chunk) { buf = Buffer.concat([buf, chunk]); continue; }
      if (ended) return null;
      await once(stream, "readable").catch(() => { ended = true; });
    }
    const frame = buf.subarray(0, PLATE_BYTES);
    buf = buf.subarray(PLATE_BYTES);
    return new ImageData(new Uint8ClampedArray(frame), PLATE_W, PLATE_H);
  };
}

// one plate frame at absolute time t (for --frame).
function plateFrameAt(t) {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error",
    "-ss", String(t), "-i", PLATE, "-frames:v", "1",
    "-f", "rawvideo", "-pix_fmt", "rgba", "-s", `${PLATE_W}x${PLATE_H}`, "-"],
    { encoding: "buffer", maxBuffer: PLATE_BYTES + 4096 });
  if (r.status !== 0 || !r.stdout || r.stdout.length < PLATE_BYTES) return null;
  return new ImageData(new Uint8ClampedArray(r.stdout.subarray(0, PLATE_BYTES)), PLATE_W, PLATE_H);
}

// ── --frame: dump one PNG and exit ───────────────────────────────────────
if (frameOnly != null) {
  const localFi = Math.round((frameOnly - winStart) * FPS);
  drawFrame(frameOnly, Math.max(0, localFi), PLATE ? plateFrameAt(frameOnly) : null);
  const fp = `${LANE}/out/listener-frame.png`;
  writeFileSync(fp, canvas.toBuffer("image/png"));
  rmSync(TMP, { recursive: true, force: true });
  console.log(`✓ frame @${frameOnly}s → ${rel(fp)}`);
  process.exit(0);
}

// ── encode ───────────────────────────────────────────────────────────────
let OUT = opt("out", null);
if (!OUT) {
  const suffix = has("dryrun") ? "-listener-dryrun" : "-listener";
  OUT = `${LANE}/out/momabobasheep${suffix}.mp4`;
}
OUT = resolve(process.cwd(), OUT);

const total = Math.ceil(winDur * FPS);
console.log(`▸ listener · ${S.title} · ${W * SS}x${H * SS}@${FPS} (${SS}x) · zoom ${ZOOM}s · window ${fmt(winStart)}–${fmt(winEnd)} · ${total} frames`);
progress.begin({ type: "video", label: `${TITLE} listener · ${total} frames` });  // → Slab bar
// custom encode so we can run a SHARPENING post-pass (unsharp ≈ lanczos edge
// sharpen) over the overlaid UI — crisps the thin waveform lines + string.
const SHARP = has("nosharp") ? null : parseFloat(opt("sharp", "1.3"));
const GRAIN = parseFloat(opt("grain", "10"));         // analog film-fuzz amount (0 = off)
// post chain: gentle temporal grain (fuzz) → whole-frame luma+chroma sharpen.
const post = [
  ...(GRAIN > 0 ? [`noise=alls=${GRAIN}:allf=t+u`] : []),
  ...(SHARP ? [`unsharp=5:5:${SHARP}:5:5:${(SHARP * 0.5).toFixed(2)}`] : []),
];
const enc = spawn("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W * SS}x${H * SS}`, "-r", String(FPS), "-i", "-",
  "-i", audioPath,
  ...(post.length ? ["-vf", post.join(",")] : []),
  "-c:v", "libx264", "-preset", "faster", "-crf", "19", "-threads", "0",
  "-maxrate", opt("maxrate", "14M"), "-bufsize", "28M",   // cap so grain doesn't bloat the file
  "-c:a", "aac", "-b:a", "192k", "-pix_fmt", "yuv420p", "-shortest", "-movflags", "+faststart", OUT],
  { stdio: ["pipe", "inherit", "inherit"] });
const plateProc = PLATE ? openPlateStream() : null;
const nextPlate = plateProc ? makeFrameReader(plateProc.stdout) : null;
let lastPlate = null;
const t0 = Date.now();
for (let fi = 0; fi < total; fi++) {
  let plate = null;
  if (nextPlate) { plate = await nextPlate(); if (plate) lastPlate = plate; else plate = lastPlate; }
  drawFrame(winStart + fi / FPS, fi, plate);
  if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
  progress.update(((fi + 1) / total) * 100, { done: fi + 1, total });
  if (total >= 10 && fi % Math.floor(total / 10) === 0)
    console.log(`  ${Math.round((fi / total) * 100)}% · ${((Date.now() - t0) / 1000).toFixed(0)}s`);
}
enc.stdin.end();
if (plateProc) { try { plateProc.kill(); } catch { /* already gone */ } }
await new Promise((res, rej) => { enc.on("close", (c) => (c === 0 ? res() : rej(new Error(`ffmpeg exit ${c}`)))); });
progress.end();
rmSync(TMP, { recursive: true, force: true });
console.log(`✓ ${rel(OUT)} (${((Date.now() - t0) / 1000).toFixed(0)}s)`);
if (has("desktop")) {
  const d = join(homedir(), "Desktop", basename(OUT));
  copyFileSync(OUT, d);
  console.log(`✓ → ${d}`);
}
process.exit(0);  // force-exit: a lingering plate/encode pipe child can keep
                  // the event loop alive at 0% CPU after a successful render
                  // (the zombies that piled up + thrashed the 8 GB box).
