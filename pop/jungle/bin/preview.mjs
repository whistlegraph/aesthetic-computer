#!/usr/bin/env node
// jungle/bin/preview.mjs — animated square preview mp4 (undabeach-style,
// reusing pop/lib/preview-shared.mjs primitives).
//
// Base = the title-free colored-pencil illustration (so the bouncing
// title isn't doubled with the cover's baked wordmark). Per frame:
//   • drawCoverKenBurns  — slow breath + envelope-driven wobble
//   • drawTitleBounce    — "solafiya" chars bounce/brighten to the audio
//   • full-track waveform with a sweeping playhead
// BGRA frames → spawnFFmpegEncode (libx264 + aac, +faststart).
//
// Usage:
//   node bin/preview.mjs --slug solafiya
//   node bin/preview.mjs --slug solafiya --desktop
//   node bin/preview.mjs --slug solafiya --size 720x720 --fps 30

import { existsSync, copyFileSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { once } from "node:events";
import { createCanvas, loadImage } from "canvas";
import {
  checkYwftAvailable, decodeAudioMono, computeRmsEnvelope,
  prerenderTitleChars, drawCoverKenBurns, drawTitleBounce,
  drawEventWaveform, spawnFFmpegEncode, AUDIO_SR_DEFAULT,
} from "../../lib/preview-shared.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}

const SLUG = flags.slug || "solafiya";
const SIZE = flags.size || "1080x1080";
const FPS  = Number(flags.fps ?? 30);
const TITLE = String(flags.title || SLUG);
const [W, H] = SIZE.split("x").map(Number);

const ILLY  = `${LANE}/out/${SLUG}.illy.png`;
const COVER = `${LANE}/out/${SLUG}-cover.png`;
const AUDIO = `${LANE}/out/${SLUG}.mp3`;
const OUT   = `${LANE}/out/${SLUG}-preview.mp4`;
const BASE  = existsSync(ILLY) ? ILLY : COVER;   // prefer the title-free art

if (!existsSync(BASE)) {
  console.error(`✗ no illustration/cover: run render.mjs --slug ${SLUG} --cover`);
  process.exit(1);
}
if (!existsSync(AUDIO)) {
  console.error(`✗ audio missing: run render.mjs --slug ${SLUG}`);
  process.exit(1);
}

checkYwftAvailable();

console.log("  decoding audio …");
const { audio, sr, audioPeak } = decodeAudioMono(AUDIO, AUDIO_SR_DEFAULT);
const DURATION = audio.length / sr;
const ENV_FPS = 60;
const env = computeRmsEnvelope(audio, sr, ENV_FPS, DURATION);
const envAt = (t) => {
  const i = Math.round(t * ENV_FPS);
  return i >= 0 && i < env.length ? env[i] : 0;
};
const FRAMES = Math.ceil(DURATION * FPS);
console.log(`▸ ${SLUG} preview · ${W}x${H} · ${DURATION.toFixed(1)}s · ${FRAMES} frames`);

// jungle title palette — vivid sunlit/electric, matches the cover word.
const TITLE_PALETTE = [
  "#7fe05a", "#ff5ab0", "#ffd400", "#4adfff",
  "#b07cff", "#ff8a3d", "#9cff2e", "#ffffff",
];
const ptSize = Math.round(W * 0.092);            // ~99 at 1080
const { chars } = await prerenderTitleChars({
  text: TITLE, ptSize, palette: TITLE_PALETTE,
  shadowColor: "rgba(0,0,0,0.78)",
  assetsDir: `${LANE}/out/.${SLUG}-pvchars`,
});
const titleX = 44;                               // top-left, like the cover
const titleBaseY = Math.round(ptSize * 2.1);

const cover = await loadImage(BASE);
const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");

// ── lyrics train (trancenwaltz singalong, ported) — words from the
// Whisper sidecar, time-scaled by the vocal stretch the render applied
// (vocal placed at bar 0, rubberband-stretched to ~span the track).
const WORDS_PATH = `${LANE}/out/${SLUG}-fia-words.json`;
const VMAP_PATH  = `${LANE}/out/${SLUG}-vocal-map.json`;
let LW = [];
if (existsSync(WORDS_PATH) && existsSync(VMAP_PATH)) {
  const raw = JSON.parse(readFileSync(WORDS_PATH, "utf8"));
  const map = JSON.parse(readFileSync(VMAP_PATH, "utf8"));   // exact render mapping
  const STRETCH = map.stretch, OFF = map.startSec || 0;
  LW = raw
    .map((w) => ({ text: String(w.word || "").trim(),
                   t0: OFF + w.start * STRETCH, t1: OFF + w.end * STRETCH }))
    .filter((w) => w.text.length);
  console.log(`  lyric train · ${LW.length} words · stretch ×${STRETCH.toFixed(2)} @ +${OFF.toFixed(1)}s`);
} else if (existsSync(WORDS_PATH)) {
  console.warn(`  ⚠ no vocal-map.json — run render.mjs --slug ${SLUG} first (lyric train skipped for sync safety)`);
}
const SYM = "!<>-_/\\[]{}=+*#%&@$?:;~^|".split("");
function drawLyricTrain(audioT) {
  if (!LW.length) return;
  const first = LW[0], last = LW[LW.length - 1];
  if (audioT < first.t0 - 1 || audioT > last.t1 + 2.5) return;
  let idx = -1;
  for (let i = 0; i < LW.length; i++) { if (audioT >= LW[i].t0) idx = i; else break; }
  if (idx < 0) idx = 0;
  const act = LW[idx];
  const prog = Math.max(0, Math.min(1, (audioT - act.t0 - 0.12) / Math.max(0.1, act.t1 - act.t0)));
  const decodeProg = Math.min(1, prog / 0.45);
  const fs = Math.round(W * 0.052);
  const baseY = Math.round(H * 0.775);
  ctx.save();
  ctx.font = `bold ${fs}px Menlo, "DejaVu Sans Mono", monospace`;
  ctx.textBaseline = "middle"; ctx.textAlign = "left";
  const GAP = Math.round(fs * 1.1);
  const wOf = (s) => ctx.measureText(s).width;
  const js = [];
  for (let d = -1; d <= 2; d++) { const j = idx + d; if (j >= 0 && j < LW.length) js.push(j); }
  let lx = 0; const cx = {};
  for (const j of js) { const ww = wOf(LW[j].text); cx[j] = lx + ww / 2; lx += ww + GAP; }
  const DWELL = 0.74;
  let slide = prog <= DWELL ? 0 : (prog - DWELL) / (1 - DWELL);
  slide = slide * slide * (3 - 2 * slide);
  const focus = cx[idx] + ((cx[idx + 1] ?? cx[idx]) - cx[idx]) * slide;
  const rowX = W / 2 - focus;
  const tB = Math.floor(audioT * 14);
  for (const j of js) {
    const word = LW[j].text, active = j === idx;
    if (!active) {
      let sym = "";
      for (let c = 0; c < word.length; c++) {
        let s = ((j * 131 + c * 17 + tB * 7) >>> 0); s = (s ^ (s >>> 13)) >>> 0;
        sym += SYM[(s >>> 3) % SYM.length];
      }
      const tw = wOf(sym), dx = Math.round(rowX + cx[j] - tw / 2);
      ctx.globalAlpha = 0.3; ctx.fillStyle = "rgba(0,0,0,0.85)"; ctx.fillText(sym, dx + 3, baseY + 4);
      ctx.globalAlpha = 0.32; ctx.fillStyle = "rgba(225,222,212,1)"; ctx.fillText(sym, dx, baseY);
      continue;
    }
    const tw = wOf(word); let dx = Math.round(rowX + cx[j] - tw / 2);
    const edge = decodeProg * word.length;
    for (let c = 0; c < word.length; c++) {
      const dec = c < edge;
      let g;
      if (dec) g = word[c];
      else { let s = ((j * 131 + c * 17 + tB * 7) >>> 0); s = (s ^ (s >>> 13)) >>> 0; g = SYM[(s >>> 3) % SYM.length]; }
      const cw = wOf(g);
      const near = Math.max(0, 1 - Math.abs((c + 0.5) - edge) / 1.6);
      const cy = baseY - (near * near) * (fs * 0.32) - Math.sin(audioT * 9 + c) * 1.4;
      ctx.globalAlpha = 0.92; ctx.fillStyle = "rgba(0,0,0,0.9)"; ctx.fillText(g, dx + 3, cy + 4);
      ctx.globalAlpha = 1; ctx.fillStyle = dec ? "#aef240" : (near > 0.15 ? "#ffffff" : "rgba(180,210,170,0.85)");
      ctx.fillText(g, dx, cy); dx += cw;
    }
  }
  ctx.restore();
}

const ff = spawnFFmpegEncode({ audioPath: AUDIO, w: W, h: H, fps: FPS, outPath: OUT });

for (let f = 0; f < FRAMES; f++) {
  const t = f / FPS;
  const e = envAt(t);

  // 1) illustration ken-burns — slow breath + envelope wobble + kick punch
  drawCoverKenBurns(ctx, cover, t, {
    baseScale: 1.16, breathAmp: 0.05, breathPeriodSec: 22,
    env: e, wobbleAmp: 10, envWobbleAmp: 16, punch: e * e,
  });

  // 2) legibility scrims (top for title, bottom for waveform)
  const gTop = ctx.createLinearGradient(0, 0, 0, 300);
  gTop.addColorStop(0, "rgba(0,0,0,0.55)");
  gTop.addColorStop(1, "rgba(0,0,0,0)");
  ctx.fillStyle = gTop; ctx.fillRect(0, 0, W, 300);
  const gBot = ctx.createLinearGradient(0, H - 170, 0, H);
  gBot.addColorStop(0, "rgba(0,0,0,0)");
  gBot.addColorStop(1, "rgba(0,0,0,0.62)");
  ctx.fillStyle = gBot; ctx.fillRect(0, H - 170, W, 170);

  // 3) bouncing audio-reactive title
  drawTitleBounce(ctx, {
    chars, ptSize, baseX: titleX, baseY: titleBaseY,
    audioT: t, env: e, getEnvAt: envAt,
    charDelay: 0.03, bounceAmp: 58, restAlpha: 0.58,
    glowThreshold: 0.4, glowMax: 22, wobbleAmp: 4,
  });

  // 3.5) lyrics train — the word being sung decodes at centre
  drawLyricTrain(t);

  // 4) full-track waveform across the bottom + sweeping playhead
  const wfY = H - 96, wfH = 70;
  drawEventWaveform(ctx, audio, sr, audioPeak, 36, wfY, W - 72, wfH,
    0, DURATION, "rgba(150,255,120,0.45)", 1);
  const playX = 36 + (t / DURATION) * (W - 72);
  drawEventWaveform(ctx, audio, sr, audioPeak, 36, wfY, Math.max(1, playX - 36), wfH,
    0, t, "rgba(255,90,176,0.95)", 1);
  ctx.save();
  ctx.strokeStyle = "rgba(255,255,255,0.9)";
  ctx.lineWidth = 2 + e * 3;
  ctx.beginPath(); ctx.moveTo(playX, wfY - 6); ctx.lineTo(playX, wfY + wfH + 6); ctx.stroke();
  ctx.restore();

  const buf = canvas.toBuffer("raw");          // BGRA
  if (!ff.stdin.write(buf)) await once(ff.stdin, "drain");
  if (f % 150 === 0) process.stdout.write(`\r  frame ${f}/${FRAMES}`);
}
ff.stdin.end();
const [code] = await once(ff, "close");
process.stdout.write("\r");
if (code !== 0) { console.error(`✗ ffmpeg exit ${code}`); process.exit(1); }

let dest = OUT;
if (flags.desktop) {
  dest = resolve(homedir(), "Desktop", `${SLUG}-preview.mp4`);
  copyFileSync(OUT, dest);
}
console.log(`✓ ${dest.replace(REPO + "/", "")}`);
