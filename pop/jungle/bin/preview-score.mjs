#!/usr/bin/env node
// jungle/bin/preview-score.mjs — multi-section square preview mp4.
//
// Reads out/<slug>.struct.json (written by render.mjs) and shows a
// different illustration per section (out/<slug>-sec-<i>-<name>.png,
// from `gen-illy.mjs --sections`; falls back to the single illy if a
// section image is missing) with ken-burns + a slide/fade transition
// between sections, a progress bar with section ticks + the current
// section name, plus the bouncing title, waveform playhead, and the
// time-synced lyrics train. Reuses pop/lib/preview-shared.mjs.
//
// Usage:
//   node bin/preview-score.mjs --slug solafiya --desktop
//   node bin/preview-score.mjs --slug solafiya --size 720x720 --fps 30

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
  const n = process.argv[i + 1];
  if (n === undefined || n.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = n; i++; }
}

const SLUG  = flags.slug || "solafiya";
// --portrait → vertical 9:16 (undabeach-style); else square.
const PORTRAIT = flags.portrait === true;
const TAG   = PORTRAIT ? "-portrait" : "";
const SIZE  = flags.size || (PORTRAIT ? "1080x1920" : "1080x1080");
const FPS   = Number(flags.fps ?? 30);
const TITLE = String(flags.title || SLUG);
const [W, H] = SIZE.split("x").map(Number);

const STRUCT = `${LANE}/out/${SLUG}.struct.json`;
const AUDIO  = `${LANE}/out/${SLUG}.mp3`;
const ILLY   = `${LANE}/out/${SLUG}.illy.png`;
const COVER  = `${LANE}/out/${SLUG}-cover.png`;
const OUT    = `${LANE}/out/${SLUG}-preview-score${TAG}.mp4`;
const FALLBACK = existsSync(ILLY) ? ILLY : COVER;

for (const [nm, p] of [["struct", STRUCT], ["audio", AUDIO]]) {
  if (!existsSync(p)) {
    console.error(`✗ ${nm} missing: ${p.replace(REPO + "/", "")} — run render.mjs --slug ${SLUG} first`);
    process.exit(1);
  }
}
if (!existsSync(FALLBACK)) { console.error(`✗ no illustration for ${SLUG}`); process.exit(1); }
checkYwftAvailable();

const struct = JSON.parse(readFileSync(STRUCT, "utf8"));
const sections = struct.sections || [];
const safeName = (s) => s.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");

console.log("  decoding audio …");
const { audio, sr, audioPeak } = decodeAudioMono(AUDIO, AUDIO_SR_DEFAULT);
const DURATION = audio.length / sr;
const ENV_FPS = 60;
const env = computeRmsEnvelope(audio, sr, ENV_FPS, DURATION);
const envAt = (t) => { const i = Math.round(t * ENV_FPS); return i >= 0 && i < env.length ? env[i] : 0; };
const FRAMES = Math.ceil(DURATION * FPS);

// per-section image (section-specific if present, else the single illy)
const secImgPath = sections.map((s, i) => {
  const p = `${LANE}/out/${SLUG}-sec-${i}-${safeName(s.name)}.png`;
  return existsSync(p) ? p : FALLBACK;
});
const uniq = [...new Set([...secImgPath, FALLBACK])];
const imgCache = new Map();
for (const p of uniq) imgCache.set(p, await loadImage(p));
const distinct = new Set(secImgPath).size;
console.log(`▸ ${SLUG} score-preview · ${W}x${H} · ${DURATION.toFixed(1)}s · ${sections.length} sections (${distinct} distinct illys) · ${FRAMES} frames`);

// title chars
const TITLE_PALETTE = ["#7fe05a", "#ff5ab0", "#ffd400", "#4adfff", "#b07cff", "#ff8a3d", "#9cff2e", "#ffffff"];
const ptSize = Math.round(W * 0.088);
const { chars } = await prerenderTitleChars({
  text: TITLE, ptSize, palette: TITLE_PALETTE,
  shadowColor: "rgba(0,0,0,0.78)", assetsDir: `${LANE}/out/.${SLUG}-pscchars`,
});

// lyric train (exact render mapping via vocal-map.json)
const WORDS = `${LANE}/out/${SLUG}-fia-words.json`;
const VMAP  = `${LANE}/out/${SLUG}-vocal-map.json`;
let LW = [];
if (existsSync(WORDS) && existsSync(VMAP)) {
  const raw = JSON.parse(readFileSync(WORDS, "utf8"));
  const m = JSON.parse(readFileSync(VMAP, "utf8"));
  const OFF = m.startSec || 0, ST = m.stretch || 1;
  LW = raw.map((w) => ({ text: String(w.word || "").trim(), t0: OFF + w.start * ST, t1: OFF + w.end * ST }))
          .filter((w) => w.text.length);
  console.log(`  lyric train · ${LW.length} words · ×${ST.toFixed(2)} @ +${OFF.toFixed(1)}s`);
}
const SYM = "!<>-_/\\[]{}=+*#%&@$?:;~^|".split("");

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");

function curSection(t) {
  for (let i = 0; i < sections.length; i++) if (t >= sections[i].startSec && t < sections[i].endSec) return i;
  return sections.length ? sections.length - 1 : 0;
}
function drawLyricTrain(audioT) {
  if (!LW.length) return;
  const last = LW[LW.length - 1];
  if (audioT < LW[0].t0 - 1 || audioT > last.t1 + 2.5) return;
  let idx = -1;
  for (let i = 0; i < LW.length; i++) { if (audioT >= LW[i].t0) idx = i; else break; }
  if (idx < 0) idx = 0;
  const act = LW[idx];
  const prog = Math.max(0, Math.min(1, (audioT - act.t0 - 0.12) / Math.max(0.1, act.t1 - act.t0)));
  const dp = Math.min(1, prog / 0.45);
  const fs = Math.round(W * 0.052), baseY = Math.round(H * 0.775);
  ctx.save();
  ctx.font = `bold ${fs}px Menlo, "DejaVu Sans Mono", monospace`;
  ctx.textBaseline = "middle"; ctx.textAlign = "left";
  const GAP = Math.round(fs * 1.1), wOf = (s) => ctx.measureText(s).width;
  const js = [];
  for (let d = -1; d <= 2; d++) { const j = idx + d; if (j >= 0 && j < LW.length) js.push(j); }
  let lx = 0; const cx = {};
  for (const j of js) { const ww = wOf(LW[j].text); cx[j] = lx + ww / 2; lx += ww + GAP; }
  const DWELL = 0.74;
  let sl = prog <= DWELL ? 0 : (prog - DWELL) / (1 - DWELL); sl = sl * sl * (3 - 2 * sl);
  const focus = cx[idx] + ((cx[idx + 1] ?? cx[idx]) - cx[idx]) * sl;
  const rowX = W / 2 - focus, tB = Math.floor(audioT * 14);
  for (const j of js) {
    const word = LW[j].text, active = j === idx;
    if (!active) {
      let sym = "";
      for (let c = 0; c < word.length; c++) { let s = ((j * 131 + c * 17 + tB * 7) >>> 0); s = (s ^ (s >>> 13)) >>> 0; sym += SYM[(s >>> 3) % SYM.length]; }
      const tw = wOf(sym), dx = Math.round(rowX + cx[j] - tw / 2);
      ctx.globalAlpha = 0.3; ctx.fillStyle = "rgba(0,0,0,0.85)"; ctx.fillText(sym, dx + 3, baseY + 4);
      ctx.globalAlpha = 0.32; ctx.fillStyle = "rgba(225,222,212,1)"; ctx.fillText(sym, dx, baseY);
      continue;
    }
    const tw = wOf(word); let dx = Math.round(rowX + cx[j] - tw / 2);
    const edge = dp * word.length;
    for (let c = 0; c < word.length; c++) {
      const dec = c < edge;
      let g;
      if (dec) g = word[c];
      else { let s = ((j * 131 + c * 17 + tB * 7) >>> 0); s = (s ^ (s >>> 13)) >>> 0; g = SYM[(s >>> 3) % SYM.length]; }
      const cw = wOf(g);
      const near = Math.max(0, 1 - Math.abs((c + 0.5) - edge) / 1.6);
      const cy = baseY - (near * near) * (fs * 0.32) - Math.sin(audioT * 9 + c) * 1.4;
      ctx.globalAlpha = 0.92; ctx.fillStyle = "rgba(0,0,0,0.9)"; ctx.fillText(g, dx + 3, cy + 4);
      ctx.globalAlpha = 1; ctx.fillStyle = dec ? "#aef240" : (near > 0.15 ? "#fff" : "rgba(180,210,170,0.85)");
      ctx.fillText(g, dx, cy); dx += cw;
    }
  }
  ctx.restore();
}

const ff = spawnFFmpegEncode({ audioPath: AUDIO, w: W, h: H, fps: FPS, outPath: OUT });
const XF = 0.8;  // section crossfade seconds

for (let f = 0; f < FRAMES; f++) {
  const t = f / FPS;
  const e = envAt(t);
  const k = curSection(t);
  const sec = sections[k];
  const imgK = imgCache.get(secImgPath[k]) || imgCache.get(FALLBACK);

  drawCoverKenBurns(ctx, imgK, t, {
    baseScale: 1.18, breathAmp: 0.05, breathPeriodSec: 22,
    env: e, wobbleAmp: 9, envWobbleAmp: 15, punch: e * e,
  });
  // slide+fade into the next section's illustration
  if (sec && k + 1 < sections.length && t > sec.endSec - XF) {
    const pr = Math.min(1, (t - (sec.endSec - XF)) / XF);
    drawCoverKenBurns(ctx, imgCache.get(secImgPath[k + 1]) || imgCache.get(FALLBACK), t, {
      baseScale: 1.18, breathAmp: 0.05, breathPeriodSec: 22, env: e,
      wobbleAmp: 9, envWobbleAmp: 15,
      alpha: pr, offsetX: (1 - pr) * W * 0.14, fillBackground: false,
    });
  }

  // scrims
  const gT = ctx.createLinearGradient(0, 0, 0, 300);
  gT.addColorStop(0, "rgba(0,0,0,0.55)"); gT.addColorStop(1, "rgba(0,0,0,0)");
  ctx.fillStyle = gT; ctx.fillRect(0, 0, W, 300);
  const gB = ctx.createLinearGradient(0, H - 210, 0, H);
  gB.addColorStop(0, "rgba(0,0,0,0)"); gB.addColorStop(1, "rgba(0,0,0,0.66)");
  ctx.fillStyle = gB; ctx.fillRect(0, H - 210, W, 210);

  drawTitleBounce(ctx, {
    chars, ptSize, baseX: 44, baseY: Math.round(ptSize * 2.0),
    audioT: t, env: e, getEnvAt: envAt,
    charDelay: 0.03, bounceAmp: 54, restAlpha: 0.58, glowThreshold: 0.4, glowMax: 22, wobbleAmp: 4,
  });

  drawLyricTrain(t);

  // waveform + playhead
  const wfY = H - 116, wfH = 58;
  drawEventWaveform(ctx, audio, sr, audioPeak, 36, wfY, W - 72, wfH, 0, DURATION, "rgba(150,255,120,0.42)", 1);
  const playX = 36 + (t / DURATION) * (W - 72);
  drawEventWaveform(ctx, audio, sr, audioPeak, 36, wfY, Math.max(1, playX - 36), wfH, 0, t, "rgba(255,90,176,0.95)", 1);
  ctx.save();
  ctx.strokeStyle = "rgba(255,255,255,0.9)"; ctx.lineWidth = 2 + e * 3;
  ctx.beginPath(); ctx.moveTo(playX, wfY - 6); ctx.lineTo(playX, wfY + wfH + 6); ctx.stroke();
  ctx.restore();

  // progress bar with section ticks + current section name
  const pbY = H - 40, pbX = 36, pbW = W - 72, pbH = 9;
  ctx.fillStyle = "rgba(255,255,255,0.18)"; ctx.fillRect(pbX, pbY, pbW, pbH);
  ctx.fillStyle = "rgba(126,224,90,0.95)"; ctx.fillRect(pbX, pbY, pbW * Math.min(1, t / DURATION), pbH);
  ctx.fillStyle = "rgba(255,255,255,0.55)";
  for (const s of sections) {
    const x = pbX + pbW * (s.startSec / DURATION);
    ctx.fillRect(x, pbY - 4, 2, pbH + 8);
  }
  ctx.font = `600 ${Math.round(W * 0.026)}px "DejaVu Sans", Arial, sans-serif`;
  ctx.textBaseline = "alphabetic"; ctx.textAlign = "left";
  ctx.fillStyle = "rgba(0,0,0,0.8)"; ctx.fillText(sec ? sec.name : "", pbX + 2, pbY - 12 + 1.5);
  ctx.fillStyle = "rgba(255,255,255,0.92)"; ctx.fillText(sec ? sec.name : "", pbX, pbY - 12);
  ctx.textAlign = "right";
  const tc = `${String(Math.floor(t / 60)).padStart(1, "0")}:${String(Math.floor(t % 60)).padStart(2, "0")}`;
  ctx.fillStyle = "rgba(0,0,0,0.8)"; ctx.fillText(tc, pbX + pbW + 1.5, pbY - 12 + 1.5);
  ctx.fillStyle = "rgba(255,255,255,0.92)"; ctx.fillText(tc, pbX + pbW, pbY - 12);

  const buf = canvas.toBuffer("raw");
  if (!ff.stdin.write(buf)) await once(ff.stdin, "drain");
  if (f % 150 === 0) process.stdout.write(`\r  frame ${f}/${FRAMES}`);
}
ff.stdin.end();
const [code] = await once(ff, "close");
process.stdout.write("\r");
if (code !== 0) { console.error(`✗ ffmpeg exit ${code}`); process.exit(1); }

let dest = OUT;
if (flags.desktop) { dest = resolve(homedir(), "Desktop", `${SLUG}-preview-score${TAG}.mp4`); copyFileSync(OUT, dest); }
console.log(`✓ ${dest.replace(REPO + "/", "")}`);
