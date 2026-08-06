#!/usr/bin/env node
// Animate the radial Special Sign cover as a playable circular score. The
// notation turns once beneath a fixed twelve-o'clock needle; active lanes glow
// at the receiver, while the outer field breathes with speed-following wetness.

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { once } from "node:events";
import { createCanvas, loadImage, registerFont } from "canvas";
import { decodeAudioMono, computeRmsEnvelope, spawnFFmpegEncode } from "../../lib/preview-shared.mjs";
import * as progress from "../../lib/render-progress.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const arg = (key, fallback) => {
  const i = process.argv.indexOf(key);
  return i >= 0 && process.argv[i + 1] ? resolve(process.argv[i + 1]) : fallback;
};
const value = (key, fallback) => {
  const i = process.argv.indexOf(key);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : fallback;
};
const scorePath = arg("--score", resolve(HERE, "../out/review/Special-Sign-SPEED-WET.scorodeon.json"));
const coverPath = arg("--cover", resolve(HERE, "../out/review/Special-Sign-SPEED-WET-cover-3000.jpg"));
const audioPath = arg("--audio", resolve(HERE, "../out/review/Special-Sign-SPEED-WET-AUDITION.wav"));
const automationPath = arg("--automation", resolve(HERE, "../out/review/Special-Sign-SPEED-WET-AUDITION.automation.json"));
const outPath = arg("--out", resolve(HERE, "../out/review/Special-Sign-SPEED-WET-circular-score.mp4"));
const frameOut = arg("--frame-out", resolve(HERE, "../out/review/Special-Sign-SPEED-WET-circular-score-frame.png"));
const frameOnly = process.argv.includes("--frame");
const frameTime = Number(value("--frame", "47.55"));
const W = Number(value("--size", "1080")), H = W;
const FPS = Number(value("--fps", "30"));
for (const path of [scorePath, coverPath, audioPath, automationPath]) {
  if (!existsSync(path)) throw new Error(`missing input: ${path}`);
}

const S = JSON.parse(readFileSync(scorePath, "utf8"));
const A = JSON.parse(readFileSync(automationPath, "utf8"));
const cover = await loadImage(coverPath);
const { audio, sr } = decodeAudioMono(audioPath);
const env = computeRmsEnvelope(audio, sr, FPS, S.dur);
const canvas = createCanvas(W, H), ctx = canvas.getContext("2d");
try { registerFont("/System/Library/Fonts/Menlo.ttc", { family: "MenloCircle" }); } catch {}
const TAU = Math.PI * 2, CREAM = "#f7f4eb", INK = "#202127", RED = "#b3402e", AQUA = "#3e7c8a";
const CX = W / 2, CY = H * 0.545, R = W * 0.378;
const mono = (px) => `${px}px MenloCircle, monospace`;
const automationAt = (t, key) => {
  const p = A.points, u = Math.max(0, Math.min(p.length - 1, t * 60));
  const i = Math.min(p.length - 2, Math.floor(u)), f = u - i;
  return p[i][key] * (1 - f) + p[i + 1][key] * f;
};
const rounded = (x, y, w, h, r) => {
  ctx.beginPath(); ctx.roundRect(x, y, w, h, r);
};
const fmt = (t) => `${Math.floor(t / 60)}:${String(Math.floor(t % 60)).padStart(2, "0")}`;

function drawFrame(now, fi) {
  const wet = automationAt(now, "wet"), speed = automationAt(now, "speedTurnsPerSecond");
  const e = env[Math.min(env.length - 1, fi)] ?? 0;
  ctx.fillStyle = CREAM; ctx.fillRect(0, 0, W, H);

  // Static title is literally the cover's title block; only its score turns.
  ctx.drawImage(cover, 0, 0, 3000, 430, W * 0.055, W * 0.018, W * 0.89, W * 0.128);
  ctx.strokeStyle = "rgba(32,33,39,.055)"; ctx.lineWidth = 1;
  for (let r = R * 0.25; r <= R * 1.08; r += R * 0.125) {
    ctx.beginPath(); ctx.arc(CX, CY, r, 0, TAU); ctx.stroke();
  }
  for (let i = 0; i < 24; i++) {
    const a = TAU * i / 24;
    ctx.beginPath(); ctx.moveTo(CX + Math.cos(a) * R * 0.18, CY + Math.sin(a) * R * 0.18);
    ctx.lineTo(CX + Math.cos(a) * R * 1.08, CY + Math.sin(a) * R * 1.08); ctx.stroke();
  }

  // The 3000-square source cover's score is centered at 1500,1605. Rotate its
  // circular notation counterclockwise so current score-time meets the needle.
  ctx.save();
  ctx.beginPath(); ctx.arc(CX, CY, R, 0, TAU); ctx.clip();
  ctx.translate(CX, CY); ctx.rotate(-TAU * now / S.dur);
  ctx.drawImage(cover, 350, 455, 2300, 2300, -R, -R, R * 2, R * 2);
  ctx.restore();

  // Speed/wet field: broad spatial bloom outside the paper score.
  ctx.save();
  ctx.shadowColor = `rgba(62,124,138,${0.25 + wet * 0.45})`;
  ctx.shadowBlur = 8 + wet * 30 + e * 16;
  ctx.strokeStyle = `rgba(62,124,138,${0.20 + wet * 0.67})`;
  ctx.lineWidth = 4 + wet * 20 + e * 6;
  ctx.beginPath(); ctx.arc(CX, CY, R + 10, 0, TAU); ctx.stroke();
  ctx.restore();

  // Sounding lanes strike the fixed receiver line at their literal cover radii.
  S.lanes.forEach((lane, laneIndex) => {
    const active = lane.events.filter((event) => now >= event.t && now <= event.t + event.dur);
    if (!active.length) return;
    const radius = R * (344 + laneIndex * ((945 - 344) / 12)) / 1150;
    const y = CY - radius, strength = Math.min(1, active.reduce((n, event) => n + (event.g ?? 0.2), 0));
    ctx.save(); ctx.shadowColor = lane.color; ctx.shadowBlur = 12 + 26 * strength;
    ctx.fillStyle = lane.color; ctx.globalAlpha = 0.50 + 0.45 * strength;
    ctx.beginPath(); ctx.arc(CX, y, 3 + 8 * strength, 0, TAU); ctx.fill(); ctx.restore();
  });

  // Fixed twelve-o'clock needle and complete progress orbit.
  ctx.strokeStyle = "rgba(32,33,39,.14)"; ctx.lineWidth = 3;
  ctx.beginPath(); ctx.arc(CX, CY, R + 28, 0, TAU); ctx.stroke();
  ctx.strokeStyle = RED; ctx.lineWidth = 5; ctx.lineCap = "round";
  ctx.beginPath(); ctx.arc(CX, CY, R + 28, -Math.PI / 2, -Math.PI / 2 + TAU * now / S.dur); ctx.stroke();
  ctx.fillStyle = RED; ctx.beginPath();
  ctx.moveTo(CX, CY - R - 42); ctx.lineTo(CX - 12, CY - R - 17); ctx.lineTo(CX + 12, CY - R - 17); ctx.closePath(); ctx.fill();
  ctx.fillRect(CX - 2, CY - R - 19, 4, 38);

  // Static receiver disc: wetness becomes the legible center of the film.
  ctx.fillStyle = "rgba(247,244,235,.96)"; ctx.beginPath(); ctx.arc(CX, CY, R * 0.205, 0, TAU); ctx.fill();
  ctx.strokeStyle = wet > 0.8 ? AQUA : RED; ctx.lineWidth = 4 + wet * 5;
  ctx.beginPath(); ctx.arc(CX, CY, R * 0.187, 0, TAU); ctx.stroke();
  ctx.textAlign = "center"; ctx.fillStyle = INK; ctx.font = mono(W * 0.034);
  ctx.fillText(`${Math.round(wet * 100)}% WET`, CX, CY - 5);
  ctx.fillStyle = "rgba(32,33,39,.62)"; ctx.font = mono(W * 0.017);
  const fieldLabel = speed > 0.60 && wet < 0.80 ? "DRY FLYBY" : wet >= 0.72 ? "SPIRAL FIELD" : "DRY FIELD";
  ctx.fillText(fieldLabel, CX, CY + 28);

  const current = S.movements?.find((m) => now >= m.t0 && now < m.t1);
  ctx.font = mono(W * 0.017); ctx.fillStyle = "rgba(32,33,39,.65)";
  ctx.fillText(current?.name ?? "Special Sign", CX, H - W * 0.060);
  ctx.font = mono(W * 0.024); ctx.fillStyle = INK;
  ctx.fillText(`${fmt(now)} / ${fmt(S.dur)}`, CX, H - W * 0.026);
  ctx.textAlign = "left"; ctx.font = mono(W * 0.014); ctx.fillStyle = "rgba(32,33,39,.46)";
  rounded(W * 0.045, H - W * 0.083, W * 0.205, W * 0.038, 8); ctx.fillStyle = "rgba(62,124,138,.10)"; ctx.fill();
  ctx.fillStyle = "rgba(32,33,39,.55)"; ctx.fillText("SPEED → SPACE", W * 0.060, H - W * 0.057);
}

if (frameOnly) {
  drawFrame(frameTime, Math.floor(frameTime * FPS));
  writeFileSync(frameOut, canvas.toBuffer("image/png"));
  console.log(`✓ frame @ ${frameTime}s → ${frameOut}`);
  process.exit(0);
}

const total = Math.ceil(S.dur * FPS);
console.log(`▸ circular score · ${W}×${H}@${FPS} · ${total} frames`);
progress.begin({ type: "video", label: `Special Sign circular score · ${total} frames` });
const enc = spawnFFmpegEncode({ audioPath, w: W, h: H, fps: FPS, outPath, crf: 17 });
const started = Date.now();
for (let fi = 0; fi < total; fi++) {
  drawFrame(fi / FPS, fi);
  if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
  progress.update(fi / total * 100, { done: fi, total });
  if (fi % Math.floor(total / 10) === 0) console.log(`  ${Math.round(fi / total * 100)}% · ${((Date.now() - started) / 1000).toFixed(0)}s`);
}
enc.stdin.end();
await new Promise((ok, fail) => enc.on("close", (code) => code === 0 ? ok() : fail(new Error(`ffmpeg exit ${code}`))));
progress.end();
console.log(`✓ ${outPath}`);
