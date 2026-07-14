#!/usr/bin/env node
// caption-burn.mjs — burn styled captions onto a video + attach an audio track.
//
// This ffmpeg build has NO libass, so captions are drawn in node-canvas over a
// raw-frame pump (same approach as pop/menuband/bin/finalize-vo.mjs), then the
// audio (VO) is muxed in by the encoder. Portrait-friendly defaults.
//
// Usage: node marketing/bin/caption-burn.mjs <video.mp4> <audio.wav> <captions.srt> <out.mp4> [W H FPS]

import { readFileSync } from "node:fs";
import { spawn } from "node:child_process";
import { once } from "node:events";
import { createCanvas, registerFont } from "canvas";
import { spawnFFmpegEncode } from "../../pop/lib/preview-shared.mjs";

const [VIDEO, AUDIO, SRT, OUT, wArg, hArg, fpsArg, crfArg] = process.argv.slice(2);
if (!VIDEO || !AUDIO || !SRT || !OUT) {
  console.error("usage: caption-burn.mjs <video> <audio> <srt> <out> [W H FPS CRF]");
  process.exit(1);
}
const W = Number(wArg) || 420, H = Number(hArg) || 900, FPS = Number(fpsArg) || 25;
const CRF = Number(crfArg) || 18;

try { registerFont("/System/Library/Fonts/Helvetica.ttc", { family: "Cap" }); } catch {}
try { registerFont("/System/Library/Fonts/SFNS.ttf", { family: "Cap" }); } catch {}

// ── parse SRT → cues [{s,e,t}] ──
function parseSRT(txt) {
  const cues = [];
  for (const block of txt.replace(/\r/g, "").trim().split(/\n\n+/)) {
    const lines = block.split("\n");
    const ti = lines.findIndex((l) => l.includes("-->"));
    if (ti === -1) continue;
    const m = lines[ti].match(/(\d+):(\d+):(\d+)[,.](\d+)\s*-->\s*(\d+):(\d+):(\d+)[,.](\d+)/);
    if (!m) continue;
    const s = +m[1] * 3600 + +m[2] * 60 + +m[3] + +m[4] / 1000;
    const e = +m[5] * 3600 + +m[6] * 60 + +m[7] + +m[8] / 1000;
    const t = lines.slice(ti + 1).join(" ").trim();
    if (t) cues.push({ s, e, t });
  }
  return cues;
}
const cues = parseSRT(readFileSync(SRT, "utf8"));
console.log(`▸ ${cues.length} caption cues`);

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const cueAt = (t) => cues.find((c) => t >= c.s && t < c.e) || null;

const CAP_MAX_W = W - 48;
const CAP_LINE_H = 30;
const FONT = "700 24px Cap";
function wrap(text) {
  ctx.font = FONT;
  const out = [];
  let line = "";
  for (const word of text.split(" ")) {
    const cand = line ? `${line} ${word}` : word;
    if (line && ctx.measureText(cand).width > CAP_MAX_W) { out.push(line); line = word; }
    else line = cand;
  }
  if (line) out.push(line);
  return out;
}
function drawCaption(t) {
  const c = cueAt(t);
  if (!c) return;
  ctx.font = FONT;
  ctx.textAlign = "center";
  ctx.textBaseline = "alphabetic";
  ctx.lineJoin = "round";
  const lines = wrap(c.t);
  const base = H - 78; // bottom, clear of the edge
  lines.forEach((line, i) => {
    const y = base - (lines.length - 1 - i) * CAP_LINE_H;
    ctx.strokeStyle = "rgba(0,0,0,0.92)";
    ctx.lineWidth = 6;
    ctx.strokeText(line, W / 2, y);
    ctx.fillStyle = "rgba(0,0,0,0.5)";
    ctx.fillText(line, W / 2 + 1.5, y + 2);
    ctx.fillStyle = "#ffffff";
    ctx.fillText(line, W / 2, y);
  });
}

console.log("▸ burning captions (node-canvas) + muxing audio …");
const dec = spawn("ffmpeg", ["-loglevel", "error", "-i", VIDEO, "-f", "rawvideo", "-pix_fmt", "rgba", "-"],
  { stdio: ["ignore", "pipe", "inherit"] });
const enc = spawnFFmpegEncode({ audioPath: AUDIO, w: W, h: H, fps: FPS, outPath: OUT, crf: CRF });
const FRAME_BYTES = W * H * 4;
const fbuf = Buffer.alloc(FRAME_BYTES);
const img = ctx.createImageData(W, H);
let off = 0, fi = 0;
for await (const ch of dec.stdout) {
  let cOff = 0;
  while (cOff < ch.length) {
    const n = Math.min(FRAME_BYTES - off, ch.length - cOff);
    ch.copy(fbuf, off, cOff, cOff + n); off += n; cOff += n;
    if (off === FRAME_BYTES) {
      off = 0;
      img.data.set(fbuf); ctx.putImageData(img, 0, 0);
      drawCaption(fi / FPS);
      if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
      fi++;
    }
  }
}
enc.stdin.end();
await new Promise((res, rej) => { enc.on("close", (c) => (c === 0 ? res() : rej(new Error(`ffmpeg exit ${c}`)))); });
console.log(`✓ ${OUT} (${fi} frames, captions + audio)`);
