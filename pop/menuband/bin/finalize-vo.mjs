#!/usr/bin/env node
// menuband/bin/finalize-vo.mjs — narrated + subtitled cut of the reel.
//
// Produces menuband-reel-vo.mp4: the waltz DUCKED under the jeffrey VO, with
// burned LACMA-style captions (Helvetica bold, white + hard black outline,
// bottom center). This ffmpeg build has NO libass, so captions are drawn in
// node-canvas (same engine as the sim) over a raw-frame pump, and the audio
// is pre-mixed (sidechain duck + amix) to a temp track that gets muxed in.
//
// Usage: node pop/menuband/bin/finalize-vo.mjs

import { readFileSync, existsSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn, spawnSync } from "node:child_process";
import { once } from "node:events";
import { createCanvas, registerFont } from "canvas";
import { spawnFFmpegEncode } from "../../lib/preview-shared.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "out");
const REEL = `${OUT}/menuband-reel.mp4`;
const VO = `${OUT}/vo.mp3`;
const TIMINGS = `${OUT}/vo.timings.json`;
const MIXED = `${OUT}/vo-mixed.m4a`;
const FINAL = `${OUT}/menuband-reel-vo.mp4`;

for (const p of [REEL, VO, TIMINGS]) {
  if (!existsSync(p)) { console.error(`✗ missing ${p} — run sim + chrome-reel + gen-vo first`); process.exit(1); }
}
try { registerFont("/System/Library/Fonts/Helvetica.ttc", { family: "MBCap" }); } catch {}
try { registerFont("/System/Library/Fonts/SFNS.ttf", { family: "MBCap" }); } catch {}

const W = 1080, H = 1920, FPS = 30;

// ── caption cues: split each VO line into short readable chunks, time them
// across the line's [t0,t0+dur] window proportionally by length ────────────
const lines = JSON.parse(readFileSync(TIMINGS, "utf8"));
const MAX = 30;
function chunk(text) {
  const out = [];
  for (const sent of text.split(/(?<=[.!?])\s+/)) {
    if (sent.length <= MAX) { if (sent.trim()) out.push(sent.trim()); continue; }
    let buf = "";
    for (const piece of sent.split(/(?<=[,—])\s+/)) {
      if ((buf + " " + piece).trim().length > MAX && buf) { out.push(buf.trim()); buf = piece; }
      else buf = (buf + " " + piece).trim();
    }
    if (buf.trim()) out.push(buf.trim());
  }
  return out;
}
const cues = [];
for (const ln of lines) {
  if (!ln.text || !ln.text.trim()) continue;
  const parts = chunk(ln.text);
  const totalChars = parts.reduce((a, p) => a + p.length, 0) || 1;
  let cursor = ln.t0;
  for (const p of parts) {
    const span = (p.length / totalChars) * ln.dur;
    cues.push({ s: cursor, e: cursor + span, t: p.replace(/\s+—\s*$/, "") });
    cursor += span;
  }
}
console.log(`▸ ${cues.length} caption cues`);
cues.forEach((c) => console.log(`   ${c.s.toFixed(1)}–${c.e.toFixed(1)}s  "${c.t}"`));

// ── 1. pre-mix audio: waltz (from the reel) ducked under the boosted VO ─────
console.log("▸ mixing audio (waltz ducked under VO) …");
const mix = spawnSync("ffmpeg", [
  "-y", "-i", REEL, "-i", VO,
  "-filter_complex",
  `[1:a]volume=1.5,asplit=2[vo1][vo2];` +
  `[0:a][vo1]sidechaincompress=threshold=0.04:ratio=6:attack=5:release=260[duck];` +
  `[duck][vo2]amix=inputs=2:duration=first:normalize=0[a]`,
  "-map", "[a]", "-c:a", "aac", "-b:a", "256k", MIXED,
], { stdio: ["ignore", "ignore", "pipe"] });
if (mix.status !== 0) { console.error(`✗ audio mix failed:\n${mix.stderr.toString().slice(-500)}`); process.exit(1); }

// ── 2. caption frame pump: decode reel → draw active caption → encode ───────
const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
function cueAt(t) { return cues.find((c) => t >= c.s && t < c.e) || null; }
/// Captions were drawn as one unbroken line, so a long cue ran off both edges
/// of the frame — "play music straight from your macos menu bar." reached
/// x=0 and x=1079 and lost its first and last letters. Wrap to a margin.
const CAP_MAX_W = W - 140;
const CAP_LINE_H = 64;
function wrapCaption(text) {
  const lines = [];
  let line = "";
  for (const word of text.split(" ")) {
    const candidate = line ? `${line} ${word}` : word;
    if (line && ctx.measureText(candidate).width > CAP_MAX_W) {
      lines.push(line);
      line = word;
    } else {
      line = candidate;
    }
  }
  if (line) lines.push(line);
  return lines;
}

function drawCaption(t) {
  const c = cueAt(t);
  if (!c) return;
  ctx.font = "700 52px MBCap";
  ctx.textAlign = "center";
  ctx.textBaseline = "alphabetic";
  ctx.lineJoin = "round";
  const lines = wrapCaption(c.t);
  // Stack upward from the baseline, so a two-line cue grows toward the middle
  // and its last line stays where a one-line cue would have sat.
  const base = H - 520;         // raised toward center (clear of Reels UI)
  lines.forEach((line, i) => {
    const y = base - (lines.length - 1 - i) * CAP_LINE_H;
    // hard black outline (LACMA look: BorderStyle outline + shadow)
    ctx.strokeStyle = "rgba(0,0,0,0.92)";
    ctx.lineWidth = 9;
    ctx.strokeText(line, W / 2, y);
    // soft drop shadow then white fill
    ctx.fillStyle = "rgba(0,0,0,0.5)";
    ctx.fillText(line, W / 2 + 2, y + 3);
    ctx.fillStyle = "white";
    ctx.fillText(line, W / 2, y);
  });
}

console.log("▸ burning captions (node-canvas) + muxing mixed audio …");
const dec = spawn("ffmpeg", ["-loglevel", "error", "-i", REEL, "-f", "rawvideo", "-pix_fmt", "rgba", "-"],
  { stdio: ["ignore", "pipe", "inherit"] });
const enc = spawnFFmpegEncode({ audioPath: MIXED, w: W, h: H, fps: FPS, outPath: FINAL, crf: 18 });
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
try { unlinkSync(MIXED); } catch {}
console.log(`✓ ${FINAL} (${fi} frames, captions + VO)`);
