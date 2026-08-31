#!/usr/bin/env node
// loner/bin/pastiche/chrome-reel.mjs — the pastiche reel's chrome:
// Camille's lyric arrives word by word as she sings it (timings from
// viz/wordclock.json — the same stroke-per-word map the score used),
// and a RADIAL TRACK OVERLAY turns beneath a fixed twelve-o'clock
// needle (the circular-score mechanic from nullabye, miniaturized):
// the record's waveform wrapped into a ring, played arc warming to
// red, with the title + artist beneath. YWFT throughout, shared
// helpers from pop/lib/preview-shared.mjs.
//
// Runs as a post pass over run.sh's graded collage:
//   node pop/loner/bin/pastiche/chrome-reel.mjs
// Reads  $PASTICHE_WORK/graded.mp4 + $PASTICHE_WORK/audio-reel.wav
// Writes pop/loner/out/lonerclub-v4pid-reel.mp4

import { spawn } from "node:child_process";
import { readFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { once } from "node:events";
import { homedir } from "node:os";
import { createCanvas } from "canvas";
import {
  magickRenderText, decodeAudioMono, computeRmsEnvelope, spawnFFmpegEncode,
} from "../../../lib/preview-shared.mjs";
import * as progress from "../../../lib/render-progress.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LONER = resolve(HERE, "..", "..");
const WORK = process.env.PASTICHE_WORK || `${homedir()}/.cache/ac/pastiche`;
const BASE = `${WORK}/graded.mp4`;
const AUDIO = `${WORK}/audio-reel.wav`;
const OUT = `${LONER}/out/lonerclub-v4pid-reel.mp4`;
const assetsDir = `${WORK}/chrome-assets`;
mkdirSync(assetsDir, { recursive: true });

const W = 1080, H = 1920, FPS = 30;
const { audio, sr } = decodeAudioMono(AUDIO);
const TOTAL = audio.length / sr;
const FRAMES = Math.round(TOTAL * FPS);
const env = computeRmsEnvelope(audio, sr, FPS, TOTAL);
const envAt = (t) =>
  env[Math.max(0, Math.min(env.length - 1, Math.floor(t * FPS)))] ?? 0;

const INK = "rgba(32,28,34,";
const CREAM = "rgba(247,242,235,";
const ROSE = "rgba(179,64,72,";

// ── lyrics: the wordclock, syllables gathered back into their word ─────
const clock = JSON.parse(readFileSync(`${LONER}/viz/wordclock.json`, "utf8"))
  .filter((e) => e.t0 < TOTAL - 0.5);
const GATHER = { pa: "pa", tient: "patient", ly: "patiently" };
const words = clock.map((e, i) => ({
  text: GATHER[e.word] ?? e.word,
  t0: e.t0,
  t1: i + 1 < clock.length && clock[i + 1].t0 - e.t0 < 3.0
    ? clock[i + 1].t0
    : e.t1 + 0.35,
}));
const wordImgs = {};
for (const w of words) {
  if (wordImgs[w.text]) continue;
  wordImgs[w.text] = await magickRenderText(w.text, {
    ptSize: 108, fill: "#f7f2eb",
    stroke: "#201c22", strokeWidth: 3,
    shadow: "#201c22", shadowSpec: "100x0+6+7",
    outPath: `${assetsDir}/word-${w.text}.png`,
  });
}

// ── the virtual whistlegraph: named vector strokes, drawn by a pen ─
// viz/wg-strokes.json (trace-strokes.py) holds the drawing as named
// curves — skeleton geometry from the finished ink, each branch
// matched to its wordclock syllable and oriented the way the pen
// moved. The score draws itself per pass, stroke by stroke under the
// words, and a virtual nib rides the ink tip.
import { readFileSync as rf } from "node:fs";
const wg = JSON.parse(rf(`${LONER}/viz/wg-strokes.json`, "utf8"));
const WG_W = 470, WG_H = Math.round(WG_W * wg.h / wg.w);
const WG_X = (W - WG_W) / 2, WG_Y = H * 0.185;
const WG_S = WG_W / wg.w;
const STROKES = wg.strokes.map((s) => {
  const subs = s.sub.map((pts) =>
    pts.map(([x, y]) => [WG_X + x * WG_S, WG_Y + y * WG_S]));
  const lens = subs.map((P) => {
    let L = 0;
    for (let i = 1; i < P.length; i++) {
      L += Math.hypot(P[i][0] - P[i - 1][0], P[i][1] - P[i - 1][1]);
    }
    return Math.max(L, 3);
  });
  return { v0: s.v0, v1: s.v1, subs, lens,
           total: lens.reduce((a, b) => a + b, 0) };
});

// reel time -> drawing time, from the wordclock's passes
const passes = [[clock[0]]];
for (let i = 1; i < clock.length; i++) {
  if (clock[i].t0 - clock[i - 1].t1 > 2.0) passes.push([]);
  passes.at(-1).push(clock[i]);
}
const vAnchors = [];
for (const p of passes) {
  vAnchors.push([p[0].t0 - 0.35, 0]);            // blank on the pickup
  for (const e of p) vAnchors.push([e.t0, e.v0]);
  vAnchors.push([p.at(-1).t1, p.at(-1).v1]);
}
vAnchors.push([TOTAL - 0.9, vAnchors.at(-1)[1]]); // hold, then un-draw
vAnchors.push([TOTAL - 0.05, 0]);
function drawTimeAt(now) {
  if (now <= vAnchors[0][0]) return 0;
  for (let i = 1; i < vAnchors.length; i++) {
    const [t1, v1] = vAnchors[i];
    if (now <= t1) {
      const [t0, v0] = vAnchors[i - 1];
      return v0 + (v1 - v0) * (now - t0) / Math.max(1e-6, t1 - t0);
    }
  }
  return 0;
}

let vPrev = 0, penX = W / 2, penY = WG_Y + WG_H / 2, penA = 0;

function pathPartial(ctx, P, budget) {
  // stroke the first `budget` px of polyline P; return the tip
  ctx.beginPath();
  ctx.moveTo(P[0][0], P[0][1]);
  let used = 0;
  for (let i = 1; i < P.length; i++) {
    const seg = Math.hypot(P[i][0] - P[i - 1][0], P[i][1] - P[i - 1][1]);
    if (used + seg >= budget) {
      const f = (budget - used) / Math.max(1e-6, seg);
      const tx = P[i - 1][0] + (P[i][0] - P[i - 1][0]) * f;
      const ty = P[i - 1][1] + (P[i][1] - P[i - 1][1]) * f;
      ctx.lineTo(tx, ty);
      ctx.stroke();
      return [tx, ty];
    }
    ctx.lineTo(P[i][0], P[i][1]);
    used += seg;
  }
  ctx.stroke();
  return P.at(-1);
}

function drawPen(ctx, now, moving, tip) {
  penA += ((moving && tip ? 1 : 0) - penA) * 0.18;
  if (tip) {
    penX += (tip[0] - penX) * 0.42;
    penY += (tip[1] - penY) * 0.42;
  }
  if (penA < 0.03) return;
  const bob = 1.6 * Math.sin(now * 9.1);
  ctx.save();
  ctx.globalAlpha = penA;
  ctx.translate(penX, penY + bob);
  ctx.rotate(-0.62 + 0.04 * Math.sin(now * 5.3));
  ctx.shadowColor = "rgba(32,28,40,0.35)";
  ctx.shadowBlur = 6;
  ctx.fillStyle = "#c9cdd4";                       // steel nib
  ctx.beginPath();
  ctx.moveTo(0, 0);
  ctx.lineTo(13, -34);
  ctx.lineTo(27, -30);
  ctx.closePath();
  ctx.fill();
  ctx.strokeStyle = "rgba(60,58,66,0.85)";        // nib slit
  ctx.lineWidth = 1.4;
  ctx.beginPath();
  ctx.moveTo(0, 0);
  ctx.lineTo(17, -30);
  ctx.stroke();
  ctx.fillStyle = "#a9705a";                       // holder
  ctx.beginPath();
  ctx.roundRect(11, -96, 18, 66, 8);
  ctx.fill();
  ctx.fillStyle = "rgba(255,255,255,0.35)";        // glint
  ctx.beginPath();
  ctx.roundRect(14, -92, 4, 52, 2);
  ctx.fill();
  ctx.restore();
}

function drawScore(ctx, now) {
  const v = drawTimeAt(now);
  const moving = Math.abs(v - vPrev) > 1e-4;
  vPrev = v;
  ctx.save();
  ctx.lineWidth = 4.6;
  ctx.lineCap = ctx.lineJoin = "round";
  ctx.strokeStyle = "rgba(34,28,40,0.92)";
  ctx.fillStyle = "rgba(34,28,40,0.92)";
  ctx.shadowColor = "rgba(247,242,235,0.9)";     // cream halo
  ctx.shadowBlur = 8;
  let tip = null;
  let lastEnd = null, nextStart = null, prevS = null;
  for (const s of STROKES) {
    const p = Math.max(0, Math.min(1, (v - s.v0) / (s.v1 - s.v0)));
    if (p <= 0) { if (!nextStart) nextStart = { s }; continue; }
    let budget = p * s.total;
    for (let i = 0; i < s.subs.length && budget > 0; i++) {
      const P = s.subs[i], L = s.lens[i];
      if (P.length < 2) {
        ctx.beginPath();
        ctx.arc(P[0][0], P[0][1], 2.6, 0, Math.PI * 2);
        ctx.fill();
        budget -= L;
        continue;
      }
      if (budget >= L) {
        pathPartial(ctx, P, Infinity);
        budget -= L;
        if (p < 1) tip = P.at(-1);
      } else {
        tip = pathPartial(ctx, P, budget);
        budget = 0;
      }
    }
    if (p >= 1) prevS = s;
  }
  ctx.restore();
  // pen travel between strokes: glide from the last mark's end to the
  // coming mark's start while nothing is actively inking
  if (!tip && prevS && v < STROKES.at(-1).v1) {
    const nxt = STROKES.find((s) => v < s.v0);
    if (nxt) {
      const a = prevS.subs.at(-1).at(-1);
      const b = nxt.subs[0][0];
      const g = Math.max(0, Math.min(1,
        (v - prevS.v1) / Math.max(1e-6, nxt.v0 - prevS.v1)));
      tip = [a[0] + (b[0] - a[0]) * g, a[1] + (b[1] - a[1]) * g];
    }
  }
  drawPen(ctx, now, moving, tip);
}

// ── track info + timecode glyphs ───────────────────────────────────────
const titleImg = await magickRenderText("lonerclub v4pid", {
  ptSize: 56, fill: "#f7f2eb", stroke: "#201c22", strokeWidth: 2,
  shadow: "#201c22", shadowSpec: "100x0+4+5",
  outPath: `${assetsDir}/title.png`,
});
const artistImg = await magickRenderText(
  "whistlegraph dot org feat. aesthetic dot computer", {
    ptSize: 26, fill: "#f7f2eb", stroke: "#201c22", strokeWidth: 1,
    shadow: "#201c22", shadowSpec: "100x0+3+4",
    outPath: `${assetsDir}/artist.png`,
  });
const fmt = (t) =>
  `${Math.floor(t / 60)}:${String(Math.floor(t % 60)).padStart(2, "0")}`;
const tcImgs = {};
for (let s = 0; s <= Math.ceil(TOTAL); s++) {
  const label = fmt(s);
  if (tcImgs[label]) continue;
  tcImgs[label] = await magickRenderText(label, {
    ptSize: 34, fill: "#f7f2eb", stroke: "#201c22", strokeWidth: 1,
    shadow: "#201c22", shadowSpec: "100x0+3+3",
    outPath: `${assetsDir}/tc-${label.replace(":", "-")}.png`,
  });
}

// ── radial ring: the waveform wrapped into SPOKES time-bins ────────────
const SPOKES = 180;
const spokeEnv = [];
for (let k = 0; k < SPOKES; k++) {
  const a = Math.floor((k / SPOKES) * env.length);
  const b = Math.max(a + 1, Math.floor(((k + 1) / SPOKES) * env.length));
  let m = 0;
  for (let i = a; i < b; i++) m = Math.max(m, env[i]);
  spokeEnv.push(m);
}
const CX = W / 2, CY = H * 0.795, R = 118;
const TAU = Math.PI * 2;

function drawRing(ctx, now) {
  const pulse = 1 + 0.05 * envAt(now);
  ctx.save();
  ctx.translate(CX, CY);
  ctx.fillStyle = `${INK}0.16)`;
  ctx.beginPath();
  ctx.arc(0, 0, (R + 34) * pulse, 0, TAU);
  ctx.fill();
  for (let k = 0; k < SPOKES; k++) {
    const tk = (k / SPOKES) * TOTAL;
    // the ring turns; the spoke holding "now" always meets the needle
    const ang = -Math.PI / 2 + ((tk - now) / TOTAL) * TAU;
    const played = tk <= now;
    const len = (5 + spokeEnv[k] * 30) * pulse;
    const r0 = R - len / 2, r1 = R + len / 2;
    ctx.strokeStyle = played ? `${ROSE}0.92)` : `${CREAM}0.55)`;
    ctx.lineWidth = played ? 3.4 : 2.4;
    ctx.beginPath();
    ctx.moveTo(Math.cos(ang) * r0, Math.sin(ang) * r0);
    ctx.lineTo(Math.cos(ang) * r1, Math.sin(ang) * r1);
    ctx.stroke();
  }
  // fixed needle at twelve o'clock
  ctx.fillStyle = `${CREAM}0.95)`;
  ctx.beginPath();
  ctx.moveTo(0, -R - 26);
  ctx.lineTo(-7, -R - 40);
  ctx.lineTo(7, -R - 40);
  ctx.closePath();
  ctx.fill();
  ctx.restore();
  // elapsed time in the ring's center
  const tc = tcImgs[fmt(now)];
  if (tc) ctx.drawImage(tc, CX - tc.width / 2, CY - tc.height / 2);
  // title + artist beneath the ring
  const tw = titleImg.width, th = titleImg.height;
  const bounce = 2.5 * Math.sin(now * 2.2) * (0.4 + envAt(now));
  ctx.drawImage(titleImg, CX - tw / 2, CY + R + 52 + bounce, tw, th);
  const aw = artistImg.width;
  ctx.drawImage(artistImg, CX - aw / 2, CY + R + 52 + th + 10 + bounce);
}

function drawLyric(ctx, now) {
  for (const w of words) {
    if (now < w.t0 || now > w.t1 + 0.18) continue;
    const img = wordImgs[w.text];
    const inK = Math.min(1, (now - w.t0) / 0.12);       // pop in
    const outK = Math.min(1, Math.max(0, (w.t1 + 0.18 - now) / 0.18));
    const scale = (0.86 + 0.14 * inK) * Math.min(1, W * 0.82 / img.width);
    const alpha = inK * outK;
    const lift = 3.5 * Math.sin(now * 3.0) * (0.3 + envAt(now));
    const dw = img.width * scale, dh = img.height * scale;
    ctx.save();
    ctx.globalAlpha = alpha;
    ctx.translate(CX, H * 0.145 + lift);
    ctx.rotate(0.015 * Math.sin(w.t0 * 7));  // deterministic tilt per word
    ctx.drawImage(img, -dw / 2, -dh / 2, dw, dh);
    ctx.restore();
  }
}

// ── frame pump: base → canvas → chrome → encoder ───────────────────────
console.log(`▸ loner reel chrome · ${FRAMES} frames · lyrics + radial track`);
progress.begin({ type: "video", label: `loner reel chrome · ${FRAMES} frames` });
const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const dec = spawn("ffmpeg", ["-loglevel", "error", "-i", BASE,
  "-f", "rawvideo", "-pix_fmt", "rgba", "-"],
  { stdio: ["ignore", "pipe", "inherit"] });
const enc = spawnFFmpegEncode({ audioPath: AUDIO, w: W, h: H, fps: FPS,
  outPath: OUT, crf: 18 });
const FRAME_BYTES = W * H * 4;
const fbuf = Buffer.alloc(FRAME_BYTES);
const img = ctx.createImageData(W, H);
let off = 0, fi = 0;
for await (const chunk of dec.stdout) {
  let cOff = 0;
  while (cOff < chunk.length) {
    const n = Math.min(FRAME_BYTES - off, chunk.length - cOff);
    chunk.copy(fbuf, off, cOff, cOff + n);
    off += n; cOff += n;
    if (off === FRAME_BYTES) {
      off = 0;
      if (fi >= FRAMES) break;
      img.data.set(fbuf);
      ctx.putImageData(img, 0, 0);
      const now = fi / FPS;
      drawScore(ctx, now);
      drawRing(ctx, now);
      drawLyric(ctx, now);
      if (!enc.stdin.write(canvas.toBuffer("raw"))) {
        await once(enc.stdin, "drain");
      }
      fi++;
      progress.update((fi / FRAMES) * 100, { done: fi, total: FRAMES });
    }
  }
}
enc.stdin.end();
await new Promise((res, rej) => {
  enc.on("close", (c) => (c === 0 ? res() : rej(new Error(`ffmpeg ${c}`))));
});
progress.end();
console.log(`✓ ${OUT} (${fi} frames chromed)`);
