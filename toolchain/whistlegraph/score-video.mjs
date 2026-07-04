#!/usr/bin/env node
// score-video.mjs — codify an old whistlegraph into the marimbagraph
// quantization scheme and render it as a score video.
//
//   node score-video.mjs <analysis.json | tiktok-id> [--out name]
//
// Reads a grab.mjs analysis (tempo, key, whistled-melody note sequence),
// cleans the pyin jitter down to the melodic skeleton, then draws one
// gesture per note under the compositional rules:
//   • directionality IS the note: each pitch maps to a heading (high →
//     upward, low → downward); a pitch change turns the pen ≥ ~30°, and
//     turns ≥ ~30° happen ONLY at note changes — the mnemonic rule.
//   • repeated pitches keep their heading; held notes curl smoothly
//     (< 30° per step, like a pom-pom loop).
//   • pen speed follows the note's attack/decay envelope; pressure is
//     the envelope value (fat attack → thin decay).
//   • the active gesture is wet dark ink, baking into its phrase color
//     after the note ends; breath gaps are animated pen lifts onto the
//     next phrase row.
// Output: ~/Desktop/<name>-whistlegraph.mp4 (+ .png of the final page),
// with a score-track lane (contour + note bars + playhead) and a sine
// rendition of the quantized whistle. Zero-dep (ffmpeg + node:zlib).

import { spawn } from "node:child_process";
import { once } from "node:events";
import { readFileSync, writeFileSync, unlinkSync, existsSync } from "node:fs";
import { tmpdir, homedir } from "node:os";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import zlib from "node:zlib";

// ── input ─────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const arg = argv.find((a) => !a.startsWith("--"));
if (!arg) {
  console.error("usage: score-video.mjs <analysis.json | tiktok-id> [--out name]");
  process.exit(1);
}
const HERE = dirname(fileURLToPath(import.meta.url));
const analysisPath = existsSync(arg)
  ? arg
  : join(HERE, "downloads", `whistlegraph-${arg}.analysis.json`);
const analysis = JSON.parse(readFileSync(analysisPath, "utf8"));
const outFlag = argv.indexOf("--out");
const NAME = outFlag >= 0 ? argv[outFlag + 1] : (analysis.title || "clip").replace(/[^a-z0-9]+/gi, "-").toLowerCase();
const OUT_MP4 = join(homedir(), "Desktop", `${NAME}-whistlegraph.mp4`);
const OUT_PNG = join(homedir(), "Desktop", `${NAME}-whistlegraph.png`);

// ── 1. melodic skeleton: merge fragments, absorb glide blips ──────────
const raw = analysis.melody.map((n) => ({ midi: n.midi, note: n.note, t0: n.startSec, t1: n.startSec + n.durSec }));
raw.sort((a, b) => a.t0 - b.t0);

const merged = [];
for (const n of raw) {
  const prev = merged[merged.length - 1];
  if (prev && n.midi === prev.midi && n.t0 - prev.t1 < 0.12) prev.t1 = Math.max(prev.t1, n.t1);
  else merged.push({ ...n });
}
// Absorb sub-70ms blips that sit within a whole tone of a neighbor —
// pyin quantizing a portamento whistle, not real articulation.
const skeleton = [];
for (const n of merged) {
  const dur = n.t1 - n.t0;
  const prev = skeleton[skeleton.length - 1];
  if (dur < 0.07 && prev && Math.abs(n.midi - prev.midi) <= 2 && n.t0 - prev.t1 < 0.1) {
    prev.t1 = Math.max(prev.t1, n.t1);
    continue;
  }
  if (dur < 0.05) continue; // stray dust
  skeleton.push({ ...n });
}

// ── 2. phrases: breath gaps are pen lifts ─────────────────────────────
const GAP = 0.32;
const phrases = [[]];
for (const n of skeleton) {
  const cur = phrases[phrases.length - 1];
  if (cur.length && n.t0 - cur[cur.length - 1].t1 > GAP) phrases.push([n]);
  else cur.push(n);
}
console.log(`🎶 ${analysis.title || NAME}: ${raw.length} pyin notes → ${skeleton.length} skeleton notes in ${phrases.length} phrases`);

// ── 3. quantize into gestures: direction IS the note ──────────────────
const PAPER = [250, 247, 238];
const WET = [30, 26, 38];
const GRAPHITE = [120, 116, 104];
const PALETTE = [
  [34, 48, 124], // marker blue
  [206, 52, 74], // crimson
  [52, 116, 122], // teal
  [122, 70, 152], // violet
  [188, 108, 32], // ochre
];
const BAKE = 0.9;
const DRAW_K = 0.85;
const ENV_N = 36;

const midis = skeleton.map((n) => n.midi);
const loMidi = Math.min(...midis);
const hiMidi = Math.max(...midis);
const midMidi = (loMidi + hiMidi) / 2;
const spanMidi = Math.max(4, hiMidi - loMidi);

// Heading from pitch: high notes point up, low notes point down.
function pitchAngle(midi) {
  return (-((midi - midMidi) / spanMidi) * 150 * Math.PI) / 180; // ±75°
}

// Gesture length from duration via the envelope integral feel.
const lenOf = (dur) => 3 + 11 * Math.pow(Math.min(dur, 1.6), 0.7);

const MARGIN = 8;
const rowY = (i) => (phrases.length === 1 ? 50 : 14 + (72 * i) / (phrases.length - 1));

const events = []; // {tones, midi, t0, t1, pts, color}
for (const [pi, phrase] of phrases.entries()) {
  const color = PALETTE[pi % PALETTE.length];
  let x = MARGIN + 4;
  let y = rowY(pi);
  let drift = 1; // +x, mirrors at margins (only at note boundaries)
  let prevMidi = null;
  for (const n of phrase) {
    const dur = n.t1 - n.t0;
    const L = lenOf(dur);
    let theta = pitchAngle(n.midi);
    // The 30° rule, generatively: a pitch change must land a legible
    // turn. If the pitch-mapped headings sit closer than 30°, push apart.
    if (prevMidi !== null && n.midi !== prevMidi) {
      const prevTheta = pitchAngle(prevMidi);
      if (Math.abs(theta - prevTheta) < Math.PI / 6) {
        theta = prevTheta + (Math.sign(n.midi - prevMidi) || 1) * -(Math.PI / 6);
      }
    }
    if (x + Math.cos(theta) * L * drift > 100 - MARGIN || x + Math.cos(theta) * L * drift < MARGIN) drift = -drift;
    // Held notes curl: constant gentle curvature, < 30° per step.
    const held = dur > 0.45;
    const steps = Math.max(2, Math.round(L / 0.8));
    const curl = held ? (((Math.min(dur, 2) - 0.45) * 1.6 * Math.PI) / steps) * (pi % 2 ? -1 : 1) : 0;
    const pts = [[x, y]];
    let h = drift > 0 ? theta : Math.PI - theta;
    for (let s = 0; s < steps; s += 1) {
      x += (Math.cos(h) * L) / steps;
      y += (Math.sin(h) * L) / steps;
      y = Math.max(6, Math.min(94, y));
      pts.push([x, y]);
      h += curl;
    }
    events.push({ tones: [n.note], midi: n.midi, t0: n.t0, t1: n.t1, pts, color });
    prevMidi = n.midi;
  }
}

// arc lengths + envelope gestures (speed = envelope integral, pressure = envelope)
function envAt(e, x) {
  const dur = e.t1 - e.t0;
  const a = Math.min(0.05, dur * 0.25);
  return x < a ? x / a : Math.exp((-1.4 / dur) * (x - a));
}
for (const e of events) {
  e.lens = [];
  e.total = 0;
  for (let i = 1; i < e.pts.length; i += 1) {
    const l = Math.hypot(e.pts[i][0] - e.pts[i - 1][0], e.pts[i][1] - e.pts[i - 1][1]);
    e.lens.push(l);
    e.total += l;
  }
  const drawDur = (e.t1 - e.t0) * DRAW_K;
  const cum = [0];
  for (let i = 1; i <= ENV_N; i += 1) {
    const x0 = ((i - 1) / ENV_N) * drawDur;
    const x1 = (i / ENV_N) * drawDur;
    cum.push(cum[i - 1] + ((envAt(e, x0) + envAt(e, x1)) / 2) * (x1 - x0));
  }
  e.gest = [];
  for (let i = 0; i <= ENV_N; i += 1) {
    e.gest.push({ l: (cum[i] / cum[ENV_N]) * e.total, r: envAt(e, (i / ENV_N) * drawDur) });
  }
}

function progressAt(e, x) {
  const drawDur = (e.t1 - e.t0) * DRAW_K;
  if (x <= 0) return 0;
  if (x >= drawDur) return e.total;
  const i = Math.min(ENV_N - 1, Math.floor((x / drawDur) * ENV_N));
  const k = (x - (i / ENV_N) * drawDur) / (drawDur / ENV_N);
  return e.gest[i].l + (e.gest[i + 1].l - e.gest[i].l) * k;
}
function pressureAt(e, len) {
  const g = e.gest;
  for (let i = 1; i < g.length; i += 1) {
    if (g[i].l >= len) {
      const span = g[i].l - g[i - 1].l;
      const k = span > 0 ? (len - g[i - 1].l) / span : 1;
      return g[i - 1].r + (g[i].r - g[i - 1].r) * k;
    }
  }
  return g[g.length - 1].r;
}

// pen lifts between disconnected marks (phrase gaps, entry, exit)
function liftPath(t0, t1, a, b) {
  const up = Math.min(7, Math.hypot(b[0] - a[0], b[1] - a[1]) * 0.3 + 2);
  return { t0, t1, a, b, c: [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2 - up] };
}
const LIFTS = [];
{
  let prev = null;
  for (const e of events) {
    if (!prev) LIFTS.push(liftPath(e.t0 - 0.6, e.t0, [50, -6], e.pts[0]));
    else if (Math.hypot(e.pts[0][0] - prev.pt[0], e.pts[0][1] - prev.pt[1]) > 0.6) {
      LIFTS.push(liftPath(prev.t, e.t0, prev.pt, e.pts[0]));
    }
    prev = { pt: e.pts[e.pts.length - 1], t: e.t0 + (e.t1 - e.t0) * DRAW_K };
  }
  LIFTS.push(liftPath(prev.t, prev.t + 1.2, prev.pt, [108, 30]));
}
function liftAt(t) {
  for (const L of LIFTS) {
    if (t >= L.t0 && t <= L.t1) {
      const u = (t - L.t0) / (L.t1 - L.t0);
      const v = 1 - u;
      return {
        x: v * v * L.a[0] + 2 * v * u * L.c[0] + u * u * L.b[0],
        y: v * v * L.a[1] + 2 * v * u * L.c[1] + u * u * L.b[1],
        u,
      };
    }
  }
  return null;
}

// ── render engine (canvas, brush, lane, png, wav) ─────────────────────
const TOTAL = skeleton[skeleton.length - 1].t1;
const LEAD = 1.2;
const TAIL = 2.5;
const FPS = 30;
const VSIZE = 1080;
const PSIZE = 1600;
const PAGE = { scale: 8.6, ox: 110, oy: 12 };
const LANE = { x: 40, w: 1000, top: 905, bot: 1050, bg: [243, 239, 227] };

function makePage(size) {
  const buf = Buffer.alloc(size * size * 4);
  for (let i = 0; i < size * size; i += 1) {
    buf[i * 4] = PAPER[0];
    buf[i * 4 + 1] = PAPER[1];
    buf[i * 4 + 2] = PAPER[2];
    buf[i * 4 + 3] = 255;
  }
  return buf;
}
const discCache = new Map();
function disc(r) {
  const key = Math.round(r * 4);
  if (!discCache.has(key)) {
    const rr = key / 4;
    const offs = [];
    const ri = Math.ceil(rr);
    for (let dy = -ri; dy <= ri; dy += 1) {
      for (let dx = -ri; dx <= ri; dx += 1) {
        if (dx * dx + dy * dy <= rr * rr) offs.push([dx, dy]);
      }
    }
    discCache.set(key, offs);
  }
  return discCache.get(key);
}
function stamp(buf, size, x, y, r, color) {
  const xi = Math.round(x);
  const yi = Math.round(y);
  for (const [dx, dy] of disc(r)) {
    const px = xi + dx;
    const py = yi + dy;
    if (px < 0 || py < 0 || px >= size || py >= size) continue;
    const i = (py * size + px) * 4;
    buf[i] = color[0];
    buf[i + 1] = color[1];
    buf[i + 2] = color[2];
  }
}
function fillRect(buf, size, x0, y0, x1, y1, color) {
  for (let y = Math.max(0, Math.round(y0)); y <= Math.min(size - 1, Math.round(y1)); y += 1) {
    for (let x = Math.max(0, Math.round(x0)); x <= Math.min(size - 1, Math.round(x1)); x += 1) {
      const i = (y * size + x) * 4;
      buf[i] = color[0];
      buf[i + 1] = color[1];
      buf[i + 2] = color[2];
    }
  }
}
function mix(c, k, base = PAPER) {
  return [0, 1, 2].map((i) => Math.round(base[i] + (c[i] - base[i]) * k));
}
function pointAt(e, len) {
  let run = 0;
  for (let i = 1; i < e.pts.length; i += 1) {
    const l = e.lens[i - 1];
    if (run + l >= len || i === e.pts.length - 1) {
      const k = l > 0 ? Math.min(1, (len - run) / l) : 1;
      const a = e.pts[i - 1];
      const b = e.pts[i];
      return [a[0] + (b[0] - a[0]) * k, a[1] + (b[1] - a[1]) * k];
    }
    run += l;
  }
  return e.pts[e.pts.length - 1];
}
function stampGesture(buf, size, geom, e, l0, l1, color) {
  const rOf = (p) => (0.16 + 0.42 * p) * geom.scale;
  const step = 0.9 / geom.scale;
  for (let l = Math.max(0, l0); l < l1; l += step) {
    const p = pointAt(e, l);
    stamp(buf, size, geom.ox + p[0] * geom.scale, geom.oy + p[1] * geom.scale, rOf(pressureAt(e, l)), color);
  }
  const p = pointAt(e, l1);
  stamp(buf, size, geom.ox + p[0] * geom.scale, geom.oy + p[1] * geom.scale, rOf(pressureAt(e, l1)), color);
}

// track lane
const laneX = (t) => LANE.x + (Math.min(Math.max(t, 0), TOTAL) / TOTAL) * LANE.w;
const laneY = (m) => LANE.bot - ((m - (loMidi - 1)) / (hiMidi - loMidi + 2)) * (LANE.bot - LANE.top);
const laneBars = events.map((e) => ({ x0: laneX(e.t0), x1: Math.max(laneX(e.t0) + 2, laneX(e.t1) - 2), y: laneY(e.midi), color: e.color, t0: e.t0, t1: e.t1 }));
const contourPts = events.map((e) => [laneX((e.t0 + e.t1) / 2), laneY(e.midi)]);
const contour = [];
for (let i = 0; i < contourPts.length - 1; i += 1) {
  const p0 = contourPts[Math.max(0, i - 1)];
  const p1 = contourPts[i];
  const p2 = contourPts[i + 1];
  const p3 = contourPts[Math.min(contourPts.length - 1, i + 2)];
  for (let s = 0; s < 8; s += 1) {
    const u = s / 8;
    const u2 = u * u;
    const u3 = u2 * u;
    contour.push([
      0.5 * (2 * p1[0] + (-p0[0] + p2[0]) * u + (2 * p0[0] - 5 * p1[0] + 4 * p2[0] - p3[0]) * u2 + (-p0[0] + 3 * p1[0] - 3 * p2[0] + p3[0]) * u3),
      0.5 * (2 * p1[1] + (-p0[1] + p2[1]) * u + (2 * p0[1] - 5 * p1[1] + 4 * p2[1] - p3[1]) * u2 + (-p0[1] + 3 * p1[1] - 3 * p2[1] + p3[1]) * u3),
    ]);
  }
}
contour.push(contourPts[contourPts.length - 1]);

const beatSec = 60 / (analysis.tempoBPM || 100);
function drawLane(frame, size, t) {
  fillRect(frame, size, 0, LANE.top - 22, size - 1, LANE.bot + 20, LANE.bg);
  for (let b = 0; b * beatSec <= TOTAL; b += 1) {
    const x = laneX(b * beatSec);
    fillRect(frame, size, x, LANE.bot + 6, x + 1, LANE.bot + 14, mix(GRAPHITE, 0.35, LANE.bg));
  }
  const faint = mix([150, 145, 130], 0.5, LANE.bg);
  for (let i = 1; i < contour.length; i += 1) {
    const a = contour[i - 1];
    const b = contour[i];
    const steps = Math.max(1, Math.ceil(Math.hypot(b[0] - a[0], b[1] - a[1])));
    for (let s = 0; s <= steps; s += 1) {
      stamp(frame, size, a[0] + ((b[0] - a[0]) * s) / steps, a[1] + ((b[1] - a[1]) * s) / steps, 1.4, faint);
    }
  }
  for (const bar of laneBars) {
    const active = t >= bar.t0 && t <= bar.t1;
    const played = t >= bar.t0;
    const bakeK = Math.min(1, Math.max(0, (t - bar.t1) / BAKE));
    const color = !played ? mix(bar.color, 0.28, LANE.bg) : active || bakeK < 1 ? mix(bar.color, bakeK, WET) : bar.color;
    const r = active ? 6 : 4;
    for (let x = bar.x0; x <= bar.x1; x += 2) stamp(frame, size, x, bar.y, r, color);
    stamp(frame, size, bar.x1, bar.y, r, color);
  }
  const px = laneX(t);
  fillRect(frame, size, px, LANE.top - 16, px + 1.5, LANE.bot + 14, GRAPHITE);
}

// png
const CRC_TABLE = new Int32Array(256).map((_, n) => {
  let c = n;
  for (let k = 0; k < 8; k += 1) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
  return c;
});
function crc32(bytes) {
  let c = -1;
  for (const b of bytes) c = CRC_TABLE[(c ^ b) & 0xff] ^ (c >>> 8);
  return (c ^ -1) >>> 0;
}
function chunk(type, data) {
  const out = Buffer.alloc(8 + data.length + 4);
  out.writeUInt32BE(data.length, 0);
  out.write(type, 4, "ascii");
  data.copy(out, 8);
  out.writeUInt32BE(crc32(out.subarray(4, 8 + data.length)), 8 + data.length);
  return out;
}
function pngEncode(rgba, w, h) {
  const ihdr = Buffer.alloc(13);
  ihdr.writeUInt32BE(w, 0);
  ihdr.writeUInt32BE(h, 4);
  ihdr[8] = 8;
  ihdr[9] = 6;
  const rawRows = Buffer.alloc((w * 4 + 1) * h);
  for (let y = 0; y < h; y += 1) {
    rawRows[y * (w * 4 + 1)] = 0;
    rgba.copy(rawRows, y * (w * 4 + 1) + 1, y * w * 4, (y + 1) * w * 4);
  }
  return Buffer.concat([
    Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]),
    chunk("IHDR", ihdr),
    chunk("IDAT", zlib.deflateSync(rawRows, { level: 9 })),
    chunk("IEND", Buffer.alloc(0)),
  ]);
}

// audio: sine rendition of the quantized whistle
function renderAudio() {
  const sr = 44100;
  const mono = new Float64Array(Math.ceil((LEAD + TOTAL + TAIL) * sr));
  for (const e of events) {
    const dur = e.t1 - e.t0;
    const f = 440 * 2 ** ((e.midi - 69) / 12);
    const n0 = Math.floor((LEAD + e.t0) * sr);
    const n1 = Math.min(mono.length, Math.floor((LEAD + e.t1 + 0.25) * sr));
    for (let n = n0; n < n1; n += 1) {
      const x = (n - n0) / sr;
      let env = Math.min(1, x / 0.015) * Math.exp((-1.2 * x) / (dur + 0.15));
      if (x > dur) env *= Math.exp(-14 * (x - dur));
      mono[n] += 0.24 * env * Math.sin(2 * Math.PI * f * x);
    }
  }
  let peak = 0;
  for (const s of mono) peak = Math.max(peak, Math.abs(s));
  const gain = peak > 0 ? 0.82 / peak : 1;
  const pcm = Buffer.alloc(mono.length * 4);
  for (let n = 0; n < mono.length; n += 1) {
    const v = Math.round(mono[n] * gain * 32767);
    pcm.writeInt16LE(v, n * 4);
    pcm.writeInt16LE(v, n * 4 + 2);
  }
  const header = Buffer.alloc(44);
  header.write("RIFF", 0, "ascii");
  header.writeUInt32LE(36 + pcm.length, 4);
  header.write("WAVEfmt ", 8, "ascii");
  header.writeUInt32LE(16, 16);
  header.writeUInt16LE(1, 20);
  header.writeUInt16LE(2, 22);
  header.writeUInt32LE(sr, 24);
  header.writeUInt32LE(sr * 4, 28);
  header.writeUInt16LE(4, 32);
  header.writeUInt16LE(16, 34);
  header.write("data", 36, "ascii");
  header.writeUInt32LE(pcm.length, 40);
  return Buffer.concat([header, pcm]);
}

// ── main ──────────────────────────────────────────────────────────────
{
  const geom = { scale: PSIZE / 100, ox: 0, oy: 0 };
  const page = makePage(PSIZE);
  for (const e of events) stampGesture(page, PSIZE, geom, e, 0, e.total, e.color);
  writeFileSync(OUT_PNG, pngEncode(page, PSIZE, PSIZE));
  console.log(`🖼️  final score → ${OUT_PNG}`);
}

const wavPath = join(tmpdir(), `${NAME}-score-audio.wav`);
writeFileSync(wavPath, renderAudio());

const ffmpeg = spawn(
  "ffmpeg",
  ["-y", "-f", "rawvideo", "-pix_fmt", "rgba", "-s", `${VSIZE}x${VSIZE}`, "-r", String(FPS), "-i", "pipe:0", "-i", wavPath, "-c:v", "libx264", "-pix_fmt", "yuv420p", "-crf", "18", "-preset", "medium", "-c:a", "aac", "-b:a", "192k", "-shortest", "-movflags", "+faststart", OUT_MP4],
  { stdio: ["pipe", "inherit", "inherit"] },
);

const substrate = makePage(VSIZE);
const frames = Math.ceil((LEAD + TOTAL + TAIL) * FPS);
for (let fr = 0; fr < frames; fr += 1) {
  const t = fr / FPS - LEAD;
  for (const e of events) {
    if (e.baked) continue;
    if (t >= e.t1 + BAKE) {
      stampGesture(substrate, VSIZE, PAGE, e, 0, e.total, e.color);
      e.baked = true;
    }
  }
  const frame = Buffer.from(substrate);
  let pen = null;
  for (const e of events) {
    if (e.baked) continue;
    const x = t - e.t0;
    if (x <= 0) continue;
    const target = progressAt(e, x);
    if (target <= 0) continue;
    const bakeK = Math.min(1, Math.max(0, (x - (e.t1 - e.t0)) / BAKE));
    stampGesture(frame, VSIZE, PAGE, e, 0, target, mix(e.color, bakeK, WET));
    if (target < e.total) {
      const p = pointAt(e, target);
      pen = [PAGE.ox + p[0] * PAGE.scale, PAGE.oy + p[1] * PAGE.scale];
    }
  }
  const lift = liftAt(t);
  if (lift) {
    stamp(frame, VSIZE, PAGE.ox + lift.x * PAGE.scale, PAGE.oy + lift.y * PAGE.scale, 6 + 5 * Math.sin(Math.PI * lift.u), mix(GRAPHITE, 0.55));
  } else if (pen) {
    stamp(frame, VSIZE, pen[0], pen[1], 7, GRAPHITE);
  }
  drawLane(frame, VSIZE, t);
  if (!ffmpeg.stdin.write(frame)) await once(ffmpeg.stdin, "drain");
}
ffmpeg.stdin.end();
const [code] = await once(ffmpeg, "close");
unlinkSync(wavPath);
if (code !== 0) {
  console.error(`❌ ffmpeg exited ${code}`);
  process.exit(code);
}
console.log(`🎬 build video → ${OUT_MP4}`);
