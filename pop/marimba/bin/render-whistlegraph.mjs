#!/usr/bin/env node
// render-whistlegraph.mjs — offline render of the marimbagraph piece.
// Imports SCORE from disks/marimbagraph.mjs (the same geometry, timing,
// and gesture envelopes the live piece draws from) and produces:
//   ~/Desktop/marimbaba-whistlegraph.png   (the finished score, 1600²)
//   ~/Desktop/marimbaba-whistlegraph.mp4   (the page building itself,
//                                           1080² @ 30fps, sine lullaby,
//                                           with a score-track lane)
// Gesture rules mirror the piece: pen speed = the note's envelope
// integral, pressure = the envelope value, active gestures draw in wet
// ink and bake into their section color after the note ends.
// Zero-dep: raw RGBA frames piped to ffmpeg, PNG via node:zlib, WAV by hand.

import { SCORE } from "../../../system/public/aesthetic.computer/disks/marimbagraph.mjs";
import { spawn } from "node:child_process";
import { once } from "node:events";
import { writeFileSync, unlinkSync } from "node:fs";
import { tmpdir, homedir } from "node:os";
import { join } from "node:path";
import zlib from "node:zlib";

const OUT_PNG = join(homedir(), "Desktop", "marimbaba-whistlegraph.png");
const OUT_MP4 = join(homedir(), "Desktop", "marimbaba-whistlegraph.mp4");

const { beat: BEAT, totalBeats, paper, events, hz, wet: WET, bakeSec: BAKE, progressAt, pressureAt, liftAt } = SCORE;
const TOTAL = totalBeats * BEAT;
const LEAD = 1.5; // blank page before the first note
const TAIL = 3.0; // rest on the finished page
const FPS = 30;
const VSIZE = 1080;
const PSIZE = 1600;
const GRAPHITE = [120, 116, 104];

// Video layout: drawing page on top, score-track lane along the bottom.
const PAGE = { scale: 8.6, ox: 110, oy: 12 }; // 100-unit page → 860px
const LANE = { x: 40, w: 1000, top: 905, bot: 1050, bg: [243, 239, 227] };

// ── pixel canvas ──────────────────────────────────────────────────────
function makePage(size) {
  const buf = Buffer.alloc(size * size * 4);
  for (let i = 0; i < size * size; i += 1) {
    buf[i * 4] = paper[0];
    buf[i * 4 + 1] = paper[1];
    buf[i * 4 + 2] = paper[2];
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

function mix(c, k, base = paper) {
  return [0, 1, 2].map((i) => Math.round(base[i] + (c[i] - base[i]) * k));
}

// Point at arc-length `len` along an event's polyline (unit space).
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

// Stamp a gesture from arc-length l0..l1: radius follows the pressure
// envelope (fat at the attack, thin through the decay).
function stampGesture(buf, size, geom, e, l0, l1, color) {
  const rOf = (p) => (0.16 + 0.42 * p) * geom.scale;
  const step = 0.9 / geom.scale; // ~1px in unit space
  for (let l = Math.max(0, l0); l < l1; l += step) {
    const p = pointAt(e, l);
    stamp(buf, size, geom.ox + p[0] * geom.scale, geom.oy + p[1] * geom.scale, rOf(pressureAt(e, l)), color);
  }
  const p = pointAt(e, l1);
  stamp(buf, size, geom.ox + p[0] * geom.scale, geom.oy + p[1] * geom.scale, rOf(pressureAt(e, l1)), color);
}

// ── the score-track lane ──────────────────────────────────────────────
// The whole melodic line is visible ahead of the pen: a smooth contour
// curve through the notes, note bars in section colors that fill in as
// the playhead passes, one tick per 3/4 bar.
function midi(f) {
  return 69 + 12 * Math.log2(f / 440);
}
const laneX = (t) => LANE.x + (Math.min(Math.max(t, 0), TOTAL) / TOTAL) * LANE.w;
const laneY = (m) => LANE.bot - ((m - 52) / (87 - 52)) * (LANE.bot - LANE.top);

const laneBars = [];
const contourPts = [];
for (const e of events) {
  if (!e.tones.length) continue;
  const t0 = e.start * BEAT;
  const t1 = (e.start + e.dur) * BEAT;
  const top = Math.max(...e.tones.map((t) => hz(t))); // melody top line
  for (const tone of e.tones) {
    laneBars.push({ x0: laneX(t0), x1: laneX(t1) - 3, y: laneY(midi(hz(tone))), color: e.color, t0, t1, e });
  }
  contourPts.push([laneX((t0 + t1) / 2), laneY(midi(top))]);
}

// Catmull-Rom through the note midpoints — the melodic curve itself.
const contour = [];
for (let i = 0; i < contourPts.length - 1; i += 1) {
  const p0 = contourPts[Math.max(0, i - 1)];
  const p1 = contourPts[i];
  const p2 = contourPts[i + 1];
  const p3 = contourPts[Math.min(contourPts.length - 1, i + 2)];
  for (let s = 0; s < 12; s += 1) {
    const u = s / 12;
    const u2 = u * u;
    const u3 = u2 * u;
    contour.push([
      0.5 * (2 * p1[0] + (-p0[0] + p2[0]) * u + (2 * p0[0] - 5 * p1[0] + 4 * p2[0] - p3[0]) * u2 + (-p0[0] + 3 * p1[0] - 3 * p2[0] + p3[0]) * u3),
      0.5 * (2 * p1[1] + (-p0[1] + p2[1]) * u + (2 * p0[1] - 5 * p1[1] + 4 * p2[1] - p3[1]) * u2 + (-p0[1] + 3 * p1[1] - 3 * p2[1] + p3[1]) * u3),
    ]);
  }
}
contour.push(contourPts[contourPts.length - 1]);

function drawLane(frame, size, t) {
  fillRect(frame, size, 0, LANE.top - 22, size - 1, LANE.bot + 20, LANE.bg);
  for (let b = 0; b <= totalBeats; b += 3) { // one tick per bar
    const x = laneX(b * BEAT);
    fillRect(frame, size, x, LANE.bot + 6, x + 1, LANE.bot + 14, mix(GRAPHITE, 0.35, LANE.bg));
  }
  const faint = mix([150, 145, 130], 0.5, LANE.bg);
  for (let i = 1; i < contour.length; i += 1) { // the melodic curve, visible ahead
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
    // Wet while sounding, bakes to section color — same rule as the page.
    const bakeK = Math.min(1, Math.max(0, (t - bar.t1) / BAKE));
    const color = !played
      ? mix(bar.color, 0.28, LANE.bg)
      : active || bakeK < 1
        ? mix(bar.color, bakeK, WET)
        : bar.color;
    const r = active ? 6 : 4;
    for (let x = bar.x0; x <= bar.x1; x += 2) stamp(frame, size, x, bar.y, r, color);
    stamp(frame, size, bar.x1, bar.y, r, color);
  }
  const px = laneX(t);
  fillRect(frame, size, px, LANE.top - 16, px + 1.5, LANE.bot + 14, GRAPHITE);
}

// ── PNG (final score) ─────────────────────────────────────────────────
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
  ihdr[8] = 8; // bit depth
  ihdr[9] = 6; // RGBA
  const raw = Buffer.alloc((w * 4 + 1) * h);
  for (let y = 0; y < h; y += 1) {
    raw[y * (w * 4 + 1)] = 0; // filter: none
    rgba.copy(raw, y * (w * 4 + 1) + 1, y * w * 4, (y + 1) * w * 4);
  }
  return Buffer.concat([
    Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]),
    chunk("IHDR", ihdr),
    chunk("IDAT", zlib.deflateSync(raw, { level: 9 })),
    chunk("IEND", Buffer.alloc(0)),
  ]);
}

// ── audio: a whistled sine rendition of the score ─────────────────────
function renderAudio() {
  const sr = 44100;
  const seconds = LEAD + TOTAL + TAIL;
  const mono = new Float64Array(Math.ceil(seconds * sr));
  for (const e of events) {
    const dur = e.dur * BEAT;
    const t0 = LEAD + e.start * BEAT;
    for (const tone of e.tones) {
      const f = hz(tone);
      const amp = e.tones.length > 1 ? 0.15 : 0.2;
      const n0 = Math.floor(t0 * sr);
      const n1 = Math.min(mono.length, Math.floor((t0 + dur + 0.8) * sr));
      for (let n = n0; n < n1; n += 1) {
        const x = (n - n0) / sr;
        let env = Math.min(1, x / 0.012) * Math.exp((-2.0 * x) / (dur + 0.3));
        if (x > dur) env *= Math.exp(-7 * (x - dur));
        mono[n] +=
          amp * env * Math.sin(2 * Math.PI * f * x) +
          0.045 * Math.exp(-6 * x) * Math.sin(2 * Math.PI * f * 3.97 * x);
      }
    }
  }
  let peak = 0;
  for (const s of mono) peak = Math.max(peak, Math.abs(s));
  const gain = peak > 0 ? 0.82 / peak : 1;
  const pcm = Buffer.alloc(mono.length * 4); // 16-bit stereo
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
  header.writeUInt16LE(1, 20); // PCM
  header.writeUInt16LE(2, 22); // stereo
  header.writeUInt32LE(sr, 24);
  header.writeUInt32LE(sr * 4, 28);
  header.writeUInt16LE(4, 32);
  header.writeUInt16LE(16, 34);
  header.write("data", 36, "ascii");
  header.writeUInt32LE(pcm.length, 40);
  return Buffer.concat([header, pcm]);
}

// ── main ──────────────────────────────────────────────────────────────
console.log(`🎼 marimbagraph → whistlegraph render (${TOTAL.toFixed(1)}s of melody)`);

// 1. The finished score as a PNG (fully baked, pressure-tapered marks).
{
  const geom = { scale: PSIZE / SCORE.size, ox: 0, oy: 0 };
  const page = makePage(PSIZE);
  for (const e of events) {
    if (e.pts) stampGesture(page, PSIZE, geom, e, 0, e.total, e.color);
  }
  writeFileSync(OUT_PNG, pngEncode(page, PSIZE, PSIZE));
  console.log(`🖼️  final score → ${OUT_PNG}`);
}

// 2. The page building itself, with the sine lullaby and the track lane.
const wavPath = join(tmpdir(), "marimbagraph-audio.wav");
writeFileSync(wavPath, renderAudio());
console.log(`🔊 audio bed → ${wavPath}`);

const ffmpeg = spawn(
  "ffmpeg",
  [
    "-y",
    "-f", "rawvideo",
    "-pix_fmt", "rgba",
    "-s", `${VSIZE}x${VSIZE}`,
    "-r", String(FPS),
    "-i", "pipe:0",
    "-i", wavPath,
    "-c:v", "libx264",
    "-pix_fmt", "yuv420p",
    "-crf", "18",
    "-preset", "medium",
    "-c:a", "aac",
    "-b:a", "192k",
    "-shortest",
    "-movflags", "+faststart",
    OUT_MP4,
  ],
  { stdio: ["pipe", "inherit", "inherit"] },
);

// Substrate holds only fully-baked gestures; active/baking ones are
// drawn per-frame on the frame copy in wet-to-section blend.
const substrate = makePage(VSIZE);
const frames = Math.ceil((LEAD + TOTAL + TAIL) * FPS);

for (let fr = 0; fr < frames; fr += 1) {
  const t = fr / FPS - LEAD;
  for (const e of events) {
    if (!e.pts || e.baked) continue;
    if (t >= (e.start + e.dur) * BEAT + BAKE) {
      stampGesture(substrate, VSIZE, PAGE, e, 0, e.total, e.color);
      e.baked = true;
    }
  }
  const frame = Buffer.from(substrate);
  let pen = null;
  for (const e of events) {
    if (!e.pts || e.baked) continue;
    const x = t - e.start * BEAT;
    if (x <= 0) continue;
    const target = progressAt(e, x);
    if (target <= 0) continue;
    const bakeK = Math.min(1, Math.max(0, (x - e.dur * BEAT) / BAKE));
    stampGesture(frame, VSIZE, PAGE, e, 0, target, mix(e.color, bakeK, WET));
    if (target < e.total) {
      const p = pointAt(e, target);
      pen = [PAGE.ox + p[0] * PAGE.scale, PAGE.oy + p[1] * PAGE.scale];
    }
  }
  const lift = liftAt(t);
  if (lift) {
    // Airborne pen: lighter, swelling mid-arc as it nears the viewer.
    stamp(
      frame, VSIZE,
      PAGE.ox + lift.x * PAGE.scale, PAGE.oy + lift.y * PAGE.scale,
      6 + 5 * Math.sin(Math.PI * lift.u), mix(GRAPHITE, 0.55),
    );
  } else if (pen) {
    stamp(frame, VSIZE, pen[0], pen[1], 7, GRAPHITE);
  }
  drawLane(frame, VSIZE, t);
  if (!ffmpeg.stdin.write(frame)) await once(ffmpeg.stdin, "drain");
  if (fr % 300 === 0) console.log(`🎥 frame ${fr}/${frames} (t=${t.toFixed(1)}s)`);
}
ffmpeg.stdin.end();
const [code] = await once(ffmpeg, "close");
unlinkSync(wavPath);
if (code !== 0) {
  console.error(`❌ ffmpeg exited ${code}`);
  process.exit(code);
}
console.log(`🎬 build video → ${OUT_MP4}`);
