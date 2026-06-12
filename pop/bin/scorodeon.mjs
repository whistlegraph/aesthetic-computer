#!/usr/bin/env node
// scorodeon — a little theatre where the score plays (score + ὠδεῖον).
//
// v2: a SCROLLING DYNAMIC CANVAS, built on the same machinery as the pop
// preview-score videos (node-canvas frames → spawnFFmpegEncode): the
// track's data flies by a FIXED CENTER LINE — every lane's events scroll
// right→left, light up as they cross the line, and a minimap of the whole
// composition rides the bottom so you never lose the shape of the piece.
//
// Any aspect ratio: lanes squash to fill whatever frame you ask for.
// Default is vertical 1080x1920 — insta-story ready. `--zoom` sets how
// many seconds of music are visible across the width.
//
// Input is a generic score JSON any lane can emit:
//   { title, artist?, dur,
//     movements?: [{name, sub?, t0, t1, level?}],
//     chords?:    [{t, dur, name}],
//     arc?:       [v0..1 per second],
//     goldenSec?: <sec>,
//     lanes:      [{name, color, events: [{t, dur, pitch?, g?}]}],
//     ribbons?:   [{name, color, env: [v0..1 per second]}] }
//
// Usage:
//   node pop/bin/scorodeon.mjs <score.json> <audio> [options]
//     --size WxH      default 1080x1920
//     --zoom <sec>    seconds visible across the width (default 12)
//     --fps <n>       default 30
//     --out <mp4>     default <data dir>/<name>-scorodeon.mp4
//     --desktop       also copy to ~/Desktop
//
// momboba:
//   node pop/momboba/bin/scorodeon-data.mjs
//   node pop/bin/scorodeon.mjs pop/momboba/out/mombobasleep.scorodeon.json \
//     pop/momboba/out/mombobasleep.mp3 --desktop

import { readFileSync, existsSync, copyFileSync, mkdtempSync, rmSync } from "node:fs";
import { resolve, dirname, basename, join } from "node:path";
import { tmpdir, homedir } from "node:os";
import { once } from "node:events";
import { createCanvas, registerFont } from "canvas";
import {
  decodeAudioMono, computeRmsEnvelope, magickRenderText, spawnFFmpegEncode,
  drawEventWaveform,
} from "../lib/preview-shared.mjs";
import * as progress from "../lib/render-progress.mjs";

const args = process.argv.slice(2);
const pos = args.filter((a) => !a.startsWith("--") && !/^\d+$/.test(a));
const opt = (k, d) => { const i = args.indexOf(`--${k}`); return i >= 0 && args[i + 1] ? args[i + 1] : d; };
const has = (k) => args.includes(`--${k}`);
if (pos.length < 2) {
  console.error("usage: scorodeon.mjs <score.json> <audio> [--size WxH] [--zoom sec] …");
  process.exit(1);
}
const DATA = resolve(pos[0]);
const AUDIO = resolve(pos[1]);
for (const p of [DATA, AUDIO]) if (!existsSync(p)) { console.error(`✗ missing ${p}`); process.exit(1); }

const S = JSON.parse(readFileSync(DATA, "utf8"));
const [W, H] = opt("size", "1080x1920").split("x").map(Number);
const ZOOM = parseFloat(opt("zoom", "12"));      // seconds across the width
const FPS = parseInt(opt("fps", "30"), 10);
const SS = parseInt(opt("ss", "2"), 10);         // pixel density: layout is W×H
                                                 // logical, frames render W*SS
                                                 // × H*SS device px (retina-sharp)
const OUT = resolve(opt("out", join(dirname(DATA), basename(DATA).replace(/\.scorodeon\.json$|\.json$/i, "") + "-scorodeon.mp4")));
const DUR = S.dur;
const PXS = W / ZOOM;                            // px per second
const CX = W / 2;                                // the fixed center line
const B = Math.min(W, H);                        // type/chrome base — keeps the
                                                 // design readable in any aspect

// ── palette (the graphic-score paper) ─────────────────────────────────
const CREAM = "#FBFAFF", INK = "#1E1E1E", FAINT = "rgba(30,30,30,0.30)";
const LINE_RED = "#B3402E";

// mono labels — try Menlo, fall back silently to node-canvas default.
try { registerFont("/System/Library/Fonts/Menlo.ttc", { family: "MenloX" }); } catch { /* default */ }
const MONO = (px) => `${px}px MenloX, monospace`;

// ── audio envelope + waveform source ──────────────────────────────────
// the rendered master also flows THROUGH the events: each pill wide
// enough carries the actual waveform of its own time window.
const { audio, sr, audioPeak } = decodeAudioMono(AUDIO);
const env = computeRmsEnvelope(audio, sr, FPS, DUR);

// ── lane geometry: squash all lanes into whatever height we have ──────
const HEAD = Math.round(B * 0.150);              // title + movement + chords
const FOOT = Math.round(B * 0.105);              // minimap + clock
const CHORD_H = Math.round(B * 0.036);
const lanesY = HEAD + CHORD_H + 8, lanesH = H - lanesY - FOOT - 8;
const ribbons = S.ribbons ?? [];
const RIB_H = ribbons.length ? Math.round(lanesH * 0.10) : 0;
// the dynamic arc is a META-TRACK in the same scrolling system as every
// other lane — NOT an absolute-scale overlay (that read as misaligned
// against the fixed center line). One coordinate system: xOf(t, now).
const ARC_H = S.arc ? Math.round(lanesH * 0.085) : 0;
const laneH = (lanesH - RIB_H * ribbons.length - ARC_H) / S.lanes.length;
for (const l of S.lanes) {                       // pitch range per lane
  const ps = l.events.map((e) => e.pitch).filter((p) => p != null);
  l.pLo = ps.length ? Math.min(...ps) : 0;
  l.pHi = ps.length ? Math.max(...ps) : 1;
  l.events.sort((a, b) => a.t - b.t);
}

// ── prerendered YWFT title (magick — cairo can't see the TTF) ─────────
const TMP = mkdtempSync(join(tmpdir(), "scorodeon-"));
// rasterize at device resolution (ptSize×SS), draw at logical size — the
// title never upscales, so it stays crisp at any density.
const title = await magickRenderText(S.title, {
  ptSize: Math.round(B * 0.050) * SS, fill: INK, outPath: join(TMP, "title.png"),
});
const titleW = title.width / SS, titleH = title.height / SS;

const xOf = (t, now) => CX + (t - now) * PXS;
const r2 = (x) => Math.round(x * 2) / 2;
const at = (arr, t) => arr?.[Math.max(0, Math.min(arr.length - 1, Math.round(t)))] ?? 0;
const fmt = (t) => `${Math.floor(t / 60)}:${String(Math.floor(t % 60)).padStart(2, "0")}`;

const canvas = createCanvas(W * SS, H * SS);
const ctx = canvas.getContext("2d");
ctx.scale(SS, SS);                               // all drawing stays in logical px

function rounded(x, y, w, h, r) {
  const rr = Math.min(r, w / 2, h / 2);
  ctx.beginPath();
  ctx.moveTo(x + rr, y);
  ctx.arcTo(x + w, y, x + w, y + h, rr);
  ctx.arcTo(x + w, y + h, x, y + h, rr);
  ctx.arcTo(x, y + h, x, y, rr);
  ctx.arcTo(x, y, x + w, y, rr);
  ctx.closePath();
}

function drawFrame(now, fi) {
  ctx.fillStyle = CREAM;
  ctx.fillRect(0, 0, W, H);

  // header — title, artist, current movement
  ctx.drawImage(title, (W - titleW) / 2, B * 0.014, titleW, titleH);
  ctx.fillStyle = FAINT;
  ctx.font = MONO(Math.round(B * 0.022));
  ctx.textAlign = "center";
  if (S.artist) ctx.fillText(S.artist, CX, B * 0.014 + titleH + B * 0.028);
  const mv = S.movements?.find((m) => now >= m.t0 && now < m.t1) ?? S.movements?.at(-1);
  if (mv) {
    ctx.fillStyle = INK;
    ctx.font = MONO(Math.round(B * 0.034));
    ctx.fillText(`${mv.name}${mv.sub ? " · " + mv.sub : ""}`, CX, HEAD - B * 0.014);
  }

  // chord rail
  if (S.chords) {
    ctx.font = MONO(Math.round(CHORD_H * 0.62));
    for (const c of S.chords) {
      const x0 = xOf(c.t, now), x1 = xOf(c.t + c.dur, now);
      if (x1 < -40 || x0 > W + 40) continue;
      const active = now >= c.t && now < c.t + c.dur;
      ctx.fillStyle = active ? "rgba(62,68,112,0.42)" : "rgba(62,68,112,0.14)";
      ctx.fillRect(r2(x0), HEAD, r2(x1 - x0 - 1.5), CHORD_H);
      if (x1 - x0 > 26) {
        ctx.fillStyle = active ? CREAM : "rgba(30,30,30,0.55)";
        ctx.fillText(c.name, (x0 + x1) / 2, HEAD + CHORD_H * 0.70);
      }
    }
  }

  // movement boundaries through the lane field
  if (S.movements) {
    ctx.font = MONO(Math.round(B * 0.022));
    ctx.textAlign = "left";
    for (const m of S.movements) {
      const x = xOf(m.t0, now);
      if (x < -200 || x > W + 10) continue;
      ctx.strokeStyle = "rgba(62,68,112,0.25)";
      ctx.setLineDash([6, 7]);
      ctx.beginPath(); ctx.moveTo(r2(x), lanesY); ctx.lineTo(r2(x), lanesY + lanesH); ctx.stroke();
      ctx.setLineDash([]);
      ctx.fillStyle = "rgba(62,68,112,0.55)";
      ctx.fillText(m.name, x + 7, lanesY + 16);
    }
    if (S.goldenSec) {
      const gx = xOf(S.goldenSec, now);
      if (gx > -10 && gx < W + 10) {
        ctx.strokeStyle = "rgba(179,64,46,0.5)";
        ctx.setLineDash([8, 6]);
        ctx.beginPath(); ctx.moveTo(r2(gx), lanesY); ctx.lineTo(r2(gx), lanesY + lanesH); ctx.stroke();
        ctx.setLineDash([]);
      }
    }
  }

  // lanes — the data flying by
  ctx.textAlign = "left";
  S.lanes.forEach((l, li) => {
    const y0 = lanesY + li * laneH;
    // color-coded band — every lane sits on a wash of its own color
    ctx.fillStyle = l.color;
    ctx.globalAlpha = 0.065 + (li % 2) * 0.035;
    ctx.fillRect(0, y0, W, laneH);
    ctx.globalAlpha = 1;
    const pSpan = Math.max(1, l.pHi - l.pLo);
    const usable = laneH - 10;
    for (const e of l.events) {
      const x0 = xOf(e.t, now);
      if (x0 > W + 8) break;                    // sorted — rest are future
      const wpx = Math.max(3, e.dur * PXS - 2);
      if (x0 + wpx < -8) continue;
      const py = e.pitch == null ? 0.5 : 1 - (e.pitch - l.pLo) / pSpan;
      const eh = Math.max(12, Math.min(usable * 0.60, 10 + (e.g ?? 0.2) * 130));
      const active = now >= e.t && now < e.t + e.dur;
      const pop = active ? 1 + 0.35 * Math.exp(-(now - e.t) / 0.13) : 1;
      const y = y0 + 5 + py * (usable - eh * pop);
      ctx.globalAlpha = active ? 1 : x0 + wpx < CX ? 0.52 : 0.30;
      ctx.fillStyle = l.color;
      if (active) {                              // cheap glow — no shadowBlur
        ctx.globalAlpha = 0.30;
        rounded(x0 - 3, y - 3, wpx + 6, eh * pop + 6, 6);
        ctx.fill();
        ctx.globalAlpha = 1;
      }
      rounded(x0, y, wpx, eh * pop, 5);
      ctx.fill();
      // the rendered master flowing through the pill
      if (wpx >= 20 && eh * pop >= 12)
        drawEventWaveform(ctx, audio, sr, audioPeak,
          Math.max(0, x0) + 2, y + 2, Math.min(x0 + wpx, W) - Math.max(0, x0) - 4,
          eh * pop - 4,
          e.t + (Math.max(0, x0) - x0) / PXS, e.t + (Math.min(x0 + wpx, W) - x0) / PXS,
          CREAM, active ? 0.75 : 0.35);
      ctx.globalAlpha = 1;
    }
    // chip label — lane color block, cream text, always on top
    ctx.font = MONO(Math.round(B * 0.022));
    const lw = ctx.measureText(l.name).width;
    ctx.fillStyle = l.color;
    rounded(10, y0 + 7, lw + 20, B * 0.034, 7);
    ctx.fill();
    ctx.fillStyle = CREAM;
    ctx.fillText(l.name, 20, y0 + 7 + B * 0.024);
  });

  // ribbons (continuous beds) — thickness from their envelope
  ribbons.forEach((rb, ri) => {
    const yMid = lanesY + S.lanes.length * laneH + RIB_H * (ri + 0.5);
    ctx.fillStyle = rb.color;
    ctx.globalAlpha = 0.55;
    ctx.beginPath();
    const step = 8;
    for (let x = 0; x <= W; x += step) {
      const v = at(rb.env, now + (x - CX) / PXS);
      const hh = Math.max(1.5, v * RIB_H * 0.42);
      x === 0 ? ctx.moveTo(x, yMid - hh) : ctx.lineTo(x, yMid - hh);
    }
    for (let x = W; x >= 0; x -= step) {
      const v = at(rb.env, now + (x - CX) / PXS);
      ctx.lineTo(x, yMid + Math.max(1.5, v * RIB_H * 0.42));
    }
    ctx.closePath(); ctx.fill();
    ctx.globalAlpha = 1;
    ctx.font = MONO(Math.round(B * 0.022));
    const lw = ctx.measureText(rb.name).width;
    ctx.fillStyle = rb.color;
    rounded(10, yMid - RIB_H * 0.5 + 5, lw + 20, B * 0.034, 7);
    ctx.fill();
    ctx.fillStyle = CREAM;
    ctx.fillText(rb.name, 20, yMid - RIB_H * 0.5 + 5 + B * 0.024);
  });

  // dynamic-arc meta-track — same scroll space, so the curve's value AT
  // the center line is exactly the current dynamic level
  if (S.arc) {
    const ay = lanesY + S.lanes.length * laneH + RIB_H * ribbons.length;
    ctx.fillStyle = "rgba(62,68,112,0.16)";
    ctx.beginPath();
    ctx.moveTo(0, ay + ARC_H);
    for (let x = 0; x <= W; x += 4)
      ctx.lineTo(x, ay + ARC_H - at(S.arc, now + (x - CX) / PXS) * (ARC_H - 6));
    ctx.lineTo(W, ay + ARC_H);
    ctx.closePath(); ctx.fill();
    ctx.strokeStyle = "#3E4470"; ctx.lineWidth = 2; ctx.beginPath();
    for (let x = 0; x <= W; x += 4) {
      const y = ay + ARC_H - at(S.arc, now + (x - CX) / PXS) * (ARC_H - 6);
      x === 0 ? ctx.moveTo(x, y) : ctx.lineTo(x, y);
    }
    ctx.stroke(); ctx.lineWidth = 1;
    ctx.font = MONO(Math.round(B * 0.022));
    const lw = ctx.measureText("dynamic arc").width;
    ctx.fillStyle = "#3E4470";
    rounded(10, ay + 5, lw + 20, B * 0.034, 7);
    ctx.fill();
    ctx.fillStyle = CREAM;
    ctx.fillText("dynamic arc", 20, ay + 5 + B * 0.024);
  }

  // the fixed center line — breathes with the track's RMS
  const e0 = env[Math.min(env.length - 1, fi)] ?? 0;
  ctx.fillStyle = `rgba(179,64,46,${(0.10 + e0 * 0.20).toFixed(3)})`;
  ctx.fillRect(CX - 7 - e0 * 8, HEAD, 14 + e0 * 16, H - HEAD - FOOT);
  ctx.fillStyle = LINE_RED;
  ctx.fillRect(CX - 1.5, HEAD - 8, 3, H - HEAD - FOOT + 16);

  // footer — whole-piece minimap + clock
  const mmY = H - FOOT + 8, mmH = FOOT - B * 0.044;
  if (S.movements) for (const m of S.movements) {
    ctx.fillStyle = `rgba(62,68,112,${(0.14 + (m.level ?? 0.5) * 0.45).toFixed(2)})`;
    ctx.fillRect(r2((m.t0 / DUR) * W), mmY, r2(((m.t1 - m.t0) / DUR) * W) - 1, mmH);
  }
  // the visible zoom-window, so the fixed center line and this moving
  // cursor read as the same thing at two scales (the cursor IS the
  // center line, seen on the whole-piece progress bar)
  const vw = (ZOOM / DUR) * W;
  ctx.fillStyle = "rgba(179,64,46,0.14)";
  ctx.fillRect(r2((now / DUR) * W - vw / 2), mmY - 3, r2(vw), mmH + 6);
  ctx.fillStyle = LINE_RED;
  ctx.fillRect(r2((now / DUR) * W) - 1.5, mmY - 5, 3, mmH + 10);
  ctx.fillStyle = INK;
  ctx.font = MONO(Math.round(B * 0.030));
  ctx.textAlign = "center";
  ctx.fillText(`${fmt(now)} / ${fmt(DUR)}`, CX, H - B * 0.011);
}

// ── --frame <sec>: dump one PNG for design iteration, skip the encode ──
if (opt("frame", null) != null) {
  const ft = parseFloat(opt("frame", "0"));
  drawFrame(ft, Math.floor(ft * FPS));
  const fp = join(dirname(OUT), "scorodeon-frame.png");
  const { writeFileSync } = await import("node:fs");
  writeFileSync(fp, canvas.toBuffer("image/png"));
  rmSync(TMP, { recursive: true, force: true });
  console.log(`✓ frame @${ft}s → ${fp}`);
  process.exit(0);
}

// ── encode ─────────────────────────────────────────────────────────────
const total = Math.ceil(DUR * FPS);
console.log(`▸ scorodeon · ${S.title} · ${W * SS}x${H * SS}@${FPS} (${SS}x density) · zoom ${ZOOM}s/screen · ${total} frames`);
// heartbeat → Slab menubar bar + the launching session's `rendering` state
progress.begin({ type: "video", label: `${S.title} scorodeon · ${total} frames` });
const enc = spawnFFmpegEncode({ audioPath: AUDIO, w: W * SS, h: H * SS, fps: FPS, outPath: OUT, crf: 18 });
const t0 = Date.now();
for (let fi = 0; fi < total; fi++) {
  drawFrame(fi / FPS, fi);
  if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
  progress.update((fi / total) * 100, { done: fi, total });
  if (fi % Math.floor(total / 10) === 0)
    console.log(`  ${Math.round((fi / total) * 100)}% · ${((Date.now() - t0) / 1000).toFixed(0)}s`);
}
enc.stdin.end();
await new Promise((res, rej) => { enc.on("close", (c) => (c === 0 ? res() : rej(new Error(`ffmpeg exit ${c}`)))); });
progress.end();
rmSync(TMP, { recursive: true, force: true });
console.log(`✓ ${OUT} (${((Date.now() - t0) / 1000).toFixed(0)}s)`);
if (has("desktop")) {
  const d = join(homedir(), "Desktop", basename(OUT));
  copyFileSync(OUT, d);
  console.log(`✓ → ${d}`);
}
