// menuband/bin/reel-lib.mjs — shared helpers for the Menu Band promo-reel
// sims (sim-announce / sim-features / sim-chords), factored out of sim.mjs so
// the launch reel's look carries across the whole campaign: the pale-lilac
// light-mode desktop, the note-particle system, the framed real-capture
// windows, and — new here — the STRIP RIG.
//
// The strip rig re-lights the REAL captured menu-bar piano WITHOUT running the
// MenuBand binary: sim.mjs already cached one capture per lit note
// (out/menubar-frames/mb-<midi>.png, notes 67..83) plus the idle strip. Each
// single-note capture differs from idle only in that key's pixels, so diffing
// finds every key's rectangle + its ROYGBIV color, and ANY chord is composited
// by blitting those real lit-key regions onto the real idle strip. Same
// pixels the app draws — no binary spawn, no guessed geometry.

import { readdirSync, readFileSync, existsSync, mkdirSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { once } from "node:events";
import { createCanvas, registerFont, loadImage } from "canvas";
import { spawnFFmpegEncode } from "../../lib/preview-shared.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
export const LANE = resolve(HERE, "..");
export const OUT = `${LANE}/out`;
export const W = 1080, H = 1920, FPS = 30;

// macOS system fonts — registered before any canvas is created.
try { registerFont("/System/Library/Fonts/SFNS.ttf", { family: "MBSans" }); } catch {}
try { registerFont("/System/Library/Fonts/SFNSRounded.ttf", { family: "MBSansRounded" }); } catch {}

// The exact Menu Band icon keyboard palette (AboutWindow.swift).
export const KEY_COLORS = [
  [255, 77, 107], [255, 153, 46], [255, 214, 56], [51, 209, 179], [97, 158, 255],
];
export const INK = "rgba(20,18,28,0.92)";
export const INK_RGB = [20, 18, 28];

export const rgb = (a, al = 1) => `rgba(${a[0]},${a[1]},${a[2]},${al})`;
export const easeOut = (u) => 1 - Math.pow(1 - Math.max(0, Math.min(1, u)), 3);
export const clamp01 = (u) => Math.max(0, Math.min(1, u));

export function makeStage() {
  const canvas = createCanvas(W, H);
  return { canvas, ctx: canvas.getContext("2d") };
}

export function roundRect(ctx, x, y, w, h, r) {
  const rr = Math.min(r, w / 2, h / 2);
  ctx.beginPath();
  ctx.moveTo(x + rr, y);
  ctx.arcTo(x + w, y, x + w, y + h, rr);
  ctx.arcTo(x + w, y + h, x, y + h, rr);
  ctx.arcTo(x, y + h, x, y, rr);
  ctx.arcTo(x, y, x + w, y, rr);
  ctx.closePath();
}

export function text(ctx, str, x, y, size, color, weight = 700, align = "center", rounded = true) {
  ctx.fillStyle = color; ctx.textAlign = align; ctx.textBaseline = "middle";
  ctx.font = `${weight} ${size}px ${rounded ? "MBSansRounded" : "MBSans"}`;
  ctx.fillText(str, x, y); ctx.textAlign = "left";
}

// The launch reel's pale-lilac light-mode desktop, verbatim.
export function drawDesktop(ctx) {
  const g = ctx.createLinearGradient(0, 0, W, H);
  g.addColorStop(0, "rgb(238,232,246)");
  g.addColorStop(0.5, "rgb(216,203,234)");
  g.addColorStop(1, "rgb(190,170,214)");
  ctx.fillStyle = g; ctx.fillRect(0, 0, W, H);
  const s = ctx.createLinearGradient(0, H, W, 0);
  s.addColorStop(0, "rgba(255,255,255,0)");
  s.addColorStop(0.6, "rgba(255,255,255,0.28)");
  s.addColorStop(1, "rgba(255,255,255,0)");
  ctx.fillStyle = s; ctx.fillRect(0, 0, W, H);
}

export function vignette(ctx) {
  const vg = ctx.createRadialGradient(W / 2, H / 2, H * 0.3, W / 2, H / 2, H * 0.75);
  vg.addColorStop(0, "rgba(0,0,0,0)"); vg.addColorStop(1, "rgba(0,0,0,0.28)");
  ctx.fillStyle = vg; ctx.fillRect(0, 0, W, H);
}

// ── the Menu Band app icon, drawn from scratch (AboutWindow.iconImage),
// lifted verbatim from sim.mjs — purple squircle + cream 5-key piano;
// litKeys (Set of 0..4) glow in the icon's own vivid palette. ──────────────
const SQ_TL = [211, 80, 196], SQ_BR = [67, 29, 113];
const CREAM = [246, 242, 232], KEYBLACK = [27, 26, 25], EDGE = [40, 28, 22];
export function drawIcon(ctx, x, y, px, litKeys) {
  const R = (x0, y0, x1, y1) => [x + x0 * px, y + y0 * px, (x1 - x0) * px, (y1 - y0) * px];
  const margin = px * 0.098;
  const sq = [x + margin, y + margin, px - 2 * margin, px - 2 * margin];
  const radius = sq[2] * 0.2235;
  ctx.save();
  roundRect(ctx, sq[0], sq[1], sq[2], sq[3], radius);
  ctx.clip();
  const g = ctx.createLinearGradient(sq[0], sq[1], sq[0] + sq[2], sq[1] + sq[3]);
  g.addColorStop(0, rgb(SQ_TL)); g.addColorStop(1, rgb(SQ_BR));
  ctx.fillStyle = g; ctx.fillRect(sq[0], sq[1], sq[2], sq[3]);
  ctx.restore();
  const plate = R(0.254, 0.428, 0.744, 0.570);
  roundRect(ctx, plate[0], plate[1], plate[2], plate[3], px * 0.012);
  ctx.fillStyle = rgb(CREAM); ctx.fill();
  const whites = [[0, 0.254, 0.417], [2, 0.417, 0.580], [4, 0.580, 0.744]];
  for (const [idx, x0, x1] of whites) {
    if (!litKeys.has(idx)) continue;
    ctx.fillStyle = rgb(KEY_COLORS[idx]);
    roundRect(ctx, x + x0 * px + 2, y + 0.428 * px + 2, (x1 - x0) * px - 4, 0.142 * px - 4, px * 0.01);
    ctx.fill();
  }
  ctx.strokeStyle = rgb(EDGE); ctx.lineWidth = px * 0.006;
  for (const xf of [0.417, 0.580]) {
    ctx.beginPath();
    ctx.moveTo(x + xf * px, y + 0.432 * px);
    ctx.lineTo(x + xf * px, y + 0.566 * px);
    ctx.stroke();
  }
  roundRect(ctx, plate[0], plate[1], plate[2], plate[3], px * 0.012);
  ctx.lineWidth = px * 0.008; ctx.strokeStyle = rgb(EDGE); ctx.stroke();
  const blacks = [[1, 0.369, 0.463], [3, 0.535, 0.629]];
  for (const [idx, x0, x1] of blacks) {
    ctx.fillStyle = litKeys.has(idx) ? rgb(KEY_COLORS[idx]) : rgb(KEYBLACK);
    roundRect(ctx, x + x0 * px, y + 0.428 * px, (x1 - x0) * px, 0.098 * px, px * 0.01);
    ctx.fill();
  }
}

// ── note-particle system (vector eighth-notes), lifted from sim.mjs ────────
export function makeParticles(ctx) {
  const particles = [];
  function drawEighthNote(ox, oy, fill) {
    ctx.save(); ctx.translate(ox, oy); ctx.fillStyle = fill;
    ctx.save(); ctx.translate(0, 14); ctx.rotate(-0.3); ctx.beginPath();
    ctx.ellipse(0, 0, 11, 8, 0, 0, Math.PI * 2); ctx.fill(); ctx.restore();
    ctx.fillRect(9, -22, 4.5, 36);
    ctx.beginPath(); ctx.moveTo(13, -22); ctx.quadraticCurveTo(30, -14, 20, 2);
    ctx.quadraticCurveTo(26, -10, 13, -10); ctx.closePath(); ctx.fill();
    ctx.restore();
  }
  return {
    spawnNote(cx, cy, color, down = false) {
      particles.push({
        x: cx + (Math.sin(particles.length * 1.7) * 6), y: cy,
        vx: (Math.sin(particles.length * 2.3)) * 60,
        vy: down ? 40 + (particles.length % 5) * 12 : -260 - (particles.length % 5) * 18,
        life: 0, ttl: 2.2, color, rot: (particles.length % 7 - 3) * 0.12,
      });
    },
    stepAndDraw(dt) {
      for (const p of particles) {
        p.life += dt;
        p.x += p.vx * dt; p.y += p.vy * dt; p.vy += 320 * dt;
      }
      for (let i = particles.length - 1; i >= 0; i--) if (particles[i].life > particles[i].ttl) particles.splice(i, 1);
      for (const p of particles) {
        const a = Math.max(0, 1 - p.life / p.ttl);
        ctx.save();
        ctx.globalAlpha = a; ctx.translate(p.x, p.y); ctx.rotate(p.rot);
        drawEighthNote(2.5, 3, "rgba(0,0,0,0.5)");
        drawEighthNote(0, 0, rgb(p.color));
        ctx.restore();
      }
    },
  };
}

// ── the strip rig — real captured strip, re-lightable per note set ─────────
/// Midis the cache holds single-note captures for: the waltz's white keys
/// (G4..B5) + the low C4..F4 the scales ladder added (round 6 — captured via
/// `MenuBand --render-menubar --notes <midi> --light`, same args as sim.mjs).
/// Together that's every white key on the strip, C4..B5. Any melody is
/// folded onto these before lighting.
export const STRIP_MIDIS = [60, 62, 64, 65, 67, 69, 71, 72, 74, 76, 77, 79, 81, 83];
const PC_TO_STRIP = new Map([[0, 72], [2, 74], [4, 76], [5, 77], [7, 79], [9, 69], [11, 71]]);

/// Fold any (white-key) midi onto a strip key: same pitch class, the cached
/// octave. G/A/B keep their low octave when they arrive below middle C's row.
export function foldToStrip(midi) {
  if (STRIP_MIDIS.includes(midi)) return midi;
  const pc = ((midi % 12) + 12) % 12;
  const base = PC_TO_STRIP.get(pc);
  if (base == null) return null;                       // black key — not cached
  if (midi < base - 6 && STRIP_MIDIS.includes(base - 12)) return base - 12;
  return base;
}

export async function loadStripRig() {
  const dir = `${OUT}/menubar-frames`;
  const idlePath = `${dir}/mb-idle.png`;
  if (!existsSync(idlePath)) throw new Error(`missing ${idlePath} — run sim.mjs once with the MenuBand binary available`);
  const idle = await loadImage(idlePath);
  const w = idle.width, h = idle.height;

  const scratch = createCanvas(w, h);
  const sctx = scratch.getContext("2d");
  const pixels = (img) => {
    sctx.clearRect(0, 0, w, h);
    sctx.drawImage(img, 0, 0);
    return sctx.getImageData(0, 0, w, h);
  };
  const idlePx = pixels(idle);

  // Diff each single-note capture against idle → that key's rect + color.
  const keys = new Map();          // midi -> { x0, x1, y0, y1, cx, color, img }
  for (const midi of STRIP_MIDIS) {
    const p = `${dir}/mb-${midi}.png`;
    if (!existsSync(p)) continue;
    const img = await loadImage(p);
    const px = pixels(img);
    const colDiff = new Int32Array(w);
    let y0 = h, y1 = 0;
    for (let y = 0; y < h; y++) {
      for (let x = 0; x < w; x++) {
        const i = (y * w + x) * 4;
        const d = Math.abs(px.data[i] - idlePx.data[i]) +
                  Math.abs(px.data[i + 1] - idlePx.data[i + 1]) +
                  Math.abs(px.data[i + 2] - idlePx.data[i + 2]);
        if (d > 36) { colDiff[x]++; if (y < y0) y0 = y; if (y > y1) y1 = y; }
      }
    }
    // Take the widest contiguous run of differing columns — that's the key
    // (guards against stray anti-aliasing noise elsewhere on the strip).
    let best = null, runStart = -1;
    for (let x = 0; x <= w; x++) {
      const on = x < w && colDiff[x] > 2;
      if (on && runStart < 0) runStart = x;
      if (!on && runStart >= 0) {
        if (!best || x - runStart > best.x1 - best.x0) best = { x0: runStart, x1: x };
        runStart = -1;
      }
    }
    if (!best) continue;
    // Average lit color inside the run (for particles).
    let r = 0, g = 0, b = 0, n = 0;
    for (let y = Math.max(0, y0); y <= Math.min(h - 1, y1); y++) {
      for (let x = best.x0; x < best.x1; x++) {
        if (colDiff[x] <= 2) continue;
        const i = (y * w + x) * 4;
        r += px.data[i]; g += px.data[i + 1]; b += px.data[i + 2]; n++;
      }
    }
    keys.set(midi, {
      x0: best.x0, x1: best.x1, y0, y1,
      cx: (best.x0 + best.x1) / 2 / w,
      color: n ? [Math.round(r / n), Math.round(g / n), Math.round(b / n)] : INK_RGB,
      img,
    });
  }

  // Composite cache: any subset of lit keys → one canvas.
  const composites = new Map();    // "72,76,79" -> canvas
  function imageFor(midis) {
    const lit = [...new Set(midis.map(foldToStrip).filter((m) => m != null && keys.has(m)))].sort((a, b) => a - b);
    if (lit.length === 0) return idle;
    const key = lit.join(",");
    if (composites.has(key)) return composites.get(key);
    const c = createCanvas(w, h);
    const cx = c.getContext("2d");
    cx.drawImage(idle, 0, 0);
    for (const m of lit) {
      const k = keys.get(m);
      const pad = 2;                               // seam-safe blit
      const bx = Math.max(0, k.x0 - pad), bw = Math.min(w, k.x1 + pad) - bx;
      const by = Math.max(0, k.y0 - pad), bh = Math.min(h, k.y1 + pad) - by;
      cx.drawImage(k.img, bx, by, bw, bh, bx, by, bw, bh);
    }
    composites.set(key, c);
    return c;
  }

  return { idle, keys, imageFor, aspect: w / h };
}

/// Draw the strip in a rect; returns nothing — callers own placement.
export function drawStrip(ctx, rig, midis, x, y, w, alpha = 1) {
  const img = rig.imageFor(midis);
  const h = w / rig.aspect;
  ctx.save(); ctx.globalAlpha = alpha;
  ctx.drawImage(img, x, y, w, h);
  ctx.restore();
  return { x, y, w, h };
}

/// Where a folded note's key sits on a drawn strip rect (for particles).
export function stripKeyX(rig, midi, rect) {
  const k = rig.keys.get(foldToStrip(midi));
  return rect.x + rect.w * (k ? k.cx : 0.5);
}
export function stripKeyColor(rig, midi) {
  return rig.keys.get(foldToStrip(midi))?.color ?? INK_RGB;
}

// ── framed macOS window around a real capture (from sim.mjs drawAbout) ─────
export const TITLEBAR_H = 72;
export function drawFramedWindow(ctx, img, { alpha = 1, yOff = 0, contentH = null, cx = W / 2, cyFrac = 0.5, borderless = false } = {}) {
  if (!img) return null;
  const tb = borderless ? 0 : TITLEBAR_H;
  const ch = contentH ?? Math.min(H * 0.66, 1180);
  const aw = ch * img.width / img.height;
  const ah = ch + tb;
  const ax = cx - aw / 2, ay = H * cyFrac - ah / 2 + yOff;
  ctx.save(); ctx.globalAlpha = alpha;
  ctx.shadowColor = "rgba(0,0,0,0.45)"; ctx.shadowBlur = 60; ctx.shadowOffsetY = 24;
  roundRect(ctx, ax, ay, aw, ah, 26); ctx.fillStyle = "white"; ctx.fill();
  ctx.shadowColor = "transparent";
  ctx.save(); roundRect(ctx, ax, ay, aw, ah, 26); ctx.clip();
  if (!borderless) {
    ctx.fillStyle = "rgb(238,235,245)"; ctx.fillRect(ax, ay, aw, tb);
    const lights = ["rgb(255,95,86)", "rgb(255,189,46)", "rgb(39,201,63)"];
    for (let i = 0; i < 3; i++) { ctx.beginPath(); ctx.fillStyle = lights[i]; ctx.arc(ax + 44 + i * 46, ay + tb / 2, 15, 0, 7); ctx.fill(); }
  }
  ctx.drawImage(img, ax, ay + tb, aw, ch);
  ctx.restore();
  ctx.restore();
  return { ax, ay, aw, ah, tb };
}

// ── karaoke captions (the sung reels) ──────────────────────────────────────
// sing-jingle.mjs writes out/<slug>.words.sung.json — absolute word timings
// on the reel clock, grouped by lyric line. The active line sits centered
// near the bottom; sung words are ink, the active word rides an ink pill in
// white (reel.mjs --song's karaoke look, drawn with node-canvas — no libass
// in this ffmpeg build).
export function loadSungWords(slug) {
  const p = `${OUT}/${slug}.words.sung.json`;
  if (!existsSync(p)) { console.error(`✗ missing ${p} — run sing-jingle.mjs first`); process.exit(1); }
  return JSON.parse(readFileSync(p, "utf8"));
}

export function makeKaraoke(words, { y = H * 0.925, size = 54 } = {}) {
  const lines = [];
  for (const w of words) {
    (lines[w.line] ??= []).push(w);
  }
  const packed = lines.filter(Boolean).map((ws) => ({
    words: ws,
    from: ws[0].fromMs / 1000,
    to: ws[ws.length - 1].toMs / 1000,
  }));
  return {
    draw(ctx, t) {
      const li = packed.findIndex((L, i) => {
        const nextFrom = i + 1 < packed.length ? packed[i + 1].from : Infinity;
        return t >= L.from - 0.4 && t < Math.min(L.to + 0.6, nextFrom - 0.05);
      });
      if (li < 0) return;
      const L = packed[li];
      const a = easeOut(clamp01((t - (L.from - 0.4)) / 0.3)) *
        (1 - easeOut(clamp01((t - (L.to + 0.25)) / 0.35)));
      if (a <= 0) return;

      let sz = size;
      ctx.font = `700 ${sz}px MBSansRounded`;
      const gap = sz * 0.38;
      const widths = L.words.map((w) => ctx.measureText(w.text).width);
      let total = widths.reduce((s, w) => s + w, 0) + gap * (L.words.length - 1);
      if (total > W * 0.92) {
        sz = sz * (W * 0.92) / total;
        ctx.font = `700 ${sz}px MBSansRounded`;
        for (let i = 0; i < L.words.length; i++) widths[i] = ctx.measureText(L.words[i].text).width;
        total = widths.reduce((s, w) => s + w, 0) + (sz * 0.38) * (L.words.length - 1);
      }
      const g = sz * 0.38;
      let x = W / 2 - total / 2;
      ctx.save();
      ctx.globalAlpha = a;
      ctx.textAlign = "left";
      ctx.textBaseline = "middle";
      for (let i = 0; i < L.words.length; i++) {
        const w = L.words[i];
        const from = w.fromMs / 1000, to = w.toMs / 1000;
        const active = t >= from && t < to;
        const sung = t >= from;
        if (active) {
          const pop = 1 + 0.10 * (1 - easeOut(clamp01((t - from) / 0.18)));
          const pw = widths[i] * pop, ph = sz * 1.42;
          const cx = x + widths[i] / 2, cy = y;
          roundRect(ctx, cx - pw / 2 - sz * 0.22, cy - ph / 2, pw + sz * 0.44, ph, sz * 0.32);
          ctx.fillStyle = INK; ctx.fill();
          ctx.save();
          ctx.translate(cx, cy); ctx.scale(pop, pop); ctx.translate(-cx, -cy);
          ctx.fillStyle = "rgba(255,255,255,0.98)";
          ctx.fillText(w.text, x, y + 1);
          ctx.restore();
        } else {
          ctx.fillStyle = sung ? INK : "rgba(20,18,28,0.35)";
          ctx.fillText(w.text, x, y + 1);
        }
        x += widths[i] + g;
      }
      ctx.restore();
    },
  };
}

/// Shared --sung plumbing: audio/base/meta paths swing to the -sung variants.
export function sungMode() {
  const sung = process.argv.includes("--sung");
  return { sung, suffix: sung ? "-sung" : "" };
}

// ── notes.json helpers ─────────────────────────────────────────────────────
export function loadScore(name) {
  const p = `${OUT}/${name}.notes.json`;
  if (!existsSync(p)) { console.error(`✗ missing ${p} — run render-jingles.mjs first`); process.exit(1); }
  return JSON.parse(readFileSync(p, "utf8"));
}
export function leadOf(score) { return (score.notes || []).filter((n) => n.lane === "lead"); }
export function litAt(lead, t, minHold = 0.12) {
  const out = [];
  for (const n of lead) if (t >= n.t && t < n.t + Math.max(minHold, n.dur)) out.push(n.midi);
  return out;
}
export function makeOnsets(lead) {
  const sorted = [...lead].sort((a, b) => a.t - b.t);
  let cursor = 0;
  return (t0, t1) => {
    const hits = [];
    while (cursor < sorted.length && sorted[cursor].t <= t1) {
      if (sorted[cursor].t >= t0) hits.push(sorted[cursor]);
      cursor++;
    }
    return hits;
  };
}

// ── the frame pump: drawFrame(t) → ffmpeg, muxed with the jingle ───────────
export async function renderVideo({ canvas, audioPath, outPath, total, drawFrame, label = "sim" }) {
  const FRAMES = Math.round(total * FPS);
  console.log(`▸ ${label} · ${FRAMES} frames · ${total.toFixed(1)}s · ${W}x${H}@${FPS}`);
  const enc = spawnFFmpegEncode({ audioPath, w: W, h: H, fps: FPS, outPath, crf: 18 });
  const t0 = Date.now();
  for (let fi = 0; fi < FRAMES; fi++) {
    drawFrame(fi / FPS);
    if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
    if (fi % 150 === 0) console.log(`  ${fi}/${FRAMES} · ${((Date.now() - t0) / 1000).toFixed(0)}s`);
  }
  enc.stdin.end();
  await new Promise((res, rej) => { enc.on("close", (c) => (c === 0 ? res() : rej(new Error(`ffmpeg exit ${c}`)))); });
  console.log(`✓ base ${outPath}`);
}

export function writeMeta(slug, total, scenes) {
  writeFileSync(`${OUT}/meta-${slug}.json`, JSON.stringify({
    total,
    slides: scenes.map((s) => ({ name: s.name, from: s.from, to: s.to, tint: s.tint })),
  }, null, 2));
}

/// Scale scene fractions to seconds, sim.mjs-style.
export function makeScenes(defs, total) {
  const scenes = defs.map((s) => ({ ...s, from: s.from * total, to: s.to * total }));
  return { scenes, sceneAt: (t) => scenes.find((s) => t >= s.from && t < s.to) ?? scenes.at(-1) };
}
