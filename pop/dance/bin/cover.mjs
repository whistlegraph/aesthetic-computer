#!/usr/bin/env node
// cover.mjs — generate a dance-lane cover PNG and optionally embed it
// into an mp3 as the ID3v2 attached picture (same shape big-pictures
// uses via pop/bin/finalize.mjs).
//
// Usage:
//   node pop/dance/bin/cover.mjs --slug trance-default \
//       --title "trance" --subtitle "138 bpm · a minor" \
//       --out ~/Desktop/trance-cover.png
//
//   node pop/dance/bin/cover.mjs --slug trance-default \
//       --title "trance" \
//       --embed ~/Desktop/trance-full.mp3      # embed into the mp3
//
// Cover style: SVG-rendered radial gradient + sawtooth horizon line +
// big title + handle. SVG → PNG via `magick`.

import {
  writeFileSync, readFileSync, existsSync, unlinkSync, mkdirSync, renameSync,
} from "node:fs";
import { resolve, dirname } from "node:path";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";
import { createHash } from "node:crypto";
// homedir is used inside FONT_TTFS for the @font-face base64 embed.

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  }
}
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const SLUG     = flags.slug || "trance";
const TITLE    = flags.title || SLUG;
const TITLE_POS = (flags["title-pos"] || "center").toLowerCase(); // center | topleft
const SUBTITLE = flags.subtitle || "";
const HANDLE   = flags.handle || "@jeffrey";
const LANE     = flags.lane || "dance";
const OUT_PATH = expandHome(flags.out) || `/tmp/${SLUG}-cover.png`;
const EMBED    = expandHome(flags.embed) || null;
const HUE      = flags.hue !== undefined ? Number(flags.hue) : null; // explicit hue override
const COLOR    = flags.color || null; // named: "yellow" | "cyan" | ...
const FONT     = flags.font || "YWFT Processing"; // serif/sans family
const WAVEFORM = expandHome(flags.waveform) || null; // audio file to render
const ILLUSTRATION = expandHome(flags.illustration) || null; // background image

// ── palette ──────────────────────────────────────────────────────────
// Priority: --color name > --hue value > slug-hashed default
const NAMED_HUES = { red:0, orange:30, yellow:54, green:120, cyan:185, blue:220, purple:275, pink:325 };
const slugHash = createHash("sha256").update(SLUG).digest();
let baseHue;
if (COLOR && NAMED_HUES[COLOR] !== undefined) baseHue = NAMED_HUES[COLOR];
else if (HUE !== null && Number.isFinite(HUE)) baseHue = HUE;
else baseHue = slugHash[0] * 360 / 256;
function hsl(hueDeg, sat, light) {
  return `hsl(${hueDeg.toFixed(1)}, ${(sat * 100).toFixed(0)}%, ${(light * 100).toFixed(0)}%)`;
}
// Yellow center, magenta/cyan edges — multi-hue cover with bands.
const isYellow = COLOR === "yellow" || (baseHue >= 45 && baseHue <= 65);
const colors = isYellow ? {
  bgInner: hsl(50, 0.95, 0.58),   // pure yellow center
  bgMid:   hsl(15, 0.85, 0.50),   // orange-red ring
  bgOuter: hsl(290, 0.75, 0.20),  // dark magenta edge
  accent:  hsl(190, 0.85, 0.22),  // teal-on-yellow ink
  fg:      hsl(20, 0.85, 0.08),   // near-black
  dim:     hsl(28, 0.70, 0.25),
  stripe1: hsl(190, 0.85, 0.50),  // cyan
  stripe2: hsl(15, 0.95, 0.55),   // orange
  stripe3: hsl(290, 0.75, 0.55),  // magenta
  stripe4: hsl(120, 0.70, 0.40),  // green
} : {
  bgInner: hsl(baseHue, 0.85, 0.25),
  bgMid:   hsl((baseHue + 60) % 360, 0.75, 0.20),
  bgOuter: hsl((baseHue + 200) % 360, 0.55, 0.06),
  accent:  hsl((baseHue + 40) % 360, 0.95, 0.62),
  fg:      hsl(baseHue, 0.12, 0.97),
  dim:     hsl(baseHue, 0.30, 0.72),
  stripe1: hsl((baseHue + 30) % 360, 0.85, 0.50),
  stripe2: hsl((baseHue + 120) % 360, 0.85, 0.50),
  stripe3: hsl((baseHue + 210) % 360, 0.85, 0.50),
  stripe4: hsl((baseHue + 300) % 360, 0.85, 0.50),
};

const W = 1500;
const H = 1500;
const parts = [];
const textOverlays = [];
parts.push(`<?xml version="1.0" encoding="UTF-8"?>`);
parts.push(`<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" font-family="'${FONT}', Helvetica, Arial, sans-serif">`);

// Embed the YWFT font as base64 so rsvg-convert (which doesn't read
// ~/Library/Fonts via macOS Core Text) can still render it.
const FONT_TTFS = {
  "YWFT Processing": {
    bold:  `${homedir()}/Library/Fonts/ywft-processing-bold.ttf`,
    light: `${homedir()}/Library/Fonts/ywft-processing-light.ttf`,
  },
};
const ttfs = FONT_TTFS[FONT];
if (ttfs && existsSync(ttfs.bold)) {
  const fontStyle = [`<style type="text/css">`];
  const b64bold = readFileSync(ttfs.bold).toString("base64");
  fontStyle.push(`@font-face { font-family: "${FONT}"; font-weight: 700; src: url(data:font/truetype;base64,${b64bold}) format("truetype"); }`);
  if (existsSync(ttfs.light)) {
    const b64light = readFileSync(ttfs.light).toString("base64");
    fontStyle.push(`@font-face { font-family: "${FONT}"; font-weight: 300; src: url(data:font/truetype;base64,${b64light}) format("truetype"); }`);
  }
  fontStyle.push(`</style>`);
  parts.push(fontStyle.join("\n"));
}

// Multi-hue radial gradient: yellow center → orange ring → magenta edge.
parts.push(`<defs>`);
parts.push(`  <radialGradient id="bg" cx="50%" cy="42%" r="75%">`);
parts.push(`    <stop offset="0%" stop-color="${colors.bgInner}"/>`);
parts.push(`    <stop offset="55%" stop-color="${colors.bgMid}"/>`);
parts.push(`    <stop offset="100%" stop-color="${colors.bgOuter}"/>`);
parts.push(`  </radialGradient>`);
parts.push(`  <linearGradient id="sawglow" x1="0" y1="0" x2="0" y2="1">`);
parts.push(`    <stop offset="0%" stop-color="${colors.accent}" stop-opacity="0.9"/>`);
parts.push(`    <stop offset="100%" stop-color="${colors.accent}" stop-opacity="0.0"/>`);
parts.push(`  </linearGradient>`);
parts.push(`</defs>`);
// If an illustration was passed, embed it as the base layer (covering the
// full canvas) and skip the radial gradient. The illustration prompt
// leaves the top quarter of the frame open for title text and the
// bottom for handle/waveform — darken bands target those zones for
// readability without covering jeffrey.
const hasIllustration = ILLUSTRATION && existsSync(ILLUSTRATION);
if (hasIllustration) {
  const imgBuf = readFileSync(ILLUSTRATION);
  const ext = ILLUSTRATION.toLowerCase().endsWith(".jpg") || ILLUSTRATION.toLowerCase().endsWith(".jpeg")
    ? "jpeg" : "png";
  const b64 = imgBuf.toString("base64");
  parts.push(`<image x="0" y="0" width="${W}" height="${H}" preserveAspectRatio="xMidYMid slice" href="data:image/${ext};base64,${b64}"/>`);
  // Top darken — under title text band (y 0-500, fading by y=600).
  parts.push(`<defs>`);
  parts.push(`  <linearGradient id="titleband" x1="0" y1="0" x2="0" y2="1">`);
  parts.push(`    <stop offset="0%"   stop-color="rgba(0,0,0,0.72)"/>`);
  parts.push(`    <stop offset="50%"  stop-color="rgba(0,0,0,0.50)"/>`);
  parts.push(`    <stop offset="100%" stop-color="rgba(0,0,0,0.0)"/>`);
  parts.push(`  </linearGradient>`);
  parts.push(`  <linearGradient id="bottomband" x1="0" y1="0" x2="0" y2="1">`);
  parts.push(`    <stop offset="0%"   stop-color="rgba(0,0,0,0.0)"/>`);
  parts.push(`    <stop offset="40%"  stop-color="rgba(0,0,0,0.45)"/>`);
  parts.push(`    <stop offset="100%" stop-color="rgba(0,0,0,0.78)"/>`);
  parts.push(`  </linearGradient>`);
  parts.push(`</defs>`);
  parts.push(`<rect x="0" y="0" width="${W}" height="600" fill="url(#titleband)"/>`);
  parts.push(`<rect x="0" y="${H - 380}" width="${W}" height="380" fill="url(#bottomband)"/>`);
} else {
  parts.push(`<rect x="0" y="0" width="${W}" height="${H}" fill="url(#bg)"/>`);
}

// No top/bottom bars — the cover keeps the colored gradient clean.
const stripeColors = [colors.stripe1, colors.stripe2, colors.stripe3, colors.stripe4];

// Scatter sprites — bullet holes for the geometric cover variant,
// rainbow sparkles when an illustration is the background. Sprites
// are seeded by the slug hash so positions are deterministic and
// reproducible across rebuilds.
const spriteRng = (() => {
  let s = slugHash[10] * 256 * 256 + slugHash[11] * 256 + slugHash[12] + 1;
  return () => { s ^= s << 13; s >>>= 0; s ^= s >>> 17; s ^= s << 5; s >>>= 0; return (s >>> 0) / 0xffffffff; };
})();
// Safe-zone test: when an illustration is the background, the face
// occupies the center-right of the frame; sprites/text must live in
// the upper-left and lower-right corner negative-space the prompt
// engineered. The geometric cover keeps its broader exclusion list.
function isInExclusion(_x, y) {
  if (hasIllustration) {
    // Image-gen can land the face anywhere along the horizontal axis;
    // exclude the central vertical band (cx 380-1120, cy 200-1200) so
    // sparkles cluster in the four corner regions regardless of where
    // the model placed jeffrey. The top + bottom darken bands handle
    // text readability separately.
    if (_x > 380 && _x < 1120 && y > 200 && y < 1200) return true;
    return false;
  }
  // Geometric cover — title (380-700), waveform (760-1200), handle (1280-1420).
  if (y >= 380 && y <= 700)  return true;
  if (y >= 760 && y <= 1200) return true;
  if (y >= 1280 && y <= 1420) return true;
  return false;
}
// For the illustrated cover we skip sparkle overlays entirely — the
// painted illustration carries enough visual interest on its own.
// The geometric cover still gets its bullet-hole scatter (9 holes).
const SPRITE_COUNT = hasIllustration ? 0 : 9;
const sprites = [];
for (let attempts = 0; attempts < 300 && sprites.length < SPRITE_COUNT; attempts++) {
  const cx = 60 + spriteRng() * (W - 120);
  const cy = 60 + spriteRng() * (H - 120);
  if (isInExclusion(cx, cy)) continue;
  const r = hasIllustration ? 12 + spriteRng() * 28 : 24 + spriteRng() * 38;
  let tooClose = false;
  for (const ex of sprites) {
    const dx = ex.cx - cx, dy = ex.cy - cy;
    if (Math.sqrt(dx*dx + dy*dy) < r + ex.r + 30) { tooClose = true; break; }
  }
  if (tooClose) continue;
  sprites.push({ cx, cy, r });
}
if (hasIllustration) {
  // Rainbow sparkles — 8-point star bursts in saturated hues, with a
  // bright core and tapered radial spokes. Cycle through a rainbow
  // palette so each sparkle is its own color.
  const sparklePalette = [
    "hsl(0, 95%, 65%)",   // red
    "hsl(28, 95%, 60%)",  // orange
    "hsl(50, 95%, 60%)",  // yellow
    "hsl(120, 80%, 55%)", // green
    "hsl(180, 80%, 60%)", // cyan
    "hsl(220, 90%, 70%)", // blue
    "hsl(275, 85%, 70%)", // purple
    "hsl(325, 90%, 70%)", // pink
  ];
  for (let i = 0; i < sprites.length; i++) {
    const sp = sprites[i];
    const color = sparklePalette[i % sparklePalette.length];
    const spokes = 8;
    for (let k = 0; k < spokes; k++) {
      const angle = (k / spokes) * Math.PI * 2;
      const len = sp.r * (k % 2 === 0 ? 1.6 : 0.8);
      const x2 = sp.cx + Math.cos(angle) * len;
      const y2 = sp.cy + Math.sin(angle) * len;
      parts.push(`<line x1="${sp.cx.toFixed(1)}" y1="${sp.cy.toFixed(1)}" x2="${x2.toFixed(1)}" y2="${y2.toFixed(1)}" stroke="${color}" stroke-width="${(1.5 + spriteRng() * 1.5).toFixed(1)}" opacity="0.92" stroke-linecap="round"/>`);
    }
    // Bright core dot.
    parts.push(`<circle cx="${sp.cx.toFixed(1)}" cy="${sp.cy.toFixed(1)}" r="${(sp.r * 0.18).toFixed(1)}" fill="white" opacity="0.95"/>`);
    parts.push(`<circle cx="${sp.cx.toFixed(1)}" cy="${sp.cy.toFixed(1)}" r="${(sp.r * 0.08).toFixed(1)}" fill="${color}"/>`);
  }
} else {
  for (const hole of sprites) {
    const cracks = 5 + Math.floor(spriteRng() * 4);
    for (let c = 0; c < cracks; c++) {
      const angle = (c / cracks) * Math.PI * 2 + spriteRng() * 0.5;
      const len = hole.r * (1.5 + spriteRng() * 1.5);
      const x2 = hole.cx + Math.cos(angle) * len;
      const y2 = hole.cy + Math.sin(angle) * len;
      parts.push(`<line x1="${hole.cx.toFixed(1)}" y1="${hole.cy.toFixed(1)}" x2="${x2.toFixed(1)}" y2="${y2.toFixed(1)}" stroke="rgba(0,0,0,0.85)" stroke-width="${(2 + spriteRng() * 2).toFixed(1)}"/>`);
    }
    parts.push(`<circle cx="${hole.cx.toFixed(1)}" cy="${hole.cy.toFixed(1)}" r="${(hole.r * 0.85).toFixed(1)}" fill="rgba(0,0,0,0.20)"/>`);
    parts.push(`<circle cx="${hole.cx.toFixed(1)}" cy="${hole.cy.toFixed(1)}" r="${(hole.r * 0.55).toFixed(1)}" fill="rgba(0,0,0,0.55)"/>`);
    parts.push(`<circle cx="${hole.cx.toFixed(1)}" cy="${hole.cy.toFixed(1)}" r="${(hole.r * 0.30).toFixed(1)}" fill="rgba(0,0,0,0.95)"/>`);
    parts.push(`<circle cx="${(hole.cx - hole.r * 0.10).toFixed(1)}" cy="${(hole.cy - hole.r * 0.10).toFixed(1)}" r="${(hole.r * 0.08).toFixed(1)}" fill="rgba(255,255,255,0.20)"/>`);
  }
}
void stripeColors;

// Horizon — actual song waveform if --waveform provided, else the
// generic sawtooth glyph. When an illustration is the background, the
// waveform spans the FULL width at the bottom of the cover, vertically
// centered on the @jeffrey handle baseline. Cooler rendering: thicker
// cyan-rainbow bars with a subtle glow rim.
// Handle text baseline = 1465, size 130 → visible midpoint ≈ 1400.
// Center waveform vertically on that midpoint so it threads through
// the @jeffrey glyphs like a clean oscilloscope baseline.
const horizonY = hasIllustration ? 1370 : 980;
const horizonW = hasIllustration ? W : W - 80; // FULL width when illustrated
const horizonX0 = hasIllustration ? 0 : 120;

if (WAVEFORM && existsSync(WAVEFORM)) {
  // Decode to f32 mono via ffmpeg, then bin into N peak-amplitude bars.
  const probe = spawnSync("ffmpeg", [
    "-hide_banner", "-loglevel", "error",
    "-i", WAVEFORM,
    "-f", "f32le", "-ar", "8000", "-ac", "1", "-",
  ], { encoding: "buffer", maxBuffer: 1024 * 1024 * 256 });
  if (probe.status !== 0) {
    console.warn(`✗ waveform decode failed, falling back to sawtooth glyph`);
  } else {
    const f32 = new Float32Array(probe.stdout.buffer, probe.stdout.byteOffset, probe.stdout.byteLength / 4);
    const BARS = 280; // dense bins → a detailed, data-rich waveform read
    const samplesPerBar = Math.floor(f32.length / BARS);
    const peaks = new Float32Array(BARS);
    for (let b = 0; b < BARS; b++) {
      let max = 0;
      const start = b * samplesPerBar;
      const end = start + samplesPerBar;
      for (let i = start; i < end; i++) {
        const a = Math.abs(f32[i]);
        if (a > max) max = a;
      }
      peaks[b] = max;
    }
    // Normalize peaks so the loudest bar = 1.0
    let maxPeak = 0;
    for (let b = 0; b < BARS; b++) if (peaks[b] > maxPeak) maxPeak = peaks[b];
    if (maxPeak > 0) for (let b = 0; b < BARS; b++) peaks[b] /= maxPeak;

    const barW = horizonW / BARS;
    const maxAmp = hasIllustration ? 70 : 180; // pixels above & below center
    if (hasIllustration) {
      // Illustrated waveform — smooth catmull-rom curves through the
      // peak amplitudes (top + mirrored bottom) drawn with a thick
      // colored-pencil-style stroke, plus sparse vertical "rib"
      // lines so it reads as an oscilloscope SHAPE rather than a
      // rectangle grid. Way more drawing-like than the bar fill.
      const sampleXs = [];
      const sampleYsTop = [];
      const sampleYsBot = [];
      for (let b = 0; b < BARS; b++) {
        const x = horizonX0 + (b + 0.5) * barW;
        const h = Math.max(3, peaks[b] * maxAmp);
        sampleXs.push(x);
        sampleYsTop.push(horizonY - h);
        sampleYsBot.push(horizonY + h);
      }
      // Catmull-Rom (uniform) → cubic Bezier path. Smooths the polyline.
      function smoothPath(xs, ys) {
        if (xs.length < 2) return "";
        let d = `M ${xs[0].toFixed(1)} ${ys[0].toFixed(1)}`;
        for (let i = 0; i < xs.length - 1; i++) {
          const p0x = xs[Math.max(0, i - 1)], p0y = ys[Math.max(0, i - 1)];
          const p1x = xs[i],                   p1y = ys[i];
          const p2x = xs[i + 1],               p2y = ys[i + 1];
          const p3x = xs[Math.min(xs.length - 1, i + 2)], p3y = ys[Math.min(ys.length - 1, i + 2)];
          const c1x = p1x + (p2x - p0x) / 6;
          const c1y = p1y + (p2y - p0y) / 6;
          const c2x = p2x - (p3x - p1x) / 6;
          const c2y = p2y - (p3y - p1y) / 6;
          d += ` C ${c1x.toFixed(1)} ${c1y.toFixed(1)}, ${c2x.toFixed(1)} ${c2y.toFixed(1)}, ${p2x.toFixed(1)} ${p2y.toFixed(1)}`;
        }
        return d;
      }
      const topPath = smoothPath(sampleXs, sampleYsTop);
      // Vertical rib lines — sparse, every 5th sample, faint.
      for (let b = 0; b < BARS; b += 5) {
        const x = sampleXs[b];
        parts.push(`<line x1="${x.toFixed(1)}" y1="${sampleYsTop[b].toFixed(1)}" x2="${x.toFixed(1)}" y2="${sampleYsBot[b].toFixed(1)}" stroke="rgba(255,255,255,0.18)" stroke-width="1.2"/>`);
      }
      // Filled body — segmented with a per-section color gradient so
      // the waveform itself shows the track's progression. Each
      // segment between adjacent samples is its own colored path.
      // Colors cycle through the title palette across the timeline.
      const WAVE_PALETTE = ["#aef240", "#f54aa6", "#ffffff", "#4ad1bf", "#f599c6", "#9aff4d", "#a44af5", "#7fe05a"];
      // Filled body using one wide path — for shape; stroked color
      // segments are drawn over the top so we get a single coherent
      // bubble with a multi-color outline.
      let reversedBot = "";
      for (let i = sampleXs.length - 1; i >= 0; i--) {
        reversedBot += `L ${sampleXs[i].toFixed(1)} ${sampleYsBot[i].toFixed(1)} `;
      }
      const filledBody = topPath + " " + reversedBot + " Z";
      parts.push(`<path d="${filledBody}" fill="rgba(255,255,255,0.12)" stroke="none"/>`);
      // Bolder, COLOR-SHIFTING pencil strokes — each chunk of the
      // waveform takes the next color in the palette so the line
      // reads as a moving rainbow ribbon across the track length.
      // Stroke width bumped 3.5 → 7 so it sits as visually prominent
      // as the title typography.
      const segLen = Math.max(1, Math.floor(BARS / WAVE_PALETTE.length / 2));
      function segPath(arrXs, arrYs, from, to) {
        let d = `M ${arrXs[from].toFixed(1)} ${arrYs[from].toFixed(1)}`;
        for (let i = from; i < to; i++) {
          const p0x = arrXs[Math.max(0, i - 1)], p0y = arrYs[Math.max(0, i - 1)];
          const p1x = arrXs[i], p1y = arrYs[i];
          const p2x = arrXs[i + 1], p2y = arrYs[i + 1];
          const p3x = arrXs[Math.min(arrXs.length - 1, i + 2)], p3y = arrYs[Math.min(arrYs.length - 1, i + 2)];
          const c1x = p1x + (p2x - p0x) / 6;
          const c1y = p1y + (p2y - p0y) / 6;
          const c2x = p2x - (p3x - p1x) / 6;
          const c2y = p2y - (p3y - p1y) / 6;
          d += ` C ${c1x.toFixed(1)} ${c1y.toFixed(1)}, ${c2x.toFixed(1)} ${c2y.toFixed(1)}, ${p2x.toFixed(1)} ${p2y.toFixed(1)}`;
        }
        return d;
      }
      for (let s = 0; s < BARS - 1; s += segLen) {
        const end = Math.min(s + segLen + 1, BARS - 1);
        const color = WAVE_PALETTE[Math.floor((s / BARS) * WAVE_PALETTE.length) % WAVE_PALETTE.length];
        parts.push(`<path d="${segPath(sampleXs, sampleYsTop, s, end)}" fill="none" stroke="${color}" stroke-width="7" stroke-linecap="round" stroke-linejoin="round" opacity="0.95"/>`);
        parts.push(`<path d="${segPath(sampleXs, sampleYsBot, s, end)}" fill="none" stroke="${color}" stroke-width="7" stroke-linecap="round" stroke-linejoin="round" opacity="0.95"/>`);
      }
      // No timestamp on the static cover — duration appears only in
      // the video visualizer.
    } else {
      // Geometric cover — unchanged plain accent-color bars.
      for (let b = 0; b < BARS; b++) {
        const x = horizonX0 + b * barW;
        const h = peaks[b] * maxAmp;
        parts.push(`<rect x="${x.toFixed(1)}" y="${(horizonY - h).toFixed(1)}" width="${(barW * 0.7).toFixed(2)}" height="${(h * 2).toFixed(1)}" fill="${colors.accent}" opacity="0.95"/>`);
      }
      parts.push(`<line x1="${horizonX0}" y1="${horizonY}" x2="${horizonX0 + horizonW}" y2="${horizonY}" stroke="${colors.dim}" stroke-width="2" opacity="0.4"/>`);
    }
  }
} else if (!hasIllustration) {
  // Fallback: original sawtooth silhouette — only on the GEOMETRIC
  // cover. The illustration cover stays clean if no --waveform is
  // provided; the yellow sawtooth was reading as a stray graphic
  // overlayed on the painted scene.
  const sawAmp = 110;
  const sawTeeth = 14;
  let sawPath = `M ${horizonX0} ${horizonY}`;
  for (let i = 0; i < sawTeeth; i++) {
    const x2 = horizonX0 + ((i + 0.5) / sawTeeth) * horizonW;
    const x3 = horizonX0 + ((i + 1) / sawTeeth) * horizonW;
    sawPath += ` L ${x2.toFixed(1)} ${(horizonY - sawAmp).toFixed(1)} L ${x3.toFixed(1)} ${horizonY}`;
  }
  const glowPath = sawPath + ` L ${horizonX0 + horizonW} ${horizonY + 220} L ${horizonX0} ${horizonY + 220} Z`;
  parts.push(`<path d="${glowPath}" fill="url(#sawglow)" opacity="0.55"/>`);
  parts.push(`<path d="${sawPath}" stroke="${colors.accent}" stroke-width="6" fill="none" stroke-linejoin="miter"/>`);
}

// Text is rendered by magick in a post-pass (rsvg-convert can't load
// YWFT from ~/Library/Fonts on macOS even with @font-face data URIs).
// Store text layout here; the magick overlay step consumes it.
const titleStr = TITLE.toLowerCase();
// textOverlays is declared up near the SVG canvas init so the waveform
// block can push the track-length label into it.

if (hasIllustration) {
  // Larger title — bumped max to 220 so the YWFT glyphs read big.
  // Rainbow per-character via magick with measured kerning + dark
  // drop shadow (glow removed so the YWFT pixel edges stay crisp).
  const illustTitleSize = Math.min(220, Math.floor((W - 240) / (titleStr.length * 0.45)));
  if (TITLE_POS === "topleft") {
    const tlSize = Math.round(illustTitleSize * 0.58);   // smaller for the corner
    textOverlays.push({ text: titleStr, x: 42, y: 168, size: tlSize, color: "white", weight: 800, anchor: "start", rainbow: true });
  } else {
    textOverlays.push({ text: titleStr, x: W / 2, y: 280, size: illustTitleSize, color: "white", weight: 800, anchor: "middle", rainbow: true });
  }
  // No subtitle, no LANE — photo + fairies + title + handle are enough.
  // @jeffrey: larger (130), pushed further into the bottom-right
  // corner with more margin (90 px from each edge).
  // Track-length label goes near the waveform (pushed below after the
  // waveform code computes the duration). No byline under the title.
} else {
  // Geometric cover — original centered layout.
  const titleFontSize = Math.min(280, Math.floor((W - 240) / (titleStr.length * 0.5)));
  textOverlays.push({ text: titleStr, x: W / 2, y: 550,  size: titleFontSize, color: colors.fg,     weight: 800, anchor: "middle" });
  if (SUBTITLE) {
    textOverlays.push({ text: SUBTITLE, x: W / 2, y: 650, size: 38, color: colors.dim, weight: 500, anchor: "middle" });
  }
  textOverlays.push({ text: LANE,   x: W / 2, y: 1180, size: 56,  color: colors.fg,     weight: 700, anchor: "middle" });
  textOverlays.push({ text: HANDLE, x: W / 2, y: 1350, size: 120, color: colors.accent, weight: 800, anchor: "middle" });
}

parts.push(`</svg>`);

const svgPath = OUT_PATH.replace(/\.png$/, ".svg");
const bgPngPath = OUT_PATH.replace(/\.png$/, ".bg.png");
mkdirSync(dirname(OUT_PATH), { recursive: true });
writeFileSync(svgPath, parts.join("\n"));

// Step 1: rsvg-convert renders the SVG (graphics only) → bg.png
const conv = spawnSync("rsvg-convert", ["-w", String(W), "-o", bgPngPath, svgPath], { stdio: "inherit" });
if (conv.status !== 0 || !existsSync(bgPngPath)) {
  console.error("✗ rsvg-convert failed — leaving SVG only:", svgPath);
  process.exit(1);
}
try { unlinkSync(svgPath); } catch {}

// Step 2: magick draws text on top using the explicit YWFT TTF path.
// rsvg can't find user-Library fonts; magick can take a -font path.
const ttfPath = ttfs?.bold || null;

// Measure a string's rendered width at a given font size using the
// supplied TTF. Returns 0 on failure. Used for accurate per-char
// kerning in the rainbow title render.
function magickMeasureWidth(text, fontSize) {
  const r = spawnSync("magick", [
    "-font", ttfPath,
    "-pointsize", String(fontSize),
    `label:${text}`,
    "-format", "%w",
    "info:",
  ], { encoding: "utf8" });
  if (r.status !== 0) return 0;
  return parseInt(r.stdout.trim(), 10) || 0;
}

// Per-character title palette — yellow dropped per the trancenwaltz
// cover direction. Chartreuse-leaning greens + pinks + cyan + warm
// off-white carry the chartreuse-album palette without the heavy
// yellow notes the rainbow set kept landing on the "a"s and "t"s.
const RAINBOW = [
  "#aef240", // chartreuse-green
  "#f54aa6", // hot pink
  "#ffffff", // warm white
  "#4ad1bf", // cyan
  "#f599c6", // bubblegum pink
  "#9aff4d", // bright chartreuse
  "#a44af5", // purple
  "#7fe05a", // mid green
];

if (ttfPath && existsSync(ttfPath)) {
  const magickArgs = [bgPngPath];
  for (const ov of textOverlays) {
    const fontSize = Math.round(ov.size);
    const drawY = Math.round(ov.y);

    if (ov.rainbow) {
      // Measure cumulative widths of each prefix so each character's
      // x position respects the font's actual kerning. Then draw per
      // character: dark drop shadow → blurred colored glow → solid
      // colored glyph on top.
      const chars = [...ov.text];
      const prefixWidths = [0];
      let cumulative = "";
      for (const ch of chars) {
        cumulative += ch;
        prefixWidths.push(magickMeasureWidth(cumulative, fontSize) || prefixWidths[prefixWidths.length - 1] + fontSize * 0.45);
      }
      const totalWidth = prefixWidths[prefixWidths.length - 1];
      let baseX;
      if (ov.anchor === "middle")     baseX = Math.round(ov.x - totalWidth / 2);
      else if (ov.anchor === "end")   baseX = Math.round(ov.x - totalWidth);
      else                            baseX = Math.round(ov.x);

      // Sharp YWFT — drop shadow + solid colored letter only. The
      // previous halo glow softened the pixel-style edges; removing
      // it keeps the letterforms crisp.
      let hueIdx = 0;
      for (let i = 0; i < chars.length; i++) {
        const ch = chars[i];
        const x = baseX + prefixWidths[i];
        // Drop shadow — sharp 5px offset, near-black, no blur.
        magickArgs.push("-font", ttfPath, "-pointsize", String(fontSize));
        magickArgs.push("-fill", "rgba(0,0,0,0.78)");
        magickArgs.push("-annotate", `+${x + 5}+${drawY + 5}`, ch);
        if (ch.trim()) {
          const color = RAINBOW[hueIdx % RAINBOW.length];
          // Solid colored letter on top — no glow halo so YWFT edges stay sharp.
          magickArgs.push("-font", ttfPath, "-pointsize", String(fontSize));
          magickArgs.push("-fill", color);
          magickArgs.push("-annotate", `+${x}+${drawY}`, ch);
          hueIdx++;
        }
      }
    } else if (ov.shadow) {
      // Single-color overlay with sharp drop shadow (no glow halo).
      // The colored glow was softening YWFT's pixel edges — dropped
      // so the letterforms read crisply on the cover.
      const charW = fontSize * 0.5;
      const totalWidth = ov.text.length * charW;
      let baseX;
      if (ov.anchor === "middle")     baseX = Math.round(ov.x - totalWidth / 2);
      else if (ov.anchor === "end")   baseX = Math.round(ov.x - totalWidth);
      else                            baseX = Math.round(ov.x);
      magickArgs.push("-font", ttfPath, "-pointsize", String(fontSize));
      magickArgs.push("-fill", "rgba(0,0,0,0.78)");
      magickArgs.push("-annotate", `+${baseX + 6}+${drawY + 6}`, ov.text);
      magickArgs.push("-font", ttfPath, "-pointsize", String(fontSize));
      magickArgs.push("-fill", ov.color);
      magickArgs.push("-annotate", `+${baseX}+${drawY}`, ov.text);
    } else {
      // Plain single-color annotate — original path for the geometric
      // cover.
      const charW = fontSize * 0.5;
      const totalWidth = ov.text.length * charW;
      let baseX;
      if (ov.anchor === "middle")     baseX = Math.round(ov.x - totalWidth / 2);
      else if (ov.anchor === "end")   baseX = Math.round(ov.x - totalWidth);
      else                            baseX = Math.round(ov.x);
      magickArgs.push("-font", ttfPath, "-pointsize", String(fontSize));
      magickArgs.push("-fill", ov.color);
      magickArgs.push("-annotate", `+${baseX}+${drawY}`, ov.text);
    }
  }
  magickArgs.push(OUT_PATH);
  const mr = spawnSync("magick", magickArgs, { stdio: "inherit" });
  if (mr.status !== 0) {
    console.error("✗ magick text overlay failed — copying bg only");
    spawnSync("cp", [bgPngPath, OUT_PATH]);
  }
} else {
  spawnSync("cp", [bgPngPath, OUT_PATH]);
}
try { unlinkSync(bgPngPath); } catch {}
console.log(`✓ ${OUT_PATH}`);

// ── optionally embed into the mp3 ─────────────────────────────────────
if (EMBED) {
  if (!existsSync(EMBED)) {
    console.error(`✗ --embed target not found: ${EMBED}`);
    process.exit(1);
  }
  console.log(`→ embedding cover into ${EMBED}`);
  const tmpOut = `${EMBED}.with-cover.mp3`;
  const args = [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", EMBED,
    "-i", OUT_PATH,
    "-map", "0:a",
    "-map", "1",
    "-c", "copy",
    "-id3v2_version", "3",
    "-write_id3v1", "1",
    "-disposition:v", "attached_pic",
    "-metadata:s:v", "title=Album cover",
    "-metadata:s:v", "comment=Cover (front)",
    "-metadata", `title=${TITLE}`,
    "-metadata", `artist=aesthetic.computer`,
    "-metadata", `album=pixsies`,
    tmpOut,
  ];
  const ff = spawnSync("ffmpeg", args, { stdio: "inherit" });
  if (ff.status !== 0) {
    console.error("✗ ffmpeg embed failed");
    try { unlinkSync(tmpOut); } catch {}
    process.exit(1);
  }
  // Atomic swap
  renameSync(tmpOut, EMBED);
  console.log(`✓ cover embedded in ${EMBED}`);
}
