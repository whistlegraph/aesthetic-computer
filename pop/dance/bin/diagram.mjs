#!/usr/bin/env node
// diagram.mjs — render a track structure PNG from a trance.struct.json.
//
// Reads the struct JSON emitted by recap/bin/trance.mjs and produces a
// 1600x900 PNG that shows: title bar (meter, BPM, scale, total length),
// section blocks colored by type, a layer activity grid (one row per
// instrument), and any per-section fx markers (wobble / bitcrush).
//
// Output is SVG first, converted to PNG via `magick`.
//
// Usage:
//   node pop/dance/bin/diagram.mjs <struct.json> [--out path.png]

import { readFileSync, writeFileSync, unlinkSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  } else positional.push(a);
}
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const structPath = positional[0];
if (!structPath) { console.error("usage: diagram.mjs <struct.json> [--out path.png]"); process.exit(2); }
const outPath = expandHome(flags.out) || structPath.replace(/\.struct\.json$/, ".struct.png");

const struct = JSON.parse(readFileSync(structPath, "utf8"));
const W = 1600;
const H = 900;
const margin = { l: 140, r: 60, t: 100, b: 60 };

// ── colors per section type ──────────────────────────────────────────
function sectionColor(name) {
  if (name.startsWith("intro"))  return "#3a3a44";
  if (name.startsWith("outro"))  return "#3a3a44";
  if (name.startsWith("break"))  return "#2c4f7a";
  if (name.startsWith("build"))  return "#c97a2a";
  if (name.startsWith("drop"))   return "#b8345a";
  return "#555";
}

const LAYERS = ["kick", "hat", "sub", "pad", "lead", "piano", "bells", "vocal", "riser", "snareRoll"];
const LAYER_LABELS = {
  kick: "kick",
  hat: "hi-hat",
  sub: "sub bass",
  pad: "pad (saw)",
  lead: "lead (saw)",
  piano: "piano",
  bells: "sinebells",
  vocal: "vocal",
  riser: "riser",
  snareRoll: "snare roll",
};

// ── layout ────────────────────────────────────────────────────────────
const innerW = W - margin.l - margin.r;
const innerH = H - margin.t - margin.b;
const sectionH = 80;
const sectionY = margin.t;
const gridY = sectionY + sectionH + 24;
const gridH = innerH - sectionH - 24 - 28; // leave room for time axis
const rowH = gridH / LAYERS.length;
const totalSec = struct.totalSec || 1;

function xForSec(s) {
  return margin.l + (s / totalSec) * innerW;
}

// ── SVG body ──────────────────────────────────────────────────────────
const parts = [];
parts.push(`<?xml version="1.0" encoding="UTF-8"?>`);
parts.push(`<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" font-family="Menlo, monospace">`);

// Background
parts.push(`<rect x="0" y="0" width="${W}" height="${H}" fill="#0d0e12"/>`);

// Title block
const meterLabel = struct.meter === 3 ? "3/4 · trancewaltz" : "4/4 · trance";
const minutes = Math.floor(totalSec / 60);
const seconds = Math.round(totalSec - minutes * 60).toString().padStart(2, "0");
parts.push(`<text x="${margin.l}" y="48" fill="#f0f0f5" font-size="32" font-weight="700">${meterLabel}</text>`);
parts.push(`<text x="${margin.l}" y="78" fill="#9da0aa" font-size="18">${struct.bpm} BPM · ${struct.scale} · ${struct.totalBars} bars · ${minutes}:${seconds}</text>`);

// Right-side stats
const c = struct.counts;
let statsTxt = `kick ${c.kickCount} · hat ${c.hatCount} · sub ${c.subCount} · pad ${c.padCount} · lead ${c.leadCount}`;
if (c.pianoCount) statsTxt += ` · piano ${c.pianoCount}`;
if (c.bellsCount) statsTxt += ` · bells ${c.bellsCount}`;
if (c.riserCount) statsTxt += ` · riser ${c.riserCount}`;
if (c.snareRollCount) statsTxt += ` · snare-roll ${c.snareRollCount}`;
parts.push(`<text x="${W - margin.r}" y="78" fill="#9da0aa" font-size="14" text-anchor="end">${statsTxt}</text>`);

// Section blocks
for (const s of struct.sections) {
  const x = xForSec(s.startSec);
  const w = Math.max(2, xForSec(s.endSec) - x);
  parts.push(`<rect x="${x.toFixed(1)}" y="${sectionY}" width="${w.toFixed(1)}" height="${sectionH}" fill="${sectionColor(s.name)}" rx="6"/>`);

  // Section label
  const labelTxt = s.name;
  const subTxt = `${s.endBar - s.startBar}b`;
  if (w > 50) {
    parts.push(`<text x="${(x + w / 2).toFixed(1)}" y="${sectionY + sectionH / 2 - 4}" fill="#f0f0f5" font-size="18" font-weight="700" text-anchor="middle">${labelTxt}</text>`);
    parts.push(`<text x="${(x + w / 2).toFixed(1)}" y="${sectionY + sectionH / 2 + 18}" fill="#d8dae0" font-size="13" text-anchor="middle">${subTxt}</text>`);
  }

  // FX marker
  if (s.fx && s.fx.length) {
    const fxTxt = s.fx.map((f) => f === "wobble" ? "~wob" : f === "bitcrush" ? "⌐crush" : f).join(" ");
    parts.push(`<text x="${(x + 6).toFixed(1)}" y="${sectionY - 6}" fill="#e8c478" font-size="13" font-weight="600">${fxTxt}</text>`);
  }
}

// Layer grid
for (let li = 0; li < LAYERS.length; li++) {
  const layer = LAYERS[li];
  const y = gridY + li * rowH;
  // Row label
  parts.push(`<text x="${margin.l - 12}" y="${(y + rowH / 2 + 5).toFixed(1)}" fill="#9da0aa" font-size="13" text-anchor="end">${LAYER_LABELS[layer]}</text>`);
  // Background row
  parts.push(`<rect x="${margin.l}" y="${y.toFixed(1)}" width="${innerW}" height="${rowH - 4}" fill="#1a1c22"/>`);
  // Per-section activity
  for (const s of struct.sections) {
    if (!s.layers[layer]) continue;
    const x = xForSec(s.startSec);
    const w = Math.max(2, xForSec(s.endSec) - x);
    const fill = sectionColor(s.name);
    parts.push(`<rect x="${x.toFixed(1)}" y="${(y + 4).toFixed(1)}" width="${w.toFixed(1)}" height="${(rowH - 12).toFixed(1)}" fill="${fill}" opacity="0.88" rx="3"/>`);
  }
}

// Time axis ticks at 10s intervals
const axisY = gridY + gridH + 12;
parts.push(`<line x1="${margin.l}" y1="${axisY}" x2="${W - margin.r}" y2="${axisY}" stroke="#3a3c44" stroke-width="1"/>`);
const tickStep = totalSec > 90 ? 20 : totalSec > 30 ? 10 : 5;
for (let s = 0; s <= totalSec; s += tickStep) {
  const x = xForSec(s);
  parts.push(`<line x1="${x.toFixed(1)}" y1="${axisY}" x2="${x.toFixed(1)}" y2="${axisY + 6}" stroke="#5a5d66" stroke-width="1"/>`);
  const mm = Math.floor(s / 60);
  const ss = Math.round(s - mm * 60).toString().padStart(2, "0");
  parts.push(`<text x="${x.toFixed(1)}" y="${axisY + 22}" fill="#9da0aa" font-size="11" text-anchor="middle">${mm}:${ss}</text>`);
}

// Footer: filename
const stem = basename(structPath).replace(/\.struct\.json$/, "");
parts.push(`<text x="${W - margin.r}" y="${H - 18}" fill="#5a5d66" font-size="11" text-anchor="end">${stem}</text>`);

parts.push(`</svg>`);

const svgPath = outPath.replace(/\.png$/, ".svg");
mkdirSync(dirname(outPath), { recursive: true });
writeFileSync(svgPath, parts.join("\n"));

// macOS qlmanage outputs <input>.png next to its -o dir. Convert there
// then rename to the requested outPath.
const conv = spawnSync("rsvg-convert", ["-w", String(W), "-o", outPath, svgPath], { stdio: "inherit" });
if (conv.status !== 0 || !existsSync(outPath)) {
  console.error("✗ rsvg-convert failed — leaving SVG only:", svgPath);
  process.exit(1);
}
try { unlinkSync(svgPath); } catch {}
console.log(`✓ ${outPath}`);
