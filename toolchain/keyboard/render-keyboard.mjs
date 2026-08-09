#!/usr/bin/env node
// Render the canonical MacBook Neo layout to a clean PNG.
//
//   node toolchain/keyboard/render-keyboard.mjs [out.png] [--scale 2]
//
// Deliberately plain: flat caps, one legend each, no lighting or press
// states. This image exists to be read and checked against a real deck,
// so anything decorative would only make a wrong key harder to spot.

import { createCanvas } from "canvas";
import { writeFileSync } from "node:fs";
import { layout, ROW_UNITS } from "./macbook-neo-layout.mjs";

const out = process.argv[2] ?? "macbook-neo-keyboard.png";
const scaleFlag = process.argv.indexOf("--scale");
const scale = scaleFlag > -1 ? Number(process.argv[scaleFlag + 1]) : 2;

const UNIT = 56;
const { caps, width, height, unit, pad } = layout({ unit: UNIT, gap: 5, pad: 30 });

// Canvas dimensions must be whole pixels; the function row's 0.62u height
// makes the deck fractional, so round up rather than truncate a hairline.
const canvas = createCanvas(Math.ceil(width * scale), Math.ceil(height * scale));
const ctx = canvas.getContext("2d");
ctx.scale(scale, scale);

const DECK = "#1b1b1f";
const CAP = "#2e2e35";
const CAP_EDGE = "#3f3f48";
const LEGEND = "#e8e8ef";
const DIM = "#9a9aa8";

function roundRect(x, y, w, h, r) {
  ctx.beginPath();
  ctx.moveTo(x + r, y);
  ctx.arcTo(x + w, y, x + w, y + h, r);
  ctx.arcTo(x + w, y + h, x, y + h, r);
  ctx.arcTo(x, y + h, x, y, r);
  ctx.arcTo(x, y, x + w, y, r);
  ctx.closePath();
}

ctx.fillStyle = DECK;
ctx.fillRect(0, 0, width, height);

// The arrow cluster is one cap-sized block holding an inverted T: left and
// right full height, up and down stacked in the middle column.
function drawArrows(cap) {
  const colW = cap.w / 3, halfH = cap.h / 2;
  const cells = [
    ["←", cap.x, cap.y, colW, cap.h],
    ["↑", cap.x + colW, cap.y, colW, halfH],
    ["↓", cap.x + colW, cap.y + halfH, colW, halfH],
    ["→", cap.x + colW * 2, cap.y, colW, cap.h],
  ];
  for (const [glyph, x, y, w, h] of cells) {
    roundRect(x + 1.5, y + 1.5, w - 3, h - 3, unit * 0.1);
    ctx.fillStyle = CAP; ctx.fill();
    ctx.strokeStyle = CAP_EDGE; ctx.lineWidth = 1; ctx.stroke();
    ctx.fillStyle = DIM;
    ctx.font = `${Math.round(unit * 0.26)}px sans-serif`;
    ctx.textAlign = "center"; ctx.textBaseline = "middle";
    ctx.fillText(glyph, x + w / 2, y + h / 2);
  }
}

for (const cap of caps) {
  if (cap.style === "arrows") { drawArrows(cap); continue; }

  roundRect(cap.x, cap.y, cap.w, cap.h, unit * 0.12);
  ctx.fillStyle = CAP; ctx.fill();
  ctx.strokeStyle = CAP_EDGE; ctx.lineWidth = 1; ctx.stroke();

  if (cap.style === "space") continue; // the space bar carries no legend

  // Single-character legends read large and centered; word legends shrink
  // to fit their cap so "caps lock" never spills past its edges.
  const isWord = cap.label.length > 1;
  const size = isWord
    ? Math.min(unit * 0.24, (cap.w - 12) / (cap.label.length * 0.52))
    : unit * 0.36;
  ctx.fillStyle = isWord ? DIM : LEGEND;
  ctx.font = `${Math.round(size)}px sans-serif`;
  ctx.textAlign = "center"; ctx.textBaseline = "middle";
  ctx.fillText(cap.label, cap.x + cap.w / 2, cap.y + cap.h / 2);
}

// Caption the deck with what it claims to be, so a stray copy of this PNG
// is still self-identifying.
ctx.fillStyle = DIM;
ctx.font = `${Math.round(unit * 0.2)}px sans-serif`;
ctx.textAlign = "left"; ctx.textBaseline = "alphabetic";
ctx.fillText(`MacBook Neo (Mac17,5) — ANSI US — ${caps.length} caps, ${ROW_UNITS}u rows`,
  pad, height - pad * 0.35);

writeFileSync(out, canvas.toBuffer("image/png"));
console.log(`wrote ${out} — ${canvas.width}×${canvas.height}px, ${caps.length} caps`);
