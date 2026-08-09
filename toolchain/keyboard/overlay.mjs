#!/usr/bin/env node
// Ghost the canonical layout over a real top-down photograph of the machine.
//
//   node toolchain/keyboard/overlay.mjs out.png
//   node toolchain/keyboard/overlay.mjs out.png --photo path.jpg --rect 0.177,0.206,0.646,0.384
//   node toolchain/keyboard/overlay.mjs out.png --solid        # filled, not outlined
//
// Proportions are the one thing a layout file cannot check about itself.
// validate-keyboard.mjs proves every row sums to 14.5u, which catches a
// fused or missing cap — but a deck that is the right shape internally can
// still be the wrong size, in the wrong place, or have the wrong row height
// relative to the actual product. Drawing the layout as outlines on top of
// the photograph makes that visible: if the geometry is right the outlines
// sit on the real keycaps, and if it is wrong they drift, and you can see
// exactly which row starts drifting first.
//
// `--rect` is the keyboard well as fractions of the photo (x, y, w, h),
// measured from the deck's outer keycap edges — not the deck's plastic
// surround.

import { createCanvas, loadImage } from "canvas";
import { writeFileSync } from "node:fs";
import { layout, ROW_UNITS, rows } from "./macbook-neo-layout.mjs";

const arg = (flag, fallback) => {
  const i = process.argv.indexOf(flag);
  return i > -1 ? process.argv[i + 1] : fallback;
};

const out = process.argv[2] ?? "overlay.png";
const photo = arg("--photo",
  new URL("../../slab/menuband/marketing/notepat-launch/refs/neo-overhead-citrus.jpg",
    import.meta.url).pathname);
const solid = process.argv.includes("--solid");
const [rx, ry, rw, rh] = arg("--rect", "0.1781,0.2141,0.6434,0.3834")
  .split(",").map(Number);

const img = await loadImage(photo);
const canvas = createCanvas(img.width, img.height);
const ctx = canvas.getContext("2d");
ctx.drawImage(img, 0, 0);

// Fit the deck to the measured rect. The layout is ROW_UNITS wide and
// `rowUnits` tall, so a single unit is derived from the rect's width and
// checked against its height — a mismatch between the two is itself a
// finding, so it gets printed rather than silently averaged away.
const rowUnits = rows.reduce((sum, r) => sum + r.h, 0);
const boxX = rx * img.width, boxY = ry * img.height;
const boxW = rw * img.width, boxH = rh * img.height;

// Fit x and y INDEPENDENTLY. Averaging one unit out of both makes a slightly
// wrong rect look like a slowly accumulating layout error — the outlines
// creep away from the real keys a little more with every column, which reads
// like a structural fault when it is only a bad measurement. Scaling each
// axis to its own extent pins the first and last cap to the real first and
// last cap, so anything still misaligned in between is genuinely the layout.
const unitX = boxW / ROW_UNITS;
const unitY = boxH / rowUnits;
const skew = ((unitX - unitY) / ((unitX + unitY) / 2)) * 100;

// Lay out in unit space, then scale per axis.
const gapU = 0.11;
const { caps } = layout({ unit: 1, gap: gapU, pad: 0 });
for (const cap of caps) {
  cap.x *= unitX; cap.w *= unitX;
  cap.y *= unitY; cap.h *= unitY;
}
const unit = unitX;

ctx.lineWidth = Math.max(1, unit * 0.035);
ctx.strokeStyle = "rgba(255,0,170,0.85)";
ctx.fillStyle = "rgba(255,0,170,0.22)";

for (const cap of caps) {
  const x = boxX + cap.x, y = boxY + cap.y;
  const r = Math.min(unit * 0.13, cap.h / 2);
  ctx.beginPath();
  ctx.moveTo(x + r, y);
  ctx.arcTo(x + cap.w, y, x + cap.w, y + cap.h, r);
  ctx.arcTo(x + cap.w, y + cap.h, x, y + cap.h, r);
  ctx.arcTo(x, y + cap.h, x, y, r);
  ctx.arcTo(x, y, x + cap.w, y, r);
  ctx.closePath();
  if (solid) ctx.fill();
  ctx.stroke();
}

// The deck's own bounding box, so edge alignment is checkable too.
ctx.strokeStyle = "rgba(0,200,255,0.9)";
ctx.lineWidth = Math.max(1, unit * 0.05);
ctx.strokeRect(boxX, boxY, boxW, boxH);

writeFileSync(out, canvas.toBuffer("image/png"));
console.log(
  `wrote ${out} — ${caps.length} caps over ${photo.split("/").pop()}\n` +
  `  unit x ${unitX.toFixed(2)}px  unit y ${unitY.toFixed(2)}px\n` +
  
  `  aspect skew ${skew >= 0 ? "+" : ""}${skew.toFixed(1)}%  ` +
  (Math.abs(skew) < 3
    ? "— rect and layout agree"
    : "— rect and layout DISAGREE; the rect or a row height is off"),
);
