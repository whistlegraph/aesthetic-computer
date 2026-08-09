#!/usr/bin/env node
// Derive the keyboard-well rect from a top-down photograph, by pixels.
//
//   node toolchain/keyboard/measure-deck.mjs [photo.jpg]
//
// overlay.mjs needs to know where the deck sits in the reference photo. That
// number was hand-estimated at first, and a hand-estimated rect is worse than
// useless: it makes the outlines drift, which reads as a layout fault and
// sends you looking for a bug that is not there. Measuring it instead turns
// the overlay's residual skew into a real signal — if the rect is measured
// and the skew persists, the layout's row heights are genuinely wrong.
//
// Keycaps are the brightest, least saturated things on a coloured deck, so
// they threshold cleanly without any edge detection.

import { createCanvas, loadImage } from "canvas";
import { ROW_UNITS, rows } from "./macbook-neo-layout.mjs";

const photo = process.argv[2] ?? new URL(
  "../../slab/menuband/marketing/notepat-launch/refs/neo-overhead-citrus.jpg",
  import.meta.url).pathname;

const img = await loadImage(photo);
const ctx = createCanvas(img.width, img.height).getContext("2d");
ctx.drawImage(img, 0, 0);
const px = ctx.getImageData(0, 0, img.width, img.height).data;

const rgb = (x, y) => {
  const i = (y * img.width + x) * 4;
  return [px[i], px[i + 1], px[i + 2]];
};
const sat = (x, y) => { const [r, g, b] = rgb(x, y); return Math.max(r, g, b) - Math.min(r, g, b); };
const lum = (x, y) => { const [r, g, b] = rgb(x, y); return Math.min(r, g, b); };

// Keycaps and the studio background are BOTH bright and near-neutral — a
// brightness threshold alone finds the background and reports the whole
// frame. The discriminator is the deck: anodised colour is saturated where
// caps and backdrop are not. So find the coloured body first, then look for
// caps strictly inside it.
//   background sat≈2   keycap sat≈5–11   deck/body sat≈19
let bx0 = Infinity, bx1 = -1, by0 = Infinity, by1 = -1;
for (let y = 0; y < img.height; y += 2) {
  for (let x = 0; x < img.width; x += 2) {
    if (sat(x, y) < 14 || lum(x, y) < 120) continue;
    if (x < bx0) bx0 = x; if (x > bx1) bx1 = x;
    if (y < by0) by0 = y; if (y > by1) by1 = y;
  }
}

const capPixel = (x, y) => sat(x, y) < 13 && lum(x, y) > 225;

// Erode. A specular highlight along the body's rounded edge is a bright
// neutral LINE — one or two pixels wide but hundreds tall — so it scores as
// high in a column profile as a real column of keycaps does. Requiring a
// pixel's neighbours to also be cap-coloured erases anything thinner than a
// keycap while leaving key interiors untouched.
const R = Math.max(3, Math.round(img.width * 0.004));
const isCap = (x, y) =>
  capPixel(x, y) && capPixel(x - R, y) && capPixel(x + R, y)
  && capPixel(x, y - R) && capPixel(x, y + R);

// The BASE, not the whole body. Seen from above with the lid open, the lid
// is wider in frame than the base — so a body-wide search band runs past the
// base's sides and out onto the studio background, which is bright and
// neutral and therefore passes any keycap test. Re-measuring the coloured
// mask over the lower half alone gives the base's true left and right edges.
const mid = Math.floor((by0 + by1) / 2);
let sx0 = Infinity, sx1 = -1, sy1 = -1;
for (let y = mid; y <= by1; y += 2) {
  for (let x = bx0; x <= bx1; x += 2) {
    if (sat(x, y) < 14 || lum(x, y) < 120) continue;
    if (x < sx0) sx0 = x; if (x > sx1) sx1 = x;
    if (y > sy1) sy1 = y;
  }
}
const inset = Math.round((sx1 - sx0) * 0.02);
const xA = sx0 + inset, xB = sx1 - inset;

// Now the base's vertical band. Starting the search at the body's midpoint
// would clip the top keyboard rows, but starting at the body's top runs
// through the gap between the lid and the base — which is background, and
// background reads as keycap. So grow a contiguous run of coloured rows
// outward from the midpoint: that run is exactly the base.
const rowColour = new Int32Array(img.height);
for (let y = by0; y <= by1; y++) {
  let n = 0;
  for (let x = xA; x <= xB; x += 2) if (sat(x, y) >= 14 && lum(x, y) >= 120) n++;
  rowColour[y] = n;
}
// An ABSOLUTE floor, not a fraction of the peak. The peak lands on the palm
// rest, which is solid colour; keyboard rows are mostly keycap and carry
// colour only in the gaps between caps, so a peak-relative cut stops the
// moment it reaches the keyboard. The gap between lid and base, by contrast,
// is background and carries essentially no colour at all — so a low absolute
// threshold separates "inside the base" from "not the base" cleanly.
const rowSamples = Math.floor((xB - xA) / 2) + 1;
const rowCut = rowSamples * 0.1;
let baseTop = mid, baseBottom = mid;
while (baseTop > by0 && rowColour[baseTop - 1] >= rowCut) baseTop--;
while (baseBottom < by1 && rowColour[baseBottom + 1] >= rowCut) baseBottom++;

const yA = baseTop + inset, yB = baseBottom - inset;
if (process.env.DEBUG_DECK)
  console.error({ bx0, bx1, by0, by1, mid, sx0, sx1, baseTop, baseBottom, xA, xB, yA, yB });

// Extents by PROFILE, not by extremes. A rounded body edge throws a bright
// neutral specular highlight that passes any cap test, and a single such
// pixel drags a min/max scan to the wrong place. The keyboard is instead the
// only structure in the frame that produces dense, sustained runs of cap
// pixels across many rows and columns — so threshold the profiles at a
// fraction of their own peak and take the first and last crossing.
const colCount = new Int32Array(img.width);
const rowCount = new Int32Array(img.height);
for (let y = yA; y < yB; y++) {
  for (let x = xA; x < xB; x++) {
    if (!isCap(x, y)) continue;
    colCount[x]++; rowCount[y]++;
  }
}
const span = (arr, lo, hi, frac) => {
  let peak = 0;
  for (let i = lo; i < hi; i++) if (arr[i] > peak) peak = arr[i];
  const cut = peak * frac;
  let a = -1, b = -1;
  for (let i = lo; i < hi; i++) if (arr[i] >= cut) { a = i; break; }
  for (let i = hi - 1; i >= lo; i--) if (arr[i] >= cut) { b = i; break; }
  return [a, b];
};
const [minX, maxX] = span(colCount, xA, xB, 0.35);
const [minY, maxY] = span(rowCount, yA, yB, 0.35);

const w = maxX - minX, h = maxY - minY;
const rowUnits = rows.reduce((s, r) => s + r.h, 0);
const unitX = w / ROW_UNITS, unitY = h / rowUnits;
const skew = ((unitX - unitY) / ((unitX + unitY) / 2)) * 100;

// Per-row bands. The aggregate skew says the deck is wider per unit than it
// is tall per unit, but not WHY — an evenly-too-short deck and one wrong row
// look identical in the total. Detecting each band separately distinguishes
// them, and is the only way to check a row-height claim against the photo.
let bands = [];
{
  const cut = Math.max(...rowCount.slice(yA, yB)) * 0.3;
  let start = -1;
  for (let y = yA; y < yB; y++) {
    const on = rowCount[y] >= cut;
    if (on && start < 0) start = y;
    if (!on && start >= 0) { bands.push([start, y - 1]); start = -1; }
  }
  if (start >= 0) bands.push([start, yB - 1]);

  // Key legends punch dark holes through a row and split it into slivers, so
  // merge anything separated by less than a cap gap, then drop the leftovers.
  const merged = [];
  for (const b of bands) {
    const last = merged[merged.length - 1];
    if (last && b[0] - last[1] <= R * 2.5) last[1] = b[1];
    else merged.push([...b]);
  }
  const heights = merged.map(([a, b]) => b - a).sort((p, q) => p - q);
  const median = heights[Math.floor(heights.length / 2)] || 1;
  bands = merged.filter(([a, b]) => b - a >= median * 0.5);
}

// PITCH is the honest vertical measure — the distance between successive row
// tops. A band's own height depends on the erosion radius and the threshold;
// the spacing between bands does not.
if (bands.length >= 2) {
  const starts = bands.map(([a]) => a);
  const gaps = starts.slice(1).map((v, i) => v - starts[i]);
  // Compare against the FULL-row pitch, taken as the median gap. Averaging
  // every gap folds a genuinely short row into the baseline and hides it.
  const sorted = [...gaps].sort((p, q) => p - q);
  const pitchY = sorted[Math.floor(sorted.length / 2)];
  gaps.forEach((g, i) => {
    const ratio = g / pitchY;
    if (Math.abs(ratio - 1) > 0.04) {
      console.log(`row h    ${rows[i]?.name ?? `row ${i}`} measures ${ratio.toFixed(3)}` +
        ` of a full row (${g}px vs ${pitchY}px) — layout says ${rows[i]?.h}`);
    }
  });
  const pitchSkew = ((unitX - pitchY) / ((unitX + pitchY) / 2)) * 100;
  console.log(`rows     ${bands.length} bands` +
    (bands.length === rows.length ? "" : `  (expected ${rows.length})`) +
    `  gaps ${gaps.join(", ")}`);
  console.log(`pitch    x ${unitX.toFixed(2)}px   y ${pitchY.toFixed(2)}px   ` +
    `skew ${pitchSkew >= 0 ? "+" : ""}${pitchSkew.toFixed(2)}%  ` + (
      Math.abs(pitchSkew) < 2.5
        ? "— keycaps are square and every row is the same height"
        : "— rows are genuinely not square; a row height is wrong"));
}

// The rect to actually USE is derived from pitch, not from raw extents.
// Extents are trimmed twice — once by the erosion radius and again by the
// profile threshold, which bites hardest on the outermost rows — so they
// under-report the deck. Pitch is measured from the spacing BETWEEN bands
// and is immune to both. Keycaps being square (verified above) means one
// pitch describes the whole deck.
let derived = null;
if (bands.length >= 2) {
  const starts = bands.map(([a]) => a);
  const g = starts.slice(1).map((v, i) => v - starts[i]).sort((p, q) => p - q);
  const pitch = g[Math.floor(g.length / 2)];
  const rowUnitsTotal = rows.reduce((s, r) => s + r.h, 0);
  // WIDTH from the raw extents, HEIGHT from pitch — they fail differently.
  // Every column spans the full deck height, so the column profile finds the
  // true left and right keycap edges; only erosion shrinks them, and that is
  // a known constant. The ROW profile is not symmetric: the function row is
  // shorter, so it contributes fewer cap pixels and falls under the cut,
  // which silently clips the deck's top. Pitch does not care.
  derived = {
    x: minX - R,
    y: starts[0] - R,
    w: w + R * 2,
    h: rowUnitsTotal * pitch,
  };
}

const f = (n, d) => (n / d).toFixed(4);
console.log(`photo    ${photo.split("/").pop()}  ${img.width}x${img.height}`);
console.log(`deck px  x=${minX} y=${minY} w=${w} h=${h}`);
console.log(`--rect   ${f(minX, img.width)},${f(minY, img.height)},${f(w, img.width)},${f(h, img.height)}`);
console.log(`unit     x ${unitX.toFixed(2)}px   y ${unitY.toFixed(2)}px`);
console.log(`skew     ${skew >= 0 ? "+" : ""}${skew.toFixed(2)}%  ` + (
  Math.abs(skew) < 2
    ? "— keycaps are square; layout row heights agree with the photo"
    : "— REAL: with a measured rect this is the layout, not the measurement"));

if (derived) {
  console.log(`\nUSE THIS  --rect ${f(derived.x, img.width)},${f(derived.y, img.height)},` +
    `${f(derived.w, img.width)},${f(derived.h, img.height)}`);
}
