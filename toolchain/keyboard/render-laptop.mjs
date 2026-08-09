#!/usr/bin/env node
// The canonical MacBook Neo, drawn flat-lay from directly above.
//
//   node toolchain/keyboard/render-laptop.mjs out.png --colorway citrus
//   node toolchain/keyboard/render-laptop.mjs sheet.png --sheet
//
// This is the accuracy reference the campaign art gets checked against.
// Everything here is deterministic on purpose: a diffusion model will not
// reliably place twelve specific keycaps, which is exactly how `i` ended up
// coloured and `u` ended up missing in the generated block. Here the
// semitones come off the same map the software plays from, so they cannot
// drift.
//
// Proportions are taken from the real machine where they could be measured:
// neo reports Mac17,5, Apple Internal Keyboard, CountryCode 0 (ANSI), and a
// 2408x1506 built-in panel — exactly 16:10. Body-to-keyboard ratios follow
// the 14" chassis. Nothing invented is load-bearing.

import { createCanvas } from "canvas";
import { writeFileSync } from "node:fs";
import { layout, ROW_UNITS } from "./macbook-neo-layout.mjs";
import { drawCapFace } from "./cap-faces.mjs";

// Semitone keycaps, derived from labelByMidiNotepat in
// slab/menuband/Sources/MenuBand/KeyboardIconRenderer.swift by the piano
// rule (midi % 12 ∈ {1,3,6,8,10}). Twelve, not eleven — the campaign brief
// omits the apostrophe. E and I are naturals and must stay unpainted.
export const SEMITONE_CAPS = ["z", "v", "s", "w", "r", "q", "t", "y", "u", "o", "p", "'"];

// Body colour and note-key colour are contrasting partners, per the rule in
// slab/menuband/marketing/notepat-launch/.
// Apple's own names, verified against apple.com August 2026. MacBook Neo
// shipped March 2026 in blush / indigo / silver / citrus — there is no
// "blueberry" and no "rose". `body` is measured from a patch inside the lid
// of each Apple Newsroom press photo; `cap` is the colour-matched Magic
// Keyboard, which Apple describes as the body colour "in lighter shades" —
// pale near-white tinted toward the body, never darker than it.
//
// The citrus test is R ≈ G. Real citrus measures R216 G214: a true yellow.
// Any value where green leads red has drifted to chartreuse.
const COLORWAYS = {
  citrus: { body: "#d8d680", trim: "#c2c06d", accent: "#5b4bc4", cap: "#f4f3dc", screen: "#15161c" },
  indigo: { body: "#495369", trim: "#3b4456", accent: "#c9cf5a", cap: "#e2e5ec", screen: "#15161c" },
  blush:  { body: "#dfc7c7", trim: "#c9b0b0", accent: "#1d6f6b", cap: "#f8efef", screen: "#15161c" },
};

// Retired names, kept so older callers do not simply throw.
const COLORWAY_ALIASES = { blueberry: "indigo", rose: "blush" };

/// Pick legible ink for a given cap fill rather than assuming a dark cap.
/// Apple prints legends in a medium grey, not black — near-black reads as a
/// cheap keyboard even when the glyphs and weight are right.
function inkFor(hex) {
  const [r, g, b] = [1, 3, 5].map((i) => parseInt(hex.slice(i, i + 2), 16));
  // Rec. 601 luma is good enough to choose between two inks.
  return (r * 299 + g * 587 + b * 114) / 1000 > 140 ? "#6e6e73" : "#f4f3fa";
}

// TrackDrum zones, outside to centre, straight from TrackDrumIcon.swift.
// Order matters and has drifted in generated art before: hi-hat, snare,
// tom, kick. No touch dots — the live-contact layer is deliberately absent
// until the trackpad plugin actually drives it.
const DRUM_ZONES = [
  { name: "hi-hat", inset: 0.00, fill: "#8f9c6b" },
  { name: "snare",  inset: 0.13, fill: "#b5674a", wires: true },
  { name: "tom",    inset: 0.28, fill: "#c9913f" },
  { name: "kick",   inset: 0.44, fill: "#4a3428" },
];

const out = process.argv[2] ?? "macbook-neo-citrus.png";
const sheet = process.argv.includes("--sheet");
const boxes = process.argv.includes("--boxes");
const cwFlag = process.argv.indexOf("--colorway");
const colorway = cwFlag > -1 ? process.argv[cwFlag + 1] : "citrus";

function roundRect(ctx, x, y, w, h, r) {
  const rr = Math.min(r, w / 2, h / 2);
  ctx.beginPath();
  ctx.moveTo(x + rr, y);
  ctx.arcTo(x + w, y, x + w, y + h, rr);
  ctx.arcTo(x + w, y + h, x, y + h, rr);
  ctx.arcTo(x, y + h, x, y, rr);
  ctx.arcTo(x, y, x + w, y, rr);
  ctx.closePath();
}

/// Draw one open laptop into `ctx` with its top-left at (ox, oy).
/// Returns the drawn size so the contact sheet can pack them.
function drawLaptop(ctx, ox, oy, bodyW, cw) {
  const resolved = COLORWAY_ALIASES[cw] ?? cw;
  const pal = COLORWAYS[resolved];
  if (!pal) throw new Error(`unknown colorway ${cw} — have ${Object.keys(COLORWAYS).join(", ")}`);

  // Keyboard spans ~79% of the body width on a 14" chassis; body depth is
  // ~70.8% of its width, and the lid shares the base's footprint.
  const unit = (bodyW * 0.79) / ROW_UNITS;
  const baseH = bodyW * 0.708;
  const lidH = baseH;
  const hinge = bodyW * 0.012;
  const corner = bodyW * 0.028;

  // --- Lid -------------------------------------------------------------
  roundRect(ctx, ox, oy, bodyW, lidH, corner);
  ctx.fillStyle = pal.body; ctx.fill();
  ctx.strokeStyle = pal.trim; ctx.lineWidth = bodyW * 0.004; ctx.stroke();

  const bezel = bodyW * 0.022;
  const screenW = bodyW - bezel * 2;
  const screenH = screenW / 1.599;               // 2408x1506, measured
  const screenY = oy + (lidH - screenH) / 2;
  roundRect(ctx, ox + bezel, screenY, screenW, screenH, corner * 0.5);
  ctx.fillStyle = pal.screen; ctx.fill();

  // Menu bar: the instrument lives here, so it is the one screen element
  // worth drawing. A lit keyboard strip beside the right-hand status items.
  const barH = screenH * 0.052;
  ctx.fillStyle = "rgba(255,255,255,0.07)";
  ctx.fillRect(ox + bezel, screenY, screenW, barH);
  const stripW = barH * 3.6, stripX = ox + bezel + screenW - stripW - barH * 4.2;
  const stripY = screenY + barH * 0.22, stripH = barH * 0.56;
  ctx.fillStyle = "rgba(255,255,255,0.55)";
  ctx.fillRect(stripX, stripY, stripW, stripH);
  for (let i = 0; i < 11; i++) {                 // mini keys, three lit
    const kx = stripX + (stripW / 11) * i;
    ctx.fillStyle = [2, 5, 8].includes(i) ? pal.accent : "rgba(20,20,26,0.55)";
    ctx.fillRect(kx + stripW / 60, stripY, stripW / 22, stripH);
  }

  // --- Base ------------------------------------------------------------
  const baseY = oy + lidH + hinge;
  roundRect(ctx, ox, baseY, bodyW, baseH, corner);
  ctx.fillStyle = pal.body; ctx.fill();
  ctx.strokeStyle = pal.trim; ctx.lineWidth = bodyW * 0.004; ctx.stroke();

  // Keyboard well, centred horizontally.
  const gap = Math.max(1, unit * 0.09);
  const { caps, width: deckW, height: deckH } = layout({ unit, gap, pad: unit * 0.22 });
  const deckX = ox + (bodyW - deckW) / 2;
  const deckY = baseY + baseH * 0.055;
  roundRect(ctx, deckX, deckY, deckW, deckH, unit * 0.14);
  ctx.fillStyle = pal.trim; ctx.fill();

  const semis = new Set(SEMITONE_CAPS);
  const radius = unit * 0.13;

  /// One physical keycap: rounded rect, fill, and its printed face.
  function drawCap(x, y, w, h, label, { fill, style = "letter" } = {}) {
    roundRect(ctx, x, y, w, h, Math.min(radius, h / 2));
    ctx.fillStyle = fill;
    ctx.fill();
    if (!label) return;
    const ink = inkFor(fill);
    // Arrows are drawn by the caller as four sub-caps and carry a bare glyph.
    if (style === "arrow") {
      ctx.fillStyle = ink;
      ctx.font = `${Math.max(4, Math.round(unit * 0.26))}px sans-serif`;
      ctx.textAlign = "center"; ctx.textBaseline = "middle";
      ctx.fillText(label, x + w / 2, y + h / 2);
      return;
    }
    drawCapFace(ctx, { x, y, w, h, label, unit, ink, boxes });
  }

  for (const cap of caps) {
    const x = deckX + cap.x, y = deckY + cap.y;

    // The arrow cluster is one 3u block in the layout, but physically it is
    // four caps in an inverted T. Measured off the top-down product photo:
    // the MIDDLE column is full height, split into half-height up and down;
    // LEFT and RIGHT are HALF height and sit on the BOTTOM, aligned with the
    // down arrow. The two corners above left and right are empty deck.
    if (cap.style === "arrows") {
      const col = cap.w / 3, half = (cap.h - gap) / 2, w = col - gap / 2;
      const bottomY = y + half + gap / 2;
      drawCap(x, bottomY, w, half, "◀", { fill: pal.cap, style: "arrow" });
      drawCap(x + col, y, w, half, "▲", { fill: pal.cap, style: "arrow" });
      drawCap(x + col, bottomY, w, half, "▼", { fill: pal.cap, style: "arrow" });
      drawCap(x + col * 2, bottomY, w, half, "▶", { fill: pal.cap, style: "arrow" });
      continue;
    }

    const isSemitone = semis.has(cap.label);
    const fill = isSemitone ? pal.accent : pal.cap;
    // The spacebar carries no legend on a real machine.
    drawCap(x, y, cap.w, cap.h, cap.style === "space" ? null : cap.label, { fill });
  }

  // --- TrackDrum -------------------------------------------------------
  const padW = bodyW * 0.416, padH = baseH * 0.362;
  const padX = ox + (bodyW - padW) / 2;
  const padY = deckY + deckH + (baseY + baseH - (deckY + deckH) - padH) / 2;
  for (const zone of DRUM_ZONES) {
    const ix = padW * zone.inset * 0.5, iy = padH * zone.inset * 0.5;
    const zx = padX + ix, zy = padY + iy;
    const zw = padW - ix * 2, zh = padH - iy * 2;
    roundRect(ctx, zx, zy, zw, zh, padW * 0.05);
    ctx.fillStyle = zone.fill; ctx.fill();
    if (!zone.wires) continue;
    ctx.save();                                  // snare wires, part of the drum
    roundRect(ctx, zx, zy, zw, zh, padW * 0.05);
    ctx.clip();
    ctx.strokeStyle = "rgba(0,0,0,0.16)";
    ctx.lineWidth = Math.max(1, padW * 0.006);
    for (let x = zx - zh; x < zx + zw; x += padW * 0.045) {
      ctx.beginPath(); ctx.moveTo(x, zy + zh); ctx.lineTo(x + zh, zy); ctx.stroke();
    }
    ctx.restore();
  }

  return { w: bodyW, h: lidH + hinge + baseH };
}

const MARGIN = 70;
const GROUND = "#f4f1e8";                        // the campaign's paper ground

if (sheet) {
  const bodyW = 900, cols = Object.keys(COLORWAYS);
  const probe = createCanvas(10, 10).getContext("2d");
  const one = drawLaptop(probe, 0, 0, bodyW, "citrus");
  const W = MARGIN + cols.length * (bodyW + MARGIN);
  const H = MARGIN + one.h + MARGIN + 46;
  const canvas = createCanvas(Math.ceil(W), Math.ceil(H));
  const ctx = canvas.getContext("2d");
  ctx.fillStyle = GROUND; ctx.fillRect(0, 0, W, H);
  cols.forEach((cw, i) => {
    const x = MARGIN + i * (bodyW + MARGIN);
    drawLaptop(ctx, x, MARGIN, bodyW, cw);
    ctx.fillStyle = "#4a4740";
    ctx.font = "30px sans-serif"; ctx.textAlign = "center"; ctx.textBaseline = "alphabetic";
    ctx.fillText(cw, x + bodyW / 2, MARGIN + one.h + 44);
  });
  writeFileSync(out, canvas.toBuffer("image/png"));
  console.log(`wrote ${out} — contact sheet, ${cols.length} colorways, ${canvas.width}x${canvas.height}px`);
} else {
  const bodyW = 1400;
  const probe = createCanvas(10, 10).getContext("2d");
  const one = drawLaptop(probe, 0, 0, bodyW, colorway);
  const W = bodyW + MARGIN * 2, H = one.h + MARGIN * 2 + 40;
  const canvas = createCanvas(Math.ceil(W), Math.ceil(H));
  const ctx = canvas.getContext("2d");
  ctx.fillStyle = GROUND; ctx.fillRect(0, 0, W, H);
  drawLaptop(ctx, MARGIN, MARGIN, bodyW, colorway);
  ctx.fillStyle = "#4a4740";
  ctx.font = "26px sans-serif"; ctx.textAlign = "left"; ctx.textBaseline = "alphabetic";
  ctx.fillText(
    `MacBook Neo (Mac17,5) — ${colorway} — ANSI US, 75 caps — ` +
    `${SEMITONE_CAPS.length} notepat semitones: ${SEMITONE_CAPS.join(" ").toUpperCase()}`,
    MARGIN, H - MARGIN * 0.55);
  writeFileSync(out, canvas.toBuffer("image/png"));
  console.log(`wrote ${out} — ${colorway}, ${canvas.width}x${canvas.height}px`);
}
