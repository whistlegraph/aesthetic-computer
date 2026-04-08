// Line, 25.09.30 (Rewritten 25.04.07)
// Freehand line brush with per-gesture alpha.
//
// Usage:
//   line purple          → freehand 1px purple lines
//   line:3 purple        → freehand 3px purple lines
//   line:3 blue 128      → freehand 3px blue at 50% alpha

import { nopaint_generateColoredLabel } from "../systems/nopaint.mjs";

let colorParams, opaqueParams, strokeAlpha, thickness, randomColor, wasPainting;
let savedParams, savedHud, savedApi; // Saved for dynamic HUD/URL updates.
let colorIndex = -1; // -1 = user-specified or random, 0+ = palette index.
const points = [];
const segmentColors = [];
let strokeToBake = null;

const palette = [
  { name: "red",     rgb: [255, 0, 0] },
  { name: "orange",  rgb: [255, 165, 0] },
  { name: "yellow",  rgb: [255, 255, 0] },
  { name: "green",   rgb: [0, 128, 0] },
  { name: "cyan",    rgb: [0, 255, 255] },
  { name: "blue",    rgb: [0, 0, 255] },
  { name: "purple",  rgb: [128, 0, 128] },
  { name: "white",   rgb: [255, 255, 255] },
  { name: "gray",    rgb: [128, 128, 128] },
  { name: "black",   rgb: [0, 0, 0] },
];

const system = "nopaint";

function boot({ params, num, colon, hud, ...api }) {
  colorParams = num.parseColor(params);
  randomColor = colorParams.length === 0;
  savedParams = params;
  savedHud = hud;
  savedApi = api;

  // Extract alpha and create an opaque version for buffer rendering.
  if (!randomColor && (colorParams.length === 2 || colorParams.length === 4)) {
    strokeAlpha = colorParams[colorParams.length - 1] / 255;
    opaqueParams = colorParams.slice(0, -1);
    if (opaqueParams.length === 1) opaqueParams.push(255);
    else opaqueParams.push(255);
  } else {
    strokeAlpha = 1;
    opaqueParams = colorParams;
  }

  thickness = Math.max(1, Math.min(parseInt(colon[0]) || 1, 50));
  wasPainting = false;
  points.length = 0;
  segmentColors.length = 0;
  strokeToBake = null;

  updateLabel();
}

function act({ event: e, net, needsPaint, sound, system: { nopaint } }) {
  if (e.is("scroll")) {
    const delta = e.y > 0 ? -1 : e.y < 0 ? 1 : 0;
    if (delta === 0) return;
    const prev = thickness;
    thickness = Math.max(1, Math.min(thickness + delta, 50));
    if (thickness !== prev) {
      updateLabel();
      rewriteURL(net);
      sound.synth({
        tone: 200 + thickness * 40,
        beats: 0.04,
        attack: 0.01,
        decay: 0.1,
        volume: 0.1,
      });
      needsPaint();
    }
  }

  // Middle-click (button 1) cycles through palette colors.
  if (e.is("touch") && e.button === 1) {
    nopaint.cancelStroke?.(); // Prevent painting from middle-click.
    colorIndex = (colorIndex + 1) % palette.length;
    applyPaletteColor();
    rewriteURL(net);
    sound.synth({
      tone: 300 + colorIndex * 80,
      beats: 0.05,
      attack: 0.01,
      decay: 0.15,
      volume: 0.12,
    });
    needsPaint();
  }

  // [ and ] keys adjust opacity.
  if (e.is("keyboard:down:[") || e.is("keyboard:down:]")) {
    const step = 0.1;
    if (e.is("keyboard:down:]")) strokeAlpha = Math.min(1, strokeAlpha + step);
    if (e.is("keyboard:down:[")) strokeAlpha = Math.max(step, strokeAlpha - step);
    strokeAlpha = Math.round(strokeAlpha * 10) / 10;
    opaqueParams = randomColor ? [] : colorParams.slice(0, 3);
    if (!randomColor && opaqueParams.length > 0) opaqueParams.push(255);
    if (!randomColor) {
      const colorName = savedParams[0] || "";
      const alphaVal = Math.round(strokeAlpha * 255);
      savedParams = alphaVal < 255 ? [colorName, String(alphaVal)] : [colorName];
    }
    updateLabel();
    rewriteURL(net);
    sound.synth({
      tone: 400 + strokeAlpha * 600,
      beats: 0.04,
      attack: 0.01,
      decay: 0.1,
      volume: 0.1,
    });
    needsPaint();
  }
}

function applyPaletteColor() {
  const entry = palette[colorIndex];
  colorParams = entry.rgb.slice();
  opaqueParams = entry.rgb.slice();
  savedParams = [entry.name];
  randomColor = false;
  strokeAlpha = 1;
  updateLabel();
}

function rewriteURL(net) {
  const colonStr = thickness > 1 ? `:${thickness}` : "";
  const paramStr = savedParams.length > 0 ? " " + savedParams.join(" ") : "";
  net.rewrite(`/line${colonStr}${paramStr}`);
}

function updateLabel() {
  const modifiers = thickness > 1 ? `:${thickness}` : "";
  nopaint_generateColoredLabel("line", colorParams, savedParams, modifiers, { hud: savedHud, ...savedApi });
}

function paint({ ink, page, paste, pen, screen, num, system: { nopaint } }) {
  const isPainting = nopaint.is("painting");
  if (isPainting && !wasPainting) {
    points.length = 0;
    segmentColors.length = 0;
  }
  wasPainting = isPainting;

  if (isPainting) {
    const bx = nopaint.brush.x, by = nopaint.brush.y;
    const last = points[points.length - 1];
    if (!last || last.x !== bx || last.y !== by) {
      points.push({ x: bx, y: by });
      if (randomColor && points.length > 1) {
        segmentColors.push(num.randIntArr(255, 3));
      }
    }
  }

  if (points.length > 0) {
    const stroke = isPainting
      ? [...points, { x: nopaint.brush.x, y: nopaint.brush.y }]
      : points;

    page(nopaint.buffer).wipe(255, 0);

    if (randomColor) {
      if (thickness === 1) {
        for (let i = 0; i < stroke.length - 1; i++) {
          ink(segmentColors[i] || num.randIntArr(255, 3))
            .line(stroke[i].x, stroke[i].y, stroke[i + 1].x, stroke[i + 1].y);
        }
      } else {
        const smoothed = stroke.length > 2 ? smoothStroke(stroke) : stroke;
        for (let i = 0; i < smoothed.length - 1; i++) {
          ink(segmentColors[i] || num.randIntArr(255, 3))
            .line(smoothed[i].x, smoothed[i].y, smoothed[i + 1].x, smoothed[i + 1].y, thickness);
        }
      }
    } else {
      // Draw fully opaque — alpha is applied per-gesture below.
      if (thickness === 1) {
        ink(opaqueParams).pppline(stroke);
      } else {
        const smoothed = stroke.length > 2 ? smoothStroke(stroke) : stroke;
        for (let i = 0; i < smoothed.length - 1; i++) {
          const a = smoothed[i], b = smoothed[i + 1];
          ink(opaqueParams).line(a.x, a.y, b.x, b.y, thickness);
        }
      }
    }

    page(screen);

    strokeToBake = () => {
      // Scale buffer alpha once at bake time for per-gesture transparency.
      if (strokeAlpha < 1) {
        const px = nopaint.buffer.pixels;
        for (let i = 3; i < px.length; i += 4) {
          if (px[i] > 0) px[i] = (px[i] * strokeAlpha + 0.5) | 0;
        }
      }
      paste(nopaint.buffer);
      page(nopaint.buffer).wipe(255, 0);
      points.length = 0;
      segmentColors.length = 0;
      strokeToBake = null;
    };
  }

  // Brush preview circle on hover (when not painting and thickness > 2).
  if (!isPainting && thickness > 2 && pen) {
    const r = Math.floor((thickness - 1) / 2);
    const a = Math.round(strokeAlpha * 100);
    if (randomColor) {
      ink(255, 255, 255, a).circle(pen.x, pen.y, r, true);
    } else {
      ink(...opaqueParams.slice(0, 3), a).circle(pen.x, pen.y, r, true);
    }
    return true;
  }
}

function bake() {
  strokeToBake?.();
}

export { boot, act, paint, bake, system };

// ── Helpers ──

function smoothStroke(raw) {
  if (raw.length < 3) return raw;
  const out = [raw[0]];
  for (let i = 1; i < raw.length - 1; i++) {
    const p = raw[i - 1], c = raw[i], n = raw[i + 1];
    const s = 0.25;
    out.push({ x: c.x * (1 - s) + (p.x + n.x) * s / 2, y: c.y * (1 - s) + (p.y + n.y) * s / 2 });
  }
  out.push(raw[raw.length - 1]);
  return out;
}
