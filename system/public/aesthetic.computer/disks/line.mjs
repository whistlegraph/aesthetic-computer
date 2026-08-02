// Line, 25.09.30 (Rewritten 25.04.07)
// Freehand line brush with per-gesture alpha.
//
// Usage:
//   line purple          → freehand 1px purple lines
//   line:3 purple        → freehand 3px purple lines
//   line:3 blue 128      → freehand 3px blue at 50% alpha

import { nopaint_generateColoredLabel } from "../systems/nopaint.mjs";
import { isFadeColor } from "../lib/num.mjs";

let colorParams, opaqueParams, strokeAlpha, thickness, randomColor, wasPainting;
let fadeColor = null; // Fade color object when using fade: syntax.
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
  randomColor = !colorParams || (Array.isArray(colorParams) && colorParams.length === 0);
  savedParams = params;
  savedHud = hud;
  savedApi = api;
  fadeColor = null;

  // 🌈 Fade color support — parseColor returns a fade object for "fade:..." params.
  if (isFadeColor(colorParams)) {
    fadeColor = colorParams;
    strokeAlpha = (fadeColor.alpha ?? 255) / 255;
    opaqueParams = fadeColor.fadeString;
  } else if (!randomColor && (colorParams.length === 2 || colorParams.length === 4)) {
    // Extract alpha and create an opaque version for buffer rendering.
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

// 🔔 Pleasant sine-wave chirp with value-based pitch and brightness.
// Maps a normalized value [0..1] to a musical pitch within a pentatonic scale
// so sequential tweaks feel tonal rather than atonal.
function chirp(sound, normalizedValue, { bright = 1 } = {}) {
  if (!sound?.synth) return;
  // Major pentatonic scale (C D E G A), two octaves.
  const scale = [0, 2, 4, 7, 9, 12, 14, 16, 19, 21, 24];
  const idx = Math.max(0, Math.min(scale.length - 1, Math.round(normalizedValue * (scale.length - 1))));
  const semitone = scale[idx];
  // Base ~C4 (261.63) then scale up with value.
  const tone = 261.63 * Math.pow(2, semitone / 12);
  sound.synth({
    type: "sine",
    tone,
    beats: 0.12,
    attack: 0.008,
    decay: 0.92,
    volume: 0.09 * bright,
  });
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
      // Pitch = thickness (thicker → higher). Brightness falls off at extremes.
      chirp(sound, (thickness - 1) / 49, { bright: 1 });
      needsPaint();
    }
  }

  // Middle-click (button 1) cycles through palette colors.
  if (e.is("touch") && e.button === 1) {
    nopaint.cancelStroke?.(); // Prevent painting from middle-click.
    colorIndex = (colorIndex + 1) % palette.length;
    applyPaletteColor();
    rewriteURL(net);
    // Pitch by palette index (follows the scale).
    chirp(sound, colorIndex / (palette.length - 1), { bright: 1.1 });
    needsPaint();
  }

  // [ and ] keys adjust opacity.
  if (e.is("keyboard:down:[") || e.is("keyboard:down:]")) {
    const step = 0.1;
    if (e.is("keyboard:down:]")) strokeAlpha = Math.min(1, strokeAlpha + step);
    if (e.is("keyboard:down:[")) strokeAlpha = Math.max(step, strokeAlpha - step);
    strokeAlpha = Math.round(strokeAlpha * 10) / 10;
    if (!isFadeColor(colorParams)) {
      opaqueParams = randomColor ? [] : colorParams.slice(0, 3);
      if (!randomColor && opaqueParams.length > 0) opaqueParams.push(255);
      if (!randomColor) {
        const colorName = savedParams[0] || "";
        const alphaVal = Math.round(strokeAlpha * 255);
        savedParams = alphaVal < 255 ? [colorName, String(alphaVal)] : [colorName];
      }
    }
    updateLabel();
    rewriteURL(net);
    // Pitch = alpha; brightness = alpha (so quieter lines sound softer).
    chirp(sound, strokeAlpha, { bright: 0.5 + strokeAlpha * 0.7 });
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

function paint({ ink, page, paste, pen, screen, setBufferAlpha, num, system: { nopaint } }) {
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
      // Draw fully opaque to the buffer.
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

    // Defer buffer alpha scaling so it runs AFTER the deferred drawing
    // commands during the layer flush. Modifying buffer pixels directly
    // here would happen before the wipe/draw commands actually execute.
    if (strokeAlpha < 1) {
      setBufferAlpha(nopaint.buffer, strokeAlpha);
    }

    page(screen);

    const bakeAlpha = strokeAlpha;
    strokeToBake = () => {
      // Sync alpha scaling on the buffer pixels — safe because this runs
      // in the bake block AFTER the previous frame's layer was flushed,
      // so the buffer has the drawn (opaque) stroke content.
      if (bakeAlpha < 1) {
        const target = Math.round(bakeAlpha * 255);
        const px = nopaint.buffer.pixels;
        for (let i = 3; i < px.length; i += 4) {
          if (px[i] > 0) px[i] = target;
        }
      }
      paste(nopaint.buffer);
      page(nopaint.buffer).wipe(255, 0);
      points.length = 0;
      segmentColors.length = 0;
      strokeToBake = null;
    };
  }

  // Brush preview circle on hover (when not painting/panning and thickness > 2).
  if (!isPainting && !nopaint.is("panning") && thickness > 2 && pen) {
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

// Machine-readable No Paint proposal contract. This lives beside the real
// brush so the surfer does not maintain a second imitation of Line.
const nopaintProposal = Object.freeze({
  version: 1,
  slug: "line",
  label: "Line",
  compatible: true,
  parameters: Object.freeze({
    color: Object.freeze({ type: "rgba", alpha: [24, 192] }),
    thickness: Object.freeze({ type: "integer", min: 1, max: 50 }),
    points: Object.freeze({ type: "path", min: 4, max: 18 }),
    durationFrames: Object.freeze({ type: "integer", min: 24, max: 120 }),
  }),
  generate({ random, width, height, base }) {
    const pointCount = 4 + Math.floor(random() * 15);
    const start = { x: base.x, y: base.y };
    const end = {
      x: Math.min(width - 1, base.x + base.w),
      y: Math.min(height - 1, base.y + base.h),
    };
    const points = Array.from({ length: pointCount }, (_, index) => {
      const t = pointCount === 1 ? 0 : index / (pointCount - 1);
      const sway = Math.sin(t * Math.PI * (1 + Math.floor(random() * 4))) * base.drift;
      return Object.freeze({
        x: Math.max(0, Math.min(width - 1,
          Math.round(start.x + (end.x - start.x) * t + sway))),
        y: Math.max(0, Math.min(height - 1,
          Math.round(start.y + (end.y - start.y) * t - sway * 0.65))),
      });
    });
    const thickness = 1 + Math.floor(random() * 50);
    const alpha = 24 + Math.floor(random() * 169);
    const color = Object.freeze([base.color[0], base.color[1], base.color[2], alpha]);
    const durationFrames = 24 + Math.floor(random() * 97);
    return Object.freeze({
      ...base,
      color,
      thickness,
      points: Object.freeze(points),
      brush: Object.freeze({
        slug: "line",
        params: Object.freeze(color.map(String)),
        colon: Object.freeze([String(thickness)]),
        parameters: Object.freeze({ thickness, alpha, pointCount, durationFrames }),
      }),
    });
  },
  render({ ink }, score, frame) {
    const duration = score.brush.parameters.durationFrames;
    const visible = Math.max(2, Math.ceil(score.points.length * Math.min(1, frame / duration)));
    for (let index = 1; index < visible; index += 1) {
      const from = score.points[index - 1];
      const to = score.points[index];
      ink(score.color).line(from.x, from.y, to.x, to.y, score.thickness);
    }
  },
});

function meta() {
  return {
    title: "Line",
    desc: "Freehand line brush with per-gesture alpha.",
    controls: "drag to draw a continuous freehand line; release to bake the stroke",
    keys: "[ / ] decrease/increase opacity; scroll to change thickness; middle-click cycles palette colors",
    params: "color (e.g. red, purple, fade:red-blue, or r,g,b[,a]); :n for thickness (e.g. line:3)",
    example: "line:3 blue 128",
  };
}

export { boot, act, paint, bake, meta, nopaintProposal, system };

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
