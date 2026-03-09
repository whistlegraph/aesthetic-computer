// roz.mjs — Native RBP-26 reference piece
// Executes the canonical $roz source behavior with native primitives/effects.

const ROZ_SOURCE = `fade:red-blue-black-blue-red
ink (? rainbow white 0) (1s... 24 64)
line w/2 0 w/2 h
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
ink (? cyan yellow magenta) 8
circle w/2 h/2 (? 2 4 8)`;

const COLOR = {
  red: [255, 40, 40],
  blue: [40, 80, 255],
  black: [0, 0, 0],
  white: [255, 255, 255],
  cyan: [40, 255, 255],
  yellow: [255, 230, 60],
  magenta: [255, 60, 255],
};

let startedAt = 0;
let frame = 0;
let contrastTriggered = false;
let seed = 0x26_52_4f_5a;

function mulberry32() {
  seed = (seed + 0x6d2b79f5) | 0;
  let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
  t ^= t + Math.imul(t ^ (t >>> 7), 61 | t);
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
}

function choose(...values) {
  return values[Math.floor(mulberry32() * values.length)];
}

function timedRepeater(seconds, ...values) {
  if (values.length === 0) return 0;
  const elapsedMs = performance.now() - startedAt;
  const idx = Math.floor(elapsedMs / (seconds * 1000)) % values.length;
  return values[idx];
}

function hsvToRgb(h) {
  const i = Math.floor(h * 6);
  const f = h * 6 - i;
  const q = 1 - f;
  const mod = i % 6;
  let r = 0, g = 0, b = 0;
  if (mod === 0) { r = 1; g = f; b = 0; }
  if (mod === 1) { r = q; g = 1; b = 0; }
  if (mod === 2) { r = 0; g = 1; b = f; }
  if (mod === 3) { r = 0; g = q; b = 1; }
  if (mod === 4) { r = f; g = 0; b = 1; }
  if (mod === 5) { r = 1; g = 0; b = q; }
  return [Math.floor(r * 255), Math.floor(g * 255), Math.floor(b * 255)];
}

function asColorToken(token) {
  if (token === "rainbow") {
    const h = ((performance.now() - startedAt) / 4000) % 1;
    return hsvToRgb(h);
  }
  if (typeof token === "number") {
    return [token, token, token];
  }
  return COLOR[token] || [255, 255, 255];
}

function lerp(a, b, t) {
  return Math.round(a + (b - a) * t);
}

function fadeColorAt(t, stops) {
  if (stops.length === 0) return [0, 0, 0];
  if (stops.length === 1) return stops[0];
  const scaled = t * (stops.length - 1);
  const i = Math.floor(scaled);
  const j = Math.min(stops.length - 1, i + 1);
  const f = scaled - i;
  return [
    lerp(stops[i][0], stops[j][0], f),
    lerp(stops[i][1], stops[j][1], f),
    lerp(stops[i][2], stops[j][2], f),
  ];
}

function drawFadeBackground({ ink, line, screen }) {
  // fade:red-blue-black-blue-red
  const stops = [
    COLOR.red,
    COLOR.blue,
    COLOR.black,
    COLOR.blue,
    COLOR.red,
  ];
  const h = Math.max(1, screen.height - 1);
  for (let y = 0; y < screen.height; y++) {
    const c = fadeColorAt(y / h, stops);
    ink(c[0], c[1], c[2], 255);
    line(0, y, screen.width - 1, y);
  }
}

function boot({ wipe, system }) {
  wipe(0, 0, 0);
  startedAt = performance.now();
  frame = 0;
  contrastTriggered = false;
  seed = 0x26_52_4f_5a;
  // Keep native presentation crisp and consistent.
  system?.fps?.(60);
}

function act({ event, system }) {
  if (event.is("keyboard:down:escape")) {
    system?.jump?.("prompt");
  }
}

function sim() {}

function paint({ wipe, ink, line, circle, scroll, spin, zoom, contrast, write, screen }) {
  frame++;
  wipe(0, 0, 0);

  // Line 1: fade:red-blue-black-blue-red
  drawFadeBackground({ ink, line, screen });

  // Line 2 + 3:
  // ink (? rainbow white 0) (1s... 24 64)
  // line w/2 0 w/2 h
  const lineColorToken = choose("rainbow", "white", 0);
  const lineAlpha = timedRepeater(1, 24, 64);
  const lc = asColorToken(lineColorToken);
  ink(lc[0], lc[1], lc[2], lineAlpha);
  line(Math.floor(screen.width / 2), 0, Math.floor(screen.width / 2), screen.height - 1);

  // Line 4: (spin (2s... -1.125 1.125)) (zoom 1.1)
  const spinAngle = timedRepeater(2, -1.125, 1.125);
  spin?.(spinAngle);
  zoom?.(1.1);

  // Line 5: (0.5s (contrast 1.05))
  if (!contrastTriggered && performance.now() - startedAt >= 500) {
    contrast?.(1.05);
    contrastTriggered = true;
  }

  // Line 6: (scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
  const sx = choose(-0.1, 0, 0.1);
  const sy = choose(-0.1, 0, 0.1);
  scroll?.(sx < 0 ? -1 : sx > 0 ? 1 : 0, sy < 0 ? -1 : sy > 0 ? 1 : 0);

  // Line 7 + 8:
  // ink (? cyan yellow magenta) 8
  // circle w/2 h/2 (? 2 4 8)
  const circleColorToken = choose("cyan", "yellow", "magenta");
  const cc = asColorToken(circleColorToken);
  ink(cc[0], cc[1], cc[2], 8);
  const radius = choose(2, 4, 8);
  circle(Math.floor(screen.width / 2), Math.floor(screen.height / 2), radius, true);

  // Tiny label to confirm this is the native RBP-26 implementation.
  ink(255, 255, 255, 70);
  write?.("$roz native", { x: 2, y: 2, size: 1, font: "6x10" });
}

export { boot, act, sim, paint, ROZ_SOURCE };
