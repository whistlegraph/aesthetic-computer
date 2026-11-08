// Object viewer: a turntable for KidLisp written in the 3D object dialect
// (the dialect dool's objects use). `isObjectLisp` decides from the calls a
// program makes, or a `; #3d` pragma; `objectPiece` returns a piece that
// draws it with a small z-buffered rasterizer straight into screen.pixels.
// Drag to orbit.

import { parseSource, sourceFrame, sourceTones, sourceHair, beatAt } from "./object-language.mjs";

const SHAPES = /\((?:slab|orb|capsule)\s/;

export function isObjectLisp(source) {
  if (typeof source !== "string") return false;
  if (/^\s*;\s*#3d\b/m.test(source)) return true;
  return SHAPES.test(source.replace(/;[^\n]*/g, ""));
}

const DEFAULT_BG = [20, 23, 32];
// GIF delays are exact centiseconds at 25 fps; 100 poses make a four-second loop.
export const OBJECT_PREVIEW_FPS = 25;
export const OBJECT_TURN_SECONDS = 4;

// A `; #bg r g b` pragma gives the object its own ground (a minted object
// carries the colour its ink asked for).
export function sourceBackground(source) {
  const m = typeof source === "string" && source.match(/^\s*;\s*#bg\s+(\d{1,3})\s+(\d{1,3})\s+(\d{1,3})\b/m);
  if (!m) return null;
  return [m[1], m[2], m[3]].map((c) => Math.min(255, Number(c)));
}

// A `; #door https://…` pragma is the object's one doorway: a tap (a press
// that never dragged) opens it. Inside a market's iframe the page may not be
// allowed to open anything, so the address is also written on the ground for
// a few seconds after the tap.
export function sourceDoor(source) {
  const m = typeof source === "string" && source.match(/^\s*;\s*#door\s+(https?:\/\/\S+)/m);
  return m ? m[1] : null;
}
// One preference per AC runtime. Preparing a neighbor never starts its audio.
let soundEnabled = false;
const SPEAKER = [
  "...#.........", "..##....#....", ".###.....#...", "####..#...#..",
  "####..#...#..", "####..#...#..", ".###.....#...", "..##....#....", "...#.........",
];
function speaker(screen, enabled) {
  const x0 = screen.width - 22, y0 = 11;
  const color = enabled ? [230, 232, 240] : [138, 143, 158];
  const dot = (x, y) => {
    if (x < 0 || x >= screen.width || y < 0 || y >= screen.height) return;
    screen.pixels.set([...color, 255], (y * screen.width + x) * 4);
  };
  SPEAKER.forEach((row, y) => [...row].forEach((c, x) => {
    if (c === "#" && (enabled || x < 5)) dot(x0 + x, y0 + y);
  }));
  if (!enabled) for (let i = 0; i < 5; i++) {
    dot(x0 + 7 + i, y0 + 2 + i); dot(x0 + 11 - i, y0 + 2 + i);
  }
}

function extent(v) {
  let x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity, z0 = Infinity, z1 = -Infinity;
  for (let i = 0; i < v.length; i += 6) {
    x0 = Math.min(x0, v[i]); x1 = Math.max(x1, v[i]);
    y0 = Math.min(y0, v[i + 1]); y1 = Math.max(y1, v[i + 1]);
    z0 = Math.min(z0, v[i + 2]); z1 = Math.max(z1, v[i + 2]);
  }
  return {
    center: { x: (x0 + x1) / 2, y: (y0 + y1) / 2, z: (z0 + z1) / 2 },
    radius: Math.hypot(x1 - x0, y1 - y0, z1 - z0) / 2,
  };
}

// options: { yaw, pitch, still } fixes the view (thumbnails); still stops the spin.
export function objectPiece(source, label = "", options = {}) {
  let shape, error;
  // By default the camera looks down on the object from above, about 55°
  // (2026-11-03). In this projection a NEGATIVE pitch is above: positive
  // looks up from underneath.
  let yaw = options.yaw ?? 0.6, pitch = options.pitch ?? -0.95, dragging = !!options.still;
  let depth, size = [0, 0];
  let preview = options.preview === true;
  let center = { x: 0, y: 0.5, z: 0 }, radius = 1;
  const BG = sourceBackground(source) || DEFAULT_BG;
  const door = sourceDoor(source);
  let press = null; // { x, y, at } while a touch may still be a tap
  let doorShownAt = 0;
  const start = Date.now();
  let lastPaint = start;
  // The object's (tone ...) oscillators, enabled by its canvas speaker control.
  let tones = [], voices = [];
  let soundPress = false;

  try {
    shape = parseSource(source);
    const b = extent(sourceFrame(shape, 0).vertices);
    center = b.center;
    radius = Math.max(0.4, b.radius);
    tones = sourceTones(shape);
  } catch (e) {
    error = String(e.message || e).slice(0, 80);
  }

  function boot({ hud, clock, query }) {
    preview ||= query?.objectPreview === "1";
    if (preview) pitch = -0.22;
    clock?.resync?.();
    if (label) hud?.label?.(label, [230, 232, 240]);
  }

  function paint({ ink, screen }) {
    const { width: w, height: h, pixels } = screen;
    // Clear and draw everything here, in order, so nothing else overwrites it.
    for (let i = 0; i < w * h; i++) {
      pixels[i * 4] = BG[0]; pixels[i * 4 + 1] = BG[1]; pixels[i * 4 + 2] = BG[2]; pixels[i * 4 + 3] = 255;
    }
    if (!shape) {
      ink(255, 120, 120).write(error || "not an object", { x: 6, y: h - 16 });
      return;
    }
    if (size[0] !== w || size[1] !== h) {
      depth = new Float32Array(w * h);
      size = [w, h];
    }
    depth.fill(Infinity);
    const nowMs = Date.now();
    const t = options.time ?? (nowMs - start) / 1000;
    // Integrate wall time so 30/60/120 Hz displays turn at the same speed.
    // Cap a resumed tab's first step; dragging keeps its current angle.
    const elapsed = Math.min(0.1, Math.max(0, (nowMs - lastPaint) / 1000));
    lastPaint = nowMs;
    if (preview) yaw = (options.yaw ?? 0.6) + t * Math.PI * 2 / OBJECT_TURN_SECONDS;
    else if (!dragging) yaw += elapsed * Math.PI * 2 / OBJECT_TURN_SECONDS;

    const v = sourceFrame(shape, t).vertices;
    if (v.length) {
      const now = extent(v).center;
      for (const k of ["x", "y", "z"]) center[k] += (now[k] - center[k]) * 0.15;
    }

    const cy = Math.cos(yaw), sy = Math.sin(yaw), cp = Math.cos(pitch), sp = Math.sin(pitch);
    const distance = radius * 2.4 + 0.6;
    // Fit the object's bounding sphere inside the capture at every yaw. Keep
    // the scale fixed throughout a turn, with room for moving limbs and fur.
    const focal = preview
      ? Math.min(w, h) * 0.44 * Math.sqrt(distance * distance - radius * radius) / radius
      : Math.min(w, h) * 0.9;
    const project = (x, y, z) => {
      const dx = x - center.x, dy = y - center.y, dz = z - center.z;
      const rx = dx * cy - dz * sy, rz = dx * sy + dz * cy;
      const ry = dy * cp - rz * sp, d = dy * sp + rz * cp + distance;
      return [w / 2 + (rx * focal) / d, h / 2 - (ry * focal) / d, d];
    };
    const near = 0.1;

    // Depth-tested points for fur. Standalone objects have a clean ground.
    const plot = (x, y, d, rgb) => {
      x |= 0; y |= 0;
      if (x < 0 || y < 0 || x >= w || y >= h) return;
      const i = y * w + x;
      if (d >= depth[i]) return;
      depth[i] = d;
      pixels[i * 4] = rgb[0]; pixels[i * 4 + 1] = rgb[1]; pixels[i * 4 + 2] = rgb[2];
    };
    for (let i = 0; i < v.length; i += 18) {
      const a = project(v[i], v[i + 1], v[i + 2]);
      const b = project(v[i + 6], v[i + 7], v[i + 8]);
      const c = project(v[i + 12], v[i + 13], v[i + 14]);
      if (a[2] < near || b[2] < near || c[2] < near) continue;
      fill(pixels, depth, w, h, a, b, c, [v[i + 3] * 255, v[i + 4] * 255, v[i + 5] * 255]);
    }
    // Fur: single aliased lines out of the surface, depth tested.
    const hair = sourceHair(shape, t);
    for (let i = 0; i < hair.length; i += 9) {
      const a = project(hair[i], hair[i + 1], hair[i + 2]),
        b = project(hair[i + 3], hair[i + 4], hair[i + 5]);
      if (a[2] < near || b[2] < near) continue;
      const rgb = [hair[i + 6] * 255, hair[i + 7] * 255, hair[i + 8] * 255];
      const n = Math.max(1, Math.ceil(Math.hypot(b[0] - a[0], b[1] - a[1])));
      for (let s = 0; s <= n; s++) {
        const k = s / n;
        plot(a[0] + (b[0] - a[0]) * k, a[1] + (b[1] - a[1]) * k, a[2] + (b[2] - a[2]) * k - 0.002, rgb);
      }
    }
    const name = (shape.name || "object").toLowerCase();
    if (!preview) {
      const packed = typeof globalThis !== "undefined" && globalThis.acPACK_MODE;
      if (!packed) ink(138, 143, 158).write(name, { x: 6, y: h - 16 });
      if (door && Date.now() - doorShownAt < 4000) {
        ink(230, 232, 240).write(door.replace(/^https?:\/\//, ""), { x: 6, y: h - 16 });
      }
      if (tones.length) speaker(screen, soundEnabled);
    }
  }

  function play(sound) {
    if (voices.length || !tones.length || !sound?.synth) return;
    voices = tones.map((t) =>
      sound.synth({
        type: t.wave,
        tone: t.frequency(0),
        duration: "🔁",
        attack: 0.05,
        volume: 0,
      }),
    );
  }

  // Tones follow AC's shared clock, so a bell rings on the same beat as the
  // game and as any clock.mjs piece.
  function sim({ clock, sound } = {}) {
    if (soundEnabled) play(sound);
    else if (voices.length) leave();
    const t = (Date.now() - start) / 1000;
    const beat = beatAt(clock?.time?.().getTime?.() ?? Date.now());
    voices.forEach((v, i) =>
      v?.update?.({
        tone: tones[i].frequency(t, beat),
        volume: tones[i].gain(t, beat) * (tones[i].wave === "sine" || tones[i].wave === "triangle" ? 0.5 : 0.25),
        duration: 0.02,
      }),
    );
  }

  function leave() {
    voices.forEach((v) => v?.kill?.(0.2));
    voices = [];
  }

  function act({ event: e, sound, jump }) {
    if (e.is("touch")) {
      soundPress = tones.length > 0 && e.x >= size[0] - 30 && e.x < size[0] && e.y >= 3 && e.y < 29;
      if (soundPress) {
        soundEnabled = !soundEnabled;
        if (soundEnabled) play(sound); else leave();
        return;
      }
      dragging = true;
      press = door ? { x: e.x, y: e.y, at: Date.now() } : null;
    }
    if (e.is("lift")) {
      dragging = false; soundPress = false;
      // A tap: under 6px of travel and under 400ms. The pack's shell opens the
      // door in the page's own gesture (see oven/bundler.mjs doorway); here the
      // address is shown, and outside a pack the runtime jumps to it.
      if (press && Math.hypot(e.x - press.x, e.y - press.y) < 6 && Date.now() - press.at < 400) {
        doorShownAt = Date.now();
        const packed = typeof globalThis !== "undefined" && globalThis.acPACK_MODE;
        if (!packed) jump?.("out:" + door);
      }
      press = null;
    }
    if (soundPress) return;
    if (e.is("draw")) {
      yaw += (e.delta?.x || 0) * 0.02;
      pitch = Math.max(-1.2, Math.min(1.4, pitch + (e.delta?.y || 0) * 0.02));
      if (press && Math.hypot(e.x - press.x, e.y - press.y) >= 6) press = null;
    }
  }

  return { boot, paint, act, sim, leave, meta: () => ({ title: shape?.name || "Object", desc: "A KidLisp object." }) };
}

// One triangle into the colour and depth buffers.
function fill(pixels, depth, w, h, a, b, c, rgb) {
  const minX = Math.max(0, Math.floor(Math.min(a[0], b[0], c[0])));
  const maxX = Math.min(w - 1, Math.ceil(Math.max(a[0], b[0], c[0])));
  const minY = Math.max(0, Math.floor(Math.min(a[1], b[1], c[1])));
  const maxY = Math.min(h - 1, Math.ceil(Math.max(a[1], b[1], c[1])));
  const area = (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0]);
  if (!area) return;
  for (let y = minY; y <= maxY; y++)
    for (let x = minX; x <= maxX; x++) {
      const px = x + 0.5, py = y + 0.5;
      const w0 = ((b[0] - px) * (c[1] - py) - (b[1] - py) * (c[0] - px)) / area;
      const w1 = ((c[0] - px) * (a[1] - py) - (c[1] - py) * (a[0] - px)) / area;
      const w2 = 1 - w0 - w1;
      if (w0 < 0 || w1 < 0 || w2 < 0) continue;
      const z = w0 * a[2] + w1 * b[2] + w2 * c[2];
      const i = y * w + x;
      if (z >= depth[i]) continue;
      depth[i] = z;
      pixels[i * 4] = rgb[0];
      pixels[i * 4 + 1] = rgb[1];
      pixels[i * 4 + 2] = rgb[2];
      pixels[i * 4 + 3] = 255;
    }
}
