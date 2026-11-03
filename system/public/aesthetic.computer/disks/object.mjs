// Object, 2026.11.02
// A turntable for dool's objects: each one is a KidLisp program in the 3D
// object dialect, stored with a $code. `object:8en` (or opening `$8en`)
// loads the source, compiles it with the same compiler the game uses, and
// draws it here with a small z-buffered rasterizer. Drag to orbit.

import { parseSource, sourceFrame } from "../lib/object/object-language.mjs";

let code, shape, error, status = "loading";
let yaw = 0.6, pitch = 0.42, spin = 0.35, dragging = false;
let depth, size = [0, 0];
let center = { x: 0, y: 1, z: 0 }, radius = 3;

async function boot({ params, hud }) {
  code = (params[0] || "").replace(/^\$/, "");
  if (!/^[A-Za-z0-9]{2,16}$/.test(code)) {
    status = "no code";
    error = "usage: object:$code";
    return;
  }
  hud.label(`$${code}`);
  try {
    const response = await fetch(`/api/store-kidlisp?code=${code}`);
    const { source } = await response.json();
    shape = parseSource(source);
    // Size the view from the object itself at one moment (not its whole
    // walking path); the camera then follows its centre as it moves.
    const b = extent(sourceFrame(shape, 0).vertices);
    center = b.center;
    radius = Math.max(0.4, b.radius);
    status = shape.name || "object";
  } catch (e) {
    status = "could not load";
    error = String(e.message || e).slice(0, 60);
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

// One triangle into the colour and depth buffers.
function fill(pixels, w, h, a, b, c, rgb) {
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

function paint({ wipe, ink, screen, write }) {
  wipe(20, 23, 32);
  const { width: w, height: h, pixels } = screen;
  if (!shape) {
    ink(138, 143, 158).write(error || status, { x: 6, y: h - 16 });
    return;
  }
  if (size[0] !== w || size[1] !== h) {
    depth = new Float32Array(w * h);
    size = [w, h];
  }
  depth.fill(Infinity);
  if (!dragging) yaw += spin / 60;

  const v = sourceFrame(shape, performance.now() / 1000).vertices;
  const now = extent(v).center;
  for (const k of ["x", "y", "z"]) center[k] += (now[k] - center[k]) * 0.15;

  const cy = Math.cos(yaw), sy = Math.sin(yaw), cp = Math.cos(pitch), sp = Math.sin(pitch);
  const distance = radius * 2.2, focal = Math.min(w, h) * 0.9;
  const project = (x, y, z) => {
    const dx = x - center.x, dy = y - center.y, dz = z - center.z;
    const rx = dx * cy - dz * sy, rz = dx * sy + dz * cy;
    const ry = dy * cp - rz * sp, d = dy * sp + rz * cp + distance;
    return [w / 2 + (rx * focal) / d, h / 2 - (ry * focal) / d, d];
  };

  // A floor grid under the object, so it stands somewhere.
  const reach = Math.ceil(radius * 1.4);
  for (let g = -reach; g <= reach; g++) {
    const a = project(center.x + g, 0, center.z - reach), b = project(center.x + g, 0, center.z + reach);
    const c = project(center.x - reach, 0, center.z + g), d = project(center.x + reach, 0, center.z + g);
    ink(44, 48, 64).line(a[0], a[1], b[0], b[1]);
    ink(44, 48, 64).line(c[0], c[1], d[0], d[1]);
  }

  for (let i = 0; i < v.length; i += 18) {
    const a = project(v[i], v[i + 1], v[i + 2]);
    const b = project(v[i + 6], v[i + 7], v[i + 8]);
    const c = project(v[i + 12], v[i + 13], v[i + 14]);
    if (a[2] < 0.05 || b[2] < 0.05 || c[2] < 0.05) continue;
    fill(pixels, w, h, a, b, c, [v[i + 3] * 255, v[i + 4] * 255, v[i + 5] * 255]);
  }
  ink(138, 143, 158).write(status.toLowerCase(), { x: 6, y: h - 16 });
}

function act({ event: e }) {
  if (e.is("touch")) dragging = true;
  if (e.is("lift")) dragging = false;
  if (e.is("draw")) {
    yaw += (e.delta?.x || 0) * 0.02;
    pitch = Math.max(-1.2, Math.min(1.4, pitch + (e.delta?.y || 0) * 0.02));
  }
}

function meta() {
  return { title: "Object", desc: "A turntable for dool objects: object:$code." };
}

export { boot, paint, act, meta };
