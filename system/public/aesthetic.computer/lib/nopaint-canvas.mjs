// A canvas a brush can keep.
//
// Half the recovered No Paint brushes do not draw a shape — they accumulate
// one. Softy stamps a soft circle every Move tick; Wafer *erases* bites out of
// a biscuit; Build lays a brick per beat; Banner leaves a ribbon behind it.
// Construct gave all of them the same thing to work on, a Drawing Canvas, and
// each of ours had grown its own private copy of it: four scanline fills, four
// alpha blends, four ideas about what a radius means.
//
// This is that one surface, in AC's own terms. It is a plain
// { width, height, pixels } so `paste` takes it directly, and every operation
// is the one a recovered brush actually needs:
//
//   box     a brick, a ribbon segment          Build, Banner
//   disc    the biscuit                        Wafer
//   erase   a bite — Construct's blend mode 7  Wafer
//   soft    a radial ramp, in or out           Softy, Vignette, Aura
//   stamp   a prebuilt mask, tinted            Softy's hot path
//   poly    a convex quad                      Banner
//
// A brush renders by advancing its canvas to whatever the clock has reached and
// pasting it once. That costs one full-canvas paste a frame — about 0.2ms —
// instead of redrawing a whole stroke, which for Softy measured 1.7 seconds.

const frozen = (value) => Object.freeze(value);
const clamp = (value, low, high) => Math.max(low, Math.min(high, value));

// Source-over of one premultiplied-by-coverage colour into a pixel.
function over(pixels, at, color, coverage) {
  if (coverage <= 0) return;
  const source = color[3] === undefined ? coverage : coverage * color[3] / 255;
  if (source <= 0) return;
  const under = pixels[at + 3] * (255 - source) / 255;
  const total = source + under;
  if (total <= 0) return;
  pixels[at] = (color[0] * source + pixels[at] * under) / total;
  pixels[at + 1] = (color[1] * source + pixels[at + 1] * under) / total;
  pixels[at + 2] = (color[2] * source + pixels[at + 2] * under) / total;
  pixels[at + 3] = Math.min(255, total);
}

// Keep the strongest coverage rather than stacking it. A field built from many
// overlapping rays should read as one bloom, not as a pile of discs.
function strongest(pixels, at, color, coverage) {
  if (coverage <= pixels[at + 3]) return;
  pixels[at] = color[0];
  pixels[at + 1] = color[1];
  pixels[at + 2] = color[2];
  pixels[at + 3] = coverage;
}

export function createNoPaintCanvas(width, height) {
  const w = Math.max(1, Math.round(width));
  const h = Math.max(1, Math.round(height));
  const pixels = new Uint8ClampedArray(w * h * 4);

  // Walk the rows a circle covers, once, so every round operation shares its
  // bounds and its span arithmetic.
  function spans(centerX, centerY, radius, each) {
    const top = Math.max(0, Math.floor(centerY - radius));
    const bottom = Math.min(h - 1, Math.ceil(centerY + radius));
    for (let y = top; y <= bottom; y += 1) {
      const reach = Math.sqrt(Math.max(0, radius * radius - (y - centerY) ** 2));
      const left = Math.max(0, Math.floor(centerX - reach));
      const right = Math.min(w - 1, Math.ceil(centerX + reach));
      for (let x = left; x <= right; x += 1) each(x, y, (y * w + x) * 4);
    }
  }

  const canvas = {
    width: w,
    height: h,
    pixels,

    wipe() {
      pixels.fill(0);
      return canvas;
    },

    box(x, y, boxWidth, boxHeight, color) {
      const left = clamp(Math.floor(x), 0, w);
      const top = clamp(Math.floor(y), 0, h);
      const right = clamp(Math.ceil(x + boxWidth), 0, w);
      const bottom = clamp(Math.ceil(y + boxHeight), 0, h);
      for (let py = top; py < bottom; py += 1) {
        for (let px = left; px < right; px += 1) over(pixels, (py * w + px) * 4, color, 255);
      }
      return canvas;
    },

    disc(x, y, radius, color) {
      spans(x, y, radius, (px, py, at) => over(pixels, at, color, 255));
      return canvas;
    },

    // Construct switches to blend mode 7, cuts, and switches back. A bite takes
    // the biscuit away rather than painting over it.
    erase(x, y, radius) {
      spans(x, y, radius, (px, py, at) => { pixels[at + 3] = 0; });
      return canvas;
    },

    // Opaque within `hardness`, falling to nothing at `radius`. `invert` turns
    // it inside out for Vignette, which closes in rather than opening up.
    soft(x, y, radius, hardness, color, { peak = 255, invert = false, blend = "over" } = {}) {
      const falloff = Math.max(1, radius - hardness);
      const write = blend === "strongest" ? strongest : over;
      if (invert) {
        for (let py = 0; py < h; py += 1) {
          for (let px = 0; px < w; px += 1) {
            const distance = Math.hypot(px - x, py - y);
            if (distance <= hardness) continue;
            write(pixels, (py * w + px) * 4, color,
              Math.min(1, (distance - hardness) / falloff) * peak);
          }
        }
        return canvas;
      }
      spans(x, y, radius, (px, py, at) => {
        const distance = Math.hypot(px - x, py - y);
        write(pixels, at, color, distance <= hardness
          ? peak
          : peak * (1 - (distance - hardness) / falloff));
      });
      return canvas;
    },

    // A reusable alpha mask, for a brush that stamps the same shape hundreds of
    // times and should not recompute its falloff each time.
    stamp(mask, x, y, color) {
      const left = Math.round(x - mask.size / 2);
      const top = Math.round(y - mask.size / 2);
      for (let my = 0; my < mask.size; my += 1) {
        const py = top + my;
        if (py < 0 || py >= h) continue;
        for (let mx = 0; mx < mask.size; mx += 1) {
          const coverage = mask.alpha[my * mask.size + mx];
          if (coverage === 0) continue;
          const px = left + mx;
          if (px < 0 || px >= w) continue;
          over(pixels, (py * w + px) * 4, color, coverage);
        }
      }
      return canvas;
    },

    // Scanline fill of a convex polygon — the banner's ribbon segment.
    poly(points, color) {
      const top = Math.max(0, Math.floor(Math.min(...points.map((point) => point[1]))));
      const bottom = Math.min(h - 1, Math.ceil(Math.max(...points.map((point) => point[1]))));
      for (let y = top; y <= bottom; y += 1) {
        let left = Infinity;
        let right = -Infinity;
        for (let index = 0; index < points.length; index += 1) {
          const [ax, ay] = points[index];
          const [bx, by] = points[(index + 1) % points.length];
          if ((ay <= y && by > y) || (by <= y && ay > y)) {
            const x = ax + (y - ay) / (by - ay) * (bx - ax);
            left = Math.min(left, x);
            right = Math.max(right, x);
          }
        }
        if (left > right) continue;
        const start = Math.max(0, Math.round(left));
        const end = Math.min(w - 1, Math.round(right));
        for (let x = start; x <= end; x += 1) over(pixels, (y * w + x) * 4, color, 255);
      }
      return canvas;
    },

    // How much of the canvas carries paint — what a test asks about.
    painted() {
      let total = 0;
      for (let at = 3; at < pixels.length; at += 4) if (pixels[at] > 0) total += 1;
      return total;
    },
  };
  return canvas;
}

// Construct's soft circle as a reusable alpha mask:
//   255 - (distance - hardness) / (radius - hardness) * 255
export function softMask(radius, hardness) {
  const size = Math.max(1, Math.ceil(radius * 2));
  const alpha = new Uint8ClampedArray(size * size);
  const falloff = Math.max(1, radius - hardness);
  for (let y = 0; y < size; y += 1) {
    for (let x = 0; x < size; x += 1) {
      const distance = Math.hypot(x - radius, y - radius);
      alpha[y * size + x] = distance <= hardness
        ? 255
        : 255 - (distance - hardness) / falloff * 255;
    }
  }
  return frozen({ size, alpha });
}

// A brush's canvas lives as long as its score does. The score is frozen and
// stable for one proposal, so this is where "what has the clock reached" hangs.
const canvases = new WeakMap();

export function canvasFor(score, build) {
  let held = canvases.get(score);
  if (!held) {
    held = { canvas: createNoPaintCanvas(score.width, score.height), placed: 0 };
    canvases.set(score, held);
    build?.(held.canvas, held);
  }
  return held;
}

// A seeded stream a brush can replay from its score alone, so a canvas can be
// advanced without threading the proposal's shared generator through it.
export function seededStream(seed) {
  let state = seed >>> 0 || 1;
  return () => {
    state += 0x6d2b79f5;
    let value = state;
    value = Math.imul(value ^ (value >>> 15), value | 1);
    value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
    return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
  };
}
