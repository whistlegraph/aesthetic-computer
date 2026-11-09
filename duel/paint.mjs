import { blockGlyphs } from './engine/block-font.mjs';
import { glyphRows as sharedGlyphRows } from './handle-font.mjs';
// The shared AC font plus the arrow keys, for the controls legend. Rows are
// ten tall and bit 1 is the left column. (handle-font.mjs is served as the
// shared font file itself, so the additions live here.)
const glyphRows = {
  ...sharedGlyphRows,
  '←': [0, 4, 2, 31, 2, 4, 0, 0, 0, 0],
  '→': [0, 4, 8, 31, 8, 4, 0, 0, 0, 0],
  '↑': [0, 4, 14, 21, 4, 4, 4, 4, 0, 0],
  '↓': [0, 4, 4, 4, 4, 21, 14, 4, 0, 0],
};
import { glyphs as chunky } from './chunky-font.mjs';

// MatrixChunky8 width of a string, in pixels (proportional).
export const smallWidth = (value) =>
  [...String(value)].reduce((w, ch) => w + (chunky[ch] || chunky['?'])[4], 0) - 1;

export const INK = 0x181818,
  PAPER = 0xffffff;
export const rgb = (color) => [
  ((color >> 16) & 255) / 255,
  ((color >> 8) & 255) / 255,
  (color & 255) / 255,
];
const glyphs = {
  ...blockGlyphs,
  '@': [14, 17, 23, 21, 23, 16, 14],
  _: [0, 0, 0, 0, 0, 0, 31],
  '>': [16, 8, 4, 2, 4, 8, 16],
  '<': [1, 2, 4, 8, 4, 2, 1],
  "'": [4, 4, 0, 0, 0, 0, 0],
  '"': [10, 10, 0, 0, 0, 0, 0],
  '(': [2, 4, 8, 8, 8, 4, 2],
  ')': [8, 4, 2, 2, 2, 4, 8],
  '+': [0, 4, 4, 31, 4, 4, 0],
  '=': [0, 0, 31, 0, 31, 0, 0],
  '|': [4, 4, 4, 4, 4, 4, 4],
  ';': [0, 4, 4, 0, 4, 4, 8],
  '{': [3, 4, 4, 8, 4, 4, 3],
  '}': [24, 4, 4, 2, 4, 4, 24],
  '[': [14, 8, 8, 8, 8, 8, 14],
  ']': [14, 2, 2, 2, 2, 2, 14],
};
const tones = new Map();
export const tint = (color, light) => {
  const channels = [16, 8, 0].map((shift) =>
    Math.round(((color >> shift) & 255) * light),
  );
  return (channels[0] << 16) | (channels[1] << 8) | channels[2];
};
const colorOf = (color) => {
  if (!tones.has(color)) tones.set(color, rgb(color));
  return tones.get(color);
};

// Models contain only solid triangles. Text uses Oskiewar's exact block glyphs.
export function model(build) {
  // Written straight into a growing float buffer: overlays rebuild every
  // frame, and per-vertex arrays and spreads made them a garbage storm that
  // stuttered the room while a build streamed (2026-11-08).
  let data = new Float32Array(4096),
    n = 0;
  const room = (k) => {
    if (n + k <= data.length) return;
    const grown = new Float32Array(Math.max(data.length * 2, n + k));
    grown.set(data);
    data = grown;
  };
  const vertex = (x, y, z, ink) => {
    data[n++] = x;
    data[n++] = y;
    data[n++] = z;
    data[n++] = ink[0];
    data[n++] = ink[1];
    data[n++] = ink[2];
  };
  const tri = (a, b, c, color) => {
    const ink = colorOf(color);
    room(18);
    vertex(a[0], a[1], a[2], ink);
    vertex(b[0], b[1], b[2], ink);
    vertex(c[0], c[1], c[2], ink);
  };
  const quad = (a, b, c, d, color) => {
    tri(a, b, c, color);
    tri(a, c, d, color);
  };
  const rect = (x, y, z, w, h, color) => {
    const ink = colorOf(color),
      X = x + w,
      Y = y + h;
    room(36);
    vertex(x, y, z, ink);
    vertex(X, y, z, ink);
    vertex(X, Y, z, ink);
    vertex(x, y, z, ink);
    vertex(X, Y, z, ink);
    vertex(x, Y, z, ink);
  };
  const box = (x, y, z, w, h, d, color, side = color, top = PAPER) => {
    // Broad ambient light keeps color on shadow faces; tops retain their material.
    if (side === INK && color !== INK) side = tint(color, 0.72);
    if (top === PAPER) top = color;
    const a = x - w / 2,
      b = x + w / 2,
      c = y - h / 2,
      e = y + h / 2,
      f = z - d / 2,
      g = z + d / 2;
    quad([a, c, f], [b, c, f], [b, e, f], [a, e, f], color);
    quad([b, c, g], [a, c, g], [a, e, g], [b, e, g], tint(color, 0.94));
    quad([a, c, g], [a, c, f], [a, e, f], [a, e, g], side);
    quad([b, c, f], [b, c, g], [b, e, g], [b, e, f], side);
    quad([a, e, f], [b, e, f], [b, e, g], [a, e, g], top);
    quad([a, c, g], [b, c, g], [b, c, f], [a, c, f], side);
  };
  // AC's font_1, lowercase as written: 6 columns, 10 rows, bit 0 leftmost.
  const text = (value, x, y, z, unit, color, keepCase = false) => {
    const string = String(value)
      .replaceAll('·', '-')
      .replaceAll('—', '-');
    let left = x - ((string.length * 6 - 1) * unit) / 2;
    for (const raw of string) {
      const letter = keepCase ? raw : raw.toLowerCase();
      const rows = glyphRows[letter] || glyphRows[raw] || glyphRows['?'];
      rows.forEach((bits, row) => {
        // Adjacent pixels are a single plane; no texture filtering or font rasterizer.
        for (let col = 0; col < 6; ) {
          if (!(bits & (1 << col))) {
            col++;
            continue;
          }
          const start = col++;
          while (col < 6 && bits & (1 << col)) col++;
          rect(
            -left - col * unit,
            y - (row - 1) * unit,
            z,
            (col - start) * unit,
            unit,
            color,
          );
        }
      });
      left += 6 * unit;
    }
  };
  const handleText = (value, x, y, z, unit, colors) => {
    const string = String(value);
    let left = x - ((string.length * 6 - 1) * unit) / 2;
    [...string].forEach((letter, index) => {
      const saved = colors?.[index];
      // Same dark-theme fallback hues as AC's shared web handle renderer.
      const hue = ((330 + index * 47) % 360) / 60,
        c = 0.476,
        m = 0.482;
      const h = c * (1 - Math.abs((hue % 2) - 1));
      const channel =
        hue < 1
          ? [c, h, 0]
          : hue < 2
            ? [h, c, 0]
            : hue < 3
              ? [0, c, h]
              : hue < 4
                ? [0, h, c]
                : hue < 5
                  ? [h, 0, c]
                  : [c, 0, h];
      const rgb = saved
        ? [saved.r, saved.g, saved.b]
        : channel.map((v) => Math.round((v + m) * 255));
      const color = (rgb[0] << 16) | (rgb[1] << 8) | rgb[2];
      (glyphRows[letter] || glyphRows['?']).forEach((bits, row) => {
        for (let col = 0; col < 6; col++)
          if (bits & (1 << col))
            rect(
              -left - (col + 1) * unit,
              y - row * unit,
              z,
              unit,
              unit,
              color,
            );
      });
      left += 6 * unit;
    });
  };
  // MatrixChunky8 for small text: proportional, 8px tall, centred at x like text().
  const small = (value, x, y, z, unit, color) => {
    const string = String(value);
    let left = x - (smallWidth(string) * unit) / 2;
    for (const ch of string) {
      const [w, h, xoff, yoff, advance, rows] = chunky[ch] || chunky['?'];
      rows.forEach((bits, r) => {
        for (let col = 0; col < w; ) {
          if (!(bits & (0x80 >> col))) {
            col++;
            continue;
          }
          const start = col++;
          while (col < w && bits & (0x80 >> col)) col++;
          // Top of the glyph box sits ascent - (yoff + h) rows below y.
          rect(-left - (xoff + col) * unit, y - (8 - yoff - h + r) * unit, z, (col - start) * unit, unit, color);
        }
      });
      left += advance * unit;
    }
  };
  build({ tri, quad, rect, box, text, handleText, small });
  return data.slice(0, n);
}
