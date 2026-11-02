import { blockGlyphs } from './engine/block-font.mjs';

export const INK = 0x181818,
  PAPER = 0xffffff;
export const rgb = (color) => [
  ((color >> 16) & 255) / 255,
  ((color >> 8) & 255) / 255,
  (color & 255) / 255,
];
const tones = new Map();
const colorOf = (color) => {
  if (!tones.has(color)) tones.set(color, rgb(color));
  return tones.get(color);
};

// Models contain only solid triangles. Text uses Oskiewar's exact block glyphs.
export function model(build) {
  const data = [];
  const tri = (a, b, c, color) => {
    const ink = colorOf(color);
    for (const p of [a, b, c]) data.push(...p, ...ink);
  };
  const quad = (a, b, c, d, color) => {
    tri(a, b, c, color);
    tri(a, c, d, color);
  };
  const rect = (x, y, z, w, h, color) =>
    quad([x, y, z], [x + w, y, z], [x + w, y + h, z], [x, y + h, z], color);
  const box = (x, y, z, w, h, d, color, side = color, top = PAPER) => {
    const a = x - w / 2,
      b = x + w / 2,
      c = y - h / 2,
      e = y + h / 2,
      f = z - d / 2,
      g = z + d / 2;
    quad([a, c, f], [b, c, f], [b, e, f], [a, e, f], color);
    quad([b, c, g], [a, c, g], [a, e, g], [b, e, g], color);
    quad([a, c, g], [a, c, f], [a, e, f], [a, e, g], side);
    quad([b, c, f], [b, c, g], [b, e, g], [b, e, f], side);
    quad([a, e, f], [b, e, f], [b, e, g], [a, e, g], top);
    quad([a, c, g], [b, c, g], [b, c, f], [a, c, f], side);
  };
  const text = (value, x, y, z, unit, color) => {
    const string = String(value).toUpperCase().replaceAll('·', '-').replaceAll('—', '-');
    let left = x - ((string.length * 6 - 1) * unit) / 2;
    for (const letter of string) {
      const rows = blockGlyphs[letter] || blockGlyphs['?'];
      rows.forEach((bits, row) => {
        // Adjacent pixels are a single plane; no texture filtering or font rasterizer.
        for (let col = 0; col < 5; ) {
          if (!(bits & (1 << (4 - col)))) {
            col++;
            continue;
          }
          const start = col++;
          while (col < 5 && bits & (1 << (4 - col))) col++;
          rect(-left - col * unit, y - row * unit, z, (col - start) * unit, unit, color);
        }
      });
      left += 6 * unit;
    }
  };
  build({ tri, quad, rect, box, text });
  return new Float32Array(data);
}
