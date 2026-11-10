// Thumbnails of objects: three views (front, side, top) side by side, drawn
// with the same software turntable the AC object viewer uses, saved as PNG.
// They are shown in menus and given to Aesel when it revises an object.

import { createRequire } from 'node:module';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { objectPiece, isObjectLisp } from '../system/public/aesthetic.computer/lib/object/viewer.mjs';

const { PNG } = createRequire(new URL('../package.json', import.meta.url))('pngjs');
const SIZE = 128;

// Render one view: the piece's turntable fixed at a yaw and pitch.
function view(source, yaw, pitch, size = SIZE, bare = false) {
  // `preview` at time 0 is the same pose without the name or speaker marks.
  const piece = objectPiece(source, '', bare ? { yaw, pitch, still: true, preview: true, time: 0 } : { yaw, pitch, still: true });
  const pixels = new Uint8ClampedArray(size * size * 4);
  const chain = { write: () => chain };
  piece.paint({ ink: () => chain, screen: { width: size, height: size, pixels } });
  return pixels;
}

// A menu-row thumbnail: one three-quarter view, TINY pixels square, sent to
// the client as base64 RGB. Cached by $code, since a code never changes.
export const TINY = 20;
const tiny = new Map();
export function tinyThumb(code, source) {
  if (tiny.has(code)) return tiny.get(code);
  let thumb = null;
  try {
    if (isObjectLisp(source)) {
      const rgba = view(source, 0.6, 0.35, TINY, true);
      const rgb = Buffer.alloc(TINY * TINY * 3);
      for (let i = 0; i < TINY * TINY; i++) rgba.slice(i * 4, i * 4 + 3).forEach((c, k) => (rgb[i * 3 + k] = c));
      thumb = rgb.toString('base64');
    }
  } catch {}
  if (tiny.size > 2048) tiny.delete(tiny.keys().next().value);
  tiny.set(code, thumb);
  return thumb;
}

export function renderThumb(geometry) {
  if (!geometry?.source) return null;
  const views = [
    view(geometry.source, 0.6, 0.35), // three-quarter front
    view(geometry.source, 0.6 + Math.PI / 2, 0.2), // side
    view(geometry.source, 0.6, 1.35), // top
  ];
  const png = new PNG({ width: SIZE * 3, height: SIZE });
  views.forEach((pixels, k) => {
    for (let y = 0; y < SIZE; y++)
      for (let x = 0; x < SIZE; x++) {
        const from = (y * SIZE + x) * 4,
          to = (y * SIZE * 3 + k * SIZE + x) * 4;
        for (let c = 0; c < 4; c++) png.data[to + c] = pixels[from + c];
      }
  });
  return PNG.sync.write(png);
}

export class Thumbs {
  constructor(dir) {
    this.dir = dir ? join(dir, 'thumbs') : null;
    this.memory = new Map();
  }
  key(id, version) {
    return `${id}-v${version}`.replace(/[^\w-]/g, '');
  }
  async put(id, version, geometry) {
    const png = renderThumb(geometry);
    if (!png) return null;
    const key = this.key(id, version);
    this.memory.set(key, png);
    if (this.memory.size > 128) this.memory.delete(this.memory.keys().next().value);
    if (this.dir) {
      await mkdir(this.dir, { recursive: true });
      await writeFile(join(this.dir, `${key}.png`), png);
    }
    return png;
  }
  async get(id, version, geometry) {
    const key = this.key(id, version);
    if (this.memory.has(key)) return this.memory.get(key);
    if (this.dir)
      try {
        const png = await readFile(join(this.dir, `${key}.png`));
        this.memory.set(key, png);
        return png;
      } catch {}
    return geometry ? this.put(id, version, geometry) : null;
  }
}
