// 🎨 A cabin bike wears its rider's own work: the two newest #paintings on
// their AC profile, one down each side of the cabin, and the bike painted in
// the first colour of their handle. Asked once a handle and kept; a guest, or
// a handle with no paintings, rides a plain bike in its seat's colour.
//
// A painting is shrunk to COLS × ROWS cells (the canvas averages it), each
// cell a colour or null where the painting is clear.
import { handleColors } from './handle-style.mjs';

export const COLS = 14, ROWS = 8;
const AC = 'https://aesthetic.computer';
const kept = new Map(); // handle → { status: 'asking' | 'ready', decor }
const listeners = new Set();

// The handle's decor if it is ready, else null (and it is asked for).
export function decorFor(handle) {
  if (!handle || typeof document === 'undefined') return null;
  const h = handle.replace(/^@/, '');
  const known = kept.get(h);
  if (known) return known.decor;
  kept.set(h, { status: 'asking', decor: null });
  load(h).then((decor) => {
    kept.set(h, { status: 'ready', decor });
    for (const fn of listeners) fn(h);
  });
  return null;
}
export const onDecor = (fn) => (listeners.add(fn), () => listeners.delete(fn));

async function load(handle) {
  const [colors, sides] = await Promise.all([handleColors(handle), paintings(handle)]);
  const body = colors?.[1] ? (colors[1].r << 16) | (colors[1].g << 8) | colors[1].b : null;
  return { body, sides, version: Date.now() };
}

async function paintings(handle) {
  try {
    const r = await fetch(`${AC}/media-collection?for=${encodeURIComponent('@' + handle)}/painting`, { signal: AbortSignal.timeout(8000) });
    if (!r.ok) return [];
    const { files } = await r.json();
    const newest = (Array.isArray(files) ? files : []).slice(-2).reverse();
    return (await Promise.all(newest.map(shrink))).filter(Boolean);
  } catch {
    return [];
  }
}

function shrink(url) {
  return new Promise((resolve) => {
    const img = new Image();
    img.crossOrigin = 'anonymous';
    img.onload = () => {
      try {
        const c = document.createElement('canvas');
        c.width = COLS; c.height = ROWS;
        const g = c.getContext('2d', { willReadFrequently: true });
        g.imageSmoothingEnabled = true;
        g.imageSmoothingQuality = 'high';
        // Cover the panel: the middle of the painting, cut to the panel's shape.
        const want = COLS / ROWS, have = img.width / img.height;
        const sw = have > want ? img.height * want : img.width, sh = have > want ? img.height : img.width / want;
        g.drawImage(img, (img.width - sw) / 2, (img.height - sh) / 2, sw, sh, 0, 0, COLS, ROWS);
        const d = g.getImageData(0, 0, COLS, ROWS).data, cells = [];
        for (let y = 0; y < ROWS; y++) {
          const row = [];
          for (let x = 0; x < COLS; x++) {
            const i = (y * COLS + x) * 4;
            row.push(d[i + 3] < 100 ? null : (d[i] << 16) | (d[i + 1] << 8) | d[i + 2]);
          }
          cells.push(row);
        }
        resolve(cells);
      } catch {
        resolve(null); // (a painting served without CORS taints the canvas)
      }
    };
    img.onerror = () => resolve(null);
    img.src = url;
  });
}
