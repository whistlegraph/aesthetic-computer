import { qrcode, ErrorCorrectLevel } from './qr.mjs';

const codes = new Map();
export function joinCode(url) {
  if (!url) return null;
  try {
    const parsed = new URL(url);
    if (!['http:', 'https:'].includes(parsed.protocol) || parsed.username || parsed.password) return null;
    if (!codes.has(url)) {
      if (codes.size >= 8) codes.clear();
      codes.set(url, qrcode(url, { errorCorrectLevel: ErrorCorrectLevel.M }).modules);
    }
    return codes.get(url);
  } catch {
    return null;
  }
}

export function hudLayout(width, height, cells, scale = 1, fixedUnit = 0) {
  const safeX = Math.max(6, Math.ceil(width * 0.02));
  const safeY = Math.max(6, Math.ceil(height * 0.03));
  // `scale` is CSS pixels per buffer pixel. A HUD unit is at least one and a
  // half CSS pixels, so the 7-pixel letters read at about 10.5 CSS pixels:
  // two units on the 0.75 buffer a HiDPI screen gets, two whole pixels on a
  // 1x screen (one unit drew them 7 pixels tall, three units read too large).
  const least = Math.ceil(1.5 / Math.max(0.5, scale));
  // A host that lays us on its own pixel grid fixes the unit (1 under the
  // prompt: its font_1 and ours are then the same letters, pixel for pixel).
  const unit = fixedUnit > 0 ? fixedUnit : Math.max(least, Math.round(Math.min(width / 480, height / 270)));
  // A corner badge about a fifth of the width (a quarter of the height), and
  // at least three screen pixels per module so a phone reads it from arm's length.
  const module = cells
    ? Math.max(
        1,
        Math.min(
          Math.max(Math.ceil(3 / Math.max(0.5, scale)), Math.floor(Math.min(width * 0.2, height * 0.26) / (cells.length + 8))),
          // Never more than half the short side, whatever the floor asks.
          Math.floor(Math.min(width, height) / 2 / (cells.length + 8)),
        ),
      )
    : 0;
  const qrSize = cells ? (cells.length + 8) * module : 0;
  return { safeX, safeY, unit, module, qrSize };
}

// 📦 A place for everything on the screen, and nothing on anything else.
// Each frame the view makes one of these and every part of the interface
// that stands at an edge asks it for room before it draws: `reserve` gives
// a rectangle of the size asked for, in the corner asked for, that touches
// nothing given out before it (a `gap` apart at the least); `claim` writes
// down a rectangle a part has already taken for itself. What is asked for
// first sits nearest its corner; the next stands clear of it, away from the
// edge (up from the bottom, down from the top), and in the next column
// inward when a corner's column is full. Rectangles are in the picture's own
// pixels, x from the left and y from the top.
export function createPack(width, height, { safeX = 4, safeY = 4, gap = 2 } = {}) {
  const taken = [];
  const hits = (r) => taken.find((t) => r.x < t.x + t.w + gap && t.x < r.x + r.w + gap && r.y < t.y + t.h + gap && t.y < r.y + r.h + gap);
  const pack = {
    taken,
    claim(id, rect) { if (rect) taken.push({ id, x: rect.x, y: rect.y, w: rect.w, h: rect.h }); return rect; },
    // corner: 'tl' 'tr' 'bl' 'br', or 'tc' 'bc' for the middle of the top or bottom edge.
    reserve(id, corner, w, h) {
      const bottom = corner[0] === 'b', side = corner[1];
      const xAt = (column) => side === 'l' ? safeX + column : side === 'r' ? width - safeX - w - column : Math.round((width - w) / 2) + column;
      let column = 0, rect = null;
      for (let tries = 0; tries < 12; tries++) {
        let y = bottom ? height - safeY - h : safeY;
        rect = { id, x: xAt(column), y, w, h };
        for (let other = hits(rect), n = 0; other && n < 64; other = hits(rect), n++)
          rect.y = y = bottom ? other.y - gap - h : other.y + other.h + gap;
        // Within the half of the screen its edge is on, it stands; past that, the next column inward.
        if (!hits(rect) && (bottom ? rect.y >= height * 0.35 : rect.y + h <= height * 0.65)) break;
        const blocker = hits({ ...rect, y: bottom ? height - safeY - h : safeY }) || rect;
        column += (blocker.w || w) + gap;
      }
      taken.push(rect);
      return rect;
    },
    // Any two parts that share pixels: [idA, idB]. Empty when the screen is in order.
    overlaps() {
      const out = [];
      for (let i = 0; i < taken.length; i++)
        for (let j = i + 1; j < taken.length; j++) {
          const a = taken[i], b = taken[j];
          if (a.x < b.x + b.w && b.x < a.x + a.w && a.y < b.y + b.h && b.y < a.y + a.h) out.push([a.id, b.id]);
        }
      return out;
    },
  };
  return pack;
}

export function drawJoinCode(rect, text, cells, width, height, layout) {
  if (!cells) return;
  const { safeX, safeY, module, qrSize, unit } = layout;
  const x = width / 2 - safeX - qrSize;
  const top = height / 2 - safeY;
  rect(x, top - qrSize, 0.000002, qrSize, qrSize, 0xffffff);
  cells.forEach((row, y) => {
    for (let col = 0; col < row.length;) {
      if (!row[col]) { col++; continue; }
      const start = col++;
      while (col < row.length && row[col]) col++;
      rect(x + (start + 4) * module, top - (y + 5) * module,
        0, (col - start) * module, module, 0x000000);
    }
  });
  rect(x, top - qrSize - 12 * unit, 0.000002, qrSize, 12 * unit, 0xffffff);
  text('JOIN', x + qrSize / 2, top - qrSize - 3 * unit, 0, unit, 0x181818);
}

// Fit the screen between the occupied HUD rectangles. Try each possible pair
// of vertical edges, then fit the picture into the open spans between them.
export function fitHudScreen(width, height, aspect, regions, gap = 2) {
  const left = gap, right = width - gap, bottom = height - gap;
  const edges = new Set([left, right]);
  const obstacles = regions.map(r => ({
    l: Math.max(left, r.x - gap), r: Math.min(right, r.x + r.w + gap),
    t: Math.max(gap, r.y - gap), b: Math.min(bottom, r.y + r.h + gap),
  })).filter(r => r.l < r.r && r.t < r.b).sort((a,b) => a.t - b.t);
  for (const o of obstacles) { edges.add(o.l); edges.add(o.r); }
  let best = null, distance = Infinity;
  for (const l of edges) for (const r of edges) {
    if (r <= l) continue;
    let y = gap;
    const fit = end => {
      const w = Math.min(r - l, (end - y) * aspect), h = w / aspect;
      if (w < 1 || h < 1 || (best && w < best.w)) return;
      const x0 = Math.max(l, Math.min(r - w, (width - w) / 2));
      const y0 = Math.max(y, Math.min(end - h, (height - h) / 2));
      const d = Math.hypot(x0 + w / 2 - width / 2, y0 + h / 2 - height / 2);
      if (!best || w > best.w || d < distance) {
        best = { x: x0, y: y0, w, h }; distance = d;
      }
    };
    for (const o of obstacles) if (o.l < r && o.r > l) {
      fit(o.t);
      y = Math.max(o.b, y);
    }
    fit(bottom);
  }
  return best;
}
