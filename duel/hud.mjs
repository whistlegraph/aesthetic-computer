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

export function hudLayout(width, height, cells) {
  const safeX = Math.max(6, Math.ceil(width * 0.02));
  const safeY = Math.max(12, Math.ceil(height * 0.05));
  const unit = Math.max(1, Math.round(Math.min(width / 480, height / 270)));
  // A corner badge: about a fifth of the short side, never under one pixel per module.
  const module = cells ? Math.max(1, Math.floor(Math.min(width * 0.14, height * 0.22) / (cells.length + 8))) : 0;
  const qrSize = cells ? (cells.length + 8) * module : 0;
  return { safeX, safeY, unit, module, qrSize };
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
