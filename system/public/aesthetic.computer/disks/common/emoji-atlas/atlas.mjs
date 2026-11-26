// Color glyphs derived from Noto Color Emoji 2.057 (SIL OFL 1.1).
// Emoji ordering from Unicode 18.0 (Unicode License v3). See adjacent licenses.
import { keys, pages } from "./data.mjs";

export const atlasKeys = keys.split(" ");
const glyphsPerPage = 64;
const bytesPerGlyph = 16 * 4 + 16 * 16 / 2;
const decodedPages = new Map();

export function atlasSprite(index) {
  if (!Number.isInteger(index) || index < 0 || index >= atlasKeys.length) return null;
  const page = Math.floor(index / glyphsPerPage);
  let bytes = decodedPages.get(page);
  if (!bytes) {
    bytes = Uint8Array.from(atob(pages[page]), byte => byte.charCodeAt(0));
    decodedPages.set(page, bytes);
  }
  const palette = index % glyphsPerPage * bytesPerGlyph;
  const indices = palette + 16 * 4;
  const pixels = new Uint8ClampedArray(16 * 16 * 4);
  for (let i = 0; i < 256; i++) {
    const pair = bytes[indices + (i >> 1)];
    const color = palette + ((i & 1) ? pair & 15 : pair >> 4) * 4;
    pixels.set(bytes.subarray(color, color + 4), i * 4);
  }
  return { width: 16, height: 16, pixels };
}
