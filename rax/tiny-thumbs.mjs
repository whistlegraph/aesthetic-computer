// The $ history's menu thumbnails: one three-quarter view of an object, TINY
// pixels square, as base64 RGB. Cached by $code, since a code never changes.
// No image library: the client draws the pixels itself.

import { objectPiece, isObjectLisp } from '../system/public/aesthetic.computer/lib/object/viewer.mjs';

export { isObjectLisp };
export const TINY = 20;
const tiny = new Map();

export function tinyThumb(code, source) {
  if (tiny.has(code)) return tiny.get(code);
  let thumb = null;
  try {
    if (isObjectLisp(source)) {
      // `preview` at time 0: the turntable's pose, fitted, without name or speaker.
      const piece = objectPiece(source, '', { yaw: 0.6, pitch: 0.35, still: true, preview: true, time: 0 });
      const rgba = new Uint8ClampedArray(TINY * TINY * 4);
      const chain = { write: () => chain };
      piece.paint({ ink: () => chain, screen: { width: TINY, height: TINY, pixels: rgba } });
      const rgb = Buffer.alloc(TINY * TINY * 3);
      for (let i = 0; i < TINY * TINY; i++) for (let k = 0; k < 3; k++) rgb[i * 3 + k] = rgba[i * 4 + k];
      thumb = rgb.toString('base64');
    }
  } catch {}
  if (tiny.size > 2048) tiny.delete(tiny.keys().next().value);
  tiny.set(code, thumb);
  return thumb;
}
