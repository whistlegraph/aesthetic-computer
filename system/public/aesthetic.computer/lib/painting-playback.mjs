// Reconstruct one saved step without expanding the whole recording into frames.
export function paintingFrame(api, piece, index) {
  const layers = piece.layers;
  let start = Math.max(0, Math.min(index, layers.length - 1));
  const end = start;
  while (start > 0 && layers[start].pixels.mode !== "composite") start--;
  const base = layers[start].pixels;
  if (base.mode !== "composite") return piece.composite;
  const frame = { width: base.width, height: base.height, pixels: base.data };
  if (start === end) return frame;
  frame.pixels = new Uint8ClampedArray(base.data);
  try {
    api.page(frame);
    for (let i = start + 1; i <= end; i++) {
      const pixels = layers[i].pixels;
      if (!pixels.width || !pixels.height) continue;
      api.paste({ width: pixels.width, height: pixels.height, pixels: pixels.data }, pixels.x, pixels.y);
      api.flatten();
    }
    return frame;
  } finally { api.page(api.screen); }
}
