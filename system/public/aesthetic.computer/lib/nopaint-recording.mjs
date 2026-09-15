// Convert accepted No Paint layers to AC's standard painting recording.
// Rebuilding from the persisted layers also recovers steps made before this
// save path existed, without recording rejected or still-running proposals.
export function createNoPaintRecording(api, piece) {
  if (!piece?.layers?.length) throw new Error("No Paint has no steps to save");
  const { width, height } = piece;
  const composite = api.painting(width, height, (p) => p.wipe(0, 0, 0, 0));
  try {
    return piece.layers.map((layer, index) => {
      const pixels = layer.pixels;
      if (pixels.mode === "composite") {
        composite.pixels.set(pixels.data);
      } else if (pixels.width && pixels.height) {
        api.page(composite).paste({
          width: pixels.width,
          height: pixels.height,
          pixels: pixels.data,
        }, pixels.x, pixels.y);
        api.flatten();
      }
      return {
        timestamp: layer.timestamp || api.num.timestamp(),
        // The index keeps filenames unique even for old layers without times.
        label: index === 0 ? "nopaint" : `nopaint~${index}~${layer.operation}`,
        painting: { width, height, pixels: new Uint8ClampedArray(composite.pixels) },
      };
    });
  } finally {
    api.page(api.screen);
  }
}
