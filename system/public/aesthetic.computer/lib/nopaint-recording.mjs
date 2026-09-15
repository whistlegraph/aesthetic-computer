// Convert accepted No Paint layers to AC's standard painting recording.
// Rebuilding from the persisted layers also recovers steps made before this
// save path existed, without recording rejected or still-running proposals.
export function createNoPaintRecording(api, piece) {
  if (!piece?.layers?.length) throw new Error("No Paint has no steps to save");
  let composite;
  const screen = api.screen;
  try {
    return piece.layers.map((layer, index) => {
      const pixels = layer.pixels;
      if (pixels.mode === "composite") {
        composite = {
          width: pixels.width, height: pixels.height,
          pixels: new Uint8ClampedArray(pixels.data),
        };
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
        label: layer.label || (index === 0 ? "nopaint" : `nopaint~${index}~${layer.operation}`),
        ...(layer.gesture?.length ? { gesture: layer.gesture } : {}),
        painting: { ...composite, pixels: new Uint8ClampedArray(composite.pixels) },
      };
    });
  } finally {
    api.page(screen);
  }
}
