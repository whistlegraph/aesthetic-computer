// A No Paint painting is an executable score whose accepted layers retain
// both the proposal code and the pixels produced by that proposal.

export const NOPAINT_PIECE_SCHEMA = "aesthetic.computer/nopaint-piece";
export const NOPAINT_PIECE_VERSION = 1;
export const NOPAINT_PIECE_STORE_KEY = "painting:piece";

function cloneValue(value) {
  if (value === null || ["string", "number", "boolean"].includes(typeof value)) return value;
  if (Array.isArray(value)) return value.map(cloneValue);
  if (ArrayBuffer.isView(value)) return Array.from(value);
  if (typeof value !== "object") return undefined;
  return Object.fromEntries(Object.entries(value)
    .filter(([, entry]) => entry !== undefined && typeof entry !== "function")
    .map(([key, entry]) => [key, cloneValue(entry)]));
}

function stableValue(value) {
  if (Array.isArray(value)) return value.map(stableValue);
  if (!value || typeof value !== "object") return value;
  return Object.fromEntries(Object.keys(value).sort()
    .map((key) => [key, stableValue(value[key])]));
}

function pixelsFrom(value, expectedLength) {
  const source = value instanceof Uint8ClampedArray
    ? value
    : ArrayBuffer.isView(value)
      ? new Uint8ClampedArray(value.buffer, value.byteOffset, value.byteLength)
      : Array.isArray(value)
        ? value
        : null;
  if (!source || source.length !== expectedLength) return null;
  return new Uint8ClampedArray(source);
}

function scoreCode(score) {
  return {
    language: "nopaint-score",
    version: 1,
    source: `paint ${JSON.stringify(stableValue(score))}`,
    score,
  };
}

function layerPixels(pixels, width, height, mode) {
  const full = pixelsFrom(pixels, width * height * 4);
  if (!full) return null;
  if (mode === "composite") {
    return { mode, x: 0, y: 0, width, height, data: full };
  }
  let left = width;
  let top = height;
  let right = -1;
  let bottom = -1;
  for (let y = 0; y < height; y += 1) {
    for (let x = 0; x < width; x += 1) {
      if (full[(y * width + x) * 4 + 3] === 0) continue;
      left = Math.min(left, x);
      top = Math.min(top, y);
      right = Math.max(right, x);
      bottom = Math.max(bottom, y);
    }
  }
  if (right < left || bottom < top) {
    return { mode, x: 0, y: 0, width: 0, height: 0, data: new Uint8ClampedArray() };
  }
  const cropWidth = right - left + 1;
  const cropHeight = bottom - top + 1;
  const data = new Uint8ClampedArray(cropWidth * cropHeight * 4);
  for (let y = 0; y < cropHeight; y += 1) {
    const sourceStart = ((top + y) * width + left) * 4;
    data.set(full.subarray(sourceStart, sourceStart + cropWidth * 4), y * cropWidth * 4);
  }
  return { mode, x: left, y: top, width: cropWidth, height: cropHeight, data };
}

export function createNoPaintPiece({ seed, width, height, pixels, role = "substrate" }) {
  const data = pixelsFrom(pixels, width * height * 4);
  if (!data) throw new Error("No Paint piece substrate has invalid pixels");
  const id = `nopaint:${seed}`;
  const score = { kind: "raster", role, seed: String(seed), width, height };
  return {
    schema: NOPAINT_PIECE_SCHEMA,
    version: NOPAINT_PIECE_VERSION,
    engine: "nopaint-3.0",
    id,
    width,
    height,
    layers: [{
      id: `${id}:base`,
      operation: "raster",
      code: scoreCode(score),
      pixels: {
        mode: "composite",
        x: 0,
        y: 0,
        width,
        height,
        data: new Uint8ClampedArray(data),
      },
    }],
    composite: { width, height, pixels: data },
  };
}

export function createNoPaintProposalLayer({
  piece,
  proposal,
  proposalNumber,
  proposalFrame,
  pixels,
  pixelMode = "overlay",
}) {
  const pixelPayload = layerPixels(pixels, piece.width, piece.height, pixelMode);
  if (!pixelPayload) throw new Error("No Paint proposal layer has invalid pixels");
  const score = cloneValue({
    kind: proposal?.kind || "unknown",
    frame: proposalFrame,
    number: proposalNumber,
    proposal,
  });
  return {
    id: `${piece.id}:layer:${proposalNumber}`,
    operation: score.kind,
    code: scoreCode(score),
    pixels: pixelPayload,
  };
}

export function appendNoPaintLayer(piece, layer, compositePixels) {
  const pixels = pixelsFrom(compositePixels, piece.width * piece.height * 4);
  if (!pixels) throw new Error("No Paint piece composite has invalid pixels");
  return {
    ...piece,
    layers: [...piece.layers, layer],
    composite: { width: piece.width, height: piece.height, pixels },
  };
}

export function recoverNoPaintPiece(value, width, height) {
  if (!value || value.schema !== NOPAINT_PIECE_SCHEMA ||
      value.version !== NOPAINT_PIECE_VERSION ||
      value.width !== width || value.height !== height ||
      !Array.isArray(value.layers) || value.layers.length === 0) return null;
  const expectedLength = width * height * 4;
  const composite = pixelsFrom(value.composite?.pixels, expectedLength);
  if (!composite) return null;
  const layers = [];
  for (const layer of value.layers) {
    const layerWidth = Number(layer?.pixels?.width);
    const layerHeight = Number(layer?.pixels?.height);
    const data = pixelsFrom(layer?.pixels?.data, layerWidth * layerHeight * 4);
    if (!layer?.id || !layer?.code?.source || !data) return null;
    layers.push({
      ...cloneValue(layer),
      pixels: { ...cloneValue(layer.pixels), data },
    });
  }
  return {
    ...cloneValue(value),
    width,
    height,
    layers,
    composite: { width, height, pixels: composite },
  };
}
