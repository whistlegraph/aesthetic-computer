// Shared pixel <-> sample conversion helpers for stample-like workflows.

// RGB encoding: 3 samples per pixel (R, G, B channels).
export function encodeSampleToBitmap(data, width = 256) {
  if (!Array.isArray(data) || data.length === 0) return null;
  const sampleLength = data.length;
  const samplesPerPixel = 3;
  const totalPixels = Math.ceil(sampleLength / samplesPerPixel);
  const height = Math.ceil(totalPixels / width);
  const pixels = new Uint8ClampedArray(width * height * 4);

  for (let i = 0; i < sampleLength; i += 1) {
    const v = Math.max(-1, Math.min(1, data[i]));
    const byte = Math.round((v + 1) * 127.5);
    const pixelIndex = Math.floor(i / samplesPerPixel);
    const channel = i % samplesPerPixel;
    const idx = pixelIndex * 4 + channel;
    pixels[idx] = byte;
    if (channel === 0) pixels[pixelIndex * 4 + 3] = 255;
  }

  return { width, height, pixels, sampleLength };
}

// RGB decoding: 3 samples per pixel (R, G, B channels).
export function decodeBitmapToSample(bitmap, meta) {
  if (!bitmap?.pixels?.length || !bitmap?.width || !bitmap?.height) return null;
  const totalPixels = bitmap.width * bitmap.height;
  const samplesPerPixel = 3;
  const maxSamples = totalPixels * samplesPerPixel;
  const sampleLength = Math.min(meta?.sampleLength || maxSamples, maxSamples);
  const data = new Array(sampleLength);

  for (let i = 0; i < sampleLength; i += 1) {
    const pixelIndex = Math.floor(i / samplesPerPixel);
    const channel = i % samplesPerPixel;
    const byte = bitmap.pixels[pixelIndex * 4 + channel] || 0;
    data[i] = byte / 127.5 - 1;
  }

  return data;
}

export async function imageToBuffer(image) {
  if (!image) return null;
  const source = image.img || image.bitmap || image;

  if (source?.pixels && source?.width && source?.height) {
    return {
      width: source.width,
      height: source.height,
      pixels: new Uint8ClampedArray(source.pixels),
    };
  }

  if (source?.data && source?.width && source?.height) {
    return {
      width: source.width,
      height: source.height,
      pixels: new Uint8ClampedArray(source.data),
    };
  }

  const width = source.width || source.naturalWidth || source.videoWidth;
  const height = source.height || source.naturalHeight || source.videoHeight;
  if (!width || !height) return null;

  let canvas;
  if (typeof OffscreenCanvas !== "undefined") {
    canvas = new OffscreenCanvas(width, height);
  } else if (typeof document !== "undefined") {
    canvas = document.createElement("canvas");
    canvas.width = width;
    canvas.height = height;
  }
  if (!canvas) return null;

  const ctx = canvas.getContext("2d");
  if (!ctx) return null;
  ctx.clearRect(0, 0, width, height);
  ctx.drawImage(source, 0, 0, width, height);
  const imageData = ctx.getImageData(0, 0, width, height);

  return {
    width,
    height,
    pixels: new Uint8ClampedArray(imageData.data),
  };
}

async function resolvePaintingCodeMetadata(code, store) {
  const normalized = decodeURIComponent(code).replace(/^#/, "");
  let metadata = store?.[`painting-code:${normalized}`];
  if (!metadata?.slug || !metadata?.handle) {
    try {
      const response = await fetch(`/api/painting-code?code=${normalized}`);
      if (response.ok) {
        metadata = await response.json();
        if (metadata?.slug && metadata?.handle && store) {
          store[`painting-code:${normalized}`] = metadata;
        }
      }
    } catch (err) {
      console.warn("🖼️ Pixel sample: failed to resolve painting code", err);
      return null;
    }
  }
  return metadata?.slug && metadata?.handle ? metadata : null;
}

async function loadPaintingByCode(code, { preload, get, store }) {
  const normalized = decodeURIComponent(code).replace(/^#/, "");
  const baseUrl =
    typeof location !== "undefined" && location.origin
      ? location.origin
      : "https://aesthetic.computer";

  if (preload && normalized.length <= 6) {
    try {
      const directUrl = `${baseUrl}/media/paintings/${normalized}.png?t=${Date.now()}`;
      const directImg = await preload(directUrl, true);
      const directBuffer = await imageToBuffer(directImg);
      if (directBuffer?.pixels?.length) return directBuffer;
    } catch (err) {
      console.warn("🖼️ Pixel sample: direct code load failed", err);
    }
  }

  const metadata = await resolvePaintingCodeMetadata(normalized, store);
  if (!metadata) return null;

  try {
    let img;
    if (preload) {
      const handle = metadata.handle.replace(/^@/, "");
      const base = `${baseUrl}/media/@${handle}/painting/${metadata.slug}.png`;
      img = await preload(`${base}?t=${Date.now()}`, true);
    }
    if (!img && get?.painting) {
      const got = await get.painting(metadata.slug).by(metadata.handle);
      img = got?.img || got?.painting || got;
    }
    if (!img) return null;
    return await imageToBuffer(img);
  } catch (err) {
    console.warn("🖼️ Pixel sample: failed to load painting image", err);
    return null;
  }
}

async function loadSystemPainting({ system, store }) {
  let source =
    (system?.nopaint?.buffer?.pixels?.length && system?.nopaint?.buffer) ||
    system?.painting ||
    null;

  if (!source?.pixels?.length || !source?.width || !source?.height) {
    source = store?.painting || store?.["painting"] || null;
  }
  if (!source?.pixels?.length || !source?.width || !source?.height) {
    try {
      source = await store?.retrieve?.("painting", "local:db");
    } catch (err) {
      source = null;
    }
  }
  if (!source?.pixels?.length || !source?.width || !source?.height) {
    return null;
  }
  return {
    width: source.width,
    height: source.height,
    pixels: new Uint8ClampedArray(source.pixels),
  };
}

async function renderKidlispSource(source, { painting, kidlispSize = 128 }) {
  if (typeof painting !== "function") return null;
  try {
    const rendered = painting(kidlispSize, kidlispSize, (paintApi) => {
      if (typeof paintApi?.kidlisp === "function") {
        paintApi.kidlisp(0, 0, kidlispSize, kidlispSize, source);
      }
    });
    if (!rendered?.pixels?.length) return null;
    return {
      width: rendered.width || kidlispSize,
      height: rendered.height || kidlispSize,
      pixels: new Uint8ClampedArray(rendered.pixels),
    };
  } catch (err) {
    console.warn("🎭 Pixel sample: kidlisp render failed", err);
    return null;
  }
}

/**
 * Load a pixel source and register it as a playable audio sample.
 */
export async function loadPaintingAsAudio(source, opts = {}) {
  const {
    sound,
    preload,
    store,
    system,
    get,
    painting,
    sampleId = "stample:bitmap",
  } = opts;

  let bitmap = null;

  if (typeof source === "string") {
    const normalized = decodeURIComponent(source);
    if (normalized.startsWith("$")) {
      bitmap = await renderKidlispSource(normalized, { painting, kidlispSize: opts.kidlispSize });
    } else if (normalized.startsWith("#") || normalized.startsWith("%23")) {
      bitmap = await loadPaintingByCode(normalized, { preload, get, store });
    } else if (normalized === "p" || normalized === "painting") {
      bitmap = await loadSystemPainting({ system, store });
    }
  } else if (source?.pixels?.length && source?.width && source?.height) {
    bitmap = {
      width: source.width,
      height: source.height,
      pixels: new Uint8ClampedArray(source.pixels),
    };
  }

  if (!bitmap?.pixels?.length) return null;

  const totalPixels = bitmap.width * bitmap.height;
  const meta = {
    sampleLength: totalPixels * 3,
    sampleRate: sound?.sampleRate || 48000,
  };
  const sampleData = decodeBitmapToSample(bitmap, meta);
  if (!sampleData?.length) return null;

  sound?.registerSample?.(sampleId, sampleData, meta.sampleRate);

  if (store) {
    store["stample:bitmap"] = {
      width: bitmap.width,
      height: bitmap.height,
      pixels: Array.from(bitmap.pixels),
      sampleLength: meta.sampleLength,
      sampleRate: meta.sampleRate,
    };
    store.persist?.("stample:bitmap", "local:db");
  }

  return { sampleData, sampleId, bitmap, meta };
}
