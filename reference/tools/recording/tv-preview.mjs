#!/usr/bin/env node

/**
 * TV Feed Terminal Preview (Sixel)
 * Fetches the /api/tv feed, downloads media assets, converts them to sixel,
 * and renders them inline in the terminal with metadata.
 */

process.env.NODE_TLS_REJECT_UNAUTHORIZED = process.env.NODE_TLS_REJECT_UNAUTHORIZED || "0";

import { PNG } from "pngjs";
import { argv, stdout, exit } from "node:process";

// ---------- CLI Parsing ----------
const DEFAULT_BASE = "https://localhost:8888/api/tv";
const DEFAULT_LIMIT = 8;
const DEFAULT_MAX_WIDTH = Number.POSITIVE_INFINITY;
const DEFAULT_MAX_HEIGHT = Number.POSITIVE_INFINITY;
const DEFAULT_FIT_SIZE = 150;

function parseArgs(args) {
  const options = {
    base: DEFAULT_BASE,
    limit: DEFAULT_LIMIT,
    types: null,
    maxWidth: DEFAULT_MAX_WIDTH,
    maxHeight: DEFAULT_MAX_HEIGHT,
    pauseMs: 0,
    raw: false,
  };

  for (const raw of args) {
    let key;
    let value;

    if (raw.startsWith("--")) {
      const [flag, maybeVal] = raw.slice(2).split("=", 2);
      key = flag;
      value = maybeVal !== undefined ? maybeVal : true;
    } else if (raw.includes("=")) {
      [key, value] = raw.split("=", 2);
    } else {
      // positional fallback, ignore for now
      continue;
    }

    switch (key) {
      case "base":
        if (value) options.base = value;
        break;
      case "limit":
        if (value) {
          const parsed = Number.parseInt(value, 10);
          if (!Number.isNaN(parsed) && parsed > 0) options.limit = parsed;
        }
        break;
      case "types":
        if (typeof value === "string" && value.length > 0) options.types = value;
        break;
      case "maxWidth":
      case "maxwidth":
        if (value) {
          const parsed = Number.parseInt(value, 10);
          if (!Number.isNaN(parsed) && parsed > 0) options.maxWidth = parsed;
        }
        break;
      case "maxHeight":
      case "maxheight":
        if (value) {
          const parsed = Number.parseInt(value, 10);
          if (!Number.isNaN(parsed) && parsed > 0) options.maxHeight = parsed;
        }
        break;
      case "fit": {
        let target = DEFAULT_FIT_SIZE;
        if (value !== true) {
          const parsed = Number.parseInt(value, 10);
          if (!Number.isNaN(parsed) && parsed > 0) target = parsed;
        }
        options.fit = true;
        options.maxWidth = target;
        options.maxHeight = target;
        break;
      }
      case "pause":
      case "pauseMs":
      case "pausems":
        if (value) {
          const parsed = Number.parseInt(value, 10);
          if (!Number.isNaN(parsed) && parsed >= 0) options.pauseMs = parsed;
        }
        break;
      case "raw":
        options.raw = value !== false;
        break;
      default:
        break;
    }
  }

  return options;
}

const options = parseArgs(argv.slice(2));

// ---------- Network Helpers ----------
async function fetchJson(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Request failed: ${response.status} ${response.statusText}`);
  }
  return response.json();
}

async function fetchBuffer(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Asset request failed: ${response.status} ${response.statusText}`);
  }
  const arrayBuffer = await response.arrayBuffer();
  return Buffer.from(arrayBuffer);
}

// ---------- Image Processing ----------
function rgbaToRgb(rgbaBuffer, width, height, background = [0, 0, 0]) {
  const rgb = new Uint8Array(width * height * 3);
  for (let i = 0, j = 0; i < rgbaBuffer.length; i += 4, j += 3) {
    const r = rgbaBuffer[i];
    const g = rgbaBuffer[i + 1];
    const b = rgbaBuffer[i + 2];
    const a = rgbaBuffer[i + 3] ?? 255;

    if (a === 255) {
      rgb[j] = r;
      rgb[j + 1] = g;
      rgb[j + 2] = b;
    } else if (a === 0) {
      rgb[j] = background[0];
      rgb[j + 1] = background[1];
      rgb[j + 2] = background[2];
    } else {
      const alpha = a / 255;
      rgb[j] = Math.round(r * alpha + background[0] * (1 - alpha));
      rgb[j + 1] = Math.round(g * alpha + background[1] * (1 - alpha));
      rgb[j + 2] = Math.round(b * alpha + background[2] * (1 - alpha));
    }
  }
  return rgb;
}

function generateSixel(rgbBuffer, width, height, maxWidth, maxHeight) {
  const aspectRatio = height / width;
  let targetWidth = Math.min(maxWidth, width);
  let targetHeight = Math.round(targetWidth * aspectRatio);

  if (targetHeight > maxHeight) {
    targetHeight = maxHeight;
    targetWidth = Math.round(targetHeight / aspectRatio);
  }

  const scaledWidth = Math.max(1, targetWidth);
  const scaledHeight = Math.max(1, targetHeight);

  const scaleX = width / scaledWidth;
  const scaleY = height / scaledHeight;

  const scaledBuffer = new Uint8Array(scaledWidth * scaledHeight * 3);
  for (let y = 0; y < scaledHeight; y++) {
    for (let x = 0; x < scaledWidth; x++) {
      const srcX = Math.floor(x * scaleX);
      const srcY = Math.floor(y * scaleY);
      const srcIdx = (srcY * width + srcX) * 3;
      const dstIdx = (y * scaledWidth + x) * 3;
      scaledBuffer[dstIdx] = rgbBuffer[srcIdx];
      scaledBuffer[dstIdx + 1] = rgbBuffer[srcIdx + 1];
      scaledBuffer[dstIdx + 2] = rgbBuffer[srcIdx + 2];
    }
  }

  let output = "\x1bPq";
  const colorMap = new Map();
  let nextColorIndex = 0;

  const bands = Math.ceil(scaledHeight / 6);
  for (let band = 0; band < bands; band++) {
    const bandRows = new Map();

    for (let x = 0; x < scaledWidth; x++) {
      for (let dy = 0; dy < 6; dy++) {
        const y = band * 6 + dy;
        if (y >= scaledHeight) break;

        const idx = (y * scaledWidth + x) * 3;
        const r = scaledBuffer[idx];
        const g = scaledBuffer[idx + 1];
        const b = scaledBuffer[idx + 2];
        const key = (r << 16) | (g << 8) | b;

        if (!colorMap.has(key)) {
          const colorIndex = nextColorIndex++;
          colorMap.set(key, colorIndex);
          output += `#${colorIndex};2;${Math.round((r / 255) * 100)};${Math.round((g / 255) * 100)};${Math.round((b / 255) * 100)}`;
        }

        const colorIndex = colorMap.get(key);
        if (!bandRows.has(colorIndex)) {
          bandRows.set(colorIndex, new Array(scaledWidth).fill(0));
        }
        bandRows.get(colorIndex)[x] |= 1 << dy;
      }
    }

    for (const [colorIndex, columns] of bandRows) {
      output += `#${colorIndex}`;
      for (let x = 0; x < scaledWidth; x++) {
        output += String.fromCharCode(63 + columns[x]);
      }
      output += "$";
    }
    output += "-";
  }

  return {
    data: output + "\x1b\\",
    width: scaledWidth,
    height: scaledHeight,
    scaled: scaledWidth !== width || scaledHeight !== height,
  };
}

function formatTimestamp(ts) {
  try {
    const d = new Date(ts);
    if (Number.isNaN(d.getTime())) return ts;
    return d.toISOString().replace("T", " ").replace(".000Z", "Z");
  } catch {
    return ts;
  }
}

function printDivider() {
  stdout.write("\n" + "-".repeat(72) + "\n");
}

async function pause(ms) {
  if (ms > 0) {
    await new Promise((resolve) => setTimeout(resolve, ms));
  }
}

(async () => {
  try {
    const apiUrl = new URL(options.base);
    apiUrl.searchParams.set("limit", String(options.limit));
    if (options.types) {
      apiUrl.searchParams.set("types", options.types);
    }

    stdout.write(`📺 Fetching ${apiUrl.toString()}\n`);
    const feed = await fetchJson(apiUrl);

    if (options.raw) {
      stdout.write(`${JSON.stringify(feed, null, 2)}\n`);
      return;
    }

    const paintings = feed?.media?.paintings ?? [];
    if (paintings.length === 0) {
      stdout.write("🤷 No paintings returned by feed.\n");
      return;
    }

    stdout.write(`🎨 Showing ${Math.min(paintings.length, options.limit)} painting(s)\n`);
    printDivider();

    for (let i = 0; i < paintings.length; i++) {
      const entry = paintings[i];
      const owner = entry?.owner?.handle || entry?.owner?.userId || "unknown";
      const when = formatTimestamp(entry?.when);
      const url = entry?.media?.url;
      const slug = entry?.slug || entry?.id || "";

      if (!url) {
        stdout.write(`⚠️  Skipping ${owner} (${slug}) — missing URL\n`);
        continue;
      }

      stdout.write(`🖼️  ${i + 1}/${paintings.length} · ${owner} · ${slug}\n`);
      stdout.write(`    ${url}\n`);
      stdout.write(`    ${when}\n`);

      try {
        const buffer = await fetchBuffer(url);
        const png = PNG.sync.read(buffer);
        const rgb = rgbaToRgb(png.data, png.width, png.height);
        const sixel = generateSixel(rgb, png.width, png.height, options.maxWidth, options.maxHeight);

        stdout.write(`    ${png.width}×${png.height}px`);
        if (sixel.scaled) {
          stdout.write(` (rendered ${sixel.width}×${sixel.height}px)`);
        }
        stdout.write("\n");

        stdout.write(sixel.data + "\n");
      } catch (error) {
        stdout.write(`    ❌ Failed to render painting: ${error.message}\n`);
      }

      if (i + 1 < paintings.length) {
        await pause(options.pauseMs);
        printDivider();
      }
    }
  } catch (error) {
    stdout.write(`❌ ${error.message}\n`);
    exit(1);
  }
})();
