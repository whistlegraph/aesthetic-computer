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
const DEFAULT_MAX_WIDTH = 800; // Reasonable default to prevent huge images
const DEFAULT_MAX_HEIGHT = 600; // Reasonable default to prevent huge images
const DEFAULT_FIT_SIZE = 150;
const MAX_SIXEL_PARTS = 500000; // Prevent excessive memory usage

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

function resizeRgbBuffer(rgbBuffer, srcWidth, srcHeight, targetWidth, targetHeight) {
  const resized = new Uint8Array(targetWidth * targetHeight * 3);
  const scaleX = srcWidth / targetWidth;
  const scaleY = srcHeight / targetHeight;
  
  for (let y = 0; y < targetHeight; y++) {
    for (let x = 0; x < targetWidth; x++) {
      const srcX = Math.min(Math.floor(x * scaleX), srcWidth - 1);
      const srcY = Math.min(Math.floor(y * scaleY), srcHeight - 1);
      const srcIdx = (srcY * srcWidth + srcX) * 3;
      const dstIdx = (y * targetWidth + x) * 3;
      
      resized[dstIdx] = rgbBuffer[srcIdx];
      resized[dstIdx + 1] = rgbBuffer[srcIdx + 1];
      resized[dstIdx + 2] = rgbBuffer[srcIdx + 2];
    }
  }
  
  return resized;
}

function generateSixel(rgbBuffer, width, height) {
  // Simple sixel generation - no scaling here, just convert pixels to sixel format
  // Use array for efficient string building instead of concatenation
  const outputParts = ["\x1bPq"];
  const colorMap = new Map();
  let nextColorIndex = 0;

  const bands = Math.ceil(height / 6);
  for (let band = 0; band < bands; band++) {
    const bandRows = new Map();

    for (let x = 0; x < width; x++) {
      for (let dy = 0; dy < 6; dy++) {
        const y = band * 6 + dy;
        if (y >= height) break;

        const idx = (y * width + x) * 3;
        
        // Bounds check to prevent buffer overflow
        if (idx + 2 >= rgbBuffer.length) {
          break;
        }
        
        const r = rgbBuffer[idx] ?? 0;
        const g = rgbBuffer[idx + 1] ?? 0;
        const b = rgbBuffer[idx + 2] ?? 0;
        const key = (r << 16) | (g << 8) | b;

        if (!colorMap.has(key)) {
          const colorIndex = nextColorIndex++;
          colorMap.set(key, colorIndex);
          outputParts.push(`#${colorIndex};2;${Math.round((r / 255) * 100)};${Math.round((g / 255) * 100)};${Math.round((b / 255) * 100)}`);
        }

        const colorIndex = colorMap.get(key);
        if (!bandRows.has(colorIndex)) {
          bandRows.set(colorIndex, new Array(width).fill(0));
        }
        bandRows.get(colorIndex)[x] |= 1 << dy;
      }
    }

    for (const [colorIndex, columns] of bandRows) {
      const rowParts = [`#${colorIndex}`];
      for (let x = 0; x < width; x++) {
        rowParts.push(String.fromCharCode(63 + columns[x]));
      }
      rowParts.push("$");
      outputParts.push(rowParts.join(''));
    }
    outputParts.push("-");
    
    // Clear bandRows for this band to free memory
    bandRows.clear();
    
    // Safety check: prevent excessive memory usage
    if (outputParts.length > MAX_SIXEL_PARTS) {
      throw new Error(`Image too complex for sixel rendering (exceeds ${MAX_SIXEL_PARTS} parts)`);
    }
  }

  outputParts.push("\x1b\\");

  return outputParts.join('');
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
        
        // Convert RGBA to RGB first
        let rgb = rgbaToRgb(png.data, png.width, png.height);
        let finalWidth = png.width;
        let finalHeight = png.height;
        
        // Resize if needed BEFORE sixel conversion
        if (png.width > options.maxWidth || png.height > options.maxHeight) {
          const aspectRatio = png.height / png.width;
          let targetWidth = png.width;
          let targetHeight = png.height;
          
          if (png.width > options.maxWidth) {
            targetWidth = options.maxWidth;
            targetHeight = Math.round(options.maxWidth * aspectRatio);
          }
          if (targetHeight > options.maxHeight) {
            targetHeight = options.maxHeight;
            targetWidth = Math.round(options.maxHeight / aspectRatio);
          }
          
          rgb = resizeRgbBuffer(rgb, png.width, png.height, targetWidth, targetHeight);
          finalWidth = targetWidth;
          finalHeight = targetHeight;
          
          stdout.write(`    ${png.width}×${png.height}px → ${finalWidth}×${finalHeight}px\n`);
        } else {
          stdout.write(`    ${png.width}×${png.height}px\n`);
        }
        
        // Generate sixel from the (possibly resized) RGB buffer
        const sixelData = generateSixel(rgb, finalWidth, finalHeight);
        stdout.write(sixelData + "\n");
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
