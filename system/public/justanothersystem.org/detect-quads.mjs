#!/usr/bin/env node
// Detect artwork quads in slideshow images using Claude vision.
// Downloads each image, sends to Claude, gets back corner coordinates.
// Usage: node detect-quads.mjs [--all | --missing | --file IMG_1234.jpeg]

import Anthropic from "@anthropic-ai/sdk";
import { readFileSync, writeFileSync } from "fs";

const MANIFEST_URL =
  "https://justanothersystem.org/assets/justanothersystem/slideshow/manifest.json";
const ASSET_BASE =
  "https://justanothersystem.org/assets/justanothersystem/slideshow/";

const client = new Anthropic();

async function detectQuad(imageUrl, filename) {
  // Download image as base64
  const res = await fetch(imageUrl);
  if (!res.ok) {
    console.error(`  Failed to fetch ${filename}: ${res.status}`);
    return null;
  }
  const buf = await res.arrayBuffer();
  const base64 = Buffer.from(buf).toString("base64");
  const contentType = res.headers.get("content-type") || "image/jpeg";

  const response = await client.messages.create({
    model: "claude-sonnet-4-20250514",
    max_tokens: 1024,
    messages: [
      {
        role: "user",
        content: [
          {
            type: "image",
            source: { type: "base64", media_type: contentType, data: base64 },
          },
          {
            type: "text",
            text: `This is a photo that may contain one or more paintings, drawings, or artworks. Your job is to find the single most prominent rectangular artwork in the image and return its four corner coordinates as percentages of image width/height.

Rules:
- If there are multiple artworks visible, pick the LARGEST one or the one most clearly in focus.
- If no clear rectangular artwork is visible (e.g. it's a landscape, a portrait photo, or abstract scene without a distinct framed/mounted piece), return null.
- Corners should be in order: top-left, top-right, bottom-right, bottom-left.
- Coordinates are percentages (0-100) of image width (x) and height (y).
- Account for perspective distortion — the corners may not form a perfect rectangle.

Respond with ONLY valid JSON, no markdown, no explanation:
{"corners": [[tl_x, tl_y], [tr_x, tr_y], [br_x, br_y], [bl_x, bl_y]]}
or
null`,
          },
        ],
      },
    ],
  });

  const text = response.content[0]?.text?.trim();
  try {
    const parsed = JSON.parse(text);
    if (parsed === null) return null;
    if (
      parsed.corners &&
      Array.isArray(parsed.corners) &&
      parsed.corners.length === 4
    ) {
      // Round to 1 decimal
      return parsed.corners.map(([x, y]) => [
        Math.round(x * 10) / 10,
        Math.round(y * 10) / 10,
      ]);
    }
  } catch (e) {
    console.error(`  Failed to parse response for ${filename}: ${text}`);
  }
  return null;
}

async function main() {
  const args = process.argv.slice(2);
  const mode = args[0] || "--all";
  const specificFile = mode === "--file" ? args[1] : null;

  // Fetch manifest
  const res = await fetch(MANIFEST_URL);
  const manifest = await res.json();
  const images = manifest.images;

  let toProcess;
  if (specificFile) {
    toProcess = images.filter((img) => img.file === specificFile);
  } else if (mode === "--missing") {
    toProcess = images.filter((img) => !img.corners);
  } else {
    toProcess = [...images];
  }

  console.log(`Processing ${toProcess.length} of ${images.length} images...`);

  for (const entry of toProcess) {
    const filename = entry.file;
    const url = ASSET_BASE + filename;
    console.log(`\n${filename}...`);

    const corners = await detectQuad(url, filename);
    if (corners) {
      entry.corners = corners;
      console.log(`  corners: ${JSON.stringify(corners)}`);
    } else {
      // Remove corners if detection says null (no artwork found)
      if (mode === "--all") {
        delete entry.corners;
        console.log("  no artwork detected");
      } else {
        console.log("  no artwork detected (keeping existing)");
      }
    }

    // Small delay to avoid rate limiting
    await new Promise((r) => setTimeout(r, 500));
  }

  // Write updated manifest
  const outPath = new URL("./manifest-updated.json", import.meta.url).pathname;
  writeFileSync(outPath, JSON.stringify(manifest, null, 2) + "\n");
  console.log(`\nWrote ${outPath}`);
}

main().catch(console.error);
