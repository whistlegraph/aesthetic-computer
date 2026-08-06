#!/usr/bin/env node
// Fetch the verified full-resolution Fuser photograph used by the closing edit.
import { createHash } from "node:crypto";
import { mkdirSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { loadImage } from "canvas";

const ROOT = dirname(fileURLToPath(import.meta.url));
const destination = resolve(ROOT, "out", "event-originals", "PXL_20260613_202338629.jpg");
const source = "https://framerusercontent.com/images/WBySAky0Y6m9S4Mc18G4KFEGwTI.jpg?height=3072&width=4080";
const expectedSha256 = "4aa0e4b0fbd7dae23382fca165c8110876915d0df66a12a19b907510425480b4";

const response = await fetch(source);
if (!response.ok) throw new Error(`event photograph download failed: HTTP ${response.status}`);
const bytes = Buffer.from(await response.arrayBuffer());
const digest = createHash("sha256").update(bytes).digest("hex");
if (digest !== expectedSha256) throw new Error(`event photograph checksum changed: ${digest}`);
mkdirSync(dirname(destination), { recursive: true });
writeFileSync(destination, bytes);
const image = await loadImage(destination);
if (image.width !== 4080 || image.height !== 3072) {
  throw new Error(`event photograph is ${image.width}x${image.height}, expected 4080x3072`);
}
console.log(`${destination} · ${image.width}x${image.height} · ${digest}`);
