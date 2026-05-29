#!/usr/bin/env node
// pop/bin/crisp-pixel-sections.mjs — downscale + crisp the raw gpt-image-2
// pixel-art outputs into a tight true-pixel grid.
//
// Input:  pop/<lane>/out/<slug>-p-sec-<NN>-<id>.pixel-raw.png
// Output: pop/<lane>/out/<slug>-p-sec-<NN>-<id>.pixel.png
//         (and mirrored to system/public/assets/pop/<slug>/sec-<N>.pixel.png)
//
// Uses ImageMagick (`magick`) with `-filter Box -resize WxH!` to do a clean
// nearest-neighbor-like downscale that snaps every pixel to the new grid
// (no smoothing). Optionally quantizes to a small indexed palette via
// `-colors 32 -dither None` for the indexed-look crisp.
//
// Usage:
//   node pop/bin/crisp-pixel-sections.mjs --lane hellsine --slug hellsine
//   node pop/bin/crisp-pixel-sections.mjs --lane hellsine --slug hellsine --force
//   node pop/bin/crisp-pixel-sections.mjs --lane hellsine --slug hellsine --size 192x288
//   node pop/bin/crisp-pixel-sections.mjs --lane hellsine --slug hellsine --colors 32

import { readdirSync, existsSync, mkdirSync, statSync, copyFileSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP  = resolve(HERE, "..");
const REPO = resolve(POP, "..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}

const LANE   = flags.lane;
const SLUG   = flags.slug;
const FORCE  = flags.force === true;
const SIZE   = typeof flags.size === "string" ? flags.size : "192x288";
const COLORS = typeof flags.colors === "string" ? parseInt(flags.colors, 10) : 0;

if (!LANE || !SLUG) {
  console.error("usage: node pop/bin/crisp-pixel-sections.mjs --lane <name> --slug <name> [--force] [--size 192x288] [--colors 32]");
  process.exit(1);
}

const SRC_DIR = `${POP}/${LANE}/out`;
const ASSET_DIR = `${REPO}/system/public/assets/pop/${SLUG}`;

if (!existsSync(SRC_DIR)) {
  console.error(`✗ source dir missing: ${SRC_DIR}`);
  process.exit(1);
}
mkdirSync(ASSET_DIR, { recursive: true });

// Discover *.pixel-raw.png files matching <slug>-p-sec-NN-<id>.pixel-raw.png
const pattern = new RegExp(`^${SLUG}-p-sec-(\\d+)-([a-z0-9-]+)\\.pixel-raw\\.png$`, "i");
const raws = readdirSync(SRC_DIR)
  .filter((f) => pattern.test(f))
  .sort();

if (raws.length === 0) {
  console.error(`✗ no pixel-raw files matching ${SLUG}-p-sec-NN-<id>.pixel-raw.png in ${SRC_DIR}`);
  process.exit(1);
}

console.log(`▸ crisping ${raws.length} panel(s) · ${SIZE}${COLORS ? ` · ${COLORS} colors` : ""}`);

let processed = 0, cached = 0;
for (const raw of raws) {
  const m = raw.match(pattern);
  const padIdx = m[1];
  const id     = m[2];
  const src    = `${SRC_DIR}/${raw}`;
  const out    = `${SRC_DIR}/${SLUG}-p-sec-${padIdx}-${id}.pixel.png`;
  // Asset mirror — strip the leading zero for the human-friendly index.
  const secN   = parseInt(padIdx, 10);
  const mirror = `${ASSET_DIR}/sec-${secN}.pixel.png`;

  const needs = FORCE || !existsSync(out) || statSync(src).mtimeMs > statSync(out).mtimeMs;

  if (needs) {
    const args = [src, "-filter", "Box", "-resize", `${SIZE}!`];
    if (COLORS > 0) args.push("-colors", String(COLORS), "-dither", "None");
    args.push(out);
    execFileSync("magick", args, { stdio: "inherit" });
    processed++;
    console.log(`✓ ${raw} → ${basename(out)}`);
  } else {
    cached++;
    console.log(`✓ cached → ${basename(out)}`);
  }

  // Mirror to system/public/assets/pop/<slug>/
  const mirrorNeeds = FORCE || !existsSync(mirror) || statSync(out).mtimeMs > statSync(mirror).mtimeMs;
  if (mirrorNeeds) {
    copyFileSync(out, mirror);
    console.log(`  → assets/pop/${SLUG}/${basename(mirror)}`);
  }
}

console.log(`\n✓ crisped ${processed} new, ${cached} cached · ${SIZE}${COLORS ? ` · ${COLORS} colors` : ""}`);
console.log(`  out: ${SRC_DIR}/${SLUG}-p-sec-NN-<id>.pixel.png`);
console.log(`  mirror: ${ASSET_DIR}/sec-N.pixel.png`);
