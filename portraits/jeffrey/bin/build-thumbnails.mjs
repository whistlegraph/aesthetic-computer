#!/usr/bin/env node
// Generate 256px thumbnails for confirmed-jeffrey IG selfies and emit a slim
// public manifest.json for the papers.aesthetic.computer/jeffrey/ dashboard.
//
// Reads:  portraits/jeffrey/curated/jeffrey-described.jsonl
// Writes: system/public/assets/jeffreys/whistlegraph/<shortcode>.jpg  (sync to CDN)
//         system/public/papers.aesthetic.computer/jeffrey/manifest.json
//
// Usage:
//   node portraits/jeffrey/bin/build-thumbnails.mjs            # only confirmed (default)
//   node portraits/jeffrey/bin/build-thumbnails.mjs --all      # include unconfirmed described rows
//   node portraits/jeffrey/bin/build-thumbnails.mjs --force    # re-render even if thumb exists
//
// Sharp is resolved out of oven/node_modules so this script can live with the
// rest of the portraits/jeffrey/bin/ pipeline without adding a dependency at
// the repo root.

import { createRequire } from "module";
import { readFileSync, writeFileSync, mkdirSync, existsSync, statSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";

const here = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(here, "..", "..", "..");
const require = createRequire(import.meta.url);

let sharp;
try {
  sharp = require(join(REPO, "oven", "node_modules", "sharp"));
} catch (e) {
  console.error("could not load sharp from oven/node_modules:", e.message);
  console.error("run `cd oven && npm install` first");
  process.exit(1);
}

const SRC_DIR = join(REPO, "portraits", "jeffrey", "ig-archive", "whistlegraph");
const DESCRIBED = join(REPO, "portraits", "jeffrey", "curated", "jeffrey-described.jsonl");
const THUMB_DIR = join(REPO, "system", "public", "assets", "jeffreys", "whistlegraph");
const MANIFEST_OUT = join(
  REPO, "system", "public", "papers.aesthetic.computer", "platter", "jeffrey", "manifest.json",
);
const CDN_PREFIX = "https://assets.aesthetic.computer/jeffreys/whistlegraph";

const args = process.argv.slice(2);
const includeAll = args.includes("--all");
const force = args.includes("--force");

mkdirSync(THUMB_DIR, { recursive: true });
mkdirSync(dirname(MANIFEST_OUT), { recursive: true });

const rows = readFileSync(DESCRIBED, "utf8")
  .split("\n")
  .filter(Boolean)
  .map((line) => JSON.parse(line))
  .filter((row) => includeAll || row.is_jeffrey_confirmed === true);

console.log(`source rows: ${rows.length} (${includeAll ? "all described" : "confirmed only"})`);

let made = 0, skipped = 0, missing = 0, failed = 0;
const manifestRows = [];

for (const row of rows) {
  const src = join(SRC_DIR, row.rel_path);
  const thumb = join(THUMB_DIR, `${row.shortcode}.jpg`);

  if (!existsSync(src)) {
    missing++;
    continue;
  }

  if (existsSync(thumb) && !force && statSync(thumb).size > 0) {
    skipped++;
  } else {
    try {
      await sharp(src)
        .rotate()
        .resize(256, 256, { fit: "inside", withoutEnlargement: true })
        .jpeg({ quality: 80, mozjpeg: true })
        .toFile(thumb);
      made++;
    } catch (e) {
      console.error(`  fail ${row.shortcode}: ${e.message}`);
      failed++;
      continue;
    }
  }

  manifestRows.push({
    shortcode: row.shortcode,
    date: row.date,
    similarity: Number((row.match_similarity ?? 0).toFixed(4)),
    confidence: row.confidence ?? null,
    confirmed: !!row.is_jeffrey_confirmed,
    thumb: `${CDN_PREFIX}/${row.shortcode}.jpg`,
    expression: row.subject?.expression ?? null,
    pose: row.subject?.pose ?? null,
    description: row.subject?.description ?? null,
    location: row.environment?.location ?? null,
    style: row.photography?.style ?? null,
    framing: row.photography?.framing ?? null,
    domain: row.domain ?? null,
    tags: row.tags ?? [],
    caption: row.caption_hint ?? null,
    n_other_people: row.n_other_people ?? 0,
  });
}

manifestRows.sort((a, b) => (a.date < b.date ? 1 : -1));

writeFileSync(
  MANIFEST_OUT,
  JSON.stringify(
    {
      generated_at: new Date().toISOString(),
      source: "portraits/jeffrey/curated/jeffrey-described.jsonl",
      cdn_prefix: CDN_PREFIX,
      counts: {
        rows: manifestRows.length,
        confirmed: manifestRows.filter((r) => r.confirmed).length,
      },
      rows: manifestRows,
    },
    null,
    2,
  ),
);

console.log(`thumbs: made=${made} skipped=${skipped} failed=${failed} missing-source=${missing}`);
console.log(`manifest: ${manifestRows.length} rows → ${MANIFEST_OUT}`);
console.log(`next: npm run assets:sync:up`);
