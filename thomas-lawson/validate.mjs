#!/usr/bin/env node
// Validates catalog.json against the JSON schemas and reports completeness.

import Ajv from "ajv";
import addFormats from "ajv-formats";
import { readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));

function loadJSON(path) {
  return JSON.parse(readFileSync(join(__dirname, path), "utf-8"));
}

// Load schemas
const artworkSchema = loadJSON("schema/artwork.schema.json");
const exhibitionSchema = loadJSON("schema/exhibition.schema.json");
const writingSchema = loadJSON("schema/writing.schema.json");
const catalogSchema = loadJSON("schema/catalog.schema.json");

// Load catalog data
let catalog;
try {
  catalog = loadJSON("data/catalog.json");
} catch (e) {
  console.error("No catalog.json found. Run `node scrape.mjs` first.");
  process.exit(1);
}

// Set up validator
const ajv = new Ajv({ allErrors: true, strict: false, validateSchema: false });
addFormats(ajv);

// Register schemas so $ref works
ajv.addSchema(artworkSchema, "artwork.schema.json");
ajv.addSchema(exhibitionSchema, "exhibition.schema.json");
ajv.addSchema(writingSchema, "writing.schema.json");

const validateCatalog = ajv.compile(catalogSchema);
const validateArtwork = ajv.compile(artworkSchema);
const validateExhibition = ajv.compile(exhibitionSchema);
const validateWriting = ajv.compile(writingSchema);

console.log("Thomas Lawson Catalog Validator");
console.log("==============================\n");

// Validate top-level catalog
const catalogValid = validateCatalog(catalog);
if (!catalogValid) {
  console.log("✗ Catalog structure invalid:");
  for (const err of validateCatalog.errors) {
    console.log(`  ${err.instancePath} ${err.message}`);
  }
} else {
  console.log("✓ Catalog structure valid\n");
}

// Validate each artwork
let artworkErrors = 0;
const optionalFields = [
  "year",
  "medium",
  "artMedium",
  "artworkSurface",
  "dimensions",
  "series",
  "description",
  "collection",
];
const completeness = {};
for (const f of optionalFields) completeness[f] = 0;

for (const artwork of catalog.artworks) {
  const valid = validateArtwork(artwork);
  if (!valid) {
    artworkErrors++;
    console.log(`✗ Artwork "${artwork.title}" (${artwork.id}):`);
    for (const err of validateArtwork.errors) {
      console.log(`    ${err.instancePath} ${err.message}`);
    }
  }

  for (const f of optionalFields) {
    if (artwork[f] !== null && artwork[f] !== undefined) completeness[f]++;
  }
}

const totalArtworks = catalog.artworks.length;
if (artworkErrors === 0) {
  console.log(`✓ All ${totalArtworks} artworks pass schema validation\n`);
} else {
  console.log(
    `\n${artworkErrors}/${totalArtworks} artworks have validation errors\n`,
  );
}

// Validate each exhibition
let exhibitionErrors = 0;
for (const exhibition of catalog.exhibitions) {
  const valid = validateExhibition(exhibition);
  if (!valid) {
    exhibitionErrors++;
    console.log(`✗ Exhibition "${exhibition.title}" (${exhibition.id}):`);
    for (const err of validateExhibition.errors) {
      console.log(`    ${err.instancePath} ${err.message}`);
    }
  }
}

const totalExhibitions = catalog.exhibitions.length;
if (exhibitionErrors === 0) {
  console.log(
    `✓ All ${totalExhibitions} exhibitions pass schema validation\n`,
  );
} else {
  console.log(
    `${exhibitionErrors}/${totalExhibitions} exhibitions have validation errors\n`,
  );
}

// Validate each writing
let writingErrors = 0;
const writings = catalog.writings || [];
for (const writing of writings) {
  const valid = validateWriting(writing);
  if (!valid) {
    writingErrors++;
    console.log(`✗ Writing "${writing.title}" (${writing.id}):`);
    for (const err of validateWriting.errors) {
      console.log(`    ${err.instancePath} ${err.message}`);
    }
  }
}

const totalWritings = writings.length;
if (totalWritings > 0) {
  if (writingErrors === 0) {
    console.log(`✓ All ${totalWritings} writings pass schema validation\n`);
  } else {
    console.log(
      `${writingErrors}/${totalWritings} writings have validation errors\n`,
    );
  }
}

// Completeness report
console.log("Field Completeness (artworks):");
console.log("─".repeat(40));
for (const f of optionalFields) {
  const pct = totalArtworks
    ? Math.round((completeness[f] / totalArtworks) * 100)
    : 0;
  const bar =
    "█".repeat(Math.round(pct / 5)) +
    "░".repeat(20 - Math.round(pct / 5));
  console.log(
    `  ${f.padEnd(16)} ${bar} ${pct}% (${completeness[f]}/${totalArtworks})`,
  );
}

// Writing completeness
if (totalWritings > 0) {
  const wFields = ["year", "publication", "url", "author"];
  console.log("\nField Completeness (writings):");
  console.log("─".repeat(40));
  for (const f of wFields) {
    const count = writings.filter(
      (w) => w[f] !== null && w[f] !== undefined,
    ).length;
    const pct = Math.round((count / totalWritings) * 100);
    const bar =
      "█".repeat(Math.round(pct / 5)) +
      "░".repeat(20 - Math.round(pct / 5));
    console.log(
      `  ${f.padEnd(16)} ${bar} ${pct}% (${count}/${totalWritings})`,
    );
  }
}

// Summary stats
console.log("\nSummary:");
console.log(`  Artworks:    ${totalArtworks}`);
console.log(`  Exhibitions: ${totalExhibitions}`);
console.log(`  Writings:    ${totalWritings}`);
console.log(`  Scraped at:  ${catalog.scrapedAt}`);
console.log(`  Version:     ${catalog.version}`);

// Sections breakdown
const sections = {};
for (const a of catalog.artworks) {
  sections[a.section] = (sections[a.section] || 0) + 1;
}
console.log("\n  By section:");
for (const [section, count] of Object.entries(sections)) {
  console.log(`    ${section}: ${count}`);
}

// Series breakdown
const series = {};
for (const a of catalog.artworks) {
  if (a.series) series[a.series] = (series[a.series] || 0) + 1;
}
if (Object.keys(series).length > 0) {
  console.log("\n  Detected series:");
  for (const [name, count] of Object.entries(series)) {
    console.log(`    "${name}": ${count} works`);
  }
}

// Writing categories
if (totalWritings > 0) {
  const categories = {};
  for (const w of writings) {
    categories[w.category] = (categories[w.category] || 0) + 1;
  }
  console.log("\n  By writing category:");
  for (const [cat, count] of Object.entries(categories)) {
    console.log(`    ${cat}: ${count}`);
  }
}
