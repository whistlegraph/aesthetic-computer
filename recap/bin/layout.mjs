#!/usr/bin/env node
// layout.mjs — compute per-segment chrome layout from cv bboxes.
//
// Reads:
//   recap/out/cv/<seg>.json    — face / laptop / shirtLogo bboxes
//   recap/audience/<name>.mjs  — exports `solveLayout(cv)`
//   recap/out/segments.json    — segment timing
//
// Writes:
//   recap/out/layouts.json     — { <segName>: { chapter, subtitle, piano,
//                                  startSec, endSec } }
//
// Subtitles.mjs and waltz-overlay.mjs both read this to position the
// pill and piano roll per segment without mid-frame collisions.
//
// Usage: node bin/layout.mjs <audience-name>

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");

const audienceName = process.argv[2];
if (!audienceName) {
  console.error("usage: layout.mjs <audience-name>");
  process.exit(2);
}

const mod = await import(`${ROOT}/audience/${audienceName}.mjs`);
const audience = mod.audience || mod.default;
const solveLayout = mod.solveLayout;
if (typeof solveLayout !== "function") {
  console.error(`✗ audience '${audienceName}' does not export solveLayout — using default top placement`);
  // Write empty layouts so downstream tools fall back gracefully.
  writeFileSync(`${ROOT}/out/layouts.json`, JSON.stringify({}, null, 2));
  process.exit(0);
}

const segments = JSON.parse(readFileSync(`${ROOT}/out/segments.json`, "utf8"));
const layouts = {};
for (const seg of segments) {
  const cvPath = `${ROOT}/out/cv/${seg.name}.json`;
  const cv = existsSync(cvPath) ? JSON.parse(readFileSync(cvPath, "utf8")) : null;
  const layout = solveLayout(cv);
  // Pull the chapter color out of the audience config so downstream
  // tools (subtitles.mjs, build-filter.mjs) can tint per-segment.
  const slide = (audience && audience.slides && audience.slides[seg.name]) || null;
  const colorAddress = slide && slide.colorAddress;
  layouts[seg.name] = {
    startSec: seg.startSec,
    endSec: seg.endSec,
    chapter: layout.chapter,
    subtitle: layout.subtitle,
    piano: layout.piano,
    color: colorAddress || null,
  };
}

writeFileSync(`${ROOT}/out/layouts.json`, JSON.stringify(layouts, null, 2));
console.log(`✓ ${ROOT}/out/layouts.json · ${Object.keys(layouts).length} segments`);
for (const [name, l] of Object.entries(layouts)) {
  console.log(`  ${name.padEnd(22)} chapter=${l.chapter.mode.padEnd(13)} sub-y=${String(l.subtitle.y).padStart(5)} piano=${l.piano.hidden ? "hidden" : `(${l.piano.x},${l.piano.y})`}`);
}
