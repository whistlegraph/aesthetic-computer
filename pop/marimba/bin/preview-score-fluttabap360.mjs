#!/usr/bin/env node
// marimba/bin/preview-score-fluttabap360.mjs — thin driver for the
// fluttabap360 storyline visualizer (1080×1920 vertical insta-story).
//
// fluttabap360 reuses the shared marimbaba storyline engine verbatim —
// preview-score.mjs is already --slug-parameterized and its SECTION_TINTS
// map carries the fluttabap360 beat keys. This wrapper just invokes it
// with the right flags so the pipeline reads as one command, and so the
// fluttabap360 cut can grow its own knobs later without touching
// marimbaba's driver.
//
// Pipeline (run in order):
//   1. node pop/marimba/bin/render-fluttabap360.mjs --no-open
//        → out/fluttabap360.struct.json  (section map)
//        ⚠ NOTE: the fluttabap360 engine does NOT yet emit per-note
//          `events`, so the visualizer's note-block LANES will be empty
//          (the string + panels + backlight + chrome still render). To
//          light the lanes, the engine needs to push events like
//          render-marimbaba.mjs does (see RELEASES / open questions).
//   2. node pop/marimba/bin/gen-sections-fluttabap360.mjs
//        → out/fluttabap360-p-cover.png + 34 -p-sec-<i>-<name>.png panels
//          (11 unique gpt-image-2 masters fanned out across the passes)
//   3. node pop/marimba/bin/preview-score-fluttabap360.mjs   ← this
//        → out/fluttabap360-preview-score-portrait-insta-story.mp4
//
// All flags pass straight through to preview-score.mjs
// (--reel, --start T --frames N for a fast test render, --out, etc).
//
// Usage:
//   node pop/marimba/bin/preview-score-fluttabap360.mjs
//   node pop/marimba/bin/preview-score-fluttabap360.mjs --reel
//   node pop/marimba/bin/preview-score-fluttabap360.mjs --start 154 --frames 30

import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const SCORE = resolve(HERE, "preview-score.mjs");

const passthrough = process.argv.slice(2);
const args = [
  SCORE,
  "--slug", "fluttabap360",
  "--title", "fluttabap360",
  ...passthrough,
];

const r = spawnSync("node", args, { stdio: "inherit" });
process.exit(r.status ?? 1);
