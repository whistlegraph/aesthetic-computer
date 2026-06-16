#!/usr/bin/env node
// date-wizard/bin/gen-icons.mjs — derive light/dark dock icons from the mascot.
// Crops the mascot into a rounded-square (macOS-ish squircle) on a themed
// backdrop: white for light, near-black slate for dark. 1024x1024 each.
//   node date-wizard/bin/gen-icons.mjs [--force]
//
// Requires ImageMagick (`magick` or `convert`) on PATH.

import { existsSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ASSETS = resolve(HERE, "../Sources/DateWizard/Assets");
const MASCOT = `${ASSETS}/datewizard-mascot.png`;
const FORCE = process.argv.includes("--force");

const LIGHT = `${ASSETS}/datewizard-icon-light.png`;
const DARK = `${ASSETS}/datewizard-icon-dark.png`;

if (!existsSync(MASCOT)) { console.error(`✗ mascot missing: ${MASCOT}`); process.exit(1); }
if (existsSync(LIGHT) && existsSync(DARK) && !FORCE) {
  console.log(`✓ cached icons (use --force to regen)`); process.exit(0);
}

// Pick an ImageMagick entrypoint.
function im() {
  for (const cmd of ["magick", "convert"]) {
    try { execFileSync(cmd, ["-version"], { stdio: "ignore" }); return cmd; }
    catch { /* try next */ }
  }
  throw new Error("ImageMagick not found (need `magick` or `convert` on PATH)");
}
const MAGICK = im();

const SIZE = 1024;
const RADIUS = 184;       // corner radius of the squircle
const INSET = 96;         // padding so the figure breathes inside the tile
const INNER = SIZE - INSET * 2;

function build(out, bg) {
  // 1) rounded-rect background tile, 2) mascot fit inside the inset and
  // centered, 3) clip the whole thing to the rounded mask.
  const args = [
    // base background tile
    "-size", `${SIZE}x${SIZE}`, "xc:none",
    "(", "-size", `${SIZE}x${SIZE}`, "xc:none",
      "-fill", bg, "-draw", `roundrectangle 0,0,${SIZE - 1},${SIZE - 1},${RADIUS},${RADIUS}`,
    ")", "-compose", "over", "-composite",
    // mascot, scaled to fit the inner box, centered
    "(", MASCOT, "-resize", `${INNER}x${INNER}`, ")",
    "-gravity", "center", "-compose", "over", "-composite",
    // final rounded clip (knock out any spill past the corners)
    "(", "-size", `${SIZE}x${SIZE}`, "xc:none",
      "-fill", "white", "-draw", `roundrectangle 0,0,${SIZE - 1},${SIZE - 1},${RADIUS},${RADIUS}`,
    ")", "-compose", "DstIn", "-composite",
    out,
  ];
  execFileSync(MAGICK, args, { stdio: "inherit" });
  console.log(`✓ ${out.replace(ASSETS + "/", "")}`);
}

build(LIGHT, "#ffffff");
build(DARK, "#14141c");
