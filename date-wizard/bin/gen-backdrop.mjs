#!/usr/bin/env node
// date-wizard/bin/gen-backdrop.mjs — animate the DateWizard mascot illy into a
// seamless looping video backdrop (Seedance 2.0 via fal). end_image = start
// frame → seamless loop. The illy carries the look; the prompt carries MOTION.
//   node date-wizard/bin/gen-backdrop.mjs [--force]

import { existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { generateShot } from "../../pop/lib/fal-seedance.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ASSETS = resolve(HERE, "../Sources/DateWizard/Assets");
const IMAGE = `${ASSETS}/datewizard-mascot.png`;
const OUT = `${ASSETS}/datewizard-backdrop.mp4`;
const FORCE = process.argv.includes("--force");

if (existsSync(OUT) && !FORCE) { console.log(`✓ cached → ${OUT}`); process.exit(0); }
if (!existsSync(IMAGE)) { console.error(`✗ mascot missing: ${IMAGE}`); process.exit(1); }

const prompt = [
  "Gentle ambient loop, hand-drawn colored-pencil illustration on a clean pure-white background, camera locked and still.",
  "The young wizard breathes softly and sways almost imperceptibly; his robe and hat tip drift gently.",
  "Sand falls slowly through the hourglass in his hand in a thin steady pencil stream.",
  "The hand-drawn calendar-week arc trailing from his wand undulates slowly and the seven colored day-bubbles strung along it pulse and bob gently in sequence, like a week turning.",
  "A small sun and a crescent moon drift slowly along the upper arc; the little floating calendar page sways; the tiny clock's hands tick almost imperceptibly.",
  "Warm, calm, dreamy, looping; pure white background stays white; no camera movement, no zoom, subtle motion only.",
].join(" ");

console.log("▸ datewizard backdrop · Seedance 2.0 (fast) · 1:1 · 720p · 5s · seamless loop");
const r = await generateShot({
  image: IMAGE, endImage: IMAGE, prompt, duration: "5", ratio: "1:1",
  resolution: "720p", tier: "fast", audio: false, outPath: OUT, label: "date-backdrop",
});
if (!r.ok) { console.error(`✗ ${r.error}`); process.exit(1); }
console.log(`✓ ${r.seconds.toFixed(0)}s · ${(r.bytes / 1e6).toFixed(1)}MB → ${OUT}`);
