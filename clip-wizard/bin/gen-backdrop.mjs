#!/usr/bin/env node
// clip-wizard/bin/gen-backdrop.mjs — animate the ClipWizard mascot illy into a
// seamless looping video backdrop (Seedance 2.0 via fal). end_image = start
// frame → the loop returns home seamlessly. The illy carries the look; the
// prompt carries only gentle ambient MOTION.
//   node clip-wizard/bin/gen-backdrop.mjs [--force]

import { existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { generateShot } from "../../pop/lib/fal-seedance.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ASSETS = resolve(HERE, "../Sources/ClipWizard/Assets");
const IMAGE = `${ASSETS}/clipwizard-mascot.png`;
const OUT = `${ASSETS}/clipwizard-backdrop.mp4`;
const FORCE = process.argv.includes("--force");

if (existsSync(OUT) && !FORCE) { console.log(`✓ cached → ${OUT}`); process.exit(0); }
if (!existsSync(IMAGE)) { console.error(`✗ mascot missing: ${IMAGE}`); process.exit(1); }

const prompt = [
  "Gentle ambient loop, hand-drawn illustration staying on the warm cream paper, camera locked and still.",
  "The little wizard breathes softly and sways almost imperceptibly; his robe and hat tip shift gently.",
  "The strip of film/celluloid curling around him undulates slowly and its few frames flicker with warm light, the tiny glowing scenes inside shimmering and shifting.",
  "The clapperboard top opens and closes once, slowly. A few soft dust motes drift through the warm light.",
  "Warm, cozy, calm, dreamy, looping; no camera movement, no zoom, subtle motion only.",
].join(" ");

console.log("▸ clipwizard backdrop · Seedance 2.0 (fast) · 1:1 · 720p · 5s · seamless loop");
const r = await generateShot({
  image: IMAGE, endImage: IMAGE, prompt, duration: "5", ratio: "1:1",
  resolution: "720p", tier: "fast", audio: false, outPath: OUT, label: "clip-backdrop",
});
if (!r.ok) { console.error(`✗ ${r.error}`); process.exit(1); }
console.log(`✓ ${r.seconds.toFixed(0)}s · ${(r.bytes / 1e6).toFixed(1)}MB → ${OUT}`);
