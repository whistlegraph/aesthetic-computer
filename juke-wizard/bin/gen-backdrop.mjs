#!/usr/bin/env node
// juke-wizard/bin/gen-backdrop.mjs — animate the JukeWizard mascot illy into
// a seamless looping video backdrop (Seedance 2.0 via fal). The illy carries
// the look; the prompt carries only gentle ambient MOTION. end_image = the
// start image so the last frame returns home → the loop is seamless.
//
//   node juke-wizard/bin/gen-backdrop.mjs            # cached if backdrop exists
//   node juke-wizard/bin/gen-backdrop.mjs --force    # regenerate

import { existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { generateShot } from "../../pop/lib/fal-seedance.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ASSETS = resolve(HERE, "../Sources/JukeWizard/Assets");
const IMAGE = `${ASSETS}/jukewizard-mascot.png`;
const OUT = `${ASSETS}/jukewizard-backdrop.mp4`;
const FORCE = process.argv.includes("--force");

if (existsSync(OUT) && !FORCE) {
  console.log(`✓ cached backdrop → ${OUT} (use --force to regen)`);
  process.exit(0);
}
if (!existsSync(IMAGE)) { console.error(`✗ mascot missing: ${IMAGE}`); process.exit(1); }

const prompt = [
  "Gentle ambient loop, hand-drawn illustration staying on the warm cream paper, camera locked and still.",
  "The little wizard breathes softly and sways almost imperceptibly; his beard and robe drift gently.",
  "The jukebox's colored lights slowly pulse and cycle through the rainbow and its record turns.",
  "The gold star on his wand twinkles and gives off slow soft sparkles.",
  "The floating comment-bubble ribbons drift slowly and a few tiny musical notes rise and fade.",
  "Warm, cozy, calm, dreamy, looping; no camera movement, no zoom, subtle motion only.",
].join(" ");

console.log("▸ jukewizard backdrop · Seedance 2.0 (fast) · 1:1 · 720p · 5s · seamless loop");
const r = await generateShot({
  image: IMAGE,
  endImage: IMAGE,        // same start/end frame → seamless loop
  prompt,
  duration: "5",
  ratio: "1:1",
  resolution: "720p",
  tier: "fast",
  audio: false,
  outPath: OUT,
  label: "backdrop",
});
if (!r.ok) { console.error(`✗ ${r.error}`); process.exit(1); }
console.log(`✓ ${r.seconds.toFixed(0)}s · ${(r.bytes / 1e6).toFixed(1)}MB → ${OUT}`);
