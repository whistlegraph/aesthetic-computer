#!/usr/bin/env node
// americomputadora/bin/gen-motion-canvas.mjs — Spotify Canvas motion pass.
// Two reel panels (cover-android/gens/reel-shot-*.png) become Seedance 2.0
// image-to-video candidates: 8 s · 9:16 · silent — the Canvas spec
// (3–8 s loop, chrome-free). Post pass scales to 1080×1920 h264.
//
// Usage:
//   node bin/gen-motion-canvas.mjs            # gen missing
//   node bin/gen-motion-canvas.mjs --force    # re-roll
//   node bin/gen-motion-canvas.mjs --only found

import { existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { generateShot } from "../../lib/fal-seedance.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const GENS = `${LANE}/cover-android/gens`;
const MOTION = `${GENS}/motion`;
mkdirSync(MOTION, { recursive: true });

const argv = process.argv.slice(2);
const force = argv.includes("--force");
const only = argv.includes("--only") ? argv[argv.indexOf("--only") + 1].split(",") : null;

// the drawing stays a drawing while it moves: pencil tooth and paper grain
// hold still on the surface; the woven digital fibers carry slow traveling
// pulses of light. never a photograph, never lens flare, no motion blur.
const MEDIUM = `The image is a colored-pencil drawing on cream paper with glowing fiber-optic threads woven through the hatching — as it moves, the pencil tooth and paper grain stay fixed like a living illustration; the hair-thin cyan, magenta and warm-white fibers carry slow traveling pulses of light along their length. Never a photograph, never cinematic, no lens flares, no motion blur — painted light only. Gentle, loopable motion: the last moments drift back toward the first frame.`;

// crayon shots hold a different medium: waxy crayon on cream paper.
const MEDIUM_CRAYON = `The image is a soft waxy crayon drawing on warm cream paper — as it moves, the crayon grain and paper tooth stay perfectly fixed like a living children's-book illustration; outlines stay wobbly and hand-drawn. Never a photograph, never 3d, never cinematic, no lens flares, no motion blur. Gentle, loopable motion: the last moments drift back toward the first frame.`;

const SHOTS = {
  patriot: {
    image: `${GENS}/v9-crayon-patriot.png`,
    medium: MEDIUM_CRAYON,
    motion: `The tiny sailor-hatted pixies lean back and tug their ropes in a slow shared rhythm — lines pulling taut, then easing slack. The three-headed robot sways very gently with each tug, its heads drifting slightly out of phase: the curly-haired head lifts singing skyward, the dark-bob head turns a touch further away, the center face blinks once, slowly, calm. The two chest badges stay put. The little crayon fireworks in the upper corners bloom, sparkle, fade, and re-bloom. A quiet peekaboo charm — playful, tender, nothing startles. The sway settles back to the opening pose so the loop closes seamlessly.`,
  },
  found: {
    image: `${GENS}/reel-shot-1-found.png`,
    motion: `Night hush: the giant sleeping robot breathes almost imperceptibly, its chest glow swelling and dimming slowly under the panel seams like a heartbeat. The tiny winged pixsies hover in place on soft wing-flutters, their lanterns swaying and flickering warm light across the grass. Grass blades sway gently; a few luminous fiber pulses travel through the meadow weave. The camera drifts forward very slowly. Nothing startles — a lullaby loop.`,
  },
  strain: {
    image: `${GENS}/reel-shot-4-strain.png`,
    motion: `The giant three-headed robot strains gently against the taut staked ropes in a slow periodic heave — rising a little, easing back — the red white and blue chest light pulsing brighter with each pull. Hand-drawn music notes drift steadily up from the three singing mouths. Pixsies swing lightly on the taut lines and haul in rhythm. Fiber pulses race along the ropes and wing veins. The camera pushes in very slowly. The heave settles back to the opening pose so the loop closes.`,
  },
};

for (const [name, s] of Object.entries(SHOTS)) {
  if (only && !only.includes(name)) continue;
  const raw = `${MOTION}/canvas-${name}-raw.mp4`;
  const out = `${MOTION}/americomputadora-canvas-${name}.mp4`;
  if (existsSync(out) && !force) { console.log(`· cached ${out}`); continue; }
  console.log(`▸ seedance canvas shot: ${name} (8s · 9:16 · silent)`);
  const r = await generateShot({
    image: s.image,
    prompt: `${s.motion}\n\n${s.medium ?? MEDIUM}`,
    duration: "8", ratio: "9:16", resolution: "720p",
    tier: "fast", audio: false,
    outPath: raw, label: `canvas-${name}`,
  });
  if (!r.ok) { console.error(`✗ ${name}: ${r.error}`); continue; }
  // Canvas post: 1080×1920, h264 yuv420p, silent, ≤8 s.
  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", raw, "-t", "8", "-an",
    "-vf", "scale=1080:1920:flags=lanczos,setsar=1",
    "-c:v", "libx264", "-pix_fmt", "yuv420p", "-crf", "18", "-preset", "slow",
    out,
  ], { stdio: "inherit" });
  if (ff.status === 0) console.log(`✓ ${out}`);
}
