#!/usr/bin/env node
// gen-narrative-shots.mjs — animate the 4 narrative key-frames with Seedance 2
// image-to-video (fal, /pop harness). The illustration carries the look; the
// prompt carries MOTION only. See NARRATIVE.md.

import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { generateShot, RATE_PER_SEC } from "../../../pop/lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const BEATS = [
  {
    label: "beat1-painter", image: "key-1-painter.png", out: "beat-1-painter.mp4",
    prompt: "Slow gentle push-in toward the desk; the colorful painting on the laptop screen swirls " +
      "and shimmers softly to life; his hand with the brush moves a little; faint drift of city " +
      "lights and a curtain outside the window; calm, nocturnal, intimate. Subtle, no fast moves.",
  },
  {
    label: "beat2-awakening", image: "key-2-awakening.png", out: "beat-2-awakening.mp4",
    prompt: "The grid of colored note-tiles lights up in sequence and the green waveform pulses across " +
      "the screen; glowing musical notes drift upward; warm colored light blooms outward from the " +
      "laptop into the dark room; a magical awakening. Smooth, joyful.",
  },
  {
    label: "beat3-commons", image: "key-3-commons.png", out: "beat-3-commons.mp4",
    prompt: "Thousands of little screens shimmer and blink alive across the night sky one after another; " +
      "faint threads of light connect them; the small figure stands still looking up; a slow majestic " +
      "upward drift; awe and scale.",
  },
  {
    label: "beat4-invitation", image: "key-4-invitation.png", out: "beat-4-invitation.mp4",
    prompt: "A slow reverent push-in toward the glowing laptop on the stage; the spotlight steadies and " +
      "the screen glows a touch warmer; the silhouette stays still at the edge; gentle, hopeful settle. " +
      "Cinematic, calm.",
  },
];

let cost = 0;
for (const b of BEATS) {
  console.log(`\n▸ ${b.label}`);
  const r = await generateShot({
    image: join(HERE, b.image), prompt: b.prompt,
    duration: "5", ratio: "16:9", resolution: "720p", tier: "fast",
    outPath: join(HERE, b.out), label: b.label,
  });
  if (!r.ok) { console.error(`  ✗ ${b.label}: ${r.error}`); continue; }
  cost += 5 * RATE_PER_SEC.fast;
  console.log(`  ✓ ${b.label}: ${(r.bytes / 1e6).toFixed(1)}MB · ${r.seconds.toFixed(0)}s`);
}
console.log(`\n[narrative-shots] done · ~$${cost.toFixed(2)} @ fast tier`);
