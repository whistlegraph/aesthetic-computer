#!/usr/bin/env node
// gen-shots.mjs — generate the Restless Egg Seedance b-roll shots.
// Source still carries the LOOK; prompt carries MOTION only. Objects/abstract
// only (no faces — the VO carries the founder). 16:9, fast tier @720p.

import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { generateShot, RATE_PER_SEC } from "../../../pop/lib/fal.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const FIG = resolve(HERE, "../figures");

const SHOTS = [
  {
    label: "shot1-emblem",
    image: join(FIG, "cover.png"),
    out: join(HERE, "seedance-1-emblem.mp4"),
    prompt:
      "Slow, reverent cinematic push-in on the laptop. The screen gently glows " +
      "brighter and the small colored mark on it shimmers softly to life. Faint " +
      "dust motes drift through the light. The stone pedestal stays perfectly " +
      "still. Premium, quiet product-reveal motion. Subtle, no fast moves.",
  },
  {
    label: "shot5-boot",
    image: join(HERE, "still-boot.png"),
    out: join(HERE, "seedance-5-boot.mp4"),
    prompt:
      "The dark laptop screen flickers and boots to life: a soft warm glow blooms " +
      "across the screen and spills further onto the keyboard and wooden desk, as " +
      "if an operating system is starting for the very first time. Slow gentle " +
      "push-in, the light grows and steadies. Dust motes drift in the beam. " +
      "Cinematic, dramatic, calm. No text on the screen.",
  },
];

let cost = 0;
for (const s of SHOTS) {
  console.log(`\n▸ ${s.label}`);
  const r = await generateShot({
    image: s.image, prompt: s.prompt,
    duration: "5", ratio: "16:9", resolution: "720p", tier: "fast",
    outPath: s.out, label: s.label,
  });
  if (!r.ok) { console.error(`  ✗ ${s.label}: ${r.error}`); continue; }
  cost += 5 * RATE_PER_SEC.fast;
  console.log(`  ✓ ${s.label}: ${(r.bytes / 1e6).toFixed(1)}MB · ${r.seconds.toFixed(0)}s · seed ${r.seed}`);
}
console.log(`\n[gen-shots] done · ~$${cost.toFixed(2)} @ fast tier`);
