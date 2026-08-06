#!/usr/bin/env node
// fal.ai Seedance 2.0 visual-motion pass for Aesthetic Wing.
// The still cut remains the source of truth for timing, captions, VO, music,
// and SFX. This driver only replaces each illustrated panel with a motion take.
//
// Usage:
//   node gen-motion.mjs --dry-run
//   node gen-motion.mjs
//   node gen-motion.mjs --only lift --force
//   node build.mjs  # automatically uses generated/picked takes

import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { runMotionCli } from "../../../../pop/lib/motion-pipeline.mjs";
import {
  COLORED_PENCIL_TOOTH_MOTION,
  FRAMING_IG_STORY_PORTRAIT_MOTION,
} from "../../../../pop/lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const GENS = resolve(HERE, "../gens");
const meta = JSON.parse(readFileSync(join(HERE, "build/meta.json"), "utf8"));
const structPath = join(HERE, "motion-struct.json");
writeFileSync(structPath, JSON.stringify({
  sections: meta.slides.map((s) => ({ name: s.name, startSec: s.from, endSec: s.to })),
}, null, 2));

const MEDIUM = [
  "Calm deadpan future-transport advertisement. Every movement controlled and unhurried.",
  "Preserve the rider's identity, the exact single-wheel vehicle geometry, six guarded ducts, cream petals, purple spine, and blue-magenta translucent fields. Never invent extra wheels, exposed rotors, handlebars, wings on the rider, or a jetpack.",
  "The colored-pencil and gouache paper tooth remains fixed to the illustrated world; no photorealistic restyling, melting contours, or text generation.",
  FRAMING_IG_STORY_PORTRAIT_MOTION,
  COLORED_PENCIL_TOOTH_MOTION,
].join("\n\n");

const PANELS = {
  road: join(GENS, "v8-road-mode.png"),
  unfold: join(GENS, "v5-wing-wakes.png"),
  lift: join(GENS, "v6-more-lift.png"),
  settle: join(GENS, "v7-settle.png"),
  design: join(GENS, "v4-design-bible.png"),
};

const SHOTS = {
  road: {
    motion: `Low tracking camera keeps pace beside Jeffrey. The single central tire turns steadily on the road while he leans forward by a fraction, knees softly absorbing the surface. His shirt and hair move in the morning air. The folded cream decks stay locked in compact road mode; the tiny green cursor glows. Houses drift backward slowly. No lift yet.`,
  },
  unfold: {
    motion: `The camera pushes in very slowly. Beneath Jeffrey's steady feet, both cream wing petals hinge outward from the purple spine in one precise symmetrical mechanical action. Guarded duct fans become visible and begin to spin behind their fixed guards. Dust moves outward; his shirt lifts. The central tire unloads and rises from one inch to one foot above the concrete while the vehicle stays perfectly level.`,
  },
  lift: {
    motion: `Sustained powered hover high in the empty concrete channel. The vehicle rises slowly another two feet and then holds level, leaving a large stable air gap. Jeffrey remains serene with loose arms and soft knees. The guarded fans shimmer behind their stationary cages; hair and shirt respond to downwash. Dust and dry leaves travel outward in a broad ring far below while the separated shadow trembles slightly. The camera cranes upward by a fraction.`,
  },
  settle: {
    motion: `A gentle controlled descent through the final eighteen inches. The single tire approaches the pavement and touches with one soft compression. As weight returns to the wheel, both cream wing petals fold halfway toward road mode and the guarded fans slow. Jacaranda petals drift outward and then settle. Jeffrey looks down, relaxed; the green cursor dims.`,
  },
  design: {
    motion: `A nearly still industrial-design folio. The three drawings remain exact and do not morph. A soft raking light moves down the warm paper, revealing pencil tooth; the tiny green status squares breathe once. The camera makes a very slow straight push toward the unfolded middle view. All lettering and geometry remain fixed and stable.`,
  },
};

await runMotionCli({
  slug: "aesthetic-wing-v1",
  laneDir: HERE,
  motionDir: join(HERE, "motion"),
  structPath,
  panelFor: (name) => PANELS[name],
  shots: SHOTS,
  mediumMotion: MEDIUM,
  ratio: "9:16",
  audio: join(HERE, "aesthetic-wing-sound.wav"),
  finalOut: join(HERE, "motion/aesthetic-wing-seedance-visuals.mp4"),
});
