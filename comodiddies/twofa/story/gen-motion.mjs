#!/usr/bin/env node
// gen-motion.mjs — Seedance 2.0 motion pass for the 2FA Brush story.
// Thin driver over pop/lib/motion-pipeline.mjs (same shape as
// pop/marimba/bin/gen-motion-marimbaba.mjs): the SHOTS table below is
// the creative surface; pricing, generation, take archiving and picks
// live in the shared pipeline.
//
// Sections come from build/meta.json (written by build.mjs from the VO
// alignment). The sheet end card is no longer the flat typeset page —
// it's the POV illy (gens/v8-sheet-pov.png, the document fed back
// through gen-promo: jeffrey holding the printed sheet up at the sink,
// brush in the other hand) so even the end card is a motion shot.
//
// ASSEMBLY IS NOT --assemble HERE: the pipeline's assembler is
// landscape-hardcoded. build.mjs cuts the story — it picks up takes
// from motion/ automatically (takes.json respected) and zoompan-falls
// back per slide.
//
// Usage:
//   node gen-motion.mjs --dry-run            # prompts + cost
//   node gen-motion.mjs                      # gen missing shots
//   node gen-motion.mjs --only auth --force  # re-roll (old take archived)
//   then: node build.mjs
//
// brush-close starts from build/panel-brush-close.png — a magick crop
// of the hero panel — because Seedance opens exactly on its input
// frame, and two back-to-back cuts from the same panel would jump-cut
// to an identical frame. Regenerate it with:
//   magick ../gens/v3-hero.png -crop 480x853+180+200 +repage build/panel-brush-close.png

import { readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { runMotionCli } from "../../../pop/lib/motion-pipeline.mjs";
import {
  COLORED_PENCIL_TOOTH_MOTION,
  FRAMING_IG_STORY_PORTRAIT_MOTION,
} from "../../../pop/lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const GENS = resolve(HERE, "../gens");

const MEDIUM_MOTION = [
  `Soft diegetic morning light only. Calm deadpan product-ad pacing — every motion small, unhurried, domestic.`,
  FRAMING_IG_STORY_PORTRAIT_MOTION,
  COLORED_PENCIL_TOOTH_MOTION,
].join("\n\n");

// struct.json for the pipeline = build.mjs slide timings, sheet dropped.
const meta = JSON.parse(readFileSync(join(HERE, "build/meta.json"), "utf8"));
const sections = meta.slides
  .map((s) => ({ name: s.name, startSec: s.from, endSec: s.to }));
const structPath = join(HERE, "motion-struct.json");
writeFileSync(structPath, JSON.stringify({ sections }, null, 2));

const PANELS = {
  brush: join(GENS, "v3-hero.png"),
  auth: join(GENS, "v7-auth.png"),
  coding: join(GENS, "v5-coding.png"),
  "brush-close": join(HERE, "build/panel-brush-close.png"),
  sheet: join(GENS, "v8-sheet-pov.png"),
};

// Mostly cuts; auth MORPHS into the coding panel — same room, same
// figure, a camera swing — the one transition shape that doesn't risk
// doubled figures (see pop-motion-pipeline memory). build.mjs
// head-trims morph shots so the arrival frame kisses the next cut.
const SHOTS = {
  brush: {
    motion:
`Morning bathroom, a man brushing his teeth with the teal electric toothbrush. The brush trembles with a tiny fast electric vibration against his mouth; his jaw works slowly, eyes half-closed and unbothered. His other hand rests flat on the counter. The yellow towel sways almost imperceptibly; dust motes drift through the window light; the little succulent stays still. The camera pushes in very slowly toward him. Nothing dramatic happens — it is just brushing, held with complete deadpan sincerity.`,
  },
  auth: {
    morphTo: "coding",
    motion:
`Seen from behind his shoulder: his right hand picks the teal electric toothbrush up off the wooden desk and flips it upside down, showing the steel-ringed USB-C port set flush into the bottom of the handle. His left hand pulls in a short white USB-C cable from beside the laptop, pushes the cable's connector into the port on the brush's base with one small decisive click, then seats the cable's other end into the side port of the bright green laptop. The brush's small blue light blinks on as the connection lands, and he sets the tethered brush down gently beside the laptop. The whole ritual is calm and practiced, like plugging in a security key is the most normal thing in the world. His face is never shown while he works — at the end the camera swings smoothly around the desk and settles on the final frame's view: the man seen from the front, typing, the brush plugged in beside him.`,
  },
  coding: {
    motion:
`The man types steadily on the bright green laptop, fingers moving in a calm rhythm, eyes on the screen through red glasses. The teal toothbrush stays plugged into the laptop's side, its small light glowing red. Steam rises gently from the coffee cup; the big plant's leaves shift softly; window light flickers through the foliage. The camera holds nearly still with the slightest drift forward.`,
  },
  "brush-close": {
    motion:
`Extreme close-up on the teal electric toothbrush in his mouth: the bristle head shivers in small fast strokes, the handle trembling with electric vibration, his fingers adjusting their grip slightly. His jaw shifts slowly; his eyes stay calm behind the red glasses. Warm window light glints on the brush handle. The camera holds almost still, drifting in by a hair. Quiet, intimate, deadpan.`,
  },
  sheet: {
    motion:
`First-person POV, his own hands in frame: the printed product sheet held up at reading distance trembles very slightly in his right hand, the paper flexing and catching the window light; his thumb shifts its grip a little. In the lower left, the teal toothbrush idles in his other hand with a faint electric tremble. The camera is his gaze — a gentle natural handheld sway, drifting a hair closer to the page as he reads. The lettering on the page stays fixed and stable on the paper, never warping. Quiet morning bathroom light. Nothing else happens.`,
  },
};

await runMotionCli({
  slug: "2fa-story",
  laneDir: HERE,
  motionDir: join(HERE, "motion"),
  structPath,
  panelFor: (name) => PANELS[name],
  shots: SHOTS,
  mediumMotion: MEDIUM_MOTION,
  ratio: "9:16",
  audio: join(HERE, "vo.mp3"),
  finalOut: join(HERE, "motion/DONT-USE-assemble-here.mp4"), // build.mjs assembles
});
