#!/usr/bin/env node
// momboba/bin/gen-motion-listener.mjs — the LANDSCAPE listener motion pass.
// Seedance 2.0 over the 9 felt movement stills (pano/out/NN-*.png), 16:9.
// Thin lane driver over pop/lib/motion-pipeline.mjs — the SHOTS table is the
// creative surface; pricing, generation, take archiving and picks live there.
//
// KEY DIFFERENCE from the reel: generation length ≠ playback length. Each
// movement plays for up to ~3 minutes in the final video, but we only PAY to
// generate a short (5 s; the-dream hero 12 s) gentle loopable clip per
// movement. bin/assemble-listener.mjs ping-pongs + tiles each clip to its
// real movement length. So the GENERATION struct below uses short sections
// purely to clamp motion-pipeline's per-shot Seedance duration (it bills
// ceil(section length), clamped [4,15] — line 58 of motion-pipeline.mjs).
//
//   8 movements × 5 s + the-dream × 12 s = 52 s billed ≈ $12.58 @ fast 720p.
//
// All CUTS — no morphs (cross-scene morphs invent doubled figures; the sleepy
// pace wants cuts anyway). Each clip is tiny + slow + returns near its start
// so the ping-pong loop is seamless.
//
// Usage:
//   node pop/momboba/bin/gen-motion-listener.mjs --dry-run    # prompts + cost
//   node pop/momboba/bin/gen-motion-listener.mjs              # gen missing shots
//   node pop/momboba/bin/gen-motion-listener.mjs --only the-dream --force
//   pick/re-roll takes in ClipWizard (macOS) — writes out/motion/takes.json

import { writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { runMotionCli, parseFlags } from "../../lib/motion-pipeline.mjs";
import { NEEDLE_FELT_WOOL_MOTION, FRAMING_YT_LANDSCAPE_MOTION } from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;
const PANO = `${LANE}/pano/out`;

const MEDIUM_MOTION = [
  `Hushed museum light. Sleepy lullaby pacing — every motion tiny, slow, and tender; nothing sudden anywhere. The shot opens and closes in nearly the same pose so it can loop gently.`,
  FRAMING_YT_LANDSCAPE_MOTION,
  NEEDLE_FELT_WOOL_MOTION,
].join("\n\n");

// movement → panel. names match pano/NN-<name>.txt and the struct below.
const PANEL = {
  arrival: `${PANO}/01-arrival.png`,
  "boba-sip": `${PANO}/02-boba-sip.png`,
  galleries: `${PANO}/03-galleries.png`,
  "waterlily-awe": `${PANO}/04-waterlily-awe.png`,
  "first-sheep": `${PANO}/05-first-sheep.png`,
  "asleep-flock": `${PANO}/06-asleep-flock.png`,
  "the-dream": `${PANO}/07-the-dream.png`,
  morning: `${PANO}/08-morning.png`,
  resolve: `${PANO}/09-resolve.png`,
};

// the GENERATION struct — short sections only to size each Seedance shot.
// the-dream is the 12 s hero; everything else is a 5 s loopable breath.
const GEN = [
  ["arrival", 5], ["boba-sip", 5], ["galleries", 5], ["waterlily-awe", 5],
  ["first-sheep", 5], ["asleep-flock", 5], ["the-dream", 12], ["morning", 5],
  ["resolve", 5],
];
let acc = 0;
const structPath = `${LANE}/pano/motion-struct.json`;
writeFileSync(structPath, JSON.stringify({
  totalSec: GEN.reduce((a, [, d]) => a + d, 0),
  sections: GEN.map(([name, d]) => { const s = { name, startSec: acc, endSec: acc + d }; acc += d; return s; }),
}, null, 2));

// The still carries the LOOK; the prompt carries only MOTION — tiny + slow.
const SHOTS = {
  arrival: { motion:
`The felt jeffrey doll walks slowly toward the museum doors with small unhurried puppet strides, the camera following at a gentle walking drift. The tiny felted boba stays firmly wrapped in his fingers — full milk-tea, dark wool pearls at the bottom — and never leaves his grip, never floats. Soft dusk light. Hopeful, calm, sleepy.` },
  "boba-sip": { motion:
`Close-ish on the felt jeffrey doll taking one long slow happy sip from the fat pastel straw — cheeks hollowing a hair, eyes half-lidded, the dark wool tapioca pearls stirring gently inside the pale cup. Both felt hands cradle the cup. Behind him the soft hanging mobile turns very slowly. The camera holds nearly still, drifting in by a breath.` },
  galleries: { motion:
`The felt jeffrey doll strolls at a slowing dawdle down the long gallery corridor, boba held snug in his hand, head turning softly from one framed abstract felt artwork to the next, posture loosening. The camera tracks alongside at his slowing pace. Quiet top-light; nothing else in the corridor moves.` },
  "waterlily-awe": { motion:
`The felt jeffrey doll stands tiny and still before the monumental felted water-lily tapestry, head tilting slowly back in awe, the boba dangling securely from his curled fingers. The vast wall of wool blues and greens shimmers almost imperceptibly, fibers catching the dim light. The camera rises very slowly up the tapestry, keeping the little doll anchored low in frame. Reverent, hushed.` },
  "first-sheep": { motion:
`The single needle-felted sheep peeks in from the gallery doorway at the frame edge with one slow careful step, woolly head turning toward the drowsy felt jeffrey doll slumped on the bench, whose head droops a little lower. A tiny curious lean from the sheep, a deeper slump from the doll. The camera holds wide and still — the moment before counting begins.` },
  "asleep-flock": { motion:
`The felt jeffrey doll lies fully asleep along the felt bench, his side rising and falling in a slow sleep-breath rhythm. Around him on the wide felt floor the small flock of needle-felted sheep barely stir — one shifts its weight, one lowers its woolly head to graze. The monumental water-lily tapestry glows softly above. The camera holds still. Deeply tender.` },
  "the-dream": { motion:
`Inside the painting: the felt jeffrey doll floats curled-up and asleep on the giant wool lily pad at centre, rocking almost imperceptibly as the felted pond ripples outward in slow wool waves of blue, green and lilac all around him. Above, two or three woolly sheep drift slowly across the felt sky like clouds. The camera drifts upward and away with the sheep, weightless and dreamy. The richest, slowest, happiest moment of the night.` },
  morning: { motion:
`Pale dawn light slowly warms the felt gallery from grey toward soft peach. The felt jeffrey doll begins to stir — lifting his tousled wool head a little, blinking awake behind his red felt glasses. The tipped-over empty boba cup rests beside him; one felt sheep settles down nearby. The camera holds nearly still. Tender, calm, waking.` },
  resolve: { motion:
`A final calm wide view: the felt jeffrey doll sits up at the end of the long bench, rested and softly smiling, the empty boba cup cradled in his lap, breathing slow and easy. The felt sheep are curled down asleep across the floor, barely stirring. Warm even daylight. The camera is almost perfectly still. Settled, content, home.` },
};

const cfg = {
  slug: "momabobasheep-listener",
  laneDir: LANE,
  structPath,
  panelFor: (name) => PANEL[name],
  shots: SHOTS,
  mediumMotion: MEDIUM_MOTION,
  ratio: "16:9",
  resolution: "720p",
  audio: `${OUT}/momabobasheep.mp3`,
  finalOut: `${OUT}/momabobasheep-listener-motion.mp4`, // unused (custom assembly)
};

await runMotionCli(cfg, parseFlags());
