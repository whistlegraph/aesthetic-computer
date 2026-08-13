#!/usr/bin/env node
// maytrax/bin/gen-motion-femrag-plusplus-shakeout.mjs — the SHAKEOUT motion
// pass: Seedance shots from the twelve femrag++ panels, cut to the track's
// own section map. Thin lane driver over pop/lib/motion-pipeline.mjs.
//
// Panels come from gen-sections-femrag-plusplus-shakeout.mjs; sections from
// out/femrag-plusplus.struct.json, which is the actual arrangement of
// bin/render-femrag-plusplus.mjs (runway → groove → buildup → two drops →
// breakdown → the 1:33 ragga-donk → outro).
//
// Shot grammar: ALL CUTS except two scripted morphs — buildup1→drop1a and
// buildup2→drop2a. Both are same-camera escalations (a coiled bunny
// releasing into a leap), which is exactly the case morphs are for; every
// other transition changes room state or camera, where a morph would invent
// doubled figures (see pop-motion-pipeline memory).
//
// Usage:
//   node pop/maytrax/bin/gen-motion-femrag-plusplus-shakeout.mjs --dry-run
//   node pop/maytrax/bin/gen-motion-femrag-plusplus-shakeout.mjs --only drop1a
//   node pop/maytrax/bin/gen-motion-femrag-plusplus-shakeout.mjs
//   node pop/maytrax/bin/gen-motion-femrag-plusplus-shakeout.mjs --assemble

import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { runMotionCli, parseFlags } from "../../lib/motion-pipeline.mjs";
import { NEEDLE_FELT_WOOL_MOTION, FRAMING_YT_LANDSCAPE_MOTION } from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;
const SLUG = "femrag-plusplus";

const MEDIUM_MOTION = [
  `A needle-felt music video at 144 BPM — every motion is DANCE, driven and rhythmic, landing on beats rather than drifting. The wool bunny never goes limp between moves; he is always either loading or releasing.`,
  FRAMING_YT_LANDSCAPE_MOTION,
  NEEDLE_FELT_WOOL_MOTION,
].join("\n\n");

// The panel carries the LOOK; the prompt carries only MOTION.
// EAR CONTINUITY is this film's through-line: heavy → swinging → whipping
// horizontal at the drops → loose and behind-the-beat in the dancehall →
// hanging spent at the end. FUZZ CONTINUITY: his wool gets progressively
// more disheveled and the room accumulates loose wisps; it never resets.
const SHOTS = {
  runway: {
    motion:
`The felt bunny starts to move on the rug — a small weight-shift from one hind paw to the other, then a second, finding the pulse. His two long ears hang heavy and swing only slightly, a few degrees, lagging behind each shift. One front paw taps twice against his own chest. The camera holds nearly still and drifts in by a hair. Dim, warm, contained — a dance that has not committed yet.`,
  },
  groove: {
    motion:
`The bunny settles into a two-step shuffle: one hind paw plants, the other slides across the wool rug and back, hips turning slightly with each step, both front paws bouncing loosely at rib height. His two long ears swing together in a matched arc, lifting further off vertical with each bar. The rug rumples under the planted paw and stays rumpled. The camera tracks slowly sideways with him, staying wide. Confident and easy — the groove is found.`,
  },
  buildup1: {
    // Same-camera escalation into the drop — a legitimate morph.
    morphTo: "drop1a",
    physical: "extreme",
    contacts: ["both hind paws → rug", "front paws → own chest", "ears → held back under tension"],
    invariants: ["exactly one bunny in frame at all times", "both long ears visible throughout"],
    beats: [
      { at: 0, action: "close on the bunny, shoulders drawing in, chin tucking, front paws pulling to his chest" },
      { at: .35, action: "his two long ears sweep back behind his head and pull taut, straining — maximum tension" },
      { at: .7, action: "he compresses further, knees bending, weight sinking onto both hind paws — the coil" },
      { at: 1, action: "the release begins: hind paws drive against the rug and he starts to rise, ears whipping forward off their tension, camera pulling back and down toward the low angle" },
    ],
    motion:
`One continuous escalation on the SAME bunny — never two figures. He winds up tight in close-up, ears swept back and straining, then explodes: hind paws drive into the rug, he launches, and the camera falls back and down to the low rug-level angle as he goes airborne — landing exactly on the final frame. The room tears open around him as he rises.`,
  },
  drop1a: {
    physical: "extreme",
    contacts: ["hind paws → airborne, no contact", "rug → visibly kicked and rucked where he launched"],
    invariants: ["both long ears present and extended", "one bunny only"],
    beats: [
      { at: 0, action: "the bunny is at the top of his leap, both hind paws off the rug, ears flung horizontal to each side" },
      { at: .3, action: "he twists through the air, front paws sweeping wide, ears whipping across the turn" },
      { at: .6, action: "hind paws reach down and strike the rug, wool compressing under the impact, ears snapping downward with the landing" },
      { at: 1, action: "he rebounds straight back up into the next shake, ears flying out again — the dance is continuous" },
    ],
    motion:
`THE DROP, shot low from the rug looking up. The bunny lands hard, rebounds, and keeps going — a full-body shakeout on the beat: hips whipping, front paws thrown wide, both long ears flying out horizontally and snapping with every direction change. Loose wool wisps lift off him with each impact and hang in the bulb light. The yarn cables on the floor jump slightly when he lands. The camera stays low and holds, letting him dominate the frame.`,
  },
  drop1b: {
    motion:
`Close on the bunny's head and shoulders, deep in the dance — head thrown back, bead eyes squeezed happy inside their red lenses, his two long ears mid-whip, carving wide S-curves past his head and doubling back with each beat. Loose wool wisps peel off the ears at the ends of their arcs and drift through the light. The camera handholds gently with him, staying tight — never pulling out to a wide.`,
  },
  breakdown: {
    motion:
`The bunny drops into slow heavy half-time. He stands at the big felted speaker cabinet with one front paw laid flat on its oatmeal wool cone; the cone visibly pushes outward against his paw and relaxes, a slow physical pulse, and his whole body rocks with it — knees bending deep, weight rolling side to side. His two long ears hang low and swing in a lazy pendulum, the two ears out of phase with each other. The camera holds wide and almost still. Enormous and patient.`,
  },
  buildup2: {
    morphTo: "drop2a",
    physical: "extreme",
    contacts: ["front paws → rug", "hind paws → rug", "ears → pinned flat along his spine"],
    invariants: ["exactly one bunny throughout", "both ears visible"],
    beats: [
      { at: 0, action: "tight close-up: the bunny crouches all the way down, chin near his knees, both front paws pressed flat to the rug" },
      { at: .4, action: "his ears pin flat back along his spine and lock there; his whole wool body gathers and stills" },
      { at: .75, action: "hind paws load and the rug compresses visibly beneath them — the deepest compression of the film" },
      { at: 1, action: "he fires upward off both hind paws, ears tearing up off his spine, camera dropping back to the low wide angle as he clears the rug" },
    ],
    motion:
`The second wind-up, steeper and lower than the first, on the SAME single bunny. He crushes down into the rug, ears pinned, everything gathered — then launches, and the camera falls back and down to the low wide angle as he rises out of the crouch, landing exactly on the final frame at the top of the leap.`,
  },
  drop2a: {
    physical: "extreme",
    contacts: ["hind paws → airborne at the apex", "rug → folded and kicked beneath him"],
    invariants: ["both long ears present", "one bunny only", "the speaker stack stays a single leaning tower"],
    beats: [
      { at: 0, action: "apex of the leap — body arched back, all four paws flung out in a star, both ears whipping up behind his head" },
      { at: .25, action: "he falls, paws gathering under him, ears streaming upward against the drop" },
      { at: .5, action: "hind paws slam the rug and it folds; loose wool bursts off him and the speaker stack shudders" },
      { at: 1, action: "he rebounds into a wilder, faster shakeout than before — bigger arcs, harder direction changes, ears cracking like whips" },
    ],
    motion:
`THE SECOND DROP, low and wide — bigger than the first in every way. He lands, folds the rug, and tears into the hardest dancing of the film: faster turns, wider paw throws, both long ears whipping and cracking through their arcs. A cloud of loose wool wisps hangs and swirls where he lands. The camera stays low and pushes in slightly.`,
  },
  drop2b: {
    motion:
`Tight on the bunny's hind paws and the rug, filling the frame at maximum fiber resolution. His felted paws stamp the beat: one paw drives down and compresses the rug's matted pile, the other lifts away trailing wisps, then they trade — over and over, locked to the rhythm. A yarn cable on the floor jumps slightly with each stamp. Above and out of focus, the blurred oatmeal wool of his body keeps moving. The camera stays locked and close.`,
  },
  "ragga-a": {
    motion:
`The dancehall. The bunny's whole rhythm changes — looser, heavier, deliberately behind the beat. He skanks: leaning forward with his weight low, one shoulder dropped, both front paws swinging across his body in long lazy diagonals, one hind paw kicking back and returning. His two long ears swing in slow wide arcs that lag noticeably behind his body — cool, not frantic. Above him the huge felted speaker cones pulse outward in slow deep pushes, the wool physically bulging and relaxing. The camera drifts slowly in a wide arc around him. Warm amber and deep green.`,
  },
  "ragga-b": {
    motion:
`Close and low — the bunny leaning back against the mouth of the biggest felted speaker cone, half-turned to it, still skanking loose and easy. One long ear swings forward across his own face and away again with each bar; the other trails behind. Behind him the enormous oatmeal wool cone visibly pushes outward at its center on every bass note and settles back, and loose fibers around its rim lift away from the surface with each push. His bead eyes stay happy inside the red lenses. The camera holds close, swaying a little with him.`,
  },
  outro: {
    motion:
`The bunny finishes. He slows over the first moments — the swinging arcs shortening, his ears sinking lower with each one — then stops entirely at the center of the wrecked rug, standing still, one front paw settling onto his own chest, his sides rising and falling with breath. His two long ears hang all the way down, completely spent. His wool is visibly fluffed and disheveled everywhere, and the last loose wisps drift down and settle onto the rug around him. Behind him the speaker cones go still. The camera drifts slowly back to a wide as the warm little lamp is left the only light. Held, warm, finished.`,
  },
};

const cfg = {
  slug: SLUG,
  laneDir: LANE,
  structPath: `${OUT}/${SLUG}.struct.json`,
  panelFor: (name, i) => `${OUT}/${SLUG}-yt-sec-${i}-${name}.png`,
  shots: SHOTS,
  mediumMotion: MEDIUM_MOTION,
  ratio: "16:9",
  audio: `${OUT}/${SLUG}.mp3`,
  finalOut: `${OUT}/${SLUG}-shakeout-yt.mp4`,
};

await runMotionCli(cfg, parseFlags());
