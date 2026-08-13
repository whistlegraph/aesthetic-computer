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

import { readdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { runMotionCli, parseFlags } from "../../lib/motion-pipeline.mjs";
import { NEEDLE_FELT_WOOL_MOTION, FRAMING_YT_LANDSCAPE_MOTION } from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;
const SLUG = "femrag-plusplus";

// EAR COUNT is the failure mode this film actually hits: a fast-swinging ear
// duplicates across frames and the bunny reads as having three or four. The
// still prompts guard it too, but a still can only be wrong once — motion has
// to hold the count every frame, so the contract says so explicitly.
const EAR_LAW =
`EAR COUNT — ABSOLUTE, EVERY SINGLE FRAME: the bunny has EXACTLY TWO EARS, attached at two fixed points on the top of his head. TWO. Never three, never four. When an ear swings fast it must stay ONE continuous ear that moves — it must NEVER split into two, echo, ghost, smear into a duplicate, leave a second copy behind at its old position, or grow an extra ear on the far side of his head. If a pose or a fast swing makes the count ambiguous, slow that motion down rather than duplicating geometry. Both ears remain attached to his head at all times; neither detaches, floats free, or passes through his skull.
LIGHT CONTINUITY — the LEDs inside his body and inside both ears stay lit and stay in the SAME physical places for the whole shot; the internal glow may pulse in brightness with the beat but never migrates, switches off entirely, or turns into a flat overlay. The neon tubes in the rafters are physical fixtures fixed in place — they may change brightness or color on the beat, but they never move, bend, drift, or reposition themselves between frames.`;

const MEDIUM_MOTION = [
  `A needle-felt music video at 144 BPM — every motion is DANCE, driven and rhythmic, landing on beats rather than drifting. The wool bunny never goes limp between moves; he is always either loading or releasing.`,
  EAR_LAW,
  FRAMING_YT_LANDSCAPE_MOTION,
  NEEDLE_FELT_WOOL_MOTION,
].join("\n\n");

// The panel carries the LOOK; the prompt carries only MOTION.
// EAR CONTINUITY is this film's through-line: heavy → swinging → whipping
// horizontal at the drops → loose and behind-the-beat in the dancehall →
// hanging spent at the end. FUZZ CONTINUITY: his wool gets progressively
// more disheveled and the room accumulates loose wisps; it never resets.
const SHOTS = {
  // `runway` is deliberately not in the struct — @jeffrey trimmed those four
  // bars off the front of the track (render-femrag-plusplus.mjs TRIM), so the
  // record opens cold on the groove. Its panel survives as the character
  // ANCHOR every other panel is generated against; it is simply never a shot.
  runway: {
    motion:
`The felt bunny starts to move on the rug — a small weight-shift from one hind paw to the other, then a second, finding the pulse. His two long ears hang heavy and swing only slightly, a few degrees, lagging behind each shift. One front paw taps twice against his own chest. The camera holds nearly still and drifts in by a hair. Dim, warm, contained — a dance that has not committed yet.`,
  },
  drop1a: {
    // The film OPENS here — the track starts cold on the drop, so this POV
    // is already at full power. No wind-up exists to morph from.
    motion:
`FIRST-PERSON POV, opening the film at full power: the camera is the bunny's own eyes looking down at the felted laptop on the rug. His own two forepaws hammer the felted keycaps in rhythm — real strikes, paws lifting and landing on the beat, the wool of the keys compressing under each hit. The colored blocks of light on the felt screen change and pulse as he plays, throwing shifting magenta, cyan and amber light up onto his paws and forearms; the LEDs under his wool flare brighter with each strike. Overhead at the edges of frame the neon rig blasts across the rafters. The camera breathes slightly with his head — small, human, never a smooth mechanical move — and NEVER shows his face or a second bunny.`,
  },
  drop1b: {
    motion:
`THIRD-PERSON VIDEO-GAME CHASE CAM, exactly like Grand Theft Auto or Roblox — the camera floats behind and slightly above the bunny and FOLLOWS him as he dances, holding him in the lower-middle of frame with the A-frame room opening out ahead. He is mid-shakeout on the rug: body twisting, front paws thrown wide, his TWO long ears whipping out to either side, the chains of colored LEDs inside them streaking as they swing. The rainbow light inside his body pulses with the beat and throws moving colored pools onto the rug beneath him. The camera drifts with him like a game camera catching up — smooth, trailing, always behind — never cutting to his face. NO HUD, no crosshair, no interface overlay of any kind: this is still a real photograph of felt.`,
  },
  breakdown: {
    motion:
`FROM OUTSIDE, THROUGH THE GABLE WINDOW, holding almost still on the breakdown. The camera stays out in the cold night looking in through the felted panes, the dark mullions fixed across the frame. Inside, small and distant, the bunny rocks in slow heavy half-time with one paw on the big speaker cone; the cone visibly pushes outward against his paw and relaxes, a slow physical pulse, and his whole body rocks with it. His rainbow glow is the only real light moving in the house — separate colored blooms breathing through his wool, his two long ears hanging low and lit. The camera creeps almost imperceptibly closer to the glass across the shot. Enormous, patient, and heard from far away.`,
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
`TOP-DOWN, straight overhead, locked off. The round green rug holds as a circle in frame and the bunny stamps the beat directly below the camera — seen from above, both hind paws driving down onto the rug and lifting alternately in rhythm, body foreshortened, his TWO long ears splayed flat on the floor to either side and swinging in arcs across the boards with each stamp. The rainbow light inside him spills outward across the rug in overlapping colored pools that PULSE with every impact, and the yarn cables loop flat on the boards and jump slightly when he lands. The camera does not move at all — the whole shot is his motion under a fixed overhead eye.`,
  },
  "ragga-a": {
    motion:
`THIRD-PERSON GAME CHASE CAM again — behind and above him, following him through the dancehall as he skanks across the room. The camera trails him at a steady distance with the A-frame laid out ahead: the speaker tower rising on the right, the gable window at the far end, cables looping over the plank floor. He is seen from behind, leaning forward with his weight low, one shoulder dropped, both front paws swinging across his body, one hind paw kicking back, his TWO long ears swinging in lazy behind-the-beat arcs and glowing rainbow. The felted speaker cones pulse outward in slow deep pushes as he passes them. Smooth trailing game-camera motion, no HUD, no interface.`,
  },
  "ragga-b": {
    motion:
`FIRST PERSON at the speaker cone. The camera is the bunny's own eyes looking straight at the enormous felted cone filling the frame; his own two forepaws are pressed flat against its wool in the bottom of frame. The cone PUSHES OUTWARD against his paws on every bass note and settles back — a real physical pulse in the wool, loose fibers around the rim lifting and falling with it — and the rainbow LEDs under the wool of his own forearms flare brighter with each push, throwing shifting magenta, cyan and lime light onto the cone where he touches it. The camera breathes slightly with his head, small and human. NEVER shows his face or any second bunny.`,
  },
  "ragga-breathe": {
    motion:
`The room takes a breath. The neon drops away almost entirely and the bunny's own internal glow becomes the light source for the whole shot. He keeps skanking but small and easy — weight low, shoulders rolling, both front paws swinging gently across his body, his TWO long ears swinging in slow behind-the-beat arcs, warm gold and translucent. The light spilling out of his chest visibly moves across the green rug and the plank boards as he sways, and the felted speaker cones beside him pulse outward in slow deep pushes. The camera drifts in slowly and steadily. All glow, no blast.`,
  },
  "ragga-push": {
    motion:
`The room switches back on around him. Neon tubes along the rafters light one after another through the shot — amber, then forest-green, then hot orange — each one adding hard color to the beams and spilling further down the sloped ceiling onto the floor, so the A-frame builds from near-dark to fully lit across the take. The bunny digs in as it builds: the skank drops lower and gets heavier, paws swinging in bigger diagonals, his TWO long ears carving wider arcs, and his internal LEDs push from warm gold up toward white. The camera pushes in slowly with the build.`,
  },
  "ragga-push-b": {
    motion:
`Maximum dancehall, shot low from the plank floor looking up into the A-frame peak. Every neon tube is blazing at once and the whole triangular room is saturated with color. The bunny dances at full power — the hardest, widest skanking of the film, body low and turning, hind paw kicking back, front paws thrown wide, his TWO long ears whipping through big arcs and blazing translucent, his whole body a lantern throwing colored light onto the rug. CAMERA: a slow continuous ORBIT around him — the camera arcs steadily sideways through the shot so the speaker tower, the rafter peak and the gable window all sweep past behind him and the parallax reads as a real move around a real room. Loose wool wisps hang lit in the colored air.`,
  },
  outro: {
    motion:
`TOP-DOWN, straight overhead, the last shot of the film. The bunny lies at the centre of the round green rug, finally still, sprawled with all four paws loose and his TWO long ears splayed flat to either side. His sides rise and fall with breath. The rainbow LEDs inside him fade slowly across the shot — separate colored blooms breathing gently and dimming, one or two winking out entirely — until only a soft slow pulse is left. Loose wool wisps drift down and settle onto the rug around him. The camera RISES very slowly straight up toward the rafter peak, so he grows smaller and the plank floor and looping cables open out around him. Held, warm, finished.`,
  },
};

// Panels resolve by NAME, not by the index baked into their filename: the
// arrangement is trimmed from the front (render-femrag-plusplus.mjs TRIM),
// so a section's position moves while its identity does not.
function panelFor(name) {
  const suffix = `-${name}.png`;
  const hit = readdirSync(OUT)
    .filter((f) => f.startsWith(`${SLUG}-yt-sec-`) && f.endsWith(suffix))
    .sort();
  if (!hit.length) return `${OUT}/${SLUG}-yt-sec-?-${name}.png`; // let the CLI report it missing
  return `${OUT}/${hit[0]}`;
}

const cfg = {
  slug: SLUG,
  laneDir: LANE,
  structPath: `${OUT}/${SLUG}.struct.json`,
  panelFor,
  shots: SHOTS,
  mediumMotion: MEDIUM_MOTION,
  ratio: "16:9",
  audio: `${OUT}/${SLUG}.mp3`,
  finalOut: `${OUT}/${SLUG}-shakeout-yt.mp4`,
};

await runMotionCli(cfg, parseFlags());
