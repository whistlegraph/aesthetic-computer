#!/usr/bin/env node
// maytrax/bin/gen-motion-femrag-plusplus-reel.mjs — the VERTICAL cut of
// SHAKEOUT: 9:16 portrait, the first 60 seconds of femrag++, for reels.
//
// It is the same film, not a different one. The panels come from
// gen-sections-femrag-plusplus-shakeout.mjs --reel, which re-shoots each
// beat in portrait against its own landscape panel as a reference, so the
// room, the props, the lighting and the bunny are identical — only the
// camera's aspect changed.
//
// The reel is the track's first FIVE sections, which land on exactly 60.0s:
//   drop1a  POV on the laptop      0 – 13.3
//   drop1b  game chase cam        13.3 – 26.7
//   breakdown  through the window 26.7 – 40.0
//   buildup2   the wind-up        40.0 – 46.7
//   drop2a     the big drop       46.7 – 60.0
// Cutting on real section boundaries means the audio slice is a clean
// musical phrase rather than a fade at an arbitrary time.
//
// Its own motion dir + struct, so takes never collide with the long cut.
//
// Usage:
//   node pop/maytrax/bin/gen-motion-femrag-plusplus-reel.mjs --dry-run
//   node pop/maytrax/bin/gen-motion-femrag-plusplus-reel.mjs
//   node pop/maytrax/bin/gen-motion-femrag-plusplus-reel.mjs --assemble

import { readdirSync, readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { runMotionCli, parseFlags } from "../../lib/motion-pipeline.mjs";
import { NEEDLE_FELT_WOOL_MOTION, FRAMING_IG_STORY_PORTRAIT_MOTION } from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;
const SLUG = "femrag-plusplus";
const REEL_DIR = `${OUT}/reel`;
mkdirSync(REEL_DIR, { recursive: true });

// Each section is SPLIT into halves. Seedance drifts over a 13-second take
// and packs far more movement into 6–7 seconds, so halving the shots doubles
// the cut rate AND raises the motion density — at identical billed seconds,
// and reusing the panel we already generated for that section.
// The reel tells its OWN story over the track's quiet-to-loud stretch
// (breakdown → ragga-a, 53.3s): the bunny finds a tangle of dead fairy
// lights in a dark A-frame, threads them through his own wool, and they do
// not come on — until the drop, which is where the whole reel turns.
// Each track section is split into named story beats.
const REEL_SECTIONS = ["breakdown", "buildup2", "drop2a", "drop2b", "ragga-a"];
const SPLITS = {
  breakdown: ["find", "thread"],   // dark: finds the lights, threads them in
  buildup2: ["coil"],              // dark: coiled, one bead flickering
  drop2a: ["ignite", "hammer"],    // THE LIGHTS COME ON
  drop2b: ["run"],                 // roblox: bolts around, clatters into things
  "ragga-a": ["skank", "orbit"],
};

// Slice the reel's struct out of the film's own struct, rebased to zero, so
// the reel inherits the real arrangement instead of restating it.
const structSrc = `${OUT}/${SLUG}.struct.json`;
if (!existsSync(structSrc)) {
  console.error(`✗ no film struct at ${structSrc} — render the track first`);
  process.exit(1);
}
const full = JSON.parse(readFileSync(structSrc, "utf8"));
const picked = REEL_SECTIONS.map((n) => full.sections?.find((s) => s.name === n)).filter(Boolean);
if (picked.length !== REEL_SECTIONS.length) {
  console.error("✗ reel sections missing from the film struct — re-render the track first");
  process.exit(1);
}
const t0 = picked[0].startSec;
const REEL_END = picked.at(-1).endSec;
const structPath = `${REEL_DIR}/${SLUG}-reel.struct.json`;
const sections = [];
for (const s of picked) {
  const parts = SPLITS[s.name];
  const a = s.startSec - t0, b = s.endSec - t0;
  if (parts.length === 1) { sections.push({ name: parts[0], startSec: a, endSec: b }); continue; }
  const mid = a + (b - a) / 2;
  sections.push({ name: parts[0], startSec: a, endSec: mid });
  sections.push({ name: parts[1], startSec: mid, endSec: b });
}
writeFileSync(structPath, JSON.stringify({ totalSec: REEL_END - t0, sections }, null, 2));

// The audio: the same slice of the master, cut once and cached.
const reelAudio = `${REEL_DIR}/${SLUG}-reel.mp3`;
if (!existsSync(reelAudio)) {
  const cut = spawnSync("ffmpeg", [
    "-hide_banner", "-loglevel", "error", "-y",
    "-ss", String(t0), "-t", String(REEL_END - t0), "-i", `${OUT}/${SLUG}.mp3`,
    "-af", "afade=t=out:st=" + (REEL_END - t0 - 1.2).toFixed(2) + ":d=1.2",
    "-c:a", "libmp3lame", "-b:a", "320k", reelAudio,
  ], { stdio: ["ignore", "inherit", "inherit"] });
  if (cut.status !== 0) { console.error("✗ audio slice failed"); process.exit(1); }
  console.log(`✓ audio slice ${(REEL_END - t0).toFixed(1)}s → ${reelAudio.split("/").pop()}`);
}

const MEDIUM_MOTION = [
  `A needle-felt music video at 144 BPM — every motion is DANCE, driven and rhythmic, landing on beats rather than drifting. The wool bunny never goes limp between moves; he is always either loading or releasing.\n\nMOTION DENSITY — this is a SHORT, FAST shot and it must be BUSY from the first frame to the last. Something moves in every single frame. At 144 BPM there are roughly TWO AND A HALF BEATS PER SECOND, so the bunny changes direction, weight or pose several times per second — quick, snappy, percussive movements, not one slow graceful gesture held across the take. Do NOT ease in, do NOT drift, do NOT hold a static pose: the shot opens already at full speed and stays there. The camera also keeps moving throughout.`,
  `EAR COUNT — ABSOLUTE, EVERY SINGLE FRAME: the bunny has EXACTLY TWO EARS, attached at two fixed points on the top of his head. TWO. Never three, never four. When an ear swings fast it must stay ONE continuous ear that moves — it must NEVER split into two, echo, ghost, smear into a duplicate, or leave a second copy behind at its old position.
LIGHT CONTINUITY — the many differently-coloured LEDs inside his body and inside both ears stay lit and stay in the SAME physical places for the whole shot; the rainbow glow may pulse in brightness with the beat but never migrates, goes out, or turns into a flat overlay. The neon tubes in the rafters are fixed physical fixtures — they may change brightness on the beat but never move or drift.`,
`RGB EYES, CYCLING FAST — his two bead eyes are lit RGB LEDs and they CHANGE COLOUR RAPIDLY throughout the shot, several times a second, snapping between saturated hues — cyan to magenta to lime to violet to gold — like a fast RGB cycle. The two eyes are often DIFFERENT colours from each other at any instant and they keep shifting independently. The colour change is a hard snap between hues, never a slow lazy fade. Each eye stays centred INSIDE its own round red lens the entire time.`,
  FRAMING_IG_STORY_PORTRAIT_MOTION,
  NEEDLE_FELT_WOOL_MOTION,
].join("\n\n");

// Portrait re-framings of the same five moments. Vertical wants the camera
// closer and the action stacked, so these are tighter than the wide cut.
const SHOTS = {
  find: {
    motion:
`THIRD-PERSON GAME CAM in a nearly DARK room. The bunny — completely UNLIT, plain off-white wool, nothing glowing on him anywhere — paws through a tangled heap of DEAD fairy lights on the plank floor, lifting loops of the wire, turning a dull glass bead over, his two long ears perking and swivelling with curiosity. He tugs a length free and it drags across the boards. The camera drifts slowly in behind him. NOTHING in this shot lights up — no glow on him, no glow in the beads, no neon. Quiet, dark, curious.`,
  },
  thread: {
    motion:
`CLOSE, still dark, still UNLIT. The bunny THREADS the dead light-string through his own wool — pulling a loop across his chest with both front paws, tucking wire into his fibers, working a bead into place, pressing another down into the felt of one long ear, then tugging the slack through. Continuous busy handwork, several distinct actions across the shot. The wire trails off his back across the floor. Not one bead lights. His eyes stay plain dark beads. The camera holds close and tilts slowly to follow his paws.`,
  },
  coil: {
    physical: "extreme",
    contacts: ["front paws → plank floor", "hind paws → floor", "ears → pinned flat along his spine"],
    invariants: ["exactly one bunny", "exactly two ears", "no light on him until the very last moment"],
    beats: [
      { at: 0, action: "tight and dark: the wired but UNLIT bunny drops into a deep crouch, chin toward his knees, front paws flat on the boards" },
      { at: .4, action: "his two ears pin flat back along his spine and lock; he gathers and stills, still completely dark" },
      { at: .75, action: "ONE single bead near his chest flickers faintly — a dim ember, the first light in the reel — then another catches" },
      { at: 1, action: "the flicker races along the wire through his wool and he begins to fire upward off both hind paws" },
    ],
    motion:
`The wind-up in the dark. He crushes down, ears pinned, everything gathered — and a single dead bead on his chest FLICKERS, catches, and the flicker starts to run along the wire through his wool as he launches. The camera pushes in hard on the flicker. Everything else stays black. This shot is the fuse, not the explosion.`,
  },
  ignite: {
    physical: "extreme",
    contacts: ["hind paws → airborne at the apex", "rug → kicked beneath him"],
    invariants: ["exactly two ears", "one bunny only", "once the lights are on they STAY on"],
    beats: [
      { at: 0, action: "he is rising off the floor, dark, with the flicker racing along the wire through his wool" },
      { at: .15, action: "EVERY LIGHT ON HIM BLASTS ON AT ONCE in full rainbow — chest, belly, both ears, and his two bead eyes lit RGB" },
      { at: .35, action: "the light bursting out of him floods the room: the coloured speaker cabinets snap into view in their candy colours, the rafters light, and every neon tube fires on with him" },
      { at: .6, action: "he reaches the apex of the leap, all four paws flung wide, both ears whipping up behind him, blazing" },
      { at: 1, action: "he lands hard and rebounds instantly into dancing — felted wave-rings and music notes burst out of the speaker cones on the impact" },
    ],
    motion:
`THE MOMENT THE LIGHTS COME ON — the turn of the whole reel, shot low looking up into the A-frame peak. He goes from a dark wool animal to a blazing rainbow lantern in a single frame, and the room lights WITH him: the colour-coded speaker wall snapping into colour, neon firing along every rafter, felted sound-waves and stitched music notes bursting out of the cones. He lands and rebounds without pausing. The camera shakes on the ignition and pushes in.`,
  },
  hammer: {
    motion:
`The hardest, fastest dancing in the reel, low and wide, room fully alive. He changes direction several times per second — hips whipping, front paws thrown wide and snapped back, hind paws stamping, his TWO long ears cracking through big arcs and reversing, blazing rainbow, RGB eyes bright. Concentric felted wave-rings keep pumping out of every coloured cone in that cabinet's own colour and stitched music notes tumble through the air around him. Neon strobes across the rafters. Wool wisps fly off him continuously. The camera pushes in and shakes with the beat, never still.`,
  },
  run: {
    motion:
`THIRD-PERSON GAME CHASE CAM, pure Roblox slapstick — the camera locks right behind the bunny at running height as he BOLTS forward across the plank floor at full tilt, legs pumping, his TWO long ears streaming straight back and blazing rainbow, glowing wire tail whipping after him. He CLATTERS into a stack of coloured speaker cabinets — the top one knocked askew and toppling, a felted amp tumbling, yarn cables kicked into the air, felted music notes and wave-rings knocked loose and spinning — then he ricochets off, turns hard, and charges off in a NEW direction, only to clip another cabinet and stumble. Multiple hard direction changes and at least two collisions in the shot. The camera swings and lurches to keep up. Fast, clumsy, funny.`,
  },
  skank: {
    motion:
`The dancehall, wide and full of colour. He skanks — leaning forward with his weight low, one shoulder dropped, both front paws swinging across his body in long lazy diagonals, one hind paw kicking back and returning, his TWO long ears swinging in behind-the-beat arcs and glowing rainbow. Every coloured cone pulses outward on the beat and throws a fresh felted wave-ring in its own colour, and stitched music notes drift and spin through the whole room around him. The camera arcs slowly sideways around him with real parallax.`,
  },
  orbit: {
    motion:
`Maximum dancehall, low and tall, looking up into the A-frame peak with every neon tube blazing. The camera ORBITS him continuously through the whole shot — a full sweeping arc so the speaker tower, the rafter peak and the gable window all travel past behind him with strong parallax. He dances at full power at the centre, turning with the camera, both front paws thrown wide, his TWO long ears whipping through big arcs and blazing rainbow, RGB eyes lit, his body throwing coloured light across the rug. The air is thick with felted music notes and expanding wave-rings in a dozen colours. Never still for a frame.`,
  },
};

// Reel panels resolve by NAME, like the wide cut, so renumbering is harmless.
function panelFor(name) {
  const suffix = `-${name}.png`;
  const hit = readdirSync(OUT)
    .filter((f) => f.startsWith(`${SLUG}-reel-sec-`) && f.endsWith(suffix))
    .sort();
  return hit.length ? `${OUT}/${hit[0]}` : `${OUT}/${SLUG}-reel-sec-?-${name}.png`;
}

await runMotionCli({
  slug: `${SLUG}-reel`,
  laneDir: LANE,
  motionDir: `${REEL_DIR}/motion`,
  structPath,
  panelFor,
  shots: SHOTS,
  mediumMotion: MEDIUM_MOTION,
  ratio: "9:16",
  xfade: 0.3,        // soft dissolves — hard cuts read as glitches here
  audio: reelAudio,
  finalOut: `${OUT}/${SLUG}-shakeout-reel.mp4`,
}, parseFlags());
