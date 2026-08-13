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

const REEL_SECTIONS = ["drop1a", "drop1b", "breakdown", "buildup2", "drop2a"];

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
writeFileSync(structPath, JSON.stringify({
  totalSec: REEL_END - t0,
  sections: picked.map((s) => ({ name: s.name, startSec: s.startSec - t0, endSec: s.endSec - t0 })),
}, null, 2));

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
  `A needle-felt music video at 144 BPM — every motion is DANCE, driven and rhythmic, landing on beats rather than drifting. The wool bunny never goes limp between moves; he is always either loading or releasing.`,
  `EAR COUNT — ABSOLUTE, EVERY SINGLE FRAME: the bunny has EXACTLY TWO EARS, attached at two fixed points on the top of his head. TWO. Never three, never four. When an ear swings fast it must stay ONE continuous ear that moves — it must NEVER split into two, echo, ghost, smear into a duplicate, or leave a second copy behind at its old position.
LIGHT CONTINUITY — the many differently-coloured LEDs inside his body and inside both ears stay lit and stay in the SAME physical places for the whole shot; the rainbow glow may pulse in brightness with the beat but never migrates, goes out, or turns into a flat overlay. The neon tubes in the rafters are fixed physical fixtures — they may change brightness on the beat but never move or drift.`,
  FRAMING_IG_STORY_PORTRAIT_MOTION,
  NEEDLE_FELT_WOOL_MOTION,
].join("\n\n");

// Portrait re-framings of the same five moments. Vertical wants the camera
// closer and the action stacked, so these are tighter than the wide cut.
const SHOTS = {
  drop1a: {
    motion:
`FIRST-PERSON POV, opening the reel at full power: the camera is the bunny's own eyes looking down at the felted laptop. His own two forepaws hammer the felted keycaps in rhythm — real strikes, paws lifting and landing on the beat, the wool of the keys compressing under each hit. The coloured tiles on the felt screen light and change as he plays, throwing shifting light up onto his paws, and the rainbow LEDs under the wool of his forearms flare brighter with every strike. Above the laptop, up the tall frame, the A-frame rafters carry the neon rig blasting magenta, cyan and lime. The camera breathes slightly with his head — small, human — and NEVER shows his face or a second bunny.`,
  },
  drop1b: {
    motion:
`THIRD-PERSON VIDEO-GAME CHASE CAM, like Grand Theft Auto or Roblox — the camera floats behind and just above the bunny and FOLLOWS him as he dances, holding him in the middle band of the tall frame with the room running away ahead and the rafters climbing the top of the frame. He is mid-shakeout: body twisting, front paws thrown wide, his TWO long ears whipping out to either side, the chains of coloured LEDs inside them streaking as they swing. His rainbow glow pulses with the beat and throws moving coloured pools onto the rug. Smooth trailing game-camera motion — no HUD, no crosshair, no interface overlay; this is still a photograph of felt.`,
  },
  breakdown: {
    motion:
`FROM OUTSIDE, THROUGH THE GABLE WINDOW, holding nearly still. The camera stays out in the cold night looking in through the felted panes, the dark mullions fixed across the tall frame. Inside, small and distant, the bunny rocks in slow heavy half-time with one paw on the big speaker cone; the cone visibly pushes outward against his paw and relaxes, a slow physical pulse, and his whole body rocks with it. His rainbow glow is the only light moving in the house — separate coloured blooms breathing through his wool, both long ears hanging low and lit. The camera creeps almost imperceptibly closer to the glass. Enormous and patient.`,
  },
  buildup2: {
    morphTo: "drop2a",
    physical: "extreme",
    contacts: ["front paws → rug", "hind paws → rug", "ears → pinned flat along his spine"],
    invariants: ["exactly one bunny throughout", "exactly two ears throughout"],
    beats: [
      { at: 0, action: "tight on the bunny, crouching all the way down, chin near his knees, both front paws pressed flat to the rug" },
      { at: .4, action: "his two ears pin flat back along his spine and lock there; his whole wool body gathers and stills" },
      { at: .75, action: "hind paws load and the rug compresses visibly beneath them; the rainbow LEDs inside him surge toward white" },
      { at: 1, action: "he fires upward off both hind paws, ears tearing up off his spine, the camera dropping back and down as he clears the rug" },
    ],
    motion:
`The wind-up, on the SAME single bunny, framed tall. He crushes down into the rug with his two ears pinned flat and everything gathered, the rainbow light inside him surging brighter and faster — then launches, and the camera falls back and down the tall frame as he rises out of the crouch, landing exactly on the final frame at the top of the leap.`,
  },
  drop2a: {
    physical: "extreme",
    contacts: ["hind paws → airborne at the apex", "rug → folded and kicked beneath him"],
    invariants: ["exactly two ears, both present", "one bunny only"],
    beats: [
      { at: 0, action: "apex of the leap — body arched back, all four paws flung out, both ears whipping up behind his head" },
      { at: .25, action: "he falls, paws gathering under him, ears streaming upward against the drop" },
      { at: .5, action: "hind paws slam the rug and it folds; loose wool bursts off him and the neon rig blasts to full" },
      { at: 1, action: "he rebounds into the hardest dancing of the reel — bigger arcs, harder direction changes, ears cracking through their swings" },
    ],
    motion:
`THE DROP, shot low and tall so the A-frame peak and the whole neon rig fill the top of the frame above him. He lands, folds the rug, and tears into the hardest dancing of the reel — faster turns, wider paw throws, both long ears whipping through their arcs and blazing rainbow, his whole body a lantern throwing coloured light onto the rug. Loose wool wisps hang and swirl in the coloured air. The camera stays low and pushes in slightly.`,
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
  audio: reelAudio,
  finalOut: `${OUT}/${SLUG}-shakeout-reel.mp4`,
}, parseFlags());
