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
const REEL_SECTIONS = ["drop1a", "drop1b", "breakdown", "buildup2", "drop2a"];
const SPLITS = {
  drop1a: ["hands", "grid"],
  drop1b: ["chase", "spin"],
  breakdown: ["window", "creep"],
  buildup2: null,                    // already short — one shot
  drop2a: ["launch", "hammer"],
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
  if (!parts) { sections.push({ name: s.name, startSec: a, endSec: b }); continue; }
  const mid = a + (b - a) / 2;
  sections.push({ name: `${s.name}-${parts[0]}`, startSec: a, endSec: mid });
  sections.push({ name: `${s.name}-${parts[1]}`, startSec: mid, endSec: b });
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
  FRAMING_IG_STORY_PORTRAIT_MOTION,
  NEEDLE_FELT_WOOL_MOTION,
].join("\n\n");

// Portrait re-framings of the same five moments. Vertical wants the camera
// closer and the action stacked, so these are tighter than the wide cut.
const SHOTS = {
  // Each shot is ~6.7s and must be BUSY end to end — see MOTION DENSITY.
  "drop1a-hands": {
    motion:
`FIRST-PERSON POV, opening the reel already at full speed. The camera is the bunny's own eyes looking down at the felted laptop. His own two forepaws HAMMER the felted keycaps — fast, percussive, several distinct strikes per second, paws lifting clear and slamming back down, the wool of the keys visibly compressing under each hit and springing back. The rainbow LEDs under the wool of his forearms FLASH brighter on every strike, so his arms strobe with the rhythm. The camera bobs and jitters slightly with each hit — small, human, never smooth. No face, no second bunny.`,
  },
  "drop1a-grid": {
    motion:
`FIRST-PERSON POV, still hammering, now pushed in CLOSE on the glowing note grid itself. The coloured tiles on the felt screen FIRE in fast rhythmic patterns — different tiles lighting and going dark several times a second, columns and rows flickering on the beat like a sequencer running. His paws keep striking in and out of the bottom of frame at speed, and the light thrown back onto them changes colour with every tile that fires. The camera pushes in steadily on the grid across the shot. Relentless, fast, no pauses.`,
  },
  "drop1b-chase": {
    motion:
`THIRD-PERSON VIDEO-GAME CHASE CAM, like Grand Theft Auto or Roblox — the camera whips around from behind the laptop to find him already up and dancing, then FOLLOWS him at speed. He is mid-shakeout the whole shot: body twisting hard left and right, front paws thrown wide and snapped back, hind paws stamping, his TWO long ears whipping out and cracking back several times per second, LEDs streaking as they swing. The camera swings and catches up with him repeatedly, never settling. No HUD, no crosshair, no overlay.`,
  },
  "drop1b-spin": {
    motion:
`THIRD-PERSON GAME CAM, now ORBITING him fast — the camera arcs a full half-circle around the bunny across the shot, so the speaker stack, the gable window and the rafter peak all sweep past behind him with strong parallax. He spins with it, turning on the spot, front paws flung out, his TWO long ears flying horizontally with the rotation and blazing rainbow. Loose wool wisps whip off him. Fast, continuous, dizzying — the camera never stops moving and neither does he.`,
  },
  "breakdown-window": {
    motion:
`HARD CUT to OUTSIDE, through the gable window, and the energy drops to nothing. The camera sits still out in the cold night, the dark mullions fixed across the tall frame. Inside, small and distant, the bunny rocks in slow heavy half-time with one paw on the big speaker cone — the ONLY slow shot in the reel, and it lands as relief. The cone visibly pushes outward against his paw and relaxes on each bass note, his rainbow glow swelling and dimming with it. Held, patient, almost still.`,
  },
  "breakdown-creep": {
    motion:
`Still outside looking in, but now the camera CREEPS steadily toward the glass across the whole shot, closing the distance so the window fills more and more of the tall frame and the bunny grows from small to clearly readable. Inside he keeps rocking in half-time, but his rainbow LEDs begin to pulse FASTER and brighter as the shot goes on — the wind-up starting while we are still outside. The pulse rate climbs steadily. Tension building through glass.`,
  },
  buildup2: {
    physical: "extreme",
    contacts: ["front paws → rug", "hind paws → rug", "ears → pinned flat along his spine"],
    invariants: ["exactly one bunny throughout", "exactly two ears throughout"],
    beats: [
      { at: 0, action: "hard cut inside, tight on the bunny — he is already crouching down fast, chin dropping toward his knees" },
      { at: .45, action: "his two ears pin flat back along his spine and lock; his whole body gathers and compresses" },
      { at: .8, action: "the rainbow LEDs inside him surge and strobe toward white, faster and faster; the rug compresses under his hind paws" },
      { at: 1, action: "he fires upward off both hind paws and clears the rug, ears tearing up off his spine" },
    ],
    motion:
`The wind-up, tight and fast. He crushes down into the rug with his two ears pinned flat, the rainbow light inside him strobing quicker and quicker as he loads — then FIRES upward and clears frame. The camera pushes in hard as he compresses and drops back as he launches. Short, violent, no drift.`,
  },
  "drop2a-launch": {
    physical: "extreme",
    contacts: ["hind paws → airborne at the apex", "rug → folded and kicked beneath him"],
    invariants: ["exactly two ears, both present", "one bunny only"],
    beats: [
      { at: 0, action: "apex of the leap — body arched, all four paws flung out, both ears whipping up behind his head" },
      { at: .3, action: "he falls fast, paws gathering under him, ears streaming upward" },
      { at: .55, action: "hind paws slam the rug, it folds, wool bursts off him and the whole neon rig blasts to full" },
      { at: 1, action: "he rebounds instantly into hard fast dancing — no pause on the landing at all" },
    ],
    motion:
`THE DROP, low and tall so the A-frame peak and the neon rig fill the top of the frame. He lands hard, folds the rug, and rebounds instantly — no settle, no pause. Loose wool bursts off him on the impact and hangs in the coloured air. The camera shakes on the landing and pushes in.`,
  },
  "drop2a-hammer": {
    motion:
`The hardest, fastest dancing in the reel — full-body shakeout at maximum speed. He changes direction several times per second: hips whipping, front paws thrown wide and snapped back, hind paws stamping the rug, his TWO long ears cracking through big arcs and reversing, blazing rainbow, wool wisps flying off him continuously. The neon rig strobes at full blast across the rafters above. The camera pushes in and shakes with the beat, never still for a frame. Ends still going — no wind-down.`,
  },
};

// Reel panels resolve by NAME, like the wide cut, so renumbering is harmless.
function panelFor(name) {
  // "drop1a-grid" and "drop1a-hands" both come from the drop1a panel.
  const base = Object.keys(SPLITS).find((k) => name === k || name.startsWith(`${k}-`)) || name;
  const suffix = `-${base}.png`;
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
