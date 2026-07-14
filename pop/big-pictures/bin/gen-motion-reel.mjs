#!/usr/bin/env node
// big-pictures/bin/gen-motion-reel.mjs — the amaythingra REEL motion pass.
// Seedance 2.0 over 12 of the 32 storyline panels (gen-sections.mjs, the
// portrait `-p` set), 9:16 for the instagram reel / story. Thin lane
// driver over pop/lib/motion-pipeline.mjs — the SHOTS table is the
// creative surface; pricing, generation, take archiving and picks live
// there. Modeled on momboba/bin/gen-motion-momabobasheep-reel.mjs.
//
// THE CUT — the 32-beat office→metaverse→office story, squeezed to the 12
// beats a 56 s reel can hold, and TIMED TO THE MUSIC. The reel opens on
// the break (96-112 s), rides the VORTEX bell-storm + turntable scratches
// through the offer / portal / spawn, and lands the METAVERSE REVEAL
// exactly on DROP 2 — the tonic G at 128 s (see gen-struct.mjs's bar map).
// That is why AUDIO_START is 104.667 and not a round number: beat 5 starts
// at 5 × (56/12) = 23.333 s, and 128 − 23.333 = 104.667.
//
//   beat  music (audio s)          story
//   0-1   break, pads + bells      the grey office; the woman he watches
//   2-4   VORTEX, scratches build  Mark's headset; the portal; the spawn
//   5     ▶ DROP 2 (tonic G)       THE LAGOON REVEALED
//   6-11  drop-2 groove            carve · grab · gag · pixsies · out
//
// Shot grammar: ALL CUTS — 120 BPM deep house wants hard cuts, and
// cross-scene morphs invent doubled figures (see pop-motion-pipeline
// memory). The panels already carry amaythingra's ECU / first-person-POV
// grammar; the prompts here carry only MOTION.
//
// Usage:
//   node pop/big-pictures/bin/gen-motion-reel.mjs --dry-run       # prompts + cost
//   node pop/big-pictures/bin/gen-motion-reel.mjs                 # gen missing shots
//   node pop/big-pictures/bin/gen-motion-reel.mjs --only gag --force  # re-roll (old take archived)
//   node pop/big-pictures/bin/gen-motion-reel.mjs --assemble      # portrait cut + chrome
//   pick/re-roll takes in ClipWizard (macOS) — writes out/motion/takes.json

import { readFileSync, writeFileSync, existsSync, mkdirSync, readdirSync, renameSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { runMotionCli, parseFlags, shotList } from "../../lib/motion-pipeline.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;

const TOTAL_SEC = 56;            // the reel
const BEAT_SEC = TOTAL_SEC / 12; // 4.6667 s — 12 beats of the 32-beat story
const DROP2_SEC = 128;           // drop 2 lands on the tonic G (c/amaythingra.c)
const DROP_BEAT = 5;             // …under beat 5, the lagoon reveal
const AUDIO_START = DROP2_SEC - DROP_BEAT * BEAT_SEC; // 104.667

// The 12 beats, in story order. `panel` is the gen-sections portrait
// panel each one animates: out/amaythingra-p-sec-<i>-<section>-<half>.png
// (i = index into that script's SECTION_ORDER).
const BEATS = [
  { name: "office",  panel: "0-intro-b"   },
  { name: "longing", panel: "0-intro-c"   },
  { name: "offer",   panel: "1-build-a"   },
  { name: "portal",  panel: "1-build-d"   },
  { name: "spawn",   panel: "2-drop1-a"   },
  { name: "lagoon",  panel: "2-drop1-c"   }, // ▶ DROP 2
  { name: "carve",   panel: "4-vortex-a"  },
  { name: "grab",    panel: "4-vortex-b"  },
  { name: "gag",     panel: "5-drop2-a"   },
  { name: "pixsies", panel: "5-drop2-c"   },
  { name: "shatter", panel: "6-outro-d"   },
  { name: "smile",   panel: "7-resolve-d" },
];
const PANEL = Object.fromEntries(
  BEATS.map((b) => [b.name, `${OUT}/amaythingra-p-sec-${b.panel}.png`]),
);

// The panel carries the LOOK — including WHICH of the story's three render
// modes it is (photoreal office / glossy Meta-blue metaverse / Meta-blue
// polygon glitch). The motion prompt must never restyle it.
const MEDIUM_MOTION = [
`HOLD THE INPUT FRAME'S RENDER STYLE EXACTLY for every frame — same medium, same palette, same grade, same level of realism as the still you were given. Do NOT restyle, do NOT drift toward photorealism, do NOT drift toward cartoon, do NOT add film grain, bloom, lens flare or colour shifts that are not already in the frame. If the frame is a cold monotone META BLUE (#0668E1) metaverse render, every frame stays cold monotone Meta blue — never rainbow, never warm, never grey.`,
`Clean real-time game-engine motion: crisp and sharp, readable at a glance. Movement is carried by pose, weight and camera — not by heavy motion blur or soft focus.`,
`A TALL 9:16 portrait frame, full-bleed, never letterboxed. The camera move keeps the subject in the middle band; the very top and very bottom stay calm (a wordmark and player UI will sit there). Nothing story-critical drifts into the outer edges mid-move.`,
`Pacing is 120 BPM deep house — confident, driving, unhurried. One clear motion idea per shot, carried all the way through; no frantic camera, no whip pans, no cuts inside the shot.`,
].join("\n\n");

const POV_GUARD =
`FIRST-PERSON POV INTEGRITY — this is jeffrey's OWN viewpoint; his eyes ARE the camera. The only part of him that may appear is his own hands / forearms in the near foreground. ABSOLUTELY NO second jeffrey anywhere in any frame — no other man resembling him, no mirror or reflection or screen showing his face. Any other people are clearly DIFFERENT individuals.`;

const SHOTS = {
  office: {
    motion:
`Jeffrey slouches deeper into his office chair, idly spinning the pen once between his fingers, the dead grey monitor light flickering almost imperceptibly across his face. His eyes drift off the screen and away. The camera creeps in on him a hair, very slowly. Nothing else in the grey office moves — the stillness is the point. Flat, bored, restless.`,
  },
  longing: {
    motion:
`${POV_GUARD}\n\nThe view breathes gently — jeffrey's own forearms rest still on the desk edge in the near foreground while the focus pulls slowly OFF the grey keyboard and mug and ONTO the woman in grey across the office, who comes sharp. She shifts slightly at her desk, unaware of being watched, and the viewpoint holds on her a beat too long before drifting a fraction away, caught. A stolen glance. Quiet, aching, small.`,
  },
  offer: {
    motion:
`Mark Zuckerberg leans further onto the cubicle partition mid-pitch, pushing the sleek grey VR headset an insistent inch closer to jeffrey, his eyebrows lifting in a bright salesman's grin. Jeffrey leans back a little further away from it in his chair, wary, his eyes flicking down to the device and back up. The camera holds steady on the two of them. Eager versus unsure.`,
  },
  portal: {
    motion:
`The tall glowing rectangular portal of pale light tears WIDER open in the middle of the grey office, its edges crackling, the grey low-poly beach inside resolving brighter. The office breaks apart around jeffrey — desks, partitions and ceiling tiles peeling off into slow floating low-poly polygon shards that drift up past the camera. Jeffrey, silhouetted, takes one slow step toward the light as Mark sweeps an arm to usher him through. The camera pushes in low toward the portal. Awe, vertigo, the threshold.`,
  },
  spawn: {
    motion:
`Jeffrey's avatar ASSEMBLES out of nothing in the blank Meta-blue void — glowing polygon shards and wireframe panels streaking in from every direction and snapping into place up his body, the burgundy hoodie resolving last, a wireframe horizon grid drawing itself in beneath his feet in a single sweeping pass. He blinks into existence and looks down at himself. The camera rises slowly up his body as he forms. Uncanny, electric, the arrival. Monochrome META BLUE only — never rainbow, never polychrome.`,
  },
  lagoon: {
    // ▶ DROP 2 lands on this shot's first frame — the reveal IS the drop.
    motion:
`THE REVEAL, timed to a huge musical drop — the camera pulls back and cranes UP in one big confident continuous move, and the full Meta-blue lagoon opens out beneath it: rank after rank of the identical grey selfie-girl avatars stretching all the way back to the horizon, every single one filming herself, none of them moving anything but their phone arms. Far out across the flat silver water, the drone-war plumes erupt in slow silent columns. Near jeffrey, the wakeboard finishes materializing on the surface with a ripple. The scale of it keeps growing as the camera rises. Overwhelming, cold, magnificent, ironic.`,
  },
  carve: {
    motion:
`Jeffrey carves HARD across the flat Meta-blue water, leaning deep into the turn so the board bites and throws a big crisp fan of grey spray off its edge, weaving fast through the maze of frozen grey replicas. The camera tracks with him low and fast, just off the water. On the horizon the drone-war plumes bloom bigger and closer. He is grinning — wild, alive, in flow for the first time.`,
  },
  grab: {
    motion:
`Mark Zuckerberg's back and shoulder fill the near foreground as he LUNGES and clamps a hand onto the back of jeffrey's burgundy hoodie, yanking him backward mid-ride. Jeffrey's board slews sideways, water churning up in a hard grey wall as he wrenches against the grip, straining forward, refusing to be pulled back. The camera shudders slightly with the impact and holds tight on the tug-of-war. Possessive, alarming, the conflict erupting.`,
  },
  gag: {
    motion:
`The comedy beat, played completely straight: jeffrey's board plows at full speed straight through a tight cluster of the grey selfie-girl replicas, and they topple sideways one after another like dominoes — stiff, rigid, arms locked — splashing over into the Meta-blue water. Every single one of them keeps her phone raised and her blissed-out duck-lipped selfie expression PERFECTLY unchanged as she falls, still filming, not one flicker of reaction, not one head turning. Jeffrey rockets on through, delighted. Gleeful chaos against total non-reaction.`,
  },
  pixsies: {
    motion:
`Warm and unhurried — jeffrey steps off the board and the diverse band at the glowing round table turn to welcome him in, leaning back to open a space for him, real smiles, a hand raised in greeting; someone slides the little handmade gadget across the table toward him. The candlelight flickers on their faces. The camera drifts gently in and settles at the table with them, as if pulling up a chair. The cold Meta-blue lagoon stays far behind. Relief, belonging, the human antidote.`,
  },
  shatter: {
    motion:
`The entire Meta-blue world SHATTERS — the lagoon, the sky and the ranks of grey figures all blowing apart at once into a rushing storm of glowing low-poly polygon shards that stream and reverse violently PAST the camera, faster and faster, toward a hard white-out that fills the last frames completely. No figures. Pure violent transition, the yank back through. Monochrome META BLUE shards against a deep blue-black void — never rainbow, never polychrome.`,
  },
  smile: {
    motion:
`Back in the quiet grey office, at last: jeffrey and the woman in grey stand a couple of feet apart, and a real, shy, genuine smile breaks slowly across BOTH their faces — his first, hers answering it. She tucks her hair back. Nothing else happens; nothing needs to. The camera holds very still and close on the two of them, easing in a hair. Warm, hopeful, human, changed. The loop closes.`,
  },
};

// struct for the pipeline = 12 equal beats.
const structPath = `${OUT}/amaythingra-reel.motion-struct.json`;
mkdirSync(OUT, { recursive: true });
writeFileSync(structPath, JSON.stringify({
  totalSec: TOTAL_SEC, // ClipWizard's StructFile requires it
  sections: BEATS.map((b, i) => ({
    name: b.name, startSec: i * BEAT_SEC, endSec: (i + 1) * BEAT_SEC,
  })),
}, null, 2));

const flags = parseFlags();

const cfg = {
  slug: "amaythingra-reel",
  laneDir: LANE,
  structPath,
  panelFor: (name) => PANEL[name],
  shots: SHOTS,
  mediumMotion: MEDIUM_MOTION,
  ratio: "9:16",
  audio: `${OUT}/amaythingra.mp3`,
  finalOut: `${OUT}/amaythingra-reel.mp4`,
};

if (!flags.assemble) {
  await runMotionCli(cfg, flags);
  process.exit(0);
}

// ── portrait assembly: picked takes → exact trims → concat → mux ──────
// Emits a chrome-less BASE at 1080×1920@30 (Seedance 720p upscaled) plus
// meta-reel.json with the beat timings, then hands off to chrome-reel.mjs
// — pals stamps, "Amaythingra" columns, progress bar, timecode — which
// writes the FINAL cuts. (The pipeline's own --assemble is landscape-
// hardcoded; this is the portrait twin, same split as momboba's.)
cfg.motionDir = `${LANE}/out/motion`;
const motionDir = cfg.motionDir;
const W = 1080, H = 1920, FPS = 30;
const shots = shotList(cfg);
const takesPath = `${motionDir}/takes.json`;
const takes = existsSync(takesPath) ? JSON.parse(readFileSync(takesPath, "utf8")) : {};
const START = Number(flags["audio-start"] ?? AUDIO_START);
const BASE = `${motionDir}/base-reel.mp4`;

console.log(`▸ assembling portrait reel base · ${shots.length} beats × ${BEAT_SEC.toFixed(2)}s · ${W}x${H}@${FPS} · audio @ ${START.toFixed(2)}s (drop 2 → beat ${DROP_BEAT})`);
const trimmed = [];
for (const s of shots) {
  let picked = takes[s.name] ? resolve(motionDir, takes[s.name]) : s.out;
  if (picked !== s.out) console.log(`  ☑ ${s.name}: ${takes[s.name]}`);
  const t = `${motionDir}/trim-${s.i}-${s.name}.mp4`;
  let res;
  if (existsSync(picked)) {
    // All cuts here — the generated clip runs ceil(exact) seconds, so the
    // excess comes off the TAIL (the shot opens on its input panel).
    res = spawnSync("ffmpeg", [
      "-y", "-i", picked,
      "-t", s.exact.toFixed(3),
      "-vf", `scale=${W}:${H}:force_original_aspect_ratio=increase,crop=${W}:${H},fps=${FPS}`, "-an",
      "-c:v", "libx264", "-preset", "medium", "-crf", "17", "-pix_fmt", "yuv420p",
      t,
    ], { stdio: ["ignore", "ignore", "pipe"] });
  } else {
    // No take → Ken Burns the panel so the cut still completes.
    const frames = Math.round(s.exact * FPS);
    console.log(`  ⚠ ${s.name}: no take — Ken Burns fallback (${frames}f)`);
    res = spawnSync("ffmpeg", [
      "-y", "-loop", "1", "-i", s.image,
      "-vf", `scale=-2:7680,zoompan=z='1+0.10*on/${frames}':x='iw/2-(iw/zoom)/2':y='ih/2-(ih/zoom)/2':d=${frames}:s=${W}x${H}:fps=${FPS}`,
      "-frames:v", String(frames), "-an",
      "-c:v", "libx264", "-preset", "medium", "-crf", "17", "-pix_fmt", "yuv420p",
      t,
    ], { stdio: ["ignore", "ignore", "pipe"] });
  }
  if (res.status !== 0) { console.error(`✗ trim failed ${s.name}: ${res.stderr.toString().slice(-300)}`); process.exit(1); }
  trimmed.push(t);
}

const listPath = `${motionDir}/concat-reel.txt`;
writeFileSync(listPath, trimmed.map((t) => `file '${t}'`).join("\n") + "\n");

console.log("  concatenating + muxing audio …");
const mux = spawnSync("ffmpeg", [
  "-y", "-f", "concat", "-safe", "0", "-i", listPath,
  "-ss", String(START), "-t", String(TOTAL_SEC), "-i", cfg.audio,
  "-map", "0:v", "-map", "1:a",
  "-af", `afade=t=in:st=0:d=1.5,afade=t=out:st=${TOTAL_SEC - 4}:d=4`,
  "-c:v", "copy", "-c:a", "aac", "-b:a", "256k", "-shortest",
  "-movflags", "+faststart", BASE,
], { stdio: ["ignore", "ignore", "pipe"] });
if (mux.status !== 0) { console.error(`✗ mux failed: ${mux.stderr.toString().slice(-400)}`); process.exit(1); }
console.log(`✓ base ${BASE}`);

// beat timings for the chrome pass's per-beat tint + progress segments
writeFileSync(`${motionDir}/meta-reel.json`, JSON.stringify({
  total: TOTAL_SEC,
  slides: shots.map((s) => ({ name: s.name, from: s.i * BEAT_SEC, to: (s.i + 1) * BEAT_SEC })),
}, null, 2));

// ClipWizard reads the lane convention: out/<slug>.struct.json + a
// <slug>.mp3 in REEL time (the 56 s excerpt, fades baked) so its
// per-section audio slices line up with the takes.
writeFileSync(`${OUT}/${cfg.slug}.struct.json`, readFileSync(structPath));
const wizAudio = spawnSync("ffmpeg", [
  "-y", "-v", "error", "-ss", String(START), "-t", String(TOTAL_SEC), "-i", cfg.audio,
  "-af", `afade=t=in:st=0:d=1.5,afade=t=out:st=${TOTAL_SEC - 4}:d=4`,
  "-c:a", "libmp3lame", "-b:a", "256k", `${OUT}/${cfg.slug}.mp3`,
], { stdio: ["ignore", "ignore", "pipe"] });
if (wizAudio.status !== 0) console.warn("  ⚠ ClipWizard audio excerpt failed (board still works without ♪)");

// two chromed cuts off the one base: the full-chrome story and the
// pals-only reel (the --reel split: bar + timecode stripped, because the
// Reels UI brings its own furniture); previous cuts archive, never clobber
for (const variant of ["story", "reel"]) {
  const cut = `${OUT}/amaythingra-${variant}.mp4`;
  if (existsSync(cut)) {
    const dir = `${motionDir}/archive`;
    mkdirSync(dir, { recursive: true });
    const base = cut.split("/").pop().replace(/\.mp4$/, "");
    const n = readdirSync(dir).filter((f) => f.startsWith(base + ".v")).length + 1;
    renameSync(cut, `${dir}/${base}.v${n}.mp4`);
    console.log(`  ⌂ archived previous ${variant} cut → archive/${base}.v${n}.mp4`);
  }
  const chrome = spawnSync("node", [`${HERE}/chrome-reel.mjs`, "--variant", variant], { stdio: ["ignore", "inherit", "inherit"] });
  if (chrome.status !== 0) { console.error(`✗ chrome pass (${variant}) failed`); process.exit(1); }
}
