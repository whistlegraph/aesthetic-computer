#!/usr/bin/env node
// momboba/bin/gen-motion-reel.mjs — the momabobasheep REEL motion pass.
// Seedance 2.0 over the 8 felt story beats (reel/out/NN-*.png), 9:16
// portrait for the instagram reel / story. Thin lane driver over
// pop/lib/motion-pipeline.mjs — the SHOTS table is the creative
// surface; pricing, generation, take archiving and picks live there.
//
// ASSEMBLY IS NOT the pipeline's --assemble (that trimmer is
// landscape-hardcoded, see comodiddies/twofa/story/gen-motion.mjs):
// --assemble here runs a portrait cut — picked takes (takes.json
// respected) trimmed to each beat's length at 720x1280, concatenated,
// muxed with a 56 s excerpt of the track that swells into THE DREAM
// chapter's golden-section climax (audio starts at --audio-start,
// default 320 s).
//
// Usage:
//   node pop/momboba/bin/gen-motion-reel.mjs --dry-run        # prompts + cost
//   node pop/momboba/bin/gen-motion-reel.mjs                  # gen missing shots
//   node pop/momboba/bin/gen-motion-reel.mjs --only dream --force  # re-roll (archives old take)
//   node pop/momboba/bin/gen-motion-reel.mjs --assemble       # portrait cut
//   node pop/bin/audition-motion.mjs --lane momboba --slug momabobasheep-reel
//
// Shot grammar: ALL CUTS — the sleepy pace wants them, and cross-scene
// morphs invent doubled figures (see pop-motion-pipeline memory).

import { readFileSync, writeFileSync, existsSync, mkdirSync, readdirSync, renameSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { runMotionCli, parseFlags, shotList } from "../../lib/motion-pipeline.mjs";
import { NEEDLE_FELT_WOOL_MOTION, FRAMING_IG_STORY_PORTRAIT_MOTION } from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;
const REEL = `${LANE}/reel/out`;

const BEAT_SEC = 7; // 8 beats × 7 s = 56 s reel

const MEDIUM_MOTION = [
  `Hushed museum light. Sleepy lullaby pacing — every motion tiny, slow, and tender; nothing sudden anywhere.`,
  FRAMING_IG_STORY_PORTRAIT_MOTION,
  NEEDLE_FELT_WOOL_MOTION,
].join("\n\n");

// The 8 felt beats, in story order — names match reel/NN-<name>.txt.
const BEATS = [
  "arrival", "boba-sip", "galleries", "waterlily-awe",
  "heavy-eyes", "first-sheep", "asleep-flock", "dream",
];
const PANEL = {
  arrival: `${REEL}/01-arrival.png`,
  "boba-sip": `${REEL}/02-boba-sip.png`,
  galleries: `${REEL}/03-galleries.png`,
  "waterlily-awe": `${REEL}/04-waterlily-awe.png`,
  "heavy-eyes": `${REEL}/05-heavy-eyes.png`,
  "first-sheep": `${REEL}/06-first-sheep.png`,
  "asleep-flock": `${REEL}/07-asleep-flock.png`,
  dream: `${REEL}/08-dream.png`,
};

// struct for the pipeline = 8 equal beats.
const structPath = `${LANE}/reel/motion-struct.json`;
writeFileSync(structPath, JSON.stringify({
  totalSec: BEATS.length * BEAT_SEC, // ClipWizard's StructFile requires it
  sections: BEATS.map((name, i) => ({ name, startSec: i * BEAT_SEC, endSec: (i + 1) * BEAT_SEC })),
}, null, 2));

// The panel carries the LOOK; the prompt carries only MOTION.
// BOBA CONTINUITY (jas): the cup DRAINS across the story — full at
// arrival, drunk down beat by beat, finished by the time he sleeps —
// and it NEVER leaves his grip while he's awake (shot 0 + 3 of the
// first pass let it float free mid-air; the grip language is the fix).
const SHOTS = {
  arrival: {
    // outside → inside as ONE continuous move (jas): the camera follows
    // him through the doors and lands on boba-sip's opening frame. The
    // one morph in the set — scripted as a single tracking move with
    // only ever ONE of him in frame (the doubled-figure guard).
    morphTo: "boba-sip",
    motion:
`One continuous tracking shot following the felt jeffrey doll — the only figure in the shot from first frame to last. He walks up the felt sidewalk toward the museum entrance, each step a small unhurried puppet stride, and the camera follows him from behind at walking pace as he pushes through the big felt-framed glass doors and steps inside. The daylight gives way to hushed white atrium light as the camera glides through the doorway with him, swings gently around his shoulder, and settles close on his face just as he raises the fat pastel straw to his mouth — landing exactly on the final frame. He carries a small felt SHOULDER BAG on a single strap slung across his body, hanging at his hip — exactly the bag in the first frame, NOT a backpack, never two straps, never worn on both shoulders. The tiny felted boba is FULL — milk tea to the brim, dark wool pearls thick at the bottom — and stays firmly wrapped in the fingers of his hand for the entire walk; it never leaves his grip, never floats, never hovers. Hopeful, calm, sleepy.`,
  },
  "boba-sip": {
    motion:
`Close on the felt jeffrey doll taking one long happy sip from the fat pastel straw — his cheeks hollow slightly, eyes half-lidded with satisfaction, the dark wool tapioca pearls stirring and thinning as the milk-tea level visibly drops inside the pale cup. Both his hands cradle the cup the whole time. Behind him the big soft felt mobile in the out-of-focus atrium turns very slowly. The camera holds nearly still, drifting in by a hair.`,
  },
  galleries: {
    motion:
`The felt jeffrey doll strolls through the gallery corridor at a slowing dawdle, the boba — about half drunk now, milk-tea line at the cup's middle — held snug in his hand the whole shot, his head turning gently from one framed abstract felt artwork to the next, posture softening as he goes. The camera tracks alongside him at his slowing pace. Quiet top-light; nothing else in the corridor moves.`,
  },
  "waterlily-awe": {
    motion:
`The felt jeffrey doll stands tiny and still before the monumental felted water-lily tapestry, head tilting slowly further back in awe. The boba is nearly finished — only a last inch of milk tea, a few wool pearls left — and hangs low at his side but stays SECURELY gripped in his curled felt fingers for the entire shot; it never slips, never floats, never leaves his hand. The camera rises slowly up the vast wall of wool blues and greens, keeping the small doll and his cup anchored at the bottom of the frame throughout. Reverent, dim, hushed.`,
  },
  "heavy-eyes": {
    motion:
`On the felt bench, the felt jeffrey doll's eyelids droop lower and lower, his head nodding slowly toward his shoulder, catching itself once with a tiny start, then sinking again. His red felt glasses slip a little further down his wool nose. The boba — drained to the last sip, just a few dark pearls in the bottom — stands perfectly still on the bench beside him where he set it down. The camera holds still. The pond tapestry glows softly behind.`,
  },
  "first-sheep": {
    motion:
`The single needle-felted sheep peeks further in from the gallery doorway with one slow careful step, woolly head turning toward the drowsy felt jeffrey doll, whose head has fully drooped on the bench in the middle distance. A tiny curious lean from the sheep, a deeper slump from the doll. The camera holds wide and still — the moment before counting begins.`,
  },
  "asleep-flock": {
    // the ASCENSION (jas): beat 7 morphs into the dream — he floats up
    // off the bench and the camera follows him INTO the tapestry,
    // landing on the dream panel. As he lifts, the finished boba tips
    // over and spills its last pearls. One continuous move, one figure.
    morphTo: "dream",
    motion:
`One continuous magical shot — the felt jeffrey doll is the only figure throughout. He lies fully asleep along the bench among the small flock of needle-felted sheep, his side rising and falling in a slow sleep-breath rhythm. Then, gently, he begins to ASCEND — his sleeping body lifts weightlessly off the bench, still curled in the same sleeping pose, drifting up toward the monumental felted water-lily tapestry. As he rises, the empty boba cup beside his head tips over on the bench and rolls a quarter turn, the last few dark wool pearls spilling out across the felt. The sheep lift their woolly heads to watch him go. The camera floats up with him as the water-lily wool fills the entire frame and the tapestry's surface becomes the dream's rippling pond — landing exactly on the final frame: him curled asleep on the giant lily pad. Weightless, tender, magical.`,
  },
  dream: {
    motion:
`Inside the painting: the felt jeffrey doll floats curled-up and asleep on the giant wool lily pad, rocking almost imperceptibly as the felted pond ripples in slow wool waves of blue, green and lilac around him. Above, the two woolly sheep drift slowly across the felt sky like clouds. The camera drifts upward and away with the sheep, weightless. The happiest, slowest ending.`,
  },
};

const flags = parseFlags();

const cfg = {
  slug: "momabobasheep-reel",
  laneDir: LANE,
  structPath,
  panelFor: (name) => PANEL[name],
  shots: SHOTS,
  mediumMotion: MEDIUM_MOTION,
  ratio: "9:16",
  audio: `${OUT}/momabobasheep.mp3`,
  finalOut: `${OUT}/momabobasheep-reel.mp4`,
};

if (!flags.assemble) {
  await runMotionCli(cfg, flags);
  process.exit(0);
}

// ── portrait assembly: picked takes → exact trims → concat → mux ──────
// Emits a chrome-less BASE at 1080×1920@30 (Seedance 720p upscaled) plus
// meta.json with the beat timings, then hands off to chrome-reel.mjs —
// pals stamps, "Momabobasheep" columns, progress bar, timecode — which
// writes the FINAL momabobasheep-reel.mp4 (same split as the twofa
// story's build.mjs → chrome.mjs).
cfg.motionDir = `${LANE}/out/motion`;
const motionDir = cfg.motionDir;
const W = 1080, H = 1920, FPS = 30;
const shots = shotList(cfg);
const takesPath = `${motionDir}/takes.json`;
const takes = existsSync(takesPath) ? JSON.parse(readFileSync(takesPath, "utf8")) : {};
const AUDIO_START = Number(flags["audio-start"] ?? 320); // swells into THE DREAM climax (~360 s)
const BASE = `${motionDir}/base-reel.mp4`;

console.log(`▸ assembling portrait reel base · ${shots.length} beats × ${BEAT_SEC}s · ${W}x${H}@${FPS} · audio @ ${AUDIO_START}s`);
const trimmed = [];
for (const s of shots) {
  let picked = takes[s.name] ? resolve(motionDir, takes[s.name]) : s.out;
  if (picked !== s.out) console.log(`  ☑ ${s.name}: ${takes[s.name]}`);
  const t = `${motionDir}/trim-${s.i}-${s.name}.mp4`;
  let res;
  if (existsSync(picked)) {
    // Cut shots open on their input panel — trim the tail. MORPH shots
    // arrive on the next cut's panel in their FINAL frames — trim the
    // head so the arrival kisses the next section's opening.
    const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
      "-of", "default=noprint_wrappers=1:nokey=1", picked], { encoding: "utf8" });
    const clipDur = Number(probe.stdout?.trim()) || 0;
    const head = s.endImage ? Math.max(0, clipDur - s.exact) : 0;
    if (head > 0.01) console.log(`  ↪ ${s.name}: morph — trimming ${head.toFixed(2)}s from head`);
    res = spawnSync("ffmpeg", [
      "-y", "-i", picked,
      ...(head > 0.01 ? ["-ss", head.toFixed(3)] : []),
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

const totalSec = shots.length * BEAT_SEC;
console.log("  concatenating + muxing audio …");
const mux = spawnSync("ffmpeg", [
  "-y", "-f", "concat", "-safe", "0", "-i", listPath,
  "-ss", String(AUDIO_START), "-t", String(totalSec), "-i", cfg.audio,
  "-map", "0:v", "-map", "1:a",
  "-af", `afade=t=in:st=0:d=1.5,afade=t=out:st=${totalSec - 4}:d=4`,
  "-c:v", "copy", "-c:a", "aac", "-b:a", "256k", "-shortest",
  "-movflags", "+faststart", BASE,
], { stdio: ["ignore", "ignore", "pipe"] });
if (mux.status !== 0) { console.error(`✗ mux failed: ${mux.stderr.toString().slice(-400)}`); process.exit(1); }
console.log(`✓ base ${BASE}`);

// beat timings for the chrome pass's per-beat tint + progress segments
writeFileSync(`${motionDir}/meta-reel.json`, JSON.stringify({
  total: totalSec,
  slides: shots.map((s) => ({ name: s.name, from: s.i * BEAT_SEC, to: (s.i + 1) * BEAT_SEC })),
}, null, 2));

// ClipWizard reads the lane convention: out/<slug>.struct.json + a
// <slug>.mp3 in REEL time (the 56 s excerpt, fades baked) so its
// per-section audio slices line up with the takes.
writeFileSync(`${OUT}/${cfg.slug}.struct.json`, readFileSync(structPath));
const wizAudio = spawnSync("ffmpeg", [
  "-y", "-v", "error", "-ss", String(AUDIO_START), "-t", String(totalSec), "-i", cfg.audio,
  "-af", `afade=t=in:st=0:d=1.5,afade=t=out:st=${totalSec - 4}:d=4`,
  "-c:a", "libmp3lame", "-b:a", "256k", `${OUT}/${cfg.slug}.mp3`,
], { stdio: ["ignore", "ignore", "pipe"] });
if (wizAudio.status !== 0) console.warn("  ⚠ ClipWizard audio excerpt failed (board still works without ♪)");

// two chromed cuts off the one base: the full-chrome story and the
// pals-only reel (marimbaba's --reel split: bar + timecode stripped);
// previous cuts archive rather than clobber
for (const variant of ["story", "reel"]) {
  const cut = `${OUT}/momabobasheep-${variant}.mp4`;
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
