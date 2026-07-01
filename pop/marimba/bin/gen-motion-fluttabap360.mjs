#!/usr/bin/env node
// marimba/bin/gen-motion-fluttabap360.mjs — the fluttabap360 MOTION pass
// ("the monarch keeper"). Each hand-drawn colored-pencil beat-master panel
// (out/_fluttabap360-beat-<name>.png) becomes the source frame for a
// Seedance 2.0 image-to-video shot. The panel carries the LOOK; the prompt
// below carries only MOTION. Clips download to out/motion/fluttabap360-<beat>.mp4.
//
// This is a STANDALONE driver (not pop/lib/motion-pipeline.mjs's runMotionCli)
// because fluttabap360.struct.json is event-based, not the sections[] shape
// the shared CLI expects — and because there is exactly one panel per beat
// NAME (the visualizer reuses each per pass), so the natural unit here is the
// beat, not the struct section. It still leans on the same fal-seedance.mjs
// client (caching, queue-resume, retries) the rest of /pop motion uses.
//
// Moderation: fal rejects face-forward likenesses. The story keeps the
// keeper face-away / mid-motion / dark on purpose; a FAILED shot is logged
// and SKIPPED (never crashes the batch) so the face-safe beats still land.
//
// Usage:
//   node pop/marimba/bin/gen-motion-fluttabap360.mjs --dry-run          # prompts + cost
//   node pop/marimba/bin/gen-motion-fluttabap360.mjs --only intro,fly,button   # PROOF (3 clips)
//   node pop/marimba/bin/gen-motion-fluttabap360.mjs                    # gen all missing beats
//   node pop/marimba/bin/gen-motion-fluttabap360.mjs --only cave --force        # re-roll one
//
// Each shot caches: a finished out/motion/fluttabap360-<beat>.mp4 is skipped;
// --force re-rolls it (the in-flight queue sidecar resumes a killed job, never
// double-bills). CUTS only — no morphs (the visualizer cross-fades passes).

import { existsSync, mkdirSync, statSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { generateShot, RATE_PER_SEC } from "../../lib/fal-seedance.mjs";
import {
  COLORED_PENCIL_TOOTH_MOTION,
  FRAMING_IG_STORY_PORTRAIT_MOTION,
} from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;
const MOTION = `${OUT}/motion`;

// Warm hand-drawn park light + tall IG-story portrait frame + colored-pencil
// tooth, exactly as the panels (and as the story.txt medium note demands:
// diegetic clean morning/park sun, NO glow/neon/motion-blur, NEVER a
// photograph, the keeper NEVER looks at camera — eyes follow the monarchs).
const MEDIUM_MOTION = [
  `Diegetic clean morning/park sunlight only — no glow filters, no neon, no motion-blur language, never cinematic, never a photograph. Hand-drawn colored-pencil + gouache on warm cream paper. The butterfly cosplayer KEEPER never looks at the camera — his eyes always follow the painted-paper monarch butterflies. Motion is graceful, unhurried, ballet-soft; the monarchs drift on their own gentle arcs.`,
  FRAMING_IG_STORY_PORTRAIT_MOTION,
  COLORED_PENCIL_TOOTH_MOTION,
].join("\n\n");

// One entry per beat NAME (matches out/_fluttabap360-beat-<name>.png).
// `lead` ranks the proof / face-safety order: lower = least face-forward
// (run first). dur in seconds (clamped 4–15).
const BEATS = {
  intro: {
    lead: 0,
    dur: 5,
    motion:
`Dawn over the empty park lawn, dew on the grass. ONE painted-paper monarch rests folded on a tall grass blade; a fat dew-drop hanging from the blade tip swells and FALLS, the blade springing gently back. The monarch's wings give one slow half-open breath. Far off and tiny at the frame edge, the keeper barely stirs, just arriving. Long soft pre-sun shadows shift a hair. The faint hand-ruled ground-grid in the lawn stays almost invisible. The whole world holding its breath before the first note — only the dew-drop and the one wing-breath move.`,
  },
  fly: {
    lead: 1,
    dur: 6,
    motion:
`Seen from behind and below: the keeper rises into a full ballet lift — relevé pushing up onto the balls of his feet, back arching, both arms sweeping up and curving overhead in a slow port de bras, the paper-mache monarch wings on his back tipping open with the motion. Four or five painted-paper monarchs circle his lifted hands and arched back, all turning upward together on the same rising arc. Open joy, chin lifting away from camera. Late-morning sun. A current of grace lifting.`,
  },
  button: {
    lead: 2,
    dur: 4,
    motion:
`The keeper at rest on the lawn, seen from behind, the dance done. He lifts one open hand and ONE last painted-paper monarch peels off his fingertip and rises slowly into the warm gold sky, wings opening as it climbs. His head tilts back a touch to follow it up, away from camera. The soft ground-shadow anchors his foot. A held breath out — everything else still. The quiet goodbye.`,
  },
  cave: {
    lead: 3,
    dur: 6,
    motion:
`The dark beat. The keeper stands very still in the mouth of a cool computational cave / deep arching shade, head bowed, seen mostly in silhouette from the side, the dusk-blue lawn falling away behind him. ONE monarch perched on his lowered hand opens and closes its dim half-folded wings once, very slowly — the data resting, the system breathing. The faint ground-grid glows a touch more visible in the low light (hand-drawn, never neon). A single slow sway of his shoulders as he holds his breath. Somber, hushed, almost motionless.`,
  },
  slinky: {
    lead: 4,
    dur: 6,
    motion:
`The keeper begins to move — a slinky, loose, spiralling step, body uncoiling, arms drawing a long S-curve through the air, seen three-quarter from behind. Three or four painted-paper monarchs trace the SAME spiralling path beside him on their arcs, learning his shape. Faint dotted hand-drawn signal-arcs describe the shared spiral. The wings on his back tip open with the motion. Playful, gathering momentum, sap-green and amber light.`,
  },
  progression: {
    lead: 5,
    dur: 6,
    motion:
`The keeper steps back OUT of the cave-shade into bright golden light, seen from behind, the monarchs streaming back to him across the lawn and gathering — a rising procession of three, four, five painted-paper butterflies converging on his lifting hands as he re-enters the dance, brighter and higher. The recommitment, sun fully up. Hands rising, the butterflies funnelling in.`,
  },
  land: {
    lead: 6,
    dur: 6,
    motion:
`The keeper comes gently down out of the lift, descending from relevé back to flat-footed, arms lowering in a soft port de bras, seen three-quarter from behind. The monarchs settle around him — one returning to rest on his fingertip, others alighting on the grass and on his shoulders. The motion resolves, weight returning to the earth. Warm, satisfied, spent. Evening-gold light.`,
  },
  ride: {
    lead: 7,
    dur: 6,
    motion:
`A quick mischievous beat: the keeper mid-turn, seen from behind and side, ONE monarch swooping in low and another answering up high — a call-and-response chase, his head whipping to follow the high one (away from camera). Two crossing hand-drawn signal-arcs, one low one high, describe the duet. Light, fast, a little wild, bright lime-gold light.`,
  },
  palofmine: {
    lead: 8,
    dur: 5,
    motion:
`Three-quarter from behind: a single monarch settles for a beat on the keeper's outstretched fingertip, the two of them regarding each other. His curly head tilts, antenna-headband springs with felt balls bobbing slightly. One or two more monarchs drift in on their own little arcs at the frame edges. The monarch's wings breathe slowly open and shut on his fingertip. Warm and companionable, warm-cream light.`,
  },
  butterfly: {
    lead: 9,
    dur: 5,
    motion:
`The hook wakes: the keeper, mid-frame and seen three-quarter from behind, lifts one open hand and the resting monarch UNFOLDS and lifts off toward his fingertips — the very first flutter. A faint hand-drawn dotted signal-arc trails the butterfly's path. Morning sun warming the lawn behind him. He shifts his weight onto the ball of one foot, just beginning the dance.`,
  },
  mommywow: {
    lead: 10,
    dur: 5,
    motion:
`A held, breath-held beat: the keeper goes still, both hands cupped softly open at chest height, seen three-quarter from behind, head lowered. A monarch hovers a hand-span above his open palms — neither touching, its wings beating slowly in place. The lawn behind blurs into warm cream paper. Soft awe, time suspended — only the hovering monarch's wings and a faint breath move. Soft gold-cream light.`,
  },
};

function parseFlags(argv = process.argv) {
  const flags = {};
  for (let i = 2; i < argv.length; i++) {
    const a = argv[i];
    if (!a.startsWith("--")) continue;
    const next = argv[i + 1];
    if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
    else { flags[a.slice(2)] = next; i++; }
  }
  return flags;
}

const flags = parseFlags();
const TIER = flags.tier || "fast";
const RATE = RATE_PER_SEC[TIER];
const ONLY = flags.only ? String(flags.only).split(",").map((s) => s.trim()) : null;

mkdirSync(MOTION, { recursive: true });

// Build the work list, ordered face-safest first (`lead`).
const shots = Object.entries(BEATS)
  .map(([name, b]) => ({
    name,
    lead: b.lead,
    dur: Math.min(15, Math.max(4, b.dur)),
    image: `${OUT}/_fluttabap360-beat-${name}.png`,
    out: `${MOTION}/fluttabap360-${name}.mp4`,
    prompt: `${b.motion}\n\n${MEDIUM_MOTION}`.trim(),
  }))
  .filter((s) => (ONLY ? ONLY.includes(s.name) : true))
  .sort((a, b) => a.lead - b.lead);

if (ONLY) {
  const missing = ONLY.filter((n) => !BEATS[n]);
  if (missing.length) { console.error(`✗ unknown beat(s): ${missing.join(", ")}`); process.exit(1); }
}

for (const s of shots) {
  if (!existsSync(s.image)) { console.error(`✗ panel missing: ${s.image}`); process.exit(1); }
}

const billed = shots.reduce((a, s) => a + s.dur, 0);
console.log(`▸ fluttabap360 motion · ${shots.length} shot(s) · ${billed}s billed · ~$${(billed * RATE).toFixed(2)} @ ${TIER} 720p 9:16`);
console.log(`  order (face-safest first): ${shots.map((s) => s.name).join(" → ")}`);

if (flags["dry-run"]) {
  for (const s of shots) {
    console.log(`\n── ${s.name} · ${s.dur}s\n${s.prompt}`);
  }
  process.exit(0);
}

// Generate sequentially so a moderation reject on one beat is obvious in the
// log and the batch keeps going to the next face-safer beat.
let made = 0, skipped = 0, failed = 0;
const rejects = [];
for (const s of shots) {
  if (!flags.force && existsSync(s.out)) {
    console.log(`· ${s.name}: cached (${(statSync(s.out).size / 1e6).toFixed(1)} MB) — --force to re-roll`);
    skipped++;
    continue;
  }
  console.log(`\n▶ ${s.name} · ${s.dur}s → ${s.out.replace(LANE + "/", "")}`);
  const r = await generateShot({
    image: s.image,
    prompt: s.prompt,
    duration: s.dur,
    ratio: "9:16",
    resolution: "720p",
    tier: TIER,
    outPath: s.out,
    label: s.name,
  });
  if (r.ok) {
    made++;
    console.log(`✓ ${s.name} · seed ${r.seed} · ${(r.bytes / 1e6).toFixed(1)} MB · ${r.seconds.toFixed(0)}s`);
  } else {
    failed++;
    const moderated = /moderation|content|policy|reject|nsfw|face/i.test(r.error || "");
    rejects.push({ name: s.name, moderated, error: r.error });
    console.error(`✗ ${s.name}: ${moderated ? "MODERATION" : "FAILED"} — ${r.error}`);
    console.error(`  (skipping ${s.name}, continuing batch)`);
  }
}

console.log(`\n── done · ${made} made · ${skipped} cached · ${failed} failed`);
if (rejects.length) {
  console.log("  rejects:");
  for (const r of rejects) console.log(`    ${r.name}${r.moderated ? " [moderation]" : ""}: ${(r.error || "").slice(0, 120)}`);
}
process.exit(failed && !made && !skipped ? 1 : 0);
