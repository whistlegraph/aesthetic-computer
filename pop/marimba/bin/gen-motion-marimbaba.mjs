#!/usr/bin/env node
// marimba/bin/gen-motion-marimbaba.mjs — the marimbaba MOTION pass.
// Thin lane driver: the SHOTS table below is the creative surface;
// all workflow (pricing, generation, take versioning, picks, assembly)
// lives in pop/lib/motion-pipeline.mjs.
//
// Usage:
//   node pop/marimba/bin/gen-motion-marimbaba.mjs --dry-run   # prompts + cost
//   node pop/marimba/bin/gen-motion-marimbaba.mjs             # gen missing shots
//   node pop/marimba/bin/gen-motion-marimbaba.mjs --only baba2 --force  # re-roll (archives old take)
//   node pop/marimba/bin/gen-motion-marimbaba.mjs --assemble  # cut from picked takes
//   node pop/bin/audition-motion.mjs --lane marimba --slug marimbaba    # takes board
//
// Shot grammar: CUTS by default; morphTo ONLY same-camera escalations
// (cross-scene morphs invent doubled figures — see pop-motion-pipeline).

import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { runMotionCli } from "../../lib/motion-pipeline.mjs";
import { COLORED_PENCIL_TOOTH_MOTION, FRAMING_YT_LANDSCAPE_MOTION } from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;

// Every shot prompt = [viewport content above] + [formal framing] +
// [formal medium]. The formal blocks live in pop/lib/mediums.mjs and
// are shared with the panel generator so stills and motion agree.
const MEDIUM_MOTION = [
  `Diegetic warm desk-lamp light only. Quiet, hushed, late-night lullaby pacing — all motion soft and unhurried.`,
  FRAMING_YT_LANDSCAPE_MOTION,
  COLORED_PENCIL_TOOTH_MOTION,
].join("\n\n");

const SHOTS = {
  hush1: {
    motion:
`Slow gentle push-in along the garden path at night. The single figure walks unhurried toward the house, hands in pockets, coat shifting softly with each step. Dark tree branches sway slightly in a night breeze. The warm amber study window glows and flickers faintly, the small silhouette inside hunched at the desk barely stirring. A sliver of moon. The walker gets gradually closer to the door but never reaches it. Only this one figure outside — calm, expectant, quiet.`,
  },
  hush2: {
    motion:
`The two men hold the greeting — the man in the doorway gives a small shy wave, the older man at the typewriter smiles warmly and beckons him in with a little gesture. The desk lamp flame of light breathes gently. Dust motes drift through the lamp glow. Bookshelf shadows still. Soft, welcoming, quiet.`,
  },
  twinkle1: {
    morphTo: "twinkle2",
    motion:
`Seen from behind the two seated figures: their backs and shoulders breathe softly, heads tilting gently toward each other in quiet companionship. The warm lamp glow spills between their silhouettes and breathes slowly. The camera drifts forward over their shoulders, easing down toward the white typewriter on the desk beyond them, settling into an extreme close-up of the chrome type ball as a hand enters frame pointing at the mechanism, the type ball giving a small delighted spin, lamplight glinting off the chrome. Faces are never shown — the backs, the lamp, and the machine carry the shot.`,
  },
  twinkle2: {
    motion:
`Extreme close-up held on the silver chrome type ball — the machine is the entire subject. A finger taps gently at the mechanism; the type ball rotates and tilts in small precise mechanical movements, catching warm glints of lamplight on its chrome. The ribbon cartridge trembles faintly with each tick of the mechanism, and the paper-bail bar gives the slightest shiver. Everything above and behind the typewriter stays soft, still, and out of focus. The camera never moves.`,
  },
  wow1: {
    morphTo: "wow2",
    motion:
`Over-the-shoulder behind the younger man: his fingers strike the typewriter keys in a soft steady rhythm, the type ball twitching and spinning with each stroke, the carriage nudging along. The older man leans in attentively from the right, eyes on the mechanism. Then the camera pulls back and rises into a wide shot as something wonderful happens — both men lean back from the typewriter and break into bright delighted smiles, the older man raising a fist in joy, the younger laughing, gesturing with delight in the lamplit study.`,
  },
  wow2: {
    motion:
`Wide lamplit study, the brightest beat: both men celebrate — the older man claps his hands once and pumps a fist, the younger laughs warmly and shakes his head in happy disbelief, rocking back in his chair. The desk lamp glow seems to swell. The white typewriter sits proudly between them. Joyful but still soft and hand-drawn, never frantic.`,
  },
  baba1: {
    morphTo: "baba2",
    motion:
`The warmth drains. The younger man re-reads, his face clouding, brow knitting with concern; the older man leans back defensively, arms beginning to cross, a "what?" gesture. They turn slightly away from each other. The lamp dims subtly. Then tension rises — both men push back their chairs and STAND UP at the desk, gesturing emphatically, the older man pointing accusingly toward the typewriter, the younger gesturing back, faces flushing, the argument breaking open.`,
  },
  baba2: {
    motion:
`The standing argument holds at full heat: both men gesture emphatically across the desk, the older man jabbing his finger toward the typewriter, the younger throwing his hands up, mouths moving mid-word, shoulders tense. The typewriter sits between them, an obstacle. Harsh-edged shadows tremble. The motion is agitated but the drawing stays hand-made and hatched.`,
  },
  sleep1: {
    motion:
`The tug-of-war: both men grip the typed sheet at the same time, pulling — the paper visibly tearing along a ragged rip, shredded fibres springing loose, their fists tight, faces taut, eyes locked. The paper stays edge-on to the camera, never readable. The pulling intensifies in slow desperate heaves until the rip completes — the sheet tears fully in two and both men stagger back a step, each left holding a ragged half, frozen in the shock of it. The camera holds steady on the desk and typewriter between them throughout.`,
  },
  sleep2: {
    motion:
`The bittersweet goodbye holds: the man at the doorway lingers, half-turned, the torn paper half trembling slightly at his side, then turns slowly toward the dark night beyond the door. The older man remains slumped at the white typewriter, head bowed, the other torn half slipping slightly in his loose fingers. The lamp flickers low. Dust drifts. Heavy, quiet, final — the saddest, slowest motion of the set.`,
  },
};

await runMotionCli({
  slug: "marimbaba",
  laneDir: LANE,
  structPath: `${OUT}/marimbaba.struct.json`,
  panelFor: (name, i) => `${OUT}/marimbaba-yt-sec-${i}-${name}.png`,
  shots: SHOTS,
  mediumMotion: MEDIUM_MOTION,
  audio: `${OUT}/marimbaba.mp3`,
  finalOut: `${OUT}/marimbaba-motion-yt.mp4`,
});
