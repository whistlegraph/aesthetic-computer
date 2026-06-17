#!/usr/bin/env node
// marimba/bin/gen-motion-marimbaba-reel.mjs — the marimbaba REEL motion pass.
// The same 10-beat lullaby story as the YouTube motion cut
// (gen-motion-marimbaba.mjs), but reframed 9:16 PORTRAIT for the
// instagram reel: the existing portrait panels (out/marimbaba-p-sec-*.png,
// drawn for the score reel) become the Seedance 2.0 inputs, and the
// motion prompts ride the IG-story framing instead of the YouTube
// landscape one. Thin lane driver over pop/lib/motion-pipeline.mjs — the
// SHOTS table is the creative surface; pricing, generation, take
// archiving and picks live there.
//
// Slug is "marimbaba-reel" so the 9:16 takes
// (out/motion/marimbaba-reel-shot-*.mp4) sit alongside — never clobber —
// the landscape "marimbaba-shot-*" YouTube takes.
//
// ASSEMBLY IS NOT the pipeline's --assemble (that trimmer is
// 1280x720-hardcoded): --assemble here runs a PORTRAIT cut — picked
// takes (takes.json respected) trimmed to each section's REAL struct
// length at 1080x1920, concatenated, muxed with the full marimbaba
// track — then hands off to chrome-reel.mjs (pals stamps + title, NO
// progress bar / timecode: Reels bring their own furniture).
//
// Usage:
//   node pop/marimba/bin/gen-motion-marimbaba-reel.mjs --dry-run     # prompts + cost
//   node pop/marimba/bin/gen-motion-marimbaba-reel.mjs               # gen missing shots (9:16)
//   node pop/marimba/bin/gen-motion-marimbaba-reel.mjs --only baba2 --force  # re-roll
//   node pop/marimba/bin/gen-motion-marimbaba-reel.mjs --assemble    # portrait cut + chrome
//   node pop/bin/audition-motion.mjs --lane marimba --slug marimbaba-reel
//
// Shot grammar: CUTS by default; morphTo ONLY same-camera escalations
// (cross-scene morphs invent doubled figures — see pop-motion-pipeline).

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { runMotionCli, parseFlags, shotList } from "../../lib/motion-pipeline.mjs";
import { COLORED_PENCIL_TOOTH_MOTION, FRAMING_IG_STORY_PORTRAIT_MOTION } from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;

// Same lullaby light + colored-pencil medium as the YouTube cut, but the
// frame is now the tall IG-story portrait instead of the landscape one.
const MEDIUM_MOTION = [
  `Diegetic warm desk-lamp light only. Quiet, hushed, late-night lullaby pacing — all motion soft and unhurried.`,
  FRAMING_IG_STORY_PORTRAIT_MOTION,
  COLORED_PENCIL_TOOTH_MOTION,
].join("\n\n");

// The motion prompts are the proven YouTube-cut descriptions — they
// describe ACTION, not aspect, so they carry straight over; the portrait
// panels + the IG framing block above do the reframing. Morphs stay on
// the same same-camera escalations as the landscape cut.
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

const flags = parseFlags();

const cfg = {
  slug: "marimbaba-reel",
  laneDir: LANE,
  structPath: `${OUT}/marimbaba.struct.json`,
  // portrait panels drawn for the score reel — same 10 sections. The 5
  // beats whose Bill Gates likeness tripped fal's partner moderation
  // (twinkle1/twinkle2/wow1/wow2/sleep2) are regenerated face-hidden via
  // `gen-sections.mjs --faceless` → `.faceless.png`; prefer those when present.
  panelFor: (name, i) => {
    const faceless = `${OUT}/marimbaba-p-sec-${i}-${name}.faceless.png`;
    return existsSync(faceless) ? faceless : `${OUT}/marimbaba-p-sec-${i}-${name}.png`;
  },
  shots: SHOTS,
  mediumMotion: MEDIUM_MOTION,
  ratio: "9:16",
  audio: `${OUT}/marimbaba.mp3`,
  finalOut: `${OUT}/marimbaba-reel.mp4`,
};

if (!flags.assemble) {
  await runMotionCli(cfg, flags);
  process.exit(0);
}

// ── portrait assembly: picked takes → exact trims → concat → mux ──────
// Emits a chrome-less BASE at 1080×1920@30 (Seedance 720p upscaled) plus
// meta-reel.json with the section timings, then hands off to
// chrome-reel.mjs — pals stamps + "Marimbaba" title, NO progress bar /
// timecode — which writes the FINAL marimbaba-reel.mp4.
cfg.motionDir = `${LANE}/out/motion`;
const motionDir = cfg.motionDir;
const W = 1080, H = 1920, FPS = 30;
const shots = shotList(cfg);
const takesPath = `${motionDir}/takes.json`;
const takes = existsSync(takesPath) ? JSON.parse(readFileSync(takesPath, "utf8")) : {};
const BASE = `${motionDir}/base-marimbaba-reel.mp4`;

const totalSec = shots.reduce((a, s) => a + s.exact, 0);
console.log(`▸ assembling portrait reel base · ${shots.length} sections · ${totalSec.toFixed(1)}s · ${W}x${H}@${FPS}`);
const trimmed = [];
for (const s of shots) {
  let picked = takes[s.name] ? resolve(motionDir, takes[s.name]) : s.out;
  if (picked !== s.out) console.log(`  ☑ ${s.name}: ${takes[s.name]}`);
  const t = `${motionDir}/trim-reel-${s.i}-${s.name}.mp4`;
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

const listPath = `${motionDir}/concat-marimbaba-reel.txt`;
writeFileSync(listPath, trimmed.map((t) => `file '${t}'`).join("\n") + "\n");

console.log("  concatenating + muxing audio …");
const mux = spawnSync("ffmpeg", [
  "-y", "-f", "concat", "-safe", "0", "-i", listPath,
  "-i", cfg.audio,
  "-map", "0:v", "-map", "1:a",
  "-af", `afade=t=in:st=0:d=1.5,afade=t=out:st=${(totalSec - 4).toFixed(3)}:d=4`,
  "-c:v", "copy", "-c:a", "aac", "-b:a", "256k", "-shortest",
  "-movflags", "+faststart", BASE,
], { stdio: ["ignore", "ignore", "pipe"] });
if (mux.status !== 0) { console.error(`✗ mux failed: ${mux.stderr.toString().slice(-400)}`); process.exit(1); }
console.log(`✓ base ${BASE}`);

// section timings for the chrome pass's per-section tint
let acc = 0;
const slides = shots.map((s) => { const from = acc; acc += s.exact; return { name: s.name, from, to: acc }; });
writeFileSync(`${motionDir}/meta-marimbaba-reel.json`, JSON.stringify({ total: totalSec, slides }, null, 2));

const chrome = spawnSync("node", [`${HERE}/chrome-reel.mjs`], { stdio: ["ignore", "inherit", "inherit"] });
if (chrome.status !== 0) { console.error(`✗ chrome pass failed`); process.exit(1); }
