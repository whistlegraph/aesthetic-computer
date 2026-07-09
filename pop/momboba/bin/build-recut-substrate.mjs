#!/usr/bin/env node
// momboba/bin/build-recut-substrate.mjs — assemble the momabobasheep BUNNY
// recut with the two new reel post-process phases:
//
//   CRITIC PASS  — inspect critical transitions/edits for semantic breaks
//     and fix them. Here: the dream→sip morph's framed-painting POPS to a
//     smaller scale at the seam (Seedance reframed the morph relative to
//     the dream shot). Fix = a scale RAMP on the morph: start zoomed to
//     match the dream's last frame, ease to native by the drink so the far
//     seam into boba-sip still matches. (First entry in what should grow
//     into a reusable transition critic — see NOTE at bottom.)
//
//   COLOR SUBSTRATE — pop/lib/reel-substrate.mjs. Homogenize color across
//     all clips BEFORE stitching (gray-world mean-match + shared grade),
//     crossfade-stitch, then a filmic final (sharpen + S-curve + split-tone
//     + vignette + grain). The video analog of the /pop audio substrate.
//
// Order of ops: critic-fix morph → homogenize each clip → xfade chain →
// filmic final → mux audio → write meta. Then chrome-reel.mjs stamps pals.
//
// Usage: node pop/momboba/bin/build-recut-substrate.mjs   (then chrome)

import { spawnSync } from "node:child_process";
import { writeFileSync } from "node:fs";
import { homogenizeClips, filmicFinal } from "../../lib/reel-substrate.mjs";

const M = "/Users/jas/aesthetic-computer/pop/momboba/bunny/out/motion";
const MP3 = "/Users/jas/aesthetic-computer/pop/momboba/out/momabobasheep.mp3";
const AUDIO_START = 320, XFADE = 0.7, W = 1080, H = 1920, FPS = 30;
const ff = (args) => { const r = spawnSync("ffmpeg", ["-y", "-v", "error", ...args], { stdio: ["ignore", "ignore", "pipe"] }); if (r.status !== 0) throw new Error(r.stderr?.toString().slice(-400)); };

// ── CRITIC PASS: fix the dream→morph scale pop ────────────────────────
// The morph's framed painting sits ~1.17× smaller than the dream shot's
// last frame. Ramp a center zoom 1.17→1.0 across the morph via a
// time-animated crop, so the near seam (dream) and far seam (boba-sip)
// both match. z(t) = 1.17 - 0.17·(t/dur).
function fixMorphScale(inPath, outPath, z0 = 1.17) {
  const dur = Number(spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
    "-of", "default=noprint_wrappers=1:nokey=1", inPath], { encoding: "utf8" }).stdout?.trim()) || 5;
  const dec = ((z0 - 1) / (dur * FPS)).toFixed(6); // per-frame zoom-out step
  // zoompan ramps a center zoom from z0 → 1.0 over the clip, constant
  // output size (crop-based ramps break the encoder with variable frames).
  const vf =
    `zoompan=z='if(eq(on,0),${z0},max(1.0,zoom-${dec}))':` +
    `x='iw/2-(iw/zoom)/2':y='ih/2-(ih/zoom)/2':d=1:s=${W}x${H}:fps=${FPS},format=yuv420p`;
  ff(["-i", inPath, "-vf", vf, "-an", "-c:v", "libx264", "-preset", "medium", "-crf", "17", outPath]);
  return outPath;
}

console.log("▸ critic pass — fixing dream→morph scale pop …");
fixMorphScale(`${M}/trim-dreamsip.mp4`, `${M}/trim-dreamsip-fixed.mp4`);

// ── clip order (the morph now uses the scale-fixed version) ───────────
const clips = [
  { name: "arrival", path: `${M}/recut-arrival-half.mp4`, dur: 2.5 },
  { name: "galleries", path: `${M}/trim-2-galleries.mp4`, dur: 7 },
  { name: "waterlily-awe", path: `${M}/trim-3-waterlily-awe.mp4`, dur: 7 },
  { name: "first-sheep", path: `${M}/trim-5-first-sheep.mp4`, dur: 7 },
  { name: "heavy-eyes", path: `${M}/trim-4-heavy-eyes.mp4`, dur: 7 },
  { name: "asleep-flock", path: `${M}/trim-6-asleep-flock.mp4`, dur: 7 },
  { name: "dream", path: `${M}/trim-7-dream.mp4`, dur: 7 },
  { name: "dreamsip", path: `${M}/trim-dreamsip-fixed.mp4`, dur: 5 },
  { name: "boba-sip", path: `${M}/trim-1-boba-sip.mp4`, dur: 7 },
];

// ── COLOR SUBSTRATE phase 1: homogenize every clip before stitching ───
console.log("▸ color substrate — homogenizing " + clips.length + " clips (gray-world + shared grade) …");
const graded = homogenizeClips(clips, { gradeDir: M });
for (const g of graded) console.log(`  ${g.name}: mean(${g.mean.r.toFixed(0)},${g.mean.g.toFixed(0)},${g.mean.b.toFixed(0)}) gains(${g.gains.gr.toFixed(2)},${g.gains.gg.toFixed(2)},${g.gains.gb.toFixed(2)})`);

// ── stitch: full crossfade chain across the graded clips ──────────────
const inputs = graded.flatMap((g) => ["-i", g.graded]);
let filt = "", prev = "0", S = [0], acc = clips[0].dur;
for (let i = 1; i < clips.length; i++) {
  const off = (acc - XFADE).toFixed(3);
  const lbl = i === clips.length - 1 ? "vout" : `v${i}`;
  filt += `[${prev}][${i}]xfade=transition=fade:duration=${XFADE}:offset=${off}${i === clips.length - 1 ? ",format=yuv420p" : ""}[${lbl}];`;
  S.push(acc - XFADE);
  acc = acc + clips[i].dur - XFADE;
  prev = lbl;
}
filt = filt.replace(/;$/, "");
console.log(`▸ crossfade chain (${XFADE}s) → ${acc.toFixed(1)}s …`);
ff([...inputs, "-filter_complex", filt, "-map", "[vout]", "-c:v", "libx264", "-preset", "medium", "-crf", "17", "-pix_fmt", "yuv420p", "-an", `${M}/recut-stitched.mp4`]);

// ── COLOR SUBSTRATE phase 3: filmic final (sharpen + grade) ───────────
console.log("▸ color substrate — filmic final (sharpen + S-curve + split-tone + vignette + grain) …");
filmicFinal(`${M}/recut-stitched.mp4`, `${M}/recut-graded.mp4`);

// ── mux audio ─────────────────────────────────────────────────────────
const TOTAL = acc;
ff(["-i", `${M}/recut-graded.mp4`, "-ss", String(AUDIO_START), "-t", TOTAL.toFixed(3), "-i", MP3,
  "-map", "0:v", "-map", "1:a",
  "-af", `afade=t=in:st=0:d=1.5,afade=t=out:st=${(TOTAL - 4).toFixed(3)}:d=4`,
  "-c:v", "copy", "-c:a", "aac", "-b:a", "256k", "-shortest", "-movflags", "+faststart", `${M}/base-reel.mp4`]);

// ── meta (slide boundaries at xfade midpoints) for the chrome pass ────
const names = ["arrival", "galleries", "waterlily-awe", "first-sheep", "heavy-eyes", "asleep-flock", "dream", "boba-sip", "boba-sip"];
const bounds = [0, ...S.slice(1).map((s) => s + XFADE / 2), TOTAL];
const slides = names.map((name, i) => ({ name, from: +bounds[i].toFixed(3), to: +bounds[i + 1].toFixed(3) }));
writeFileSync(`${M}/meta-reel.json`, JSON.stringify({ total: +TOTAL.toFixed(3), slides }, null, 2));
console.log(`✓ base-reel.mp4 ${TOTAL.toFixed(1)}s + meta written — run chrome-reel.mjs next`);

// NOTE (reusability): homogenizeClips/filmicFinal already live in
// pop/lib/reel-substrate.mjs. The critic pass (fixMorphScale + boundary
// inspection) wants to grow into pop/lib/reel-critic.mjs: sample each
// transition's before/after frames, detect scale/color/subject pops
// (gpt-5.5 vision, like the illy QA), and emit fixes. This script is the
// first concrete caller of both phases.
