// validate-clip.mjs <clip.mp4> — post-render QC for whistlegraph clips.
// The storyboard still validates the DESTINATION; this validates the JOURNEY.
// Samples the clip densely (24 frames) into numbered review sheets so a
// vision pass can check the temporal rules the keyframe can't encode:
//   1. STROKE COUNT — every element the choreography names appears as its
//      own stroke (three gills = three separate draws, not two-in-one).
//   2. CONTACT — new chalk only ever appears at the chalk tip; a mark that
//      shows up away from the hand between two samples is a violation.
//   3. PERSISTENCE — drawn marks never shift, vanish, or redraw.
//   4. END STATE — the last frame matches the validated end keyframe.
// Audio rule (whistle follows stroke) still needs ears — sheets can't hear.
import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";

const clip = process.argv[2];
if (!clip || !existsSync(clip)) {
  console.error("usage: validate-clip.mjs <clip.mp4>");
  process.exit(1);
}

const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "default=noprint_wrappers=1:nokey=1", clip], { encoding: "utf8" });
const dur = parseFloat(probe.stdout);
const fps = 24 / dur; // 24 samples spread across the clip

const base = clip.replace(/\.mp4$/i, "");
for (let sheet = 0; sheet < 3; sheet++) {
  const out = `${base}.qc${sheet + 1}.png`;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
    "-i", clip,
    "-vf", `fps=${fps},select='between(n\\,${sheet * 8}\\,${sheet * 8 + 7})',scale=200:356,tile=4x2`,
    "-vsync", "vfr", "-frames:v", "1", out], { stdio: "inherit" });
  if (r.status !== 0) process.exit(r.status);
  console.log(`✓ ${out}`);
}
console.log(`\n${dur.toFixed(1)}s → 3 sheets × 8 frames. Review against the shot's choreography:`);
console.log("  [ ] stroke count: each named element drawn as its own stroke");
console.log("  [ ] contact: new chalk only at the chalk tip, never elsewhere");
console.log("  [ ] persistence: existing marks never shift/vanish/redraw");
console.log("  [ ] end state: final frame matches the validated keyframe");
