#!/usr/bin/env node
// build-deck.mjs — assemble the Restless Egg concept film from the 4 stills
// (gen-deck.mjs output) as a Ken Burns slideshow, cut on the VO sentence
// boundaries, with the jeffrey-pvc narration laid over an optional notepat bed.
// No generative video — slow ffmpeg zoompan moves over real stills.
//
// Usage: node build-deck.mjs   →   restless-egg-film.mp4

import { readFileSync, existsSync, mkdtempSync, writeFileSync, rmSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const FPS = 30;
const W = 1920, H = 1080;

// beat cut points from the VO alignment (sentence-end times). 5 periods,
// 4 beats — beat 4 spans from the 3rd period to the last.
const align = JSON.parse(readFileSync(join(HERE, "vo-narrative.json"), "utf8")).normalized_alignment;
const ends = align.character_end_times_seconds;
const periodEnds = align.characters
  .map((c, i) => (c === "." ? ends[i] : null))
  .filter((t) => t !== null);
const cuts = [periodEnds[0], periodEnds[1], periodEnds[2], periodEnds[periodEnds.length - 1]];
const durs = [cuts[0], cuts[1] - cuts[0], cuts[2] - cuts[1], cuts[3] - cuts[2]];
const total = cuts[3];

// Ken Burns move per beat: [zStart, zEnd]. >1 zoom-in, reverse for pull-back.
const BEATS = [
  { still: "still-1-painter.png",    z: [1.00, 1.12] }, // slow push-in
  { still: "still-2-awakening.png",  z: [1.02, 1.18] }, // push-in onto the bloom
  { still: "still-3-commons.png",    z: [1.16, 1.00] }, // pull-back to reveal the galaxy
  { still: "still-4-invitation.png", z: [1.00, 1.10] }, // gentle settle
];

for (const b of BEATS) {
  if (!existsSync(join(HERE, b.still))) {
    console.error(`✗ missing ${b.still} — run gen-deck.mjs first`);
    process.exit(1);
  }
}

const work = mkdtempSync(join(tmpdir(), "re-deck-"));
const clips = [];

BEATS.forEach((b, i) => {
  const dur = durs[i];
  const N = Math.round(dur * FPS);
  const [z0, z1] = b.z;
  // per-output-frame zoom, centered pan. `on` is the current output frame.
  const z = `${z0}+(${(z1 - z0).toFixed(5)})*on/${N}`;
  const clip = join(work, `clip-${i}.mp4`);
  const vf =
    `scale=3840:2160:force_original_aspect_ratio=increase,crop=3840:2160,` +
    `zoompan=z='${z}':d=${N}:x='iw/2-(iw/zoom/2)':y='ih/2-(ih/zoom/2)':s=${W}x${H}:fps=${FPS},` +
    `format=yuv420p`;
  console.log(`▸ clip ${i + 1}/4  ${b.still}  ${dur.toFixed(2)}s  z ${z0}→${z1}`);
  execFileSync("ffmpeg", [
    "-y", "-loop", "1", "-i", join(HERE, b.still),
    "-t", dur.toFixed(3), "-r", String(FPS),
    "-vf", vf, "-c:v", "libx264", "-pix_fmt", "yuv420p", clip,
  ], { stdio: ["ignore", "ignore", "inherit"] });
  clips.push(clip);
});

// concat the 4 clips (hard cuts)
const listFile = join(work, "list.txt");
writeFileSync(listFile, clips.map((c) => `file '${c}'`).join("\n"));
const silent = join(work, "video.mp4");
execFileSync("ffmpeg", ["-y", "-f", "concat", "-safe", "0", "-i", listFile, "-c", "copy", silent],
  { stdio: ["ignore", "ignore", "inherit"] });

// audio: VO over an optional notepat bed (look for a likely bed file).
const vo = join(HERE, "vo-narrative.wav");
const bedCandidates = ["notepat-bed.wav", "notepat-bed.mp3", "bed.wav", "bed.mp3"].map((f) => join(HERE, f));
const bed = bedCandidates.find((f) => existsSync(f));
const out = join(HERE, "restless-egg-film.mp4");

const args = ["-y", "-i", silent, "-i", vo];
if (bed) args.push("-i", bed);
if (bed) {
  args.push("-filter_complex",
    `[2:a]volume=0.18,afade=t=out:st=${(total - 1.5).toFixed(2)}:d=1.5[bed];` +
    `[1:a][bed]amix=inputs=2:duration=first:dropout_transition=0[a]`,
    "-map", "0:v", "-map", "[a]");
  console.log(`  bed: ${bed.split("/").pop()}`);
} else {
  args.push("-map", "0:v", "-map", "1:a");
  console.log("  bed: (none — VO only)");
}
args.push("-c:v", "copy", "-c:a", "aac", "-b:a", "192k", "-shortest", out);
execFileSync("ffmpeg", args, { stdio: ["ignore", "ignore", "inherit"] });

rmSync(work, { recursive: true, force: true });
console.log(`✓ ${out}  (~${total.toFixed(1)}s)`);
