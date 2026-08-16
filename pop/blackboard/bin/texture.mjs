#!/usr/bin/env node
// texture.mjs — the Ingold whisper texture stem, and an alt mix that carries it.
//
//   node pop/blackboard/bin/texture.mjs
//
// NON-RELEASABLE: the source is unlicensed lecture audio (Ingold, "Thinking
// through Making"). The stem and alt mix exist for the working version only
// and are named accordingly; the canonical master never includes them.
//
// Span 35.4–39.6 s of the platter clip = "when you stand at the blackboard
// and you scrape a line", washed to a whisper (band-limited, echo tail,
// -20 dB) and placed once in the intro at t = 6 s.

import { execFileSync } from "node:child_process";
import { existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const LANE = dirname(dirname(fileURLToPath(import.meta.url)));
const REPO = resolve(LANE, "../..");
const CLIP = resolve(REPO, "papers/lines-platter/sources/ingold-blackboard-clip.wav");
const STEM = resolve(LANE, "stems/ingold-texture-NONRELEASABLE.wav");
const MASTER = resolve(LANE, "out/blackboard.wav");
const ALT = resolve(LANE, "out/blackboard-alt-texture-NONRELEASABLE.wav");
const DURATION = 164.0; // pinned to the bed length, like everything else

const ffmpeg = (args) => execFileSync("ffmpeg", ["-v", "error", "-y", ...args], { stdio: ["ignore", "inherit", "inherit"] });

ffmpeg([
  "-ss", "35.4", "-to", "39.6", "-i", CLIP,
  "-af", [
    "highpass=f=220", "lowpass=f=2400",
    "aecho=0.7:0.75:90|180:0.35|0.22",
    "afade=t=in:d=0.35", "afade=t=out:st=3.4:d=0.8",
    "volume=-20dB",
    "adelay=6000|6000", `apad=whole_dur=${DURATION}`,
  ].join(","),
  "-ar", "44100", "-ac", "2", STEM,
]);
console.log(`texture stem → ${STEM}`);

if (existsSync(MASTER)) {
  ffmpeg([
    "-i", MASTER, "-i", STEM,
    "-filter_complex", "[0:a][1:a]amix=inputs=2:duration=first:normalize=0[out]",
    "-map", "[out]", ALT,
  ]);
  console.log(`alt mix → ${ALT}`);
} else {
  console.log(`master not rendered yet (${MASTER}) — run again after sing.mjs finishes for the alt mix`);
}
