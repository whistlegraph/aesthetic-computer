// pop/lib/reel-substrate.mjs — the FILMIC COLOR SUBSTRATE for reels.
//
// The video analog of the audio "substrate" in the /pop flow: a unifying
// layer that ties disparate clips into one filmic whole. AI shot clips
// (Seedance) drift in white balance and exposure from shot to shot; a
// cut between two of them reads as a color jump. This module removes the
// jump and lays a shared filmic grade over everything.
//
// Three phases, in order:
//   1. homogenize — BEFORE stitching. Measure each clip's average color,
//      pull every clip toward the shared target (gray-world / mean match),
//      then lay the same gentle base grade on all of them. Now the clips
//      share a white point and tone, so cuts + crossfades don't jump.
//   2. (caller stitches the homogenized clips — xfade/concat)
//   3. filmicFinal — AFTER stitching. Sharpen a touch, gentle S-curve,
//      warm/cool split-tone, soft vignette, fine grain. The "color
//      substrate" that makes the whole reel feel shot on one stock.
//
// Kept deliberately gentle — the felt/wool look must stay soft, not
// crunchy. Tunable via the opts. Reusable by any reel lane's assembly;
// momboba's build-recut-substrate.mjs is the first caller.

import { spawnSync } from "node:child_process";

const ff = (args, opts = {}) =>
  spawnSync("ffmpeg", ["-y", "-v", "error", ...args], { stdio: ["ignore", "ignore", "pipe"], ...opts });

// ── measure a clip's average RGB by scaling whole frames to 1×1 ────────
// scale=1:1 collapses a frame to its mean pixel; sampling a few frames
// across the clip and averaging gives a robust per-clip color signature.
export function measureClipMean(path, samples = 5) {
  const dur = Number(spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
    "-of", "default=noprint_wrappers=1:nokey=1", path], { encoding: "utf8" }).stdout?.trim()) || 5;
  let r = 0, g = 0, b = 0, n = 0;
  for (let i = 0; i < samples; i++) {
    const t = (dur * (i + 0.5)) / samples;
    const out = spawnSync("ffmpeg", ["-v", "error", "-ss", t.toFixed(2), "-i", path,
      "-frames:v", "1", "-vf", "scale=1:1,format=rgb24", "-f", "rawvideo", "-"],
      { maxBuffer: 1 << 20 }).stdout;
    if (out && out.length >= 3) { r += out[0]; g += out[1]; b += out[2]; n++; }
  }
  return n ? { r: r / n, g: g / n, b: b / n } : { r: 128, g: 128, b: 128 };
}

// clamp a per-channel gain so a wildly-off clip can't blow out
const clampGain = (x, lo = 0.82, hi = 1.22) => Math.min(hi, Math.max(lo, x));

// ── phase 1: homogenize one clip toward the shared target + base grade ─
// gains pull the clip's mean to the target (WB + exposure match); the
// shared eq/curves is the common tone laid on every clip.
export function homogenizeClip(inPath, outPath, { mean, target, grade = {} }) {
  const gr = clampGain(target.r / Math.max(1, mean.r));
  const gg = clampGain(target.g / Math.max(1, mean.g));
  const gb = clampGain(target.b / Math.max(1, mean.b));
  const { contrast = 1.05, saturation = 1.06, gamma = 1.0 } = grade;
  const vf = [
    `lutrgb=r='clip(val*${gr.toFixed(4)},0,255)':g='clip(val*${gg.toFixed(4)},0,255)':b='clip(val*${gb.toFixed(4)},0,255)'`,
    `eq=contrast=${contrast}:saturation=${saturation}:gamma=${gamma}`,
    `format=yuv420p`, `fps=30`,
  ].join(",");
  const res = ff(["-i", inPath, "-vf", vf, "-an", "-c:v", "libx264", "-preset", "medium", "-crf", "17", outPath]);
  if (res.status !== 0) throw new Error(`homogenize ${inPath}: ${res.stderr?.toString().slice(-300)}`);
  return { gains: { gr, gg, gb } };
}

// homogenize a whole ordered clip list; returns the graded paths in order.
export function homogenizeClips(clips, { gradeDir, grade } = {}) {
  const means = clips.map((c) => measureClipMean(c.path));
  // shared target = channel-wise average of all clip means (gray-world).
  const target = ["r", "g", "b"].reduce((o, k) => (o[k] = means.reduce((a, m) => a + m[k], 0) / means.length, o), {});
  return clips.map((c, i) => {
    const out = `${gradeDir}/graded-${i}-${c.name}.mp4`;
    const { gains } = homogenizeClip(c.path, out, { mean: means[i], target, grade });
    return { ...c, graded: out, mean: means[i], gains };
  });
}

// ── phase 3: filmic final on the stitched reel — sharpen + grade ───────
// gentle unsharp, a soft filmic S-curve, a warm-shadow / cool-highlight
// split, a soft vignette, and fine temporal grain. This is the substrate.
export function filmicFinal(inPath, outPath, opts = {}) {
  const {
    sharpen = 0.6, contrast = 1.04, saturation = 1.03,
    warm = 6, vignette = "PI/5", grain = 4,
  } = opts;
  const vf = [
    `unsharp=5:5:${sharpen}:5:5:0.0`,
    // soft filmic S-curve — lifts shadows a hair, rolls highlights
    `curves=r='0/0.02 0.25/0.24 0.5/0.52 0.75/0.79 1/0.99':g='0/0.01 0.5/0.5 1/0.99':b='0/0.03 0.25/0.24 0.5/0.49 0.75/0.74 1/0.97'`,
    `eq=contrast=${contrast}:saturation=${saturation}`,
    // warm shadows / cool highlights split-tone
    `colorbalance=rs=${(warm / 100).toFixed(3)}:bs=${(-warm / 100).toFixed(3)}:rh=${(-warm / 150).toFixed(3)}:bh=${(warm / 150).toFixed(3)}`,
    `vignette=${vignette}`,
    `noise=alls=${grain}:allf=t`,
    `format=yuv420p`,
  ].join(",");
  const res = ff(["-i", inPath, "-vf", vf, "-an", "-c:v", "libx264", "-preset", "medium", "-crf", "17", outPath]);
  if (res.status !== 0) throw new Error(`filmicFinal: ${res.stderr?.toString().slice(-300)}`);
  return outPath;
}

// ── alternate final: "realistic through a LOMO lens" ───────────────────
// A lomography-camera grade — the opposite mood from filmicFinal's soft
// hand-drawn warmth: punchy saturation, cross-processed color (teal-cool
// shadows, warm-gold highlights), crushed contrast, a heavy dark vignette,
// a whisper of bloom for that plastic-lens dreaminess, and coarse grain.
// Reads less "illustration", more "shot on a Lomo/Holga toy camera".
export function lomoFinal(inPath, outPath, opts = {}) {
  const {
    saturation = 1.34, contrast = 1.14, sharpen = 0.5,
    vignette = "PI/3.3", grain = 9, bloom = 0.7,
  } = opts;
  const vf = [
    `unsharp=5:5:${sharpen}:5:5:0.0`,
    // cross-process curves: lift+cool the shadows, warm+roll the highlights
    `curves=r='0/0.00 0.25/0.22 0.5/0.55 0.75/0.82 1/1':g='0/0.03 0.5/0.5 1/0.97':b='0/0.10 0.25/0.24 0.5/0.44 0.75/0.66 1/0.88'`,
    `eq=contrast=${contrast}:saturation=${saturation}`,
    // teal shadows / gold highlights split — the classic lomo cross-process
    `colorbalance=rs=-0.08:gs=0.03:bs=0.08:rm=0.05:gm=0.02:bm=-0.03:rh=0.10:gh=0.02:bh=-0.10`,
    // toy-lens bloom: blend a soft-blurred copy back over the frame
    `split[a][b];[b]gblur=sigma=6[bl];[a][bl]blend=all_mode=screen:all_opacity=${bloom * 0.22}`,
    `vignette=${vignette}`,
    `noise=alls=${grain}:allf=t+u`,
    `format=yuv420p`,
  ].join(",");
  const res = ff(["-i", inPath, "-vf", vf, "-an", "-c:v", "libx264", "-preset", "medium", "-crf", "17", outPath]);
  if (res.status !== 0) throw new Error(`lomoFinal: ${res.stderr?.toString().slice(-300)}`);
  return outPath;
}
