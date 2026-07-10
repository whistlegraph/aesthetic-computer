#!/usr/bin/env node
// marimba/bin/build-fluttabap-substrate.mjs — the CLEAN, watermark-free
// finish for the fluttabap360 BUNNY reel, with the reel COLOR SUBSTRATE
// (pop/lib/reel-substrate.mjs) — the same treatment momboba's recut got.
//
// The motion driver's --assemble already emits a chrome-LESS base (no pals
// stamps) plus the per-beat trim clips. This script skips the chrome pass
// entirely (no watermark) and instead:
//   1. homogenize — pull every trim clip toward a shared gray-world target
//      + a common base grade, so shot-to-shot white balance doesn't jump.
//   2. concat — HARD cuts on the beat, exactly as the driver designed the
//      loop-clean phrase (NOT crossfades — the metamorphosis cuts on rhythm).
//   3. filmic final — sharpen + S-curve + split-tone + vignette + grain.
//   4. mux the loop-phrase audio (same window + anti-click fades as driver).
//
// Run AFTER: gen-motion-fluttabap360-bunny-reel.mjs --assemble
// Usage:     node pop/marimba/bin/build-fluttabap-substrate.mjs
// Output:    pop/marimba/out/fluttabap360-bunny-reel-clean.mp4

import { spawnSync } from "node:child_process";
import { existsSync, writeFileSync } from "node:fs";
import { homogenizeClips, filmicFinal, lomoFinal } from "../../lib/reel-substrate.mjs";

const LANE = "/Users/jas/aesthetic-computer/pop/marimba";
const SLUG = "fluttabap360-bunny-reel";
const M = `${LANE}/out/motion-bunny`;
const AUDIO = `${LANE}/out/fluttabap360.mp3`;
const AUDIO_START = 34.7;

// --look filmic|lomo picks the final aesthetic; --reuse skips homogenize +
// stitch and re-grades the existing clean-stitched.mp4 (fast A/B of looks).
const argv = process.argv.slice(2);
const LOOK = (argv.includes("--look") ? argv[argv.indexOf("--look") + 1] : "filmic").toLowerCase();
const REUSE = argv.includes("--reuse");
const FINAL = LOOK === "lomo" ? lomoFinal : filmicFinal;
const OUT = `${LANE}/out/${SLUG}-${LOOK === "lomo" ? "lomo" : "clean"}.mp4`;
const STITCH = `${M}/clean-stitched.mp4`;

// same beat order as the driver; trims are named trim-<slug>-<i>-<name>.mp4
const ORDER = [
  "cocoon", "crack", "hatch", "hatchpov", "growdance",
  "laptops", "mallets", "laptopspov", "molt",
  "wing", "drone", "spacepov", "space",
];

const ff = (args) => {
  const r = spawnSync("ffmpeg", ["-y", "-v", "error", ...args], { stdio: ["ignore", "ignore", "pipe"] });
  if (r.status !== 0) throw new Error(r.stderr?.toString().slice(-400));
};
const probe = (p) => Number(spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "default=noprint_wrappers=1:nokey=1", p], { encoding: "utf8" }).stdout?.trim()) || 0;

// ── gather the per-beat trim clips (the driver's exact cuts) ───────────
const clips = ORDER.map((name, i) => {
  const path = `${M}/trim-${SLUG}-${i}-${name}.mp4`;
  if (!existsSync(path)) { console.error(`✗ missing trim ${path} — run --assemble first`); process.exit(1); }
  return { name, path, dur: probe(path) };
});
const TOTAL = clips.reduce((a, c) => a + c.dur, 0);
console.log(`▸ fluttabap substrate · ${LOOK.toUpperCase()} look · ${clips.length} beats · ${TOTAL.toFixed(1)}s · no chrome (watermark-free)`);

// ── COLOR SUBSTRATE phases 1+2: homogenize + hard-cut stitch ──────────
// (--reuse skips these and re-grades the existing stitch — fast look A/B.)
if (REUSE && existsSync(STITCH)) {
  console.log("▸ reusing existing homogenized stitch (--reuse) …");
} else {
  console.log("▸ color substrate — homogenizing (gray-world + shared grade) …");
  const graded = homogenizeClips(clips, { gradeDir: M });
  for (const g of graded) console.log(`  ${g.name}: gains(${g.gains.gr.toFixed(2)},${g.gains.gg.toFixed(2)},${g.gains.gb.toFixed(2)})`);
  const listPath = `${M}/concat-clean-${SLUG}.txt`;
  writeFileSync(listPath, graded.map((g) => `file '${g.graded}'`).join("\n") + "\n");
  console.log("▸ concat (hard beat-cuts) …");
  ff(["-f", "concat", "-safe", "0", "-i", listPath, "-c:v", "libx264", "-preset", "medium",
    "-crf", "17", "-pix_fmt", "yuv420p", "-an", STITCH]);
}

// ── COLOR SUBSTRATE phase 3: final grade (filmic or lomo) ─────────────
console.log(`▸ color substrate — ${LOOK} final grade …`);
FINAL(STITCH, `${M}/clean-graded.mp4`);

// ── mux the loop-phrase audio (same window + fades as the driver) ──────
console.log(`▸ muxing audio (${AUDIO_START}s → ${(AUDIO_START + TOTAL).toFixed(1)}s, loop-clean) …`);
ff(["-i", `${M}/clean-graded.mp4`, "-ss", String(AUDIO_START), "-t", TOTAL.toFixed(3), "-i", AUDIO,
  "-map", "0:v", "-map", "1:a",
  "-af", `afade=t=in:st=0:d=0.4,afade=t=out:st=${(TOTAL - 0.8).toFixed(3)}:d=0.8`,
  "-c:v", "copy", "-c:a", "aac", "-b:a", "256k", "-shortest", "-movflags", "+faststart", OUT]);

console.log(`✓ ${OUT} · ${TOTAL.toFixed(1)}s · ${LOOK} substrate (no watermark)`);
