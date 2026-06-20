#!/usr/bin/env node
// pop/bin/gen-ref-motion.mjs — reference-to-video via Seedance 2.0 on
// fal.ai. Generate a FRESH clip steered by --prompt, pulling style /
// motion / sound from reference media (not frame-locked restyling).
//
// Refer to the refs in the prompt by index: @Image1, @Video1, @Audio1.
// Caps: ≤9 images (≤30MB ea) · ≤3 videos (combined 2–15s, <50MB total,
// ~480–720p) · ≤3 audio (combined ≤15s, ≤15MB ea). Audio needs ≥1
// image or video alongside it.
//
// Usage:
//   node pop/bin/gen-ref-motion.mjs \
//     --video ~/Downloads/ref-prep/5-video-720p.mp4 \
//     --audio ~/Downloads/ref-prep/5-audio.mp3 \
//     --prompt "..." \
//     [--image path.png]          # repeatable: --image a --image b
//     [--duration auto]           # 4..15 | auto
//     [--ratio auto]              # auto | 21:9 | 16:9 | 4:3 | 1:1 | 3:4 | 9:16
//     [--resolution 720p]         # 480p | 720p | 1080p
//     [--tier fast]               # fast | standard
//     [--no-audio]                # disable synthesized audio (default ON)
//     [--seed N] [--out path.mp4]
//
// FAL_KEY from env or vault devcontainer.env.

import { existsSync } from "node:fs";
import { resolve, basename } from "node:path";
import { generateReferenceShot, RATE_PER_SEC } from "../lib/fal-seedance.mjs";

// Parse flags; --image/--video/--audio repeat into arrays.
const flags = {};
const multi = { image: [], video: [], audio: [] };
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const k = a.slice(2);
  const next = process.argv[i + 1];
  const val = next !== undefined && !next.startsWith("--") ? (i++, next) : true;
  if (k in multi && val !== true) multi[k].push(val);
  else flags[k] = val;
}

const abs = (p) => resolve(process.cwd(), p.replace(/^~/, process.env.HOME));
const PROMPT = flags.prompt;
if (!PROMPT || PROMPT === true) { console.error("✗ --prompt required"); process.exit(1); }

const images = multi.image.map(abs);
const videos = multi.video.map(abs);
const audios = multi.audio.map(abs);
for (const p of [...images, ...videos, ...audios]) {
  if (!existsSync(p)) { console.error(`✗ ref not found: ${p}`); process.exit(1); }
}
if (!videos.length && !images.length) {
  console.error("✗ need at least one --video or --image ref"); process.exit(1);
}

const TIER = flags.tier || "fast";
const DURATION = String(flags.duration ?? "auto");
const RESOLUTION = flags.resolution || "720p";
const RATIO = flags.ratio || "auto";
const OUT = flags.out
  ? abs(flags.out)
  : abs(`~/Downloads/ref-prep/ref-motion-${TIER}-${DURATION}s.mp4`);

console.log(`▸ reference-to-video · ${TIER} ${RESOLUTION} ${DURATION}s ${RATIO}`);
if (images.length) console.log(`  images: ${images.map((p) => basename(p)).join(", ")}`);
if (videos.length) console.log(`  videos: ${videos.map((p) => basename(p)).join(", ")}`);
if (audios.length) console.log(`  audio:  ${audios.map((p) => basename(p)).join(", ")}`);
console.log(`  prompt: ${PROMPT.slice(0, 140)}${PROMPT.length > 140 ? "…" : ""}`);
if (DURATION !== "auto") {
  const est = Number(DURATION) * RATE_PER_SEC[TIER];
  console.log(`  ~$${est.toFixed(2)} @720p (video refs get a 0.6× multiplier)`);
}
if (flags["dry-run"]) { console.log("  (dry run — not submitting)"); process.exit(0); }

const res = await generateReferenceShot({
  images, videos, audios, prompt: PROMPT,
  duration: DURATION, ratio: RATIO, resolution: RESOLUTION, tier: TIER,
  audio: flags["no-audio"] !== true,
  seed: flags.seed ? Number(flags.seed) : null,
  outPath: OUT, label: "ref",
});

if (!res.ok) { console.error(`✗ ${res.error}`); process.exit(1); }
console.log(`✓ ${OUT.replace(process.env.HOME, "~")} (${(res.bytes / 1e6).toFixed(1)} MB, seed ${res.seed}, ${res.seconds.toFixed(0)}s)`);
