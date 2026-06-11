#!/usr/bin/env node
// pop/bin/gen-motion.mjs — animate one storyboard panel into a motion
// shot via Seedance 2.0 on fal.ai (image-to-video, first frame = the
// illy panel, optional last frame for panel→panel morphs).
//
// The illy panel carries the LOOK; the --prompt carries only MOTION
// (what moves, how the camera behaves). Tiers: fast (~$0.24/s @720p)
// or standard (~$0.30/s @720p) — keep pilots on fast.
//
// Usage:
//   node pop/bin/gen-motion.mjs \
//     --image pop/hellsine/out/hellsine-yt-sec-13-climax-a.png \
//     --prompt "felt figures dance, flames flicker..." \
//     [--end-image path.png]      # last frame → shot morph
//     [--duration 5]              # 4..15 | auto
//     [--ratio 16:9]              # auto | 21:9 | 16:9 | 4:3 | 1:1 | 3:4 | 9:16
//     [--resolution 720p]         # 480p | 720p
//     [--tier fast]               # fast | standard
//     [--audio]                   # default OFF — the track is the audio
//     [--seed N] [--out path.mp4]
//
// FAL_KEY from env or vault devcontainer.env.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}

function falKey() {
  if (process.env.FAL_KEY) return process.env.FAL_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      if (line.startsWith("FAL_KEY=")) {
        return line.slice("FAL_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
      }
    }
  }
  throw new Error("FAL_KEY not set and not found in vault devcontainer.env");
}

const KEY = falKey();
const IMAGE = flags.image && resolve(process.cwd(), flags.image);
const PROMPT = flags.prompt;
if (!IMAGE || !existsSync(IMAGE)) { console.error("✗ --image missing or not found"); process.exit(1); }
if (!PROMPT) { console.error("✗ --prompt required (motion description)"); process.exit(1); }

const TIER = flags.tier || "fast";
const ENDPOINT = TIER === "standard"
  ? "bytedance/seedance-2.0/image-to-video"
  : "bytedance/seedance-2.0/fast/image-to-video";
const DURATION = String(flags.duration ?? "5");
const RESOLUTION = flags.resolution || "720p";
const RATIO = flags.ratio || "auto";   // 16:9 for the YT cut; auto follows the input panel
const OUT = flags.out
  ? resolve(process.cwd(), flags.out)
  : IMAGE.replace(/\.png$/i, "") + `.motion-${TIER}-${DURATION}s.mp4`;

const mime = (p) => p.match(/\.jpe?g$/i) ? "image/jpeg" : p.match(/\.webp$/i) ? "image/webp" : "image/png";
const dataUri = (p) => `data:${mime(p)};base64,${readFileSync(p).toString("base64")}`;

const input = {
  prompt: PROMPT,
  image_url: dataUri(IMAGE),
  duration: DURATION,
  resolution: RESOLUTION,
  aspect_ratio: RATIO,
  generate_audio: flags.audio === true,
  end_image_url: undefined,
  seed: undefined,
};
if (flags["end-image"]) {
  const end = resolve(process.cwd(), flags["end-image"]);
  if (!existsSync(end)) { console.error("✗ --end-image not found"); process.exit(1); }
  input.end_image_url = dataUri(end);
}
if (flags.seed) input.seed = Number(flags.seed);

const auth = { Authorization: `Key ${KEY}`, "Content-Type": "application/json" };

console.log(`▸ ${basename(IMAGE)} → ${TIER} ${RESOLUTION} ${DURATION}s ${input.end_image_url ? "(first+last frame)" : "(first frame)"}`);
console.log(`  prompt: ${PROMPT.slice(0, 120)}${PROMPT.length > 120 ? "…" : ""}`);

const submit = await fetch(`https://queue.fal.run/${ENDPOINT}`, {
  method: "POST", headers: auth, body: JSON.stringify(input),
});
if (!submit.ok) {
  console.error(`✗ submit failed ${submit.status}: ${(await submit.text()).slice(0, 500)}`);
  process.exit(1);
}
const { request_id, status_url, response_url } = await submit.json();
console.log(`  queued: ${request_id}`);

let status = "";
const t0 = Date.now();
while (status !== "COMPLETED") {
  await new Promise((r) => setTimeout(r, 4000));
  const res = await fetch(status_url, { headers: auth });
  const body = await res.json();
  if (body.status !== status) {
    status = body.status;
    console.log(`  ${status.toLowerCase()} · ${((Date.now() - t0) / 1000).toFixed(0)}s`);
  }
  if (status === "FAILED" || body.error) {
    console.error(`✗ generation failed: ${JSON.stringify(body).slice(0, 500)}`);
    process.exit(1);
  }
}

const result = await (await fetch(response_url, { headers: auth })).json();
const videoUrl = result.video?.url;
if (!videoUrl) { console.error(`✗ no video in response: ${JSON.stringify(result).slice(0, 400)}`); process.exit(1); }
const mp4 = Buffer.from(await (await fetch(videoUrl)).arrayBuffer());
writeFileSync(OUT, mp4);
console.log(`✓ ${OUT.replace(REPO + "/", "")} (${(mp4.length / 1e6).toFixed(1)} MB, seed ${result.seed})`);
