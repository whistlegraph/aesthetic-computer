// pop/lib/fal-seedance.mjs — shared Seedance 2.0 motion-shot client
// (fal.ai queue API). Used by pop/bin/gen-motion.mjs (single shot) and
// the per-track batch drivers (gen-motion-<track>.mjs).
//
// The illy panel carries the LOOK; the prompt carries only MOTION.
// Tiers @720p: fast ≈ $0.2419/s · standard ≈ $0.3024/s.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../..");

export const RATE_PER_SEC = { fast: 0.2419, standard: 0.3024 };

export function falKey() {
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

const mime = (p) => p.match(/\.jpe?g$/i) ? "image/jpeg" : p.match(/\.webp$/i) ? "image/webp" : "image/png";
export const dataUri = (p) => `data:${mime(p)};base64,${readFileSync(p).toString("base64")}`;

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Submit one shot and download the finished mp4 to outPath.
// Returns { ok, seed, seconds, error }. Retries transient submit
// failures (fal billing-state lag shows up as a spurious 403).
export async function generateShot({
  image, endImage = null, prompt, duration = "5", ratio = "16:9",
  resolution = "720p", tier = "fast", audio = false, seed = null,
  outPath, label = "shot", log = console.log,
}) {
  const key = falKey();
  const auth = { Authorization: `Key ${key}`, "Content-Type": "application/json" };
  const endpoint = tier === "standard"
    ? "bytedance/seedance-2.0/image-to-video"
    : "bytedance/seedance-2.0/fast/image-to-video";

  const input = {
    prompt,
    image_url: dataUri(image),
    end_image_url: endImage ? dataUri(endImage) : undefined,
    duration: String(duration),
    resolution,
    aspect_ratio: ratio,
    generate_audio: audio,
    seed: seed ?? undefined,
  };

  let queued = null;
  const MAX_SUBMIT = 4;
  for (let attempt = 1; attempt <= MAX_SUBMIT; attempt++) {
    const res = await fetch(`https://queue.fal.run/${endpoint}`, {
      method: "POST", headers: auth, body: JSON.stringify(input),
    });
    if (res.ok) { queued = await res.json(); break; }
    const err = await res.text();
    const transient = res.status === 429 || res.status >= 500 || res.status === 403;
    if (transient && attempt < MAX_SUBMIT) {
      log(`  ⚠ ${label}: submit ${res.status} — retry ${attempt}/${MAX_SUBMIT - 1} in ${5 * attempt}s`);
      await sleep(5000 * attempt);
      continue;
    }
    return { ok: false, error: `submit ${res.status}: ${err.slice(0, 300)}` };
  }

  const t0 = Date.now();
  let status = "";
  while (status !== "COMPLETED") {
    await sleep(4000);
    const res = await fetch(queued.status_url, { headers: auth });
    const body = await res.json();
    if (body.status !== status) {
      status = body.status;
      log(`  ${label}: ${status.toLowerCase()} · ${((Date.now() - t0) / 1000).toFixed(0)}s`);
    }
    if (status === "FAILED" || body.error) {
      return { ok: false, error: `generation failed: ${JSON.stringify(body).slice(0, 300)}` };
    }
  }

  const result = await (await fetch(queued.response_url, { headers: auth })).json();
  const videoUrl = result.video?.url;
  if (!videoUrl) return { ok: false, error: `no video in response: ${JSON.stringify(result).slice(0, 300)}` };
  const mp4 = Buffer.from(await (await fetch(videoUrl)).arrayBuffer());
  writeFileSync(outPath, mp4);
  return { ok: true, seed: result.seed, seconds: (Date.now() - t0) / 1000, bytes: mp4.length };
}
