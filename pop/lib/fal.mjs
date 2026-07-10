// pop/lib/fal.mjs — shared fal.ai client for AC's motion pipelines.
// Covers Seedance 2.0 (image/text/reference/blend-to-video), sync-lipsync,
// Kling AI Avatar (talking-head from a still), and fal storage upload.
// Used by pop/bin/gen-motion.mjs, the per-track batch drivers, and the
// wizards. All video jobs share one queue runner (runQueueJob).
//
// The illy panel carries the LOOK; the prompt carries only MOTION.
// Seedance @720p: fast ≈ $0.2419/s · standard ≈ $0.3024/s.
// sync-lipsync patches a mouth (weak on flat cels); Kling AI Avatar
// synthesizes a whole head performance and handles stylized characters.

import { readFileSync, writeFileSync, existsSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

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

const mime = (p) =>
  p.match(/\.jpe?g$/i) ? "image/jpeg" :
  p.match(/\.webp$/i) ? "image/webp" :
  p.match(/\.mp4$/i) ? "video/mp4" :
  p.match(/\.mov$/i) ? "video/quicktime" :
  p.match(/\.mp3$/i) ? "audio/mpeg" :
  p.match(/\.wav$/i) ? "audio/wav" :
  "image/png";
export const dataUri = (p) => `data:${mime(p)};base64,${readFileSync(p).toString("base64")}`;

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// Upload a local file to fal storage (CDN v3) and return its public URL.
// Some endpoints (sync-lipsync et al.) reject inline data: URIs above a small
// cap — upload first and pass the URL. The auth token is valid ~30 days, so a
// per-call fetch is cheap enough.
export async function uploadToFalStorage(path, log = console.log) {
  if (!existsSync(path)) throw new Error(`upload: file not found: ${path}`);
  const key = falKey();
  const tRes = await fetch(
    "https://rest.alpha.fal.ai/storage/auth/token?storage_type=fal-cdn-v3",
    { method: "POST", headers: { Authorization: `Key ${key}`, "Content-Type": "application/json" }, body: "{}" },
  );
  if (!tRes.ok) throw new Error(`upload token ${tRes.status}: ${(await tRes.text()).slice(0, 200)}`);
  const { token, base_url } = await tRes.json();
  const uRes = await fetch(`${base_url}/files/upload`, {
    method: "POST",
    headers: { Authorization: `Bearer ${token}`, "Content-Type": mime(path) },
    body: readFileSync(path),
  });
  if (!uRes.ok) throw new Error(`upload ${uRes.status}: ${(await uRes.text()).slice(0, 200)}`);
  const { access_url } = await uRes.json();
  if (!access_url) throw new Error("upload: no access_url in response");
  log(`  ↑ ${path.split("/").pop()} → ${access_url.split("/").pop()}`);
  return access_url;
}

// Submit one shot and download the finished mp4 to outPath.
// Returns { ok, seed, seconds, error }. Retries transient submit
// failures (fal billing-state lag shows up as a spurious 403).
export async function generateShot({
  image, endImage = null, prompt, duration = "5", ratio = "16:9",
  resolution = "720p", tier = "fast", audio = false, seed = null,
  outPath, label = "shot", log = console.log,
}) {
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
  return runQueueJob({ endpoint, input, outPath, label, log });
}

// Reference-to-video: generate a fresh clip steered by `prompt`, pulling
// style/motion/sound from up to 9 images, 3 videos (combined 2–15s, <50MB),
// and 3 audio clips (combined ≤15s). Refer to them in the prompt as
// @Image1 / @Video1 / @Audio1. Each ref is a local file path → data URI.
export async function generateReferenceShot({
  images = [], videos = [], audios = [], prompt,
  duration = "auto", ratio = "auto", resolution = "720p",
  tier = "fast", audio = true, seed = null,
  outPath, label = "ref-shot", log = console.log,
}) {
  if (!videos.length && !images.length)
    throw new Error("reference-to-video needs at least one image or video ref");
  const endpoint = tier === "standard"
    ? "bytedance/seedance-2.0/reference-to-video"
    : "bytedance/seedance-2.0/fast/reference-to-video";
  const input = {
    prompt,
    image_urls: images.length ? images.map(dataUri) : undefined,
    video_urls: videos.length ? videos.map(dataUri) : undefined,
    audio_urls: audios.length ? audios.map(dataUri) : undefined,
    duration: String(duration),
    resolution,
    aspect_ratio: ratio,
    generate_audio: audio,
    seed: seed ?? undefined,
  };
  return runQueueJob({ endpoint, input, outPath, label, log });
}

// Submit one job to a fal queue endpoint, poll to completion, download the
// mp4 to outPath. Shared by every Seedance entry point above.
async function runQueueJob({ endpoint, input, outPath, label, log }) {
  const key = falKey();
  const auth = { Authorization: `Key ${key}`, "Content-Type": "application/json" };

  // A submitted job keeps running (and billing) on fal even if this
  // process dies, so the queue handles are persisted beside the output
  // the moment they exist — a rerun RESUMES the in-flight job instead
  // of resubmitting and paying twice.
  const queuePath = `${outPath}.queue.json`;
  let queued = null;
  if (existsSync(queuePath)) {
    queued = JSON.parse(readFileSync(queuePath, "utf8"));
    log(`  ↻ ${label}: resuming in-flight job from ${queuePath.split("/").pop()}`);
  }

  const MAX_SUBMIT = 4;
  for (let attempt = 1; !queued && attempt <= MAX_SUBMIT; attempt++) {
    let res;
    try {
      res = await fetch(`https://queue.fal.run/${endpoint}`, {
        method: "POST", headers: auth, body: JSON.stringify(input),
      });
    } catch (err) {
      // A thrown fetch (e.g. UND_ERR_HEADERS_TIMEOUT uploading a big data
      // URI body) means the job never reached fal — safe to retry, nothing
      // billed and no queue handle to resume.
      if (attempt < MAX_SUBMIT) {
        log(`  ⚠ ${label}: submit ${err.cause?.code ?? err.message} — retry ${attempt}/${MAX_SUBMIT - 1} in ${5 * attempt}s`);
        await sleep(5000 * attempt);
        continue;
      }
      return { ok: false, error: `submit threw: ${err.cause?.code ?? err.message}` };
    }
    if (res.ok) {
      queued = await res.json();
      writeFileSync(queuePath, JSON.stringify(queued, null, 2));
      break;
    }
    const err = await res.text();
    const transient = res.status === 429 || res.status >= 500 || res.status === 403;
    if (transient && attempt < MAX_SUBMIT) {
      log(`  ⚠ ${label}: submit ${res.status} — retry ${attempt}/${MAX_SUBMIT - 1} in ${5 * attempt}s`);
      await sleep(5000 * attempt);
      continue;
    }
    return { ok: false, error: `submit ${res.status}: ${err.slice(0, 300)}` };
  }

  // Polls and downloads ride out transient network failures (ENETUNREACH
  // et al) — only a long unbroken streak gives up, and the queue sidecar
  // survives for the next rerun to resume.
  const MAX_NET_FAILS = 8;
  let netFails = 0;
  const tryFetch = async (...args) => {
    try {
      const res = await fetch(...args);
      netFails = 0;
      return res;
    } catch (err) {
      netFails++;
      log(`  ⚠ ${label}: ${err.cause?.code ?? err.message} — retry ${netFails}/${MAX_NET_FAILS}`);
      if (netFails >= MAX_NET_FAILS) throw err;
      await sleep(5000 * netFails);
      return null;
    }
  };

  const t0 = Date.now();
  let status = "";
  let lastTick = 0;
  while (status !== "COMPLETED") {
    await sleep(4000);
    const res = await tryFetch(queued.status_url, { headers: auth });
    if (!res) continue;
    const body = await res.json();
    if (body.status !== status) {
      status = body.status;
      lastTick = Date.now();
      log(`  ${label}: ${status.toLowerCase()} · ${((Date.now() - t0) / 1000).toFixed(0)}s`);
    } else if (Date.now() - lastTick > 12000) {
      // heartbeat between state changes — ClipWizard's status line and
      // anyone tailing the CLI see elapsed time, not minutes of silence
      lastTick = Date.now();
      log(`  ${label}: ${status.toLowerCase()} · ${((Date.now() - t0) / 1000).toFixed(0)}s`);
    }
    if (status === "FAILED" || body.error) {
      unlinkSync(queuePath);
      return { ok: false, error: `generation failed: ${JSON.stringify(body).slice(0, 300)}` };
    }
  }

  let result = null;
  while (!result) {
    const res = await tryFetch(queued.response_url, { headers: auth });
    if (res) result = await res.json();
  }
  const videoUrl = result.video?.url;
  if (!videoUrl) return { ok: false, error: `no video in response: ${JSON.stringify(result).slice(0, 300)}` };
  let mp4 = null;
  while (!mp4) {
    const res = await tryFetch(videoUrl);
    if (res) mp4 = Buffer.from(await res.arrayBuffer());
  }
  writeFileSync(outPath, mp4);
  unlinkSync(queuePath);
  return { ok: true, seed: result.seed, seconds: (Date.now() - t0) / 1000, bytes: mp4.length };
}

// Audio-driven lipsync (sync.so lipsync-2 via fal). Takes a source VIDEO and
// an AUDIO clip and rewrites the speaker's mouth to match the speech, leaving
// the rest of the frame untouched — so it layers cleanly on top of a Seedance
// motion shot or even a static hold. model: "lipsync-2" | "lipsync-2-pro"
// (pro ≈ 1.67× the cost). sync_mode governs length mismatch: "cut_off" trims
// to the shorter of the two, "loop"/"bounce" repeat the video to cover audio.
// Reuses runQueueJob — the endpoint also returns { video: { url } }.
export async function generateLipsync({
  video, audio, model = "lipsync-2", syncMode = "cut_off",
  outPath, label = "lipsync", log = console.log,
}) {
  if (!existsSync(video)) throw new Error(`lipsync source video not found: ${video}`);
  if (!existsSync(audio)) throw new Error(`lipsync audio not found: ${audio}`);
  // sync-lipsync rejects inline data: URIs — upload both media first.
  const input = {
    model,
    video_url: await uploadToFalStorage(video, log),
    audio_url: await uploadToFalStorage(audio, log),
    sync_mode: syncMode,
  };
  return runQueueJob({ endpoint: "fal-ai/sync-lipsync/v2", input, outPath, label, log });
}

// Audio-driven talking AVATAR (Kling AI Avatar v2 via fal). Unlike lipsync —
// which only patches the mouth of an existing video and can't grip a flat
// drawn lip-line — this synthesizes the whole head performance (mouth, blinks,
// small head motion) from a single still portrait + audio, and Kling handles
// "cartoons or stylized characters" without rigging. Output length follows the
// audio (2–60s, ≤5MB). tier: "standard" ($0.056/s) | "pro" ($0.115/s).
export async function generateAvatar({
  image, audio, prompt = ".", tier = "standard",
  outPath, label = "avatar", log = console.log,
}) {
  if (!existsSync(image)) throw new Error(`avatar image not found: ${image}`);
  if (!existsSync(audio)) throw new Error(`avatar audio not found: ${audio}`);
  const endpoint = tier === "pro"
    ? "fal-ai/kling-video/ai-avatar/v2/pro"
    : "fal-ai/kling-video/ai-avatar/v2/standard";
  const input = {
    prompt,
    image_url: await uploadToFalStorage(image, log),
    audio_url: await uploadToFalStorage(audio, log),
  };
  return runQueueJob({ endpoint, input, outPath, label, log });
}

// Text-to-video: generate a clip from a prompt alone (no source image). The
// counterpart to generateShot (image+text) — together the helper covers both
// the image-led and text-led paths.
export async function generateTextShot({
  prompt, duration = "5", ratio = "16:9", resolution = "720p",
  tier = "fast", audio = false, seed = null, outPath, label = "text-shot", log = console.log,
}) {
  const endpoint = tier === "standard"
    ? "bytedance/seedance-2.0/text-to-video"
    : "bytedance/seedance-2.0/fast/text-to-video";
  const input = {
    prompt,
    duration: String(duration),
    resolution,
    aspect_ratio: ratio,
    generate_audio: audio,
    seed: seed ?? undefined,
  };
  return runQueueJob({ endpoint, input, outPath, label, log });
}

// Blend across 2–3+ keyframe stills (e.g. gpt-image gens): render a Seedance
// morph for each adjacent pair (images[i] → images[i+1], using start+end frames)
// then concat the segments into one mp4. `prompts` is either one MOTION string
// reused for every segment, or one per segment (length images.length - 1).
// Each segment caches/resumes independently via generateShot. Returns
// { ok, outPath, segments, seconds, error }.
export async function generateBlendSequence({
  images, prompts = "a smooth, gentle blend between the two scenes; subtle continuous motion; locked camera",
  duration = "5", ratio = "9:16", resolution = "720p", tier = "fast", audio = false,
  seed = null, outPath, label = "blend", log = console.log,
}) {
  if (!Array.isArray(images) || images.length < 2)
    throw new Error("blend sequence needs ≥2 keyframe images");
  for (const img of images) if (!existsSync(img)) throw new Error(`keyframe not found: ${img}`);
  const segPrompts = Array.isArray(prompts) ? prompts : images.slice(1).map(() => prompts);

  const t0 = Date.now();
  const segPaths = [];
  for (let i = 0; i < images.length - 1; i++) {
    const segOut = outPath.replace(/\.mp4$/i, `.seg${i + 1}.mp4`);
    log(`  ${label}: segment ${i + 1}/${images.length - 1} · ${images[i].split("/").pop()} → ${images[i + 1].split("/").pop()}`);
    const r = await generateShot({
      image: images[i], endImage: images[i + 1], prompt: segPrompts[i] ?? segPrompts[0],
      duration, ratio, resolution, tier, audio, seed: seed ?? undefined,
      outPath: segOut, label: `${label}-seg${i + 1}`, log,
    });
    if (!r.ok) return { ok: false, error: `segment ${i + 1}: ${r.error}`, segments: segPaths };
    segPaths.push(segOut);
  }

  if (segPaths.length === 1) {
    writeFileSync(outPath, readFileSync(segPaths[0]));
    return { ok: true, outPath, segments: segPaths, seconds: (Date.now() - t0) / 1000 };
  }

  // concat the segments (re-encode — Seedance segments share params but a clean
  // re-encode avoids concat-demuxer timestamp seams between morph clips)
  const listPath = `${outPath}.concat.txt`;
  writeFileSync(listPath, segPaths.map((p) => `file '${p.replace(/'/g, "'\\''")}'`).join("\n") + "\n");
  const r = spawnSync("ffmpeg", [
    "-hide_banner", "-loglevel", "error", "-y", "-f", "concat", "-safe", "0", "-i", listPath,
    "-c:v", "libx264", "-preset", "faster", "-crf", "18", "-pix_fmt", "yuv420p",
    ...(audio ? ["-c:a", "aac", "-b:a", "192k"] : ["-an"]),
    "-movflags", "+faststart", outPath,
  ], { stdio: "inherit" });
  unlinkSync(listPath);
  if (r.status !== 0) return { ok: false, error: `concat ffmpeg exit ${r.status}`, segments: segPaths };
  return { ok: true, outPath, segments: segPaths, seconds: (Date.now() - t0) / 1000 };
}
