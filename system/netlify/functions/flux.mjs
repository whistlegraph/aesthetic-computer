// flux, 26.04.23
// Proxy to NVIDIA NIM FLUX.1 schnell image generation.
// Hides the NVIDIA_API_KEY, applies one of two AC style presets,
// returns the raw JPEG as a data URL the piece can decode directly.
//
// Usage from a piece:
//   const res = await fetch("/api/flux", {
//     method: "POST",
//     headers: { "Content-Type": "application/json" },
//     body: JSON.stringify({ prompt: "a happy frog", preset: "kidlisp", seed: 7 }),
//   });
//   const { ok, png, reason, elapsed_ms, seed } = await res.json();
//
// On safety-filter rejection: { ok: false, reason: "filtered" } (200, so the
// piece can react gracefully). On NVIDIA upstream error: 502.
//
// Env: NVIDIA_API_KEY (required). Lives in lith/.env in production.

import { respond } from "../../backend/http.mjs";

const FLUX_URL =
  "https://ai.api.nvidia.com/v1/genai/black-forest-labs/flux.1-schnell";

// Two filter-safe AC style suffixes. The bisect that pinned these down lives
// in ~/Desktop/nvidia-flux-log/README.md — short version: NVIDIA's safety
// classifier filters on clusters of proper nouns + dense modifiers, so the
// suffixes deliberately avoid naming the platform / maker / language.
const PRESETS = {
  // Soft pastel mascot energy — animals, food, friendly subjects
  warm:
    "chunky pixel-art bitmap, crisp 1-pixel edges, no anti-aliasing, " +
    "saturated palette of black, navy, hot pink, lime, cyan, yellow, magenta, white, " +
    "centered subject on flat solid color background, " +
    "soft 1-pixel offset pastel shadow beneath subject, " +
    "square mobile composition, 90s indie computing aesthetic, " +
    "handmade lo-fi warmth, no text, no UI, no watermarks",

  // High-contrast CRT energy — devices, abstract objects, default
  kidlisp:
    "high-contrast pixel-art bitmap, crisp 1-pixel edges, no anti-aliasing, " +
    "strict palette of black, hot pink, lime, cyan, yellow, white, " +
    "solid black background, " +
    "hard cyan 1-pixel shadow beneath subject, " +
    "square composition, no text",

  // No styling — pass the user's prompt through verbatim
  raw: "",
};

const ALLOWED_WIDTHS = [768, 832, 896, 960, 1024, 1088, 1152, 1216, 1280, 1344];

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return respond(200, "");
  }
  if (event.httpMethod !== "POST") {
    return respond(405, { ok: false, reason: "method" });
  }

  if (!process.env.NVIDIA_API_KEY) {
    console.error("flux: NVIDIA_API_KEY not configured");
    return respond(500, { ok: false, reason: "no_key" });
  }

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return respond(400, { ok: false, reason: "bad_json" });
  }

  const prompt = (body.prompt || "").toString().trim();
  if (!prompt) return respond(400, { ok: false, reason: "no_prompt" });
  if (prompt.length > 1000)
    return respond(400, { ok: false, reason: "prompt_too_long" });

  const presetName = body.preset || "kidlisp";
  const styleSuffix = PRESETS[presetName] ?? PRESETS.kidlisp;
  const fullPrompt = styleSuffix ? `${prompt} — ${styleSuffix}` : prompt;

  // Width/height clamp to FLUX's literal allowed set. Default 768 (smallest
  // → fastest, most reliable). Pieces that want bigger pay the latency tail.
  const width = ALLOWED_WIDTHS.includes(+body.width) ? +body.width : 768;
  const height = ALLOWED_WIDTHS.includes(+body.height) ? +body.height : width;

  const seed = Number.isInteger(body.seed)
    ? body.seed
    : Math.floor(Math.random() * 1e9);

  // 30s timeout — FLUX schnell normally returns in 1-4s. NVIDIA has been
  // observed hanging for minutes before 504'ing during outages; fail fast
  // so the piece can show an error and let the user retry.
  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), 30000);

  const t0 = Date.now();
  let upstream;
  try {
    upstream = await fetch(FLUX_URL, {
      method: "POST",
      headers: {
        Authorization: `Bearer ${process.env.NVIDIA_API_KEY}`,
        "Content-Type": "application/json",
        Accept: "application/json",
      },
      body: JSON.stringify({
        prompt: fullPrompt,
        cfg_scale: 0,
        steps: 4,
        seed,
        width,
        height,
        mode: "base",
      }),
      signal: controller.signal,
    });
  } catch (err) {
    if (err.name === "AbortError") {
      return respond(504, { ok: false, reason: "timeout" });
    }
    console.error("flux: upstream fetch failed", err);
    return respond(502, { ok: false, reason: "network", detail: err.message });
  } finally {
    clearTimeout(timeoutId);
  }

  if (!upstream.ok) {
    const detail = await upstream.text().catch(() => "");
    console.error("flux: upstream", upstream.status, detail.slice(0, 300));
    return respond(502, {
      ok: false,
      reason: "upstream",
      status: upstream.status,
      detail: detail.slice(0, 300),
    });
  }

  let data;
  try {
    data = await upstream.json();
  } catch {
    return respond(502, { ok: false, reason: "bad_upstream_json" });
  }

  const art = data?.artifacts?.[0];
  if (!art) return respond(502, { ok: false, reason: "no_artifact" });

  if (art.finishReason !== "SUCCESS") {
    // Safety filter — return 200 so the piece can react.
    return respond(200, {
      ok: false,
      reason: "filtered",
      finish: art.finishReason,
    });
  }

  const elapsed_ms = Date.now() - t0;
  return respond(200, {
    ok: true,
    png: `data:image/jpeg;base64,${art.base64}`,
    width,
    height,
    seed: art.seed,
    preset: presetName,
    elapsed_ms,
  });
}
