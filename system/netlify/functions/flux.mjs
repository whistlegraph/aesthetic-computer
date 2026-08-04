// flux, 26.04.23
// Proxy to NVIDIA NIM FLUX.1 schnell image generation, with a bounded
// GPT Image fallback when NVIDIA is unavailable. Provider keys stay server-side.
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
// piece can react gracefully). Transient provider failures return 503.
//
// Env: NVIDIA_API_KEY or OPENAI_API_KEY. Lives in lith/.env in production.

import { respond } from "../../backend/http.mjs";

const FLUX_URL =
  "https://ai.api.nvidia.com/v1/genai/black-forest-labs/flux.1-schnell";
const OPENAI_IMAGE_URL = "https://api.openai.com/v1/images/generations";
const FLUX_TIMEOUT_MS = 30000;
const FLUX_OUTAGE_COOLDOWN_MS = 60000;
const OPENAI_FALLBACK_TIMEOUT_MS = 90000;
const OPENAI_FALLBACK_WINDOW_MS = 60 * 60 * 1000;
const OPENAI_FALLBACK_LIMIT = 10;

let outageUntil = 0;
let fallbackWindowStartedAt = 0;
let fallbackCount = 0;

function temporarilyUnavailable(
  retryAfterMs = FLUX_OUTAGE_COOLDOWN_MS,
  reason = "temporarily_unavailable",
) {
  const retryAfter = Math.max(1, Math.ceil(retryAfterMs / 1000));
  return respond(
    503,
    { ok: false, reason, retry_after: retryAfter },
    { "Retry-After": String(retryAfter) },
  );
}

function openOutageCircuit(now = Date.now()) {
  outageUntil = now + FLUX_OUTAGE_COOLDOWN_MS;
}

export function resetFluxOutageCircuit() {
  outageUntil = 0;
}

export function resetFluxFallbackBudget() {
  fallbackWindowStartedAt = 0;
  fallbackCount = 0;
}

function reserveOpenAIFallback(now = Date.now()) {
  if (
    !fallbackWindowStartedAt ||
    now - fallbackWindowStartedAt >= OPENAI_FALLBACK_WINDOW_MS
  ) {
    fallbackWindowStartedAt = now;
    fallbackCount = 0;
  }

  if (fallbackCount >= OPENAI_FALLBACK_LIMIT) {
    return {
      allowed: false,
      retryAfterMs: OPENAI_FALLBACK_WINDOW_MS - (now - fallbackWindowStartedAt),
    };
  }

  fallbackCount += 1;
  return { allowed: true, retryAfterMs: 0 };
}

function openAIImageSize(width, height) {
  if (width > height) return { size: "1536x1024", width: 1536, height: 1024 };
  if (height > width) return { size: "1024x1536", width: 1024, height: 1536 };
  return { size: "1024x1024", width: 1024, height: 1024 };
}

async function generateWithOpenAI({ fullPrompt, width, height, presetName, t0 }) {
  if (!process.env.OPENAI_API_KEY) return temporarilyUnavailable();

  const budget = reserveOpenAIFallback();
  if (!budget.allowed) {
    console.warn("flux: OpenAI fallback hourly budget exhausted");
    return temporarilyUnavailable(
      budget.retryAfterMs,
      "fallback_budget_exhausted",
    );
  }

  const output = openAIImageSize(width, height);
  const controller = new AbortController();
  const timeoutId = setTimeout(
    () => controller.abort(),
    OPENAI_FALLBACK_TIMEOUT_MS,
  );

  let upstream;
  try {
    upstream = await fetch(OPENAI_IMAGE_URL, {
      method: "POST",
      headers: {
        Authorization: `Bearer ${process.env.OPENAI_API_KEY}`,
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        model: "gpt-image-1-mini",
        prompt: fullPrompt,
        n: 1,
        size: output.size,
        quality: "low",
        output_format: "jpeg",
        moderation: "auto",
      }),
      signal: controller.signal,
    });
  } catch (err) {
    console.error("flux: OpenAI fallback failed", err?.name || "unknown");
    return temporarilyUnavailable();
  } finally {
    clearTimeout(timeoutId);
  }

  let data;
  try {
    data = await upstream.json();
  } catch {
    data = null;
  }

  if (!upstream.ok) {
    const code = data?.error?.code || "unknown";
    console.error("flux: OpenAI fallback", upstream.status, code);
    if (code === "moderation_blocked") {
      return respond(200, { ok: false, reason: "filtered" });
    }
    if (upstream.status === 429 || upstream.status >= 500) {
      return temporarilyUnavailable();
    }
    return respond(502, {
      ok: false,
      reason: "fallback_upstream",
      status: upstream.status,
    });
  }

  const image = data?.data?.[0]?.b64_json;
  if (!image) return respond(502, { ok: false, reason: "no_artifact" });

  return respond(200, {
    ok: true,
    png: `data:image/jpeg;base64,${image}`,
    width: output.width,
    height: output.height,
    seed: null,
    provider: "openai",
    preset: presetName,
    elapsed_ms: Date.now() - t0,
  });
}

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

  if (!process.env.NVIDIA_API_KEY && !process.env.OPENAI_API_KEY) {
    console.error("flux: no image provider key configured");
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

  const now = Date.now();
  if (outageUntil > now) {
    return generateWithOpenAI({ fullPrompt, width, height, presetName, t0: now });
  }

  if (!process.env.NVIDIA_API_KEY) {
    return generateWithOpenAI({ fullPrompt, width, height, presetName, t0: now });
  }

  // 30s timeout — FLUX schnell normally returns in 1-4s. NVIDIA has been
  // observed hanging for minutes before 504'ing during outages; fail fast
  // so the piece can show an error and let the user retry.
  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), FLUX_TIMEOUT_MS);

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
      openOutageCircuit();
      console.warn("flux: upstream timed out; outage circuit opened");
      return generateWithOpenAI({ fullPrompt, width, height, presetName, t0 });
    }
    console.error("flux: upstream fetch failed", err);
    openOutageCircuit();
    return generateWithOpenAI({ fullPrompt, width, height, presetName, t0 });
  } finally {
    clearTimeout(timeoutId);
  }

  if (!upstream.ok) {
    const detail = await upstream.text().catch(() => "");
    console.error("flux: upstream", upstream.status, detail.slice(0, 300));
    if (upstream.status === 429 || upstream.status >= 500) {
      openOutageCircuit();
      return generateWithOpenAI({ fullPrompt, width, height, presetName, t0 });
    }
    return respond(502, {
      ok: false,
      reason: "upstream",
      status: upstream.status,
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
    provider: "nvidia",
    preset: presetName,
    elapsed_ms,
  });
}
