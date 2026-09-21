// flux, 26.09.21
// Budgeted Cloudflare FLUX.1 schnell image generation. Provider keys stay server-side.
// POST { prompt, preset?: "kidlisp" | "warm" | "raw" }.
// The hosted model uses four steps and returns a fixed 1024×1024 image.

import { respond } from "../../backend/http.mjs";
import { reserveImageBudget } from "../../backend/image-generation-budget.mjs";

const MODEL = "@cf/black-forest-labs/flux-1-schnell";
const TIMEOUT_MS = 30000;
const COOLDOWN_MS = 60000;
const MAX_COOLDOWN_MS = 15 * 60000;
const MAX_IN_FLIGHT = 2;
const HEADERS = { "Cache-Control": "no-store" };

function reply(status, body, headers = {}) {
  return respond(status, body, { ...HEADERS, ...headers });
}
function retryResponse(status, seconds, reason = "temporarily_unavailable") {
  const retryAfter = Math.max(1, Math.ceil(Number(seconds) || 60));
  return reply(
    status,
    { ok: false, reason, retry_after: retryAfter },
    {
      "Retry-After": String(retryAfter),
    },
  );
}
function retryAfterMs(value, now) {
  if (!value) return 0;
  const seconds = Number(value);
  const ms = Number.isFinite(seconds)
    ? seconds * 1000
    : Date.parse(value) - now;
  return Number.isFinite(ms) && ms > 0 ? ms : 0;
}

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

// A separate factory makes the budget and provider boundary testable without
// a real account, database connection, or image-generation charge.
export function createHandler({
  fetch: providerFetch = (...args) => globalThis.fetch(...args),
  reserveBudget = reserveImageBudget,
  env = process.env,
  now = Date.now,
} = {}) {
  let outageUntil = 0;
  let quotaUntil = 0;
  let failures = 0;
  let probeInFlight = false;
  let inFlight = 0;

  function openCircuit(providerRetry = 0) {
    failures += 1;
    outageUntil =
      now() +
      Math.max(
        providerRetry,
        Math.min(COOLDOWN_MS * 2 ** Math.min(failures - 1, 4), MAX_COOLDOWN_MS),
      );
  }
  function unavailable() {
    return retryResponse(
      503,
      Math.max(probeInFlight ? TIMEOUT_MS : 1000, outageUntil - now()) / 1000,
    );
  }

  return async function handler(event) {
    if (event.httpMethod === "OPTIONS") return reply(200, "");
    if (event.httpMethod !== "POST")
      return reply(405, { ok: false, reason: "method" });
    let body;
    try {
      body = JSON.parse(event.body || "{}");
    } catch {
      return reply(400, { ok: false, reason: "bad_json" });
    }
    if (!body || typeof body !== "object" || Array.isArray(body)) {
      return reply(400, { ok: false, reason: "bad_json" });
    }
    if (typeof body.prompt !== "string" || !body.prompt.trim()) {
      return reply(400, { ok: false, reason: "no_prompt" });
    }
    const prompt = body.prompt.trim();
    if (prompt.length > 1000)
      return reply(400, { ok: false, reason: "prompt_too_long" });
    const presetName =
      typeof body.preset === "string" && Object.hasOwn(PRESETS, body.preset)
        ? body.preset
        : "kidlisp";
    const fullPrompt = PRESETS[presetName]
      ? `${prompt} — ${PRESETS[presetName]}`
      : prompt;
    if (!env.CLOUDFLARE_AI_TOKEN || !env.CLOUDFLARE_ACCOUNT_ID) {
      return retryResponse(503, 60, "provider_unavailable");
    }
    if (quotaUntil > now()) {
      return retryResponse(
        429,
        (quotaUntil - now()) / 1000,
        "image_budget_exhausted",
      );
    }
    if (outageUntil > now() || probeInFlight) return unavailable();
    if (inFlight >= MAX_IN_FLIGHT) return retryResponse(429, 5, "busy");

    const isProbe = failures > 0;
    if (isProbe) probeInFlight = true;
    inFlight += 1;
    let timeout;
    const startedAt = now();
    try {
      let budget;
      try {
        budget = await reserveBudget();
      } catch {
        console.warn("flux: image budget unavailable");
        return retryResponse(503, 60, "budget_unavailable");
      }
      if (!budget?.allowed) {
        return retryResponse(
          429,
          budget?.retryAfterSeconds,
          "image_budget_exhausted",
        );
      }

      // One reserved attempt, no retry after an ambiguous network failure.
      // Keep the deadline active until the entire image response is read.
      const controller = new AbortController();
      timeout = setTimeout(() => controller.abort(), TIMEOUT_MS);
      const endpoint = `https://api.cloudflare.com/client/v4/accounts/${encodeURIComponent(env.CLOUDFLARE_ACCOUNT_ID)}/ai/run/${MODEL}`;
      let upstream, data;
      try {
        upstream = await providerFetch(endpoint, {
          method: "POST",
          headers: {
            Authorization: `Bearer ${env.CLOUDFLARE_AI_TOKEN}`,
            "Content-Type": "application/json",
          },
          body: JSON.stringify({ prompt: fullPrompt, steps: 4 }),
          signal: controller.signal,
        });
        data = await upstream.json();
      } catch (error) {
        console.warn(
          "flux: Cloudflare request failed",
          error?.name || "unknown",
        );
        openCircuit(retryAfterMs(upstream?.headers?.get("Retry-After"), now()));
        return unavailable();
      }

      // Cloudflare's free allocation is a daily limit, not transient capacity.
      // Code 3036 resets at midnight UTC; retrying sooner would waste local
      // budget reservations while the provider cannot generate an image.
      if (
        upstream.status === 429 &&
        Array.isArray(data?.errors) &&
        data.errors.some((error) => Number(error?.code) === 3036)
      ) {
        quotaUntil = (Math.floor(now() / 86400000) + 1) * 86400000;
        failures = 0;
        outageUntil = 0;
        return retryResponse(
          429,
          (quotaUntil - now()) / 1000,
          "image_budget_exhausted",
        );
      }
      if (!upstream.ok) {
        console.warn("flux: Cloudflare HTTP", upstream.status);
        if (
          upstream.status === 408 ||
          upstream.status === 429 ||
          upstream.status === 401 ||
          upstream.status === 403 ||
          upstream.status >= 500
        ) {
          openCircuit(retryAfterMs(upstream.headers.get("Retry-After"), now()));
          return unavailable();
        }
        return reply(502, {
          ok: false,
          reason: "upstream",
          status: upstream.status,
        });
      }
      const image = data?.result?.image;
      if (
        data?.success !== true ||
        typeof image !== "string" ||
        !image.length
      ) {
        console.warn("flux: Cloudflare response missing image");
        openCircuit();
        return unavailable();
      }
      failures = 0;
      outageUntil = 0;
      return reply(200, {
        ok: true,
        png: `data:image/jpeg;base64,${image}`,
        width: 1024,
        height: 1024,
        seed: null,
        provider: "cloudflare",
        preset: presetName,
        elapsed_ms: now() - startedAt,
      });
    } finally {
      clearTimeout(timeout);
      inFlight -= 1;
      if (isProbe) probeInFlight = false;
    }
  };
}

export const handler = createHandler();
