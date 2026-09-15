// Inference for Easel, on Aesthetic Computer's account.
//
// Easel drives a vendor CLI — `claude` or `codex` — which is elegant inside a
// fleet where those are installed and useless to anyone else: a stranger who
// installs Easel and holds neither subscription gets a correct interface
// attached to nothing. This endpoint is the other option. It buys the inference
// on AC's OpenRouter account and rents it to a handle, so the only thing an
// install needs is an @handle.
//
// It speaks the Anthropic Messages API, because OpenRouter serves a compatible
// endpoint and because that is the protocol Easel's existing bridge already
// understands. The client gets a stream it can parse with code it already has.
//
// What stands between this and a bill nobody meant to run up:
//
//   1. A token. No handle, no inference — there is no anonymous tier here,
//      unlike /api/ask, because this is not a demo surface. It is the engine of
//      a tool someone deliberately installed.
//   2. A model allowlist. The caller names a model and the server decides
//      whether that is one we are willing to pay for. A caller-chosen model with
//      no ceiling is the exact defect /api/ask shipped with for months.
//   3. The same daily budget /api/ask meters against, in the same collection,
//      so a handle has one allowance across everything AC buys for them rather
//      than one per endpoint.
//
// Provider usage feeds the shared token allowance, with discounts for cached
// input. This is not a dollar limit or an atomic reservation; premium models
// cost more per allowance, and concurrent requests can overshoot the balance.

import { stream } from "@netlify/functions";
import { relayInference } from "../../backend/easel-stream.mjs";
import { EASEL_MODELS as MODELS, inferenceRequest, inferenceBudgetFailure } from "../../backend/easel-policy.mjs";

const OPENROUTER = "https://openrouter.ai/api/v1/messages";

const AUTH_TIMEOUT_MS = 3000;

function fail(statusCode, message) {
  return {
    statusCode,
    headers: { "Content-Type": "application/json; charset=utf-8" },
    body: JSON.stringify({ error: { message } }),
  };
}

export const handler = stream(async (event) => {
  if (event.httpMethod === "OPTIONS") {
    return {
      statusCode: 200,
      headers: {
        "Access-Control-Allow-Origin": "*",
        "Access-Control-Allow-Methods": "POST, OPTIONS",
        "Access-Control-Allow-Headers": "Content-Type, Authorization",
      },
      body: "",
    };
  }
  if (event.httpMethod !== "POST") return fail(405, "POST only.");

  const key = process.env.OPENROUTER_API_KEY;
  if (!key) return fail(503, "Hosted inference is not configured.");

  if (!event.headers?.authorization) {
    return fail(401, "Easel's hosted inference needs an Aesthetic Computer handle. Run /login.");
  }

  // Who is asking, and may they?
  let handle = "";
  try {
    const { authorize, getHandleOrEmail } = await import("../../backend/authorization.mjs");
    const user = await Promise.race([
      authorize(event.headers),
      new Promise((_, reject) => setTimeout(() => reject(new Error("auth timeout")), AUTH_TIMEOUT_MS)),
    ]);
    if (!user?.sub) return fail(401, "That token is not valid.");
    const handleOrEmail = await getHandleOrEmail(user.sub);
    if (typeof handleOrEmail === "string" && handleOrEmail.startsWith("@")) {
      handle = handleOrEmail.slice(1);
    }
  } catch (error) {
    // Unlike /api/ask, a failed check here refuses rather than falling back:
    // there is no cheap tier to fall back to, and serving inference to a caller
    // we could not identify is the thing this endpoint exists to prevent.
    return fail(503, `Could not verify that token: ${error.message}`);
  }
  if (!handle) {
    return fail(403, "This account has no @handle yet. Claim one at aesthetic.computer.");
  }

  let body;
  try {
    body = JSON.parse(event.body);
  } catch (error) {
    return fail(400, `Malformed request: ${error.message}`);
  }

  let model, maxTokens;
  try { ({ model, maxTokens } = inferenceRequest(body)); }
  catch (error) { return fail(400, error.message); }

  // Has this handle spent its day? Over budget is a refusal here rather than a
  // downgrade, because there is nothing cheaper to downgrade to — and a clear
  // "come back tomorrow" beats a session that mysteriously gets worse.
  let budget = null;
  try {
    const { checkBudget } = await import("../../backend/ai-budget.mjs");
    budget = await checkBudget(handle);
  } catch (error) {
    console.log("🪙 easel: budget unavailable —", error.message);
  }
  const budgetFailure = inferenceBudgetFailure(budget, handle);
  if (budgetFailure) return fail(budgetFailure.statusCode, budgetFailure.message);

  console.log(`🎨 easel @${handle} — ${MODELS[model].label}${budget ? ` · ${budget.remaining} left` : ""}`);

  const controller = new AbortController();
  const upstream = await fetch(OPENROUTER, {
    signal: controller.signal,
    method: "POST",
    headers: {
      Authorization: `Bearer ${key}`,
      "Content-Type": "application/json",
      "anthropic-version": "2023-06-01",
      // OpenRouter attributes traffic by these, so ours is legible in their
      // dashboard as Easel rather than as anonymous API calls.
      "HTTP-Referer": "https://aesthetic.computer",
      "X-Title": "Easel",
    },
    body: JSON.stringify({
      model,
      max_tokens: maxTokens,
      system: body.system,
      messages: body.messages,
      tools: body.tools,
      stream: true,
    }),
  });

  if (!upstream.ok) {
    const detail = await upstream.text();
    console.log(`🎨 easel upstream ${upstream.status}: ${detail.slice(0, 200)}`);
    return fail(upstream.status, `Inference provider returned ${upstream.status}.`);
  }

  // Pass the SSE through untouched, watching for the usage block on the way so
  // the meter records real spend. Tapping the stream rather than buffering it
  // keeps the first token as fast as the provider makes it.
  const passthrough = relayInference(upstream.body, {
    abort: () => controller.abort(),
    onUsage: async (spent) => {
      const { recordUsage } = await import("../../backend/ai-budget.mjs");
      await recordUsage(handle, spent, { model });
    },
  });

  return {
    statusCode: 200,
    headers: {
      "Content-Type": "text/event-stream; charset=utf-8",
      "Cache-Control": "no-cache, no-transform",
      "X-Accel-Buffering": "no",
      "Access-Control-Allow-Origin": "*",
    },
    body: passthrough,
  };
});
