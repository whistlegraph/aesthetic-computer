// Aesel's provider transport. Callers own authorization, budgets and settlement;
// this module only builds and sends a server-selected Messages request.
import { ANTHROPIC_DIRECT, EASEL_MODELS, anthropicThinking } from "./easel-policy.mjs";

export function inferenceProviderRequest({ model, maxTokens, body, reasoning, thinking,
  credits, key, signal, serverFallbacks = false }) {
  if (!Object.hasOwn(EASEL_MODELS, model)) throw new Error("Unsupported Aesel model");
  const direct = credits && ANTHROPIC_DIRECT[model];
  if (!direct && !key) throw new Error("Inference provider is not configured for that model");
  return {
    url: direct ? "https://api.anthropic.com/v1/messages" : "https://openrouter.ai/api/v1/messages",
    init: {
      signal, method: "POST",
      headers: direct ? {
        "x-api-key": credits,
        "Content-Type": "application/json",
        "anthropic-version": "2023-06-01",
        ...(serverFallbacks ? { "anthropic-beta": "server-side-fallback-2026-07-01" } : {}),
      } : {
        Authorization: `Bearer ${key}`,
        "Content-Type": "application/json",
        "anthropic-version": "2023-06-01",
        "HTTP-Referer": "https://aesthetic.computer",
        "X-Title": "Easel",
      },
      body: JSON.stringify({
        model: direct || model, max_tokens: maxTokens,
        system: body.system, messages: body.messages, tools: body.tools,
        ...(direct ? anthropicThinking(model, { thinking, reasoning }) : {
          ...(reasoning ? { reasoning } : {}), ...(thinking ? { thinking } : {}),
        }),
        ...(direct && serverFallbacks ? { fallbacks: "default" } : {}),
        stream: true,
      }),
    },
  };
}

export function requestInference(options, fetchImpl = fetch) {
  const { url, init } = inferenceProviderRequest(options);
  return fetchImpl(url, init);
}
