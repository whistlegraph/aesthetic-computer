// Aesel's provider transport. Callers own authorization, budgets and settlement;
// this module only builds and sends a server-selected Messages request.
import { ANTHROPIC_DIRECT, EASEL_MODELS, anthropicThinking } from "./easel-policy.mjs";

// Claude caches only what a request marks. Aesel marks its system prompt, so
// without this the whole conversation is billed at full input price on every
// round of a tool loop. Mark the end of the conversation, and the end of the
// user turn before it: the last turn can carry a note the client drops from
// the next request (runtime feedback, a triage cue), and a mark there alone
// would never be read back. Cache reads bill at a tenth of input.
// Other models cache a repeated prefix on their own and are left untouched,
// as is a request whose client placed its own marks in the conversation, and
// a single opening message: a one-shot never comes back to read it, and a
// session writes it one round later.
const CACHE_MARKS = 4; // Anthropic's ceiling per request.
const MARKABLE = new Set(["text", "tool_result", "tool_use", "image", "document"]);
const marked = block => Boolean(block && typeof block === "object" && block.cache_control);
function markEnd(message) {
  const content = typeof message.content === "string" ? [{ type: "text", text: message.content }] : message.content;
  if (!Array.isArray(content) || !content.length) return null;
  const last = content.at(-1);
  if (!last || !MARKABLE.has(last.type) || (last.type === "text" && !String(last.text || "").trim())) return null;
  return { ...message, content: [...content.slice(0, -1), { ...last, cache_control: { type: "ephemeral" } }] };
}
export function cacheConversation(model, body) {
  const messages = body?.messages;
  if (!String(model).startsWith("anthropic/") || !Array.isArray(messages) || messages.length < 2) return messages;
  if (messages.some(m => Array.isArray(m?.content) && m.content.some(marked))) return messages;
  const used = (Array.isArray(body.system) ? body.system.filter(marked).length : 0)
    + (Array.isArray(body.tools) ? body.tools.filter(marked).length : 0);
  let room = CACHE_MARKS - used;
  if (room < 1) return messages;
  const out = [...messages];
  const ends = [out.length - 1];
  for (let i = out.length - 2; i >= 0; i--) if (out[i]?.role === "user") { ends.push(i); break; }
  for (const i of ends) {
    if (room < 1) break;
    const next = markEnd(out[i]);
    if (next) { out[i] = next; room--; }
  }
  return out;
}

export function inferenceProviderRequest({ model, maxTokens, body, reasoning, thinking,
  credits, key, signal, serverFallbacks = false, cache = true }) {
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
        system: body.system, messages: cache ? cacheConversation(model, body) : body.messages, tools: body.tools,
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

// A direct billing refusal precedes generation. Retry the same model once on
// AC's router account, and tell the meter which provider actually served it.
export async function requestHostedInference(options, fetchImpl = fetch) {
  const direct = Boolean(options.credits && ANTHROPIC_DIRECT[options.model]);
  let response = await requestInference(options, fetchImpl);
  // A refusal that names the cache marks is ours to fix, not the caller's:
  // send the conversation as it came. Nothing was generated or billed.
  if (response.status === 400 && options.cache !== false) {
    const detail = await response.clone().json().catch(() => null);
    if (/cache_control/i.test(detail?.error?.message || "")) {
      await response.body?.cancel();
      options = { ...options, cache: false };
      response = await requestInference(options, fetchImpl);
    }
  }
  if (direct && options.key && [400, 402].includes(response.status)) {
    const detail = await response.clone().json().catch(() => null);
    if (/credit balance is too low/i.test(detail?.error?.message || "")) {
      await response.body?.cancel();
      const routed = await requestInference({ ...options, credits: undefined }, fetchImpl);
      return { response: routed, provider: "openrouter" };
    }
  }
  return { response, provider: direct ? "anthropic" : "openrouter" };
}
