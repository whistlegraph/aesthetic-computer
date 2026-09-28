// Explicit hosted selections. Every model is billed in braincells: at twice
// what OpenRouter reports it cost, or at the model's fixed rate when the
// provider reports no cost (easel-paid-credits.mjs). The older entries stay
// because installed clients still ask for them by name.
export const EASEL_MODELS = {
  "deepseek/deepseek-v4.1-flash": { label: "DeepSeek V4.1 Flash" },
  "deepseek/deepseek-v4-pro": { label: "DeepSeek V4 Pro" },
  "moonshotai/kimi-k3": { label: "Kimi K3" },
  "qwen/qwen3.7-plus": { label: "Qwen 3.7 Plus" },
  "minimax/minimax-m3": { label: "MiniMax M3" },
  "z-ai/glm-5.3-flash": { label: "GLM 5.3 Flash" },
  "openai/gpt-5.6-luna": { label: "Luna" },
  "anthropic/claude-opus-5": { label: "Opus (premium)" },
  "z-ai/glm-4.6": { label: "glm" },
  "qwen/qwen3-coder": { label: "qwen" },
  "deepseek/deepseek-chat-v3.1": { label: "deepseek" },
  "anthropic/claude-sonnet-4.6": { label: "sonnet (premium)" },
  "openai/gpt-5.4": { label: "gpt (premium)" },
};
// Best of the 2026-09-28 trials, and among the cheapest to run.
export const DEFAULT_EASEL_MODEL = "deepseek/deepseek-v4.1-flash";
// What a request gets when it does not say, and the most it may ask for. A
// workspace turn writes whole files, so the ceiling is the client's 32,000;
// the braincell hold grows with it, so a long answer is paid for up front.
export const DEFAULT_MAX_TOKENS = 8192;
export const HOSTED_MAX_TOKENS = 32000;

export function inferenceRequest(body) {
  if (!body || typeof body !== "object" || Array.isArray(body)) throw new Error("Expected an inference request object.");
  const model = body.model === undefined ? DEFAULT_EASEL_MODEL : body.model;
  if (typeof model !== "string" || !Object.hasOwn(EASEL_MODELS, model)) throw new Error("Unsupported model. Select a model from /model.");
  const wanted = body.max_tokens === undefined ? DEFAULT_MAX_TOKENS : body.max_tokens;
  if (!Number.isSafeInteger(wanted) || wanted < 1) throw new Error("max_tokens must be a positive integer.");
  if (!Array.isArray(body.messages) || body.messages.length === 0) throw new Error("At least one message is required.");
  return { model, maxTokens: Math.min(wanted, HOSTED_MAX_TOKENS) };
}

export function inferenceBudgetFailure(budget, handle) {
  if (!budget || budget.unknown || !Number.isFinite(budget.remaining) || !Number.isFinite(budget.budget) || budget.budget <= 0) {
    return { statusCode: 503, message: "Hosted inference cannot verify your remaining allowance. Try again shortly." };
  }
  if (budget.exhausted || budget.remaining <= 0) {
    return { statusCode: 429, message: `@${handle} has used today's free braincells (${budget.used}/${budget.budget}). They reset at midnight UTC.` };
  }
  return null;
}
