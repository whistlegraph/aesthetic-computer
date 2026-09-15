// Explicit hosted selections. The account budget is token-based, not a dollar
// ceiling; premium models are never substituted for the inexpensive default.
export const EASEL_MODELS = {
  "z-ai/glm-4.6": { label: "glm" },
  "qwen/qwen3-coder": { label: "qwen" },
  "deepseek/deepseek-chat-v3.1": { label: "deepseek" },
  "anthropic/claude-sonnet-4.6": { label: "sonnet (premium)" },
  "openai/gpt-5.4": { label: "gpt (premium)" },
};
export const DEFAULT_EASEL_MODEL = "z-ai/glm-4.6";
const MAX_TOKENS = 8192;

export function inferenceRequest(body) {
  if (!body || typeof body !== "object" || Array.isArray(body)) throw new Error("Expected an inference request object.");
  const model = body.model === undefined ? DEFAULT_EASEL_MODEL : body.model;
  if (typeof model !== "string" || !Object.hasOwn(EASEL_MODELS, model)) throw new Error("Unsupported model. Select a model from /model.");
  const wanted = body.max_tokens === undefined ? MAX_TOKENS : body.max_tokens;
  if (!Number.isSafeInteger(wanted) || wanted < 1) throw new Error("max_tokens must be a positive integer.");
  if (!Array.isArray(body.messages) || body.messages.length === 0) throw new Error("At least one message is required.");
  return { model, maxTokens: Math.min(wanted, MAX_TOKENS) };
}

export function inferenceBudgetFailure(budget, handle) {
  if (!budget || budget.unknown || !Number.isFinite(budget.remaining) || !Number.isFinite(budget.budget) || budget.budget <= 0) {
    return { statusCode: 503, message: "Hosted inference cannot verify your remaining allowance. Try again shortly." };
  }
  if (budget.exhausted || budget.remaining <= 0) {
    return { statusCode: 429, message: `@${handle} has used today's allowance (${budget.used}/${budget.budget} tokens). It resets at midnight UTC.` };
  }
  return null;
}
