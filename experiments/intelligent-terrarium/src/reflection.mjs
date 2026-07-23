import { canonical, hash } from "./canonical.mjs";
import { decideReflection, validateReflectionProposal } from "./reflection-policy.mjs";

const MAX_CONTEXT_TOKENS = 2048;
const MAX_OUTPUT_TOKENS = 128;
const MAX_PROMPT_CHARS = 6000;
const MAX_OUTPUT_CHARS = 8192;

function episodeCounts(episodes) {
  const counts = {};
  for (const episode of episodes.slice(-64)) {
    const key = String(episode.kind || "unknown").slice(0, 32);
    counts[key] = (counts[key] || 0) + 1;
  }
  return Object.fromEntries(Object.entries(counts).sort(([a], [b]) => a.localeCompare(b)));
}

export function reflectionDigest(repository) {
  const state = repository.terrarium.state;
  const energy = state.entities.reduce((sum, entity) => sum + entity.energy, 0) / state.entities.length;
  return {
    schema: 1,
    stateHash: repository.stateHash(),
    tick: state.tick,
    entityCount: state.entities.length,
    meanEnergy: Math.round(energy * 1000) / 1000,
    drives: state.mind.drives,
    weights: state.mind.weights,
    recentKinds: episodeCounts(state.mind.episodes),
  };
}

export function reflectionPrompt(repository) {
  const prompt = [
    "You are the bounded reflection organ of a deterministic terrarium.",
    "Return exactly one JSON object and nothing else. Do not think aloud.",
    "The object must contain exactly four fields: schema, action, target, and intensity.",
    "Set schema to integer 1 and action to string attune.",
    "The target may be sensory, spatial, drive, memory, action, or voice.",
    "Use action attune. Keep intensity numeric and between -0.25 and 0.25. /no_think",
    `State digest: ${canonical(reflectionDigest(repository))}`,
  ].join("\n");
  if (prompt.length > MAX_PROMPT_CHARS) throw new Error("reflection prompt exceeded bound");
  return prompt;
}

export function parseReflectionOutput(text) {
  const bounded = String(text || "");
  if (!bounded || bounded.length > MAX_OUTPUT_CHARS) throw new TypeError("reflection output is empty or oversized");
  const withoutThought = bounded.replace(/<think>[\s\S]*?<\/think>/gi, "").replace(/```(?:json)?|```/gi, "").trim();
  const candidates = withoutThought.match(/\{[^{}]*\}/g) || [];
  for (let index = candidates.length - 1; index >= 0; index -= 1) {
    try {
      return validateReflectionProposal(JSON.parse(candidates[index]));
    } catch { /* Try the preceding flat object; llama-cli may echo the prompt. */ }
  }
  throw new TypeError("reflection output has no valid proposal object");
}

function failureKind(error) {
  if (error?.code === "ETIMEDOUT" || error?.name === "TimeoutError" || error?.name === "AbortError") return "timeout";
  if (error instanceof SyntaxError || error instanceof TypeError) return "malformed";
  return "unavailable";
}

async function boundedInference(infer, request, timeoutMs) {
  const controller = new AbortController();
  let timer;
  const timeout = new Promise((resolve, reject) => {
    timer = setTimeout(() => {
      controller.abort();
      const error = new Error("reflection inference timed out");
      error.name = "TimeoutError";
      error.code = "ETIMEDOUT";
      reject(error);
    }, timeoutMs);
  });
  try {
    return await Promise.race([infer({ ...request, signal: controller.signal }), timeout]);
  } finally {
    clearTimeout(timer);
  }
}

export class ReflectionOrgan {
  constructor(repository, {
    infer = null,
    engine = "1gb-policy",
    contextTokens = MAX_CONTEXT_TOKENS,
    maxOutputTokens = 96,
    timeoutMs = 30_000,
  } = {}) {
    this.repository = repository;
    this.infer = infer;
    this.engine = String(engine).replace(/[^a-zA-Z0-9._:/-]/g, "-").slice(0, 160) || "unknown";
    this.contextTokens = Math.max(256, Math.min(MAX_CONTEXT_TOKENS, Math.floor(contextTokens)));
    this.maxOutputTokens = Math.max(1, Math.min(MAX_OUTPUT_TOKENS, Math.floor(maxOutputTokens)));
    this.timeoutMs = Math.max(100, Math.min(30_000, Math.floor(timeoutMs)));
    this.tail = Promise.resolve();
  }

  reflect() {
    const result = this.tail.then(() => this.#reflect());
    this.tail = result.catch(() => {});
    return result;
  }

  async #reflect() {
    const prompt = reflectionPrompt(this.repository);
    let proposal;
    let failure;
    let inference = null;
    let outputDigest = null;
    if (!this.infer) {
      failure = "disabled";
      outputDigest = hash("reflection-disabled");
    } else {
      try {
        inference = await boundedInference(this.infer, {
          prompt,
          contextTokens: this.contextTokens,
          maxOutputTokens: this.maxOutputTokens,
          timeoutMs: this.timeoutMs,
        }, this.timeoutMs);
        const text = typeof inference === "string" ? inference : inference?.text;
        outputDigest = hash(String(text || ""));
        proposal = parseReflectionOutput(text);
      } catch (error) {
        failure = failureKind(error);
        outputDigest ||= hash(`${failure}:${error?.name || "Error"}`);
      }
    }
    const evaluation = decideReflection({ proposal, failure });
    const requestId = hash({ head: this.repository.headRecordHash, outputDigest }).slice(0, 24);
    const payload = {
      schema: 1,
      requestId,
      engine: this.engine,
      outputDigest,
      decision: evaluation.decision,
      reason: evaluation.reason,
    };
    if (evaluation.proposal) payload.proposal = evaluation.proposal;
    else payload.failure = failure;
    const transaction = await this.repository.transact("reflection-decision", payload);
    return {
      ...transaction,
      decision: evaluation.decision,
      reason: evaluation.reason,
      promptChars: prompt.length,
      contextTokens: this.contextTokens,
      maxOutputTokens: this.maxOutputTokens,
      timeoutMs: this.timeoutMs,
      metrics: typeof inference === "object" ? inference?.metrics || null : null,
    };
  }
}
