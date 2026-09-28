// open-server.mjs — Aesel's own agent loop on an open-weight model.
//
// The hosted bridge (ac-server.mjs) already runs the loop itself and speaks
// the Anthropic Messages API, only to aesthetic.computer's relay. OpenRouter
// answers that same API for every model it routes, so this bridge is that loop
// pointed straight at OpenRouter with the person's own key — no vendor CLI, no
// handle budget. In a pro session (a repository rather than a piece) it also
// carries the workspace tools, which is what makes it a coding agent.
import { readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import { parseEnv } from "node:util";

import { AcServer } from "./ac-server.mjs";
import { McpTools } from "./mcp-client.mjs";

const OPENROUTER = "https://openrouter.ai/api/v1/messages";

// Names a person would type, and what the picker says about each: how capable
// (out of five) and how dear. The marks come from Aesel's own trials on
// 2026-09-28 — five coding tasks with hidden tests and a stick-figure piece —
// not from a leaderboard; prices are OpenRouter's that day, per million tokens
// as cache-read / input / output. An agent day is mostly cache reads.
export const OPEN_MODEL_INFO = {
  flash: { id: "deepseek/deepseek-v4.1-flash", label: "DeepSeek V4.1 Flash", smart: 4, cost: 1 }, // 5/5, best scene · 0.006 / 0.30 / 1.20
  deepseek: { id: "deepseek/deepseek-v4-pro", label: "DeepSeek V4 Pro", smart: 4, cost: 2 }, // 5/5, plain scene · 0.065 / 0.78 / 1.57
  kimi: { id: "moonshotai/kimi-k3", label: "Kimi K3", smart: 4, cost: 4 }, // 5/5, good scene · 0.30 / 3.00 / 15.0
  qwen: { id: "qwen/qwen3.7-plus", label: "Qwen 3.7 Plus", smart: 3, cost: 1 }, // 5/5, scene lost its ground · 0.064 / 0.32 / 1.28
  minimax: { id: "minimax/minimax-m3", label: "MiniMax M3", smart: 3, cost: 1 }, // 5/5 slowly, scene lost its ground · 0.06 / 0.30 / 1.20
  glm: { id: "z-ai/glm-5.3-flash", label: "GLM 5.3 Flash", smart: 2, cost: 1 }, // 5/5 slowly, piece drew nothing · 0.03 / 0.15 / 0.50
};
export const OPEN_MODELS = Object.fromEntries(Object.entries(OPEN_MODEL_INFO).map(([name, info]) => [name, info.id]));

export const DEFAULT_OPEN_MODEL = OPEN_MODELS.flash;

export function openRouterKey({ env = process.env, home = homedir() } = {}) {
  if (env.OPENROUTER_API_KEY) return env.OPENROUTER_API_KEY;
  for (const file of [".config/aesthetic-computer/openrouter.env", ".config/aesthetic-computer/jev.env"]) {
    try {
      const key = parseEnv(readFileSync(join(home, file), "utf8")).OPENROUTER_API_KEY;
      if (key) return key;
    } catch {}
  }
  return "";
}

export class OpenServer extends AcServer {
  constructor({ pro = false, apiKey = openRouterKey(), model = DEFAULT_OPEN_MODEL, ...options } = {}) {
    super({
      ...options,
      model: model || DEFAULT_OPEN_MODEL,
      endpoint: OPENROUTER,
      apiKey,
      models: OPEN_MODELS,
      fallbackModel: DEFAULT_OPEN_MODEL,
      workspace: pro,
      // A repository task reads, edits and tests; twelve rounds is a piece.
      rounds: pro ? 80 : 12,
      jev: null,
      // The person's MCP servers, as Claude Code would load them here.
      extensions: pro && options.cwd ? new McpTools(options.cwd) : null,
    });
    if (!apiKey) throw new Error("No OpenRouter key: set OPENROUTER_API_KEY or put it in ~/.config/aesthetic-computer/openrouter.env");
  }
}
