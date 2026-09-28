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

const OPENROUTER = "https://openrouter.ai/api/v1/messages";

// Names a person would type. Prices on OpenRouter, 2026-09-28, USD per million
// tokens as cache-read / input / output. An agent day is mostly cache reads, so
// the first column decides the bill.
export const OPEN_MODELS = {
  flash: "deepseek/deepseek-v4.1-flash", // 0.006 / 0.30 / 1.20
  deepseek: "deepseek/deepseek-v4-pro", // 0.065 / 0.78 / 1.57
  glm: "z-ai/glm-5.3-flash", // 0.03 / 0.15 / 0.50
  qwen: "qwen/qwen3.7-plus", // 0.064 / 0.32 / 1.28
  minimax: "minimax/minimax-m3", // 0.06 / 0.30 / 1.20
  kimi: "moonshotai/kimi-k3", // 0.30 / 3.00 / 15.0
};

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
  constructor({ pro = false, apiKey = openRouterKey(), ...options } = {}) {
    super({
      ...options,
      endpoint: OPENROUTER,
      apiKey,
      models: OPEN_MODELS,
      fallbackModel: DEFAULT_OPEN_MODEL,
      workspace: pro,
      // A repository task reads, edits and tests; twelve rounds is a piece.
      rounds: pro ? 80 : 12,
      jev: null,
    });
    if (!apiKey) throw new Error("No OpenRouter key: set OPENROUTER_API_KEY or put it in ~/.config/aesthetic-computer/openrouter.env");
  }
}
