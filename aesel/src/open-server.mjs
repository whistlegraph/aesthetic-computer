// open-server.mjs — Aesel's own agent loop on an open-weight model.
//
// The hosted bridge (ac-server.mjs) already runs the loop itself and speaks
// the Anthropic Messages API, only to aesthetic.computer's relay. OpenRouter
// answers that same API for every model it routes, so this bridge is that loop
// pointed straight at OpenRouter with the person's own key — no vendor CLI, no
// handle budget. In a pro session (a repository rather than a piece) it also
// carries the workspace tools, which is what makes it a coding agent.
import {openRouterKey} from './open-key.mjs';
export {openRouterKey};

import { AcServer } from "./ac-server.mjs";
import { McpTools } from "./mcp-client.mjs";

const OPENROUTER = "https://openrouter.ai/api/v1/messages";

import { DEFAULT_OPEN_MODEL, OPEN_MODELS } from "./open-models.mjs";
export { OPEN_MODEL_INFO, OPEN_MODELS, DEFAULT_OPEN_MODEL } from "./open-models.mjs";

// What a pro session (a repository rather than a piece) adds to either loop:
// the workspace tools, room for a repository task, and the person's MCP
// servers, as Claude Code would load them here. The tools run on this machine;
// only the messages go to the model.
export function proLoop({ pro = false, cwd } = {}) {
  return {
    workspace: pro,
    // A repository task reads, edits and tests; twelve rounds is a piece.
    rounds: pro ? 80 : 12,
    extensions: pro && cwd ? new McpTools(cwd) : null,
  };
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
      ...proLoop({ pro, cwd: options.cwd }),
      jev: null,
    });
    if (!apiKey) throw new Error("No OpenRouter key: set OPENROUTER_API_KEY or put it in ~/.config/aesthetic-computer/openrouter.env");
  }
}

// The same loop on aesthetic.computer's relay, paid in braincells: in pro it
// carries exactly what OpenServer does.
export class HostedServer extends AcServer {
  constructor({ pro = false, ...options } = {}) {
    super({ ...options, ...proLoop({ pro, cwd: options.cwd }) });
  }
}
