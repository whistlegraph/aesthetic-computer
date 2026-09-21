import { readFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { parseEnv } from 'node:util';
import { evaluateChoices } from '../../easel/src/jev-decisions.mjs';

// Loaded only for an explicit decision request. Never attach credentials to
// browser state, MCP replies, logs, or the page itself.
export function evaluateConfiguredChoices(request, options = {}) {
  let apiKey = process.env.OPENROUTER_API_KEY;
  if (!apiKey) try {
    apiKey = parseEnv(readFileSync(join(homedir(), '.config/aesthetic-computer/jev.env'), 'utf8')).OPENROUTER_API_KEY;
  } catch {}
  return evaluateChoices(request, { ...options, apiKey });
}
