// model-catalog — what each provider can run, asked of the provider itself.
//
// A model list typed into the source goes stale the week a model ships, and
// the picker then offers last season. So the list is fetched: Codex's from its
// own app-server, Claude's from the Models API with whatever credential the
// Claude CLI already holds on this machine — an API key in the environment, or
// the login Claude Code keeps in the keychain. The answer is cached on disk
// for a few hours so the drop-down opens at once, and a fetch that fails falls
// back to the cache, then to a short table that is at least never empty.
import { execFile } from "node:child_process";
import { mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import { promisify } from "node:util";
import { codexModels } from "./provider-picker.mjs";

const run = promisify(execFile);
export const CATALOG_TTL_MS = 6 * 60 * 60 * 1000;

// The table the picker shows when nothing can be asked. Ids as the API
// names them today; the fetch replaces this the moment it works.
export const CLAUDE_FALLBACK = [
  { id: "claude-fable-5-1", displayName: "Claude Fable 5.1" },
  { id: "claude-opus-5", displayName: "Claude Opus 5" },
  { id: "claude-sonnet-5", displayName: "Claude Sonnet 5" },
  { id: "claude-opus-4-8", displayName: "Claude Opus 4.8" },
  { id: "claude-opus-4-7", displayName: "Claude Opus 4.7" },
  { id: "claude-sonnet-4-6", displayName: "Claude Sonnet 4.6" },
  { id: "claude-haiku-4-5", displayName: "Claude Haiku 4.5" },
];

// The credential the way the SDK resolves it: a key, a token, or the Claude
// Code login this machine already holds. Nothing is stored; the token is read
// for one request.
export async function claudeCredential({ env = process.env, keychain = readKeychainToken } = {}) {
  if (env.ANTHROPIC_API_KEY) return { "x-api-key": env.ANTHROPIC_API_KEY };
  if (env.ANTHROPIC_AUTH_TOKEN) return { authorization: `Bearer ${env.ANTHROPIC_AUTH_TOKEN}` };
  const token = await keychain().catch(() => "");
  if (token) return { authorization: `Bearer ${token}`, "anthropic-beta": "oauth-2025-04-20" };
  return null;
}

async function readKeychainToken() {
  if (process.platform !== "darwin") return "";
  const { stdout } = await run("security", ["find-generic-password", "-s", "Claude Code-credentials", "-w"], { timeout: 3000 });
  return JSON.parse(stdout.trim())?.claudeAiOauth?.accessToken || "";
}

export async function claudeModels({ fetch = globalThis.fetch, env = process.env, keychain, timeoutMs = 6000 } = {}) {
  const auth = await claudeCredential({ env, keychain });
  if (!auth) throw new Error("no Anthropic credential on this machine");
  const response = await fetch("https://api.anthropic.com/v1/models?limit=100", {
    headers: { "anthropic-version": "2023-06-01", ...auth },
    signal: AbortSignal.timeout(timeoutMs),
  });
  if (!response.ok) throw new Error(`Models API answered ${response.status}`);
  const { data = [] } = await response.json();
  return data
    .filter((m) => typeof m.id === "string" && m.id.startsWith("claude-"))
    .map((m) => ({ id: m.id, displayName: m.display_name || m.id, created: m.created_at || "" }))
    .sort((a, b) => (b.created > a.created ? 1 : b.created < a.created ? -1 : 0));
}

const LOADERS = {
  claude: (options) => claudeModels(options),
  // Codex's rows keep their own fields (supported efforts, hidden); `id` is
  // added so every catalog reads the same way.
  codex: async (options) => (await codexModels(options)).map((m) => ({ ...m, id: m.model })),
};

export function catalogFile(provider, root = join(process.env.XDG_CACHE_HOME || join(homedir(), ".cache"), "easel")) {
  return join(root, `models-${provider}.json`);
}

function readCache(file) {
  try {
    const { at, models } = JSON.parse(readFileSync(file, "utf8"));
    return Array.isArray(models) ? { at: Number(at) || 0, models } : null;
  } catch {
    return null;
  }
}

// Fresh cache → at once. Otherwise ask, write, answer; a failed ask answers
// with the stale cache, then the fallback table.
export async function loadCatalog(provider, { force = false, ttlMs = CATALOG_TTL_MS, root, now = Date.now, loaders = LOADERS, ...options } = {}) {
  const loader = loaders[provider];
  if (!loader) return [];
  const file = catalogFile(provider, root);
  const cached = readCache(file);
  if (cached && !force && now() - cached.at < ttlMs) return cached.models;
  try {
    const models = await loader(options);
    if (models.length) {
      mkdirSync(join(file, ".."), { recursive: true, mode: 0o700 });
      const temporary = `${file}.${process.pid}.tmp`;
      writeFileSync(temporary, `${JSON.stringify({ at: now(), models })}\n`, { mode: 0o600 });
      renameSync(temporary, file);
      return models;
    }
  } catch {}
  if (cached?.models.length) return cached.models;
  return provider === "claude" ? CLAUDE_FALLBACK : [];
}
