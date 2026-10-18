// Service-scoped credential lookup. Never source an env file as shell code or
// hand the whole vault to an MCP. Add an explicit service contract here instead.
import { existsSync, readFileSync, statSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { parseEnv } from "node:util";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
export const INSTAGRAM_ACCOUNTS = { oskiewar: "OSKIEWAR", whistlegraph: "WHISTLEGRAPH", aesthetic: "AESTHETIC", menuband: "MENUBAND" };
export const X_ACCOUNTS = { promptdotac: "PROMPTDOTAC", whistlegraph: "WHISTLEGRAPH" };
const SERVICES = {
  cloudflare: {
    files: ["cloudflare.env", "cloudflare/.env", "feed/.env", "lith/.env", ".env",
      ".devcontainer/envs/devcontainer.env", "oven/deploy.env", "nanos/conductor.env",
      "help/deploy.env", "at/deploy.env"],
    variants: [
      { auth: "api-token", keys: ["CLOUDFLARE_API_TOKEN"] },
      { auth: "global-key", keys: ["CLOUDFLARE_EMAIL", "CLOUDFLARE_API_KEY"] },
    ],
  },
  porkbun: {
    files: ["porkbun.env", "porkbun/.env", ".env"],
    variants: [{ auth: "api-key", keys: ["PORKBUN_API_KEY", "PORKBUN_SECRET_API_KEY"] }],
  },
  openai: { files: ["openai.env", "openai/.env", ".devcontainer/envs/devcontainer.env"],
    variants: [{ auth: "api-key", keys: ["OPENAI_API_KEY"] }] },
  fal: { files: ["fal.env", "fal/.env", ".devcontainer/envs/devcontainer.env"],
    variants: [{ auth: "api-key", keys: ["FAL_KEY"] }] },
  exa: { files: ["mcp/exa.env", "exa/.env"], envFile: "AC_WEB_SEARCH_ENV", localFile: ".config/ac-web-search/exa.env",
    variants: [{ auth: "api-key", keys: ["EXA_API_KEY"] }] },
  slack: { files: ["mcp/slack.env"], envFile: "SLACK_MCP_ENV",
    optionalKeys: ["SLACK_MCP_ADD_MESSAGE_TOOL"],
    variants: [
      { auth: "user-token", keys: ["SLACK_MCP_XOXP_TOKEN"] },
      { auth: "bot-token", keys: ["SLACK_MCP_XOXB_TOKEN"] },
      { auth: "session-token", keys: ["SLACK_MCP_XOXC_TOKEN", "SLACK_MCP_XOXD_TOKEN"] },
    ] },
  bluesky: { files: ["at/.env"], repoFile: "at/.env", optionalKeys: ["BSKY_SERVICE"],
    variants: [{ auth: "app-password", keys: ["BSKY_IDENTIFIER", "BSKY_APP_PASSWORD"] }] },
  "apple-notary": { files: ["apple/app-specific-password.env"], optionalKeys: ["APPLE_TEAM_ID"],
    variants: [
      { auth: "app-password", keys: ["APPLE_ID", "APPLE_APP_PASSWORD"] },
      { auth: "app-password", keys: ["APPLE_ID", "APP_SPECIFIC_PASSWORD"] },
    ] },
  ...Object.fromEntries(Object.entries(INSTAGRAM_ACCOUNTS).map(([account, prefix]) => [`instagram:${account}`, {
    files: [`${account}/instagram.env`],
    optionalKeys: ["ENDPOINT", "BUCKET", "KEY", "SECRET"].map(key => `${prefix}_SPACES_${key}`),
    variants: [{ auth: "access-token", keys: [`${prefix}_IG_USER_ID`, `${prefix}_IG_TOKEN`] }],
  }])),
  ...Object.fromEntries(Object.entries(X_ACCOUNTS).map(([account, prefix]) => [`x:${account}`, {
    files: [`${account}/x.env`],
    variants: [{ auth: "oauth1", keys: ["API_KEY", "API_SECRET", "ACCESS_TOKEN", "ACCESS_SECRET"].map(key => `${prefix}_X_${key}`) }],
  }])),
};

export function vaultRoot({ env = process.env, repo = REPO, home = homedir() } = {}) {
  // An explicit override is authoritative, even when missing. Never silently
  // switch accounts because a configured mount is temporarily unavailable.
  if (env.AC_VAULT) return resolve(env.AC_VAULT);
  return [resolve(repo, "aesthetic-computer-vault"), resolve(repo, "vault"),
    resolve(repo, "../aesthetic-computer-vault"), resolve(home, "aesthetic-computer-vault")]
    .find(path => { try { return statSync(path).isDirectory(); } catch { return false; } });
}

export function vaultPath(relative, options) {
  const root = vaultRoot(options) || resolve(options?.repo || REPO, "aesthetic-computer-vault");
  const path = resolve(root, relative);
  if (!path.startsWith(`${root}/`)) throw new Error("Expected a path within the vault");
  return path;
}

function select(service, definition, data, source, path, env = {}) {
  const allKeys = [...new Set(definition.variants.flatMap(v => v.keys))];
  if (!allKeys.some(key => data[key]?.trim())) return null;
  const variant = definition.variants.find(v => v.keys.every(key => data[key]?.trim()));
  const result = { service, state: variant ? "ready" : "incomplete", source,
    present: allKeys.filter(key => data[key]?.trim()),
    ...(variant ? { auth: variant.auth } : { requiredAlternatives: definition.variants.map(v => v.keys) }) };
  // Accidental JSON.stringify/logging of a resolution returns status only.
  const values = Object.fromEntries((variant?.keys || []).map(key => [key, data[key].trim()]));
  for (const key of definition.optionalKeys || []) {
    const value = env[key] ?? data[key];
    if (value?.trim()) values[key] = value.trim();
  }
  Object.defineProperty(result, "values", { value: values });
  Object.defineProperty(result, "path", { value: path });
  return result;
}

export function resolveCredentials(service, options = {}) {
  if (!Object.hasOwn(SERVICES, service)) throw new Error("Unknown vault service");
  const definition = SERVICES[service];
  const env = options.env || process.env;
  const fromEnv = select(service, definition, env, "environment");
  if (fromEnv) return fromEnv;
  const root = vaultRoot(options);
  const override = options.file || env[definition.envFile];
  const candidates = override ? [{ path: resolve(override), source: "configured-file" }] : [
    ...(!env.AC_VAULT && definition.localFile ? [{ path: resolve(options.home || homedir(), definition.localFile), source: "local-config" }] : []),
    ...(!env.AC_VAULT && definition.repoFile ? [{ path: resolve(options.repo || REPO, definition.repoFile), source: `checkout:${definition.repoFile}` }] : []),
    ...(root ? definition.files.map(file => ({ path: resolve(root, file), source: `vault:${file}` })) : []),
  ];
  let incomplete;
  const locked = [];
  for (const { path, source } of candidates) {
    let data;
    try { data = parseEnv(readFileSync(path, "utf8")); }
    catch (error) {
      if (error.code !== "ENOENT") return { service, state: "unreadable", source };
      if (existsSync(`${path}.gpg`)) locked.push(source);
      continue;
    }
    const found = select(service, definition, data, source, path, env);
    if (found?.state === "ready") return found;
    incomplete ||= found;
  }
  return incomplete || { service, state: locked.length ? "locked" : "missing",
    ...(locked.length ? { encryptedFiles: locked } : {}),
    requiredAlternatives: definition.variants.map(v => v.keys) };
}

export function credentialStatus(service, options) {
  return { ...resolveCredentials(service, options) };
}

export function vaultStatus(options, services = Object.keys(SERVICES)) {
  return { services: services.map(service => credentialStatus(service, options)) };
}

export function requireCredentials(service, options) {
  const resolved = resolveCredentials(service, options);
  if (resolved.state !== "ready") throw new Error(
    `${service} credentials ${resolved.state}${resolved.source ? ` (${resolved.source})` : ""}. ` +
    "Use the unlocked aesthetic-computer-vault (AC_VAULT overrides its location), or service environment variables.");
  return resolved;
}

export function redactCredentials(text, values) {
  let safe = String(text);
  for (const value of Object.values(values || {}).filter(Boolean).sort((a, b) => b.length - a.length)) {
    for (const spelling of new Set([value, encodeURIComponent(value), JSON.stringify(value).slice(1, -1)])) {
      safe = safe.split(spelling).join("[redacted]");
    }
  }
  return safe;
}
