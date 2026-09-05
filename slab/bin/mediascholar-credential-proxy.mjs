#!/usr/bin/env node
// Loopback-only credential broker for Mediascholar. Agent harnesses receive a
// dummy credential and a local base URL; this process alone reads the real
// provider credential. It forwards only the inference routes the harnesses
// need, applies request/concurrency ceilings, and never logs prompt bodies.

import { createServer } from "node:http";
import { access, mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const exists = async (path) => access(path).then(() => true, () => false);
const envNumber = (env, key, fallback) => {
  const value = Number(env[key]);
  return Number.isFinite(value) ? value : fallback;
};

export function proxyConfig(env = process.env) {
  return {
    host: env.MEDIASCHOLAR_PROXY_HOST || "127.0.0.1",
    port: envNumber(env, "MEDIASCHOLAR_PROXY_PORT", 7431),
    statePath: resolve(env.MEDIASCHOLAR_PROXY_STATE
      || `${homedir()}/.local/share/mediascholar/proxy-usage.json`),
    claudeTokenFile: resolve(env.MEDIASCHOLAR_CLAUDE_TOKEN_FILE
      || `${homedir()}/.config/claude/oauth-token`),
    anthropicKeyFile: resolve(env.MEDIASCHOLAR_ANTHROPIC_KEY_FILE
      || `${homedir()}/.config/mediascholar/credentials/anthropic`),
    openaiKeyFile: resolve(env.MEDIASCHOLAR_OPENAI_KEY_FILE
      || `${homedir()}/.config/mediascholar/credentials/openai`),
    anthropicBase: (env.MEDIASCHOLAR_ANTHROPIC_UPSTREAM || "https://api.anthropic.com").replace(/\/$/, ""),
    openaiBase: (env.MEDIASCHOLAR_OPENAI_UPSTREAM || "https://api.openai.com").replace(/\/$/, ""),
    maxDailyRequests: envNumber(env, "MEDIASCHOLAR_PROXY_MAX_DAILY_REQUESTS", 160),
    maxConcurrent: envNumber(env, "MEDIASCHOLAR_PROXY_MAX_CONCURRENT", 2),
    maxBodyBytes: envNumber(env, "MEDIASCHOLAR_PROXY_MAX_BODY_MIB", 16) * 1024 * 1024,
  };
}

async function secret(path) {
  if (!await exists(path)) return null;
  const value = (await readFile(path, "utf8")).trim();
  return value || null;
}

export async function loadCredentials(config) {
  const anthropicKey = await secret(config.anthropicKeyFile);
  const claudeOAuth = await secret(config.claudeTokenFile);
  const openaiKey = await secret(config.openaiKeyFile);
  return {
    claude: anthropicKey
      ? { kind: "api-key", value: anthropicKey }
      : claudeOAuth ? { kind: "oauth", value: claudeOAuth } : null,
    openai: openaiKey ? { kind: "api-key", value: openaiKey } : null,
  };
}

const day = () => new Date().toISOString().slice(0, 10);

async function readUsage(path) {
  try {
    const value = JSON.parse(await readFile(path, "utf8"));
    return value.day === day() ? value : { day: day(), requests: 0, providers: {} };
  } catch { return { day: day(), requests: 0, providers: {} }; }
}

async function writeUsage(path, value) {
  await mkdir(dirname(path), { recursive: true });
  const temp = `${path}.${process.pid}.tmp`;
  await writeFile(temp, `${JSON.stringify(value, null, 2)}\n`, { mode: 0o600 });
  await rename(temp, path);
}

function providerRoute(pathname) {
  if (pathname.startsWith("/anthropic/")) {
    return { provider: "claude", upstreamPath: pathname.slice("/anthropic".length) };
  }
  if (pathname.startsWith("/openai/")) {
    return { provider: "openai", upstreamPath: pathname.slice("/openai".length) };
  }
  return null;
}

function allowed(provider, path) {
  if (provider === "openai") {
    return path === "/v1/responses" || path.startsWith("/v1/responses/") || path === "/v1/models";
  }
  return path === "/v1/messages"
    || path === "/v1/messages/count_tokens"
    || path === "/v1/models"
    || path === "/api/oauth/usage";
}

function methodAllowed(method, provider, path) {
  if (method === "POST") return allowed(provider, path);
  return method === "GET" && (path === "/v1/models" || (provider === "claude" && path === "/api/oauth/usage"));
}

function upstreamHeaders(req, credential) {
  const headers = new Headers();
  const blocked = new Set([
    "authorization", "x-api-key", "cookie", "host", "content-length",
    "connection", "keep-alive", "proxy-authenticate", "proxy-authorization",
    "te", "trailers", "transfer-encoding", "upgrade",
  ]);
  for (const [key, value] of Object.entries(req.headers)) {
    if (!blocked.has(key.toLowerCase()) && value != null) headers.set(key, Array.isArray(value) ? value.join(",") : value);
  }
  if (credential.kind === "oauth") headers.set("authorization", `Bearer ${credential.value}`);
  else if (credential.provider === "openai") headers.set("authorization", `Bearer ${credential.value}`);
  else headers.set("x-api-key", credential.value);
  return headers;
}

async function readBody(req, maxBytes) {
  const chunks = [];
  let size = 0;
  for await (const chunk of req) {
    size += chunk.length;
    if (size > maxBytes) throw Object.assign(new Error("request body too large"), { status: 413 });
    chunks.push(chunk);
  }
  return Buffer.concat(chunks);
}

function safeResponseHeaders(upstream) {
  const out = {};
  const blocked = new Set(["connection", "content-length", "transfer-encoding", "content-encoding"]);
  for (const [key, value] of upstream.headers) if (!blocked.has(key.toLowerCase())) out[key] = value;
  return out;
}

export function createCredentialProxy(config, credentials) {
  let concurrent = 0;
  return createServer(async (req, res) => {
    const url = new URL(req.url || "/", `http://${config.host}:${config.port}`);
    if (req.method === "GET" && url.pathname === "/health") {
      const usage = await readUsage(config.statePath);
      res.writeHead(200, { "content-type": "application/json", "cache-control": "no-store" });
      res.end(JSON.stringify({
        ok: true,
        providers: { claude: Boolean(credentials.claude), openai: Boolean(credentials.openai) },
        usage: { day: usage.day, requests: usage.requests, limit: config.maxDailyRequests },
      }));
      return;
    }
    const route = providerRoute(url.pathname);
    if (!route || !methodAllowed(req.method, route.provider, route.upstreamPath)) {
      res.writeHead(404, { "content-type": "application/json" });
      res.end(JSON.stringify({ error: "route not allowed" }));
      return;
    }
    const providerCredential = credentials[route.provider];
    if (!providerCredential) {
      res.writeHead(503, { "content-type": "application/json" });
      res.end(JSON.stringify({ error: `${route.provider} credential unavailable` }));
      return;
    }
    if (concurrent >= config.maxConcurrent) {
      res.writeHead(429, { "content-type": "application/json", "retry-after": "30" });
      res.end(JSON.stringify({ error: "credential proxy concurrency limit" }));
      return;
    }
    const usage = await readUsage(config.statePath);
    if (usage.requests >= config.maxDailyRequests) {
      res.writeHead(429, { "content-type": "application/json", "retry-after": "3600" });
      res.end(JSON.stringify({ error: "credential proxy daily request limit" }));
      return;
    }
    concurrent += 1;
    const started = Date.now();
    try {
      const body = req.method === "POST" ? await readBody(req, config.maxBodyBytes) : null;
      usage.requests += 1;
      usage.providers[route.provider] = (usage.providers[route.provider] || 0) + 1;
      await writeUsage(config.statePath, usage);
      const credential = { ...providerCredential, provider: route.provider };
      const upstreamBase = route.provider === "openai" ? config.openaiBase : config.anthropicBase;
      const upstream = await fetch(`${upstreamBase}${route.upstreamPath}${url.search}`, {
        method: req.method,
        headers: upstreamHeaders(req, credential),
        ...(body ? { body } : {}),
        signal: AbortSignal.timeout(15 * 60_000),
      });
      res.writeHead(upstream.status, safeResponseHeaders(upstream));
      if (upstream.body) for await (const chunk of upstream.body) res.write(chunk);
      res.end();
      console.error(`${new Date().toISOString()} ${route.provider} ${route.upstreamPath} ${upstream.status} ${Date.now() - started}ms`);
    } catch (error) {
      const status = Number(error.status) || 502;
      if (!res.headersSent) res.writeHead(status, { "content-type": "application/json" });
      res.end(JSON.stringify({ error: status === 413 ? error.message : "upstream request failed" }));
      console.error(`${new Date().toISOString()} ${route.provider} ${route.upstreamPath} ${status} ${Date.now() - started}ms`);
    } finally {
      concurrent -= 1;
    }
  });
}

async function main() {
  const config = proxyConfig();
  const credentials = await loadCredentials(config);
  if (process.argv.includes("--check")) {
    console.log(JSON.stringify({
      host: config.host,
      port: config.port,
      providers: { claude: Boolean(credentials.claude), openai: Boolean(credentials.openai) },
    }, null, 2));
    return;
  }
  const server = createCredentialProxy(config, credentials);
  server.listen(config.port, config.host, () => {
    console.error(`Mediascholar credential proxy on http://${config.host}:${config.port}`);
  });
}

const isMain = process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isMain) main().catch((error) => {
  console.error(`mediascholar-credential-proxy: ${error.message}`);
  process.exit(1);
});
