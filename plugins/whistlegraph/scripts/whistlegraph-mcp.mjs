#!/usr/bin/env node
// Whistlegraph Desk MCP — authenticated, path-scoped Git publishing.
// Node builtins only; JSON-RPC/MCP over newline-delimited stdio.

import { randomUUID, createHash } from "node:crypto";
import { execFile as execFileCallback } from "node:child_process";
import {
  chmodSync, existsSync, lstatSync, mkdirSync, readFileSync, realpathSync,
  writeFileSync,
} from "node:fs";
import { readFile, writeFile } from "node:fs/promises";
import { homedir } from "node:os";
import { dirname, join, resolve, sep } from "node:path";
import { promisify } from "node:util";
import * as readline from "node:readline";

const execFile = promisify(execFileCallback);
const SITE_PREFIX = "system/public/whistlegraph.org/";
const STATE_ROOT = join(homedir(), ".cache", "whistlegraph-desk");
const TOKEN_FILE = join(homedir(), ".ac-token");
const AUTH0_DOMAIN = "hi.aesthetic.computer";
const AUTH0_CLIENT_ID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
const API = process.env.WHISTLEGRAPH_API || "https://whistlegraph.org/api/whistlegraph-admin";
const MAX_FILE_BYTES = 2_000_000;
const MAX_DIFF_BYTES = 500_000;
const MAX_CHANGED_FILES = 25;
const UA = "Mozilla/5.0 Whistlegraph-Desk-Codex/0.1";

function output(value) {
  return typeof value === "string" ? value : JSON.stringify(value, null, 2);
}

async function command(program, args, options = {}) {
  try {
    const result = await execFile(program, args, {
      cwd: options.cwd,
      env: options.env || process.env,
      maxBuffer: options.maxBuffer || 4_000_000,
      timeout: options.timeout || 60_000,
      encoding: "utf8",
    });
    return { stdout: result.stdout || "", stderr: result.stderr || "" };
  } catch (error) {
    const detail = String(error.stderr || error.stdout || error.message || error).trim();
    throw new Error(`${program} ${args[0] || ""} failed${detail ? `: ${detail}` : ""}`);
  }
}

async function git(repo, ...args) {
  return command("git", ["-C", repo, ...args]);
}

async function loadTokens() {
  let tokens;
  try {
    tokens = JSON.parse(await readFile(TOKEN_FILE, "utf8"));
  } catch {
    throw new Error("Not signed in. Run `ac-login`, then try again.");
  }
  const stale = tokens.expires_at && Date.now() > tokens.expires_at - 60_000;
  if (stale) {
    if (!tokens.refresh_token) throw new Error("Your AC session expired. Run `ac-login` again.");
    const response = await fetch(`https://${AUTH0_DOMAIN}/oauth/token`, {
      method: "POST",
      headers: { "Content-Type": "application/json", "User-Agent": UA },
      body: JSON.stringify({
        grant_type: "refresh_token",
        client_id: AUTH0_CLIENT_ID,
        refresh_token: tokens.refresh_token,
      }),
      signal: AbortSignal.timeout(20_000),
    });
    if (!response.ok) throw new Error(`AC token refresh failed (HTTP ${response.status}). Run \`ac-login\`.`);
    const next = await response.json();
    tokens = {
      ...tokens,
      ...next,
      refresh_token: next.refresh_token || tokens.refresh_token,
      expires_at: Date.now() + (next.expires_in || 3600) * 1000,
    };
    await writeFile(TOKEN_FILE, `${JSON.stringify(tokens, null, 2)}\n`, { mode: 0o600 });
  }
  if (!tokens.access_token) throw new Error("AC session has no access token. Run `ac-login`.");
  return tokens;
}

async function session() {
  const tokens = await loadTokens();
  const response = await fetch(`${API}?action=session`, {
    headers: { Authorization: `Bearer ${tokens.access_token}`, "User-Agent": UA },
    cache: "no-store",
    signal: AbortSignal.timeout(20_000),
  });
  const body = await response.json().catch(() => ({}));
  if (!response.ok || !body.authorized) {
    throw new Error(body.message || `Whistlegraph Desk authorization failed (HTTP ${response.status}).`);
  }
  return { ...body, accessToken: tokens.access_token };
}

function statePath(id) {
  if (!/^[a-z0-9-]{8,80}$/i.test(String(id || ""))) throw new Error("Invalid change ID.");
  return join(STATE_ROOT, "changes", `${id}.json`);
}

function saveState(state) {
  mkdirSync(join(STATE_ROOT, "changes"), { recursive: true, mode: 0o700 });
  mkdirSync(join(STATE_ROOT, "worktrees"), { recursive: true, mode: 0o700 });
  const path = statePath(state.id);
  writeFileSync(path, `${JSON.stringify({ ...state, updatedAt: new Date().toISOString() }, null, 2)}\n`, { mode: 0o600 });
  chmodSync(path, 0o600);
}

function loadState(id) {
  const path = statePath(id);
  if (!existsSync(path)) throw new Error(`Unknown change: ${id}`);
  const state = JSON.parse(readFileSync(path, "utf8"));
  if (!state.repo || !state.worktree || state.id !== id) throw new Error("Change record is invalid.");
  return state;
}

function cleanSummary(value, fallback = "Update Whistlegraph frontend") {
  const text = String(value || fallback).replace(/[\r\n\t]+/g, " ").replace(/\s+/g, " ").trim();
  if (!text) return fallback;
  if (text.length > 120) throw new Error("Summary is too long (max 120 characters).");
  return text;
}

function slug(value) {
  return String(value || "maintainer").toLowerCase().replace(/^@/, "").replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "").slice(0, 32) || "maintainer";
}

async function resolveRepo(input) {
  const start = resolve(input || process.env.WHISTLEGRAPH_REPO || process.cwd());
  let top;
  try { top = (await git(start, "rev-parse", "--show-toplevel")).stdout.trim(); }
  catch { throw new Error("Open Codex inside an aesthetic-computer checkout, or pass repoPath."); }
  const repo = realpathSync(top);
  if (!existsSync(join(repo, SITE_PREFIX))) throw new Error("This checkout does not contain the Whistlegraph frontend.");
  const remote = (await git(repo, "remote", "get-url", "origin")).stdout.trim();
  if (!/(knot\.aesthetic\.computer[:/]aesthetic\.computer\/core|github\.com[/:]whistlegraph\/aesthetic-computer(?:\.git)?$)/.test(remote)) {
    throw new Error(`Unexpected origin remote: ${remote}`);
  }
  return { repo, remote };
}

function cleanRelativePath(value) {
  const path = String(value || "").replaceAll("\\", "/").replace(/^\.\//, "");
  if (!path.startsWith(SITE_PREFIX) || /[\u0000-\u001f\u007f]/.test(path) || path.split("/").includes("..")) {
    throw new Error(`Path must stay under ${SITE_PREFIX}`);
  }
  if (path === SITE_PREFIX || path.endsWith("/")) throw new Error("Path must name a file.");
  return path;
}

function safeFile(worktree, input, { mustExist = false } = {}) {
  const rel = cleanRelativePath(input);
  const root = resolve(worktree, SITE_PREFIX);
  const path = resolve(worktree, rel);
  if (path !== root && !path.startsWith(`${root}${sep}`)) throw new Error("Path escaped the Whistlegraph frontend.");
  if (existsSync(root) && lstatSync(root).isSymbolicLink()) throw new Error("The Whistlegraph frontend root may not be a symlink.");
  let cursor = path;
  while (cursor.startsWith(root) && cursor !== root) {
    if (existsSync(cursor) && lstatSync(cursor).isSymbolicLink()) throw new Error("Symlinks are not editable through Whistlegraph Desk.");
    cursor = dirname(cursor);
  }
  if (mustExist && !existsSync(path)) throw new Error(`File not found: ${rel}`);
  return { rel, path };
}

async function changedFiles(state) {
  const tracked = (await git(state.worktree, "diff", "--name-only", "--diff-filter=ACMRTUXB", "HEAD", "--", SITE_PREFIX)).stdout.split("\n");
  const untracked = (await git(state.worktree, "ls-files", "--others", "--exclude-standard", "--", SITE_PREFIX)).stdout.split("\n");
  return [...new Set([...tracked, ...untracked].map((x) => x.trim()).filter(Boolean))];
}

async function validate(state) {
  const errors = [];
  const warnings = [];
  const files = await changedFiles(state);
  if (!files.length) errors.push("No frontend files have changed.");
  if (files.length > MAX_CHANGED_FILES) errors.push(`Too many changed files (${files.length}; max ${MAX_CHANGED_FILES}).`);
  for (const rel of files) {
    try {
      const target = safeFile(state.worktree, rel, { mustExist: true });
      const stat = lstatSync(target.path);
      if (!stat.isFile()) errors.push(`${rel} is not a regular file.`);
      if (stat.size > MAX_FILE_BYTES) errors.push(`${rel} is too large (${stat.size} bytes).`);
      if (rel.endsWith(".json")) {
        try { JSON.parse(readFileSync(target.path, "utf8")); }
        catch (error) { errors.push(`${rel}: invalid JSON (${error.message}).`); }
      }
      if (rel.endsWith(".html")) {
        const text = readFileSync(target.path, "utf8");
        if (!/^<!doctype html>/i.test(text.trimStart())) warnings.push(`${rel}: missing <!doctype html>.`);
        if (!text.includes("</html>")) errors.push(`${rel}: missing closing </html>.`);
      }
    } catch (error) { errors.push(`${rel}: ${error.message}`); }
  }
  let diff = "";
  try {
    diff = (await git(state.worktree, "diff", "--no-ext-diff", "--binary", "HEAD", "--", SITE_PREFIX)).stdout;
    const untracked = files.filter((file) => !diff.includes(`b/${file}`));
    for (const file of untracked) {
      const target = safeFile(state.worktree, file, { mustExist: true });
      const text = readFileSync(target.path, "utf8");
      diff += `\n--- /dev/null\n+++ b/${file}\n@@ new file @@\n${text}`;
    }
  } catch (error) { errors.push(error.message); }
  if (Buffer.byteLength(diff) > MAX_DIFF_BYTES) errors.push(`Diff is too large (${Buffer.byteLength(diff)} bytes; max ${MAX_DIFF_BYTES}).`);
  try { await git(state.worktree, "diff", "--check", "HEAD", "--", SITE_PREFIX); }
  catch (error) { errors.push(error.message); }
  return { ok: errors.length === 0, files, errors, warnings, diffBytes: Buffer.byteLength(diff) };
}

async function beginChange(args) {
  const actor = await session();
  const { repo, remote } = await resolveRepo(args.repoPath);
  await git(repo, "fetch", "origin", "main", "--quiet");
  const base = (await git(repo, "rev-parse", "origin/main")).stdout.trim();
  const id = `${Date.now().toString(36)}-${randomUUID().slice(0, 8)}`;
  const actorName = slug(actor.handle || actor.sub);
  const branch = `whistlegraph/${actorName}/${id}`;
  const worktree = join(STATE_ROOT, "worktrees", createHash("sha256").update(repo).digest("hex").slice(0, 12), id);
  mkdirSync(dirname(worktree), { recursive: true, mode: 0o700 });
  await git(repo, "worktree", "add", "-b", branch, worktree, base);
  const state = {
    id, repo, remote, worktree, branch, base,
    summary: cleanSummary(args.summary), actorSub: actor.sub, actorHandle: actor.handle || null,
    stage: "draft", createdAt: new Date().toISOString(),
  };
  saveState(state);
  return { changeId: id, branch, base, summary: state.summary, actor: actor.handle ? `@${actor.handle}` : actor.sub, worktree };
}

async function listFrontend(args) {
  await session();
  const state = loadState(args.changeId);
  const files = (await git(state.worktree, "ls-files", SITE_PREFIX)).stdout.split("\n").filter(Boolean);
  const query = String(args.query || "").toLowerCase();
  return { changeId: state.id, files: files.filter((file) => !query || file.toLowerCase().includes(query)).slice(0, 500) };
}

async function readFrontend(args) {
  await session();
  const state = loadState(args.changeId);
  const target = safeFile(state.worktree, args.path, { mustExist: true });
  const bytes = readFileSync(target.path);
  if (bytes.includes(0)) throw new Error("Binary files cannot be read through this tool.");
  const text = bytes.toString("utf8");
  const offset = Math.max(0, Number(args.offset) || 0);
  const limit = Math.min(100_000, Math.max(1, Number(args.limit) || 40_000));
  return { path: target.rel, offset, totalCharacters: text.length, content: text.slice(offset, offset + limit), truncated: offset + limit < text.length };
}

async function replaceFrontend(args) {
  await session();
  const state = loadState(args.changeId);
  if (state.stage !== "draft") throw new Error(`Change is already ${state.stage}; it is no longer editable.`);
  const target = safeFile(state.worktree, args.path, { mustExist: true });
  const before = readFileSync(target.path, "utf8");
  const find = String(args.find ?? "");
  const replacement = String(args.replacement ?? "");
  if (!find) throw new Error("find cannot be empty.");
  const count = before.split(find).length - 1;
  if (!count) throw new Error("The exact text was not found; read the current file and try again.");
  if (!args.all && count !== 1) throw new Error(`The text occurs ${count} times. Supply more context or set all=true intentionally.`);
  const after = args.all ? before.split(find).join(replacement) : before.replace(find, replacement);
  if (Buffer.byteLength(after) > MAX_FILE_BYTES) throw new Error("Resulting file is too large.");
  writeFileSync(target.path, after);
  return { changed: true, path: target.rel, replacements: args.all ? count : 1, bytes: Buffer.byteLength(after) };
}

async function writeFrontend(args) {
  await session();
  const state = loadState(args.changeId);
  if (state.stage !== "draft") throw new Error(`Change is already ${state.stage}; it is no longer editable.`);
  const target = safeFile(state.worktree, args.path);
  const created = !existsSync(target.path);
  const content = String(args.content ?? "");
  if (Buffer.byteLength(content) > MAX_FILE_BYTES) throw new Error("File is too large.");
  mkdirSync(dirname(target.path), { recursive: true });
  writeFileSync(target.path, content);
  return { changed: true, path: target.rel, bytes: Buffer.byteLength(content), created };
}

async function diffChange(args) {
  await session();
  const state = loadState(args.changeId);
  const files = await changedFiles(state);
  const diff = (await git(state.worktree, "diff", "--no-ext-diff", "--stat", "HEAD", "--", SITE_PREFIX)).stdout;
  const patch = (await git(state.worktree, "diff", "--no-ext-diff", "--unified=3", "HEAD", "--", SITE_PREFIX)).stdout;
  const limit = Math.min(200_000, Math.max(1, Number(args.limit) || 80_000));
  return { changeId: state.id, stage: state.stage, files, stat: diff, patch: patch.slice(0, limit), truncated: patch.length > limit };
}

async function validateChange(args) {
  await session();
  const state = loadState(args.changeId);
  const result = await validate(state);
  state.validation = { ...result, at: new Date().toISOString() };
  saveState(state);
  return { changeId: state.id, ...result };
}

async function triggerDeploy(state, actor) {
  const response = await fetch(`${API}?action=deploy`, {
    method: "POST",
    headers: { Authorization: `Bearer ${actor.accessToken}`, "Content-Type": "application/json", "User-Agent": UA },
    body: JSON.stringify({ commit: state.commit, changeId: state.id, branch: state.branch }),
    signal: AbortSignal.timeout(30_000),
  });
  const body = await response.json().catch(() => ({}));
  if (!response.ok) throw new Error(body.message || `Deploy request failed (HTTP ${response.status}).`);
  state.stage = "deploying";
  state.deployment = body;
  saveState(state);
  return body;
}

async function publishChange(args) {
  const actor = await session();
  const state = loadState(args.changeId);
  if (state.actorSub !== actor.sub) throw new Error("A change may only be published by the maintainer who began it.");
  if (!/knot\.aesthetic\.computer[:/]aesthetic\.computer\/core/.test(state.remote)) {
    throw new Error("Publishing requires the Tangled knot as `origin`. Set origin to git@knot.aesthetic.computer:aesthetic.computer/core and retry.");
  }
  if (state.stage === "deploying" || state.stage === "deployed") return { changeId: state.id, stage: state.stage, commit: state.commit, deployment: state.deployment };

  if (!state.commit) {
    const checks = await validate(state);
    if (!checks.ok) throw new Error(`Validation failed:\n${checks.errors.join("\n")}`);
    await git(state.repo, "fetch", "origin", "main", "--quiet");
    const current = (await git(state.repo, "rev-parse", "origin/main")).stdout.trim();
    if (current !== state.base) throw new Error(`main moved from ${state.base.slice(0, 12)} to ${current.slice(0, 12)}. Begin a fresh change and reapply this patch; nothing was pushed.`);
    const message = cleanSummary(args.message || state.summary);
    await git(state.worktree, "add", "--", SITE_PREFIX);
    const author = `${actor.handle ? `@${actor.handle}` : "Whistlegraph maintainer"} via Whistlegraph Desk`;
    await git(state.worktree,
      "-c", `user.name=${author}`,
      "-c", "user.email=desk@whistlegraph.org",
      "-c", "commit.gpgsign=false",
      "commit", "-m", message,
      "-m", `Whistlegraph-Actor-Sub: ${actor.sub}\nWhistlegraph-Actor-Handle: ${actor.handle ? `@${actor.handle}` : "unknown"}\nWhistlegraph-Change: ${state.id}`,
    );
    state.commit = (await git(state.worktree, "rev-parse", "HEAD")).stdout.trim();
    state.message = message;
    state.stage = "committed";
    saveState(state);
  }

  if (state.stage === "committed") {
    await git(state.worktree, "push", "-u", "origin", `${state.commit}:refs/heads/${state.branch}`);
    state.stage = "branch-pushed";
    saveState(state);
  }

  if (state.stage === "branch-pushed") {
    await git(state.repo, "fetch", "origin", "main", "--quiet");
    const current = (await git(state.repo, "rev-parse", "origin/main")).stdout.trim();
    if (current !== state.base) throw new Error(`main moved before publish (${current.slice(0, 12)}). The review branch is safe at ${state.branch}; main was not changed.`);
    await git(state.worktree, "push", "origin", `${state.commit}:refs/heads/main`);
    state.stage = "main-pushed";
    saveState(state);
  }

  const deployment = await triggerDeploy(state, actor);
  return { changeId: state.id, stage: state.stage, branch: state.branch, commit: state.commit, deployment };
}

async function changeStatus(args) {
  const actor = await session();
  const state = loadState(args.changeId);
  return {
    changeId: state.id, summary: state.summary, stage: state.stage, branch: state.branch,
    base: state.base, commit: state.commit || null, actor: state.actorHandle ? `@${state.actorHandle}` : state.actorSub,
    currentActor: actor.handle ? `@${actor.handle}` : actor.sub,
    validation: state.validation || null, deployment: state.deployment || null,
  };
}

const TOOLS = [
  { name: "whistlegraph_whoami", description: "Verify the current Aesthetic Computer token and immutable Auth0-sub maintainer access.", inputSchema: { type: "object", properties: {} } },
  { name: "whistlegraph_begin_change", description: "Create an isolated Whistlegraph frontend worktree and review branch from current origin/main. Does not alter the user's working tree.", inputSchema: { type: "object", properties: { summary: { type: "string" }, repoPath: { type: "string", description: "Path inside an aesthetic-computer checkout; defaults to Codex's current directory." } }, required: ["summary"] } },
  { name: "whistlegraph_list_frontend", description: "List tracked files available to a Whistlegraph change.", inputSchema: { type: "object", properties: { changeId: { type: "string" }, query: { type: "string" } }, required: ["changeId"] } },
  { name: "whistlegraph_read_frontend", description: "Read a bounded text slice from an allowed Whistlegraph frontend file.", inputSchema: { type: "object", properties: { changeId: { type: "string" }, path: { type: "string" }, offset: { type: "integer" }, limit: { type: "integer" } }, required: ["changeId", "path"] } },
  { name: "whistlegraph_replace_frontend", description: "Replace exact text inside an allowed Whistlegraph frontend file in the isolated worktree.", inputSchema: { type: "object", properties: { changeId: { type: "string" }, path: { type: "string" }, find: { type: "string" }, replacement: { type: "string" }, all: { type: "boolean" } }, required: ["changeId", "path", "find", "replacement"] } },
  { name: "whistlegraph_write_frontend", description: "Write a complete text file under system/public/whistlegraph.org in the isolated worktree. Cannot write outside that directory or through symlinks.", inputSchema: { type: "object", properties: { changeId: { type: "string" }, path: { type: "string" }, content: { type: "string" } }, required: ["changeId", "path", "content"] } },
  { name: "whistlegraph_diff_change", description: "Show the changed file list, diff stat, and bounded patch for review.", inputSchema: { type: "object", properties: { changeId: { type: "string" }, limit: { type: "integer" } }, required: ["changeId"] } },
  { name: "whistlegraph_validate_change", description: "Run path, file-size, JSON, HTML, and Git whitespace checks. Publishing refuses failed validation.", inputSchema: { type: "object", properties: { changeId: { type: "string" } }, required: ["changeId"] } },
  { name: "whistlegraph_publish_change", description: "DESTRUCTIVE EXTERNAL ACTION: create an attributed commit, push its review branch, fast-forward main if unchanged, and request deployment of the exact pushed SHA. Call only after explicit user approval.", inputSchema: { type: "object", properties: { changeId: { type: "string" }, message: { type: "string" } }, required: ["changeId"] } },
  { name: "whistlegraph_change_status", description: "Read the local validation, Git, push, and deployment state for a change.", inputSchema: { type: "object", properties: { changeId: { type: "string" } }, required: ["changeId"] } },
];

async function callTool(name, args = {}) {
  if (name === "whistlegraph_whoami") {
    const actor = await session();
    return { authorized: true, sub: actor.sub, handle: actor.handle || null, role: "maintainer", api: API };
  }
  if (name === "whistlegraph_begin_change") return beginChange(args);
  if (name === "whistlegraph_list_frontend") return listFrontend(args);
  if (name === "whistlegraph_read_frontend") return readFrontend(args);
  if (name === "whistlegraph_replace_frontend") return replaceFrontend(args);
  if (name === "whistlegraph_write_frontend") return writeFrontend(args);
  if (name === "whistlegraph_diff_change") return diffChange(args);
  if (name === "whistlegraph_validate_change") return validateChange(args);
  if (name === "whistlegraph_publish_change") return publishChange(args);
  if (name === "whistlegraph_change_status") return changeStatus(args);
  throw new Error(`Unknown tool: ${name}`);
}

async function handle(message) {
  const { id, method, params } = message;
  try {
    if (method === "initialize") return { jsonrpc: "2.0", id, result: { protocolVersion: "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "whistlegraph", version: "0.1.0" } } };
    if (method === "initialized" || method === "notifications/initialized") return null;
    if (method === "ping") return { jsonrpc: "2.0", id, result: {} };
    if (method === "tools/list") return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
    if (method === "tools/call") {
      const value = await callTool(params?.name, params?.arguments || {});
      return { jsonrpc: "2.0", id, result: { content: [{ type: "text", text: output(value) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
  } catch (error) {
    return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
  }
}

const rl = readline.createInterface({ input: process.stdin, terminal: false });
rl.on("line", async (line) => {
  if (!line.trim()) return;
  try {
    const response = await handle(JSON.parse(line));
    if (response) process.stdout.write(`${JSON.stringify(response)}\n`);
  } catch (error) {
    process.stdout.write(`${JSON.stringify({ jsonrpc: "2.0", id: null, error: { code: -32700, message: String(error.message || error) } })}\n`);
  }
});
