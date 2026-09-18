#!/usr/bin/env node
// x-mcp.mjs — discoverable MCP front door for AC's X (twitter) CLI, and for
// the Bluesky poster it mirrors. Secret values never enter MCP responses;
// toolchain/x/x.mjs and at/cli.mjs resolve them from the vault.
//
// The two platforms sit behind one server on purpose: an AC drop goes to both
// or neither, and keeping them together means one `post` habit, not two.

import { spawn } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";

const root = resolve(import.meta.dirname, "../..");
const x = resolve(root, "toolchain/x/x.mjs");
const at = resolve(root, "at/cli.mjs");
const accounts = { promptdotac: "PROMPTDOTAC", whistlegraph: "WHISTLEGRAPH" };
const text = (value) => [{ type: "text", text: String(value) }];

function account(value = "promptdotac") {
  if (!accounts[value]) {
    throw new Error(`unknown account ${value}; use ${Object.keys(accounts).join(", ")}`);
  }
  return value;
}

// Report readiness without ever reading a value back out.
function provisioned(name) {
  const path = resolve(root, "vault", name, "x.env");
  const source = existsSync(path) ? readFileSync(path, "utf8") : "";
  const has = (suffix) => {
    const key = `${accounts[name]}_X_${suffix}`;
    return Boolean(process.env[key]) || new RegExp(`^${key}=.+$`, "m").test(source);
  };
  const keys = ["API_KEY", "API_SECRET", "ACCESS_TOKEN", "ACCESS_SECRET"];
  return { account: `@${name}`, vaultPath: path, vaultPresent: existsSync(path),
    present: keys.filter(has), missing: keys.filter((k) => !has(k)),
    ready: keys.every(has) };
}

function run(script, args, cwd = root, timeout = 120_000) {
  return new Promise((done, reject) => {
    const child = spawn(process.execPath, [script, ...args], { cwd,
      env: process.env, stdio: ["ignore", "pipe", "pipe"] });
    let output = "";
    const append = (chunk) => { output = (output + chunk).slice(-24_000); };
    child.stdout.on("data", append); child.stderr.on("data", append);
    const timer = setTimeout(() => { child.kill("SIGTERM"); reject(new Error("operation timed out")); }, timeout);
    child.on("error", reject);
    child.on("close", (code) => {
      clearTimeout(timer);
      if (code) reject(new Error(output.trim() || `command exited ${code}`));
      else done(output.trim());
    });
  });
}

const TOOLS = [
  { name: "x_accounts", description: "List AC X aliases and whether their vault-backed OAuth 1.0a credentials resolve on this host. Never returns secret values.", inputSchema: { type: "object", properties: {} } },
  { name: "x_me", description: "Verify an X account's credentials and return its public follower/post counts.", inputSchema: { type: "object", properties: { account: { type: "string", enum: Object.keys(accounts), default: "promptdotac" } } } },
  { name: "x_search", description: "Search X posts from the last seven days through the official v2 API. Pay-per-use: a local daily estimate blocks searches beyond the configured budget; the X Console Billing Cycle Cap remains authoritative.", inputSchema: { type: "object", properties: { account: { type: "string", enum: Object.keys(accounts), default: "promptdotac" }, query: { type: "string", maxLength: 512 }, maxResults: { type: "integer", minimum: 10, maximum: 100, default: 10 } }, required: ["query"] } },
  { name: "x_budget", description: "Show today's local X search budget estimate without making an API request.", inputSchema: { type: "object", properties: { account: { type: "string", enum: Object.keys(accounts), default: "promptdotac" } } } },
  { name: "x_draft", description: "Weigh a post or reply against X's 280-character limit and show exactly what would be sent. Posts nothing.", inputSchema: { type: "object", properties: { account: { type: "string", enum: Object.keys(accounts), default: "promptdotac" }, text: { type: "string" }, replyTo: { type: "string", description: "Optional X post id or x.com status URL." }, mediaPath: { type: "string" }, alt: { type: "string" } }, required: ["text"] } },
  { name: "x_post", description: "Publish a post to X through the official v2 API. SIDE EFFECT: creates a live public post. Requires confirm:true. Always run x_draft first.", inputSchema: { type: "object", properties: { account: { type: "string", enum: Object.keys(accounts), default: "promptdotac" }, text: { type: "string" }, mediaPath: { type: "string", description: "Local image path; requires alt." }, alt: { type: "string", description: "Alt text, required whenever mediaPath is given." }, confirm: { type: "boolean" } }, required: ["text", "confirm"] } },
  { name: "x_reply", description: "Publish a public reply when the original author has mentioned or quoted this account. X self-serve API restriction: other replies must be made manually in X. SIDE EFFECT: creates a live public post. Requires confirm:true.", inputSchema: { type: "object", properties: { account: { type: "string", enum: Object.keys(accounts), default: "promptdotac" }, target: { type: "string", description: "X post id or x.com status URL." }, text: { type: "string" }, mediaPath: { type: "string", description: "Local image path; requires alt." }, alt: { type: "string", description: "Alt text, required whenever mediaPath is given." }, confirm: { type: "boolean" } }, required: ["target", "text", "confirm"] } },
  { name: "bluesky_profile", description: "Read a Bluesky profile's public counts (default @aesthetic.computer).", inputSchema: { type: "object", properties: { handle: { type: "string", default: "aesthetic.computer" } } } },
  { name: "bluesky_post", description: "Publish a post to Bluesky as @aesthetic.computer, with link facets detected so URLs are clickable. SIDE EFFECT: creates a live public post. Requires confirm:true.", inputSchema: { type: "object", properties: { text: { type: "string" }, imagePath: { type: "string" }, alt: { type: "string" }, confirm: { type: "boolean" } }, required: ["text", "confirm"] } },
];

async function callTool(name, args = {}) {
  if (name === "x_accounts") {
    return text(JSON.stringify(Object.keys(accounts).map(provisioned), null, 2));
  }
  if (name === "x_me") return text(await run(x, ["--as", account(args.account), "me"]));
  if (name === "x_search") {
    const argv = ["--as", account(args.account), "search", args.query];
    if (args.maxResults !== undefined) argv.push("--max-results", String(args.maxResults));
    return text(await run(x, argv));
  }
  if (name === "x_budget") return text(await run(x, ["--as", account(args.account), "budget"]));
  if (name === "x_draft") {
    const argv = ["--as", account(args.account), "post", args.text, "--dry-run"];
    if (args.replyTo) argv.push("--reply-to", args.replyTo);
    if (args.mediaPath) argv.push("--media", args.mediaPath, "--alt", args.alt || "");
    return text(await run(x, argv));
  }
  if (name === "x_post") {
    if (args.confirm !== true) throw new Error("confirm:true is required to post publicly to X");
    const argv = ["--as", account(args.account), "post", args.text];
    if (args.mediaPath) argv.push("--media", args.mediaPath, "--alt", args.alt || "");
    return text(await run(x, argv, root, 300_000));
  }
  if (name === "x_reply") {
    if (args.confirm !== true) throw new Error("confirm:true is required to reply publicly on X");
    const argv = ["--as", account(args.account), "reply", args.target, args.text];
    if (args.mediaPath) argv.push("--media", args.mediaPath, "--alt", args.alt || "");
    return text(await run(x, argv, root, 300_000));
  }
  if (name === "bluesky_profile") {
    return text(await run(at, ["profile", args.handle || "aesthetic.computer"], resolve(root, "at")));
  }
  if (name === "bluesky_post") {
    if (args.confirm !== true) throw new Error("confirm:true is required to post publicly to Bluesky");
    const argv = ["post", args.text];
    if (args.imagePath) argv.push(`--image=${args.imagePath}`, `--alt=${args.alt || ""}`);
    // at/cli.mjs loads its .env from the working directory, so run it there.
    return text(await run(at, argv, resolve(root, "at"), 300_000));
  }
  throw new Error(`unknown tool ${name}`);
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    if (method === "initialize") return { jsonrpc: "2.0", id, result: {
      protocolVersion: "2024-11-05", capabilities: { tools: {} },
      serverInfo: { name: "x-mcp", version: "1.1.0" },
      instructions: "Official X API v2 (OAuth 1.0a) and Bluesky. Credentials resolve from <aesthetic-computer>/vault/<account>/x.env and vault/at/.env and are never returned. X search is pay-per-use and locally budgeted. Draft before posting; public post/reply tools require confirm:true." } };
    if (method === "initialized" || method === "notifications/initialized") return null;
    if (method === "ping") return { jsonrpc: "2.0", id, result: {} };
    if (method === "tools/list") return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
    if (method === "tools/call") return { jsonrpc: "2.0", id,
      result: { content: await callTool(params?.name, params?.arguments) } };
    return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
  } catch (error) {
    return { jsonrpc: "2.0", id, result: { isError: true, content: text(error.message || error) } };
  }
}

const port = httpPort(process.argv, 7784);
if (port) serveHttp({ handleMessage, port, banner: "✖️ x-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "✖️ x-mcp started" });
