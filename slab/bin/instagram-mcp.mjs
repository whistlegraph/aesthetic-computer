#!/usr/bin/env node
// instagram-mcp.mjs — discoverable MCP front door for AC's official
// multi-account Instagram CLI and Oskiewar Reel factory. Secret values never
// enter MCP responses; toolchain/instagram/ig.mjs resolves them from the vault.

import { spawn } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { resolve } from "node:path";
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";

const root = resolve(import.meta.dirname, "../..");
const ig = resolve(root, "toolchain/instagram/ig.mjs");
const reel = resolve(root, "xbox/live/marketing/reel.mjs");
const accounts = { oskiewar: "OSKIEWAR", whistlegraph: "WHISTLEGRAPH",
  aesthetic: "AESTHETIC" };
const text = (value) => [{ type: "text", text: String(value) }];

function vaultPath(account) {
  return resolve(root, "vault", account, "instagram.env");
}
function provisioned(account) {
  const prefix = accounts[account];
  const path = vaultPath(account);
  const source = existsSync(path) ? readFileSync(path, "utf8") : "";
  const has = (key) => Boolean(process.env[key]) ||
    new RegExp(`^${key}=.+$`, "m").test(source);
  return { account: `@${account}`, host: process.env.HOSTNAME || "local",
    vaultPath: path, vaultPresent: existsSync(path),
    userIdPresent: has(`${prefix}_IG_USER_ID`),
    tokenPresent: has(`${prefix}_IG_TOKEN`),
    ready: has(`${prefix}_IG_USER_ID`) && has(`${prefix}_IG_TOKEN`) };
}
function account(value = "oskiewar") {
  if (!accounts[value]) throw new Error(`unknown account ${value}; use ${Object.keys(accounts).join(", ")}`);
  return value;
}
function run(script, args, timeout = 420_000) {
  return new Promise((done, reject) => {
    const child = spawn(process.execPath, [script, ...args], { cwd: root,
      env: process.env, stdio: ["ignore", "pipe", "pipe"] });
    let output = "";
    const append = (chunk) => { output = (output + chunk).slice(-24_000); };
    child.stdout.on("data", append); child.stderr.on("data", append);
    const timer = setTimeout(() => { child.kill("SIGTERM"); reject(new Error("Instagram operation timed out")); }, timeout);
    child.on("error", reject);
    child.on("close", (code) => {
      clearTimeout(timer);
      if (code) reject(new Error(output.trim() || `Instagram command exited ${code}`));
      else done(output.trim());
    });
  });
}

const TOOLS = [
  { name: "instagram_accounts", description: "List AC Instagram aliases and whether their vault-backed user ID/token resolve on this host. Never returns secret values.", inputSchema: { type: "object", properties: {} } },
  { name: "instagram_me", description: "Verify an official Instagram Graph account token and return public account statistics.", inputSchema: { type: "object", properties: { account: { type: "string", enum: Object.keys(accounts), default: "oskiewar" } } } },
  { name: "instagram_quota", description: "Read an account's official 24-hour Instagram content publishing quota.", inputSchema: { type: "object", properties: { account: { type: "string", enum: Object.keys(accounts), default: "oskiewar" } } } },
  { name: "instagram_insights", description: "Read official Reel insights for one media ID.", inputSchema: { type: "object", properties: { account: { type: "string", enum: Object.keys(accounts), default: "oskiewar" }, mediaId: { type: "string" } }, required: ["mediaId"] } },
  { name: "instagram_refresh", description: "Refresh and persist an account's long-lived token in its vault file. SIDE EFFECT: rewrites only the token line. Requires confirm:true.", inputSchema: { type: "object", properties: { account: { type: "string", enum: Object.keys(accounts), default: "oskiewar" }, confirm: { type: "boolean" } }, required: ["confirm"] } },
  { name: "instagram_post_reel", description: "Publish a verified local MP4 through the official Instagram Graph API: public Spaces upload, container poll, publish, and receipt. SIDE EFFECT: creates a live Instagram Reel. Requires confirm:true.", inputSchema: { type: "object", properties: { account: { type: "string", enum: Object.keys(accounts), default: "oskiewar" }, videoPath: { type: "string" }, coverPath: { type: "string" }, caption: { type: "string", maxLength: 2200 }, confirm: { type: "boolean" } }, required: ["videoPath", "caption", "confirm"] } },
  { name: "oskiewar_reel_queue", description: "List staged Oskiewar Reel artifacts and their verification state.", inputSchema: { type: "object", properties: {} } },
  { name: "oskiewar_reel_render", description: "Render and verify an Oskiewar Reel into tmp/oskiewar-reels/queue. This may take several minutes but does not publish.", inputSchema: { type: "object", properties: { day: { type: "string", description: "YYYY-MM-DD" }, index: { type: "integer", minimum: 0 }, segment: { type: "string" }, seconds: { type: "number", minimum: 8, maximum: 120 } } } },
  { name: "oskiewar_reel_publish", description: "Run the Oskiewar queue publisher. live:false writes/returns the dry-run payload; live:true uploads and posts. SIDE EFFECT when live:true. Requires confirm:true for live publication.", inputSchema: { type: "object", properties: { id: { type: "string" }, live: { type: "boolean", default: false }, confirm: { type: "boolean" } }, required: ["id"] } },
];

async function callTool(name, args = {}) {
  const as = account(args.account);
  if (name === "instagram_accounts") return text(JSON.stringify(Object.keys(accounts).map(provisioned), null, 2));
  if (name === "instagram_me") return text(await run(ig, ["--as", as, "me"]));
  if (name === "instagram_quota") return text(await run(ig, ["--as", as, "quota"]));
  if (name === "instagram_insights") return text(await run(ig, ["--as", as, "insights", args.mediaId]));
  if (name === "instagram_refresh") {
    if (args.confirm !== true) throw new Error("confirm:true is required to rewrite the vault token");
    return text(await run(ig, ["--as", as, "refresh"]));
  }
  if (name === "instagram_post_reel") {
    if (args.confirm !== true) throw new Error("confirm:true is required to publish a live Reel");
    const argv = ["--as", as, "post", resolve(args.videoPath), "--caption", args.caption];
    if (args.coverPath) argv.push("--cover", resolve(args.coverPath));
    return text(await run(ig, argv));
  }
  if (name === "oskiewar_reel_queue") return text(await run(reel, ["--queue"]));
  if (name === "oskiewar_reel_render") {
    const argv = [];
    if (args.day) argv.push("--day", args.day);
    if (args.index !== undefined) argv.push("--index", String(args.index));
    if (args.segment) argv.push("--segment", args.segment);
    if (args.seconds) argv.push("--seconds", String(args.seconds));
    return text(await run(reel, argv, 900_000));
  }
  if (name === "oskiewar_reel_publish") {
    if (args.live && args.confirm !== true) throw new Error("confirm:true is required for live publication");
    return text(await run(reel, ["--publish", args.id, ...(args.live ? ["--live"] : [])], 600_000));
  }
  throw new Error(`unknown tool ${name}`);
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    if (method === "initialize") return { jsonrpc: "2.0", id, result: {
      protocolVersion: "2024-11-05", capabilities: { tools: {} },
      serverInfo: { name: "instagram-mcp", version: "1.0.0" },
      instructions: "Official Instagram Graph API only. Credentials resolve from <aesthetic-computer>/vault/<account>/instagram.env and are never returned." } };
    if (method === "initialized" || method === "notifications/initialized") return null;
    if (method === "ping") return { jsonrpc: "2.0", id, result: {} };
    if (method === "tools/list") return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
    if (method === "tools/call") return { jsonrpc: "2.0", id,
      result: { content: await callTool(params?.name, params?.arguments) } };
    return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
  } catch (error) {
    return { jsonrpc: "2.0", id, result: { isError: true,
      content: text(error.message || error) } };
  }
}

const port = httpPort(process.argv, 7780);
if (port) serveHttp({ handleMessage, port, banner: "📸 instagram-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "📸 instagram-mcp started" });
