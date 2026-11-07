#!/usr/bin/env node
// x-mcp.mjs — discoverable MCP front door for AC's X (twitter) CLI, and for
// the Bluesky poster it mirrors. Secret values never enter MCP responses;
// toolchain/x/x.mjs and at/cli.mjs resolve them from the vault.
//
// The two platforms sit behind one server on purpose: an AC drop goes to both
// or neither, and keeping them together means one `post` habit, not two.

import { execFile, spawn } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { resolve } from "node:path";
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";
import { credentialStatus, X_ACCOUNTS } from "../../toolchain/vault/credentials.mjs";

const root = resolve(import.meta.dirname, "../..");
const x = resolve(root, "toolchain/x/x.mjs");
const at = resolve(root, "at/cli.mjs");
const accounts = X_ACCOUNTS;
const text = (value) => [{ type: "text", text: String(value) }];

function account(value = "promptdotac") {
  if (!Object.hasOwn(accounts, value)) {
    throw new Error(`unknown account ${value}; use ${Object.keys(accounts).join(", ")}`);
  }
  return value;
}

// Report readiness without ever reading a value back out.
function provisioned(name) {
  const status = credentialStatus(`x:${name}`);
  const has = suffix => status.present?.includes(`${accounts[name]}_X_${suffix}`);
  const keys = ["API_KEY", "API_SECRET", "ACCESS_TOKEN", "ACCESS_SECRET"];
  return { account: `@${name}`, ...status,
    present: keys.filter(has), missing: keys.filter((k) => !has(k)),
    ready: status.state === "ready" };
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

// ---- social drafts (queue written nightly by social/bin/draft-updates.mjs on jasellite) ----
const QUEUE_REMOTE = "~/.local/share/ac/social/queue.json";
const QUEUE_LOCAL = resolve(homedir(), ".local/share/ac/social/queue.json");
const STREAMS = ["daily", "deadlines", "papers", "pop", "reel", "note"];

// jasellite's login shell is fish, so scripts go through `bash -s` on stdin.
function sshScript(script, timeout = 20_000) {
  return new Promise((done, reject) => {
    const child = execFile("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=8", "jasellite", "bash", "-s"],
      { timeout, maxBuffer: 4 << 20 }, (error, stdout, stderr) => error ? reject(new Error((stderr || error.message).trim().slice(0, 400))) : done(stdout));
    child.stdin.end(script);
  });
}

async function readQueue() {
  try { return { where: "jasellite", queue: JSON.parse(await sshScript(`cat ${QUEUE_REMOTE} 2>/dev/null || echo "[]"`)) }; }
  catch (error) {
    if (existsSync(QUEUE_LOCAL)) return { where: "local", queue: JSON.parse(readFileSync(QUEUE_LOCAL, "utf8")), note: `jasellite unreachable (${error.message}); read the local copy` };
    throw new Error(`no drafts queue: jasellite unreachable (${error.message}) and no local copy at ${QUEUE_LOCAL}`);
  }
}

// Apply a mutation to the queue on jasellite atomically (node rewrites the file in place).
async function mutateQueue(mutation) {
  const script = `set -e; mkdir -p ~/.local/share/ac/social; f=~/.local/share/ac/social/queue.json; [ -f "$f" ] || echo "[]" > "$f"
node -e 'const fs=require("fs");const f=process.argv[1];const q=JSON.parse(fs.readFileSync(f,"utf8"));const m=JSON.parse(process.argv[2]);
const out=(${mutation.toString()})(q,m);fs.writeFileSync(f,JSON.stringify(q,null,2)+"\\n");console.log(JSON.stringify(out));' "$f" '${JSON.stringify(mutation.args).replace(/'/g, "'\\''")}'`;
  return JSON.parse(await sshScript(script));
}
const fmtDraft = (d) => `[${d.id}] ${d.stream} · ${d.status}${d.posted ? " · " + Object.entries(d.posted).map(([k, v]) => k + ":" + v).join(" ") : ""}\n  x    ${d.x}\n  bsky ${d.bsky}`;

const TOOLS = [
  { name: "social_drafts", description: "Pending Aesthetic network posts drafted nightly on jasellite (daily token, deadlines board, papers, pop) for human approval. Call this when jeffrey asks for Aesthetic network news. Read-only.", inputSchema: { type: "object", properties: { status: { type: "string", enum: ["pending", "posted", "skipped", "all"], default: "pending" }, limit: { type: "integer", minimum: 1, maximum: 50, default: 20 } } } },
  { name: "social_draft_add", description: "Add a hand-written draft to the queue (status pending). Give one text, or separate x and bsky texts.", inputSchema: { type: "object", properties: { stream: { type: "string", enum: STREAMS, default: "note" }, text: { type: "string" }, x: { type: "string" }, bsky: { type: "string" }, url: { type: "string" } } } },
  { name: "social_draft_post", description: "Publish one queued draft to X (@promptDOTac) and/or Bluesky (@aesthetic.computer) and mark it posted. SIDE EFFECT: live public posts. Requires confirm:true; without it, shows exactly what would be sent.", inputSchema: { type: "object", properties: { id: { type: "string" }, to: { type: "array", items: { type: "string", enum: ["x", "bluesky"] }, default: ["x", "bluesky"] }, xText: { type: "string", description: "Override the X text before posting." }, bskyText: { type: "string", description: "Override the Bluesky text before posting." }, confirm: { type: "boolean" } }, required: ["id"] } },
  { name: "social_draft_mark", description: "Set a draft's status (skipped, pending, posted) without posting.", inputSchema: { type: "object", properties: { id: { type: "string" }, status: { type: "string", enum: ["pending", "skipped", "posted"] }, note: { type: "string" } }, required: ["id", "status"] } },
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
  if (name === "social_drafts") {
    const { where, queue, note } = await readQueue();
    const want = args.status || "pending";
    const rows = queue.filter((d) => want === "all" || d.status === want).slice(-(args.limit || 20)).reverse();
    const counts = queue.reduce((acc, d) => ((acc[d.status] = (acc[d.status] || 0) + 1), acc), {});
    return text([`drafts queue on ${where}: ${JSON.stringify(counts)}${note ? " — " + note : ""}`, ...(rows.length ? rows.map(fmtDraft) : [`no ${want} drafts`])].join("\n\n"));
  }
  if (name === "social_draft_add") {
    const body = args.text || args.x || args.bsky;
    if (!body) throw new Error("give text, or x and bsky");
    const id = `${args.stream || "note"}:${Date.now().toString(36)}`;
    const row = { id, stream: args.stream || "note", created: new Date().toISOString(), date: new Date().toISOString().slice(0, 10), url: args.url || null, x: args.x || args.text, bsky: args.bsky || args.text, status: "pending" };
    const out = await mutateQueue(Object.assign((q, m) => { q.push(m.row); return m.row.id; }, { args: { row } }));
    return text(`added ${out}\n${fmtDraft(row)}`);
  }
  if (name === "social_draft_post") {
    const { queue } = await readQueue();
    const d = queue.find((r) => r.id === args.id);
    if (!d) throw new Error(`no draft ${args.id}`);
    if (d.status === "posted") throw new Error(`${d.id} is already posted: ${JSON.stringify(d.posted)}`);
    const to = args.to?.length ? args.to : ["x", "bluesky"];
    const xText = args.xText || d.x, bskyText = args.bskyText || d.bsky;
    if (args.confirm !== true) return text(`would post ${d.id} to ${to.join(" + ")}:\n  x    ${xText}\n  bsky ${bskyText}\nCall again with confirm: true.`);
    const posted = {}, errors = [];
    if (to.includes("x")) { try { posted.x = await run(x, ["--as", "promptdotac", "post", xText], root, 300_000); } catch (e) { errors.push("x: " + e.message); } }
    if (to.includes("bluesky")) { try { posted.bluesky = await run(at, ["post", bskyText], resolve(root, "at"), 300_000); } catch (e) { errors.push("bluesky: " + e.message); } }
    if (Object.keys(posted).length) await mutateQueue(Object.assign((q, m) => { const r = q.find((z) => z.id === m.id); r.status = "posted"; r.posted = { ...(r.posted || {}), ...m.posted }; r.postedAt = m.at; if (m.xText !== r.x) r.xSent = m.xText; if (m.bskyText !== r.bsky) r.bskySent = m.bskyText; return r.id; }, { args: { id: d.id, posted: Object.fromEntries(Object.entries(posted).map(([k, v]) => [k, String(v).slice(0, 200)])), at: new Date().toISOString(), xText, bskyText } }));
    return text([`posted ${d.id} to ${Object.keys(posted).join(" + ") || "nothing"}`, ...Object.entries(posted).map(([k, v]) => `${k}: ${v}`), ...errors.map((e) => "FAILED " + e)].join("\n"));
  }
  if (name === "social_draft_mark") {
    const out = await mutateQueue(Object.assign((q, m) => { const r = q.find((z) => z.id === m.id); if (!r) throw new Error("no draft " + m.id); r.status = m.status; if (m.note) r.note = m.note; return r.id + " → " + r.status; }, { args: { id: args.id, status: args.status, note: args.note } }));
    return text(out);
  }
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
      serverInfo: { name: "x-mcp", version: "1.2.0" },
      instructions: "Official X API v2 (OAuth 1.0a) and Bluesky. Account-scoped credentials resolve automatically through the shared aesthetic-computer-vault registry (AC_VAULT overrides its location) and are never returned. X search is pay-per-use and locally budgeted. Draft before posting; public post/reply tools require confirm:true. Nightly, jasellite drafts the Aesthetic network's news (social_drafts); when jeffrey asks for network news, show the pending drafts and post only the ones he approves, via social_draft_post with confirm:true." } };
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
