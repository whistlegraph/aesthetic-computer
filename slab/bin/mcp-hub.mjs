#!/usr/bin/env node
// mcp-hub.mjs — one copy of each stdio MCP server, shared over HTTP.
//
// Every Claude session spawns its own copy of every stdio server it is
// given. With a dozen servers and six sessions that is seventy node
// processes on an 8 GB machine, and the machine swaps. The servers in
// toolchain/mcp/hub-servers.json keep no per-session state (they wrap CLIs
// and APIs), so one copy can answer everyone: this daemon starts each on
// first use, serves it at http://127.0.0.1:<port>/mcp/<name>, and lets it go
// after it has sat idle.
//
// Those servers are deliberately NOT in .mcp.json. Newer Claude Code flags a
// name defined at project scope and again at local scope, so each machine
// registers them once, at local scope:
//
//   node slab/bin/mcp-hub.mjs [--port 7788]       run the hub (launchd does this)
//   node slab/bin/mcp-hub.mjs --register          point Claude (and Codex) at the hub
//   node slab/bin/mcp-hub.mjs --register --stdio  no hub here: one process per session
//
// toolchain/mcp/install-daemons.sh installs the hub and runs --register.
//
// The hub also keeps the shelf: servers in toolchain/mcp/shelf.json are retired
// from every Mac's registration, so they cost a session no tool list and no
// process, yet stay within reach through one small server at /mcp/shelf —
// shelf_list browses them, shelf_tools reads one's tools, shelf_call runs one.
// Servers that seat or claim a session (acin, the coaches, venue) are not
// here and stay per-session stdio in .mcp.json.
import { spawn, spawnSync } from "node:child_process";
import { createServer } from "node:http";
import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { createInterface } from "node:readline";
import { fileURLToPath } from "node:url";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const flag = (name) => process.argv.indexOf(name);
// --http is the installer's spelling (install-daemons.sh write_plist).
const PORT = Number(process.argv[flag("--port") + 1] || (flag("--http") > 0 ? process.argv[flag("--http") + 1] : 0)) || 7788;
const IDLE_MS = 30 * 60 * 1000;
const CALL_MS = 5 * 60 * 1000;

function specs() {
  return JSON.parse(readFileSync(join(REPO, "toolchain/mcp/hub-servers.json"), "utf8")).mcpServers || {};
}
export const SHARED = Object.keys(specs());

const home = (text) => text.replace("$HOME", process.env.HOME);
export function shelved() {
  return JSON.parse(readFileSync(join(REPO, "toolchain/mcp/shelf.json"), "utf8")).servers || {};
}

// Point this machine's Claude (local scope, this checkout) and Codex at the
// hub, or at plain per-session stdio when the machine runs no hub.
// Remove-then-add is idempotent.
function register({ stdio = false } = {}) {
  const has = (cmd) => spawnSync("sh", ["-c", `command -v ${cmd}`]).status === 0;
  const run = (cmd, args) => spawnSync(cmd, args, { cwd: REPO, encoding: "utf8" });
  // A shelved name is taken off this Mac wherever it was registered.
  for (const name of Object.keys(shelved())) {
    if (has("claude")) for (const scope of ["local", "user"]) run("claude", ["mcp", "remove", name, "--scope", scope]);
    if (has("codex")) run("codex", ["mcp", "remove", name]);
  }
  const served = stdio ? specs() : { ...specs(), shelf: { args: [] } };
  for (const [name, spec] of Object.entries(served)) {
    const url = `http://127.0.0.1:${PORT}/mcp/${name}`;
    const local = { type: "stdio", command: spec.command, args: spec.args.map((arg) => arg.endsWith(".mjs") ? join(REPO, arg) : arg) };
    if (has("claude")) {
      run("claude", ["mcp", "remove", name, "--scope", "local"]);
      const added = stdio ? run("claude", ["mcp", "add-json", "--scope", "local", name, JSON.stringify(local)])
        : run("claude", ["mcp", "add", "--transport", "http", "--scope", "local", name, url]);
      console.log(`${added.status === 0 ? "✓" : "✗"} claude ${name} → ${stdio ? "stdio" : url}${added.status ? ` (${added.stderr.trim()})` : ""}`);
    }
    if (has("codex")) {
      run("codex", ["mcp", "remove", name]);
      const added = stdio ? run("codex", ["mcp", "add", name, "--", local.command, ...local.args]) : run("codex", ["mcp", "add", name, "--url", url]);
      console.log(`${added.status === 0 ? "✓" : "✗"} codex  ${name} → ${stdio ? "stdio" : url}`);
    }
  }
}

const MAIN = process.argv[1] === fileURLToPath(import.meta.url);
if (MAIN && process.argv.includes("--register")) {
  register({ stdio: process.argv.includes("--stdio") });
  process.exit(0);
}

// One child per server. Requests from every client are renumbered on the way
// in, so two sessions that both send id 1 get their own answers back.
class Child {
  constructor(name, spec) {
    this.name = name;
    this.spec = spec;
    this.proc = null;
    this.next = 0;
    this.pending = new Map();
    this.timer = null;
  }
  #start() {
    const { command, args = [], env = {} } = this.spec;
    this.proc = spawn(home(command), args.map(home), { cwd: REPO, env: { ...process.env, ...env }, stdio: ["pipe", "pipe", "inherit"] });
    this.proc.on("exit", () => {
      this.proc = null;
      for (const { reply, id } of this.pending.values()) reply({ jsonrpc: "2.0", id, error: { code: -32000, message: `${this.name} exited` } });
      this.pending.clear();
    });
    createInterface({ input: this.proc.stdout }).on("line", (line) => {
      let message;
      try { message = JSON.parse(line); } catch { return; }
      const waiting = this.pending.get(message.id);
      if (!waiting) return;
      this.pending.delete(message.id);
      clearTimeout(waiting.timer);
      waiting.reply({ ...message, id: waiting.id });
    });
    console.log(`${new Date().toISOString()} start ${this.name}`);
  }
  #touch() {
    clearTimeout(this.timer);
    this.timer = setTimeout(() => {
      if (this.pending.size) return this.#touch();
      console.log(`${new Date().toISOString()} idle ${this.name}`);
      this.proc?.kill();
    }, IDLE_MS);
    this.timer.unref();
  }
  send(message) {
    if (!this.proc) this.#start();
    this.#touch();
    if (message.id === undefined) {
      this.proc.stdin.write(`${JSON.stringify(message)}\n`);
      return Promise.resolve(null);
    }
    return new Promise((reply) => {
      const id = ++this.next;
      const timer = setTimeout(() => {
        this.pending.delete(id);
        reply({ jsonrpc: "2.0", id: message.id, error: { code: -32000, message: `${this.name} timed out` } });
      }, CALL_MS);
      this.pending.set(id, { reply, id: message.id, timer });
      this.proc.stdin.write(`${JSON.stringify({ ...message, id })}\n`);
    });
  }
}

const children = Object.fromEntries(Object.entries(specs()).map(([name, spec]) => [name, new Child(name, spec)]));

// The shelf. A shelved child is made on first ask and greeted once per start,
// since no client of its own ever sends it an initialize.
const resting = {};
async function wake(name) {
  const spec = shelved()[name];
  if (!spec) throw new Error(`${name} is not on the shelf — shelf_list names what is`);
  if (!spec.command) throw new Error(`${name} is a remote server, listed only. Register it with: ${spec.restore}`);
  const child = (resting[name] ||= new Child(name, spec));
  if (!child.proc) {
    const hello = await child.send({ jsonrpc: "2.0", id: 0, method: "initialize", params: { protocolVersion: "2024-11-05", capabilities: {}, clientInfo: { name: "mcp-hub-shelf", version: "1" } } });
    if (hello.error) throw new Error(`${name} did not start: ${hello.error.message}`);
    await child.send({ jsonrpc: "2.0", method: "notifications/initialized" });
  }
  return child;
}

// Put every woken shelf server back down (the tests call this to exit).
export const rest = () => { for (const child of Object.values(resting)) child.proc?.kill(); };

const text = (value) => ({ content: [{ type: "text", text: typeof value === "string" ? value : JSON.stringify(value, null, 2) }] });
export const SHELF_TOOLS = [
  { name: "shelf_list", description: "List the retired MCP servers kept on the shelf: what each does, why it was retired, what to use instead, whether shelf_call can run it here, and the line that registers it again. Look here before concluding a capability does not exist.", inputSchema: { type: "object", properties: {} } },
  { name: "shelf_tools", description: "Start a shelved server and return its tools with their input schemas.", inputSchema: { type: "object", properties: { server: { type: "string" } }, required: ["server"] } },
  { name: "shelf_call", description: "Call one tool of a shelved server. Read its schema with shelf_tools first.", inputSchema: { type: "object", properties: { server: { type: "string" }, tool: { type: "string" }, arguments: { type: "object" } }, required: ["server", "tool"] } },
];

export async function shelf(message) {
  const ok = (result) => ({ jsonrpc: "2.0", id: message.id, result });
  if (message.id === undefined) return null;
  if (message.method === "initialize") return ok({ protocolVersion: message.params?.protocolVersion || "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "shelf", version: "1" },
    instructions: "Retired MCP servers, kept callable. shelf_list to browse, shelf_tools for one server's tools, shelf_call to run one. Nothing here is running until asked for." });
  if (message.method === "ping") return ok({});
  if (message.method === "tools/list") return ok({ tools: SHELF_TOOLS });
  if (message.method !== "tools/call") return { jsonrpc: "2.0", id: message.id, error: { code: -32601, message: `no method ${message.method}` } };
  const { name, arguments: args = {} } = message.params || {};
  try {
    if (name === "shelf_list") return ok(text(Object.fromEntries(Object.entries(shelved()).map(([n, s]) => [n, { about: s.about, retired: s.retired, why: s.why, ...(s.instead ? { instead: s.instead } : {}), callable: !!s.command, restore: s.restore }]))));
    if (name === "shelf_tools") {
      const reply = await (await wake(args.server)).send({ jsonrpc: "2.0", id: 0, method: "tools/list" });
      if (reply.error) throw new Error(reply.error.message);
      return ok(text(reply.result.tools));
    }
    if (name === "shelf_call") {
      const reply = await (await wake(args.server)).send({ jsonrpc: "2.0", id: 0, method: "tools/call", params: { name: args.tool, arguments: args.arguments || {} } });
      if (reply.error) throw new Error(reply.error.message);
      return ok(reply.result);
    }
    throw new Error(`no tool ${name}`);
  } catch (error) { return ok({ ...text(error.message), isError: true }); }
}

if (MAIN) createServer((request, response) => {
  const name = /^\/mcp\/([\w-]+)\/?$/.exec(request.url || "")?.[1];
  const child = name === "shelf" ? { send: shelf } : name && children[name];
  if (request.method === "GET" && request.url === "/health") {
    response.writeHead(200, { "Content-Type": "application/json" });
    return response.end(JSON.stringify({ servers: Object.keys(children), running: [...Object.values(children), ...Object.values(resting)].filter((c) => c.proc).map((c) => c.name), shelf: Object.keys(shelved()) }));
  }
  if (!child) { response.writeHead(404); return response.end(); }
  // No server-to-client stream: a GET for one is declined, per the spec.
  if (request.method !== "POST") { response.writeHead(405, { Allow: "POST" }); return response.end(); }
  let body = "";
  request.on("data", (chunk) => { body += chunk; });
  request.on("end", async () => {
    let message;
    try { message = JSON.parse(body); } catch {
      response.writeHead(400, { "Content-Type": "application/json" });
      return response.end(JSON.stringify({ jsonrpc: "2.0", id: null, error: { code: -32700, message: "parse error" } }));
    }
    const reply = await child.send(message);
    if (!reply) { response.writeHead(202); return response.end(); }
    response.writeHead(200, { "Content-Type": "application/json" });
    response.end(JSON.stringify(reply));
  });
}).listen(PORT, "127.0.0.1", () => console.log(`mcp-hub on :${PORT} · ${Object.keys(children).join(" ")}`));
