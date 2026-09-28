#!/usr/bin/env node
// mcp-hub.mjs — one copy of each stdio MCP server, shared over HTTP.
//
// Every Claude session spawns its own copy of every stdio server in the
// project's .mcp.json. With a dozen servers and six sessions that is seventy
// node processes on an 8 GB machine, and the machine swaps. The servers named
// below keep no per-session state (they wrap CLIs and APIs), so one copy can
// answer everyone: this daemon starts each on first use, serves it at
// http://127.0.0.1:<port>/mcp/<name>, and lets it go after it has sat idle.
//
// Servers that seat or claim a session (acin, the coaches, venue) are not
// here and stay per-session stdio.
//
//   node slab/bin/mcp-hub.mjs [--port 7788]
import { spawn } from "node:child_process";
import { createServer } from "node:http";
import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { createInterface } from "node:readline";
import { fileURLToPath } from "node:url";

const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const PORT = Number(process.argv[process.argv.indexOf("--port") + 1]) || 7788;
const IDLE_MS = 30 * 60 * 1000;
const CALL_MS = 5 * 60 * 1000;

export const SHARED = [
  "aesel", "dmgify", "oskiewar", "crypto", "analytics", "x",
  "jukewizard", "roblox", "sweep", "illy",
];

function specs() {
  const servers = JSON.parse(readFileSync(join(REPO, ".mcp.json"), "utf8")).mcpServers || {};
  return Object.fromEntries(SHARED.filter((name) => servers[name]?.command).map((name) => [name, servers[name]]));
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
    this.proc = spawn(command, args, { cwd: REPO, env: { ...process.env, ...env }, stdio: ["pipe", "pipe", "inherit"] });
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

createServer((request, response) => {
  const name = /^\/mcp\/([\w-]+)\/?$/.exec(request.url || "")?.[1];
  const child = name && children[name];
  if (request.method === "GET" && request.url === "/health") {
    response.writeHead(200, { "Content-Type": "application/json" });
    return response.end(JSON.stringify({ servers: Object.keys(children), running: Object.values(children).filter((c) => c.proc).map((c) => c.name) }));
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
