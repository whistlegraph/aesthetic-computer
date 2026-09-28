// mcp-client.mjs — the person's MCP servers, for Aesel's own agent loop.
//
// Claude and Codex bring their MCP servers with them: the CLI reads the
// config and speaks the protocol. When Aesel runs the loop itself (the open
// backend) nothing does, so a model asked to use frame goes looking for a
// shell command called frame. This is the smallest client that closes that
// gap: read the servers Claude Code would load for this directory, connect to
// each (streamable HTTP or stdio), list their tools, and call them.
//
// Servers that are down or slow to start are skipped rather than waited on; a
// session should open in the time it always has.
import { spawn } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { createInterface } from "node:readline";

const PROTOCOL = "2025-06-18";
const CONNECT_TIMEOUT = 6000;
const CALL_TIMEOUT = 120000;
const CLIENT = { name: "aesel", version: "1" };

function readJson(file) {
  try { return JSON.parse(readFileSync(file, "utf8")); } catch { return null; }
}

// The servers Claude Code loads in `cwd`: user scope from ~/.claude.json, its
// per-project entries for this directory, and the nearest .mcp.json above it.
// Later scopes win on a name clash, as they do in Claude Code.
export function mcpServers(cwd, { home = homedir() } = {}) {
  const servers = {};
  const add = (entries = {}, root = cwd) => {
    for (const [name, spec] of Object.entries(entries || {})) servers[name] = { ...spec, root };
  };
  const user = readJson(join(home, ".claude.json")) || {};
  add(user.mcpServers);
  add(user.projects?.[resolve(cwd)]?.mcpServers);
  for (let dir = resolve(cwd); ; dir = dirname(dir)) {
    const file = join(dir, ".mcp.json");
    if (existsSync(file)) { add(readJson(file)?.mcpServers, dir); break; }
    if (dirname(dir) === dir) break;
  }
  return servers;
}

// A tool name the Messages API accepts: [a-zA-Z0-9_-], at most 64 characters.
function toolName(server, tool) {
  return `mcp__${server}__${tool}`.replace(/[^a-zA-Z0-9_-]/g, "_").slice(0, 64);
}

function withTimeout(promise, ms, what) {
  let timer;
  return Promise.race([
    promise,
    new Promise((_, reject) => { timer = setTimeout(() => reject(new Error(`${what} timed out`)), ms); }),
  ]).finally(() => clearTimeout(timer));
}

// Streamable HTTP: every request is a POST; the answer comes back as JSON or
// as an event stream holding it.
class HttpTransport {
  constructor(url, headers = {}) { this.url = url; this.headers = headers; this.session = ""; }
  async request(message, { signal } = {}) {
    const response = await fetch(this.url, {
      method: "POST",
      signal,
      headers: {
        "Content-Type": "application/json",
        Accept: "application/json, text/event-stream",
        "MCP-Protocol-Version": PROTOCOL,
        ...(this.session ? { "Mcp-Session-Id": this.session } : {}),
        ...this.headers,
      },
      body: JSON.stringify(message),
    });
    this.session = response.headers.get("mcp-session-id") || this.session;
    if (message.id === undefined) return null;
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    const text = await response.text();
    if (!(response.headers.get("content-type") || "").includes("text/event-stream")) return JSON.parse(text);
    for (const line of text.split("\n")) {
      if (!line.startsWith("data:")) continue;
      try {
        const reply = JSON.parse(line.slice(5));
        if (reply.id === message.id) return reply;
      } catch {}
    }
    throw new Error("no reply in the event stream");
  }
  close() {}
}

// stdio: one JSON-RPC message per line, the server's own process.
class StdioTransport {
  constructor({ command, args = [], env = {}, root }) {
    this.pending = new Map();
    this.child = spawn(command, args, { cwd: root, env: { ...process.env, ...env }, stdio: ["pipe", "pipe", "ignore"] });
    this.child.on("error", (error) => this.#fail(error));
    this.child.on("exit", () => this.#fail(new Error("server exited")));
    createInterface({ input: this.child.stdout }).on("line", (line) => {
      let reply;
      try { reply = JSON.parse(line); } catch { return; }
      const waiting = this.pending.get(reply.id);
      if (waiting) { this.pending.delete(reply.id); waiting.resolve(reply); }
    });
  }
  #fail(error) {
    for (const waiting of this.pending.values()) waiting.reject(error);
    this.pending.clear();
  }
  request(message, { signal } = {}) {
    this.child.stdin.write(`${JSON.stringify(message)}\n`);
    if (message.id === undefined) return Promise.resolve(null);
    return new Promise((resolve, reject) => {
      this.pending.set(message.id, { resolve, reject });
      signal?.addEventListener("abort", () => { this.pending.delete(message.id); reject(signal.reason); }, { once: true });
    });
  }
  close() { this.child.kill(); }
}

class McpServer {
  constructor(name, spec) {
    this.name = name;
    this.transport = spec.url
      ? new HttpTransport(spec.url, spec.headers)
      : new StdioTransport(spec);
    this.next = 0;
  }
  async call(method, params, options) {
    const reply = await this.transport.request({ jsonrpc: "2.0", id: ++this.next, method, params }, options);
    if (reply?.error) throw new Error(reply.error.message || JSON.stringify(reply.error));
    return reply?.result;
  }
  async open() {
    await this.call("initialize", { protocolVersion: PROTOCOL, capabilities: {}, clientInfo: CLIENT });
    await this.transport.request({ jsonrpc: "2.0", method: "notifications/initialized" });
    const tools = [];
    let cursor;
    do {
      const page = await this.call("tools/list", cursor ? { cursor } : {});
      tools.push(...(page?.tools || []));
      cursor = page?.nextCursor;
    } while (cursor);
    return tools;
  }
  close() { this.transport.close(); }
}

// Every reachable server's tools, named the way Claude Code names them.
export class McpTools {
  constructor(cwd) { this.cwd = cwd; this.servers = []; this.routes = new Map(); this.ready = null; this.failed = []; }

  load() {
    this.ready ??= Promise.all(Object.entries(mcpServers(this.cwd)).map(async ([name, spec]) => {
      let server;
      try {
        server = new McpServer(name, spec);
        const tools = await withTimeout(server.open(), CONNECT_TIMEOUT, name);
        this.servers.push(server);
        for (const tool of tools) {
          this.routes.set(toolName(name, tool.name), { server, tool: tool.name, definition: {
            name: toolName(name, tool.name),
            description: (tool.description || tool.title || tool.name).slice(0, 1024),
            input_schema: tool.inputSchema?.type === "object" ? tool.inputSchema : { type: "object", properties: {} },
          } });
        }
      } catch (error) {
        server?.close();
        this.failed.push(`${name}: ${error.message}`);
      }
    })).then(() => this);
    return this.ready;
  }

  async tools() {
    await this.load();
    return [...this.routes.values()].map((route) => route.definition);
  }

  has(name) { return this.routes.has(name); }

  describe(name) {
    const route = this.routes.get(name);
    return route ? `${route.server.name} · ${route.tool}` : name;
  }

  // The result as text. Images are named, not sent: the open models this loop
  // runs are mostly text-only, and frame returns OCR beside its pictures.
  async call(name, input = {}, { signal } = {}) {
    const route = this.routes.get(name);
    if (!route) throw new Error(`No MCP tool named ${name}.`);
    const result = await withTimeout(
      route.server.call("tools/call", { name: route.tool, arguments: input }, { signal }),
      CALL_TIMEOUT, name);
    const text = (result?.content || []).map((part) =>
      part.type === "text" ? part.text
        : part.type === "image" ? `[${part.mimeType || "image"} omitted — this model reads text only]`
        : part.type === "resource" ? part.resource?.text || `[resource ${part.resource?.uri || ""}]`
        : `[${part.type}]`).join("\n");
    if (result?.isError) throw new Error(text || "tool failed");
    return text || JSON.stringify(result?.structuredContent ?? result ?? {});
  }

  close() { for (const server of this.servers) server.close(); }
}
