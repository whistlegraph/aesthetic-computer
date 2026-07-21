#!/usr/bin/env node
// puppet-mcp.mjs — the ACTING half of the fleet loop over MCP: drive browsers
// and native apps on any registered Mac, discoverable as named tools. Companion
// to frame-mcp (which OBSERVES); together they close observe→act from any agent
// session without anyone knowing the `puppet` CLI exists.
//
// LATENCY: this does NOT shell out to the puppet CLI. The CDP verbs talk the
// SAME unix-socket JSON-lines protocol the CLI's rpc() speaks, straight to the
// resident puppet daemon that already holds warm CDP WebSockets to every
// browser — so an action is one socket hop to an open connection (~5ms), and
// because this MCP server is itself resident we skip even the per-call `node`
// cold-start the CLI pays. The OS-automation verbs (type/keys/term) call into
// macos.mjs directly (ssh + osascript), the same cost the CLI pays.
//
// Hand-rolled stdio JSON-RPC in the house style (artery/emacs-mcp.mjs), no SDK.
// Registry stays in the untracked ~/.config/slab/puppet.json `puppet` reads.
import { existsSync, readFileSync } from "node:fs";
import net from "node:net";
import { homedir } from "node:os";
import { join } from "node:path";
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";
import { termList, typeText, sendKeys } from "./macos.mjs";

const HOME = homedir();
const CONFIG_PATH = process.env.SLAB_PUPPET_CONFIG || join(HOME, ".config", "slab", "puppet.json");
const SOCK_PATH = process.env.SLAB_PUPPET_SOCK || join(HOME, ".local", "share", "puppet", "puppet.sock");

function machineSpec(name) {
  if (!name) throw new Error("`machine` is required");
  let cfg = null;
  try { cfg = JSON.parse(readFileSync(CONFIG_PATH, "utf8")); } catch {}
  const spec = cfg?.machines?.[name];
  if (!spec) throw new Error(`unknown machine: ${name} — see puppet_list / puppet config`);
  return spec;
}

// One request/reply against the resident daemon over its unix socket. Mirrors
// puppet.mjs rpc(): open, send one JSON line, read one JSON line, close. Unix
// socket setup is microseconds; the win the daemon already banked (warm CDP,
// no WS handshake) carries through untouched.
function rpc(req, { timeoutMs = 20000 } = {}) {
  return new Promise((resolve, reject) => {
    if (!existsSync(SOCK_PATH)) {
      reject(new Error("puppet daemon not running — start it with:  puppet daemon  (needs a machine with a cdpUrl in puppet.json)"));
      return;
    }
    const sock = net.createConnection(SOCK_PATH);
    const timer = setTimeout(() => { sock.destroy(); reject(new Error(`daemon rpc timeout (${req.cmd})`)); }, timeoutMs);
    let buf = "";
    sock.on("error", () => { clearTimeout(timer); reject(new Error("puppet daemon not reachable — start with: puppet daemon")); });
    sock.on("data", (chunk) => {
      buf += chunk;
      const nl = buf.indexOf("\n");
      if (nl < 0) return;
      clearTimeout(timer);
      sock.end();
      const msg = JSON.parse(buf.slice(0, nl));
      if (msg.error) reject(new Error(msg.error));
      else resolve(msg.result);
    });
    sock.write(JSON.stringify(req) + "\n");
  });
}

const text = (t) => [{ type: "text", text: typeof t === "string" ? t : JSON.stringify(t, null, 2) }];

// ── CDP verbs (via the warm daemon) ─────────────────────────────────────────
async function toolList() {
  return text(await rpc({ cmd: "list" }));
}
async function toolEval({ machine, js, target }) {
  return text(await rpc({ cmd: "eval", machine, args: { expr: js, target } }));
}
async function toolWaitFor({ machine, js, timeout, interval, target }) {
  return text(await rpc({ cmd: "waitFor", machine, args: { expr: js, target, timeout, interval } }));
}
async function toolNav({ machine, url, target }) {
  return text(await rpc({ cmd: "nav", machine, args: { url, target } }));
}
async function toolReload({ machine, target }) {
  return text(await rpc({ cmd: "reload", machine, args: { target } }));
}
async function toolShot({ machine, target, format = "jpeg", fresh = false }) {
  const b64 = await rpc({ cmd: "shot", machine, args: { target, format, quality: 80, fresh } });
  return [{ type: "image", data: b64, mimeType: format === "png" ? "image/png" : "image/jpeg" }];
}
async function toolStroke({ machine, points, target }) {
  const n = await rpc({ cmd: "stroke", machine, args: { points, target } });
  return text(`${n} points dispatched`);
}
async function toolGesture({ machine, from, to, opts = {}, target }) {
  const steps = await rpc({ cmd: "gesture", machine, args: { from, to, opts, target } });
  return text(`${steps} steps`);
}
async function toolKey({ machine, key, modifiers = 0, target }) {
  return text(`key: ${await rpc({ cmd: "key", machine, args: { key, modifiers, target } })}`);
}
async function toolCursor({ machine, x, y, target }) {
  await rpc({ cmd: "cursor", machine, args: { x, y, target } });
  return text("cursor placed");
}

// ── OS automation (ssh + osascript; no daemon needed) ───────────────────────
async function toolType({ machine, text: t, tty, paste = false, clear = false, enter = false }) {
  return text(typeText(machineSpec(machine), t, { tty, paste, clear, enter }));
}
async function toolKeys({ machine, key, mod }) {
  const mods = Array.isArray(mod) ? mod : (mod ? String(mod).split(",").filter(Boolean) : []);
  return text(sendKeys(machineSpec(machine), key, mods));
}
async function toolTerm({ machine }) {
  return text(termList(machineSpec(machine)));
}

const TOOLS = [
  { name: "puppet_list", act: false,
    description: "List registered machines with CDP connection state and open page targets (from the puppet daemon). Read-only.",
    inputSchema: { type: "object", properties: {} } },
  { name: "puppet_eval", act: false,
    description: "Evaluate a JavaScript expression in the active page of a machine's browser (via CDP) and return the value. Reads/inspects page state; can also mutate the DOM.",
    inputSchema: { type: "object", properties: { machine: { type: "string" }, js: { type: "string", description: "Expression to evaluate (awaited if it returns a promise)." }, target: { type: "string", description: "Optional target url/id substring; defaults to most-recent http(s) page." } }, required: ["machine", "js"] } },
  { name: "puppet_waitfor", act: false,
    description: "Poll a JS expression in the active page until it returns truthy (or timeout). Use to synchronize on page state instead of fixed sleeps.",
    inputSchema: { type: "object", properties: { machine: { type: "string" }, js: { type: "string" }, timeout: { type: "number", description: "ms (default 15000)" }, interval: { type: "number", description: "ms (default 150)" }, target: { type: "string" } }, required: ["machine", "js"] } },
  { name: "puppet_shot", act: false,
    description: "Screenshot the active browser PAGE (CDP) and return it inline. For a WHOLE-screen capture of any native app, use the frame tool instead.",
    inputSchema: { type: "object", properties: { machine: { type: "string" }, target: { type: "string" }, format: { type: "string", enum: ["jpeg", "png"], description: "default jpeg" }, fresh: { type: "boolean", description: "force a real capture instead of a live watch frame" } }, required: ["machine"] } },
  { name: "puppet_nav", act: true,
    description: "ACTS: navigate the active page of a machine's browser to a URL.",
    inputSchema: { type: "object", properties: { machine: { type: "string" }, url: { type: "string" }, target: { type: "string" } }, required: ["machine", "url"] } },
  { name: "puppet_reload", act: true,
    description: "ACTS: hard-reload (cache-bypassing) the active page.",
    inputSchema: { type: "object", properties: { machine: { type: "string" }, target: { type: "string" } }, required: ["machine"] } },
  { name: "puppet_stroke", act: true,
    description: "ACTS: trusted mouse drag through a series of page pixel coordinates (a real, dispatched pointer stream — clicks, drags, swipes). Points are [[x,y],...].",
    inputSchema: { type: "object", properties: { machine: { type: "string" }, points: { type: "array", items: { type: "array", items: { type: "number" } }, description: "e.g. [[100,200],[140,240]] — a single point is a click." }, target: { type: "string" } }, required: ["machine", "points"] } },
  { name: "puppet_gesture", act: true,
    description: "ACTS: a human-like trusted drag from one point to another — steered heading, eased velocity, micro-jitter. Good for latency/feel testing.",
    inputSchema: { type: "object", properties: { machine: { type: "string" }, from: { type: "array", items: { type: "number" } }, to: { type: "array", items: { type: "number" } }, opts: { type: "object", description: "{speed,bend,wobble,hesitation}" }, target: { type: "string" } }, required: ["machine", "from", "to"] } },
  { name: "puppet_key", act: true,
    description: "ACTS: dispatch a trusted key press to the active page. key is a DOM key name (Enter, Backspace, ArrowLeft, or a character). modifiers bitmask: 1 alt, 2 ctrl, 4 meta, 8 shift.",
    inputSchema: { type: "object", properties: { machine: { type: "string" }, key: { type: "string" }, modifiers: { type: "number" }, target: { type: "string" } }, required: ["machine", "key"] } },
  { name: "puppet_cursor", act: true,
    description: "ACTS: show/move the virtual cursor overlay at page pixel (x,y) — a pointing aid, no click.",
    inputSchema: { type: "object", properties: { machine: { type: "string" }, x: { type: "number" }, y: { type: "number" }, target: { type: "string" } }, required: ["machine", "x", "y"] } },
  { name: "puppet_type", act: true,
    description: "ACTS (OS-level, ssh+osascript — no browser needed): type text into the frontmost app on a machine, or into a specific Terminal tab. --tty targets a tab; paste is multi-line-safe; clear wipes the input first; enter submits.",
    inputSchema: { type: "object", properties: { machine: { type: "string" }, text: { type: "string" }, tty: { type: "string", description: "e.g. /dev/ttys003 — target a Terminal tab" }, paste: { type: "boolean" }, clear: { type: "boolean" }, enter: { type: "boolean" } }, required: ["machine", "text"] } },
  { name: "puppet_keys", act: true,
    description: "ACTS (OS-level): send a key or chord to the frontmost app on a machine. key is enter|escape|tab|up|... or a literal character; mod is e.g. \"cmd,shift\".",
    inputSchema: { type: "object", properties: { machine: { type: "string" }, key: { type: "string" }, mod: { type: "string", description: "comma-separated modifiers, e.g. cmd,shift" } }, required: ["machine", "key"] } },
  { name: "puppet_term", act: false,
    description: "List a machine's Terminal tabs (tty, busy state, running processes) over ssh. Read-only; pairs with puppet_type --tty to drive a specific tab.",
    inputSchema: { type: "object", properties: { machine: { type: "string" } }, required: ["machine"] } },
];

const HANDLERS = {
  puppet_list: toolList, puppet_eval: toolEval, puppet_waitfor: toolWaitFor,
  puppet_nav: toolNav, puppet_reload: toolReload, puppet_shot: toolShot,
  puppet_stroke: toolStroke, puppet_gesture: toolGesture, puppet_key: toolKey,
  puppet_cursor: toolCursor, puppet_type: toolType, puppet_keys: toolKeys, puppet_term: toolTerm,
};

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return { jsonrpc: "2.0", id, result: { protocolVersion: "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "puppet-mcp", version: "1.0.0" } } };
      case "initialized":
      case "notifications/initialized":
        return null;
      case "ping":
        return { jsonrpc: "2.0", id, result: {} };
      case "tools/list":
        return { jsonrpc: "2.0", id, result: { tools: TOOLS.map(({ act, ...t }) => t) } };
      case "tools/call": {
        const fn = HANDLERS[params?.name];
        if (!fn) throw new Error(`Unknown tool: ${params?.name}`);
        const content = await fn(params.arguments || {});
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default:
        return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call")
      return { jsonrpc: "2.0", id, result: { isError: true, content: text(String(error.message || error)) } };
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

// stdio by default (Claude spawns one process per session), or `--http [port]`
// for one resident daemon every session shares — installed by
// toolchain/mcp/install-daemons.sh. handleMessage is stateless per call, and
// the real state lives in the puppet daemon behind the unix socket anyway.
const port = httpPort(process.argv, 7769); // 7768 is Spotify's
if (port) serveHttp({ handleMessage, port, banner: "🎭 puppet-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "🎭 puppet-mcp server started (act: nav/reload/stroke/gesture/key/cursor/type/keys + eval/shot/list/term)" });
