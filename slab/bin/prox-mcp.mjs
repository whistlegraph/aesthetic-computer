#!/usr/bin/env node
// prox-mcp.mjs — an MCP over the slab "prompt rocks" ledger, so any
// agent can LIST, FIND, and POKE the little tumbling sigil stones the slab
// menubar parks over every live Claude session across the fleet.
//
// A "rock" is one live session (or headless agent), advertised by its machine
// as `host:name` — e.g. neo:regif, blueberry:flock, panda:iris. The name is the
// stable pet-name (deterministic from the session/thread id), so it matches
// exactly what you see rendered on that machine's overlay even as the rock's
// prompt-driven texture and form evolve. This is how a
// `machine:promptname` reference resolves without an SSH+find crawl.
//
// The data source is the fleet ledger the menubar already publishes + caches:
//   ~/.config/slab/ledger/local.json      — THIS machine's rocks
//   ~/.config/slab/ledger/peers/<host>.json — each online peer's rocks
// Each file is {host, ip, updatedAt, entries:[{id,host,name,subject,status,
// kind,seed,cwd,updated}]}. Reads are O(1) local file loads (the menubar keeps
// them fresh over the tailnet); a poke is a POST to the owning machine's ledger
// server (:5252 /poke {by,id,name}), which makes its rock blink + rattle.
//
// Hand-rolled JSON-RPC over stdio, matching the house style of the sibling
// frame-mcp / puppet-mcp — no SDK, only node builtins + the shared front.
import { readFile, readdir, writeFile, mkdir } from "node:fs/promises";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { join } from "node:path";
import { homedir, hostname } from "node:os";
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";

const pexec = promisify(execFile);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const LEDGER_DIR = join(homedir(), ".config", "slab", "ledger");
const LOCAL_FILE = join(LEDGER_DIR, "local.json");
const PEERS_DIR = join(LEDGER_DIR, "peers");
const PORT = 5252; // the menubar's LedgerHTTPServer port on every machine
const IMSG_BINDING = join(homedir(), ".config", "slab", "imsg-prox.json");

// Per-session marker files (written by the slab claude hooks) carry the tty +
// pid a rock is running on — the same source the menubar overlay reads. Keyed
// by sessionId (== the ledger entry `id`), so prox_close can find the terminal.
const SLAB_HOME = process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab");
const MARKER_DIRS = [join(SLAB_HOME, "state", "active-prompts"), join(SLAB_HOME, "state", "awaiting-prompts")];

// ── load the fleet ledger off disk ──────────────────────────────────────────
async function readJson(path) {
  try {
    return JSON.parse(await readFile(path, "utf8"));
  } catch {
    return null;
  }
}

// Every ledger this machine knows about: its own local one first, then each
// cached peer. Returns [{host, ip, updatedAt, entries, self}].
async function allLedgers() {
  const out = [];
  const local = await readJson(LOCAL_FILE);
  if (local) out.push({ ...local, self: true });
  let peerFiles = [];
  try {
    peerFiles = (await readdir(PEERS_DIR)).filter((f) => f.endsWith(".json"));
  } catch {}
  for (const f of peerFiles) {
    const p = await readJson(join(PEERS_DIR, f));
    if (p) out.push({ ...p, self: false });
  }
  return out;
}

// Flatten to one row per rock, carrying its machine's host + ip so a poke knows
// where to go. Sorted newest-activity-first within each host.
async function allRocks() {
  const rows = [];
  for (const led of await allLedgers()) {
    for (const e of led.entries || []) {
      rows.push({ ...e, host: e.host || led.host, ip: led.ip, self: led.self });
    }
  }
  return rows;
}

// ── formatting ───────────────────────────────────────────────────────────────
function age(ms) {
  if (!ms) return "?";
  const s = Math.max(0, Math.round((Date.now() - ms) / 1000));
  if (s < 90) return `${s}s`;
  const m = Math.round(s / 60);
  if (m < 90) return `${m}m`;
  return `${Math.round(m / 60)}h`;
}

const STATUS_MARK = {
  working: "●", awaiting: "◐", complete: "○",
  rendering: "◍", blank: "·", interrupted: "✕",
};

function line(r) {
  const mark = STATUS_MARK[r.status] || "•";
  const subj = (r.subject || "").replace(/\s+/g, " ").slice(0, 64);
  // Tag the owning agent when it isn't the default (Claude), so a mixed
  // fleet reads clearly: "session·codex".
  const agent = r.agentType && r.agentType !== "claude" ? `·${r.agentType}` : "";
  return `${mark} ${r.host}:${r.name}  [${r.status}] ${r.kind}${agent}  ·${age(r.updated)}  ${subj}`;
}

// ── resolve a `host:name` / bare-name / fuzzy handle to rock rows ────────────
function resolve(rocks, handle) {
  if (!handle) return rocks;
  const h = handle.trim().toLowerCase();
  let host = null;
  let name = h;
  if (h.includes(":")) {
    [host, name] = h.split(":", 2);
    host = host === "local" ? null : host; // "local:foo" → any host with name foo on self
  }
  const inHost = (r) => !host || r.host.toLowerCase() === host || (host === "local" && r.self);
  // Stable session id is the strongest identity; then exact pet name, prefix,
  // and substring — so `neo:reg` still finds regif.
  const id = rocks.filter((r) => inHost(r) && r.id.toLowerCase() === name);
  if (id.length) return id;
  const exact = rocks.filter((r) => inHost(r) && r.name.toLowerCase() === name);
  if (exact.length) return exact;
  const prefix = rocks.filter((r) => inHost(r) && r.name.toLowerCase().startsWith(name));
  if (prefix.length) return prefix;
  return rocks.filter((r) => inHost(r) && (r.name.toLowerCase().includes(name) ||
    (r.subject || "").toLowerCase().includes(name)));
}

// ── close plumbing (local machine only) ─────────────────────────────────────
// Read a session's marker (tty + claude pid) by its ledger id == sessionId.
async function readMarker(id) {
  for (const d of MARKER_DIRS) {
    const m = await readJson(join(d, id));
    if (m) return m;
  }
  return null;
}

const pidAlive = (pid) => { try { process.kill(pid, 0); return true; } catch { return false; } };

// Walk the parent chain of `pid` so prox_close can refuse to close the very
// session that is calling it (the MCP runs as a child of its own claude).
async function ancestorPids(pid) {
  const chain = new Set();
  let cur = pid;
  for (let i = 0; i < 40 && cur > 1; i++) {
    try {
      const { stdout } = await pexec("ps", ["-o", "ppid=", "-p", String(cur)]);
      const ppid = parseInt(stdout.trim(), 10);
      if (!ppid || ppid <= 1 || chain.has(ppid)) break;
      chain.add(ppid); cur = ppid;
    } catch { break; }
  }
  return chain;
}

// Close the Terminal.app window hosting a tty (SIGHUPs its process tree —
// claude traps SIGTERM, so closing the window is what actually ends it).
async function closeTerminalTty(tty) {
  const dev = tty.startsWith("/dev/") ? tty : `/dev/${tty}`;
  const osa = `tell application "Terminal"
  set n to 0
  repeat with w in windows
    repeat with t in tabs of w
      try
        if (tty of t) is "${dev}" then
          close w saving no
          set n to n + 1
        end if
      end try
    end repeat
  end repeat
  return n
end tell`;
  try { const { stdout } = await pexec("osascript", ["-e", osa]); return parseInt(stdout.trim(), 10) || 0; }
  catch { return 0; }
}

// ── tools ─────────────────────────────────────────────────────────────────────
async function toolList({ host, status, kind, agent } = {}) {
  let rocks = await allRocks();
  if (host) rocks = rocks.filter((r) => r.host.toLowerCase() === host.toLowerCase());
  if (status) rocks = rocks.filter((r) => r.status === status);
  if (kind) rocks = rocks.filter((r) => r.kind === kind);
  if (agent) rocks = rocks.filter((r) => (r.agentType || "claude").toLowerCase() === agent.toLowerCase());
  if (!rocks.length) return [{ type: "text", text: "(no prompt rocks match — is SlabMenubar running? try again in a few seconds)" }];
  // group by host, self first
  rocks.sort((a, b) => (a.self === b.self ? a.host.localeCompare(b.host) : a.self ? -1 : 1) || 0);
  const byHost = new Map();
  for (const r of rocks) (byHost.get(r.host) || byHost.set(r.host, []).get(r.host)).push(r);
  const L = [`${rocks.length} prompt rock(s) across ${byHost.size} machine(s):`];
  for (const [hst, rs] of byHost) {
    L.push(`\n${hst} (${rs.length}):`);
    for (const r of rs.sort((a, b) => (b.updated || 0) - (a.updated || 0))) L.push("  " + line(r));
  }
  return [{ type: "text", text: L.join("\n") }];
}

async function toolFind({ handle }) {
  if (!handle) throw new Error("`handle` is required — a `host:name` (e.g. neo:regif), a bare name, or a fuzzy fragment.");
  const hits = resolve(await allRocks(), handle);
  if (!hits.length) return [{ type: "text", text: `no rock resolves «${handle}». Run prox_list to see what's live.` }];
  const L = [`«${handle}» → ${hits.length} match(es):`];
  for (const r of hits) {
    L.push(
      `\n${r.host}:${r.name}  ${r.self ? "(this machine)" : ""}`,
      `  status:  ${r.status}   kind: ${r.kind}   last active: ${age(r.updated)} ago`,
      `  subject: ${(r.subject || "").replace(/\s+/g, " ")}`,
      `  cwd:     ${r.cwd || "?"}`,
      `  id:      ${r.id}`,
      `  seed:    ${r.seed || "?"}   (re-render the same sigil anywhere)`,
    );
  }
  return [{ type: "text", text: L.join("\n") }];
}

async function toolPoke({ handle, by }) {
  if (!handle) throw new Error("`handle` is required (a `host:name` or fuzzy name; see prox_find).");
  const hits = resolve(await allRocks(), handle);
  if (!hits.length) throw new Error(`no rock resolves «${handle}» to poke.`);
  if (hits.length > 1) {
    return [{ type: "text", text: `«${handle}» is ambiguous (${hits.map((r) => `${r.host}:${r.name}`).join(", ")}). Poke a specific host:name.` }];
  }
  const r = hits[0];
  if (!r.ip) throw new Error(`no tailnet ip known for ${r.host} — can't reach its ledger server.`);
  const self = (await readJson(LOCAL_FILE))?.host || hostname().split(".")[0];
  const poker = by || `${self}:prox`;
  const body = JSON.stringify({ by: poker, id: r.id, name: r.name });
  const res = await fetch(`http://${r.ip}:${PORT}/poke`, {
    method: "POST",
    headers: { "content-type": "application/json", "content-length": Buffer.byteLength(body) },
    body,
    signal: AbortSignal.timeout(5000),
  }).catch((e) => { throw new Error(`poke to ${r.host} (${r.ip}) failed: ${e.message}`); });
  return [{ type: "text", text: `poked ${r.host}:${r.name} as «${poker}» — its rock should blink + rattle (HTTP ${res.status}).` }];
}

async function toolBindNotification({ handle, event = "imessage", wake = true }) {
  if (event !== "imessage") throw new Error("only the `imessage` Slab notification is supported");
  if (!handle) throw new Error("`handle` is required (use the stable host:name or session id)");
  const hits = resolve(await allRocks(), handle);
  if (!hits.length) throw new Error(`no rock resolves «${handle}» to bind.`);
  if (hits.length > 1) throw new Error(`«${handle}» is ambiguous (${hits.map((r) => `${r.host}:${r.name}`).join(", ")}).`);
  const r = hits[0];
  if (!r.self) throw new Error("iMessage notification wake targets must be a local prox on this machine");
  const binding = {
    event: "imessage",
    sessionId: r.id,
    host: r.host,
    name: r.name,
    wake: wake !== false,
    assignedAt: new Date().toISOString(),
  };
  await mkdir(join(homedir(), ".config", "slab"), { recursive: true });
  await writeFile(IMSG_BINDING, JSON.stringify(binding, null, 2) + "\n", { mode: 0o600 });
  return [{ type: "text", text: `bound Slab iMessage arrivals to ${r.host}:${r.name} (${r.id}) — poke${binding.wake ? " + reactivate" : " only"}.` }];
}

async function toolClose({ handle }) {
  if (!handle) throw new Error("`handle` is required (a `host:name` or fuzzy name; see prox_find).");
  const hits = resolve(await allRocks(), handle);
  if (!hits.length) throw new Error(`no rock resolves «${handle}» to close.`);
  if (hits.length > 1) {
    return [{ type: "text", text: `«${handle}» is ambiguous (${hits.map((r) => `${r.host}:${r.name}`).join(", ")}). Close a specific host:name.` }];
  }
  const r = hits[0];
  // Closing means killing a process + shutting its terminal window — only doable
  // on the machine that owns the window. Remote close would need a ledger
  // endpoint the menubar doesn't expose yet.
  if (!r.self) throw new Error(`${r.host}:${r.name} runs on another machine — prox_close only closes rocks on this machine (no remote-close endpoint yet). Run it from ${r.host}.`);
  const mk = await readMarker(r.id);
  const tty = mk?.tty || "";
  // Prefer the generic `agent_pid` (Codex + future agents); fall back to the
  // legacy `claude_pid` so existing Claude markers still close.
  const pid = mk?.agent_pid || mk?.claude_pid || 0;
  if (!tty && !pid) throw new Error(`no live tty/pid marker for ${r.host}:${r.name} (id ${r.id.slice(0, 8)}) — it may already be gone.`);
  // Never close the session that is asking.
  const anc = await ancestorPids(process.pid);
  if (pid && anc.has(pid)) throw new Error(`refusing to close ${r.host}:${r.name} — that is the session calling prox_close.`);
  const steps = [];
  // Graceful first, then force. claude traps SIGTERM, so don't wait long on it.
  if (pid && pidAlive(pid)) {
    try { process.kill(pid, "SIGTERM"); } catch {}
    for (let i = 0; i < 6 && pidAlive(pid); i++) await sleep(200);
    if (pidAlive(pid)) { try { process.kill(pid, "SIGKILL"); steps.push(`SIGKILL ${pid}`); } catch (e) { steps.push(`kill ${pid} failed: ${e.message}`); } }
    else steps.push(`terminated ${pid}`);
  } else if (pid) steps.push(`pid ${pid} already exited`);
  // Close the terminal window so no "[Process completed]" husk is left behind.
  if (tty) { const n = await closeTerminalTty(tty); steps.push(`closed ${n} window(s) on /dev/${tty}`); }
  return [{ type: "text", text: `closed ${r.host}:${r.name} — ${steps.join("; ")}.` }];
}

const TOOLS = [
  {
    name: "prox_list",
    description:
      "List the 'prompt rocks' across the slab fleet — every live agent session (Claude or Codex) and headless agent the menubar advertises, as host:name with its status (working/awaiting/complete/rendering/blank/interrupted), kind, owning agent, age, and a one-line subject. Use this to see what every machine is working on right now. Reads the local fleet ledger cache (no SSH).",
    inputSchema: {
      type: "object",
      properties: {
        host: { type: "string", description: "Only rocks on this machine (e.g. neo, blueberry, panda)." },
        status: { type: "string", description: "Filter by status: working | awaiting | complete | rendering | blank | interrupted." },
        kind: { type: "string", description: "Filter by kind: session | agent." },
        agent: { type: "string", description: "Filter by owning agent: claude | codex." },
      },
    },
  },
  {
    name: "prox_find",
    description:
      "Resolve a `machine:promptname` reference (e.g. neo:regif) — or a bare name / fuzzy fragment — to the exact session: its status, subject, working directory (cwd), session id, and sigil seed. This is how you turn a `host:name` handle someone mentions into what/where it actually is.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "`host:name` (e.g. neo:regif), a bare pet-name, or a fuzzy fragment of the name or subject." },
      },
      required: ["handle"],
    },
  },
  {
    name: "prox_poke",
    description:
      "Poke a prompt rock — send an attention beacon to the owning machine so its sigil blinks and rattles on that machine's overlay (a lightweight 'I'm looking at you' ping). Resolves the same host:name / fuzzy handle as prox_find; refuses ambiguous matches.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "`host:name` or a name that resolves to exactly one rock." },
        by: { type: "string", description: "Who is poking (shown on the target). Defaults to <thisHost>:rocks-mcp." },
      },
      required: ["handle"],
    },
  },
  {
    name: "prox_close",
    description:
      "Close a prompt rock — end that Claude session and shut its terminal window. Resolves a `host:name` / fuzzy handle (refuses ambiguous matches), ends the session (SIGTERM then SIGKILL — claude traps SIGTERM), and closes its Terminal.app window. DESTRUCTIVE: the running session is terminated (its transcript persists and is resumable). Only closes rocks on THIS machine (it needs the terminal window); refuses to close the calling session.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "`host:name` (e.g. neo:regif) or a name that resolves to exactly one rock on this machine." },
      },
      required: ["handle"],
    },
  },
  {
    name: "prox_bind_notification",
    description:
      "Assign a Slab notification to one stable local prox. For event `imessage`, every new inbound pokes the rock and, by default, reactivates its terminal session with a steering prompt. The binding uses session id, so it survives prompt/title/visual changes for that thread. This does not send or react to the incoming message.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "Stable local host:name, session id, or an unambiguous subject fragment." },
        event: { type: "string", enum: ["imessage"], default: "imessage" },
        wake: { type: "boolean", default: true, description: "Also reactivate the agent session; false means visual poke only." },
      },
      required: ["handle"],
    },
  },
];

async function callTool(name, args) {
  switch (name) {
    case "prox_list": return toolList(args || {});
    case "prox_find": return toolFind(args || {});
    case "prox_poke": return toolPoke(args || {});
    case "prox_bind_notification": return toolBindNotification(args || {});
    case "prox_close": return toolClose(args || {});
    default: throw new Error(`Unknown tool: ${name}`);
  }
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "prox-mcp", version: "1.0.0" },
          },
        };
      case "initialized":
      case "notifications/initialized":
        return null;
      case "ping":
        return { jsonrpc: "2.0", id, result: {} };
      case "tools/list":
        return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default:
        return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 7773);
if (port) serveHttp({ handleMessage, port, banner: "🪨 prox shared daemon" });
else serveStdio({ handleMessage, banner: "🪨 prox started (prox_list, prox_find, prox_poke, prox_close)" });
