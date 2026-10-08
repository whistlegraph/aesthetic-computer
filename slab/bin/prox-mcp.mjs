#!/usr/bin/env node
// prox-mcp.mjs — an MCP over the slab "prompt rocks" ledger, so any
// agent can LIST, FIND, POKE, and launch the little tumbling sigil stones the slab
// menubar parks over every live agent session across the fleet.
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
import { readFile, readdir, writeFile, mkdir, copyFile, chmod } from "node:fs/promises";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { join } from "node:path";
import { homedir, hostname } from "node:os";
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";
import { deliverLocal, drain, makeMessage, peek, stamp } from "./prox-inbox.mjs";
import { readLoopboyMode, writeLoopboyMode } from "../lib/loopboy-mode.mjs";
import { clip, toon } from "../../shared/toon.mjs";

const pexec = promisify(execFile);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const LEDGER_DIR = join(homedir(), ".config", "slab", "ledger");
const LOCAL_FILE = join(LEDGER_DIR, "local.json");
const PEERS_DIR = join(LEDGER_DIR, "peers");
const PORT = 5252; // the menubar's LedgerHTTPServer port on every machine
const LOOPBOY_CONFIG = join(homedir(), ".config", "slab", "loopboy.json");

// Per-session marker files (written by the slab claude hooks) carry the tty +
// pid a rock is running on — the same source the menubar overlay reads. Keyed
// by sessionId (== the ledger entry `id`), so prox_close can find the terminal.
const SLAB_HOME = process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab");
const MARKER_DIRS = [join(SLAB_HOME, "state", "active-prompts"), join(SLAB_HOME, "state", "awaiting-prompts")];

const shellQuote = (s) => `'${String(s).replaceAll("'", `'"'"'`)}'`;
// Aesel was Easel. Either spelling names the same interface, and inside prox it
// is "easel" until every writer has flipped (see aesel/src/slab-session.mjs):
// a menubar that predates the rename launches and lists only "easel".
const canonicalAgent = (agent) => (agent === "aesel" ? "easel" : agent);
const isCodexBacked = (agent) => agent === "codex" || canonicalAgent(agent) === "easel";

async function findFile(root, suffix) {
  let entries;
  try { entries = await readdir(root, { withFileTypes: true }); } catch { return null; }
  for (const entry of entries) {
    const path = join(root, entry.name);
    if (entry.isFile() && entry.name.endsWith(suffix)) return path;
    if (entry.isDirectory()) {
      const hit = await findFile(path, suffix);
      if (hit) return hit;
    }
  }
  return null;
}

async function transcriptFor(rock, marker) {
  if (marker?.transcript_path) return marker.transcript_path;
  const agent = canonicalAgent(marker?.agent_type || rock.agentType || "claude");
  const providerId = marker?.provider_session_id || marker?.codex_session_id || rock.id;
  if (isCodexBacked(agent)) {
    return findFile(join(homedir(), ".codex", "sessions"), `${providerId}.jsonl`);
  }
  return findFile(join(homedir(), ".claude", "projects"), `${rock.id}.jsonl`);
}

async function renderRockBundle(rock, bundle) {
  if (!rock.seed && !rock.creature) return false;
  const exporter = join(import.meta.dirname, "prox-sigil-export");
  try {
    const source = rock.creature ? join(bundle, "character.json") : rock.seed;
    await pexec(exporter, [source, bundle, "dark"], { timeout: 90_000 });
    return true;
  } catch { return false; }
}

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
      const mode = led.self ? await readLoopboyMode(e.id) : null;
      rows.push({ ...e, ...(mode ? { loopboyContact: mode.contact, name: mode.name || e.name } : {}),
        agentType: canonicalAgent(e.agentType), host: e.host || led.host, ip: led.ip, self: led.self });
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

// ── resolve a `host:name` / bare-name / fuzzy handle to rock rows ────────────
function resolve(rocks, handle) {
  if (!handle) return rocks;
  const h = handle.trim().toLowerCase();
  if (/^(prox:)?(easel|aesel):/.test(h)) {
    const parts = h.replace(/^prox:/, "").split(":").slice(1);
    if (parts.length < 1 || parts.length > 2 || parts.some(p => !p)) return [];
    const [host, name] = parts.length === 2 ? parts : [null, parts[0]];
    // Namespace references are exact. Colliding cached peer names remain
    // ambiguous; mutating callers already reject multiple matches.
    return rocks.filter(r => r.agentType === "easel" &&
      (!host || (host === "local" ? r.self : r.host.toLowerCase() === host)) &&
      (r.proxName || r.name).toLowerCase() === name);
  }
  let host = null;
  let name = h;
  if (h.includes(":")) {
    [host, name] = h.split(":", 2);
    // local remains a real scope; never match a same-named remote session.
  }
  const inHost = (r) => !host || (host === "local" ? r.self : r.host.toLowerCase() === host);
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
// Finished rocks that have not moved in a day are ledger residue, not fleet
// state. The default list hides them and says how many it hid; `all` shows
// every row and an explicit `status` filter is never second-guessed.
const STALE_MS = 24 * 3600 * 1000;
const FINISHED = new Set(["complete", "interrupted", "blank"]);
const ROCK_FIELDS = ["host", "name", "status", "kind", "age", "subject", "alias"];

async function toolList({ host, status, kind, agent, all } = {}) {
  let rocks = await allRocks();
  if (host) rocks = rocks.filter((r) => r.host.toLowerCase() === host.toLowerCase());
  if (status) rocks = rocks.filter((r) => r.status === status);
  if (kind) rocks = rocks.filter((r) => r.kind === kind);
  if (agent) rocks = rocks.filter((r) => (r.agentType || "claude").toLowerCase() === canonicalAgent(agent.toLowerCase()));
  let hidden = 0;
  if (!all && !status) {
    const now = Date.now();
    const live = rocks.filter((r) => !(FINISHED.has(r.status) && now - (r.updated || 0) > STALE_MS));
    hidden = rocks.length - live.length;
    rocks = live;
  }
  // self first, then by host, newest first within a host
  rocks.sort((a, b) => (a.self === b.self ? 0 : a.self ? -1 : 1) || a.host.localeCompare(b.host) || (b.updated || 0) - (a.updated || 0));
  const rows = rocks.map((r) => ({
    host: r.host,
    name: r.name,
    status: r.status,
    kind: r.agentType && r.agentType !== "claude" ? `${r.kind}·${r.agentType === "easel" ? "aesel" : r.agentType}` : r.kind,
    age: age(r.updated),
    subject: clip(r.subject, 64),
    alias: r.proxName ? `prox:aesel:${r.proxName}` : "",
  }));
  const notes = [];
  if (hidden) notes.push(`${hidden} finished rock(s) idle >24h hidden — all:true to include`);
  if (!rows.length && !hidden) notes.push("no rocks in the ledger — is SlabMenubar running? try again in a few seconds");
  notes.push("one rock in full: prox_find <host:name>");
  return [{ type: "text", text: toon("rocks", rows, ROCK_FIELDS, { note: notes.join(" · ") }) }];
}

async function toolFind({ handle }) {
  if (!handle) throw new Error("`handle` is required — a `host:name` (e.g. neo:regif), a bare name, or a fuzzy fragment.");
  const hits = resolve(await allRocks(), handle);
  if (!hits.length) return [{ type: "text", text: `no rock resolves «${handle}». Run prox_list to see what's live.` }];
  const L = [`«${handle}» → ${hits.length} match(es):`];
  for (const r of hits) {
    L.push(
      `\n${r.host}:${r.name}  ${r.self ? "(this machine)" : ""}`,
      ...(r.proxName ? [`  address: prox:aesel:${r.proxName} (scoped: prox:aesel:${r.host}:${r.proxName})`] : []),
      `  status:  ${r.status}   kind: ${r.kind}   last active: ${age(r.updated)} ago`,
      `  subject: ${(r.subject || "").replace(/\s+/g, " ")}`,
      `  cwd:     ${r.cwd || "?"}`,
      `  id:      ${r.id}`,
      `  seed:    ${r.seed || "?"}`,
      ...(r.creature ? [`  creature: ${r.creature.stage}; ${r.creature.traits.map(t => t.feature).join(", ") || "egg shell"} (prox_character for portable appearance)`] : []),
    );
  }
  return [{ type: "text", text: L.join("\n") }];
}

async function toolCharacter({ handle, destination } = {}) {
  if (!handle) throw new Error("`handle` is required.");
  const hits = resolve(await allRocks(), handle);
  if (hits.length !== 1) throw new Error(`Expected one prox for «${handle}»; found ${hits.length}. Use host:name.`);
  const r = hits[0];
  if (!r.creature) throw new Error("This prox has no saved creature yet; its owning Slab needs the creature update.");
  if (!destination) return [{ type: "text", text: JSON.stringify(r.creature, null, 2) }];
  const safeName = `${r.host}-${r.name}`.replace(/[^a-zA-Z0-9._-]+/g, "-");
  const out = join(String(destination), `${safeName}.creature`);
  await mkdir(out, { recursive: false, mode: 0o700 });
  await writeFile(join(out, "character.json"), JSON.stringify(r.creature, null, 2) + "\n", { mode: 0o600 });
  const rendered = await renderRockBundle(r, out);
  return [{ type: "text", text: `${out}\ncharacter.json${rendered ? " + sigil.png + sigil.gif" : " (image renderer unavailable)"}` }];
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

// ── inbox: hand a session words, not keystrokes ─────────────────────────────
// Same resolution as a poke. A local rock takes the line socket-first then
// file; a remote one gets it through its owner's /send, which does the same.
// ── who is calling ───────────────────────────────────────────────────────────
// prox runs as one shared HTTP daemon, so its own process.env says nothing
// about the caller. In order: the session id a harness forwards as a header;
// the process env, but only when we are a per-session stdio child; and, for
// Claude Code (which forwards nothing), the process that owns the caller's
// end of the loopback socket — its pid, or an ancestor's, is in a session marker.
const isLoopback = (a) => ["127.0.0.1", "::1", "::ffff:127.0.0.1"].includes(a);

async function markerPids() {
  const pids = new Map();
  for (const dir of MARKER_DIRS) {
    for (const name of await readdir(dir).catch(() => [])) {
      const m = await readJson(join(dir, name));
      const pid = Number(m?.agent_pid || m?.claude_pid || 0);
      if (pid > 0) pids.set(pid, name);
    }
  }
  return pids;
}

async function sessionByPeerPort(port) {
  const { stdout } = await pexec("lsof", ["-nP", "-a", `-iTCP:${port}`, "-sTCP:ESTABLISHED", "-Fp"], { timeout: 1500 }).catch(() => ({ stdout: "" }));
  const owners = stdout.split("\n").filter((l) => l.startsWith("p")).map((l) => Number(l.slice(1))).filter((p) => p > 0 && p !== process.pid);
  if (!owners.length) return null;
  const markers = await markerPids();
  for (const owner of owners) {
    let pid = owner;
    for (let hop = 0; hop < 8 && pid > 1; hop++) {
      if (markers.has(pid)) return markers.get(pid);
      const { stdout: ppid } = await pexec("ps", ["-o", "ppid=", "-p", String(pid)], { timeout: 1000 }).catch(() => ({ stdout: "" }));
      pid = Number(ppid.trim()) || 0;
    }
  }
  return null;
}

async function callerSessionId(context) {
  const header = context?.headers?.["x-slab-prompt-session-id"];
  if (typeof header === "string" && header) return header;
  if (!context) return process.env.AGENT_SESSION_ID || process.env.CLAUDE_SESSION_ID || process.env.SLAB_PROMPT_SESSION_ID || null;
  if (isLoopback(context.remoteAddress) && context.remotePort) return sessionByPeerPort(context.remotePort);
  return null;
}

// The caller's real `host:name`, or null when it can't be told (then the
// message goes out as `<host>:prox`, exactly as before).
async function callerHandle(context, rocks) {
  const id = await callerSessionId(context);
  const rock = id && rocks.find((r) => r.self && r.id === id);
  return rock ? `${rock.host}:${rock.name}` : null;
}

const candidates = (hits) => hits.map((r) => `${r.host}:${r.name} (${r.status}, ${age(r.updated)})`).join(", ");

async function toolSend({ handle, text, urgency = "queue", by }, context) {
  if (!handle) throw new Error("`handle` is required (a `host:name` or fuzzy name; see prox_find).");
  const rocks = await allRocks();
  const hits = resolve(rocks, handle);
  if (!hits.length) throw new Error(`no rock resolves «${handle}» to send to.`);
  if (hits.length > 1) throw new Error(`«${handle}» is ambiguous — nothing sent. Candidates: ${candidates(hits)}. Send to a specific host:name.`);
  const r = hits[0];
  const self = (await readJson(LOCAL_FILE))?.host || hostname().split(".")[0];
  const from = by || (await callerHandle(context, rocks)) || `${self}:prox`;
  const message = makeMessage({ from, to: `${r.host}:${r.name}`, toId: r.id, text, urgency });
  // What the sender learns: where it went, exactly how the receiver will read
  // it, and — for a file drop to a session that is not mid-turn — when.
  const receipt = (via, where) => {
    const idle = via === "file" && r.status !== "working"
      ? `\nnote: ${r.name} is ${r.status}; message queued until its next hook or prox_receive call. Terminal input is never submitted.` : "";
    return [{ type: "text", text: `sent to ${r.host}:${r.name} via ${where} as «${message.from}» (${urgency}, id ${message.id}).\nreceiver sees: ${clip(stamp(message), 400)}${idle}` }];
  };
  if (r.self) {
    const { via } = await deliverLocal(message);
    return receipt(via, via);
  }
  if (!r.ip) throw new Error(`no tailnet ip known for ${r.host} — can't reach its inbox.`);
  const body = JSON.stringify(message);
  const res = await fetch(`http://${r.ip}:${PORT}/send`, {
    method: "POST",
    headers: { "content-type": "application/json", "content-length": Buffer.byteLength(body) },
    body,
    signal: AbortSignal.timeout(5000),
  }).catch((e) => { throw new Error(`send to ${r.host} (${r.ip}) failed: ${e.message}`); });
  let result;
  try { result = await res.json(); } catch { throw new Error(`${r.host} returned an invalid /send response (HTTP ${res.status}).`); }
  if (!res.ok || !result.ok) throw new Error(`send to ${r.host}:${r.name} failed: ${result.error || `HTTP ${res.status}`}`);
  if (!["file", "socket"].includes(result.via) || result.id !== message.id) {
    throw new Error(`send to ${r.host}:${r.name} returned an invalid receipt; delivery is unconfirmed (id ${message.id}).`);
  }
  return receipt(result.via, `remote (${result.via || "?"} on ${r.host})`);
}

// Reading is local only — an inbox is private to the machine that owns the
// session. No handle means "my own", found through the session id the
// harness exports to its children.
async function toolInbox({ handle, consume = false } = {}, context) {
  let id;
  let label;
  if (handle) {
    const hits = resolve(await allRocks(), handle);
    if (!hits.length) throw new Error(`no rock resolves «${handle}».`);
    if (hits.length > 1) throw new Error(`«${handle}» is ambiguous (${hits.map((r) => `${r.host}:${r.name}`).join(", ")}).`);
    const r = hits[0];
    if (!r.self) throw new Error(`${r.host}:${r.name} runs on another machine — its inbox is only readable there.`);
    id = r.id; label = `${r.host}:${r.name}`;
  } else {
    id = await callerSessionId(context);
    if (!id) throw new Error("`handle` is required — can't tell which session is calling (no forwarded session id, env id, or session owning this connection).");
    label = `this session (${id.slice(0, 8)})`;
  }
  const messages = consume ? await drain(id) : await peek(id);
  if (!messages.length) return [{ type: "text", text: `inbox for ${label} is empty.` }];
  const head = `${messages.length} ${consume ? "drained" : "pending"} message(s) for ${label}:`;
  return [{ type: "text", text: [head, ...messages.map((m) => stamp(m))].join("\n") }];
}

// A waiting tool call is the passive delivery path for terminal agents.
// Its identity comes from the existing prox connection, not launch-only contact
// headers. Each poll rechecks mutable mode so an exit revokes an in-flight wait.
async function toolLoopboyWait({ contact = "", timeoutSeconds = 30 } = {}, context) {
  const id = await callerSessionId(context);
  if (!id) throw new Error("Cannot identify the calling prox; use prox_inbox with an explicit local handle.");
  const requested = String(contact || "").trim().toLowerCase();
  const seconds = Number(timeoutSeconds);
  if (!Number.isFinite(seconds) || seconds < 0 || seconds > 55) throw new Error("timeoutSeconds must be between 0 and 55");
  async function boundContact() {
    const mode = await readLoopboyMode(id);
    const marker = await readMarker(id);
    const key = mode ? mode.contact : marker?.loopboy_contact;
    const route = key && (await readJson(LOOPBOY_CONFIG))?.loops?.[key];
    if (!key || route?.sessionId !== id || (route.channel || route.event || "imessage") !== "imessage") {
      throw new Error("Loopboy mode is off or its route changed; the prox and conversation remain open.");
    }
    if (requested && requested !== key) throw new Error(`This prox is bound to ${key}, not ${requested}.`);
    return key;
  }
  const initial = await boundContact();
  const deadline = Date.now() + seconds * 1000;
  do {
    if (await boundContact() !== initial) throw new Error("Loopboy contact changed while waiting; start a new wait.");
    if ((await peek(id)).length) {
      const messages = await drain(id);
      if (messages.length) return [{ type: "text", text: messages.map(stamp).join("\n") }];
    }
    if (Date.now() >= deadline) break;
    await sleep(Math.min(250, deadline - Date.now()));
  } while (true);
  return [{ type: "text", text: `No queued updates for ${initial}. The same prox can call prox_loopboy_wait again; no terminal input was submitted.` }];
}

// Cached clients may still call prox_wake. Route through the same inbox,
// including on peers with an old menubar: never call the retired /wake route.
async function toolWake({ handle, prompt, by }, context) {
  const text = String(prompt || "").trim();
  if (!text) throw new Error("`prompt` is required.");
  if (text.length > 1000) throw new Error("`prompt` exceeds 1000 characters.");
  const result = await toolSend({ handle, text, by }, context);
  return [{ type: "text", text: "prox_wake is deprecated; delivered through the inbox only. No terminal input was submitted.\n" + result[0].text }];
}

// Asynchronous builds use the same data path as ordinary peer messages.
async function toolArtifactReady({ handle, artifacts, by }, context) {
  if (!Array.isArray(artifacts) || !artifacts.length || artifacts.length > 20
      || artifacts.some((path) => typeof path !== "string" || !path.trim())) {
    throw new Error("`artifacts` must contain 1–20 nonempty paths.");
  }
  return toolSend({ handle, by, text: `Artifacts ready:\n${artifacts.join("\n")}\nInspect the outputs, iterate if needed, and continue the original task.` }, context);
}

// Any agent can keep a tool call open for its own inbox. An idle terminal
// does not get resumed: its agent must call this or use its lifecycle hooks.
async function toolReceive({ timeoutSeconds = 30 } = {}, context) {
  const seconds = Number(timeoutSeconds);
  if (!Number.isFinite(seconds) || seconds < 0 || seconds > 55) {
    throw new Error("timeoutSeconds must be between 0 and 55");
  }
  const id = await callerSessionId(context);
  if (!id) throw new Error("Cannot identify the calling prox; forward x-slab-prompt-session-id or use prox_inbox with an explicit local handle.");
  const deadline = Date.now() + seconds * 1000;
  do {
    const messages = await drain(id);
    if (messages.length) return [{ type: "text", text: `${messages.length} received message(s):\n${messages.map((m) => stamp(m)).join("\n")}` }];
    if (Date.now() >= deadline) break;
    await sleep(Math.min(250, deadline - Date.now()));
  } while (true);
  return [{ type: "text", text: "No queued messages. Call prox_receive again when waiting for a peer; no terminal input was submitted." }];
}

async function toolDump({ handle, destination } = {}) {
  if (!handle) throw new Error("`handle` is required (a local `host:name` or fuzzy name; see prox_find).");
  const hits = resolve(await allRocks(), handle);
  if (!hits.length) throw new Error(`no rock resolves «${handle}» to dump.`);
  if (hits.length > 1) {
    return [{ type: "text", text: `«${handle}» is ambiguous (${hits.map((r) => `${r.host}:${r.name}`).join(", ")}). Dump a specific host:name.` }];
  }
  const r = hits[0];
  if (!r.self) throw new Error(`${r.host}:${r.name} runs on another machine — prox_dump is local-only so raw session state never crosses the ledger server. Run it from ${r.host}.`);
  const marker = await readMarker(r.id);
  if (!marker) throw new Error(`no local session marker remains for ${r.host}:${r.name}.`);
  const transcript = await transcriptFor(r, marker);
  if (!transcript) throw new Error(`could not locate the persisted transcript for ${r.host}:${r.name}.`);

  const agent = canonicalAgent(marker.agent_type || r.agentType || "claude");
  const providerId = marker.provider_session_id || marker.codex_session_id || r.id;
  const base = destination ? String(destination) : join(homedir(), "Desktop");
  const safeName = `${r.host}-${r.name}`.replace(/[^a-zA-Z0-9._-]+/g, "-");
  const out = join(base, `${safeName}.prox`);
  await mkdir(out, { recursive: false, mode: 0o700 });
  await copyFile(transcript, join(out, "transcript.jsonl"));
  await chmod(join(out, "transcript.jsonl"), 0o600);
  const manifest = {
    format: "computer.aesthetic.prox-dump/v1",
    dumpedAt: new Date().toISOString(),
    host: r.host, name: r.name, sessionId: r.id, providerSessionId: providerId,
    agent, cwd: marker.cwd || r.cwd || "", subject: marker.subject || r.subject || "",
    seed: r.seed || "", status: r.status || marker.state || "",
    ...(r.creature ? { creature: r.creature } : {}),
  };
  await writeFile(join(out, "manifest.json"), JSON.stringify(manifest, null, 2) + "\n", { mode: 0o600 });
  if (r.creature) await writeFile(join(out, "character.json"), JSON.stringify(r.creature, null, 2) + "\n", { mode: 0o600 });

  let resume;
  if (agent === "easel") {
    resume = `#!/bin/sh
set -eu
bundle=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
day=$(date +%Y/%m/%d)
store="$HOME/.codex/sessions/$day"
mkdir -p "$store"
cp "$bundle/transcript.jsonl" "$store/rollout-prox-${providerId}.jsonl"
cd ${shellQuote(marker.cwd || r.cwd || homedir())} 2>/dev/null || cd "$HOME"
exec aesthetic --resume ${shellQuote(providerId)}
`;
  } else if (agent === "codex") {
    resume = `#!/bin/sh
set -eu
bundle=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
day=$(date +%Y/%m/%d)
store="$HOME/.codex/sessions/$day"
mkdir -p "$store"
cp "$bundle/transcript.jsonl" "$store/rollout-prox-${providerId}.jsonl"
cd ${shellQuote(marker.cwd || r.cwd || homedir())} 2>/dev/null || cd "$HOME"
exec codex resume ${shellQuote(providerId)}
`;
  } else {
    resume = `#!/bin/sh
set -eu
bundle=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
project=${shellQuote((marker.cwd || r.cwd || homedir()).replaceAll("/", "-") || "-")}
store="$HOME/.claude/projects/$project"
mkdir -p "$store"
cp "$bundle/transcript.jsonl" "$store/${r.id}.jsonl"
cd ${shellQuote(marker.cwd || r.cwd || homedir())} 2>/dev/null || cd "$HOME"
exec claude --resume ${shellQuote(r.id)}
`;
  }
  if (r.creature && /^[0-9a-f]{16}$/.test(r.creature.seed)) {
    // A character travels with the private session bundle, too. Never replace
    // a creature that has already grown further on the destination machine.
    const restore = `\nmkdir -p "$HOME/.config/slab/creatures"\ncp -n "$bundle/character.json" "$HOME/.config/slab/creatures/${r.creature.seed}.json"\n`;
    resume = resume.replace(/\nexec /, `${restore}\nexec `);
  }
  await writeFile(join(out, "resume.sh"), resume, { mode: 0o700 });
  await chmod(join(out, "resume.sh"), 0o700);
  await writeFile(join(out, "README.txt"),
    `Portable prox state for ${r.host}:${r.name}\n\nRun ./resume.sh to install the transcript into ${agent}'s native session store and resume it.\nThe animated sigil.gif is rendered from the saved appearance.\nThis bundle contains raw private agent history, including tool results and local paths. Do not publish it. Use prox_character to export appearance alone.\n`,
    { mode: 0o600 });
  const rendered = await renderRockBundle(r, out);
  return [{ type: "text", text: `dumped ${r.host}:${r.name} → ${out}\nagent: ${agent}\nresume id: ${providerId}\nrock: ${rendered ? "animated exact-model sigil.gif + Finder icon" : "renderer unavailable; state bundle is still complete"}\nprivate raw transcript included; move the .prox folder as one bundle.` }];
}

async function toolLaunch({ host, agent, cwd, prompt = "", by, loopboyContact = "" }) {
  const wanted = String(host || "").trim().toLowerCase().replace(/\.local$/, "");
  if (!wanted) throw new Error("`host` is required (for example, poorslice).");
  const agentName = canonicalAgent(String(agent || "").trim().toLowerCase());
  if (!new Set(["claude", "codex", "easel"]).has(agentName)) {
    throw new Error("`agent` must be `claude`, `codex`, or `aesel` (or `easel`).");
  }
  if (String(prompt).length > 4000) throw new Error("`prompt` exceeds 4000 characters.");
  const contactKey = String(loopboyContact || "").trim().toLowerCase();
  if (contactKey && !/^[a-z0-9_-]{1,40}$/.test(contactKey)) throw new Error("Invalid Loopboy contact key");
  if (contactKey) {
    const previous = (await readJson(LOOPBOY_CONFIG))?.loops?.[contactKey];
    const marker = previous?.sessionId ? await readMarker(previous.sessionId) : null;
    const pid = Number(marker?.agent_pid || marker?.claude_pid || 0);
    if (pid && pidAlive(pid)) throw new Error(`${contactKey} already has a live prox (${previous.sessionId}); change its mode with prox_bind_notification/prox_unbind_notification, without launching a replacement.`);
  }


  // Snapshot the live markers before launching so the poll below can tell
  // the newborn Loopboy's marker apart from every session already running.
  const existingMarkerIds = new Set();
  if (contactKey) {
    for (const dir of MARKER_DIRS) {
      let names = [];
      try { names = await readdir(dir); } catch {}
      for (const name of names) {
        const value = await readJson(join(dir, name));
        existingMarkerIds.add(value?.session_id || name);
      }
    }
  }

  const ledgers = await allLedgers();
  const target = ledgers.find((l) => String(l.host || "").toLowerCase() === wanted);
  if (!target) throw new Error(`no cached ledger for host «${host}» — it must be online in prox first.`);
  if (contactKey && !target.self) throw new Error("Loopboy routes must be configured on the owning host; use prox_bind_notification there for an existing session.");
  if (!target.ip) throw new Error(`no tailnet IP known for ${target.host}.`);
  const self = (await readJson(LOCAL_FILE))?.host || hostname().split(".")[0];
  const launcher = by || `${self}:prox`;
  const body = JSON.stringify({
    agent: agentName,
    prompt: String(prompt),
    ...(cwd ? { cwd: String(cwd) } : {}),
    ...(contactKey ? { loopboyContact: contactKey } : {}),
    by: launcher,
  });
  const res = await fetch(`http://${target.ip}:${PORT}/launch`, {
    method: "POST",
    headers: { "content-type": "application/json", "content-length": Buffer.byteLength(body) },
    body,
    signal: AbortSignal.timeout(8000),
  }).catch((e) => { throw new Error(`launch on ${target.host} (${target.ip}) failed: ${e.message}`); });
  const text = await res.text();
  let result;
  try { result = JSON.parse(text); } catch { throw new Error(`launch on ${target.host} returned invalid JSON (HTTP ${res.status}).`); }
  if (!res.ok || !result.ok) throw new Error(`launch on ${target.host} failed: ${result.error || `HTTP ${res.status}`}`);
  let binding = "";
  if (contactKey) {
    if (String(target.host).toLowerCase() !== String(self).toLowerCase()) {
      throw new Error("Loopboy contact routes can only be launched on this local iMessage host");
    }
    // Loopboys live on the real Terminal PTY now — the ledger stopped
    // wrapping them in GNU Screen (focus-report sequences leaked into the
    // agent as literal input) and returns `nudgeScreen` as an empty string.
    // The field's PRESENCE is still the version handshake with the prompt
    // host; the marker poll below is the actual proof of launch.
    if (!("nudgeScreen" in result)) {
      throw new Error("Loopboy launch reply is missing nudgeScreen; prompt host needs the updated Slab build");
    }
    let marker = null;
    for (let attempt = 0; attempt < 20 && !marker; attempt++) {
      for (const dir of MARKER_DIRS) {
        let names = [];
        try { names = await readdir(dir); } catch {}
        for (const name of names) {
          const value = await readJson(join(dir, name));
          const id = value?.session_id || name;
          if (!existingMarkerIds.has(id) && value?.loopboy_contact === contactKey) {
            marker = { id: value.session_id || name, value };
            break;
          }
        }
        if (marker) break;
      }
      if (!marker) await sleep(250);
    }
    if (!marker) throw new Error("Loopboy launched but its live marker did not appear");
    await mkdir(join(homedir(), ".config", "slab"), { recursive: true });
    const cfg = (await readJson(LOOPBOY_CONFIG)) || { version: 1, loops: {} };
    cfg.version = 1; cfg.loops ||= {};
    cfg.loops[contactKey] = {
      event: "imessage", contact: contactKey,
      sessionId: marker.id, host: result.host || target.host,
      agent: agentName, wake: false, delivery: "inbox", assignedAt: new Date().toISOString(),
    };
    await writeFile(LOOPBOY_CONFIG, JSON.stringify(cfg, null, 2) + "\n", { mode: 0o600 });
    binding = ` and bound Loopboy contact ${contactKey}`;
  }
  return [{
    type: "text",
    text: `launched ${agentName} on ${result.host || target.host} in ${result.cwd} as «${launcher}»${prompt ? " with an initial prompt" : ""}${binding}.`,
  }];
}

async function toolJob({ host, job = "mediascholar", action = "status" }) {
  const wanted = String(host || "").trim().toLowerCase().replace(/\.local$/, "");
  if (!wanted) throw new Error("`host` is required (for example, jasellite).");
  const jobName = String(job || "").trim().toLowerCase();
  if (jobName !== "mediascholar") throw new Error("`job` must be `mediascholar`.");
  const jobAction = String(action || "status").trim().toLowerCase();
  if (!new Set(["start", "status", "cancel"]).has(jobAction)) {
    throw new Error("`action` must be `start`, `status`, or `cancel`.");
  }
  const ledgers = await allLedgers();
  const target = ledgers.find((ledger) => String(ledger.host || "").toLowerCase() === wanted);
  if (!target) throw new Error(`no cached ledger for host «${host}» — its headless Prox must be online first.`);
  if (!target.ip) throw new Error(`no tailnet IP known for ${target.host}.`);
  const body = JSON.stringify({ job: jobName, action: jobAction });
  const response = await fetch(`http://${target.ip}:${PORT}/job`, {
    method: "POST",
    headers: { "content-type": "application/json", "content-length": Buffer.byteLength(body) },
    body,
    signal: AbortSignal.timeout(20_000),
  }).catch((error) => {
    throw new Error(`${jobAction} ${jobName} on ${target.host} (${target.ip}) failed: ${error.message}`);
  });
  let result;
  try { result = await response.json(); }
  catch { throw new Error(`${target.host} returned an invalid job response (HTTP ${response.status}).`); }
  if (!response.ok || !result.ok) {
    throw new Error(`${jobAction} ${jobName} on ${target.host} failed: ${result.error || `HTTP ${response.status}`}`);
  }
  const detail = result.properties
    ? Object.entries(result.properties).map(([key, value]) => `${key}=${value}`).join(" ")
    : `state=${result.state || "ok"}`;
  return [{ type: "text", text: `${target.host}:${jobName} ${jobAction} — ${detail}` }];
}

// Loopboy is a mutable mode of an existing prox, for every agent type.
async function bindingTarget(handle) {
  if (!handle) throw new Error("`handle` is required (the stable host:name or session id)");
  const hits = resolve(await allRocks(), handle);
  if (!hits.length) throw new Error(`no rock resolves «${handle}».`);
  if (hits.length > 1) throw new Error(`«${handle}» is ambiguous (${candidates(hits)}).`);
  const r = hits[0];
  if (!r.self) throw new Error(`Run this mode change on ${r.host}; Loopboy routes belong to the owning host.`);
  const path = join(MARKER_DIRS[0], r.id);
  const marker = await readJson(path);
  if (!marker || marker.session_id !== r.id) throw new Error(`${r.host}:${r.name} has no live session marker.`);
  const pid = Number(marker.agent_pid || marker.claude_pid || 0);
  if (!pid || !pidAlive(pid)) throw new Error(`${r.host}:${r.name} marker points at a dead process (pid ${pid || "?"}).`);
  return { r, path, marker };
}

async function toolBindNotification({ handle, contact, event = "imessage", adopt = true, name = "" }) {
  if (event !== "imessage") throw new Error("only the `imessage` Slab notification is supported");
  const contactKey = String(contact || "").trim().toLowerCase();
  if (!/^[a-z0-9_-]{1,40}$/.test(contactKey)) throw new Error("`contact` must be a short contact key from ~/.config/slab/imsg.json");
  const petName = String(name || "").trim().toLowerCase();
  if (petName && !/^[a-z][a-z0-9_-]{1,15}$/.test(petName)) throw new Error("`name` must be a short lowercase pet name (2–16 letters, digits, - or _)");
  const { r, path, marker } = await bindingTarget(handle);
  const mode = await readLoopboyMode(r.id);
  const boundContact = mode ? mode.contact : String(marker.loopboy_contact || "").toLowerCase();
  if (boundContact && boundContact !== contactKey) throw new Error(`${r.host}:${r.name} is bound to ${boundContact}; use prox_unbind_notification before changing contact.`);
  if (!boundContact && !adopt) throw new Error("Pass adopt=true to enter Loopboy mode in place. No relaunch is needed.");
  const cfg = (await readJson(LOOPBOY_CONFIG)) || { version: 1, loops: {} };
  cfg.version = 1; cfg.loops ||= {};
  const previous = cfg.loops[contactKey];
  if (previous?.sessionId && previous.sessionId !== r.id) {
    const other = await readMarker(previous.sessionId);
    const pid = Number(other?.agent_pid || other?.claude_pid || 0);
    if (pid && pidAlive(pid)) throw new Error(`${contactKey} already has a live listener (${previous.sessionId}); unbind it before assigning another prox.`);
  }
  // Do not carry autoRespond or legacy wake authorization into the new mode.
  const loop = { event, channel: event, contact: contactKey, sessionId: r.id,
    host: r.host, name: petName || r.name, agent: r.agentType || "claude",
    wake: false, delivery: "inbox", assignedAt: new Date().toISOString() };
  await writeLoopboyMode(r.id, { contact: contactKey, name: loop.name });
  // Mirror for older renderers; the mode record wins over stale watcher writes.
  const latest = await readJson(path);
  if (latest) await writeFile(path, JSON.stringify({ ...latest, loopboy_contact: contactKey }) + "\n");
  cfg.loops[contactKey] = loop;
  await mkdir(join(homedir(), ".config", "slab"), { recursive: true });
  await writeFile(LOOPBOY_CONFIG, JSON.stringify(cfg, null, 2) + "\n", { mode: 0o600 });
  return [{ type: "text", text: `Loopboy bound ${contactKey} → ${r.host}:${loop.name} (${r.id}) in place — inbox + visual poke; no automatic typing. Same process and conversation.` }];
}

async function toolUnbindNotification({ handle }) {
  const { r, path } = await bindingTarget(handle);
  const cfg = (await readJson(LOOPBOY_CONFIG)) || { version: 1, loops: {} };
  // Persist OFF before removing the route: old launch env/headers cannot rearm it.
  await writeLoopboyMode(r.id, { contact: "", name: r.name });
  const marker = await readJson(path);
  if (marker) await writeFile(path, JSON.stringify({ ...marker,
    loopboy_contact: "", loopboy_state: "", loopboy_response: "" }) + "\n");
  for (const [contact, loop] of Object.entries(cfg.loops || {})) {
    if (loop.sessionId === r.id) delete cfg.loops[contact];
  }
  await mkdir(join(homedir(), ".config", "slab"), { recursive: true });
  await writeFile(LOOPBOY_CONFIG, JSON.stringify(cfg, null, 2) + "\n", { mode: 0o600 });
  return [{ type: "text", text: `Loopboy mode off for ${r.host}:${r.name} (${r.id}). Same process, window, name, and conversation.` }];
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
    name: "prox_character",
    description: "Read a prox's versioned egg-creature appearance, growth stage, and acquired features. With destination, export character.json and transparent PNG/animated GIF to a new .creature directory for reuse elsewhere. Includes no transcript, subject, memoir, or local paths. Reads the cached ledger; never triggers inference or a poke.",
    inputSchema: { type: "object", properties: {
      handle: { type: "string", description: "Exactly one prox, usually host:name." },
      destination: { type: "string", description: "Existing local parent directory for a new <host>-<name>.creature bundle. Omit to read JSON only." },
    }, required: ["handle"] },
  },
  {
    name: "prox_list",
    description:
      "List the prompt rocks across the Slab fleet — every live Claude, Codex, or Easel session and headless agent the menubar advertises — as one compact table: host, name, status, kind, age, subject, alias. Finished rocks idle for more than a day are hidden by default (the footer counts them). Reads the local fleet ledger cache (no SSH).",
    inputSchema: {
      type: "object",
      properties: {
        host: { type: "string", description: "Only rocks on this machine (e.g. neo, blueberry, panda)." },
        status: { type: "string", description: "Filter by status: working | awaiting | complete | rendering | blank | interrupted. Disables the stale-rock hiding." },
        kind: { type: "string", description: "Filter by kind: session | agent." },
        agent: { type: "string", description: "Filter by owning interface: claude | codex | aesel (easel is the same)." },
        all: { type: "boolean", description: "Include finished rocks idle for more than 24h (hidden by default)." },
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
    name: "prox_send",
    description:
      "Send a text message to a prompt rock's inbox — the session receives it through prox_receive, its next lifecycle hook, or its inbox socket. A file receipt confirms queued delivery, not that the agent has read it. Wait for an explicit reply when acknowledgement matters. No keystrokes are injected. Resolves the same host:name / fuzzy handle as prox_poke and refuses ambiguous matches; a rock on another machine is reached through its owner's ledger server.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "`host:name` or a name that resolves to exactly one rock." },
        text: { type: "string", description: "The message, at most 8000 characters." },
        urgency: { type: "string", enum: ["queue", "urgent"], default: "queue", description: "`queue` waits for the next turn boundary; `urgent` lets a socket-listening harness interrupt its turn." },
        by: { type: "string", description: "Sender shown to the receiver as host:name. Defaults to the calling session's own host:name (resolved from the connection), else <thisHost>:prox." },
      },
      required: ["handle", "text"],
    },
  },
  {
    name: "prox_inbox",
    description:
      "Read a local prompt rock's pending inbox messages. Peeks by default; consume=true drains them (they move to the inbox log). Without a handle, reads the calling session's own inbox via AGENT_SESSION_ID / CLAUDE_SESSION_ID. Local machine only.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "A local `host:name`, session id, or unambiguous fuzzy name. Omit for this session's own inbox." },
        consume: { type: "boolean", default: false, description: "Drain the messages instead of peeking." },
      },
    },
  },
  {
    name: "prox_artifact_ready",
    description: "Queue completed artifact paths and a continuation message in a session's Slab inbox. Never types, focuses, or resumes a terminal. Receipt confirms delivery to the inbox, not that the agent has read it.",
    inputSchema: { type: "object", properties: {
      handle: { type: "string", description: "A stable host:name or session id." },
      artifacts: { type: "array", items: { type: "string" }, minItems: 1, maxItems: 20 },
      by: { type: "string", description: "Optional sender label." },
    }, required: ["handle", "artifacts"] },
  },
  {
    name: "prox_receive",
    description: "Wait for peer messages in the calling session's own Slab inbox, then consume and return them through this tool call. Works across agents and fleet hosts via prox_send. Use while waiting for a peer. No keyboard, focus changes, terminal input, or session resume. Timeout leaves messages queued for the next receive or lifecycle hook.",
    inputSchema: { type: "object", properties: {
      timeoutSeconds: { type: "number", default: 30, minimum: 0, maximum: 55 },
    } },
  },
  {
    name: "prox_wake",
    description:
      "Deprecated compatibility alias for prox_send. Queues the prompt in the session inbox only; never focuses, types, presses Return, or resumes a terminal. Use prox_send to send and prox_receive to wait for messages.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "A host:name, session id, or prox:aesel:name (prox:easel:name) resolving to exactly one rock." },
        prompt: { type: "string", description: "Continuation prompt, at most 1000 characters." },
        by: { type: "string", description: "Optional caller label recorded by the target." },
      },
      required: ["handle", "prompt"],
    },
  },
  {
    name: "prox_close",
    description:
      "Close a prompt rock — end that agent session and shut its terminal window. Resolves a `host:name` / fuzzy handle (refuses ambiguous matches), ends the session, and closes its Terminal.app window. DESTRUCTIVE: the running session is terminated; its transcript remains resumable. Only closes rocks on this machine and refuses to close the calling session.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "`host:name` (e.g. neo:regif) or a name that resolves to exactly one rock on this machine." },
      },
      required: ["handle"],
    },
  },
  {
    name: "prox_dump",
    description:
      "Export one local prompt rock as a portable, resumable private bundle. Copies the raw native transcript plus session/cwd metadata and writes a resume.sh installer. Defaults to ~/Desktop/<host>-<name>.prox. Raw transcripts can contain tool output and local paths, so the bundle must remain private. Local machine only; no transcript data is sent over the fleet ledger.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "A local host:name, session id, or unambiguous fuzzy name." },
        destination: { type: "string", description: "Optional existing destination directory. Defaults to ~/Desktop." },
      },
      required: ["handle"],
    },
  },
  {
    name: "prox_launch",
    description:
      "Launch a new interactive Claude, Codex, or Easel prompt in Terminal.app on a Slab fleet host. SIDE EFFECT: opens a live agent session and may consume account usage. The target accepts only fixed allowlisted launchers, limits cwd to that user's home folder, and binds the endpoint to its tailnet IP; no arbitrary command is accepted.",
    inputSchema: {
      type: "object",
      properties: {
        host: { type: "string", description: "Target Slab hostname, for example poorslice." },
        agent: { type: "string", enum: ["claude", "codex", "aesel", "easel"], description: "Interface to launch (aesel and easel are the same)." },
        cwd: { type: "string", description: "Optional absolute directory on the target. Defaults to its aesthetic-computer checkout and must stay under its home folder." },
        prompt: { type: "string", description: "Optional initial prompt, at most 4000 characters. Omit to open an idle TUI." },
        by: { type: "string", description: "Optional caller label recorded by the target." },
        loopboyContact: { type: "string", description: "Optional contact key for a NEW prox. To convert an existing prox, use prox_bind_notification; it preserves the conversation." },
      },
      required: ["host", "agent"],
    },
  },
  {
    name: "prox_job",
    description:
      "Start, inspect, or cancel one allowlisted headless job on a Linux Prox host. The remote endpoint accepts only a fixed job name mapped to a systemd user unit; it never accepts command text, paths, prompts, environment variables, or arbitrary executables. Starting may consume provider usage; cancel stops the named unit.",
    inputSchema: {
      type: "object",
      properties: {
        host: { type: "string", description: "Headless Prox hostname, for example jasellite." },
        job: { type: "string", enum: ["mediascholar"], default: "mediascholar" },
        action: { type: "string", enum: ["start", "status", "cancel"], default: "status" },
      },
      required: ["host"],
    },
  },
  {
    name: "prox_bind_notification",
    description:
      "Enter Loopboy mode on an existing local Claude, Codex, or Aesel prox. Preserves its process, window, name, and conversation. Arrivals use inbox + visual poke, never typing or automatic replies. Use prox_unbind_notification to leave the mode.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "Stable local host:name, session id, or an unambiguous subject fragment." },
        contact: { type: "string", description: "Contact key from ~/.config/slab/imsg.json, for example alex." },
        event: { type: "string", enum: ["imessage"], default: "imessage" },
        wake: { type: "boolean", default: false, description: "Deprecated compatibility field. Automatic typing is always disabled, even when true." },
        adopt: { type: "boolean", default: true, description: "Enter Loopboy mode in place on any supported agent. No relaunch or launch-time contact headers required." },
        name: { type: "string", description: "Optional pet name for the Loopboy, for example surizo. Defaults to the rock's current name." },
      },
      required: ["handle", "contact"],
    },
  },
  {
    name: "prox_loopboy_wait",
    description: "Wait for inbox updates inside the current Loopboy session, without typing, focus changes, resumes, or a new prox. Mode and contact are checked on every poll; leaving Loopboy ends the wait. No launch-time contact headers required.",
    inputSchema: { type: "object", properties: {
      contact: { type: "string", description: "Optional expected contact key; a different contact is refused." },
      timeoutSeconds: { type: "number", default: 30, minimum: 0, maximum: 55 },
    } },
  },
  {
    name: "prox_unbind_notification",
    description: "Leave Loopboy mode in place. Keeps the same prox process, window, name, and conversation. Clears the contact route and prevents stale launch settings from re-enabling it.",
    inputSchema: { type: "object", properties: {
      handle: { type: "string", description: "Stable local host:name or session id." },
    }, required: ["handle"] },
  },
];

async function callTool(name, args, context) {
  switch (name) {
    case "prox_list": return toolList(args || {});
    case "prox_find": return toolFind(args || {});
    case "prox_character": return toolCharacter(args || {});
    case "prox_poke": return toolPoke(args || {});
    case "prox_send": return toolSend(args || {}, context);
    case "prox_inbox": return toolInbox(args || {}, context);
    case "prox_wake": return toolWake(args || {}, context);
    case "prox_receive": return toolReceive(args || {}, context);
    case "prox_artifact_ready": return toolArtifactReady(args || {}, context);
    case "prox_launch": return toolLaunch(args || {});
    case "prox_job": return toolJob(args || {});
    case "prox_bind_notification": return toolBindNotification(args || {});
    case "prox_unbind_notification": return toolUnbindNotification(args || {});
    case "prox_loopboy_wait": return toolLoopboyWait(args || {}, context);
    case "prox_close": return toolClose(args || {});
    case "prox_dump": return toolDump(args || {});
    default: throw new Error(`Unknown tool: ${name}`);
  }
}

async function handleMessage(message, context) {
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
        const content = await callTool(params?.name, params?.arguments, context);
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
else serveStdio({ handleMessage, banner: "🪨 prox started (prox_list, prox_find, prox_poke, prox_send, prox_inbox, prox_receive, prox_launch, prox_job, prox_close, prox_dump)" });
