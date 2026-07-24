#!/usr/bin/env node
// prox-mcp.mjs — an MCP over the slab "prompt rocks" ledger, so any
// agent can LIST, FIND, POKE, WAKE, and launch the little tumbling sigil stones the slab
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
// kind,seed,cwd,started,updated,memoir}]}. Reads are O(1) local file loads (the menubar keeps
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
import { boundedNudge, makeIrisContact, parseAgentAddress } from "../lib/loopboy-family.mjs";
import { enqueueLoopboyEvent, waitLoopboyEvent } from "../lib/loopboy-inbox.mjs";
import { authorizeLoopboyWait } from "../lib/loopboy-request-auth.mjs";
import {
  RUNNING_STATUSES,
  actionableTarget,
  canonicalHandle,
  duplicateReport,
  isoTime,
  ledgerFreshness,
  resolveRocks,
} from "../lib/prox-resolver.mjs";

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

// ── load the fleet ledger off disk ──────────────────────────────────────────
async function readJson(path) {
  try {
    return JSON.parse(await readFile(path, "utf8"));
  } catch {
    return null;
  }
}

async function loopboyContactsBySession() {
  const cfg = await readJson(LOOPBOY_CONFIG);
  const contacts = new Map();
  for (const [contact, loop] of Object.entries(cfg?.loops || {})) {
    if (loop?.sessionId) contacts.set(String(loop.sessionId), contact);
  }
  return contacts;
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
async function allRocks(ledgers = null) {
  const rows = [];
  const loopboyContacts = await loopboyContactsBySession();
  for (const led of ledgers || await allLedgers()) {
    for (const e of led.entries || []) {
      rows.push({
        ...e,
        host: e.host || led.host,
        ip: led.ip,
        self: led.self,
        ledgerUpdatedAt: led.updatedAt,
        loopboyContact: e.loopboyContact || (led.self ? loopboyContacts.get(e.id) || "" : ""),
      });
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

function timeAndAge(ms) {
  return `${isoTime(ms)} (${age(ms)} ago)`;
}

function line(r, now = Date.now()) {
  const mark = STATUS_MARK[r.status] || "•";
  const subj = (r.subject || "").replace(/\s+/g, " ").slice(0, 64);
  // Tag the owning agent when it isn't the default (Claude), so a mixed
  // fleet reads clearly: "session·codex".
  const agent = r.agentType && r.agentType !== "claude" ? `·${r.agentType}` : "";
  const loopboy = r.loopboyContact ? `·loopboy:${r.loopboyContact}` : "";
  const up = r.started ? `  ·up ${age(r.started)}` : "";
  const fresh = ledgerFreshness(r.ledgerUpdatedAt, now).state;
  return `${mark} ${canonicalHandle(r)}  [${r.status}] ${r.kind}${agent}${loopboy}${up}  ·active ${timeAndAge(r.updated)}  ·ledger ${fresh}  ${subj}`;
}

// ── resolve a `host:name` / bare-name / fuzzy handle to rock rows ────────────
function duplicateSummary(report) {
  return `duplicate check: ids=${report.ids.length}, host:name aliases=${report.hostNames.length}, fleet pet names=${report.names.length}`;
}

function duplicateLines(report) {
  const lines = [duplicateSummary(report)];
  for (const [label, groups] of [["id", report.ids], ["host:name", report.hostNames], ["pet name", report.names]]) {
    for (const group of groups.slice(0, 10)) {
      lines.push(`  duplicate ${label} «${group.key}»: ${group.matches.map(canonicalHandle).join(", ")}`);
    }
    if (groups.length > 10) lines.push(`  … ${groups.length - 10} more duplicate ${label} group(s)`);
  }
  return lines;
}

function actionResolution(rocks, handle, verb, now = Date.now()) {
  return { ...actionableTarget(resolveRocks(rocks, handle), { now, verb }), resolvedAt: now };
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
  const now = Date.now();
  let ledgers = await allLedgers();
  if (host) ledgers = ledgers.filter((l) => String(l.host).toLowerCase() === host.toLowerCase());
  const all = await allRocks(ledgers);
  const duplicates = duplicateReport(all);
  let rocks = all;
  if (host) rocks = rocks.filter((r) => r.host.toLowerCase() === host.toLowerCase());
  if (status) rocks = rocks.filter((r) => r.status === status);
  if (kind) rocks = rocks.filter((r) => r.kind === kind);
  if (agent) rocks = rocks.filter((r) => (r.agentType || "claude").toLowerCase() === agent.toLowerCase());
  // group by host, self first
  rocks.sort((a, b) => (a.self === b.self ? a.host.localeCompare(b.host) : a.self ? -1 : 1) || 0);
  const byHost = new Map();
  for (const r of rocks) (byHost.get(r.host) || byHost.set(r.host, []).get(r.host)).push(r);
  const L = [
    `checked_at: ${isoTime(now)}`,
    `ledger snapshots (${ledgers.length}):`,
    ...ledgers.map((l) => {
      const fresh = ledgerFreshness(l.updatedAt, now);
      return `  ${l.host}: ${isoTime(l.updatedAt)} (${age(l.updatedAt)} ago, ${fresh.state}), ${(l.entries || []).length} rock(s)`;
    }),
    `${rocks.length} prompt rock(s) across ${byHost.size} machine(s):`,
    ...duplicateLines(duplicates),
  ];
  if (!rocks.length) L.push("(no prompt rocks match)");
  for (const [hst, rs] of byHost) {
    const ledgerUpdatedAt = rs[0]?.ledgerUpdatedAt;
    const fresh = ledgerFreshness(ledgerUpdatedAt, now);
    L.push(`\n${hst} (${rs.length}) · ledger ${isoTime(ledgerUpdatedAt)} (${age(ledgerUpdatedAt)} ago, ${fresh.state}):`);
    for (const r of rs.sort((a, b) => (b.updated || 0) - (a.updated || 0))) L.push("  " + line(r, now));
  }
  return [{ type: "text", text: L.join("\n") }];
}

async function toolFind({ handle }) {
  if (!handle) throw new Error("`handle` is required — a `host:name` (e.g. neo:regif), a bare name, or a fuzzy fragment.");
  const now = Date.now();
  const rocks = await allRocks();
  const resolution = resolveRocks(rocks, handle);
  const { hits, matchType } = resolution;
  const duplicates = duplicateReport(rocks);
  if (!hits.length) return [{ type: "text", text: [
    `checked_at: ${isoTime(now)}`,
    `no rock resolves «${handle}». Run prox_list to inspect every ledger snapshot.`,
    ...duplicateLines(duplicates),
  ].join("\n") }];
  const weak = new Set(["name-prefix", "name-substring", "subject-substring"]).has(matchType);
  const L = [
    `checked_at: ${isoTime(now)}`,
    `«${handle}» → ${hits.length} match(es) by ${matchType}:`,
    ...duplicateLines(duplicates),
    ...(weak ? ["warning: this is a discovery-only match, not an authoritative identity"] : []),
  ];
  for (const r of hits) {
    const fresh = ledgerFreshness(r.ledgerUpdatedAt, now);
    const running = fresh.state === "fresh" && RUNNING_STATUSES.has(r.status);
    L.push(
      `\n${canonicalHandle(r)}  ${r.self ? "(this machine)" : ""}`,
      `  alias:   ${r.host}:${r.name}`,
      `  status:  ${r.status}   kind: ${r.kind}   running now: ${running ? "yes" : "no"}`,
      `  ledger:  ${timeAndAge(r.ledgerUpdatedAt)}   freshness: ${fresh.state}`,
      `  active:  ${timeAndAge(r.updated)}`,
      `  uptime:  ${r.started ? age(r.started) : "?"}`,
      ...(r.loopboyContact ? [`  loopboy: ${r.loopboyContact}`] : []),
      `  subject: ${(r.subject || "").replace(/\s+/g, " ")}`,
      `  memoir:  ${(r.memoir || "(still gathering its story)").replace(/\s+/g, " ")}`,
      `  cwd:     ${r.cwd || "?"}`,
      `  id:      ${r.id}`,
      `  seed:    ${r.seed || "?"}   (re-render the same sigil anywhere)`,
    );
  }
  return [{ type: "text", text: L.join("\n") }];
}

// A narrative-first view for agents deciding whether a prox is the right
// continuation target. Inference remains owned by the menubar heartbeat; this
// read can never fan out model calls or transcript I/O.
async function toolRecap({ handle }) {
  if (!handle) throw new Error("`handle` is required (use host:name, session id, or a fuzzy fragment).");
  const now = Date.now();
  const rocks = await allRocks();
  const duplicates = duplicateReport(rocks);
  const resolution = resolveRocks(rocks, handle);
  const { hits, matchType } = resolution;
  if (!hits.length) return [{ type: "text", text: [
    `checked_at: ${isoTime(now)}`,
    `no rock resolves «${handle}».`,
    ...duplicateLines(duplicates),
  ].join("\n") }];
  if (hits.length > 1) {
    return [{ type: "text", text: [
      `checked_at: ${isoTime(now)}`,
      `«${handle}» is ambiguous (${hits.map(canonicalHandle).join(", ")}). Use a session id or canonical host:name#id.`,
      ...duplicateLines(duplicates),
    ].join("\n") }];
  }
  const r = hits[0];
  const fresh = ledgerFreshness(r.ledgerUpdatedAt, now);
  const story = (r.memoir || r.subject || "No story has landed yet.").replace(/\s+/g, " ").trim();
  return [{ type: "text", text: [
    `checked_at: ${isoTime(now)}`,
    `${canonicalHandle(r)} · ${r.status} · ${r.agentType || "claude"} · match ${matchType}`,
    `Ledger ${timeAndAge(r.ledgerUpdatedAt)} (${fresh.state}); active ${timeAndAge(r.updated)}; up ${r.started ? age(r.started) : "?"}.`,
    ...(new Set(["name-prefix", "name-substring", "subject-substring"]).has(matchType)
      ? ["Discovery-only match: confirm with the canonical handle or session id before acting."] : []),
    story,
  ].join("\n\n") }];
}

async function toolPoke({ handle, by }) {
  if (!handle) throw new Error("`handle` is required (a canonical host:name#id, exact host:name, unique pet name, or session id; see prox_find).");
  const r = actionResolution(await allRocks(), handle, "poke");
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
  return [{ type: "text", text: `checked_at: ${isoTime(r.resolvedAt)}\npoked ${canonicalHandle(r)} as «${poker}» — its rock should blink + rattle (HTTP ${res.status}).` }];
}

// First-class non-human Loopboy contacts. V1 is intentionally a status +
// bounded-accountability surface over the existing prompt ledger. It cannot
// send iMessage, run shell commands, or wake an arbitrary remote terminal.
function agentContact(address) {
  const parsed = parseAgentAddress(address);
  const iris = makeIrisContact();
  if (address.toLowerCase() === iris.address) return { ...iris, parsed };
  throw new Error(`unknown Loopboy agent contact: ${address}`);
}

async function toolLoopboyAgentStatus({ address }) {
  const contact = agentContact(address);
  const now = Date.now();
  const rocks = await allRocks();
  const rock = rocks.find((r) => r.host.toLowerCase() === contact.machine &&
    (r.id.toLowerCase() === contact.ledgerId || r.name.toLowerCase() === contact.ledgerId));
  const status = rock ? {
    state: rock.status, updated: rock.updated, age: age(rock.updated),
    subject: rock.subject || "", host: rock.host, ledgerId: rock.id,
    canonicalHandle: canonicalHandle(rock),
    ledgerUpdatedAt: isoTime(rock.ledgerUpdatedAt),
    ledgerFreshness: ledgerFreshness(rock.ledgerUpdatedAt, now).state,
  } : { state: "offline", host: contact.machine, ledgerId: contact.ledgerId };
  return [{ type: "text", text: JSON.stringify({
    checkedAt: isoTime(now),
    address: contact.address, displayName: contact.displayName,
    kind: contact.kind, status, responsibility: contact.responsibility,
  }, null, 2) }];
}

async function toolLoopboyAgentNudge({ address, text }) {
  const contact = agentContact(address);
  const nudge = boundedNudge(contact, text);
  const rocks = await allRocks();
  const rock = rocks.find((r) => r.host.toLowerCase() === contact.machine &&
    (r.id.toLowerCase() === contact.ledgerId || r.name.toLowerCase() === contact.ledgerId));
  if (!rock) throw new Error(`${contact.address} is offline — no ${contact.machine}:${contact.ledgerId} agent rock is available.`);
  // The ledger's fixed /poke endpoint is the only V1 transport. Encode the
  // bounded note in the observer label so it is auditable without opening a
  // general-purpose remote prompt or command channel.
  return toolPoke({ handle: rock.id, by: `loopboy:${contact.address} · ${nudge.text}` });
}

async function toolWake({ handle, prompt, by }) {
  if (!handle) throw new Error("`handle` is required (use the stable local host:name or session id).");
  const steering = String(prompt || "").replace(/\s+/g, " ").trim();
  if (!steering) throw new Error("`prompt` is required.");
  if (steering.length > 1000) throw new Error("`prompt` exceeds 1000 characters.");
  const r = actionResolution(await allRocks(), handle, "wake");
  if (!r.self) throw new Error(`${r.host}:${r.name} runs on another machine — prox_wake currently requires the MCP on the rock's owning host.`);

  // Loopboys never accept Terminal/UI injection. Their own long-polling MCP
  // call consumes this session-addressed event without touching user focus.
  if (r.loopboyContact) {
    await enqueueLoopboyEvent({
      sessionId: r.id,
      contact: r.loopboyContact,
      displayName: r.loopboyContact,
      kind: "message",
      prompt: steering,
      excerpt: String(by || "prox-wake"),
    });
    return [{ type: "text", text: `checked_at: ${isoTime(r.resolvedAt)}\nqueued ${canonicalHandle(r)} in its isolated Loopboy inbox; no Terminal or GUI input was emitted.` }];
  }

  if (!r.ip) throw new Error(`no tailnet ip known for ${r.host} — can't reach its menubar wake path.`);

  // Route through the menubar so prox and Loopboy share one re-entry UX:
  // poke/fly the rock, coalesce overlapping wakes, paste + Return through the
  // guarded Accessibility path, restore the clipboard/frontmost app, and
  // retry transient focus failures. Do not inject keyboard events here.
  const self = (await readJson(LOCAL_FILE))?.host || hostname().split(".")[0];
  const waker = by || `${self}:prox-wake`;
  const body = JSON.stringify({ by: waker, id: r.id, name: r.name, prompt: steering });
  const res = await fetch(`http://${r.ip}:${PORT}/wake`, {
    method: "POST",
    headers: { "content-type": "application/json", "content-length": Buffer.byteLength(body) },
    body,
    signal: AbortSignal.timeout(3000),
  }).catch((e) => { throw new Error(`menubar wake for ${r.host}:${r.name} failed: ${e.message}`); });
  const responseText = await res.text();
  let result;
  try { result = JSON.parse(responseText); }
  catch { throw new Error(`menubar wake for ${r.host}:${r.name} returned invalid JSON (HTTP ${res.status}).`); }
  if (!res.ok || !result.ok) {
    throw new Error(`menubar wake for ${r.host}:${r.name} failed: ${result.error || `HTTP ${res.status}`}`);
  }
  return [{ type: "text", text: `checked_at: ${isoTime(r.resolvedAt)}\nqueued ${canonicalHandle(r)} through the shared Loopboy heartbeat re-entry path as «${waker}».` }];
}

async function toolArtifactReady({ handle, artifacts, by }) {
  if (!Array.isArray(artifacts) || artifacts.length === 0) {
    throw new Error("`artifacts` must contain at least one output path.");
  }
  if (artifacts.length > 8) throw new Error("`artifacts` accepts at most 8 paths.");
  const paths = artifacts.map((value) => String(value || "").trim());
  if (paths.some((value) => !value || value.length > 500)) {
    throw new Error("each artifact path must be non-empty and at most 500 characters.");
  }
  const quoted = paths.map((value) => `«${value}»`).join(", ");
  return toolWake({
    handle,
    by: by || "artifact-ready",
    prompt: `Artifact output is ready at ${quoted}. Open it now and inspect it against the active request. Iterate if needed, copy the accepted artifact into the project, connect it to its intended consumer, verify the result, and continue the original task until its actual completion criterion is met. The artifact appearing is not task completion.`,
  });
}

async function toolLaunch({ host, agent, cwd, prompt = "", by, loopboyContact = "" }) {
  const wanted = String(host || "").trim().toLowerCase().replace(/\.local$/, "");
  if (!wanted) throw new Error("`host` is required (for example, poorslice).");
  const agentName = String(agent || "").trim().toLowerCase();
  if (!new Set(["claude", "codex"]).has(agentName)) {
    throw new Error("`agent` must be `claude` or `codex`.");
  }
  if (String(prompt).length > 4000) throw new Error("`prompt` exceeds 4000 characters.");

  const ledgers = await allLedgers();
  const target = ledgers.find((l) => String(l.host || "").toLowerCase() === wanted);
  if (!target) throw new Error(`no cached ledger for host «${host}» — it must be online in prox first.`);
  const targetFreshness = ledgerFreshness(target.updatedAt);
  if (targetFreshness.state !== "fresh") {
    throw new Error(`${target.host}'s ledger is ${targetFreshness.state} (${isoTime(target.updatedAt)}); refusing to launch`);
  }
  if (!target.ip) throw new Error(`no tailnet IP known for ${target.host}.`);
  const self = (await readJson(LOCAL_FILE))?.host || hostname().split(".")[0];
  const contactKey = String(loopboyContact || "").trim().toLowerCase();
  const loopboyListenerPrompt = contactKey
    ? `\n\nLoopboy listener contract: after completing this turn, call prox_loopboy_wait with contact ${contactKey} and timeoutSeconds 50. Call it again after every event or timeout so this client loop remains available. Events arrive only through that private MCP inbox. Never use Terminal, clipboard, mouse, keyboard, browser, or GUI injection to wake or operate this loop.`
    : "";
  const launchPrompt = `${String(prompt)}${loopboyListenerPrompt}`.trim();
  if (launchPrompt.length > 4000) {
    throw new Error("Loopboy listener instructions make the initial prompt exceed 4000 characters.");
  }
  const existingMarkerIds = new Set();
  if (contactKey) {
    for (const dir of MARKER_DIRS) {
      let names = [];
      try { names = await readdir(dir); } catch {}
      for (const name of names) existingMarkerIds.add(name);
    }
  }
  const launcher = by || `${self}:prox`;
  const body = JSON.stringify({
    agent: agentName,
    prompt: launchPrompt,
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
    let marker = null;
    for (let attempt = 0; attempt < 20 && !marker; attempt++) {
      for (const dir of MARKER_DIRS) {
        let names = [];
        try { names = await readdir(dir); } catch {}
        for (const name of names) {
          const value = await readJson(join(dir, name));
          const id = value?.session_id || name;
          if (!existingMarkerIds.has(id) && value?.loopboy_contact === contactKey) {
            marker = { id, value };
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
    cfg.version = 1;
    cfg.loops ||= {};
    cfg.loops[contactKey] = {
      event: "imessage",
      contact: contactKey,
      sessionId: marker.id,
      host: result.host || target.host,
      agent: agentName,
      wake: false,
      delivery: "inbox",
      assignedAt: new Date().toISOString(),
    };
    await writeFile(LOOPBOY_CONFIG, JSON.stringify(cfg, null, 2) + "\n", { mode: 0o600 });
    binding = ` and bound Loopboy contact ${contactKey}`;
  }
  return [{
    type: "text",
    text: `launched ${agentName} on ${result.host || target.host} in ${result.cwd} as «${launcher}»${prompt ? " with an initial prompt" : ""}${binding}.`,
  }];
}

async function toolBindNotification({ handle, contact, event = "imessage" }) {
  if (event !== "imessage") throw new Error("only the `imessage` Slab notification is supported");
  if (!handle) throw new Error("`handle` is required (use the stable host:name or session id)");
  const contactKey = String(contact || "").trim().toLowerCase();
  if (!contactKey) throw new Error("`contact` is required (the key from ~/.config/slab/imsg.json)");
  const r = actionResolution(await allRocks(), handle, "bind");
  if (!r.self) throw new Error("iMessage notification wake targets must be a local prox on this machine");
  const loop = {
    event: "imessage",
    contact: contactKey,
    sessionId: r.id,
    host: r.host,
    name: r.name,
    wake: false,
    delivery: "inbox",
    assignedAt: new Date().toISOString(),
  };
  await mkdir(join(homedir(), ".config", "slab"), { recursive: true });
  const cfg = (await readJson(LOOPBOY_CONFIG)) || { version: 1, loops: {} };
  cfg.version = 1;
  cfg.loops ||= {};
  cfg.loops[contactKey] = loop;
  await writeFile(LOOPBOY_CONFIG, JSON.stringify(cfg, null, 2) + "\n", { mode: 0o600 });
  return [{ type: "text", text: `checked_at: ${isoTime(r.resolvedAt)}\nLoopboy bound ${contactKey} → ${canonicalHandle(r)} (${r.id}) — isolated inbox delivery; no Terminal/UI injection.` }];
}

async function toolLoopboyWait({ handle, contact, timeoutSeconds = 50 }, context = {}) {
  const cfg = await readJson(LOOPBOY_CONFIG);
  const loops = cfg?.loops || {};
  const { contact: contactKey, sessionId: callerSessionId, loop } = authorizeLoopboyWait({
    context,
    loops,
    requestedContact: contact,
  });
  if (handle) {
    const rock = actionResolution(await allRocks(), handle, "wait on");
    if (String(rock.id) !== callerSessionId) {
      throw new Error(`«${handle}» is not this Loopboy session`);
    }
  }
  const seconds = Math.max(0, Math.min(55, Number(timeoutSeconds) || 0));
  const event = await waitLoopboyEvent(loop.sessionId, { timeoutMs: seconds * 1000 });
  if (!event) {
    return [{
      type: "text",
      text: `No event arrived for Loopboy ${contactKey} during this wait. Call prox_loopboy_wait again; do not poll Messages through GUI automation.`,
    }];
  }
  return [{
    type: "text",
    text: [
      `Loopboy inbox event for ${contactKey} (${event.kind}, ${event.createdAt}).`,
      event.prompt,
      "After handling this event, call prox_loopboy_wait again to remain available. Never use Terminal, clipboard, mouse, keyboard, browser, or GUI injection.",
    ].filter(Boolean).join("\n\n"),
  }];
}

async function toolClose({ handle }) {
  if (!handle) throw new Error("`handle` is required (a canonical host:name#id, exact host:name, unique pet name, or session id; see prox_find).");
  const r = actionResolution(await allRocks(), handle, "close");
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
  return [{ type: "text", text: `checked_at: ${isoTime(r.resolvedAt)}\nclosed ${canonicalHandle(r)} — ${steps.join("; ")}.` }];
}

const TOOLS = [
  {
    name: "prox_list",
    description:
      "List the prompt rocks advertised across the slab fleet with an absolute check time, each host ledger's timestamp/freshness, canonical host:name#id handles, last-active timestamps, status, and duplicate counts. Stale cached hosts remain visible but are labeled and are not evidence that a session is running now.",
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
      "Look up a canonical host:name#id, session id, host:name, pet name, or discovery fragment. Always reports the check timestamp, match method, ledger timestamp/freshness, last-active timestamp, running-now classification, and fleet duplicate counts. Prefix/substring/subject matches are explicitly discovery-only.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "`host:name` (e.g. neo:regif), a bare pet-name, or a fuzzy fragment of the name or subject." },
      },
      required: ["handle"],
    },
  },
  {
    name: "prox_recap",
    description:
      "Read the cached living recap for one prompt rock, including how long it has been up and how recently it was active. This is a cheap local-ledger read: it never starts inference or reads a transcript. Use it to understand a session before poking or waking it.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "Stable host:name, session id, or an unambiguous name/subject fragment." },
      },
      required: ["handle"],
    },
  },
  {
    name: "prox_poke",
    description:
      "Poke a prompt rock so its sigil blinks and rattles. Requires a fresh ledger and a canonical host:name#id, session id, exact host:name, or fleet-unique exact pet name. Refuses duplicate, stale, prefix, substring, and subject-only matches.",
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
    name: "prox_wake",
    description:
      "Wake one stable local prompt rock with a bounded steering prompt, using the same poke + TTY reactivation pattern as Loopboy. Use this when an asynchronous artifact or render completes after its agent turn: tell that same agent to open the artifact, inspect it, and continue the original task. SIDE EFFECT: submits a new prompt to the live Claude/Codex session. Refuses remote or ambiguous targets.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "Fresh canonical local host:name#id, session id, exact host:name, or fleet-unique exact pet name." },
        prompt: { type: "string", description: "Continuation instruction, at most 1000 characters." },
        by: { type: "string", description: "Optional source label, such as artifact:tokens-2-tlds." },
      },
      required: ["handle", "prompt"],
    },
  },
  {
    name: "prox_artifact_ready",
    description:
      "Deliver an asynchronous artifact-complete event to the stable local rock that launched it. Prox constructs and submits the continuation prompt itself: open and inspect the outputs, iterate, place accepted files in the project, wire them into their consumer, and continue the original task. Uses the Loopboy-style poke + TTY wake path. SIDE EFFECT: submits a new prompt to the live Claude/Codex session.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "Fresh canonical local host:name#id, session id, exact host:name, or fleet-unique exact pet name." },
        artifacts: {
          type: "array",
          items: { type: "string" },
          minItems: 1,
          maxItems: 8,
          description: "Absolute or project-relative output paths that just became ready.",
        },
        by: { type: "string", description: "Optional event-source label." },
      },
      required: ["handle", "artifacts"],
    },
  },
  {
    name: "prox_close",
    description:
      "Close a prompt rock and its terminal window. DESTRUCTIVE: requires a fresh ledger and a canonical host:name#id, session id, exact host:name, or fleet-unique exact pet name; refuses fuzzy, duplicate, stale, remote, or calling-session targets.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "`host:name` (e.g. neo:regif) or a name that resolves to exactly one rock on this machine." },
      },
      required: ["handle"],
    },
  },
  {
    name: "prox_launch",
    description:
      "Launch a new interactive Claude or Codex prompt in Terminal.app on a Slab fleet host. SIDE EFFECT: opens a live agent session and may consume account usage. The target accepts only the fixed claude/codex launchers, limits cwd to that user's home folder, and binds the endpoint to its tailnet IP; no arbitrary command is accepted.",
    inputSchema: {
      type: "object",
      properties: {
        host: { type: "string", description: "Target Slab hostname, for example poorslice." },
        agent: { type: "string", enum: ["claude", "codex"], description: "Agent CLI to launch." },
        cwd: { type: "string", description: "Optional absolute directory on the target. Defaults to its aesthetic-computer checkout and must stay under its home folder." },
        prompt: { type: "string", description: "Optional initial prompt, at most 4000 characters. Omit to open an idle TUI." },
        by: { type: "string", description: "Optional caller label recorded by the target." },
        loopboyContact: { type: "string", description: "Optional iMessage contact key. Launches a guarded Loopboy, binds its isolated inbox after the live marker appears, and starts its listener contract." },
      },
      required: ["host", "agent"],
    },
  },
  {
    name: "prox_bind_notification",
    description:
      "Create or replace one contact-keyed Loopboy route from iMessage to a stable local prox. Events are delivered only through that session's isolated inbox and prox_loopboy_wait; this never types into Terminal or other user-space UI.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "Fresh canonical local host:name#id, session id, exact host:name, or fleet-unique exact pet name." },
        contact: { type: "string", description: "Contact key from ~/.config/slab/imsg.json, for example alex." },
        event: { type: "string", enum: ["imessage"], default: "imessage" },
      },
      required: ["handle", "contact"],
    },
  },
  {
    name: "prox_loopboy_wait",
    description:
      "Wait up to 55 seconds for the next event in one bound Loopboy session's isolated inbox. The event is claimed exactly once and returned only to that session/contact. This is the safe replacement for Terminal, clipboard, mouse, and keyboard wake injection. Call it again after handling each event.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "Bound Loopboy host:name or session id. Either handle or contact is required." },
        contact: { type: "string", description: "Bound iMessage contact key, for example alex or loretta." },
        timeoutSeconds: { type: "number", minimum: 0, maximum: 55, default: 50 },
      },
    },
  },
  {
    name: "prox_loopboy_agent_status",
    description:
      "Report a first-class Loopboy agent contact by stable agent:<name>@<machine> address. V1 reads the fleet prompt ledger and the contact's bounded responsibility; it is not an iMessage identity.",
    inputSchema: {
      type: "object",
      properties: { address: { type: "string", description: "Stable agent address, currently agent:iris@panda." } },
      required: ["address"],
    },
  },
  {
    name: "prox_loopboy_agent_nudge",
    description:
      "Send a bounded accountability attention nudge (max 500 characters) to a live Loopboy agent contact. V1 uses the target agent rock's fixed poke channel and provides no arbitrary remote control.",
    inputSchema: {
      type: "object",
      properties: {
        address: { type: "string", description: "Stable agent address, currently agent:iris@panda." },
        text: { type: "string", maxLength: 500, description: "Accountability note, not a shell command or unrestricted prompt." },
      },
      required: ["address", "text"],
    },
  },
];

async function callTool(name, args, context) {
  switch (name) {
    case "prox_list": return toolList(args || {});
    case "prox_find": return toolFind(args || {});
    case "prox_recap": return toolRecap(args || {});
    case "prox_poke": return toolPoke(args || {});
    case "prox_wake": return toolWake(args || {});
    case "prox_artifact_ready": return toolArtifactReady(args || {});
    case "prox_launch": return toolLaunch(args || {});
    case "prox_bind_notification": return toolBindNotification(args || {});
    case "prox_loopboy_wait": return toolLoopboyWait(args || {}, context);
    case "prox_loopboy_agent_status": return toolLoopboyAgentStatus(args || {});
    case "prox_loopboy_agent_nudge": return toolLoopboyAgentNudge(args || {});
    case "prox_close": return toolClose(args || {});
    default: throw new Error(`Unknown tool: ${name}`);
  }
}

async function handleMessage(message, context = {}) {
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
else serveStdio({ handleMessage, banner: "🪨 prox started (prox_list, prox_find, prox_recap, prox_poke, prox_wake, prox_artifact_ready, prox_launch, prox_close)" });
