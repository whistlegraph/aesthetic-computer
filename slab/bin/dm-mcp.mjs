#!/usr/bin/env node
// dm-mcp.mjs — one MCP surface over a person's real DMs: iMessage + Signal,
// read AND send, so any agent can digest "what did X say" and reply without
// knowing which app or CLI it lives in.
//
// It wraps the sibling bridges that already exist in slab/bin:
//   • signal.mjs  — reads Signal Desktop's synced SQLCipher DB (status / read /
//                   attachments / save). Read-only; carries no send verb.
//   • signal-cli  — the linked-device CLI (Homebrew). Adds the SEND path Signal
//                   Desktop can't script, plus contact resolution + receive.
//   • imsg.mjs    — reads Messages' chat.db + sends via Messages.app
//                   (status / send). No one-shot history verb yet.
//
// House style: hand-rolled JSON-RPC over stdio (newline-delimited), matching
// frame-mcp.mjs / ants/mail-mcp / artery/emacs-mcp.mjs — no SDK, node builtins
// only, so it travels with the repo and needs no PATH setup. `--http [port]`
// runs one resident daemon every session shares (toolchain/mcp/http-front.mjs).
//
// LOCAL-FIRST. This instance operates on the machine it runs on (neo). neo and
// blueberry are linked to the SAME Signal account and the SAME iMessage account,
// so their message CONTENT mirrors — the reason to have both is availability,
// not different inboxes. The `machine` arg is accepted and, for signal-cli-
// backed calls (send / contacts / receive), routes over `ssh <machine>` using
// the host in ~/.config/slab/puppet.json (same registry `frame` reads).
// Desktop-DB reads (signal.mjs / imsg.mjs) stay local — to read blueberry's
// Desktop DB, run a dm-mcp instance THERE. That per-machine daemon is the
// documented next step, mirroring how the http-front daemons already deploy.
//
// SAFETY: dm_send NEVER sends on the first call. It resolves + echoes the target
// and the message and asks for `confirm: true` — outward, hard-to-unsend, often
// to NDA contacts. This is the send guardrail baked in, not bolted on.
import { execFile } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const SIGNAL = join(HERE, "signal.mjs");
const IMSG = join(HERE, "imsg.mjs");
const PUPPET_JSON = join(homedir(), ".config", "slab", "puppet.json");

// ── shell helpers ───────────────────────────────────────────────────────────
// Run a command, resolve { stdout, stderr }. Rejects with stderr (not a bare
// exit code) so tool errors read cleanly back to the agent.
function run(cmd, args, { timeoutMs = 30000, input } = {}) {
  return new Promise((resolve, reject) => {
    const child = execFile(
      cmd, args,
      { timeout: timeoutMs, maxBuffer: 32 * 1024 * 1024, encoding: "utf8" },
      (err, stdout, stderr) => {
        if (err && !stdout) return reject(new Error((stderr || err.message).trim()));
        resolve({ stdout, stderr });
      },
    );
    if (input != null) { child.stdin.write(input); child.stdin.end(); }
  });
}

// The ssh host for a machine name, from the registry `frame`/`puppet` share.
// Falls back to the bare name (works when it's already an ssh alias, e.g. blueberry).
function sshHost(machine) {
  try {
    if (existsSync(PUPPET_JSON)) {
      const reg = JSON.parse(readFileSync(PUPPET_JSON, "utf8"));
      const m = (reg.machines || reg)[machine];
      if (m) return m.ssh || m.host || machine;
    }
  } catch { /* fall through */ }
  return machine;
}

const LOCAL = new Set(["", "local", "neo", "this", undefined, null]);
const isLocal = (machine) => LOCAL.has(machine);

// Run a signal-cli invocation, locally or over ssh for a remote machine.
function runSignalCli(args, machine, opts) {
  if (isLocal(machine)) return run("signal-cli", args, opts);
  // quote each arg for the remote shell; signal-cli is on PATH on linked machines
  const remote = ["signal-cli", ...args].map((a) => `'${String(a).replace(/'/g, "'\\''")}'`).join(" ");
  return run("ssh", [sshHost(machine), remote], opts);
}

// A node bridge (signal.mjs / imsg.mjs) only runs where its Desktop DB lives.
function runBridge(bridge, args, machine, opts) {
  if (!isLocal(machine)) {
    throw new Error(
      `Desktop-DB read for "${machine}" isn't available from this instance — ` +
      `run a dm-mcp instance on ${machine} for its view. (signal-cli-backed ` +
      `tools like dm_send/dm_contacts DO route to ${machine} over ssh.)`,
    );
  }
  return run("node", [bridge, ...args], opts);
}

// ── Signal account (the linked +number) — detected once, cached ──────────────
let _acctCache;
async function signalAccount(machine) {
  if (isLocal(machine) && _acctCache) return _acctCache;
  const { stdout } = await runSignalCli(["listAccounts"], machine);
  const m = stdout.match(/\+[0-9]+/);
  if (!m) throw new Error("no linked signal-cli account (run: signal-cli link)");
  if (isLocal(machine)) _acctCache = m[0];
  return m[0];
}

// Resolve a Signal recipient the agent named. Accept ACI/UUID, +E164, or a
// profile-name substring (resolved via signal-cli listContacts → its ACI).
async function resolveSignalRecipient(to, machine) {
  if (!to) throw new Error("`to` is required for a Signal send (ACI, +number, or a name)");
  if (/^\+[0-9]{6,}$/.test(to)) return { id: to, label: to, how: "e164" };
  if (/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(to))
    return { id: to, label: `ACI ${to}`, how: "aci" };
  const acct = await signalAccount(machine);
  const { stdout } = await runSignalCli(["-a", acct, "listContacts"], machine);
  const needle = to.toLowerCase();
  const hits = stdout.split("\n").filter((l) => l.toLowerCase().includes(needle));
  const withAci = hits.map((l) => l.match(/ACI:\s*([0-9a-f-]{36})/i)?.[1]).filter(Boolean);
  if (!withAci.length) throw new Error(`no Signal contact matched "${to}" — try dm_contacts to find the ACI`);
  if (withAci.length > 1) throw new Error(`"${to}" matched ${withAci.length} contacts — pass the exact ACI (dm_contacts)`);
  return { id: withAci[0], label: `${to} (ACI ${withAci[0]})`, how: "name→aci" };
}

const text = (s) => [{ type: "text", text: s }];

// ── tools ────────────────────────────────────────────────────────────────────

// Unified "what's waiting" across both channels.
async function toolInbox({ machine } = {}) {
  const L = [];
  const names = ["Signal", "iMessage"];
  const results = await Promise.allSettled([
    runBridge(SIGNAL, ["status"], machine),
    runBridge(IMSG, ["status"], machine),
  ]);
  results.forEach((r, i) => {
    const name = names[i];
    if (r.status === "rejected") { L.push(`${name}: ⚠️ ${r.reason.message}`); return; }
    let j; try { j = JSON.parse(r.value.stdout); } catch { L.push(`${name}: ${r.value.stdout.trim()}`); return; }
    const last = j.last || {};
    L.push(
      `${name}: ${j.name || j.displayName || "(contact)"} — unread ${j.unread ?? "?"}` +
      (last.text ? ` · last ${last.dir || last.direction || ""} "${String(last.text).slice(0, 80)}" (${last.ago || "?"})` : ""),
    );
  });
  return text(L.join("\n"));
}

// The recent-conversation list — who's talking, unread, last message. Find a
// thread here, then pass its name/id to dm_read's `to` (Signal only).
async function toolChats({ n = 20, machine } = {}) {
  const { stdout } = await runBridge(SIGNAL, ["chats", String(n)], machine);
  return text(stdout.trim() || "(no conversations)");
}

// The groups-only view of the above (Signal group threads: name + id).
async function toolGroups({ n = 20, machine } = {}) {
  const { stdout } = await runBridge(SIGNAL, ["groups", String(n)], machine);
  return text(stdout.trim() || "(no groups)");
}

// Thread history. Signal has a real one-shot read; iMessage only a status
// summary today (imsg.mjs has no history verb yet — noted, not silently empty).
// `to` (alias `group`) targets ANY conversation — group or person — by name or
// conversationId; omit for the configured 1:1 contact. Group reads attribute
// each sender. An ambiguous target fails loudly rather than guessing.
async function toolRead({ channel, n = 15, to, group, machine } = {}) {
  const ch = (channel || "signal").toLowerCase();
  const dest = to || group;
  if (ch === "signal") {
    const toArgs = dest ? ["--to", String(dest)] : [];
    const { stdout } = await runBridge(SIGNAL, ["read", String(n), ...toArgs], machine);
    return text(stdout.trim() || "(no messages)");
  }
  if (dest) throw new Error("`to`/`group` is only supported on the Signal channel");
  if (ch === "imessage" || ch === "imsg") {
    const { stdout } = await runBridge(IMSG, ["status"], machine);
    return text(
      "iMessage (summary — imsg.mjs has no history verb yet, so only the latest is shown):\n" +
      stdout.trim(),
    );
  }
  throw new Error(`unknown channel "${channel}" (use "signal" or "imessage")`);
}

// List / decrypt Signal attachments (the voice-memo → transcribe pipeline).
async function toolAttachments({ n = 10, save, machine } = {}) {
  if (save !== undefined && save !== null) {
    const { stdout } = await runBridge(SIGNAL, ["save", String(save)], machine);
    const path = stdout.trim().split("\n").pop();
    return text(
      `saved → ${path}\n` +
      `(audio? transcribe: ffmpeg -y -i "${path}" -ar 16000 -ac 1 -c:a pcm_s16le /tmp/x.wav ` +
      `&& whisper-cli -m ~/.whisper-models/ggml-small.bin -f /tmp/x.wav -otxt -of /tmp/x)`,
    );
  }
  const { stdout } = await runBridge(SIGNAL, ["attachments", String(n)], machine);
  return text(stdout.trim() || "(no attachments)");
}

// Find a Signal contact's ACI by name.
async function toolContacts({ query, machine } = {}) {
  const acct = await signalAccount(machine);
  const { stdout } = await runSignalCli(["-a", acct, "listContacts"], machine);
  const lines = stdout.split("\n").filter(Boolean);
  const shown = query ? lines.filter((l) => l.toLowerCase().includes(query.toLowerCase())) : lines;
  return text(shown.slice(0, 40).join("\n") || `(no contacts${query ? ` matching "${query}"` : ""})`);
}

// Send — two-step by design. First call previews the resolved target + message;
// only `confirm: true` actually sends.
async function toolSend({ channel, to, text: body, confirm, machine } = {}) {
  const ch = (channel || "").toLowerCase();
  if (!body) throw new Error("`text` (the message) is required");

  if (ch === "signal") {
    const rcpt = await resolveSignalRecipient(to, machine);
    if (!confirm) {
      return text(
        `PREVIEW — not sent. Re-call with confirm:true to send.\n` +
        `channel: Signal   machine: ${isLocal(machine) ? "local" : machine}\n` +
        `to: ${rcpt.label}  [${rcpt.how}]\n--- message ---\n${body}`,
      );
    }
    const acct = await signalAccount(machine);
    const { stdout } = await runSignalCli(["-a", acct, "send", "-m", body, rcpt.id], machine, { timeoutMs: 60000 });
    const ts = (stdout.match(/\d{10,}/) || [])[0];
    return text(`✅ sent to ${rcpt.label}${ts ? ` (ts ${ts})` : ""}`);
  }

  if (ch === "imessage" || ch === "imsg") {
    // `to` selects a named contact / raw handle from imsg.json's contacts map
    // (imsg send --to). Omit it to hit the config's default contact.
    const toArgs = to ? ["--to", String(to)] : [];
    if (!confirm) {
      return text(
        `PREVIEW — not sent. Re-call with confirm:true to send.\n` +
        `channel: iMessage   machine: ${isLocal(machine) ? "local" : machine}\n` +
        `to: ${to ? `contact/handle "${to}" (from imsg.json contacts)` : "the default contact in ~/.config/slab/imsg.json"}\n` +
        `--- message ---\n${body}`,
      );
    }
    const { stdout } = await runBridge(IMSG, ["send", body, ...toArgs], machine, { timeoutMs: 30000 });
    return text(`✅ iMessage sent${to ? ` to "${to}"` : ""}${stdout.trim() ? `: ${stdout.trim()}` : ""}`);
  }

  throw new Error(`unknown channel "${channel}" (use "signal" or "imessage")`);
}

const TOOLS = [
  {
    name: "dm_inbox",
    description: "Unified 'what's waiting' across Signal + iMessage on this machine: per channel, the configured contact, unread count, and the last message (direction, text, how long ago). Start here to see if anyone replied.",
    inputSchema: { type: "object", properties: { machine: { type: "string", description: "Machine to read (default local). Desktop-DB reads are local-only." } } },
  },
  {
    name: "dm_chats",
    description: "List the most recent Signal conversations (most-recent-first): name, dm/group, unread count, and the age + short preview of the last message. Use this to see what threads exist, then pass a name to dm_read's `to` to read one.",
    inputSchema: {
      type: "object",
      properties: {
        n: { type: "number", description: "How many conversations (default 20)." },
        machine: { type: "string", description: "Machine (default local; Signal Desktop DB is local-only)." },
      },
    },
  },
  {
    name: "dm_read",
    description: "Read recent thread history. Signal returns the last N messages (← inbound / → outbound). Pass `group` (Signal only) to read ANY group or person thread by name or conversationId instead of the configured 1:1 contact — group reads attribute each sender. iMessage currently returns only the latest (imsg.mjs has no history verb yet). Use this to digest what someone said.",
    inputSchema: {
      type: "object",
      properties: {
        channel: { type: "string", enum: ["signal", "imessage"], description: "Which channel (default signal)." },
        n: { type: "number", description: "How many recent messages (Signal only; default 15)." },
        to: { type: "string", description: "Signal only: which conversation — a name substring (dm or group, per dm_chats) or an exact conversationId. Omit for the configured contact. An ambiguous name errors with the candidates rather than guessing." },
        group: { type: "string", description: "Signal only: alias for `to` — a group/person name substring or conversationId (find ids with dm_groups)." },
        machine: { type: "string", description: "Machine (default local)." },
      },
    },
  },
  {
    name: "dm_groups",
    description: "List recently-active Signal group threads (name + conversationId, newest first). Use this to find the `group` argument for dm_read.",
    inputSchema: {
      type: "object",
      properties: {
        n: { type: "number", description: "How many groups to list (default 20)." },
        machine: { type: "string", description: "Machine (default local; Signal Desktop DB is local-only)." },
      },
    },
  },
  {
    name: "dm_attachments",
    description: "List recent Signal attachments (indexed: type, size, filename), or decrypt one to a temp path by passing `save: <index>`. Voice memos come back as m4a/aac — the result includes the exact ffmpeg+whisper line to transcribe them.",
    inputSchema: {
      type: "object",
      properties: {
        n: { type: "number", description: "How many recent attachments to list (default 10)." },
        save: { type: "number", description: "Index from the list to decrypt to a temp file." },
        machine: { type: "string", description: "Machine (default local; Signal Desktop DB is local-only)." },
      },
    },
  },
  {
    name: "dm_contacts",
    description: "Find a Signal contact and its ACI (the stable id used to send), filtered by a name substring. Use this to resolve who to send to before dm_send.",
    inputSchema: {
      type: "object",
      properties: {
        query: { type: "string", description: "Name substring to filter (omit to list all)." },
        machine: { type: "string", description: "Machine (default local; routes over ssh for remote)." },
      },
    },
  },
  {
    name: "dm_send",
    description: "Send a DM. TWO-STEP AND SAFE: the first call PREVIEWS the resolved recipient + message and does NOT send; call again with confirm:true to actually send. Signal recipient (`to`) may be an ACI, +E164, or a name (resolved to its ACI). iMessage sends to the handle configured in imsg.json (an arbitrary `to` is ignored, and flagged in the preview).",
    inputSchema: {
      type: "object",
      properties: {
        channel: { type: "string", enum: ["signal", "imessage"], description: "Which channel." },
        to: { type: "string", description: "Signal: ACI / +number / name. iMessage: advisory only." },
        text: { type: "string", description: "The message body (multi-line ok)." },
        confirm: { type: "boolean", description: "Must be true to actually send. Omit/false = preview only." },
        machine: { type: "string", description: "Machine (default local; signal-cli sends route over ssh for remote)." },
      },
      required: ["channel", "text"],
    },
  },
];

async function callTool(name, args) {
  switch (name) {
    case "dm_inbox": return toolInbox(args || {});
    case "dm_chats": return toolChats(args || {});
    case "dm_read": return toolRead(args || {});
    case "dm_groups": return toolGroups(args || {});
    case "dm_attachments": return toolAttachments(args || {});
    case "dm_contacts": return toolContacts(args || {});
    case "dm_send": return toolSend(args || {});
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
            serverInfo: { name: "dm-mcp", version: "1.0.0" },
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
      return { jsonrpc: "2.0", id, result: { isError: true, content: text(String(error.message || error)) } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 7771);
if (port) serveHttp({ handleMessage, port, banner: "✉️  dm-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "✉️  dm-mcp server started (dm_inbox, dm_chats, dm_read, dm_groups, dm_attachments, dm_contacts, dm_send)" });
