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
//                   (status / indexed chats / history / FTS search / send).
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
import { createHash } from "node:crypto";
import { execFile } from "node:child_process";
import { existsSync, readFileSync, statSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, isAbsolute, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const SIGNAL = join(HERE, "signal.mjs");
const IMSG = join(HERE, "imsg.mjs");
const VISUAL_EVIDENCE = join(HERE, "visual-evidence.mjs");
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

// Unified "what's waiting" across both channels. Returning the snapshot is
// ingestion, so both Slab-owned notification cursors advance afterward.
async function toolInbox({ machine } = {}) {
  const L = [];
  const names = ["Signal", "iMessage"];
  const results = await Promise.allSettled([
    runBridge(SIGNAL, ["status"], machine),
    runBridge(IMSG, ["status"], machine),
  ]);
  // Capturing the summaries is ingestion: clear Slab's passive notification
  // cursors only after both snapshots have been obtained.
  await Promise.allSettled([
    runBridge(SIGNAL, ["ack"], machine),
    runBridge(IMSG, ["ack"], machine),
  ]);
  results.forEach((r, i) => {
    const name = names[i];
    if (r.status === "rejected") { L.push(`${name}: ⚠️ ${r.reason.message}`); return; }
    let j; try { j = JSON.parse(r.value.stdout); } catch { L.push(`${name}: ${r.value.stdout.trim()}`); return; }
    const last = j.last || {};
    const direction = last.dir || last.direction ||
      (last.fromMe === true ? "outgoing" : last.fromMe === false ? "incoming" : "");
    L.push(
      `${name}: ${j.name || j.displayName || "(contact)"} — un-ingested ${j.unread ?? "?"}` +
      (last.text ? ` · last ${direction} "${String(last.text).slice(0, 80)}" (${last.ago || "?"})` : ""),
    );
  });
  return text(L.join("\n"));
}

// The recent-conversation list — who's talking, unread, last message. Find a
// thread here, then pass its name/id to dm_read's `to`.
async function toolChats({ channel = "signal", n = 20, machine } = {}) {
  const ch = String(channel).toLowerCase();
  if (!["signal", "imessage", "imsg"].includes(ch)) {
    throw new Error(`unknown channel "${channel}" (use "signal" or "imessage")`);
  }
  const bridge = ch === "imessage" || ch === "imsg" ? IMSG : SIGNAL;
  const { stdout } = await runBridge(bridge, ["chats", String(n)], machine, { timeoutMs: 120000 });
  return text(stdout.trim() || "(no conversations)");
}

// The groups-only view of the above (Signal group threads: name + id).
async function toolGroups({ n = 20, machine } = {}) {
  const { stdout } = await runBridge(SIGNAL, ["groups", String(n)], machine);
  return text(stdout.trim() || "(no groups)");
}

// Thread history. Both channels accept an optional target without changing the
// notification/default contact. iMessage history comes from its private local
// incremental FTS index; Signal reads its Desktop database directly. Reading
// the watched conversation acknowledges its Slab cursor on both channels.
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
  if (ch === "imessage" || ch === "imsg") {
    if (group) throw new Error("iMessage group lookup uses `to`, not the Signal-only `group` alias");
    const toArgs = dest ? ["--to", String(dest)] : [];
    const { stdout } = await runBridge(IMSG, ["read", String(n), ...toArgs], machine, { timeoutMs: 120000 });
    return text(stdout.trim() || "(no messages)");
  }
  throw new Error(`unknown channel "${channel}" (use "signal" or "imessage")`);
}

// Full-text iMessage search, optionally scoped to one configured contact or
// raw handle. This never changes or acknowledges the watched/default thread.
async function toolSearch({ query, to, n = 25, machine } = {}) {
  if (!String(query || "").trim()) throw new Error("`query` is required");
  const args = ["search", String(query), "--limit", String(n)];
  if (to) args.push("--to", String(to));
  const { stdout } = await runBridge(IMSG, args, machine, { timeoutMs: 120000 });
  return text(stdout.trim() || "(no matching iMessages)");
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
function inspectOutgoingFiles(values = []) {
  return values.map((value) => {
    const path = isAbsolute(String(value)) ? String(value) : resolve(String(value));
    if (!existsSync(path)) throw new Error(`attachment does not exist: ${path}`);
    const info = statSync(path);
    if (!info.isFile()) throw new Error(`attachment is not a file: ${path}`);
    if (info.size > 20 * 1024 * 1024) throw new Error(`attachment exceeds the 20 MB backend limit: ${path}`);
    return {
      path,
      bytes: info.size,
      sha256: createHash("sha256").update(readFileSync(path)).digest("hex"),
    };
  });
}

async function toolSend({
  channel, to, text: body = "", image, attachments = [], linkPreview,
  visibleTitle, mediaTransport = "auto", confirm, machine,
} = {}) {
  const ch = (channel || "").toLowerCase();
  const requestedFiles = [image, ...(Array.isArray(attachments) ? attachments : [attachments])].filter(Boolean);
  const files = inspectOutgoingFiles(requestedFiles);
  const message = String(body || "");
  if (!message && !files.length && !linkPreview) {
    throw new Error("provide `text`, `image`/`attachments`, or `linkPreview`");
  }

  if (ch === "signal") {
    if (linkPreview) throw new Error("`linkPreview` is Messages-only; put the URL in `text` for Signal");
    const rcpt = await resolveSignalRecipient(to, machine);
    if (!confirm) {
      return text(
        `PREVIEW — not sent. Re-call with confirm:true to send.\n` +
        `channel: Signal   machine: ${isLocal(machine) ? "local" : machine}\n` +
        `to: ${rcpt.label}  [${rcpt.how}]\n` +
        `${files.map((file) => `attachment: ${file.path} (${file.bytes} bytes, sha256 ${file.sha256})`).join("\n")}` +
        `${files.length && message ? "\n" : ""}${message ? `--- message ---\n${message}` : ""}`,
      );
    }
    const acct = await signalAccount(machine);
    const sendArgs = ["-a", acct, "send"];
    if (message) sendArgs.push("-m", message);
    sendArgs.push(rcpt.id);
    // signal-cli's attachment option has nargs="*" and therefore consumes
    // every following token. Keep the positional recipient before it.
    for (const file of files) sendArgs.push("--attachment", file.path);
    const { stdout } = await runSignalCli(sendArgs, machine, { timeoutMs: 60000 });
    // Best effort: if this is the conversation Slab watches, replying is also
    // an acknowledgement. Other conversations keep independent cursors.
    await runBridge(SIGNAL, ["ack", "--to", String(to)], machine).catch(() => {});
    const ts = (stdout.match(/\d{10,}/) || [])[0];
    return text(`✅ sent to ${rcpt.label}${ts ? ` (ts ${ts})` : ""}`);
  }

  if (ch === "imessage" || ch === "imsg") {
    if (!new Set(["auto", "backend", "ui"]).has(String(mediaTransport))) {
      throw new Error('`mediaTransport` must be "auto", "backend", or "ui"');
    }
    const hasDistinctText = message && message.trim() !== String(linkPreview || "").trim();
    const sendParts = files.length + (linkPreview ? 1 : 0) + (hasDistinctText ? 1 : 0);
    if (sendParts > 1) {
      throw new Error(
        "Messages sends accept one attachment, link preview, or text part per confirmed call; preview and send each part separately to prevent partial-success retries",
      );
    }
    // Never silently fall back to imsg.json's default contact. An explicit
    // recipient is required, resolved before preview, and resolved again by
    // imsg.mjs at send time.
    if (!to) throw new Error("`to` is required for an iMessage send (named contact or raw handle)");
    const toArgs = ["--to", String(to)];
    const { stdout: resolved } = await runBridge(IMSG, ["resolve", ...toArgs], machine, { timeoutMs: 30000 });
    const rcpt = JSON.parse(resolved);
    if (!confirm) {
      const previewLines = [
        "PREVIEW — not sent. Re-call with confirm:true to send.",
        `channel: Messages (iMessage/RCS/SMS)   machine: ${isLocal(machine) ? "local" : machine}`,
        `to: ${rcpt.displayName}  [requested: "${to}"]`,
        `UI fallback recipient guard: ${visibleTitle || rcpt.displayName}`,
        ...(files.length ? [`Messages media transport: ${mediaTransport}`] : []),
        ...files.map((file) => `attachment: ${file.path} (${file.bytes} bytes, sha256 ${file.sha256})`),
        ...(linkPreview ? [`rich link preview: ${linkPreview}`] : []),
        ...(message ? ["--- message ---", message] : []),
      ];
      return text(
        previewLines.join("\n"),
      );
    }
    const guardArgs = ["--expected-title", String(visibleTitle || rcpt.displayName)];
    const transportArgs = ["--media-transport", String(mediaTransport)];
    const receipts = [];
    for (const file of files) {
      const { stdout } = await runBridge(
        IMSG,
        ["send", "--media", file.path, ...transportArgs, ...guardArgs, ...toArgs],
        machine,
        { timeoutMs: 60000 },
      );
      receipts.push({ kind: "attachment", ...JSON.parse(stdout.trim()) });
    }
    if (linkPreview) {
      const { stdout } = await runBridge(
        IMSG,
        ["send", "--link-preview", String(linkPreview), ...guardArgs, ...toArgs],
        machine,
        { timeoutMs: 60000 },
      );
      receipts.push({ kind: "link-preview", ...JSON.parse(stdout.trim()) });
    }
    // If text is exactly the rich-preview URL, the URL balloon already carries
    // it; do not emit a duplicate plain bubble.
    if (hasDistinctText) {
      const { stdout } = await runBridge(IMSG, ["send", message, ...toArgs], machine, { timeoutMs: 30000 });
      receipts.push({ kind: "text", ...JSON.parse(stdout.trim()) });
    }
    for (const receipt of receipts) {
      if (receipt.status === "failed") throw new Error(`Messages rejected ${receipt.kind} to ${rcpt.displayName}`);
    }
    return text(JSON.stringify({ ok: true, to: rcpt.displayName, receipts }, null, 2));
  }

  throw new Error(`unknown channel "${channel}" (use "signal" or "imessage")`);
}

async function toolVisualCapture({ kind = "url", url, selector, label, machine, region, out } = {}) {
  const mode = String(kind).toLowerCase();
  const args = [mode];
  if (mode === "url") {
    if (!url || !selector) throw new Error("URL evidence requires `url` and a CSS `selector`");
    args.push("--url", String(url), "--selector", String(selector));
    if (label) args.push("--label", String(label));
  } else if (mode === "frame") {
    args.push("--machine", String(machine || "local"));
    if (region) args.push("--region", String(region));
  } else {
    throw new Error('`kind` must be "url" or "frame"');
  }
  if (out) args.push("--out", String(out));
  const { stdout } = await run(process.execPath, [VISUAL_EVIDENCE, ...args], { timeoutMs: 90000 });
  return text(stdout.trim());
}

const TAPBACK_LABELS = new Map([
  ["heart", "heart"], ["love", "heart"],
  ["thumbs-up", "thumbs up"], ["thumbsup", "thumbs up"], ["like", "thumbs up"],
  ["thumbs-down", "thumbs down"], ["thumbsdown", "thumbs down"], ["dislike", "thumbs down"],
  ["haha", "Ha Ha"], ["laugh", "Ha Ha"],
  ["emphasis", "emphasis"], ["!!", "emphasis"],
  ["question", "question mark"], ["?", "question mark"],
]);

// Classic iMessage Tapback on the selected contact's most recent incoming
// message. Apple exposes this through Messages' Cmd-T, 1–6 shortcuts rather
// than its scripting dictionary, so execution uses guarded UI automation.
async function toolReact({ to, reaction, confirm, machine } = {}) {
  if (!to) throw new Error("`to` is required for an iMessage reaction (named contact or raw handle)");
  const requested = String(reaction || "").trim().toLowerCase();
  const label = TAPBACK_LABELS.get(requested);
  if (!label) throw new Error("unknown `reaction` — use heart, thumbs-up, thumbs-down, haha, emphasis, or question");
  const toArgs = ["--to", String(to)];
  const { stdout: resolved } = await runBridge(IMSG, ["resolve", ...toArgs], machine, { timeoutMs: 30000 });
  const rcpt = JSON.parse(resolved);
  if (!confirm) {
    return text(
      `PREVIEW — no reaction added. Re-call with confirm:true to react.\n` +
      `channel: iMessage   machine: ${isLocal(machine) ? "local" : machine}\n` +
      `to: ${rcpt.displayName}  [requested: "${to}"]\n` +
      `target: most recent incoming message\nreaction: ${label}`,
    );
  }
  const { stdout } = await runBridge(IMSG, ["react", requested, ...toArgs], machine, { timeoutMs: 30000 });
  const result = JSON.parse(stdout);
  return text(`✅ added ${result.reaction} Tapback to ${result.displayName}'s most recent incoming message`);
}

const TOOLS = [
  {
    name: "dm_inbox",
    description: "Unified 'what's waiting' across Signal + iMessage on this machine: per channel, the configured contact, un-ingested count, and the last message (direction, text, how long ago). Reading this snapshot acknowledges both Slab notification cursors.",
    inputSchema: { type: "object", properties: { machine: { type: "string", description: "Machine to read (default local). Desktop-DB reads are local-only." } } },
  },
  {
    name: "dm_chats",
    description: "List recent conversations for Signal or iMessage (most-recent-first). iMessage uses a private local incremental index and does not change the watched/default contact. Pass a result's name/id to dm_read's `to`.",
    inputSchema: {
      type: "object",
      properties: {
        channel: { type: "string", enum: ["signal", "imessage"], description: "Channel to list (default signal)." },
        n: { type: "number", description: "How many conversations (default 20)." },
        machine: { type: "string", description: "Machine (default local; Signal Desktop DB is local-only)." },
      },
    },
  },
  {
    name: "dm_read",
    description: "Read recent thread history (← inbound / → outbound). `to` targets a person/thread without changing the channel's watched/default contact. Reading the watched conversation marks its Slab notification cursor ingested; unrelated thread reads remain passive. Signal accepts a name or conversationId; iMessage accepts a configured contact name or raw handle and reads its private local incremental index.",
    inputSchema: {
      type: "object",
      properties: {
        channel: { type: "string", enum: ["signal", "imessage"], description: "Which channel (default signal)." },
        n: { type: "number", description: "How many recent messages (default 15)." },
        to: { type: "string", description: "Conversation target. Signal: name/conversationId. iMessage: configured contact name or raw +number/email. Omit for the channel's default contact." },
        group: { type: "string", description: "Signal only: alias for `to` — a group/person name substring or conversationId (find ids with dm_groups)." },
        machine: { type: "string", description: "Machine (default local)." },
      },
    },
  },
  {
    name: "dm_search",
    description: "Full-text search the private local iMessage index, optionally scoped to a configured contact name or raw handle. Search is read-only and does not change or acknowledge the watched/default contact.",
    inputSchema: {
      type: "object",
      properties: {
        query: { type: "string", description: "Words to find (all terms must match)." },
        to: { type: "string", description: "Optional configured iMessage contact name or raw +number/email." },
        n: { type: "number", description: "Maximum results (default 25, max 200)." },
        machine: { type: "string", description: "Machine (default local; Messages DB reads are local-only)." },
      },
      required: ["query"],
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
    description: "Send one text, image/PDF attachment, or real rich link preview per call. TWO-STEP AND SAFE: the first call PREVIEWS the resolved recipient and exact payload and does NOT send; call again with confirm:true. Messages media uses a request-scoped staged-file backend for verified iMessage routes; PDFs are backend-only. RCS/SMS remain guarded-UI by default and can be tested explicitly with mediaTransport=backend. Use linkPreview when an Apple URL preview card is required.",
    inputSchema: {
      type: "object",
      properties: {
        channel: { type: "string", enum: ["signal", "imessage"], description: "Which channel." },
        to: { type: "string", description: "Required recipient. Signal: ACI / +number / name. iMessage: named imsg.json contact or raw +number/email." },
        text: { type: "string", description: "Optional message body (multi-line ok)." },
        image: { type: "string", description: "Optional absolute/local image path; alias for one attachments entry." },
        attachments: { type: "array", items: { type: "string" }, description: "Optional file paths. Messages accepts supported images and PDFs; Signal passes files to signal-cli." },
        linkPreview: { type: "string", description: "Messages only: send this http(s) URL as an Apple rich URL balloon with metadata." },
        visibleTitle: { type: "string", description: "Optional exact/contained Messages conversation title for the recipient guard; defaults to the resolved contact display name." },
        mediaTransport: { type: "string", enum: ["auto", "backend", "ui"], description: "Messages attachments only. auto (default) uses the verified non-UI backend for iMessage and guarded UI for unverified RCS/SMS; backend explicitly tests non-UI and forbids ambiguous UI fallback; ui forces the guarded visible-conversation path and does not accept PDFs." },
        confirm: { type: "boolean", description: "Must be true to actually send. Omit/false = preview only." },
        machine: { type: "string", description: "Machine (default local; signal-cli sends route over ssh for remote)." },
      },
      required: ["channel", "to"],
    },
  },
  {
    name: "dm_visual_capture",
    description: "Create send-ready visual evidence without raw browser control. URL mode loads one http(s) page, applies a Captutor spotlight to one ordinary CSS selector, labels it, and crops until the evidence dominates. Frame mode captures the focused window from a named fleet Mac and can crop an explicit x,y,width,height region. Returns path, hash, bounds, and source metadata; it never sends.",
    inputSchema: {
      type: "object",
      properties: {
        kind: { type: "string", enum: ["url", "frame"], description: "Capture source (default url)." },
        url: { type: "string", description: "URL mode: http(s) page to capture." },
        selector: { type: "string", description: "URL mode: ordinary CSS selector to spotlight; js=/text= selectors are refused." },
        label: { type: "string", description: "URL mode: short visible evidence label (max 80 characters)." },
        machine: { type: "string", description: "Frame mode: fleet machine (default local)." },
        region: { type: "string", description: "Frame mode: optional x,y,width,height crop in image pixels." },
        out: { type: "string", description: "Optional output path; defaults under ~/.local/share/slab/visual-evidence/." },
      },
    },
  },
  {
    name: "dm_react",
    description: "Add a classic iMessage Tapback to a contact's most recent incoming message. TWO-STEP AND SAFE: preview first, then re-call with confirm:true. Requires explicit `to`. Supported reactions: heart, thumbs-up, thumbs-down, haha, emphasis, question.",
    inputSchema: {
      type: "object",
      properties: {
        to: { type: "string", description: "Required named imsg.json contact or raw +number/email." },
        reaction: { type: "string", enum: ["heart", "thumbs-up", "thumbs-down", "haha", "emphasis", "question"], description: "Classic Tapback to add." },
        confirm: { type: "boolean", description: "Must be true to actually react. Omit/false = preview only." },
        machine: { type: "string", description: "Machine (default local; Messages UI automation is local-only)." },
      },
      required: ["to", "reaction"],
    },
  },
];

async function callTool(name, args) {
  switch (name) {
    case "dm_inbox": return toolInbox(args || {});
    case "dm_chats": return toolChats(args || {});
    case "dm_read": return toolRead(args || {});
    case "dm_search": return toolSearch(args || {});
    case "dm_groups": return toolGroups(args || {});
    case "dm_attachments": return toolAttachments(args || {});
    case "dm_contacts": return toolContacts(args || {});
    case "dm_send": return toolSend(args || {});
    case "dm_visual_capture": return toolVisualCapture(args || {});
    case "dm_react": return toolReact(args || {});
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
            serverInfo: { name: "dm-mcp", version: "1.2.0" },
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
else serveStdio({ handleMessage, banner: "✉️  dm-mcp server started (dm_inbox, dm_chats, dm_read, dm_search, dm_groups, dm_attachments, dm_contacts, dm_send, dm_visual_capture, dm_react)" });
