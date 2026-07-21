#!/usr/bin/env node
// signal.mjs — a tiny, generic Signal Desktop bridge for the slab menubar.
//
// Reads Signal Desktop's encrypted SQLCipher DB (read-only) for a configured
// contact, reports an unread/last-message summary as JSON, optionally rings a bell on a
// NEW inbound message, prints recent messages / a live tail, and decrypts
// attachments to a temp dir (handing PDFs to the slab viewer).
//
// This file ships in the PUBLIC aesthetic.computer repo, so it carries NO
// personal data: the contact match / display name / bell target all live in
// an UNTRACKED config at ~/.config/slab/signal.json (see `signal config`).
//
// How the DB is opened (all in-process — the key never touches disk or logs):
//   1. config.json holds an `encryptedKey` (Electron safeStorage, "v10"…).
//   2. The wrapping password is the macOS Keychain item "Signal Safe Storage"
//      (prompts once for approval); PBKDF2-SHA1(salt "saltysalt", 1003, 16B)
//      → AES-128-CBC (IV = 16 spaces) decrypts it to the 64-hex SQLCipher key.
//   3. sqlcipher opens db.sqlite with `PRAGMA key="x'<hex>'"` + compat 4.
// Attachments (version 2) are AES-256-CBC: file = IV‖ciphertext‖HMAC, key =
// first 32B of localKey, mac = last 32B, plaintext truncated to `size`.
//
// Subcommands:
//   signal chats [N]         list the N most recent conversations (default 20)
//   signal status            JSON summary; optional bell on new inbound
//   signal read [N] [target] print the last N messages (default 15); with a
//                            target (group/person name or conversationId) read
//                            ANY thread — group reads attribute each sender
//   signal groups [N]        list recently-active group threads (name + id)
//   signal tail              live terminal client (prints + BEL on new inbound)
//   signal attachments [N]   list the last N attachments (default 10), indexed
//   signal save [N|all]      decrypt recent attachments → temp dir; open PDFs
//   signal open              open Signal.app
//   signal config            print the resolved config path (creates a stub)
//
// Every read-ish subcommand takes `--to <name|id>` to address ONE conversation
// for that invocation (`signal read 30 --to jayson`) without touching config.
// Without it they read the configured contact, exactly as they always have.

import { spawnSync, execFileSync } from "node:child_process";
import crypto from "node:crypto";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  writeFileSync,
  unlinkSync,
} from "node:fs";
import { homedir, tmpdir } from "node:os";
import { join, dirname } from "node:path";

const HOME = homedir();
const SIGNAL_DIR = join(HOME, "Library", "Application Support", "Signal");
const DB_PATH = join(SIGNAL_DIR, "sql", "db.sqlite");
const CONFIG_JSON = join(SIGNAL_DIR, "config.json");
const ATTACH_DIR = join(SIGNAL_DIR, "attachments.noindex");
const CONFIG_PATH =
  process.env.SLAB_SIGNAL_CONFIG || join(HOME, ".config", "slab", "signal.json");
const STATE_DIR = join(
  process.env.SLAB_HOME || join(HOME, ".local", "share", "slab"),
  "signal",
);
const STATE_PATH = join(STATE_DIR, "state.json");
const ACK_PATH = join(STATE_DIR, "ack.json");
const SAVE_DIR = join(STATE_DIR, "attachments");
const SQLCIPHER = "/opt/homebrew/bin/sqlcipher";
const SECURITY = "/usr/bin/security";

// ─── config ────────────────────────────────────────────────────────────────

const CONFIG_STUB = {
  _README:
    "slab signal contact config — UNTRACKED, never committed. Fill in `match`.",
  displayName: "REPLACE_ME",
  match: {
    _comment:
      "identify the conversation by ONE of: conversationId (exact), or any " +
      "substring matched (case-insensitive) against the contact's full name / " +
      "profile name / phone (e164).",
    conversationId: "",
    contains: "REPLACE_ME",
    e164: "",
  },
  bellTty: "",
  bellTtyComment:
    "optional: a tty like /dev/ttys004 — writing BEL there flashes that terminal.",
  sound: false,
  soundFile: "",
};

function loadConfig() {
  if (!existsSync(CONFIG_PATH)) return null;
  try {
    const cfg = JSON.parse(readFileSync(CONFIG_PATH, "utf8"));
    const m = cfg?.match || {};
    const configured =
      cfg &&
      cfg.displayName !== "REPLACE_ME" &&
      (m.conversationId ||
        (m.contains && m.contains !== "REPLACE_ME") ||
        m.e164);
    return configured ? cfg : null;
  } catch {
    return null;
  }
}

function ensureConfigStub() {
  if (existsSync(CONFIG_PATH)) return;
  mkdirSync(dirname(CONFIG_PATH), { recursive: true });
  writeFileSync(CONFIG_PATH, JSON.stringify(CONFIG_STUB, null, 2) + "\n", {
    mode: 0o600,
  });
}

// ─── DB key (in-process; never logged or written) ────────────────────────────

let DB_KEY = null;
function dbKey() {
  if (DB_KEY) return DB_KEY;
  const pw = execFileSync(SECURITY, [
    "find-generic-password",
    "-ws",
    "Signal Safe Storage",
  ])
    .toString()
    .trim();
  const aesKey = crypto.pbkdf2Sync(pw, "saltysalt", 1003, 16, "sha1");
  const cfg = JSON.parse(readFileSync(CONFIG_JSON, "utf8"));
  if (!cfg.encryptedKey) {
    // Legacy plaintext-key installs (pre-safeStorage) keep `key` directly.
    if (cfg.key) return (DB_KEY = cfg.key);
    throw new Error("config.json has neither encryptedKey nor key");
  }
  const blob = Buffer.from(cfg.encryptedKey, "hex").subarray(3); // strip "v10"
  const dec = crypto.createDecipheriv("aes-128-cbc", aesKey, Buffer.alloc(16, 0x20));
  DB_KEY = Buffer.concat([dec.update(blob), dec.final()]).toString();
  return DB_KEY;
}

// ─── SQLCipher query (read-only) ─────────────────────────────────────────────

// Route ONLY the SELECT's rows to a temp file via `.output` so the keying
// PRAGMAs (which also print in -json mode) don't pollute the parsed JSON.
function sql(query) {
  const out = join(tmpdir(), `slab-signal-${process.pid}-${Math.random().toString(36).slice(2)}.json`);
  const script =
    `PRAGMA key="x'${dbKey()}'";\n` +
    `PRAGMA cipher_compatibility=4;\n` +
    `.mode json\n.output ${out}\n${query}\n.output stdout\n`;
  const r = spawnSync(SQLCIPHER, ["-batch", "-readonly", DB_PATH], {
    input: script,
    encoding: "utf8",
    maxBuffer: 128 * 1024 * 1024,
  });
  if (r.status !== 0) {
    try { unlinkSync(out); } catch {}
    const e = new Error((r.stderr || "sqlcipher failed").trim());
    e.code = "SQLCIPHER";
    throw e;
  }
  let text = "";
  try { text = readFileSync(out, "utf8").trim(); } catch {}
  try { unlinkSync(out); } catch {}
  return text ? JSON.parse(text) : [];
}

const sqlStr = (s) => "'" + String(s).replace(/'/g, "''") + "'";

// ─── conversation resolution ─────────────────────────────────────────────────

const CONVO_COLS =
  "id, type, name, profileFullName, profileName, profileFamilyName, e164, serviceId, active_at AS activeAt";

// The fields a person would actually type: contact name, profile name, group
// name, phone. Case-insensitive substring — the `match.contains` semantics.
function likeAny(needle) {
  const like = sqlStr(`%${String(needle).toLowerCase()}%`);
  return (
    `(LOWER(IFNULL(profileFullName,'')) LIKE ${like} OR ` +
    `LOWER(IFNULL(profileName,'')||' '||IFNULL(profileFamilyName,'')) LIKE ${like} OR ` +
    `LOWER(IFNULL(name,'')) LIKE ${like} OR ` +
    `IFNULL(e164,'') LIKE ${like})`
  );
}

const convoName = (c) =>
  c.name ||
  c.profileFullName ||
  [c.profileName, c.profileFamilyName].filter(Boolean).join(" ") ||
  c.e164 ||
  (c.type === "group" ? "(group)" : "(unknown)");

const kindOf = (c) => (c.type === "group" ? "group" : "dm");

function resolveConversationId(cfg) {
  const m = cfg.match || {};
  if (m.conversationId) return m.conversationId;
  if (m.e164) {
    const r = sql(`SELECT id FROM conversations WHERE e164=${sqlStr(m.e164)} LIMIT 1;`);
    if (r.length) return r[0].id;
  }
  if (m.contains) {
    const r = sql(
      `SELECT id FROM conversations WHERE type='private' AND ${likeAny(m.contains)} ` +
        `ORDER BY active_at DESC LIMIT 1;`,
    );
    if (r.length) return r[0].id;
  }
  return null;
}

// `--to` addresses a conversation for one invocation. An exact conversationId
// wins; otherwise the needle must land on exactly ONE conversation. A needle
// that silently picks among several is how you read the wrong person's thread,
// so ambiguity prints the candidates and exits nonzero. Groups and 1:1s share
// the conversations table, so `--to` reaches either.
function resolveTo(to) {
  const exact = sql(
    `SELECT ${CONVO_COLS} FROM conversations ` +
      `WHERE id=${sqlStr(to)} OR serviceId=${sqlStr(to)} LIMIT 1;`,
  );
  if (exact.length) return exact[0];

  const hits = sql(
    `SELECT ${CONVO_COLS} FROM conversations ` +
      `WHERE active_at IS NOT NULL AND ${likeAny(to)} ORDER BY active_at DESC LIMIT 20;`,
  );
  if (!hits.length) {
    console.error(`signal: no conversation matched "${to}" — see: signal chats`);
    process.exit(1);
  }
  if (hits.length > 1) {
    console.error(`signal: "${to}" matched ${hits.length} conversations — narrow it, or pass an id:`);
    for (const c of hits) {
      console.error(`  ${clip(convoName(c), 28).padEnd(28)} ${kindOf(c).padEnd(5)} ${c.id}`);
    }
    process.exit(1);
  }
  return hits[0];
}

// Every read-ish command picks its conversation here: `--to` when given, else
// the configured contact (unchanged — this is the path the menubar/dm-mcp use).
// Returns the conversation `type` too, so group reads can attribute senders.
function target(to) {
  if (to) {
    const c = resolveTo(to);
    return { convoId: c.id, name: convoName(c), type: c.type, cfg: loadConfig() || {} };
  }
  const cfg = loadConfig() || needConfig();
  const convoId = resolveConversationId(cfg);
  if (!convoId) {
    console.error("signal: conversation not found");
    process.exit(1);
  }
  return { convoId, name: cfg.displayName, type: "private", cfg };
}

// Most-recent-first, with each conversation's last message + unread count. The
// LEFT JOIN pins one row per conversation (its newest message worth showing).
function recentConversations(limit) {
  return sql(
    `SELECT c.id, c.type, c.name, c.profileFullName, c.profileName, c.profileFamilyName, ` +
      `c.e164, c.active_at AS activeAt, ` +
      `(SELECT COUNT(*) FROM messages u WHERE u.conversationId=c.id ` +
      `AND u.type='incoming' AND u.readStatus=0) AS unread, ` +
      `m.type AS lastType, m.body AS lastBody, m.sent_at AS lastAt, ` +
      `m.hasAttachments AS lastHasAttachments ` +
      `FROM conversations c LEFT JOIN messages m ON m.rowid=(` +
      `SELECT rowid FROM messages x WHERE x.conversationId=c.id ` +
      `AND ((x.body IS NOT NULL AND x.body!='') OR x.hasAttachments=1) ` +
      `ORDER BY x.sent_at DESC LIMIT 1) ` +
      `WHERE c.active_at IS NOT NULL ORDER BY c.active_at DESC LIMIT ${Number(limit) || 20};`,
  );
}

// The groups-only filter behind `signal groups` / dm_groups (id, name, active).
function listGroups(limit) {
  return sql(
    `SELECT id, IFNULL(name,'(unnamed group)') AS name, active_at AS activeAt ` +
      `FROM conversations WHERE type='group' AND active_at IS NOT NULL ` +
      `ORDER BY active_at DESC LIMIT ${Number(limit) || 20};`,
  );
}

// ─── messages ────────────────────────────────────────────────────────────────

function recentMessages(convoId, limit) {
  // sent_at is unix ms. type is 'incoming'/'outgoing'. body holds the text.
  return sql(
    `SELECT rowid AS id, type, body, sent_at AS sentAt, hasAttachments ` +
      `FROM messages WHERE conversationId=${sqlStr(convoId)} ` +
      `AND ((body IS NOT NULL AND body!='') OR hasAttachments=1) ` +
      `ORDER BY sent_at DESC LIMIT ${Number(limit) || 15};`,
  ).reverse();
}

// Group history with sender attribution. In a group, each incoming message
// carries the author's ACI in `sourceServiceId`; join it back to that person's
// 1:1 conversation row to get their name. Outgoing messages are "me".
function recentGroupMessages(convoId, limit) {
  return sql(
    `SELECT m.rowid AS id, m.type, m.body, m.sent_at AS sentAt, m.hasAttachments, ` +
      `IFNULL(c.profileFullName, IFNULL(c.name, IFNULL(c.profileName, c.e164))) AS sender ` +
      `FROM messages m ` +
      `LEFT JOIN conversations c ON c.serviceId = m.sourceServiceId AND c.type='private' ` +
      `WHERE m.conversationId=${sqlStr(convoId)} ` +
      `AND ((m.body IS NOT NULL AND m.body!='') OR m.hasAttachments=1) ` +
      `ORDER BY m.sent_at DESC LIMIT ${Number(limit) || 15};`,
  ).reverse();
}

function unreadInbound(convoId) {
  // readStatus: 0 = unread (Signal's ReadStatus.Unread) for incoming messages.
  const r = sql(
    `SELECT COUNT(*) AS n FROM messages WHERE conversationId=${sqlStr(convoId)} ` +
      `AND type='incoming' AND readStatus=0;`,
  );
  return r.length ? r[0].n : 0;
}

function maxInboundRowid(convoId) {
  const r = sql(
    `SELECT IFNULL(MAX(rowid),0) AS r FROM messages ` +
      `WHERE conversationId=${sqlStr(convoId)} AND type='incoming';`,
  );
  return r.length ? r[0].r : 0;
}

// ─── attachments ─────────────────────────────────────────────────────────────

function recentAttachments(convoId, limit) {
  return sql(
    `SELECT messageId, path, localKey, size, contentType, fileName, version, sentAt ` +
      `FROM message_attachments WHERE conversationId=${sqlStr(convoId)} ` +
      `AND path IS NOT NULL ORDER BY sentAt DESC LIMIT ${Number(limit) || 10};`,
  );
}

// Decrypt a version-2 local attachment to `destPath`. Returns true on success.
function decryptAttachment(att, destPath) {
  const onDisk = join(ATTACH_DIR, att.path);
  if (!existsSync(onDisk)) return false;
  const buf = readFileSync(onDisk);
  if (!att.localKey) {
    // Unencrypted-at-rest (older/version 1) — copy through, trimmed to size.
    writeFileSync(destPath, att.size ? buf.subarray(0, att.size) : buf);
    return true;
  }
  const lk = Buffer.from(att.localKey, "base64"); // 32B AES + 32B HMAC
  const encKey = lk.subarray(0, 32);
  const macKey = lk.subarray(32, 64);
  const iv = buf.subarray(0, 16);
  const mac = buf.subarray(buf.length - 32);
  const body = buf.subarray(16, buf.length - 32);
  const want = crypto.createHmac("sha256", macKey).update(buf.subarray(0, buf.length - 32)).digest();
  if (!crypto.timingSafeEqual(want, mac)) return false; // tamper / wrong key
  const dec = crypto.createDecipheriv("aes-256-cbc", encKey, iv);
  dec.setAutoPadding(false);
  let pt = Buffer.concat([dec.update(body), dec.final()]);
  if (att.size) pt = pt.subarray(0, att.size); // strip bucket padding
  writeFileSync(destPath, pt);
  return true;
}

// ─── bell / state ────────────────────────────────────────────────────────────

function loadState() {
  try { return JSON.parse(readFileSync(STATE_PATH, "utf8")); } catch { return {}; }
}
function saveState(s) {
  mkdirSync(STATE_DIR, { recursive: true });
  writeFileSync(STATE_PATH, JSON.stringify(s, null, 2));
}
function loadAcknowledgements() {
  try { return JSON.parse(readFileSync(ACK_PATH, "utf8")); } catch { return {}; }
}
function saveAcknowledgements(s) {
  mkdirSync(STATE_DIR, { recursive: true });
  writeFileSync(ACK_PATH, JSON.stringify(s, null, 2));
}
function pendingInbound(convoId, afterRowid) {
  const r = sql(
    `SELECT COUNT(*) AS n FROM messages WHERE conversationId=${sqlStr(convoId)} ` +
      `AND type='incoming' AND rowid>${Number(afterRowid) || 0};`,
  );
  return r.length ? Number(r[0].n) || 0 : 0;
}
function acknowledgeConversation(convoId) {
  const acks = loadAcknowledgements();
  const rowid = maxInboundRowid(convoId);
  acks[convoId] = rowid;
  saveAcknowledgements(acks);
  return rowid;
}
function ringBell(cfg) {
  if (cfg.bellTty && existsSync(cfg.bellTty)) {
    try { writeFileSync(cfg.bellTty, "\x07"); } catch {}
  }
  // Audio is opt-in so older configs without a `sound` field stay quiet.
  if (cfg.sound === true) {
    const snd = cfg.soundFile || "/System/Library/Sounds/Glass.aiff";
    if (existsSync(snd)) spawnSync("/usr/bin/afplay", [snd], { stdio: "ignore" });
  }
}

// ─── formatting ──────────────────────────────────────────────────────────────

const clip = (s, n) => { s = String(s ?? ""); return s.length > n ? s.slice(0, n - 1) + "…" : s; };
function humanAgo(ms) {
  const s = Math.max(0, Math.floor((Date.now() - ms) / 1000));
  if (s < 60) return `${s}s`;
  if (s < 3600) return `${Math.floor(s / 60)}m`;
  if (s < 86400) return `${Math.floor(s / 3600)}h`;
  return `${Math.floor(s / 86400)}d`;
}
const arrow = (type) => (type === "outgoing" ? "→" : "←");
function print(obj) { process.stdout.write(JSON.stringify(obj) + "\n"); }

// ─── commands ────────────────────────────────────────────────────────────────

function needConfig() {
  ensureConfigStub();
  console.error(`signal: not configured — edit ${CONFIG_PATH}`);
  process.exit(2);
}

// The at-a-glance list: who's talking, how stale, how loud. Previews stay short
// — this gets read over shoulders and on screen-shares.
function cmdChats(n) {
  const convos = recentConversations(n || 20);
  for (const c of convos) {
    // Collapse newlines — a wrapped body would tear the table apart.
    const body = String(c.lastBody || (c.lastHasAttachments ? "📎 attachment" : "")).replace(/\s+/g, " ").trim();
    const preview = c.lastAt ? `${arrow(c.lastType)} ${clip(body, 44)}` : "";
    console.log(
      `${humanAgo(c.lastAt || c.activeAt).padStart(4)}  ` +
        `${(c.unread ? `●${c.unread}` : "").padStart(5)}  ` +
        `${kindOf(c).padEnd(5)}  ${clip(convoName(c), 26).padEnd(26)}  ${preview}`,
    );
  }
  if (!convos.length) console.log("(no conversations)");
}

function cmdStatus(to) {
  const cfg = loadConfig();
  if (!cfg && !to) { print({ configured: false, label: "Signal: setup" }); return; }
  const c = to ? resolveTo(to) : null;
  const convoId = c ? c.id : resolveConversationId(cfg);
  if (!convoId) { print({ configured: true, found: false, label: "Signal: ?" }); return; }
  const name = c ? convoName(c) : cfg.displayName;

  const systemUnread = unreadInbound(convoId);
  const last = recentMessages(convoId, 1)[0];
  const maxRowid = maxInboundRowid(convoId);

  const acks = loadAcknowledgements();
  if (!Number(acks[convoId])) {
    // Migration/first run: don't reinterpret the entire existing history as
    // newly un-ingested.
    acks[convoId] = maxRowid;
    saveAcknowledgements(acks);
  }
  const unread = pendingInbound(convoId, acks[convoId]);

  const state = loadState();
  const prev = state[convoId]?.lastInboundRowid || 0;
  let newSinceLast = false;
  if (maxRowid > prev) {
    newSinceLast = prev !== 0;
    if (newSinceLast && systemUnread > 0) ringBell(cfg || {}); // don't blast history on first run
    state[convoId] = { lastInboundRowid: maxRowid };
    saveState(state);
  }

  print({
    configured: true,
    found: true,
    name,
    unread,
    systemUnread,
    newSinceLast,
    label: unread > 0 ? `Signal: ${unread} un-ingested` : "Signal: ingested",
    last: last
      ? { dir: last.type, text: clip(last.body || (last.hasAttachments ? "📎 attachment" : ""), 80), ago: humanAgo(last.sentAt) }
      : null,
  });
}

// read [N], optionally targeted with `--to <name|id>`. No target → the
// configured 1:1 contact. When the resolved conversation is a group, each
// line is prefixed with the sender.
function cmdRead(n, to) {
  const { convoId, name, type } = target(to);
  if (to && name) console.log(`— ${name} —`);
  if (type === "group") {
    for (const m of recentGroupMessages(convoId, n || 15)) {
      const body = m.body || (m.hasAttachments ? "📎 attachment" : "");
      const who = m.type === "outgoing" ? "me" : (m.sender || "?");
      console.log(`${arrow(m.type)} ${humanAgo(m.sentAt).padStart(3)}  ${who}: ${body}`);
    }
  } else {
    for (const m of recentMessages(convoId, n || 15)) {
      const body = m.body || (m.hasAttachments ? "📎 attachment" : "");
      console.log(`${arrow(m.type)} ${humanAgo(m.sentAt).padStart(3)}  ${body}`);
    }
  }
  acknowledgeConversation(convoId);
}

function cmdAck(to) {
  const { convoId } = target(to);
  print({ acknowledged: true, rowid: acknowledgeConversation(convoId) });
}

// groups [N] — list the most-recently-active group conversations.
function cmdGroups(n) {
  const groups = listGroups(n || 20);
  if (!groups.length) { console.log("(no groups)"); return; }
  for (const g of groups) {
    console.log(`${humanAgo(g.activeAt).padStart(4)}  ${g.name}   [${g.id}]`);
  }
}

function cmdAttachments(n, to) {
  const { convoId } = target(to);
  const atts = recentAttachments(convoId, n || 10);
  atts.forEach((a, i) => {
    console.log(
      `[${i}] ${humanAgo(a.sentAt).padStart(3)}  ${(a.contentType || "?").padEnd(40)}  ` +
        `${((a.size || 0) / 1024).toFixed(0)}K  ${a.fileName || "(unnamed)"}`,
    );
  });
  if (!atts.length) console.log("(no downloaded attachments)");
}

function cmdSave(which, to) {
  const { convoId } = target(to);
  const atts = recentAttachments(convoId, 30);
  let picks;
  if (which === "all") picks = atts;
  else if (which === undefined || which === "latest") picks = atts.slice(0, 1);
  else picks = [atts[Number(which)]].filter(Boolean);
  if (!picks.length) { console.error("signal: nothing to save"); process.exit(1); }

  mkdirSync(SAVE_DIR, { recursive: true });
  const saved = [];
  for (const a of picks) {
    const name = (a.fileName || `${a.messageId}.bin`).replace(/[/:]/g, "_");
    const dest = join(SAVE_DIR, name);
    if (decryptAttachment(a, dest)) {
      saved.push(dest);
      console.log(dest);
      // PDFs go straight to the slab viewer.
      if ((a.contentType || "").includes("pdf") || /\.pdf$/i.test(name)) {
        spawnSync(join(dirname(new URL(import.meta.url).pathname), "slab-pdf"), [dest], { stdio: "ignore" });
      }
    } else {
      console.error(`signal: failed to decrypt ${name}`);
    }
  }
  return saved;
}

async function cmdTail(to) {
  const { convoId, name } = target(to);
  console.log(`— signal tail: ${name} (Ctrl-C to quit) —`);
  for (const m of recentMessages(convoId, 10)) {
    console.log(`${arrow(m.type)} ${m.body || (m.hasAttachments ? "📎 attachment" : "")}`);
  }
  let last = maxInboundRowid(convoId);
  acknowledgeConversation(convoId);
  for (;;) {
    await new Promise((r) => setTimeout(r, 3000));
    const cur = maxInboundRowid(convoId);
    if (cur > last) {
      const fresh = sql(
        `SELECT type, body, hasAttachments FROM messages WHERE conversationId=${sqlStr(convoId)} ` +
          `AND type='incoming' AND rowid>${last} ORDER BY rowid ASC;`,
      );
      for (const m of fresh) {
        process.stdout.write("\x07");
        console.log(`← ${m.body || (m.hasAttachments ? "📎 attachment" : "")}`);
      }
      last = cur;
      acknowledgeConversation(convoId);
    }
  }
}

// ─── main ────────────────────────────────────────────────────────────────────

// `--to <name|id>` can sit anywhere; lift it out before the positionals.
const argv = process.argv.slice(2);
const ti = argv.indexOf("--to");
const to = ti < 0 ? null : argv.splice(ti, 2)[1];
// A bare `--to` must not quietly fall back to the configured contact — that
// reads the wrong person while you think you retargeted.
if (ti >= 0 && !to) {
  console.error("signal: --to needs a value (a name substring, or a conversationId)");
  process.exit(1);
}
const [cmd, arg] = argv;

try {
  switch (cmd) {
    case "chats": case "recent": cmdChats(arg ? Number(arg) : 20); break;
    case "groups": cmdGroups(arg ? Number(arg) : 20); break;
    case "status": cmdStatus(to); break;
    case "ack": cmdAck(to); break;
    case "read": cmdRead(arg ? Number(arg) : 15, to); break;
    case "attachments": case "atts": cmdAttachments(arg ? Number(arg) : 10, to); break;
    case "save": cmdSave(arg, to); break;
    case "tail": await cmdTail(to); break;
    case "open": {
      const { convoId } = target(to);
      acknowledgeConversation(convoId);
      spawnSync("/usr/bin/open", ["-a", "Signal"], { stdio: "ignore" });
      break;
    }
    case "config": ensureConfigStub(); console.log(CONFIG_PATH); break;
    default:
      console.log(
        "usage: signal chats [N]|groups [N]|status|ack|read [N]|attachments [N]|save [N|all]|tail|open|config\n" +
          "       read-ish commands take --to <name|id> to target one conversation (dm or group)",
      );
  }
} catch (e) {
  if (cmd === "status") { print({ configured: !!loadConfig(), error: e.message }); }
  else { console.error("signal:", e.message); process.exit(1); }
}
