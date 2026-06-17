#!/usr/bin/env node
// signal.mjs — a tiny, generic Signal Desktop bridge for the slab menubar.
//
// Reads Signal Desktop's encrypted SQLCipher DB (read-only) for a configured
// contact, reports an unread/last-message summary as JSON, rings a bell on a
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
//   signal status            JSON summary; rings bell on new inbound
//   signal read [N]          print the last N messages (default 15)
//   signal tail              live terminal client (prints + BEL on new inbound)
//   signal attachments [N]   list the last N attachments (default 10), indexed
//   signal save [N|all]      decrypt recent attachments → temp dir; open PDFs
//   signal open              open Signal.app
//   signal config            print the resolved config path (creates a stub)

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
  sound: true,
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

function resolveConversationId(cfg) {
  const m = cfg.match || {};
  if (m.conversationId) return m.conversationId;
  if (m.e164) {
    const r = sql(`SELECT id FROM conversations WHERE e164=${sqlStr(m.e164)} LIMIT 1;`);
    if (r.length) return r[0].id;
  }
  if (m.contains) {
    const like = `%${m.contains.toLowerCase()}%`;
    const r = sql(
      `SELECT id FROM conversations WHERE type='private' AND (` +
        `LOWER(IFNULL(profileFullName,'')) LIKE ${sqlStr(like)} OR ` +
        `LOWER(IFNULL(profileName,'')||' '||IFNULL(profileFamilyName,'')) LIKE ${sqlStr(like)} OR ` +
        `LOWER(IFNULL(name,'')) LIKE ${sqlStr(like)} OR ` +
        `IFNULL(e164,'') LIKE ${sqlStr(like)}) ` +
        `ORDER BY active_at DESC LIMIT 1;`,
    );
    if (r.length) return r[0].id;
  }
  return null;
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
function ringBell(cfg) {
  if (cfg.bellTty && existsSync(cfg.bellTty)) {
    try { writeFileSync(cfg.bellTty, "\x07"); } catch {}
  }
  if (cfg.sound !== false) {
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

function cmdStatus() {
  const cfg = loadConfig();
  if (!cfg) { print({ configured: false, label: "Signal: setup" }); return; }
  const convoId = resolveConversationId(cfg);
  if (!convoId) { print({ configured: true, found: false, label: "Signal: ?" }); return; }

  const unread = unreadInbound(convoId);
  const last = recentMessages(convoId, 1)[0];
  const maxRowid = maxInboundRowid(convoId);

  const state = loadState();
  const prev = state[convoId]?.lastInboundRowid || 0;
  if (maxRowid > prev) {
    if (prev !== 0) ringBell(cfg); // don't blast history on first run
    state[convoId] = { lastInboundRowid: maxRowid };
    saveState(state);
  }

  print({
    configured: true,
    found: true,
    name: cfg.displayName,
    unread,
    label: unread > 0 ? `Signal: ${unread}` : "Signal",
    last: last
      ? { dir: last.type, text: clip(last.body || (last.hasAttachments ? "📎 attachment" : ""), 80), ago: humanAgo(last.sentAt) }
      : null,
  });
}

function cmdRead(n) {
  const cfg = loadConfig() || needConfig();
  const convoId = resolveConversationId(cfg);
  if (!convoId) { console.error("signal: conversation not found"); process.exit(1); }
  for (const m of recentMessages(convoId, n || 15)) {
    const body = m.body || (m.hasAttachments ? "📎 attachment" : "");
    console.log(`${arrow(m.type)} ${humanAgo(m.sentAt).padStart(3)}  ${body}`);
  }
}

function cmdAttachments(n) {
  const cfg = loadConfig() || needConfig();
  const convoId = resolveConversationId(cfg);
  if (!convoId) { console.error("signal: conversation not found"); process.exit(1); }
  const atts = recentAttachments(convoId, n || 10);
  atts.forEach((a, i) => {
    console.log(
      `[${i}] ${humanAgo(a.sentAt).padStart(3)}  ${(a.contentType || "?").padEnd(40)}  ` +
        `${((a.size || 0) / 1024).toFixed(0)}K  ${a.fileName || "(unnamed)"}`,
    );
  });
  if (!atts.length) console.log("(no downloaded attachments)");
}

function cmdSave(which) {
  const cfg = loadConfig() || needConfig();
  const convoId = resolveConversationId(cfg);
  if (!convoId) { console.error("signal: conversation not found"); process.exit(1); }
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

async function cmdTail() {
  const cfg = loadConfig() || needConfig();
  const convoId = resolveConversationId(cfg);
  if (!convoId) { console.error("signal: conversation not found"); process.exit(1); }
  console.log(`— signal tail: ${cfg.displayName} (Ctrl-C to quit) —`);
  for (const m of recentMessages(convoId, 10)) {
    console.log(`${arrow(m.type)} ${m.body || (m.hasAttachments ? "📎 attachment" : "")}`);
  }
  let last = maxInboundRowid(convoId);
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
    }
  }
}

// ─── main ────────────────────────────────────────────────────────────────────

const [cmd, arg] = process.argv.slice(2);
try {
  switch (cmd) {
    case "status": cmdStatus(); break;
    case "read": cmdRead(arg ? Number(arg) : 15); break;
    case "attachments": case "atts": cmdAttachments(arg ? Number(arg) : 10); break;
    case "save": cmdSave(arg); break;
    case "tail": await cmdTail(); break;
    case "open": spawnSync("/usr/bin/open", ["-a", "Signal"], { stdio: "ignore" }); break;
    case "config": ensureConfigStub(); console.log(CONFIG_PATH); break;
    default:
      console.log("usage: signal status|read [N]|attachments [N]|save [N|all]|tail|open|config");
  }
} catch (e) {
  if (cmd === "status") { print({ configured: !!loadConfig(), error: e.message }); }
  else { console.error("signal:", e.message); process.exit(1); }
}
