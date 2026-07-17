#!/usr/bin/env node
// imsg.mjs — a tiny, generic iMessage bridge for the slab menubar.
//
// Reads Messages' chat.db (read-only) for a configured contact, reports an
// unread/last-message summary as JSON, rings a terminal/audio bell when a
// NEW inbound message arrives, sends replies via Messages.app, and offers a
// dependency-free live `tail` client.
//
// This file ships in the PUBLIC aesthetic.computer repo, so it carries NO
// personal data: the contact's handle / display name / bell target all live
// in an UNTRACKED config at ~/.config/slab/imsg.json (see `imsg config`).
//
// macOS 26 (Tahoe) note: message.text is usually NULL — the body lives in
// the `attributedBody` typedstream blob, which we decode below.
//
// Subcommands:
//   imsg status        JSON summary to stdout; rings bell on new inbound
//   imsg chats [N]     recent indexed conversations (default 20)
//   imsg read [N]      recent messages; optional --to <name|handle>
//   imsg search <text> full-text search; optional --to and --limit
//   imsg index         incrementally refresh the private local FTS index
//   imsg use <name>    switch the notification/default contact safely
//   imsg send <text>   send to an explicitly selected contact via Messages.app
//   imsg react <kind>  classic Tapback on that contact's latest incoming message
//   imsg tail          live terminal client (prints + BEL on new inbound)
//   imsg open          open the Messages.app conversation
//   imsg config        print the resolved config path (and create a stub)

import { spawnSync } from "node:child_process";
import {
  chmodSync,
  existsSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { homedir } from "node:os";
import { join, dirname } from "node:path";

const HOME = homedir();
const CONFIG_PATH =
  process.env.SLAB_IMSG_CONFIG || join(HOME, ".config", "slab", "imsg.json");
const STATE_DIR = join(
  process.env.SLAB_HOME || join(HOME, ".local", "share", "slab"),
  "imsg",
);
const STATE_PATH = join(STATE_DIR, "state.json");
const ACK_PATH = join(STATE_DIR, "ack.json");
const INDEX_PATH = join(STATE_DIR, "index.sqlite");
const CHAT_DB = join(HOME, "Library", "Messages", "chat.db");
const SQLITE3 = "/usr/bin/sqlite3";
const DEFAULT_INDEX_DAYS = 730;

// ─── config ──────────────────────────────────────────────────────────────

const CONFIG_STUB = {
  _README:
    "slab imsg contact config — UNTRACKED, never committed. Fill in below.",
  displayName: "REPLACE_ME",
  handles: ["+15551234567", "name@example.com"],
  bellTty: "",
  bellTtyComment:
    "optional: a tty like /dev/ttys004 — writing BEL there flashes that terminal. `tty` in the pane you want flashed.",
  sound: true,
  soundFile: "",
  soundFileComment:
    "optional: path to an aiff/wav for afplay; default is the system Glass chime.",
};

function loadConfig() {
  if (!existsSync(CONFIG_PATH)) return null;
  try {
    const cfg = JSON.parse(readFileSync(CONFIG_PATH, "utf8"));
    const stub = (h) => /REPLACE|example\.com|5551234567/.test(h);
    const okHandles = (hs) => Array.isArray(hs) && hs.length > 0 && !hs.some(stub);
    // Valid if the legacy default contact is filled in, OR a `contacts` map has
    // at least one real entry (new multi-contact schema).
    const legacyOk = cfg && cfg.displayName !== "REPLACE_ME" && okHandles(cfg.handles);
    const contactsOk = cfg && cfg.contacts && typeof cfg.contacts === "object" &&
      Object.values(cfg.contacts).some((c) => c && okHandles(c.handles));
    if (!cfg || (!legacyOk && !contactsOk)) return null; // present but still a stub
    return cfg;
  } catch {
    return null;
  }
}

function ensureConfigStub() {
  mkdirSync(dirname(CONFIG_PATH), { recursive: true });
  if (!existsSync(CONFIG_PATH)) {
    writeFileSync(CONFIG_PATH, JSON.stringify(CONFIG_STUB, null, 2) + "\n", {
      mode: 0o600,
    });
  }
}

// ─── contacts ────────────────────────────────────────────────────────────
// A config may declare many named contacts (`contacts: { artur: {...} }`) with
// an optional `default`; the legacy top-level {displayName, handles} is treated
// as the "default" contact so old single-contact configs keep working.

function contactsMap(cfg) {
  const m = {};
  if (cfg && cfg.contacts && typeof cfg.contacts === "object") {
    for (const [k, v] of Object.entries(cfg.contacts)) {
      if (v && Array.isArray(v.handles) && v.handles.length) {
        m[k.toLowerCase()] = { displayName: v.displayName || k, handles: v.handles };
      }
    }
  }
  return m;
}

function defaultContact(cfg) {
  if (!cfg) return null;
  const m = contactsMap(cfg);
  if (cfg.default && m[String(cfg.default).toLowerCase()]) return m[String(cfg.default).toLowerCase()];
  if (Array.isArray(cfg.handles) && cfg.handles.length) {
    return { displayName: cfg.displayName || "default", handles: cfg.handles };
  }
  return Object.values(m)[0] || null;
}

// Resolve who a `--to` argument names: a contact key/displayName, a raw handle
// (+E164 or an email), or — when omitted — the default contact.
function resolveRecipient(cfg, toArg) {
  if (!toArg) {
    const d = defaultContact(cfg);
    if (!d) throw new Error("no default contact configured");
    return d;
  }
  const m = contactsMap(cfg);
  const key = String(toArg).toLowerCase();
  if (m[key]) return m[key];
  const hit = Object.entries(m).find(
    ([k, v]) => k.includes(key) || (v.displayName || "").toLowerCase().includes(key),
  );
  if (hit) return hit[1];
  if (/^\+[0-9]{6,}$/.test(toArg) || /@/.test(toArg)) {
    return { displayName: String(toArg), handles: [String(toArg)] };
  }
  throw new Error(`no contact matched "${toArg}" — known: ${Object.keys(m).join(", ") || "(none)"}`);
}

function resolveContactKey(cfg, name) {
  const m = contactsMap(cfg);
  const needle = String(name || "").trim().toLowerCase();
  if (!needle) throw new Error("a configured contact name is required");
  if (m[needle]) return needle;
  const hits = Object.entries(m).filter(
    ([key, value]) =>
      key.includes(needle) ||
      String(value.displayName || "").toLowerCase().includes(needle),
  );
  if (hits.length === 1) return hits[0][0];
  if (hits.length > 1) {
    throw new Error(`"${name}" is ambiguous — matched: ${hits.map(([key]) => key).join(", ")}`);
  }
  throw new Error(`no configured contact matched "${name}" — known: ${Object.keys(m).join(", ") || "(none)"}`);
}

function contactNameForHandles(cfg, handles, fallback = "") {
  const ids = new Set((handles || []).map((h) => String(h).toLowerCase()));
  for (const contact of Object.values(contactsMap(cfg))) {
    if (contact.handles.some((h) => ids.has(String(h).toLowerCase()))) {
      return contact.displayName;
    }
  }
  if (Array.isArray(cfg?.handles) && cfg.handles.some((h) => ids.has(String(h).toLowerCase()))) {
    return cfg.displayName || fallback;
  }
  return fallback || handles.join(", ") || "Unknown";
}

// ─── chat.db ─────────────────────────────────────────────────────────────

function sqlite(query) {
  // Read-only via URI; -json so blobs ride out as hex strings safely.
  const r = spawnSync(
    SQLITE3,
    // Messages briefly takes an exclusive lock while committing. A short busy
    // timeout lets the passive watcher ride through that normal write window
    // instead of reporting a false failure; the surrounding Slab poll still
    // has an 8-second hard timeout.
    ["-readonly", "-cmd", ".timeout 1500", "-json", `file:${CHAT_DB}?mode=ro`, query],
    { encoding: "utf8", maxBuffer: 64 * 1024 * 1024 },
  );
  if (r.status !== 0) {
    const e = new Error((r.stderr || "sqlite3 failed").trim());
    e.code = "SQLITE";
    throw e;
  }
  const out = (r.stdout || "").trim();
  return out ? JSON.parse(out) : [];
}

function sqlString(value) {
  return `'${String(value ?? "").replace(/\0/g, "").replace(/'/g, "''")}'`;
}

function indexSql(query, { json = true } = {}) {
  mkdirSync(STATE_DIR, { recursive: true });
  const args = ["-cmd", ".timeout 3000"];
  if (json) args.push("-json");
  args.push(INDEX_PATH, query);
  const r = spawnSync(SQLITE3, args, {
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
  });
  if (r.status !== 0) throw new Error((r.stderr || "iMessage index failed").trim());
  if (existsSync(INDEX_PATH)) chmodSync(INDEX_PATH, 0o600);
  const out = (r.stdout || "").trim();
  return json && out ? JSON.parse(out) : [];
}

function ensureIndexSchema() {
  indexSql(
    `PRAGMA journal_mode=DELETE;
     CREATE TABLE IF NOT EXISTS meta (
       key TEXT PRIMARY KEY,
       value TEXT NOT NULL
     );
     CREATE TABLE IF NOT EXISTS messages (
       source_rowid INTEGER NOT NULL,
       chat_id INTEGER NOT NULL,
       chat_guid TEXT NOT NULL DEFAULT '',
       display_name TEXT NOT NULL DEFAULT '',
       handles TEXT NOT NULL DEFAULT '[]',
       from_me INTEGER NOT NULL DEFAULT 0,
       body TEXT NOT NULL DEFAULT '',
       at INTEGER NOT NULL DEFAULT 0,
       PRIMARY KEY (source_rowid, chat_id)
     );
     CREATE INDEX IF NOT EXISTS messages_chat_at ON messages(chat_id, at DESC);
     CREATE INDEX IF NOT EXISTS messages_at ON messages(at DESC);
     CREATE VIRTUAL TABLE IF NOT EXISTS messages_fts USING fts5(
       body, display_name, handles,
       content='messages', content_rowid='rowid', tokenize='unicode61'
     );
     CREATE TRIGGER IF NOT EXISTS messages_ai AFTER INSERT ON messages BEGIN
       INSERT INTO messages_fts(rowid, body, display_name, handles)
       VALUES (new.rowid, new.body, new.display_name, new.handles);
     END;
     CREATE TRIGGER IF NOT EXISTS messages_ad AFTER DELETE ON messages BEGIN
       INSERT INTO messages_fts(messages_fts, rowid, body, display_name, handles)
       VALUES ('delete', old.rowid, old.body, old.display_name, old.handles);
     END;
     CREATE TRIGGER IF NOT EXISTS messages_au AFTER UPDATE ON messages BEGIN
       INSERT INTO messages_fts(messages_fts, rowid, body, display_name, handles)
       VALUES ('delete', old.rowid, old.body, old.display_name, old.handles);
       INSERT INTO messages_fts(rowid, body, display_name, handles)
       VALUES (new.rowid, new.body, new.display_name, new.handles);
     END;`,
    { json: false },
  );
}

function indexMeta(key) {
  return indexSql(`SELECT value FROM meta WHERE key=${sqlString(key)} LIMIT 1;`)[0]?.value;
}

function sourceIndexBatch(afterRowid, { all, limit = 750 } = {}) {
  const floor = all
    ? ""
    : `AND date >= (strftime('%s','now','-${DEFAULT_INDEX_DAYS} days') - ${APPLE_EPOCH}) * 1000000000`;
  return sqlite(
    `SELECT m.ROWID AS id, cmj.chat_id AS chatId,
            IFNULL(c.guid, '') AS chatGuid,
            IFNULL(c.display_name, '') AS chatName,
            IFNULL(c.chat_identifier, '') AS chatIdentifier,
            m.is_from_me AS fromMe, m.text AS text,
            hex(m.attributedBody) AS body, m.date AS date,
            IFNULL((
              SELECT group_concat(participant.id, char(31))
              FROM (
                SELECT DISTINCT h.id AS id
                FROM chat_handle_join chj
                JOIN handle h ON h.ROWID=chj.handle_id
                WHERE chj.chat_id=c.ROWID
                ORDER BY h.id
              ) participant
            ), '') AS handles
     FROM message m
     JOIN chat_message_join cmj ON cmj.message_id=m.ROWID
     JOIN chat c ON c.ROWID=cmj.chat_id
     WHERE m.ROWID IN (
       SELECT ROWID FROM message
       WHERE ROWID > ${Number(afterRowid) || 0} ${floor}
       ORDER BY ROWID ASC LIMIT ${Math.max(1, Math.min(2000, Number(limit) || 750))}
     )
     ORDER BY m.ROWID ASC, cmj.chat_id ASC;`,
  );
}

function syncIndex({ all = false, rebuild = false } = {}) {
  if (rebuild && existsSync(INDEX_PATH)) {
    rmSync(INDEX_PATH, { force: true });
  }
  ensureIndexSchema();
  if (all && indexMeta("full") !== "1" && Number(indexMeta("last_rowid"))) {
    rmSync(INDEX_PATH, { force: true });
    ensureIndexSchema();
  }
  let lastRowid = Number(indexMeta("last_rowid")) || 0;
  const cfg = loadConfig() || {};
  let added = 0;
  let batches = 0;

  for (;;) {
    const rows = sourceIndexBatch(lastRowid, { all });
    if (!rows.length) break;
    let batchMax = lastRowid;
    const statements = [];
    for (const row of rows) {
      batchMax = Math.max(batchMax, Number(row.id) || 0);
      const handles = String(row.handles || "").split("\x1f").filter(Boolean);
      const fallback = row.chatName || (handles.length === 1 ? handles[0] : row.chatIdentifier);
      const displayName = contactNameForHandles(cfg, handles, fallback);
      const body = decodeBody(row.text, row.body);
      statements.push(
        `INSERT INTO messages
          (source_rowid, chat_id, chat_guid, display_name, handles, from_me, body, at)
         VALUES (
          ${Number(row.id) || 0}, ${Number(row.chatId) || 0}, ${sqlString(row.chatGuid)},
          ${sqlString(displayName)}, ${sqlString(JSON.stringify(handles))}, ${row.fromMe ? 1 : 0},
          ${sqlString(body)}, ${appleNsToUnix(row.date)}
         )
         ON CONFLICT(source_rowid, chat_id) DO UPDATE SET
          chat_guid=excluded.chat_guid, display_name=excluded.display_name,
          handles=excluded.handles, from_me=excluded.from_me,
          body=excluded.body, at=excluded.at;`,
      );
    }
    indexSql(
      `BEGIN IMMEDIATE;
       ${statements.join("\n")}
       INSERT INTO meta(key,value) VALUES('last_rowid',${sqlString(batchMax)})
       ON CONFLICT(key) DO UPDATE SET value=excluded.value;
       COMMIT;`,
      { json: false },
    );
    added += rows.length;
    batches += 1;
    lastRowid = batchMax;
  }

  if (all) {
    indexSql(
      `INSERT INTO meta(key,value) VALUES('full','1')
       ON CONFLICT(key) DO UPDATE SET value='1';`,
      { json: false },
    );
  }
  const total = Number(indexSql("SELECT COUNT(*) AS n FROM messages;")[0]?.n || 0);
  return { path: INDEX_PATH, added, total, batches, full: indexMeta("full") === "1", lastRowid };
}

// Decode the message body. On modern macOS `text` is usually NULL and the
// content is an NSAttributedString typedstream in `attributedBody`. The
// canonical heuristic: after the "NSString" class marker there is a 5-byte
// class-chain prelude, then a length (1 byte; or 0x81→u16, 0x82→u32 LE),
// then the UTF-8 bytes.
function decodeBody(text, attributedHex) {
  if (text != null && text !== "") return String(text);
  if (!attributedHex) return "";
  const buf = Buffer.from(attributedHex, "hex");
  const marker = buf.indexOf("NSString");
  if (marker === -1) return "";
  let i = marker + 8 + 5; // "NSString" + \x01\x94\x84\x01\x2b
  if (i >= buf.length) return "";
  let len = buf[i];
  i += 1;
  if (len === 0x81) {
    len = buf.readUInt16LE(i);
    i += 2;
  } else if (len === 0x82) {
    len = buf.readUInt32LE(i);
    i += 4;
  }
  if (len <= 0 || i + len > buf.length) return "";
  // Strip object-replacement chars (attachment placeholders) and trim.
  return buf
    .toString("utf8", i, i + len)
    .replace(/￼|�/g, "")
    .trim();
}

const APPLE_EPOCH = 978307200; // 2001-01-01 → unix seconds
const appleNsToUnix = (ns) => Math.floor(Number(ns) / 1e9) + APPLE_EPOCH;

function handleList(cfg) {
  const c = defaultContact(cfg);
  return (c ? c.handles : [])
    .map((h) => `'${String(h).replace(/'/g, "''")}'`)
    .join(",");
}

// Latest inbound message + unread count for the configured contact.
function fetchSummary(cfg) {
  const ids = handleList(cfg);
  const unread = Number(
    sqlite(
      `SELECT COUNT(*) AS n FROM message m
       JOIN handle h ON h.ROWID = m.handle_id
       WHERE h.id IN (${ids}) AND m.is_from_me=0 AND m.is_read=0;`,
    )[0]?.n || 0,
  );
  const maxInbound = Number(
    sqlite(
      `SELECT IFNULL(MAX(m.ROWID),0) AS r FROM message m
       JOIN handle h ON h.ROWID = m.handle_id
       WHERE h.id IN (${ids}) AND m.is_from_me=0;`,
    )[0]?.r || 0,
  );
  const lastRows = sqlite(
    `SELECT m.is_from_me AS fromMe, m.text AS text,
            hex(m.attributedBody) AS body, m.date AS date
     FROM message m
     JOIN handle h ON h.ROWID = m.handle_id
     WHERE h.id IN (${ids})
     ORDER BY m.date DESC LIMIT 1;`,
  );
  let last = null;
  if (lastRows[0]) {
    const row = lastRows[0];
    last = {
      fromMe: !!row.fromMe,
      text: decodeBody(row.text, row.body),
      at: appleNsToUnix(row.date),
    };
  }
  return { unread, maxInbound, last };
}

// ─── state (so the bell fires once per new message, not every poll) ──────

function loadState() {
  try {
    return JSON.parse(readFileSync(STATE_PATH, "utf8"));
  } catch {
    return { lastNotifiedRowid: 0, primed: false };
  }
}

function saveState(s) {
  mkdirSync(STATE_DIR, { recursive: true });
  writeFileSync(STATE_PATH, JSON.stringify(s));
}

// Slab owns an acknowledgement cursor separate from Messages.app's read bit.
// Reading chat.db is intentionally passive, so Apple may keep `is_read=0`
// even after an agent has ingested and answered a message. The cursor is what
// the menubar means by "un-ingested" and lives in its own file so the 3-second
// status poll cannot race an acknowledgement write.
function loadAcknowledgedRowid() {
  try {
    return Number(JSON.parse(readFileSync(ACK_PATH, "utf8")).rowid) || 0;
  } catch {
    return 0;
  }
}

function saveAcknowledgedRowid(rowid) {
  mkdirSync(STATE_DIR, { recursive: true });
  writeFileSync(ACK_PATH, JSON.stringify({ rowid: Number(rowid) || 0 }));
}

function pendingInbound(cfg, afterRowid) {
  const ids = handleList(cfg);
  return Number(
    sqlite(
      `SELECT COUNT(*) AS n FROM message m
       JOIN handle h ON h.ROWID = m.handle_id
       WHERE h.id IN (${ids}) AND m.is_from_me=0
         AND m.ROWID > ${Number(afterRowid) || 0};`,
    )[0]?.n || 0,
  );
}

function acknowledge(cfg) {
  const rowid = fetchSummary(cfg).maxInbound;
  saveAcknowledgedRowid(rowid);
  return rowid;
}

// ─── bell ────────────────────────────────────────────────────────────────

function ringBell(cfg) {
  if (cfg.bellTty) {
    try {
      writeFileSync(cfg.bellTty, ""); // BEL → terminal flash/beep
    } catch {
      /* tty may be gone; ignore */
    }
  }
  if (cfg.sound !== false) {
    const snd =
      cfg.soundFile && existsSync(cfg.soundFile)
        ? cfg.soundFile
        : "/System/Library/Sounds/Glass.aiff";
    if (existsSync(snd)) {
      spawnSync("/usr/bin/afplay", [snd], { stdio: "ignore" });
    } else {
      spawnSync("/usr/bin/osascript", ["-e", "beep 1"], { stdio: "ignore" });
    }
  }
}

// ─── send ────────────────────────────────────────────────────────────────

function sendMessage(handles, body) {
  const to = String(handles[0]);
  // buddy-of-first-service path is the most reliable across macOS versions.
  const script = `
on run argv
  set msg to item 1 of argv
  set dest to item 2 of argv
  tell application "Messages"
    set svc to 1st account whose service type = iMessage
    set bud to participant dest of svc
    send msg to bud
  end tell
end run`;
  const r = spawnSync(
    "/usr/bin/osascript",
    ["-e", script, body, to],
    { encoding: "utf8" },
  );
  if (r.status !== 0) {
    // Fallback: SMS/last-used service via the generic `buddy` form.
    const fb = `on run argv
  tell application "Messages" to send (item 1 of argv) to buddy (item 2 of argv)
end run`;
    const r2 = spawnSync("/usr/bin/osascript", ["-e", fb, body, to], {
      encoding: "utf8",
    });
    if (r2.status !== 0) {
      throw new Error((r.stderr || r2.stderr || "send failed").trim());
    }
  }
}

const TAPBACKS = new Map([
  ["heart", { key: "1", label: "heart" }],
  ["love", { key: "1", label: "heart" }],
  ["thumbs-up", { key: "2", label: "thumbs up" }],
  ["thumbsup", { key: "2", label: "thumbs up" }],
  ["like", { key: "2", label: "thumbs up" }],
  ["thumbs-down", { key: "3", label: "thumbs down" }],
  ["thumbsdown", { key: "3", label: "thumbs down" }],
  ["dislike", { key: "3", label: "thumbs down" }],
  ["haha", { key: "4", label: "Ha Ha" }],
  ["laugh", { key: "4", label: "Ha Ha" }],
  ["emphasis", { key: "5", label: "emphasis" }],
  ["!!", { key: "5", label: "emphasis" }],
  ["question", { key: "6", label: "question mark" }],
  ["?", { key: "6", label: "question mark" }],
]);

function reactToLatest(handles, kind) {
  const spec = TAPBACKS.get(String(kind).trim().toLowerCase());
  if (!spec) {
    throw new Error("unknown Tapback — use heart, thumbs-up, thumbs-down, haha, emphasis, or question");
  }
  const to = String(handles[0]);
  const opened = spawnSync("/usr/bin/open", [`imessage://${to}`], { encoding: "utf8" });
  if (opened.status !== 0) throw new Error((opened.stderr || "could not open Messages conversation").trim());
  const script = `
tell application "Messages" to activate
delay 1
tell application "System Events"
  tell process "Messages"
    keystroke "t" using command down
    delay 0.25
    keystroke "${spec.key}"
  end tell
end tell`;
  const reacted = spawnSync("/usr/bin/osascript", ["-e", script], { encoding: "utf8" });
  if (reacted.status !== 0) throw new Error((reacted.stderr || "Tapback failed").trim());
  return spec.label;
}

// ─── command: status ─────────────────────────────────────────────────────

function cmdStatus() {
  const cfg = loadConfig();
  if (!cfg) {
    ensureConfigStub();
    print({
      configured: false,
      label: "iMessage: set up needed",
      hint: CONFIG_PATH,
    });
    return;
  }
  let s;
  try {
    s = fetchSummary(cfg);
  } catch (e) {
    const noFDA = /authorization denied|unable to open|not permitted/i.test(
      e.message,
    );
    const watched = defaultContact(cfg);
    print({
      configured: true,
      label: noFDA
        ? `${watched?.displayName || cfg.displayName}: needs Full Disk Access`
        : `${watched?.displayName || cfg.displayName}: read error`,
      error: e.message,
    });
    return;
  }

  const st = loadState();
  let acknowledgedRowid = loadAcknowledgedRowid();
  if (!acknowledgedRowid) {
    // Migration/first run: baseline history instead of presenting every old
    // message as newly un-ingested.
    acknowledgedRowid = s.maxInbound;
    saveAcknowledgedRowid(acknowledgedRowid);
  }
  let newSinceLast = false;
  if (!st.primed) {
    // First run: baseline silently, never blast the bell for history.
    st.primed = true;
    st.lastNotifiedRowid = s.maxInbound;
  } else if (s.maxInbound > st.lastNotifiedRowid) {
    // Arrival is an event even if another Apple device marked the thread read
    // before this poll. Slab/prox consumers still need the edge; only the
    // audible bell remains conditional on the message being unread here.
    newSinceLast = true;
    st.lastNotifiedRowid = s.maxInbound;
    if (s.unread > 0) ringBell(cfg);
  }
  saveState(st);

  const pending = pendingInbound(cfg, acknowledgedRowid);

  const name = defaultContact(cfg)?.displayName || cfg.displayName;
  const label =
    pending > 0
      ? `${name}: ${pending} un-ingested`
      : s.last
        ? `${name}: ingested`
        : `${name}: —`;
  print({
    configured: true,
    label,
    displayName: name,
    // `unread` remains the compatibility field consumed by dm-mcp + Swift,
    // but now means Slab-un-ingested. Apple's independent read count remains
    // available for diagnostics.
    unread: pending,
    systemUnread: s.unread,
    newSinceLast,
    last: s.last
      ? {
          fromMe: s.last.fromMe,
          text: clip(s.last.text, 80),
          ago: humanAgo(s.last.at),
        }
      : null,
  });
}

function takeFlag(args, flag) {
  const i = args.indexOf(flag);
  if (i < 0) return null;
  const value = args[i + 1];
  args.splice(i, value === undefined ? 1 : 2);
  return value ?? "";
}

function recipientWhere(recipient, alias = "m") {
  const handles = (recipient?.handles || []).map(sqlString).join(",");
  if (!handles) throw new Error("recipient has no iMessage handles");
  return `EXISTS (
    SELECT 1 FROM json_each(${alias}.handles) participant
    WHERE participant.value IN (${handles})
  )`;
}

function stamp(unix) {
  if (!unix) return "unknown time";
  const d = new Date(Number(unix) * 1000);
  const pad = (n) => String(n).padStart(2, "0");
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}`;
}

function formatMessages(rows) {
  return rows.map((row) => {
    const direction = row.from_me ? "→ you" : `← ${row.display_name || "them"}`;
    return `${stamp(row.at)}  ${direction} | ${row.body || "[attachment or empty message]"}`;
  }).join("\n");
}

function cmdIndex(args = []) {
  const all = args.includes("--all");
  const rebuild = args.includes("--rebuild");
  print(syncIndex({ all, rebuild }));
}

function cmdChats(args = []) {
  syncIndex();
  const n = Math.max(1, Math.min(100, Number(args[0]) || 20));
  const rows = indexSql(
    `WITH ranked AS (
       SELECT m.*,
              ROW_NUMBER() OVER (PARTITION BY chat_id ORDER BY at DESC, source_rowid DESC) AS rank,
              COUNT(*) OVER (PARTITION BY chat_id) AS message_count
       FROM messages m
     )
     SELECT chat_id, chat_guid, display_name, handles, from_me, body, at, message_count
     FROM ranked WHERE rank=1
     ORDER BY at DESC LIMIT ${n};`,
  );
  const out = rows.map((row) => {
    const direction = row.from_me ? "→" : "←";
    const handles = JSON.parse(row.handles || "[]").join(", ");
    return `${row.display_name || handles || `chat ${row.chat_id}`} — ${stamp(row.at)} · ${row.message_count} indexed · ${direction} "${clip(row.body, 90)}"${handles ? ` · ${handles}` : ""}`;
  });
  process.stdout.write((out.join("\n") || "(no indexed iMessage conversations)") + "\n");
}

function cmdRead(args = []) {
  syncIndex();
  const cfg = loadConfig();
  if (!cfg) throw new Error(`No contacts configured. Edit ${CONFIG_PATH}`);
  const argv = [...args];
  const toArg = takeFlag(argv, "--to");
  const n = Math.max(1, Math.min(200, Number(argv[0]) || 15));
  const recipient = resolveRecipient(cfg, toArg);
  const rows = indexSql(
    `SELECT source_rowid, chat_id, display_name, handles, from_me, body, at
     FROM messages m
     WHERE ${recipientWhere(recipient)}
     ORDER BY at DESC, source_rowid DESC LIMIT ${n};`,
  ).reverse();
  process.stdout.write((formatMessages(rows) || `(no indexed messages for ${recipient.displayName})`) + "\n");
}

function ftsQuery(input) {
  const terms = String(input || "").match(/[\p{L}\p{N}_@.+-]+/gu) || [];
  if (!terms.length) throw new Error("search text is required");
  return terms.map((term) => `"${term.replace(/"/g, '""')}"`).join(" AND ");
}

function cmdSearch(args = []) {
  syncIndex();
  const cfg = loadConfig();
  const argv = [...args];
  const toArg = takeFlag(argv, "--to");
  const limitArg = takeFlag(argv, "--limit");
  const limit = Math.max(1, Math.min(200, Number(limitArg) || 25));
  const query = argv.join(" ").trim();
  const recipient = toArg ? resolveRecipient(cfg, toArg) : null;
  const scoped = recipient ? `AND ${recipientWhere(recipient)}` : "";
  const rows = indexSql(
    `SELECT m.source_rowid, m.chat_id, m.display_name, m.handles,
            m.from_me, m.body, m.at
     FROM messages_fts
     JOIN messages m ON m.rowid=messages_fts.rowid
     WHERE messages_fts MATCH ${sqlString(ftsQuery(query))} ${scoped}
     ORDER BY m.at DESC, m.source_rowid DESC LIMIT ${limit};`,
  );
  process.stdout.write((formatMessages(rows) || `(no indexed iMessages matching "${query}")`) + "\n");
}

function cmdUse(name) {
  const cfg = loadConfig();
  if (!cfg) throw new Error(`No contacts configured. Edit ${CONFIG_PATH}`);
  const key = resolveContactKey(cfg, name);
  cfg.default = key;
  writeFileSync(CONFIG_PATH, JSON.stringify(cfg, null, 2) + "\n", { mode: 0o600 });
  const summary = fetchSummary(cfg);
  saveAcknowledgedRowid(summary.maxInbound);
  saveState({ primed: true, lastNotifiedRowid: summary.maxInbound });
  print({ default: key, displayName: defaultContact(cfg).displayName });
}

// ─── command: tail (live terminal client) ────────────────────────────────

async function cmdTail() {
  const cfg = loadConfig();
  if (!cfg) {
    console.error(`No contact configured. Edit ${CONFIG_PATH}`);
    process.exit(1);
  }
  const watched = defaultContact(cfg);
  const ids = handleList(cfg);
  let seen = Number(
    sqlite(
      `SELECT IFNULL(MAX(ROWID),0) AS r FROM message m
       JOIN handle h ON h.ROWID=m.handle_id WHERE h.id IN (${ids});`,
    )[0]?.r || 0,
  );
  acknowledge(cfg);
  process.stdout.write(
    `\x1b[2m── imsg · ${watched.displayName} · type to reply, Ctrl-C to quit ──\x1b[0m\n`,
  );

  const readline = await import("node:readline");
  const rl = readline.createInterface({ input: process.stdin });
  rl.on("line", (line) => {
    const body = line.trim();
    if (!body) return;
    try {
      sendMessage(defaultContact(cfg).handles, body);
      acknowledge(cfg);
      process.stdout.write(`\x1b[36m  you ›\x1b[0m ${body}\n`);
    } catch (e) {
      process.stdout.write(`\x1b[31m  send failed: ${e.message}\x1b[0m\n`);
    }
  });

  const poll = () => {
    let rows;
    try {
      rows = sqlite(
        `SELECT m.ROWID AS id, m.is_from_me AS fromMe, m.text AS text,
                hex(m.attributedBody) AS body, m.date AS date
         FROM message m JOIN handle h ON h.ROWID=m.handle_id
         WHERE h.id IN (${ids}) AND m.ROWID > ${seen}
         ORDER BY m.ROWID ASC;`,
      );
    } catch (e) {
      process.stdout.write(`\x1b[31m  db: ${e.message}\x1b[0m\n`);
      return;
    }
    for (const r of rows) {
      seen = Math.max(seen, Number(r.id));
      const txt = decodeBody(r.text, r.body);
      if (!txt) continue;
      if (r.fromMe) {
        process.stdout.write(`\x1b[36m  you ›\x1b[0m ${txt}\n`);
      } else {
        ringBell(cfg);
        process.stdout.write(
          `\x07\x1b[35m  ${watched.displayName} ›\x1b[0m ${txt}\n`,
        );
        saveAcknowledgedRowid(Number(r.id));
      }
    }
  };
  setInterval(poll, 2000);
}

// ─── helpers ─────────────────────────────────────────────────────────────

function clip(s, n) {
  s = (s || "").replace(/\s+/g, " ").trim();
  return s.length > n ? s.slice(0, n - 1) + "…" : s;
}

function humanAgo(unix) {
  const d = Math.max(0, Math.floor(Date.now() / 1000 - unix));
  if (d < 60) return `${d}s`;
  if (d < 3600) return `${Math.floor(d / 60)}m`;
  if (d < 86400) return `${Math.floor(d / 3600)}h`;
  return `${Math.floor(d / 86400)}d`;
}

function print(obj) {
  process.stdout.write(JSON.stringify(obj) + "\n");
}

// ─── dispatch ────────────────────────────────────────────────────────────

const [cmd, ...rest] = process.argv.slice(2);
try {
  switch (cmd) {
    case "status":
      cmdStatus();
      break;
    case "index":
      cmdIndex(rest);
      break;
    case "chats":
      cmdChats(rest);
      break;
    case "read":
      cmdRead(rest);
      break;
    case "search":
      cmdSearch(rest);
      break;
    case "use":
      cmdUse(rest.join(" ").trim());
      break;
    case "ack": {
      const cfg = loadConfig();
      if (!cfg) {
        console.error(`No contact configured. Edit ${CONFIG_PATH}`);
        process.exit(1);
      }
      print({ acknowledged: true, rowid: acknowledge(cfg) });
      break;
    }
    case "resolve": {
      const cfg = loadConfig();
      if (!cfg) {
        console.error(`No contact configured. Edit ${CONFIG_PATH}`);
        process.exit(1);
      }
      const ti = rest.indexOf("--to");
      const toArg = ti >= 0 ? rest[ti + 1] : rest[0];
      const rcpt = resolveRecipient(cfg, toArg);
      print({ displayName: rcpt.displayName });
      break;
    }
    case "send": {
      const cfg = loadConfig();
      if (!cfg) {
        console.error(`No contact configured. Edit ${CONFIG_PATH}`);
        process.exit(1);
      }
      // Optional `--to <name|handle>` selects a contact; default otherwise.
      const args = [...rest];
      let toArg = null;
      const ti = args.indexOf("--to");
      if (ti >= 0) { toArg = args[ti + 1]; args.splice(ti, 2); }
      const body = args.join(" ").trim();
      if (!body) {
        console.error("usage: imsg send <text> [--to <name|handle>]");
        process.exit(1);
      }
      const rcpt = resolveRecipient(cfg, toArg);
      sendMessage(rcpt.handles, body);
      const watched = defaultContact(cfg);
      if (watched && rcpt.handles.some((h) => watched.handles.includes(h))) {
        acknowledge(cfg);
      }
      break;
    }
    case "react": {
      const cfg = loadConfig();
      if (!cfg) {
        console.error(`No contact configured. Edit ${CONFIG_PATH}`);
        process.exit(1);
      }
      const args = [...rest];
      let toArg = null;
      const ti = args.indexOf("--to");
      if (ti >= 0) { toArg = args[ti + 1]; args.splice(ti, 2); }
      if (!toArg) {
        console.error("usage: imsg react <kind> --to <name|handle>");
        process.exit(1);
      }
      const kind = args.join(" ").trim();
      const rcpt = resolveRecipient(cfg, toArg);
      const reaction = reactToLatest(rcpt.handles, kind);
      const watched = defaultContact(cfg);
      if (watched && rcpt.handles.some((h) => watched.handles.includes(h))) {
        acknowledge(cfg);
      }
      print({ displayName: rcpt.displayName, reaction });
      break;
    }
    case "tail":
      await cmdTail();
      break;
    case "open": {
      const cfg = loadConfig();
      const to = cfg ? (defaultContact(cfg)?.handles[0] || "") : "";
      spawnSync("/usr/bin/open", [`imessage://${to}`], { stdio: "ignore" });
      if (cfg) acknowledge(cfg);
      break;
    }
    case "config":
      ensureConfigStub();
      process.stdout.write(CONFIG_PATH + "\n");
      break;
    default:
      console.error(
        "usage: imsg status|chats [N]|read [N] [--to <name|handle>]|search <text> [--to <name|handle>] [--limit N]|index [--all] [--rebuild]|use <contact>|ack|resolve [--to] <name|handle>|send <text> [--to <name|handle>]|react <kind> --to <name|handle>|tail|open|config",
      );
      process.exit(1);
  }
} catch (e) {
  // status must never break its caller (the menubar) — emit JSON instead.
  if (cmd === "status") {
    print({ configured: false, label: "iMessage: error", error: e.message });
  } else {
    console.error(e.message);
    process.exit(1);
  }
}
