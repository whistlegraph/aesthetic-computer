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
//   imsg send <text>   send to the first configured handle via Messages.app
//   imsg tail          live terminal client (prints + BEL on new inbound)
//   imsg open          open the Messages.app conversation
//   imsg config        print the resolved config path (and create a stub)

import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  readFileSync,
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
const CHAT_DB = join(HOME, "Library", "Messages", "chat.db");
const SQLITE3 = "/usr/bin/sqlite3";

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
    if (
      !cfg ||
      cfg.displayName === "REPLACE_ME" ||
      !Array.isArray(cfg.handles) ||
      cfg.handles.length === 0 ||
      cfg.handles.some((h) => /REPLACE|example\.com|5551234567/.test(h))
    ) {
      return null; // present but still a stub
    }
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

// ─── chat.db ─────────────────────────────────────────────────────────────

function sqlite(query) {
  // Read-only via URI; -json so blobs ride out as hex strings safely.
  const r = spawnSync(
    SQLITE3,
    ["-readonly", "-json", `file:${CHAT_DB}?mode=ro`, query],
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
  return cfg.handles
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

function sendMessage(cfg, body) {
  const to = String(cfg.handles[0]);
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
    print({
      configured: true,
      label: noFDA
        ? `${cfg.displayName}: needs Full Disk Access`
        : `${cfg.displayName}: read error`,
      error: e.message,
    });
    return;
  }

  const st = loadState();
  let newSinceLast = false;
  if (!st.primed) {
    // First run: baseline silently, never blast the bell for history.
    st.primed = true;
    st.lastNotifiedRowid = s.maxInbound;
  } else if (s.maxInbound > st.lastNotifiedRowid && s.unread > 0) {
    newSinceLast = true;
    st.lastNotifiedRowid = s.maxInbound;
    ringBell(cfg);
  } else if (s.maxInbound > st.lastNotifiedRowid) {
    // New inbound that's already marked read elsewhere — track, don't ring.
    st.lastNotifiedRowid = s.maxInbound;
  }
  saveState(st);

  const name = cfg.displayName;
  const label =
    s.unread > 0
      ? `${name}: ${s.unread} new`
      : s.last
        ? `${name}: all read`
        : `${name}: —`;
  print({
    configured: true,
    label,
    displayName: name,
    unread: s.unread,
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

// ─── command: tail (live terminal client) ────────────────────────────────

async function cmdTail() {
  const cfg = loadConfig();
  if (!cfg) {
    console.error(`No contact configured. Edit ${CONFIG_PATH}`);
    process.exit(1);
  }
  const ids = handleList(cfg);
  let seen = Number(
    sqlite(
      `SELECT IFNULL(MAX(ROWID),0) AS r FROM message m
       JOIN handle h ON h.ROWID=m.handle_id WHERE h.id IN (${ids});`,
    )[0]?.r || 0,
  );
  process.stdout.write(
    `\x1b[2m── imsg · ${cfg.displayName} · type to reply, Ctrl-C to quit ──\x1b[0m\n`,
  );

  const readline = await import("node:readline");
  const rl = readline.createInterface({ input: process.stdin });
  rl.on("line", (line) => {
    const body = line.trim();
    if (!body) return;
    try {
      sendMessage(cfg, body);
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
          `\x07\x1b[35m  ${cfg.displayName} ›\x1b[0m ${txt}\n`,
        );
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
    case "send": {
      const cfg = loadConfig();
      if (!cfg) {
        console.error(`No contact configured. Edit ${CONFIG_PATH}`);
        process.exit(1);
      }
      const body = rest.join(" ").trim();
      if (!body) {
        console.error("usage: imsg send <text>");
        process.exit(1);
      }
      sendMessage(cfg, body);
      break;
    }
    case "tail":
      await cmdTail();
      break;
    case "open": {
      const cfg = loadConfig();
      const to = cfg ? cfg.handles[0] : "";
      spawnSync("/usr/bin/open", [`imessage://${to}`], { stdio: "ignore" });
      break;
    }
    case "config":
      ensureConfigStub();
      process.stdout.write(CONFIG_PATH + "\n");
      break;
    default:
      console.error(
        "usage: imsg status|send <text>|tail|open|config",
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
