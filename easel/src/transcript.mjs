// transcript.mjs — one history per session, whatever engine wrote it.
//
// Claude, Codex and the AC engine each keep their own logs in their own
// shapes, in their own places, some of them not at all. A harness that can
// switch engines mid-session (`/backend`) needs a history that survives the
// switch and reads the same afterwards, so this writes one: a directory per
// session with `events.jsonl` (what happened, in order) and `meta.json` (what
// the session is). Both 0600 in a 0700 directory — it is the user's work.
//
// Events are batched. A streaming engine can emit hundreds of deltas a second
// and one appendFileSync per token is a syscall per token; instead every event
// in a tick lands in one write on the next setImmediate. `flush()` forces it,
// `close()` flushes, and nothing is ever rewritten — only appended — so a
// crash mid-turn leaves a readable prefix rather than a torn file.
import {
  appendFileSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

export const SUMMARY_MAX = 500;
export const KINDS = [
  "user", "inbox", "assistant", "tool_call", "tool_result",
  "approval", "notice", "turn", "engine",
];

export const defaultRoot = () =>
  process.env.AESEL_TRANSCRIPTS || join(homedir(), ".local", "share", "aesel", "transcripts");

const now = () => new Date().toISOString();

// Tool inputs and results are the bulk of a transcript and rarely what anyone
// re-reads; keep the first 500 characters so the shape is visible.
function clip(value, max = SUMMARY_MAX) {
  if (value === undefined || value === null) return "";
  const text = typeof value === "string" ? value : JSON.stringify(value);
  return text.length > max ? `${text.slice(0, max - 1)}…` : text;
}

function readJson(path) {
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch {
    return null;
  }
}

export class Transcript {
  constructor({ sessionId, root = defaultRoot(), private: isPrivate = false }) {
    this.sessionId = sessionId;
    this.private = Boolean(isPrivate);
    this.dir = join(root, sessionId);
    this.eventsPath = join(this.dir, "events.jsonl");
    this.metaPath = join(this.dir, "meta.json");
    this.pending = [];
    this.scheduled = null;
    mkdirSync(this.dir, { recursive: true, mode: 0o700 });
    this.record = readJson(this.metaPath) || {
      session_id: sessionId,
      started: now(),
      pro: false,
    };
    this.meta({});
  }

  meta(patch = {}) {
    Object.assign(this.record, patch, {
      session_id: this.sessionId,
      updated: now(),
      private: this.private,
    });
    // A private session's subject never reaches disk as prose, so nothing that
    // lists transcripts can read the work back off the client's directory.
    if (this.private) this.record.subject = "private";
    const temporary = `${this.metaPath}.${process.pid}.tmp`;
    try {
      writeFileSync(temporary, `${JSON.stringify(this.record, null, 2)}\n`, { mode: 0o600 });
      renameSync(temporary, this.metaPath);
    } catch {
      rmSync(temporary, { force: true });
    }
    return { ...this.record };
  }

  event(kind, data = {}) {
    const entry = { ts: Date.now(), kind, ...data };
    if (kind === "tool_call") entry.input = clip(data.input);
    if (kind === "tool_result") entry.summary = clip(data.summary);
    this.pending.push(JSON.stringify(entry));
    if (!this.scheduled) this.scheduled = setImmediate(() => this.flush());
    return entry;
  }

  flush() {
    if (this.scheduled) clearImmediate(this.scheduled);
    this.scheduled = null;
    if (!this.pending.length) return 0;
    const lines = this.pending;
    this.pending = [];
    appendFileSync(this.eventsPath, `${lines.join("\n")}\n`, { mode: 0o600 });
    return lines.length;
  }

  close() {
    this.flush();
  }

  static list(root = defaultRoot()) {
    let names = [];
    try {
      names = readdirSync(root);
    } catch {
      return [];
    }
    return names
      .map((name) => readJson(join(root, name, "meta.json")))
      .filter((meta) => meta && meta.session_id)
      .sort((a, b) => String(b.updated || "").localeCompare(String(a.updated || "")));
  }

  static read(sessionId, root = defaultRoot()) {
    let text;
    try {
      text = readFileSync(join(root, sessionId, "events.jsonl"), "utf8");
    } catch {
      return [];
    }
    const events = [];
    for (const line of text.split("\n")) {
      if (!line.trim()) continue;
      try {
        events.push(JSON.parse(line));
      } catch {
        // A torn last line after a crash is expected; everything before it is good.
      }
    }
    return events;
  }
}
