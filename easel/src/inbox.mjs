// inbox.mjs — receive messages from other machines and sessions, mid-turn.
//
// A session in the terminal has one keyboard. The fleet has more: a Slab on
// another machine wants to tell this harness "the build finished" or "stop,
// the client changed the brief" without anyone typing it here. The sender
// (slab's prox-inbox) drops a JSON line into $SLAB_HOME/inbox/<session>/ —
// straight down the unix socket when the harness is up, or appended to
// messages.jsonl when it is not. This side listens on the socket, drains the
// file, acks each line, and hands the harness one normalized message at a
// time, already stamped the way the model should read it.
//
// No import from slab: Easel ships standalone as a tarball, so the contract is
// the directory layout and the JSON shape, not shared code. Anything that
// goes wrong inside a socket handler is emitted as "error" rather than thrown,
// because a malformed line from a peer must never take the session down.
import { EventEmitter } from "node:events";
import {
  appendFileSync,
  chmodSync,
  existsSync,
  mkdirSync,
  readFileSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { createServer } from "node:net";
import { randomUUID } from "node:crypto";
import { homedir } from "node:os";
import { join } from "node:path";

export const MAX_TEXT = 8000;
export const LOG_CAP = 500;
// A line on the wire can be at most the text plus its envelope; anything past
// this is not a message, it is a peer misbehaving.
const MAX_LINE = MAX_TEXT * 2 + 4096;

const pad = (n) => String(n).padStart(2, "0");

// Turn one raw line into a message, or say why it is not one. Fields the
// sender left out get defaults; fields that would mislead the model (no text,
// a text too long, a line addressed to a different session) are rejected.
export function normalize(line, sessionId) {
  let raw;
  try {
    raw = JSON.parse(line);
  } catch {
    return { error: "bad json" };
  }
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) return { error: "not an object" };
  if (typeof raw.text !== "string" || !raw.text.trim()) return { error: "missing text" };
  if (raw.text.length > MAX_TEXT) return { error: `text longer than ${MAX_TEXT}` };
  if (raw.to_id !== undefined && raw.to_id !== null && raw.to_id !== "" && raw.to_id !== sessionId) {
    return { error: "wrong to_id" };
  }
  const message = {
    v: 1,
    id: typeof raw.id === "string" && raw.id ? raw.id : randomUUID(),
    ts: Number.isFinite(raw.ts) ? raw.ts : Date.now(),
    from: typeof raw.from === "string" ? raw.from : "",
    to: typeof raw.to === "string" ? raw.to : "",
    to_id: sessionId,
    text: raw.text,
    urgency: raw.urgency === "urgent" ? "urgent" : "queue",
    kind: "message",
  };
  return { message };
}

export class Inbox extends EventEmitter {
  constructor({
    sessionId,
    slabHome = process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab"),
  }) {
    super();
    this.sessionId = sessionId;
    this.dir = join(slabHome, "inbox", sessionId);
    this.socketPath = join(this.dir, "inbox.sock");
    this.messagesPath = join(this.dir, "messages.jsonl");
    this.logPath = join(this.dir, "log.jsonl");
    this.server = null;
    this.connections = new Set();
    this.logged = 0;
    // Ids already in the log. A sender that misses the ack falls back to the
    // file with the same line, so the same message can arrive twice; the
    // second copy is acknowledged and not heard again.
    this.seen = new Set();
  }

  static stamp(message, now = new Date()) {
    const date = `${now.getFullYear()}-${pad(now.getMonth() + 1)}-${pad(now.getDate())}`;
    const time = `${pad(now.getHours())}:${pad(now.getMinutes())}`;
    return `[inbox from ${message.from || "unknown"} · ${date} ${time}] ${message.text}`;
  }

  async open() {
    mkdirSync(this.dir, { recursive: true, mode: 0o700 });
    // A socket left by a session that died is a file nobody answers; listening
    // on it would fail with EADDRINUSE, so it goes before we bind.
    rmSync(this.socketPath, { force: true });
    this.logged = this.#countLog();
    this.seen = this.#loggedIds();
    this.server = createServer((socket) => this.#serve(socket));
    await new Promise((resolve, reject) => {
      this.server.once("error", reject);
      this.server.listen(this.socketPath, () => {
        this.server.off("error", reject);
        resolve();
      });
    });
    this.server.on("error", (error) => this.#fail(error));
    try { chmodSync(this.socketPath, 0o600); } catch {}
    this.drainFile();
    return this.socketPath;
  }

  // Take whatever piled up in messages.jsonl while nobody was listening. The
  // rename is the claim: a sender appending after it lands in a fresh file,
  // so nothing is read twice and nothing is lost between read and unlink.
  drainFile() {
    if (!existsSync(this.messagesPath)) return 0;
    const claimed = `${this.messagesPath}.${process.pid}.draining`;
    try {
      renameSync(this.messagesPath, claimed);
    } catch (error) {
      if (error.code !== "ENOENT") this.#fail(error);
      return 0;
    }
    let delivered = 0;
    try {
      const lines = readFileSync(claimed, "utf8").split("\n").filter((l) => l.trim());
      for (const line of lines) {
        const { message, error } = normalize(line, this.sessionId);
        if (error) {
          this.#fail(new Error(`inbox: dropped queued line (${error})`));
          continue;
        }
        if (this.#deliver(message)) delivered += 1;
      }
    } catch (error) {
      this.#fail(error);
    }
    rmSync(claimed, { force: true });
    return delivered;
  }

  async close() {
    for (const socket of this.connections) socket.destroy();
    this.connections.clear();
    const server = this.server;
    this.server = null;
    if (server) {
      await new Promise((resolve) => server.close(() => resolve()));
    }
    rmSync(this.socketPath, { force: true });
  }

  #serve(socket) {
    this.connections.add(socket);
    socket.setEncoding("utf8");
    let buffer = "";
    let answered = false;
    const reply = (body) => {
      if (answered) return;
      answered = true;
      try {
        socket.end(`${JSON.stringify(body)}\n`);
      } catch {}
    };
    socket.on("data", (chunk) => {
      if (answered) return;
      buffer += chunk;
      if (buffer.length > MAX_LINE) {
        reply({ ok: false, error: "line too long" });
        return;
      }
      const newline = buffer.indexOf("\n");
      if (newline === -1) return;
      this.#receive(buffer.slice(0, newline), reply);
    });
    // A sender that half-closes without a trailing newline still sent a line.
    socket.on("end", () => {
      if (!answered && buffer.trim()) this.#receive(buffer, reply);
      else if (!answered) reply({ ok: false, error: "empty" });
    });
    socket.on("error", () => {});
    socket.on("close", () => this.connections.delete(socket));
  }

  #receive(line, reply) {
    try {
      const { message, error } = normalize(line, this.sessionId);
      if (error) {
        reply({ ok: false, error });
        return;
      }
      if (this.seen.has(message.id)) {
        reply({ ok: true, duplicate: true });
        return;
      }
      // Ack only once the log holds it: an ok means "this session has it",
      // not "the bytes arrived". But ack before the harness hears it — hearing
      // may start a turn on the spot, and the sender's ack window should not
      // pay for that work.
      if (!this.#record(message)) {
        reply({ ok: false, error: "delivery failed" });
        return;
      }
      reply({ ok: true });
      this.#hear(message);
    } catch (error) {
      reply({ ok: false, error: "internal" });
      this.#fail(error);
    }
  }

  #deliver(message) {
    if (this.seen.has(message.id)) return false;
    if (!this.#record(message)) return false;
    this.#hear(message);
    return true;
  }

  #record(message) {
    try {
      this.#log(message);
    } catch (error) {
      this.#fail(error);
      return false;
    }
    this.seen.add(message.id);
    return true;
  }

  #hear(message) {
    try {
      this.emit("message", { ...message, stamped: Inbox.stamp(message) });
    } catch (error) {
      // A listener that throws is the harness's bug, not the sender's; the
      // message is already logged, so it counts as delivered.
      this.#fail(error);
    }
  }

  #loggedIds() {
    if (!existsSync(this.logPath)) return new Set();
    const ids = new Set();
    for (const line of readFileSync(this.logPath, "utf8").split("\n")) {
      if (!line.trim()) continue;
      try { ids.add(JSON.parse(line).id); } catch {}
    }
    return ids;
  }

  #log(message) {
    appendFileSync(this.logPath, `${JSON.stringify(message)}\n`, { mode: 0o600 });
    this.logged += 1;
    if (this.logged <= LOG_CAP) return;
    const kept = readFileSync(this.logPath, "utf8").split("\n").filter(Boolean).slice(-LOG_CAP);
    const temporary = `${this.logPath}.${process.pid}.tmp`;
    writeFileSync(temporary, `${kept.join("\n")}\n`, { mode: 0o600 });
    renameSync(temporary, this.logPath);
    this.logged = kept.length;
  }

  #countLog() {
    try {
      return readFileSync(this.logPath, "utf8").split("\n").filter(Boolean).length;
    } catch {
      return 0;
    }
  }

  // "error" with nobody listening would throw out of the very handler this is
  // meant to keep quiet, so it only fires when someone asked for it.
  #fail(error) {
    if (this.listenerCount("error") > 0) this.emit("error", error);
  }
}
