#!/usr/bin/env node
// prox-inbox.mjs — hand a live agent session a message without typing into it.
//
// Every session prox knows as `host:name` gets a mailbox on its own machine:
//   $SLAB_HOME/inbox/<session_id>/
//     messages.jsonl   pending, append-only; the session drains it at a turn boundary
//     inbox.sock       present only while a live harness (Easel pro) is listening
//     log.jsonl        delivered, appended by whoever drained, last 500 lines kept
//
// Delivery on the owning machine goes socket-first (the harness acks the line
// and can interrupt its turn for an `urgent` one), then falls back to the file.
// A sender on another machine POSTs /send to the owner's :5252, and the owner
// runs this same code. No keystroke injection anywhere on the path.
//
// Dependency-free on purpose: hooks import it, the worker imports it, and a
// tiny CLI at the bottom lets a shell script deliver/peek/drain by session id.
import { appendFile, mkdir, readFile, readdir, rename, rm, stat, writeFile } from "node:fs/promises";
import { connect } from "node:net";
import { randomUUID } from "node:crypto";
import { homedir } from "node:os";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

export const TEXT_MAX = 8000;
export const LOG_CAP = 500;
export const SOCKET_CONNECT_MS = 200;
// The ack is a one-line reply from a listener that already accepted the
// connection, so it gets a little longer than the connect.
export const SOCKET_ACK_MS = 1000;
const URGENCIES = new Set(["queue", "urgent"]);
// Session ids are provider ids (uuids, rollout ids); anything else could walk
// out of the inbox root, so the shape is enforced everywhere an id becomes a path.
const ID_SHAPE = /^[A-Za-z0-9._-]{1,180}$/;

// ── paths ────────────────────────────────────────────────────────────────────
// Read env at call time so a test (or a hook with a different SLAB_HOME) can
// point the whole module elsewhere without re-importing it.
export function slabHome(env = process.env) {
  return env.SLAB_HOME || join(homedir(), ".local", "share", "slab");
}
export const inboxRoot = (env) => join(slabHome(env), "inbox");
export function inboxDir(sessionId, env) {
  return join(inboxRoot(env), checkId(sessionId));
}
export const socketPath = (sessionId, env) => join(inboxDir(sessionId, env), "inbox.sock");
const pendingPath = (sessionId, env) => join(inboxDir(sessionId, env), "messages.jsonl");
const logPath = (sessionId, env) => join(inboxDir(sessionId, env), "log.jsonl");

function checkId(sessionId) {
  const id = String(sessionId ?? "");
  if (!ID_SHAPE.test(id)) throw new Error("session id must be 1–180 chars of letters, digits, . _ -");
  return id;
}

// ── message shape ────────────────────────────────────────────────────────────
export function makeMessage({ from, to = "", toId, text, urgency = "queue" } = {}) {
  return checkMessage({
    v: 1,
    id: randomUUID(),
    ts: Date.now(),
    from,
    to,
    to_id: toId,
    text,
    urgency,
    kind: "message",
  });
}

// Accept a message from any sender (our own makeMessage, a remote /send body,
// a CLI) and return one that is safe to write down. Missing envelope fields
// get defaults; a bad address or text is refused rather than repaired.
export function checkMessage(raw) {
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) throw new Error("message must be an object");
  const from = String(raw.from ?? "").trim();
  if (!from) throw new Error("`from` is required (host:name of the sender)");
  if (typeof raw.text !== "string" || !raw.text.trim()) throw new Error("`text` is required");
  if (raw.text.length > TEXT_MAX) throw new Error(`text exceeds ${TEXT_MAX} characters`);
  const urgency = raw.urgency ?? "queue";
  if (!URGENCIES.has(urgency)) throw new Error("`urgency` must be `queue` or `urgent`");
  return {
    v: 1,
    id: typeof raw.id === "string" && raw.id ? raw.id.slice(0, 64) : randomUUID(),
    ts: Number.isFinite(raw.ts) ? raw.ts : Date.now(),
    from: from.slice(0, 120),
    to: String(raw.to ?? "").slice(0, 120),
    to_id: checkId(raw.to_id ?? raw.toId),
    text: raw.text,
    urgency,
    kind: "message",
  };
}

// ── local delivery ───────────────────────────────────────────────────────────
export async function appendMessage(message, env) {
  const m = checkMessage(message);
  const dir = inboxDir(m.to_id, env);
  await mkdir(dir, { recursive: true, mode: 0o700 });
  const file = join(dir, "messages.jsonl");
  await appendFile(file, `${JSON.stringify(m)}\n`, { mode: 0o600 });
  return { via: "file", id: m.id, path: file };
}

// Socket first, file second. Any socket trouble — nobody listening, a stale
// socket file, a slow or negative ack — lands the line in messages.jsonl, so
// a message is never lost to a harness that happened to be restarting.
export async function deliverLocal(message, env) {
  const m = checkMessage(message);
  const sock = socketPath(m.to_id, env);
  if (await exists(sock)) {
    try {
      await sendOverSocket(sock, m);
      return { via: "socket", id: m.id, path: sock };
    } catch {}
  }
  return appendMessage(m, env);
}

function sendOverSocket(path, message) {
  return new Promise((done, fail) => {
    let buffer = "";
    let settled = false;
    const finish = (fn, value) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      socket.destroy();
      fn(value);
    };
    let timer = setTimeout(() => finish(fail, new Error("connect timed out")), SOCKET_CONNECT_MS);
    const socket = connect(path);
    socket.setEncoding("utf8");
    socket.once("connect", () => {
      clearTimeout(timer);
      timer = setTimeout(() => finish(fail, new Error("ack timed out")), SOCKET_ACK_MS);
      socket.write(`${JSON.stringify(message)}\n`);
    });
    socket.on("data", (chunk) => {
      buffer += chunk;
      const nl = buffer.indexOf("\n");
      const line = nl === -1 ? null : buffer.slice(0, nl);
      if (line === null && buffer.length < 4096) return;
      let ack;
      try { ack = JSON.parse(line ?? buffer); } catch { return finish(fail, new Error("bad ack")); }
      ack?.ok === true ? finish(done) : finish(fail, new Error(ack?.error || "refused"));
    });
    socket.once("error", (e) => finish(fail, e));
    socket.once("close", () => finish(fail, new Error("closed before ack")));
  });
}

// ── consuming ────────────────────────────────────────────────────────────────
export async function peek(sessionId, env) {
  return parseLines(await readText(pendingPath(sessionId, env)));
}

// The rename is the claim: a sender appending after it lands in a fresh
// messages.jsonl, so nothing is read twice or lost between read and delete.
// Draining files left by a drainer that died mid-way are picked up first.
export async function drain(sessionId, env) {
  const dir = inboxDir(sessionId, env);
  const pending = pendingPath(sessionId, env);
  const claimed = join(dir, `messages.draining.${Date.now()}`);
  try { await rename(pending, claimed); } catch (e) { if (e.code !== "ENOENT") throw e; }
  const names = (await readdir(dir).catch(() => []))
    .filter((n) => n.startsWith("messages.draining."))
    .sort();
  const messages = [];
  for (const name of names) {
    const file = join(dir, name);
    messages.push(...parseLines(await readText(file)));
    await rm(file, { force: true });
  }
  if (messages.length) await appendLog(logPath(sessionId, env), messages);
  return messages;
}

async function appendLog(file, messages) {
  const kept = parseLinesRaw(await readText(file)).concat(messages.map((m) => JSON.stringify(m))).slice(-LOG_CAP);
  const temporary = `${file}.${process.pid}.tmp`;
  await writeFile(temporary, `${kept.join("\n")}\n`, { mode: 0o600 });
  await rename(temporary, file);
}

// ── rendering ────────────────────────────────────────────────────────────────
// What the model sees. The clock is the receiver's local zone; the moment is
// the send time, falling back to `now` for a line that arrived without one.
// `urgent` is tagged so the model knows the sender meant to interrupt.
const pad = (n) => String(n).padStart(2, "0");
export function stamp(message, now = Date.now()) {
  const d = new Date(Number.isFinite(message?.ts) ? message.ts : +now);
  const when = `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}`;
  const urgent = message?.urgency === "urgent" ? " · urgent" : "";
  return `[inbox from ${message?.from || "unknown"} · ${when}${urgent}] ${message?.text ?? ""}`;
}

// ── small helpers ────────────────────────────────────────────────────────────
const exists = (p) => stat(p).then(() => true, () => false);
const readText = (p) => readFile(p, "utf8").catch((e) => { if (e.code === "ENOENT") return ""; throw e; });
const parseLinesRaw = (text) => text.split("\n").filter((l) => l.trim());
function parseLines(text) {
  const out = [];
  for (const line of parseLinesRaw(text)) {
    try { out.push(JSON.parse(line)); } catch {} // a torn line is not a message
  }
  return out;
}

// ── cli ──────────────────────────────────────────────────────────────────────
//   prox-inbox.mjs deliver <session_id> --from host:name --text "..." [--to host:name] [--urgency urgent]
//   prox-inbox.mjs peek <session_id> [--stamped]
//   prox-inbox.mjs drain <session_id> [--stamped]
function flags(argv) {
  const out = {};
  for (let i = 0; i < argv.length; i++) {
    if (argv[i].startsWith("--")) out[argv[i].slice(2)] = argv[i + 1] === undefined || argv[i + 1].startsWith("--") ? true : argv[++i];
  }
  return out;
}

async function main(argv) {
  const [verb, sessionId, ...rest] = argv;
  const f = flags(rest);
  if (verb === "deliver") {
    const message = makeMessage({ from: f.from, to: f.to, toId: sessionId, text: f.text, urgency: f.urgency || "queue" });
    console.log(JSON.stringify(await deliverLocal(message)));
    return;
  }
  if (verb === "peek" || verb === "drain") {
    const messages = verb === "peek" ? await peek(sessionId) : await drain(sessionId);
    console.log(f.stamped ? messages.map((m) => stamp(m)).join("\n") : JSON.stringify(messages));
    return;
  }
  throw new Error("usage: prox-inbox.mjs deliver <session_id> --from host:name --text \"...\" | peek <id> | drain <id>");
}

const isMain = process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isMain) main(process.argv.slice(2)).catch((e) => { console.error(`prox-inbox: ${e.message}`); process.exit(1); });
