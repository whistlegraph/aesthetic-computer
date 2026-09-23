import test from "node:test";
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { once } from "node:events";
import { mkdir, mkdtemp, readFile, rm, stat, writeFile } from "node:fs/promises";
import { createServer } from "node:net";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import {
  LOG_CAP, TEXT_MAX, appendMessage, deliverLocal, drain, inboxDir, makeMessage, peek, socketPath, stamp,
} from "../bin/prox-inbox.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const cli = join(here, "..", "bin", "prox-inbox.mjs");
const SID = "aaaaaaaa-1111-2222-3333-444444444444";

// Unix socket paths cap at 104 bytes on macOS, and the default tmpdir
// (/var/folders/…) already spends ~50 of them, so socket tests root under a
// short /tmp path; everything else can use the ordinary tmpdir.
async function home(t, { short = false } = {}) {
  const root = await mkdtemp(join(short ? "/tmp" : tmpdir(), "prox-inbox-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  return { SLAB_HOME: root };
}

const listenOn = (server, path) => new Promise((done, fail) => {
  server.once("error", fail);
  server.listen(path, () => { server.off("error", fail); done(); });
});
const closeServer = (server) => new Promise((r) => server.close(r));

const note = (text, extra = {}) => makeMessage({ from: "neo:sip", to: "neo:fotos", toId: SID, text, ...extra });

test("makeMessage fills the envelope and refuses what a model should not see", () => {
  const m = note("look at the diff");
  assert.equal(m.v, 1);
  assert.equal(m.kind, "message");
  assert.equal(m.urgency, "queue");
  assert.equal(m.to_id, SID);
  assert.match(m.id, /^[0-9a-f-]{36}$/);
  assert.ok(Date.now() - m.ts < 5_000);
  assert.equal(note("now", { urgency: "urgent" }).urgency, "urgent");

  assert.throws(() => note("x".repeat(TEXT_MAX + 1)), /exceeds 8000/);
  assert.equal(note("x".repeat(TEXT_MAX)).text.length, TEXT_MAX);
  assert.throws(() => note("   "), /`text` is required/);
  assert.throws(() => note("hi", { urgency: "asap" }), /`urgency`/);
  assert.throws(() => makeMessage({ from: "neo:sip", toId: SID }), /`text`/);
  assert.throws(() => makeMessage({ toId: SID, text: "hi" }), /`from`/);
  assert.throws(() => makeMessage({ from: "neo:sip", toId: "../escape", text: "hi" }), /session id/);
});

test("append, peek, drain, and a capped log", async (t) => {
  const env = await home(t);
  assert.deepEqual(await peek(SID, env), []);
  assert.deepEqual(await drain(SID, env), []);

  const a = await appendMessage(note("one"), env);
  await appendMessage(note("two"), env);
  assert.equal(a.via, "file");
  assert.equal((await stat(inboxDir(SID, env))).mode & 0o777, 0o700);
  assert.equal((await stat(a.path)).mode & 0o777, 0o600);

  const pending = await peek(SID, env);
  assert.deepEqual(pending.map((m) => m.text), ["one", "two"]);
  assert.equal((await peek(SID, env)).length, 2, "peek does not consume");

  const drained = await drain(SID, env);
  assert.deepEqual(drained.map((m) => m.text), ["one", "two"]);
  assert.deepEqual(await peek(SID, env), []);
  assert.deepEqual(await drain(SID, env), []);
  const log = (await readFile(join(inboxDir(SID, env), "log.jsonl"), "utf8")).trim().split("\n");
  assert.equal(log.length, 2);
  assert.equal(JSON.parse(log[1]).text, "two");

  for (let round = 0; round < 3; round++) {
    for (let i = 0; i < 200; i++) await appendMessage(note(`r${round}-${i}`), env);
    await drain(SID, env);
  }
  const capped = (await readFile(join(inboxDir(SID, env), "log.jsonl"), "utf8")).trim().split("\n");
  assert.equal(capped.length, LOG_CAP);
  assert.equal(JSON.parse(capped.at(-1)).text, "r2-199");
  assert.equal(JSON.parse(capped[0]).text, "r0-100"); // 602 written, oldest 102 gone
});

test("a live inbox socket gets the line first and acks it", async (t) => {
  const env = await home(t, { short: true });
  await mkdir(inboxDir(SID, env), { recursive: true, mode: 0o700 });
  const seen = [];
  const server = createServer((socket) => {
    socket.setEncoding("utf8");
    let buffer = "";
    socket.on("data", (chunk) => {
      buffer += chunk;
      if (!buffer.includes("\n")) return;
      seen.push(JSON.parse(buffer.slice(0, buffer.indexOf("\n"))));
      socket.end('{"ok":true}\n');
    });
  });
  await listenOn(server, socketPath(SID, env));
  t.after(() => closeServer(server));

  const m = note("via the wire", { urgency: "urgent" });
  const result = await deliverLocal(m, env);
  assert.equal(result.via, "socket");
  assert.equal(result.id, m.id);
  assert.equal(seen.length, 1);
  assert.equal(seen[0].text, "via the wire");
  assert.equal(seen[0].urgency, "urgent");
  assert.deepEqual(await peek(SID, env), [], "nothing queued when the socket took it");
});

test("a dead socket falls back to the file", async (t) => {
  const env = await home(t, { short: true });
  await mkdir(inboxDir(SID, env), { recursive: true, mode: 0o700 });
  const sock = socketPath(SID, env);
  // A harness that died leaves its socket file behind with nobody answering.
  const server = createServer(() => {});
  await listenOn(server, sock);
  await closeServer(server);
  if (!(await stat(sock).catch(() => null))) await writeFile(sock, "");

  const result = await deliverLocal(note("still arrives"), env);
  assert.equal(result.via, "file");
  assert.deepEqual((await peek(SID, env)).map((m) => m.text), ["still arrives"]);

  // A listener that refuses the line is also a fallback, not a loss.
  await rm(sock, { force: true });
  const refusing = createServer((socket) => {
    socket.end('{"ok":false,"error":"busy"}\n');
    // A socket nobody reads never notices the peer hang up, and the server
    // would wait on it forever; destroying after the ack is flushed lets it go.
    socket.on("finish", () => socket.destroy());
  });
  await listenOn(refusing, sock);
  t.after(() => closeServer(refusing));
  assert.equal((await deliverLocal(note("refused"), env)).via, "file");
  assert.equal((await peek(SID, env)).length, 2);
});

test("stamp renders the sender, the local send time, and the text", () => {
  const ts = Date.UTC(2026, 8, 23, 17, 58, 0);
  const d = new Date(ts);
  const p = (n) => String(n).padStart(2, "0");
  const when = `${d.getFullYear()}-${p(d.getMonth() + 1)}-${p(d.getDate())} ${p(d.getHours())}:${p(d.getMinutes())}`;
  assert.equal(stamp({ ...note("look at the diff"), ts }), `[inbox from neo:sip · ${when}] look at the diff`);
  assert.equal(stamp({ ...note("stop"), ts, urgency: "urgent" }), `[inbox from neo:sip · ${when} · urgent] stop`);
  assert.match(stamp({ from: "x:y", text: "no ts" }, ts), new RegExp(`^\\[inbox from x:y · ${when}\\] no ts$`));
});

async function run(env, args) {
  const child = spawn(process.execPath, [cli, ...args], { env: { ...process.env, ...env }, stdio: ["ignore", "pipe", "pipe"] });
  let stdout = "";
  let stderr = "";
  child.stdout.on("data", (c) => { stdout += c; });
  child.stderr.on("data", (c) => { stderr += c; });
  const [code] = await once(child, "close");
  return { code, stdout: stdout.trim(), stderr: stderr.trim() };
}

test("the cli delivers, peeks, and drains by session id", async (t) => {
  const env = await home(t);
  const delivered = await run(env, ["deliver", SID, "--from", "neo:sip", "--text", "from a hook"]);
  assert.equal(delivered.code, 0, delivered.stderr);
  assert.equal(JSON.parse(delivered.stdout).via, "file");

  const peeked = await run(env, ["peek", SID]);
  assert.equal(JSON.parse(peeked.stdout)[0].text, "from a hook");
  const drained = await run(env, ["drain", SID, "--stamped"]);
  assert.match(drained.stdout, /^\[inbox from neo:sip · \d{4}-\d{2}-\d{2} \d{2}:\d{2}\] from a hook$/);
  assert.equal((await run(env, ["peek", SID])).stdout, "[]");

  const tooLong = await run(env, ["deliver", SID, "--from", "neo:sip", "--text", "x".repeat(TEXT_MAX + 1)]);
  assert.equal(tooLong.code, 1);
  assert.match(tooLong.stderr, /exceeds 8000/);
});
