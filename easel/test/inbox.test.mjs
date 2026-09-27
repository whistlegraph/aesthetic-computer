import assert from "node:assert/strict";
import { mkdir, mkdtemp, readFile, rm, stat, writeFile } from "node:fs/promises";
import { spawn } from "node:child_process";
import { connect } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { Inbox, LOG_CAP, MAX_TEXT } from "../src/inbox.mjs";

// Short prefix and short session ids: a unix socket path is capped at 104
// bytes on macOS, and tmpdir() already spends fifty of them.
async function home(context) {
  const root = await mkdtemp(join(tmpdir(), "ei-"));
  context.after(() => rm(root, { recursive: true, force: true }));
  return root;
}

const exists = (path) => stat(path).then(() => true, () => false);

// Speak the sender side of the contract: one line in, one line back.
function send(socketPath, line) {
  return new Promise((resolve, reject) => {
    const socket = connect(socketPath);
    let reply = "";
    socket.setEncoding("utf8");
    socket.on("connect", () => socket.write(`${line}\n`));
    socket.on("data", (chunk) => { reply += chunk; });
    socket.on("end", () => resolve(JSON.parse(reply.trim())));
    socket.on("error", reject);
  });
}

const next = (inbox, event = "message") =>
  new Promise((resolve) => inbox.once(event, resolve));

const message = (extra = {}) => JSON.stringify({
  v: 1,
  id: "m1",
  ts: 1_700_000_000_000,
  from: "neo:sip",
  to: "blueberry:easel",
  to_id: "s1",
  text: "the build finished",
  urgency: "queue",
  kind: "message",
  ...extra,
});

test("a line down the socket is acked, logged and emitted with a stamp", async (context) => {
  const inbox = new Inbox({ sessionId: "s1", slabHome: await home(context) });
  const socketPath = await inbox.open();
  context.after(() => inbox.close());
  assert.equal(socketPath, join(inbox.dir, "inbox.sock"));
  assert.equal((await stat(inbox.dir)).mode & 0o777, 0o700);

  const arrived = next(inbox);
  const ack = await send(socketPath, message());
  assert.deepEqual(ack, { ok: true });
  const got = await arrived;
  assert.equal(got.text, "the build finished");
  assert.equal(got.from, "neo:sip");
  assert.equal(got.urgency, "queue");
  assert.match(got.stamped, /^\[inbox from neo:sip · \d{4}-\d{2}-\d{2} \d{2}:\d{2}\] the build finished$/);

  const log = (await readFile(inbox.logPath, "utf8")).trim().split("\n").map(JSON.parse);
  assert.equal(log.length, 1);
  assert.equal(log[0].id, "m1");
  assert.equal((await stat(inbox.logPath)).mode & 0o777, 0o600);
});

test("malformed lines are refused with ok:false and never emitted", async (context) => {
  const inbox = new Inbox({ sessionId: "s1", slabHome: await home(context) });
  const socketPath = await inbox.open();
  context.after(() => inbox.close());
  let emitted = 0;
  inbox.on("message", () => { emitted += 1; });

  assert.deepEqual(await send(socketPath, "{not json"), { ok: false, error: "bad json" });
  assert.deepEqual(await send(socketPath, message({ text: "" })), { ok: false, error: "missing text" });
  assert.deepEqual(await send(socketPath, JSON.stringify({ from: "neo:sip" })), { ok: false, error: "missing text" });
  assert.deepEqual(
    await send(socketPath, message({ text: "x".repeat(MAX_TEXT + 1) })),
    { ok: false, error: `text longer than ${MAX_TEXT}` },
  );
  assert.deepEqual(await send(socketPath, message({ to_id: "someone-else" })), { ok: false, error: "wrong to_id" });
  assert.equal(emitted, 0);
  assert.equal(await exists(inbox.logPath), false);

  // A line with no to_id is for whoever answers at this path.
  const ack = await send(socketPath, JSON.stringify({ text: "hi", from: "neo:sip" }));
  assert.deepEqual(ack, { ok: true });
  assert.equal(emitted, 1);
});

test("messages queued in the file while the socket was down are delivered on open", async (context) => {
  const root = await home(context);
  const inbox = new Inbox({ sessionId: "s1", slabHome: root });
  // A sender that gets there before the receiver makes the directory too.
  await mkdir(join(root, "inbox", "s1"), { recursive: true, mode: 0o700 });
  await writeFile(
    join(root, "inbox", "s1", "messages.jsonl"),
    [message({ id: "q1", text: "first" }), "garbage", message({ id: "q2", text: "second", urgency: "urgent" }), ""].join("\n"),
  );

  const got = [];
  const errors = [];
  inbox.on("message", (m) => got.push(m));
  inbox.on("error", (e) => errors.push(e));
  await inbox.open();
  context.after(() => inbox.close());

  assert.deepEqual(got.map((m) => m.text), ["first", "second"]);
  assert.equal(got[1].urgency, "urgent");
  assert.equal(errors.length, 1, "the garbage line is reported, not thrown");
  assert.equal(await exists(inbox.messagesPath), false, "the queue is consumed");
  const log = (await readFile(inbox.logPath, "utf8")).trim().split("\n");
  assert.equal(log.length, 2);

  // Nothing queued means nothing delivered, and calling again is harmless.
  assert.equal(inbox.drainFile(), 0);
  await writeFile(inbox.messagesPath, `${message({ id: "q3", text: "third" })}\n`);
  assert.equal(inbox.drainFile(), 1);
  assert.equal(got.at(-1).text, "third");
});

test("a stale socket from a dead session is replaced", async (context) => {
  const root = await home(context);
  const socketPath = join(root, "inbox", "s1", "inbox.sock");
  await mkdir(join(root, "inbox", "s1"), { recursive: true, mode: 0o700 });
  // A session killed outright leaves its socket bound to nobody. server.close()
  // would unlink the path politely, so the only honest way to make one is to
  // let another process bind it and then kill -9 that process.
  const dead = spawn(process.execPath, [
    "-e",
    "require('node:net').createServer().listen(process.argv[1], () => process.stdout.write('ready'))",
    socketPath,
  ], { stdio: ["ignore", "pipe", "ignore"] });
  await new Promise((resolve) => dead.stdout.once("data", resolve));
  const gone = new Promise((resolve) => dead.once("exit", resolve));
  dead.kill("SIGKILL");
  await gone;
  assert.equal(await exists(socketPath), true);

  const fresh = new Inbox({ sessionId: "s1", slabHome: root });
  assert.equal(await fresh.open(), socketPath);
  context.after(() => fresh.close());
  const arrived = next(fresh);
  assert.deepEqual(await send(socketPath, message()), { ok: true });
  assert.equal((await arrived).id, "m1");
});

test("close stops listening and unlinks the socket", async (context) => {
  const inbox = new Inbox({ sessionId: "s1", slabHome: await home(context) });
  const socketPath = await inbox.open();
  await inbox.close();
  assert.equal(await exists(socketPath), false);
  await assert.rejects(send(socketPath, message()));
  // Closing twice is fine.
  await inbox.close();
});

test("a message id this session already holds is acked but not heard twice", async (context) => {
  const inbox = new Inbox({ sessionId: "s1", slabHome: await home(context) });
  const socketPath = await inbox.open();
  context.after(() => inbox.close());
  const heard = [];
  inbox.on("message", (m) => heard.push(m.id));

  // A sender that missed the ack retries on the socket, then falls back to
  // the file with the same line. Both repeats are fine; neither is a turn.
  assert.deepEqual(await send(socketPath, message()), { ok: true });
  assert.deepEqual(await send(socketPath, message()), { ok: true, duplicate: true });
  await writeFile(inbox.messagesPath, `${message()}\n${message({ id: "m2", text: "second" })}\n`);
  assert.equal(inbox.drainFile(), 1);
  assert.deepEqual(heard, ["m1", "m2"]);

  // The log is the memory: a fresh Inbox on the same session still knows m1.
  await inbox.close();
  const again = new Inbox({ sessionId: "s1", slabHome: inbox.dir.replace(/\/inbox\/s1$/, "") });
  const path = await again.open();
  context.after(() => again.close());
  assert.deepEqual(await send(path, message()), { ok: true, duplicate: true });
  assert.equal((await readFile(again.logPath, "utf8")).trim().split("\n").length, 2);
});

test("the delivered log keeps only the last 500 lines", async (context) => {
  const inbox = new Inbox({ sessionId: "s1", slabHome: await home(context) });
  await inbox.open();
  context.after(() => inbox.close());
  const lines = [];
  for (let i = 0; i < LOG_CAP + 25; i += 1) lines.push(message({ id: `m${i}`, text: `n${i}` }));
  await writeFile(inbox.messagesPath, `${lines.join("\n")}\n`);
  assert.equal(inbox.drainFile(), LOG_CAP + 25);
  const log = (await readFile(inbox.logPath, "utf8")).trim().split("\n").map(JSON.parse);
  assert.equal(log.length, LOG_CAP);
  assert.equal(log[0].id, "m25");
  assert.equal(log.at(-1).id, `m${LOG_CAP + 24}`);
});

test("the stamp is the receiver's local clock to the minute", () => {
  const when = new Date(2026, 8, 23, 17, 58, 42);
  assert.equal(
    Inbox.stamp({ from: "neo:sip", text: "text" }, when),
    "[inbox from neo:sip · 2026-09-23 17:58] text",
  );
  assert.equal(Inbox.stamp({ text: "x" }, when), "[inbox from unknown · 2026-09-23 17:58] x");
});
