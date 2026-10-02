import assert from "node:assert/strict";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { AppServer } from "../src/app-server.mjs";

const directory = path.dirname(fileURLToPath(import.meta.url));

test("drives a thread without opening a managed client", async () => {
  const engine = new AppServer({
    cwd: directory,
    command: process.execPath,
    args: [path.join(directory, "fake-app-server.mjs")],
  });
  const deltas = [];
  const completed = new Promise((resolve) => {
    engine.on("notification", (message) => {
      if (message.method === "item/agentMessage/delta") deltas.push(message.params.delta);
      if (message.method === "turn/completed") resolve(message.params.turn.status);
    });
  });
  engine.on("request", (message) => engine.respond(message.id, { decision: "accept" }));

  const connection = await engine.connect();
  assert.equal(connection.thread.id, "thread-1");
  assert.equal(connection.model, "test-model");

  const turn = await engine.startTurn("run the tests");
  assert.equal(turn.turn.id, "turn-1");
  assert.equal(await completed, "completed");
  assert.equal(deltas.join(""), "Tests pass.");
  engine.close();
});

test("resumes a provider thread behind the same interface", async () => {
  const threadId = "00000000-0000-0000-0000-000000000001";
  const engine = new AppServer({
    cwd: directory,
    resumeThreadId: threadId,
    command: process.execPath,
    args: [path.join(directory, "fake-app-server.mjs")],
  });
  const connection = await engine.connect();
  assert.equal(connection.thread.id, threadId);
  assert.equal(engine.threadId, threadId);
  engine.close();
});

test("reads Codex account limits and merges sparse updates without mixing quota buckets", async (t) => {
  const engine = new AppServer({ cwd: directory, command: process.execPath, args: [path.join(directory, "fake-app-server.mjs")] });
  t.after(() => engine.close());
  await engine.connect();
  await engine.refreshRateLimits();
  assert.equal(engine.rateLimits.primary.usedPercent, 23);
  assert.equal(engine.rateLimits.secondary.usedPercent, 61);
  await engine.request("test/rateLimits", { limitId: "codex", primary: { usedPercent: 30, windowDurationMins: 300 }, secondary: null });
  assert.equal(engine.rateLimits.primary.usedPercent, 30);
  assert.equal(engine.rateLimits.secondary.usedPercent, 61, "null in a rolling update does not erase the weekly reading");
  await engine.request("test/rateLimits", { limitId: "codex_other", primary: { usedPercent: 99 } });
  assert.equal(engine.rateLimits.primary.usedPercent, 30, "another bucket must not replace Codex usage");
});

test("unavailable usage does not block connecting or fabricate a reading", async (t) => {
  const engine = new AppServer({ cwd: directory, command: process.execPath, args: [path.join(directory, "fake-app-server.mjs")], environment: { AESEL_TEST_RATE_LIMITS: "unavailable" } });
  t.after(() => engine.close());
  const connection = await engine.connect();
  await engine.refreshRateLimits();
  assert.equal(connection.thread.id, "thread-1");
  assert.equal(engine.rateLimits, null);
});

test("unanswered usage reads are bounded and do not hold up the thread", async (t) => {
  const engine = new AppServer({ cwd: directory, command: process.execPath, args: [path.join(directory, "fake-app-server.mjs")], environment: { AESEL_TEST_RATE_LIMITS: "silent" } });
  t.after(() => engine.close());
  assert.equal((await engine.connect()).thread.id, "thread-1");
  const pending = engine.pending.size;
  await assert.rejects(engine.request("account/rateLimits/read", {}, { timeout: 20 }), /timed out/);
  assert.equal(engine.pending.size, pending, "timed-out request is removed");
  engine.close();
  await engine.rateLimitsRequest;
  assert.equal(engine.pending.size, 0);
});

test("bridge requests have a default deadline and preserve structured provider failures", async (t) => {
  const engine = new AppServer({cwd:directory,command:process.execPath,args:[path.join(directory,"fake-app-server.mjs")]});
  t.after(()=>engine.close());
  await engine.connect();
  // Test a silent request's default deadline, independently of process launch.
  const normalTimeout=engine.requestTimeout;
  engine.requestTimeout=100;
  await assert.rejects(engine.request("test/silent",{}),{name:"TimeoutError",method:"test/silent"});
  engine.requestTimeout=normalTimeout;
  await assert.rejects(engine.request("test/error",{}),error=>error.status===503&&error.code===-32000&&!!error.codexErrorInfo.responseStreamDisconnected);
  assert.equal(engine.pending.size,0);
});

test("a bridge exit fails waiters once and subsequent writes fail immediately", async (t) => {
  const engine = new AppServer({cwd:directory,command:process.execPath,args:[path.join(directory,"fake-app-server.mjs")]});
  t.after(()=>engine.close());let fatals=0;
  engine.on('fatal',()=>fatals++);
  await engine.connect();
  await assert.rejects(engine.request('test/exit',{}),error=>error.bridgeFailure===true);
  await assert.rejects(engine.request('test/silent',{}),/not writable/);
  assert.equal(fatals,1);assert.equal(engine.pending.size,0);
});
