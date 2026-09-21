// node --experimental-vm-modules --test system/tests/kv.test.mjs
import test from "node:test";
import assert from "node:assert/strict";
import vm from "node:vm";
import { EventEmitter } from "node:events";
import { readFile } from "node:fs/promises";

async function fixture({ handshake = async () => {}, quit = async () => {} } = {}) {
  const clients = [];
  const env = {};
  const context = vm.createContext({ process: { env }, console });
  const module = new vm.SourceTextModule(
    await readFile(new URL("../backend/kv.mjs", import.meta.url), "utf8"),
    { context },
  );
  const redis = new vm.SyntheticModule(["createClient"], function () {
    this.setExport("createClient", () => {
      const client = Object.assign(new EventEmitter(), {
        isOpen: false, isReady: false, quits: 0, destroys: 0,
        async connect() {
          this.isOpen = true;
          await handshake(clients.length);
          if (this.isOpen) { this.isReady = true; this.emit("ready"); }
        },
        async HGET() {
          assert.ok(this.isOpen, "The client is closed");
          assert.ok(this.isReady, "The client is not ready");
          return "stored value";
        },
        async quit() {
          this.quits++;
          this.isOpen = false;
          await quit();
          this.isReady = false;
          this.emit("end");
        },
        destroy() {
          this.destroys++;
          this.isOpen = false;
          this.isReady = false;
          this.emit("end");
        },
      });
      clients.push(client);
      return client;
    });
  }, { context });
  await module.link((name) => {
    assert.equal(name, "redis");
    return redis;
  });
  await module.evaluate();
  return { kv: module.namespace, clients, env };
}

test("one completed request cannot close another request's Redis client", async () => {
  const { kv, clients } = await fixture();
  await kv.connect();
  await kv.connect();
  await kv.get("macpal", "synthetic");
  await kv.disconnect();
  assert.equal(await kv.get("macpalart", "synthetic"), "stored value");
  await kv.disconnect();
  assert.equal(clients.length, 1);
  assert.equal(clients[0].quits, 0);
});

test("simultaneous connects wait for one Redis handshake", async () => {
  const gate = Promise.withResolvers();
  const { kv, clients } = await fixture({ handshake: () => gate.promise });
  const first = kv.connect();
  await Promise.resolve();
  let secondReady = false;
  const second = kv.connect().then(() => { secondReady = true; });
  await new Promise(setImmediate);
  assert.equal(secondReady, false);
  assert.equal(clients.length, 1);
  gate.resolve();
  await Promise.all([first, second]);
  assert.equal(await kv.get("test", "synthetic"), "stored value");
});

test("a failed handshake permits a later connection attempt", async () => {
  const { kv, clients } = await fixture({ handshake: async (attempt) => {
    if (attempt === 1) throw new Error("synthetic connection failure");
  } });
  await assert.rejects(kv.connect(), /synthetic connection failure/);
  assert.equal(clients[0].isOpen, false);
  assert.equal(clients[0].destroys, 1);
  await kv.connect();
  assert.equal(clients.length, 2);
  assert.equal(await kv.get("test", "synthetic"), "stored value");
});

test("scripts can explicitly close Redis and reconnect", async () => {
  const { kv, clients, env } = await fixture();
  await kv.connect();
  await kv.closeConnection();
  await kv.closeConnection();
  assert.equal(clients[0].quits, 1);
  await kv.connect();
  env.AC_KV_CLOSE = "1";
  await kv.disconnect();
  assert.equal(clients.length, 2);
  assert.equal(clients[1].quits, 1);
});


test("automatic reconnect waits for readiness without creating another client", async () => {
  const { kv, clients } = await fixture();
  await kv.connect();
  const connection = clients[0];
  connection.isReady = false;
  let ready = false;
  const first = kv.connect().then(() => { ready = true; });
  const second = kv.connect();
  await new Promise(setImmediate);
  connection.emit("error", new Error("synthetic transient failure"));
  assert.equal(ready, false);
  assert.equal(clients.length, 1);
  connection.isReady = true;
  connection.emit("ready");
  await Promise.all([first, second]);
  assert.equal(await kv.get("test", "synthetic"), "stored value");
  assert.equal(connection.listenerCount("ready"), 0);
  assert.equal(connection.listenerCount("end"), 0);
  assert.equal(connection.listenerCount("error"), 1);
});

test("terminal reconnect failures reject waiters and permit a fresh client", async () => {
  const { kv, clients } = await fixture();
  await kv.connect();
  clients[0].isReady = false;
  const pending = kv.connect();
  const rejected = assert.rejects(pending, /synthetic terminal failure/);
  await new Promise(setImmediate);
  clients[0].isOpen = false;
  clients[0].emit("error", new Error("synthetic terminal failure"));
  await rejected;
  await kv.connect();
  assert.equal(clients.length, 2);
});

test("explicit shutdown cancels reconnect waiters instead of waiting indefinitely", async () => {
  const { kv, clients } = await fixture();
  await kv.connect();
  clients[0].isReady = false;
  const pending = kv.connect();
  const rejected = assert.rejects(pending, /closed before it was ready/);
  await new Promise(setImmediate);
  await kv.closeConnection();
  await rejected;
  assert.equal(clients[0].quits, 0);
  assert.equal(clients[0].destroys, 1);
  await kv.connect();
  assert.equal(clients.length, 2);
});

test("shutdown during initial handshake cannot clear a newer connection", async () => {
  const firstGate = Promise.withResolvers();
  const nextGate = Promise.withResolvers();
  const { kv, clients } = await fixture({ handshake: (attempt) =>
    attempt === 1 ? firstGate.promise : nextGate.promise });
  const first = kv.connect();
  const rejected = assert.rejects(first, /closed before it was ready/);
  await new Promise(setImmediate);
  await kv.closeConnection();
  assert.equal(clients[0].destroys, 1);
  const next = kv.connect();
  await new Promise(setImmediate);
  firstGate.resolve();
  await rejected;
  let peerReady = false;
  const peer = kv.connect().then(() => { peerReady = true; });
  await new Promise(setImmediate);
  assert.equal(peerReady, false);
  assert.equal(clients.length, 2);
  nextGate.resolve();
  await Promise.all([next, peer]);
  assert.equal(await kv.get("test", "synthetic"), "stored value");
});

test("new connects and repeated shutdown wait for one graceful close", async () => {
  const gate = Promise.withResolvers();
  const { kv, clients } = await fixture({ quit: () => gate.promise });
  await kv.connect();
  const close = kv.closeConnection();
  const duplicate = kv.closeConnection();
  let connected = false;
  const next = kv.connect().then(() => { connected = true; });
  await new Promise(setImmediate);
  assert.equal(connected, false);
  assert.equal(clients.length, 1);
  assert.equal(clients[0].quits, 1);
  gate.resolve();
  await Promise.all([close, duplicate, next]);
  assert.equal(clients.length, 2);
  assert.equal(await kv.get("test", "synthetic"), "stored value");
});
