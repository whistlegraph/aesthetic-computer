import assert from "node:assert/strict";
import test from "node:test";

import { BrowserCrashError, Session } from "../lib/cdp.mjs";

class FakeWebSocket {
  static instances = [];

  constructor() {
    this.listeners = new Map();
    this.readyState = 0;
    this.respond = true;
    FakeWebSocket.instances.push(this);
    queueMicrotask(() => {
      this.readyState = 1;
      this.emit("open", {});
    });
  }

  addEventListener(type, listener, options = {}) {
    const wrapped = options.once
      ? (event) => {
        this.listeners.get(type)?.delete(wrapped);
        listener(event);
      }
      : listener;
    if (!this.listeners.has(type)) this.listeners.set(type, new Set());
    this.listeners.get(type).add(wrapped);
  }

  emit(type, event) {
    for (const listener of [...(this.listeners.get(type) || [])]) listener(event);
  }

  message(payload) {
    this.emit("message", { data:JSON.stringify(payload) });
  }

  send(raw) {
    const request = JSON.parse(raw);
    if (!this.respond) return;
    queueMicrotask(() => this.message({
      id:request.id,
      result:request.method === "Runtime.evaluate"
        ? { result:{ value:{ readyState:"complete", href:"https://app.fuser.studio/flow/test" } } }
        : {},
    }));
  }

  close() {
    this.readyState = 3;
    this.emit("close", {});
  }
}

test("CDP health accepts a responsive renderer and catches its crash event", async (t) => {
  const RealWebSocket = globalThis.WebSocket;
  globalThis.WebSocket = FakeWebSocket;
  t.after(() => { globalThis.WebSocket = RealWebSocket; });

  const session = new Session("ws://fake.test/page/one");
  t.after(() => session.close());
  await session.monitorCrashes();
  assert.equal((await session.assertHealthy()).readyState, "complete");

  const ws = FakeWebSocket.instances.at(-1);
  ws.message({ method:"Inspector.targetCrashed", params:{ status:"crashed" } });
  await assert.rejects(
    session.assertHealthy("beat 3"),
    (error) => error instanceof BrowserCrashError
      && error.code === "BROWSER_RENDERER_CRASH"
      && error.details.signal === "Inspector.targetCrashed",
  );
});

test("CDP health converts an unresponsive renderer into a bounded crash failure", async (t) => {
  const RealWebSocket = globalThis.WebSocket;
  globalThis.WebSocket = FakeWebSocket;
  t.after(() => { globalThis.WebSocket = RealWebSocket; });

  const session = new Session("ws://fake.test/page/two");
  t.after(() => session.close());
  await session.monitorCrashes();
  FakeWebSocket.instances.at(-1).respond = false;

  const started = Date.now();
  await assert.rejects(
    session.assertHealthy("pre-record", { timeoutMs:25 }),
    (error) => error instanceof BrowserCrashError
      && error.details.signal === "CDP.unresponsive",
  );
  assert.ok(Date.now() - started < 500, "health failure should be bounded");
});
