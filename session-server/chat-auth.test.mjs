// node --test session-server/chat-auth.test.mjs
// Chat authorization: signed device tokens, locked accounts, and erasing a
// deleted account from memory (system/backend/account-deletion.mjs).
import test from "node:test";
import assert from "node:assert/strict";
import { createHmac } from "node:crypto";
import { ChatManager } from "./chat-manager.mjs";

const sign = (secret, sub, ts) =>
  createHmac("sha256", secret).update(`${sub}:${ts}`).digest("hex").slice(0, 32) + "." + ts;

function manager({ pending = null } = {}) {
  const m = new ChatManager({});
  const handles = [{ _id: "auth0|me", handle: "me" }];
  m.db = {
    collection: (name) => ({
      findOne: async (q) =>
        name === "@handles" ? handles.find((h) => h.handle === q.handle) ?? null
          : name === "account-deletions" ? pending : null,
    }),
  };
  return m;
}

test("device tokens must carry a valid signature for the handle's account", async () => {
  const m = manager();
  const saved = process.env.JWT_SECRET;
  const savedDevice = process.env.AC_DEVICE_SECRET;
  try {
    delete process.env.JWT_SECRET;
    delete process.env.AC_DEVICE_SECRET;
    assert.equal(await m.authorizeDeviceToken({}, "me", sign("ac-device", "auth0|me", "abc")), undefined);
    process.env.JWT_SECRET = "s3cret";
    assert.equal(await m.authorizeDeviceToken({}, "me", "anything.here"), undefined);
    assert.equal(await m.authorizeDeviceToken({}, "me", sign("wrong", "auth0|me", "abc")), undefined);
    assert.equal(await m.authorizeDeviceToken({}, "them", sign("s3cret", "auth0|me", "abc")), undefined);
    assert.deepEqual(await m.authorizeDeviceToken({}, "@me", sign("s3cret", "auth0|me", "abc")), { sub: "auth0|me" });
  } finally {
    if (saved === undefined) delete process.env.JWT_SECRET;
    else process.env.JWT_SECRET = saved;
    if (savedDevice === undefined) delete process.env.AC_DEVICE_SECRET;
    else process.env.AC_DEVICE_SECRET = savedDevice;
  }
});

test("an account waiting to be deleted is locked out of chat", async () => {
  assert.equal(await manager().accountLocked("auth0|me"), false);
  assert.equal(await manager({ pending: { _id: "auth0|me" } }).accountLocked("auth0|me"), true);
});

test("erasing an account drops its messages from memory without posting a log line", async () => {
  const m = manager();
  m.loggerKey = "k";
  const sent = [];
  const instance = {
    config: { name: "chat-system" },
    messages: [{ sub: "a", text: "one" }, { sub: "b", text: "two" }, { users: ["a"], text: "hi @a" }],
    subsToHandles: { a: "a", b: "b" },
    connections: { 1: { readyState: 1, send: (x) => sent.push(x) } },
  };
  assert.equal((await m.handleLog(instance, { action: "account:erase", users: ["a"] }, "Bearer wrong")).status, 403);
  assert.equal((await m.handleLog(instance, { action: "account:erase", users: ["a"] }, "Bearer k")).status, 200);
  assert.deepEqual(instance.messages, [{ sub: "b", text: "two" }]);
  assert.deepEqual(instance.subsToHandles, { b: "b" });
  assert.equal(sent.length, 1);
  assert.match(sent[0], /chat-system:mute/);
});
