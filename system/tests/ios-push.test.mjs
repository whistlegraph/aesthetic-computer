import test from "node:test";
import assert from "node:assert/strict";
import { installIOSPushBridge } from "../public/aesthetic.computer/lib/ios-push.mjs";

const token = "a".repeat(64);
const newerToken = "b".repeat(64);
function setup({ storage = new Map(), fetch } = {}) {
  const requests = [];
  const listeners = {};
  const win = {
    localStorage: { getItem: (key) => storage.get(key) ?? null, setItem: (key, value) => storage.set(key, value) },
    crypto: { randomUUID: () => "a-device-uuid" },
    navigator: { userAgent: "Aesthetic" },
    addEventListener: (event, handler) => { listeners[event] = handler; },
    async fetch(url, options) {
      const body = JSON.parse(options.body);
      requests.push({ body, headers: options.headers });
      return fetch ? fetch(body) : { ok: true };
    },
  };
  installIOSPushBridge(win);
  return { win, requests, storage, listeners };
}

test("boot and login do not subscribe; native token receipt confirms only after HTTP success", async () => {
  const { win, requests } = setup();
  await win.iOSTryRegisterPushToken();
  assert.equal(requests.length, 0);
  assert.deepEqual(await win.iOSReceivePushToken(token), { ok: true, enabled: true });
  await win.iOSTryRegisterPushToken();
  assert.equal(requests.length, 1);
  win.acUSER = { sub: "test-user" };
  win.auth0Client = { getTokenSilently: async () => "test-bearer" };
  await win.iOSTryRegisterPushToken();
  assert.equal(requests[1].headers.Authorization, "Bearer test-bearer");
});

test("nonotifs wins over a registration already in flight and later login", async () => {
  let release;
  let started;
  const began = new Promise((resolve) => { started = resolve; });
  const { win, requests } = setup({ fetch: async (body) => {
    if (!body.remove) { started(); await new Promise((resolve) => { release = resolve; }); }
    return { ok: true };
  } });
  const subscribe = win.iOSReceivePushToken(token);
  await began;
  const unsubscribe = win.iOSUnregisterPushToken();
  release();
  assert.equal((await subscribe).ok, false);
  assert.deepEqual(await unsubscribe, { ok: true, enabled: false });
  win.acUSER = { sub: "test-user" };
  await win.iOSTryRegisterPushToken();
  assert.deepEqual(requests.map((request) => !!request.body.remove), [false, true]);
});

test("HTTP failure never reports success; failed deletion retries across a reload", async () => {
  let online = true;
  const first = setup({ fetch: async () => ({ ok: online, status: 503 }) });
  await first.win.iOSReceivePushToken(token);
  online = false;
  assert.equal((await first.win.iOSUnregisterPushToken()).ok, false);
  const second = setup({ storage: first.storage });
  await second.win.iOSTryRegisterPushToken();
  assert.equal(second.requests.length, 1);
  assert.deepEqual(second.requests[0].body, { kind: "apns", token, remove: true });
  assert.equal(second.storage.get("ac-ios-push-removals"), "[]");
});

test("registration failure can retry on reconnect without a false acknowledgement", async () => {
  let online = false;
  const { win, requests, listeners } = setup({ fetch: async () => ({ ok: online, status: 503 }) });
  assert.equal((await win.iOSReceivePushToken(token)).ok, false);
  online = true;
  assert.deepEqual(await listeners.online(), { ok: true, enabled: true });
  assert.equal(requests.length, 2);
});

test("opt-out survives reload and opt-in can restore the same token", async () => {
  const first = setup();
  await first.win.iOSReceivePushToken(token);
  await first.win.iOSUnregisterPushToken();
  const second = setup({ storage: first.storage });
  await second.win.iOSTryRegisterPushToken();
  assert.equal(second.requests.length, 0);
  assert.equal((await second.win.iOSReceivePushToken(token)).enabled, true);
  assert.equal(second.requests.length, 1);
});

test("another app window's opt-out prevents login from re-registering", async () => {
  const first = setup();
  const second = setup({ storage: first.storage });
  await first.win.iOSReceivePushToken(token);
  await second.win.iOSReceivePushToken(token);
  await second.win.iOSUnregisterPushToken();
  first.win.acUSER = { sub: "test-user" };
  first.win.auth0Client = { getTokenSilently: async () => "test-bearer" };
  assert.deepEqual(await first.win.iOSTryRegisterPushToken(), { ok: true, enabled: false });
  assert.equal(first.requests.length, 1);
});

test("rotating tokens removes the previous subscription before registering the new one", async () => {
  const { win, requests } = setup();
  await win.iOSReceivePushToken(token);
  await win.iOSReceivePushToken(newerToken);
  assert.deepEqual(requests.map(({ body }) => [body.token, !!body.remove]), [
    [token, false], [token, true], [newerToken, false],
  ]);
});

test("disable during sign-in prevents registration; invalid tokens cannot reach the server", async () => {
  let resolveAuth;
  let started;
  const began = new Promise((resolve) => { started = resolve; });
  const { win, requests } = setup();
  win.acUSER = { sub: "test-user" };
  win.auth0Client = { getTokenSilently: () => { started(); return new Promise((resolve) => { resolveAuth = resolve; }); } };
  const subscribe = win.iOSReceivePushToken(token);
  await began;
  const unsubscribe = win.iOSUnregisterPushToken();
  resolveAuth("test-bearer");
  await subscribe;
  await unsubscribe;
  await win.iOSReceivePushToken("not-a-token");
  assert.ok(requests.every(({ body }) => body.remove));
});
