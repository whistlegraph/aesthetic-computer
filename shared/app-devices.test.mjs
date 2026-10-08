import test from "node:test";
import assert from "node:assert/strict";
import { APPS, appConfig, userKey } from "./app-registry.mjs";
import { normalizeReport, reportUpdate, targetFilter, deviceKey } from "./app-devices.mjs";

const base = { app: "whistlegraph", deviceId: "6F9619FF-8B86-D011-B42D-00C04FC964FF", platform: "ios", event: "open", version: "0.1.0", build: 114 };
const now = new Date("2026-10-08T20:00:00Z");

test("every app names a tenant; sotce users are prefixed", () => {
  for (const [id, app] of Object.entries(APPS)) assert.ok(["aesthetic", "sotce"].includes(app.tenant), id);
  assert.equal(userKey("whistlegraph", "auth0|1"), "auth0|1");
  assert.equal(userKey("sotce-net", "auth0|1"), "sotce-auth0|1");
  assert.equal(appConfig("nope"), null);
});

test("reports are validated before they reach Mongo", () => {
  const ok = normalizeReport(base);
  assert.equal(ok.build, "114");
  for (const bad of [{ app: "nope" }, { deviceId: "x" }, { platform: "amiga" }, { event: "boom" }, { version: "1.x" },
    { build: "11a" }, { topics: ["Bad Topic"] }, { push: { kind: "apns", token: "zz" } }, { push: { kind: "webpush" } }])
    assert.throws(() => normalizeReport({ ...base, ...bad }), e => e.statusCode === 400, JSON.stringify(bad));
  // Web Push only for apps with a web surface; APNs only for apps with a bundle.
  const sub = { endpoint: "https://push.example/x", keys: { p256dh: "p", auth: "a" } };
  assert.throws(() => normalizeReport({ ...base, push: { kind: "webpush", subscription: sub } }));
  assert.equal(normalizeReport({ ...base, app: "sotce-net", platform: "web", push: { kind: "webpush", subscription: sub } }).push.kind, "webpush");
  assert.throws(() => normalizeReport({ ...base, app: "sotce-net", platform: "web", push: { kind: "apns", token: "ab".repeat(32) } }));
  assert.equal(normalizeReport({ ...base, push: { kind: "apns", token: "AB".repeat(32), env: "sandbox" } }).push.token, "ab".repeat(32));
  assert.equal(normalizeReport({ ...base, push: null }).push, null);
});

test("an open counts, binds the verified account, and records the build", () => {
  const update = reportUpdate(normalizeReport(base), { user: "auth0|artur", handle: "dreamdeal", now });
  assert.deepEqual(update.$inc, { opens: 1 });
  assert.equal(update.$set.user, "auth0|artur");
  assert.equal(update.$set.handle, "dreamdeal");
  assert.equal(update.$set.build, "114");
  assert.equal(update.$set.lastOpenAt, now);
  assert.equal(update.$setOnInsert.firstAt, now);
});

test("an unsigned report never binds or unbinds; logout unbinds; revoked push unsets", () => {
  const anon = reportUpdate(normalizeReport({ ...base, event: "seen" }), { now });
  assert.equal(anon.$set.user, undefined);
  assert.equal(anon.$unset, undefined);
  assert.equal(anon.$inc, undefined);
  const out = reportUpdate(normalizeReport({ ...base, event: "logout" }), { now });
  assert.deepEqual(out.$unset, { user: "", handle: "" });
  const revoked = reportUpdate(normalizeReport({ ...base, event: "push", push: null }), { now });
  assert.deepEqual(revoked.$unset, { push: "" });
});

test("notification targets: device, person, group", () => {
  assert.deepEqual(targetFilter({ app: "whistlegraph", deviceId: "abc12345" }), { _id: deviceKey("whistlegraph", "abc12345") });
  assert.deepEqual(targetFilter({ user: "auth0|1" }), { user: "auth0|1" });
  assert.deepEqual(targetFilter({ user: "auth0|1", app: "aesel" }), { user: "auth0|1", app: "aesel" });
  assert.deepEqual(targetFilter({ app: "whistlegraph", topic: "testers" }), { app: "whistlegraph", topics: "testers" });
  assert.throws(() => targetFilter({ deviceId: "abc12345" }));
  assert.throws(() => targetFilter({ topic: "testers" }));
  assert.throws(() => targetFilter({}));
});
