import test from "node:test";
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { signupReturnPath, validateSignupEvent, signupUpdate } from "../public/aesthetic.computer/lib/signup-model.mjs";
import { createSignupTrackingHandler } from "../backend/signup-tracking.mjs";
import { createSignupStatusHandler } from "../backend/signup-status.mjs";
import { readSignupAccounts, summarizeSignupCohort } from "../backend/signup-report.mjs";

const origin = "https://aesthetic.computer";
const snapshot = () => ({ version: 1, id: randomUUID(), mode: "signup", source: "prompt", stages: ["started"], automated: false });

test("return targets preserve pieces but cannot redirect externally or retain secrets", () => {
  assert.equal(signupReturnPath("/laer-klokken?token=secret#private", origin), "/laer-klokken");
  assert.equal(signupReturnPath("/@somebody/art", origin), "/@somebody/art");
  assert.equal(signupReturnPath("/$abc", origin), "/$abc");
  for (const path of ["https://evil.test", "//evil.test/path", "/\\evil.test", "javascript:alert(1)", "/prompt~secret", "/mail", "/login", "/%61dmin", "/@person/../../account", "/foo%2fbar", "/hi", undefined])
    assert.equal(signupReturnPath(path, origin), null, path);
});

test("collector stores only bounded stages, deduplicates cumulative retries and honors opt-outs", async () => {
  const writes = [], indexes = [];
  const handler = createSignupTrackingHandler({ connect: async () => ({ db: { collection: () => ({
    createIndex: async (...args) => indexes.push(args), updateOne: async (...args) => writes.push(args),
  }) } }) });
  const body = { ...snapshot(), stages: ["started", "handle_failed"], error: "taken", email: "secret@example.com", handle: "private-name", returnTo: "/private", referrerHost: "https://example.org/private?secret=1" };
  const event = { httpMethod: "POST", headers: { origin }, body: JSON.stringify(body) };
  assert.equal((await handler(event)).statusCode, 204);
  await handler(event);
  assert.deepEqual(writes[0][0], writes[1][0]);
  assert.doesNotMatch(JSON.stringify(writes), /secret|private/);
  assert.equal(writes[0][1].$setOnInsert.referrerHost, "example.org");
  assert.equal(writes[0][1].$max["stages.handle_failed"], true);
  assert.equal(writes[0][1].$max["errors.taken"], true);
  assert.ok(indexes.some(([, options]) => options?.expireAfterSeconds === 0));
  for (const headers of [{ ...event.headers, dnt: "1" }, { ...event.headers, "sec-gpc": "1" }, { ...event.headers, "user-agent": "HeadlessChrome" }])
    assert.equal((await handler({ ...event, headers })).statusCode, 204);
  assert.equal(writes.length, 2);
  for (const patch of [{ stages: ["account_created"] }, { error: "raw failure with email" }, { mode: "anything" }, { source: "private-path" }, { id: "not-a-uuid" }])
    assert.equal(validateSignupEvent({ ...body, ...patch }, origin), null);
  assert.equal(validateSignupEvent(body, "https://false.work"), null);
  assert.equal((await handler({ ...event, body: "x".repeat(2049) })).statusCode, 400);
  assert.equal((await handler({ ...event, httpMethod: "GET" })).statusCode, 405);
  const now = new Date();
  assert.equal(+signupUpdate(validateSignupEvent(body, origin), now).$setOnInsert.expiresAt - +now, 35 * 86400000);
});

test("signup status and resend use only the authenticated AC subject and limit resends", async () => {
  let user = null, verified = false;
  const lookedUp = [], resent = [], forgotten = [];
  const handler = createSignupStatusHandler({
    authorize: async (_headers, tenant) => { assert.equal(tenant, "aesthetic"); return user; },
    userEmailFromID: async sub => { lookedUp.push(sub); return { email_verified: verified, email: "secret@example.com" }; },
    handleFor: async () => null, forgetAuthorizations: sub => forgotten.push(sub),
    resendVerificationEmail: async sub => resent.push(sub),
  });
  const event = { httpMethod: "GET", headers: {}, queryStringParameters: { sub: "victim" } };
  assert.equal((await handler(event)).statusCode, 401);
  assert.equal(lookedUp.length, 0);
  user = { sub: "auth0|caller" };
  assert.deepEqual(JSON.parse((await handler(event)).body), { verified: false, handle: null });
  const post = { ...event, httpMethod: "POST", body: JSON.stringify({ action: "resend", sub: "victim" }) };
  assert.equal((await handler(post)).statusCode, 200);
  assert.equal((await handler(post)).statusCode, 429);
  assert.deepEqual(resent, ["auth0|caller"]);
  verified = true;
  assert.deepEqual(JSON.parse((await handler(event)).body), { verified: true, handle: null });
  assert.deepEqual(forgotten, ["auth0|caller"]);
  assert.ok(lookedUp.every(sub => sub === "auth0|caller"));
});

test("report separates new accounts, cohort completion and handles from older accounts", () => {
  const start = new Date("2026-10-02T00:00:00Z"), end = new Date("2026-10-03T00:00:00Z");
  const users = [{ user_id: "a", created_at: start.toISOString(), email_verified: true },
    { user_id: "b", created_at: "2026-10-02T01:00:00Z", email_verified: false },
    { user_id: "c", created_at: end.toISOString(), email_verified: true }];
  const report = summarizeSignupCohort(users, new Set(["a", "c"]), [{ when: start }, { when: start }, { when: end }], start, end);
  assert.equal(report.accountsCreated, 2);
  assert.equal(report.cohortWithHandleNow, 1);
  assert.equal(report.handlesCreated, 2);
  assert.equal(report.accountToHandlePercent, 50);
  assert.equal(summarizeSignupCohort([], new Set(), [], start, end).accountToHandlePercent, null);
});

test("Auth0 search cap splits intervals, deduplicates boundaries and keeps secrets out of errors", async () => {
  const start = new Date("2026-10-02T00:00:00Z"), end = new Date("2026-10-03T00:00:00Z");
  let queries = 0;
  const fetch = async (url, options) => {
    if (String(url).endsWith("/oauth/token")) return { ok: true, json: async () => ({ access_token: "secret" }) };
    assert.equal(options.headers.Authorization, "Bearer secret");
    assert.equal(new URL(url).searchParams.get("fields"), "user_id,created_at,email_verified");
    if (++queries === 1) return { ok: true, json: async () => ({ total: 1000, users: [] }) };
    return { ok: true, json: async () => ({ total: 1, users: [{ user_id: "a", created_at: "2026-10-02T12:00:00.000Z" }] }) };
  };
  const accounts = await readSignupAccounts({ start, end, fetch, env: {} });
  assert.equal(queries, 3); assert.equal(accounts.length, 1);
  await assert.rejects(readSignupAccounts({ start, end, fetch: async () => ({ ok: false, status: 403 }), env: { AUTH0_M2M_SECRET: "private" } }), error => !error.message.includes("private"));
});
