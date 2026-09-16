import { test } from "node:test";
import assert from "node:assert/strict";
import { createClient, validateDraft, safe, clip, width, wrap, replyTo } from "./client.mjs";

const draft = { to: "@friend", subject: "Hello", body: "hi!" };
const session = { access_token: "synthetic-token", user: { sub: "test-user" } };
test("uses the shared session and mail API for reads, sending and read state", async () => {
  const calls = [];
  const client = createClient({ tokens: async () => session, request: async (url, options) => {
    calls.push({ url, ...options }); return { ok: true, json: async () => ({ status: "mailed" }) };
  } });
  await client.inbox(); await client.send(draft); await client.read("letter-id");
  assert.equal(calls.length, 3);
  assert.ok(calls.every((c) => c.url === "https://aesthetic.computer/api/mail" && c.headers.Authorization === "Bearer synthetic-token" && c.redirect === "error"));
  assert.deepEqual(JSON.parse(calls[1].body), { to: "@friend", subject: "Hello", text: "hi!" });
  assert.deepEqual(JSON.parse(calls[2].body), { action: "read", id: "letter-id" });
});
test("account changes cannot silently send from a different identity", async () => {
  let current = session; let calls = 0;
  const client = createClient({ tokens: async () => current, request: async () => { calls++; return { ok: true, json: async () => ({}) }; } });
  await client.inbox(); current = { ...session, user: { sub: "another-user" } };
  await assert.rejects(() => client.send(draft), /account changed/);
  assert.equal(calls, 1);
});
test("send errors never leak provider contents or silently retry", async () => {
  let calls = 0;
  const client = createClient({ tokens: async () => session, request: async () => { calls++; throw new Error("PRIVATE_BODY_AND_TOKEN"); } });
  await assert.rejects(() => client.send(draft), { message: "Delivery unconfirmed. Refresh Sent before resending." });
  assert.equal(calls, 1);
});
test("authentication and HTTP failures remain actionable without raw responses", async () => {
  const unauth = createClient({ tokens: async () => { throw new Error("SECRET"); } });
  await assert.rejects(() => unauth.inbox(), /ac-login/);
  for (const status of [401, 403, 404, 500]) {
    const client = createClient({ tokens: async () => session, request: async () => ({ ok: false, status, text: () => { throw new Error("Never read raw error body"); } }) });
    await assert.rejects(() => client.send(draft), (e) => !e.message.includes("SECRET"));
  }
});
test("rejects API truncation and empty letters before sending", () => {
  assert.equal(validateDraft(draft), null);
  assert.match(validateDraft({ ...draft, body: "x".repeat(501) }), /500/);
  assert.match(validateDraft({ ...draft, subject: "x".repeat(81) }), /80/);
  assert.match(validateDraft({ ...draft, to: "" }), /recipient/);
  assert.match(validateDraft({ ...draft, body: "  " }), /letter/);
  assert.match(validateDraft({ ...draft, subject: "a\nb" }), /single lines/);
});
test("untrusted content cannot emit terminal controls", () => {
  assert.ok(!/[\x1b\x07\x9b\u202e]/.test(safe("hi\x1b]52;c;clipboard\x07\x9b31m\u202e")));
  assert.equal(clip("hello\nworld", 7), "hello w");
  assert.equal(width("你好"), 4);
  assert.equal(clip("你好x", 3), "你");
  assert.deepEqual(wrap("ab\ncd", 1), ["a", "b", "c", "d"]);
  assert.equal(replyTo({ from: "Person", fromEmail: "person@example.invalid" }), "person@example.invalid");
});
