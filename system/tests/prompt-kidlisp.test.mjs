import test from "node:test";
import assert from "node:assert/strict";
import { savePromptKidlisp } from "../public/aesthetic.computer/lib/prompt-kidlisp.mjs";
const source = "(wipe blue)";
const api = () => ({ handle: () => "@alice", authorize: async () => "token", store: {}, notice() {} });

test("anonymous entries and disabled caching never request a handled save", async () => {
  const request = () => { throw Error("must not fetch"); };
  assert.deepEqual(await savePromptKidlisp({ handle: () => null }, source, { code: "shared", request }), { code: "shared", saved: false });
  const disabled = api(); disabled.store["kidlisp:cache-enabled"] = false;
  assert.deepEqual(await savePromptKidlisp(disabled, source, { request }), { code: null, saved: false });
});

test("typed and generated sources use the same authenticated save, not a claimed handle", async () => {
  for (const code of [null, "shared"]) {
    const result = await savePromptKidlisp(api(), source, { code, request: async (url, options) => {
      assert.equal(url, "/api/store-kidlisp");
      assert.equal(options.headers.Authorization, "Bearer token");
      assert.deepEqual(JSON.parse(options.body), { source, handled: true });
      return Response.json({ code: "alicecode", handled: true, handle: "@alice" });
    } });
    assert.deepEqual(result, { code: "alicecode", saved: true, handle: "@alice" });
  }
});

test("a failed or unverified save preserves the runnable source/shared code and reports failure", async () => {
  const a = api(), notices = []; a.notice = text => notices.push(text);
  for (const response of [Response.json({ error: "expired" }, { status: 401 }), Response.json({ code: "othercode" }),
    Response.json({ code: "othercode", handled: true, handle: "@bob" })]) {
    const result = await savePromptKidlisp(a, source, { code: "shared", request: async () => response });
    assert.deepEqual(result, { code: "shared", saved: false, failed: true });
  }
  assert.equal(notices.length, 3);
});

test("account changes cannot label an old account's save as the new account's work", async () => {
  let handle = "@alice";
  const a = api(); a.handle = () => handle;
  const result = await savePromptKidlisp(a, source, { request: async () => {
    handle = "@bob";
    return Response.json({ code: "alicecode", handled: true, handle: "@alice" });
  } });
  assert.deepEqual(result, { code: null, saved: false, changed: true });
});
