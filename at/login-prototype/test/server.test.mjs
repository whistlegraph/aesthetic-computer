import assert from "node:assert/strict";
import { once } from "node:events";
import { request as httpRequest } from "node:http";
import { test } from "node:test";
import { createPrototype } from "../server.mjs";
import { memoryStore } from "../link-flow.mjs";

const DID = "did:plc:k3k3wknzkcnekbnyde4dbatz";
async function harness(t, { now } = {}) {
  const stateStore = memoryStore(), sessionStore = memoryStore();
  const calls = { callback: 0, revoked: [] };
  const client = {
    async authorize(handle, { state, scope }) {
      assert.equal(handle, "aesthetic.computer");
      assert.equal(scope, "atproto");
      await stateStore.set("provider-state", { appState: state });
      return new URL("https://bsky.social/oauth/authorize");
    },
    async callback() {
      calls.callback++;
      const { appState } = await stateStore.get("provider-state");
      await stateStore.del("provider-state");
      return { session: { did: DID }, state: appState };
    },
    async revoke(did) { calls.revoked.push(did); },
  };
  const { server, flow, origin } = createPrototype({ now, oauth: { client, stateStore, sessionStore } });
  server.listen(0, "127.0.0.1"); await once(server, "listening");
  t.after(() => new Promise(resolve => { server.close(resolve); server.closeAllConnections(); }));
  const base = `http://127.0.0.1:${server.address().port}`;
  // Native HTTP lets this test exercise exact Host headers on an ephemeral
  // port; fetch may normalize Host to the connection address.
  const request = (path, options = {}) => new Promise((resolve, reject) => {
    const req = httpRequest(base + path, { method: options.method ?? "GET", headers: { Host: "127.0.0.1:4287", ...options.headers } }, res => {
      const chunks = [];
      res.on("data", chunk => chunks.push(chunk));
      res.on("end", () => resolve(new Response(Buffer.concat(chunks), { status: res.statusCode, headers: res.headers })));
    });
    req.on("error", reject);
    req.end(options.body?.toString());
  });
  const first = await request("/");
  assert.equal(first.headers.get("referrer-policy"), "same-origin");
  assert.match(first.headers.get("content-security-policy"), /form-action 'self' https:\/\/bsky.social https:\/\/at.aesthetic.computer;/);
  const cookie = first.headers.get("set-cookie").split(";")[0];
  const html = await first.text();
  const csrf = html.match(/name="csrf" value="([^"]+)"/)[1];
  const post = (path, fields = {}, headers = {}) => request(path, { method: "POST", body: new URLSearchParams({ csrf, ...fields }), headers: { Cookie: cookie, Origin: origin, "Content-Type": "application/x-www-form-urlencoded", ...headers } });
  return { request, post, cookie, flow, calls };
}

test("HTTP flow links only the SDK DID, requires confirmation, and discards the OAuth session", async t => {
  const h = await harness(t);
  assert.equal((await h.post("/start", { handle: "aesthetic.computer" })).status, 303);
  const callback = await h.request("/callback?state=provider-state&did=did:plc:attacker", { headers: { Cookie: h.cookie } });
  assert.equal(callback.status, 303);
  assert.equal(callback.headers.get("location"), "/confirm");
  assert.equal(h.flow.byDid.size, 0);
  assert.deepEqual(h.calls.revoked, [DID]);
  assert.equal((await h.post("/confirm")).status, 303);
  assert.ok(h.flow.byDid.has(DID));
  assert.equal((await h.post("/confirm")).status, 400);
  assert.equal((await h.post("/disconnect")).status, 303);
  assert.equal(h.flow.byDid.size, 0);
});

test("cross-site forms, wrong CSRF, and hostile Host headers cannot start OAuth", async t => {
  const h = await harness(t);
  assert.equal((await h.post("/start", { handle: "aesthetic.computer" }, { Origin: "https://evil.example" })).status, 403);
  assert.equal((await h.post("/start", { handle: "aesthetic.computer", csrf: "wrong" })).status, 403);
  assert.equal((await h.request("/", { headers: { Host: "evil.example" } })).status, 403);
  assert.equal(h.flow.transactions.size, 0);
});

test("callback in another browser is rejected before exchanging the code", async t => {
  const h = await harness(t);
  await h.post("/start", { handle: "aesthetic.computer" });
  const other = await h.request("/");
  const cookie = other.headers.get("set-cookie").split(";")[0];
  assert.equal((await h.request("/callback?state=provider-state", { headers: { Cookie: cookie } })).status, 400);
  assert.equal(h.calls.callback, 0);
  assert.equal(h.flow.byDid.size, 0);
});

test("callback errors never echo credentials, state, or query strings", async t => {
  const h = await harness(t);
  const r = await h.request("/callback?state=missing&code=private-code", { headers: { Cookie: h.cookie } });
  assert.equal(r.status, 400);
  assert.doesNotMatch(await r.text(), /private-code|state=missing/);
  assert.equal(r.headers.get("referrer-policy"), "no-referrer");
  assert.equal(r.headers.get("cache-control"), "no-store");
});

test("an expired local test session releases its disposable DID binding", async t => {
  let now = 1_000;
  const h = await harness(t, { now: () => now });
  await h.post("/start", { handle: "aesthetic.computer" });
  await h.request("/callback?state=provider-state", { headers: { Cookie: h.cookie } });
  await h.post("/confirm");
  assert.ok(h.flow.byDid.has(DID));
  now += 300_000;
  const r = await h.request("/", { headers: { Cookie: h.cookie } });
  assert.equal(r.status, 200);
  assert.equal(h.flow.byDid.size, 0);
  assert.notEqual(r.headers.get("set-cookie").split(";")[0], h.cookie);
});
