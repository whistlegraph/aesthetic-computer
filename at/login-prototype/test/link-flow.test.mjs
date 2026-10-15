import assert from "node:assert/strict";
import { test } from "node:test";
import { LinkFlow, memoryStore } from "../link-flow.mjs";
import { allowedNetworkURL, makeOAuth } from "../oauth.mjs";

const DID = "did:plc:k3k3wknzkcnekbnyde4dbatz";
const OTHER = "did:plc:a2eiol3qtvncznlbt6v4wvww";
const principal = (overrides = {}) => ({ subject: "auth0|existing", browser: "browser-a", authenticatedAt: 1_000, locked: false, ...overrides });
const setup = () => new LinkFlow({ now: () => 2_000 });

test("OAuth proof needs explicit confirmation and preserves the canonical subject", () => {
  const flow = setup(), p = principal(), id = flow.begin(p);
  assert.throws(() => flow.confirm(id, p), /authorization first/);
  flow.verify(id, p, DID);
  assert.equal(flow.byDid.size, 0);
  assert.equal(flow.confirm(id, p), DID);
  assert.equal(flow.byDid.get(DID), "auth0|existing");
  assert.throws(() => flow.confirm(id, p), /already used/);
});

test("another browser or account cannot consume a linking transaction", () => {
  const flow = setup(), p = principal(), id = flow.begin(p);
  assert.throws(() => flow.verify(id, principal({ browser: "browser-b" }), DID), /same signed-in browser/);
  assert.throws(() => flow.verify(id, principal({ subject: "auth0|attacker" }), DID), /same signed-in browser/);
  flow.verify(id, p, DID);
  assert.equal(flow.confirm(id, p), DID);
});

test("stale, future, missing, and locked AC sessions are refused", () => {
  const flow = new LinkFlow({ now: () => 500_000 });
  for (const patch of [{}, { authenticatedAt: 600_000 }, { authenticatedAt: undefined }, { subject: "" }, { browser: "" }, { authenticatedAt: 499_999, locked: true }]) {
    assert.throws(() => flow.begin(principal(patch)), /fresh, unlocked/);
  }
});

test("an account locked after OAuth cannot confirm", () => {
  const flow = setup(), p = principal(), id = flow.begin(p);
  flow.verify(id, p, DID);
  assert.throws(() => flow.confirm(id, { ...p, locked: true }), /fresh, unlocked/);
  assert.equal(flow.byDid.size, 0);
});

test("transaction expiry still applies after AC reauthentication", () => {
  let now = 2_000;
  const flow = new LinkFlow({ now: () => now }), p = principal(), id = flow.begin(p);
  flow.verify(id, p, DID);
  now += 300_000;
  assert.throws(() => flow.confirm(id, { ...p, authenticatedAt: now }), /expired/);
});

test("a verified DID can belong to only one AC account", () => {
  const flow = setup(), p = principal(), other = principal({ subject: "google-oauth2|other", browser: "browser-b" });
  const first = flow.begin(p), second = flow.begin(other);
  flow.verify(first, p, DID);
  flow.verify(second, other, DID);
  flow.confirm(first, p);
  assert.throws(() => flow.confirm(second, other), /another account/);
  assert.equal(flow.byDid.get(DID), p.subject);
  assert.equal(flow.bySubject.has(other.subject), false);
});

test("linking another DID never silently replaces the existing identity", () => {
  const flow = setup(), p = principal();
  const first = flow.begin(p);
  flow.verify(first, p, DID); flow.confirm(first, p);
  const next = flow.begin(p);
  flow.verify(next, p, OTHER);
  assert.throws(() => flow.confirm(next, p), /Disconnect/);
  flow.disconnect(p);
  assert.equal(flow.byDid.size, 0);
  const retry = flow.begin(p);
  flow.verify(retry, p, OTHER); flow.confirm(retry, p);
  assert.equal(flow.bySubject.get(p.subject), OTHER);
});

test("handles and email addresses are never accepted as verified DIDs", () => {
  const flow = setup(), p = principal(), id = flow.begin(p);
  for (const value of ["aesthetic.computer", "mail@aesthetic.computer", "did:plc:short", null]) {
    assert.throws(() => flow.verify(id, p, value), /valid AT Protocol identity/);
  }
});

test("a new attempt invalidates the older browser transaction", () => {
  const flow = setup(), p = principal(), first = flow.begin(p), second = flow.begin(p);
  assert.throws(() => flow.pending(first, p), /expired/);
  assert.ok(flow.pending(second, p));
});

test("OAuth secrets expire in memory and are returned as copies", async () => {
  let now = 100;
  const store = memoryStore({ now: () => now, ttl: 10 });
  await store.set("id", { verifier: "test-value" });
  const copy = await store.get("id"); copy.verifier = "changed";
  assert.equal((await store.get("id")).verifier, "test-value");
  now = 110;
  assert.equal(await store.get("id"), undefined);
});

test("discovery rejects private, deceptive, credential-bearing, and arbitrary hosts", () => {
  for (const url of ["http://bsky.social", "https://127.0.0.1", "https://169.254.169.254", "https://bsky.social.evil.example", "https://user:secret@bsky.social", "https://bsky.social:8443", "https://example.com"]) {
    assert.equal(allowedNetworkURL(url), false, url);
  }
  for (const url of ["https://bsky.social", "https://plc.directory/did:plc:test", "https://chanterelle.us-west.host.bsky.network", "https://at.aesthetic.computer"]) {
    assert.equal(allowedNetworkURL(url), true, url);
  }
});

test("the real SDK accepts the loopback client and identity-only scope", () => {
  const { client } = makeOAuth("http://127.0.0.1:4287");
  assert.equal(client.clientMetadata.scope, "atproto");
  assert.deepEqual(client.clientMetadata.redirect_uris, ["http://127.0.0.1:4287/callback"]);
  assert.equal(client.clientMetadata.dpop_bound_access_tokens, true);
  assert.equal(client.clientMetadata.token_endpoint_auth_method, "none");
});
