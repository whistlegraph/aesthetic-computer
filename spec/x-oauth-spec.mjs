// Checks the OAuth 1.0a signing in toolchain/x/x.mjs against the worked
// example X publishes in "Authorizing a request", and checks the weighted
// character count that decides whether a post fits.
//
// This matters because the signing code cannot be exercised against the live
// API until a developer app exists (toolchain/x/SETUP.md). A wrong signature
// fails as a bare 401, which reads exactly like a bad key.

import assert from "node:assert/strict";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const { authorize, explain, parsePostId, searchBudgetPlan, weigh } = await import(
  resolve(ROOT, "toolchain/x/x.mjs"),
);

// https://developer.x.com/en/docs/authentication/oauth-1-0a/creating-a-signature
const VECTOR = {
  method: "POST",
  url: "https://api.twitter.com/1.1/statuses/update.json",
  params: {
    status: "Hello Ladies + Gentlemen, a signed OAuth request!",
    include_entities: "true",
  },
  creds: {
    apiKey: "xvz1evFS4wEEPTGEFPHBog",
    apiSecret: "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw",
    accessToken: "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb",
    accessSecret: "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE",
  },
  fixed: {
    nonce: "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg",
    timestamp: "1318622958",
  },
  signature: "hCtSmYh+iHYCEqBWrE7C7hYmtUk=",
};

test("signs X's published example exactly", () => {
  const header = authorize(VECTOR.method, VECTOR.url, VECTOR.params,
    VECTOR.creds, VECTOR.fixed);
  const signature = decodeURIComponent(
    header.match(/oauth_signature="([^"]+)"/)[1],
  );
  assert.equal(signature, VECTOR.signature);
});

test("header carries every required oauth field", () => {
  const header = authorize("GET", "https://api.x.com/2/users/me", {},
    VECTOR.creds, VECTOR.fixed);
  for (const field of ["oauth_consumer_key", "oauth_nonce",
    "oauth_signature", "oauth_signature_method", "oauth_timestamp",
    "oauth_token", "oauth_version"]) {
    assert.ok(header.includes(`${field}="`), `missing ${field}`);
  }
});

test("a fresh call does not reuse a nonce", () => {
  const nonce = (header) => header.match(/oauth_nonce="([^"]+)"/)[1];
  const a = authorize("GET", "https://api.x.com/2/users/me", {}, VECTOR.creds);
  const b = authorize("GET", "https://api.x.com/2/users/me", {}, VECTOR.creds);
  assert.notEqual(nonce(a), nonce(b));
});

test("a URL weighs 23 no matter how long it is", () => {
  const short = weigh("a https://x.co");
  const long = weigh("a https://www.culturehub.org/events/grokaesthetic-workshop");
  assert.equal(short, long);
  assert.equal(long, 2 + 23);
});

test("plain text weighs its own length", () => {
  assert.equal(weigh("hello"), 5);
});

test("a post that only overflows on raw length is allowed through", () => {
  const url = "https://www.culturehub.org/events/grokaesthetic-workshop";
  const text = "x".repeat(250) + " " + url;
  assert.ok(text.length > 280, "raw length should exceed the limit");
  assert.ok(weigh(text) <= 280, "weighted length should fit");
});

test("reply targets accept ids and canonical status URLs", () => {
  assert.equal(parsePostId("2095871975876972564"), "2095871975876972564");
  assert.equal(
    parsePostId("https://x.com/justinaversano/status/2095871975876972564?s=20"),
    "2095871975876972564",
  );
  assert.equal(
    parsePostId("https://twitter.com/justinaversano/status/2095871975876972564"),
    "2095871975876972564",
  );
  assert.throws(() => parsePostId("https://example.com/post/1"), /X post id/);
});

test("search budget reserves the request's worst-case read cost", () => {
  const plan = searchBudgetPlan({
    spentUsd: 0.05,
    maxResults: 10,
    rateUsd: 0.005,
    budgetUsd: 0.25,
  });
  assert.equal(plan.estimatedUsd, 0.05);
  assert.ok(Math.abs(plan.remainingUsd - 0.15) < Number.EPSILON * 2);
});

test("search budget blocks before a request can cross the daily limit", () => {
  assert.throws(() => searchBudgetPlan({
    spentUsd: 0.21,
    maxResults: 10,
    rateUsd: 0.005,
    budgetUsd: 0.25,
  }), /search blocked/);
});

test("explains the self-serve reply restriction instead of blaming credentials", () => {
  const message = explain(403, JSON.stringify({
    detail: "You can only reply to or quote posts where you are mentioned or are the author.",
  }));
  assert.match(message, /self-serve API accounts/);
  assert.match(message, /manually in X/);
  assert.doesNotMatch(message, /lacks Write permission/);
});
