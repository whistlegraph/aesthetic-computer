// /user answers a handle for anyone, but an email only for its signed-in
// owner (or an admin). Open, it was an email → account → handle → wallet oracle.
import assert from "node:assert/strict";
import test, { mock } from "node:test";

let asker = null; // Who the bearer token resolves to.
let admin = false;
let emailLookups = 0;

mock.module("../backend/authorization.mjs", {
  exports: {
    authorize: async ({ authorization } = {}) => (authorization ? asker : undefined),
    hasAdmin: async () => admin,
    userIDFromHandle: async (handle) => (handle === "jeffrey" ? "auth0|jeffrey" : undefined),
    userIDFromEmail: async () => {
      emailLookups++;
      return { userID: "auth0|owner", email_verified: true };
    },
    handleFor: async () => "owner",
  },
});
mock.module("../backend/kv.mjs", {
  exports: { connect: async () => {}, get: async () => null, set: async () => {} },
});
mock.module("../backend/shell.mjs", {
  exports: { shell: { log() {}, error() {}, warn() {} } },
});

const { handler } = await import("../netlify/functions/user.js");
const ask = (from, authorization) =>
  handler({
    httpMethod: "GET",
    headers: authorization ? { authorization } : {},
    queryStringParameters: { from, withHandle: "true" },
  });

test("a handle lookup stays public", async () => {
  const res = await ask("@jeffrey");
  assert.equal(res.statusCode, 200);
  assert.equal(JSON.parse(res.body).sub, "auth0|jeffrey");
});

test("an email lookup without a token is refused before any lookup runs", async () => {
  const res = await ask("owner@example.com");
  assert.equal(res.statusCode, 401);
  assert.equal(emailLookups, 0);
  assert.equal(res.body.includes("auth0|"), false);
});

test("someone else's email is refused", async () => {
  asker = { sub: "auth0|other", email: "other@example.com" };
  const res = await ask("owner@example.com", "Bearer other");
  assert.equal(res.statusCode, 403);
  assert.equal(emailLookups, 0);
});

test("the owner gets their own record, whatever the letter case", async () => {
  asker = { sub: "auth0|owner", email: "Owner@Example.com" };
  const res = await ask("owner@example.com", "Bearer owner");
  assert.equal(res.statusCode, 200);
  assert.deepEqual(JSON.parse(res.body), {
    sub: "auth0|owner",
    email_verified: true,
    handle: "owner",
  });
});

test("an admin may look up any email", async () => {
  asker = { sub: "auth0|admin", email: "admin@example.com" };
  admin = true;
  const res = await ask("owner@example.com", "Bearer admin");
  assert.equal(res.statusCode, 200);
  admin = false;
});

test("a bare word that is neither handle nor email never reaches Auth0", async () => {
  const before = emailLookups;
  const res = await ask("jeffrey");
  assert.equal(res.statusCode, 400);
  assert.equal(emailLookups, before);
});
