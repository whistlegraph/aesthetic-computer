import test from "node:test";
import assert from "node:assert/strict";
import {
  SIGNUP_HEADER, UNVERIFIED_GRACE_MS, signupSecret, signupClaim, signupBody, signupMessage,
  signupReply, signupLookup, abandoned, releaseAbandoned,
} from "../backend/signup-handle.mjs";

const env = { AUTH0_M2M_SECRET: "root-secret" };
const secret = signupSecret(env);
const respond = (statusCode, body) => ({ statusCode, body: JSON.stringify(body) });
const read = (response) => JSON.parse(response.body);

test("the secret is derived, stable, and absent without its root", () => {
  assert.match(secret, /^[a-f0-9]{64}$/);
  assert.equal(signupSecret(env), secret);
  assert.notEqual(secret, env.AUTH0_M2M_SECRET);
  assert.equal(signupSecret({}), null);
});

test("no header is not a signup claim; a wrong one is refused", () => {
  assert.equal(signupClaim({}, { sub: "auth0|abc" }, env), undefined);
  assert.equal(signupClaim({ [SIGNUP_HEADER]: "nope" }, { sub: "auth0|abc" }, env), null);
  assert.equal(signupClaim({ [SIGNUP_HEADER]: secret }, { sub: "auth0|abc" }, {}), null);
});

test("the right secret vouches for an aesthetic subject only", () => {
  const headers = { [SIGNUP_HEADER]: secret };
  assert.deepEqual(signupClaim(headers, { sub: "auth0|abc123", email: "a@b.c", email_verified: false }, env),
    { sub: "auth0|abc123", email: "a@b.c", unverified: true });
  assert.equal(signupClaim(headers, { sub: "google-oauth2|1234", email_verified: true }, env).unverified, false);
  assert.equal(signupClaim(headers, { sub: "email|6aa4", email_verified: "true" }, env).unverified, false);
  for (const sub of [undefined, "", "abc", "sotce-auth0|abc", "auth0|a b", { $ne: 1 }])
    assert.equal(signupClaim(headers, { sub }, env), null, String(sub));
});

test("only the handle rides in: no tenant, action or hold", () => {
  assert.deepEqual(JSON.parse(signupBody({ handle: "x", action: "strip", tenant: "sotce", hold: "h", createOnly: false })),
    { handle: "x", createOnly: true });
});

test("the form gets a flat reply with the words to show", () => {
  assert.deepEqual(read(signupReply(respond(200, { handle: "@Cow" }), "Cow", respond)), { ok: "yes", handle: "Cow" });
  assert.deepEqual(read(signupReply(respond(500, { message: "taken" }), "@cow", respond)), { ok: "no", message: "@cow is taken. Try another." });
  assert.equal(read(signupReply(respond(400, { message: "naughty" }), "x", respond)).message, "Try a different name.");
  assert.equal(read(signupReply(respond(400, { message: "too long" }), "x", respond)).message, signupMessage("invalid"));
  assert.equal(read(signupReply({ statusCode: 502, body: "<html>" }, "x", respond)).ok, "no");
  assert.equal(signupReply(respond(500, { message: "error" }), "x", respond).statusCode, 200);
});

test("an unverified handle gives way after a week, and only then", async () => {
  const now = new Date("2026-11-18T00:00:00Z");
  const old = new Date(+now - UNVERIFIED_GRACE_MS - 1000), fresh = new Date(+now - 1000);
  assert.equal(abandoned({ unverifiedAt: old }, now), true);
  assert.equal(abandoned({ unverifiedAt: fresh }, now), false);
  assert.equal(abandoned({}, now), false);

  const calls = [];
  const handles = { deleteOne: async (q) => calls.push(["delete", q]), updateOne: async (q, u) => calls.push(["update", q, u]) };
  const doc = { _id: "auth0|x", handle: "cow", unverifiedAt: old };
  const forgot = [];
  const forget = async (d) => forgot.push(d.handle);

  assert.equal(await releaseAbandoned(handles, { ...doc, unverifiedAt: fresh }, { verified: async () => false, forget, now }), false);
  assert.equal(calls.length, 0);

  assert.equal(await releaseAbandoned(handles, doc, { verified: async () => true, forget, now }), false);
  assert.deepEqual(calls.pop(), ["update", { _id: "auth0|x" }, { $unset: { unverifiedAt: "" } }]);

  assert.equal(await releaseAbandoned(handles, doc, { verified: async () => undefined, forget, now }), false);
  assert.equal(calls.length, 0);

  assert.equal(await releaseAbandoned(handles, doc, { verified: async () => false, forget, now }), true);
  assert.deepEqual(calls.pop(), ["delete", { _id: "auth0|x" }]);
  assert.deepEqual(forgot, ["cow"]);
});

test("the lookup finds a handle on the account, or on a verified sibling", async () => {
  const handles = { "auth0|old": "@cow" };
  const deps = { handleFor: async (sub) => handles[sub], subsForEmail: async () => ["google-oauth2|new", "auth0|old"] };
  assert.deepEqual(await signupLookup({ sub: "auth0|old", unverified: false }, deps), { ok: "yes", handle: "cow" });
  assert.deepEqual(await signupLookup({ sub: "google-oauth2|new", email: "a@b.c", unverified: false }, deps), { ok: "yes", handle: "cow" });
  // An unverified address proves nothing about who owns the older account.
  assert.deepEqual(await signupLookup({ sub: "auth0|new", email: "a@b.c", unverified: true }, deps), { ok: "no" });
  assert.deepEqual(await signupLookup({ sub: "auth0|new", unverified: false }, deps), { ok: "no" });
});
