import test from "node:test";
import assert from "node:assert/strict";
import { operationsAuth } from "./operations-auth.mjs";

async function request(authorization, load, serviceToken = "", method = "GET") {
  const result = { status: 200, headers: {}, reachedData: false };
  const res = {
    set(key, value) { result.headers[key] = value; return this; },
    status(code) { result.status = code; return this; },
    json(body) { result.body = body; return this; },
  };
  await operationsAuth(load, serviceToken)({ method, headers: { authorization } }, res, () => { result.reachedData = true; });
  assert.equal(result.headers["Cache-Control"], "private, no-store");
  return result;
}

test("anonymous and malformed credentials cannot load authorization or operational data", async () => {
  for (const value of [undefined, "", "Basic abc", "Bearer", "Bearer a b"]) {
    const r = await request(value, () => { assert.fail("anonymous auth lookup"); });
    assert.equal(r.status, 401);
    assert.equal(r.reachedData, false);
  }
});
test("invalid tokens and ordinary accounts cannot read operational data", async () => {
  for (const [user, status] of [[undefined, 401], [{ sub: "member" }, 403]]) {
    const r = await request("Bearer fixture", async () => ({ authorize: async () => user, hasAdmin: async () => false }));
    assert.equal(r.status, status);
    assert.equal(r.reachedData, false);
  }
});
test("verified admins retain access using their existing bearer token", async () => {
  const user = { sub: "fixture-admin" };
  const r = await request("Bearer fixture", async () => ({
    authorize: async headers => { assert.equal(headers.authorization, "Bearer fixture"); return user; },
    hasAdmin: async candidate => { assert.equal(candidate, user); return true; },
  }));
  assert.equal(r.status, 200);
  assert.equal(r.reachedData, true);
});
test("authorization errors fail closed without disclosing internal error text", async () => {
  const r = await request("Bearer fixture", async () => { throw new Error("private database details"); });
  assert.equal(r.status, 503);
  assert.equal(r.reachedData, false);
  assert.doesNotMatch(JSON.stringify(r), /private database details/);
});

test("configured service credentials allow read diagnostics and cannot authorize writes", async () => {
  const noUser = async () => ({ authorize: async () => undefined });
  const allowed = await request("Bearer fixture-service", () => { assert.fail("interactive auth lookup"); }, "fixture-service");
  assert.equal(allowed.reachedData, true);
  for (const [token, method] of [["fixture-wrong", "GET"], ["fixture-service", "POST"]]) {
    const denied = await request(`Bearer ${token}`, noUser, "fixture-service", method);
    assert.equal(denied.status, 401);
    assert.equal(denied.reachedData, false);
  }
});
