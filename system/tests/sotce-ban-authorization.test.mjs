import assert from "node:assert/strict";
import { mock, test } from "node:test";

let banned = false;
let authLookups = 0;
const user = { sub: "auth0|test", email: "reader@example.com", email_verified: true };
mock.module("../backend/database.mjs", { namedExports: {
  connect: async () => ({ db: { collection: () => ({
    findOne: async () => banned ? { _id: "ban" } : null,
  }) } }),
} });
mock.module("got", { namedExports: {
  got: async () => { authLookups++; return { body: user }; },
} });
const { authorize } = await import("../backend/authorization.mjs");

test("a newly banned Sotce account loses cached and fresh authorization", async () => {
  const headers = { authorization: "Bearer test-token" };
  assert.equal((await authorize(headers, "sotce"))?.sub, user.sub);
  assert.equal(authLookups, 1);
  banned = true;
  assert.equal(await authorize(headers, "sotce"), undefined);
  assert.equal(authLookups, 1, "cached token rejected without waiting for Auth0 cache expiry");
  assert.equal(await authorize({ authorization: "Bearer another-token" }, "sotce"), undefined);
  assert.equal(authLookups, 2, "fresh token also rejected");
});
