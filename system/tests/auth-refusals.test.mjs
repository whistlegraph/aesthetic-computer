import assert from "node:assert/strict";
import { mock, test } from "node:test";

const writes = [];
let failing = false;
mock.module("../backend/database.mjs", { namedExports: {
  connect: async () => {
    if (failing) throw new Error("offline");
    return { db: { collection: (name) => ({
      updateOne: async (filter, update, options) => writes.push({ name, filter, update, options }),
    }) } };
  },
} });
const { recordRefusal } = await import("../backend/auth-refusals.mjs");
const settle = () => new Promise((resolve) => setImmediate(resolve));

test("a refusal counts into one row per account, tenant and day", async () => {
  recordRefusal({ tenant: "sotce", user: { sub: "auth0|x", email: "x@example.com" }, reason: "sotce-ban" });
  await settle();
  assert.equal(writes.length, 1);
  const [w] = writes;
  assert.equal(w.name, "auth-refusals");
  assert.match(w.filter._id, /^sotce:auth0\|x:\d{4}-\d{2}-\d{2}$/);
  assert.deepEqual(w.update.$inc, { count: 1 });
  assert.equal(w.update.$setOnInsert.reason, "sotce-ban");
  assert.equal(w.update.$setOnInsert.email, "x@example.com");
  assert.equal(w.options.upsert, true);
});

test("no account, no record; a lost database never throws", async () => {
  writes.length = 0;
  recordRefusal({ tenant: "aesthetic", user: {}, reason: "account-locked" });
  failing = true;
  assert.doesNotThrow(() => recordRefusal({ tenant: "aesthetic", user: { sub: "auth0|y" }, reason: "account-locked" }));
  await settle();
  assert.equal(writes.length, 0);
});
