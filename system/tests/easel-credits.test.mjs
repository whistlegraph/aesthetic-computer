import test from "node:test";
import assert from "node:assert/strict";
import { createHandler } from "../netlify/functions/easel-credits.mjs";
const event = { httpMethod: "GET", headers: { authorization: "Bearer test" } };
const deps = {
  authorize: async () => ({ sub: "user" }),
  getHandleOrEmail: async () => "@tester",
  checkBudget: async () => ({
    day: "2026-09-18",
    used: 0,
    budget: 200000,
    remaining: 200000,
  }),
  paidBalance: async () => 1000000,
};
test("braincell balance values free and purchased allowance at the actual pack rate", async () => {
  const result = await createHandler(deps)(event);
  assert.equal(result.statusCode, 200);
  const value = JSON.parse(result.body);
  assert.deepEqual(value.dollars, {
    currency: "USD",
    free: 1,
    purchased: 5,
    total: 6,
  });
  assert.equal(value.offer, null); // The balance is valued even when checkout is disabled.
  const alternate = JSON.parse(
    (
      await createHandler({
        ...deps,
        creditPack: { amount: 1000, credits: 1000000 },
      })(event)
    ).body,
  );
  assert.equal(alternate.dollars.total, 12);
});
test("unknown allowances and malformed purchased balances never become dollar balances", async () => {
  for (const changes of [
    { paidBalance: async () => NaN },
    { paidBalance: async () => -10 },
    { checkBudget: async () => ({ unknown: true }) },
  ]) {
    const result = await createHandler({ ...deps, ...changes })(event);
    assert.equal(result.statusCode, 503);
    assert.equal(JSON.parse(result.body).dollars, undefined);
  }
  assert.equal(
    (await createHandler(deps)({ ...event, headers: {} })).statusCode,
    401,
  );
});
