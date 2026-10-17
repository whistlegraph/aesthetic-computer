import test from "node:test";
import assert from "node:assert/strict";
import { hasMonthlySupport, createGiveStatusHandler } from "../backend/monthly-support.mjs";
import { createHandleColorsHandler } from "../netlify/functions/handle-colors.mjs";

const owner = { sub: "auth0|owner", email: "Owner@Example.test", email_verified: true };
const sequence = rows => ({ async *[Symbol.asyncIterator]() { yield* rows; } });
function stripeFixture(subscription = {}) {
  return {
    customers: { list: args => { assert.equal(args.email, "owner@example.test"); return sequence([{ id: "first" }, { id: "second" }]); } },
    subscriptions: { list: args => sequence(args.customer === "first" ? [] : [{ status: "active",
      items: { data: [{ price: { unit_amount: 100, recurring: { interval: "month", interval_count: 1 }, product: "give" } }] },
      ...subscription }]) },
    products: { retrieve: async id => ({ id, name: "Give" }) },
  };
}
test("Verified monthly supporters qualify at any amount, including legacy Give customers beyond the first result", async () => {
  const stripe = stripeFixture();
  assert.equal(await hasMonthlySupport(owner, { stripe }), true);
  for (const status of ["canceled", "incomplete", "past_due", "trialing", "unpaid"])
    assert.equal(await hasMonthlySupport(owner, { stripe: stripeFixture({ status }) }), false);
  for (const recurring of [null, { interval: "year", interval_count: 1 }, { interval: "month", interval_count: 3 }])
    assert.equal(await hasMonthlySupport(owner, { stripe: stripeFixture({ items: { data: [{ price: { recurring, product: "give" } }] } }) }), false);
  stripe.products.retrieve = async () => ({ name: "Another product" });
  assert.equal(await hasMonthlySupport(owner, { stripe }), false);
  stripe.products.retrieve = async () => ({ name: "Monthly AC support", metadata: { ac_support: "monthly" } });
  assert.equal(await hasMonthlySupport(owner, { stripe }), true);
  assert.equal(await hasMonthlySupport({ ...owner, email_verified: false }, { stripe: {} }), false);
  assert.equal(await hasMonthlySupport(null, { stripe: {} }), false);
});

test("Support status exposes only the authenticated caller's entitlement and fails safely", async () => {
  let caller = owner, fail = false;
  const handler = createGiveStatusHandler({ authorize: async () => caller, support: async user => {
    assert.equal(user, owner); if (fail) throw Error("private Stripe error"); return true;
  } });
  const event = { httpMethod: "GET", headers: {}, queryStringParameters: { email: "victim@example.test" } };
  const result = await handler(event);
  assert.deepEqual(JSON.parse(result.body), { monthlySupporter: true });
  assert.equal(result.headers["Cache-Control"], "no-store");
  fail = true; assert.equal((await handler(event)).statusCode, 503);
  caller = null; assert.equal((await handler(event)).statusCode, 401);
});

test("Colors remain readable after cancellation; saving requires ownership and active support", async () => {
  const colors = [{ r: 10, g: 20, b: 30 }, { r: 40, g: 50, b: 60 }];
  let paid = false, unavailable = false, writes = 0, checks = 0, closes = 0, broadcasts = 0;
  const handler = createHandleColorsHandler({ authorize: async () => owner, findSisterSub: async () => null,
    connect: async () => ({ db: { collection: () => ({ findOne: async () => ({ handle: "x", colors }),
      find: () => ({ toArray: async () => [{ handle: "x", colors }] }),
      updateOne: async () => { writes++; } }) }, disconnect: async () => { closes++; } }),
    support: async () => { checks++; if (unavailable) throw Error("Unavailable"); return paid; },
    broadcast: async () => { broadcasts++; },
  });
  const get = await handler({ httpMethod: "GET", queryStringParameters: { handle: "x" } });
  assert.deepEqual(JSON.parse(get.body).colors, colors); assert.equal(checks, 0);
  const post = { httpMethod: "POST", headers: {}, body: JSON.stringify({ handle: "x", colors, monthlySupporter: true }) };
  assert.equal((await handler(post)).statusCode, 403); assert.equal(writes, 0);
  paid = true;
  assert.equal((await handler({ ...post, body: JSON.stringify({ handle: "y", colors }) })).statusCode, 403);
  assert.equal(checks, 1); // ownership checked before billing
  assert.equal((await handler(post)).statusCode, 200); assert.equal(writes, 1); assert.equal(broadcasts, 1);
  unavailable = true;
  assert.equal((await handler(post)).statusCode, 503); assert.equal(writes, 1);
  assert.equal(closes, 5);
  for (const bad of [null, { r: null, g: 0, b: 0 }, { r: 1.1, g: 0, b: 0 }])
    assert.equal((await handler({ ...post, body: JSON.stringify({ handle: "x", colors: [bad, colors[1]] }) })).statusCode, 400);
});

test("a handle shared by two accounts in different case reads the one with colours", async () => {
  const colors = [{ r: 207, g: 230, b: 255 }, { r: 74, g: 144, b: 226 }];
  // The empty "Jeffrey" record comes back first, as it did in production.
  const handler = createHandleColorsHandler({
    connect: async () => ({ db: { collection: () => ({
      find: () => ({ toArray: async () => [{ handle: "Jeffrey" }, { handle: "jeffrey", colors }] }),
    }) }, disconnect: async () => {} }),
  });
  for (const asked of ["jeffrey", "@jeffrey", "JEFFREY", "Jeffrey"]) {
    const read = await handler({ httpMethod: "GET", queryStringParameters: { handle: asked } });
    assert.deepEqual(JSON.parse(read.body).colors, colors, asked);
  }
});
