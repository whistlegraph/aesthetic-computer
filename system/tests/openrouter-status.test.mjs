import test from "node:test";
import assert from "node:assert/strict";
import { createBalance } from "../backend/openrouter-balance.mjs";
import { createHandler } from "../netlify/functions/openrouter-status.mjs";

const credits = (total_credits, total_usage, status = 200) => async (url, options) => {
  assert.equal(url, "https://openrouter.ai/api/v1/credits");
  assert.equal(options.headers.Authorization, "Bearer k");
  return { ok: status === 200, status, json: async () => ({ data: { total_credits, total_usage } }) };
};
const get = (headers = {}) => ({ httpMethod: "GET", headers });
const body = (response) => JSON.parse(response.body);

test("the balance is credits less usage, cached, and unknown rather than wrong", async () => {
  let calls = 0, t = 0;
  const fetch = async (...a) => { calls++; return credits(50, 42.5)(...a); };
  const balance = createBalance({ key: "k", fetch, now: () => t });
  assert.equal((await balance()).balance_usd, 7.5);
  await balance(); assert.equal(calls, 1);
  t = 61_000; await balance(); assert.equal(calls, 2);
  assert.equal((await createBalance({ key: "k", fetch: credits(1, 1, 401) })()).unknown, true);
  assert.equal((await createBalance({ key: "" })()).unknown, true);
  assert.equal((await createBalance({ key: "k", fetch: async () => { throw Error("down"); } })()).unknown, true);
});

test("anyone hears funded or not; only an admin hears the dollars", async () => {
  const balance = createBalance({ key: "k", fetch: credits(20, 17) });
  const handler = createHandler({ balance, floor: 5, authorize: async (h) => h.authorization === "Bearer admin" ? { sub: "a" } : { sub: "b" }, hasAdmin: async (user) => user.sub === "a" });
  const anon = body(await handler(get()));
  assert.equal(anon.funded, false); assert.equal(anon.floor_usd, 5); assert.equal(anon.balance_usd, undefined); assert.match(anon.message, /402/);
  assert.equal(body(await handler(get({ authorization: "Bearer someone" }))).balance_usd, undefined);
  const admin = body(await handler(get({ authorization: "Bearer admin" })));
  assert.equal(admin.balance_usd, 3); assert.equal(admin.credits_usd, 20);
  const rich = createHandler({ balance: createBalance({ key: "k", fetch: credits(100, 1) }), floor: 5, authorize: async () => null, hasAdmin: async () => false });
  assert.equal(body(await rich(get())).funded, true);
  const blind = createHandler({ balance: createBalance({ key: "" }), authorize: async () => null, hasAdmin: async () => false });
  assert.equal(body(await blind(get())).funded, null);
  assert.equal((await handler({ httpMethod: "POST", headers: {} })).statusCode, 405);
});
