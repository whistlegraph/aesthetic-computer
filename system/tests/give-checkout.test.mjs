import test from "node:test";
import assert from "node:assert/strict";
import Stripe from "stripe";
import { handler, createGiveHandler } from "../netlify/functions/give.js";

test("Give constructs the installed Stripe client before validating amounts", async () => {
  const key = process.env.CONTEXT === "dev" ? "STRIPE_API_TEST_PRIV_KEY" : "STRIPE_API_PRIV_KEY";
  const previous = process.env[key];
  process.env[key] = "sk_test_validation_only";
  try {
    // Each request stops before Stripe network I/O; the current SDK is a class.
    for (const recurring of [false, true]) {
      const result = await handler({ httpMethod: "POST", headers: {},
        body: JSON.stringify({ amount: 50, currency: "usd", recurring }) });
      assert.equal(result.statusCode, 400);
      assert.match(JSON.parse(result.body).error, /Invalid amount/);
    }
  } finally {
    if (previous === undefined) delete process.env[key];
    else process.env[key] = previous;
  }
});


test("Give retains only reviewed placement attribution on checkout and payment records", async (t) => {
  const key = process.env.CONTEXT === "dev" ? "STRIPE_API_TEST_PRIV_KEY" : "STRIPE_API_PRIV_KEY";
  const previous = process.env[key];
  process.env[key] = "sk_test_validation_only";
  const sessions = Object.getPrototypeOf(new Stripe(process.env[key]).checkout.sessions);
  let submitted;
  t.mock.method(sessions, "create", async (config) => {
    submitted = config;
    return { id: "cs_test_attribution", url: "https://pay.aesthetic.computer/test" };
  });
  try {
    for (const recurring of [true, false]) {
      for (const source of ["homepage", "give-piece", undefined, "unreviewed-private-text", { homepage: true }]) {
        const result = await handler({ httpMethod: "POST", headers: {},
          body: JSON.stringify({ amount: 800, currency: "usd", recurring, source }) });
        assert.equal(result.statusCode, 200);
        const expected = ["homepage", "give-piece"].includes(source) ? source : undefined;
        assert.equal(submitted.metadata.source, expected);
        const record = recurring ? submitted.subscription_data : submitted.payment_intent_data;
        assert.deepEqual(record, recurring ? { metadata: { ...(expected ? { source: expected } : {}), ac_support: "monthly" } }
          : expected ? { metadata: { source: expected } } : undefined);
        assert.equal(new URL(submitted.cancel_url).searchParams.get("source"), expected ?? null);
        assert.equal(submitted.mode, recurring ? "subscription" : "payment");
        assert.equal(submitted.line_items[0].price_data.unit_amount, 800);
        assert.ok(!JSON.stringify(submitted).includes("unreviewed-private-text"));
      }
    }
  } finally {
    if (previous === undefined) delete process.env[key];
    else process.env[key] = previous;
  }
});

test("Handle-color checkout binds support to verified identity, rejects duplicates and returns only to the editor", async t => {
  const key = process.env.CONTEXT === "dev" ? "STRIPE_API_TEST_PRIV_KEY" : "STRIPE_API_PRIV_KEY";
  const previous = process.env[key]; process.env[key] = "sk_test_validation_only";
  let user = null, supporting = false, submitted, calls = 0;
  const checkout = createGiveHandler({ authenticate: async () => user, support: async caller => {
    assert.equal(caller, user); return supporting;
  } });
  t.mock.method(Object.getPrototypeOf(new Stripe(process.env[key]).checkout.sessions), "create", async config => {
    calls++; submitted = config; return { id: "cs_test_colors", url: "https://pay.aesthetic.computer/test" };
  });
  try {
    const event = { httpMethod: "POST", headers: {}, body: JSON.stringify({ amount: 100, currency: "usd",
      recurring: true, source: "signup", email: "someone-else@example.test", returnUrl: "https://untrusted.test" }) };
    assert.equal((await checkout(event)).statusCode, 401);
    user = { email: "owner@example.test", email_verified: false };
    assert.equal((await checkout(event)).statusCode, 401);
    user.email_verified = true; supporting = true;
    assert.equal((await checkout(event)).statusCode, 409);
    assert.equal(calls, 0);
    supporting = false;
    assert.equal((await checkout(event)).statusCode, 200);
    assert.equal(submitted.customer_email, user.email);
    assert.equal(submitted.cancel_url, "https://aesthetic.computer/handle");
    assert.equal(submitted.success_url, "https://aesthetic.computer/handle?support=return");
    assert.deepEqual(submitted.subscription_data.metadata, { source: "signup", ac_support: "monthly" });
    assert.equal(submitted.line_items[0].price_data.unit_amount, 100);
  } finally { if (previous === undefined) delete process.env[key]; else process.env[key] = previous; }
});


test("Native Give returns to AC with the selected amount and accepts no arbitrary return URL", async t => {
  const key = process.env.CONTEXT === "dev" ? "STRIPE_API_TEST_PRIV_KEY" : "STRIPE_API_PRIV_KEY";
  const previous = process.env[key]; process.env[key] = "sk_test_validation_only";
  let submitted;
  t.mock.method(Object.getPrototypeOf(new Stripe(process.env[key]).checkout.sessions), "create", async config => {
    submitted = config; return {id:"cs_test_return",url:"https://pay.aesthetic.computer/test"};
  });
  try {
    for (const recurring of [true,false]) {
      const result = await handler({httpMethod:"POST",headers:{},body:JSON.stringify({amount:800,
        currency:"usd",recurring,source:"homepage",surface:"piece",returnUrl:"https://untrusted.test"})});
      assert.equal(result.statusCode,200);
      const cancel=new URL(submitted.cancel_url), success=new URL(submitted.success_url);
      assert.equal(cancel.origin,"https://aesthetic.computer"); assert.equal(cancel.pathname,"/give");
      assert.equal(cancel.searchParams.get("amount"),"8.00");
      assert.equal(cancel.searchParams.get("frequency"),recurring?"monthly":"once");
      assert.equal(cancel.searchParams.get("source"),"homepage");
      assert.equal(cancel.searchParams.get("thanks"),null); assert.equal(success.searchParams.get("thanks"),"1");
      assert.equal(success.origin,cancel.origin);
    }
  } finally {
    if(previous===undefined)delete process.env[key]; else process.env[key]=previous;
  }
});
