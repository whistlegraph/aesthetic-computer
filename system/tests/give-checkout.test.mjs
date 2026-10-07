import test from "node:test";
import assert from "node:assert/strict";
import Stripe from "stripe";
import { handler } from "../netlify/functions/give.js";

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
      for (const source of ["homepage", undefined, "unreviewed-private-text", { homepage: true }]) {
        const result = await handler({ httpMethod: "POST", headers: {},
          body: JSON.stringify({ amount: 800, currency: "usd", recurring, source }) });
        assert.equal(result.statusCode, 200);
        const expected = source === "homepage" ? "homepage" : undefined;
        assert.equal(submitted.metadata.source, expected);
        const record = recurring ? submitted.subscription_data : submitted.payment_intent_data;
        assert.deepEqual(record, expected ? { metadata: { source: expected } } : undefined);
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
