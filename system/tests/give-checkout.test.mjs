import test from "node:test";
import assert from "node:assert/strict";
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
