import test from "node:test";
import assert from "node:assert/strict";
import {
  invoiceProductId,
  stripeId,
  subscriptionProductId,
} from "../backend/stripe-product.mjs";

test("reads a product from a subscription item", () => {
  assert.equal(
    subscriptionProductId({
      items: { data: [{ price: { product: "prod_subscription" } }] },
    }),
    "prod_subscription",
  );
});

test("reads current and legacy Stripe invoice-line product shapes", () => {
  assert.equal(
    invoiceProductId({
      lines: {
        data: [
          { pricing: { price_details: { product: "prod_current" } } },
        ],
      },
    }),
    "prod_current",
  );
  assert.equal(
    invoiceProductId({
      lines: { data: [{ price: { product: "prod_legacy" } }] },
    }),
    "prod_legacy",
  );
});

test("normalizes expanded Stripe objects and missing data", () => {
  assert.equal(stripeId({ id: "prod_expanded" }), "prod_expanded");
  assert.equal(subscriptionProductId({ items: { data: [] } }), null);
  assert.equal(invoiceProductId({ lines: { data: [] } }), null);
});
