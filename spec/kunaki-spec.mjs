import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { CASSETTE_SPECS, orderUrl, parseResponse, shippingOptionsUrl } from "../marketing/podcast/lib/kunaki.mjs";

describe("Kunaki integration", () => {
  it("encodes duplicate product fields in shipping quotes", () => {
    const url = new URL(shippingOptionsUrl({ country: "United States", stateProvince: "CA", postalCode: "90012", items: [{ productId: "PX0012345", quantity: 2 }] }));
    assert.equal(url.searchParams.get("RequestType"), "ShippingOptions");
    assert.deepEqual(url.searchParams.getAll("ProductId"), ["PX0012345"]);
  });
  it("parses shipping XML", () => {
    const parsed = parseResponse("<Response><ErrorCode>0</ErrorCode><ErrorText>success</ErrorText><Option><Description>USPS</Description><DeliveryTime>2-5 days</DeliveryTime><Price>5.25</Price></Option></Response>");
    assert.deepEqual(parsed.options, [{ description: "USPS", deliveryTime: "2-5 days", priceUsd: 5.25 }]);
  });
  it("guards live orders", () => {
    assert.throws(() => orderUrl({ mode: "Live", credentials: { userId: "x", password: "y" }, recipient: {}, shippingDescription: "USPS", items: [{ productId: "PX0012345" }] }), /KUNAKI_ALLOW_LIVE/);
  });
  it("tracks current cassette artwork dimensions", () => {
    assert.deepEqual(CASSETTE_SPECS.artwork.jCard, { width: 1200, height: 1110 });
    assert.deepEqual(CASSETTE_SPECS.artwork.labelA, { width: 1062, height: 496 });
  });
});
