import assert from "node:assert/strict";
import { describe, it } from "node:test";
import { CASSETTE_SPECS, CD_SPECS, orderUrl, parseResponse, shippingOptionsUrl } from "../marketing/podcast/lib/kunaki.mjs";

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
  it("tracks current jewel-case CD limits and artwork dimensions", () => {
    assert.equal(CD_SPECS.priceUsd, 2);
    assert.equal(CD_SPECS.audio.maxMinutes, 80);
    assert.equal(CD_SPECS.audio.maxTracks, 25);
    assert.deepEqual(CD_SPECS.artwork.disc, { width: 1394, height: 1394 });
    assert.deepEqual(CD_SPECS.artwork.frontCover, { width: 1423, height: 1411 });
    assert.deepEqual(CD_SPECS.artwork.trayCard, { width: 1772, height: 1385, spineWidth: 74 });
  });
});
