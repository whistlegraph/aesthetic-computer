import test from "node:test";
import assert from "node:assert/strict";
import { printListing, recentPrints, PRINT_KINDS } from "../shop-prints.mjs";

// A stand-in for the two Mongo collections the listing reads.
const fakeDb = (rows) => ({
  collection: (name) => ({
    find(query) {
      let out = rows[name] || [];
      if (name === "products") out = out.filter((p) => query.product.$in.includes(p.product) && typeof p.preview === "string" && p.preview);
      if (name === "paintings") out = out.filter((p) => query.$or[0].slug.$in.includes(p.slug) || query.$or[1].code.$in.includes(p.code));
      const cursor = { sort: () => (out = [...out].sort((a, b) => b.createdAt - a.createdAt), cursor), limit: (n) => (out = out.slice(0, n), cursor), toArray: async () => out };
      return cursor;
    },
  }),
});
const mug = (code, extra = {}) => ({ code, product: "mug", variant: "white", preview: `https://art.aesthetic.computer/products/${code}.webp`,
  source: { type: "painting", code: "oay", user: "auth0|1" }, createdAt: new Date("2026-11-11T16:39:12Z"), ...extra });

test("a print reads like a shop product and points at its own + page", () => {
  const listing = printListing(mug("upteau"), { handle: "@jeffrey" });
  assert.equal(listing.title, "mug of #oay");
  assert.equal(listing.vendor, "@jeffrey");
  assert.equal(listing.price, "$18 USD");
  assert.equal(listing.priceRaw, PRINT_KINDS.mug.price);
  assert.equal(listing.shopUrl, "https://aesthetic.computer/+upteau");
  assert.equal(listing.jump, "+upteau");
  assert.equal(listing.source, "print");
  assert.equal(listing.available, true);
  assert.deepEqual(listing.images, ["https://art.aesthetic.computer/products/upteau.webp"]);
  assert.deepEqual(listing.tags, ["print", "mug", "white"]);
  assert.equal(listing.createdAt, "2026-11-11T16:39:12.000Z");
});

test("the title names the colour, the painting's short code and the KidLisp it came by", () => {
  const product = mug("sdfxyi", { variant: "blue", source: { type: "painting", code: "2026.11.10.15.57.39", via: "39i" } });
  assert.equal(printListing(product, { codes: { "2026.11.10.15.57.39": "rlt" } }).title, "blue mug of #rlt in $39i");
  assert.equal(printListing(mug("abc123", { source: { type: "painting", slug: "long-slug", code: "xyz" } })).title, "mug of #xyz");
  assert.equal(printListing(mug("abc123")).vendor, "aesthetic.computer", "no handle, no invented maker");
});

test("what is not a known print, or has no usable code, is left out", () => {
  assert.equal(printListing({ code: "abc123", product: "poster" }), null);
  assert.equal(printListing(mug("../etc")), null);
  assert.equal(printListing(null), null);
});

test("the recent list is newest first, skips prints with no picture, and survives a failed handle lookup", async () => {
  const db = fakeDb({
    products: [
      mug("older1", { createdAt: new Date("2026-11-01") }),
      mug("newest", { createdAt: new Date("2026-11-12"), source: { type: "painting", code: "slug-a", user: "auth0|2" } }),
      mug("nopic1", { preview: "", createdAt: new Date("2026-11-13") }),
      { code: "stick1", product: "sticker", preview: "x", createdAt: new Date("2026-11-14") },
    ],
    paintings: [{ slug: "slug-a", code: "aaa" }],
  });
  const handleFor = async (user) => { if (user === "auth0|1") throw new Error("auth0 is down"); return "fifi"; };
  const prints = await recentPrints(db, { handleFor });
  assert.deepEqual(prints.map((p) => p.printCode), ["+newest", "+older1"]);
  assert.equal(prints[0].title, "mug of #aaa");
  assert.equal(prints[0].vendor, "@fifi");
  assert.equal(prints[1].vendor, "aesthetic.computer");
  assert.equal((await recentPrints(db, { limit: 1 })).length, 1);
});
