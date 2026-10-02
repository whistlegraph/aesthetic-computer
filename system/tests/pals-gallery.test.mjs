import test from "node:test";
import assert from "node:assert/strict";
import { handler } from "../netlify/functions/logo.js";
import { drippedPals } from "../backend/pals-gallery.mjs";
import { logoUrl, turnaroundUrl } from "../backend/logo.mjs";

const request = (path, query = {}) => handler({ httpMethod: "GET", path: `/api/logo/${path}`, queryStringParameters: query, headers: { accept: "text/html" } });

test("the collection exposes both stills and every published motion format", async () => {
  const catalogue = JSON.parse((await request("pals.json")).body);
  assert.equal(catalogue.collections[0].url, "https://pals.aesthetic.computer/dripped");
  for (const { slug } of drippedPals) {
    assert.ok(catalogue.stills.find((item) => item.slug === slug));
    assert.ok(catalogue.turnarounds.find((item) => item.slug === slug));
    for (const format of ["png", "mp4", "webp", "apng"]) {
      const response = await request(`pals-${slug}.${format}`);
      assert.equal(response.statusCode, 302);
      assert.equal(response.headers.Location, format === "png" ? logoUrl(slug) : turnaroundUrl(slug, format));
    }
  }
});

test("the gallery offers downloads and explicit motion controls", async () => {
  const response = await request("dripped");
  assert.equal(response.statusCode, 200);
  assert.match(response.headers["Content-Type"], /text\/html/);
  assert.equal((response.body.match(/<video /g) || []).length, 2);
  assert.equal((response.body.match(/\?download=1/g) || []).length, 8);
  assert.match(response.body, /prefers-reduced-motion/);
  assert.match(response.body, /aria-label="Play Pink sky"/);
  assert.doesNotMatch(response.body, /(?:src|poster)="null"/);
});

test("existing embeds and random browser entry remain available", async () => {
  const still = await request("pals-chrome.png");
  assert.equal(still.headers.Location, logoUrl("chrome"));
  const animation = await request("pals-chrome.mp4");
  assert.equal(animation.headers.Location, turnaroundUrl("chrome", "mp4"));
  const random = await request("random.json");
  assert.ok(JSON.parse(random.body).url.startsWith("https://"));
  const home = await request("");
  assert.match(home.body, /previousLogo/);
  assert.match(home.body, /href="\/dripped"/);
});

test("unknown named assets remain 404", async () => {
  for (const extension of ["png", "mp4", "webp", "apng"])
    assert.equal((await request(`pals-not-a-real-pal.${extension}`)).statusCode, 404);
});
