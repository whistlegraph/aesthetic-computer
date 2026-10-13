import test from "node:test";
import assert from "node:assert/strict";
import { handler } from "../netlify/functions/index.mjs";

test("direct handle URLs return a DOM profile without booting the canvas", async () => {
  const response = await handler({
    path: "/%40jeffrey", headers: { host: "aesthetic.computer" }, queryStringParameters: {},
  });
  assert.equal(response.statusCode, 200);
  assert.match(response.body, /<ac-profile handle="@jeffrey">/);
  assert.ok(!response.body.includes("bios.mjs") && !response.body.includes("boot.mjs"));
});

test("favicon.ico redirects to the platform SVG", async () => {
  const response = await handler({
    path: "/favicon.ico",
    headers: {},
    queryStringParameters: {},
  });

  assert.equal(response.statusCode, 302);
  assert.equal(response.headers.Location, "/purple-pals.svg");
});

test("missing requestProvider source map is a 404", async () => {
  const response = await handler({
    path: "/requestProvider.js.map",
    headers: {},
    queryStringParameters: {},
  });

  assert.equal(response.statusCode, 404);
});
