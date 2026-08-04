import test from "node:test";
import assert from "node:assert/strict";
import { handler } from "../netlify/functions/index.mjs";

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
