import test from "node:test";
import assert from "node:assert/strict";
import {
  handler,
  resetFluxOutageCircuit,
} from "../netlify/functions/flux.mjs";

const originalFetch = globalThis.fetch;
const originalKey = process.env.NVIDIA_API_KEY;

test.afterEach(() => {
  globalThis.fetch = originalFetch;
  if (originalKey === undefined) delete process.env.NVIDIA_API_KEY;
  else process.env.NVIDIA_API_KEY = originalKey;
  resetFluxOutageCircuit();
});

test("opens a short outage circuit after an upstream timeout", async () => {
  process.env.NVIDIA_API_KEY = "test-key";
  let requests = 0;
  globalThis.fetch = async () => {
    requests += 1;
    throw new DOMException("timed out", "AbortError");
  };

  const event = {
    httpMethod: "POST",
    body: JSON.stringify({ prompt: "a square", preset: "raw" }),
  };
  const first = await handler(event);
  const second = await handler(event);

  assert.equal(first.statusCode, 503);
  assert.equal(first.headers["Retry-After"], "60");
  assert.equal(JSON.parse(first.body).reason, "temporarily_unavailable");
  assert.equal(second.statusCode, 503);
  assert.equal(requests, 1);
});

test("client errors remain visible without opening the outage circuit", async () => {
  process.env.NVIDIA_API_KEY = "test-key";
  let requests = 0;
  globalThis.fetch = async () => {
    requests += 1;
    return new Response("bad request", { status: 422 });
  };

  const event = {
    httpMethod: "POST",
    body: JSON.stringify({ prompt: "a square", preset: "raw" }),
  };
  const first = await handler(event);
  const second = await handler(event);

  assert.equal(first.statusCode, 502);
  assert.equal(JSON.parse(first.body).status, 422);
  assert.equal(second.statusCode, 502);
  assert.equal(requests, 2);
});
