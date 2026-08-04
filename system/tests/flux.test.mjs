import test from "node:test";
import assert from "node:assert/strict";
import {
  handler,
  resetFluxFallbackBudget,
  resetFluxOutageCircuit,
} from "../netlify/functions/flux.mjs";

const originalFetch = globalThis.fetch;
const originalKey = process.env.NVIDIA_API_KEY;
const originalOpenAIKey = process.env.OPENAI_API_KEY;

test.afterEach(() => {
  globalThis.fetch = originalFetch;
  if (originalKey === undefined) delete process.env.NVIDIA_API_KEY;
  else process.env.NVIDIA_API_KEY = originalKey;
  if (originalOpenAIKey === undefined) delete process.env.OPENAI_API_KEY;
  else process.env.OPENAI_API_KEY = originalOpenAIKey;
  resetFluxFallbackBudget();
  resetFluxOutageCircuit();
});

test("opens a short outage circuit after an upstream timeout", async () => {
  process.env.NVIDIA_API_KEY = "test-key";
  delete process.env.OPENAI_API_KEY;
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

test("falls back to bounded low-quality GPT Image after NVIDIA times out", async () => {
  process.env.NVIDIA_API_KEY = "nvidia-test-key";
  process.env.OPENAI_API_KEY = "openai-test-key";
  const requests = [];
  globalThis.fetch = async (url, options) => {
    requests.push({ url, options });
    if (url.includes("nvidia.com")) {
      throw new DOMException("timed out", "AbortError");
    }
    return Response.json({ data: [{ b64_json: "jpeg-data" }] });
  };

  const event = {
    httpMethod: "POST",
    body: JSON.stringify({ prompt: "a square", preset: "raw" }),
  };
  const first = await handler(event);
  const second = await handler(event);
  const firstBody = JSON.parse(first.body);
  const openAIRequest = JSON.parse(requests[1].options.body);

  assert.equal(first.statusCode, 200);
  assert.equal(firstBody.provider, "openai");
  assert.equal(firstBody.png, "data:image/jpeg;base64,jpeg-data");
  assert.equal(firstBody.seed, null);
  assert.deepEqual(openAIRequest, {
    model: "gpt-image-1-mini",
    prompt: "a square",
    n: 1,
    size: "1024x1024",
    quality: "low",
    output_format: "jpeg",
    moderation: "auto",
  });
  assert.equal(second.statusCode, 200);
  assert.equal(requests.length, 3);
  assert.match(requests[2].url, /api\.openai\.com/);
});

test("caps paid fallback generation at ten requests per process-hour", async () => {
  delete process.env.NVIDIA_API_KEY;
  process.env.OPENAI_API_KEY = "openai-test-key";
  let requests = 0;
  globalThis.fetch = async () => {
    requests += 1;
    return Response.json({ data: [{ b64_json: "jpeg-data" }] });
  };

  const event = {
    httpMethod: "POST",
    body: JSON.stringify({ prompt: "a square", preset: "raw" }),
  };
  const responses = [];
  for (let i = 0; i < 11; i += 1) responses.push(await handler(event));

  assert.equal(requests, 10);
  assert.equal(responses[9].statusCode, 200);
  assert.equal(responses[10].statusCode, 503);
  assert.equal(
    JSON.parse(responses[10].body).reason,
    "fallback_budget_exhausted",
  );
});

test("client errors remain visible without opening the outage circuit", async () => {
  process.env.NVIDIA_API_KEY = "test-key";
  delete process.env.OPENAI_API_KEY;
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
