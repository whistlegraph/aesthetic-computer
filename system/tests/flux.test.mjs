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

test("background requests never spend fallback during a provider outage", async () => {
  process.env.NVIDIA_API_KEY = "nvidia-test-key";
  process.env.OPENAI_API_KEY = "openai-test-key";
  const requests = [];
  globalThis.fetch = async (url) => {
    requests.push(url);
    throw new DOMException("timed out", "AbortError");
  };
  const event = {
    httpMethod: "POST",
    body: JSON.stringify({ prompt: "square", allow_fallback: false }),
  };
  assert.equal((await handler(event)).statusCode, 503);
  assert.equal((await handler(event)).statusCode, 503);
  assert.equal(requests.length, 1);
  assert.match(requests[0], /nvidia.com/);
  // Opting out did not reserve any of the ten paid attempts.
  delete process.env.NVIDIA_API_KEY;
  globalThis.fetch = async () =>
    Response.json({ data: [{ b64_json: "jpeg" }] });
  for (let i = 0; i < 10; i++) {
    assert.equal(
      (await handler({ ...event, body: JSON.stringify({ prompt: "square" }) }))
        .statusCode,
      200,
    );
  }
});

test("recovery backs off repeated failures and admits only one probe", async (t) => {
  t.mock.method(Date, "now", () => now);
  let now = 1000000;
  process.env.NVIDIA_API_KEY = "test-key";
  delete process.env.OPENAI_API_KEY;
  const event = {
    httpMethod: "POST",
    body: JSON.stringify({ prompt: "square" }),
  };
  let requests = 0;
  globalThis.fetch = async () => {
    requests++;
    throw new DOMException("timeout", "AbortError");
  };
  assert.equal((await handler(event)).headers["Retry-After"], "60");
  now += 60000;
  let resolveProbe;
  globalThis.fetch = () => {
    requests++;
    return new Promise((resolve) => {
      resolveProbe = resolve;
    });
  };
  const probe = handler(event);
  const duringProbe = await handler({
    ...event,
    body: JSON.stringify({ prompt: "square", allow_fallback: false }),
  });
  assert.equal(duringProbe.statusCode, 503);
  assert.equal(duringProbe.headers["Retry-After"], "30");
  assert.equal(requests, 2);
  resolveProbe(new Response("unavailable", { status: 503 }));
  assert.equal((await probe).headers["Retry-After"], "120");
  now += 120000;
  globalThis.fetch = async () =>
    Response.json({
      artifacts: [{ finishReason: "SUCCESS", base64: "jpeg", seed: 1 }],
    });
  assert.equal((await handler(event)).statusCode, 200);
  globalThis.fetch = async () => {
    throw new DOMException("timeout", "AbortError");
  };
  assert.equal((await handler(event)).headers["Retry-After"], "60");
});

test("paid exhaustion does not hide the earlier NVIDIA recovery probe", async (t) => {
  t.mock.method(Date, "now", () => 1000000);
  process.env.NVIDIA_API_KEY = "test-key";
  process.env.OPENAI_API_KEY = "test-key";
  globalThis.fetch = async (url) => {
    if (url.includes("nvidia.com"))
      throw new DOMException("timeout", "AbortError");
    return Response.json({ data: [{ b64_json: "jpeg" }] });
  };
  const event = {
    httpMethod: "POST",
    body: JSON.stringify({ prompt: "square" }),
  };
  for (let i = 0; i < 10; i++)
    assert.equal((await handler(event)).statusCode, 200);
  const exhausted = await handler(event);
  assert.equal(JSON.parse(exhausted.body).reason, "fallback_budget_exhausted");
  assert.equal(exhausted.headers["Retry-After"], "60");
});

test("NVIDIA timeout includes reading the response body", async (t) => {
  process.env.NVIDIA_API_KEY = "test-key";
  delete process.env.OPENAI_API_KEY;
  const originalSetTimeout = globalThis.setTimeout;
  t.mock.method(globalThis, "setTimeout", (callback, ms) =>
    originalSetTimeout(callback, ms === 30000 ? 5 : ms),
  );
  globalThis.fetch = async (url, { signal }) => ({
    ok: true,
    json: () =>
      new Promise((resolve, reject) => {
        signal.addEventListener(
          "abort",
          () => reject(new DOMException("timeout", "AbortError")),
          { once: true },
        );
      }),
  });
  const result = await handler({
    httpMethod: "POST",
    body: JSON.stringify({ prompt: "square", allow_fallback: false }),
  });
  assert.equal(result.statusCode, 503);
  assert.equal(result.headers["Retry-After"], "60");
});
