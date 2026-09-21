import test from "node:test";
import assert from "node:assert/strict";
import { createHandler } from "../netlify/functions/flux.mjs";

const env = {
  CLOUDFLARE_AI_TOKEN: "synthetic-token",
  CLOUDFLARE_ACCOUNT_ID: "synthetic-account",
};
const event = (body = { prompt: "a square", preset: "raw" }) => ({
  httpMethod: "POST",
  body: JSON.stringify(body),
});
const success = () =>
  Response.json({ success: true, result: { image: "jpeg-data" }, errors: [] });
const allowed = async () => ({ allowed: true });

test("Cloudflare uses exactly one reserved four-step generation and preserves the image API", async () => {
  const calls = [],
    order = [];
  const handler = createHandler({
    env,
    now: () => 1000,
    reserveBudget: async () => {
      order.push("reserve");
      return { allowed: true };
    },
    fetch: async (url, options) => {
      order.push("fetch");
      calls.push({ url, options });
      return success();
    },
  });
  const response = await handler(
    event({
      prompt: "a square",
      preset: "raw",
      seed: 9,
      width: 768,
      height: 768,
      allow_fallback: false,
    }),
  );
  assert.deepEqual(order, ["reserve", "fetch"]);
  assert.equal(calls.length, 1);
  assert.equal(
    calls[0].url,
    "https://api.cloudflare.com/client/v4/accounts/synthetic-account/ai/run/@cf/black-forest-labs/flux-1-schnell",
  );
  assert.deepEqual(JSON.parse(calls[0].options.body), {
    prompt: "a square",
    steps: 4,
  });
  assert.equal(
    calls[0].options.headers.Authorization,
    "Bearer synthetic-token",
  );
  assert.equal(response.statusCode, 200);
  assert.deepEqual(JSON.parse(response.body), {
    ok: true,
    png: "data:image/jpeg;base64,jpeg-data",
    width: 1024,
    height: 1024,
    seed: null,
    provider: "cloudflare",
    preset: "raw",
    elapsed_ms: 0,
  });
  assert.equal(response.headers["Cache-Control"], "no-store");
});

test("malformed requests and missing configuration never reserve or call a provider", async () => {
  const unexpected = () => {
    throw new Error("unexpected external operation");
  };
  const handler = createHandler({
    env,
    fetch: unexpected,
    reserveBudget: unexpected,
  });
  for (const body of [
    null,
    [],
    "string",
    42,
    {},
    { prompt: {} },
    { prompt: " " },
    { prompt: "x".repeat(1001) },
  ]) {
    assert.equal((await handler(event(body))).statusCode, 400);
  }
  assert.equal(
    (await handler({ httpMethod: "POST", body: "{" })).statusCode,
    400,
  );
  assert.equal((await handler({ httpMethod: "GET" })).statusCode, 405);
  assert.equal((await handler({ httpMethod: "OPTIONS" })).statusCode, 200);
  const unconfigured = createHandler({
    env: {},
    fetch: unexpected,
    reserveBudget: unexpected,
  });
  assert.equal((await unconfigured(event())).statusCode, 503);
});

test("budget exhaustion and accounting failures fail closed without generation", async () => {
  let fetches = 0;
  const fetch = async () => {
    fetches++;
    return success();
  };
  const exhausted = createHandler({
    env,
    fetch,
    reserveBudget: async () => ({ allowed: false, retryAfterSeconds: 3600 }),
  });
  const response = await exhausted(event());
  assert.equal(response.statusCode, 429);
  assert.equal(response.headers["Retry-After"], "3600");
  assert.equal(JSON.parse(response.body).reason, "image_budget_exhausted");
  const unavailable = createHandler({
    env,
    fetch,
    reserveBudget: async () => {
      throw new Error("synthetic database failure");
    },
  });
  assert.equal((await unavailable(event())).statusCode, 503);
  assert.equal(fetches, 0);
});

test("a failed provider attempt is not retried and cooldown spends no further budget", async () => {
  let reservations = 0,
    fetches = 0;
  const handler = createHandler({
    env,
    now: () => 1000,
    reserveBudget: async () => {
      reservations++;
      return { allowed: true };
    },
    fetch: async () => {
      fetches++;
      throw new DOMException("timeout", "AbortError");
    },
  });
  const first = await handler(event());
  assert.equal(first.statusCode, 503);
  assert.equal(first.headers["Retry-After"], "60");
  assert.equal((await handler(event())).statusCode, 503);
  assert.equal(reservations, 1);
  assert.equal(fetches, 1);
});

test("recovery permits one probe, backs off failures, then resets after success", async () => {
  let now = 1000,
    fetches = 0,
    resolveProbe;
  const handler = createHandler({
    env,
    now: () => now,
    reserveBudget: allowed,
    fetch: async () => {
      fetches++;
      if (fetches === 1 || fetches === 4)
        throw new Error("synthetic network failure");
      if (fetches === 2)
        return new Promise((resolve) => {
          resolveProbe = resolve;
        });
      return success();
    },
  });
  assert.equal((await handler(event())).headers["Retry-After"], "60");
  now += 60000;
  const probe = handler(event());
  await new Promise(setImmediate);
  const waiting = await handler(event());
  assert.equal(waiting.statusCode, 503);
  assert.equal(waiting.headers["Retry-After"], "30");
  assert.equal(fetches, 2);
  resolveProbe(Response.json({ success: false }, { status: 503 }));
  assert.equal((await probe).headers["Retry-After"], "120");
  now += 120000;
  assert.equal((await handler(event())).statusCode, 200);
  assert.equal((await handler(event())).headers["Retry-After"], "60");
});

test("in-flight limit rejects excess work before reserving its budget", async () => {
  let reservations = 0;
  const resolveCalls = [];
  const handler = createHandler({
    env,
    reserveBudget: async () => {
      reservations++;
      return { allowed: true };
    },
    fetch: () => new Promise((resolve) => resolveCalls.push(resolve)),
  });
  const first = handler(event()),
    second = handler(event());
  await new Promise(setImmediate);
  const busy = await handler(event());
  assert.equal(busy.statusCode, 429);
  assert.equal(JSON.parse(busy.body).reason, "busy");
  assert.equal(reservations, 2);
  for (const resolve of resolveCalls) resolve(success());
  await Promise.all([first, second]);
});

test("provider Retry-After survives a non-JSON error response", async () => {
  const handler = createHandler({
    env,
    reserveBudget: allowed,
    now: () => 1000,
    fetch: async () =>
      new Response("unavailable", {
        status: 429,
        headers: { "Retry-After": "300" },
      }),
  });
  const response = await handler(event());
  assert.equal(response.statusCode, 503);
  assert.equal(response.headers["Retry-After"], "300");
});

test("timeout includes stalled provider response-body decoding", async (t) => {
  const originalSetTimeout = globalThis.setTimeout;
  t.mock.method(globalThis, "setTimeout", (callback, ms) =>
    originalSetTimeout(callback, ms === 30000 ? 5 : ms),
  );
  const handler = createHandler({
    env,
    reserveBudget: allowed,
    fetch: async (url, { signal }) => ({
      ok: true,
      json: () =>
        new Promise((resolve, reject) =>
          signal.addEventListener(
            "abort",
            () => reject(new DOMException("timeout", "AbortError")),
            { once: true },
          ),
        ),
    }),
  });
  assert.equal((await handler(event())).statusCode, 503);
});

test("an HTTP 200 without a successful image is not reported as generated", async () => {
  const handler = createHandler({
    env,
    reserveBudget: allowed,
    fetch: async () => Response.json({ success: true, result: {} }),
  });
  assert.equal((await handler(event())).statusCode, 503);
});

test("Cloudflare free-quota exhaustion waits for midnight without more reservations", async () => {
  let now = Date.parse("2026-09-21T23:55:00Z"),
    reservations = 0,
    fetches = 0;
  const handler = createHandler({
    env,
    now: () => now,
    reserveBudget: async () => {
      reservations++;
      return { allowed: true };
    },
    fetch: async () => {
      fetches++;
      return fetches === 1
        ? Response.json(
            {
              success: false,
              errors: [{ code: 3036, message: "daily allowance exhausted" }],
            },
            { status: 429 },
          )
        : success();
    },
  });
  const exhausted = await handler(event());
  assert.equal(exhausted.statusCode, 429);
  assert.equal(exhausted.headers["Retry-After"], "300");
  assert.equal(JSON.parse(exhausted.body).reason, "image_budget_exhausted");
  now += 120000;
  assert.equal((await handler(event())).headers["Retry-After"], "180");
  assert.equal(reservations, 1);
  assert.equal(fetches, 1);
  now += 180000;
  assert.equal((await handler(event())).statusCode, 200);
  assert.equal(reservations, 2);
});
