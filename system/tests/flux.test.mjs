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

const quota = () =>
  Response.json({ success: false, errors: [{ code: 3036 }] }, { status: 429 });
const sanaImage = () => ({
  png: "data:image/jpeg;base64,sana-image",
  width: 768,
  height: 768,
  seed: 17,
});

test("wrapped AiError daily quota selects Sana only for subsequent budgeted requests", async () => {
  for (const key of [undefined, "synthetic-fal"]) {
    const providers = [];
    const handler = createHandler({
      env: { ...env, IMAGE_FAL_KEY: key },
      now: () => 1000,
      reserveBudget: async ({ provider }) => {
        providers.push(provider);
        return { allowed: true };
      },
      fetch: async () => Response.json({ errors: [{
        code: 4006,
        message: "AiError: AiError: you have used up your daily free allocation of 10,000 neurons, please upgrade to Cloudflare's Workers Paid plan if you would like to continue usage.",
      }] }, { status: 429 }),
      generateSana: async () => sanaImage(),
    });
    assert.equal((await handler(event())).statusCode, 429);
    assert.deepEqual(providers, ["cloudflare"]);
    const next = await handler(event());
    assert.equal(next.statusCode, key ? 200 : 429);
    assert.deepEqual(providers, key ? ["cloudflare", "fal-sana"] : ["cloudflare"]);
  }
});

test("confirmed Cloudflare quota routes only a subsequent request to separately reserved Sana", async () => {
  const reservations = [],
    calls = [];
  const handler = createHandler({
    env: { ...env, IMAGE_FAL_KEY: "synthetic-fal" },
    reserveBudget: async (request) => {
      reservations.push(request);
      return { allowed: true };
    },
    fetch: async () => {
      calls.push("cloudflare");
      return quota();
    },
    generateSana: async ({ prompt, key, signal, fetchImpl }) => {
      assert.equal(prompt, "a square");
      assert.equal(key, "synthetic-fal");
      assert.equal(signal.aborted, false);
      assert.equal(typeof fetchImpl, "function");
      calls.push("fal-sana");
      return sanaImage();
    },
  });
  const first = await handler(
    event({ prompt: "a square", preset: "raw", provider: "fal-sana" }),
  );
  assert.equal(first.statusCode, 429);
  assert.equal(first.headers["Retry-After"], "1");
  assert.equal(JSON.parse(first.body).reason, "provider_quota");
  assert.deepEqual(
    calls,
    ["cloudflare"],
    "caller cannot select provider or trigger same-request failover",
  );
  assert.deepEqual(reservations, [{ provider: "cloudflare" }]);
  const second = await handler(event());
  assert.equal(second.statusCode, 200);
  const image = JSON.parse(second.body);
  assert.equal(image.provider, "fal-sana");
  assert.equal(image.width, 768);
  assert.equal(image.height, 768);
  assert.equal(image.seed, 17);
  assert.deepEqual(calls, ["cloudflare", "fal-sana"]);
  assert.deepEqual(reservations, [
    { provider: "cloudflare" },
    { provider: "fal-sana" },
  ]);
});

test("network errors and capacity429 never select Sana even with its key configured", async () => {
  for (const failure of [
    () => Response.json({ errors: [{ code: 4006, message: "AiError: model unavailable" }] }, { status: 429 }),
    () => {
      throw new DOMException("timeout", "AbortError");
    },
    () =>
      Response.json(
        { success: false, errors: [{ code: 3040 }] },
        { status: 429 },
      ),
  ]) {
    let sanaCalls = 0,
      reservations = 0;
    const handler = createHandler({
      env: { ...env, IMAGE_FAL_KEY: "synthetic-fal" },
      now: () => 1000,
      reserveBudget: async ({ provider }) => {
        assert.equal(provider, "cloudflare");
        reservations++;
        return { allowed: true };
      },
      fetch: failure,
      generateSana: async () => {
        sanaCalls++;
        return sanaImage();
      },
    });
    assert.equal((await handler(event())).statusCode, 503);
    assert.equal((await handler(event())).statusCode, 503);
    assert.equal(sanaCalls, 0);
    assert.equal(reservations, 1);
  }
});

test("shared budget denial prevents Sana and does not try another provider", async () => {
  const providers = [];
  let cloudflareCalls = 0,
    sanaCalls = 0;
  const handler = createHandler({
    env: { ...env, IMAGE_FAL_KEY: "synthetic-fal" },
    reserveBudget: async ({ provider }) => {
      providers.push(provider);
      return provider === "cloudflare"
        ? { allowed: true }
        : { allowed: false, retryAfterSeconds: 86400 };
    },
    fetch: async () => {
      cloudflareCalls++;
      return quota();
    },
    generateSana: async () => {
      sanaCalls++;
      return sanaImage();
    },
  });
  await handler(event());
  const denied = await handler(event());
  assert.equal(denied.statusCode, 429);
  assert.equal(denied.headers["Retry-After"], "86400");
  assert.equal(JSON.parse(denied.body).reason, "image_budget_exhausted");
  assert.deepEqual(providers, ["cloudflare", "fal-sana"]);
  assert.equal(sanaCalls, 0);
  assert.equal(cloudflareCalls, 1);
});

test("Sana failure has its own circuit and does not block Cloudflare at midnight", async () => {
  let now = Date.parse("2026-09-21T23:59:55Z"),
    sanaCalls = 0,
    cloudflareCalls = 0;
  const providers = [];
  const handler = createHandler({
    env: { ...env, IMAGE_FAL_KEY: "synthetic-fal" },
    now: () => now,
    reserveBudget: async ({ provider }) => {
      providers.push(provider);
      return { allowed: true };
    },
    fetch: async () => (++cloudflareCalls === 1 ? quota() : success()),
    generateSana: async () => {
      sanaCalls++;
      throw new DOMException("timeout", "AbortError");
    },
  });
  await handler(event());
  assert.equal((await handler(event())).headers["Retry-After"], "60");
  assert.equal((await handler(event())).statusCode, 503);
  assert.equal(
    sanaCalls,
    1,
    "ambiguous Sana failure is not retried or switched",
  );
  now += 5000;
  const reset = await handler(event());
  assert.equal(reset.statusCode, 200);
  assert.equal(JSON.parse(reset.body).provider, "cloudflare");
  assert.deepEqual(providers, ["cloudflare", "fal-sana", "cloudflare"]);
});

test("Sana filtering is explicit and does not open an availability circuit", async () => {
  let calls = 0;
  const handler = createHandler({
    env: { ...env, IMAGE_FAL_KEY: "synthetic-fal" },
    reserveBudget: allowed,
    fetch: async () => quota(),
    generateSana: async () => {
      calls++;
      if (calls === 1)
        throw Object.assign(new Error("synthetic filter"), {
          reason: "filtered",
        });
      return sanaImage();
    },
  });
  await handler(event());
  const filtered = await handler(event());
  assert.equal(filtered.statusCode, 422);
  assert.equal(JSON.parse(filtered.body).reason, "filtered");
  assert.equal((await handler(event())).statusCode, 200);
  assert.equal(calls, 2);
});

test("in-flight bound also applies to Sana before additional reservations", async () => {
  let reservations = 0;
  const pending = [];
  const handler = createHandler({
    env: { ...env, IMAGE_FAL_KEY: "synthetic-fal" },
    reserveBudget: async () => {
      reservations++;
      return { allowed: true };
    },
    fetch: async () => quota(),
    generateSana: () => new Promise((resolve) => pending.push(resolve)),
  });
  await handler(event());
  const first = handler(event()),
    second = handler(event());
  await new Promise(setImmediate);
  const busy = await handler(event());
  assert.equal(busy.statusCode, 429);
  assert.equal(JSON.parse(busy.body).reason, "busy");
  assert.equal(reservations, 3, "quota request plus two Sana reservations");
  for (const resolve of pending) resolve(sanaImage());
  await Promise.all([first, second]);
});

test("unrelated FAL_KEY cannot enable image overflow", async () => {
  let sanaCalls = 0,
    reservations = 0;
  const handler = createHandler({
    env: { ...env, FAL_KEY: "unrelated-easel-key" },
    now: () => Date.parse("2026-09-21T23:55:00Z"),
    reserveBudget: async () => {
      reservations++;
      return { allowed: true };
    },
    fetch: async () => quota(),
    generateSana: async () => {
      sanaCalls++;
      return sanaImage();
    },
  });
  const first = await handler(event());
  assert.equal(first.headers["Retry-After"], "300");
  assert.equal(JSON.parse(first.body).reason, "image_budget_exhausted");
  assert.equal((await handler(event())).statusCode, 429);
  assert.equal(sanaCalls, 0);
  assert.equal(reservations, 1);
});
