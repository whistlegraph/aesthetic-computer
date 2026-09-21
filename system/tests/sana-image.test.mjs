import test from "node:test";
import assert from "node:assert/strict";
import { generateSana } from "../backend/sana-image.mjs";

const jpeg = "data:image/jpeg;base64,/9j/2Q==";
const result = () => ({ images: [{ url: jpeg, width: 768, height: 768 }], seed: 7 });
const request = { prompt: "synthetic fabric", key: "synthetic-key" };

test("Sana submits one bounded image with inline output and the caller deadline", async () => {
  const signal = new AbortController().signal;
  let calls = 0;
  const image = await generateSana({ ...request, signal, fetchImpl: async (url, options) => {
    calls++;
    assert.equal(url, "https://fal.run/fal-ai/sana");
    assert.equal(options.redirect, "error");
    assert.equal(options.signal, signal);
    assert.equal(options.headers.Authorization, "Key synthetic-key");
    const body = JSON.parse(options.body);
    assert.deepEqual(body.image_size, { width: 768, height: 768 });
    assert.equal(body.num_images, 1);
    assert.equal(body.num_inference_steps, 18);
    assert.equal(body.sync_mode, true);
    assert.equal(body.enable_safety_checker, true);
    return Response.json(result());
  } });
  assert.equal(calls, 1);
  assert.deepEqual(image, { png: jpeg, width: 768, height: 768, seed: 7 });
});

test("HTTP rejection and ambiguous failures never submit again", async () => {
  for (const fail of [() => new Response("quota", { status: 429, headers: { "Retry-After": "30" } }), () => { throw new Error("network failed"); }]) {
    let calls = 0;
    await assert.rejects(generateSana({ ...request, fetchImpl: async () => { calls++; return fail(); } }));
    assert.equal(calls, 1);
  }
});

test("external image URLs, wrong sizes, malformed bytes and filtered images are rejected", async () => {
  const invalid = [null, {}, { ...result(), has_nsfw_concepts: [true] },
    { images: [{ url: "https://example.com/image", width: 768, height: 768 }] },
    { images: [{ url: jpeg, width: 2048, height: 2048 }] },
    { images: [{ url: "data:image/jpeg;base64,aGVsbG8=", width: 768, height: 768 }] }];
  for (const data of invalid) {
    let calls = 0;
    await assert.rejects(generateSana({ ...request, fetchImpl: async () => { calls++; return Response.json(data); } }));
    assert.equal(calls, 1);
  }
});

test("a stalled response body retains the request's abort signal", async () => {
  const controller = new AbortController();
  const pending = generateSana({ ...request, signal: controller.signal, fetchImpl: async (_url, options) => ({
    ok: true,
    json: () => new Promise((_resolve, reject) => options.signal.addEventListener("abort", () => reject(options.signal.reason), { once: true })),
  }) });
  await new Promise(resolve => setImmediate(resolve));
  controller.abort(new Error("deadline"));
  await assert.rejects(pending, /deadline/);
});
