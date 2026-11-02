import test from "node:test";
import assert from "node:assert/strict";
import { inferenceProviderRequest, requestInference } from "../backend/easel-provider.mjs";
import { drawSketch } from "../backend/aesel-sketch.mjs";

const body = { system: "Draw", messages: [{ role: "user", content: "otter" }], tools: [{ name: "draw" }] };
test("hosted Aesel retains direct Anthropic routing, thinking and cancellation", () => {
  const controller = new AbortController();
  const request = inferenceProviderRequest({ model: "anthropic/claude-sonnet-5.5", maxTokens: 8192,
    body, credits: "test-credits", key: "test-router", thinking: { type: "disabled" }, signal: controller.signal });
  assert.equal(request.url, "https://api.anthropic.com/v1/messages");
  assert.equal(request.init.signal, controller.signal);
  assert.deepEqual(JSON.parse(request.init.body), { ...body, model: "claude-sonnet-5-5", max_tokens: 8192,
    thinking: { type: "between_tools" }, stream: true });
  assert.equal(request.init.headers["x-api-key"], "test-credits");
  assert.equal(request.init.headers.Authorization, undefined);
  assert.equal(request.init.headers["anthropic-beta"], undefined);
});

test("router requests preserve Aesel attribution and never inherit direct-only options", async () => {
  let request;
  const response = await requestInference({ model: "deepseek/deepseek-v4.1-flash", maxTokens: 1000,
    body: { ...body, model: "untrusted", fallbacks: "untrusted" }, credits: "test-credits", key: "test-router",
    reasoning: { effort: "none" }, serverFallbacks: true }, async (url, init) => {
    request = { url, init }; return new Response("ok");
  });
  assert.equal(await response.text(), "ok");
  assert.equal(request.url, "https://openrouter.ai/api/v1/messages");
  assert.equal(request.init.headers["X-Title"], "Easel");
  assert.equal(request.init.headers.Authorization, "Bearer test-router");
  assert.equal(request.init.headers["x-api-key"], undefined);
  const sent = JSON.parse(request.init.body);
  assert.equal(sent.model, "deepseek/deepseek-v4.1-flash");
  assert.equal(sent.fallbacks, undefined);
  assert.deepEqual(sent.reasoning, { effort: "none" });
});

test("unsupported models and missing credentials cannot reach a provider", () => {
  assert.throws(() => inferenceProviderRequest({ model: "unlisted", key: "test" }), /Unsupported/);
  assert.throws(() => inferenceProviderRequest({ model: "anthropic/claude-opus-5.5" }), /not configured/);
});

const delta = text => ({ type: "content_block_delta", delta: { type: "text_delta", text } });
const end = reason => ({ type: "message_delta", delta: { stop_reason: reason } });
function response(events) {
  const bytes = new TextEncoder().encode(events.map(e => `data: ${JSON.stringify(e)}\n\n`).join(""));
  return new Response(new ReadableStream({ start(c) {
    for (let i = 0; i < bytes.length; i += 7) c.enqueue(bytes.slice(i, i + 7));
    c.close();
  } }));
}

test("sketch uses the Aesel provider with its fixed server-funded profile and streams code", async () => {
  let sent, headers, url;
  const chunks = [];
  const result = await drawSketch("otter", t => chunks.push(t), { env: { ANTHROPIC_CREDITS_KEY: "test" },
    fetchImpl: async (u, init) => {
      url = u; sent = JSON.parse(init.body); headers = init.headers;
      return response([delta("```lisp\n(wipe "), delta("blue)\n```"), end("end_turn")]);
    } });
  assert.equal(url, "https://api.anthropic.com/v1/messages");
  assert.equal(sent.model, "claude-opus-5-5");
  assert.equal(sent.max_tokens, 4000);
  assert.deepEqual(sent.output_config, { effort: "low" });
  assert.equal(sent.fallbacks, "default");
  assert.equal(headers["anthropic-beta"], "server-side-fallback-2026-07-01");
  assert.deepEqual(sent.messages, [{ role: "user", content: "otter" }]);
  assert.equal(sent.tools, undefined);
  assert.deepEqual(result, { program: "(wipe blue)" });
  assert.equal(chunks.join(""), "```lisp\n(wipe blue)\n```");
});

test("sketch refusals, skips and provider failures do not become generated programs", async () => {
  const env = { ANTHROPIC_API_KEY: "test" };
  for (const events of [[delta("SKIP"), end("end_turn")], [delta("no"), end("refusal")]]) {
    assert.deepEqual(await drawSketch("why", undefined, { env, fetchImpl: async () => response(events) }), { skip: true });
  }
  await assert.rejects(drawSketch("otter", undefined, { env, fetchImpl: async () => new Response("busy", { status: 503 }) }), /anthropic 503/);
  await assert.rejects(drawSketch("otter", undefined, { env, fetchImpl: async () => response([{ type: "error", error: { message: "failed" } }]) }), /anthropic stream/);
  await assert.rejects(drawSketch("otter", undefined, { env: {} }), /no anthropic key/);
});
