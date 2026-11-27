import test from "node:test";
import assert from "node:assert/strict";
import { inferenceProviderRequest, requestHostedInference, requestInference } from "../backend/easel-provider.mjs";
import { drawSketch } from "../backend/aesel-sketch.mjs";

const body = { system: "Draw", messages: [{ role: "user", content: "otter" }], tools: [{ name: "draw" }] };
test("direct credit refusal retries Sonnet on OpenRouter and identifies the billing source", async () => {
  const model = "anthropic/claude-sonnet-5.5";
  const requests = [];
  const { response, provider } = await requestHostedInference({ model, body, credits: "direct", key: "router", maxTokens: 50 }, async (url, init) => {
    requests.push({ url, init });
    return requests.length === 1
      ? Response.json({ error: { message: "Your credit balance is too low to access the Anthropic API." } }, { status: 400 })
      : new Response("stream");
  });
  assert.equal(await response.text(), "stream");
  assert.equal(provider, "openrouter");
  assert.equal(requests.length, 2);
  assert.equal(requests[1].url, "https://openrouter.ai/api/v1/messages");
  assert.equal(JSON.parse(requests[1].init.body).model, model);
  assert.deepEqual(JSON.parse(requests[1].init.body).tools, body.tools);
  assert.equal(requests[1].init.headers["x-api-key"], undefined);
});

test("bad input and in-stream errors do not replay a request", async () => {
  for (const status of [400, 200]) {
    let calls = 0;
    const { response, provider } = await requestHostedInference({ model: "anthropic/claude-sonnet-5.5", body, credits: "direct", key: "router" }, () => {
      calls++;
      return status === 400 ? Response.json({ error: { message: "Invalid input" } }, { status }) : new Response("event: error");
    });
    assert.equal(calls, 1);
    assert.equal(provider, "anthropic");
    assert.equal(response.status, status);
    assert.ok(await response.text());
  }
});
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
  await assert.rejects(drawSketch("otter", undefined, { env: {} }), /no inference key/);
});

test("a Claude conversation is marked for the prompt cache at its end and at the user turn before it", async () => {
  const { cacheConversation } = await import("../backend/easel-provider.mjs");
  const mark = { type: "ephemeral" };
  const turns = { system: [{ type: "text", text: "guides", cache_control: mark }], messages: [
    { role: "user", content: "draw an otter" },
    { role: "assistant", content: [{ type: "text", text: "ok" }, { type: "tool_use", id: "t1", name: "draw", input: {} }] },
    { role: "user", content: [{ type: "tool_result", tool_use_id: "t1", content: "drawn" }, { type: "text", text: "runtime note" }] },
  ] };
  const out = cacheConversation("anthropic/claude-sonnet-5.5", turns);
  assert.deepEqual(out[0].content, [{ type: "text", text: "draw an otter", cache_control: mark }]);
  assert.equal(out[1], turns.messages[1]);
  assert.equal(out[2].content[0].cache_control, undefined);
  assert.deepEqual(out[2].content[1], { type: "text", text: "runtime note", cache_control: mark });
  // The caller's own conversation is never rewritten in place.
  assert.equal(turns.messages[0].content, "draw an otter");
  assert.equal(turns.messages[2].content[1].cache_control, undefined);
  // Other models cache on their own; a client's own marks stand; four is the ceiling.
  assert.equal(cacheConversation("deepseek/deepseek-v4-pro", turns), turns.messages);
  assert.equal(cacheConversation("anthropic/claude-sonnet-5.5", { messages: out }), out);
  const full = { ...turns, system: Array(3).fill(turns.system[0]) };
  assert.equal(cacheConversation("anthropic/claude-sonnet-5.5", full).filter(m => Array.isArray(m.content) && m.content.some(b => b.cache_control)).length, 1);
  assert.equal(cacheConversation("anthropic/claude-sonnet-5.5", { ...turns, system: Array(4).fill(turns.system[0]) }), turns.messages);
  const sent = JSON.parse(inferenceProviderRequest({ model: "anthropic/claude-sonnet-5.5", maxTokens: 100, body: turns, key: "router" }).init.body);
  assert.deepEqual(sent.messages, out);
});

test("a refusal that names the cache marks is retried once as the conversation came", async () => {
  const sent = [];
  const body = { messages: [{ role: "user", content: "otter" }, { role: "assistant", content: "ok" }, { role: "user", content: "bigger" }] };
  const { response } = await requestHostedInference({ model: "anthropic/claude-sonnet-5.5", body, key: "router", maxTokens: 50 }, async (url, init) => {
    sent.push(JSON.parse(init.body));
    return sent.length === 1
      ? Response.json({ error: { message: "messages.0.content.0.cache_control: Extra inputs are not permitted" } }, { status: 400 })
      : new Response("stream");
  });
  assert.equal(await response.text(), "stream");
  assert.equal(sent.length, 2);
  assert.ok(sent[0].messages[2].content[0].cache_control);
  assert.deepEqual(sent[1].messages, body.messages);
  assert.deepEqual(JSON.parse(inferenceProviderRequest({ model: "anthropic/claude-sonnet-5.5", maxTokens: 50, key: "router",
    body: { messages: body.messages.slice(0, 1) } }).init.body).messages, body.messages.slice(0, 1));
});
