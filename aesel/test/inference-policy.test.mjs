import assert from "node:assert/strict";
import test from "node:test";
import { AC_MODELS, DEFAULT_AC_MODEL } from "../src/ac-server.mjs";
import { OPEN_MODEL_INFO } from "../src/open-models.mjs";
import { EASEL_MODELS, inferenceRequest, inferenceBudgetFailure } from "../../system/backend/easel-policy.mjs";
const messages = [{ role: "user", content: "hello" }];

test("hosted defaults remain inexpensive and each advertised model is explicitly allowed", () => {
  assert.equal(inferenceRequest({ messages }).model, "deepseek/deepseek-v4.1-flash");
  assert.equal(DEFAULT_AC_MODEL, inferenceRequest({ messages }).model);
  assert.equal(AC_MODELS.flash, DEFAULT_AC_MODEL);
  assert.equal(AC_MODELS.opus, "anthropic/claude-opus-5");
  // The hosted provider offers every open model the OpenRouter bridge does.
  for (const { id } of Object.values(OPEN_MODEL_INFO)) assert.ok(Object.hasOwn(EASEL_MODELS, id), id);
  // Installed clients still name the older models; they stay allowed.
  for (const model of ["openai/gpt-5.6-luna", "z-ai/glm-4.6", "qwen/qwen3-coder", "deepseek/deepseek-chat-v3.1"]) assert.equal(inferenceRequest({ model, messages }).model, model);
  for (const model of Object.values(AC_MODELS)) {
    assert.ok(Object.hasOwn(EASEL_MODELS, model));
    assert.equal(inferenceRequest({ model, messages }).model, model);
  }
  assert.equal(inferenceRequest({ model: "anthropic/claude-sonnet-4.6", messages }).model, "anthropic/claude-sonnet-4.6");
  assert.equal(inferenceRequest({ model: "openai/gpt-5.4", messages }).model, "openai/gpt-5.4");
});

test("unsupported models and malformed requests refuse rather than silently falling back", () => {
  for (const model of ["unknown", "", null, [], ["openai/gpt-5.4"], "toString", "__proto__"]) {
    assert.throws(() => inferenceRequest({ model, messages }), /Unsupported model/);
  }
  for (const body of [null, [], "request"]) assert.throws(() => inferenceRequest(body), /request object/);
  assert.throws(() => inferenceRequest({ messages: [] }), /message/);
  for (const max_tokens of [-1, 0, 1.5, "100", null, Infinity]) assert.throws(() => inferenceRequest({ messages, max_tokens }), /positive integer/);
  assert.equal(inferenceRequest({ messages, max_tokens: 99999 }).maxTokens, 32000);
  assert.equal(inferenceRequest({ messages }).maxTokens, 8192);
  assert.equal(inferenceRequest({ messages, max_tokens: 100 }).maxTokens, 100);
});

test("unknown or failed budget checks never permit paid hosted inference", () => {
  const valid = { used: 0, budget: 200000, remaining: 200000, exhausted: false };
  assert.equal(inferenceBudgetFailure(valid, "test"), null);
  for (const budget of [null, undefined, { ...valid, unknown: true }, {}, { ...valid, remaining: NaN }]) {
    assert.equal(inferenceBudgetFailure(budget, "test").statusCode, 503);
  }
  assert.equal(inferenceBudgetFailure({ ...valid, remaining: 0 }, "test").statusCode, 429);
  assert.equal(inferenceBudgetFailure({ ...valid, exhausted: true }, "test").statusCode, 429);
});
