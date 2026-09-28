import assert from "node:assert/strict";
import test from "node:test";
import { AC_MODELS, DEFAULT_AC_MODEL } from "../src/ac-server.mjs";
import { EASEL_MODELS, inferenceRequest, inferenceBudgetFailure } from "../../system/backend/easel-policy.mjs";
const messages = [{ role: "user", content: "hello" }];

test("hosted defaults remain inexpensive and each advertised model is explicitly allowed", () => {
  assert.equal(inferenceRequest({ messages }).model, "openai/gpt-5.6-luna");
  assert.equal(DEFAULT_AC_MODEL, inferenceRequest({ messages }).model);
  assert.equal(AC_MODELS.luna, DEFAULT_AC_MODEL);
  assert.equal(AC_MODELS.opus, "anthropic/claude-opus-5");
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
  assert.equal(inferenceRequest({ messages, max_tokens: 99999 }).maxTokens, 8192);
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
