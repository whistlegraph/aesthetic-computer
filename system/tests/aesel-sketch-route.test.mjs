// Run with --experimental-test-module-mocks; no database or paid provider calls.
import test, { mock } from "node:test";
import assert from "node:assert/strict";

const cache = new Map(), stored = [], logged = [];
let generations = 0, feedUpdates = 0, finish;
const sketches = {
  createIndex: async () => {}, findOne: async ({ word }) => cache.get(word),
  updateOne: async ({ word }, { $set }) => cache.set(word, $set),
};
mock.module("../backend/database.mjs", { exports: { connect: async () => ({
  db: { collection: name => name === "sketches" ? sketches : { updateOne: async () => {} } }, disconnect: async () => {},
}) } });
mock.module("../netlify/functions/store-kidlisp.mjs", { exports: { handler: async event => {
  stored.push(JSON.parse(event.body).source);
  return { body: JSON.stringify({ code: `code${stored.length}` }) };
} } });
mock.module("../netlify/functions/prompt-log.mjs", { exports: { logPrompt: async (_, entry) => logged.push(entry) } });
mock.module("../backend/mime-feed-engine.mjs", { exports: { freshenFeed: () => feedUpdates++ } });
mock.module("../backend/aesel-sketch.mjs", { exports: { drawSketch: async (word, onText) => {
  generations++;
  onText("(wipe blue)");
  if (word === "otter cloud") await new Promise(resolve => { finish = resolve; });
  return { program: "(wipe blue)" };
} } });
const { handler } = await import("../netlify/functions/aesel-sketch.mjs");
const { handler: legacy } = await import("../netlify/functions/sketch.mjs");
mock.module("@netlify/functions", { exports: { stream: fn => fn } });
const { handler: authenticatedInference } = await import("../netlify/functions/easel-inference.mjs");
const event = (word, source = "test", stream = "1") => ({ httpMethod: "GET",
  headers: { "client-ip": source }, queryStringParameters: { word, stream } });

test("old and new URLs share generation, disconnected streams, cache, tune and feed", async () => {
  assert.equal(legacy, handler);
  const first = await handler(event("otter cloud"));
  await first.body.cancel();
  const second = await legacy(event("otter cloud"));
  assert.equal(generations, 1);
  const reading = new Response(second.body).text();
  finish();
  const text = await reading;
  assert.ok(text.startsWith("(wipe blue)"));
  const result = JSON.parse(text.split("\u0000")[1]);
  assert.equal(result.code, "code1");
  assert.match(result.source, /\(melody /);
  assert.equal(feedUpdates, 1);
  assert.equal(logged.length, 1);
  const cached = await handler(event("otter cloud"));
  assert.deepEqual(JSON.parse(cached.body), result);
  assert.equal(generations, 1);
});

test("aliases cannot bypass the per-source generation limit", async () => {
  const before = generations;
  for (const [i, word] of ["red otter", "blue otter", "green otter", "yellow otter", "pink otter", "black otter", "white otter"].entries()) {
    const reply = await (i % 2 ? legacy : handler)(event(word, "limited-source", ""));
    assert.equal(Boolean(JSON.parse(reply.body).code), i < 6);
  }
  assert.equal(generations - before, 6);
});

test("invalid input and methods never reach generation", async () => {
  const before = generations;
  assert.equal((await handler({ httpMethod: "POST" })).statusCode, 405);
  assert.deepEqual(JSON.parse((await handler(event("https://example.com"))).body), { skip: true });
  assert.equal(generations, before);
});

test("public sketch access does not open authenticated Aesel inference", async () => {
  const previous = process.env.OPENROUTER_API_KEY;
  process.env.OPENROUTER_API_KEY = "test-only";
  try {
    const response = await authenticatedInference({ httpMethod: "POST", headers: {},
      body: JSON.stringify({ messages: [{ role: "user", content: "otter" }] }) });
    assert.equal(response.statusCode, 401);
    assert.match(JSON.parse(response.body).error.message, /handle/);
  } finally {
    if (previous === undefined) delete process.env.OPENROUTER_API_KEY;
    else process.env.OPENROUTER_API_KEY = previous;
  }
});

test("signed-in Aesel sends Sonnet to OpenRouter even with a direct key configured", async t => {
  t.mock.module("../backend/authorization.mjs", { exports: {
    authorize: async () => ({sub:"fixture"}), getHandleOrEmail: async () => "@fixture",
  } });
  t.mock.module("../backend/ai-budget.mjs", { exports: { checkBudget: async () => ({remaining:5, budget:5}) } });
  t.mock.module("../backend/easel-paid-credits.mjs", { exports: { INFERENCE_DEADLINE_MS: 2000 } });
  t.mock.module("../backend/aesel-curation.mjs", { exports: { readCuration: async () => null, curatedModel: (_menu, model) => model } });
  const previous = {OPENROUTER_API_KEY:process.env.OPENROUTER_API_KEY, ANTHROPIC_CREDITS_KEY:process.env.ANTHROPIC_CREDITS_KEY};
  t.after(() => {
    for (const [key,value] of Object.entries(previous)) {
      if (value === undefined) delete process.env[key];
      else process.env[key] = value;
    }
  });
  process.env.OPENROUTER_API_KEY = "test-router";
  process.env.ANTHROPIC_CREDITS_KEY = "test-direct";
  const transport = t.mock.method(globalThis, "fetch", async () => new Response("", {status:503}));
  const model = "anthropic/claude-sonnet-5.5";
  const reply = await authenticatedInference({httpMethod:"POST", headers:{authorization:"Bearer fixture"},
    body:JSON.stringify({model, messages:[{role:"user",content:"hi"}]})});
  assert.equal(reply.statusCode, 503);
  assert.equal(transport.mock.callCount(), 1);
  const [url, options] = transport.mock.calls[0].arguments;
  assert.equal(url, "https://openrouter.ai/api/v1/messages");
  assert.equal(options.headers.Authorization, "Bearer test-router");
  assert.equal(options.headers["x-api-key"], undefined);
  assert.equal(JSON.parse(options.body).model, model);
});
