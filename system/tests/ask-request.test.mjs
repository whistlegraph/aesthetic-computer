import test from "node:test";
import assert from "node:assert/strict";
import vm from "node:vm";
import { readFileSync } from "node:fs";

const serverSource = readFileSync(new URL("../netlify/functions/ask.js", import.meta.url), "utf8");
function server() {
  const requests = [];
  const context = vm.createContext({
    exports: {}, require: () => ({ stream: (handler) => handler }),
    process: { env: { OPENAI_API_KEY: "synthetic-test-key" } },
    console: { log() {}, error() {} },
    AbortController, ReadableStream, TextEncoder, TextDecoder,
    setTimeout: () => 1, clearTimeout() {},
    fetch: async (url, options) => {
      requests.push({ url, payload: JSON.parse(options.body) });
      return new Response("data: [DONE]\n\n");
    },
  });
  new vm.Script(serverSource, {
    // Avoid loading real authorization/database code; its failed lookup follows
    // the endpoint's ordinary anonymous path without touching any service.
    importModuleDynamically: async () => { throw Error("Synthetic auth unavailable"); },
  }).runInContext(context);
  return {
    requests,
    request: (body) => context.exports.handler({
      httpMethod: "POST", headers: { origin: "https://aesthetic.computer" },
      body: typeof body === "string" ? body : JSON.stringify(body),
    }),
  };
}

test("invalid and empty ask requests stop before any provider request", async () => {
  const f = server();
  for (const input of ["{", null, {}, { messages: [] }, { messages: "bad" },
    { messages: [null] }, { messages: [{ by: "unknown", text: "synthetic" }] },
    { messages: [{ by: "user", text: {} }] },
    { messages: [{ by: "user", text: "synthetic" }], hint: {} }]) {
    const result = await f.request(input);
    assert.equal(result.statusCode, 400);
    assert.equal(result.headers["Access-Control-Allow-Origin"], "https://aesthetic.computer");
  }
  assert.equal(f.requests.length, 0);
});

test("a valid anonymous conversation retains content and defaults a missing hint", async () => {
  const f = server();
  const result = await f.request({ messages: [{ by: "user", text: "synthetic prompt" }] });
  assert.equal(result.statusCode, 200);
  assert.deepEqual(f.requests[0].payload.messages, [{ role: "user", content: "synthetic prompt" }]);
  assert.equal(f.requests[0].payload.model, "gpt-4o-mini");
  await new Response(result.body).text();
});

const clientSource = readFileSync(new URL("../public/aesthetic.computer/lib/ask.mjs", import.meta.url), "utf8")
  .replace(/^import .*;$/gm, "const DEBUG = false;")
  .replace(/export (function|class) /g, "$1 ");

test("forgetful asks snapshot their messages before asynchronous token lookup", async () => {
  const token = Promise.withResolvers(), done = Promise.withResolvers();
  const requests = [];
  const context = vm.createContext({
    AbortController, TextDecoder, setTimeout: () => 1, clearTimeout() {},
    fetch: async (_url, options) => {
      requests.push({ headers: options.headers, body: JSON.parse(options.body) });
      return new Response("synthetic reply");
    },
  });
  vm.runInContext(clientSource, context);
  context.setAskTokenProvider(() => token.promise);
  const Conversation = vm.runInContext("Conversation", context);
  const conversation = new Conversation({}, "synthetic", true);
  let reply = "";
  conversation.ask({ prompt: "synthetic prompt", hint: "character:gpt-4o",
    program: { before: "synthetic instruction", after: "" } },
    (text) => { reply += text; }, done.resolve, () => done.reject(Error("Ask failed")));
  assert.equal(conversation.messages.length, 0);
  assert.equal(requests.length, 0);
  token.resolve("synthetic-token");
  await done.promise;
  assert.equal(requests[0].headers.Authorization, "Bearer synthetic-token");
  assert.deepEqual(requests[0].body.messages, [
    { by: "system", text: "synthetic instruction" },
    { by: "user", text: "synthetic prompt" },
  ]);
  assert.equal(reply, "synthetic reply");
});
