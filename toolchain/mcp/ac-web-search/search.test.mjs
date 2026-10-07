import assert from "node:assert/strict";
import test from "node:test";
import { createExa, readRpc } from "./exa.mjs";
import { createHandler } from "./server.mjs";

test("SSE parser ignores notifications and joins split CRLF frames", async () => {
  const chunks = ["event: message\r\ndata: {\"method\":\"notifications/test\"}\r\n\r\n",
    "event: message\r\ndata: {\"id\":7,\"result\":", "{\"ok\":true}}\r\n\r\n"];
  const body = new ReadableStream({ start(controller) {
    for (const chunk of chunks) controller.enqueue(new TextEncoder().encode(chunk));
    controller.close();
  } });
  const response = new Response(body, { headers: { "content-type": "text/event-stream" } });
  assert.deepEqual(await readRpc(response, 7), { id: 7, result: { ok: true } });
});

test("Exa adapter sends only requested inputs and keeps key out of URL and returned text", async () => {
  const provider = createExa({ apiKey: "test-secret", fetchImpl: async (url, request) => {
    assert.equal(url.includes("test-secret"), false);
    assert.equal(request.headers["x-api-key"], "test-secret");
    const body = JSON.parse(request.body);
    assert.deepEqual(body.params, { name: "web_search_exa", arguments: {
      query: "example", objective: "verify", numResults: 3,
    } });
    return Response.json({ id: body.id, result: { content: [{ type: "text", text: "source test-secret" }] } });
  } });
  const result = await provider.search({ query: "example", objective: "verify", limit: 3 });
  assert.equal(result.content[0].text, "source [redacted]");
});

test("rate limits are reported without retries or upstream error body", async () => {
  let calls = 0;
  const provider = createExa({ fetchImpl: async () => { calls++; return new Response("private error details", { status: 429 }); } });
  await assert.rejects(provider.search({ query: "test", objective: "verify", limit: 1 }), /rate limit/);
  assert.equal(calls, 1);
});

test("validation blocks private URLs and oversized requests before backend calls", async () => {
  let calls = 0;
  const handle = createHandler({ getKey: () => "", backend: () => ({
    fetch: async () => { calls++; return { content: [] }; },
    search: async () => { calls++; return { content: [] }; },
  }) });
  const invoke = (name, args) => handle({ jsonrpc: "2.0", id: 1, method: "tools/call", params: { name, arguments: args } });
  for (const url of ["file:///etc/passwd", "http://localhost/a", "http://127.0.0.1/", "http://[::1]/", "http://machine.local/", "https://user:password@example.com/"]) {
    assert.equal((await invoke("ac_web_fetch", { urls: [url] })).result.isError, true);
  }
  assert.equal((await invoke("ac_web_search", { query: "test", limit: 11 })).result.isError, true);
  assert.equal((await invoke("ac_web_search", { query: "test", session: "private context" })).result.isError, true);
  assert.equal(calls, 0);
  assert.equal((await invoke("ac_web_fetch", { urls: ["https://example.com/"] })).result.isError, undefined);
  assert.equal(calls, 1);
});

test("MCP discovery/status work without provider access and never return key", async () => {
  const handle = createHandler({ getKey: () => "secret", backend: () => { throw new Error("unexpected network"); } });
  const initialized = await handle({ id: 1, method: "initialize" });
  assert.equal(initialized.result.serverInfo.name, "ac-web-search");
  const listed = await handle({ id: 2, method: "tools/list" });
  assert.equal(listed.result.tools.length, 3);
  const status = await handle({ id: 3, method: "tools/call", params: { name: "ac_web_search_status" } });
  assert.equal(JSON.stringify(status).includes("secret"), false);
  assert.equal(JSON.parse(status.result.content[0].text).authentication, "api-key");
  assert.equal(await handle({ method: "notifications/initialized" }), null);
});
