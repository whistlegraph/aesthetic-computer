import assert from "node:assert/strict";
import { createServer } from "node:http";
import { once } from "node:events";
import test from "node:test";
import { relayInference } from "../../system/backend/easel-stream.mjs";
import { sendStream } from "../../lith/stream-response.mjs";

const bytes = (text) => new TextEncoder().encode(text);

test("relay forwards first chunk before EOF and cancellation stops the provider", async () => {
  let source, cancelled = false, aborted = false;
  const body = new ReadableStream({ start(c) { source = c; }, cancel() { cancelled = true; } });
  const reader = relayInference(body, { abort: () => { aborted = true; } }).getReader();
  source.enqueue(bytes("data: first\n\n"));
  assert.equal(new TextDecoder().decode((await reader.read()).value), "data: first\n\n");
  await reader.cancel();
  assert.ok(cancelled && aborted);
});

test("usage survives chunk boundaries and final output-only usage updates", async () => {
  let charge = 0;
  const data = 'data: {"message":{"usage":{"input_tokens":100}}}\n\ndata: {"usage":{"output_tokens":8}}\n\n';
  const body = new ReadableStream({ start(c) { for (const char of data) c.enqueue(bytes(char)); c.close(); } });
  await new Response(relayInference(body, { onUsage: (n) => { charge = n; } })).text();
  await Promise.resolve();
  assert.equal(charge, 108);
});

test("HTTP adapter delivers data before generation ends and cancels on disconnect", async (t) => {
  let provider, resolveCancel;
  const cancelled = new Promise((resolve) => { resolveCancel = resolve; });
  const server = createServer((_req, res) => {
    res.setHeader("Content-Type", "text/event-stream");
    const stream = new ReadableStream({ start(c) { provider = c; c.enqueue(bytes("data: token\n\n")); }, cancel() { resolveCancel(); } });
    sendStream(res, stream).catch(() => {});
  });
  server.listen(0, "127.0.0.1");
  await once(server, "listening");
  t.after(() => { server.closeAllConnections(); server.close(); });
  const abort = new AbortController();
  const response = await fetch(`http://127.0.0.1:${server.address().port}`, { signal: abort.signal });
  const reader = response.body.getReader();
  assert.match(new TextDecoder().decode((await reader.read()).value), /token/);
  assert.ok(provider, "provider is still open");
  abort.abort();
  await cancelled;
});
