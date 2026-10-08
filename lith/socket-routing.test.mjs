import test from "node:test";
import assert from "node:assert/strict";
import { createServer } from "node:http";
import { once } from "node:events";
import { WebSocket } from "ws";
import { attachMusicalSocket } from "./musical-socket.mjs";
import { attachWhistlegraphSocket } from "./whistlegraph-socket.mjs";

test("unmatched upgrades close promptly while all three registered routes still upgrade", { timeout: 5000 }, async t => {
  const server = createServer((_req, res) => res.end("ok"));
  const sockets = new Set();
  server.on("connection", socket => { sockets.add(socket); socket.on("close", () => sockets.delete(socket)); });
  const musical = attachMusicalSocket(server, {}), whistlegraph = attachWhistlegraphSocket(server, {});
  t.after(() => { musical.close(); whistlegraph.close(); for (const socket of sockets) socket.destroy(); server.close(); });
  server.listen(0, "127.0.0.1"); await once(server, "listening");
  const base = `ws://127.0.0.1:${server.address().port}`;
  for (const path of ["/", "/unknown", "/api/easel-musical-stream?unexpected=1"]) {
    const status = await new Promise(resolve => {
      const ws = new WebSocket(base + path);
      const timer = setTimeout(() => { ws.terminate(); resolve("timeout"); }, 500);
      ws.on("error", () => {});
      ws.on("unexpected-response", (_req, res) => { clearTimeout(timer); res.resume(); ws.terminate(); resolve(res.statusCode); });
    });
    assert.equal(status, 404, path);
  }
  for (const path of ["/api/easel-musical-stream", "/api/whistlegraph-stream", "/api/walkieware-stream"]) {
    const ws = new WebSocket(base + path);
    await once(ws, "open"); ws.close(); await once(ws, "close");
  }
  musical.close();
  const ws = new WebSocket(base + "/api/whistlegraph-stream");
  await once(ws, "open"); ws.close(); await once(ws, "close");
  assert.equal(await (await fetch(base.replace("ws:", "http:") + "/")).text(), "ok");
});
