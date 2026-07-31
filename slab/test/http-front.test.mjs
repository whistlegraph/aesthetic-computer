import test from "node:test";
import assert from "node:assert/strict";
import { once } from "node:events";
import { serveHttp } from "../../toolchain/mcp/http-front.mjs";

test("shared HTTP MCP forwards request headers as per-client context", async () => {
  const server = serveHttp({
    port: 0,
    banner: "http-front-test",
    handleMessage(message, context) {
      return {
        jsonrpc: "2.0",
        id: message.id,
        result: {
          contact: context.headers["x-slab-loopboy-contact"],
          sessionId: context.headers["x-slab-prompt-session-id"],
        },
      };
    },
  });

  try {
    await once(server, "listening");
    const { port } = server.address();
    const response = await fetch(`http://127.0.0.1:${port}/mcp`, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        "x-slab-loopboy-contact": "fia",
        "x-slab-prompt-session-id": "session-fia",
      },
      body: JSON.stringify({ jsonrpc: "2.0", id: 1, method: "ping" }),
    });
    assert.equal(response.status, 200);
    assert.deepEqual((await response.json()).result, {
      contact: "fia",
      sessionId: "session-fia",
    });
  } finally {
    server.close();
    await once(server, "close");
  }
});
