import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { createInterface } from "node:readline";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { offlineResponse } from "./iris-mcp.mjs";

test("offline server advertises no Iris tools", () => {
  assert.deepEqual(offlineResponse({ jsonrpc: "2.0", id: 1, method: "tools/list" }), {
    jsonrpc: "2.0", id: 1, result: { tools: [] },
  });
});

test("unreachable Iris still completes the MCP handshake", async () => {
  const child = spawn(process.execPath, [fileURLToPath(new URL("./iris-mcp.mjs", import.meta.url))], {
    env: { ...process.env, IRIS_MCP_SSH: "/usr/bin/false" },
    stdio: ["pipe", "pipe", "pipe"],
  });
  child.stdin.write(`${JSON.stringify({
    jsonrpc: "2.0",
    id: 7,
    method: "initialize",
    params: { protocolVersion: "test-version" },
  })}\n`);
  const line = await new Promise((resolve, reject) => {
    const lines = createInterface({ input: child.stdout });
    lines.once("line", resolve);
    child.once("error", reject);
    child.once("exit", (code) => reject(new Error(`offline MCP exited before replying (${code})`)));
  });
  assert.deepEqual(JSON.parse(line), {
    jsonrpc: "2.0", id: 7,
    result: {
      protocolVersion: "test-version",
      capabilities: { tools: { listChanged: false } },
      serverInfo: { name: "iris-offline", version: "1.0.0" },
    },
  });
  child.kill();
});
