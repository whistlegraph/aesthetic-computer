#!/usr/bin/env node
// Keep the Iris MCP available when panda is asleep or off the network.

import { spawn, spawnSync } from "node:child_process";
import * as readline from "node:readline";
import { pathToFileURL } from "node:url";

const ssh = process.env.IRIS_MCP_SSH || "ssh";
const host = process.env.IRIS_MCP_HOST || "panda";
const connectTimeout = process.env.IRIS_MCP_CONNECT_TIMEOUT || "3";
const remoteCommand = process.env.IRIS_MCP_REMOTE_COMMAND
  || "/opt/homebrew/bin/node /Users/fusermacminipanda/Developer/fuser/tools/iris/iris-mcp.mjs";
const remoteProbe = process.env.IRIS_MCP_REMOTE_PROBE
  || "test -x /opt/homebrew/bin/node && test -f /Users/fusermacminipanda/Developer/fuser/tools/iris/iris-mcp.mjs";

export function offlineResponse(message) {
  if (message.id === undefined || message.id === null) return null;
  const result = {
    initialize: {
      protocolVersion: message.params?.protocolVersion || "2025-06-18",
      capabilities: { tools: { listChanged: false } },
      serverInfo: { name: "iris-offline", version: "1.0.0" },
    },
    "tools/list": { tools: [] },
    "resources/list": { resources: [] },
    "prompts/list": { prompts: [] },
    ping: {},
  }[message.method];
  if (result) return { jsonrpc: "2.0", id: message.id, result };
  return {
    jsonrpc: "2.0",
    id: message.id,
    error: { code: -32001, message: "Iris is offline or unreachable" },
  };
}

function serveOffline(reason) {
  console.error(`iris-mcp: Iris unavailable (${reason}); serving an empty MCP`);
  const lines = readline.createInterface({ input: process.stdin, terminal: false });
  lines.on("line", (line) => {
    if (!line.trim()) return;
    try {
      const response = offlineResponse(JSON.parse(line));
      if (response) process.stdout.write(`${JSON.stringify(response)}\n`);
    } catch (error) {
      process.stderr.write(`iris-mcp: ignored invalid JSON: ${error.message}\n`);
    }
  });
}

export function main() {
  const common = ["-o", "BatchMode=yes", "-o", `ConnectTimeout=${connectTimeout}`, host];
  const probe = spawnSync(ssh, [...common, remoteProbe], {
    encoding: "utf8",
    timeout: (Number(connectTimeout) + 1) * 1000,
  });
  if (probe.status !== 0) {
    serveOffline(probe.error?.message || probe.stderr.trim() || `ssh exited ${probe.status}`);
    return;
  }

  const child = spawn(ssh, [...common, remoteCommand], { stdio: ["pipe", "pipe", "inherit"] });
  process.stdin.pipe(child.stdin);
  child.stdout.pipe(process.stdout);
  child.on("error", (error) => {
    console.error(`iris-mcp: connection failed after probe: ${error.message}`);
    process.exitCode = 0;
  });
  child.on("exit", (code) => { process.exitCode = code || 0; });
}

if (import.meta.url === pathToFileURL(process.argv[1]).href) main();
