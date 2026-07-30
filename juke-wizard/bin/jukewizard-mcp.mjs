#!/usr/bin/env node
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { serveStdio } from "../../toolchain/mcp/http-front.mjs";

const exec = promisify(execFile);
const cli = join(dirname(fileURLToPath(import.meta.url)), "jukewizard-control.mjs");
const TOOLS = [
  { name: "jukewizard_status", description: "Return Jukewizard's mode, current track, transport position, speed, and actual playing state.", inputSchema: { type: "object", properties: {} } },
  { name: "jukewizard_list", description: "List the native Jukewizard queue with exact indices, titles, lanes, and paths.", inputSchema: { type: "object", properties: { limit: { type: "integer", minimum: 1, maximum: 1000 } } } },
  { name: "jukewizard_play", description: "Play the current track, or select and play one queue item by exact path, exact unambiguous title, or index.", inputSchema: { type: "object", properties: { path: { type: "string" }, title: { type: "string" }, index: { type: "integer" } } } },
  { name: "jukewizard_pause", description: "Pause library playback.", inputSchema: { type: "object", properties: {} } },
  { name: "jukewizard_toggle", description: "Toggle library playback.", inputSchema: { type: "object", properties: {} } },
  { name: "jukewizard_seek", description: "Seek the current library track to seconds.", inputSchema: { type: "object", properties: { seconds: { type: "number", minimum: 0 } }, required: ["seconds"] } },
  { name: "jukewizard_speed", description: "Set library playback speed from 0.5 through 1.5.", inputSchema: { type: "object", properties: { speed: { type: "number", minimum: 0.5, maximum: 1.5 } }, required: ["speed"] } },
  { name: "jukewizard_next", description: "Select and play the next library track.", inputSchema: { type: "object", properties: {} } },
  { name: "jukewizard_previous", description: "Select and play the previous library track.", inputSchema: { type: "object", properties: {} } },
];

async function run(name, args) {
  const command = name.replace("jukewizard_", "");
  const argv = [command];
  if (command === "list" && args.limit) argv.push(String(args.limit));
  if (command === "seek") argv.push(String(args.seconds));
  if (command === "speed") argv.push(String(args.speed));
  if (command === "play") {
    if (args.path) argv.push(args.path);
    else if (args.title) argv.push("--title", args.title);
    else if (Number.isInteger(args.index)) argv.push("--index", String(args.index));
  }
  const { stdout } = await exec(process.execPath, [cli, ...argv], { timeout: 5000, maxBuffer: 4_000_000 });
  return [{ type: "text", text: stdout.trim() }];
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    if (method === "initialize") return { jsonrpc: "2.0", id, result: { protocolVersion: "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "jukewizard-mcp", version: "1.0.0" } } };
    if (method === "initialized" || method === "notifications/initialized") return null;
    if (method === "ping") return { jsonrpc: "2.0", id, result: {} };
    if (method === "tools/list") return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
    if (method === "tools/call") return { jsonrpc: "2.0", id, result: { content: await run(params?.name, params?.arguments || {}) } };
    return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
  } catch (error) {
    return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.stderr || error.message || error) }] } };
  }
}

serveStdio({ handleMessage, banner: "💿 jukewizard MCP started" });
