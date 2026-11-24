#!/usr/bin/env node
// slack-proxy.mjs — sit between Claude and slack-mcp-server and add the one
// tool it lacks: files_upload. Without it the Slack tool list is text-only and
// attaching a screenshot means knowing that slack-upload.mjs exists.
//
//   node toolchain/mcp/slack-proxy.mjs <server command...>
//
// MCP stdio is newline-delimited JSON-RPC. Every line passes through untouched
// except two: a tools/list response gets files_upload appended, and a
// tools/call for files_upload is answered here by running slack-upload.mjs
// (which owns the credentials) instead of reaching the server.

import { spawn, execFile } from "node:child_process";
import { createInterface } from "node:readline";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const UPLOAD = join(HERE, "slack-upload.mjs");

const TOOL = {
  name: "files_upload",
  description:
    "Upload a local file (screenshot, image, PDF, video, any file) to a Slack channel or into a thread. " +
    "Use this to attach images to messages: post the text with conversations_add_message, then upload " +
    "with thread_ts set to that message's ts. Posts as the same account as the other Slack tools.",
  inputSchema: {
    type: "object",
    properties: {
      channel_id: { type: "string", description: "Channel ID (Cxxxxxxxxxx) or name starting with #." },
      file_path: { type: "string", description: "Absolute path of the local file to upload." },
      thread_ts: { type: "string", description: "Optional ts of a message to reply in its thread." },
      title: { type: "string", description: "Optional title shown on the file." },
      filename: { type: "string", description: "Optional filename shown in Slack (defaults to the local name)." },
      comment: { type: "string", description: "Optional message text posted with the file." },
    },
    required: ["channel_id", "file_path"],
  },
};

const [cmd, ...cmdArgs] = process.argv.slice(2);
if (!cmd) {
  console.error("slack-proxy: give the server command to wrap");
  process.exit(1);
}
const server = spawn(cmd, cmdArgs, { stdio: ["pipe", "pipe", "inherit"] });
server.on("exit", (code) => process.exit(code ?? 1));

const send = (msg) => process.stdout.write(JSON.stringify(msg) + "\n");
const listIds = new Set();

function upload(args) {
  const argv = [UPLOAD, "--channel", args.channel_id, "--file", args.file_path];
  if (args.thread_ts) argv.push("--thread", args.thread_ts);
  if (args.title) argv.push("--title", args.title);
  if (args.filename) argv.push("--name", args.filename);
  if (args.comment) argv.push("--comment", args.comment);
  return new Promise((resolve) => {
    execFile(process.execPath, argv, { timeout: 120_000 }, (err, stdout, stderr) => {
      const text = (stdout + (err ? "\n" + (stderr || err.message) : "")).trim();
      resolve({ content: [{ type: "text", text }], isError: Boolean(err) });
    });
  });
}

createInterface({ input: process.stdin }).on("line", async (line) => {
  let msg;
  try { msg = JSON.parse(line); } catch { server.stdin.write(line + "\n"); return; }
  if (msg.method === "tools/list" && msg.id !== undefined) listIds.add(msg.id);
  if (msg.method === "tools/call" && msg.params?.name === TOOL.name) {
    send({ jsonrpc: "2.0", id: msg.id, result: await upload(msg.params.arguments || {}) });
    return;
  }
  server.stdin.write(line + "\n");
}).on("close", () => server.stdin.end());

createInterface({ input: server.stdout }).on("line", (line) => {
  let msg;
  try { msg = JSON.parse(line); } catch { process.stdout.write(line + "\n"); return; }
  if (msg.id !== undefined && listIds.has(msg.id)) {
    listIds.delete(msg.id);
    if (Array.isArray(msg.result?.tools)) msg.result.tools.push(TOOL);
    send(msg);
    return;
  }
  process.stdout.write(line + "\n");
});
