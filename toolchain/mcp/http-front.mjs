// http-front.mjs — let every Claude session share ONE resident MCP server.
//
// Claude Code spawns a fresh stdio child for each MCP server in each session,
// so N parallel sessions cost N copies of every server. On an 8 GB box (neo)
// that adds up: three sessions once carried ~570 MB of duplicate MCP processes.
//
// These servers answer each JSON-RPC call from scratch — `handleMessage` holds
// no per-connection state — so a plain POST front is enough for any number of
// sessions to share one process. Streamable HTTP minus the streaming: POST a
// message, get the reply. A notification (no `id`) must get a bare 202 and no
// body, or `notifications/initialized` falls through to method-not-found and
// every session logs a warning.
//
// Bind loopback only. ants/mail-mcp keeps its own SDK transport because it also
// binds off-loopback (jasellite) behind bearer auth — a different problem.

import { createServer } from "node:http";
import * as readline from "node:readline";

const answerable = (m) => m.id !== undefined && m.id !== null;

/** Port from `--http [port]` in argv, or null when the flag is absent. */
export function httpPort(argv, fallback) {
  const i = argv.indexOf("--http");
  if (i === -1) return null;
  return Number(argv[i + 1]) || fallback;
}

/** One resident process, many sessions. */
export function serveHttp({ handleMessage, port, host = "127.0.0.1", banner }) {
  createServer(async (req, res) => {
    if (req.method !== "POST") {
      res.writeHead(405, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ jsonrpc: "2.0", id: null, error: { code: -32000, message: "stateless server: POST only" } }));
      return;
    }
    let body = "";
    for await (const chunk of req) body += chunk;
    try {
      const message = JSON.parse(body);
      const response = Array.isArray(message)
        ? (await Promise.all(message.filter(answerable).map(handleMessage))).filter(Boolean)
        : answerable(message) ? await handleMessage(message) : null;
      if (!response || (Array.isArray(response) && !response.length)) {
        res.writeHead(202);
        res.end();
        return;
      }
      res.writeHead(200, { "Content-Type": "application/json" });
      res.end(JSON.stringify(response));
    } catch (e) {
      res.writeHead(400, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ jsonrpc: "2.0", id: null, error: { code: -32700, message: `Parse error: ${e.message}` } }));
    }
  }).listen(port, host, () => console.error(`${banner} on http://${host}:${port}`));
}

/** One process per session — what Claude Code does when it spawns us directly. */
export function serveStdio({ handleMessage, banner }) {
  const rl = readline.createInterface({ input: process.stdin, output: process.stdout, terminal: false });
  rl.on("line", async (line) => {
    if (!line.trim()) return;
    try {
      const response = await handleMessage(JSON.parse(line));
      if (response) console.log(JSON.stringify(response));
    } catch (e) {
      console.error(JSON.stringify({ jsonrpc: "2.0", id: null, error: { code: -32700, message: `Parse error: ${e.message}` } }));
    }
  });
  console.error(banner);
}
