#!/usr/bin/env node
// rax-dev.mjs — small helpers for whoever is building rax.
//
// rax_say speaks a status line in @jeffrey's ElevenLabs voice (rax/bin/say.mjs,
// which falls back to macOS `say`). rax_look plays a local rax through a head
// attack and a fall and returns a screenshot a second (rax/test/look-browser.mjs).
// rax_served tells what production serves beside local HEAD.
//
// Nothing here deploys, commits or reads a key. ac-deploy is only ever run
// with --verify: with any other argument it ships lith.

import { execFile } from "node:child_process";
import { mkdtempSync, readdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";

const ROOT = join(dirname(fileURLToPath(import.meta.url)), "../..");

// Run a command to its end; never throws. `code` is null when it could not start.
const run = (cmd, args, timeout = 60_000) => new Promise((done) => {
  execFile(cmd, args, { cwd: ROOT, timeout, maxBuffer: 8 * 1024 * 1024 }, (error, stdout, stderr) => {
    done({ code: error ? (typeof error.code === "number" ? error.code : null) : 0, stdout: String(stdout).trim(), stderr: String(stderr).trim(), error: error ? String(error.message).split("\n")[0] : null });
  });
});

// 🗣️ Say
async function raxSay({ text } = {}) {
  if (typeof text !== "string" || !text.trim()) throw new Error("text is required");
  const r = await run(process.execPath, [join(ROOT, "rax/bin/say.mjs"), text.trim().slice(0, 600)], 90_000);
  if (r.code !== 0) throw new Error(r.stderr || r.error || "say.mjs failed");
  // say.mjs writes one line to stderr only when it fell back to macOS `say`.
  return r.stderr ? { spoken: true, voice: "macOS say", fallback: r.stderr } : { spoken: true, voice: "jeffrey (ElevenLabs)" };
}

// 👀 Look
async function raxLook({ seconds = 12, fallAt, slow = false } = {}) {
  const secs = Math.max(1, Math.min(40, Math.round(Number(seconds) || 12)));
  const fall = Math.max(1, Math.round(Number(fallAt) || 9)); // (look-browser's own default)
  const out = mkdtempSync(join(tmpdir(), "rax-look-shots-"));
  const args = [join(ROOT, "rax/test/look-browser.mjs"), out, String(secs), String(fall)];
  if (slow) args.push("slow");
  const r = await run(process.execPath, args, (secs + 60) * 1000);
  const pngs = readdirSync(out).filter((f) => f.endsWith(".png")).sort().map((f) => join(out, f));
  let report = null; // (the JSON the script prints last: page errors and where the body ended)
  try { report = JSON.parse(r.stdout.slice(r.stdout.lastIndexOf("\n{") + 1)); } catch {}
  return { dir: out, pngs, errors: report?.errors ?? null, last: report?.last ?? null, ...(r.code === 0 ? {} : { failed: r.stderr.slice(-800) || r.error }) };
}

// 🚢 Served
async function raxServed() {
  const head = (await run("git", ["rev-parse", "--short=10", "HEAD"])).stdout || null;
  const origin = (await run("git", ["rev-parse", "--short=10", "origin/main"])).stdout || null;
  const v = await run("ac-deploy", ["--verify"], 120_000);
  if (v.code === null) return { served: null, head, origin, live: null, note: `ac-deploy did not run (${v.error}); served revision unknown` };
  const served = v.stdout.match(/serving\s+([0-9a-f]{7,40})/)?.[1] ?? null;
  const live = served && head ? head.startsWith(served) || served.startsWith(head) : null;
  return { served, head, origin, live, verify: v.stdout.split("\n").slice(-6) };
}

const TOOLS = [
  {
    name: "rax_say",
    description: "Speak one short status line aloud on this Mac in @jeffrey's own ElevenLabs voice (falls back to macOS `say` and reports why). Each call spends ElevenLabs characters: keep it to a sentence.",
    inputSchema: { type: "object", properties: { text: { type: "string", description: "The sentence to speak (cut at 600 characters)." } }, required: ["text"], additionalProperties: false },
  },
  {
    name: "rax_look",
    description: "See rax without a screen: starts a local rax, walks one signed-in body to the gun, lets the heads attack, then walks it off the roof, saving a 960x600 PNG each second. Returns the PNG paths (read them to look), the page's errors and the body's last state. Takes about the seconds asked plus twenty.",
    inputSchema: {
      type: "object",
      properties: {
        seconds: { type: "number", description: "Seconds to play and shoot (default 12, at most 40).", default: 12, minimum: 1, maximum: 40 },
        fallAt: { type: "number", description: "The second at which the body turns and walks off the roof (default 9).", default: 9, minimum: 1 },
        slow: { type: "boolean", description: "Step off slowly on to the fire escape, then look back at the tower, instead of running off.", default: false },
      },
      additionalProperties: false,
    },
  },
  {
    name: "rax_served",
    description: "Is the deploy live? Returns the revision production serves (from `ac-deploy --verify`, which only checks), local HEAD, origin/main, and `live`: whether served equals HEAD. Read-only; never deploys.",
    inputSchema: { type: "object", properties: {}, additionalProperties: false },
  },
];

async function callTool(name, args = {}) {
  const tool = { rax_say: raxSay, rax_look: raxLook, rax_served: raxServed }[name];
  if (!tool) throw new Error(`unknown tool ${name}`);
  return [{ type: "text", text: JSON.stringify(await tool(args), null, 2) }];
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: params?.protocolVersion || "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "rax-dev", version: "1.0.0" },
          },
        };
      case "notifications/initialized": return null;
      case "ping": return { jsonrpc: "2.0", id, result: {} };
      case "tools/list": return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default: return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 0);
if (port) serveHttp({ handleMessage, port, banner: "🎮 rax-dev shared daemon" });
else serveStdio({ handleMessage, banner: "🎮 rax-dev started (rax_say, rax_look, rax_served)" });
