#!/usr/bin/env node
// venue-cli.mjs — call one ac-venue tool from a shell, no JSON quoting.
//
//   node toolchain/mcp/venue-cli.mjs venue_setlist
//   node toolchain/mcp/venue-cli.mjs venue_enqueue out=/Users/jas/Shelf/set-midi-01-rush-e label="Rush E"
//   node toolchain/mcp/venue-cli.mjs venue_autoplay on=true
//   node toolchain/mcp/venue-cli.mjs venue_next            (blocks until that pass ends)
//
// Arguments are key=value (true/false/numbers parsed) or one JSON object.
// Runs the MCP as a child on this machine: the queue, rig file and tunnels are
// this machine's, so run it on the conductor (over ssh from elsewhere).
import { spawn } from "node:child_process";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
const [tool, ...rest] = process.argv.slice(2);
if (!tool) { console.error("usage: venue-cli.mjs <tool> [key=value … | '{json}']"); process.exit(2); }
let args = {};
if (rest.length === 1 && rest[0].trim().startsWith("{")) args = JSON.parse(rest[0]);
else for (const kv of rest) { const i = kv.indexOf("="); if (i < 0) continue; const k = kv.slice(0, i), v = kv.slice(i + 1); args[k] = v === "true" ? true : v === "false" ? false : (v !== "" && !isNaN(Number(v)) ? Number(v) : v); }
const child = spawn(process.execPath, [join(dirname(fileURLToPath(import.meta.url)), "ac-venue-mcp.mjs")], { stdio: ["pipe", "pipe", "inherit"] });
let buf = "";
child.stdout.on("data", (d) => { buf += d; let i; while ((i = buf.indexOf("\n")) >= 0) { const line = buf.slice(0, i); buf = buf.slice(i + 1); if (!line.startsWith("{")) continue; const m = JSON.parse(line); if (m.id === 2) { const c = m.result?.content?.[0]?.text ?? JSON.stringify(m.error ?? m); console.log(c); child.kill(); process.exit(m.result?.isError ? 1 : 0); } } });
child.stdin.write(JSON.stringify({ jsonrpc: "2.0", id: 1, method: "initialize", params: {} }) + "\n");
child.stdin.write(JSON.stringify({ jsonrpc: "2.0", id: 2, method: "tools/call", params: { name: tool, arguments: args } }) + "\n");
