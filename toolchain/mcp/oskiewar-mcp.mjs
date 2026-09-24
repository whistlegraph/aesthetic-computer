#!/usr/bin/env node
// oskiewar-mcp.mjs — the room's displays: the Xbox running Oskiewar and the
// ac7 AC OS screen, as seen from the conductor.
//
// Two real services stand behind these tools. blueberry:8796 is the display
// feed (one transport at a time: what is playing, how far in, the lyric being
// sung). neo runs the Oskiewar stage service (xbox/tools/oskiewar-stage-mcp.mjs,
// launchd `computer.aesthetic.oskiewar-stage`, its own MCP on neo's loopback
// :7792) which polls that feed and relays it to the Xbox and ac7. The Trio
// runner already heartbeats the feed while a piece plays; these tools let a
// session look at the displays, put words on them by hand, clear them, and
// curtain, switch or restart the stage — the same calls the venue MCP makes.
import { spawn } from "node:child_process";
import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";
import { FEED, MEMBER_RGB, feedStatus, postTransport, showLyric, clearFeed } from "./oskiewar-feed.mjs";

const STAGE_HOST = process.env.OSKIEWAR_STAGE_HOST || "neo";
const STAGE_PORT = process.env.OSKIEWAR_STAGE_PORT || "7792";
const STAGE_LOG = "/Users/jas/.ac-os/oskiewar-stage.log";
const STAGE_LABEL = "computer.aesthetic.oskiewar-stage";
const AC7 = process.env.OSKIEWAR_AC7 || "http://192.168.1.245";
const text = (s) => [{ type: "text", text: typeof s === "string" ? s : JSON.stringify(s, null, 2) }];

function ssh(script, ms = 12000) {
  return new Promise((done) => {
    const child = spawn("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=6", STAGE_HOST, "bash -s"]);
    let out = "", err = ""; const t = setTimeout(() => child.kill(), ms);
    child.stdout.on("data", (d) => (out += d)); child.stderr.on("data", (d) => (err += d));
    child.on("close", (code) => { clearTimeout(t); done({ code, out, err }); });
    child.stdin.end(script + "\n");
  });
}
// neo's stage MCP listens on neo's loopback only: call it through ssh + curl.
async function stageCall(name, args = {}) {
  const rpc = JSON.stringify({ jsonrpc: "2.0", id: 1, method: "tools/call", params: { name, arguments: args } }).replace(/'/g, "'\\''");
  const r = await ssh(`curl -s --max-time 8 -X POST http://127.0.0.1:${STAGE_PORT}/ -H 'Content-Type: application/json' -d '${rpc}'`);
  try { const m = JSON.parse(r.out); const c = m.result?.content?.[0]?.text; try { return JSON.parse(c); } catch { return c ?? m.error ?? m; } }
  catch { return { unreachable: (r.err || r.out || "no reply").trim().slice(0, 200) }; }
}
async function status() {
  const [feed, stage, proc, log, ac7] = await Promise.all([
    feedStatus(),
    stageCall("oskiewar_performance_status"),
    ssh(`pgrep -fl oskiewar-stage-mcp | head -1; launchctl print gui/$(id -u)/${STAGE_LABEL} 2>/dev/null | grep -E "state|pid" | head -2`),
    ssh(`tail -n 6 ${STAGE_LOG} 2>/dev/null | cut -c1-200`),
    (async () => { try { const r = await fetch(`${AC7}/status`, { signal: AbortSignal.timeout(2500) }); const j = await r.json(); return { reachable: true, piece: j.piece, name: j.name }; } catch (e) { return { reachable: false }; } })(),
  ]);
  return { feed: { url: FEED, ...feed }, stage: { host: STAGE_HOST, process: proc.out.trim().split("\n").filter(Boolean), performance: stage, logTail: log.out.trim().split("\n").filter(Boolean) }, ac7: { url: AC7, ...ac7 } };
}
const TOOLS = [
  { name: "oskiewar_status", description: "The displays right now: the feed on blueberry:8796 (reachable, what it is showing: title, dance, elapsed, lyric), neo's Oskiewar stage service (process, its own performance/curtain/display readout, log tail), and whether ac7 answers.", inputSchema: { type: "object", properties: {} } },
  { name: "oskiewar_transport", description: "POST one transport to the display feed, in the Trio runner's shape: playing, elapsed, title, dance ('trio-round-v1' or 'femrag-round-v1'), bpm, duration, lyric {text, member, rgb, t, dur}, next {text, member, rgb, in}. The feed forgets it after 1.25 s unless repeated.", inputSchema: { type: "object", properties: { playing: { type: "boolean" }, elapsed: { type: "number" }, title: { type: "string" }, dance: { type: "string" }, bpm: { type: "number" }, duration: { type: "number" }, lyric: { type: "object" }, next: { type: "object" } }, required: ["playing", "elapsed"] } },
  { name: "oskiewar_lyric", description: "Put words on the displays by hand: a playing transport carrying `text` in the member's colour (neo, blueberry, frisbee, or an rgb), an optional `next` line, kept fresh with heartbeats for `seconds` (default 4), then cleared. Blocks for that long.", inputSchema: { type: "object", properties: { text: { type: "string" }, member: { type: "string" }, rgb: { type: "array", items: { type: "number" } }, next: { type: "object" }, seconds: { type: "number" }, title: { type: "string" } }, required: ["text"] } },
  { name: "oskiewar_clear", description: "Tell the feed nothing is playing (playing:false); the stage falls back to its own music source or idle.", inputSchema: { type: "object", properties: {} } },
  { name: "oskiewar_stage", description: "Neo's Oskiewar stage service: `log` (tail its log), `restart` (launchctl kickstart of computer.aesthetic.oskiewar-stage; use only between pieces), `performance_status`, `curtain` (args as the stage MCP's oskiewar_curtain), `display` (args as its oskiewar_display), `tools` (list the stage MCP's own tools).", inputSchema: { type: "object", properties: { action: { type: "string", enum: ["log", "restart", "performance_status", "curtain", "display", "tools"] }, args: { type: "object" }, lines: { type: "number" } }, required: ["action"] } },
];
async function callTool(name, args = {}) {
  switch (name) {
    case "oskiewar_status": return text(await status());
    case "oskiewar_transport": { const body = { ...args }; if (body.lyric && !body.lyric.rgb && body.lyric.member) body.lyric.rgb = MEMBER_RGB[body.lyric.member]; return text(await postTransport(body)); }
    case "oskiewar_lyric": { const r = await showLyric(args); return text({ ...r, showed: args.text, member: args.member || "neo", then: "cleared" }); }
    case "oskiewar_clear": return text(await clearFeed());
    case "oskiewar_stage": {
      switch (args.action) {
        case "log": { const r = await ssh(`tail -n ${args.lines || 40} ${STAGE_LOG} 2>/dev/null`); return text(r.out.trim() || r.err.trim() || "(empty)"); }
        case "restart": { const r = await ssh(`launchctl kickstart -k gui/$(id -u)/${STAGE_LABEL} && sleep 2 && pgrep -fl oskiewar-stage-mcp | head -1`); return text({ restarted: r.code === 0, detail: (r.out || r.err).trim() }); }
        case "tools": { const r = await ssh(`curl -s --max-time 6 -X POST http://127.0.0.1:${STAGE_PORT}/ -H 'Content-Type: application/json' -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'`); try { return text(JSON.parse(r.out).result.tools.map((t) => ({ name: t.name, description: t.description }))); } catch { return text(r.out || r.err); } }
        case "performance_status": return text(await stageCall("oskiewar_performance_status", args.args || {}));
        case "curtain": return text(await stageCall("oskiewar_curtain", args.args || {}));
        case "display": return text(await stageCall("oskiewar_display", args.args || {}));
        default: throw new Error(`unknown stage action ${args.action}`);
      }
    }
    default: throw new Error(`Unknown tool: ${name}`);
  }
}
async function handleMessage(msg) {
  const { id, method, params } = msg;
  try {
    switch (method) {
      case "initialize": return { jsonrpc: "2.0", id, result: { protocolVersion: params?.protocolVersion || "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "oskiewar-mcp", version: "1.0.0" },
        instructions: "The room's displays: the Xbox running Oskiewar and the ac7 AC OS screen. oskiewar_status shows what the feed (blueberry:8796) is showing and how neo's stage service is doing; oskiewar_lyric puts words up by hand; oskiewar_transport posts a raw transport; oskiewar_clear stops it; oskiewar_stage curtains, switches, restarts or reads the stage service. The venue MCP's runs feed the same displays automatically." } };
      case "initialized": case "notifications/initialized": return null;
      case "ping": return { jsonrpc: "2.0", id, result: {} };
      case "tools/list": return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": return { jsonrpc: "2.0", id, result: { content: await callTool(params?.name, params?.arguments) } };
      default: return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}
const port = httpPort(process.argv, 0);
if (port) serveHttp({ handleMessage, port, banner: "📺 oskiewar-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "📺 oskiewar-mcp started (oskiewar_status, oskiewar_transport, oskiewar_lyric, oskiewar_clear, oskiewar_stage)" });
