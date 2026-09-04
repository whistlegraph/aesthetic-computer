#!/usr/bin/env node
// acin-mcp.mjs — "claude in" to aesthetic.computer.
//
// One front door for agent presence: attach this session as an agent to a
// handle's presence room (session-server /agent-presence) so every AC surface
// signed in as that handle lights its linked-agent mark, and optionally open
// a Chrome window already pointed at aesthetic.computer — clauded in from the
// first frame. The socket lives as long as this MCP process does, so presence
// holds for the whole Claude session and drops cleanly when it ends.

import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";

const pexec = promisify(execFile);
const RELAY =
  process.env.AC_PRESENCE_RELAY || "wss://session-server.aesthetic.computer";
const DEFAULT_HANDLE = process.env.AC_HANDLE || "jeffrey";
const DEFAULT_URL = "https://aesthetic.computer/prompt";

// One attachment at a time — acin_in to a new room supersedes the old one.
const state = {
  socket: null,
  room: null,
  label: null,
  lastStatus: null,
  connectedAt: null,
};

function canonicalRoom(value) {
  const name = String(value || "").toLowerCase().replace(/^@/, "");
  return /^[a-z0-9_-]{1,32}$/.test(name) ? name : null;
}

function detach() {
  if (state.socket) {
    try { state.socket.close(); } catch { /* already gone */ }
  }
  state.socket = null;
  state.room = null;
  state.label = null;
  state.lastStatus = null;
  state.connectedAt = null;
}

// Attach and resolve on the first status frame, so the caller learns how many
// surfaces saw the mark light before the tool returns.
function attach(room, label) {
  return new Promise((resolvePromise, reject) => {
    const url = `${RELAY}/agent-presence?room=${room}&role=agent&label=${
      encodeURIComponent(label)}`;
    const socket = new WebSocket(url);
    const timeout = setTimeout(() => {
      try { socket.close(); } catch { /* unreachable relay */ }
      reject(new Error("Presence relay did not answer within 10s"));
    }, 10000);

    socket.onmessage = (event) => {
      let message;
      try { message = JSON.parse(event.data); } catch { return; }
      if (message.type === "agent-presence:error") {
        clearTimeout(timeout);
        reject(new Error(message.content?.message || "Relay refused"));
        return;
      }
      if (message.type !== "agent-presence:status") return;
      state.lastStatus = message.content;
      if (state.socket !== socket) {
        // First frame: adopt this socket as the live attachment.
        detach();
        state.socket = socket;
        state.room = room;
        state.label = label;
        state.connectedAt = new Date().toISOString();
        clearTimeout(timeout);
        resolvePromise(message.content);
      }
    };
    socket.onclose = () => {
      if (state.socket === socket) detach();
    };
    socket.onerror = () => {
      clearTimeout(timeout);
      try { socket.close(); } catch { /* already closing */ }
      reject(new Error("Could not reach the presence relay"));
    };
  });
}

async function openChrome(url) {
  await pexec("open", ["-a", "Google Chrome", url]);
}

function text(value) {
  return [{ type: "text", text: typeof value === "string"
    ? value : JSON.stringify(value, null, 2) }];
}

async function toolIn(args) {
  const room = canonicalRoom(args.handle || DEFAULT_HANDLE);
  if (!room) throw new Error("Invalid handle");
  const label = /^[a-z0-9 _-]{1,24}$/i.test(args.label || "")
    ? args.label : "claude";
  const status = await attach(room, label);
  let opened = null;
  if (args.open !== false) {
    const url = args.url || DEFAULT_URL;
    await openChrome(url);
    opened = url;
  }
  return text({
    linked: true,
    room: `@${room}`,
    label,
    surfaces: status.surfaces,
    agents: status.agents,
    opened,
    note: opened
      ? "Chrome window opened; the linked-agent mark lights bottom-right once the surface signs in and joins the room."
      : "Attached without opening a window.",
  });
}

function toolStatus() {
  if (!state.socket) return text({ linked: false });
  return text({
    linked: true,
    room: `@${state.room}`,
    label: state.label,
    connectedAt: state.connectedAt,
    lastStatus: state.lastStatus,
  });
}

function toolOut() {
  if (!state.socket) return text({ linked: false, note: "Nothing to detach." });
  const room = state.room;
  detach();
  return text({ linked: false, note: `Detached from @${room}; the mark goes dark on every surface.` });
}

const TOOLS = [
  {
    name: "acin_in",
    description:
      "Claude in: attach this session as an agent to an aesthetic.computer " +
      "handle's presence room (lights the linked-agent mark bottom-right on " +
      "every AC surface signed in as that handle) and open a Chrome window " +
      "on aesthetic.computer, already clauded in. Presence holds until " +
      "acin_out or the session ends.",
    inputSchema: {
      type: "object",
      properties: {
        handle: { type: "string", description: "Handle whose room to join (default @jeffrey)" },
        label: { type: "string", description: "Agent label shown in the mark tooltip (default claude)" },
        open: { type: "boolean", description: "Open a Chrome window (default true)" },
        url: { type: "string", description: "URL to open (default https://aesthetic.computer/prompt)" },
      },
    },
  },
  {
    name: "acin_status",
    description: "Report the current claude-in attachment: room, label, and the latest presence counts (surfaces watching, agents linked).",
    inputSchema: { type: "object", properties: {} },
  },
  {
    name: "acin_out",
    description: "Claude out: detach from the presence room so the linked-agent mark goes dark. Does not close any Chrome window.",
    inputSchema: { type: "object", properties: {} },
  },
];

async function callTool(name, args) {
  switch (name) {
    case "acin_in": return toolIn(args || {});
    case "acin_status": return toolStatus();
    case "acin_out": return toolOut();
    default: throw new Error(`Unknown tool: ${name}`);
  }
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "acin-mcp", version: "1.0.0" },
            instructions:
              "Claude in / claude out of aesthetic.computer. acin_in attaches " +
              "agent presence to a handle's room and opens Chrome on AC; the " +
              "linked-agent mark shows bottom-right while attached. Presence " +
              "drops when this session ends.",
          },
        };
      case "initialized":
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
if (port) serveHttp({ handleMessage, port, banner: "🛰️ acin-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "🛰️ acin-mcp started (acin_in, acin_status, acin_out)" });
