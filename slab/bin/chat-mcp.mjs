#!/usr/bin/env node
// chat-mcp.mjs — an MCP over AC chat. It lets an agent READ and POST messages in
// the AC chat channels as @jeffrey, using the same signed-in session `ac-login`
// mints for cal-mcp and DateWizard.
//
// Two channels live here (see session-server/chat-manager.mjs):
//   system → chat-system.aesthetic.computer   the main `chat` piece (/chat)
//   clock  → chat-clock.aesthetic.computer    the laer-klokken / r8dio piece
//
// Reads come over HTTP from /api/chat-messages (a 2-min Redis cache). Posts go
// over the chat WebSocket: there is no separate handshake — the token rides on
// every `chat:message`, and the server authorizes it by sending it to Auth0
// /userinfo and checking the `sub` we claim is the one it gets back. So we ask
// /userinfo for our own sub first rather than guessing it.
//
// POSTING IS PUBLIC AND NOT UNDOABLE FROM HERE. A message posted to `clock` lands
// in front of the Danish laer-klokken community; there is no delete tool in this
// server on purpose. Read before you write.
//
// Hand-rolled JSON-RPC over stdio, matching the sibling cal-mcp / prox-mcp —
// no SDK, only node builtins + the shared http-front.
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";
import { UA, whoami } from "../../toolchain/mcp/ac-token.mjs";

// `instance` is what /api/chat-messages calls a channel ("system" | "clock") —
// NOT the Mongo collection name. Passing the collection, or any unknown param,
// silently serves the default instance instead of erroring, so the read comes
// back full of plausible messages from the WRONG channel. Send `instance`.
const CHANNELS = {
  system: { host: "chat-system.aesthetic.computer", collection: "chat-system" },
  clock: { host: "chat-clock.aesthetic.computer", collection: "chat-clock" },
};
const READ_API = "https://aesthetic.computer/api/chat-messages";
const MAX_TEXT = 128; // chat-manager.mjs rejects anything longer with "too-long"

function channel(name = "system") {
  const key = String(name).replace(/^chat-/, "");
  const ch = CHANNELS[key];
  if (!ch) {
    throw new Error(
      `unknown channel "${name}" — expected one of: ${Object.keys(CHANNELS).join(", ")}`,
    );
  }
  return { key, ...ch };
}

// ── read ─────────────────────────────────────────────────────────────────────
async function toolRead(args = {}) {
  const ch = channel(args.channel);
  const limit = Math.min(Math.max(parseInt(args.limit ?? 20, 10) || 20, 1), 100);
  const q = new URLSearchParams({ instance: ch.key, limit: String(limit) });
  const res = await fetch(`${READ_API}?${q}`, {
    headers: { "User-Agent": UA, Accept: "application/json" },
    signal: AbortSignal.timeout(20_000),
  });
  if (!res.ok) throw new Error(`chat-messages → HTTP ${res.status}`);
  const body = await res.json();
  // The server echoes back which instance it actually served. If that isn't the
  // one we asked for, we're reading someone else's channel — say so rather than
  // print a convincing list of the wrong messages.
  if (body.instance && body.instance !== ch.key) {
    throw new Error(
      `asked for "${ch.key}" but the API served "${body.instance}" — refusing to show the wrong channel.`,
    );
  }
  const messages = body.messages || [];
  if (!messages.length) return [{ type: "text", text: `(no messages in ${ch.collection})` }];
  const lines = messages.map((m) => {
    const when = m.when ? new Date(m.when).toISOString().replace("T", " ").slice(0, 16) : "";
    const hearts = m.hearts ? `  ♥${m.hearts}` : "";
    return `${when}  ${m.from || "anon"}: ${m.text}${hearts}`;
  });
  return [{ type: "text", text: `${ch.collection} — last ${lines.length}:\n\n${lines.join("\n")}` }];
}

// ── post ─────────────────────────────────────────────────────────────────────
// One message, one socket: connect, send, wait for the server to echo it back (or
// tell us it refused), then close. The echo is the only confirmation the protocol
// offers, so a silent 10s means something is wrong and we say so.
async function toolSend(args = {}) {
  const text = String(args.text ?? "").trim();
  if (!text) throw new Error("`text` is required.");
  // The server hard-caps at 128 (chat-manager.mjs) and answers "too-long" — catch
  // it here so a long message fails with its own length rather than a round trip.
  if (text.length > MAX_TEXT) {
    throw new Error(
      `message is ${text.length} chars; the chat server caps at ${MAX_TEXT}. Shorten it (a long asset URL eats most of the budget).`,
    );
  }
  const ch = channel(args.channel);
  const { sub, access_token } = await whoami();

  return await new Promise((resolve, reject) => {
    const ws = new WebSocket(`wss://${ch.host}`);
    const done = (fn, v) => {
      clearTimeout(timer);
      try { ws.close(); } catch {}
      fn(v);
    };
    const timer = setTimeout(
      () => done(reject, new Error(`timed out waiting for ${ch.collection} to accept the message`)),
      15_000,
    );

    ws.onerror = () => done(reject, new Error(`could not reach ${ch.host}`));

    ws.onopen = () => {
      ws.send(
        JSON.stringify({
          type: "chat:message",
          content: { text, sub, token: access_token },
        }),
      );
    };

    ws.onmessage = (event) => {
      let msg;
      try {
        msg = JSON.parse(event.data);
      } catch {
        return;
      }
      // pack() JSON-stringifies content into a string before sending, so every
      // frame needs a second parse. Reading fields straight off msg.content
      // silently yields undefined — which reads as a hang, not an error.
      let content = msg.content;
      if (typeof content === "string") {
        try {
          content = JSON.parse(content);
        } catch {
          content = {};
        }
      }

      if (msg.type === "unauthorized") {
        done(reject, new Error("chat server rejected the token — run `ac-login`."));
      } else if (msg.type === "muted") {
        done(reject, new Error("this user is muted in that channel."));
      } else if (msg.type === "too-long") {
        done(reject, new Error(content?.message || `over the ${MAX_TEXT}-char limit.`));
      } else if (msg.type === "message" && content?.sub === sub && content?.text === text) {
        // The server broadcasts the stored message back — that's the receipt.
        const who = content.handle ? `@${content.handle}` : sub;
        done(resolve, [
          { type: "text", text: `posted to ${ch.collection} as ${who}:\n\n  ${content.text}` },
        ]);
      }
    };
  });
}

async function toolChannels() {
  const rows = Object.entries(CHANNELS).map(
    ([key, ch]) => `• ${key.padEnd(7)} ${ch.host}  (collection: ${ch.collection})`,
  );
  return [{ type: "text", text: `AC chat channels:\n\n${rows.join("\n")}` }];
}

// ── tools ────────────────────────────────────────────────────────────────────
const TOOLS = [
  {
    name: "chat_read",
    description:
      "Read the most recent messages in an AC chat channel. `system` is the main /chat piece; `clock` is the laer-klokken / r8dio channel.",
    inputSchema: {
      type: "object",
      properties: {
        channel: { type: "string", description: '"system" (default) or "clock".' },
        limit: { type: "number", description: "How many recent messages (1–100, default 20)." },
      },
    },
  },
  {
    name: "chat_send",
    description:
      "Post a message to an AC chat channel AS @jeffrey, using his signed-in session. PUBLIC AND PERMANENT — it appears immediately to everyone in the channel and this server has no delete. Confirm the exact wording with him before calling.",
    inputSchema: {
      type: "object",
      properties: {
        text: { type: "string", description: "The message to post, verbatim." },
        channel: { type: "string", description: '"system" (default) or "clock".' },
      },
      required: ["text"],
    },
  },
  {
    name: "chat_channels",
    description: "List the AC chat channels this server can read and post to.",
    inputSchema: { type: "object", properties: {} },
  },
];

async function callTool(name, args) {
  switch (name) {
    case "chat_read": return toolRead(args || {});
    case "chat_send": return toolSend(args || {});
    case "chat_channels": return toolChannels();
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
            serverInfo: { name: "chat-mcp", version: "1.0.0" },
          },
        };
      case "initialized":
      case "notifications/initialized":
        return null;
      case "ping":
        return { jsonrpc: "2.0", id, result: {} };
      case "tools/list":
        return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default:
        return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 7775);
if (port) serveHttp({ handleMessage, port, banner: "💬 chat shared daemon" });
else serveStdio({ handleMessage, banner: "💬 chat started (chat_read, chat_send, chat_channels)" });
