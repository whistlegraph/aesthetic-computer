// chat-capabilities, 26.08.08
// GET: What the AC chat server will accept — the length cap, how it's counted,
//      whether the channel filters, the message syntax, and the bare-word
//      commands. For bots and LLMs that want the contract before they post.
//
// Query params:
//   channel — "system" | "clock" | "sotce" (also accepts the full
//             `chat-system` / host form). Omit for every channel.
//
// The authority is the session server, which enforces these on the socket and
// hands the same object to every client in its `connected` packet; both read
// shared/chat-capabilities.mjs. This route exists because a bot that only knows
// `aesthetic.computer/api/...` shouldn't have to learn the chat hostnames —
// `https://chat-clock.aesthetic.computer/chat/capabilities` is the same answer
// straight from the server.

import { respond } from "../../backend/http.mjs";
import { chatCapabilities } from "../../../shared/chat-capabilities.mjs";

const CHANNELS = {
  system: "chat-system.aesthetic.computer",
  clock: "chat-clock.aesthetic.computer",
  sotce: "chat.sotce.net",
};

const forChannel = (key) => ({
  ...chatCapabilities(`chat-${key}`),
  host: CHANNELS[key],
  socket: `wss://${CHANNELS[key]}`,
  handshake: "The `connected` packet carries this same object as `capabilities`.",
});

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, null);
  if (event.httpMethod !== "GET") return respond(405, { message: "Method Not Allowed" });

  const asked = (event.queryStringParameters?.channel || "")
    .trim()
    .replace(/^chat[-.]/, "")
    .replace(/\..*$/, "");

  if (!asked) {
    return respond(200, { channels: Object.keys(CHANNELS).map(forChannel) });
  }

  if (!CHANNELS[asked]) {
    return respond(400, {
      message: `Unknown channel "${asked}" — expected one of: ${Object.keys(CHANNELS).join(", ")}`,
    });
  }

  return respond(200, forChannel(asked));
}
