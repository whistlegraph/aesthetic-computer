// Slash, 23.10.13.00.08
// A helpful Discord webhook for auto-linking AC pieces.

// TODO: Test via the Netlify logs.

import { respond } from "../../backend/http.mjs";
import {
  verifyKey,
  InteractionResponseType,
  InteractionType,
} from "discord-interactions";

export async function handler(event) {
  const timestamp = event.headers["X-Signature-Timestamp"];
  const signature = event.headers["X-Signature-Ed25519"];

  console.log("Timestamp:", timestamp, "Signature:", signature);

  if (!timestamp && !signature)
    return respond(500, { message: "😈 Unauthorized." });

  const isValidRequest = verifyKey(
    Buffer.from(event.body, "utf8"),
    signature,
    timestamp,
    process.env.DISCORD_PAL_PUBLIC,
  );

  console.log("Valid request:", isValidRequest);

  if (!isValidRequest) {
    return respond(401, { message: "😫 Invalid request signature." });
  }

  const body = JSON.parse(event.body);

  console.log("Body:", body);

  if (body.type === InteractionType.PING) {
    return respond(200, { type: InteractionResponseType.PONG });
  }

  if (body.type === InteractionType.APPLICATION_COMMAND) {
    return respond(200, {
      type: InteractionResponseType.CHANNEL_MESSAGE_WITH_SOURCE,
      data: { content: "hello world 🗺️" },
    });
  }

  return respond(400, { message: "🫠 Unhandled interaction type." });
}
