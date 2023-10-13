// Slash, 23.10.13.00.08
// A helpful Discord webhook for auto-linking AC pieces.

import { respond } from "../../backend/http.mjs";
import { verifyKey } from "discord-interactions";

export async function handler(event) {
  const timestamp = event.headers["X-Signature-Timestamp"];
  const signature = event.headers["X-Signature-Ed25519"];

  if (!timestamp && !signature)
    return respond(500, { message: "😈 Unauthorized." });

  const isValidRequest = verifyKey(
    buf,
    signature,
    timestamp,
    process.env.DISCORD_PAL_PUBLIC,
  );

  if (!isValidRequest) {
    return respond(401, {
      type: 4,
      data: { content: "😫 Invalid request signature." },
    });
  }

  return respond(200, {
    type: 4, // a message response
    data: { content: "hello world 🗺️" },
  });
}
