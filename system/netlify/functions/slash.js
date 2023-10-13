// Slash, 23.10.13.00.08
// A helpful Discord webhook for auto-linking AC pieces.

import { createHmac } from "crypto";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  const timestamp = event.headers["X-Signature-Timestamp"];
  const discordSignature = event.headers["X-Signature-Ed25519"];

  if (!timestamp && !discordSignature)
    return respond(500, { message: "😈 Unauthorized." });

  const signature = createHmac("sha256", process.env.DISCORD_PAL_PUBLIC)
    .update(timestamp + (await event.text()))
    .digest("hex");

  if (discordSignature !== signature) {
    return respond(200, {
      type: 4,
      data: { content: "😫 Invalid request signature." },
    });
  }

  return respond(200, {
    type: 4, // a message response
    data: { content: "hello world 🗺️" },
  });
}
