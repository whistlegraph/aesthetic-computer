// push, 26.06.11
// Self-notify: send a push to your own registered device(s) — ntfy-style.
// Useful from pieces, scripts, and the CLI (e.g. "ping my phone when the
// render finishes").
//
// POST /api/push
//   body: {
//     body: string,            // notification text (required)
//     title?: string,          // defaults to "aesthetic.computer"
//     device?: string,         // deviceId (exact) or label substring;
//                              // omit to hit every registered device
//     piece?: string,          // piece to open on tap, e.g. "chat"
//   }
//   headers: Authorization: Bearer <Auth0 token>
//   → { status: "pushed", push: { attempted, succeeded, failed, pruned } }

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { sendToUser } from "../../../shared/push.mjs";

const MAX_TEXT = 500;

export async function handler(event) {
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return respond(400, { message: "Invalid JSON body" });
  }

  const text = typeof body.body === "string" ? body.body.trim().slice(0, MAX_TEXT) : "";
  if (!text) return respond(400, { message: "Empty body" });
  const title =
    typeof body.title === "string" && body.title.trim()
      ? body.title.trim().slice(0, 120)
      : "aesthetic.computer";

  const user = await authorize(event.headers);
  if (!user?.sub) return respond(401, { message: "Unauthorized" });

  const database = await connect();
  try {
    const note = { title, body: text, data: { kind: "push" } };
    if (typeof body.piece === "string" && body.piece) {
      note.data.piece = body.piece.slice(0, 128);
    }
    const push = await sendToUser(database.db, user.sub, note, {
      device: typeof body.device === "string" ? body.device : undefined,
    });
    if (push.attempted === 0) {
      return respond(404, {
        message: body.device
          ? `No device matching "${body.device}"`
          : "No registered devices",
      });
    }
    return respond(200, { status: "pushed", push });
  } catch (err) {
    console.error("🔴 push error:", err);
    return respond(500, { message: err?.message || "Server error" });
  } finally {
    await database.disconnect();
  }
}
