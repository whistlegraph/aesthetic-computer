// tell, 26.04.23 → 26.09.11 (send path moved into `backend/mail.mjs`)
// Send a one-way "tell" from one AC user to another. The recipient gets a push
// notification on every registered device — or just one, when `device` names a
// deviceId or label — and the message lands in their mail.
//
// POST /api/tell
//   body: { to: "@handle" | "ac25namuc", text: "message", device?: "id-or-label" }
//   headers: Authorization: Bearer <Auth0 token>

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { clean, deliver, subFromAddress } from "../../backend/mail.mjs";

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

  if (!body.to) return respond(400, { message: "Missing recipient" });
  const text = clean(body.text);
  if (!text) return respond(400, { message: "Empty message" });

  const sender = await authorize(event.headers);
  if (!sender?.sub) return respond(401, { message: "Unauthorized" });

  const database = await connect();
  try {
    const to = await subFromAddress(body.to, database);
    if (!to) return respond(404, { message: "Recipient not found" });

    const told = await deliver(
      { from: sender.sub, to, text, device: body.device, verb: "told" },
      database,
    );

    return respond(200, {
      status: "told",
      to: told.toHandle,
      when: told.when,
      push: told.push,
    });
  } catch (err) {
    console.error("🔴 tell error:", err);
    return respond(500, { message: err?.message || "Server error" });
  } finally {
    await database.disconnect();
  }
}
