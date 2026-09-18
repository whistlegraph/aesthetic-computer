// tell, 26.04.23 → 26.09.11 (send path moved into `backend/mail.mjs`)
// Send a one-way "tell" from one AC user to another. The recipient gets a push
// notification on every registered device — or just one, when `device` names a
// deviceId or label — and the message lands in their mail.
//
// POST /api/tell
//   body: { to: "@handle" | "ac25namuc", text: "message", device?: "id-or-label" }
//   headers: Authorization: Bearer <Auth0 token>

import { mailTrace, recordMailEvent } from "../../backend/mail-events.mjs";
import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { clean, deliver, subFromAddress } from "../../backend/mail.mjs";
import { mailErrorCode } from "../../../shared/mail-privacy.mjs";

export async function handler(event) {
  const context = { trace: mailTrace(), action: "send" };
  const started = Date.now();
  let response;
  try { response = await handleTell(event, context); }
  catch (err) {
    recordMailEvent(context.database, { event: "failed", transport: "tell", trace: context.trace, error: mailErrorCode(err) });
    response = respond(500, { message: "Could not complete mail request" });
  }
  const status = response.statusCode;
  recordMailEvent(context.database, { event: "request", transport: "tell", trace: context.trace,
    action: context.action, status, durationMs: Date.now() - started,
    reason: status === 401 ? "unauthorized" : status === 400 ? "invalid" : status === 404 ? "not_found" : status === 405 ? "method" : status >= 500 ? "request" : undefined });
  return { ...response, headers: { ...response.headers, "X-Mail-Trace": context.trace } };
}

async function handleTell(event, context) {
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
  context.database = database;
  try {
    const to = await subFromAddress(body.to, database);
    if (!to) return respond(404, { message: "Recipient not found" });

    const told = await deliver(
      { trace: context.trace, from: sender.sub, to, text, device: body.device, verb: "told" },
      database,
    );

    return respond(200, {
      status: "told",
      to: told.toHandle,
      when: told.when,
      push: told.push,
      nudge: told.nudge,
    });
  } catch (err) {
    recordMailEvent(database, { event: "failed", transport: "tell", trace: context.trace, error: mailErrorCode(err) });
    return respond(500, { message: "Could not send letter" });
  } finally {
    await database.disconnect();
  }
}
