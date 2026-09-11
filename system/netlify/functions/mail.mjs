// mail, 26.09.11
// The inbox half of AC mail. Sending already existed as `tell`; nothing ever
// read what it wrote, so this is the other end.
//
// GET  /api/mail                       → inbox, sent, unread count, addresses
// POST /api/mail { to, text }          → send
// POST /api/mail { action: "read" }    → mark all read (or one, with `id`)
//   headers: Authorization: Bearer <Auth0 token>

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import {
  addressesFor,
  clean,
  deliver,
  mailbox,
  MAX_SUBJECT_LENGTH,
  subFromAddress,
} from "../../backend/mail.mjs";
import { ObjectId } from "mongodb";

const PAGE = 50;

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(200, {});
  if (event.httpMethod !== "GET" && event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  const user = await authorize(event.headers);
  if (!user?.sub) return respond(401, { message: "unauthorized" });

  const database = await connect();
  try {
    const tells = await mailbox(database);

    if (event.httpMethod === "GET") {
      const [inbox, sent, unread, addresses] = await Promise.all([
        tells.find({ to: user.sub }).sort({ when: -1 }).limit(PAGE).toArray(),
        tells.find({ from: user.sub }).sort({ when: -1 }).limit(PAGE).toArray(),
        tells.countDocuments({ to: user.sub, read: { $ne: true } }),
        addressesFor(user.sub, database),
      ]);

      return respond(200, {
        addresses,
        unread,
        inbox: inbox.map((m) => ({
          id: m._id,
          from: m.fromHandle,
          subject: m.subject || null,
          text: m.text,
          when: m.when,
          read: m.read === true,
        })),
        sent: sent.map((m) => ({
          id: m._id,
          to: m.toHandle,
          subject: m.subject || null,
          text: m.text,
          when: m.when,
        })),
      });
    }

    let body;
    try {
      body = JSON.parse(event.body || "{}");
    } catch {
      return respond(400, { message: "Invalid JSON body" });
    }

    if (body.action === "read") {
      const where = { to: user.sub, read: { $ne: true } };
      if (body.id) where._id = new ObjectId(body.id);
      const result = await tells.updateMany(where, {
        $set: { read: true, readAt: new Date() },
      });
      return respond(200, { read: result.modifiedCount });
    }

    const text = clean(body.text);
    const subject = clean(body.subject, MAX_SUBJECT_LENGTH);
    if (!body.to) return respond(400, { message: "Missing recipient" });
    if (!text) return respond(400, { message: "Empty message" });

    const to = await subFromAddress(body.to, database);
    if (!to) return respond(404, { message: "Recipient not found" });

    const sentMail = await deliver(
      { from: user.sub, to, text, subject, device: body.device },
      database,
    );

    return respond(200, {
      status: "mailed",
      to: sentMail.toHandle,
      when: sentMail.when,
      push: sentMail.push,
    });
  } catch (err) {
    console.error("🔴 mail error:", err);
    return respond(500, { message: err?.message || "Server error" });
  } finally {
    await database.disconnect();
  }
}
