// mail, 26.09.11
// The inbox half of AC mail. Sending already existed as `tell`; nothing ever
// read what it wrote, so this is the other end.
//
// GET  /api/mail                       → inbox, sent, unread count, addresses
// POST /api/mail { to, text }          → send
// POST /api/mail { action: "read" }    → mark all read (or one, with `id`)
//   headers: Authorization: Bearer <Auth0 token>

import { mailTrace, recordMailEvent } from "../../backend/mail-events.mjs";
import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond as httpRespond } from "../../backend/http.mjs";
import { attachmentList, attachmentThumbnail, resolveMailMedia } from "../../backend/mail-media.mjs";
import {
  addressesFor,
  clean,
  deliver,
  mailbox,
  MAX_SUBJECT_LENGTH,
  sendOutside,
  subFromAddress,
} from "../../backend/mail.mjs";
import { ObjectId } from "mongodb";
import { mailErrorCode } from "../../../shared/mail-privacy.mjs";

const PAGE = 50;
const respond = (status, body, headers = {}) => httpRespond(status, body, { "Cache-Control": "private, no-store", ...headers });
const NO_FILES = { projection: { "attachments.data": 0 } };

export async function handler(event) {
  const context = { trace: mailTrace(), action: (event.httpMethod === "GET" ? (event.queryStringParameters?.attachment !== undefined ? "download" : event.queryStringParameters?.count !== undefined ? "count" : "inbox") : "send") };
  const started = Date.now();
  let response;
  try { response = await handleMail(event, context); }
  catch (err) {
    recordMailEvent(context.database, { event: "failed", transport: "api", trace: context.trace, error: mailErrorCode(err) });
    response = respond(500, { message: "Could not complete mail request" });
  }
  const status = response.statusCode;
  // Count polls are frequent; keep failures, writes, inbox and file reads.
  if (!(event.httpMethod === "GET" && event.queryStringParameters?.count !== undefined && status === 200) && event.httpMethod !== "OPTIONS") {
    recordMailEvent(context.database, { event: "request", transport: "api", trace: context.trace,
      action: context.action, status, durationMs: Date.now() - started,
      reason: status === 401 ? "unauthorized" : status === 400 ? "invalid" : status === 404 ? "not_found" : status === 405 ? "method" : status >= 500 ? "request" : undefined });
  }
  return { ...response, headers: { ...response.headers, "X-Mail-Trace": context.trace } };
}

async function handleMail(event, context) {
  if (event.httpMethod === "OPTIONS") return respond(200, {});
  if (event.httpMethod !== "GET" && event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  const user = await authorize(event.headers);
  if (!user?.sub) return respond(401, { message: "unauthorized" });

  const database = await connect();
  context.database = database;
  try {
    const tells = await mailbox(database);

    if (event.httpMethod === "GET") {
      const query = event.queryStringParameters || {};
      if (query.attachment !== undefined) {
        if (!/^[a-f\d]{24}$/i.test(query.id || "") || !/^\d{1,2}$/.test(query.attachment)) {
          return respond(400, { message: "Invalid attachment" });
        }
        // Authorize against the letter, never just a guessable file index.
        const letter = await tells.findOne({
          _id: new ObjectId(query.id), $or: [{ to: user.sub }, { from: user.sub }],
        }, { projection: { attachments: 1 } });
        const file = letter?.attachments?.[Number(query.attachment)];
        if (!file) return respond(404, { message: "Attachment not found" });
        if (query.preview !== undefined) {
          const preview = await attachmentThumbnail(file);
          return preview ? respond(200, preview) : respond(404, { message: "Preview unavailable" });
        }
        if (query.json !== undefined) return respond(200, { name: file.name, type: file.type, data: file.data });
        return {
          ...respond(200, file.data, {
            "Content-Type": "application/octet-stream",
            "Content-Disposition": `attachment; filename="file"; filename*=UTF-8''${encodeURIComponent(file.name).replace(/'/g, "%27")}`,
            "X-Content-Type-Options": "nosniff",
            "Content-Security-Policy": "sandbox",
          }),
          isBase64Encoded: true,
        };
      }
      // `?count` is the cheap one — the prompt asks it on every boot just to
      // know whether to draw the envelope.
      if (event.queryStringParameters?.count !== undefined) {
        const [unread, total] = await Promise.all([
          tells.countDocuments({ to: user.sub, read: { $ne: true } }),
          tells.countDocuments({ to: user.sub }),
        ]);
        return respond(200, { unread, total });
      }

      const [inbox, sent, unread, addresses] = await Promise.all([
        tells.find({ to: user.sub }, NO_FILES).sort({ when: -1 }).limit(PAGE).toArray(),
        tells.find({ from: user.sub }, NO_FILES).sort({ when: -1 }).limit(PAGE).toArray(),
        tells.countDocuments({ to: user.sub, read: { $ne: true } }),
        addressesFor(user.sub, database),
      ]);

      const media = new Map();
      // Cache repeated text within this request, and recheck media visibility
      // on each inbox read rather than persisting public preview URLs.
      const references = (text) => {
        if (!media.has(text)) media.set(text, resolveMailMedia(text, database));
        return media.get(text);
      };
      return respond(200, {
        addresses,
        unread,
        inbox: await Promise.all(inbox.map(async (m) => ({
          id: m._id,
          from: m.fromHandle,
          fromEmail: m.fromEmail || null, // set when the letter came from outside
          auth: m.auth || null, // what Google's gate found out about that sender
          subject: m.subject || null,
          text: m.text,
          when: m.when,
          read: m.read === true,
          attachments: attachmentList(m.attachments),
          media: await references(m.text),
        }))),
        sent: await Promise.all(sent.map(async (m) => ({
          id: m._id,
          to: m.toHandle,
          toEmail: m.toEmail || null, // set when the letter left the wall
          subject: m.subject || null,
          text: m.text,
          when: m.when,
          attachments: attachmentList(m.attachments),
          media: await references(m.text),
        }))),
      });
    }

    let body;
    try {
      body = JSON.parse(event.body || "{}");
    } catch {
      return respond(400, { message: "Invalid JSON body" });
    }

    if (body.action === "read") {
      context.action = "read";
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
    if (!to) {
      // Not a handle, and not an email anyone here signed up with: if it
      // is an address at all, the letter leaves the wall as real email.
      const toEmail = body.to.trim().toLowerCase();
      if (!/^[^@\s]+@[^@\s]+\.[^@\s]+$/.test(toEmail)) {
        return respond(404, { message: "Recipient not found" });
      }
      const sent = await sendOutside(
        { trace: context.trace, from: user.sub, toEmail, subject, text },
        database,
      );
      return respond(200, {
        status: "mailed",
        to: sent.toHandle,
        when: sent.when,
        outside: true,
      });
    }

    const sentMail = await deliver(
      { trace: context.trace, from: user.sub, to, text, subject, device: body.device },
      database,
    );

    return respond(200, {
      status: "mailed",
      to: sentMail.toHandle,
      when: sentMail.when,
      push: sentMail.push,
      nudge: sentMail.nudge,
    });
  } catch (err) {
    recordMailEvent(database, { event: "failed", transport: "api", trace: context.trace, error: mailErrorCode(err) });
    return respond(500, { message: "Could not complete mail request" });
  } finally {
    await database.disconnect();
  }
}
