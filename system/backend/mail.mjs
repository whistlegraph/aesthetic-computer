// mail, 26.09.11
// AC mail: internal letters and the inbound/outbound SMTP bridge share `tells`.
//
// Addressing: a message is filed against the recipient's `sub`, never a handle,
// so a rename can't orphan a thread. Two spellings reach a person — their
// permahandle (`ac25namuc`, permanent) and their @handle (follows the rename).
// The permahandle is canonical; the @handle is an alias over it.

import { observeMail } from "./mail-events.mjs";
import { handleFor, userIDFromHandleOrEmail } from "./authorization.mjs";
import { filter } from "./filter.mjs";
import { sendToUser } from "../../shared/push.mjs";
import { letterNotification, mailErrorCode, quietMailPush } from "../../shared/mail-privacy.mjs";
import { resolveMailMedia, outsideMediaBody } from "./mail-media.mjs";

// Since 26.09.13 the root domain is the address: Google Workspace holds its
// MX, catches every unknown @aesthetic.computer, and hands the letter to
// lith/mail-inbound.mjs. `mail.` stays an alias for the older spelling.
export const ROOT_DOMAIN = "aesthetic.computer";
export const MAIL_DOMAIN = "mail.aesthetic.computer";
export const INBOUND_DOMAINS = [ROOT_DOMAIN, MAIL_DOMAIN];
// The post office's own mailbox. Outbound letters are signed by it, with the
// writer's permahandle in the plus-tag so a reply finds its way home.
export const POST_OFFICE = "amail";
export const MAX_TEXT_LENGTH = 500;
export const OUTSIDE_TEXT_LENGTH = 2000; // an email runs longer than a tell
export const MAX_SUBJECT_LENGTH = 80;
export const NUDGE_EVERY = 6 * 60 * 60 * 1000; // one email nudge per reader per six hours

const PERMAHANDLE = /^ac\d\d[a-z]{5}$/; // see lib/user-code.mjs

// `@handle`, `ac25namuc`, `jeffrey@mail.aesthetic.computer`, or an email → sub.
export async function subFromAddress(address, database) {
  let to = typeof address === "string" ? address.trim() : "";
  if (!to) return undefined;
  for (const domain of INBOUND_DOMAINS) {
    if (to.toLowerCase().endsWith("@" + domain)) to = to.slice(0, -(domain.length + 1));
  }
  if (to.startsWith("@")) to = to.slice(1);
  if (!to) return undefined;
  if (PERMAHANDLE.test(to.toLowerCase())) {
    const user = await database.db
      .collection("users")
      .findOne({ code: to.toLowerCase() }, { projection: { _id: 1 } });
    return user?._id;
  }
  const exact = await userIDFromHandleOrEmail(to, database);
  if (exact || to.includes("@")) return exact;

  // Preserve exact matches; forgive case only when one full handle matches.
  // Short nicknames and ambiguous spellings must never pick a recipient.
  const escaped = to.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const matches = await database.db.collection("@handles")
    .find({ handle: { $regex: `^${escaped}$`, $options: "i" } }, { projection: { handle: 1 }, maxTimeMS: 2000 })
    .limit(2).toArray();
  if (matches.length !== 1) return undefined;
  return userIDFromHandleOrEmail(matches[0].handle, database);
}

// How a message signs itself. Deliberately not `getHandleOrEmail` — that falls
// back to the sender's real email, and an inbox is the wrong place to leak one.
export async function nameFor(sub, database) {
  const handle = await handleFor(sub);
  if (handle) return "@" + handle;
  const user = await database.db
    .collection("users")
    .findOne({ _id: sub }, { projection: { code: 1 } });
  return user?.code;
}

// Both spellings of a mailbox, for showing someone how to be reached.
export async function addressesFor(sub, database) {
  const [handle, user] = await Promise.all([
    handleFor(sub),
    database.db
      .collection("users")
      .findOne({ _id: sub }, { projection: { code: 1 } }),
  ]);
  const out = [];
  if (user?.code) out.push(user.code + "@" + ROOT_DOMAIN);
  if (handle) out.push(handle + "@" + ROOT_DOMAIN);
  return out;
}

export async function mailbox(database) {
  const tells = database.db.collection("tells");
  await tells.createIndex({ to: 1, when: -1 });
  await tells.createIndex({ from: 1, when: -1 });
  return tells;
}

// A client once escaped a heart on the way in (`thank you &lt;3`), so the
// server undoes HTML entities before anything looks at the letter. `&amp;`
// goes last: `&amp;lt;` is a literal `&lt;`, not a `<`.
const NAMED = { lt: "<", gt: ">", quot: '"', apos: "'" };
const codepoint = (match, n) => (n > 0 && n <= 0x10ffff ? String.fromCodePoint(n) : match);
export function decodeEntities(text) {
  return (text || "")
    .replace(/&#x([0-9a-f]{1,6});/gi, (m, hex) => codepoint(m, parseInt(hex, 16)))
    .replace(/&#(\d{1,7});/g, (m, dec) => codepoint(m, Number(dec)))
    .replace(/&(lt|gt|quot|apos);/g, (_, name) => NAMED[name])
    .replace(/&amp;/g, "&");
}

export function clean(text, max = MAX_TEXT_LENGTH) {
  return filter(decodeEntities(text).trim()).slice(0, max);
}

export const deliver = (options, database) => observeMail("internal", options, database,
  (options, event) => deliverInternal(options, database, event));
export const deliverFromOutside = (options, database) => observeMail("smtp-in", options, database,
  (options, event) => receiveOutside(options, database, event));
export const sendOutside = (options, database) => observeMail("smtp-out", options, database,
  (options, event) => relayOutside(options, database, event));

// Put one message in a mailbox and buzz whatever devices the reader carries.
async function deliverInternal(
  { from, to, text, subject, device, verb = "mailed" },
  database,
  event,
) {
  const tells = await mailbox(database);
  const [fromHandle, toHandle] = await Promise.all([
    nameFor(from, database),
    nameFor(to, database),
  ]);
  const when = new Date();

  const { insertedId } = await tells.insertOne({
    to,
    toHandle,
    from,
    fromHandle,
    text,
    // A letter may have a subject; a `tell` never does, so don't store an
    // empty one and leave every message before today shaped as it was.
    ...(subject ? { subject } : {}),
    when,
    read: false,
  });

  event("stored", { letterId: insertedId });
  let push = { attempted: 0, succeeded: 0, failed: 0, pruned: 0 };
  try {
    push = await sendToUser(
      database.db,
      to,
      letterNotification(insertedId),
      { device },
      quietMailPush,
    );
    event("push", { letterId: insertedId, ...push, ...(push.attempted === 0 ? { reason: "no_devices" } : {}) });
  } catch (err) {
    // A silent phone shouldn't eat the letter — it's already in the mailbox.
    event("push_failed", { letterId: insertedId, error: mailErrorCode(err) });
  }
  const nudge = push.succeeded > 0
    ? { status: "pushed" }
    : await nudgeReader({ to, toHandle, fromHandle, letterId: insertedId }, database, event);

  return { id: insertedId, fromHandle, toHandle, when, push, nudge };
}

// Most readers carry no push device, so a letter used to land in silence. When
// no phone buzzed, one plain email says a letter is waiting — never what it
// says — and a reader hears from us at most once per NUDGE_EVERY. The throttle
// row is claimed first and atomically (unique `user`), so two letters landing
// together send one note and never two Auth0 lookups apiece.
let nudgesIndexed = false;
export async function nudgeReader({ to, toHandle, fromHandle, letterId }, database, event) {
  let status = "failed";
  try {
    const nudges = database.db.collection("mail-nudges");
    if (!nudgesIndexed) {
      await nudges.createIndex({ user: 1 }, { unique: true });
      nudgesIndexed = true;
    }
    const now = new Date();
    try {
      await nudges.updateOne(
        { user: to, $or: [{ lastNudgedAt: { $exists: false } }, { lastNudgedAt: { $lt: new Date(now - NUDGE_EVERY) } }] },
        { $set: { lastNudgedAt: now }, $inc: { count: 1 } },
        { upsert: true },
      );
    } catch (err) {
      if (err?.code !== 11000) throw err;
      status = "throttled"; // someone else's letter just took this window
      event("nudge", { letterId, status });
      return { status };
    }

    // Lazy like email(): the Auth0 lookup is only paid for once a nudge is due.
    const { userEmailFromID } = await import("./authorization.mjs");
    const found = await userEmailFromID(to);
    const address = found?.email_verified ? (found.email || "").trim().toLowerCase() : "";
    if (!address) {
      status = "no_email";
    } else if (await database.db.collection("email-blast-unsubscribes").findOne({ email: address })) {
      status = "unsubscribed";
    } else {
      const { email } = await import("./email.mjs"); // lazy: no SMTP in local dev is a quiet "failed"
      const who = (fromHandle || "").replace(/[\r\n]+/g, " ").trim().slice(0, 60);
      const writer = who ? `${who} wrote you a letter` : "Someone wrote you a letter";
      const sent = await email({
        from: `${ROOT_DOMAIN} <mail@${ROOT_DOMAIN}>`,
        to: address,
        subject: `${writer} on ${ROOT_DOMAIN}`,
        text: [
          `${writer} on ${ROOT_DOMAIN}${toHandle ? ` — it's waiting for ${toHandle}` : ""}.`,
          "",
          `Read it at https://${ROOT_DOMAIN}/mail`,
          "",
          "You'll get at most one of these notes every few hours, and only when no device of yours could be buzzed.",
        ].join("\n"),
      });
      status = sent ? "sent" : "failed";
    }
    event("nudge", { letterId, status });
  } catch (err) {
    // A silent nudge shouldn't eat the letter — it's already in the mailbox.
    status = "failed";
    event("nudge_failed", { letterId, error: mailErrorCode(err) });
  }
  return { status };
}

// A letter from outside the wall. Google Workspace catches the address and
// lith/mail-inbound.mjs hands it here over SMTP. There is no sender `sub` —
// the sender lives in `fromEmail` (and `fromHandle` carries their name so the
// inbox reads the same as an inside letter). `auth` is what Google's gate
// found out about the sender (spf/dkim/dmarc, and `verified`); `quiet` files
// the letter without buzzing a phone, for a sender who has already buzzed it
// enough this hour. `messageId` keeps a relay retry from filing the same
// letter twice — scoped to the box, so nobody can pre-empt another's letter.
let dedupeIndexed = false;
async function receiveOutside(
  { to, fromEmail, fromName, subject, text, messageId, auth = null, quiet = false, attachments = [] },
  database,
  event,
) {
  const tells = await mailbox(database);
  if (!dedupeIndexed) {
    // Partial, not sparse: sparse still indexes a row that has `to` but no
    // messageId (as null), and every inside letter is one of those — the
    // build collided on the first two and took a real letter down with it.
    await tells.createIndex(
      { to: 1, messageId: 1 },
      { unique: true, partialFilterExpression: { messageId: { $type: "string" } } },
    );
    await tells.dropIndex("messageId_1").catch(() => {}); // the first cut's global one
    dedupeIndexed = true;
  }
  const toHandle = await nameFor(to, database);
  const name = (fromName || "").trim();
  const fromHandle = name || fromEmail;
  const when = new Date();

  let insertedId;
  try {
    ({ insertedId } = await tells.insertOne({
      to,
      toHandle,
      from: null,
      fromHandle,
      fromEmail,
      text,
      ...(subject ? { subject } : {}),
      ...(messageId ? { messageId } : {}),
      ...(auth ? { auth } : {}),
      via: "smtp",
      ...(attachments.length ? { attachments } : {}),
      when,
      read: false,
    }));
  } catch (err) {
    if (err?.code === 11000) { event("duplicate"); return { duplicate: true, toHandle }; } // relay retried
    throw err;
  }

  event("stored", { letterId: insertedId });
  let push = { attempted: 0, succeeded: 0, failed: 0, pruned: 0 };
  if (quiet) {
    event("push_quiet", { letterId: insertedId, reason: "push_limit" });
    return { id: insertedId, fromHandle, toHandle, when, push, quiet, nudge: { status: "skipped" } };
  }
  try {
    push = await sendToUser(
      database.db,
      to,
      letterNotification(insertedId),
      {},
      quietMailPush,
    );
    event("push", { letterId: insertedId, ...push, ...(push.attempted === 0 ? { reason: "no_devices" } : {}) });
  } catch (err) {
    event("push_failed", { letterId: insertedId, error: mailErrorCode(err) });
  }
  // The nudge names the sender by display name only; a raw address is theirs
  // to share, not ours — with no name they are "someone".
  const nudge = push.succeeded > 0
    ? { status: "pushed" }
    : await nudgeReader({ to, toHandle, fromHandle: name, letterId: insertedId }, database, event);

  return { id: insertedId, fromHandle, toHandle, when, push, nudge };
}

// A letter leaving the wall. Signed by the post office, never as the handle:
//
//   From:     @jeffrey via aesthetic.computer <amail+ac25namuc@aesthetic.computer>
//   Reply-To: jeffrey@aesthetic.computer
//
// The recipient sees who wrote, the signature stays honest (the SPF and DKIM
// are the post office's), and a reply to either address comes back through
// the door — the plus-tag carries the permahandle, which outlives a rename.
// It leaves through Google's SMTP relay with the mail@ credentials; the relay
// lets any address in the domain sign, which plain smtp.gmail.com would not.
async function relayOutside({ from, toEmail, subject, text }, database, event) {
  const nodemailer = (await import("nodemailer")).default;
  const [handle, user] = await Promise.all([
    handleFor(from),
    database.db
      .collection("users")
      .findOne({ _id: from }, { projection: { code: 1 } }),
  ]);
  const code = user?.code;
  if (!code && !handle) throw new Error("a letter needs a handle to be signed");
  const fromHandle = handle ? "@" + handle : code;
  const home = `${handle || code}@${ROOT_DOMAIN}`;

  // The relay greets with a 421 unless EHLO names a real host and TLS comes
  // first. If the relay still won't have us, fall back to plain Gmail SMTP,
  // which rewrites From to the post office's own address — the name and
  // Reply-To survive, so a reply still finds its way home.
  const auth = { user: process.env.SMTP_USER, pass: process.env.SMTP_PASS };
  const common = { port: 587, secure: false, requireTLS: true, name: "inbound.aesthetic.computer", auth };
  const letter = {
    from: { name: `${fromHandle} via aesthetic.computer`, address: `${POST_OFFICE}+${code || handle}@${ROOT_DOMAIN}` },
    replyTo: home,
    to: toEmail,
    subject: subject || `a letter from ${fromHandle}`,
    ...outsideMediaBody(
      `${text}\n\n— ${fromHandle}, via aesthetic.computer mail · reply to ${home}`,
      await resolveMailMedia(text, database),
    ),
  };
  let info;
  try {
    info = await nodemailer
      .createTransport({ ...common, host: process.env.AMAIL_SMTP_SERVER || "smtp-relay.gmail.com" })
      .sendMail(letter);
  } catch (err) {
    event("relay_fallback", { error: mailErrorCode(err) });
    info = await nodemailer
      .createTransport({ ...common, host: process.env.SMTP_SERVER || "smtp.gmail.com" })
      .sendMail(letter);
  }

  event("relay_accepted");
  const tells = await mailbox(database);
  const when = new Date();
  const { insertedId } = await tells.insertOne({
    to: null,
    toHandle: toEmail,
    toEmail,
    from,
    fromHandle,
    text,
    ...(subject ? { subject } : {}),
    ...(info.messageId ? { messageId: info.messageId } : {}),
    via: "smtp-out",
    when,
    read: true,
  });
  event("stored", { letterId: insertedId });
  return { id: insertedId, fromHandle, toHandle: toEmail, when };
}
