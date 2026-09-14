// mail, 26.09.11
// Internal AC mail. Nothing leaves the wall yet, so there is no SMTP in here —
// a message is a row in `tells`, the collection `tell` has been filling since
// 26.04 with nothing on the other end to read it.
//
// Addressing: a message is filed against the recipient's `sub`, never a handle,
// so a rename can't orphan a thread. Two spellings reach a person — their
// permahandle (`ac25namuc`, permanent) and their @handle (follows the rename).
// The permahandle is canonical; the @handle is an alias over it.

import { handleFor, userIDFromHandleOrEmail } from "./authorization.mjs";
import { filter } from "./filter.mjs";
import { shell } from "./shell.mjs";
import { sendToUser } from "../../shared/push.mjs";

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

const PERMAHANDLE = /^ac\d\d[a-z]{5}$/; // see lib/user-code.mjs

// `@handle`, `ac25namuc`, `jeffrey@mail.aesthetic.computer`, or an email → sub.
export async function subFromAddress(address, database) {
  let to = (address || "").trim();
  if (!to) return undefined;
  for (const domain of INBOUND_DOMAINS) {
    if (to.toLowerCase().endsWith("@" + domain)) to = to.slice(0, -(domain.length + 1));
  }
  if (PERMAHANDLE.test(to)) {
    const user = await database.db
      .collection("users")
      .findOne({ code: to }, { projection: { _id: 1 } });
    return user?._id;
  }
  return userIDFromHandleOrEmail(to, database);
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

export function clean(text, max = MAX_TEXT_LENGTH) {
  return filter((text || "").trim()).slice(0, max);
}

// Put one message in a mailbox and buzz whatever devices the reader carries.
export async function deliver(
  { from, to, text, subject, device, verb = "mailed" },
  database,
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

  let push = { attempted: 0, succeeded: 0, failed: 0, pruned: 0 };
  try {
    push = await sendToUser(
      database.db,
      to,
      {
        title: `${fromHandle} ${verb} you`,
        body: subject ? `${subject} — ${text}` : text,
        data: {
          kind: "tell",
          from: fromHandle || "",
          tellId: insertedId.toString(),
          piece: "mail",
        },
      },
      { device },
      shell.log,
    );
  } catch (err) {
    // A silent phone shouldn't eat the letter — it's already in the mailbox.
    shell.log("🔴 mail push failed:", err?.message || err);
  }

  return { id: insertedId, fromHandle, toHandle, when, push };
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
export async function deliverFromOutside(
  { to, fromEmail, fromName, subject, text, messageId, auth = null, quiet = false },
  database,
) {
  const tells = await mailbox(database);
  if (!dedupeIndexed) {
    await tells.createIndex({ to: 1, messageId: 1 }, { unique: true, sparse: true });
    await tells.dropIndex("messageId_1").catch(() => {}); // the first cut's global one
    dedupeIndexed = true;
  }
  const toHandle = await nameFor(to, database);
  const fromHandle = (fromName || "").trim() || fromEmail;
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
      when,
      read: false,
    }));
  } catch (err) {
    if (err?.code === 11000) return { duplicate: true, toHandle }; // relay retried
    throw err;
  }

  let push = { attempted: 0, succeeded: 0, failed: 0, pruned: 0 };
  if (quiet) return { id: insertedId, fromHandle, toHandle, when, push, quiet };
  try {
    push = await sendToUser(
      database.db,
      to,
      {
        title: `${fromHandle} wrote you`,
        body: subject ? `${subject} — ${text}` : text,
        data: {
          kind: "tell",
          from: fromHandle,
          tellId: insertedId.toString(),
          piece: "amail",
        },
      },
      {},
      shell.log,
    );
  } catch (err) {
    shell.log("🔴 mail push failed:", err?.message || err);
  }

  return { id: insertedId, fromHandle, toHandle, when, push };
}

// A letter leaving the wall. Signed by the post office, never as the handle:
//
//   From:     @jeffrey via Amail <amail+ac25namuc@aesthetic.computer>
//   Reply-To: jeffrey@aesthetic.computer
//
// The recipient sees who wrote, the signature stays honest (the SPF and DKIM
// are the post office's), and a reply to either address comes back through
// the door — the plus-tag carries the permahandle, which outlives a rename.
// It leaves through Google's SMTP relay with the mail@ credentials; the relay
// lets any address in the domain sign, which plain smtp.gmail.com would not.
export async function sendOutside({ from, toEmail, subject, text }, database) {
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

  const transporter = nodemailer.createTransport({
    host: process.env.AMAIL_SMTP_SERVER || "smtp-relay.gmail.com",
    port: 587,
    secure: false,
    auth: { user: process.env.SMTP_USER, pass: process.env.SMTP_PASS },
  });
  const info = await transporter.sendMail({
    from: { name: `${fromHandle} via Amail`, address: `${POST_OFFICE}+${code || handle}@${ROOT_DOMAIN}` },
    replyTo: home,
    to: toEmail,
    subject: subject || `a letter from ${fromHandle}`,
    text: `${text}\n\n— ${fromHandle}, via Amail · reply to ${home}`,
  });

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
  return { id: insertedId, fromHandle, toHandle: toEmail, when };
}
