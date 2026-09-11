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

export const MAIL_DOMAIN = "mail.aesthetic.computer"; // tier 2 binds this for real
export const MAX_TEXT_LENGTH = 500;
export const MAX_SUBJECT_LENGTH = 80;

const PERMAHANDLE = /^ac\d\d[a-z]{5}$/; // see lib/user-code.mjs

// `@handle`, `ac25namuc`, `jeffrey@mail.aesthetic.computer`, or an email → sub.
export async function subFromAddress(address, database) {
  let to = (address || "").trim();
  if (!to) return undefined;
  if (to.endsWith("@" + MAIL_DOMAIN)) to = to.slice(0, -(MAIL_DOMAIN.length + 1));
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
  if (user?.code) out.push(user.code + "@" + MAIL_DOMAIN);
  if (handle) out.push(handle + "@" + MAIL_DOMAIN);
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
