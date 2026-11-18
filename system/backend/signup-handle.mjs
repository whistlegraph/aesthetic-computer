// signup-handle, 26.11.18
// The hosted sign-in page asks a new account for its @handle (an Auth0 Form,
// backend/auth0-forms) and claims it here, server to server, before the person
// ever returns to AC. `/handle` stays the one place a handle is made: this
// only stands in for the bearer token on a create, never a rename.
//
//   POST /handle   x-ac-signup: <secret>
//        { handle, sub, email, email_verified }
//     → 200 { ok: "yes", handle } | 200 { ok: "no", message }
//   POST /handle   x-ac-signup: <secret>      (no handle: a lookup)
//        { sub, email, email_verified }
//     → 200 { ok: "yes", handle } | 200 { ok: "no" }
//
// The lookup is how the page knows not to ask: the account may already have a
// handle here, on sotce, or on the older account a verified address links to.
//
// The reply is always 200 with the words the form shows, so the rules and
// their wording live here and not in the form.
//
// The secret is derived from the Management API secret lith already holds, so
// there is no new key to carry; auth0-forms/install.sh derives the same value.
//
// A password signup has not verified its address yet. Its handle is real at
// once, marked `unverifiedAt`; if the account is still unverified a week later
// the name can be taken by the next person who asks for it.

import { createHmac, timingSafeEqual } from "node:crypto";

export const SIGNUP_HEADER = "x-ac-signup";
export const UNVERIFIED_GRACE_MS = 7 * 24 * 60 * 60 * 1000;

export function signupSecret(env = process.env) {
  const root = env.AUTH0_M2M_SECRET;
  return root ? createHmac("sha256", root).update("ac:signup-handle:v1").digest("hex") : null;
}

// undefined: not a signup claim. null: one that must be refused.
// Otherwise the account Auth0 vouches for.
export function signupClaim(headers = {}, body = {}, env = process.env) {
  const given = headers[SIGNUP_HEADER];
  if (given === undefined) return undefined;
  const secret = signupSecret(env);
  const a = Buffer.from(String(given)), b = Buffer.from(secret || "");
  if (!secret || a.length !== b.length || !timingSafeEqual(a, b)) return null;
  // An aesthetic-tenant subject: `provider|id`. Sotce's are prefixed `sotce-`
  // on our side and never arrive this way.
  if (typeof body.sub !== "string" || !/^[a-z0-9-]+\|[\w.|-]+$/i.test(body.sub) || body.sub.startsWith("sotce-")) return null;
  return {
    sub: body.sub,
    email: typeof body.email === "string" ? body.email : undefined,
    unverified: body.email_verified !== true && body.email_verified !== "true",
  };
}

// Only what a create needs: no tenant, action or hold rides in on the secret.
export const signupBody = (body = {}) => JSON.stringify({ handle: body.handle, createOnly: true });

export function signupMessage(reason, handle) {
  if (reason === "taken") return `@${handle} is taken. Try another.`;
  if (reason === "naughty") return "Try a different name.";
  if (reason === "invalid" || reason === "too long")
    return "Use 1–16 letters or numbers. Dots and underscores can go between them.";
  return "Couldn’t save your handle. Please try again.";
}

// Turn `/handle`'s reply into the flat one the form reads.
export function signupReply(response, handle, respond) {
  let body = {};
  try { body = JSON.parse(response.body); } catch {}
  if (response.statusCode === 200 && body.handle) return respond(200, { ok: "yes", handle: String(body.handle).replace(/^@/, "") });
  return respond(200, { ok: "no", message: signupMessage(body.message, String(handle || "").replace(/^@/, "")) });
}

// Does this account already have a handle? Its own, its sotce sister's (both
// through `handleFor`), or one on another verified account with its address.
export async function signupLookup(claimed, { handleFor, subsForEmail }) {
  let handle = await handleFor(claimed.sub);
  if (!handle && !claimed.unverified && claimed.email) {
    for (const sub of await subsForEmail(claimed.email)) {
      if (sub === claimed.sub) continue;
      handle = await handleFor(sub);
      if (handle) break;
    }
  }
  return handle ? { ok: "yes", handle: String(handle).replace(/^@/, "") } : { ok: "no" };
}

export const abandoned = (doc, now = new Date()) =>
  doc?.unverifiedAt instanceof Date && +now - +doc.unverifiedAt > UNVERIFIED_GRACE_MS;

// A handle whose account never verified, a week on, gives way. `verified`
// asks Auth0; when it cannot answer, the handle stays.
export async function releaseAbandoned(handles, doc, { verified, forget = async () => {}, now = new Date() }) {
  if (!abandoned(doc, now)) return false;
  const answer = await verified(doc._id);
  if (answer === true) { await handles.updateOne({ _id: doc._id }, { $unset: { unverifiedAt: "" } }); return false; }
  if (answer !== false) return false;
  await handles.deleteOne({ _id: doc._id });
  await forget(doc);
  return true;
}
