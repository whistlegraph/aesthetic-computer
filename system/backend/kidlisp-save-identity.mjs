// Shared by Mongo and Datomic: an explicit handled save belongs to the verified
// caller. Global content dedup remains unchanged for ordinary cache requests.
import { createHash } from "node:crypto";
const digest = value => createHash("sha256").update(value).digest("hex");

export function kidlispSaveIdentity(source, { handled = false, user, handle } = {}) {
  if (typeof handled !== "boolean") throw Object.assign(new Error("handled must be a boolean"), { statusCode: 400 });
  if (handled && !user?.sub) throw Object.assign(new Error("Sign in to save KidLisp under your handle"), { statusCode: 401 });
  if (handled && !(typeof handle === "string" && handle.startsWith("@")))
    throw Object.assign(new Error("Claim a handle before saving handled KidLisp"), { statusCode: 403 });
  const contentHash = digest(source.trim());
  // hash is the storage dedup key. contentHash remains the hash of the bytes.
  // No schema/index migration: each user's copy gets an ordinary unique code.
  const hash = handled ? digest(JSON.stringify(["handled-kidlisp-v1", user.sub, source.trim()])) : contentHash;
  return { hash, contentHash, handled, user: handled ? user.sub : null,
    receipt: handled ? { handled: true, handle } : {} };
}

export async function findSavedKidlisp(identity, source, lookup) {
  // Reuse a caller's earlier ordinary save without changing its code or owner.
  const global = await lookup(identity.contentHash);
  if (global && (!identity.handled || global.user === identity.user) && global.source === source.trim()) return global;
  if (!identity.handled) return null;
  const owned = await lookup(identity.hash);
  if (owned && (owned.user !== identity.user || owned.source !== source.trim()))
    throw Object.assign(new Error("Stored KidLisp identity does not match this save"), { statusCode: 409 });
  return owned;
}
