// rax-deaths, 26.11.27
// How many times each handle has died in rax, kept in the database
// (collection `rax-deaths`, one document a handle).

// GET  `api/rax-deaths?handle=jeffrey` → { handle, deaths }
// POST `api/rax-deaths` { handle, kind } → { handle, deaths }
//   Only the rax server writes: it signs with RAX_BUILDER_TOKEN, the secret it
//   already shares with lith. A page cannot count a death for anyone.

import { timingSafeEqual } from "node:crypto";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const KINDS = ["shot", "self", "fell", "other"];
export const cleanHandle = (raw) => {
  const handle = String(raw || "").replace(/^@/, "").toLowerCase();
  return /^[a-z0-9._-]{1,64}$/.test(handle) ? handle : null;
};
export function trusted(headers, secret = process.env.RAX_BUILDER_TOKEN) {
  const given = String(headers?.authorization || headers?.Authorization || "").replace(/^Bearer\s+/i, "");
  if (!secret || !given) return false;
  const a = Buffer.from(given), b = Buffer.from(secret);
  return a.length === b.length && timingSafeEqual(a, b);
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, null);
  if (event.httpMethod === "GET") {
    const handle = cleanHandle(event.queryStringParameters?.handle);
    if (!handle) return respond(400, { message: "A handle is required." });
    const database = await connect();
    const doc = await database.db.collection("rax-deaths").findOne({ _id: handle });
    return respond(200, { handle, deaths: doc?.deaths || 0 });
  }
  if (event.httpMethod !== "POST") return respond(405, { message: "GET or POST only." });
  if (!trusted(event.headers)) return respond(401, { message: "Only the rax server counts a death." });
  let body;
  try { body = JSON.parse(event.body || "{}"); } catch { return respond(400, { message: "Malformed body." }); }
  const handle = cleanHandle(body.handle);
  if (!handle) return respond(400, { message: "A handle is required." });
  const kind = KINDS.includes(body.kind) ? body.kind : "other";
  const database = await connect();
  const doc = await database.db.collection("rax-deaths").findOneAndUpdate(
    { _id: handle },
    { $inc: { deaths: 1, [`kinds.${kind}`]: 1 }, $set: { last: new Date() }, $setOnInsert: { first: new Date() } },
    { upsert: true, returnDocument: "after" },
  );
  return respond(200, { handle, deaths: (doc?.value ?? doc)?.deaths || 1 });
}
