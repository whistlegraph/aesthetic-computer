// spells, 2026.11.16
// Approvals for the spells listing at aesthetic.computer/spells: the page that
// shows each spell in "Aesthetic Spells" (papers/arxiv-magicians-manual) beside
// a clip of a browser casting it. Anyone can read the approvals. Only @jeffrey
// can set one.
//
// GET                     Public. 200 { approvals: { <spell>: { status, note,
//                         at } }, admin } — admin is true when the request
//                         carries @jeffrey's token.
// POST { name, status,    Admin only. status is "approved", "flagged" or ""
//        note? }          (clears it). Returns the stored row.
//
// Collection: "spell-approvals". _id is the spell's name. Schema:
//   { _id, status, note, at, by }

import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const COLLECTION = "spell-approvals";
const STATUSES = new Set(["approved", "flagged", ""]);

async function admin(event) {
  const user = await authorize(event.headers);
  if (!user) return { user: null, ok: false };
  return { user, ok: !!(await hasAdmin(user, "aesthetic")) };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "");

  if (event.httpMethod === "GET") {
    const who = await admin(event).catch(() => ({ ok: false }));
    const database = await connect();
    try {
      const rows = await database.db.collection(COLLECTION).find({}).toArray();
      const approvals = {};
      for (const r of rows) approvals[r._id] = { status: r.status || "", note: r.note || "", at: r.at || null };
      return respond(200, { approvals, admin: who.ok });
    } finally {
      await database.disconnect();
    }
  }

  if (event.httpMethod === "POST") {
    const who = await admin(event);
    if (!who.user) return respond(401, { message: "unauthorized" });
    if (!who.ok) return respond(403, { message: "admin only" });

    let body;
    try { body = JSON.parse(event.body || "{}"); } catch { return respond(400, { message: "bad json" }); }
    const name = typeof body.name === "string" ? body.name.trim().slice(0, 64) : "";
    const status = typeof body.status === "string" ? body.status : null;
    if (!name) return respond(400, { message: "name required" });
    if (status === null || !STATUSES.has(status)) return respond(400, { message: "status must be approved, flagged or empty" });
    const note = typeof body.note === "string" ? body.note.slice(0, 500) : "";

    const database = await connect();
    try {
      const row = { status, note, at: new Date().toISOString(), by: "jeffrey" };
      await database.db.collection(COLLECTION).updateOne({ _id: name }, { $set: row }, { upsert: true });
      return respond(200, { name, ...row });
    } finally {
      await database.disconnect();
    }
  }

  return respond(405, { message: "method not allowed" });
}
