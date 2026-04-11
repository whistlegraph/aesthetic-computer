// delete-tape.mjs — Soft-delete a tape from the cloud
//
// POST /api/delete-tape with { code }   (Bearer token required)
//   → sets nuked:true + nukedAt:Date on the tapes doc if the requesting
//     user owns it. Returns {ok:true} or {error}.
//
// The file itself is NOT removed from DO Spaces (soft-delete only) so
// audit logs / recovery remain possible. A separate scheduled cleanup
// job can purge nuked tapes' bucket objects later.
//
// Used by:
//   - ac-native `tapes.mjs` when the user hits Delete on a cloud tape
//   - Web AC tape management UI (future)

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";
import { authorize } from "../../backend/authorization.mjs";

export async function handler(event) {
  if (event.httpMethod !== "POST" && event.httpMethod !== "DELETE") {
    return respond(405, { error: "Method not allowed" });
  }

  try {
    const user = await authorize(event.headers);
    if (!user || !user.sub) {
      return respond(401, { error: "Not authenticated" });
    }

    let body = {};
    try {
      body = JSON.parse(event.body || "{}");
    } catch (_e) {
      return respond(400, { error: "Invalid JSON body" });
    }
    const code = (body.code || "").trim();
    if (!code) {
      return respond(400, { error: "code is required" });
    }

    const database = await connect();
    const tapes = database.db.collection("tapes");

    const tape = await tapes.findOne({ code });
    if (!tape) {
      return respond(404, { error: "Tape not found" });
    }
    if (tape.user !== user.sub) {
      return respond(403, { error: "Not the tape owner" });
    }
    if (tape.nuked) {
      return respond(200, { ok: true, alreadyNuked: true });
    }

    await tapes.updateOne(
      { _id: tape._id },
      { $set: { nuked: true, nukedAt: new Date() } }
    );

    console.log(`🗑️  Tape ${code} nuked by ${user.sub}`);
    return respond(200, { ok: true, code });
  } catch (err) {
    console.error("[delete-tape] error:", err);
    return respond(500, { error: err.message || "Internal error" });
  }
}
