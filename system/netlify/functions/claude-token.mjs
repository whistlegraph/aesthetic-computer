// claude-token, 2026.03.16
// Store/retrieve device tokens (Claude OAuth + GitHub PAT) per handle.
// GET                  → returns tokens for the authenticated user's own handle
// GET ?handle=X        → admin-only: returns tokens for handle X (used by
//                        ac-inscribe --for-handle when an admin flashes a
//                        USB on behalf of another user). Returns sub so the
//                        inscription's usbConfig can be built without an
//                        access_token (admins cannot sign for other users).
// POST {token, githubPat} → stores tokens for authenticated user's handle

import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { getDeviceCreds, setDeviceCreds } from "../../backend/device-creds.mjs";

export async function handler(event) {
  // GET: Retrieve tokens for the authenticated user — or, when an admin
  // passes ?handle=X, for that other handle so the admin can flash a USB
  // pre-baked with that user's claude/github creds.
  if (event.httpMethod === "GET") {
    const user = await authorize(event.headers);
    if (!user) return respond(401, { message: "unauthorized" });

    const targetHandle = event.queryStringParameters?.handle;
    const database = await connect();
    try {
      // Resolve which handle/sub we're fetching creds for (self, or another
      // handle when an admin is flashing on their behalf).
      let existing;
      if (targetHandle) {
        const isAdmin = await hasAdmin(user, "aesthetic");
        if (!isAdmin) return respond(403, { message: "admin only" });
        existing = await database.db
          .collection("@handles")
          .findOne({ handle: targetHandle }, { projection: { handle: 1 } });
      } else {
        existing = await database.db
          .collection("@handles")
          .findOne({ _id: user.sub }, { projection: { handle: 1 } });
      }
      if (!existing) return respond(404, { message: "Handle not found" });

      // Creds live in the dedicated device-creds store, keyed by sub.
      const creds = await getDeviceCreds(database.db, existing._id);
      return respond(200, {
        handle: existing.handle,
        sub: existing._id,
        token: creds?.claudeToken || null,
        githubPat: creds?.githubPat || null,
      });
    } finally {
      await database.disconnect();
    }
  }

  // POST: Save tokens for authenticated user
  if (event.httpMethod === "POST") {
    const user = await authorize(event.headers);
    if (!user) return respond(401, { message: "unauthorized" });

    let body;
    try {
      body = JSON.parse(event.body);
    } catch {
      return respond(400, { message: "Invalid JSON" });
    }

    const database = await connect();
    try {
      const handles = database.db.collection("@handles");
      const existing = await handles.findOne(
        { _id: user.sub },
        { projection: { handle: 1 } },
      );
      if (!existing) return respond(404, { message: "Handle not found" });

      // Write into the dedicated device-creds store (never onto @handles).
      const { saved } = await setDeviceCreds(database.db, user.sub, {
        claudeToken: body.token,
        githubPat: body.githubPat,
      });
      if (saved.length === 0) {
        return respond(400, { message: "No valid tokens provided" });
      }

      return respond(200, {
        message: `Tokens saved for @${existing.handle}`,
        handle: existing.handle,
        saved,
      });
    } finally {
      await database.disconnect();
    }
  }

  return respond(405, { message: "Method Not Allowed" });
}
