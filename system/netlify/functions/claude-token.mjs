// claude-token, 2026.03.16
// Store/retrieve device tokens (Claude OAuth + GitHub PAT) per handle.
// GET ?handle=X → returns tokens
// POST {token, githubPat} → stores tokens for authenticated user's handle

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  // GET: Retrieve tokens for a handle
  if (event.httpMethod === "GET") {
    const handle = event.queryStringParameters?.handle;
    if (!handle) return respond(400, { message: "Missing handle" });

    const database = await connect();
    try {
      const user = await database.db.collection("@handles").findOne({
        handle: { $regex: new RegExp(`^${handle}$`, "i") },
      });
      return respond(200, {
        token: user?.claudeCodeToken || null,
        githubPat: user?.githubPat || null,
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

    const updates = {};
    if (body.token && body.token.startsWith("sk-ant-")) {
      updates.claudeCodeToken = body.token;
      updates.claudeCodeTokenUpdated = new Date();
    }
    if (body.githubPat && body.githubPat.startsWith("ghp_")) {
      updates.githubPat = body.githubPat;
      updates.githubPatUpdated = new Date();
    }

    if (Object.keys(updates).length === 0) {
      return respond(400, { message: "No valid tokens provided" });
    }

    const database = await connect();
    try {
      const handles = database.db.collection("@handles");
      const existing = await handles.findOne({ _id: user.sub });
      if (!existing) return respond(404, { message: "Handle not found" });

      await handles.updateOne({ _id: user.sub }, { $set: updates });

      return respond(200, {
        message: `Tokens saved for @${existing.handle}`,
        handle: existing.handle,
        saved: Object.keys(updates).filter((k) => !k.endsWith("Updated")),
      });
    } finally {
      await database.disconnect();
    }
  }

  return respond(405, { message: "Method Not Allowed" });
}
