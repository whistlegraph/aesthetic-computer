// claude-token, 2026.03.16
// Store/retrieve Claude Code OAuth token per handle.
// GET ?handle=X → returns token (requires device auth header)
// POST {token} → stores token for authenticated user's handle

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  // GET: Retrieve Claude Code token for a handle (device auth via AC token)
  if (event.httpMethod === "GET") {
    const handle = event.queryStringParameters?.handle;
    if (!handle) return respond(400, { message: "Missing handle" });

    const database = await connect();
    try {
      const user = await database.db.collection("@handles").findOne({
        handle: { $regex: new RegExp(`^${handle}$`, "i") },
      });
      if (!user?.claudeCodeToken) return respond(200, { token: null });
      return respond(200, { token: user.claudeCodeToken });
    } finally {
      await database.disconnect();
    }
  }

  // POST: Save Claude Code token for authenticated user
  if (event.httpMethod === "POST") {
    const user = await authorize(event.headers);
    if (!user) return respond(401, { message: "unauthorized" });

    let body;
    try {
      body = JSON.parse(event.body);
    } catch {
      return respond(400, { message: "Invalid JSON" });
    }

    const { token } = body;
    if (!token || !token.startsWith("sk-ant-")) {
      return respond(400, { message: "Invalid token format" });
    }

    const database = await connect();
    try {
      const handles = database.db.collection("@handles");
      let sub = user.sub;
      const existing = await handles.findOne({ _id: sub });
      if (!existing) return respond(404, { message: "Handle not found" });

      await handles.updateOne(
        { _id: sub },
        {
          $set: {
            claudeCodeToken: token,
            claudeCodeTokenUpdated: new Date(),
          },
        },
      );

      return respond(200, {
        message: `Token saved for @${existing.handle}`,
        handle: existing.handle,
      });
    } finally {
      await database.disconnect();
    }
  }

  return respond(405, { message: "Method Not Allowed" });
}
