// device-pair, 2026.03.18
// Device pairing flow for AC Native OS identity switching.
// POST {action:"create"} → generates a 6-char code (no auth required)
// GET  ?code=XXXXXX       → polls for claimed status (no auth required)
// POST {action:"claim", code:"XXXXXX"} → authenticated user claims the code

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

function generateCode() {
  const chars = "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"; // no I/O/0/1 for clarity
  let code = "";
  for (let i = 0; i < 6; i++) {
    code += chars[Math.floor(Math.random() * chars.length)];
  }
  return code;
}

export async function handler(event) {
  // CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, "");
  }

  // GET: Poll for claimed code
  if (event.httpMethod === "GET") {
    const code = event.queryStringParameters?.code;
    if (!code) return respond(400, { message: "Missing code parameter" });

    const database = await connect();
    try {
      const doc = await database.db
        .collection("device-pairs")
        .findOne({ _id: code.toUpperCase() });

      if (!doc) return respond(404, { message: "Code not found or expired" });

      if (doc.claimed) {
        // Return the full config for the device
        return respond(200, {
          status: "claimed",
          handle: doc.handle,
          sub: doc.sub,
          email: doc.email,
          token: doc.token,
          claudeToken: doc.claudeToken || null,
          githubPat: doc.githubPat || null,
        });
      }

      return respond(200, { status: "pending" });
    } finally {
      await database.disconnect();
    }
  }

  // POST: Create or claim
  if (event.httpMethod === "POST") {
    let body;
    try {
      body = JSON.parse(event.body);
    } catch {
      return respond(400, { message: "Invalid JSON" });
    }

    const database = await connect();
    try {
      const pairs = database.db.collection("device-pairs");

      // Ensure TTL index exists (codes expire after 10 minutes)
      await pairs
        .createIndex({ createdAt: 1 }, { expireAfterSeconds: 600 })
        .catch(() => {});

      if (body.action === "create") {
        // Generate unique code
        let code;
        let attempts = 0;
        do {
          code = generateCode();
          const existing = await pairs.findOne({ _id: code });
          if (!existing) break;
          attempts++;
        } while (attempts < 10);

        await pairs.insertOne({
          _id: code,
          claimed: false,
          createdAt: new Date(),
        });

        return respond(200, { code });
      }

      if (body.action === "claim") {
        const code = body.code?.toUpperCase();
        if (!code) return respond(400, { message: "Missing code" });

        // Require authentication
        const user = await authorize(event.headers);
        if (!user) return respond(401, { message: "unauthorized" });

        // Look up handle + device tokens
        const handleDoc = await database.db
          .collection("@handles")
          .findOne({ _id: user.sub });
        if (!handleDoc)
          return respond(404, { message: "No handle found for this account" });

        // Get the auth token from the request (pass it through to the device)
        const authHeader = event.headers.authorization || "";
        const authToken = authHeader.startsWith("Bearer ")
          ? authHeader.slice(7).trim()
          : "";

        const result = await pairs.updateOne(
          { _id: code, claimed: false },
          {
            $set: {
              claimed: true,
              claimedAt: new Date(),
              handle: handleDoc.handle,
              sub: user.sub,
              email: user.email || "",
              token: authToken,
              claudeToken: handleDoc.claudeCodeToken || "",
              githubPat: handleDoc.githubPat || "",
            },
          },
        );

        if (result.matchedCount === 0) {
          return respond(404, {
            message: "Code not found, expired, or already claimed",
          });
        }

        return respond(200, {
          message: `Device paired as @${handleDoc.handle}`,
          handle: handleDoc.handle,
        });
      }

      return respond(400, { message: "Invalid action" });
    } finally {
      await database.disconnect();
    }
  }

  return respond(405, { message: "Method Not Allowed" });
}
