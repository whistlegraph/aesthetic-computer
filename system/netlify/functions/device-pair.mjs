// device-pair, 2026.03.18
// Device pairing flow for AC Native OS identity switching.
// POST {action:"create"} → generates a 6-char code (no auth required)
// GET  ?code=XXXXXX       → polls for claimed status (no auth required)
// POST {action:"claim", code:"XXXXXX"} → authenticated user claims the code

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { getDeviceCreds } from "../../backend/device-creds.mjs";
import crypto from "node:crypto";

function generateCode() {
  const chars = "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"; // no I/O/0/1 for clarity
  let code = "";
  for (let i = 0; i < 6; i++) {
    code += chars[Math.floor(Math.random() * chars.length)];
  }
  return code;
}

function generatePollSecret() {
  return crypto.randomBytes(32).toString("base64url");
}

function secretDigest(secret) {
  return crypto.createHash("sha256").update(String(secret)).digest("hex");
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

      // Browser/TV pairings are bearer flows: the short public code shown in
      // the QR is not sufficient to retrieve a session. Only the creating TV
      // possesses this 256-bit secret, and only its digest is stored.
      if (doc.kind === "browser") {
        const supplied = secretDigest(event.queryStringParameters?.secret || "");
        const expected = String(doc.pollSecretDigest || "");
        if (!expected || supplied.length !== expected.length ||
            !crypto.timingSafeEqual(Buffer.from(supplied), Buffer.from(expected))) {
          return respond(403, { message: "Invalid polling credential" });
        }
        if (doc.claimed) {
          const result = {
            status: "claimed",
            handle: doc.handle,
            session: { accessToken: doc.token, account: { label: `@${doc.handle}`, id: doc.sub } },
          };
          // A browser session can be collected once. Never place native
          // Claude/GitHub credentials in this document or response.
          const delivery = await database.db.collection("device-pairs").updateOne(
            { _id: doc._id, deliveredAt: { $exists: false } },
            { $set: { deliveredAt: new Date() }, $unset: { token: "" } },
          );
          if (doc.deliveredAt || !doc.token || delivery.modifiedCount !== 1)
            return respond(410, { message: "Session already collected" });
          return respond(200, result);
        }
        return respond(200, { status: "pending" });
      }

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

        const kind = body.kind === "browser" ? "browser" : "native";
        const pollSecret = kind === "browser" ? generatePollSecret() : null;
        await pairs.insertOne({
          _id: code,
          kind,
          claimed: false,
          createdAt: new Date(),
          ...(pollSecret ? { pollSecretDigest: secretDigest(pollSecret) } : {}),
        });

        return respond(200, { code, ...(pollSecret ? { pollSecret } : {}) });
      }

      if (body.action === "claim") {
        const code = body.code?.toUpperCase();
        if (!code) return respond(400, { message: "Missing code" });

        // Require authentication
        const user = await authorize(event.headers);
        if (!user) return respond(401, { message: "unauthorized" });

        // Look up handle (for display) + device creds (from the dedicated store)
        const handleDoc = await database.db
          .collection("@handles")
          .findOne({ _id: user.sub }, { projection: { handle: 1 } });
        if (!handleDoc)
          return respond(404, { message: "No handle found for this account" });

        const pair = await pairs.findOne({ _id: code });
        if (!pair) return respond(404, { message: "Code not found or expired" });
        const creds = pair.kind === "browser" ? null : await getDeviceCreds(database.db, user.sub);

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
              ...(pair.kind === "browser" ? {} : {
                claudeToken: creds?.claudeToken || "",
                githubPat: creds?.githubPat || "",
              }),
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
          kind: pair.kind,
        });
      }

      return respond(400, { message: "Invalid action" });
    } finally {
      await database.disconnect();
    }
  }

  return respond(405, { message: "Method Not Allowed" });
}
