// device-token.mjs — Generate device auth tokens for ac-native flash
// GET ?handle=jeffrey&secret=<DEVICE_TOKEN_SECRET>
// Returns: { token, handle, sub, email, claudeToken, githubPat }
//
// Used by ac-usb flash to pre-populate config.json with all credentials.
// The secret prevents unauthorized token generation.

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import crypto from "crypto";

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(200, "");
  if (event.httpMethod !== "GET") return respond(405, { message: "GET only" });

  const handle = event.queryStringParameters?.handle;
  const secret = event.queryStringParameters?.secret;
  const expectedSecret = process.env.DEVICE_TOKEN_SECRET;

  if (!handle) return respond(400, { message: "Missing handle" });
  if (!secret || !expectedSecret || secret !== expectedSecret) {
    return respond(401, { message: "unauthorized" });
  }

  const database = await connect();
  try {
    // Look up handle → sub
    const handleDoc = await database.db
      .collection("@handles")
      .findOne({ handle });

    if (!handleDoc) return respond(404, { message: "Handle not found" });

    const sub = handleDoc._id; // _id is the auth0 sub

    // Look up user for email
    const userDoc = await database.db
      .collection("@users")
      .findOne({ _id: sub });

    // Look up device tokens
    const tokenDoc = await database.db
      .collection("device-tokens")
      .findOne({ _id: sub });

    // Generate a fresh device auth token (signed with secret + timestamp)
    const ts = Date.now().toString(36);
    const token = crypto
      .createHmac("sha256", expectedSecret)
      .update(`${sub}:${ts}`)
      .digest("hex")
      .slice(0, 32);

    return respond(200, {
      handle,
      sub,
      email: userDoc?.email || "",
      token: `${token}.${ts}`,
      claudeToken: tokenDoc?.claudeToken || null,
      githubPat: tokenDoc?.githubPat || null,
    });
  } finally {
    await database.disconnect();
  }
}
