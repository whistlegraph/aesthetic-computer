// device-token.mjs — Fetch device credentials for ac-native flash
// GET ?handle=jeffrey
// Returns: { token, handle, sub, email, claudeToken, githubPat }
//
// Used by ac-usb flash to pre-populate config.json with all credentials.

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import crypto from "crypto";

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(200, "");
  if (event.httpMethod !== "GET") return respond(405, { message: "GET only" });

  const handle = event.queryStringParameters?.handle;
  if (!handle) return respond(400, { message: "Missing handle" });

  const database = await connect();
  try {
    const handleDoc = await database.db
      .collection("@handles")
      .findOne({ handle });
    if (!handleDoc) return respond(404, { message: "Handle not found" });

    const sub = handleDoc._id;
    const userDoc = await database.db.collection("@users").findOne({ _id: sub });
    const tokenDoc = await database.db.collection("device-tokens").findOne({ _id: sub });

    // Generate a device auth token
    const ts = Date.now().toString(36);
    const hmacKey = process.env.JWT_SECRET || "ac-device";
    const token = crypto
      .createHmac("sha256", hmacKey)
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
