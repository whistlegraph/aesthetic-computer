// device-token.mjs — Fetch device credentials for ac-native flash
// GET ?handle=jeffrey   (requires header: x-ac-device-secret)
// Returns: { token, handle, sub, email, claudeToken, githubPat }
//
// Used by ac-usb flash to pre-populate config.json with all credentials.
//
// SECURITY: this endpoint hands out a per-user device auth token plus
// (optionally) the user's Claude/GitHub credentials. It MUST stay gated.
// It is provisioning infrastructure, not a public lookup — it is locked
// behind a shared secret (AC_DEVICE_SECRET) that only the flashing host
// holds. Fails closed: if the secret is unset on the server, every request
// is denied. See ac-usb (fedac/native) for the caller.

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { getDeviceCreds } from "../../backend/device-creds.mjs";
import crypto from "crypto";

// Constant-time string compare so the gate can't be timing-probed.
function secretMatches(provided, expected) {
  if (!provided || !expected) return false;
  const a = Buffer.from(String(provided));
  const b = Buffer.from(String(expected));
  if (a.length !== b.length) return false;
  return crypto.timingSafeEqual(a, b);
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(200, "");
  if (event.httpMethod !== "GET") return respond(405, { message: "GET only" });

  // Gate: require the shared device secret. Fail closed if unconfigured.
  const expected = process.env.AC_DEVICE_SECRET;
  const provided =
    event.headers?.["x-ac-device-secret"] ||
    event.headers?.["X-Ac-Device-Secret"];
  if (!secretMatches(provided, expected)) {
    return respond(401, { message: "unauthorized" });
  }

  const handle = event.queryStringParameters?.handle;
  if (!handle) return respond(400, { message: "Missing handle" });

  const database = await connect();
  try {
    const handleDoc = await database.db
      .collection("@handles")
      .findOne({ handle }, { projection: { _id: 1 } });
    if (!handleDoc) return respond(404, { message: "Handle not found" });

    const sub = handleDoc._id;
    const userDoc = await database.db.collection("@users").findOne({ _id: sub });
    const creds = await getDeviceCreds(database.db, sub);

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
      claudeToken: creds?.claudeToken || null,
      githubPat: creds?.githubPat || null,
    });
  } finally {
    await database.disconnect();
  }
}
