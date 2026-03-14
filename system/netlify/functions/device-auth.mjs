// device-auth.mjs — Device code auth flow for AC Native
//
// Flow:
//   1. Device POST /api/device-auth { action: "request" }
//      → returns { code: "WOLF-3847", pollUrl, authUrl }
//   2. User visits authUrl on phone → logs in via Auth0 → Claude OAuth
//   3. Phone POST /api/device-auth { action: "approve", code, credentials }
//   4. Device POST /api/device-auth { action: "poll", code }
//      → returns { status: "pending" } or { status: "approved", credentials }
//
// Codes expire after 10 minutes. Approved tokens are deleted after first poll.

import { MongoClient } from "mongodb";
import { respond } from "../../backend/http.mjs";

let mongoClient;
async function getDb() {
  if (!mongoClient) {
    const uri = process.env.MONGODB_CONNECTION_STRING;
    if (!uri) throw new Error("No MONGODB_CONNECTION_STRING");
    mongoClient = new MongoClient(uri);
    await mongoClient.connect();
  }
  return mongoClient.db("aesthetic");
}

// Generate a short human-readable code like "WOLF-3847"
const WORDS = [
  "wolf", "bear", "deer", "hawk", "lynx", "fox", "owl", "elk",
  "crab", "moth", "frog", "crow", "wasp", "newt", "wren", "dove",
  "hare", "mink", "swan", "toad", "lark", "colt", "lamb", "puma",
];

function generateCode() {
  const word = WORDS[Math.floor(Math.random() * WORDS.length)].toUpperCase();
  const num = String(Math.floor(1000 + Math.random() * 9000));
  return `${word}-${num}`;
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return respond(200, null, "", {
      "access-control-allow-origin": "*",
      "access-control-allow-methods": "POST, GET, OPTIONS",
      "access-control-allow-headers": "content-type",
    });
  }

  const headers = {
    "access-control-allow-origin": "*",
    "content-type": "application/json",
  };

  try {
    // GET requests = poll (simpler for device curl)
    if (event.httpMethod === "GET") {
      const params = new URLSearchParams(event.rawQuery || "");
      const code = params.get("code");
      const action = params.get("action") || "poll";

      if (action === "request") {
        return await handleRequest(headers);
      }
      if (action === "poll" && code) {
        return await handlePoll(code, headers);
      }
      return respond(400, null, JSON.stringify({ error: "missing code param" }), headers);
    }

    // POST requests
    let body;
    try {
      body = JSON.parse(event.body || "{}");
    } catch {
      return respond(400, null, JSON.stringify({ error: "invalid json" }), headers);
    }

    const { action, code, credentials } = body;

    switch (action) {
      case "request":
        return await handleRequest(headers);
      case "approve":
        return await handleApprove(code, credentials, headers);
      case "poll":
        return await handlePoll(code, headers);
      default:
        return respond(400, null, JSON.stringify({ error: "unknown action" }), headers);
    }
  } catch (err) {
    console.error("device-auth error:", err);
    return respond(500, null, JSON.stringify({ error: "server error" }), headers);
  }
}

async function handleRequest(headers) {
  const db = await getDb();
  const col = db.collection("device_auth");

  // Clean up expired codes (>10 min old)
  await col.deleteMany({ createdAt: { $lt: new Date(Date.now() - 10 * 60 * 1000) } });

  // Generate unique code
  let code;
  for (let i = 0; i < 10; i++) {
    code = generateCode();
    const exists = await col.findOne({ _id: code });
    if (!exists) break;
  }

  await col.insertOne({
    _id: code,
    status: "pending",
    createdAt: new Date(),
  });

  const baseUrl = process.env.URL || "https://aesthetic.computer";

  return respond(200, null, JSON.stringify({
    code,
    authUrl: `${baseUrl}/device-login?code=${code}`,
    pollUrl: `${baseUrl}/api/device-auth?action=poll&code=${code}`,
    expiresIn: 600,
  }), headers);
}

async function handleApprove(code, credentials, headers) {
  if (!code || !credentials) {
    return respond(400, null, JSON.stringify({ error: "missing code or credentials" }), headers);
  }

  const db = await getDb();
  const col = db.collection("device_auth");

  const doc = await col.findOne({ _id: code });
  if (!doc) {
    return respond(404, null, JSON.stringify({ error: "code not found or expired" }), headers);
  }
  if (doc.status !== "pending") {
    return respond(409, null, JSON.stringify({ error: "code already used" }), headers);
  }

  await col.updateOne({ _id: code }, {
    $set: {
      status: "approved",
      credentials,
      approvedAt: new Date(),
    },
  });

  return respond(200, null, JSON.stringify({ ok: true }), headers);
}

async function handlePoll(code, headers) {
  if (!code) {
    return respond(400, null, JSON.stringify({ error: "missing code" }), headers);
  }

  const db = await getDb();
  const col = db.collection("device_auth");

  const doc = await col.findOne({ _id: code });
  if (!doc) {
    return respond(404, null, JSON.stringify({ error: "code not found or expired" }), headers);
  }

  if (doc.status === "pending") {
    return respond(200, null, JSON.stringify({ status: "pending" }), headers);
  }

  if (doc.status === "approved") {
    // Return credentials and delete the code (one-time use)
    await col.deleteOne({ _id: code });
    return respond(200, null, JSON.stringify({
      status: "approved",
      credentials: doc.credentials,
    }), headers);
  }

  return respond(200, null, JSON.stringify({ status: doc.status }), headers);
}
