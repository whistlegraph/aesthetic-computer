// device-auth.mjs — Device code auth flow for AC Native
//
// Flow:
//   1. Device GET /api/device-auth?action=request → { code, authUrl, pollUrl }
//   2. User visits authUrl on phone → logs in via Claude OAuth
//   3. Phone POST /api/device-auth { action: "approve", code, credentials }
//   4. Device GET /api/device-auth?action=poll&code=WOLF-3847 → { status, credentials }

import { MongoClient } from "mongodb";

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

function json(statusCode, data) {
  return {
    statusCode,
    headers: {
      "Content-Type": "application/json",
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
      "Access-Control-Allow-Headers": "Content-Type",
    },
    body: JSON.stringify(data),
  };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return json(200, { ok: true });
  }

  try {
    const params = new URLSearchParams(event.rawQuery || "");
    let action, code, credentials;

    if (event.httpMethod === "GET") {
      action = params.get("action") || "poll";
      code = params.get("code");
    } else {
      const body = JSON.parse(event.body || "{}");
      action = body.action;
      code = body.code;
      credentials = body.credentials;
    }

    if (action === "request") {
      const db = await getDb();
      const col = db.collection("device_auth");
      await col.deleteMany({ createdAt: { $lt: new Date(Date.now() - 10 * 60 * 1000) } });

      let newCode;
      for (let i = 0; i < 10; i++) {
        newCode = generateCode();
        const exists = await col.findOne({ _id: newCode });
        if (!exists) break;
      }

      await col.insertOne({ _id: newCode, status: "pending", createdAt: new Date() });
      const baseUrl = process.env.URL || "https://aesthetic.computer";

      return json(200, {
        code: newCode,
        authUrl: `${baseUrl}/api/device-login?code=${newCode}`,
        pollUrl: `${baseUrl}/api/device-auth?action=poll&code=${newCode}`,
        expiresIn: 600,
      });
    }

    if (action === "approve") {
      if (!code || !credentials) return json(400, { error: "missing code or credentials" });
      const db = await getDb();
      const col = db.collection("device_auth");
      const doc = await col.findOne({ _id: code });
      if (!doc) return json(404, { error: "code not found or expired" });
      if (doc.status !== "pending") return json(409, { error: "code already used" });
      await col.updateOne({ _id: code }, { $set: { status: "approved", credentials, approvedAt: new Date() } });
      return json(200, { ok: true });
    }

    if (action === "poll") {
      if (!code) return json(400, { error: "missing code" });
      const db = await getDb();
      const col = db.collection("device_auth");
      const doc = await col.findOne({ _id: code });
      if (!doc) return json(404, { error: "code not found or expired" });
      if (doc.status === "pending") return json(200, { status: "pending" });
      if (doc.status === "approved") {
        await col.deleteOne({ _id: code });
        return json(200, { status: "approved", credentials: doc.credentials });
      }
      return json(200, { status: doc.status });
    }

    return json(400, { error: "unknown action" });
  } catch (err) {
    console.error("device-auth error:", err);
    return json(500, { error: "server error", message: err.message });
  }
}
