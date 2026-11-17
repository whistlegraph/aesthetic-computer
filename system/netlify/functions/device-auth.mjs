// device-auth.mjs — Device code auth flow for AC Native
//
// Flow:
//   1. Device GET /api/device-auth?action=request
//        → { code, secret, authUrl, pollUrl, expiresIn }
//   2. User visits authUrl on phone → logs in via Claude OAuth
//   3. device-login POSTs { action: "approve", code, credentials } with the
//      x-ac-device-secret header (server to server only)
//   4. Device GET pollUrl (carries code + secret) → { status, credentials }
//
// The code (WOLF-3847) is only a label a person can read off the screen. It
// is short, so it is never enough to collect credentials: that takes the
// `secret` handed to the device in step 1, which is stored here as a hash.
// A few wrong secrets burn the code. Everything expires after ten minutes.

import crypto from "node:crypto";
import { MongoClient } from "mongodb";

const TTL_MS = 10 * 60 * 1000;
const MAX_BAD_POLLS = 5;
const MAX_PENDING = 500; // Open requests at once, across everyone.
const CODE_SHAPE = /^[A-Z]{4}-\d{4}$/;

let mongoClient;
let indexed;
async function getCollection() {
  if (!mongoClient) {
    const uri = process.env.MONGODB_CONNECTION_STRING;
    mongoClient = new MongoClient(uri);
    await mongoClient.connect();
  }
  const col = mongoClient
    .db(process.env.MONGODB_NAME || "aesthetic")
    .collection("device_auth");
  if (!indexed) {
    // Mongo drops each record ten minutes after it was made, polled or not.
    indexed = col
      .createIndex({ createdAt: 1 }, { expireAfterSeconds: TTL_MS / 1000 })
      .catch(() => { indexed = null; });
  }
  await indexed;
  return col;
}

const WORDS = [
  "wolf", "bear", "lynx", "hawk", "deer", "seal", "orca", "ibis",
  "crab", "moth", "frog", "crow", "wasp", "newt", "wren", "dove",
  "hare", "mink", "swan", "toad", "lark", "colt", "lamb", "puma",
];

function generateCode() {
  const word = WORDS[crypto.randomInt(WORDS.length)].toUpperCase();
  return `${word}-${crypto.randomInt(1000, 10000)}`;
}

const hash = (text) => crypto.createHash("sha256").update(String(text)).digest("hex");

function same(a, b) {
  if (!a || !b) return false;
  const x = Buffer.from(String(a));
  const y = Buffer.from(String(b));
  return x.length === y.length && crypto.timingSafeEqual(x, y);
}

// Server-to-server calls (device-login) carry the shared device secret.
// Fails closed when the secret is not configured.
function internal(event) {
  return same(
    event.headers?.["x-ac-device-secret"] || event.headers?.["X-Ac-Device-Secret"],
    process.env.AC_DEVICE_SECRET,
  );
}

function json(statusCode, data) {
  return {
    statusCode,
    headers: {
      "Content-Type": "application/json",
      "Cache-Control": "no-store",
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
      "Access-Control-Allow-Headers": "Content-Type",
    },
    body: JSON.stringify(data),
  };
}

const GONE = { error: "code not found or expired" };

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return json(200, { ok: true });
  }

  try {
    let action, code, secret, credentials;

    if (event.httpMethod === "GET") {
      // lith hands over a parsed query; Netlify handed over the raw string.
      const raw = new URLSearchParams(event.rawQuery || "");
      const query = event.queryStringParameters || {};
      const param = (name) => query[name] ?? raw.get(name);
      action = param("action") || "poll";
      code = param("code");
      secret = param("secret");
    } else {
      const body = JSON.parse(event.body || "{}");
      action = body.action;
      code = body.code;
      secret = body.secret;
      credentials = body.credentials;
    }

    if (code != null && !CODE_SHAPE.test(String(code))) return json(404, GONE);
    const fresh = { createdAt: { $gte: new Date(Date.now() - TTL_MS) } };

    if (action === "request") {
      const col = await getCollection();
      await col.deleteMany({ createdAt: { $lt: new Date(Date.now() - TTL_MS) } });
      if ((await col.countDocuments(fresh)) >= MAX_PENDING) {
        return json(429, { error: "too many devices waiting, try again shortly" });
      }

      const newSecret = crypto.randomBytes(32).toString("base64url");
      let newCode;
      for (let i = 0; i < 10 && !newCode; i++) {
        const candidate = generateCode();
        try {
          await col.insertOne({
            _id: candidate,
            status: "pending",
            secretHash: hash(newSecret),
            badPolls: 0,
            createdAt: new Date(),
          });
          newCode = candidate;
        } catch (err) {
          if (err?.code !== 11000) throw err; // 11000: that code is taken.
        }
      }
      if (!newCode) return json(503, { error: "no code available, try again" });

      const baseUrl = process.env.URL || "https://aesthetic.computer";
      return json(200, {
        code: newCode,
        secret: newSecret,
        authUrl: `${baseUrl}/api/device-login?code=${newCode}`,
        pollUrl: `${baseUrl}/api/device-auth?action=poll&code=${newCode}&secret=${newSecret}`,
        expiresIn: TTL_MS / 1000,
      });
    }

    if (action === "approve") {
      // Only device-login, after a finished Claude sign-in, may approve.
      if (!internal(event)) return json(401, { error: "unauthorized" });
      if (!code || !credentials) return json(400, { error: "missing code or credentials" });
      const col = await getCollection();
      const result = await col.updateOne(
        { _id: code, status: "pending", ...fresh },
        { $set: { status: "approved", credentials, approvedAt: new Date() } },
      );
      if (result.matchedCount) return json(200, { ok: true });
      const doc = await col.findOne({ _id: code, ...fresh }, { projection: { status: 1 } });
      return doc ? json(409, { error: "code already used" }) : json(404, GONE);
    }

    if (action === "poll") {
      if (!code) return json(400, { error: "missing code" });
      const col = await getCollection();

      // Without the device's secret, only device-login may ask, and it only
      // ever learns whether the code is waiting.
      if (!secret) {
        if (!internal(event)) return json(401, { error: "unauthorized" });
        const doc = await col.findOne({ _id: code, ...fresh }, { projection: { status: 1 } });
        return doc ? json(200, { status: doc.status }) : json(404, GONE);
      }

      const doc = await col.findOne({ _id: code, ...fresh });
      if (!doc) return json(404, GONE);
      if (!same(hash(secret), doc.secretHash)) {
        // A wrong secret looks the same as a missing code, and a few of them
        // burn the code so it cannot be guessed at.
        if ((doc.badPolls || 0) + 1 >= MAX_BAD_POLLS) await col.deleteOne({ _id: code });
        else await col.updateOne({ _id: code }, { $inc: { badPolls: 1 } });
        return json(404, GONE);
      }
      if (doc.status === "approved") {
        // Hand the credentials over exactly once.
        const taken = await col.findOneAndDelete({ _id: code, status: "approved" });
        const handed = taken?.value ?? taken; // Driver 5 wraps, driver 6 does not.
        if (!handed?.credentials) return json(404, GONE);
        return json(200, { status: "approved", credentials: handed.credentials });
      }
      return json(200, { status: doc.status });
    }

    return json(400, { error: "unknown action" });
  } catch (err) {
    console.error("device-auth error:", err);
    return json(500, { error: "server error" });
  }
}
