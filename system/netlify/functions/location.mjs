// /api/location — ingest location points for @jeffrey.
//
// Forward-feed counterpart to the vault photo backfill. Accepts a single
// point or a batch from iOS Shortcuts / Slab / any authenticated client.
// Auth is via a bearer token stored in the `secrets` collection under
// `location-ingest` (`token` or `ingestToken` field). Token is provisioned
// out of band; rotate by updating the secret doc.
//
// Body shapes:
//   { lat, lon, acc?, ts?, source?, note? }
//   { points: [ { lat, lon, acc?, ts?, source?, note? }, ... ] }
//
// Defaults: handle="jeffrey", source="shortcut", ts=now.
//
// Mongo collection `locations`:
//   { handle, ts, lat, lon, acc, source, note?, ip, user_agent, received_at }
// Indexes: { handle:1, ts:-1 }, { ts:-1 }.

import { MongoClient } from "mongodb";

let mongoClient;
let mongoDb;
let cachedToken = "";
let cachedTokenAt = 0;
const TOKEN_TTL_MS = 60_000;
const SECRET_ID = "location-ingest";

function respond(statusCode, body) {
  return {
    statusCode,
    headers: {
      "content-type": "application/json",
      "cache-control": "no-store",
      "access-control-allow-origin": "*",
      "access-control-allow-methods": "POST, OPTIONS",
      "access-control-allow-headers": "content-type, authorization",
    },
    body: JSON.stringify(body),
  };
}

function getBearerToken(event) {
  const auth = event?.headers?.authorization || event?.headers?.Authorization || "";
  if (!auth) return "";
  const m = auth.match(/^Bearer\s+(.+)$/i);
  return m?.[1] || "";
}

async function getDb() {
  if (mongoDb) return mongoDb;
  const connectionString =
    process.env.LOCATION_MONGODB_CONNECTION_STRING ||
    process.env.MONGODB_CONNECTION_STRING;
  const dbName = process.env.MONGODB_NAME || "aesthetic";
  if (!connectionString) throw new Error("MongoDB connection string is not configured");
  mongoClient = new MongoClient(connectionString, {
    serverSelectionTimeoutMS: 8000,
    connectTimeoutMS: 8000,
  });
  await mongoClient.connect();
  mongoDb = mongoClient.db(dbName);
  return mongoDb;
}

async function getExpectedToken() {
  if (cachedToken && Date.now() - cachedTokenAt < TOKEN_TTL_MS) return cachedToken;
  const db = await getDb();
  const doc = await db.collection("secrets").findOne({ _id: SECRET_ID });
  const token = String(doc?.token || doc?.ingestToken || "").trim();
  cachedToken = token;
  cachedTokenAt = Date.now();
  return token;
}

async function ensureIndexes(col) {
  await col.createIndex({ handle: 1, ts: -1 }, { background: true });
  await col.createIndex({ ts: -1 }, { background: true });
}

function normalizePoint(raw, fallback) {
  if (raw == null || typeof raw !== "object") return null;
  const lat = Number(raw.lat);
  const lon = Number(raw.lon);
  if (!Number.isFinite(lat) || !Number.isFinite(lon)) return null;
  if (lat < -90 || lat > 90 || lon < -180 || lon > 180) return null;
  const ts = raw.ts ? new Date(raw.ts) : fallback.now;
  if (Number.isNaN(ts.getTime())) return null;
  const point = {
    handle: "jeffrey",
    ts,
    lat: Math.round(lat * 1e6) / 1e6,
    lon: Math.round(lon * 1e6) / 1e6,
    source: raw.source ? String(raw.source).slice(0, 32) : "shortcut",
    received_at: fallback.now,
    ip: fallback.ip,
    user_agent: fallback.userAgent,
  };
  if (raw.acc != null && Number.isFinite(Number(raw.acc))) {
    point.acc = Math.round(Number(raw.acc) * 10) / 10;
  }
  if (raw.note) point.note = String(raw.note).slice(0, 280);
  return point;
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return {
      statusCode: 204,
      headers: {
        "access-control-allow-origin": "*",
        "access-control-allow-methods": "POST, OPTIONS",
        "access-control-allow-headers": "content-type, authorization",
      },
      body: "",
    };
  }
  if (event.httpMethod !== "POST") return respond(405, { error: "Method not allowed" });

  let expected;
  try {
    expected = await getExpectedToken();
  } catch (err) {
    return respond(503, { error: "Token lookup failed", message: err.message });
  }
  if (!expected) {
    return respond(503, {
      error: "Token is not configured",
      secret_id: SECRET_ID,
      expected_field: "token (or ingestToken)",
    });
  }
  const token = getBearerToken(event);
  if (!token || token !== expected) return respond(401, { error: "Unauthorized" });

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return respond(400, { error: "Invalid JSON payload" });
  }

  const now = new Date();
  const ip =
    event.headers?.["x-forwarded-for"]?.split(",")[0]?.trim() ||
    event.headers?.["client-ip"] ||
    "";
  const userAgent = event.headers?.["user-agent"] || "";
  const fallback = { now, ip, userAgent };

  const rawPoints = Array.isArray(body.points) ? body.points : [body];
  const points = rawPoints.map((p) => normalizePoint(p, fallback)).filter(Boolean);
  if (!points.length) return respond(400, { error: "No valid points in payload" });

  try {
    const db = await getDb();
    const col = db.collection("locations");
    await ensureIndexes(col);
    const result = await col.insertMany(points, { ordered: false });
    return respond(200, { ok: true, inserted: result.insertedCount, total: points.length });
  } catch (err) {
    return respond(500, { error: "insert failed", message: err.message });
  }
}
